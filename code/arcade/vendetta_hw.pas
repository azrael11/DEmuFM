unit vendetta_hw;

interface

uses
  WinApi.Windows,
  nz80,
  konami,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ym_2151,
  k052109,
  k053260,
  k053246_k053247_k055673,
  k054000,
  k053251,
  timer_engine,
  eepromser;

function start_vendetta: boolean;

implementation

const
  // vendetta
  vendetta_rom: tipo_roms = (n: '081u01.17c'; l: $40000; p: 0; crc: $B4D9ADE5);
  vendetta_sound: tipo_roms = (n: '081b02'; l: $10000; p: 0; crc: $4C604D9B);
  vendetta_tiles: array [0 .. 1] of tipo_roms = ((n: '081a09'; l: $80000; p: 0; crc: $B4C777A9), (n: '081a08';
    l: $80000; p: 2; crc: $272AC8D9));
  vendetta_sprites: array [0 .. 3] of tipo_roms = ((n: '081a04'; l: $100000; p: 0; crc: $464B9AA4),
    (n: '081a05'; l: $100000; p: 2; crc: $4E173759), (n: '081a06'; l: $100000; p: 4; crc: $E9FE6D80),
    (n: '081a07'; l: $100000; p: 6; crc: $8A22B29A));
  vendetta_k053260: tipo_roms = (n: '081a03'; l: $100000; p: 0; crc: $14B6BAEA);
  vendetta_eeprom: tipo_roms = (n: 'vendetta.nv'; l: $80; p: 0; crc: $FBAC4E30);
  // DIP
  vendetta_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
    dip: ((dip_val: $02; dip_name: '4C 1C'), (dip_val: $05; dip_name: '3C 1C'), (dip_val: $08;
    dip_name: '2C 1C'), (dip_val: $04; dip_name: '3C 2C'), (dip_val: $01; dip_name: '4C 3C'), (dip_val: $0F;
    dip_name: '1C 1C'), (dip_val: $03; dip_name: '3C 4C'), (dip_val: $07; dip_name: '2C 3C'), (dip_val: $0E;
    dip_name: '1C 2C'), (dip_val: $06; dip_name: '2C 5C'), (dip_val: $0D; dip_name: '1C 3C'), (dip_val: $0C;
    dip_name: '1C 4C'), (dip_val: $0B; dip_name: '1C 5C'), (dip_val: $0A; dip_name: '1C 6C'), (dip_val: $09;
    dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80;
    dip_name: '2C 1C'), (dip_val: $40; dip_name: '3C 2C'), (dip_val: $10; dip_name: '4C 3C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $30; dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0;
    dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0; dip_name: '1C 3C'), (dip_val: $C0;
    dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90;
    dip_name: '1C 7C'), (dip_val: $0; dip_name: 'No Coin'))), ());
  vendetta_dip_b: array [0 .. 3] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '1'), (dip_val: $2; dip_name: '2'), (dip_val: $1;
    dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $60; name: 'Difficulty'; number: 4; dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40;
    dip_name: 'Normal'), (dip_val: $20; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  vendetta_dip_c: array [0 .. 1] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());

var
  tiles_rom, sprite_rom, k053260_rom: pbyte;
  sound_latch, sprite_colorbase, rom_bank1, video_bank, timer_n: byte;
  irq_enabled: boolean;
  layer_colorbase, layerpri: array [0 .. 2] of byte;
  rom_bank: array [0 .. 27, 0 .. $1FFF] of byte;

procedure vendetta_cb(layer, bank: word; var code: dword; var color: word; var flags: word;
  var priority: word);
begin
  code := code or (((color and $03) shl 8) or ((color and $30) shl 6) or ((color and $0C) shl 10) or
    (bank shl 14));
  color := layer_colorbase[layer] + ((color and $C0) shr 6);
end;

procedure vendetta_sprite_cb(var code: dword; var color: word; var priority_mask: word);
var
  pri: integer;
begin
  pri := (color and $03E0) shr 4; // ???????
  if (pri <= layerpri[2]) then
    priority_mask := 0
  else if ((pri > layerpri[2]) and (pri <= layerpri[1])) then
    priority_mask := 1
  else if ((pri > layerpri[1]) and (pri <= layerpri[0])) then
    priority_mask := 2
  else
    priority_mask := 3;
  color := sprite_colorbase + (color and $001F);
end;

procedure update_video_vendetta;
var
  bg_colorbase: byte;
  sorted_layer: array [0 .. 2] of byte;
begin
  sprite_colorbase := k053251_0.get_palette_index(K053251_CI1);
  layer_colorbase[0] := k053251_0.get_palette_index(K053251_CI2);
  layer_colorbase[1] := k053251_0.get_palette_index(K053251_CI3);
  layer_colorbase[2] := k053251_0.get_palette_index(K053251_CI4);
  sorted_layer[0] := 0;
  layerpri[0] := k053251_0.get_priority(K053251_CI2);
  sorted_layer[1] := 1;
  layerpri[1] := k053251_0.get_priority(K053251_CI3);
  sorted_layer[2] := 2;
  layerpri[2] := k053251_0.get_priority(K053251_CI4);
  konami_sortlayers3(@sorted_layer, @layerpri);
  bg_colorbase := k053251_0.get_palette_index(K053251_CI0);
  if k053251_0.dirty_tmap[K053251_CI2] then
  begin
    k052109_0.clean_video_buffer_layer(0);
    k053251_0.dirty_tmap[K053251_CI2] := false;
  end;
  if k053251_0.dirty_tmap[K053251_CI3] then
  begin
    k052109_0.clean_video_buffer_layer(1);
    k053251_0.dirty_tmap[K053251_CI3] := false;
  end;
  if k053251_0.dirty_tmap[K053251_CI4] then
  begin
    k052109_0.clean_video_buffer_layer(2);
    k053251_0.dirty_tmap[K053251_CI4] := false;
  end;
  k052109_0.draw_tiles;
  k053246_0.k053247_update_sprites;
  fill_full_screen(4, bg_colorbase * 16);
  k053246_0.k053247_draw_sprites(3);
  k052109_0.draw_layer(sorted_layer[0], 4);
  k053246_0.k053247_draw_sprites(2);
  k052109_0.draw_layer(sorted_layer[1], 4);
  k053246_0.k053247_draw_sprites(1);
  k052109_0.draw_layer(sorted_layer[2], 4);
  k053246_0.k053247_draw_sprites(0);
  update_final_piece(112, 16, 288, 224, 4);
end;

procedure events_vendetta;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // Service
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
  end;
end;

procedure vendetta_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := konami_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        konami_0.run(frame_m);
        frame_m := frame_m + konami_0.tframes - konami_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          if irq_enabled then
            konami_0.change_irq(HOLD_LINE);
          update_video_vendetta;
        end;
      end;
      events_vendetta;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function vendetta_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      vendetta_getbyte := rom_bank[rom_bank1, direccion];
    $2000 .. $3FFF, $8000 .. $FFFF:
      vendetta_getbyte := memory[direccion];
    $4000 .. $4FFF:
      if video_bank = 0 then
        vendetta_getbyte := k052109_0.read(direccion and $FFF)
      else
        vendetta_getbyte := k053246_0.k053247_r(direccion and $FFF);
    $5F80 .. $5F9F:
      vendetta_getbyte := k054000_0.read(direccion and $1F);
    $5FC0:
      vendetta_getbyte := marcade.in0; // p1
    $5FC1:
      vendetta_getbyte := marcade.in1; // p2
    $5FC2:
      vendetta_getbyte := $FF; // p3
    $5FC3:
      vendetta_getbyte := $FF; // p3
    $5FD0:
      vendetta_getbyte := eepromser_0.do_read + (eepromser_0.ready_read shl 1) + $F4;
    $5FD1:
      vendetta_getbyte := marcade.in2; // service
    $5FE4:
      begin
        z80_0.change_irq(HOLD_LINE);
        vendetta_getbyte := 0;
      end;
    $5FE6 .. $5FE7:
      vendetta_getbyte := k053260_0.main_read(direccion and 1);
    $5FE8 .. $5FE9:
      vendetta_getbyte := k053246_0.read(direccion and 1);
    $5FEA:
      vendetta_getbyte := 0;
    $6000 .. $6FFF:
      if video_bank = 0 then
        vendetta_getbyte := k052109_0.read($2000 + (direccion and $FFF))
      else
        vendetta_getbyte := buffer_paleta[direccion and $FFF];
  else
    vendetta_getbyte := k052109_0.read(direccion and $3FFF);
  end;
end;

procedure vendetta_putbyte(direccion: word; valor: byte);
  procedure change_color(pos: word);
  var
    color: tcolor;
    valor: word;
  begin
    valor := (buffer_paleta[pos * 2] shl 8) + buffer_paleta[(pos * 2) + 1];
    color.b := pal5bit(valor shr 10);
    color.g := pal5bit(valor shr 5);
    color.r := pal5bit(valor);
    set_pal_color_alpha(color, pos);
    k052109_0.clean_video_buffer;
  end;

begin
  case direccion of
    0 .. $1FFF, $8000 .. $FFFF:
      ; // ROM
    $2000 .. $3FFF:
      memory[direccion] := valor;
    $4000 .. $4FFF:
      if video_bank = 0 then
        k052109_0.write(direccion and $FFF, valor)
      else
        k053246_0.k053247_w(direccion and $FFF, valor);
    $5F80 .. $5F9F:
      k054000_0.write(direccion and $1F, valor);
    $5FA0 .. $5FAF:
      k053251_0.write(direccion and $F, valor);
    $5FB0 .. $5FB7:
      k053246_0.write(direccion and $7, valor);
    $5FE0:
      begin
        if (valor and $8) <> 0 then
          k052109_0.set_rmrd_line(ASSERT_LINE)
        else
          k052109_0.set_rmrd_line(CLEAR_LINE);
        if (valor and $20) <> 0 then
          k053246_0.set_objcha_line(ASSERT_LINE)
        else
          k053246_0.set_objcha_line(CLEAR_LINE);
      end;
    $5FE2:
      begin
        if valor = $FF then
          exit;
        irq_enabled := ((valor shr 6) and 1) <> 0;
        video_bank := valor and 1;
        eepromser_0.di_write((valor shr 5) and 1);
        eepromser_0.cs_write((valor shr 3) and 1);
        eepromser_0.clk_write((valor shr 4) and 1);
      end;

    $5FE4:
      z80_0.change_irq(HOLD_LINE);
    $5FE6 .. $5FE7:
      k053260_0.main_write(direccion and 1, valor);
    $6000 .. $6FFF:
      if video_bank = 0 then
        k052109_0.write((direccion and $FFF) + $2000, valor)
      else if buffer_paleta[direccion and $FFF] <> valor then
      begin
        buffer_paleta[direccion and $FFF] := valor;
        change_color((direccion and $FFF) shr 1);
      end;
  else
    k052109_0.write(direccion and $3FFF, valor);
  end;
end;

procedure vendetta_bank(valor: byte);
begin
  rom_bank1 := valor and $1F;
end;

function vendetta_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $F7FF:
      vendetta_snd_getbyte := mem_snd[direccion];
    $F801:
      vendetta_snd_getbyte := ym2151_0.status;
    $FC00 .. $FC2F:
      vendetta_snd_getbyte := k053260_0.read(direccion and $3F);
  end;
end;

procedure vendetta_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $EFFF:
      ; // ROM
    $F000 .. $F7FF:
      mem_snd[direccion] := valor;
    $F800:
      ym2151_0.reg(valor);
    $F801:
      ym2151_0.write(valor);
    $FA00:
      begin
        z80_0.change_nmi(ASSERT_LINE);
        timers.enabled(timer_n, true);
      end;
    $FC00 .. $FC2F:
      k053260_0.write(direccion and $3F, valor);
  end;
end;

// procedure vendetta_snd_nmi(valor:byte);
// begin
// if ((valor=ASSERT_LINE) and not(nmi_block)) then z80_0.change_nmi(ASSERT_LINE);
// end;

procedure vendetta_clear_nmi;
begin
  timers.enabled(timer_n, false);
  // nmi_block:=false;
  z80_0.change_nmi(CLEAR_LINE);
end;

procedure vendetta_sound_update;
begin
  ym2151_0.update;
  k053260_0.update;
end;

// Main
procedure reset_vendetta;
begin
  konami_0.reset;
  z80_0.reset;
 eepromser_0.reset;
  k052109_0.reset;
  k053260_0.reset;
  k053251_0.reset;
  k054000_0.reset;
  k053246_0.reset;
  ym2151_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_latch := 0;
  rom_bank1 := 0;
  irq_enabled := false;
  video_bank := 0;
end;

procedure close_vendetta;
begin
  if k053260_rom <> nil then
    freemem(k053260_rom);
  if sprite_rom <> nil then
    freemem(sprite_rom);
  if tiles_rom <> nil then
    freemem(tiles_rom);
  k053260_rom := nil;
  sprite_rom := nil;
  tiles_rom := nil;
end;

function start_vendetta: boolean;
var
  memory_temp: array [0 .. $3FFFF] of byte;
  f: byte;
begin
  start_vendetta := false;
  machine_calls.close := close_vendetta;
  machine_calls.reset := reset_vendetta;
  machine_calls.general_loop := vendetta_loop;
  machine_calls.fps_max := 59.17;
  // Pantallas para el K052109
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 1024, 1024, false, true);
  start_video(288, 224, true);
  start_audio(true);
  // cargar roms y ponerlas en su sitio...
  if not(roms_load(@memory_temp, vendetta_rom)) then
    exit;
  copymemory(@memory[$8000], @memory_temp[$38000], $8000);
  for f := 0 to 27 do
    copymemory(@rom_bank[f, 0], @memory_temp[f * $2000], $2000);
  // cargar sonido
  if not(roms_load(@mem_snd, vendetta_sound)) then
    exit;
  // Main CPU
  konami_0 := cpu_konami.create(12000000, 256);
  konami_0.change_ram_calls(vendetta_getbyte, vendetta_putbyte);
  konami_0.change_set_lines(vendetta_bank);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(vendetta_snd_getbyte, vendetta_snd_putbyte);
  z80_0.init_sound(vendetta_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  getmem(k053260_rom, $100000);
  if not(roms_load(k053260_rom, vendetta_k053260)) then
    exit;
  k053260_0 := tk053260_chip.create(3579545, k053260_rom, $100000, 0.70);
  // k053260_0.change_calls(vendetta_snd_nmi,nil);
  timer_n := timers.init(z80_0.numero_cpu, 90, vendetta_clear_nmi, nil, false);
  // Iniciar video
  layer_colorbase[0] := 0;
  layer_colorbase[1] := 0;
  layer_colorbase[2] := 0;
  layerpri[0] := 0;
  layerpri[1] := 0;
  layerpri[2] := 0;
  sprite_colorbase := 0;
  // Prioridad
  k053251_0 := k053251_chip.create;
  // tiles
  getmem(tiles_rom, $100000);
  if not(roms_load32b(tiles_rom, vendetta_tiles)) then
    exit;
  k052109_0 := k052109_chip.create(1, 2, 3, 0, vendetta_cb, tiles_rom, $100000);
  // sprites
  getmem(sprite_rom, $400000);
  if not(roms_load64b(sprite_rom, vendetta_sprites)) then
    exit;
  k053246_0 := k053246_chip.create(4, vendetta_sprite_cb, sprite_rom, $400000);
  k053246_0.k053247_start;
  // eeprom
  eepromser_0 := eepromser_chip.create(ER5911, 8);
  if not(eepromser_0.load_data('vendetta.nv')) then
  begin
    if not(roms_load(@memory_temp, vendetta_eeprom)) then
      exit;
    copymemory(eepromser_0.get_data, @memory_temp, $80);
  end; // protection
  k054000_0 := k054000_chip.create;
  // DIP
  marcade.dswa := $FF;
  marcade.dswa_val := @vendetta_dip_a;
  marcade.dswb := $5E;
  marcade.dswb_val := @vendetta_dip_b;
  marcade.dswc := $FF;
  marcade.dswc_val := @vendetta_dip_c;
  // final
  reset_vendetta;
  start_vendetta := true;
end;

end.
