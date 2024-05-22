unit aliens_hw;

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
  k051960,
  k007232;

function start_aliens: boolean;

implementation

const
  // aliens
  aliens_rom: array [0 .. 1] of tipo_roms = ((n: '875_j01.c24'; l: $20000; p: 0; crc: $6A529CD6),
    (n: '875_j02.e24'; l: $10000; p: $20000; crc: $56C20971));
  aliens_sound: tipo_roms = (n: '875_b03.g04'; l: $8000; p: 0; crc: $1AC4D283);
  aliens_tiles: array [0 .. 3] of tipo_roms = ((n: '875b11.k13'; l: $80000; p: 0; crc: $89C5C885),
    (n: '875b12.k19'; l: $80000; p: 2; crc: $EA6BDC17), (n: '875b07.j13'; l: $40000; p: $100000;
    crc: $E9C56D66), (n: '875b08.j19'; l: $40000; p: $100002; crc: $F9387966));
  aliens_sprites: array [0 .. 3] of tipo_roms = ((n: '875b10.k08'; l: $80000; p: 0; crc: $0B1035B1),
    (n: '875b09.k02'; l: $80000; p: 2; crc: $E76B3C19), (n: '875b06.j08'; l: $40000; p: $100000;
    crc: $081A0566), (n: '875b05.j02'; l: $40000; p: $100002; crc: $19A261F2));
  aliens_k007232: tipo_roms = (n: '875b04.e05'; l: $40000; p: 0; crc: $4E209AC8);
  // DIP
  aliens_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
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
  aliens_dip_b: array [0 .. 3] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '1'), (dip_val: $2; dip_name: '2'), (dip_val: $1;
    dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $60; name: 'Difficulty'; number: 4; dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40;
    dip_name: 'Normal'), (dip_val: $20; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  aliens_dip_c: array [0 .. 1] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  layer_colorbase: array [0 .. 2] of byte = (0, 4, 8);

var
  tiles_rom, sprite_rom, k007232_rom: pbyte;
  sound_latch, bank0_bank, rom_bank1: byte;
  rom_bank: array [0 .. 19, 0 .. $1FFF] of byte;
  ram_bank: array [0 .. 1, 0 .. $3FF] of byte;

procedure aliens_cb(layer, bank: word; var code: dword; var color: word; var flags: word; var priority: word);
begin
  code := code or (((color and $3F) shl 8) or (bank shl 14));
  color := layer_colorbase[layer] + ((color and $C0) shr 6);
end;

procedure aliens_sprite_cb(var code: word; var color: word; var pri: word; var shadow: word);
begin
  // The PROM allows for mixed priorities, where sprites would have
  // priority over text but not on one or both of the other two planes.
  case (color and $70) of
    $20, $60:
      pri := 7; // over -, not ABF
    $0:
      pri := 4; // over AB, not F
    $40:
      pri := 6; // over A, not BF
    $10:
      pri := 0; // over ABF
    // No posibles debido a como pinta la pantalla el driver!!
    $50:
      pri := 2; // over AF, not B
    $30, $70:
      pri := 3; // over F, not AB
  end;
  code := code or ((color and $80) shl 6);
  color := 16 + (color and $F);
  shadow := 0; // shadows are not used by this game
end;

procedure aliens_k007232_cb(valor: byte);
begin
  k007232_0.set_volume(0, (valor and $F) * $11, 0);
  k007232_0.set_volume(1, 0, (valor shr 4) * $11);
end;

procedure aliens_k051960_cb(state: byte);
begin
  konami_0.change_irq(state);
end;

procedure update_video_aliens;
begin
  k052109_0.draw_tiles;
  k051960_0.update_sprites;
  fill_full_screen(4, layer_colorbase[1] * 16);
  k051960_0.draw_sprites(7, -1);
  k052109_0.draw_layer(1, 4); // A
  k051960_0.draw_sprites(6, -1);
  k052109_0.draw_layer(2, 4); // B
  k051960_0.draw_sprites(4, -1);
  k052109_0.draw_layer(0, 4); // F
  k051960_0.draw_sprites(0, -1);
  actualiza_trozo_final(112, 16, 288, 224, 4);
end;

procedure events_aliens;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
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
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
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
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure aliens_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := konami_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
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
          update_video_aliens;
        k051960_0.update_line(f);
      end;
      events_aliens;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function aliens_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FF:
      aliens_getbyte := ram_bank[bank0_bank, direccion];
    $400 .. $1FFF, $8000 .. $FFFF:
      aliens_getbyte := memory[direccion];
    $2000 .. $3FFF:
      aliens_getbyte := rom_bank[rom_bank1, direccion and $1FFF];
    $4000 .. $7FFF:
      case direccion of
        $5F80:
          aliens_getbyte := marcade.dswc; // DSW3
        $5F81:
          aliens_getbyte := marcade.in0; // p1
        $5F82:
          aliens_getbyte := marcade.in1; // p2
        $5F83:
          aliens_getbyte := marcade.dswb; // dsw2
        $5F84:
          aliens_getbyte := marcade.dswa; // dsw1
      else
        begin
          direccion := direccion and $3FFF;
          if k052109_0.get_rmrd_line = CLEAR_LINE then
          begin
            if ((direccion >= $3800) and (direccion < $3808)) then
              aliens_getbyte := k051960_0.k051937_read(direccion - $3800)
            else if (direccion < $3C00) then
              aliens_getbyte := k052109_0.read(direccion)
            else
              aliens_getbyte := k051960_0.read(direccion - $3C00);
          end
          else
            aliens_getbyte := k052109_0.read(direccion and $3FFF);
        end;
      end;
  end;
end;

procedure aliens_putbyte(direccion: word; valor: byte);

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
    0 .. $3FF:
      begin
        ram_bank[bank0_bank, direccion] := valor;
        if bank0_bank = 1 then
        begin
          if buffer_paleta[direccion] <> valor then
          begin
            buffer_paleta[direccion] := valor;
            change_color(direccion shr 1);
          end;
        end;
      end;
    $400 .. $1FFF:
      memory[direccion] := valor;
    $2000 .. $3FFF:
      ;
    $4000 .. $7FFF:
      case direccion of
        $5F88:
          begin
            bank0_bank := (valor and $20) shr 5;
            if (valor and $40) <> 0 then
              k052109_0.set_rmrd_line(ASSERT_LINE)
            else
              k052109_0.set_rmrd_line(CLEAR_LINE);
          end;
        $5F8C:
          begin
            sound_latch := valor;
            z80_0.change_irq(HOLD_LINE);
          end;
      else
        begin
          direccion := direccion and $3FFF;
          case direccion of
            0 .. $37FF, $3808 .. $3BFF:
              k052109_0.write(direccion, valor);
            $3800 .. $3807:
              k051960_0.k051937_write(direccion - $3800, valor);
            $3C00 .. $3FFF:
              k051960_0.write(direccion - $3C00, valor);
          end;
        end;
      end;
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

procedure aliens_bank(valor: byte);
begin
  rom_bank1 := valor and $1F;
end;

function aliens_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      aliens_snd_getbyte := mem_snd[direccion];
    $A001:
      aliens_snd_getbyte := ym2151_0.status;
    $C000:
      aliens_snd_getbyte := sound_latch;
    $E000 .. $E00D:
      aliens_snd_getbyte := k007232_0.read(direccion and $F);
  end;
end;

procedure aliens_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $A000:
      ym2151_0.reg(valor);
    $A001:
      ym2151_0.write(valor);
    $E000 .. $E00D:
      k007232_0.write(direccion and $F, valor);
  end;
end;

procedure aliens_snd_bankswitch(valor: byte);
begin
  // b1: bank for chanel A */
  // b0: bank for chanel B */
  k007232_0.set_bank((valor shr 1) and 1, valor and 1);
end;

procedure aliens_sound_update;
begin
  ym2151_0.update;
  k007232_0.update;
end;

// Main
procedure reset_aliens;
begin
  konami_0.reset;
  z80_0.reset;
  k052109_0.reset;
  ym2151_0.reset;
  k051960_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  sound_latch := 0;
  bank0_bank := 0;
  rom_bank1 := 0;
end;

procedure close_aliens;
begin
  if k007232_rom <> nil then
    freemem(k007232_rom);
  if sprite_rom <> nil then
    freemem(sprite_rom);
  if tiles_rom <> nil then
    freemem(tiles_rom);
  k007232_rom := nil;
  sprite_rom := nil;
  tiles_rom := nil;
end;

function start_aliens: boolean;
var
  temp_mem: array [0 .. $2FFFF] of byte;
  f: byte;
begin
  start_aliens := false;
  machine_calls.close := close_aliens;
  machine_calls.reset := reset_aliens;
  machine_calls.general_loop := aliens_loop;
  machine_calls.fps_max := 59.185606;
  // Pantallas para el K052109
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 1024, 1024, false, true);
  // start_video(288, 224, true);
  start_video(1426, 1074, true);
  start_audio(false);
  // cargar roms y ponerlas en su sitio...
  if not(roms_load(@temp_mem, aliens_rom)) then
    exit;
  copymemory(@memory[$8000], @temp_mem[$28000], $8000);
  for f := 0 to 19 do
    copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
  // cargar sonido
  if not(roms_load(@mem_snd, aliens_sound)) then
    exit;
  // Main CPU
  konami_0 := cpu_konami.create(3000000, 256);
  konami_0.change_ram_calls(aliens_getbyte, aliens_putbyte);
  konami_0.change_set_lines(aliens_bank);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(aliens_snd_getbyte, aliens_snd_putbyte);
  z80_0.init_sound(aliens_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  ym2151_0.change_port_func(aliens_snd_bankswitch);
  getmem(k007232_rom, $40000);
  if not(roms_load(k007232_rom, aliens_k007232)) then
    exit;
  k007232_0 := k007232_chip.create(3579545, k007232_rom, $40000, 0.20, aliens_k007232_cb);
  // Iniciar video
  getmem(tiles_rom, $200000);
  if not(roms_load32b(tiles_rom, aliens_tiles)) then
    exit;
  k052109_0 := k052109_chip.create(1, 2, 3, 0, aliens_cb, tiles_rom, $200000);
  getmem(sprite_rom, $200000);
  if not(roms_load32b(sprite_rom, aliens_sprites)) then
    exit;
  k051960_0 := k051960_chip.create(4, 1, sprite_rom, $200000, aliens_sprite_cb, 2);
  k051960_0.change_irqs(aliens_k051960_cb, nil, nil);
  // DIP
  marcade.dswa := $FF;
  marcade.dswa_val := @aliens_dip_a;
  marcade.dswb := $5E;
  marcade.dswb_val := @aliens_dip_b;
  marcade.dswc := $FF;
  marcade.dswc_val := @aliens_dip_c;
  // final
  reset_aliens;
  start_aliens := true;
end;

end.
