unit knucklejoe_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m680x,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  timer_engine,
  sn_76496,
  rom_engine,
  pal_engine,
  sound_engine;

function start_knucklejoe: boolean;

implementation

const
  knjoe_rom: array [0 .. 2] of tipo_roms = ((n: 'kj-1.bin'; l: $4000; p: 0; crc: $4E4F5FF2), (n: 'kj-2.bin'; l: $4000; p: $4000; crc: $CB11514B), (n: 'kj-3.bin'; l: $4000; p: $8000; crc: $0F50697B));
  knjoe_pal: array [0 .. 4] of tipo_roms = ((n: 'kjclr1.bin'; l: $100; p: 0; crc: $C3378AC2), (n: 'kjclr2.bin'; l: $100; p: $100; crc: $2126DA97), (n: 'kjclr3.bin'; l: $100; p: $200; crc: $FDE62164), (n: 'kjprom5.bin'; l: $20; p: $300; crc: $5A81DD9F), (n: 'kjprom4.bin'; l: $100;
    p: $320; crc: $48DC2066));
  knjoe_sprites: array [0 .. 2] of tipo_roms = ((n: 'kj-4.bin'; l: $8000; p: 0; crc: $A499EA10), (n: 'kj-6.bin'; l: $8000; p: $8000; crc: $815F5C0A), (n: 'kj-5.bin'; l: $8000; p: $10000; crc: $11111759));
  knjoe_sprites2: array [0 .. 2] of tipo_roms = ((n: 'kj-7.bin'; l: $4000; p: 0; crc: $121FCCCB), (n: 'kj-9.bin'; l: $4000; p: $4000; crc: $AFFBE3EB), (n: 'kj-8.bin'; l: $4000; p: $8000; crc: $E057E72A));
  knjoe_tiles: array [0 .. 2] of tipo_roms = ((n: 'kj-10.bin'; l: $4000; p: 0; crc: $74D3BA33), (n: 'kj-11.bin'; l: $4000; p: $4000; crc: $8EA01455), (n: 'kj-12.bin'; l: $4000; p: $8000; crc: $33367C41));
  knjoe_sound: tipo_roms = (n: 'kj-13.bin'; l: $2000; p: $6000; crc: $0A0BE3F5);
  // Dip
  knjoe_dip_a: array [0 .. 4] of def_dip2 = ((mask: 7; name: 'Coin A'; number: 8; val8: (0, 4, 2, 6, 7, 3, 5, 1); name8: ('5C 1C', '4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 5C')), (mask: $18; name: 'Coin B'; number: 4; val4: (0, $10, $18, 8);
    name4: ('3C 1C', '2C 1C', '1C 1C', '1C 2C')), (mask: $20; name: 'Infinite Energy'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $40; name: 'Free Play (not working)'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  knjoe_dip_b: array [0 .. 5] of def_dip2 = ((mask: 2; name: 'Cabinet'; number: 2; val2: (2, 0); name2: ('Upright', 'Cocktail')), (mask: 4; name: 'Lives'; number: 2; val2: (4, 0); name2: ('3', '5')), (mask: $18; name: 'Bonus Life'; number: 4; val4: ($18, $10, 8, 0);
    name4: ('10K 20K+', '20K 40K+', '30K 60K+', '40K 80K+')), (mask: $60; name: 'Difficulty'; number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: $80; name: 'Demo Sound'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  sound_command, val_port1, val_port2, tile_bank, sprite_bank: byte;
  scroll_x: word;

procedure update_video_knjoe;
const
  pribase: array [0 .. 3] of word = ($E980, $E880, $E900, $E800);
var
  f, nchar, y, x, offs: word;
  i, atrib, color: byte;
begin
  // Background
  for f := 0 to $7FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := 31 - (f div 64);
      atrib := memory[$C001 + (f * 2)];
      color := (atrib and $F) shl 3;
      nchar := memory[$C000 + (f * 2)] + ((atrib and $C0) shl 2) + (tile_bank shl 6);
      put_gfx_flip(x * 8, y * 8, nchar and $7FF, color, 1, 0, (atrib and $20) <> 0, (atrib and $10) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x(1, 2, scroll_x);
  // Sprites
  for i := 0 to 3 do
  begin
    for f := $1F downto 0 do
    begin
      offs := pribase[i] + (f * 4);
      y := memory[offs] + 1;
      x := memory[offs + 3];
      atrib := memory[offs + 1];
      nchar := (memory[offs + 2] + ((atrib and $10) shl 5) + ((atrib and $20) shl 3));
      color := (atrib and $F) shl 3;
      put_gfx_sprite(nchar, color, (atrib and $40) <> 0, (atrib and $80) = 0, sprite_bank);
      update_gfx_sprite(x, y, 2, sprite_bank);
    end;
  end;
  // Devolver la parte de arriba!
  update_region(0, 0, 256, 64, 1, 0, 0, 256, 64, 2);
  update_final_piece(8, 0, 240, 256, 2);
end;

procedure events_knjoe;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
  end;
end;

procedure knjoe_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // snd
        m6800_0.run(frame_snd);
        frame_snd := frame_snd + m6800_0.tframes - m6800_0.contador;
      end;
      z80_0.change_irq(HOLD_LINE);
      update_video_knjoe;
      events_knjoe;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function knjoe_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $CFFF, $E800 .. $FFFF:
      knjoe_getbyte := memory[direccion];
    $D800:
      knjoe_getbyte := marcade.in0;
    $D801:
      knjoe_getbyte := marcade.in1;
    $D802:
      knjoe_getbyte := marcade.in2;
    $D803:
      knjoe_getbyte := marcade.dswa;
    $D804:
      knjoe_getbyte := marcade.dswb;
  end;
end;

procedure knjoe_putbyte(direccion: word; valor: byte);
var
  tempb: byte;
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $CFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $D000:
      scroll_x := (scroll_x and $FF00) or valor;
    $D001:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $D800:
      if ((valor and $80) = 0) then
        sound_command := valor and $7F
      else
        m6800_0.change_irq(ASSERT_LINE);
    $D801:
      begin
        tempb := valor and $10;
        if tile_bank <> tempb then
        begin
          tile_bank := tempb;
          fillchar(gfx[0].buffer, $800, 1);
        end;
        tempb := 1 + ((valor and 4) shr 2);
        if sprite_bank <> tempb then
        begin
          sprite_bank := tempb;
          fillchar(memory[$F100], $180, 0);
        end;
        main_screen.flip_main_screen := (valor and 1) <> 0;
      end;
    $D802:
      sn_76496_0.Write(valor);
    $D803:
      sn_76496_1.Write(valor);
    $E800 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

// sonido
function snd_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    $2000 .. $7FFF:
      snd_getbyte := mem_snd[direccion];
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    $1000 .. $1FFF:
      m6800_0.change_irq(CLEAR_LINE);
    $2000 .. $7FFF:
      ;
  end;
end;

procedure out_port1(valor: byte);
begin
  val_port1 := valor;
end;

procedure out_port2(valor: byte);
begin
  if (((val_port2 and 1) <> 0) and ((not(valor and 1)) <> 0)) then
  begin
    // control or data port?
    if (val_port2 and 4) <> 0 then
    begin
      if (val_port2 and 8) <> 0 then
        ay8910_0.control(val_port1);
    end
    else
    begin
      if (val_port2 and 8) <> 0 then
        ay8910_0.Write(val_port1);
    end;
  end;
  val_port2 := valor;
end;

function in_port1: byte;
begin
  if (val_port2 and 8) <> 0 then
    in_port1 := ay8910_0.read
  else
    in_port1 := $FF;
end;

function in_port2: byte;
begin
  in_port2 := 0;
end;

function ay0_porta_r: byte;
begin
  ay0_porta_r := sound_command;
end;

procedure knjoe_sound_update;
begin
  ay8910_0.update;
  sn_76496_0.update;
  sn_76496_1.update;
end;

procedure knjoe_snd_nmi;
begin
  m6800_0.change_nmi(PULSE_LINE);
end;

// Main
procedure reset_knjoe;
begin
  z80_0.reset;
  m6800_0.reset;
  frame_main := z80_0.tframes;
  frame_snd := m6800_0.tframes;
  ay8910_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  reset_game_general;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_command := 0;
  val_port1 := 0;
  val_port2 := 0;
  tile_bank := 0;
  sprite_bank := 1;
  scroll_x := 0;
end;

function start_knucklejoe: boolean;
var
  f: word;
  colores: tpaleta;
  ctemp1, ctemp2, ctemp3: byte;
  memory_temp: array [0 .. $17FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
  pc_y: array [0 .. 7] of dword = (7 * 8, 6 * 8, 5 * 8, 4 * 8, 3 * 8, 2 * 8, 1 * 8, 0 * 8);
begin
  start_knucklejoe := false;
  machine_calls.general_loop := knjoe_loop;
  machine_calls.reset := reset_knjoe;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 512, 256, false, true);
  screen_mod_sprites(2, 256, 0, $FF, 0);
  start_video(240, 256);
  // Main CPU
  z80_0 := cpu_z80.create(7000000, 256);
  z80_0.change_ram_calls(knjoe_getbyte, knjoe_putbyte);
  if not(roms_load(@memory, knjoe_rom)) then
    exit;
  // Sound CPU
  m6800_0 := cpu_m6800.create(3579545, 256, TCPU_M6803);
  m6800_0.change_ram_calls(snd_getbyte, snd_putbyte);
  m6800_0.change_io_calls(in_port1, in_port2, nil, nil, out_port1, out_port2, nil, nil);
  m6800_0.init_sound(knjoe_sound_update);
  if not(roms_load(@mem_snd, knjoe_sound)) then
    exit;
  // sound chips
  ay8910_0 := ay8910_chip.create(3579545 div 4, AY8910);
  ay8910_0.change_io_calls(ay0_porta_r, nil, nil, nil);
  sn_76496_0 := sn76496_chip.create(3579545);
  sn_76496_1 := sn76496_chip.create(3579545);
  // Timers (Se divide por cuatro, por que internamente el M6800 va dividido!!!
  timers.init(m6800_0.numero_cpu, 3579545 / 4 / 3970, knjoe_snd_nmi, nil, true);
  // convertir tiles
  if not(roms_load(@memory_temp, knjoe_tiles)) then
    exit;
  init_gfx(0, 8, 8, $800);
  gfx_set_desc_data(3, 0, 8 * 8, 2 * $800 * 8 * 8, $800 * 8 * 8, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, knjoe_sprites)) then
    exit;
  init_gfx(1, 16, 16, $400);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(3, 0, 32 * 8, 2 * $400 * 32 * 8, $400 * 32 * 8, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // convertir sprites 2
  if not(roms_load(@memory_temp, knjoe_sprites2)) then
    exit;
  init_gfx(2, 16, 16, $200);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(3, 0, 32 * 8, 2 * $200 * 32 * 8, $200 * 32 * 8, 0);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, knjoe_pal)) then
    exit;
  for f := 0 to $7F do
  begin
    colores[f].r := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
    colores[f].g := ((memory_temp[f + $100] and $F) shl 4) or (memory_temp[f + $100] and $F);
    colores[f].b := ((memory_temp[f + $200] and $F) shl 4) or (memory_temp[f + $200] and $F);
  end;
  for f := 0 to $F do
  begin
    // sprites
    ctemp1 := 0;
    ctemp2 := (memory_temp[f + $300] shr 6) and 1;
    ctemp3 := (memory_temp[f + $300] shr 7) and 1;
    colores[$80 + f].r := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
    ctemp1 := (memory_temp[f + $300] shr 3) and 1;
    ctemp2 := (memory_temp[f + $300] shr 4) and 1;
    ctemp3 := (memory_temp[f + $300] shr 5) and 1;
    colores[$80 + f].g := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
    ctemp1 := (memory_temp[f + $300] shr 0) and 1;
    ctemp2 := (memory_temp[f + $300] shr 1) and 1;
    ctemp3 := (memory_temp[f + $300] shr 2) and 1;
    colores[$80 + f].b := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
  end;
  set_pal(colores, $100);
  // CLUT
  for f := 0 to $7F do
  begin
    gfx[1].colores[f] := (memory_temp[f + $320] and $F) + $80;
    gfx[2].colores[f] := (memory_temp[f + $320] and $F) + $80;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $7F;
  marcade.dswa_val2 := @knjoe_dip_a;
  marcade.dswb_val2 := @knjoe_dip_b;
  // final
  reset_knjoe;
  start_knucklejoe := true;
end;

end.
