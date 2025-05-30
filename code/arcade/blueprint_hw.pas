unit blueprint_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_blueprint: boolean;

implementation

const
  blueprint_rom: array [0 .. 4] of tipo_roms = ((n: 'bp-1.1m'; l: $1000; p: 0; crc: $B20069A6), (n: 'bp-2.1n'; l: $1000; p: $1000; crc: $4A30302E), (n: 'bp-3.1p'; l: $1000; p: $2000; crc: $6866CA07), (n: 'bp-4.1r'; l: $1000; p: $3000; crc: $5D3CFAC3), (n: 'bp-5.1s'; l: $1000;
    p: $4000; crc: $A556CAC4));
  blueprint_sonido: array [0 .. 1] of tipo_roms = ((n: 'snd-1.3u'; l: $1000; p: 0; crc: $FD38777A), (n: 'snd-2.3v'; l: $1000; p: $2000; crc: $33D5BF5B));
  blueprint_char: array [0 .. 1] of tipo_roms = ((n: 'bg-1.3c'; l: $1000; p: 0; crc: $AC2A61BC), (n: 'bg-2.3d'; l: $1000; p: $1000; crc: $81FE85D7));
  blueprint_sprites: array [0 .. 2] of tipo_roms = ((n: 'red.17d'; l: $1000; p: 0; crc: $A73B6483), (n: 'blue.18d'; l: $1000; p: $1000; crc: $7D622550), (n: 'green.20d'; l: $1000; p: $2000; crc: $2FCB4F26));
  blueprint_dipa: array [0 .. 5] of def_dip2 = ((mask: $6; name: 'Bonus Life'; number: 4; val4: (0, 2, 4, 6); name4: ('20K', '30K', '40K', '60K')), (mask: $8; name: 'Free Play'; number: 2; val2: (0, 8); name2: ('Off', 'On')), (mask: $10; name: 'Maze Monster Appears In';
    number: 2; val2: (0, $10); name2: ('2nd Maze', '3rd Maze')), (mask: $20; name: 'Coin A'; number: 2; val2: ($20, 0); name2: ('2C 1C', '1C 1C')), (mask: $40; name: 'Coin B'; number: 2; val2: ($40, 0); name2: ('1C 3C', '1C 5C')), ());
  blueprint_dipb: array [0 .. 3] of def_dip2 = ((mask: $3; name: 'Lives'; number: 4; val4: (0, 1, 2, 3); name4: ('2', '3', '4', '5')), (mask: $8; name: 'Cabinet'; number: 2; val2: (0, 8); name2: ('Upright', 'Cocktail')), (mask: $30; name: 'Difficulty'; number: 4;
    val4: (0, $10, $20, $30); name4: ('Level 1', 'Level 2', 'Level 3', 'Level 4')), ());
  saturnzi_rom: array [0 .. 5] of tipo_roms = ((n: 'r1'; l: $1000; p: 0; crc: $18A6D68E), (n: 'r2'; l: $1000; p: $1000; crc: $A7DD2665), (n: 'r3'; l: $1000; p: $2000; crc: $B9CFA791), (n: 'r4'; l: $1000; p: $3000; crc: $C5A997E7), (n: 'r5'; l: $1000; p: $4000; crc: $43444D00),
    (n: 'r6'; l: $1000; p: $5000; crc: $4D4821F6));
  saturnzi_sonido: array [0 .. 1] of tipo_roms = ((n: 'r7'; l: $1000; p: 0; crc: $DD43E02F), (n: 'r8'; l: $1000; p: $2000; crc: $7F9D0877));
  saturnzi_char: array [0 .. 1] of tipo_roms = ((n: 'r10'; l: $1000; p: 0; crc: $35987D61), (n: 'r9'; l: $1000; p: $1000; crc: $CA6A7FDA));
  saturnzi_sprites: array [0 .. 2] of tipo_roms = ((n: 'r11'; l: $1000; p: 0; crc: $6E4E6E5D), (n: 'r12'; l: $1000; p: $1000; crc: $46FC049E), (n: 'r13'; l: $1000; p: $2000; crc: $8B3E8C32));
  saturnzi_dipa: array [0 .. 2] of def_dip2 = ((mask: $2; name: 'Cabinet'; number: 2; val2: (0, 2); name2: ('Upright', 'Cocktail')), (mask: $C0; name: 'Lives'; number: 4; val4: (0, $40, $80, $C0); name4: ('3', '4', '5', '6')), ());
  saturnzi_dipb: array [0 .. 2] of def_dip2 = ((mask: $2; name: 'Coinage'; number: 2; val2: (2, 0); name2: ('A 2C/1C B 1C/3C', 'A 1C/1C B 1C/6C')), (mask: $4; name: 'Demo Sounds'; number: 2; val2: (0, 4); name2: ('Off', 'On')), ());
  grasspin_rom: array [0 .. 4] of tipo_roms = ((n: 'prom_1.4b'; l: $1000; p: 0; crc: $6FD50509), (n: 'jaleco-2.4c'; l: $1000; p: $1000; crc: $CD319007), (n: 'jaleco-3.4d'; l: $1000; p: $2000; crc: $AC73CCC2), (n: 'jaleco-4.4f'; l: $1000; p: $3000; crc: $41F6279D),
    (n: 'jaleco-5.4h'; l: $1000; p: $4000; crc: $D20AEAD9));
  grasspin_sonido: array [0 .. 1] of tipo_roms = ((n: 'jaleco-6.4j'; l: $1000; p: 0; crc: $F58BF3B0), (n: 'jaleco-7.4l'; l: $1000; p: $2000; crc: $2D587653));
  grasspin_char: array [0 .. 1] of tipo_roms = ((n: 'jaleco-9.4p'; l: $1000; p: 0; crc: $BCCCA24C), (n: 'jaleco-8.3p'; l: $1000; p: $1000; crc: $9D6185CA));
  grasspin_sprites: array [0 .. 2] of tipo_roms = ((n: 'jaleco-10.5p'; l: $1000; p: 0; crc: $3A0765C6), (n: 'jaleco-11.6p'; l: $1000; p: $1000; crc: $CCCFBEB4), (n: 'jaleco-12.7p'; l: $1000; p: $2000; crc: $615B3299));
  grasspin_dipa: array [0 .. 2] of def_dip2 = ((mask: $60; name: 'Coinage'; number: 4; val4: (0, $40, $60, $20); name4: ('2C 1C', '2C 3C', '1C 1C', '1C 2C')), (mask: $80; name: 'Freeze'; number: 2; val2: (0, $80); name2: ('Off', 'On')), ());
  grasspin_dipb: array [0 .. 3] of def_dip2 = ((mask: $3; name: 'Lives'; number: 4; val4: (0, 3, 2, 1); name4: ('2', '3', '4', '5')), (mask: $20; name: 'Cabinet'; number: 2; val2: (0, $20); name2: ('Upright', 'Cocktail')), (mask: $40; name: 'Freeze'; number: 2; val2: (0, $40);
    name2: ('Off', 'On')), ());

var
  sound_latch, dipsw, gfx_bank: byte;
  read_dip: function: byte;

procedure update_video_blueprint;
var
  x, y, atrib, bank: byte;
  f, nchar, color: word;
  flipx, flipy: boolean;
  scroll_def: array [0 .. 31] of word;
begin
  for f := 0 to $3FF do
  begin
    atrib := memory[$F000 + f];
    bank := memory[$F000 + ((f - 32) and $3FF)] and $40;
    color := atrib and $7F;
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f shr 5);
      y := f and $1F;
      nchar := memory[$9000 + f];
      if (bank <> 0) then
        nchar := nchar + (gfx_bank * $100);
      put_gfx(x * 8, y * 8, nchar, color shl 2, 1, 0);
      if (atrib and $80) = 0 then
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8)
      else
        put_gfx_trans(x * 8, y * 8, nchar, color shl 2, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  for y := 0 to $1F do
    scroll_def[y] := memory[$A000 + ((30 - y) and $FF)];
  scroll__y_part2(1, 3, 8, @scroll_def);
  flipy := false;
  for f := 0 to $3F do
  begin
    x := 2 + memory[$B003 + (f * 4)];
    y := 240 - memory[$B000 + (f * 4)] - 1;
    nchar := memory[$B001 + (f * 4)];
    atrib := memory[$B002 + (f * 4)];
    flipx := (atrib and $40) <> 0;
    put_gfx_sprite(nchar, $200, flipx, flipy, 1);
    update_gfx_sprite(x, y, 3, 1);
    // Raaaaaaaaaaaaaro
    flipy := (atrib and $80) <> 0;
  end;
  scroll__y_part2(2, 3, 8, @scroll_def);
  update_final_piece(0, 16, 256, 224, 3);
end;

procedure events_blueprint;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // p2
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
  end;
end;

// needs pause
procedure blueprint_loop;
var
  f:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
  for f:=0 to 255 do begin
    events_blueprint;
    if f=239 then begin
      z80_0.change_irq(HOLD_LINE);
      update_video_blueprint;
    end;
    //Main CPU
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
    //Sound
    z80_1.run(frame_snd);
    frame_snd:=frame_snd+z80_1.tframes-z80_1.contador;
  end;
  video_sync;
  end;
end;

function blueprint_read_dip: byte;
begin
  blueprint_read_dip := dipsw;
end;

function grasspin_read_dip: byte;
begin
  grasspin_read_dip := (dipsw and $7F) or $80;
end;

function blueprint_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF, $A000 .. $A0FF, $B000 .. $B0FF:
      blueprint_getbyte := memory[direccion];
    $9000 .. $9FFF:
      blueprint_getbyte := memory[$9000 + (direccion and $3FF)];
    $C000:
      blueprint_getbyte := marcade.in0;
    $C001:
      blueprint_getbyte := marcade.in1;
    $C003:
      blueprint_getbyte := read_dip;
    $E000:
      ; // WD
    $F000 .. $FFFF:
      blueprint_getbyte := memory[$F000 + (direccion and $3FF)];
  end;
end;

procedure blueprint_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF, $A000 .. $A0FF, $B000 .. $B0FF:
      memory[direccion] := valor;
    $9000 .. $9FFF:
      if memory[$9000 + (direccion and $3FF)] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[$9000 + (direccion and $3FF)] := valor;
      end;
    $D000:
      begin
        sound_latch := valor;
        z80_1.change_nmi(PULSE_LINE);
      end;
    $E000:
      begin
        main_screen.flip_main_screen := (valor and 2) = 0;
        if (gfx_bank <> ((valor and $4) shr 2)) then
        begin
          gfx_bank := (valor and $4) shr 2;
          fillchar(gfx[0].buffer, $400, 1);
        end;
      end;
    $F000 .. $FFFF:
      if memory[$F000 + (direccion and $3FF)] <> valor then
      begin
        memory[$F000 + (direccion and $3FF)] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
        direccion := (direccion and $3FF) - 32;
        gfx[0].buffer[direccion and $3FF] := true;
        direccion := direccion + 64;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      snd_getbyte := mem_snd[direccion and $FFF];
    $2000 .. $3FFF:
      snd_getbyte := mem_snd[$2000 + (direccion and $FFF)];
    $4000 .. $43FF:
      snd_getbyte := mem_snd[direccion];
    $6002:
      snd_getbyte := ay8910_0.read;
    $8002:
      snd_getbyte := ay8910_1.read;
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
    $6000:
      ay8910_0.control(valor);
    $6001:
      ay8910_0.write(valor);
    $8000:
      ay8910_1.control(valor);
    $8001:
      ay8910_1.write(valor);
  end;
end;

function ay0_portb_read: byte;
begin
  ay0_portb_read := sound_latch;
end;

procedure ay0_porta_write(valor: byte);
begin
  dipsw := valor;
end;

function ay1_porta_read: byte;
begin
  ay1_porta_read := marcade.dswa;
end;

function ay1_portb_read: byte;
begin
  ay1_portb_read := marcade.dswb;
end;

procedure blueprint_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

procedure blueprint_update_sound;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

// Main
procedure blueprint_reset;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
frame_main:=z80_0.tframes;
frame_snd:=z80_1.tframes;
  sound_latch := 0;
  marcade.in0 := 0;
  marcade.in1 := 0;
  gfx_bank := 0;
  dipsw := 0;
end;

function start_blueprint: boolean;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  memory_temp: array [0 .. $2FFF] of byte;
  colores: tpaleta;
  pen, bit0, bit1: byte;
  f: word;
  procedure convert_chars(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(2, 0, 8 * 8, $1000 * 8, 0);
    convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  end;
  procedure convert_sprites(num: word);
  begin
    init_gfx(1, 8, 16, num);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(3, 0, 16 * 8, $1000 * 8 * 2, $1000 * 8 * 1, 0);
    convert_gfx(1, 0, @memory_temp, @pc_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := blueprint_loop;
  machine_calls.reset := blueprint_reset;
  start_blueprint := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  screen_init(3, 256, 256, false, true);
  main_screen.rot270_screen := true;
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(7000000 div 2, 256);
  z80_0.change_ram_calls(blueprint_getbyte, blueprint_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(10000000 div 8, 256);
  z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_1.init_sound(blueprint_update_sound);
  timers.init(z80_1.numero_cpu, 1250000 / (4 * 60), blueprint_snd_irq, nil, true);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(1250000, AY8910);
  ay8910_0.change_io_calls(nil, ay0_portb_read, ay0_porta_write, nil);
  ay8910_1 := ay8910_chip.create(625000, AY8910);
  ay8910_1.change_io_calls(ay1_porta_read, ay1_portb_read, nil, nil);
  read_dip := blueprint_read_dip;
  case main_vars.machine_type of
    377:
      begin // Blue Print
        if not(roms_load(@memory, blueprint_rom)) then
          exit;
        if not(roms_load(@mem_snd, blueprint_sonido)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, blueprint_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, blueprint_sprites)) then
          exit;
        convert_sprites($100);
        // DIP
        marcade.dswa := $C3;
        marcade.dswa_val2 := @blueprint_dipa;
        marcade.dswb := $D5;
        marcade.dswb_val2 := @blueprint_dipb;
      end;
    378:
      begin // Saturn
        if not(roms_load(@memory, saturnzi_rom)) then
          exit;
        if not(roms_load(@mem_snd, saturnzi_sonido)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, saturnzi_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, saturnzi_sprites)) then
          exit;
        convert_sprites($100);
        // DIP
        marcade.dswa := $3D;
        marcade.dswa_val2 := @saturnzi_dipa;
        marcade.dswb := $FD;
        marcade.dswb_val2 := @saturnzi_dipb;
      end;
    379:
      begin // Grasspin
        read_dip := grasspin_read_dip;
        if not(roms_load(@memory, grasspin_rom)) then
          exit;
        if not(roms_load(@mem_snd, grasspin_sonido)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, grasspin_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, grasspin_sprites)) then
          exit;
        convert_sprites($100);
        // DIP
        marcade.dswa := $7F;
        marcade.dswa_val2 := @grasspin_dipa;
        marcade.dswb := $9F;
        marcade.dswb_val2 := @grasspin_dipb;
      end;
  end;
  // Palette
  for f := 0 to $207 do
  begin
    if (f < $200) then
    begin // characters
      if (f and 2) <> 0 then
        bit0 := ((f and $0E0) shr 5)
      else
        bit0 := 0;
      if (f and 1) <> 0 then
        bit1 := ((f and $01C) shr 2)
      else
        bit1 := 0;
      pen := ((f and $100) shr 5) or bit0 or bit1;
    end
    else
      pen := f - $200; // sprites
    if (pen and 8) <> 0 then
    begin
      colores[f].r := ((pen shr 0) and 1) * $BF;
      colores[f].g := ((pen shr 2) and 1) * $BF;
      colores[f].b := ((pen shr 1) and 1) * $BF;
    end
    else
    begin
      colores[f].r := ((pen shr 0) and 1) * $FF;
      colores[f].g := ((pen shr 2) and 1) * $FF;
      colores[f].b := ((pen shr 1) and 1) * $FF;
    end;
  end;
  set_pal(colores, $208);
  // final
  start_blueprint := true;
end;

end.
