unit pinballaction_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_pinballaction: boolean;

const
  pinballaction_rom: array [0 .. 2] of tipo_roms = ((n: 'b-p7.bin'; l: $4000; p: 0; crc: $8D6DCAAE), (n: 'b-n7.bin'; l: $4000; p: $4000; crc: $D54D5402), (n: 'b-l7.bin'; l: $2000; p: $8000; crc: $E7412D68));
  pinballaction_sound: tipo_roms = (n: 'a-e3.bin'; l: $2000; p: 0; crc: $0E53A91F);
  pinballaction_chars: array [0 .. 2] of tipo_roms = ((n: 'a-s6.bin'; l: $2000; p: 0; crc: $9A74A8E1), (n: 'a-s7.bin'; l: $2000; p: $2000; crc: $5CA6AD3C), (n: 'a-s8.bin'; l: $2000; p: $4000; crc: $9F00B757));
  pinballaction_sprites: array [0 .. 2] of tipo_roms = ((n: 'b-c7.bin'; l: $2000; p: 0; crc: $D1795EF5), (n: 'b-d7.bin'; l: $2000; p: $2000; crc: $F28DF203), (n: 'b-f7.bin'; l: $2000; p: $4000; crc: $AF6E9817));
  pinballaction_tiles: array [0 .. 3] of tipo_roms = ((n: 'a-j5.bin'; l: $4000; p: 0; crc: $21EFE866), (n: 'a-j6.bin'; l: $4000; p: $4000; crc: $7F984C80), (n: 'a-j7.bin'; l: $4000; p: $8000; crc: $DF69E51B), (n: 'a-j8.bin'; l: $4000; p: $C000; crc: $0094CB8B));
  // DIP
  pinballaction_dipa: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin B'; number: 4; val4: (0, 1, 2, 3); name4: ('1C 1C', '1C 2C', '1C 3C', '1C 6C')), (mask: $C; name: 'Coin A'; number: 4; val4: (4, 0, 8, $C); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $30;
    name: 'Lives'; number: 4; val4: ($30, 0, $10, $20); name4: ('2', '3', '4', '5')), (mask: $40; name: 'Cabinet'; number: 2; val2: ($40, 0); name2: ('Upright', 'Cocktail')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  pinballaction_dipb: array [0 .. 4] of def_dip2 = ((mask: 7; name: 'Bonus Life'; number: 8; val8: (1, 4, 0, 3, 6, 2, 5, 7); name8: ('70K 200K 1000K', '100K 300K 1000K', '70K 200K', '100K 300K', '200K 1000K', '100K', '200K', 'None')), (mask: 8; name: 'Extra'; number: 2;
    val2: (8, 0); name2: ('Hard', 'Easy')), (mask: $30; name: 'Flippers'; number: 4; val4: (0, $10, $20, $30); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: $C0; name: 'Difficulty (Outlanes)'; number: 4; val4: (0, $40, $80, $C0);
    name4: ('Easy', 'Medium', 'Hard', 'Hardest')), ());

var
  sound_latch, scroll_y: byte;
  nmi_mask: boolean;

implementation

procedure update_video_pinballaction;
var
  f, color, nchar, x, y: word;
  atrib, atrib2: byte;
begin
  // background
  for f := 0 to $3FF do
  begin
    atrib := memory[$DC00 + f];
    color := atrib and 7;
    if (gfx[1].buffer[f] or buffer_color[color + $10]) then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      nchar := memory[$D800 + f] + $10 * (atrib and $70);
      put_gfx_flip(x * 8, y * 8, nchar, (color shl 4) + 128, 1, 1, (atrib and $80) <> 0, false);
      gfx[1].buffer[f] := false;
    end;
    // chars
    atrib := memory[$D400 + f];
    color := atrib and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      nchar := memory[$D000 + f] + $10 * (atrib and $30);
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 3, 2, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__y(1, 3, scroll_y);
  // Sprites
  for f := $1F downto 0 do
  begin
    // Si el siguiente es doble no lo pongo
    if ((f > 0) and ((memory[($E000 + (f * 4)) - 4] and $80) <> 0)) then
      continue;
    atrib := memory[$E000 + (f * 4)];
    atrib2 := memory[$E001 + (f * 4)];
    y := memory[$E003 + (f * 4)];
    x := memory[$E002 + (f * 4)];
    color := (atrib2 and $F) shl 3;
    if (atrib and $80) <> 0 then
    begin
      nchar := 3;
      atrib := atrib and $1F;
    end
    else
    begin
      nchar := 2;
    end;
    put_gfx_sprite(atrib, color, (atrib2 and $40) <> 0, (atrib2 and $80) <> 0, nchar);
    update_gfx_sprite(x, y, 3, nchar);
  end;
  scroll__y(2, 3, scroll_y);
  update_final_piece(16, 0, 224, 256, 3);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_pinballaction;
begin
  if event.arcade then
  begin
    // Player 1

    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.but3[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // Player 2

    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.but3[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure pinballaction_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
 for f:=0 to $ff do begin
    events_pinballaction;
    if f=240 then begin
      if nmi_mask then z80_0.change_nmi(PULSE_LINE);
      update_video_pinballaction;
    end;
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
    z80_1.run(frame_snd);
    frame_snd:=frame_snd+z80_1.tframes-z80_1.contador;
 end;
 video_sync;
    end
    else
      pause_action;
  end;
end;

function pinballaction_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $E07F:
      pinballaction_getbyte := memory[direccion];
    $E400 .. $E5FF:
      pinballaction_getbyte := buffer_paleta[direccion and $1FF];
    $E600:
      pinballaction_getbyte := marcade.in0; // p1
    $E601:
      pinballaction_getbyte := marcade.in1; // p2
    $E602:
      pinballaction_getbyte := marcade.in2; // system
    $E604:
      pinballaction_getbyte := marcade.dswa;
    $E605:
      pinballaction_getbyte := marcade.dswb;
  end;
end;

procedure pinballaction_putbyte(direccion: word; valor: byte);
  procedure change_color(dir: word); inline;
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[dir + 1];
    color.b := pal4bit(tmp_color);
    tmp_color := buffer_paleta[dir];
    color.g := pal4bit(tmp_color shr 4);
    color.r := pal4bit(tmp_color);
    dir := dir shr 1;
    set_pal_color(color, dir);
    case dir of
      0 .. 127:
        buffer_color[dir shr 3] := true;
      128 .. 255:
        buffer_color[((dir shr 4) and 7) + $10] := true;
    end;
  end;

begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $CFFF, $E000 .. $E07F:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin // chars
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin // tiles
        memory[direccion] := valor;
        gfx[1].buffer[direccion and $3FF] := true;
      end;
    $E400 .. $E5FF:
      if buffer_paleta[direccion and $1FF] <> valor then
      begin
        buffer_paleta[direccion and $1FF] := valor;
        change_color(direccion and $1FE);
      end;
    $E600:
      nmi_mask := (valor and 1) <> 0;
    $E604:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $E606:
      scroll_y := valor - 3;
    $E800:
      begin
        sound_latch := valor;
            z80_1.change_irq_vector(HOLD_LINE,0);
      end;
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $47FF:
      snd_getbyte := mem_snd[direccion];
    $8000:
      snd_getbyte := sound_latch;
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ;
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
  end;
end;

procedure snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $10:
      ay8910_0.Control(valor);
    $11:
      ay8910_0.Write(valor);
    $20:
      ay8910_1.Control(valor);
    $21:
      ay8910_1.Write(valor);
    $30:
      ay8910_2.Control(valor);
    $31:
      ay8910_2.Write(valor);
  end;
end;

procedure pbaction_sound_irq;
begin
  z80_1.change_irq_vector(HOLD_LINE,2);
end;

procedure pinballaction_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
  ay8910_2.update;
end;

// Main
procedure reset_pinballaction;
begin
  z80_0.reset;
  z80_1.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  ay8910_0.reset;
  ay8910_1.reset;
  ay8910_2.reset;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  scroll_y := 0;
  sound_latch := 0;
  nmi_mask := false;
end;

function start_pinballaction: boolean;
const
  psd_x: array [0 .. 31] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 256, 257, 258, 259, 260, 261, 262, 263, 320, 321, 322, 323, 324, 325, 326, 327);
  psd_y: array [0 .. 31] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 128 + (8 * 0), 128 + (8 * 1), 128 + (8 * 2), 128 + (8 * 3), 128 + (8 * 4), 128 + (8 * 5), 128 + (8 * 6), 128 + (8 * 7), 512 + (8 * 0), 512 + (8 * 1), 512 + (8 * 2), 512 + (8 * 3),
    512 + (8 * 4), 512 + (8 * 5), 512 + (8 * 6), 512 + (8 * 7), 640 + (8 * 0), 640 + (8 * 1), 640 + (8 * 2), 640 + (8 * 3), 640 + (8 * 4), 640 + (8 * 5), 640 + (8 * 6), 640 + (8 * 7));
var
  memory_temp: array [0 .. $FFFF] of byte;
begin
  machine_calls.general_loop := pinballaction_loop;
  machine_calls.reset := reset_pinballaction;
  start_pinballaction := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  screen_init(3, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, $100);
  z80_0.change_ram_calls(pinballaction_getbyte, pinballaction_putbyte);
  if not(roms_load(@memory, pinballaction_rom)) then
    exit;
  // Sound CPU
  z80_1 := cpu_z80.create(3072000, $100);
  z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_1.change_io_calls(nil, snd_outbyte);
  z80_1.init_sound(pinballaction_sound_update);
  timers.init(z80_1.numero_cpu, 3072000 / (2 * 60), pbaction_sound_irq, nil, true);
  if not(roms_load(@mem_snd, pinballaction_sound)) then
    exit;
  // Sound Chip
ay8910_0:=ay8910_chip.create(1500000,AY8910);
ay8910_1:=ay8910_chip.create(1500000,AY8910);
ay8910_2:=ay8910_chip.create(1500000,AY8910);
  // convertir chars
  if not(roms_load(@memory_temp, pinballaction_chars)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(3, 0, 8 * 8, $400 * 0 * 8 * 8, $400 * 1 * 8 * 8, $400 * 2 * 8 * 8);
  convert_gfx(0, 0, @memory_temp, @psd_x, @psd_y, true, false);
  // tiles
  if not(roms_load(@memory_temp, pinballaction_tiles)) then
    exit;
  init_gfx(1, 8, 8, $800);
  gfx_set_desc_data(4, 0, 8 * 8, $800 * 0 * 8 * 8, $800 * 1 * 8 * 8, $800 * 2 * 8 * 8, $800 * 3 * 8 * 8);
  convert_gfx(1, 0, @memory_temp, @psd_x, @psd_y, true, false);
  // convertir sprites
  if not(roms_load(@memory_temp, pinballaction_sprites)) then
    exit;
  init_gfx(2, 16, 16, $100);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(3, 0, 32 * 8, $100 * 0 * 8 * 32, $100 * 1 * 8 * 32, $100 * 2 * 8 * 32);
  convert_gfx(2, 0, @memory_temp, @psd_x, @psd_y, true, false);
  // convertir sprites double
  init_gfx(3, 32, 32, $20);
  gfx[3].trans[0] := true;
  gfx_set_desc_data(3, 0, 128 * 8, $40 * 0 * 8 * 128, $40 * 1 * 8 * 128, $40 * 2 * 8 * 128);
  convert_gfx(3, 0, @memory_temp[$1000], @psd_x, @psd_y, true, false);
  // DIP
  marcade.dswa := $40;
  marcade.dswa_val2 := @pinballaction_dipa;
  marcade.dswb := 0;
  marcade.dswb_val2 := @pinballaction_dipb;
  start_pinballaction := true;
end;

end.
