unit tehkanworldcup_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  msm5205,
  rom_engine,
  pal_engine,
  sound_engine;

function start_tehkanworldcup: boolean;

implementation

const
  tehkanwc_rom: array [0 .. 3] of tipo_roms = ((n: 'twc-1.bin'; l: $4000; p: 0; crc: $34D6D5FF), (n: 'twc-2.bin'; l: $4000; p: $4000; crc: $7017A221), (n: 'twc-3.bin'; l: $4000; p: $8000;
    crc: $8B662902), ());
  tehkanwc_cpu2: tipo_roms = (n: 'twc-4.bin'; l: $8000; p: 0; crc: $70A9F883);
  tehkanwc_sound: tipo_roms = (n: 'twc-6.bin'; l: $4000; p: 0; crc: $E3112BE2);
  tehkanwc_chars: tipo_roms = (n: 'twc-12.bin'; l: $4000; p: 0; crc: $A9E274F8);
  tehkanwc_sprites: array [0 .. 2] of tipo_roms = ((n: 'twc-8.bin'; l: $8000; p: 0; crc: $055A5264), (n: 'twc-7.bin'; l: $8000; p: $8000; crc: $59FAEBE7), ());
  tehkanwc_tiles: array [0 .. 2] of tipo_roms = ((n: 'twc-11.bin'; l: $8000; p: 0; crc: $669389FC), (n: 'twc-9.bin'; l: $8000; p: $8000; crc: $347EF108), ());
  tehkanwc_adpcm: tipo_roms = (n: 'twc-5.bin'; l: $4000; p: 0; crc: $444B5544);
  // DIP
  tehkanwc_dipa: array [0 .. 3] of def_dip = ((mask: $7; name: 'Coin A'; number: 7; dip: ((dip_val: $1; dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $0;
    dip_name: '2C 3C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 3C'), (dip_val: $4; dip_name: '1C 4C'), (dip_val: $3; dip_name: '1C 5C'), (dip_val: $2;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $38; name: 'Coin B'; number: 7; dip: ((dip_val: $8; dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $0;
    dip_name: '2C 3C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28; dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 5C'), (dip_val: $10;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Start Credits P1/P2'; number: 4; dip: ((dip_val: $80; dip_name: '1C/1C'), (dip_val: $C0; dip_name: '1C/2C'), (dip_val: $40;
    dip_name: '2C/2C'), (dip_val: $0; dip_name: '2C/3C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  tehkanwc_dipb: array [0 .. 3] of def_dip = ((mask: $3; name: '1P Game Time'; number: 4; dip: ((dip_val: $0; dip_name: '2:30'), (dip_val: $1; dip_name: '2:00'), (dip_val: $3;
    dip_name: '1:30'), (dip_val: $2; dip_name: '1:00'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $7C; name: '2P Game Time'; number: 16;
    dip: ((dip_val: $0; dip_name: '5:00/3:00 Extra'), (dip_val: $60; dip_name: '5:00/2:45 Extra'), (dip_val: $20; dip_name: '5:00/2:35 Extra'), (dip_val: $40;
    dip_name: '5:00/2:30 Extra'), (dip_val: $4; dip_name: '4:00/2:30 Extra'), (dip_val: $64; dip_name: '4:00/2:15 Extra'), (dip_val: $24; dip_name: '4:00/2:05 Extra'), (dip_val: $44;
    dip_name: '4:00/2:00 Extra'), (dip_val: $1C; dip_name: '3:30/2:15 Extra'), (dip_val: $7C; dip_name: '3:30/2:00 Extra'), (dip_val: $3C; dip_name: '3:30/1:50 Extra'), (dip_val: $5C;
    dip_name: '3:30/1:45 Extra'), (dip_val: $8; dip_name: '3:00/2:00 Extra'), (dip_val: $68; dip_name: '3:00/1:45 Extra'), (dip_val: $28; dip_name: '3:00/1:35 Extra'), (dip_val: $48;
    dip_name: '3:00/1:30 Extra'))), (mask: $80; name: 'Game Type'; number: 2; dip: ((dip_val: $80; dip_name: 'Timer In'), (dip_val: $0; dip_name: 'Credit In'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), ());
  tehkanwc_dipc: array [0 .. 3] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $2; dip_name: 'Easy'), (dip_val: $3; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Timer Speed'; number: 2;
    dip: ((dip_val: $4; dip_name: '60/60'), (dip_val: $0; dip_name: '55/60'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $8; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 4;

var
  scroll_x: word;
  sound_latch, sound_latch2, scroll_y: byte;
  track0, track1: array [0 .. 1] of byte;

procedure update_video_tehkanwc;
var
  f, color, nchar, atrib, x, y: word;
begin
  // background
  for f := 0 to $3FF do
  begin
    atrib := memory[$E001 + (f * 2)];
    color := atrib and $F;
    if (gfx[2].buffer[f] or buffer_color[color + $10]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$E000 + (f * 2)] + ((atrib and $30) shl 4);
      put_gfx_flip(x * 16, y * 8, nchar, (color shl 4) + 512, 1, 2, (atrib and $40) <> 0, (atrib and $80) <> 0);
      gfx[2].buffer[f] := false;
    end;
  end;
  scroll_x_y(1, 2, scroll_x, scroll_y);
  // chars
  for f := 0 to $3FF do
  begin
    atrib := memory[$D400 + f];
    color := atrib and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$D000 + f] + ((atrib and $10) shl 4);
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 3, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
      if (atrib and $20) = 0 then
        put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 4, 0, (atrib and $40) <> 0, (atrib and $80) <> 0)
      else
        put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
      gfx[0].buffer[f] := false;
    end;
  end;
  // chars de arriba
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 2);
  // Sprites
  for f := 0 to $FF do
  begin
    atrib := memory[$E801 + (f * 4)];
    x := memory[$E802 + (f * 4)] + ((atrib and $20) shl 3) - 128;
    y := memory[$E803 + (f * 4)];
    color := (atrib and $7) shl 4;
    nchar := memory[$E800 + (f * 4)] + ((atrib and $08) shl 5);
    put_gfx_sprite(nchar, color + 256, (atrib and $40) <> 0, (atrib and $80) <> 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  // Prioridad de los chars
  update_region(0, 0, 256, 256, 4, 0, 0, 256, 256, 2);
  update_final_piece(0, 16, 256, 224, 2);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_tehkanwc;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // P2
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
  end;
end;

procedure tehkanwc_loop;
var
  frame_m, frame_s, frame_m2: single;
  f, h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_m2 := z80_1.tframes;
  frame_s := z80_2.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // CPU 1
          z80_0.run(frame_m);
          frame_m := frame_m + z80_0.tframes - z80_0.contador;
          // CPU 2
          z80_1.run(frame_m2);
          frame_m2 := frame_m2 + z80_1.tframes - z80_1.contador;
          // CPU Sound
          z80_2.run(frame_s);
          frame_s := frame_s + z80_2.tframes - z80_2.contador;
        end;
        if f = 239 then
        begin
          z80_0.change_irq(HOLD_LINE);
          z80_1.change_irq(HOLD_LINE);
          z80_2.change_irq(HOLD_LINE);
          update_video_tehkanwc;
        end;
      end;
      events_tehkanwc;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure change_color(dir: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[dir];
  color.b := pal4bit(tmp_color);
  tmp_color := buffer_paleta[dir + 1];
  color.g := pal4bit(tmp_color shr 4);
  color.r := pal4bit(tmp_color);
  dir := dir shr 1;
  set_pal_color(color, dir);
  case dir of
    0 .. 255:
      buffer_color[dir shr 4] := true;
    512 .. 767:
      buffer_color[((dir shr 4) and $F) + $10] := true;
  end;
end;

procedure mem_shared_w(direccion: word; valor: byte);
begin
  memory[direccion] := valor;
  case (direccion - $C800) of
    $800 .. $FFF:
      gfx[0].buffer[direccion and $3FF] := true;
    $1000 .. $17FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color(direccion and $7FE);
      end;
    $1800 .. $2000:
      gfx[2].buffer[(direccion and $7FF) shr 1] := true;
    $2400:
      scroll_x := (scroll_x and $100) or valor;
    $2401:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $2402:
      scroll_y := valor;
  end;
end;

function tehkanwc_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $EC02:
      tehkanwc_getbyte := memory[direccion];
    $F800:
      tehkanwc_getbyte := track0[0] - analog.c[0].x[0];
    $F801:
      tehkanwc_getbyte := track0[1] - analog.c[0].y[0];
    $F802, $F806:
      tehkanwc_getbyte := marcade.in2;
    $F803:
      tehkanwc_getbyte := marcade.in0;
    $F810:
      tehkanwc_getbyte := track1[0] - analog.c[0].x[1];
    $F811:
      tehkanwc_getbyte := track1[1] - analog.c[0].y[1];
    $F813:
      tehkanwc_getbyte := marcade.in1;
    $F820:
      tehkanwc_getbyte := sound_latch2;
    $F840:
      tehkanwc_getbyte := marcade.dswa;
    $F850:
      tehkanwc_getbyte := marcade.dswb;
    $F860:
      ;
    $F870:
      tehkanwc_getbyte := marcade.dswc;
  end;
end;

procedure tehkanwc_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      memory[direccion] := valor;
    $C800 .. $EC02:
      mem_shared_w(direccion, valor);
    $F800:
      track0[0] := valor;
    $F801:
      track0[1] := valor;
    $F810:
      track1[0] := valor;
    $F811:
      track1[1] := valor;
    $F820:
      begin
        sound_latch := valor;
        z80_2.change_nmi(ASSERT_LINE);
      end;
    $F840:
      if valor = 0 then
        z80_1.change_reset(ASSERT_LINE)
      else
        z80_1.change_reset(CLEAR_LINE);
  end;
end;

function tehkanwc_misc_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $C7FF:
      tehkanwc_misc_getbyte := mem_misc[direccion];
    $C800 .. $EC02:
      tehkanwc_misc_getbyte := memory[direccion];
    $F860:
      ; // WatchDog
  end;
end;

procedure tehkanwc_misc_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $C7FF:
      mem_misc[direccion] := valor;
    $C800 .. $EC02:
      mem_shared_w(direccion, valor);
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $47FF:
      snd_getbyte := mem_snd[direccion];
    $C000:
      snd_getbyte := sound_latch;
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
    $8001:
      msm5205_0.reset_w((valor and 1) = 0);
    $8003:
      z80_2.change_nmi(CLEAR_LINE);
    $C000:
      sound_latch2 := valor;
  end;
end;

function snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      snd_inbyte := ay8910_0.Read;
    1:
      snd_inbyte := ay8910_1.Read;
  end;
end;

procedure snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      ay8910_0.Write(valor);
    $1:
      ay8910_0.Control(valor);
    $2:
      ay8910_1.Write(valor);
    $3:
      ay8910_1.Control(valor);
  end;
end;

procedure tehkan_porta_write(valor: byte);
begin
  msm5205_0.pos := (msm5205_0.pos and $FF00) or valor;
end;

procedure tehkan_portb_write(valor: byte);
begin
  msm5205_0.pos := (msm5205_0.pos and $FF) or (valor shl 8);
end;

function tehkan_porta_read: byte;
begin
  tehkan_porta_read := msm5205_0.pos and $FF;
end;

function tehkan_portb_read: byte;
begin
  tehkan_portb_read := msm5205_0.pos shr 8;
end;

procedure msm5205_sound;
begin
  if msm5205_0.data_val <> -1 then
  begin
    msm5205_0.data_w(msm5205_0.data_val and $F);
    msm5205_0.pos := (msm5205_0.pos + 1) and $7FFF;
    msm5205_0.data_val := -1;
  end
  else
  begin
    msm5205_0.data_val := msm5205_0.rom_data[msm5205_0.pos and $7FFF];
    msm5205_0.data_w(msm5205_0.data_val shr 4);
  end;
end;

procedure tehkanwc_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
  msm5205_0.update;
end;

// Main
procedure reset_tehkanwc;
begin
  z80_0.reset;
  z80_1.reset;
  z80_2.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  msm5205_0.reset;
  reset_audio;
  marcade.in0 := $20;
  marcade.in1 := $20;
  marcade.in2 := $F;
  scroll_x := 0;
  scroll_y := 0;
  sound_latch := 0;
  sound_latch2 := 0;
end;

function start_tehkanworldcup: boolean;
const
  ps_x: array [0 .. 15] of dword = (1 * 4, 0 * 4, 3 * 4, 2 * 4, 5 * 4, 4 * 4, 7 * 4, 6 * 4, 8 * 32 + 1 * 4, 8 * 32 + 0 * 4, 8 * 32 + 3 * 4, 8 * 32 + 2 * 4, 8 * 32 + 5 * 4, 8 * 32 + 4 * 4,
    8 * 32 + 7 * 4, 8 * 32 + 6 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 16 * 32, 17 * 32, 18 * 32, 19 * 32, 20 * 32, 21 * 32, 22 * 32, 23 * 32);
var
  memory_temp: array [0 .. $FFFF] of byte;
begin
  machine_calls.general_loop := tehkanwc_loop;
  machine_calls.reset := reset_tehkanwc;
  start_tehkanworldcup := false;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 512, 256, false, true);
  screen_init(3, 256, 256, true);
  screen_init(4, 256, 256, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(4608000, $100 * CPU_SYNC);
  z80_0.change_ram_calls(tehkanwc_getbyte, tehkanwc_putbyte);
  // Misc CPU
  z80_1 := cpu_z80.create(4608000, $100 * CPU_SYNC);
  z80_1.change_ram_calls(tehkanwc_misc_getbyte, tehkanwc_misc_putbyte);
  // analog
  init_analog(z80_0.numero_cpu, z80_0.clock);
  analog_0(100, 10, 0, 63, -63, true);
  // Sound CPU
  z80_2 := cpu_z80.create(4608000, $100 * CPU_SYNC);
  z80_2.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_2.change_io_calls(snd_inbyte, snd_outbyte);
  z80_2.init_sound(tehkanwc_sound_update);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(1536000, AY8910, 0.50);
  ay8910_0.change_io_calls(nil, nil, tehkan_porta_write, tehkan_portb_write);
  ay8910_1 := ay8910_chip.create(1536000, AY8910, 0.50);
  ay8910_1.change_io_calls(tehkan_porta_read, tehkan_portb_read, nil, nil);
  msm5205_0 := MSM5205_chip.create(384000, MSM5205_S96_4B, 0.2, $8000);
  msm5205_0.change_advance(msm5205_sound);
  if not(roms_load(msm5205_0.rom_data, tehkanwc_adpcm)) then
    exit;
  // cargar roms
  if not(roms_load(@memory, tehkanwc_rom)) then
    exit;
  // cargar cpu 2
  if not(roms_load(@mem_misc, tehkanwc_cpu2)) then
    exit;
  // cargar sonido
  if not(roms_load(@mem_snd, tehkanwc_sound)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, tehkanwc_chars)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, tehkanwc_sprites)) then
    exit;
  init_gfx(1, 16, 16, 512);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // tiles
  if not(roms_load(@memory_temp, tehkanwc_tiles)) then
    exit;
  init_gfx(2, 16, 8, 1024);
  gfx_set_desc_data(4, 0, 64 * 8, 0, 1, 2, 3);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := $FF;
  marcade.dswa_val := @tehkanwc_dipa;
  marcade.dswb := $FF;
  marcade.dswb_val := @tehkanwc_dipb;
  marcade.dswc := $F;
  marcade.dswc_val := @tehkanwc_dipc;
  reset_tehkanwc;
  start_tehkanworldcup := true;
end;

end.
