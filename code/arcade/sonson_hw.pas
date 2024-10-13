unit sonson_hw;

interface

uses
  WinApi.Windows,
  m6809,
  ay_8910,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  qsnapshot;

function start_sonson: boolean;

implementation

const
  sonson_rom: array [0 .. 2] of tipo_roms = ((n: 'ss.01e'; l: $4000; p: $4000; crc: $CD40CC54), (n: 'ss.02e'; l: $4000; p: $8000; crc: $C3476527), (n: 'ss.03e'; l: $4000; p: $C000; crc: $1FD0E729));
  sonson_sonido: tipo_roms = (n: 'ss_6.c11'; l: $2000; p: $E000; crc: $1135C48A);
  sonson_char: array [0 .. 1] of tipo_roms = ((n: 'ss_7.b6'; l: $2000; p: 0; crc: $990890B1), (n: 'ss_8.b5'; l: $2000; p: $2000; crc: $9388FF82));
  sonson_sprites: array [0 .. 5] of tipo_roms = ((n: 'ss_9.m5'; l: $2000; p: 0; crc: $8CB1CACF), (n: 'ss_10.m6'; l: $2000; p: $2000; crc: $F802815E), (n: 'ss_11.m3'; l: $2000; p: $4000;
    crc: $4DBAD88A), (n: 'ss_12.m4'; l: $2000; p: $6000; crc: $AA05E687), (n: 'ss_13.m1'; l: $2000; p: $8000; crc: $66119BFA), (n: 'ss_14.m2'; l: $2000; p: $A000; crc: $E14EF54E));
  sonson_prom: array [0 .. 3] of tipo_roms = ((n: 'ssb4.b2'; l: $20; p: 0; crc: $C8EAF234), (n: 'ssb5.b1'; l: $20; p: $20; crc: $0E434ADD), (n: 'ssb2.c4'; l: $100; p: $40; crc: $C53321C6),
    (n: 'ssb3.h7'; l: $100; p: $140; crc: $7D2C324A));
  // Dip
  sonson_dip_a: array [0 .. 5] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $10; name: 'Coinage affects'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Coin A'), (dip_val: $0; dip_name: 'Coin B'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Service'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  sonson_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '4'), (dip_val: $1; dip_name: '5'), (dip_val: $0;
    dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: '2 Players Game'; number: 2;
    dip: ((dip_val: $4; dip_name: '1 Credit'), (dip_val: $0; dip_name: '2 Credit'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $8; dip_name: '20K 80K 100K'), (dip_val: $0; dip_name: '30K 90K 120K'), (dip_val: $18; dip_name: '20K'), (dip_val: $10; dip_name: '30K'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $60; name: 'Difficulty'; number: 4; dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Freeze'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  soundlatch, last, scroll_x: byte;

procedure update_video_sonson;
var
  f, color, nchar: word;
  x, y, atrib: byte;
begin
  // chars
  for f := $0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      y := f div 32;
      x := f mod 32;
      atrib := memory[$1400 + f];
      color := atrib and $FC;
      nchar := memory[$1000 + f] + ((atrib and $3) shl 8);
      put_gfx(x * 8, y * 8, nchar, color, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x(2, 1, scroll_x);
  actualiza_trozo(0, 0, 256, 40, 2, 0, 0, 256, 40, 1);
  // sprites
  for f := $17 downto 0 do
  begin
    atrib := memory[$2021 + (f * 4)];
    nchar := memory[$2022 + (f * 4)] + ((atrib and $20) shl 3);
    color := (atrib and $1F) shl 3;
    x := memory[$2023 + (f * 4)];
    y := memory[$2020 + (f * 4)];
    put_gfx_sprite(nchar, color, (atrib and $40) = 0, (atrib and $80) = 0, 1);
    update_gfx_sprite(x, y, 1, 1);
  end;
  update_final_piece(8, 8, 240, 240, 1);
end;

procedure events_sonson;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
  end;
end;

procedure sonson_loop;
var
  f: byte;
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := m6809_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 255 do
      begin
        // Main CPU
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Snd CPU
        m6809_1.run(frame_s);
        frame_s := frame_s + m6809_1.tframes - m6809_1.contador;
        if f = 247 then
        begin
          m6809_0.change_irq(HOLD_LINE);
          update_video_sonson;
        end;
      end;
      events_sonson;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function sonson_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $17FF, $2020 .. $207F, $4000 .. $FFFF:
      sonson_getbyte := memory[direccion];
    $3002:
      sonson_getbyte := marcade.in0;
    $3003:
      sonson_getbyte := marcade.in1;
    $3004:
      sonson_getbyte := marcade.in2;
    $3005:
      sonson_getbyte := marcade.dswa;
    $3006:
      sonson_getbyte := marcade.dswb;
  end;
end;

procedure sonson_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $FFF, $2020 .. $207F:
      memory[direccion] := valor;
    $1000 .. $17FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $3000:
      scroll_x := valor;
    $3010:
      soundlatch := valor;
    $3018:
      main_screen.flip_main_screen := (valor and 1) <> 1;
    $3019:
      begin
        if ((last = 0) and ((valor and 1) = 1)) then
          m6809_1.change_firq(HOLD_LINE);
        last := valor and 1;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function ssonson_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF, $E000 .. $FFFF:
      ssonson_getbyte := mem_snd[direccion];
    $A000:
      ssonson_getbyte := soundlatch
  end;
end;

procedure ssonson_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $2000:
      ay8910_0.Control(valor);
    $2001:
      ay8910_0.Write(valor);
    $4000:
      ay8910_1.Control(valor);
    $4001:
      ay8910_1.Write(valor);
    $E000 .. $FFFF:
      ; // ROM
  end;
end;

procedure sonson_snd_irq;
begin
  m6809_1.change_irq(HOLD_LINE);
end;

procedure sonson_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

procedure sonson_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 2] of byte;
  size: word;
begin
  open_qsnapshot_save('sonson' + nombre);
  getmem(data, 250);
  // CPU
  size := m6809_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := m6809_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ay8910_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := ay8910_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory, $4000);
  savedata_qsnapshot(@mem_snd, $E000);
  // MISC
  buffer[0] := soundlatch;
  buffer[1] := last;
  buffer[2] := scroll_x;
  savedata_qsnapshot(@buffer, 3);
  freemem(data);
  close_qsnapshot;
end;

procedure sonson_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 2] of byte;
begin
  if not(open_qsnapshot_load('sonson' + nombre)) then
    exit;
  getmem(data, 250);
  // CPU
  loaddata_qsnapshot(data);
  m6809_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  m6809_1.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ay8910_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  ay8910_1.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory);
  loaddata_qsnapshot(@mem_snd);
  // MISC
  loaddata_qsnapshot(@buffer);
  soundlatch := buffer[0];
  last := buffer[1];
  scroll_x := buffer[2];
  freemem(data);
  close_qsnapshot;
  // END
  fillchar(gfx[0].buffer, $400, 1);
end;

// Main
procedure reset_sonson;
begin
  m6809_0.reset;
  m6809_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  reset_audio;
  soundlatch := 0;
  last := 0;
  scroll_x := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
end;

function start_sonson: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $BFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  ps_x: array [0 .. 15] of dword = (8 * 16 + 7, 8 * 16 + 6, 8 * 16 + 5, 8 * 16 + 4, 8 * 16 + 3, 8 * 16 + 2, 8 * 16 + 1, 8 * 16 + 0, 7, 6, 5, 4, 3, 2, 1, 0);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  machine_calls.general_loop := sonson_loop;
  machine_calls.reset := reset_sonson;
  machine_calls.save_qsnap := sonson_qsave;
  machine_calls.load_qsnap := sonson_qload;
  start_sonson := false;
  start_audio(false);
  screen_init(1, 256, 256, false, true);
  screen_init(2, 256, 256);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  start_video(240, 240);
  // Main CPU
  m6809_0 := cpu_m6809.Create(12000000 div 8, 256, TCPU_M6809);
  m6809_0.change_ram_calls(sonson_getbyte, sonson_putbyte);
  // Sound CPU
  m6809_1 := cpu_m6809.Create(12000000 div 8, 256, TCPU_M6809);
  m6809_1.change_ram_calls(ssonson_getbyte, ssonson_putbyte);
  m6809_1.init_sound(sonson_sound_update);
  // IRQ Sound CPU
  timers.init(1, (12000000 / 8) / (4 * 60), sonson_snd_irq, nil, true);
  // Sound Chip
  ay8910_0 := ay8910_chip.Create(1500000, AY8910, 0.3);
  ay8910_1 := ay8910_chip.Create(1500000, AY8910, 0.3);
  // cargar roms
  if not(roms_load(@memory, sonson_rom)) then
    exit;
  // Cargar Sound
  if not(roms_load(@mem_snd, sonson_sonido)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, sonson_char)) then
    exit;
  init_gfx(0, 8, 8, 1024);
  gfx_set_desc_data(2, 0, 8 * 8, $2000 * 8, 0 * 8);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, sonson_sprites)) then
    exit;
  init_gfx(1, 16, 16, 512);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(3, 0, 32 * 8, $8000 * 8, $4000 * 8, 0 * 8);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // colores
  if not(roms_load(@memory_temp, sonson_prom)) then
    exit;
  for f := 0 to 31 do
  begin
    colores[f].r := ((memory_temp[f + $20] and $F) shl 4) or (memory_temp[f + $20] and $F);
    colores[f].g := ((memory_temp[f] and $F0) shr 4) or (memory_temp[f] and $F0);
    colores[f].b := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
  end;
  set_pal(colores, 32);
  for f := 0 to 255 do
  begin
    gfx[0].colores[f] := memory_temp[$40 + f];
    gfx[1].colores[f] := memory_temp[$140 + f] + 16;
  end;
  // DIP
  marcade.dswa := $DF;
  marcade.dswb := $CB;
  marcade.dswa_val := @sonson_dip_a;
  marcade.dswb_val := @sonson_dip_b;
  // final
  reset_sonson;
  start_sonson := true;
end;

end.
