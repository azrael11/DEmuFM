unit epos_hw;

interface

uses
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  rom_engine,
  pal_engine,
  sound_engine;

function start_epos: boolean;

implementation

const
  // The Glob
  theglob_rom: array [0 .. 7] of tipo_roms = ((n: 'globu10.bin'; l: $1000; p: 0; crc: $08FDB495),
    (n: 'globu9.bin'; l: $1000; p: $1000; crc: $827CD56C), (n: 'globu8.bin'; l: $1000; p: $2000;
    crc: $D1219966), (n: 'globu7.bin'; l: $1000; p: $3000; crc: $B1649DA7), (n: 'globu6.bin';
    l: $1000; p: $4000; crc: $B3457E67), (n: 'globu5.bin'; l: $1000; p: $5000; crc: $89D582CD),
    (n: 'globu4.bin'; l: $1000; p: $6000; crc: $7EE9FDEB), (n: 'globu11.bin'; l: $800; p: $7000;
    crc: $9E05DEE3));
  theglob_pal: tipo_roms = (n: '82s123.u66'; l: $20; p: 0; crc: $F4F6DDC5);
  // Super Glob
  superglob_rom: array [0 .. 7] of tipo_roms = ((n: 'u10'; l: $1000; p: 0; crc: $C0141324),
    (n: 'u9'; l: $1000; p: $1000; crc: $58BE8128), (n: 'u8'; l: $1000; p: $2000; crc: $6D088C16),
    (n: 'u7'; l: $1000; p: $3000; crc: $B2768203), (n: 'u6'; l: $1000; p: $4000; crc: $976C8F46),
    (n: 'u5'; l: $1000; p: $5000; crc: $340F5290), (n: 'u4'; l: $1000; p: $6000; crc: $173BD589),
    (n: 'u11'; l: $800; p: $7000; crc: $D45B740D));
  theglob_dip: array [0 .. 5] of def_dip = ((mask: $1; name: 'Coinage'; number: 2;
    dip: ((dip_val: $0; dip_name: '1C 1C'), (dip_val: $1; dip_name: '1C 2C'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $50; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '3'), (dip_val: $10; dip_name: '4'), (dip_val: $40;
    dip_name: '5'), (dip_val: $50; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $26; name: 'Difficulty'; number: 8; dip: ((dip_val: $0; dip_name: '1'), (dip_val: $2;
    dip_name: '2'), (dip_val: $20; dip_name: '3'), (dip_val: $22; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (dip_val: $6; dip_name: '6'), (dip_val: $24; dip_name: '7'), (dip_val: $26;
    dip_name: '8'), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $0; dip_name: '10K + Difficulty * 10K'), (dip_val: $8;
    dip_name: '90K + Difficulty * 10K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  palette: byte;
  buffer: array [0 .. $7FFF] of boolean;

procedure update_video_epos;
var
  f, x, y, temp: word;
  atrib: byte;
begin
  for f := 0 to $7FFF do
  begin
    if buffer[f] then
    begin
      x := f div 136;
      y := 270 - ((f mod 136) * 2);
      atrib := memory[f + $8000];
      temp := paleta[(palette shl 4) + (atrib and $0F)];
      putpixel(x, y + 1, 1, @temp, 1);
      temp := paleta[(palette shl 4) + (atrib shr 4)];
      putpixel(x, y, 1, @temp, 1);
      buffer[f] := false;
    end;
  end;
  actualiza_trozo_simple(0, 0, 236, 272, 1);
end;

procedure events_epos;
begin
  if event.arcade then
  begin
    // input
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
  end;
end;

procedure epos_hw_loop;
var
  frame: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 240 do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 235 then
        begin
          z80_0.change_irq(HOLD_LINE);
          update_video_epos;
        end;
      end;
      events_epos;
      video_sync;
    end;
  end;
end;

function epos_getbyte(direccion: word): byte;
begin
  epos_getbyte := memory[direccion];
end;

procedure epos_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $77FF:
      ;
    $7800 .. $7FFF:
      memory[direccion] := valor;
    $8000 .. $FFFF:
      if memory[direccion] <> valor then
      begin
        buffer[direccion and $7FFF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function epos_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $00:
      epos_inbyte := marcade.dswa; // DSW
    $01:
      epos_inbyte := marcade.in1; // SYSTEM
    $02:
      epos_inbyte := marcade.in0; // INPUTS
    $03:
      epos_inbyte := 0;
  end;
end;

procedure epos_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $01:
      palette := (valor shr 3) and 1;
    $02:
      ay8910_0.Write(valor);
    $06:
      ay8910_0.Control(valor);
  end;
end;

procedure epos_sound_update;
begin
  ay8910_0.update;
end;

// Main
procedure reset_epos_hw;
begin
  z80_0.reset;
  ay8910_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $BE;
  palette := 0;
  fillchar(buffer, $8000, 1);
end;

function start_epos: boolean;
var
  colores: tpaleta;
  memory_temp: array [0 .. $1F] of byte;
  f,bit0, bit1, bit2: byte;
begin
  start_epos := false;
  machine_calls.general_loop := epos_hw_loop;
  machine_calls.reset := reset_epos_hw;
  start_audio(false);
  screen_init(1, 241, 272);
  start_video(236, 272);
  // Main CPU
  z80_0 := cpu_z80.create(2750000, 241);
  z80_0.change_ram_calls(epos_getbyte, epos_putbyte);
  z80_0.change_io_calls(epos_inbyte, epos_outbyte);
  z80_0.init_sound(epos_sound_update);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(687500, AY8910, 1);
  case main_vars.machine_type of
    94:
      begin // The Glob
        // cargar roms
        if not(roms_load(@memory, theglob_rom)) then
          exit;
        // poner la paleta y clut
        if not(roms_load(@memory_temp, theglob_pal)) then
          exit;
        // DIP
        marcade.dswa := 0;
        marcade.dswa_val := @theglob_dip;
      end;
    95:
      begin // Super Glob
        // cargar roms
        if not(roms_load(@memory, superglob_rom)) then
          exit;
        // poner la paleta y clut
        if not(roms_load(@memory_temp, theglob_pal)) then
          exit;
        // DIP
        marcade.dswa := 0;
        marcade.dswa_val := @theglob_dip;
      end;
  end;
  for f := 0 to $1F do
  begin
    bit0 := (memory_temp[f] shr 7) and $01;
    bit1 := (memory_temp[f] shr 6) and $01;
    bit2 := (memory_temp[f] shr 5) and $01;
    colores[f].r := $92 * bit0 + $4A * bit1 + $23 * bit2;
    bit0 := (memory_temp[f] shr 4) and $01;
    bit1 := (memory_temp[f] shr 3) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    colores[f].g := $92 * bit0 + $4A * bit1 + $23 * bit2;
    bit0 := (memory_temp[f] shr 1) and $01;
    bit1 := (memory_temp[f] shr 0) and $01;
    colores[f].b := $AD * bit0 + $52 * bit1;
  end;
  set_pal(colores, $20);
  // final
  reset_epos_hw;
  start_epos := true;
end;

end.
