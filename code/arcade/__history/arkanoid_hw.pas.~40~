unit arkanoid_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  m6805,
  rom_engine,
  pal_engine,
  sound_engine,
  ay_8910;

function start_arkanoid: boolean;

implementation

const
  arkanoid_rom: array [0 .. 1] of tipo_roms = ((n: 'a75-01-1.ic17'; l: $8000; p: $0;
    crc: $5BCDA3B0), (n: 'a75-11.ic16'; l: $8000; p: $8000; crc: $EAFD7191));
  arkanoid_mcu: tipo_roms = (n: 'a75__06.ic14'; l: $800; p: $0; crc: $0BE83647);
  arkanoid_tiles: array [0 .. 2] of tipo_roms = ((n: 'a75-03.ic64'; l: $8000; p: 0; crc: $038B74BA),
    (n: 'a75-04.ic63'; l: $8000; p: $8000; crc: $71FAE199), (n: 'a75-05.ic62'; l: $8000; p: $10000;
    crc: $C76374E2));
  arkanoid_proms: array [0 .. 2] of tipo_roms = ((n: 'a75-07.ic24'; l: $200; p: $0; crc: $0AF8B289),
    (n: 'a75-08.ic23'; l: $200; p: $200; crc: $ABB002FB), (n: 'a75-09.ic22'; l: $200; p: $400;
    crc: $A7C6C277));
  // Dip
  arkanoid_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $1; dip_name: 'No'), (dip_val: $0; dip_name: 'Yes'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $8; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $10; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $10; dip_name: '20K 60K 60K+'), (dip_val: $0; dip_name: '20K'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Lives'; number: 2;
    dip: ((dip_val: $20; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $C0; name: 'Coinage'; number: 4;
    dip: ((dip_val: $40; dip_name: '2C 1C'), (dip_val: $C0; dip_name: '1C 1C'), (dip_val: $80;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: '1C 6C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());

var
  mcu_mem: array [0 .. $7FF] of byte;
  paddle_select, palettebank, gfxbank: byte;
  port_a_in, port_a_out, port_c_in, port_c_out, ddr_a, ddr_c, from_main, from_mcu: byte;
  main_sent, mcu_sent: boolean;

procedure update_video_arkanoid;
var
  f, nchar: word;
  color, x, y, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f shr 5);
      y := f and $1F;
      atrib := memory[$E000 + (f * 2)];
      color := ((atrib and $F8) shr 3) + palettebank;
      nchar := memory[$E001 + (f * 2)] + ((atrib and $07) shl 8) + 2048 * gfxbank;
      put_gfx(x * 8, y * 8, nchar, color shl 3, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  // Sprites
  for f := 0 to $F do
  begin
    atrib := memory[$E802 + (f * 4)];
    nchar := memory[$E803 + (f * 4)] + ((atrib and $03) shl 8) + 1024 * gfxbank;
    color := ((atrib and $F8) shr 3) + palettebank;
    x := memory[$E801 + (f * 4)];
    y := memory[$E800 + (f * 4)];
    put_gfx_sprite_diff(2 * nchar, color shl 3, false, false, 0, 8, 0);
    put_gfx_sprite_diff(2 * nchar + 1, color shl 3, false, false, 0, 0, 0);
    actualiza_gfx_sprite_size(x, y, 2, 16, 8);
  end;
  actualiza_trozo_final(16, 0, 224, 256, 2);
end;

procedure events_arkanoid;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 or $10
    else
      marcade.in0 := marcade.in0 and $EF;
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := marcade.in0 or $20
    else
      marcade.in0 := marcade.in0 and $DF;
    // p2
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
  end;
end;

procedure arkanoid_loop;
var
  frame_m, frame_mcu: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_mcu := m6805_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // mcu
        m6805_0.run(frame_mcu);
        frame_mcu := frame_mcu + m6805_0.tframes - m6805_0.contador;
        if f = 239 then
          z80_0.change_irq(HOLD_LINE);
      end;
      update_video_arkanoid;
      events_arkanoid;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function getbyte_arkanoid(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF, $E000 .. $EFFF:
      getbyte_arkanoid := memory[direccion];
    $C000 .. $CFFF:
      getbyte_arkanoid := memory[$C000 + (direccion and $7FF)];
    $D000 .. $DFFF:
      case (direccion and $1F) of
        $1, $3, $5, $7:
          getbyte_arkanoid := ay8910_0.read;
        $8 .. $B:
          getbyte_arkanoid := $FF;
        $C .. $F:
          getbyte_arkanoid := byte(not(mcu_sent)) * $80 or byte(not(main_sent)) * $40 or
            marcade.in0;
        $10 .. $17:
          getbyte_arkanoid := marcade.in1;
        $18 .. $1F:
          begin
            getbyte_arkanoid := from_mcu;
            mcu_sent := false;
          end;
      end;
    $F000 .. $FFFF:
      getbyte_arkanoid := 0;
  end;
end;

procedure putbyte_arkanoid(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF:
      memory[$C000 + (direccion and $7FF)] := valor;
    $D000 .. $DFFF:
      case (direccion and $1F) of
        $0, $2, $4, $6:
          ay8910_0.control(valor);
        $1, $3, $5, $7:
          ay8910_0.write(valor);
        $8 .. $F:
          begin
            paddle_select := (valor and $4) shr 2;
            if gfxbank <> ((valor and $20) shr 5) then
            begin
              fillchar(gfx[0].buffer, $400, 1);
              gfxbank := (valor and $20) shr 5;
            end;
            if palettebank <> ((valor and $40) shr 1) then
            begin
              fillchar(gfx[0].buffer, $400, 1);
              palettebank := (valor and $40) shr 1;
            end;
            if (valor and $80) = 0 then
              m6805_0.change_reset(ASSERT_LINE)
            else
              m6805_0.change_reset(CLEAR_LINE);
          end;
        $18 .. $1F:
          begin
            main_sent := true;
            from_main := valor;
            m6805_0.irq_request(0, ASSERT_LINE);
          end;
      end;
    $E000 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $E800 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function arkanoid_mcu_getbyte(direccion: word): byte;
var
  res: byte;
begin
  direccion := direccion and $7FF;
  case direccion of
    0:
      arkanoid_mcu_getbyte := (port_a_out and ddr_a) or (port_a_in and not(ddr_a));
    1:
      arkanoid_mcu_getbyte := analog.c[0].x[paddle_select];
    2:
      begin
        res := byte(main_sent) or (byte(not(mcu_sent)) shl 1);
        arkanoid_mcu_getbyte := (port_c_out and ddr_c) or (res and not(ddr_c));
      end;
    $10 .. $7FF:
      arkanoid_mcu_getbyte := mcu_mem[direccion];
  end;
end;

procedure arkanoid_mcu_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FF;
  case direccion of
    0:
      port_a_out := valor;
    2:
      begin
        if (((ddr_c and $4) <> 0) and ((not(valor) and $4) <> 0) and ((port_c_out and $4) <> 0))
        then
        begin
          port_a_in := from_main;
          main_sent := false;
          m6805_0.irq_request(0, CLEAR_LINE);
        end;
        if (((ddr_c and $8) <> 0) and ((not(valor) and $8) <> 0) and ((port_c_out and $8) <> 0))
        then
        begin
          from_mcu := port_a_out;
          mcu_sent := true;
        end;
        port_c_out := valor;
      end;
    4:
      ddr_a := valor;
    6:
      ddr_c := valor;
    $10 .. $7F:
      mcu_mem[direccion] := valor;
    $80 .. $7FF:
      ; // ROM
  end;
end;

procedure arkanoid_sound_update;
begin
  ay8910_0.update;
end;

function arkanoid_porta_r: byte;
begin
  arkanoid_porta_r := $FF;
end;

function arkanoid_portb_r: byte;
begin
  arkanoid_portb_r := marcade.dswa;
end;

// Main
procedure reset_arkanoid;
begin
  z80_0.reset;
  m6805_0.reset;
  ay8910_0.reset;
  marcade.in0 := $F;
  marcade.in1 := $FF;
  port_a_in := 0;
  port_a_out := 0;
  port_c_in := 0;
  port_c_out := 0;
  ddr_a := 0;
  ddr_c := 0;
  from_main := 0;
  from_mcu := 0;
  palettebank := 0;
  gfxbank := 0;
  paddle_select := 0;
  main_sent := false;
  mcu_sent := false;
end;

function start_arkanoid: boolean;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $17FFF] of byte;
begin
  machine_calls.general_loop := arkanoid_loop;
  machine_calls.reset := reset_arkanoid;
  machine_calls.fps_max := 59.185608;
  start_arkanoid := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(12000000 div 2, 264);
  z80_0.change_ram_calls(getbyte_arkanoid, putbyte_arkanoid);
  z80_0.init_sound(arkanoid_sound_update);
  // MCU CPU
  m6805_0 := cpu_m6805.create(12000000 div 4, 264, tipo_m68705);
  m6805_0.change_ram_calls(arkanoid_mcu_getbyte, arkanoid_mcu_putbyte);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(3000000, AY8910, 0.5);
  ay8910_0.change_io_calls(arkanoid_porta_r, arkanoid_portb_r, nil, nil);
  // analog
  init_analog(z80_0.numero_cpu, z80_0.clock);
  analog_0(30, 15, $7F, $FF, 0, false, true, true);
  // cargar roms
  if not(roms_load(@memory, arkanoid_rom)) then
    exit;
  if not(roms_load(@mcu_mem, arkanoid_mcu)) then
    exit;
  // Cargar tiles
  if not(roms_load(@memory_temp, arkanoid_tiles)) then
    exit;
  init_gfx(0, 8, 8, $1000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(3, 0, 8 * 8, 2 * 4096 * 8 * 8, 4096 * 8 * 8, 0);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, true, false);
  // pal
  if not(roms_load(@memory_temp, arkanoid_proms)) then
    exit;
  for f := 0 to $1FF do
  begin
    colores[f].r := pal4bit(memory_temp[f + $000]);
    colores[f].g := pal4bit(memory_temp[f + $200]);
    colores[f].b := pal4bit(memory_temp[f + $400]);
  end;
  set_pal(colores, $200);
  // Dip
  marcade.dswa := $FE;
  marcade.dswa_val := @arkanoid_dip_a;
  // final
  reset_arkanoid;
  start_arkanoid := true;
end;

end.
