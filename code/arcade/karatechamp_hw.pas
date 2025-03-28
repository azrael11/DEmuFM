unit karatechamp_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ay_8910,
  timer_engine,
  dac;

function start_karatechamp: boolean;

implementation

const
  karatechamp_rom: array [0 .. 5] of tipo_roms = ((n: 'b014.bin'; l: $2000; p: 0; crc: $0000D1A0), (n: 'b015.bin'; l: $2000; p: $2000; crc: $03FAE67E), (n: 'b016.bin'; l: $2000; p: $4000;
    crc: $3B6E1D08), (n: 'b017.bin'; l: $2000; p: $6000; crc: $C1848D1A), (n: 'b018.bin'; l: $2000; p: $8000; crc: $B824ABC7), (n: 'b019.bin'; l: $2000; p: $A000; crc: $3B487A46));
  karatechamp_sound: array [0 .. 6] of tipo_roms = ((n: 'b026.bin'; l: $2000; p: 0; crc: $999ED2C7), (n: 'b025.bin'; l: $2000; p: $2000; crc: $33171E07), (n: 'b024.bin'; l: $2000; p: $4000;
    crc: $910B48B9), (n: 'b023.bin'; l: $2000; p: $6000; crc: $47F66AAC), (n: 'b022.bin'; l: $2000; p: $8000; crc: $5928E749), (n: 'b021.bin'; l: $2000; p: $A000; crc: $CA17E3BA), (n: 'b020.bin';
    l: $2000; p: $C000; crc: $ADA4F2CD));
  karatechamp_char: array [0 .. 1] of tipo_roms = ((n: 'b000.bin'; l: $2000; p: 0; crc: $A4FA98A1), (n: 'b001.bin'; l: $2000; p: $4000; crc: $FEA09F7C));
  karatechamp_sprt: array [0 .. 11] of tipo_roms = ((n: 'b013.bin'; l: $2000; p: 0; crc: $EAAD4168), (n: 'b004.bin'; l: $2000; p: $2000; crc: $10A47E2D), (n: 'b012.bin'; l: $2000; p: $4000;
    crc: $B4842EA9), (n: 'b003.bin'; l: $2000; p: $6000; crc: $8CD166A5), (n: 'b011.bin'; l: $2000; p: $8000; crc: $4CBD3AA3), (n: 'b002.bin'; l: $2000; p: $A000; crc: $6BE342A6), (n: 'b007.bin';
    l: $2000; p: $C000; crc: $CB91D16B), (n: 'b010.bin'; l: $2000; p: $E000; crc: $489C9C04), (n: 'b006.bin'; l: $2000; p: $10000; crc: $7346DB8A), (n: 'b009.bin'; l: $2000; p: $12000;
    crc: $B78714FC), (n: 'b005.bin'; l: $2000; p: $14000; crc: $B2557102), (n: 'b008.bin'; l: $2000; p: $16000; crc: $C85ABA0E));
  karatechamp_pal: array [0 .. 2] of tipo_roms = ((n: 'br27'; l: $100; p: 0; crc: $F683C54A), (n: 'br26'; l: $100; p: $100; crc: $3DDBB6C4), (n: 'br25'; l: $100; p: $200; crc: $BA4A5651));
  // Dip
  karatechamp_dip: array [0 .. 6] of def_dip = ((mask: $3; name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Difficulty'; number: 2; dip: ((dip_val: $0; dip_name: 'Hard'), (dip_val: $10; dip_name: 'Normal'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Free Play'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Demo Sounds';
    number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  sound_latch: byte;
  nmi_enable, nmi_enable_sound: boolean;

procedure update_video_karatechamp;
var
  x, y, atrib, color: byte;
  f, nchar: word;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory[$E400 + f];
      color := atrib shr 3;
      nchar := memory[$E000 + f] + ((atrib and $7) shl 8);
      put_gfx(x * 8, y * 8, nchar, (color shl 2) + 128, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  for f := 0 to $3F do
  begin
    atrib := memory[$EA02 + (f * 4)];
    nchar := memory[$EA01 + (f * 4)] + ((atrib and $10) shl 4);
    color := (atrib and $F) shl 2;
    x := memory[$EA00 + (f * 4)] - 9;
    y := memory[$EA03 + (f * 4)] - 8;
    put_gfx_sprite(nchar, color, (atrib and $80) <> 0, false, 1 + ((atrib and $60) shr 5));
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_karatechamp;
begin
  if event.arcade then
  begin
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or $1;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or $2;
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or $4;
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or $8;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
  end;
end;

procedure karatechamp_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        // Main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if (f = 241) then
        begin
          if nmi_enable then
            z80_0.change_nmi(ASSERT_LINE);
          update_video_karatechamp;
        end;
      end;
      events_karatechamp;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function karatechamp_getbyte(direccion: word): byte;
begin
  karatechamp_getbyte := memory[direccion];
end;

procedure karatechamp_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $DFFF, $EA00 .. $FFFF:
      memory[direccion] := valor;
    $E000 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
  end;
end;

function karatechamp_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $80:
      karatechamp_inbyte := marcade.dswa;
    $90:
      karatechamp_inbyte := marcade.in1;
    $98:
      karatechamp_inbyte := $FF;
    $A0:
      karatechamp_inbyte := marcade.in0;
    $A8:
      ;
  end;
end;

procedure karatechamp_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $80:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $81:
      begin
        nmi_enable := valor <> 0;
        if not(nmi_enable) then
          z80_0.change_nmi(CLEAR_LINE);
      end;
    $A8:
      begin
        sound_latch := valor;
        z80_1.change_irq(ASSERT_LINE);
      end;
  end;
end;

// sound
function karatechamp_getbyte_snd(direccion: word): byte;
begin
  karatechamp_getbyte_snd := mem_snd[direccion];
end;

procedure karatechamp_putbyte_snd(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ; // ROM
    $E000 .. $E2FF:
      mem_snd[direccion] := valor;
  end;
end;

function karatechamp_inbyte_snd(puerto: word): byte;
begin
  if (puerto and $FF) = $6 then
  begin
    karatechamp_inbyte_snd := sound_latch;
    z80_1.change_irq(CLEAR_LINE);
  end;
end;

procedure karatechamp_outbyte_snd(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ay8910_0.write(valor);
    1:
      ay8910_0.control(valor);
    2:
      ay8910_1.write(valor);
    3:
      ay8910_1.control(valor);
    4:
      dac_0.data8_w(valor);
    5:
      begin
        nmi_enable_sound := (valor and $80) <> 0;
        if not(nmi_enable_sound) then
          z80_1.change_nmi(CLEAR_LINE);
      end;
  end;
end;

procedure karatechamp_snd_irq;
begin
  if nmi_enable_sound then
    z80_1.change_nmi(ASSERT_LINE);
end;

procedure karatechamp_sound_update;
begin
  ay8910_0.Update;
  ay8910_1.Update;
  dac_0.Update;
end;

// Main
procedure karatechamp_reset;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  dac_0.reset;
reset_video;
  reset_audio;
  nmi_enable := false;
  nmi_enable_sound := false;
  sound_latch := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
end;

function start_karatechamp: boolean;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, $2000 * 8 + 0, $2000 * 8 + 1, $2000 * 8 + 2, $2000 * 8 + 3, $2000 * 8 + 4, $2000 * 8 + 5, $2000 * 8 + 6, $2000 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $17FFF] of byte;
begin
  start_karatechamp := false;
  machine_calls.general_loop := karatechamp_loop;
  machine_calls.reset := karatechamp_reset;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(12000000 div 4, 256);
  z80_0.change_ram_calls(karatechamp_getbyte, karatechamp_putbyte);
  z80_0.change_io_calls(karatechamp_inbyte, karatechamp_outbyte);
  if not(roms_load(@memory, karatechamp_rom)) then
    exit;
  // Sound Chip
  z80_1 := cpu_z80.create(12000000 div 4, 256);
  z80_1.change_ram_calls(karatechamp_getbyte_snd, karatechamp_putbyte_snd);
  z80_1.change_io_calls(karatechamp_inbyte_snd, karatechamp_outbyte_snd);
  z80_1.init_sound(karatechamp_sound_update);
  if not(roms_load(@mem_snd, karatechamp_sound)) then
    exit;
  // IRQ Sound CPU
  timers.init(z80_1.numero_cpu, 3000000 / 125, karatechamp_snd_irq, nil, true);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(12000000 div 12, AY8910, 1);
  ay8910_1 := ay8910_chip.create(12000000 div 12, AY8910, 1);
  dac_0 := dac_chip.create;
  // cargar chars
  if not(roms_load(@memory_temp, karatechamp_char)) then
    exit;
  init_gfx(0, 8, 8, $800);
  gfx_set_desc_data(2, 0, 8 * 8, $4000 * 8, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // cargar sprites (3 bancos)
  if not(roms_load(@memory_temp, karatechamp_sprt)) then
    exit;
  init_gfx(1, 16, 16, $200 * 3);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(2, 0, 16 * 8, $C000 * 8, 0);
  convert_gfx(1, 0, @memory_temp[$8000], @ps_x, @ps_y, true, false);
  init_gfx(2, 16, 16, $200);
  gfx[2].trans[0] := true;
  convert_gfx(2, 0, @memory_temp[$4000], @ps_x, @ps_y, true, false);
  init_gfx(3, 16, 16, $200);
  gfx[3].trans[0] := true;
  convert_gfx(3, 0, @memory_temp[$0], @ps_x, @ps_y, true, false);
  // paleta de colores
  if not(roms_load(@memory_temp, karatechamp_pal)) then
    exit;
  for f := 0 to 255 do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
  end;
  set_pal(colores, $100);
  // DIP
  marcade.dswa := $3F;
  marcade.dswa_val := @karatechamp_dip;
  // Final
  karatechamp_reset;
  start_karatechamp := true;
end;

end.
