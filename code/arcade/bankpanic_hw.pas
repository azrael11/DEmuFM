unit bankpanic_hw;

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
  sn_76496;

function start_bankpanic: boolean;

implementation

const
  bankpanic_rom: array [0 .. 3] of tipo_roms = ((n: 'epr-6175.7e'; l: $4000; p: 0; crc: $044552B8), (n: 'epr-6174.7f'; l: $4000; p: $4000; crc: $D29B1598), (n: 'epr-6173.7h'; l: $4000; p: $8000; crc: $B8405D38), (n: 'epr-6176.7d'; l: $2000; p: $C000; crc: $C98AC200));
  bankpanic_char: array [0 .. 1] of tipo_roms = ((n: 'epr-6165.5l'; l: $2000; p: 0; crc: $AEF34A93), (n: 'epr-6166.5k'; l: $2000; p: $2000; crc: $CA13CB11));
  bankpanic_bg: array [0 .. 5] of tipo_roms = ((n: 'epr-6172.5b'; l: $2000; p: 0; crc: $C4C4878B), (n: 'epr-6171.5d'; l: $2000; p: $2000; crc: $A18165A1), (n: 'epr-6170.5e'; l: $2000; p: $4000; crc: $B58AA8FA), (n: 'epr-6169.5f'; l: $2000; p: $6000; crc: $1AA37FCE),
    (n: 'epr-6168.5h'; l: $2000; p: $8000; crc: $05F3A867), (n: 'epr-6167.5i'; l: $2000; p: $A000; crc: $3FA337E1));
  bankpanic_prom: array [0 .. 2] of tipo_roms = ((n: 'pr-6177.8a'; l: $20; p: 0; crc: $EB70C5AE), (n: 'pr-6178.6f'; l: $100; p: $20; crc: $0ACCA001), (n: 'pr-6179.5a'; l: $100; p: $120; crc: $E53BAFDB));
  bankpanic_dip: array [0 .. 7] of def_dip2 = ((mask: $3; name: 'Coin A'; number: 4; val4: (3, 2, 0, 1); name4: ('3C 1C', '2C 1C', '1C 1C', '1C 2C')), (mask: $4; name: 'Coin B'; number: 2; val2: (4, 0); name2: ('2C 1C', '1C 1C')), (mask: $8; name: 'Lives'; number: 2;
    val2: (0, 8); name2: ('3', '4')), (mask: $10; name: 'Bonus Life'; number: 2; val2: (0, $10); name2: ('70K 200K 500K', '100K 400K 800K')), (mask: $20; name: 'Difficulty'; number: 2; val2: (0, $20); name2: ('Easy', 'Hard')), (mask: $40; name: 'Demo Sounds'; number: 2;
    val2: (0, $40); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: ($80, 0); name2: ('Upright', 'Cocktail')), ());
  combathawk_rom: array [0 .. 3] of tipo_roms = ((n: 'epr-10904.7e'; l: $4000; p: 0; crc: $4B106335), (n: 'epr-10905.7f'; l: $4000; p: $4000; crc: $A76FC390), (n: 'epr-10906.7h'; l: $4000; p: $8000; crc: $16D54885), (n: 'epr-10903.7d'; l: $2000; p: $C000; crc: $B7A59CAB));
  combathawk_char: array [0 .. 1] of tipo_roms = ((n: 'epr-10914.5l'; l: $2000; p: 0; crc: $7D7A2340), (n: 'epr-10913.5k'; l: $2000; p: $2000; crc: $D5C1A8AE));
  combathawk_bg: array [0 .. 5] of tipo_roms = ((n: 'epr-10907.5b'; l: $2000; p: 0; crc: $08E5EEA3), (n: 'epr-10908.5d'; l: $2000; p: $2000; crc: $D9E413F5), (n: 'epr-10909.5e'; l: $2000; p: $4000; crc: $FEC7962C), (n: 'epr-10910.5f'; l: $2000; p: $6000; crc: $33DB0FA7),
    (n: 'epr-10911.5h'; l: $2000; p: $8000; crc: $565D9E6D), (n: 'epr-10912.5i'; l: $2000; p: $A000; crc: $CBE22738));
  combathawk_prom: array [0 .. 2] of tipo_roms = ((n: 'pr-10900.8a'; l: $20; p: 0; crc: $F95FCD66), (n: 'pr-10901.6f'; l: $100; p: $20; crc: $6FD981C8), (n: 'pr-10902.5a'; l: $100; p: $120; crc: $84D6BDED));
  combathawk_dip: array [0 .. 6] of def_dip2 = ((mask: $1; name: 'Flip Screen'; number: 2; val2: (0, 1); name2: ('Off', 'On')), (mask: $6; name: 'Coinage'; number: 4; val4: (6, 0, 2, 4); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $8; name: 'Lives'; number: 2;
    val2: (0, 8); name2: ('3', '4')), (mask: $10; name: 'Cabinet'; number: 2; val2: ($10, 0); name2: ('Upright', 'Cocktail')), (mask: $40; name: 'Difficulty'; number: 2; val2: (0, $40); name2: ('Easy', 'Hard')), (mask: $80; name: 'Fuel'; number: 2; val2: (0, $80);
    name2: ('120 Units', '90 Units')), ());

var
  priority, display_on, nmi_vblank: boolean;
  color_hi, scroll_x: byte;

procedure update_video_bankpanic;
var
  x, y, atrib: byte;
  f, nchar, color: word;
  flip_x: boolean;
begin
  if display_on then
  begin
    for f := 0 to $3FF do
    begin
      y := f shr 5;
      x := f and $1F;
      // Background
      if gfx[1].buffer[f] then
      begin
        atrib := memory[$FC00 + f];
        color := (atrib shr 4) or (color_hi shl 4);
        nchar := memory[$F800 + f] + ((atrib and $7) shl 8);
        flip_x := (atrib and $8) <> 0;
        if priority then
          put_gfx_mask_flip(x * 8, y * 8, nchar, (color shl 3) + 256, 2, 1, 0, $F, flip_x, false)
        else
          put_gfx_flip(x * 8, y * 8, nchar, (color shl 3) + 256, 2, 1, flip_x, false);
        gfx[1].buffer[f] := false;
      end;
      // Foreground
      if gfx[0].buffer[f] then
      begin
        atrib := memory[$F400 + f];
        color := (atrib shr 3) or (color_hi shl 5);
        nchar := memory[$F000 + f] + ((atrib and $3) shl 8);
        flip_x := (atrib and $4) <> 0;
        if priority then
          put_gfx_flip(x * 8, y * 8, nchar, color shl 2, 1, 0, flip_x, false)
        else
          put_gfx_mask_flip(x * 8, y * 8, nchar, color shl 2, 1, 0, 0, $1F, flip_x, false);
        gfx[0].buffer[f] := false;
      end;
    end;
    if not(priority) then
    begin
      update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
      scroll__x(1, 3, scroll_x);
    end
    else
    begin
      scroll__x(1, 3, scroll_x);
      update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
    end;
  end
  else
    fill_full_screen(3, $400);
  update_final_piece(24, 16, 224, 224, 3);
end;

procedure events_bankpanic;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // p2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
    // System
    if p_contrls.map_arcade.but2[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
  end;
end;

// needs pause
procedure bankpanic_loop;
var
  f:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
  for f:=0 to 255 do begin
    events_bankpanic;
    if f=240 then begin
      if nmi_vblank then z80_0.change_nmi(PULSE_LINE);
      update_video_bankpanic;
    end;
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
  end;
  video_sync;
  end;
end;

function bankpanic_getbyte(direccion: word): byte;
begin
  bankpanic_getbyte := memory[direccion];
end;

procedure bankpanic_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ; // ROM
    $E000 .. $EFFF:
      memory[direccion] := valor;
    $F000 .. $F7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $F800 .. $FFFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function bankpanic_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      bankpanic_inbyte := marcade.in0;
    1:
      bankpanic_inbyte := marcade.in1;
    2:
      bankpanic_inbyte := marcade.in2;
    4:
      bankpanic_inbyte := marcade.dswa;
  end;
end;

procedure bankpanic_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      sn_76496_0.write(valor);
    1:
      sn_76496_1.write(valor);
    2:
      sn_76496_2.write(valor);
    5:
      scroll_x := valor;
    7:
      begin
        if priority <> ((valor and $2) <> 0) then
        begin
          priority := (valor and $2) <> 0;
          fillchar(gfx[0].buffer, $400, 1);
          fillchar(gfx[1].buffer, $400, 1);
        end;
        display_on := (valor and 4) <> 0;
        if color_hi <> ((valor and 8) shr 3) then
        begin
          color_hi := (valor and 8) shr 3;
          fillchar(gfx[0].buffer, $400, 1);
          fillchar(gfx[1].buffer, $400, 1);
        end;
        nmi_vblank := (valor and $10) <> 0;
      end;
  end;
end;

procedure bankpanic_update_sound;
begin
  sn_76496_0.update;
  sn_76496_1.update;
  sn_76496_2.update;
end;

// Main
procedure bankpanic_reset;
begin
  z80_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  sn_76496_2.reset;
frame_main:=z80_0.tframes;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  nmi_vblank := false;
  scroll_x := 0;
  color_hi := $FF;
  display_on := true;
  priority := false;
end;

function start_bankpanic: boolean;
const
  pc_x: array [0 .. 7] of dword = (8 * 8 + 3, 8 * 8 + 2, 8 * 8 + 1, 8 * 8 + 0, 3, 2, 1, 0);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  pt_x: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
var
  memory_temp: array [0 .. $FFFF] of byte;
  colores: tpaleta;
  bit0, bit1, bit2, f: byte;
  index: word;
  procedure chars_gfx;
  begin
    // convertir fg
    init_gfx(0, 8, 8, $400);
    gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
    convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  end;
  procedure tiles_gfx;
  begin
    // convertir bg
    init_gfx(1, 8, 8, $800);
    gfx_set_desc_data(3, 0, 8 * 8, 0, 2048 * 8 * 8, 2048 * 2 * 8 * 8);
    convert_gfx(1, 0, @memory_temp, @pt_x, @pc_y, false, false);
  end;

begin
  machine_calls.general_loop := bankpanic_loop;
  machine_calls.reset := bankpanic_reset;
  machine_calls.fps_max := 61.034091;
  start_bankpanic := false;
  start_audio(false);
  if main_vars.machine_type = 362 then
    main_screen.rot270_screen := true;
  screen_init(1, 256, 256, true);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  start_video(224, 224);
  // Main CPU
  z80_0 := cpu_z80.create(15468480 div 6, 256);
  z80_0.change_ram_calls(bankpanic_getbyte, bankpanic_putbyte);
  z80_0.change_io_calls(bankpanic_inbyte, bankpanic_outbyte);
  z80_0.init_sound(bankpanic_update_sound);
  // Sound Chip
  sn_76496_0 := sn76496_chip.create(15468480 div 6);
  sn_76496_1 := sn76496_chip.create(15468480 div 6);
  sn_76496_2 := sn76496_chip.create(15468480 div 6);
  case main_vars.machine_type of
    361:
      begin // Bank Panic
        if not(roms_load(@memory, bankpanic_rom)) then
          exit;
        if not(roms_load(@memory_temp, bankpanic_char)) then
          exit;
        chars_gfx;
        if not(roms_load(@memory_temp, bankpanic_bg)) then
          exit;
        tiles_gfx;
        if not(roms_load(@memory_temp, bankpanic_prom)) then
          exit;
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val2 := @bankpanic_dip;
      end;
    362:
      begin // Combat Hawk
        if not(roms_load(@memory, combathawk_rom)) then
          exit;
        if not(roms_load(@memory_temp, combathawk_char)) then
          exit;
        chars_gfx;
        if not(roms_load(@memory_temp, combathawk_bg)) then
          exit;
        tiles_gfx;
        if not(roms_load(@memory_temp, combathawk_prom)) then
          exit;
        // DIP
        marcade.dswa := $10;
        marcade.dswa_val2 := @combathawk_dip;
      end;
  end;
  // color
  for f := 0 to $1F do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and $1;
    bit1 := (memory_temp[f] shr 1) and $1;
    bit2 := (memory_temp[f] shr 2) and $1;
    colores[f].r := $21 * bit0 + $47 * bit1 + $97 * bit2;
    // green component
    bit0 := (memory_temp[f] shr 3) and $1;
    bit1 := (memory_temp[f] shr 4) and $1;
    bit2 := (memory_temp[f] shr 5) and $1;
    colores[f].g := $21 * bit0 + $47 * bit1 + $97 * bit2;
    // blue component
    bit1 := (memory_temp[f] shr 6) and $1;
    bit2 := (memory_temp[f] shr 7) and $1;
    colores[f].b := 0 + $47 * bit1 + $97 * bit2;
  end;
  set_pal(colores, $20);
  for f := 0 to 255 do
  begin
    index := ((f shl 1) and $100) or (f and $7F);
    gfx[0].colores[index] := memory_temp[$20 + index] and $F;
    gfx[1].colores[index] := memory_temp[$20 + index] and $F;
    gfx[0].colores[index or $80] := (memory_temp[$20 + index] and $F) or $10;
    gfx[1].colores[index or $80] := (memory_temp[$20 + index] and $F) or $10;
  end;
  // final
  start_bankpanic := true;
end;

end.
