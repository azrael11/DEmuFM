unit gyruss_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m6809,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  ay_8910,
  sound_engine,
  konami_decrypt,
  mcs48,
  dac;

function start_gyruss: boolean;

implementation

const
  gyruss_rom: array [0 .. 2] of tipo_roms = ((n: 'gyrussk.1'; l: $2000; p: 0; crc: $C673B43D), (n: 'gyrussk.2'; l: $2000; p: $2000; crc: $A4EC03E4), (n: 'gyrussk.3'; l: $2000; p: $4000; crc: $27454A98));
  gyruss_sub: tipo_roms = (n: 'gyrussk.9'; l: $2000; p: $E000; crc: $822BF27E);
  gyruss_sound: array [0 .. 1] of tipo_roms = ((n: 'gyrussk.1a'; l: $2000; p: 0; crc: $F4AE1C17), (n: 'gyrussk.2a'; l: $2000; p: $2000; crc: $BA498115));
  gyruss_sound_sub: tipo_roms = (n: 'gyrussk.3a'; l: $1000; p: 0; crc: $3F9B5DEA);
  gyruss_char: tipo_roms = (n: 'gyrussk.4'; l: $2000; p: 0; crc: $27D8329B);
  gyruss_sprites: array [0 .. 3] of tipo_roms = ((n: 'gyrussk.6'; l: $2000; p: 0; crc: $C949DB10), (n: 'gyrussk.5'; l: $2000; p: $2000; crc: $4F22411A), (n: 'gyrussk.8'; l: $2000; p: $4000; crc: $47CD1FBC), (n: 'gyrussk.7'; l: $2000; p: $6000; crc: $8E8D388C));
  gyruss_pal: array [0 .. 2] of tipo_roms = ((n: 'gyrussk.pr3'; l: $20; p: 0; crc: $98782DB3), (n: 'gyrussk.pr1'; l: $100; p: $20; crc: $7ED057DE), (n: 'gyrussk.pr2'; l: $100; p: $120; crc: $DE823A81));
  // Dip
  gyruss_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), ());
  gyruss_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '255')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: 8; name: 'Bonus Life'; number: 2; val2: (8, 0);
    name2: ('30K 90K 60K+', '40K 110K 70K+')), (mask: $70; name: 'Difficulty'; number: 8; val8: ($70, $60, $50, $40, $30, $20, $10, 0); name8: ('1 (Easiest)', '2', '3', '4', '5 (Average)', '6', '7', '8 (Hardest)')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());
  gyruss_dip_c: array [0 .. 1] of def_dip2 = ((mask: 1; name: 'Demo Music'; number: 2; val2: (1, 0); name2: ('Off', 'On')), ());

var
  scan_line, sound_latch, sound_latch2: byte;
  main_nmi, sub_irq: boolean;
  mem_opcodes: array [0 .. $1FFF] of byte;
  mem_sound_sub: array [0 .. $FFF] of byte;

procedure update_video_gyruss;
var
  x, y, atrib: byte;
  f, nchar, color: word;
  flipx, flipy: boolean;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory[$8000 + f];
      color := (atrib and $F) shl 2;
      nchar := memory[$8400 + f] + ((atrib and $20) shl 3);
      flipx := (atrib and $80) <> 0;
      flipy := (atrib and $40) <> 0;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, flipx, flipy);
      if (atrib and $10) <> 0 then
        put_gfx_flip(x * 8, y * 8, nchar, color, 2, 0, flipx, flipy)
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  for f := $2F downto 0 do
  begin
    atrib := mem_misc[$4042 + (f * 4)];
    nchar := (mem_misc[$4041 + (f * 4)] shr 1) + ((atrib and $20) shl 2) + ((mem_misc[$4041 + (f * 4)] and 1) shl 8);
    color := (atrib and $F) shl 4;
    y := mem_misc[$4040 + (f * 4)];
    flipy := (atrib and $40) = 0;
    if main_screen.flip_main_screen then
    begin
      flipy := not(flipy);
      y := not(y);
    end;
    x := mem_misc[$4043 + (f * 4)] - 1;
    put_gfx_sprite_mask(nchar, color, (atrib and $80) <> 0, flipy, 1, 0, $F);
    update_gfx_sprite(x, y, 3, 1);
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(16, 0, 224, 256, 3);
end;

procedure events_gyruss;
begin
  if event.arcade then
  begin
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
  end;
end;

procedure gyruss_loop;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for scan_line := 0 to 255 do
      begin
        events_gyruss;
        if scan_line = 240 then
        begin
          if main_nmi then
            z80_0.change_nmi(ASSERT_LINE);
          if sub_irq then
            m6809_0.change_irq(ASSERT_LINE);
          update_video_gyruss;
        end;
        // main
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // sub
        m6809_0.run(frame_sub);
        frame_sub := frame_sub + m6809_0.tframes - m6809_0.contador;
        // snd
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
        // snd sub
        mcs48_0.run(frame_snd2);
        frame_snd2 := frame_snd2 + mcs48_0.tframes - mcs48_0.contador;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function gyruss_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF, $9000 .. $A7FF:
      gyruss_getbyte := memory[direccion];
    $C000:
      gyruss_getbyte := marcade.dswb;
    $C080:
      gyruss_getbyte := marcade.in2;
    $C0A0:
      gyruss_getbyte := marcade.in0;
    $C0C0:
      gyruss_getbyte := marcade.in1;
    $C0E0:
      gyruss_getbyte := marcade.dswa;
    $C100:
      gyruss_getbyte := marcade.dswc;
  end;
end;

procedure gyruss_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $7FFF:
      ;
    $8000 .. $87FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $9000 .. $A7FF:
      memory[direccion] := valor;
    $C080:
      z80_1.change_irq(HOLD_LINE);
    $C100:
      sound_latch := valor;
    $C180:
      begin
        main_nmi := (valor and 1) <> 0;
        if not(main_nmi) then
          z80_0.change_nmi(CLEAR_LINE);
      end;
    $C185:
      main_screen.flip_main_screen := (valor and 1) <> 0;
  end;
end;

function gyruss_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0:
      gyruss_sub_getbyte := scan_line;
    $6000 .. $67FF:
      gyruss_sub_getbyte := memory[$A000 + (direccion and $7FF)];
    $4000 .. $47FF:
      gyruss_sub_getbyte := mem_misc[direccion];
    $E000 .. $FFFF:
      if m6809_0.opcode then
        gyruss_sub_getbyte := mem_opcodes[direccion and $1FFF]
      else
        gyruss_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure gyruss_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $2000:
      begin
        sub_irq := (valor and 1) <> 0;
        if not(sub_irq) then
          m6809_0.change_irq(CLEAR_LINE);
      end;
    $4000 .. $47FF:
      mem_misc[direccion] := valor;
    $6000 .. $67FF:
      memory[$A000 + (direccion and $7FF)] := valor;
    $E000 .. $FFFF:
      ;
  end;
end;

function gyruss_sound_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $63FF:
      gyruss_sound_getbyte := mem_snd[direccion];
    $8000:
      gyruss_sound_getbyte := sound_latch;
  end;
end;

procedure gyruss_sound_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF:
      ;
    $6000 .. $63FF:
      mem_snd[direccion] := valor;
  end;
end;

function gyruss_sound_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    1:
      gyruss_sound_inbyte := ay8910_0.Read;
    5:
      gyruss_sound_inbyte := ay8910_1.Read;
    9:
      gyruss_sound_inbyte := ay8910_2.Read;
    $D:
      gyruss_sound_inbyte := ay8910_3.Read;
    $11:
      gyruss_sound_inbyte := ay8910_4.Read;
  end;
end;

procedure gyruss_sound_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ay8910_0.Control(valor);
    2:
      ay8910_0.Write(valor);
    4:
      ay8910_1.Control(valor);
    6:
      ay8910_1.Write(valor);
    8:
      ay8910_2.Control(valor);
    $A:
      ay8910_2.Write(valor);
    $C:
      ay8910_3.Control(valor);
    $E:
      ay8910_3.Write(valor);
    $10:
      ay8910_4.Control(valor);
    $12:
      ay8910_4.Write(valor);
    $14:
      mcs48_0.change_irq(ASSERT_LINE);
    $18:
      sound_latch2 := valor;
  end;
end;

function gyruss_sound2_getbyte(direccion: word): byte;
begin
  if direccion < $1000 then
    gyruss_sound2_getbyte := mem_sound_sub[direccion];
end;

function gyruss_sound2_inport(puerto: word): byte;
begin
  if puerto < $100 then
    gyruss_sound2_inport := sound_latch2;
end;

procedure gyruss_sound2_outport(puerto: word; valor: byte);
begin
  case puerto of
    MCS48_PORT_P1:
      dac_0.data8_w(valor);
    MCS48_PORT_P2:
      mcs48_0.change_irq(CLEAR_LINE);
  end;
end;

function gyruss_portar: byte;
const
  gyruss_timer: array [0 .. 9] of byte = (0, 1, 2, 3, 4, 9, $A, $B, $A, $D);
begin
  gyruss_portar := gyruss_timer[(z80_1.totalt div 1024) mod 10];
end;

procedure gyruss_sound_update;
var
  out_left, out_right: integer;
begin
  out_right := ay8910_0.update_internal^ + ay8910_2.update_internal^ + ay8910_3.update_internal^;
  out_left := ay8910_1.update_internal^ + ay8910_4.update_internal^ + dac_0.internal_update;
  if out_right > $7FFF then
    out_right := $7FFF
  else if out_right < -$7FFF then
    out_right := -$7FFF;
  if out_left > $7FFF then
    out_left := $7FFF
  else if out_right < -$7FFF then
    out_left := -$7FFF;
  tsample[ay8910_0.get_sample_num, sound_status.sound_position] := out_left;
  tsample[ay8910_0.get_sample_num, sound_status.sound_position + 1] := out_right;
end;

// Main
procedure gyruss_reset;
begin
  z80_0.reset;
  m6809_0.reset;
  z80_1.reset;
  mcs48_0.reset;
  frame_main := z80_0.tframes;
  frame_sub := m6809_0.tframes;
  frame_snd := z80_1.tframes;
  frame_snd2 := mcs48_0.tframes;
  ay8910_0.reset;
  ay8910_1.reset;
  ay8910_2.reset;
  ay8910_3.reset;
  ay8910_4.reset;
  dac_0.reset;
  main_nmi := false;
  sub_irq := false;
  sound_latch := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
end;

function start_gyruss: boolean;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $7FFF] of byte;
  rgweights: array [0 .. 2] of single;
  bweights: array [0 .. 1] of single;
const
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
begin
  start_gyruss := false;
  machine_calls.general_loop := gyruss_loop;
  machine_calls.reset := gyruss_reset;
  machine_calls.fps_max := 60.606060606060606060;
  start_audio(true);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(18432000 div 6, 256);
  z80_0.change_ram_calls(gyruss_getbyte, gyruss_putbyte);
  if not(roms_load(@memory, gyruss_rom)) then
    exit;
  // Sub CPU
  m6809_0 := cpu_m6809.create(18432000 div 12, 256, TCPU_M6809);
  m6809_0.change_ram_calls(gyruss_sub_getbyte, gyruss_sub_putbyte);
  if not(roms_load(@mem_misc, gyruss_sub)) then
    exit;
  konami1_decode(@mem_misc[$E000], @mem_opcodes[0], $2000);
  // Sound CPU
  z80_1 := cpu_z80.create(14318180 div 4, 256);
  z80_1.change_ram_calls(gyruss_sound_getbyte, gyruss_sound_putbyte);
  z80_1.change_io_calls(gyruss_sound_inbyte, gyruss_sound_outbyte);
  z80_1.init_sound(gyruss_sound_update);
  if not(roms_load(@mem_snd, gyruss_sound)) then
    exit;
  // Sound CPU 2
  mcs48_0 := cpu_mcs48.create(8000000, 256, I8039);
  mcs48_0.change_ram_calls(gyruss_sound2_getbyte, nil);
  mcs48_0.change_io_calls(nil, gyruss_sound2_outport, gyruss_sound2_inport, nil);
  if not(roms_load(@mem_sound_sub, gyruss_sound_sub)) then
    exit;
  // Sound Chip
  ay8910_0 := ay8910_chip.create(14318180 div 8, AY8910, 0.5);
  ay8910_1 := ay8910_chip.create(14318180 div 8, AY8910, 1, true);
  ay8910_2 := ay8910_chip.create(14318180 div 8, AY8910, 1, true);
  ay8910_2.change_io_calls(gyruss_portar, nil, nil, nil);
  ay8910_3 := ay8910_chip.create(14318180 div 8, AY8910, 1, true);
  ay8910_4 := ay8910_chip.create(14318180 div 8, AY8910, 1, true);
  dac_0 := dac_chip.create(1, true);
  // cargar chars
  if not(roms_load(@memory_temp, gyruss_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, true, false);
  // cargar sprites
  if not(roms_load(@memory_temp, gyruss_sprites)) then
    exit;
  init_gfx(1, 8, 16, $200);
  gfx_set_desc_data(4, 2, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @pc_x, @ps_y, true, false);
  gfx_set_desc_data(4, 2, 64 * 8, ($4000 + $10) * 8 + 4, ($4000 + $10) * 8 + 0, ($10 * 8) + 4, ($10 * 8) + 0);
  convert_gfx(1, $100 * 8 * 16, @memory_temp, @pc_x, @ps_y, true, false);
  // paleta de colores
  if not(roms_load(@memory_temp, gyruss_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg[0], @rgweights[0], 0, 0, 2, @resistances_b[0], @bweights[0], 0, 0, 0, nil, nil, 0, 0);
  for f := 0 to 31 do
  begin
    // red component */
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rgweights, bit0, bit1, bit2);
    // green component */
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@rgweights, bit0, bit1, bit2);
    // blue component */
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  // CLUT Sprites y chars
  for f := 0 to $FF do
  begin
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
    gfx[0].colores[f] := (memory_temp[$120 + f] and $F) + $10;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $3B;
  marcade.dswc := $FE;
  marcade.dswa_val2 := @gyruss_dip_a;
  marcade.dswb_val2 := @gyruss_dip_b;
  marcade.dswc_val2 := @gyruss_dip_c;
  // Final
  start_gyruss := true;
end;

end.
