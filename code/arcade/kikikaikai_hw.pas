unit kikikaikai_hw;

interface

uses WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  m680x,
  rom_engine,
  pal_engine,
  sound_engine;

function start_kikikaikai: boolean;

implementation

const
  kikikaikai_rom: array [0 .. 1] of tipo_roms = ((n: 'a85-17.h16'; l: $10000; p: 0; crc: $C141D5AB), (n: 'a85-16.h18'; l: $10000; p: $10000; crc: $4094D750));
  kikikaikai_snd: tipo_roms = (n: 'a85-11.f6'; l: $8000; p: 0; crc: $CC3539DB);
  kikikaikai_mcu_rom: tipo_roms = (n: 'a85-01_jph1020p.h8'; l: $1000; p: $0; crc: $01771197);
  kikikaikai_chars: array [0 .. 3] of tipo_roms = ((n: 'a85-15.a1'; l: $10000; p: 0; crc: $AEBC8C32), (n: 'a85-14.a3'; l: $10000; p: $10000; crc: $A9DF0453), (n: 'a85-13.a4'; l: $10000; p: $20000; crc: $3EEAF878), (n: 'a85-12.a6'; l: $10000; p: $30000; crc: $91E58067));
  kikikaikai_prom: array [0 .. 2] of tipo_roms = ((n: 'a85-08.g15'; l: $100; p: 0; crc: $D15F61A8), (n: 'a85-10.g12'; l: $100; p: $100; crc: $8FC3FA86), (n: 'a85-09.g14'; l: $100; p: $200; crc: $B931C94D));
  // Dip
  kikikaikai_dip_a: array [0 .. 4] of def_dip2 = ((mask: $1; name: 'Cabinet'; number: 2; val2: (0, 1); name2: ('Upright', 'Cocktail')), (mask: $2; name: 'Flip Screen'; number: 2; val2: (2, 0); name2: ('Off', 'On')), (mask: $30; name: 'Coin A'; number: 4; val4: ($30, $20, $10, 0);
    name4: ('1C 1C', '1C 2C', '2C 1C/3C 1C', '2C 3C/4C 1C')), (mask: $C0; name: 'Coin B'; number: 4; val4: ($C0, $80, $40, 0); name4: ('1C 1C', '1C 2C', '2C 1C/1C 4C', '2C 3C/1C 6C')), ());
  kikikaikai_dip_b: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Difficulty'; number: 4; val4: (2, 3, 1, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $C; name: 'Bonus Life'; number: 4; val4: (0, $C, 8, 4); name4: ('50K 100K', '70K 150K', '70K 200K', '100K 300K')),
    (mask: $30; name: 'Lives'; number: 4; val4: (0, $30, $20, $10); name4: ('2', '3', '4', '5')), (mask: $40; name: 'Coinage Type'; number: 2; val2: ($40, 0); name2: ('Type 1', 'Type 2')), (mask: $80; name: 'Number Match'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  kickrun_rom: array [0 .. 1] of tipo_roms = ((n: 'a87-23.h16'; l: $10000; p: 0; crc: $37182560), (n: 'a87-22.h18'; l: $10000; p: $10000; crc: $3B5A8354));
  kickrun_snd: tipo_roms = (n: 'a87-06.f6'; l: $8000; p: 0; crc: $1625B587);
  kickrun_sub: tipo_roms = (n: 'a87-09-1'; l: $4000; p: 0; crc: $6A2AD32F);
  kickrun_mcu_rom: tipo_roms = (n: 'a87-01_jph1021p.h8'; l: $1000; p: $0; crc: $9451E880);
  kickrun_chars: array [0 .. 3] of tipo_roms = ((n: 'a87-05.a1'; l: $10000; p: 0; crc: $4EEE3A8A), (n: 'a87-04.a3'; l: $8000; p: $10000; crc: $8B438D20), (n: 'a87-03.a4'; l: $10000; p: $20000; crc: $F42E8A88), (n: 'a87-02.a6'; l: $8000; p: $30000; crc: $64F1A85F));
  kickrun_prom: array [0 .. 2] of tipo_roms = ((n: 'a87-10.g15'; l: $100; p: 0; crc: $BE6EB1F0), (n: 'a87-12.g12'; l: $100; p: $100; crc: $3E953444), (n: 'a87-11.g14'; l: $100; p: $200; crc: $14F6C28D));
  // Dip
  kickrun_dip_a: array [0 .. 4] of def_dip2 = ((mask: $1; name: 'Master/Slave Mode'; number: 2; val2: (1, 0); name2: ('Off', 'On')), (mask: $8; name: 'Demo Sounds'; number: 2; val2: (0, 8); name2: ('Off', 'On')), (mask: $30; name: 'Coin A'; number: 4; val4: ($10, $30, 0, $20);
    name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $C0; name: 'Coin B'; number: 4; val4: ($40, $C0, 0, $80); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), ());
  kickrun_dip_b: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Difficulty'; number: 4; val4: (3, 2, 1, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $C; name: 'Playing Time'; number: 4; val4: (0, $C, 8, 4);
    name4: ('40 Seconds', '60 Seconds', '80 Seconds', '100 Seconds')), (mask: $20; name: 'Board ID'; number: 2; val2: ($20, 0); name2: ('Master', 'Slave')), (mask: $40; name: 'Number of Matches'; number: 2; val2: (0, $40); name2: ('2', '6')), (mask: $80;
    name: 'Single board 4 Players Mode'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  memoria_rom: array [0 .. 5, $0 .. $3FFF] of byte;
  banco_rom, banco_char: byte;
  mcu_port3_in, mcu_port2_out, mcu_port3_out, mcu_port4_out: byte;

procedure update_video_kikikaikai;
var
  nchar, color, sy, sx, goffs, gfx_offs, offs: word;
  yc, y, gfx_num, tx, ty, height: byte;
begin
  fill_full_screen(1, $100);
  sx := 0;
  for offs := $540 to $5FF do
  begin
    if ((memory[$C000 + (offs * 4)] = 0) and (memory[$C001 + (offs * 4)] = 0) and (memory[$C002 + (offs * 4)] = 0) and (memory[$C003 + (offs * 4)] = 0)) then
      continue;
    ty := memory[$C000 + (offs * 4)];
    gfx_num := memory[$C001 + (offs * 4)];
    tx := memory[$C002 + (offs * 4)];
    if (gfx_num and $80) <> 0 then
    begin
      gfx_offs := (gfx_num and $3F) shl 7;
      height := 32;
      if (gfx_num and $40) <> 0 then
        sx := sx + 16
      else
        sx := tx;
    end
    else
    begin
      if not((ty <> 0) and (tx <> 0)) then
        continue;
      gfx_offs := ((gfx_num and $1F) shl 7) + ((gfx_num and $60) shr 1) + 12;
      height := 2;
      sx := tx;
    end;
    sy := 256 - (height shl 3) - ty;
    for yc := 0 to (height - 1) do
    begin
      y := (sy + ((yc * 2) shl 2)) and $FF;
      goffs := gfx_offs + (yc * 2);
      nchar := memory[$C000 + goffs] + ((memory[$C001 + goffs] and $1F) shl 8);
      color := (memory[$C001 + goffs] and $E0) shr 5;
      goffs := goffs + $40;
      put_gfx_sprite(nchar, color shl 4, false, false, 0);
      update_gfx_sprite(sx and $FF, y, 1, 0);
      nchar := memory[$C000 + goffs] + ((memory[$C001 + goffs] and $1F) shl 8);
      color := (memory[$C001 + goffs] and $E0) shr 5;
      put_gfx_sprite(nchar, color shl 4, false, false, 0);
      update_gfx_sprite((sx + 8) and $FF, y, 1, 0);
    end;
  end;
  update_final_piece(0, 16, 256, 224, 1);
end;

procedure update_video_kickrun;
var
  nchar, color, sy, sx, x, goffs, gfx_offs, offs: word;
  yc, y, gfx_num, height, xc, gfx_attr: byte;
  flipx: boolean;
begin
  fill_full_screen(1, $100);
  sx := 0;
  for offs := $540 to $7FF do
  begin
    if ((offs >= $600) and (offs < $660)) then
      continue;
    if (offs >= $670) then
      continue;
    if ((memory[$C000 + (offs * 4)] = 0) and (memory[$C001 + (offs * 4)] = 0) and (memory[$C002 + (offs * 4)] = 0) and (memory[$C003 + (offs * 4)] = 0)) then
      continue;
    gfx_num := memory[$C001 + (offs * 4)];
    gfx_attr := memory[$C003 + (offs * 4)];
    if (gfx_num and $80) = 0 then
    begin // 16x16 sprites
      gfx_offs := ((gfx_num and $1F) * $80) + ((gfx_num and $60) shr 1) + 12;
      height := 2;
    end
    else
    begin // tilemaps (each sprite is a 16x256 column)
      gfx_offs := ((gfx_num and $3F) * $80);
      height := 32;
    end;
    if ((gfx_num and $C0) = $C0) then
    begin // next column
      sx := sx + 16;
    end
    else
    begin
      sx := memory[$C002 + (offs * 4)];
      // if (gfx_attr & 0x40) sx -= 256;
    end;
    sy := 256 - height * 8 - (memory[$C000 + (offs * 4)]);
    for xc := 0 to 1 do
    begin
      for yc := 0 to (height - 1) do
      begin
        goffs := gfx_offs + xc * $40 + yc * 2;
        nchar := memory[$C000 + goffs] + ((memory[$C001 + goffs] and 7) shl 8) + ((memory[$C001 + goffs] and $80) shl 4) + (banco_char shl 12);
        color := ((memory[$C001 + goffs] and $38) shr 3) + ((gfx_attr and 2) shl 2);
        flipx := (memory[$C001 + goffs] and $40) <> 0;
        // const int x = sx + xc * 8;
        x := (sx + xc * 8) and $FF;
        y := (sy + yc * 8) and $FF;
        put_gfx_sprite(nchar, color shl 4, flipx, false, 0);
        update_gfx_sprite(x, y, 1, 0);
      end;
    end;
  end;
  update_final_piece(0, 16, 256, 224, 1);
end;

procedure events_kikikaikai;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // MISC
    if p_contrls.map_arcade.start[0] then
      marcade.in3 := (marcade.in3 and $F7)
    else
      marcade.in3 := (marcade.in3 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in3 := (marcade.in3 and $EF)
    else
      marcade.in3 := (marcade.in3 or $10);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
  end;
end;

procedure kikikaikai_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 263 do
    begin
      events_kikikaikai;
      if f = 240 then
      begin
        update_video_kikikaikai;
        z80_0.change_irq(HOLD_LINE);
        z80_1.change_irq(HOLD_LINE);
        m6800_0.change_irq(HOLD_LINE);
      end;
      // main
      z80_0.run(frame_main);
      frame_main := frame_main + z80_0.tframes - z80_0.contador;
      // sound
      z80_1.run(frame_snd);
      frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
      // mcu
      m6800_0.run(frame_mcu);
      frame_mcu := frame_mcu + m6800_0.tframes - m6800_0.contador;
    end;
    video_sync;
  end;
end;

function kikikaikai_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $EFFF, $F800 .. $FFFF:
      kikikaikai_getbyte := memory[direccion];
    $8000 .. $BFFF:
      kikikaikai_getbyte := memoria_rom[banco_rom, (direccion and $3FFF)];
    $F010:
      kikikaikai_getbyte := marcade.in3;
  end;
end;

procedure kikikaikai_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $EFFF, $F800 .. $FFFF:
      memory[direccion] := valor;
    $F000:
      begin
        banco_rom := valor and 7;
        banco_char := (valor shr 5) and 1;
      end;
    $F008:
      begin
        main_screen.flip_main_screen := (valor and $1) <> 0;
        if (valor and 4) <> 0 then
          z80_1.change_reset(CLEAR_LINE)
        else
          z80_1.change_reset(ASSERT_LINE);
        if (valor and 2) <> 0 then
          m6800_0.change_reset(CLEAR_LINE)
        else
          m6800_0.change_reset(ASSERT_LINE);
      end;
  end;
end;

function kikisnd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $A800 .. $BFFF:
      kikisnd_getbyte := mem_snd[direccion];
    $8000 .. $A7FF:
      kikisnd_getbyte := memory[$C000 + (direccion and $7FFF)];
    $C000:
      kikisnd_getbyte := ym2203_0.status;
    $C001:
      kikisnd_getbyte := ym2203_0.read;
  end;
end;

procedure kikisnd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $A7FF:
      memory[$C000 + (direccion and $7FFF)] := valor;
    $A800 .. $BFFF:
      mem_snd[direccion] := valor;
    $C000:
      ym2203_0.control(valor);
    $C001:
      ym2203_0.write(valor);
  end;
end;

function kikikai_port_a_read: byte;
begin
  kikikai_port_a_read := marcade.dswa;
end;

function kikikai_port_b_read: byte;
begin

  kikikai_port_b_read := marcade.dswb;
end;

procedure kiki_sound_update;
begin
  ym2203_0.update;
end;

function mcu_port1_r: byte;
begin
  mcu_port1_r := marcade.in0;
end;

function mcu_port3_r: byte;
begin
  mcu_port3_r := mcu_port3_in;
end;

procedure mcu_port2_w(valor: byte);
begin
  if (((mcu_port2_out and $4) <> 0) and ((not(valor) and $4) <> 0)) then
  begin
    if (valor and $10) <> 0 then
    begin // read
      if (valor and 1) <> 0 then
      begin
        mcu_port3_in := memory[$E800 + mcu_port4_out];
      end
      else
      begin
        case (mcu_port4_out and $1) of
          0:
            mcu_port3_in := marcade.in1;
          1:
            mcu_port3_in := marcade.in2;
        end;
      end
    end
    else
    begin // write
      memory[$E800 + mcu_port4_out] := mcu_port3_out;
    end;
  end;
  mcu_port2_out := valor;
end;

procedure mcu_port3_w(valor: byte);
begin
  mcu_port3_out := valor;
end;

procedure mcu_port4_w(valor: byte);
begin
  mcu_port4_out := valor;
end;

// kick and run
// needs pause
procedure kickandrun_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 263 do
    begin
      events_kikikaikai;
      if f = 240 then
      begin
        update_video_kickrun;
        z80_0.change_irq(ASSERT_LINE);
        z80_1.change_irq(HOLD_LINE);
        z80_2.change_irq(HOLD_LINE);
        m6800_0.change_irq(HOLD_LINE);
      end;
      // main
      z80_0.run(frame_main);
      frame_main := frame_main + z80_0.tframes - z80_0.contador;
      // sound
      z80_1.run(frame_snd);
      frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
      // sub
      z80_2.run(frame_sub);
      frame_sub := frame_sub + z80_2.tframes - z80_2.contador;
      // mcu
      m6800_0.run(frame_mcu);
      frame_mcu := frame_mcu + m6800_0.tframes - m6800_0.contador;
    end;
    video_sync;
  end;
end;

function kickrun_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF:
      kickrun_sub_getbyte := mem_misc[direccion];
    $8000 .. $87FF:
      kickrun_sub_getbyte := memory[$F800 + (direccion and $7FF)];
    $C000:
      kickrun_sub_getbyte := $FF;
    $C001:
      kickrun_sub_getbyte := $FF;
    $C002:
      kickrun_sub_getbyte := $FF;
    $C003:
      kickrun_sub_getbyte := $FF;
  end;
end;

procedure kickrun_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $47FF:
      mem_misc[direccion] := valor;
    $8000 .. $87FF:
      memory[$F800 + (direccion and $7FF)] := valor;
    $C004:
      ;
  end;
end;

function kickrun_irq_vector: byte;
begin
  z80_0.change_irq(CLEAR_LINE);
  kickrun_irq_vector := memory[$E800];
end;

// Main
procedure reset_kikikaikai;
begin
  z80_0.reset;
  z80_1.reset;
  if main_vars.machine_type = 389 then
  begin
    z80_2.change_reset(ASSERT_LINE);
    frame_sub := z80_2.tframes;
  end;
  m6800_0.reset;
  ym2203_0.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  frame_mcu := m6800_0.tframes;
  banco_rom := 0;
  banco_char := 0;
  marcade.in0 := $0;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  marcade.in3 := $FF;
  mcu_port3_in := 0;
  mcu_port2_out := 0;
  mcu_port3_out := 0;
  mcu_port4_out := 0;
end;

function start_kikikaikai: boolean;
var
  f: byte;
  memoria_temp, memoria_temp2: array [0 .. $3FFFF] of byte;
  colores: tpaleta;
  procedure convert_chars;
  const
    pc_x: array [0 .. 7] of dword = (3, 2, 1, 0, 8 + 3, 8 + 2, 8 + 1, 8 + 0);
    pc_y: array [0 .. 7] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16);
  begin
    init_gfx(0, 8, 8, $2000);
    gfx[0].trans[15] := true;
    gfx_set_desc_data(4, 0, 16 * 8, $20000 * 8 + 0, $20000 * 8 + 4, 0, 4);
    convert_gfx(0, 0, @memoria_temp, @pc_x, @pc_y, false, false, true);
  end;

begin
  machine_calls.reset := reset_kikikaikai;
  machine_calls.fps_max := 59.185606;
  start_kikikaikai := false;
  start_audio(false);
  if main_vars.machine_type = 388 then
    main_screen.rot90_screen := true;
  screen_init(1, 512, 512, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(6000000, 264);
  z80_0.change_ram_calls(kikikaikai_getbyte, kikikaikai_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(6000000, 264);
  z80_1.change_ram_calls(kikisnd_getbyte, kikisnd_putbyte);
  z80_1.init_sound(kiki_sound_update);
  // MCU
  m6800_0 := cpu_m6800.create(3000000, 264, TCPU_M6801);
  m6800_0.change_io_calls(mcu_port1_r, nil, mcu_port3_r, nil, nil, mcu_port2_w, mcu_port3_w, mcu_port4_w);
  // Sound Chip
  ym2203_0 := ym2203_chip.create(3000000, 1, 0.3);
  ym2203_0.change_io_calls(kikikai_port_a_read, kikikai_port_b_read, nil, nil);
  case main_vars.machine_type of
    388:
      begin // kiki kaikai
        machine_calls.general_loop := kikikaikai_loop;
        if not(roms_load(@memoria_temp, kikikaikai_rom)) then
          exit;
        // poner las roms y los bancos de rom
        copymemory(@memory, @memoria_temp, $8000);
        copymemory(@memoria_rom[0, 0], @memoria_temp[$10000], $4000);
        copymemory(@memoria_rom[1, 0], @memoria_temp[$14000], $4000);
        copymemory(@memoria_rom[2, 0], @memoria_temp[$18000], $4000);
        copymemory(@memoria_rom[3, 0], @memoria_temp[$1C000], $4000);
        copymemory(@memoria_rom[4, 0], @memoria_temp[$8000], $4000);
        copymemory(@memoria_rom[5, 0], @memoria_temp[$C000], $4000);
        // Sound
        if not(roms_load(@mem_snd, kikikaikai_snd)) then
          exit;
        // MCU
        if not(roms_load(m6800_0.get_rom_addr, kikikaikai_mcu_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memoria_temp, kikikaikai_chars)) then
          exit;
        convert_chars;
        // DIP
        marcade.dswa := $3E;
        marcade.dswb := $7F;
        marcade.dswa_val2 := @kikikaikai_dip_a;
        marcade.dswb_val2 := @kikikaikai_dip_b;
        // Paleta
        if not(roms_load(@memoria_temp, kikikaikai_prom)) then
          exit;
      end;
    389:
      begin // kick and run
        machine_calls.general_loop := kickandrun_loop;
        z80_0.change_misc_calls(nil, nil, nil, kickrun_irq_vector);
        if not(roms_load(@memoria_temp, kickrun_rom)) then
          exit;
        // poner las roms y los bancos de rom
        copymemory(@memory, @memoria_temp, $8000);
        copymemory(@memoria_rom[0, 0], @memoria_temp[$10000], $4000);
        copymemory(@memoria_rom[1, 0], @memoria_temp[$14000], $4000);
        copymemory(@memoria_rom[2, 0], @memoria_temp[$18000], $4000);
        copymemory(@memoria_rom[3, 0], @memoria_temp[$1C000], $4000);
        copymemory(@memoria_rom[4, 0], @memoria_temp[$8000], $4000);
        copymemory(@memoria_rom[5, 0], @memoria_temp[$C000], $4000);
        // Sound
        if not(roms_load(@mem_snd, kickrun_snd)) then
          exit;
        // Main CPU
        z80_2 := cpu_z80.create(4000000, 264);
        if not(roms_load(@mem_misc, kickrun_sub)) then
          exit;
        z80_2.change_ram_calls(kickrun_sub_getbyte, kickrun_sub_putbyte);
        // MCU
        if not(roms_load(m6800_0.get_rom_addr, kickrun_mcu_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memoria_temp2, kickrun_chars)) then
          exit;
        copymemory(@memoria_temp[$8000], @memoria_temp2[0], $8000);
        copymemory(@memoria_temp[0], @memoria_temp2[$8000], $8000);
        copymemory(@memoria_temp[$10000], @memoria_temp2[$10000], $8000);
        copymemory(@memoria_temp[$18000], @memoria_temp2[$10000], $8000);
        copymemory(@memoria_temp[$28000], @memoria_temp2[$20000], $8000);
        copymemory(@memoria_temp[$20000], @memoria_temp2[$28000], $8000);
        copymemory(@memoria_temp[$30000], @memoria_temp2[$30000], $8000);
        copymemory(@memoria_temp[$38000], @memoria_temp2[$30000], $8000);
        convert_chars;
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $FB;
        marcade.dswa_val2 := @kickrun_dip_a;
        marcade.dswb_val2 := @kickrun_dip_b;
        // Paleta
        if not(roms_load(@memoria_temp, kickrun_prom)) then
          exit;
      end;
  end;
  // Paleta
  for f := 0 to 255 do
  begin
    colores[f].r := pal4bit(memoria_temp[f]);
    colores[f].g := pal4bit(memoria_temp[f + $100]);
    colores[f].b := pal4bit(memoria_temp[f + $200]);
  end;
  set_pal(colores, 256);
  // final
  start_kikikaikai := true;
end;

end.
