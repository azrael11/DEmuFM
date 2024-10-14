unit dec8_hw;

interface

uses
  WinApi.Windows,
  m6502,
  m6809,
  main_engine,
  controls_engine,
  ym_2203,
  ym_3812,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  misc_functions,
  mcs51,
  timer_engine;

function start_dec8: boolean;

implementation

const
  srd_rom: array [0 .. 1] of tipo_roms = ((n: 'dy01-e.b14'; l: $10000; p: $0; crc: $176E9299), (n: 'dy00.b16'; l: $10000; p: $10000; crc: $2BF6B461));
  srd_char: tipo_roms = (n: 'dy05.b6'; l: $4000; p: $0000; crc: $8780E8A3);
  srd_tiles: array [0 .. 1] of tipo_roms = ((n: 'dy03.b4'; l: $10000; p: $0000; crc: $44F2A4F9), (n: 'dy02.b5'; l: $10000; p: $10000; crc: $522D9A9E));
  srd_snd: tipo_roms = (n: 'dy04.d7'; l: $8000; p: $8000; crc: $2AE3591C);
  srd_sprites: array [0 .. 5] of tipo_roms = ((n: 'dy07.h16'; l: $8000; p: $0000; crc: $97EABA60), (n: 'dy06.h14'; l: $8000; p: $8000; crc: $C279541B), (n: 'dy09.k13'; l: $8000; p: $10000;
    crc: $D30D1745), (n: 'dy08.k11'; l: $8000; p: $18000; crc: $71D645FD), (n: 'dy11.k16'; l: $8000; p: $20000; crc: $FD9CCC5B), (n: 'dy10.k14'; l: $8000; p: $28000; crc: $88770AB8));
  srd_mcu: tipo_roms = (n: 'id8751h.mcu'; l: $1000; p: 0; crc: $11CD6CA4);
  // Dip
  srd_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin A'; number: 4; dip: ((dip_val: $3; dip_name: '1C 2C'), (dip_val: $2; dip_name: '1C 3C'), (dip_val: $1;
    dip_name: '1C 4C'), (dip_val: $0; dip_name: '1C 6C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $4; dip_name: '3C 1C'), (dip_val: $8; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet';
    number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  srd_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $1; dip_name: '1'), (dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '5'), (dip_val: $0;
    dip_name: '28'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Difficulty'; number: 4; dip: ((dip_val: $8; dip_name: 'Easy'), (dip_val: $C; dip_name: 'Normal'), (dip_val: $4;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Every 50K'), (dip_val: $0; dip_name: 'Every 100K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'After Stage 10'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Back to Stager 1'), (dip_val: $0; dip_name: 'Game Over'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $80; dip_name: 'No'), (dip_val: $0; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  scroll_x, i8751_return, i8751_value: word;
  i8751_port0, mcu_irq_timer, rom_bank, sound_latch: byte;
  rom: array [0 .. 5, 0 .. $3FFF] of byte;
  snd_dec: array [0 .. $7FFF] of byte;

procedure update_video_dec8;
var
  f, nchar, color, atrib, x, y: word;

  procedure draw_sprites(pri: byte);
  var
    x, y, f, nchar: word;
    color, atrib: byte;
    flipy: boolean;
  begin
    for f := 0 to $7F do
    begin
      atrib := buffer_sprites[(f * 4) + 1];
      color := (atrib and $03) + ((atrib and $08) shr 1);
      if ((pri = 0) and (color <> 0)) then
        continue;
      if ((pri = 1) and (color = 0)) then
        continue;
      nchar := buffer_sprites[(f * 4) + 3] + ((atrib and $E0) shl 3);
      if (nchar = 0) then
        continue;
      x := buffer_sprites[f * 4];
      y := buffer_sprites[(f * 4) + 2];
      flipy := (atrib and $04) <> 0;
      if (atrib and $10) <> 0 then
      begin
        put_gfx_sprite_diff(nchar, $40 + (color shl 3), false, flipy, 2, 0, 0);
        put_gfx_sprite_diff(nchar + 1, $40 + (color shl 3), false, flipy, 2, 16, 0);
        actualiza_gfx_sprite_size(x, y, 4, 32, 16);
      end
      else
      begin
        put_gfx_sprite(nchar, $40 + (color shl 3), false, flipy, 2);
        update_gfx_sprite(x, y, 4, 2);
      end;
    end;
  end;

begin
  for f := 0 to $1FF do
  begin
    atrib := memory[$1400 + (f * 2)];
    color := (atrib and $F0) shr 4;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      nchar := memory[$1401 + (f * 2)] + ((atrib and $3) shl 8);
      put_gfx(x * 16, y * 16, nchar, color shl 4, 2, 1);
      if color = 0 then
        put_gfx_block_trans(x * 16, y * 16, 3, 16, 16)
      else
        put_gfx_trans(x * 16, y * 16, nchar, color shl 4, 3, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll__y(2, 4, 256 - scroll_x);
  draw_sprites(0);
  scroll__y(3, 4, 256 - scroll_x);
  draw_sprites(1);
  // Foreground
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      nchar := memory[$800 + f];
      put_gfx_trans(x * 8, y * 8, nchar, $80, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(8, 0, 240, 256, 4);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_dec8;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.left[0] then
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
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.left[1] then
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
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    // i8751
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := marcade.in2 and $DF
    else
      marcade.in2 := marcade.in2 or $20;
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := marcade.in2 and $BF
    else
      marcade.in2 := marcade.in2 or $40;
  end;
end;

procedure dec8_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        // Main
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Sound
        m6502_0.run(frame_s);
        frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        // MCU
        mcs51_0.run(frame_mcu);
        frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        case f of
          247:
            begin
              m6809_0.change_nmi(PULSE_LINE);
              update_video_dec8;
              marcade.in1 := marcade.in1 or $40;
            end;
          263:
            marcade.in1 := marcade.in1 and $BF;
        end;
      end;
      events_dec8;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function getbyte_dec8(direccion: word): byte;
begin
  case direccion of
    $0 .. $17FF, $8000 .. $FFFF:
      getbyte_dec8 := memory[direccion];
    $2000:
      getbyte_dec8 := i8751_return shr 8;
    $2001:
      getbyte_dec8 := i8751_return and $FF;
    $2800 .. $288F:
      getbyte_dec8 := buffer_paleta[direccion and $FF];
    $3000 .. $308F:
      getbyte_dec8 := buffer_paleta[(direccion and $FF) + $100];
    $3800:
      getbyte_dec8 := marcade.dswa; // dsw0
    $3801:
      getbyte_dec8 := marcade.in0;
    $3802:
      getbyte_dec8 := marcade.in1;
    $3803:
      getbyte_dec8 := marcade.dswb; // dsw1
    $4000 .. $7FFF:
      getbyte_dec8 := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure change_color(dir: word);
var
  tmp_color: byte;
  color: tcolor;
  bit0, bit1, bit2, bit3: byte;
begin
  tmp_color := buffer_paleta[dir];
  // Red
  bit0 := (tmp_color and 1) shr 0;
  bit1 := (tmp_color and 2) shr 1;
  bit2 := (tmp_color and 4) shr 2;
  bit3 := (tmp_color and 8) shr 3;
  color.r := $03 * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  // Green
  bit0 := (tmp_color and $10) shr 4;
  bit1 := (tmp_color and $20) shr 5;
  bit2 := (tmp_color and $40) shr 6;
  bit3 := (tmp_color and $80) shr 7;
  color.g := $03 * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  // Blue
  tmp_color := buffer_paleta[dir + $100];
  bit0 := tmp_color and 1;
  bit1 := (tmp_color and 2) shr 1;
  bit2 := (tmp_color and 4) shr 2;
  bit3 := (tmp_color and 8) shr 3;
  color.b := $03 * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  set_pal_color(color, dir);
  buffer_color[(dir shr 4) and $F] := true;
end;

procedure putbyte_dec8(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF, $C00 .. $13FF:
      memory[direccion] := valor;
    $800 .. $BFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $1400 .. $17FF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $3FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $1800:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        timers.enabled(mcu_irq_timer, true);
      end;
    $1801:
      i8751_value := (i8751_value and $FF00) or valor;
    $1802:
      ; // i8751_return:=0;
    $1804:
      copymemory(@buffer_sprites[0], @memory[$600], $200); // DMA
    $1805:
      begin
        rom_bank := valor shr 5;
        scroll_x := (scroll_x and $FF) or ((valor and $F) shl 8);
      end;
    $1806:
      scroll_x := (scroll_x and $F00) or valor;
    $2000:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(PULSE_LINE);
      end;
    $2001:
      main_screen.flip_main_screen := valor <> 0;
    $2800 .. $288F:
      if buffer_paleta[direccion and $FF] <> valor then
      begin
        buffer_paleta[direccion and $FF] := valor;
        change_color(direccion and $FF);
      end;
    $3000 .. $308F:
      if buffer_paleta[(direccion and $FF) + $100] <> valor then
      begin
        buffer_paleta[(direccion and $FF) + $100] := valor;
        change_color(direccion and $FF);
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function getbyte_snd_dec8(direccion: word): byte;
begin
  case direccion of
    0 .. $5FF:
      getbyte_snd_dec8 := mem_snd[direccion];
    $2000:
      getbyte_snd_dec8 := ym2203_0.status;
    $2001:
      getbyte_snd_dec8 := ym2203_0.Read;
    $4000:
      getbyte_snd_dec8 := ym3812_0.status;
    $6000:
      getbyte_snd_dec8 := sound_latch;
    $8000 .. $FFFF:
      if m6502_0.opcode then
        getbyte_snd_dec8 := snd_dec[direccion and $7FFF]
      else
        getbyte_snd_dec8 := mem_snd[direccion];
  end;
end;

procedure putbyte_snd_dec8(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FF:
      mem_snd[direccion] := valor;
    $2000:
      ym2203_0.control(valor);
    $2001:
      ym2203_0.write(valor);
    $4000:
      ym3812_0.control(valor);
    $4001:
      ym3812_0.write(valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

procedure dec8_sound_update;
begin
  ym2203_0.Update;
  ym3812_0.Update;
end;

// MCU
function in_port0: byte;
begin
  in_port0 := i8751_port0;
end;

function in_port2: byte;
begin
  in_port2 := $FF;
end;

function in_port3: byte;
begin
  in_port3 := marcade.in2;
end;

procedure out_port0(valor: byte);
begin
  i8751_port0 := valor;
end;

procedure out_port2(valor: byte);
begin
  if (valor and $10) = 0 then
    i8751_port0 := i8751_value shr 8;
  if (valor and $20) = 0 then
    i8751_port0 := i8751_value and $FF;
  if (valor and $40) = 0 then
    i8751_return := (i8751_return and $FF) or (i8751_port0 shl 8);
  if (valor and $80) = 0 then
    i8751_return := (i8751_return and $FF00) or i8751_port0;
  if (valor and $4) = 0 then
    m6809_0.change_irq(ASSERT_LINE);
  if (valor and $2) = 0 then
    m6809_0.change_irq(CLEAR_LINE);
end;

procedure i8751_irq;
begin
  timers.enabled(mcu_irq_timer, false);
  mcs51_0.change_irq1(CLEAR_LINE);
end;

procedure snd_irq(irqstate: byte);
begin
  m6502_0.change_irq(irqstate);
end;

// Main
procedure reset_dec8;
begin
  m6809_0.reset;
  m6502_0.reset;
  mcs51_0.reset;
  ym2203_0.reset;
  ym3812_0.reset;
  marcade.in0 := $FF;
  marcade.in1 := $BF; // VBlank off
  marcade.in2 := $FF;
  sound_latch := 0;
  rom_bank := 0;
  i8751_return := 0;
  i8751_value := 0;
  i8751_port0 := 0;
  scroll_x := 0;
end;

function start_dec8: boolean;
const
  pc_x: array [0 .. 7] of dword = ($2000 * 8 + 0, $2000 * 8 + 1, $2000 * 8 + 2, $2000 * 8 + 3, 0, 1, 2, 3);
  ps_x: array [0 .. 15] of dword = (16 * 8, 1 + (16 * 8), 2 + (16 * 8), 3 + (16 * 8), 4 + (16 * 8), 5 + (16 * 8), 6 + (16 * 8), 7 + (16 * 8), 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 1024 * 8 * 8 + 0, 1024 * 8 * 8 + 1, 1024 * 8 * 8 + 2, 1024 * 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 1024 * 8 * 8 + 0,
    16 * 8 + 1024 * 8 * 8 + 1, 16 * 8 + 1024 * 8 * 8 + 2, 16 * 8 + 1024 * 8 * 8 + 3);
var
  f: word;
  memory_temp, memory_temp2: array [0 .. $3FFFF] of byte;
begin
  start_dec8 := false;
  machine_calls.general_loop := dec8_loop;
  machine_calls.reset := reset_dec8;
  machine_calls.fps_max := 57.444583;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 256, 512);
  screen_mod_scroll(2, 256, 256, 255, 512, 256, 511);
  screen_init(3, 256, 512, true);
  screen_mod_scroll(3, 256, 256, 255, 512, 256, 511);
  screen_init(4, 256, 512, false, true);
  start_video(240, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(2000000, 264, TCPU_M6809);
  m6809_0.change_ram_calls(getbyte_dec8, putbyte_dec8);
  // Sound CPU
  m6502_0 := cpu_m6502.Create(1500000, 264, TCPU_M6502);
  m6502_0.change_ram_calls(getbyte_snd_dec8, putbyte_snd_dec8);
  m6502_0.init_sound(dec8_sound_update);
  // MCU
  mcs51_0 := cpu_mcs51.Create(I8X51, 8000000, 264);
  mcs51_0.change_io_calls(in_port0, nil, in_port2, in_port3, out_port0, nil, out_port2, nil);
  mcu_irq_timer := timers.init(mcs51_0.numero_cpu, 64, i8751_irq, nil, false);
  // Sound Chip
  ym2203_0 := ym2203_chip.Create(1500000, 0.5, 0.5);
  ym3812_0 := ym3812_chip.Create(YM3812_FM, 3000000, 0.7);
  ym3812_0.change_irq_calls(snd_irq);
  // cargar roms y ponerlas en su sitio
  if not(roms_load(@memory_temp, srd_rom)) then
    exit;
  copymemory(@rom[4, 0], @memory_temp[0], $4000);
  copymemory(@rom[5, 0], @memory_temp[$4000], $4000);
  copymemory(@memory[$8000], @memory_temp[$8000], $8000);
  // Cheat!
  // memory[$96e4]:=$39;
  copymemory(@rom[0, 0], @memory_temp[$10000], $4000);
  copymemory(@rom[1, 0], @memory_temp[$14000], $4000);
  copymemory(@rom[2, 0], @memory_temp[$18000], $4000);
  copymemory(@rom[3, 0], @memory_temp[$1C000], $4000);
  // cargar roms audio y desencriptar
  if not(roms_load(@mem_snd, srd_snd)) then
    exit;
  for f := $8000 to $FFFF do
    snd_dec[f - $8000] := bitswap8(mem_snd[f], 7, 5, 6, 4, 3, 2, 1, 0);
  // cargar ROM MCU
  if not(roms_load(mcs51_0.get_rom_addr, srd_mcu)) then
    exit;
  // Cargar chars
  if not(roms_load(@memory_temp, srd_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(2, 0, 8 * 8, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, true);
  // Cargar tiles y ponerlas en su sitio
  if not(roms_load(@memory_temp, srd_tiles)) then
    exit;
  for f := 0 to 3 do
  begin
    copymemory(@memory_temp2[$10000 * f], @memory_temp[$4000 * f], $4000);
    copymemory(@memory_temp2[$8000 + ($10000 * f)], @memory_temp[$10000 + ($4000 * f)], $4000);
  end;
  init_gfx(1, 16, 16, $400);
  for f := 0 to 7 do
    gfx[1].trans[f] := true;
  gfx_set_desc_data(4, 4, 32 * 8, $8000 * 8, $8000 * 8 + 4, 0, 4);
  for f := 0 to 3 do
    convert_gfx(1, $100 * f * 16 * 16, @memory_temp2[$10000 * f], @pt_x, @ps_y, false, true);
  // Cargar sprites
  if not(roms_load(@memory_temp, srd_sprites)) then
    exit;
  init_gfx(2, 16, 16, $800);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(3, 0, 16 * 16, $10000 * 8, $20000 * 8, $0 * 8);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // DIP
  marcade.dswa := $7F;
  marcade.dswb := $FF;
  marcade.dswa_val := @srd_dip_a;
  marcade.dswb_val := @srd_dip_b;
  // final
  reset_dec8;
  start_dec8 := true;
end;

end.
