unit shootout_hw;

interface

uses
  WinApi.Windows,
  m6502,
  main_engine,
  controls_engine,
  ym_2203,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  misc_functions;

function start_shootout: boolean;

implementation

const
  shootout_rom: array [0 .. 2] of tipo_roms = ((n: 'cu00.b1'; l: $8000; p: $0; crc: $090EDEB6), (n: 'cu02.c3'; l: $8000; p: $8000; crc: $2A913730), (n: 'cu01.c1'; l: $4000; p: $10000;
    crc: $8843C3AE));
  shootout_char: tipo_roms = (n: 'cu11.h19'; l: $4000; p: $0; crc: $EFF00460);
  shootout_sprite: array [0 .. 5] of tipo_roms = ((n: 'cu04.c7'; l: $8000; p: $0; crc: $CEEA6B20), (n: 'cu03.c5'; l: $8000; p: $8000; crc: $B786BB3E), (n: 'cu06.c10'; l: $8000; p: $10000;
    crc: $2EC1D17F), (n: 'cu05.c9'; l: $8000; p: $18000; crc: $DD038B85), (n: 'cu08.c13'; l: $8000; p: $20000; crc: $91290933), (n: 'cu07.c12'; l: $8000; p: $28000; crc: $19B6B94F));
  shootout_audio: tipo_roms = (n: 'cu09.j1'; l: $4000; p: $C000; crc: $C4CBD558);
  shootout_tiles: tipo_roms = (n: 'cu10.h17'; l: $8000; p: $0; crc: $3854C877);
  shootout_pal: tipo_roms = (n: 'gb08.k10'; l: $100; p: $0; crc: $509C65B6);
  // Dip
  shootout_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $1; dip_name: '1C 3C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (dip_val: $4; dip_name: '1C 3C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $40; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Freeze';
    number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  shootout_dip_b: array [0 .. 3] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $1; dip_name: '1'), (dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '5'), (dip_val: $0;
    dip_name: 'Infinite'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $C; dip_name: '20K 70K+'), (dip_val: $8; dip_name: '30K 80K+'), (dip_val: $4; dip_name: '40K 90K+'), (dip_val: $0; dip_name: '70K'), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $30; name: 'Difficulty'; number: 4; dip: ((dip_val: $30; dip_name: 'Easy'), (dip_val: $20; dip_name: 'Normal'), (dip_val: $10; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  mem_bank, mem_bank_dec: array [0 .. 2, 0 .. $3FFF] of byte;
  mem_dec: array [0 .. $7FFF] of byte;
  banco, sound_latch: byte;
  bflicker, old_val: boolean;

procedure update_video_shootout;
var
  f, nchar, color: word;
  x, y, atrib: byte;
  procedure sprites(prioridad: byte);
  var
    f, atrib, x, y: byte;
    nchar: word;
  begin
    { 76543210
      xxx-----    bank
      ---x----    vertical size
      ----x---    priority
      -----x--    horizontal flip
      ------x-    flicker
      -------x    enable }
    for f := $7F downto 0 do
    begin
      atrib := memory[$19FD - (f * 4)];
      if (((atrib and $1) = 0) or ((atrib and 8) <> prioridad)) then
        continue;
      if (bflicker or ((atrib and $2) = 0)) then
      begin
        nchar := memory[$19FF - (f * 4)] + ((atrib shl 3) and $700);
        x := 240 - memory[$19FE - (f * 4)];
        y := 240 - memory[$19FC - (f * 4)];
        if (atrib and $10) <> 0 then
        begin // tamaño doble
          nchar := nchar and $7FE;
          put_gfx_sprite_diff(nchar, 64, (atrib and $4) <> 0, false, 1, 0, 0);
          put_gfx_sprite_diff(nchar + 1, 64, (atrib and $4) <> 0, false, 1, 0, 16);
          actualiza_gfx_sprite_size(x, y - 16, 3, 16, 32);
        end
        else
        begin
          put_gfx_sprite(nchar, 64, (atrib and $4) <> 0, false, 1);
          update_gfx_sprite(x, y, 3, 1);
        end;
      end;
    end;
  end;

begin
  for f := 0 to $3FF do
  begin
    // tiles
    if gfx[2].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      atrib := memory[$2C00 + f];
      color := (atrib and $F0) shr 2;
      nchar := memory[$2800 + f] + ((atrib and $07) shl 8);
      put_gfx(x * 8, y * 8, nchar, color, 2, 2);
      gfx[2].buffer[f] := false;
    end;
    // Chars
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      atrib := memory[$2400 + f];
      color := (atrib and $F0) shr 2;
      nchar := memory[$2000 + f] + ((atrib and $03) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color + 128, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  sprites(8);
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  sprites(0);
  update_final_piece(0, 8, 256, 240, 3);
  bflicker := not(bflicker);
end;

procedure events_shootout;
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
    if (p_contrls.map_arcade.coin[0] and not(old_val)) then
    begin
      marcade.dswb := marcade.dswb and $BF;
      marcade.in1 := marcade.in1 and $7F;
      m6502_0.change_nmi(ASSERT_LINE);
    end
    else
    begin
      marcade.dswb := (marcade.dswb or $40);
      marcade.in1 := (marcade.in1 or $80);
      m6502_0.change_nmi(CLEAR_LINE);
    end;
    old_val := p_contrls.map_arcade.coin[0];
  end;
end;

procedure shootout_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m6502_0.tframes;
  frame_s := m6502_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 261 do
      begin
        m6502_0.run(frame_m);
        frame_m := frame_m + m6502_0.tframes - m6502_0.contador;
        m6502_1.run(frame_s);
        frame_s := frame_s + m6502_1.tframes - m6502_1.contador;
        case f of
          7:
            marcade.dswb := marcade.dswb or $80;
          247:
            begin
              marcade.dswb := marcade.dswb and $7F;
              update_video_shootout;
            end;
        end;
      end;
      events_shootout;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function getbyte_shootout(direccion: word): byte;
begin
  case direccion of
    0 .. $FFF, $1004 .. $19FF, $2000 .. $2FFF:
      getbyte_shootout := memory[direccion];
    $1000:
      getbyte_shootout := marcade.dswa;
    $1001:
      getbyte_shootout := marcade.in0;
    $1002:
      getbyte_shootout := marcade.in1;
    $1003:
      getbyte_shootout := marcade.dswb;
    $4000 .. $7FFF:
      if m6502_0.opcode then
        getbyte_shootout := mem_bank_dec[banco, direccion and $3FFF]
      else
        getbyte_shootout := mem_bank[banco, direccion and $3FFF];
    $8000 .. $FFFF:
      if m6502_0.opcode then
        getbyte_shootout := mem_dec[direccion and $7FFF]
      else
        getbyte_shootout := memory[direccion];
  end;
end;

procedure putbyte_shootout(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FFF, $1004 .. $19FF:
      memory[direccion] := valor;
    $1000:
      banco := valor and $F;
    $1003:
      begin
        sound_latch := valor;
        m6502_1.change_nmi(ASSERT_LINE);
      end;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $2800 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function getbyte_snd_shootout(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF, $C000 .. $FFFF:
      getbyte_snd_shootout := mem_snd[direccion];
    $4000:
      getbyte_snd_shootout := ym2203_0.status;
    $A000:
      begin
        getbyte_snd_shootout := sound_latch;
        m6502_1.change_nmi(CLEAR_LINE);
      end;
  end;
end;

procedure putbyte_snd_shootout(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $4000:
      ym2203_0.control(valor);
    $4001:
      ym2203_0.write(valor);
    $C000 .. $FFFF:
      ; // ROM
  end;
end;

procedure shootout_sound_update;
begin
  ym2203_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  m6502_1.change_irq(irqstate);
end;

// Main
procedure reset_shootout;
begin
  m6502_0.reset;
  m6502_1.reset;
  ym2203_0.reset;
reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $3F;
  bflicker := false;
  banco := 0;
  sound_latch := 0;
  old_val := false;
end;

function start_shootout: boolean;
var
  colores: tpaleta;
  f: word;
  mem_temp: array [0 .. $7FFF] of byte;
  memory_temp: array [0 .. $2FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (($2000 * 8) + 0, ($2000 * 8) + 1, ($2000 * 8) + 2, ($2000 * 8) + 3, 0, 1, 2, 3);
  ps_x: array [0 .. 15] of dword = (128 + 0, 128 + 1, 128 + 2, 128 + 3, 128 + 4, 128 + 5, 128 + 6, 128 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  pt_x: array [0 .. 7] of dword = (($4000 * 8) + 0, ($4000 * 8) + 1, ($4000 * 8) + 2, ($4000 * 8) + 3, 0, 1, 2, 3);
begin
  machine_calls.general_loop := shootout_loop;
  machine_calls.reset := reset_shootout;
  machine_calls.fps_max := 12000000 / 2 / 384 / 262;
  start_shootout := false;
  start_audio(false);
  // Chars trans
  screen_init(1, 256, 256, true);
  screen_init(2, 256, 256);
  screen_init(3, 256, 256, false, true);
  start_video(256, 240);
  // Main CPU
  m6502_0 := cpu_m6502.create(2000000, 262, TCPU_M6502);
  m6502_0.change_ram_calls(getbyte_shootout, putbyte_shootout);
  // sound CPU
  m6502_1 := cpu_m6502.create(1500000, 262, TCPU_M6502);
  m6502_1.change_ram_calls(getbyte_snd_shootout, putbyte_snd_shootout);
  m6502_1.init_sound(shootout_sound_update);
  // Sound Chip
  ym2203_0 := ym2203_chip.create(1500000);
  ym2203_0.change_irq_calls(snd_irq);
  // cargar roms
  if not(roms_load(@memory_temp, shootout_rom)) then
    exit;
  // Copio las ROM en su sitio
  copymemory(@memory[$8000], @memory_temp[0], $8000);
  copymemory(@mem_bank[0, 0], @memory_temp[$8000], $4000);
  copymemory(@mem_bank[1, 0], @memory_temp[$C000], $4000);
  copymemory(@mem_bank[2, 0], @memory_temp[$10000], $4000);
  // Y las desencripto
  for f := 0 to $7FFF do
    mem_dec[f] := BITSWAP8(memory[f + $8000], 7, 5, 6, 4, 3, 2, 1, 0);
  for f := 0 to $3FFF do
  begin
    mem_bank_dec[0, f] := BITSWAP8(mem_bank[0, f], 7, 5, 6, 4, 3, 2, 1, 0);
    mem_bank_dec[1, f] := BITSWAP8(mem_bank[1, f], 7, 5, 6, 4, 3, 2, 1, 0);;
    mem_bank_dec[2, f] := BITSWAP8(mem_bank[2, f], 7, 5, 6, 4, 3, 2, 1, 0);;
  end;
  // Roms audio
  if not(roms_load(@mem_snd, shootout_audio)) then
    exit;
  // Cargar chars
  if not(roms_load(@memory_temp, shootout_char)) then
    exit;
  init_gfx(0, 8, 8, 1024);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(2, 0, 8 * 8, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, shootout_sprite)) then
    exit;
  init_gfx(1, 16, 16, 2048);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(3, 0, 32 * 8, 0 * $10000 * 8, 1 * $10000 * 8, 2 * $10000 * 8);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // tiles
  if not(roms_load(@memory_temp, shootout_tiles)) then
    exit;
  // mover para sacar tiles
  copymemory(@mem_temp[0], @memory_temp[0], $2000);
  copymemory(@mem_temp[$4000], @memory_temp[$2000], $2000);
  copymemory(@mem_temp[$2000], @memory_temp[$4000], $2000);
  copymemory(@mem_temp[$6000], @memory_temp[$6000], $2000);
  init_gfx(2, 8, 8, 2048);
  gfx_set_desc_data(2, 0, 8 * 8, 0, 4);
  convert_gfx(2, 0, @mem_temp, @pt_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, shootout_pal)) then
    exit;
  for f := 0 to 255 do
  begin
    colores[f].r := $21 * ((memory_temp[f] shr 0) and 1) + $47 * ((memory_temp[f] shr 1) and 1) + $97 * ((memory_temp[f] shr 2) and 1);
    colores[f].g := $21 * ((memory_temp[f] shr 3) and 1) + $47 * ((memory_temp[f] shr 4) and 1) + $97 * ((memory_temp[f] shr 5) and 1);
    colores[f].b := $21 * 0 + $47 * ((memory_temp[f] shr 6) and 1) + $97 * ((memory_temp[f] shr 7) and 1);
  end;
  set_pal(colores, 256);
  // Dip
  marcade.dswa := $BF;
  marcade.dswb := $FF;
  marcade.dswa_val := @shootout_dip_a;
  marcade.dswb_val := @shootout_dip_b;
  // final
  reset_shootout;
  start_shootout := true;
end;

end.
