unit sauro_hw;

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
  timer_engine,
  ym_3812;

function start_sauro: boolean;

implementation

const
  sauro_rom: array [0 .. 1] of tipo_roms = ((n: 'sauro-2.bin'; l: $8000; p: 0; crc: $19F8DE25), (n: 'sauro-1.bin'; l: $8000; p: $8000; crc: $0F8B876F));
  sauro_snd_rom: tipo_roms = (n: 'sauro-3.bin'; l: $8000; p: 0; crc: $0D501E1B);
  sauro_pal: array [0 .. 2] of tipo_roms = ((n: '82s137-3.bin'; l: $400; p: 0; crc: $D52C4CD0), (n: '82s137-2.bin'; l: $400; p: $400; crc: $C3E96D5D), (n: '82s137-1.bin'; l: $400; p: $800; crc: $BDFCF00C));
  sauro_char_bg: array [0 .. 1] of tipo_roms = ((n: 'sauro-6.bin'; l: $8000; p: 0; crc: $4B77CB0F), (n: 'sauro-7.bin'; l: $8000; p: $8000; crc: $187DA060));
  sauro_char_fg: array [0 .. 1] of tipo_roms = ((n: 'sauro-4.bin'; l: $8000; p: 0; crc: $9B617CDA), (n: 'sauro-5.bin'; l: $8000; p: $8000; crc: $A6E2640D));
  sauro_sprites: array [0 .. 3] of tipo_roms = ((n: 'sauro-8.bin'; l: $8000; p: 0; crc: $E08B5D5E), (n: 'sauro-9.bin'; l: $8000; p: $8000; crc: $7C707195), (n: 'sauro-10.bin'; l: $8000; p: $10000; crc: $C93380D1), (n: 'sauro-11.bin'; l: $8000; p: $18000; crc: $F47982A8));
  // DIP
  sauro_dip_a: array [0 .. 6] of def_dip2 = ((mask: 2; name: 'Demo Sounds'; number: 2; val2: (0, 2); name2: ('Off', 'On')), (mask: 4; name: 'Cabinet'; number: 2; val2: (4, 0); name2: ('Upright', 'Cocktail')), (mask: 8; name: 'Free Play'; number: 2; val2: (0, 8);
    name2: ('Off', 'On')), (mask: $30; name: 'Difficult'; number: 4; val4: ($30, $20, $10, 0); name4: ('Very Easy', 'Easy', 'Hard', 'Very Hard')), (mask: $40; name: 'Allow Continue'; number: 2; val2: (0, $40); name2: ('No', 'Yes')), (mask: $80; name: 'Freeze'; number: 2;
    val2: (0, $80); name2: ('Off', 'On')), ());
  sauro_dip_b: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (0, 1, 2, 3); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $C; name: 'Coin B'; number: 4; val4: ($C, 8, 4, 0); name4: ('1C 2C', '1C 3C', '1C 4C', '1C 5C')), (mask: $30; name: 'Lives';
    number: 4; val4: ($30, $20, $10, 0); name4: ('2', '3', '4', '5')), ());

var
  scroll_bg, scroll_fg, sound_latch, pal_bank: byte;

procedure update_video_sauro;
var
  f, color, nchar, x, y: word;
  attr: byte;
begin
  for f := 0 to $3FF do
  begin
    // bg
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := f mod 32;
      attr := memory[$F400 + f];
      nchar := memory[$F000 + f] + ((attr and 7) shl 8);
      color := ((attr shr 4) and $F) or pal_bank;
      put_gfx_flip(x * 8, y * 8, nchar, color shl 4, 1, 0, (attr and 8) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
    // fg
    if gfx[1].buffer[f] then
    begin
      x := f div 32;
      y := f mod 32;
      attr := memory[f + $FC00];
      color := ((attr shr 4) and $F) or pal_bank;
      nchar := memory[f + $F800] + ((attr and 7) shl 8);
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 2, 1, (attr and 8) <> 0, false);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll__x(1, 3, scroll_bg);
  scroll__x(2, 3, scroll_fg);
  // sprites
  for f := 0 to $FE do
  begin
    y := memory[$E803 + (f * 4)];
    if y = $F8 then
      continue;
    attr := memory[$E803 + (f * 4) + 3];
    nchar := memory[$E803 + (f * 4) + 1] + ((attr and 3) shl 8);
    x := memory[$E803 + (f * 4) + 2];
    y := 236 - y;
    color := ((attr shr 4) and $F) or pal_bank;
    // I'm not really sure how this bit works
    if (attr and 8) <> 0 then
    begin
      if (x > $C0) then
        x := x + 256;
    end
    else if (x < $40) then
      continue;
    put_gfx_sprite(nchar, color shl 4, (attr and 4) <> 0, false, 2);
    update_gfx_sprite(x, y, 3, 2);
  end;
  update_final_piece(8, 16, 240, 224, 3);
end;

procedure events_sauro;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // P2
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
  end;
end;

procedure sauro_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to 255 do begin
    events_sauro;
    if f=240 then begin
      z80_0.change_irq(HOLD_LINE);
      update_video_sauro;
    end;
    //main
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
    //snd
    z80_1.run(frame_snd);
    frame_snd:=frame_snd+z80_1.tframes-z80_1.contador;
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

function sauro_getbyte(direccion: word): byte;
begin
  sauro_getbyte := memory[direccion];
end;

procedure sauro_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ; // ROM
    $E000 .. $EBFF:
      memory[direccion] := valor; // NVRAM + Sprite ram
    $F000 .. $F7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $F800 .. $FFFF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[1].buffer[direccion and $3FF] := true;
      end;
  end;
end;

function sauro_inbyte(port: word): byte;
begin
  case (port and $FF) of
    0:
      sauro_inbyte := marcade.dswa;
    $20:
      sauro_inbyte := marcade.dswb;
    $40:
      sauro_inbyte := marcade.in0;
    $60:
      sauro_inbyte := marcade.in1;
  end;
end;

procedure sauro_outbyte(port: word; valor: byte);
const
  scroll_map: array [0 .. 7] of byte = (2, 1, 4, 3, 6, 5, 0, 7);
  scroll_map_flip: array [0 .. 7] of byte = (0, 7, 2, 1, 4, 3, 6, 5);
begin
  case (port and $FF) of
    $80:
      sound_latch := $80 or valor;
    $A0:
      scroll_bg := valor;
    $A1:
      if main_screen.flip_main_screen then
        scroll_fg := (valor and $F8) or scroll_map_flip[valor and 7]
      else
        scroll_fg := (valor and $F8) or scroll_map[valor and 7];
    $C0:
      main_screen.flip_main_screen := (valor <> 0);
    $CA .. $CB:
      if pal_bank <> ((valor and 3) shl 4) then
      begin
        pal_bank := (valor and 3) shl 4;
        fillchar(gfx[0].buffer, $400, 1);
        fillchar(gfx[1].buffer, $400, 1);
      end;
  end;
end;

function sauro_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      sauro_snd_getbyte := mem_snd[direccion];
    $E000:
      begin
        sauro_snd_getbyte := sound_latch;
        sound_latch := 0;
      end;
  end;
end;

procedure sauro_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $A000:
      ; // adpcm
    $C000:
      ym3812_0.control(valor);
    $C001:
      ym3812_0.write(valor);
  end;
end;

procedure sauro_sound_update;
begin
  ym3812_0.update;
end;

procedure sauro_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

// Main
procedure reset_sauro;
begin
  z80_0.reset;
  z80_1.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  ym3812_0.reset;
  marcade.in0 := 0;
  marcade.in1 := 0;
  scroll_bg := 0;
  scroll_fg := 0;
  sound_latch := 0;
  pal_bank := 0;
end;

function start_sauro: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $1FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 4 * 8, 1 * 4 * 8, 2 * 4 * 8, 3 * 4 * 8, 4 * 4 * 8, 5 * 4 * 8, 6 * 4 * 8, 7 * 4 * 8);
  ps_x: array [0 .. 15] of dword = (1 * 4, 0 * 4, 3 * 4, 2 * 4, 5 * 4, 4 * 4, 7 * 4, 6 * 4, 9 * 4, 8 * 4, 11 * 4, 10 * 4, 13 * 4, 12 * 4, 15 * 4, 14 * 4);
  ps_y: array [0 .. 15] of dword = ($18000 * 8 + 0 * 4 * 16, $10000 * 8 + 0 * 4 * 16, $8000 * 8 + 0 * 4 * 16, 0 + 0 * 4 * 16, $18000 * 8 + 1 * 4 * 16, $10000 * 8 + 1 * 4 * 16, $8000 * 8 + 1 * 4 * 16, 0 + 1 * 4 * 16, $18000 * 8 + 2 * 4 * 16, $10000 * 8 + 2 * 4 * 16,
    $8000 * 8 + 2 * 4 * 16, 0 + 2 * 4 * 16, $18000 * 8 + 3 * 4 * 16, $10000 * 8 + 3 * 4 * 16, $8000 * 8 + 3 * 4 * 16, 0 + 3 * 4 * 16);
begin
  machine_calls.general_loop := sauro_loop;
  machine_calls.reset := reset_sauro;
  machine_calls.fps_max := 55.72;
  start_sauro := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  screen_init(3, 512, 256, false, true);
  start_video(240, 224);
  // Main CPU
  z80_0 := cpu_z80.create(5000000, 256);
  z80_0.change_ram_calls(sauro_getbyte, sauro_putbyte);
  z80_0.change_io_calls(sauro_inbyte, sauro_outbyte);
  if not(roms_load(@memory, sauro_rom)) then
    exit;
  // Sound CPU
  z80_1 := cpu_z80.create(4000000, 256);
  z80_1.change_ram_calls(sauro_snd_getbyte, sauro_snd_putbyte);
  z80_1.init_sound(sauro_sound_update);
  if not(roms_load(@mem_snd, sauro_snd_rom)) then
    exit;
  // IRQ Sound CPU
  timers.init(z80_1.numero_cpu, 4000000 / (8 * 60), sauro_snd_irq, nil, true);
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3812_FM, 2500000);
  // convertir chars
  if not(roms_load(@memory_temp, sauro_char_bg)) then
    exit;
  init_gfx(0, 8, 8, 2048);
  gfx_set_desc_data(4, 0, 8 * 8 * 4, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  if not(roms_load(@memory_temp, sauro_char_fg)) then
    exit;
  init_gfx(1, 8, 8, 2048);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 8 * 8 * 4, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, sauro_sprites)) then
    exit;
  init_gfx(2, 16, 16, 1024);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 0, 16 * 16, 0, 1, 2, 3);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, sauro_pal)) then
    exit;
  for f := 0 to $3FF do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $400]);
    colores[f].b := pal4bit(memory_temp[f + $800]);
  end;
  set_pal(colores, $400);
  // DIP
  marcade.dswa := $66;
  marcade.dswb := $2F;
  marcade.dswa_val2 := @sauro_dip_a;
  marcade.dswb_val2 := @sauro_dip_b;
  // final
  start_sauro := true;
end;

end.
