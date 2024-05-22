unit baraduke_hw;

interface

uses
  WinApi.Windows,
  m6809,
  m680x,
  namco_snd,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  misc_functions,
  sound_engine;

function start_baraduke: boolean;

implementation

const
  // Baraduke
  baraduke_rom: array [0 .. 2] of tipo_roms = ((n: 'bd1_3.9c'; l: $2000; p: $6000; crc: $EA2EA790),
    (n: 'bd1_1.9a'; l: $4000; p: $8000; crc: $4E9F2BDC), (n: 'bd1_2.9b'; l: $4000; p: $C000;
    crc: $40617FCD));
  baraduke_mcu: array [0 .. 1] of tipo_roms = ((n: 'bd1_4b.3b'; l: $4000; p: $8000; crc: $A47ECD32),
    (n: 'cus60-60a1.mcu'; l: $1000; p: $F000; crc: $076EA82A));
  baraduke_chars: tipo_roms = (n: 'bd1_5.3j'; l: $2000; p: 0; crc: $706B7FEE);
  baraduke_tiles: array [0 .. 2] of tipo_roms = ((n: 'bd1_8.4p'; l: $4000; p: 0; crc: $B0BB0710),
    (n: 'bd1_7.4n'; l: $4000; p: $4000; crc: $0D7EBEC9), (n: 'bd1_6.4m'; l: $4000; p: $8000;
    crc: $E5DA0896));
  baraduke_sprites: array [0 .. 3] of tipo_roms = ((n: 'bd1_9.8k'; l: $4000; p: 0; crc: $87A29ACC),
    (n: 'bd1_10.8l'; l: $4000; p: $4000; crc: $72B6D20C), (n: 'bd1_11.8m'; l: $4000; p: $8000;
    crc: $3076AF9C), (n: 'bd1_12.8n'; l: $4000; p: $C000; crc: $8B4C09A3));
  baraduke_prom: array [0 .. 1] of tipo_roms = ((n: 'bd1-1.1n'; l: $800; p: $0; crc: $0D78EBC6),
    (n: 'bd1-2.2m'; l: $800; p: $800; crc: $03F7241F));
  baraduke_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $4; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $18; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $8; dip_name: '2C 1C'), (dip_val: $18;
    dip_name: '1C 1C'), (dip_val: $10; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $60; name: 'Lives'; number: 4;
    dip: ((dip_val: $40; dip_name: '2'), (dip_val: $60; dip_name: '3'), (dip_val: $20;
    dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  baraduke_dip_b: array [0 .. 5] of def_dip = ((mask: $2; name: 'Allow Continue From Last Level';
    number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Freeze'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $8; name: 'Round Select'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $30; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $20; dip_name: 'Easy'), (dip_val: $30; dip_name: 'Normal'), (dip_val: $10;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C0; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $80; dip_name: '10K+'), (dip_val: $C0; dip_name: '10K 20K+'), (dip_val: $40;
    dip_name: '20K+'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  baraduke_dip_c: array [0 .. 1] of def_dip = ((mask: $2; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), ());
  // Metro-cross
  metrocross_rom: array [0 .. 2] of tipo_roms = ((n: 'mc1-3.9c'; l: $2000; p: $6000;
    crc: $3390B33C), (n: 'mc1-1.9a'; l: $4000; p: $8000; crc: $10B0977E), (n: 'mc1-2.9b'; l: $4000;
    p: $C000; crc: $5C846F35));
  metrocross_mcu: array [0 .. 1] of tipo_roms = ((n: 'mc1-4.3b'; l: $2000; p: $8000;
    crc: $9C88F898), (n: 'cus60-60a1.mcu'; l: $1000; p: $F000; crc: $076EA82A));
  metrocross_chars: tipo_roms = (n: 'mc1-5.3j'; l: $2000; p: 0; crc: $9B5EA33A);
  metrocross_tiles: array [0 .. 1] of tipo_roms = ((n: 'mc1-7.4p'; l: $4000; p: 0; crc: $C9DFA003),
    (n: 'mc1-6.4n'; l: $4000; p: $4000; crc: $9686DC3C));
  metrocross_sprites: array [0 .. 1] of tipo_roms = ((n: 'mc1-8.8k'; l: $4000; p: 0;
    crc: $265B31FA), (n: 'mc1-9.8l'; l: $4000; p: $4000; crc: $541EC029));
  metrocross_prom: array [0 .. 1] of tipo_roms = ((n: 'mc1-1.1n'; l: $800; p: $0; crc: $32A78A8B),
    (n: 'mc1-2.2m'; l: $800; p: $800; crc: $6F4DCA7B));
  metrocross_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $4; dip_name: 'Yes'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $18; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $18; dip_name: 'Normal'), (dip_val: $8;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $60; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $60;
    dip_name: '1C 1C'), (dip_val: $40; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());
  metrocross_dip_b: array [0 .. 3] of def_dip = ((mask: $20; name: 'Freeze'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $40; name: 'Round Select'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());

var
  inputport_selected, scroll_y0, scroll_y1: byte;
  sprite_mask, counter, scroll_x0, scroll_x1: word;
  prio, copy_sprites: boolean;
  spritex_add, spritey_add: integer;

procedure update_video_baraduke;
  procedure draw_sprites(prior: byte);
  var
    x, y, sizex, sizey, sy, f, atrib1, atrib2: byte;
    nchar, sx, sprite_xoffs, sprite_yoffs, color: word;
    flipx, flipy: boolean;
  const
    gfx_offs: array [0 .. 1, 0 .. 1] of byte = ((0, 1), (2, 3));
  begin
    sprite_xoffs := memory[$07F5] - 256 * (memory[$07F4] and 1);
    sprite_yoffs := memory[$07F7];
    for f := 0 to $7E do
    begin
      atrib1 := memory[$180A + (f * $10)];
      if prior <> (atrib1 and 1) then
        continue;
      atrib2 := memory[$180E + (f * $10)];
      color := memory[$180C + (f * $10)];
      flipx := (atrib1 and $20) <> 0;
      flipy := (atrib2 and $01) <> 0;
      sizex := (atrib1 and $80) shr 7;
      sizey := (atrib2 and $04) shr 2;
      sx := ((memory[$180D + (f * $10)] + ((color and $01) shl 8)) + sprite_xoffs +
        spritex_add) and $1FF;
      sy := (240 - memory[$180F + (f * $10)]) - sprite_yoffs - (16 * sizey) + spritey_add;
      nchar := memory[$180B + (f * $10)] * 4;
      if (((atrib1 and $10) <> 0) and (sizex = 0)) then
        nchar := nchar + 1;
      if (((atrib2 and $10) <> 0) and (sizey = 0)) then
        nchar := nchar + 2;
      color := (color and $FE) shl 3;
      for y := 0 to sizey do
        for x := 0 to sizex do
          put_gfx_sprite_diff((nchar + gfx_offs[y xor (sizey * byte(flipy))
            ][x xor (sizex * byte(flipx))]) and sprite_mask, color, flipx, flipy, 3,
            16 * x, 16 * y);
      actualiza_gfx_sprite_size(sx, sy, 4, 16 * (sizex + 1), 16 * (sizey + 1));
    end;
  end;

var
  f, color, nchar, pos: word;
  sx, sy, x, y, atrib: byte;
begin
  for x := 0 to 35 do
  begin
    for y := 0 to 27 do
    begin
      sx := x - 2;
      sy := y + 2;
      if (sx and $20) <> 0 then
        pos := sy + ((sx and $1F) shl 5)
      else
        pos := sx + (sy shl 5);
      if gfx[0].buffer[pos] then
      begin
        color := memory[$4C00 + pos];
        nchar := memory[$4800 + pos];
        put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 1, 0);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  for f := 0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    if gfx[1].buffer[f] then
    begin
      atrib := memory[$2001 + (f * 2)];
      nchar := memory[$2000 + (f * 2)] + (atrib and $3) shl 8;
      if prio then
        put_gfx_trans(x * 8, y * 8, nchar, atrib shl 3, 2, 1)
      else
        put_gfx(x * 8, y * 8, nchar, atrib shl 3, 2, 1);
      gfx[1].buffer[f] := false;
    end;
    if gfx[2].buffer[f] then
    begin
      atrib := memory[$3001 + (f * 2)];
      nchar := memory[$3000 + (f * 2)] + (atrib and $3) shl 8;
      if prio then
        put_gfx(x * 8, y * 8, nchar, atrib shl 3, 3, 2)
      else
        put_gfx_trans(x * 8, y * 8, nchar, atrib shl 3, 3, 2);
      gfx[2].buffer[f] := false;
    end;
  end;
  if prio then
  begin
    scroll_x_y(3, 4, scroll_x1 + 24, scroll_y1 + 25);
    draw_sprites(0);
    scroll_x_y(2, 4, scroll_x0 + 26, scroll_y0 + 25);
  end
  else
  begin
    scroll_x_y(2, 4, scroll_x0 + 26, scroll_y0 + 25);
    draw_sprites(0);
    scroll_x_y(3, 4, scroll_x1 + 24, scroll_y1 + 25);
  end;
  draw_sprites(1);
  actualiza_trozo(0, 0, 288, 224, 1, 0, 0, 288, 224, 4);
  actualiza_trozo_final(0, 0, 288, 224, 4);
end;

procedure events_baraduke;
begin
  if event.arcade then
  begin
    // marcade.in0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // marcade.in1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    // marcade.in2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure copy_sprites_hw; inline;
var
  i, j: byte;
begin
  for i := 0 to $7F do
  begin
    for j := 10 to 15 do
      memory[$1800 + (i * $10) + j] := memory[$1800 + (i * $10) + j - 6];
  end;
  copy_sprites := false;
end;

procedure baraduke_loop;
var
  f: word;
  frame_m, frame_mcu: single;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_mcu := m6800_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Sound CPU
        m6800_0.run(frame_mcu);
        frame_mcu := frame_mcu + m6800_0.tframes - m6800_0.contador;
        if f = 239 then
        begin
          update_video_baraduke;
          m6809_0.change_irq(ASSERT_LINE);
          m6800_0.change_irq(HOLD_LINE);
          if copy_sprites then
            copy_sprites_hw;
        end;
      end;
      events_baraduke;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function baraduke_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $4800 .. $4FFF, $6000 .. $FFFF:
      baraduke_getbyte := memory[direccion];
    $4000 .. $43FF:
      baraduke_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
  end;
end;

procedure baraduke_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FF1, $1FF3 .. $1FFF:
      memory[direccion] := valor;
    $1FF2:
      begin
        memory[direccion] := valor;
        copy_sprites := true;
      end;
    $2000 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $FFF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $3000 .. $3FFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[(direccion and $FFF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $43FF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $4800 .. $4FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $8000:
      ; // WD
    $8800:
      m6809_0.change_irq(CLEAR_LINE); // irq acknowledge
    $B000:
      begin
        scroll_x0 := (scroll_x0 and $FF) or (valor shl 8);
        prio := ((scroll_x0 and $E00) shr 9) = 6;
      end;
    $B001:
      scroll_x0 := (scroll_x0 and $FF00) or valor;
    $B002:
      scroll_y0 := valor;
    $B004:
      scroll_x1 := (scroll_x1 and $FF) or (valor shl 8);
    $B005:
      scroll_x1 := (scroll_x1 and $FF00) or valor;
    $B006:
      scroll_y1 := valor;
    $6000 .. $7FFF, $8001 .. $87FF, $8801 .. $AFFF, $B003, $B007 .. $FFFF:
      ; // ROM
  end;
end;

function baraduke_mcu_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $FF:
      baraduke_mcu_getbyte := m6800_0.m6803_internal_reg_r(direccion);
    $1000 .. $1104, $1106 .. $13FF:
      baraduke_mcu_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
    $1105:
      begin
        counter := counter + 1;
        baraduke_mcu_getbyte := (counter shr 4) and $FF;
      end;
    $8000 .. $C7FF, $F000 .. $FFFF:
      baraduke_mcu_getbyte := mem_snd[direccion];
  end;
end;

procedure baraduke_mcu_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $FF:
      m6800_0.m6803_internal_reg_w(direccion, valor);
    $1000 .. $13FF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $8000 .. $BFFF, $F000 .. $FFFF:
      exit;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
  end;
end;

function in_port1: byte;
var
  ret: byte;
begin
  ret := $FF;
  case inputport_selected of
    0:
      ret := (marcade.dswa and $F8) shr 3; // DSWA 0-4
    1:
      ret := ((marcade.dswa and 7) shl 2) or ((marcade.dswb and $C0) shr 6); // DSWA 5-7 DSWB 0-1
    2:
      ret := (marcade.dswb and $3E) shr 1; // DSWB 2-6
    3:
      ret := ((marcade.dswb and 1) shl 4) or (marcade.dswc and $F); // DSWB 7 DSWC 0-4
    4:
      ret := marcade.in0;
    5:
      ret := marcade.in2;
    6:
      ret := marcade.in1;
  end;
  in_port1 := ret;
end;

procedure out_port1(valor: byte);
begin
  if (valor and $E0) = $60 then
    inputport_selected := valor and $7;
end;

procedure sound_update_baraduke;
begin
  namco_snd_0.update;
end;

procedure reset_baraduke;
begin
  m6809_0.reset;
  m6800_0.reset;
  namco_snd_0.reset;
  reset_audio;
  marcade.in0 := $1F;
  marcade.in1 := $1F;
  marcade.in2 := $1F;
  scroll_x0 := 0;
  scroll_y0 := 0;
  scroll_x1 := 0;
  scroll_y1 := 0;
  copy_sprites := false;
end;

function start_baraduke: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $7FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  pt_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3);
  pt_y: array [0 .. 7] of dword = (0 * 8, 2 * 8, 4 * 8, 6 * 8, 8 * 8, 10 * 8, 12 * 8, 14 * 8);
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 8 * 4,
    9 * 4, 10 * 4, 11 * 4, 12 * 4, 13 * 4, 14 * 4, 15 * 4);
  ps_y: array [0 .. 15] of dword = (8 * 8 * 0, 8 * 8 * 1, 8 * 8 * 2, 8 * 8 * 3, 8 * 8 * 4,
    8 * 8 * 5, 8 * 8 * 6, 8 * 8 * 7, 8 * 8 * 8, 8 * 8 * 9, 8 * 8 * 10, 8 * 8 * 11, 8 * 8 * 12,
    8 * 8 * 13, 8 * 8 * 14, 8 * 8 * 15);
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $200);
    gfx[0].trans[3] := true;
    gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
    convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  end;
  procedure convert_tiles;
  var
    f: word;
  begin
    for f := $2000 to $3FFF do
    begin
      memory_temp[$8000 + f + $2000] := memory_temp[$8000 + f];
      memory_temp[$8000 + f + $4000] := memory_temp[$8000 + f] shl 4;
    end;
    for f := 0 to $1FFF do
      memory_temp[$8000 + f + $2000] := memory_temp[$8000 + f] shl 4;
    gfx_set_desc_data(3, 0, 16 * 8, $8000 * 8, 0, 4);
    init_gfx(1, 8, 8, $400);
    gfx[1].trans[7] := true;
    convert_gfx(1, 0, @memory_temp[0], @pt_x, @pt_y, false, false);
    init_gfx(2, 8, 8, $400);
    gfx[2].trans[7] := true;
    convert_gfx(2, 0, @memory_temp[$4000], @pt_x, @pt_y, false, false);
  end;
  procedure convert_sprites(num: word);
  begin
    init_gfx(3, 16, 16, num);
    gfx[3].trans[15] := true;
    gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
    convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  start_baraduke := false;
  machine_calls.general_loop := baraduke_loop;
  machine_calls.reset := reset_baraduke;
  machine_calls.fps_max := 60.606060;
  start_audio(false);
  screen_init(1, 288, 224, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 512, 256, false, true);
  start_video(288, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(49152000 div 32, 264, TCPU_M6809);
  m6809_0.change_ram_calls(baraduke_getbyte, baraduke_putbyte);
  // MCU CPU
  m6800_0 := cpu_m6800.Create(49152000 div 8, 264, TCPU_HD63701);
  m6800_0.change_ram_calls(baraduke_mcu_getbyte, baraduke_mcu_putbyte);
  m6800_0.change_io_calls(in_port1, nil, nil, nil, out_port1, nil, nil, nil);
  m6800_0.init_sound(sound_update_baraduke);
  // Sound
  namco_snd_0 := namco_snd_chip.Create(8, true);
  case main_vars.machine_type of
    287:
      begin // Baraduke
        // cargar roms main CPU
        if not(roms_load(@memory, baraduke_rom)) then
          exit;
        // Cargar MCU
        if not(roms_load(@mem_snd, baraduke_mcu)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, baraduke_chars)) then
          exit;
        convert_chars;
        // tiles
        if not(roms_load(@memory_temp, baraduke_tiles)) then
          exit;
        convert_tiles;
        // sprites
        if not(roms_load(@memory_temp, baraduke_sprites)) then
          exit;
        convert_sprites($200);
        sprite_mask := $1FF;
        spritex_add := 184;
        spritey_add := -14;
        // Paleta
        if not(roms_load(@memory_temp, baraduke_prom)) then
          exit;
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswc := $FF;
        marcade.dswa_val := @baraduke_dip_a;
        marcade.dswb_val := @baraduke_dip_b;
        marcade.dswc_val := @baraduke_dip_c;
      end;
    288:
      begin // Metro Cross
        // cargar roms main CPU
        if not(roms_load(@memory, metrocross_rom)) then
          exit;
        // Cargar MCU
        if not(roms_load(@mem_snd, metrocross_mcu)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, metrocross_chars)) then
          exit;
        convert_chars;
        // tiles
        if not(roms_load(@memory_temp, metrocross_tiles)) then
          exit;
        for f := $8000 to $BFFF do
          memory_temp[f] := $FF;
        convert_tiles;
        // sprites
        if not(roms_load(@memory_temp, metrocross_sprites)) then
          exit;
        convert_sprites($100);
        sprite_mask := $FF;
        spritex_add := -1;
        spritey_add := -32;
        // Paleta
        if not(roms_load(@memory_temp, metrocross_prom)) then
          exit;
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswc := $FF;
        marcade.dswa_val := @metrocross_dip_a;
        marcade.dswb_val := @metrocross_dip_b;
        marcade.dswc_val := @baraduke_dip_c;
      end;
  end;
  for f := 0 to $7FF do
  begin
    colores[f].r := ((memory_temp[f + $800] shr 0) and $01) * $0E +
      ((memory_temp[f + $800] shr 1) and $01) * $1F + ((memory_temp[f + $800] shr 2) and $01) * $43
      + ((memory_temp[f + $800] shr 3) and $01) * $8F;
    colores[f].g := ((memory_temp[f] shr 0) and $01) * $0E + ((memory_temp[f] shr 1) and $01) * $1F
      + ((memory_temp[f] shr 2) and $01) * $43 + ((memory_temp[f] shr 3) and $01) * $8F;
    colores[f].b := ((memory_temp[f] shr 4) and $01) * $0E + ((memory_temp[f] shr 5) and $01) * $1F
      + ((memory_temp[f] shr 6) and $01) * $43 + ((memory_temp[f] shr 7) and $01) * $8F;
  end;
  set_pal(colores, $800);
  // final
  reset_baraduke;
  start_baraduke := true;
end;

end.
