unit taitosj_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  dac,
  m6805;

function start_taitosj: boolean;

implementation

const
  // Elevator Action
  elevator_rom: array [0 .. 3] of tipo_roms = ((n: 'ba3__01.2764.ic1'; l: $2000; p: 0;
    crc: $DA775A24), (n: 'ba3__02.2764.ic2'; l: $2000; p: $2000; crc: $FBFD8B3A),
    (n: 'ba3__03-1.2764.ic3'; l: $2000; p: $4000; crc: $A2E69833), (n: 'ba3__04-1.2764.ic6';
    l: $2000; p: $6000; crc: $2B78C462));
  elevator_sonido: array [0 .. 1] of tipo_roms = ((n: 'ba3__09.2732.ic70'; l: $1000; p: 0;
    crc: $6D5F57CB), (n: 'ba3__10.2732.ic71'; l: $1000; p: $1000; crc: $F0A769A1));
  elevator_mcu: tipo_roms = (n: 'ba3__11.mc68705p3.ic24'; l: $800; p: 0; crc: $9CE75AFC);
  elevator_char: array [0 .. 3] of tipo_roms = ((n: 'ba3__05.2764.ic4'; l: $2000; p: 0;
    crc: $6C4EE58F), (n: 'ba3__06.2764.ic5'; l: $2000; p: $2000; crc: $41AB0AFC),
    (n: 'ba3__07.2764.ic9'; l: $2000; p: $4000; crc: $EFE43731), (n: 'ba3__08.2764.ic10'; l: $2000;
    p: $6000; crc: $3CA20696));
  elevator_prom: tipo_roms = (n: 'eb16.22'; l: $100; p: 0; crc: $B833B5EA);
  elevator_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $3; dip_name: '10000'), (dip_val: $2; dip_name: '15000'), (dip_val: $1;
    dip_name: '20000'), (dip_val: $0; dip_name: '25000'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'Free Play'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $18; name: 'Lives'; number: 4;
    dip: ((dip_val: $18; dip_name: '3'), (dip_val: $10; dip_name: '4'), (dip_val: $8;
    dip_name: '5'), (dip_val: $0; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  elevator_dip_c: array [0 .. 5] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $3; dip_name: 'Easiest'), (dip_val: $2; dip_name: 'Easy'), (dip_val: $1;
    dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $10; name: 'Coinage Display'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Coins/Credits'), (dip_val: $0; dip_name: 'Insert Coin'), (), (),
    (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Year Display'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20; dip_name: 'Yes'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $40; name: 'Hit Detection'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Normal Game'), (dip_val: $0; dip_name: 'No Hit'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Coin Slots'; number: 2;
    dip: ((dip_val: $80; dip_name: 'A and B'), (dip_val: $0; dip_name: 'A only'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), ());
  // Jungle King
  junglek_rom: array [0 .. 8] of tipo_roms = ((n: 'kn21-1.bin'; l: $1000; p: 0; crc: $45F55D30),
    (n: 'kn22-1.bin'; l: $1000; p: $1000; crc: $07CC9A21), (n: 'kn43.bin'; l: $1000; p: $2000;
    crc: $A20E5A48), (n: 'kn24.bin'; l: $1000; p: $3000; crc: $19EA7F83), (n: 'kn25.bin'; l: $1000;
    p: $4000; crc: $844365EA), (n: 'kn46.bin'; l: $1000; p: $5000; crc: $27A95FD5), (n: 'kn47.bin';
    l: $1000; p: $6000; crc: $5C3199E0), (n: 'kn28.bin'; l: $1000; p: $7000; crc: $194A2D09),
    (n: 'kn60.bin'; l: $1000; p: $8000; crc: $1A9C0A26));
  junglek_sonido: array [0 .. 2] of tipo_roms = ((n: 'kn37.bin'; l: $1000; p: 0; crc: $DEE7F5D4),
    (n: 'kn38.bin'; l: $1000; p: $1000; crc: $BFFD3D21), (n: 'kn59-1.bin'; l: $1000; p: $2000;
    crc: $CEE485FC));
  junglek_char: array [0 .. 7] of tipo_roms = ((n: 'kn29.bin'; l: $1000; p: 0; crc: $8F83C290),
    (n: 'kn30.bin'; l: $1000; p: $1000; crc: $89FD19F1), (n: 'kn51.bin'; l: $1000; p: $2000;
    crc: $70E8FC12), (n: 'kn52.bin'; l: $1000; p: $3000; crc: $BCBAC1A3), (n: 'kn53.bin'; l: $1000;
    p: $4000; crc: $B946C87D), (n: 'kn34.bin'; l: $1000; p: $5000; crc: $320DB2E1), (n: 'kn55.bin';
    l: $1000; p: $6000; crc: $70AEF58F), (n: 'kn56.bin'; l: $1000; p: $7000; crc: $932EB667));
  junglek_prom: tipo_roms = (n: 'eb16.22'; l: $100; p: 0; crc: $B833B5EA);
  junglek_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Finish Bonus'; number: 4;
    dip: ((dip_val: $3; dip_name: 'None'), (dip_val: $2; dip_name: 'Timer x1'), (dip_val: $1;
    dip_name: 'Timer x2'), (dip_val: $0; dip_name: 'Timer x3'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $18; name: 'Lives'; number: 4;
    dip: ((dip_val: $18; dip_name: '3'), (dip_val: $10; dip_name: '4'), (dip_val: $8;
    dip_name: '5'), (dip_val: $0; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Flip Screen'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  junglek_dip_c: array [0 .. 4] of def_dip = ((mask: $3; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $2; dip_name: '10000'), (dip_val: $1; dip_name: '20000'), (dip_val: $0;
    dip_name: '30000'), (dip_val: $3; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $20; name: 'Year Display'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20; dip_name: 'Yes'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $40; name: 'Infinite Lives'; number: 2;
    dip: ((dip_val: $40; dip_name: 'No'), (dip_val: $0; dip_name: 'Yes'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Coin Slots'; number: 2;
    dip: ((dip_val: $80; dip_name: 'A and B'), (dip_val: $0; dip_name: 'A only'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), ());
  // General
  coin_dip: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
    dip: ((dip_val: $0F; dip_name: '9C 1C'), (dip_val: $0E; dip_name: '8C 1C'), (dip_val: $0D;
    dip_name: '7C 1C'), (dip_val: $0C; dip_name: '6C 1C'), (dip_val: $0B;
    dip_name: '5C 1C'), (dip_val: $0A; dip_name: '4C 1C'), (dip_val: $09;
    dip_name: '3C 1C'), (dip_val: $08; dip_name: '2C 1C'), (dip_val: $00;
    dip_name: '1C 1C'), (dip_val: $01; dip_name: '1C 2C'), (dip_val: $02;
    dip_name: '1C 3C'), (dip_val: $03; dip_name: '1C 4C'), (dip_val: $04;
    dip_name: '1C 5C'), (dip_val: $05; dip_name: '1C 6C'), (dip_val: $06;
    dip_name: '1C 7C'), (dip_val: $07; dip_name: '1C 8C'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $F0; dip_name: '9C 1C'), (dip_val: $E0; dip_name: '8C 1C'), (dip_val: $D0;
    dip_name: '7C 1C'), (dip_val: $C0; dip_name: '6C 1C'), (dip_val: $B0;
    dip_name: '5C 1C'), (dip_val: $A0; dip_name: '4C 1C'), (dip_val: $90;
    dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $00;
    dip_name: '1C 1C'), (dip_val: $10; dip_name: '1C 2C'), (dip_val: $20;
    dip_name: '1C 3C'), (dip_val: $30; dip_name: '1C 4C'), (dip_val: $40;
    dip_name: '1C 5C'), (dip_val: $50; dip_name: '1C 6C'), (dip_val: $60;
    dip_name: '1C 7C'), (dip_val: $70; dip_name: '1C 8C'))), ());

var
  memory_rom: array [0 .. 1, 0 .. $1FFF] of byte;
  gfx_rom: array [0 .. $7FFF] of byte;
  rweights, gweights, bweights: array [0 .. 2] of single;
  collision_reg: array [0 .. 3] of byte;
  scroll: array [0 .. 5] of byte;
  colorbank: array [0 .. 1] of byte;
  scroll_y: array [0 .. $5F] of word;
  draw_order: array [0 .. 31, 0 .. 3] of byte;
  gfx_pos: word;
  video_priority, soundlatch, rom_bank, video_mode, dac_out, dac_vol: byte;
  sound_semaphore, rechars1, rechars2: boolean;
  sound_nmi: array [0 .. 1] of boolean;
  pos_x: array [0 .. 4] of shortint;
  // mcu
  mcu_mem: array [0 .. $7FF] of byte;
  mcu_toz80, mcu_address, mcu_fromz80, mcu_portA_in, mcu_portA_out: byte;
  mcu_zaccept, mcu_zready, mcu_busreq: boolean;

function get_sprite_xy(offs: word; sx, sy: pbyte; sprite_offset: byte): boolean;
begin
  sx^ := memory[$D100 + sprite_offset + offs + 0] + 1;
  sy^ := 240 - memory[$D100 + sprite_offset + offs + 1] - 2;
  get_sprite_xy := (sy^) < 240;
end;

procedure update_video_taitosj;
const
  ps_x: array [0 .. 15] of dword = (7, 6, 5, 4, 3, 2, 1, 0, 8 * 8 + 7, 8 * 8 + 6, 8 * 8 + 5,
    8 * 8 + 4, 8 * 8 + 3, 8 * 8 + 2, 8 * 8 + 1, 8 * 8 + 0);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8,
    17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
  procedure conv_chars1;
  begin
    gfx_set_desc_data(3, 0, 8 * 8, 512 * 8 * 8, 256 * 8 * 8, 0);
    convert_gfx(0, 0, @memory[$9000], @ps_x, @ps_y, false, false);
    // sprites
    gfx_set_desc_data(3, 0, 32 * 8, 128 * 16 * 16, 64 * 16 * 16, 0);
    convert_gfx(1, 0, @memory[$9000], @ps_x, @ps_y, false, false);
  end;

  procedure conv_chars2;
  begin
    // Chars 2
    gfx_set_desc_data(3, 0, 8 * 8, 512 * 8 * 8, 256 * 8 * 8, 0);
    convert_gfx(2, 0, @memory[$A800], @ps_x, @ps_y, false, false);
    // sprites
    gfx_set_desc_data(3, 0, 32 * 8, 128 * 16 * 16, 64 * 16 * 16, 0);
    convert_gfx(3, 0, @memory[$A800], @ps_x, @ps_y, false, false);
  end;

  procedure taitosj_putsprites(sprite_offset: byte);
  var
    f, sx, sy, nchar, which, atrib, ngfx, offs, color: byte;
  begin
    // drawing order is a bit strange. The last sprite has to be moved at the start of the list
    for f := $1F downto 0 do
    begin
      which := (f - 1) and $1F; // move last sprite at the head of the list
      offs := which * 4;
      if ((which >= $10) and (which <= $17)) then
        continue; // no sprites here
      sx := memory[$D100 + sprite_offset + offs + 0] + pos_x[3];
      sy := 240 - memory[$D100 + sprite_offset + offs + 1] + pos_x[4];
      if (sy < 240) then
      begin
        atrib := memory[$D100 + sprite_offset + offs + 2];
        nchar := memory[$D100 + sprite_offset + offs + 3] and $3F;
        ngfx := ((memory[$D100 + sprite_offset + offs + 3] and $40) shr 5) or 1;
        color := 2 * ((colorbank[1] shr 4) and $03) + ((atrib shr 2) and $01);
        put_gfx_sprite(nchar, color shl 3, (atrib and $01) <> 0, (atrib and $02) <> 0, ngfx);
        update_gfx_sprite(sx, sy, 4, ngfx);
      end;
    end;
  end;

var
  color_back, color_mid, color_front, gfx_back, gfx_mid, gfx_front, nchar, layer: byte;
  f, x, y: word;
  scroll_def: array [0 .. 31] of word;
begin
  if rechars1 then
  begin
    conv_chars1;
    rechars1 := false;
  end;
  if rechars2 then
  begin
    conv_chars2;
    rechars2 := false;
  end;
  color_back := (colorbank[0] and $07) shl 3;
  color_mid := ((colorbank[0] shr 4) and $07) shl 3;
  color_front := (colorbank[1] and $07) shl 3;
  gfx_back := (colorbank[0] and $08) shr 2;
  gfx_mid := (colorbank[0] and $80) shr 6;
  gfx_front := (colorbank[1] and $08) shr 2;
  for f := $0 to $3FF do
  begin
    // back
    x := f mod 32;
    y := f div 32;
    if (video_mode and $10) <> 0 then
    begin
      if gfx[0].buffer[f] then
      begin
        nchar := memory[$C400 + f];
        put_gfx_trans(x * 8, y * 8, nchar, color_back, 1, gfx_back);
        gfx[0].buffer[f] := false;
      end;
    end;
    // mid
    if (video_mode and $20) <> 0 then
    begin
      if gfx[0].buffer[f + $400] then
      begin
        nchar := memory[$C800 + f];
        put_gfx_trans(x * 8, y * 8, nchar, color_mid, 2, gfx_mid);
        gfx[0].buffer[f + $400] := false;
      end;
    end;
    // front
    if (video_mode and $40) <> 0 then
    begin
      if gfx[0].buffer[f + $800] then
      begin
        nchar := memory[$CC00 + f];
        put_gfx_trans(x * 8, y * 8, nchar, color_front, 3, gfx_front);
        gfx[0].buffer[f + $800] := false;
      end;
    end;
  end;
  fill_full_screen(4, (colorbank[1] and $07) shl 3);
  for f := 0 to 3 do
  begin
    layer := draw_order[video_priority and $1F, f];
    case layer of
      0:
        if (video_mode and $80) <> 0 then
          taitosj_putsprites((video_mode and $4) shl 5);
      1:
        if (video_mode and $10) <> 0 then
        begin
          x := scroll[0];
          x := (x and $F8) + ((x + 3) and 7) + pos_x[0];
          // Ordena los scrolls de la Y!!!
          for y := 0 to $1F do
            scroll_def[y] := scroll_y[(y + (x div 8)) and $1F];
          scroll__y_part2(1, 4, 8, @scroll_def, x, scroll[1]);
        end;
      2:
        if (video_mode and $20) <> 0 then
        begin
          x := scroll[2];
          x := (x and $F8) + ((x + 1) and 7) + pos_x[1];
          // Ordena los scrolls de la Y!!!
          for y := 0 to $1F do
            scroll_def[y] := scroll_y[32 + ((y + (x div 8)) and $1F)];
          scroll__y_part2(2, 4, 8, @scroll_def, x, scroll[3]);
        end;
      3:
        if (video_mode and $40) <> 0 then
        begin
          x := scroll[4];
          x := (x and $F8) + ((x - 1) and 7) + pos_x[2];
          // Ordena los scrolls de la Y!!!
          for y := 0 to $1F do
            scroll_def[y] := scroll_y[64 + ((y + (x div 8)) and $1F)];
          scroll__y_part2(3, 4, 8, @scroll_def, x, scroll[5]);
        end;
    end;
  end;
  actualiza_trozo_final(0, 16, 256, 224, 4);
end;

procedure events_taitosj;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // System
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
  end;
end;

procedure taitosj_nomcu_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 239 then
        begin
          z80_0.change_irq(HOLD_LINE);
          update_video_taitosj;
        end;
      end;
      events_taitosj;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure taitosj_nomcu_putbyte(direccion: word; valor: byte);
  procedure change_color(dir: word);
  var
    val, bit0, bit1, bit2: byte;
    color: tcolor;
  begin
    dir := dir and $FE;
    // blue component */
    val := not(buffer_paleta[dir or 1]);
    bit0 := (val shr 0) and $01;
    bit1 := (val shr 1) and $01;
    bit2 := (val shr 2) and $01;
    color.b := combine_3_weights(@bweights, bit0, bit1, bit2);
    // green component */
    bit0 := (val shr 3) and $01;
    bit1 := (val shr 4) and $01;
    bit2 := (val shr 5) and $01;
    color.g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // red component
    bit0 := (val shr 6) and $01;
    bit1 := (val shr 7) and $01;
    val := not(buffer_paleta[dir]);
    bit2 := (val shr 0) and $01;
    color.r := combine_3_weights(@rweights, bit0, bit1, bit2);
    set_pal_color(color, dir shr 1);
  end;

begin
  case direccion of
    0 .. $7FFF, $D700 .. $FFFF:
      ;
    $8000 .. $87FF, $C000 .. $C3FF, $D100 .. $D1FF:
      memory[direccion] := valor;
    $8800 .. $8FFF:
      ; // Fake MCU
    $9000 .. $A7FF:
      if memory[direccion] <> valor then
      begin
        rechars1 := true;
        memory[direccion] := valor;
      end;
    $A800 .. $BFFF:
      if memory[direccion] <> valor then
      begin
        rechars2 := true;
        memory[direccion] := valor;
      end;
    $C400 .. $CFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion - $C400] := true;
        memory[direccion] := valor;
      end;
    $D000 .. $D05F:
      scroll_y[direccion and $7F] := valor;
    $D200 .. $D2FF:
      if buffer_paleta[direccion and $7F] <> valor then
      begin
        buffer_paleta[direccion and $7F] := valor;
        change_color(direccion and $7F);
      end;
    $D300 .. $D3FF:
      video_priority := valor;
    $D400 .. $D4FF:
      case (direccion and $F) of
        $E:
          ay8910_0.Control(valor);
        $F:
          ay8910_0.Write(valor);
      end;
    $D500 .. $D5FF:
      case (direccion and $F) of
        $0 .. $5:
          scroll[direccion and $F] := valor;
        $6, $7:
          if colorbank[direccion and 1] <> valor then
          begin
            colorbank[direccion and 1] := valor;
            fillchar(gfx[0].buffer, $C00, 1);
          end;
        $8:
          fillchar(collision_reg, 4, 0);
        $9:
          gfx_pos := (gfx_pos and $FF00) or valor;
        $A:
          gfx_pos := (gfx_pos and $FF) or (valor shl 8);
        $B:
          begin
            soundlatch := valor;
            sound_nmi[1] := true;
            if (sound_nmi[0] and sound_nmi[1]) then
              z80_1.change_nmi(PULSE_LINE);
          end;
        $C:
          begin
            sound_semaphore := (valor and 1) <> 0;
            if sound_semaphore then
              z80_1.change_nmi(PULSE_LINE);
          end;
        $D:
          ; // WD
        $E:
          rom_bank := valor shr 7;
      end;
    $D600 .. $D6FF:
      if video_mode <> valor then
      begin
        video_mode := valor;
        fillchar(gfx[0].buffer, $C00, 1);
        main_screen.flip_main_x := (valor and 1) <> 0;
        main_screen.flip_main_y := (valor and 2) <> 0;
      end;
  end;
end;

function taitosj_nomcu_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $5FFF, $8000 .. $87FF, $C000 .. $CFFF, $E000 .. $FFFF:
      taitosj_nomcu_getbyte := memory[direccion];
    $6000 .. $7FFF:
      taitosj_nomcu_getbyte := memory_rom[rom_bank, direccion and $1FFF];
    $8800 .. $8FFF:
      taitosj_nomcu_getbyte := 0; // Fake MCU
    $D000 .. $D05F:
      taitosj_nomcu_getbyte := scroll_y[direccion and $7F];
    $D200 .. $D2FF:
      taitosj_nomcu_getbyte := buffer_paleta[direccion and $7F];
    $D400 .. $D4FF:
      case (direccion and $F) of
        $0 .. $3:
          taitosj_nomcu_getbyte := collision_reg[direccion and $F];
        $4 .. $7:
          begin
            if gfx_pos < $8000 then
              taitosj_nomcu_getbyte := gfx_rom[gfx_pos]
            else
              taitosj_nomcu_getbyte := 0;
            gfx_pos := gfx_pos + 1;
          end;
        $8:
          taitosj_nomcu_getbyte := marcade.in0;
        $9:
          taitosj_nomcu_getbyte := marcade.in1;
        $A:
          taitosj_nomcu_getbyte := marcade.dswa;
        $B:
          taitosj_nomcu_getbyte := marcade.in2;
        $C:
          taitosj_nomcu_getbyte := $EF;
        $D:
          taitosj_nomcu_getbyte := marcade.in4 or $F;
        $F:
          taitosj_nomcu_getbyte := ay8910_0.Read;
      end;
  end;
end;

procedure taitosj_mcu_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  frame_mcu := m6805_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        // mcu
        m6805_0.run(frame_mcu);
        frame_mcu := frame_mcu + m6805_0.tframes - m6805_0.contador;
        if f = 239 then
        begin
          z80_0.change_irq(HOLD_LINE);
          update_video_taitosj;
        end;
      end;
      events_taitosj;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function taitosj_mcu_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF, $9000 .. $FFFF:
      taitosj_mcu_getbyte := taitosj_nomcu_getbyte(direccion);
    $8800 .. $8FFF:
      if (direccion and 1) = 0 then
      begin
        taitosj_mcu_getbyte := mcu_toz80;
        mcu_zaccept := true;
      end
      else
      begin
        taitosj_mcu_getbyte := not(byte(mcu_zready) or (byte(mcu_zaccept) * 2));
      end;
  end;
end;

procedure taitosj_mcu_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $7FFF:
      ;
    $8000 .. $87FF, $9000 .. $FFFF:
      taitosj_nomcu_putbyte(direccion, valor);
    $8800 .. $8FFF:
      if (direccion and 1) = 0 then
      begin
        mcu_zready := true;
        m6805_0.irq_request(0, ASSERT_LINE);
        mcu_fromz80 := valor;
      end;
  end;
end;

function mcu_taitosj_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FF;
  case direccion of
    0:
      mcu_taitosj_getbyte := mcu_portA_in;
    1:
      mcu_taitosj_getbyte := $FF;
    2:
      mcu_taitosj_getbyte := byte(mcu_zready) or byte(mcu_zaccept) * 2 or byte(not(mcu_busreq)) * 4;
    3 .. $7FF:
      mcu_taitosj_getbyte := mcu_mem[direccion];
  end;
end;

procedure mcu_taitosj_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FF;
  case direccion of
    0:
      mcu_portA_out := valor;
    1:
      begin
        if (not(valor) and $01) <> 0 then
          exit;
        if (not(valor) and $02) <> 0 then
        begin
          // 68705 is going to read data from the Z80
          mcu_zready := false;
          m6805_0.irq_request(0, CLEAR_LINE);
          mcu_portA_in := mcu_fromz80;
        end;
        mcu_busreq := (not(valor) and $08) <> 0;
        if (not(valor) and $04) <> 0 then
        begin
          // 68705 is writing data for the Z80
          mcu_toz80 := mcu_portA_out;
          mcu_zaccept := false;
        end;
        if (not(valor) and $10) <> 0 then
        begin
          memory[mcu_address] := mcu_portA_out;
          // increase low 8 bits of latched address for burst writes
          mcu_address := (mcu_address and $FF00) or ((mcu_address + 1) and $FF);
        end;
        if (not(valor) and $20) <> 0 then
          mcu_portA_in := memory[mcu_address];
        if (not(valor) and $40) <> 0 then
          mcu_address := (mcu_address and $FF00) or mcu_portA_out;
        if (not(valor) and $80) <> 0 then
          mcu_address := (mcu_address and $00FF) or (mcu_portA_out shl 8);
      end;
    2:
      ;
    3 .. $7F:
      mcu_mem[direccion] := valor;
  end;
end;

function taitosj_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $43FF:
      taitosj_snd_getbyte := mem_snd[direccion];
    $4801:
      taitosj_snd_getbyte := ay8910_1.Read;
    $4803:
      taitosj_snd_getbyte := ay8910_2.Read;
    $4805:
      taitosj_snd_getbyte := ay8910_3.Read;
    $5000:
      begin
        sound_nmi[1] := false;
        taitosj_snd_getbyte := soundlatch;
      end;
    $5001:
      taitosj_snd_getbyte := byte(sound_nmi[1]) * 8 or byte(sound_semaphore) * 4 or 3;
  end;
end;

procedure taitosj_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
    $4800:
      ay8910_1.Control(valor);
    $4801:
      ay8910_1.Write(valor);
    $4802:
      ay8910_2.Control(valor);
    $4803:
      ay8910_2.Write(valor);
    $4804:
      ay8910_3.Control(valor);
    $4805:
      ay8910_3.Write(valor);
    $5000:
      soundlatch := soundlatch and $7F;
    $5001:
      sound_semaphore := false;
  end;
end;

function ay0_porta_read: byte;
begin
  ay0_porta_read := marcade.dswb;
end;

function ay0_portb_read: byte;
begin
  ay0_portb_read := marcade.dswc;
end;

procedure ay1_porta_write(valor: byte);
begin
  dac_out := not(valor);
  dac_0.signed_data16_w(dac_out * dac_vol);
end;

procedure ay1_portb_write(valor: byte);
const
  voltable: array [0 .. $FF] of byte = ($FF, $FE, $FC, $FB, $F9, $F7, $F6, $F4, $F3, $F2, $F1, $EF,
    $EE, $EC, $EB, $EA, $E8, $E7, $E5, $E4, $E2, $E1, $E0, $DF, $DE, $DD, $DC, $DB, $D9, $D8, $D7,
    $D6, $D5, $D4, $D3, $D2, $D1, $D0, $CF, $CE, $CD, $CC, $CB, $CA, $C9, $C8, $C7, $C6, $C5, $C4,
    $C3, $C2, $C1, $C0, $BF, $BF, $BE, $BD, $BC, $BB, $BA, $BA, $B9, $B8, $B7, $B7, $B6, $B5, $B4,
    $B3, $B3, $B2, $B1, $B1, $B0, $AF, $AE, $AE, $AD, $AC, $AB, $AA, $AA, $A9, $A8, $A8, $A7, $A6,
    $A6, $A5, $A5, $A4, $A3, $A2, $A2, $A1, $A1, $A0, $A0, $9F, $9E, $9E, $9D, $9D, $9C, $9C, $9B,
    $9B, $9A, $99, $99, $98, $97, $97, $96, $96, $95, $95, $94, $94, $93, $93, $92, $92, $91, $91,
    $90, $90, $8B, $8B, $8A, $8A, $89, $89, $89, $88, $88, $87, $87, $87, $86, $86, $85, $85, $84,
    $84, $83, $83, $82, $82, $82, $81, $81, $81, $80, $80, $7F, $7F, $7F, $7E, $7E, $7E, $7D, $7D,
    $7C, $7C, $7C, $7B, $7B, $7B, $7A, $7A, $7A, $79, $79, $79, $78, $78, $77, $77, $77, $76, $76,
    $76, $75, $75, $75, $74, $74, $74, $73, $73, $73, $73, $72, $72, $72, $71, $71, $71, $70, $70,
    $70, $70, $6F, $6F, $6F, $6E, $6E, $6E, $6D, $6D, $6D, $6C, $6C, $6C, $6C, $6B, $6B, $6B, $6B,
    $6A, $6A, $6A, $6A, $69, $69, $69, $68, $68, $68, $68, $68, $67, $67, $67, $66, $66, $66, $66,
    $65, $65, $65, $65, $64, $64, $64, $64, $64, $63, $63, $63, $63, $62, $62, $62);
begin
  dac_vol := voltable[valor];
  dac_0.signed_data16_w(dac_out * dac_vol);
end;

procedure ay2_porta_write(valor: byte);
begin
  marcade.in4 := valor and $F0;
end;

procedure ay3_portb_write(valor: byte);
begin
  sound_nmi[0] := (not(valor) and 1) <> 0;
  if (sound_nmi[0] and sound_nmi[1]) then
    z80_1.change_nmi(PULSE_LINE);
end;

procedure taitosj_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

procedure taitosj_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
  ay8910_2.update;
  ay8910_3.update;
  dac_0.update;
end;

// Main
procedure taitosj_reset;
begin
  z80_0.reset;
  z80_1.reset;
  if main_vars.machine_type = 185 then
    m6805_0.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  ay8910_2.reset;
  ay8910_3.reset;
  dac_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  marcade.in4 := 0;
  fillchar(collision_reg, 4, 0);
  gfx_pos := 0;
  video_priority := 0;
  fillchar(scroll[0], 6, 0);
  colorbank[0] := 0;
  colorbank[1] := 0;
  rechars1 := false;
  rechars2 := false;
  rom_bank := 0;
  video_mode := 0;
  dac_vol := 0;
  sound_semaphore := false;
  sound_nmi[0] := false;
  sound_nmi[1] := false;
  // mcu
  mcu_zaccept := true;
  mcu_zready := false;
  mcu_busreq := false;
end;

function start_taitosj: boolean;
const
  resistances: array [0 .. 2] of integer = (1000, 470, 270);
var
  memory_temp: array [0 .. $8FFF] of byte;
  i, j, mask, data: byte;
begin
  start_taitosj := false;
  case main_vars.machine_type of
    185:
      machine_calls.general_loop := taitosj_mcu_loop;
    189:
      machine_calls.general_loop := taitosj_nomcu_loop;
  end;
  machine_calls.reset := taitosj_reset;
  start_audio(false);
  // Back
  screen_init(1, 256, 256, true);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  // Mid
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  // Front
  screen_init(3, 256, 256, true);
  screen_mod_scroll(3, 256, 256, 255, 256, 256, 255);
  screen_init(4, 256, 256, false, true); // Final
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 256);
  // Sound CPU
  z80_1 := cpu_z80.create(3000000, 256);
  z80_1.change_ram_calls(taitosj_snd_getbyte, taitosj_snd_putbyte);
  z80_1.init_sound(taitosj_sound_update);
  // IRQ sonido
  timers.init(z80_1.numero_cpu, 3000000 / (6000000 / (4 * 16 * 16 * 10 * 16)), taitosj_snd_irq,
    nil, true);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(1500000, AY8910, 0.15);
  ay8910_0.change_io_calls(ay0_porta_read, ay0_portb_read, nil, nil);
  ay8910_1 := ay8910_chip.create(1500000, AY8910, 0.5);
  ay8910_1.change_io_calls(nil, nil, ay1_porta_write, ay1_portb_write);
  ay8910_2 := ay8910_chip.create(1500000, AY8910, 0.5);
  ay8910_2.change_io_calls(nil, nil, ay2_porta_write, nil);
  ay8910_3 := ay8910_chip.create(1500000, AY8910, 1);
  ay8910_3.change_io_calls(nil, nil, nil, ay3_portb_write);
  dac_0 := dac_chip.create(0.15);
  case main_vars.machine_type of
    185:
      begin // Elevator Action
        z80_0.change_ram_calls(taitosj_mcu_getbyte, taitosj_mcu_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, elevator_rom)) then
          exit;
        // Poner roms en sus bancos
        copymemory(@memory[0], @memory_temp[0], $6000);
        copymemory(@memory_rom[0, 0], @memory_temp[$6000], $2000);
        copymemory(@memory_rom[1, 0], @memory_temp[$6000], $1000);
        copymemory(@memory_rom[1, $1000], @memory_temp[$8000], $1000);
        // cargar roms sonido
        if not(roms_load(@mem_snd, elevator_sonido)) then
          exit;
        // MCU CPU
        if not(roms_load(@mcu_mem, elevator_mcu)) then
          exit;
        m6805_0 := cpu_m6805.create(3000000, 256, tipo_m68705);
        m6805_0.change_ram_calls(mcu_taitosj_getbyte, mcu_taitosj_putbyte);
        // cargar chars
        if not(roms_load(@gfx_rom, elevator_char)) then
          exit;
        // Calculo de prioridades
        if not(roms_load(@memory_temp, elevator_prom)) then
          exit;
        marcade.dswa := $7F;
        marcade.dswa_val := @elevator_dip_a;
        marcade.dswc := $FF;
        marcade.dswc_val := @elevator_dip_c;
        pos_x[0] := -8;
        pos_x[1] := -23;
        pos_x[2] := -21;
        pos_x[3] := -2;
        pos_x[4] := 0;
      end;
    189:
      begin // Jungle King
        main_screen.rot180_screen := true;
        z80_0.change_ram_calls(taitosj_nomcu_getbyte, taitosj_nomcu_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, junglek_rom)) then
          exit;
        // Poner roms en sus bancos
        copymemory(@memory[0], @memory_temp[0], $6000);
        copymemory(@memory_rom[0, 0], @memory_temp[$6000], $2000);
        copymemory(@memory_rom[1, 0], @memory_temp[$6000], $1000);
        copymemory(@memory_rom[1, $1000], @memory_temp[$8000], $1000);
        // cargar roms sonido
        if not(roms_load(@mem_snd, junglek_sonido)) then
          exit;
        // cargar chars
        if not(roms_load(@gfx_rom, junglek_char)) then
          exit;
        // Calculo de prioridades
        if not(roms_load(@memory_temp, junglek_prom)) then
          exit;
        marcade.dswa := $3F;
        marcade.dswa_val := @junglek_dip_a;
        marcade.dswc := $FF;
        marcade.dswc_val := @junglek_dip_c;
        pos_x[0] := 8;
        pos_x[1] := 10;
        pos_x[2] := 12;
        pos_x[3] := 1;
        pos_x[4] := -2;
      end;
  end;
  // crear gfx
  init_gfx(0, 8, 8, 256);
  gfx[0].trans[0] := true;
  init_gfx(1, 16, 16, 64);
  gfx[1].trans[0] := true;
  init_gfx(2, 8, 8, 256);
  gfx[2].trans[0] := true;
  init_gfx(3, 16, 16, 64);
  gfx[3].trans[0] := true;
  marcade.dswb := $0;
  marcade.dswb_val := @coin_dip;
  for i := 0 to $1F do
  begin
    mask := 0;
    // start with all four layers active, so we'll get the highest
    // priority one in the first loop
    for j := 3 downto 0 do
    begin
      data := memory_temp[$10 * (i and $0F) + mask] and $0F;
      if (i and $10) <> 0 then
        data := data shr 2
      else
        data := data and $03;
      mask := mask or (1 shl data);
      // in next loop, we'll see which of the remaining
      // layers has top priority when this one is transparent
      draw_order[i, j] := data;
    end;
  end;
  // precalculo de la paleta
  compute_resistor_weights(0, 255, -1.0, 3, @resistances[0], @rweights[0], 0, 0, 3, @resistances[0],
    @gweights[0], 0, 0, 3, @resistances[0], @bweights[0], 0, 0);
  // final
  taitosj_reset;
  start_taitosj := true;
end;

end.
