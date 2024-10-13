unit shadow_warriors_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  oki6295,
  rom_engine,
  pal_engine,
  sound_engine;

function start_shadoww: boolean;

implementation

const
  shadoww_rom: array [0 .. 1] of tipo_roms = ((n: 'shadowa_1.3s'; l: $20000; p: 0; crc: $8290D567),
    (n: 'shadowa_2.4s'; l: $20000; p: $1; crc: $F3F08921));
  shadoww_sound: tipo_roms = (n: 'gaiden_3.4b'; l: $10000; p: 0; crc: $75FD3E6A);
  shadoww_char: tipo_roms = (n: 'gaiden_5.7a'; l: $10000; p: 0; crc: $8D4035F7);
  shadoww_bg: array [0 .. 3] of tipo_roms = ((n: '14.3a'; l: $20000; p: 0; crc: $1ECFDDAA),
    (n: '15.3b'; l: $20000; p: $20000; crc: $1291A696), (n: '16.1a'; l: $20000; p: $40000;
    crc: $140B47CA), (n: '17.1b'; l: $20000; p: $60000; crc: $7638CCCB));
  shadoww_fg: array [0 .. 3] of tipo_roms = ((n: '18.6a'; l: $20000; p: 0; crc: $3FADAFD6),
    (n: '19.6b'; l: $20000; p: $20000; crc: $DDAE9D5B), (n: '20.4b'; l: $20000; p: $40000;
    crc: $08CF7A93), (n: '21.4b'; l: $20000; p: $60000; crc: $1AC892F5));
  shadoww_sprites: array [0 .. 7] of tipo_roms = ((n: '6.3m'; l: $20000; p: 0; crc: $E7CCDF9F),
    (n: '7.1m'; l: $20000; p: $1; crc: $016BEC95), (n: '8.3n'; l: $20000; p: $40000;
    crc: $7EF7F880), (n: '9.1n'; l: $20000; p: $40001; crc: $6E9B7FD3), (n: '10.3r'; l: $20000;
    p: $80000; crc: $A6451DEC), (n: '11.1r'; l: $20000; p: $80001; crc: $7FBFDF5E), (n: '12.3s';
    l: $20000; p: $C0000; crc: $94A836D8), (n: '13.1s'; l: $20000; p: $C0001; crc: $E9CAEA3B));
  shadoww_oki: tipo_roms = (n: '4.4a'; l: $20000; p: 0; crc: $B0E0FAF9);
  shadoww_dip: array [0 .. 7] of def_dip = ((mask: $E0; name: 'Coin A'; number: 8;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $80; dip_name: '4C 1C'), (dip_val: $40;
    dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $E0;
    dip_name: '1C 1C'), (dip_val: $60; dip_name: '1C 2C'), (dip_val: $A0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (), (), (), (), (), (), (), ())),
    (mask: $1C; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $10;
    dip_name: '4C 1C'), (dip_val: $08; dip_name: '3C 1C'), (dip_val: $04;
    dip_name: '2C 1C'), (dip_val: $1C; dip_name: '1C 1C'), (dip_val: $0C;
    dip_name: '1C 2C'), (dip_val: $14; dip_name: '1C 3C'), (dip_val: $18; dip_name: '1C 4C'), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $1; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $1; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $C000; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '1'), (dip_val: $C000; dip_name: '2'), (dip_val: $4000;
    dip_name: '3'), (dip_val: $8000; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $3000; name: 'Energy'; number: 4; dip: ((dip_val: $0; dip_name: '2'), (dip_val: $3000;
    dip_name: '3'), (dip_val: $1000; dip_name: '4'), (dip_val: $2000; dip_name: '5'), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $C00; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'TBL 1'), (dip_val: $800;
    dip_name: 'TBL 2'), (dip_val: $0; dip_name: 'TBL 3'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());
  wildfang_rom: array [0 .. 1] of tipo_roms = ((n: '1.3st'; l: $20000; p: 0; crc: $AB876C9B),
    (n: '2.5st'; l: $20000; p: $1; crc: $1DC74B3B));
  wildfang_sound: tipo_roms = (n: 'tkni3.bin'; l: $10000; p: 0; crc: $15623EC7);
  wildfang_char: tipo_roms = (n: 'tkni5.bin'; l: $10000; p: 0; crc: $5ED15896);
  wildfang_bg: array [0 .. 3] of tipo_roms = ((n: '14.3a'; l: $20000; p: 0; crc: $0D20C10C),
    (n: '15.3b'; l: $20000; p: $20000; crc: $3F40A6B4), (n: '16.1a'; l: $20000; p: $40000;
    crc: $0F31639E), (n: '17.1b'; l: $20000; p: $60000; crc: $F32C158E));
  wildfang_fg: tipo_roms = (n: 'tkni6.bin'; l: $80000; p: 0; crc: $F68FAFB1);
  wildfang_sprites: array [0 .. 1] of tipo_roms = ((n: 'tkni9.bin'; l: $80000; p: 0;
    crc: $D22F4239), (n: 'tkni8.bin'; l: $80000; p: $1; crc: $4931B184));
  wildfang_oki: tipo_roms = (n: 'tkni4.bin'; l: $20000; p: 0; crc: $A7A1DBCF);
  wildfang_dip: array [0 .. 8] of def_dip = ((mask: $E0; name: 'Coin A'; number: 8;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $80; dip_name: '4C 1C'), (dip_val: $40;
    dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $E0;
    dip_name: '1C 1C'), (dip_val: $60; dip_name: '1C 2C'), (dip_val: $A0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (), (), (), (), (), (), (), ())),
    (mask: $1C; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $10;
    dip_name: '4C 1C'), (dip_val: $08; dip_name: '3C 1C'), (dip_val: $04;
    dip_name: '2C 1C'), (dip_val: $1C; dip_name: '1C 1C'), (dip_val: $0C;
    dip_name: '1C 2C'), (dip_val: $14; dip_name: '1C 3C'), (dip_val: $18; dip_name: '1C 4C'), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $1; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $1; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $100; name: 'Title'; number: 2;
    dip: ((dip_val: $100; dip_name: 'Wild Fang'), (dip_val: $0; dip_name: 'Tecmo Knight'), (), (),
    (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C000; name: 'Lives'; number: 3;
    dip: ((dip_val: $8000; dip_name: '1'), (dip_val: $C000; dip_name: '2'), (dip_val: $4000;
    dip_name: '3'), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $3000;
    name: 'Difficulty (Tecmo Knight)'; number: 4;
    dip: ((dip_val: $3000; dip_name: 'Easy'), (dip_val: $1000; dip_name: 'Normal'), (dip_val: $2000;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C00; name: 'Difficulty (Wild Fang)'; number: 4;
    dip: ((dip_val: $C00; dip_name: 'Easy'), (dip_val: $400; dip_name: 'Normal'), (dip_val: $800;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());

var
  scroll_x_txt, scroll_y_txt, scroll_x_bg, scroll_y_bg, scroll_x_fg, scroll_y_fg: word;
  rom: array [0 .. $1FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  video_ram1: array [0 .. $7FF] of word;
  video_ram2, video_ram3, sprite_ram: array [0 .. $FFF] of word;
  sound_latch, scroll_y_txt_off, scroll_y_bg_off, scroll_y_fg_off: byte;
  // Proteccion
  wf_prot, wf_jumpcode: word;

procedure update_video_shadoww;
var
  f, x, y, nchar: word;
  color: byte;

  procedure draw_sprites(pri: byte);
  var
    posx, posy, sx, sy, atrib, nchar, color: word;
    f, sizex, sizey, row, col: byte;
    blend, flipx, flipy: boolean;
  const
    layout: array [0 .. 7, 0 .. 7] of byte = ((0, 1, 4, 5, 16, 17, 20, 21),
      (2, 3, 6, 7, 18, 19, 22, 23), (8, 9, 12, 13, 24, 25, 28, 29),
      (10, 11, 14, 15, 26, 27, 30, 31), (32, 33, 36, 37, 48, 49, 52, 53),
      (34, 35, 38, 39, 50, 51, 54, 55), (40, 41, 44, 45, 56, 57, 60, 61),
      (42, 43, 46, 47, 58, 59, 62, 63));

    procedure put_gfx_sprite_diff_sw(nchar: word; x_diff, y_diff: word);
    var
      x, y, pos_x, pos_y: byte;
      ptemp, ptemp2: pword;
      pos: pbyte;
      dir_x, dir_y: integer;
      punt, punt2, temp1, temp2, temp3: word;
    begin
      pos := gfx[3].datos;
      inc(pos, nchar * 8 * 8);
      pos_y := 7 * byte(flipy);
      if flipy then
        dir_y := -1
      else
        dir_y := 1;
      ptemp2 := punbuf;
      if flipx then
      begin
        inc(ptemp2, 7);
        dir_x := -1;
      end
      else
        dir_x := 1;
      for y := 0 to 7 do
      begin
        ptemp := ptemp2;
        pos_x := 7 * byte(flipx);
        for x := 0 to 7 do
        begin
          if not(gfx[3].trans[pos^]) then
          begin
            if blend then
            begin
              punt := getpixel(((posx + pos_x + x_diff) and $1FF) + ADD_SPRITE,
                ((posy + pos_y + y_diff) and $1FF) + ADD_SPRITE, 4);
              punt2 := paleta[pos^ + color + $400];
              temp1 := (((punt and $F800) + (punt2 and $F800)) shr 1) and $F800;
              temp2 := (((punt and $7E0) + (punt2 and $7E0)) shr 1) and $7E0;
              temp3 := (((punt and $1F) + (punt2 and $1F)) shr 1) and $1F;
              punt2 := temp1 or temp2 or temp3;
            end
            else
              punt2 := paleta[pos^ + color];
            ptemp^ := punt2;
          end
          else
            ptemp^ := paleta[MAX_COLORS];
          inc(ptemp, dir_x);
          pos_x := pos_x + dir_x;
          inc(pos);
        end;
        putpixel_gfx_int(0 + x_diff, pos_y + y_diff, 8, PANT_SPRITES);
        pos_y := pos_y + dir_y;
      end;
    end;

  begin
    for f := 0 to $FF do
    begin
      atrib := sprite_ram[f * 8];
      if ((atrib and $C0) shr 6) <> pri then
        continue;
      if (atrib and $4) = 0 then
        continue;
      flipx := (atrib and 1) <> 0;
      flipy := (atrib and 2) <> 0;
      blend := (atrib and $20) <> 0;
      color := sprite_ram[2 + (f * 8)];
      sizex := 1 shl (color and 3);
      sizey := sizex;
      nchar := sprite_ram[1 + (f * 8)];
      if (sizex >= 2) then
        nchar := nchar and $7FFE;
      if (sizey >= 2) then
        nchar := nchar and $7FFD;
      if (sizex >= 4) then
        nchar := nchar and $7FFB;
      if (sizey >= 4) then
        nchar := nchar and $7FF7;
      if (sizex >= 8) then
        nchar := nchar and $7FEF;
      if (sizey >= 8) then
        nchar := nchar and $7FDF;
      color := color and $F0;
      posx := sprite_ram[4 + (f * 8)] and $1FF;
      posy := sprite_ram[3 + (f * 8)] and $1FF;
      for row := 0 to (sizey - 1) do
      begin
        for col := 0 to (sizex - 1) do
        begin
          if flipx then
            sx := 8 * (sizex - 1 - col)
          else
            sx := 8 * col;
          if flipy then
            sy := 8 * (sizey - 1 - row)
          else
            sy := 8 * row;
          put_gfx_sprite_diff_sw(nchar + layout[row][col], sx, sy);
        end;
      end;
      actualiza_gfx_sprite_size(posx, posy, 4, 8 * sizex, 8 * sizey);
    end;
  end;

begin
  for f := $0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    color := (video_ram3[f] and $F0) shr 4;
    if (gfx[1].buffer[f] or buffer_color[color + $30]) then
    begin
      nchar := video_ram3[f + $800] and $FFF;
      put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $300, 2, 1);
      gfx[1].buffer[f] := false;
    end;
    color := (video_ram2[f] and $F0) shr 4;
    if (gfx[2].buffer[f] or buffer_color[color + $20]) then
    begin
      nchar := video_ram2[f + $800] and $FFF;
      put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $200, 3, 2);
      gfx[2].buffer[f] := false;
    end;
  end;
  for f := $0 to $3FF do
  begin
    color := (video_ram1[f] and $F0) shr 4;
    if (gfx[0].buffer[f] or buffer_color[color + $10]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := video_ram1[f + $400] and $7FF;
      put_gfx_trans(x * 8, y * 8, nchar, (color shl 4) + $100, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  fill_full_screen(4, $200);
  draw_sprites(3);
  scroll_x_y(2, 4, scroll_x_bg, scroll_y_bg - scroll_y_bg_off + 16);
  draw_sprites(2);
  scroll_x_y(3, 4, scroll_x_fg, scroll_y_fg - scroll_y_fg_off + 16);
  draw_sprites(1);
  scroll_x_y(1, 4, scroll_x_txt, scroll_y_txt - scroll_y_txt_off + 16);
  draw_sprites(0);
  actualiza_trozo_final(0, 16, 256, 224, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_shadoww;
begin
  if event.arcade then
  begin
    // P1/P2
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FEFF)
    else
      marcade.in1 := (marcade.in1 or $100);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $200);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FBFF)
    else
      marcade.in1 := (marcade.in1 or $400);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7FF)
    else
      marcade.in1 := (marcade.in1 or $800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure shadoww_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
      // main
      m68000_0.run(frame_m);
      frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
      // sound
      z80_0.run(frame_s);
      frame_s := frame_s + z80_0.tframes - z80_0.contador;
      if f = 239 then
      begin
        update_video_shadoww;
        m68000_0.irq[5] := ASSERT_LINE;
      end;
    end;
    events_shadoww;
    video_sync;
  end;
end;

function shadoww_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      shadoww_getword := rom[direccion shr 1];
    $60000 .. $63FFF:
      shadoww_getword := ram[(direccion and $3FFF) shr 1];
    $70000 .. $70FFF:
      shadoww_getword := video_ram1[(direccion and $FFF) shr 1];
    $72000 .. $73FFF:
      shadoww_getword := video_ram2[(direccion and $1FFF) shr 1];
    $74000 .. $75FFF:
      shadoww_getword := video_ram3[(direccion and $1FFF) shr 1];
    $76000 .. $77FFF:
      shadoww_getword := sprite_ram[(direccion and $1FFF) shr 1];
    $78000 .. $79FFF:
      shadoww_getword := buffer_paleta[(direccion and $1FFF) shr 1];
    $7A000:
      shadoww_getword := marcade.in0;
    $7A002:
      shadoww_getword := marcade.in1;
    $7A004:
      shadoww_getword := marcade.dswa;
  end;
end;

procedure shadoww_putword(direccion: dword; valor: word);
  procedure cambiar_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal4bit(tmp_color shr 8);
    color.g := pal4bit(tmp_color shr 4);
    color.r := pal4bit(tmp_color);
    set_pal_color(color, numero);
    case numero of
      $100 .. $1FF:
        buffer_color[((numero shr 4) and $F) + $10] := true;
      $200 .. $2FF:
        buffer_color[((numero shr 4) and $F) + $20] := true;
      $300 .. $3FF:
        buffer_color[((numero shr 4) and $F) + $30] := true;
    end;
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ;
    $60000 .. $63FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $70000 .. $70FFF:
      if video_ram1[(direccion and $FFF) shr 1] <> valor then
      begin
        gfx[0].buffer[((direccion and $FFF) shr 1) and $3FF] := true;
        video_ram1[(direccion and $FFF) shr 1] := valor;
      end;
    $72000 .. $73FFF:
      if video_ram2[(direccion and $1FFF) shr 1] <> valor then
      begin
        gfx[2].buffer[((direccion and $1FFF) shr 1) and $7FF] := true;
        video_ram2[(direccion and $1FFF) shr 1] := valor;
      end;
    $74000 .. $75FFF:
      if video_ram3[(direccion and $1FFF) shr 1] <> valor then
      begin
        gfx[1].buffer[((direccion and $1FFF) shr 1) and $7FF] := true;
        video_ram3[(direccion and $1FFF) shr 1] := valor;
      end;
    $76000 .. $77FFF:
      sprite_ram[(direccion and $1FFF) shr 1] := valor;
    $78000 .. $79FFF:
      if buffer_paleta[(direccion and $1FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $1FFF) shr 1] := valor;
        cambiar_color(valor, ((direccion and $1FFF) shr 1));
      end;
    $7A104:
      scroll_y_txt := valor;
    $7A108:
      scroll_y_txt_off := valor and $FF;
    $7A10C:
      scroll_x_txt := valor;
    $7A204:
      scroll_y_fg := valor;
    $7A208:
      scroll_y_fg_off := valor and $FF;
    $7A20C:
      scroll_x_fg := valor;
    $7A304:
      scroll_y_bg := valor;
    $7A308:
      scroll_y_bg_off := valor and $FF;
    $7A30C:
      scroll_x_bg := valor;
    $7A800:
      ; // watchdog
    $7A802:
      begin
        sound_latch := valor;
        z80_0.change_nmi(ASSERT_LINE);
      end;
    $7A806:
      m68000_0.irq[5] := CLEAR_LINE;
    $7A808:
      main_screen.flip_main_screen := (valor and 1) <> 0;
  end;
end;

function shadoww_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $F7FF:
      shadoww_snd_getbyte := mem_snd[direccion];
    $F800:
      shadoww_snd_getbyte := oki_6295_0.read;
    $FC20:
      begin
        shadoww_snd_getbyte := sound_latch;
        z80_0.change_nmi(CLEAR_LINE);
      end;
  end;
end;

procedure shadoww_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $EFFF:
      ;
    $F000 .. $F7FF:
      mem_snd[direccion] := valor;
    $F800:
      oki_6295_0.write(valor);
    $F810:
      ym2203_0.Control(valor);
    $F811:
      ym2203_0.write(valor);
    $F820:
      ym2203_1.Control(valor);
    $F821:
      ym2203_1.write(valor);
  end;
end;

procedure shadoww_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
  oki_6295_0.Update;
end;

procedure shadoww_snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

// Wild Fang/Tecmo Knight
function wildfang_getword(direccion: dword): word;
begin
  case direccion of
    $7A006:
      wildfang_getword := wf_prot;
  else
    wildfang_getword := shadoww_getword(direccion);
  end;
end;

procedure wildfang_putword(direccion: dword; valor: word);
const
  jumppoints: array [0 .. 16] of word = ($0C0C, $0CAC, $0D42, $0DA2, $0EEA, $112E, $1300, $13FA,
    $159A, $1630, $109A, $1700, $1750, $1806, $18D6, $1A44, $1B52);
begin
  case direccion of
    $7A802:
      begin
        sound_latch := valor shr 8;
        z80_0.change_nmi(ASSERT_LINE);
      end;
    $7A804:
      begin
        valor := valor shr 8;
        case (valor and $F0) of
          $00:
            wf_prot := 0; // init
          $10:
            begin // high 4 bits of jump code
              wf_jumpcode := (valor and $0F) shl 4;
              wf_prot := $10;
            end;
          $20:
            begin // low 4 bits of jump code
              wf_jumpcode := wf_jumpcode or (valor and $0F);
              wf_prot := $20;
            end;
          $30:
            wf_prot := $40 or ((jumppoints[wf_jumpcode] shr 12) and $0F);
          // ask for bits 12-15 of function address
          $40:
            wf_prot := $50 or ((jumppoints[wf_jumpcode] shr 8) and $0F);
          // ask for bits 8-11 of function address
          $50:
            wf_prot := $60 or ((jumppoints[wf_jumpcode] shr 4) and $0F);
          // ask for bits 4-7 of function address
          $60:
            wf_prot := $70 or ((jumppoints[wf_jumpcode] shr 0) and $0F);
          // ask for bits 0-3 of function address
        end;
      end;
  else
    shadoww_putword(direccion, valor);
  end;
end;

// Main
procedure reset_shadoww;
begin
  m68000_0.reset;
  z80_0.reset;
  ym2203_0.reset;
  ym2203_1.reset;
  oki_6295_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FFFF;
  scroll_x_bg := 0;
  scroll_y_bg := 0;
  scroll_x_fg := 0;
  scroll_y_fg := 0;
  scroll_y_txt_off := 0;
  scroll_y_bg_off := 0;
  scroll_y_fg_off := 0;
  sound_latch := 0;
  wf_prot := 0;
  wf_jumpcode := 0;
end;

function start_shadoww: boolean;
var
  ptemp: pbyte;
const
  pg_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4,
    8 * 32 + 0 * 4, 8 * 32 + 1 * 4, 8 * 32 + 2 * 4, 8 * 32 + 3 * 4, 8 * 32 + 4 * 4, 8 * 32 + 5 * 4,
    8 * 32 + 6 * 4, 8 * 32 + 7 * 4);
  pg_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32,
    16 * 32 + 0 * 32, 16 * 32 + 1 * 32, 16 * 32 + 2 * 32, 16 * 32 + 3 * 32, 16 * 32 + 4 * 32,
    16 * 32 + 5 * 32, 16 * 32 + 6 * 32, 16 * 32 + 7 * 32);
  procedure convert_8(num: word; chr: byte);
  begin
    init_gfx(chr, 8, 8, num);
    gfx[chr].trans[0] := true;
    gfx_set_desc_data(4, 0, 8 * 8 * 4, 0, 1, 2, 3);
    convert_gfx(chr, 0, ptemp, @pg_x, @pg_y, false, false);
  end;
  procedure convert_16(num: word; chr: byte);
  begin
    init_gfx(chr, 16, 16, num);
    gfx[chr].trans[0] := true;
    gfx_set_desc_data(4, 0, 4 * 8 * 32, 0, 1, 2, 3);
    convert_gfx(chr, 0, ptemp, @pg_x, @pg_y, false, false);
  end;

begin
  machine_calls.general_loop := shadoww_loop;
  machine_calls.reset := reset_shadoww;
  machine_calls.fps_max := 59.169998;
  start_shadoww := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 1024, 512, true);
  screen_mod_scroll(2, 1024, 512, 1023, 512, 512, 511);
  screen_init(3, 1024, 512, true);
  screen_mod_scroll(3, 1024, 512, 1023, 512, 512, 511);
  screen_init(4, 512, 512, false, true);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(18432000 div 2, 256);
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, 256);
  z80_0.change_ram_calls(shadoww_snd_getbyte, shadoww_snd_putbyte);
  z80_0.init_sound(shadoww_sound_update);
  // Sound Chips
  ym2203_0 := ym2203_chip.create(4000000, 1, 0.5);
  ym2203_0.change_irq_calls(shadoww_snd_irq);
  ym2203_1 := ym2203_chip.create(4000000, 1, 0.5);
  oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH, 0.5);
  getmem(ptemp, $100000);
  case main_vars.machine_type of
    338:
      begin // Shadow Warriors
        // cargar roms
        if not(roms_load16w(@rom, shadoww_rom)) then
          exit;
        m68000_0.change_ram16_calls(shadoww_getword, shadoww_putword);
        // cargar sonido
        if not(roms_load(@mem_snd, shadoww_sound)) then
          exit;
        if not(roms_load(oki_6295_0.get_rom_addr, shadoww_oki)) then
          exit;
        // convertir chars
        if not(roms_load(ptemp, shadoww_char)) then
          exit;
        convert_8($800, 0);
        // convertir fondo
        if not(roms_load(ptemp, shadoww_bg)) then
          exit;
        convert_16($1000, 1);
        if not(roms_load(ptemp, shadoww_fg)) then
          exit;
        convert_16($1000, 2);
        // convertir sprites
        if not(roms_load16b(ptemp, shadoww_sprites)) then
          exit;
        convert_8($8000, 3);
        // DIP
        marcade.dswa := $FFFF;
        marcade.dswa_val := @shadoww_dip;
      end;
    339:
      begin // Wild Fang/Tecmo Knight
        // cargar roms
        if not(roms_load16w(@rom, wildfang_rom)) then
          exit;
        m68000_0.change_ram16_calls(wildfang_getword, wildfang_putword);
        // cargar sonido
        if not(roms_load(@mem_snd, wildfang_sound)) then
          exit;
        if not(roms_load(oki_6295_0.get_rom_addr, wildfang_oki)) then
          exit;
        // convertir chars
        if not(roms_load(ptemp, wildfang_char)) then
          exit;
        convert_8($800, 0);
        // convertir fondo
        if not(roms_load(ptemp, wildfang_bg)) then
          exit;
        convert_16($1000, 1);
        if not(roms_load(ptemp, wildfang_fg)) then
          exit;
        convert_16($1000, 2);
        // convertir sprites
        if not(roms_load16b(ptemp, wildfang_sprites)) then
          exit;
        convert_8($8000, 3);
        // DIP
        marcade.dswa := $FFFF;
        marcade.dswa_val := @wildfang_dip;
      end;
  end;
  freemem(ptemp);
  // final
  reset_shadoww;
  start_shadoww := true;
end;

end.
