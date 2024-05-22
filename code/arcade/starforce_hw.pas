unit starforce_hw; // Senjyo

interface

uses
  WinApi.Windows,
  nz80,
  z80pio,
  z80daisy,
  main_engine,
  controls_engine,
  gfx_engine,
  sn_76496,
  z80ctc,
  rom_engine,
  pal_engine,
  sound_engine;

function start_starforce: boolean;

implementation

const
  starforce_rom: array [0 .. 1] of tipo_roms = ((n: '3.3p'; l: $4000; p: 0; crc: $8BA27691),
    (n: '2.3mn'; l: $4000; p: $4000; crc: $0FC4D2D6));
  starforce_char: array [0 .. 2] of tipo_roms = ((n: '7.2fh'; l: $1000; p: 0; crc: $F4803339),
    (n: '8.3fh'; l: $1000; p: $1000; crc: $96979684), (n: '9.3fh'; l: $1000; p: $2000;
    crc: $EEAD1D5C));
  starforce_bg1: array [0 .. 2] of tipo_roms = ((n: '15.10jk'; l: $2000; p: 0; crc: $C3BDA12F),
    (n: '14.9jk'; l: $2000; p: $2000; crc: $9E9384FE), (n: '13.8jk'; l: $2000; p: $4000;
    crc: $84603285));
  starforce_bg2: array [0 .. 2] of tipo_roms = ((n: '12.10de'; l: $2000; p: 0; crc: $FDD9E38B),
    (n: '11.9de'; l: $2000; p: $2000; crc: $668AEA14), (n: '10.8de'; l: $2000; p: $4000;
    crc: $C62A19C1));
  starforce_bg3: array [0 .. 2] of tipo_roms = ((n: '18.10pq'; l: $1000; p: 0; crc: $6455C3AD),
    (n: '17.9pq'; l: $1000; p: $1000; crc: $68C60D0F), (n: '16.8pq'; l: $1000; p: $2000;
    crc: $CE20B469));
  starforce_sound: tipo_roms = (n: '1.3hj'; l: $2000; p: 0; crc: $2735BB22);
  starforce_sprites: array [0 .. 2] of tipo_roms = ((n: '6.10lm'; l: $4000; p: 0; crc: $5468A21D),
    (n: '5.9lm'; l: $4000; p: $4000; crc: $F71717F8), (n: '4.8lm'; l: $4000; p: $8000;
    crc: $DD9D68A4));
  // DIP
  starforce_dipa: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin A'; number: 4;
    dip: ((dip_val: $1; dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $3; dip_name: '1C 3C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $4; dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $8;
    dip_name: '1C 2C'), (dip_val: $C; dip_name: '1C 3C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $30; name: 'Lives'; number: 4;
    dip: ((dip_val: $30; dip_name: '2'), (dip_val: $0; dip_name: '3'), (dip_val: $10;
    dip_name: '4'), (dip_val: $20; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Cabinet'; number: 2; dip: ((dip_val: $40; dip_name: 'Upright'), (dip_val: $0;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'On'), (dip_val: $0;
    dip_name: 'Off'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  starforce_dipb: array [0 .. 2] of def_dip = ((mask: $7; name: 'Bonus Life'; number: 8;
    dip: ((dip_val: $0; dip_name: '50k 200k 500k'), (dip_val: $1; dip_name: '100k 300k 800k'),
    (dip_val: $2; dip_name: '50k 200k'), (dip_val: $3; dip_name: '100k 300k'), (dip_val: $4;
    dip_name: '50k'), (dip_val: $5; dip_name: '100k'), (dip_val: $6;
    dip_name: '200k'), (dip_val: $7; dip_name: 'None'), (), (), (), (), (), (), (), ())),
    (mask: $38; name: 'Difficulty'; number: 6;
    dip: ((dip_val: $0; dip_name: 'Easyest'), (dip_val: $8; dip_name: 'Easy'), (dip_val: $10;
    dip_name: 'Medium'), (dip_val: $18; dip_name: 'Difficult'), (dip_val: $20;
    dip_name: 'Hard'), (dip_val: $28; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (),
    ())), ());
  starforce_dipc: array [0 .. 1] of def_dip = ((mask: $1; name: 'Inmunnity'; number: 2;
    dip: ((dip_val: $1; dip_name: 'On'), (dip_val: $0; dip_name: 'Off'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), ());
  // Senjyo
  senjyo_rom: array [0 .. 3] of tipo_roms = ((n: '08m_05t.bin'; l: $2000; p: 0; crc: $B1F3544D),
    (n: '08k_04t.bin'; l: $2000; p: $2000; crc: $E34468A8), (n: '08j_03t.bin'; l: $2000; p: $4000;
    crc: $C33AEDEE), (n: '08f_02t.bin'; l: $2000; p: $6000; crc: $0EF4DB9E));
  senjyo_sound: tipo_roms = (n: '02h_01t.bin'; l: $2000; p: 0; crc: $C1C24455);
  senjyo_char: array [0 .. 2] of tipo_roms = ((n: '08h_08b.bin'; l: $1000; p: 0; crc: $0C875994),
    (n: '08f_07b.bin'; l: $1000; p: $1000; crc: $497BEA8E), (n: '08d_06b.bin'; l: $1000; p: $2000;
    crc: $4EF69B00));
  senjyo_bg1: array [0 .. 1] of tipo_roms = ((n: '05n_16m.bin'; l: $1000; p: 0; crc: $0D3E00FB),
    (n: '05k_15m.bin'; l: $2000; p: $2000; crc: $93442213));
  senjyo_bg2: array [0 .. 1] of tipo_roms = ((n: '07n_18m.bin'; l: $1000; p: 0; crc: $D50FCED3),
    (n: '07k_17m.bin'; l: $2000; p: $2000; crc: $10C3A5F0));
  senjyo_bg3: array [0 .. 1] of tipo_roms = ((n: '09n_20m.bin'; l: $1000; p: 0; crc: $54CB8126),
    (n: '09k_19m.bin'; l: $2000; p: $1000; crc: $373E047C));
  senjyo_sprites: array [0 .. 5] of tipo_roms = ((n: '08p_13b.bin'; l: $2000; p: 0; crc: $40127EFD),
    (n: '08s_14b.bin'; l: $2000; p: $2000; crc: $42648FFA), (n: '08m_11b.bin'; l: $2000; p: $4000;
    crc: $CCC4680B), (n: '08n_12b.bin'; l: $2000; p: $6000; crc: $742FAFED), (n: '08j_09b.bin';
    l: $2000; p: $8000; crc: $1EE63B5C), (n: '08k_10b.bin'; l: $2000; p: $A000; crc: $A9F41EC9));
  senjyo_dipb: array [0 .. 2] of def_dip = ((mask: $2; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $2; dip_name: '100k'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $80; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Medium'), (dip_val: $0;
    dip_name: 'Hard'), (dip_val: $C0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());
  senjyo_dipc: array [0 .. 1] of def_dip = ((mask: $F; name: 'Disable Enemy Fire'; number: 2;
    dip: ((dip_val: $F; dip_name: 'On'), (dip_val: $0; dip_name: 'Off'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), ());
  // Baluba
  baluba_rom: array [0 .. 1] of tipo_roms = ((n: '0'; l: $4000; p: 0; crc: $0E2EBE32), (n: '1';
    l: $4000; p: $4000; crc: $CDE97076));
  baluba_char: array [0 .. 2] of tipo_roms = ((n: '15'; l: $1000; p: 0; crc: $3DDA0D84), (n: '16';
    l: $1000; p: $1000; crc: $3EBC79D8), (n: '17'; l: $1000; p: $2000; crc: $C4430DEB));
  baluba_bg1: array [0 .. 2] of tipo_roms = ((n: '9'; l: $2000; p: 0; crc: $90F88C43), (n: '10';
    l: $2000; p: $2000; crc: $AB117070), (n: '11'; l: $2000; p: $4000; crc: $E13B44B0));
  baluba_bg2: array [0 .. 2] of tipo_roms = ((n: '12'; l: $2000; p: 0; crc: $A6541C8D), (n: '13';
    l: $2000; p: $2000; crc: $AFCCDD18), (n: '14'; l: $2000; p: $4000; crc: $69542E65));
  baluba_bg3: array [0 .. 2] of tipo_roms = ((n: '8'; l: $1000; p: 0; crc: $31E97EF9), (n: '7';
    l: $1000; p: $1000; crc: $5915C5E2), (n: '6'; l: $1000; p: $2000; crc: $AD6881DA));
  baluba_sound: tipo_roms = (n: '2'; l: $2000; p: 0; crc: $441FBC64);
  baluba_sprites: array [0 .. 2] of tipo_roms = ((n: '5'; l: $4000; p: 0; crc: $3B6B6E96), (n: '4';
    l: $4000; p: $4000; crc: $DD954124), (n: '3'; l: $4000; p: $8000; crc: $7AC24983));

var
  scroll_x: array [0 .. 2] of word;
  scroll_y: array [0 .. 2] of byte;
  sound_latch: byte;
  draw_video: procedure;
  char_scroll: array [0 .. $1F] of word;

procedure update_video_starforce;
const
  color_code: array [0 .. 7] of byte = (0, 2, 4, 6, 1, 3, 5, 7);
var
  nchar, f: word;
  stripe, pen, color, x, y, atrib: byte;
  count: integer;
  procedure draw_sprites_starforce(prioridad: byte);
  var
    nchar, x, y, color, f: word;
    atrib: byte;
  begin
    for f := $1F downto 0 do
    begin
      atrib := memory[$9801 + (f * 4)];
      if ((atrib and $30) shr 4) = prioridad then
      begin
        nchar := memory[$9800 + (f * 4)];
        x := memory[$9802 + (f * 4)];
        y := memory[$9803 + (f * 4)];
        color := (atrib and 7) shl 3 + 320;
        if (nchar and $C0) <> $C0 then
        begin
          put_gfx_sprite(nchar, color, (atrib and $80) <> 0, (atrib and $40) <> 0, 4);
          update_gfx_sprite(x, y, 5, 4);
        end
        else
        begin // Big
          put_gfx_sprite_diff(((nchar and $7F) shl 2) + 2, color, (atrib and $80) <> 0,
            (atrib and $40) <> 0, 4, 0, 0);
          put_gfx_sprite_diff(((nchar and $7F) shl 2) + 0, color, (atrib and $80) <> 0,
            (atrib and $40) <> 0, 4, 16, 0);
          put_gfx_sprite_diff(((nchar and $7F) shl 2) + 3, color, (atrib and $80) <> 0,
            (atrib and $40) <> 0, 4, 0, 16);
          put_gfx_sprite_diff(((nchar and $7F) shl 2) + 1, color, (atrib and $80) <> 0,
            (atrib and $40) <> 0, 4, 16, 16);
          actualiza_gfx_sprite_size(x, y, 5, 32, 32);
        end;
      end;
    end;
  end;

begin
  // No usa stripe de fondo ni radar!
  for f := 0 to $1FF do
  begin
    // bg1
    nchar := memory[$B000 + f];
    color := color_code[((nchar and $E0) shr 5)];
    if (gfx[1].buffer[f] or buffer_color[color + $18]) then
    begin
      x := 31 - (f div 16);
      y := f mod 16;
      put_gfx_trans(x * 16, y * 16, nchar, (color shl 3) + 64, 1, 1);
      gfx[1].buffer[f] := false;
    end;
    // bg2
    atrib := memory[$A800 + f];
    color := (atrib and $E0) shr 5;
    if (gfx[2].buffer[f] or buffer_color[color + $10]) then
    begin
      x := 31 - (f div 16);
      y := f mod 16;
      put_gfx_trans(x * 16, y * 16, atrib, (color shl 3) + 128, 2, 2);
      gfx[2].buffer[f] := false;
    end;
    // bg3
    atrib := memory[$A000 + f];
    color := (atrib and $E0) shr 5;
    if (gfx[3].buffer[f] or buffer_color[color + 8]) then
    begin
      x := 31 - (f div 16);
      y := f mod 16;
      nchar := atrib and $7F;
      put_gfx(x * 16, y * 16, nchar, (color shl 3) + 192, 3, 3);
      gfx[3].buffer[f] := false;
    end;
  end;
  // chars
  for f := 0 to $3FF do
  begin
    atrib := memory[$9400 + f];
    color := atrib and $7;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      nchar := memory[$9000 + f] + ((atrib and $10) shl 4);
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 3, 4, 0, (atrib and $80) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  draw_sprites_starforce(0);
  scroll_x_y(3, 5, 256 - scroll_x[2], scroll_y[2]); // bg3
  draw_sprites_starforce(1);
  // OJO!! Que esta version no los valores del scroll2!!!
  scroll_x_y(2, 5, 256 - scroll_x[0], scroll_y[0]); // bg2
  draw_sprites_starforce(2);
  scroll_x_y(1, 5, 256 - scroll_x[0], scroll_y[0]); // bg1
  draw_sprites_starforce(3);
  actualiza_trozo(0, 0, 256, 256, 4, 0, 0, 256, 256, 5); // chars
  actualiza_trozo_final(16, 0, 224, 256, 5);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure draw_sprites_senjyo(prioridad: byte);
var
  nchar, color, f: word;
  x, y, atrib: byte;
begin
  for f := $1F downto 0 do
  begin
    atrib := memory[$9801 + (f * 4)];
    if ((atrib and $30) shr 4) = prioridad then
    begin
      nchar := memory[$9800 + (f * 4)];
      x := memory[$9802 + (f * 4)];
      y := memory[$9803 + (f * 4)];
      color := (atrib and 7) shl 3 + 320;
      if (nchar and $80) = 0 then
      begin
        put_gfx_sprite(nchar, color, (atrib and $80) <> 0, (atrib and $40) <> 0, 4);
        update_gfx_sprite(x, y, 5, 4);
      end
      else
      begin // Big
        put_gfx_sprite_diff(((nchar and $7F) shl 2) + 2, color, (atrib and $80) <> 0,
          (atrib and $40) <> 0, 4, 0, 0);
        put_gfx_sprite_diff(((nchar and $7F) shl 2) + 0, color, (atrib and $80) <> 0,
          (atrib and $40) <> 0, 4, 16, 0);
        put_gfx_sprite_diff(((nchar and $7F) shl 2) + 3, color, (atrib and $80) <> 0,
          (atrib and $40) <> 0, 4, 0, 16);
        put_gfx_sprite_diff(((nchar and $7F) shl 2) + 1, color, (atrib and $80) <> 0,
          (atrib and $40) <> 0, 4, 16, 16);
        actualiza_gfx_sprite_size(x, y, 5, 32, 32);
      end;
    end;
  end;
end;

procedure update_video_senjyo;
var
  sx, sy, nchar, f: word;
  stripe, pen, x, y, color, atrib: byte;
  count: integer;
begin
  // Fondo
  stripe := memory[$9E27] + 1;
  if stripe = 0 then
    fill_full_screen(5, 0)
  else
  begin
    pen := 0;
    count := 0;
    for y := 0 to 255 do
    begin
      single_line(0 + ADD_SPRITE, y + ADD_SPRITE, paleta[384 + pen], $100, 5);
      count := count + $10;
      if (count >= stripe) then
      begin
        pen := (pen + 1) and $0F;
        count := count - stripe;
      end;
    end;
  end;
  for f := 0 to $1FF do
  begin
    // bg1 512x256
    nchar := memory[$B000 + f];
    color := (nchar and $70) shr 4;
    if (gfx[1].buffer[f] or buffer_color[color + $18]) then
    begin
      x := 31 - (f div 16);
      y := f mod 16;
      put_gfx_trans(x * 16, y * 16, nchar, (color shl 3) + $40, 1, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  for f := 0 to $2FF do
  begin
    // bg2 768x256
    atrib := memory[$A800 + f];
    color := (atrib and $E0) shr 5;
    if (gfx[2].buffer[f] or buffer_color[color + $10]) then
    begin
      x := 48 - (f div 16);
      y := f mod 16;
      put_gfx_trans(x * 16, y * 16, atrib, (color shl 3) + $80, 2, 2);
      gfx[2].buffer[f] := false;
    end
  end;
  for f := 0 to $37F do
  begin
    // bg3 896x256
    atrib := memory[$A000 + f];
    color := (atrib and $E0) shr 5;
    if (gfx[3].buffer[f] or buffer_color[color + 8]) then
    begin
      x := 56 - (f div 16);
      y := f mod 16;
      nchar := atrib and $7F;
      put_gfx_trans(x * 16, y * 16, nchar, (color shl 3) + $C0, 3, 3);
      gfx[3].buffer[f] := false;
    end;
  end;
  // chars
  for f := 0 to $3FF do
  begin
    atrib := memory[$9400 + f];
    color := atrib and $7;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      nchar := memory[$9000 + f] + ((atrib and $10) shl 4);
      if y >= 24 then
        put_gfx_flip(x * 8, y * 8, nchar, color shl 3, 4, 0, (atrib and $80) <> 0, false)
      else
        put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 3, 4, 0, (atrib and $80) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  draw_sprites_senjyo(0);
  scroll_x_y(3, 5, scroll_x[2], scroll_y[2]); // bg3
  draw_sprites_senjyo(1);
  scroll_x_y(2, 5, scroll_x[1], scroll_y[1]); // bg2
  draw_sprites_senjyo(2);
  scroll_x_y(1, 5, scroll_x[0], scroll_y[0]); // bg1
  draw_sprites_senjyo(3);
  scroll__x_part2(4, 5, 8, @char_scroll[0]); // chars
  // Radar
  for f := 0 to $3FF do
  begin
    for x := 0 to 7 do
    begin
      if (memory[$B800 + f] and (1 shl x)) <> 0 then
      begin
        punbuf^ := paleta[$200 or ((f shr 9) and 1)]; // Si es <$200 el color $200 si es mayor $201
        sy := (8 * (f mod 8) + x) + 256;
        sx := 224 - (((f and $1FF) div 8));
        putpixel(sx, sy, 1, punbuf, 5);
      end;
    end;
  end;
  actualiza_trozo_final(16, 0, 224, 256, 5);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_starforce;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure starforce_loop;
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
        // Sound CPU
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 239 then
        begin
          z80_0.change_irq(ASSERT_LINE);
          draw_video;
        end;
      end;
      events_starforce;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function starforce_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $987F, $9E00 .. $9E3F, $A000 .. $BBFF:
      starforce_getbyte := memory[direccion];
    $9C00 .. $9DFF:
      starforce_getbyte := buffer_paleta[direccion and $1FF];
    $D000:
      starforce_getbyte := marcade.in0;
    $D001:
      starforce_getbyte := marcade.in1;
    $D002:
      starforce_getbyte := marcade.in2;
    $D003:
      starforce_getbyte := marcade.dswc; // 0 --> inmunidad!!
    $D004:
      starforce_getbyte := marcade.dswa;
    $D005:
      starforce_getbyte := marcade.dswb;
  end;
end;

procedure change_color(numero: word);
var
  i, c, data: byte;
  color: tcolor;
begin
  data := buffer_paleta[numero];
  i := (data shr 6) and $03;
  c := (data shl 2) and $0C;
  if (c <> 0) then
    color.r := pal4bit(c or i)
  else
    color.r := 0;
  c := (data shr 0) and $0C;
  if (c <> 0) then
    color.g := pal4bit(c or i)
  else
    color.g := 0;
  c := (data shr 2) and $0C;
  if (c <> 0) then
    color.b := pal4bit(c or i)
  else
    color.b := 0;
  set_pal_color(color, numero);
  case numero of
    0 .. 63:
      buffer_color[numero shr 3] := true;
    64 .. 127:
      buffer_color[((numero shr 3) and $7) + $18] := true;
    128 .. 191:
      buffer_color[((numero shr 3) and $7) + $10] := true;
    192 .. 255:
      buffer_color[((numero shr 3) and $7) + 8] := true;
  end;
end;

procedure starforce_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $8FFF, $9800 .. $987F, $B800 .. $BBFF:
      memory[direccion] := valor;
    $9000 .. $97FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $9C00 .. $9DFF:
      if buffer_paleta[direccion and $1FF] <> valor then
      begin
        buffer_paleta[direccion and $1FF] := valor;
        change_color(direccion and $1FF);
      end;
    $9E00 .. $9E3F:
      begin
        case (direccion and $3F) of
          0 .. $1F:
            char_scroll[direccion and $1F] := not(valor);
          $20:
            scroll_x[2] := (scroll_x[2] and $300) or valor;
          $21:
            scroll_x[2] := (scroll_x[2] and $FF) or ((valor and 3) shl 8);
          $25:
            scroll_y[2] := valor;
          $28:
            scroll_x[1] := (scroll_x[1] and $300) or valor;
          $29:
            scroll_x[1] := (scroll_x[1] and $FF) or ((valor and 3) shl 8);
          $2D:
            scroll_y[1] := valor;
          $30:
            scroll_x[0] := (scroll_x[0] and $100) or valor;
          $31:
            scroll_x[0] := (scroll_x[0] and $FF) or ((valor and 1) shl 8);
          $35:
            scroll_y[0] := valor;
        end;
        memory[direccion] := valor;
      end;
    $A000 .. $A7FF:
      if memory[direccion] <> valor then
      begin // bg3
        gfx[3].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $A800 .. $AFFF:
      if memory[direccion] <> valor then
      begin // bg2
        gfx[2].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $B000 .. $B7FF:
      if memory[direccion] <> valor then
      begin // bg1
        gfx[1].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $D000:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $D002:
      z80_0.change_irq(CLEAR_LINE);
    $D004:
      begin
        sound_latch := valor;
        z80pio_astb_w(0, false);
        z80pio_astb_w(0, true);
      end;
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $43FF:
      snd_getbyte := mem_snd[direccion];
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ;
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
    $8000:
      sn_76496_0.Write(valor);
    $9000:
      sn_76496_1.Write(valor);
    $A000:
      sn_76496_2.Write(valor);
    // $d000:volumen
  end;
end;

function snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $0 .. $3:
      snd_inbyte := z80pio_ba_cd_r(0, puerto and $3);
    $8 .. $B:
      snd_inbyte := ctc_0.read(puerto and $3);
  end;
end;

procedure snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0 .. $3:
      z80pio_ba_cd_w(0, puerto and $3, valor);
    $8 .. $B:
      ctc_0.Write(puerto and $3, valor);
  end;
end;

procedure starforce_sound_update;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
  sn_76496_2.Update;
end;

// PIO
function pio_read_porta: byte;
begin
  pio_read_porta := sound_latch;
end;

// PIO+CTC INT
procedure pio_int_main(state: byte);
begin
  z80_1.change_irq(state);
end;

// Main
procedure reset_starforce;
begin
  z80_0.reset;
  z80_1.reset;
  z80pio_reset(0);
  ctc_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  sn_76496_2.reset;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  fillword(@scroll_x, 3, 0);
  fillchar(scroll_y, 3, 0);
  sound_latch := 0;
  fillword(@char_scroll, $20, 0);
end;

procedure close_starforce;
begin
  z80pio_close(0);
end;

function start_starforce: boolean;
const
  pbs_x: array [0 .. 31] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2,
    8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2,
    32 * 8 + 3, 32 * 8 + 4, 32 * 8 + 5, 32 * 8 + 6, 32 * 8 + 7, 40 * 8 + 0, 40 * 8 + 1, 40 * 8 + 2,
    40 * 8 + 3, 40 * 8 + 4, 40 * 8 + 5, 40 * 8 + 6, 40 * 8 + 7);
  pbs_y: array [0 .. 31] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8,
    17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8, 64 * 8, 65 * 8, 66 * 8, 67 * 8, 68 * 8,
    69 * 8, 70 * 8, 71 * 8, 80 * 8, 81 * 8, 82 * 8, 83 * 8, 84 * 8, 85 * 8, 86 * 8, 87 * 8);
var
  memory_temp: array [0 .. $FFFF] of byte;
  color: tcolor;
  procedure decode_char(tiles: word);
  begin
    init_gfx(0, 8, 8, tiles);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(3, 0, 8 * 8, 0, tiles * 8 * 8, 2 * tiles * 8 * 8);
    convert_gfx(0, 0, @memory_temp, @pbs_x, @pbs_y, true, false);
  end;
  procedure decode_bg(gfx_num: byte; tiles: word);
  begin
    init_gfx(gfx_num, 16, 16, tiles);
    gfx[gfx_num].trans[0] := true;
    gfx_set_desc_data(3, 0, 32 * 8, 0, tiles * 16 * 16, 2 * tiles * 16 * 16);
    convert_gfx(gfx_num, 0, @memory_temp, @pbs_x, @pbs_y, true, false);
  end;

begin
  machine_calls.general_loop := starforce_loop;
  machine_calls.close := close_starforce;
  machine_calls.reset := reset_starforce;
  start_starforce := false;
  start_audio(false);
  screen_init(1, 512, 256, true); // bg1
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  case main_vars.machine_type of
    25, 320:
      begin
        screen_init(2, 512, 256, true); // bg2
        screen_mod_scroll(2, 512, 256, 511, 256, 256, 255);
        screen_init(3, 512, 256); // bg3
        screen_mod_scroll(3, 512, 256, 511, 256, 256, 255);
        screen_init(4, 256, 256, true); // chars
      end;
    319:
      begin
        screen_init(2, 768, 256, true); // bg2
        screen_mod_scroll(2, 768, 256, 1023, 256, 256, 255);
        screen_init(3, 896, 256, true); // bg3
        screen_mod_scroll(3, 896, 256, 1023, 256, 256, 255);
        screen_init(4, 256, 256, true); // chars
        screen_mod_scroll(4, 256, 256, 255, 256, 256, 255);
      end;
  end;
  screen_init(5, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, $100);
  z80_0.change_ram_calls(starforce_getbyte, starforce_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(2000000, $100);
  z80_1.daisy := true;
  z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_1.change_io_calls(snd_inbyte, snd_outbyte);
  z80_1.init_sound(starforce_sound_update);
  // Daisy Chain PIO+CTC
  ctc_0 := tz80ctc.create(z80_1.numero_cpu, 2000000, z80_1.clock, NOTIMER_2, CTC0_TRG01);
  ctc_0.change_calls(pio_int_main);
  z80pio_init(0, pio_int_main, pio_read_porta);
  z80daisy_init(Z80_PIO_TYPE, Z80_CTC0_TYPE);
  // Chip CPU
  sn_76496_0 := sn76496_chip.create(2000000);
  sn_76496_1 := sn76496_chip.create(2000000);
  sn_76496_2 := sn76496_chip.create(2000000);
  case main_vars.machine_type of
    25:
      begin // Star Force
        draw_video := update_video_starforce;
        // cargar roms
        if not(roms_load(@memory, starforce_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, starforce_sound)) then
          exit;
        // chars
        if not(roms_load(@memory_temp, starforce_char)) then
          exit;
        decode_char($200);
        // bg1
        if not(roms_load(@memory_temp, starforce_bg1)) then
          exit;
        decode_bg(1, $100);
        // bg2
        if not(roms_load(@memory_temp, starforce_bg2)) then
          exit;
        decode_bg(2, $100);
        // bg3
        if not(roms_load(@memory_temp, starforce_bg3)) then
          exit;
        decode_bg(3, $80);
        // sprites
        if not(roms_load(@memory_temp, starforce_sprites)) then
          exit;
        decode_bg(4, $200);
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val := @starforce_dipa;
        marcade.dswb := 0;
        marcade.dswb_val := @starforce_dipb;
        marcade.dswc := 0;
        marcade.dswc_val := @starforce_dipc;
      end;
    319:
      begin // Senjyo
        draw_video := update_video_senjyo;
        // cargar roms
        if not(roms_load(@memory, senjyo_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, senjyo_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, senjyo_char)) then
          exit;
        decode_char($200);
        // bg1
        if not(roms_load(@memory_temp, senjyo_bg1)) then
          exit;
        copymemory(@memory_temp[$4000], @memory_temp[$3000], $1000);
        decode_bg(1, $100);
        // bg2
        if not(roms_load(@memory_temp, senjyo_bg2)) then
          exit;
        copymemory(@memory_temp[$4000], @memory_temp[$3000], $1000);
        decode_bg(2, $100);
        // bg3
        if not(roms_load(@memory_temp, senjyo_bg3)) then
          exit;
        decode_bg(3, $80);
        // sprites
        if not(roms_load(@memory_temp, senjyo_sprites)) then
          exit;
        decode_bg(4, $200);
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val := @starforce_dipa;
        marcade.dswb := $43;
        marcade.dswb_val := @senjyo_dipb;
        marcade.dswc := 0;
        marcade.dswc_val := @senjyo_dipc;
      end;
    320:
      begin // Baluba
        draw_video := update_video_starforce;
        // cargar roms
        if not(roms_load(@memory, baluba_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, baluba_sound)) then
          exit;
        // chars
        if not(roms_load(@memory_temp, baluba_char)) then
          exit;
        decode_char($200);
        // bg1
        if not(roms_load(@memory_temp, baluba_bg1)) then
          exit;
        decode_bg(1, $100);
        // bg2
        if not(roms_load(@memory_temp, baluba_bg2)) then
          exit;
        decode_bg(2, $100);
        // bg3
        if not(roms_load(@memory_temp, baluba_bg3)) then
          exit;
        decode_bg(3, $80);
        // sprites
        if not(roms_load(@memory_temp, baluba_sprites)) then
          exit;
        decode_bg(4, $200);
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val := @starforce_dipa;
        marcade.dswb := 0;
        marcade.dswb_val := @starforce_dipb;
      end;
  end;
  // Paleta radar
  color.r := $FF;
  color.g := 0;
  color.b := 0;
  set_pal_color(color, $200);
  color.r := $FF;
  color.g := $FF;
  color.b := 0;
  set_pal_color(color, $201);
  reset_starforce;
  start_starforce := true;
end;

end.
