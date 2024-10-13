unit gaelco_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_3812,
  m6809,
  oki6295,
  gaelco_hw_decrypt,
  rom_engine,
  pal_engine,
  sound_engine;

function start_gaelco: boolean;

implementation

const
  // Big Karnak
  bigkarnak_rom: array [0 .. 1] of tipo_roms = ((n: 'd16'; l: $40000; p: 0; crc: $44FB9C73),
    (n: 'd19'; l: $40000; p: $1; crc: $FF79DFDD));
  bigkarnak_sound: tipo_roms = (n: 'd5'; l: $10000; p: 0; crc: $3B73B9C5);
  bigkarnak_gfx: array [0 .. 7] of tipo_roms = ((n: 'h5'; l: $80000; p: $0; crc: $20E239FF),
    (n: 'h5'; l: $80000; p: $80000; crc: $20E239FF), (n: 'h10'; l: $80000; p: $100000;
    crc: $AB442855), (n: 'h10'; l: $80000; p: $180000; crc: $AB442855), (n: 'h8'; l: $80000;
    p: $200000; crc: $83DCE5A3), (n: 'h8'; l: $80000; p: $280000; crc: $83DCE5A3), (n: 'h6';
    l: $80000; p: $300000; crc: $24E84B24), (n: 'h6'; l: $80000; p: $380000; crc: $24E84B24));
  bigkarnak_adpcm: tipo_roms = (n: 'd1'; l: $40000; p: 0; crc: $26444AD1);
  // Thunder Hoop
  thoop_rom: array [0 .. 1] of tipo_roms = ((n: 'th18dea1.040'; l: $80000; p: 0; crc: $59BAD625),
    (n: 'th161eb4.020'; l: $40000; p: $1; crc: $6ADD61ED));
  thoop_gfx: array [0 .. 3] of tipo_roms = ((n: 'c09'; l: $100000; p: $0; crc: $06F0EDBF),
    (n: 'c10'; l: $100000; p: $100000; crc: $2D227085), (n: 'c11'; l: $100000; p: $200000;
    crc: $7403EF7E), (n: 'c12'; l: $100000; p: $300000; crc: $29A5CA36));
  thoop_adpcm: tipo_roms = (n: 'sound'; l: $100000; p: 0; crc: $99F80961);
  // Squash
  squash_rom: array [0 .. 1] of tipo_roms = ((n: 'squash.d18'; l: $20000; p: 0; crc: $CE7AAE96),
    (n: 'squash.d16'; l: $20000; p: $1; crc: $8FFAEDD7));
  squash_gfx: array [0 .. 3] of tipo_roms = ((n: 'squash.c09'; l: $80000; p: $0; crc: $0BB91C69),
    (n: 'squash.c10'; l: $80000; p: $80000; crc: $892A035C), (n: 'squash.c11'; l: $80000;
    p: $100000; crc: $9E19694D), (n: 'squash.c12'; l: $80000; p: $180000; crc: $5C440645));
  squash_adpcm: tipo_roms = (n: 'squash.d01'; l: $80000; p: 0; crc: $A1B9651B);
  // Biomechanical Toy
  biomtoy_rom: array [0 .. 1] of tipo_roms = ((n: 'd18'; l: $80000; p: 0; crc: $4569CE64),
    (n: 'd16'; l: $80000; p: $1; crc: $739449BD));
  biomtoy_gfx: array [0 .. 7] of tipo_roms = ((n: 'h6'; l: $80000; p: $0; crc: $9416A729), (n: 'j6';
    l: $80000; p: $80000; crc: $E923728B), (n: 'h7'; l: $80000; p: $100000; crc: $9C984D7B),
    (n: 'j7'; l: $80000; p: $180000; crc: $0E18FAC2), (n: 'h9'; l: $80000; p: $200000;
    crc: $8C1F6718), (n: 'j9'; l: $80000; p: $280000; crc: $1C93F050), (n: 'h10'; l: $80000;
    p: $300000; crc: $ACA1702B), (n: 'j10'; l: $80000; p: $380000; crc: $8E3E96CC));
  biomtoy_adpcm: array [0 .. 1] of tipo_roms = ((n: 'c1'; l: $80000; p: 0; crc: $0F02DE7E),
    (n: 'c3'; l: $80000; p: $80000; crc: $914E4BBC));
  // DIP
  gaelco_dip: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 11;
    dip: ((dip_val: $07; dip_name: '4C 1C'), (dip_val: $08; dip_name: '3C 1C'), (dip_val: $09;
    dip_name: '2C 1C'), (dip_val: $0F; dip_name: '1C 1C'), (dip_val: $06;
    dip_name: '2C 3C'), (dip_val: $0E; dip_name: '1C 2C'), (dip_val: $0D;
    dip_name: '1C 3C'), (dip_val: $0C; dip_name: '1C 4C'), (dip_val: $0B;
    dip_name: '1C 5C'), (dip_val: $0A; dip_name: '1C 6C'), (dip_val: $00;
    dip_name: 'Free Play (If Coin B too)'), (), (), (), (), ())), (mask: $F0; name: 'Coin B';
    number: 11; dip: ((dip_val: $70; dip_name: '4C 1C'), (dip_val: $80; dip_name: '3C 1C'),
    (dip_val: $90; dip_name: '2C 1C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $60;
    dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0;
    dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $00;
    dip_name: 'Free Play (If Coin A too)'), (), (), (), (), ())), ());
  bigkarnak_dsw_2: array [0 .. 5] of def_dip = ((mask: $07; name: 'Difficulty'; number: 8;
    dip: ((dip_val: $07; dip_name: '0'), (dip_val: $06; dip_name: '1'), (dip_val: $05;
    dip_name: '2'), (dip_val: $04; dip_name: '3'), (dip_val: $03; dip_name: '4'), (dip_val: $02;
    dip_name: '5'), (dip_val: $01; dip_name: '6'), (dip_val: $00; dip_name: '7'), (), (), (), (),
    (), (), (), ())), (mask: $18; name: 'Lives'; number: 4;
    dip: ((dip_val: $18; dip_name: '1'), (dip_val: $10; dip_name: '2'), (dip_val: $08;
    dip_name: '3'), (dip_val: $00; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Impact'; number: 2; dip: ((dip_val: $40; dip_name: 'On'), (dip_val: $0;
    dip_name: 'Off'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Service'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  bigkarnak_dsw_3: array [0 .. 1] of def_dip = ((mask: $2; name: 'Service'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), ());
  thoop_dsw_1: array [0 .. 4] of def_dip = ((mask: $07; name: 'Coin A'; number: 8;
    dip: ((dip_val: $02; dip_name: '6C 1C'), (dip_val: $03; dip_name: '5C 1C'), (dip_val: $04;
    dip_name: '4C 1C'), (dip_val: $05; dip_name: '3C 1C'), (dip_val: $06;
    dip_name: '2C 1C'), (dip_val: $01; dip_name: '3C 2C'), (dip_val: $00;
    dip_name: '4C 3C'), (dip_val: $07; dip_name: '1C 1C'), (), (), (), (), (), (), (), ())),
    (mask: $38; name: 'Coin B'; number: 8; dip: ((dip_val: $38; dip_name: '1C 1C'), (dip_val: $00;
    dip_name: '3C 4C'), (dip_val: $08; dip_name: '2C 3C'), (dip_val: $30;
    dip_name: '1C 2C'), (dip_val: $28; dip_name: '1C 3C'), (dip_val: $20;
    dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 5C'), (dip_val: $10; dip_name: '1C 6C'), (),
    (), (), (), (), (), (), ())), (mask: $40; name: '2 Credits to Start, 1 to Continue'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Free Play'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  thoop_dsw_2: array [0 .. 6] of def_dip = ((mask: $03; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $03; dip_name: 'Easy'), (dip_val: $02; dip_name: 'Normal'), (dip_val: $01;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $04; name: 'Player Controls'; number: 2;
    dip: ((dip_val: $4; dip_name: '2 Joysticks'), (dip_val: $0; dip_name: '1 Joysticks'), (), (),
    (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '4'), (dip_val: $8; dip_name: '3'), (dip_val: $10;
    dip_name: '2'), (dip_val: $18; dip_name: '1'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $40; dip_name: 'Upright'), (dip_val: $0;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Service'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  squash_dsw_1: array [0 .. 4] of def_dip = ((mask: $07; name: 'Coin A'; number: 8;
    dip: ((dip_val: $02; dip_name: '6C 1C'), (dip_val: $03; dip_name: '5C 1C'), (dip_val: $04;
    dip_name: '4C 1C'), (dip_val: $05; dip_name: '3C 1C'), (dip_val: $06;
    dip_name: '2C 1C'), (dip_val: $01; dip_name: '3C 2C'), (dip_val: $00;
    dip_name: '4C 3C'), (dip_val: $07; dip_name: '1C 1C'), (), (), (), (), (), (), (), ())),
    (mask: $38; name: 'Coin B'; number: 8; dip: ((dip_val: $38; dip_name: '1C 1C'), (dip_val: $00;
    dip_name: '3C 4C'), (dip_val: $08; dip_name: '2C 3C'), (dip_val: $30;
    dip_name: '1C 2C'), (dip_val: $28; dip_name: '1C 3C'), (dip_val: $20;
    dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 5C'), (dip_val: $10; dip_name: '1C 6C'), (),
    (), (), (), (), (), (), ())), (mask: $40; name: '2 Player Continue'; number: 2;
    dip: ((dip_val: $40; dip_name: '2 Credits / 5 Games'), (dip_val: $0;
    dip_name: '1 Credit / 3 Games'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Free Play'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  squash_dsw_2: array [0 .. 4] of def_dip = ((mask: $03; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $02; dip_name: 'Easy'), (dip_val: $03; dip_name: 'Normal'), (dip_val: $01;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $0C; name: 'Number of Faults'; number: 4;
    dip: ((dip_val: $8; dip_name: '4'), (dip_val: $C; dip_name: '5'), (dip_val: $4;
    dip_name: '6'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Service'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  biomtoy_dsw_2: array [0 .. 4] of def_dip = ((mask: $1; name: 'Service'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $8; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $8; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $30; name: 'Lives'; number: 4;
    dip: ((dip_val: $20; dip_name: '1'), (dip_val: $10; dip_name: '2'), (dip_val: $30;
    dip_name: '3'), (dip_val: $0; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C0; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $C0; dip_name: 'Normal'), (dip_val: $80;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());

var
  scroll_x0, scroll_y0, scroll_x1, scroll_y1: word;
  rom: array [0 .. $7FFFF] of word;
  video_ram: array [0 .. $1FFF] of word;
  sprite_ram: array [0 .. $7FF] of word;
  main_ram: array [0 .. $7FFF] of word;
  sound_latch, gaelco_dec_val: byte;
  oki_rom: array [0 .. $C, 0 .. $FFFF] of byte;

procedure draw_sprites_bk(pri: byte);
var
  x, i, color, attr, attr2, nchar: word;
  flipx, flipy: boolean;
  y, a, priority: byte;
begin
  for i := $1 to $1FF do
  begin
    attr := sprite_ram[(i * 4) - 1];
    if (attr and $FF) = $F0 then
      continue; // el sprite no se va a ver...
    attr2 := sprite_ram[(i * 4) + 1];
    color := (attr2 and $7E00) shr 9;
    if (color >= $38) then
      priority := 4
    else
      priority := (attr and $3000) shr 12;
    if pri <> priority then
      continue;
    y := 240 - (attr and $FF);
    color := color shl 4;
    flipx := (attr and $4000) <> 0;
    flipy := (attr and $8000) <> 0;
    x := (attr2 and $1FF) - 15;
    nchar := sprite_ram[(i * 4) + 2];
    if (attr and $800) <> 0 then
    begin
      put_gfx_sprite(nchar, color, flipx, flipy, 0);
      update_gfx_sprite(x, y, 17, 0);
    end
    else
    begin
      nchar := nchar and $FFFC;
      a := (byte(flipx) shl 1) or byte(flipy);
      put_gfx_sprite_diff((nchar + 0) xor a, color, flipx, flipy, 0, 0, 0);
      put_gfx_sprite_diff((nchar + 2) xor a, color, flipx, flipy, 0, 8, 0);
      put_gfx_sprite_diff((nchar + 1) xor a, color, flipx, flipy, 0, 0, 8);
      put_gfx_sprite_diff((nchar + 3) xor a, color, flipx, flipy, 0, 8, 8);
      actualiza_gfx_sprite_size(x, y, 17, 16, 16);
    end;
  end;
end;

procedure draw_all_bigk;
var
  f, color, sx, sy, pos, x, y, nchar, atrib1, atrib2: word;
  pant, h: byte;
begin
  for f := 0 to $164 do
  begin
    y := f div 21;
    x := f mod 21;
    // Draw back
    // Calcular posicion
    sx := x + ((scroll_x0 and $1F0) shr 4);
    sy := y + ((scroll_y0 and $1F0) shr 4);
    pos := (sx and $1F) + ((sy and $1F) * 32);
    // Calcular color
    atrib2 := video_ram[$1 + (pos * 2)];
    color := atrib2 and $3F;
    if (gfx[1].buffer[pos] or buffer_color[color]) then
    begin
      pant := ((atrib2 shr 6) and $3) + 1;
      atrib1 := video_ram[$0 + (pos * 2)];
      nchar := $4000 + ((atrib1 and $FFFC) shr 2);
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 4, pant, 1, (atrib1 and 1) <> 0,
        (atrib1 and 2) <> 0);
      if pant <> 4 then
        put_gfx_trans_flip_alt(x * 16, y * 16, nchar, color shl 4, pant + 4, 1, (atrib1 and 1) <> 0,
          (atrib1 and 2) <> 0, pant);
      for h := 1 to 4 do
        if (h <> pant) then
        begin
          put_gfx_block_trans(x * 16, y * 16, h, 16, 16);
          if h <> 4 then
            put_gfx_block_trans(x * 16, y * 16, h + 4, 16, 16)
        end;
      gfx[1].buffer[pos] := false;
    end;
    // Draw Front
    // Calcular posicion
    sx := x + ((scroll_x1 and $1F0) shr 4);
    sy := y + ((scroll_y1 and $1F0) shr 4);
    pos := (sx and $1F) + ((sy and $1F) * 32);
    // Calcular color
    atrib2 := video_ram[$801 + (pos * 2)];
    color := atrib2 and $3F;
    if (gfx[1].buffer[pos + $400] or buffer_color[color]) then
    begin
      pant := ((atrib2 shr 6) and $3) + 9;
      atrib1 := video_ram[$800 + (pos * 2)];
      nchar := $4000 + ((atrib1 and $FFFC) shr 2);
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 4, pant, 1, (atrib1 and 1) <> 0,
        (atrib1 and 2) <> 0);
      if pant <> 12 then
        put_gfx_trans_flip_alt(x * 16, y * 16, nchar, color shl 4, pant + 4, 1, (atrib1 and 1) <> 0,
          (atrib1 and 2) <> 0, pant - 8);
      for h := 9 to 12 do
        if (h <> pant) then
        begin
          put_gfx_block_trans(x * 16, y * 16, h, 16, 16);
          if h <> 12 then
            put_gfx_block_trans(x * 16, y * 16, h + 4, 16, 16);
        end;
      gfx[1].buffer[pos + $400] := false;
    end;
  end;
end;

procedure update_video_bigk;
begin
  fill_full_screen(17, 0);
  draw_all_bigk;
  scroll_x_y(4, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI0
  scroll_x_y(12, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI0
  draw_sprites_bk(3);
  // scroll_x_y(8,17,scroll_x0 and $f,scroll_y0 and $f); //Totalmente transparente!
  // scroll_x_y(16,17,scroll_x1 and $f,scroll_y1 and $f); //Totalmente transparente!
  scroll_x_y(3, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI1
  scroll_x_y(11, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI1
  draw_sprites_bk(2);
  scroll_x_y(7, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI1 encima sprites
  scroll_x_y(15, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI1 encima sprites
  scroll_x_y(2, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI2
  scroll_x_y(10, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI2
  draw_sprites_bk(1);
  scroll_x_y(6, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI2 encima sprites
  scroll_x_y(14, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI2 encima sprites
  scroll_x_y(1, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI3
  scroll_x_y(9, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI3
  draw_sprites_bk(0);
  scroll_x_y(5, 17, scroll_x0 and $F, scroll_y0 and $F); // PRI3 encima sprites
  scroll_x_y(13, 17, scroll_x1 and $F, scroll_y1 and $F); // PRI3 encima sprites
  draw_sprites_bk(4);
  actualiza_trozo_final(0, 16, 320, 240, 17);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_gaelco_hw;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure change_color(tmp_color, numero: word);
var
  color: tcolor;
begin
  color.b := pal5bit(tmp_color shr 10);
  color.g := pal5bit(tmp_color shr 5);
  color.r := pal5bit(tmp_color);
  set_pal_color(color, numero);
  buffer_color[(numero shr 4) and $3F] := true;
end;

// Big Karnak
procedure bigk_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6809_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 511 do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sound
        m6809_0.run(frame_s);
        frame_s := frame_s + m6809_0.tframes - m6809_0.contador;
        if f = 255 then
        begin
          m68000_0.irq[6] := HOLD_LINE;
          update_video_bigk;
        end;
      end;
      events_gaelco_hw;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function bigk_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      bigk_getword := rom[direccion shr 1];
    $100000 .. $103FFF:
      bigk_getword := video_ram[(direccion and $3FFF) shr 1];
    $440000 .. $440FFF:
      bigk_getword := sprite_ram[(direccion and $FFF) shr 1];
    $200000 .. $2007FF:
      bigk_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $700000:
      bigk_getword := marcade.dswa;
    $700002:
      bigk_getword := marcade.dswb;
    $700004:
      bigk_getword := marcade.in0;
    $700006:
      bigk_getword := marcade.in1;
    $700008:
      bigk_getword := marcade.dswc;
    $FF8000 .. $FFFFFF:
      bigk_getword := main_ram[(direccion and $7FFF) shr 1];
  end;
end;

procedure bigk_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $7FFFF:
      ; // ROM
    $100000 .. $100FFF:
      if video_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        video_ram[(direccion and $FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $FFF) div 4] := true;
      end;
    $101000 .. $101FFF:
      if video_ram[(direccion and $1FFF) shr 1] <> valor then
      begin
        video_ram[(direccion and $1FFF) shr 1] := valor;
        gfx[1].buffer[((direccion and $FFF) shr 2) + $400] := true;
      end;
    $102000 .. $103FFF:
      video_ram[(direccion and $3FFF) shr 1] := valor;
    $108000:
      if scroll_y0 <> (valor and $1FF) then
      begin
        if abs((scroll_y0 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[0], $400, 1);
        scroll_y0 := valor and $1FF;
      end;
    $108002:
      if scroll_x0 <> ((valor + 4) and $1FF) then
      begin
        if abs((scroll_x0 and $1F0) - ((valor + 4) and $1F0)) > 15 then
          fillchar(gfx[1].buffer[0], $400, 1);
        scroll_x0 := (valor + 4) and $1FF;
      end;
    $108004:
      if scroll_y1 <> (valor and $1FF) then
      begin
        if abs((scroll_y1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[$400], $400, 1);
        scroll_y1 := valor and $1FF;
      end;
    $108006:
      if scroll_x1 <> (valor and $1FF) then
      begin
        if abs((scroll_x1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[$400], $400, 1);
        scroll_x1 := valor and $1FF;
      end;
    $10800C:
      ;
    $200000 .. $2007FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        change_color(valor, (direccion and $7FF) shr 1);
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
      end;
    $440000 .. $440FFF:
      sprite_ram[(direccion and $FFF) shr 1] := valor;
    // 70000a..70003b:;
    $70000E:
      begin
        sound_latch := valor and $FF;
        m6809_0.change_firq(HOLD_LINE);
      end;
    $FF8000 .. $FFFFFF:
      main_ram[(direccion and $7FFF) shr 1] := valor;
  end;
end;

function bigk_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF, $C00 .. $FFFF:
      bigk_snd_getbyte := mem_snd[direccion];
    $800, $801:
      bigk_snd_getbyte := oki_6295_0.read;
    $A00:
      bigk_snd_getbyte := ym3812_0.status;
    $B00:
      bigk_snd_getbyte := sound_latch;
  end;
end;

procedure bigk_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $800, $801:
      oki_6295_0.write(valor);
    $A00:
      ym3812_0.control(valor);
    $A01:
      ym3812_0.write(valor);
    $C00 .. $FFFF:
      ; // ROM
  end;
end;

procedure bigk_sound_update;
begin
  ym3812_0.update;
  oki_6295_0.update;
end;

// Thunder Hoop
procedure draw_sprites_thoop(pri: byte);
var
  x, i, color, attr, attr2, nchar: word;
  flipx, flipy: boolean;
  y, a, priority: byte;
begin
  for i := $1 to $1FF do
  begin
    attr := sprite_ram[(i * 4) - 1];
    if (attr and $FF) = $F0 then
      continue; // El sprite no se va a ver
    priority := (attr shr 12) and $3;
    if pri <> priority then
      continue;
    attr2 := sprite_ram[(i * 4) + 1];
    color := (attr2 and $7E00) shr 9;
    y := 240 - (attr and $FF);
    color := color shl 4;
    flipx := (attr and $4000) <> 0;
    flipy := (attr and $8000) <> 0;
    x := (attr2 and $1FF) - 15;
    nchar := sprite_ram[(i * 4) + 2];
    if (attr and $800) <> 0 then
    begin
      put_gfx_sprite(nchar, color, flipx, flipy, 0);
      update_gfx_sprite(x, y, 17, 0);
    end
    else
    begin
      nchar := nchar and $FFFC;
      a := (byte(flipx) shl 1) or byte(flipy);
      put_gfx_sprite_diff((nchar + 0) xor a, color, flipx, flipy, 0, 0, 0);
      put_gfx_sprite_diff((nchar + 2) xor a, color, flipx, flipy, 0, 8, 0);
      put_gfx_sprite_diff((nchar + 1) xor a, color, flipx, flipy, 0, 0, 8);
      put_gfx_sprite_diff((nchar + 3) xor a, color, flipx, flipy, 0, 8, 8);
      actualiza_gfx_sprite_size(x, y, 17, 16, 16);
    end;
  end;
end;

procedure draw_all_thoop; inline;
var
  f, color, sx, sy, x, y, nchar, atrib1, atrib2, pos: word;
  pant, h: byte;
begin
  for f := 0 to $164 do
  begin
    y := f div 21;
    x := f mod 21;
    // Draw back
    // Calcular posicion
    sx := x + ((scroll_x0 and $1F0) shr 4);
    sy := y + ((scroll_y0 and $1F0) shr 4);
    pos := (sx and $1F) + ((sy and $1F) * 32);
    // Calcular color
    atrib2 := video_ram[$1 + (pos * 2)];
    color := atrib2 and $3F;
    if (gfx[1].buffer[pos] or buffer_color[color]) then
    begin
      pant := ((atrib2 shr 6) and $3) + 1;
      atrib1 := video_ram[$0 + (pos * 2)];
      nchar := $4000 + ((atrib1 and $FFFC) shr 2);
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 4, pant, 1, (atrib1 and 1) <> 0,
        (atrib1 and 2) <> 0);
      for h := 1 to 4 do
        if (h <> pant) then
          put_gfx_block_trans(x * 16, y * 16, h, 16, 16);
      gfx[1].buffer[pos] := false;
    end;
    // Draw Front
    // Calcular posicion
    sx := x + ((scroll_x1 and $1F0) shr 4);
    sy := y + ((scroll_y1 and $1F0) shr 4);
    pos := (sx and $1F) + ((sy and $1F) * 32);
    // Calcular color
    atrib2 := video_ram[$801 + (pos * 2)];
    color := atrib2 and $3F;
    if (gfx[1].buffer[pos + $400] or buffer_color[color]) then
    begin
      pant := ((atrib2 shr 6) and $3) + 5;
      atrib1 := video_ram[$800 + (pos * 2)];
      nchar := $4000 + ((atrib1 and $FFFC) shr 2);
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 4, pant, 1, (atrib1 and 1) <> 0,
        (atrib1 and 2) <> 0);
      for h := 5 to 8 do
        if (h <> pant) then
          put_gfx_block_trans(x * 16, y * 16, h, 16, 16);
      gfx[1].buffer[pos + $400] := false;
    end;
  end;
end;

procedure update_video_thoop;
begin
  fill_full_screen(17, 0);
  draw_all_thoop;
  scroll_x_y(4, 17, scroll_x0 and $F, scroll_y0 and $F);
  scroll_x_y(8, 17, scroll_x1 and $F, scroll_y1 and $F);
  draw_sprites_thoop(3);
  scroll_x_y(3, 17, scroll_x0 and $F, scroll_y0 and $F);
  scroll_x_y(7, 17, scroll_x1 and $F, scroll_y1 and $F);
  draw_sprites_thoop(2);
  draw_sprites_thoop(1);
  scroll_x_y(2, 17, scroll_x0 and $F, scroll_y0 and $F);
  scroll_x_y(6, 17, scroll_x1 and $F, scroll_y1 and $F);
  draw_sprites_thoop(0);
  scroll_x_y(1, 17, scroll_x0 and $F, scroll_y0 and $F);
  scroll_x_y(5, 17, scroll_x1 and $F, scroll_y1 and $F);
  actualiza_trozo_final(0, 16, 320, 240, 17);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure thoop_loop;
var
  frame_m: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 511 do
      begin
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        if f = 255 then
        begin
          m68000_0.irq[6] := HOLD_LINE;
          update_video_thoop;
        end;
      end;
      events_gaelco_hw;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function thoop_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      thoop_getword := rom[direccion shr 1];
    $100000 .. $103FFF:
      thoop_getword := video_ram[(direccion and $3FFF) shr 1];
    $440000 .. $440FFF:
      thoop_getword := sprite_ram[(direccion and $FFF) shr 1];
    $200000 .. $2007FF:
      thoop_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $700000:
      thoop_getword := marcade.dswb;
    $700002:
      thoop_getword := marcade.dswa;
    $700004:
      thoop_getword := marcade.in0;
    $700006:
      thoop_getword := marcade.in1;
    $70000E:
      thoop_getword := oki_6295_0.read;
    $FF0000 .. $FFFFFF:
      thoop_getword := main_ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure thoop_putword(direccion: dword; valor: word);
var
  dec: word;
  ptemp: pbyte;
begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $100FFF:
      begin
        dec := gaelco_dec((direccion and $FFF) shr 1, valor, gaelco_dec_val, $4228,
          m68000_0.r.pc.l);
        if video_ram[(direccion and $FFF) shr 1] <> dec then
        begin
          video_ram[(direccion and $FFF) shr 1] := dec;
          gfx[1].buffer[(direccion and $FFF) shr 2] := true;
        end;
      end;
    $101000 .. $101FFF:
      begin
        dec := gaelco_dec((direccion and $1FFF) shr 1, valor, gaelco_dec_val, $4228,
          m68000_0.r.pc.l);
        if video_ram[(direccion and $1FFF) shr 1] <> dec then
        begin
          video_ram[(direccion and $1FFF) shr 1] := dec;
          gfx[1].buffer[((direccion and $FFF) shr 2) + $400] := true;
        end;
      end;
    $102000 .. $103FFF:
      begin
        dec := gaelco_dec((direccion and $1FFF) shr 1, valor, gaelco_dec_val, $4228,
          m68000_0.r.pc.l);
        video_ram[(direccion and $3FFF) shr 1] := dec;
      end;
    $108000:
      if scroll_y0 <> (valor and $1FF) then
      begin
        if abs((scroll_y0 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[0], $400, 1);
        scroll_y0 := valor and $1FF;
      end;
    $108002:
      if scroll_x0 <> ((valor + 4) and $1FF) then
      begin
        if abs((scroll_x0 and $1F0) - ((valor + 4) and $1F0)) > 15 then
          fillchar(gfx[1].buffer[0], $400, 1);
        scroll_x0 := (valor + 4) and $1FF;
      end;
    $108004:
      if scroll_y1 <> (valor and $1FF) then
      begin
        if abs((scroll_y1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[$400], $400, 1);
        scroll_y1 := valor and $1FF;
      end;
    $108006:
      if scroll_x1 <> (valor and $1FF) then
      begin
        if abs((scroll_x1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[$400], $400, 1);
        scroll_x1 := valor and $1FF;
      end;
    $10800C:
      ;
    $200000 .. $2007FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        change_color(valor, (direccion and $7FF) shr 1);
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
      end;
    $440000 .. $440FFF:
      sprite_ram[(direccion and $FFF) shr 1] := valor;
    $70000C:
      begin
        ptemp := oki_6295_0.get_rom_addr;
        inc(ptemp, $30000);
        copymemory(ptemp, @oki_rom[(valor and $F), 0], $10000);
      end;
    $70000E:
      oki_6295_0.write(valor);
    $FF0000 .. $FFFFFF:
      main_ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

// Biomechanical Toy
procedure biomtoy_putword(direccion: dword; valor: word);
var
  ptemp: pbyte;
begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $100FFF:
      if video_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        video_ram[(direccion and $FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $FFF) shr 2] := true;
      end;
    $101000 .. $101FFF:
      if video_ram[(direccion and $1FFF) shr 1] <> valor then
      begin
        video_ram[(direccion and $1FFF) shr 1] := valor;
        gfx[1].buffer[((direccion and $FFF) shr 2) + $400] := true;
      end;
    $102000 .. $103FFF:
      video_ram[(direccion and $3FFF) shr 1] := valor;
    $108000:
      if scroll_y0 <> (valor and $1FF) then
      begin
        if abs((scroll_y0 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[0], $400, 1);
        scroll_y0 := valor and $1FF;
      end;
    $108002:
      if scroll_x0 <> ((valor + 4) and $1FF) then
      begin
        if abs((scroll_x0 and $1F0) - ((valor + 4) and $1F0)) > 15 then
          fillchar(gfx[1].buffer[0], $400, 1);
        scroll_x0 := (valor + 4) and $1FF;
      end;
    $108004:
      if scroll_y1 <> (valor and $1FF) then
      begin
        if abs((scroll_y1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[$400], $400, 1);
        scroll_y1 := valor and $1FF;
      end;
    $108006:
      if scroll_x1 <> (valor and $1FF) then
      begin
        if abs((scroll_x1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[1].buffer[$400], $400, 1);
        scroll_x1 := valor and $1FF;
      end;
    $10800C:
      ;
    $200000 .. $2007FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        change_color(valor, (direccion and $7FF) shr 1);
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
      end;
    $440000 .. $440FFF:
      sprite_ram[(direccion and $FFF) shr 1] := valor;
    $70000C:
      begin
        ptemp := oki_6295_0.get_rom_addr;
        inc(ptemp, $30000);
        copymemory(ptemp, @oki_rom[(valor and $F), 0], $10000);
      end;
    $70000E:
      oki_6295_0.write(valor); // OKI
    $FF0000 .. $FFFFFF:
      main_ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

procedure thoop_sound_update;
begin
  oki_6295_0.update;
end;

// Main
procedure reset_gaelco_hw;
begin
  m68000_0.reset;
  if main_vars.machine_type = 78 then
  begin
    m6809_0.reset;
    ym3812_0.reset;
  end;
  oki_6295_0.reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  scroll_x0 := 1;
  scroll_y0 := 1;
  scroll_x1 := 1;
  scroll_y1 := 1;
  sound_latch := 0;
end;

function start_gaelco: boolean;
var
  ptemp, ptemp2, ptemp3, memory_temp: pbyte;
  f, pants: byte;
const
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2,
    16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8,
    9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  procedure convert_sprites;
  begin
    init_gfx(0, 8, 8, $20000);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 8 * 8, 0 * $100000 * 8, 1 * $100000 * 8, 2 * $100000 * 8,
      3 * $100000 * 8);
    convert_gfx(0, 0, memory_temp, @pt_x, @pt_y, false, false);
  end;
  procedure convert_tiles;
  var
    f: byte;
  begin
    init_gfx(1, 16, 16, $8000);
    gfx[1].trans[0] := true;
    for f := 1 to 3 do
      gfx[1].trans_alt[f, 0] := true;
    for f := 8 to 15 do
      gfx[1].trans_alt[1, f] := true;
    for f := 4 to 15 do
      gfx[1].trans_alt[2, f] := true;
    for f := 2 to 15 do
      gfx[1].trans_alt[3, f] := true;
    gfx_set_desc_data(4, 0, 32 * 8, 0 * $100000 * 8, 1 * $100000 * 8, 2 * $100000 * 8,
      3 * $100000 * 8);
    convert_gfx(1, 0, memory_temp, @pt_x, @pt_y, false, false);
  end;

begin
  start_gaelco := false;
  case main_vars.machine_type of
    78:
      machine_calls.general_loop := bigk_loop;
    101, 173, 174:
      begin
        machine_calls.general_loop := thoop_loop;
        machine_calls.fps_max := 57.42;
      end;
  end;
  machine_calls.reset := reset_gaelco_hw;
  start_audio(false);
  if main_vars.machine_type = 78 then
    pants := 15
  else
    pants := 8;
  for f := 1 to pants do
  begin
    screen_init(f, 336, 272, true);
    screen_mod_scroll(f, 336, 336, 511, 272, 272, 511);
  end;
  // Final
  screen_init(17, 512, 512, false, true);
  start_video(320, 240);
  marcade.dswa := $00FF;
  // Main CPU
  m68000_0 := cpu_m68000.create(12000000, $200);
  getmem(memory_temp, $400000);
  case main_vars.machine_type of
    78:
      begin // Big Karnak
        // Main CPU
        if not(roms_load16w(@rom, bigkarnak_rom)) then
          exit;
        m68000_0.change_ram16_calls(bigk_getword, bigk_putword);
        // Sound CPU
        if not(roms_load(@mem_snd, bigkarnak_sound)) then
          exit;
        m6809_0 := cpu_m6809.create(2000000, $200, TCPU_M6809);
        m6809_0.change_ram_calls(bigk_snd_getbyte, bigk_snd_putbyte);
        m6809_0.init_sound(bigk_sound_update);
        // Sound Chips
        ym3812_0 := ym3812_chip.create(YM3812_FM, 4000000);
        oki_6295_0 := snd_okim6295.create(1056000, OKIM6295_PIN7_HIGH, 1);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, bigkarnak_adpcm)) then
          exit;
        // Sprites
        if not(roms_load(memory_temp, bigkarnak_gfx)) then
          exit;
        convert_sprites;
        // Tiles
        convert_tiles;
        marcade.dswb := $00CE;
        marcade.dswc := $00FF;
        marcade.dswa_val := @gaelco_dip;
        marcade.dswb_val := @bigkarnak_dsw_2;
        marcade.dswc_val := @bigkarnak_dsw_3;
      end;
    101:
      begin // Thunder Hoop
        // Main CPU
        if not(roms_load16w(@rom, thoop_rom)) then
          exit;
        m68000_0.change_ram16_calls(thoop_getword, thoop_putword);
        m68000_0.init_sound(thoop_sound_update);
        // Sound Chips
        oki_6295_0 := snd_okim6295.create(1056000, OKIM6295_PIN7_HIGH, 2);
        // Cargar ADPCM ROMS
        if not(roms_load(memory_temp, thoop_adpcm)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, memory_temp, $40000);
        ptemp2 := memory_temp;
        for f := 0 to $F do
        begin
          copymemory(@oki_rom[f, 0], ptemp2, $10000);
          inc(ptemp2, $10000);
        end;
        // Sprites
        getmem(ptemp, $400000);
        if not(roms_load(ptemp, thoop_gfx)) then
          exit;
        // Ordenar los GFX
        ptemp3 := ptemp;
        for f := 3 downto 0 do
        begin
          ptemp2 := memory_temp;
          inc(ptemp2, $100000 * f);
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
          ptemp2 := memory_temp;
          inc(ptemp2, ($100000 * f) + $80000);
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
          ptemp2 := memory_temp;
          inc(ptemp2, ($100000 * f) + $40000);
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
          ptemp2 := memory_temp;
          inc(ptemp2, ($100000 * f) + $C0000);
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
        end;
        freemem(ptemp);
        convert_sprites;
        // Tiles
        convert_tiles;
        gaelco_dec_val := $E;
        marcade.dswb := $00CF;
        marcade.dswa_val := @thoop_dsw_1;
        marcade.dswb_val := @thoop_dsw_2;
      end;
    173:
      begin // Squash
        // Main CPU
        if not(roms_load16w(@rom, squash_rom)) then
          exit;
        m68000_0.change_ram16_calls(thoop_getword, thoop_putword);
        m68000_0.init_sound(thoop_sound_update);
        // Sound Chips
        oki_6295_0 := snd_okim6295.create(1056000, OKIM6295_PIN7_HIGH, 2);
        // Cargar ADPCM ROMS
        if not(roms_load(memory_temp, squash_adpcm)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, memory_temp, $40000);
        ptemp2 := memory_temp;
        for f := 0 to $7 do
        begin
          copymemory(@oki_rom[f, 0], ptemp2, $10000);
          inc(ptemp2, $10000);
        end;
        // Sprites
        getmem(ptemp, $400000);
        if not(roms_load(ptemp, squash_gfx)) then
          exit;
        // Ordenar los GFX
        ptemp3 := ptemp;
        for f := 3 downto 0 do
        begin
          ptemp2 := memory_temp;
          inc(ptemp2, $100000 * f);
          copymemory(ptemp2, ptemp3, $80000);
          ptemp2 := memory_temp;
          inc(ptemp2, ($100000 * f) + $80000);
          copymemory(ptemp2, ptemp3, $80000);
          inc(ptemp3, $80000);
        end;
        freemem(ptemp);
        convert_sprites;
        // Tiles
        convert_tiles;
        gaelco_dec_val := $F;
        marcade.dswb := $00DF;
        marcade.dswa_val := @squash_dsw_1;
        marcade.dswb_val := @squash_dsw_2;
      end;
    174:
      begin // Biomechanical Toy
        // Main CPU
        if not(roms_load16w(@rom, biomtoy_rom)) then
          exit;
        m68000_0.change_ram16_calls(thoop_getword, biomtoy_putword);
        m68000_0.init_sound(thoop_sound_update);
        // Sound Chips
        oki_6295_0 := snd_okim6295.create(1056000, OKIM6295_PIN7_HIGH, 2);
        // Cargar ADPCM ROMS
        if not(roms_load(memory_temp, biomtoy_adpcm)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, memory_temp, $40000);
        ptemp2 := memory_temp;
        for f := 0 to $F do
        begin
          copymemory(@oki_rom[f, 0], ptemp2, $10000);
          inc(ptemp2, $10000);
        end;
        // Sprites
        getmem(ptemp, $400000);
        if not(roms_load(ptemp, biomtoy_gfx)) then
          exit;
        // Ordenar los GFX
        ptemp3 := ptemp; // orig
        for f := 0 to 3 do
        begin
          ptemp2 := memory_temp;
          inc(ptemp2, $040000 + (f * $100000));
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
          ptemp2 := memory_temp;
          inc(ptemp2, $0C0000 + (f * $100000));
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
          ptemp2 := memory_temp;
          inc(ptemp2, $000000 + (f * $100000));
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
          ptemp2 := memory_temp;
          inc(ptemp2, $080000 + (f * $100000));
          copymemory(ptemp2, ptemp3, $40000);
          inc(ptemp3, $40000);
        end;
        freemem(ptemp);
        convert_sprites;
        // Tiles
        convert_tiles;
        marcade.dswb := $00FB;
        marcade.dswa_val := @gaelco_dip;
        marcade.dswb_val := @biomtoy_dsw_2;
      end;
  end;
  freemem(memory_temp);
  // final
  reset_gaelco_hw;
  start_gaelco := true;
end;

end.
