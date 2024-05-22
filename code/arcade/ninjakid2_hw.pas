unit ninjakid2_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ym_2203,
  gfx_engine,
  rom_engine,
  pal_engine,
  mc8123,
  sound_engine,
  timer_engine,
  dac;

function start_ninjakid2: boolean;

implementation

const
  // Ninja Kid II
  ninjakid2_rom: array [0 .. 4] of tipo_roms = ((n: '1.3s'; l: $8000; p: 0; crc: $3CDBB906),
    (n: '2.3q'; l: $8000; p: $8000; crc: $B5CE9A1A), (n: '3.3r'; l: $8000; p: $10000;
    crc: $AD275654), (n: '4.3p'; l: $8000; p: $18000; crc: $E7692A77), (n: '5.3m'; l: $8000;
    p: $20000; crc: $5DAC9426));
  ninjakid2_snd_rom: tipo_roms = (n: '6.3h'; l: $10000; p: 0; crc: $D3A18A79);
  ninjakid2_fgtiles: tipo_roms = (n: '12.5n'; l: $8000; p: 0; crc: $DB5657A9);
  ninjakid2_sprites: array [0 .. 1] of tipo_roms = ((n: '8.6l'; l: $10000; p: 0; crc: $1B79C50A),
    (n: '7.6n'; l: $10000; p: $10000; crc: $0BE5CD13));
  ninjakid2_bgtiles: array [0 .. 1] of tipo_roms = ((n: '11.2n'; l: $10000; p: 0; crc: $41A714B3),
    (n: '10.2r'; l: $10000; p: $10000; crc: $C913C4AB));
  ninjakid2_snd_key: tipo_roms = (n: 'ninjakd2.key'; l: $2000; p: 0; crc: $EC25318F);
  ninjakid2_pcm_rom: tipo_roms = (n: '9.6c'; l: $10000; p: 0; crc: $C1D2D170);
  // DIP
  ninjakid2_dip_a: array [0 .. 7] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $6; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $4; dip_name: '20K 50K+'), (dip_val: $6; dip_name: '30K 50K+'), (dip_val: $2;
    dip_name: '50K 50K+'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $8; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $8; dip_name: 'Yes'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $20; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Lives'; number: 2;
    dip: ((dip_val: $40; dip_name: '3'), (dip_val: $0; dip_name: '4'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $80; name: 'Language'; number: 2;
    dip: ((dip_val: $0; dip_name: 'English'), (dip_val: $80; dip_name: 'Japanese'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), ());
  ninjakid2_dip_b: array [0 .. 4] of def_dip = ((mask: $2; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Credit Service'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $4; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $18; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '2C 1C/6C 4C'), (dip_val: $18; dip_name: '1C 1C/3C 4C'),
    (dip_val: $10; dip_name: '1C 2C/2C 6C'), (dip_val: $8; dip_name: '1C 3C/3C 12C'), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $E0; name: 'Coin B'; number: 8;
    dip: ((dip_val: $80; dip_name: '1C 4C'), (dip_val: $0; dip_name: '5C 1C/15C 4C'), (dip_val: $20;
    dip_name: '4C 1C/12C 4C'), (dip_val: $40; dip_name: '3C 1C/9C 4C'), (dip_val: $60;
    dip_name: '2C 1C/6C 4C'), (dip_val: $E0; dip_name: '1C 1C/3C 4C'), (dip_val: $C0;
    dip_name: '1C 2C/2C 6C'), (dip_val: $A0; dip_name: '1C 3C/3C 12C'), (), (), (), (), (), (), (),
    ())), ());
  // Ark Area
  aarea_rom: array [0 .. 4] of tipo_roms = ((n: 'arkarea.008'; l: $8000; p: 0; crc: $1CE1B5B9),
    (n: 'arkarea.009'; l: $8000; p: $8000; crc: $DB1C81D1), (n: 'arkarea.010'; l: $8000; p: $10000;
    crc: $5A460DAE), (n: 'arkarea.011'; l: $8000; p: $18000; crc: $63F022C9), (n: 'arkarea.012';
    l: $8000; p: $20000; crc: $3C4C65D5));
  aarea_snd_rom: tipo_roms = (n: 'arkarea.013'; l: $8000; p: 0; crc: $2D409D58);
  aarea_fgtiles: tipo_roms = (n: 'arkarea.004'; l: $8000; p: 0; crc: $69E36AF2);
  aarea_sprites: array [0 .. 2] of tipo_roms = ((n: 'arkarea.007'; l: $10000; p: 0; crc: $D5684A27),
    (n: 'arkarea.006'; l: $10000; p: $10000; crc: $2C0567D6), (n: 'arkarea.005'; l: $10000;
    p: $20000; crc: $9886004D));
  aarea_bgtiles: array [0 .. 2] of tipo_roms = ((n: 'arkarea.003'; l: $10000; p: 0; crc: $6F45A308),
    (n: 'arkarea.002'; l: $10000; p: $10000; crc: $051D3482), (n: 'arkarea.001'; l: $10000;
    p: $20000; crc: $09D11AB7));
  // DIP
  aarea_dip_a: array [0 .. 6] of def_dip = ((mask: $3; name: 'Coinage'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $20; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $40; dip_name: '50K 50K+'), (dip_val: $0; dip_name: '100K 100K+'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Lives'; number: 2;
    dip: ((dip_val: $80; dip_name: '3'), (dip_val: $0; dip_name: '4'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), ());
  // Mutant Night
  mnight_rom: array [0 .. 4] of tipo_roms = ((n: '1.j19'; l: $8000; p: 0; crc: $56678D14),
    (n: '2.j17'; l: $8000; p: $8000; crc: $2A73F88E), (n: '3.j16'; l: $8000; p: $10000;
    crc: $C5E42BB4), (n: '4.j14'; l: $8000; p: $18000; crc: $DF6A4F7A), (n: '5.j12'; l: $8000;
    p: $20000; crc: $9C391D1B));
  mnight_snd_rom: tipo_roms = (n: '6.j7'; l: $10000; p: 0; crc: $A0782A31);
  mnight_fgtiles: tipo_roms = (n: '13.b10'; l: $8000; p: 0; crc: $37B8221F);
  mnight_sprites: array [0 .. 2] of tipo_roms = ((n: '9.e11'; l: $10000; p: 0; crc: $4883059C),
    (n: '8.e12'; l: $10000; p: $10000; crc: $02B91445), (n: '7.e14'; l: $10000; p: $20000;
    crc: $9F08D160));
  mnight_bgtiles: array [0 .. 2] of tipo_roms = ((n: '12.b20'; l: $10000; p: 0; crc: $4D37E0F4),
    (n: '11.b22'; l: $10000; p: $10000; crc: $B22CBBD3), (n: '10.b23'; l: $10000; p: $20000;
    crc: $65714070));
  // DIP
  mnight_dip_a: array [0 .. 7] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $2; dip_name: '30K 50K+'), (dip_val: $0; dip_name: '50K 80K+'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Difficult'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Infinite Lives'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $10; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $10; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $80; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $40;
    dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  mnight_dip_b: array [0 .. 1] of def_dip = ((mask: $E0; name: 'Coinage'; number: 8;
    dip: ((dip_val: $80; dip_name: '1C 4C'), (dip_val: $0; dip_name: '5C 1C'), (dip_val: $20;
    dip_name: '4C 1C'), (dip_val: $40; dip_name: '3C 1C'), (dip_val: $60;
    dip_name: '2C 1C'), (dip_val: $E0; dip_name: '1C 1C'), (dip_val: $C0;
    dip_name: '1C 2C'), (dip_val: $A0; dip_name: '1C 3C'), (), (), (), (), (), (), (), ())), ());
  // Atomic Robo-Kid
  robokid_rom: array [0 .. 3] of tipo_roms = ((n: 'robokid1.18j'; l: $10000; p: 0; crc: $378C21FC),
    (n: 'robokid2.18k'; l: $10000; p: $10000; crc: $DDEF8C5A), (n: 'robokid3.15k'; l: $10000;
    p: $20000; crc: $05295EC3), (n: 'robokid4.12k'; l: $10000; p: $30000; crc: $3BC3977F));
  robokid_snd_rom: tipo_roms = (n: 'robokid.k7'; l: $10000; p: 0; crc: $F490A2E9);
  robokid_fgtiles: tipo_roms = (n: 'robokid.b9'; l: $8000; p: 0; crc: $FAC59C3F);
  robokid_sprites: array [0 .. 3] of tipo_roms = ((n: 'robokid.15f'; l: $10000; p: 0;
    crc: $BA61F5AB), (n: 'robokid.16f'; l: $10000; p: $10000; crc: $D9B399CE), (n: 'robokid.17f';
    l: $10000; p: $20000; crc: $AFE432B9), (n: 'robokid.18f'; l: $10000; p: $30000;
    crc: $A0AA2A84));
  robokid_bgtiles0: array [0 .. 6] of tipo_roms = ((n: 'robokid.19c'; l: $10000; p: 0;
    crc: $02220421), (n: 'robokid.20c'; l: $10000; p: $10000; crc: $02D59BC2), (n: 'robokid.17d';
    l: $10000; p: $20000; crc: $2FA29B99), (n: 'robokid.18d'; l: $10000; p: $30000; crc: $AE15CE02),
    (n: 'robokid.19d'; l: $10000; p: $40000; crc: $784B089E), (n: 'robokid.20d'; l: $10000;
    p: $50000; crc: $B0B395ED), (n: 'robokid.19f'; l: $10000; p: $60000; crc: $0F9071C6));
  robokid_bgtiles1: array [0 .. 7] of tipo_roms = ((n: 'robokid.12c'; l: $10000; p: 0;
    crc: $0AB45F94), (n: 'robokid.14c'; l: $10000; p: $10000; crc: $029BBD4A), (n: 'robokid.15c';
    l: $10000; p: $20000; crc: $7DE67EBB), (n: 'robokid.16c'; l: $10000; p: $30000; crc: $53C0E582),
    (n: 'robokid.17c'; l: $10000; p: $40000; crc: $0CAE5A1E), (n: 'robokid.18c'; l: $10000;
    p: $50000; crc: $56AC7C8A), (n: 'robokid.15d'; l: $10000; p: $60000; crc: $CD632A4D),
    (n: 'robokid.16d'; l: $10000; p: $70000; crc: $18D92B2B));
  robokid_bgtiles2: array [0 .. 5] of tipo_roms = ((n: 'robokid.12a'; l: $10000; p: 0;
    crc: $E64D1C10), (n: 'robokid.14a'; l: $10000; p: $10000; crc: $8F9371E4), (n: 'robokid.15a';
    l: $10000; p: $20000; crc: $469204E7), (n: 'robokid.16a'; l: $10000; p: $30000; crc: $4E340815),
    (n: 'robokid.17a'; l: $10000; p: $40000; crc: $F0863106), (n: 'robokid.18a'; l: $10000;
    p: $50000; crc: $FDFF7441));
  robokid_dip_a: array [0 .. 7] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $2; dip_name: '50K 100K+'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Free Play'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $10; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $10; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $80; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $40;
    dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());

var
  rom_bank: array [0 .. $F, 0 .. $3FFF] of byte;
  mem_snd_opc: array [0 .. $7FFF] of byte;
  fg_data: array [0 .. $7FF] of byte;
  xshift, yshift, rom_nbank, sound_latch: byte;
  fg_color, sprite_color: word;
  sprite_overdraw: boolean;
  scroll_x, scroll_y: array [0 .. 2] of word;
  bg_enable: array [0 .. 2] of boolean;
  bg_bank: array [0 .. 2] of byte;
  pant_sprites_tmp: array [0 .. $3FFFF] of byte;
  bg_ram: array [0 .. 2, 0 .. $1FFF] of byte;
  update_background: procedure;
  update_video_upl: procedure;
  sprite_comp: function(pos: word): boolean;
  // Ninjakid2 PCM
  ninjakid2_pcm: array [0 .. $FFFF] of byte;
  ninjakid2_pcm_pos: dword;
  ninjakid2_timer: byte;

procedure bg_ninjakid2;
var
  f, color, nchar: word;
  x, y, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    atrib := memory[$D801 + (f * 2)];
    color := atrib and $F;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := (memory[$D800 + (f * 2)] + ((atrib and $C0) shl 2)) and $3FF;
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 2, 1, (atrib and $10) <> 0,
        (atrib and $20) <> 0);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x[0], scroll_y[0]);
end;

procedure bg_upl;
var
  f, color, nchar: word;
  x, y, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    atrib := bg_ram[0, $1 + (f * 2)];
    color := atrib and $F;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := (bg_ram[0, f * 2] + ((atrib and $10) shl 6) + ((atrib and $C0) shl 2)) mod $600;
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 2, 1, false, (atrib and $20) <> 0);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x[0], scroll_y[0]);
end;

procedure put_gfx_sprite_upl(nchar: dword; color: word; flipx, flipy: boolean; pos_x, pos_y: word);
var
  x, y: byte;
  pos_temp: dword;
  temp: pword;
  pos, post: pbyte;
begin
  if flipx then
  begin
    pos := gfx[2].datos;
    inc(pos, nchar * 16 * 16 + 15);
    for y := 0 to 15 do
    begin
      post := pos;
      inc(post, (y * 16));
      temp := punbuf;
      if flipy then
        pos_temp := (pos_y + (15 - y)) * 512 + pos_x + 15
      else
        pos_temp := (pos_y + y) * 512 + pos_x + 15;
      for x := 15 downto 0 do
      begin
        if post^ <> 15 then
          temp^ := paleta[gfx[2].colores[post^ + color + sprite_color]]
        else
          temp^ := paleta[MAX_COLORS];
        pant_sprites_tmp[pos_temp] := color and $FF;
        pos_temp := pos_temp - 1;
        dec(post);
        inc(temp);
      end;
      if flipy then
        putpixel(0, (15 - y), 16, punbuf, PANT_SPRITES)
      else
        putpixel(0, y, 16, punbuf, PANT_SPRITES);
    end;
  end
  else
  begin
    pos := gfx[2].datos;
    inc(pos, nchar * 16 * 16);
    for y := 0 to 15 do
    begin
      temp := punbuf;
      if flipy then
        pos_temp := (pos_y + (15 - y)) * 512 + pos_x
      else
        pos_temp := (pos_y + y) * 512 + pos_x;
      for x := 0 to 15 do
      begin
        if pos^ <> 15 then
          temp^ := paleta[gfx[2].colores[pos^ + color + sprite_color]]
        else
          temp^ := paleta[MAX_COLORS];
        pant_sprites_tmp[pos_temp] := color and $FF;
        pos_temp := pos_temp + 1;
        inc(temp);
        inc(pos);
      end;
      if flipy then
        putpixel(0, (15 - y), 16, punbuf, PANT_SPRITES)
      else
        putpixel(0, y, 16, punbuf, PANT_SPRITES);
    end;
  end;
end;

function sprite_comp_upl(pos: word): boolean;
begin
  sprite_comp_upl := ((pant_sprites_tmp[pos] and $F0) = $F0);
end;

function sprite_comp_robokid(pos: word): boolean;
begin
  sprite_comp_robokid := ((pant_sprites_tmp[pos] and $F0) < $E0);
end;

procedure draw_sprites;
var
  f, color, nchar, sx, tile: word;
  x, y, sy, atrib, num_sprites, big: byte;
  flipx, flipy: boolean;
  tf: dword;
  pos_pixels: pword;
begin
  if not(sprite_overdraw) then
  begin
    fill_full_screen(4, MAX_COLORS);
    fillchar(pant_sprites_tmp[0], 512 * 256, 0);
  end
  else
  begin
    for sy := 0 to 255 do
    begin
      pos_pixels := gscreen[4].pixels;
      inc(pos_pixels, (sy * gscreen[4].pitch) shr 1);
      tf := sy * 512;
      for sx := 0 to 255 do
      begin
        if sprite_comp(tf) then
        begin
          pant_sprites_tmp[tf] := 0;
          pos_pixels^ := paleta[MAX_COLORS];
        end;
        tf := tf + 1;
        inc(pos_pixels);
      end;
    end;
  end;
  num_sprites := 0;
  f := 0;
  repeat
    atrib := buffer_sprites[$D + f];
    if (atrib and $2) <> 0 then
    begin
      sx := buffer_sprites[$C + f] - ((atrib and $01) shl 8);
      sy := buffer_sprites[$B + f];
      // Ninja Kid II doesn't use the topmost bit (it has smaller ROMs) so it might not be connected on the board
      nchar := buffer_sprites[$E + f] + ((atrib and $C0) shl 2) + ((atrib and $08) shl 7);
      flipx := (atrib and $10) <> 0;
      flipy := (atrib and $20) <> 0;
      color := (buffer_sprites[$F + f] and $F) shl 4;
      // Ninja Kid II doesn't use the 'big' feature so it might not be available on the board
      big := (atrib and $04) shr 2;
      if big <> 0 then
      begin
        nchar := nchar and $FFFC;
        nchar := nchar xor (byte(flipx) shl xshift);
        nchar := nchar xor (byte(flipy) shl yshift);
      end;
      for y := 0 to big do
      begin
        for x := 0 to big do
        begin
          tile := nchar xor (x shl xshift) xor (y shl yshift);
          put_gfx_sprite_upl(tile, color, flipx, flipy, sx + 16 * x, sy + 16 * y);
          actualiza_trozo(0, 0, 16, 16, PANT_SPRITES, sx + 16 * x, sy + 16 * y, 16, 16, 4);
          num_sprites := num_sprites + 1;
        end;
      end;
    end
    else
      num_sprites := num_sprites + 1;
    f := f + $10;
  until num_sprites = 96;
end;

procedure update_foreground;
var
  f, nchar: word;
  x, y, atrib, color: byte;
begin
  for f := $0 to $3FF do
  begin
    atrib := fg_data[1 + (f * 2)];
    color := atrib and $F;
    if (gfx[0].buffer[f] or buffer_color[color + $10]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := (fg_data[f * 2] + ((atrib and $C0) shl 2)) and $3FF;
      put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + fg_color, 1, 0, (atrib and $10) <> 0,
        (atrib and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
end;

procedure update_video_ninjakid2;
begin
  // background
  if bg_enable[0] then
    update_background
  else
    fill_full_screen(3, $400);
  // Sprites
  draw_sprites;
  actualiza_trozo(0, 0, 256, 256, 4, 0, 0, 256, 256, 3);
  // Chars
  update_foreground;
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  // Final
  actualiza_trozo_final(0, 32, 256, 192, 3);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure update_video_robokid;
  procedure robokid_bg(nbg, npant, ngfx: byte; trans: boolean);
  var
    f, pos, nchar: word;
    x, y, atrib, color: byte;
  begin
    for f := 0 to $3FF do
    begin
      x := f mod 32;
      y := f div 32;
      pos := (x and $0F) or ((y and $1F) shl 4) or ((x and $10) shl 5);
      atrib := bg_ram[nbg, (pos shl 1) or 1];
      color := atrib and $F;
      if (gfx[ngfx].buffer[pos] or buffer_color[color]) then
      begin
        nchar := (((atrib and $10) shl 7) or ((atrib and $20) shl 5) or ((atrib and $C0) shl 2) or
          bg_ram[nbg, pos shl 1]) and $FFF;
        if trans then
          put_gfx_trans(x * 16, y * 16, nchar, color shl 4, npant, ngfx)
        else
          put_gfx(x * 16, y * 16, nchar, color shl 4, npant, ngfx);
        gfx[ngfx].buffer[pos] := false;
      end;
    end;
    scroll_x_y(npant, 3, scroll_x[nbg], scroll_y[nbg]);
  end;

begin
  // background 0-1
  if bg_enable[0] then
    robokid_bg(0, 2, 1, false)
  else
    fill_full_screen(3, $400);
  if bg_enable[1] then
    robokid_bg(1, 5, 3, true);
  // Sprites
  draw_sprites;
  actualiza_trozo(0, 0, 256, 256, 4, 0, 0, 256, 256, 3);
  // background 2
  if bg_enable[2] then
    robokid_bg(2, 6, 4, true);
  // Chars
  update_foreground;
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  // Final
  actualiza_trozo_final(0, 32, 256, 192, 3);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_upl;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
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
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.left[1] then
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
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // COIN
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure upl_loop;
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
        // main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // snd
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 223 then
        begin
          z80_0.change_irq(HOLD_LINE);
          update_video_upl;
        end;
      end;
      events_upl;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure change_color(pos: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[pos];
  color.r := pal4bit(tmp_color shr 4);
  color.g := pal4bit(tmp_color);
  tmp_color := buffer_paleta[pos + 1];
  color.b := pal4bit(tmp_color shr 4);
  pos := pos shr 1;
  set_pal_color(color, pos);
  case pos of
    $0 .. $FF:
      buffer_color[pos shr 4] := true;
    $200 .. $2FF:
      buffer_color[((pos shr 4) and $F) + $10] := true;
  end;
end;

// Generic
function upl_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $D9FF:
      upl_getbyte := memory[direccion];
    $8000 .. $BFFF:
      upl_getbyte := rom_bank[rom_nbank, direccion and $3FFF];
    $DA00 .. $DFFF:
      upl_getbyte := buffer_sprites[direccion - $DA00];
    $E000 .. $E7FF:
      upl_getbyte := bg_ram[0, direccion and $7FF];
    $E800 .. $EFFF:
      upl_getbyte := fg_data[direccion and $7FF];
    $F000 .. $F5FF:
      upl_getbyte := buffer_paleta[direccion and $7FF];
    $F800:
      upl_getbyte := marcade.in0;
    $F801:
      upl_getbyte := marcade.in1;
    $F802:
      upl_getbyte := marcade.in2;
    $F803:
      upl_getbyte := marcade.dswa;
    $F804:
      upl_getbyte := marcade.dswb;
  end;
end;

procedure upl_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $D9FF:
      memory[direccion] := valor;
    $DA00 .. $DFFF:
      buffer_sprites[direccion - $DA00] := valor;
    $E000 .. $E7FF:
      if bg_ram[0, direccion and $7FF] <> valor then
      begin
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
        bg_ram[0, direccion and $7FF] := valor;
      end;
    $E800 .. $EFFF:
      if fg_data[direccion and $7FF] <> valor then
      begin
        fg_data[direccion and $7FF] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $F000 .. $F5FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color(direccion and $7FE);
      end;
    $FA00:
      sound_latch := valor;
    $FA01:
      begin
        if (valor and $10) <> 0 then
          z80_1.reset;
        main_screen.flip_main_screen := (valor and $80) <> 0;
      end;
    $FA02:
      rom_nbank := valor and $7;
    $FA03:
      sprite_overdraw := (valor and $1) <> 0;
    $FA08:
      scroll_x[0] := (scroll_x[0] and $FF00) or valor;
    $FA09:
      scroll_x[0] := (scroll_x[0] and $00FF) or ((valor and $1) shl 8);
    $FA0A:
      scroll_y[0] := (scroll_y[0] and $FF00) or valor;
    $FA0B:
      scroll_y[0] := (scroll_y[0] and $00FF) or ((valor and $1) shl 8);
    $FA0C:
      bg_enable[0] := (valor and $1) <> 0;
  end;
end;

// Ninja Kid II
function ninjakid2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $D800 .. $F9FF:
      ninjakid2_getbyte := memory[direccion];
    $8000 .. $BFFF:
      ninjakid2_getbyte := rom_bank[rom_nbank, direccion and $3FFF];
    $C000:
      ninjakid2_getbyte := marcade.in0;
    $C001:
      ninjakid2_getbyte := marcade.in1;
    $C002:
      ninjakid2_getbyte := marcade.in2;
    $C003:
      ninjakid2_getbyte := marcade.dswa;
    $C004:
      ninjakid2_getbyte := marcade.dswb;
    $C800:
      ninjakid2_getbyte := buffer_paleta[direccion and $7FF];
    $D000 .. $D7FF:
      ninjakid2_getbyte := fg_data[direccion and $7FF];
    $FA00 .. $FFFF:
      ninjakid2_getbyte := buffer_sprites[direccion - $FA00];
  end;
end;

procedure ninjakid2_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C200:
      sound_latch := valor;
    $C201:
      begin
        if (valor and $10) <> 0 then
          z80_1.reset;
        main_screen.flip_main_screen := (valor and $80) <> 0;
      end;
    $C202:
      rom_nbank := valor and $7;
    $C203:
      sprite_overdraw := (valor and $1) <> 0;
    $C208:
      scroll_x[0] := (scroll_x[0] and $FF00) or valor;
    $C209:
      scroll_x[0] := (scroll_x[0] and $00FF) or ((valor and $1) shl 8);
    $C20A:
      scroll_y[0] := (scroll_y[0] and $FF00) or valor;
    $C20B:
      scroll_y[0] := (scroll_y[0] and $00FF) or ((valor and $1) shl 8);
    $C20C:
      bg_enable[0] := (valor and $1) <> 0;
    $C800 .. $CDFF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color(direccion and $7FE);
      end;
    $D000 .. $D7FF:
      if fg_data[direccion and $7FF] <> valor then
      begin
        fg_data[direccion and $7FF] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $E000 .. $F9FF:
      memory[direccion] := valor;
    $FA00 .. $FFFF:
      buffer_sprites[direccion - $FA00] := valor;
  end;
end;

function ninjakid2_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF:
      if z80_1.opcode then
        ninjakid2_snd_getbyte := mem_snd_opc[direccion]
      else
        ninjakid2_snd_getbyte := mem_snd[direccion];
    $8000 .. $C7FF:
      ninjakid2_snd_getbyte := mem_snd[direccion];
    $E000:
      ninjakid2_snd_getbyte := sound_latch;
  end;
end;

procedure ninjakid2_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $F000:
      begin // PCM
        ninjakid2_pcm_pos := valor shl 8;
        if ninjakid2_pcm[ninjakid2_pcm_pos] <> 0 then
        begin
          dac_0.data8_w(ninjakid2_pcm[ninjakid2_pcm_pos]);
          timers.enabled(ninjakid2_timer, true);
        end
        else
        begin
          timers.enabled(ninjakid2_timer, false);
          dac_0.data8_w(0);
        end;
      end;
  end;
end;

procedure ninjakid2_snd_timer;
begin
  ninjakid2_pcm_pos := ninjakid2_pcm_pos + 1;
  if ((ninjakid2_pcm_pos = $10000) or (ninjakid2_pcm[ninjakid2_pcm_pos] = 0)) then
  begin
    timers.enabled(ninjakid2_timer, false);
    dac_0.data8_w(0);
  end
  else
    dac_0.data8_w(ninjakid2_pcm[ninjakid2_pcm_pos]);
end;

procedure ninjakid2_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
  dac_0.Update;
end;

// Atomic Robo-kid
function robokid_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $E000 .. $F9FF:
      robokid_getbyte := memory[direccion];
    $8000 .. $BFFF:
      robokid_getbyte := rom_bank[rom_nbank, direccion and $3FFF];
    $C000 .. $C7FF:
      robokid_getbyte := buffer_paleta[direccion and $7FF];
    $C800 .. $CFFF:
      robokid_getbyte := fg_data[direccion and $7FF];
    $D000 .. $D3FF:
      robokid_getbyte := bg_ram[2, (direccion and $3FF) + (bg_bank[2] * $400)];
    $D400 .. $D7FF:
      robokid_getbyte := bg_ram[1, (direccion and $3FF) + (bg_bank[1] * $400)];
    $D800 .. $DBFF:
      robokid_getbyte := bg_ram[0, (direccion and $3FF) + (bg_bank[0] * $400)];
    $DC00:
      robokid_getbyte := marcade.in0;
    $DC01:
      robokid_getbyte := marcade.in1;
    $DC02:
      robokid_getbyte := marcade.in2;
    $DC03:
      robokid_getbyte := marcade.dswa;
    $DC04:
      robokid_getbyte := marcade.dswb;
    $FA00 .. $FFFF:
      robokid_getbyte := buffer_sprites[direccion - $FA00];
  end;
end;

procedure change_color_robokid(pos: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[pos];
  color.r := pal4bit(tmp_color shr 4);
  color.g := pal4bit(tmp_color);
  tmp_color := buffer_paleta[pos + 1];
  color.b := pal4bit(tmp_color shr 4);
  pos := pos shr 1;
  set_pal_color(color, pos);
  case pos of
    $0 .. $FF:
      buffer_color[pos shr 4] := true;
    $300 .. $3FF:
      buffer_color[((pos shr 4) and $F) + $10] := true;
  end;
end;

procedure robokid_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color_robokid(direccion and $7FE);
      end;
    $C800 .. $CFFF:
      if fg_data[direccion and $7FF] <> valor then
      begin
        fg_data[direccion and $7FF] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $D000 .. $D3FF:
      if bg_ram[2, (direccion and $3FF) + (bg_bank[2] * $400)] <> valor then
      begin
        bg_ram[2, (direccion and $3FF) + (bg_bank[2] * $400)] := valor;
        gfx[4].buffer[((direccion and $3FF) + (bg_bank[2] * $400)) shr 1] := true;
      end;
    $D400 .. $D7FF:
      if bg_ram[1, (direccion and $3FF) + (bg_bank[1] * $400)] <> valor then
      begin
        bg_ram[1, (direccion and $3FF) + (bg_bank[1] * $400)] := valor;
        gfx[3].buffer[((direccion and $3FF) + (bg_bank[1] * $400)) shr 1] := true;
      end;
    $D800 .. $DBFF:
      if bg_ram[0, (direccion and $3FF) + (bg_bank[0] * $400)] <> valor then
      begin
        bg_ram[0, (direccion and $3FF) + (bg_bank[0] * $400)] := valor;
        gfx[1].buffer[((direccion and $3FF) + (bg_bank[0] * $400)) shr 1] := true;
      end;
    $DC00:
      sound_latch := valor;
    $DC01:
      begin
        if (valor and $10) <> 0 then
          z80_1.reset;
        main_screen.flip_main_screen := (valor and $80) <> 0;
      end;
    $DC02:
      rom_nbank := valor and $F;
    $DC03:
      sprite_overdraw := (valor and $1) <> 0;
    $DD00:
      scroll_x[0] := (scroll_x[0] and $FF00) or valor;
    $DD01:
      scroll_x[0] := (scroll_x[0] and $00FF) or ((valor and $1) shl 8);
    $DD02:
      scroll_y[0] := (scroll_y[0] and $FF00) or valor;
    $DD03:
      scroll_y[0] := (scroll_y[0] and $00FF) or ((valor and $1) shl 8);
    $DD04:
      bg_enable[0] := (valor and $1) <> 0;
    $DD05:
      bg_bank[0] := valor and $1;
    $DE00:
      scroll_x[1] := (scroll_x[1] and $FF00) or valor;
    $DE01:
      scroll_x[1] := (scroll_x[1] and $00FF) or ((valor and $1) shl 8);
    $DE02:
      scroll_y[1] := (scroll_y[1] and $FF00) or valor;
    $DE03:
      scroll_y[1] := (scroll_y[1] and $00FF) or ((valor and $1) shl 8);
    $DE04:
      bg_enable[1] := (valor and $1) <> 0;
    $DE05:
      bg_bank[1] := valor and $1;
    $DF00:
      scroll_x[2] := (scroll_x[2] and $FF00) or valor;
    $DF01:
      scroll_x[2] := (scroll_x[2] and $00FF) or ((valor and $1) shl 8);
    $DF02:
      scroll_y[2] := (scroll_y[2] and $FF00) or valor;
    $DF03:
      scroll_y[2] := (scroll_y[2] and $00FF) or ((valor and $1) shl 8);
    $DF04:
      bg_enable[2] := (valor and $1) <> 0;
    $DF05:
      bg_bank[2] := valor and $1;
    $E000 .. $F9FF:
      memory[direccion] := valor;
    $FA00 .. $FFFF:
      buffer_sprites[direccion - $FA00] := valor;
  end;
end;

// Sound
function upl_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $C7FF:
      upl_snd_getbyte := mem_snd[direccion];
    $E000:
      upl_snd_getbyte := sound_latch;
  end;
end;

procedure upl_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $F000:
      ;
  end;
end;

function upl_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $00:
      upl_snd_inbyte := ym2203_0.status;
    $01:
      upl_snd_inbyte := ym2203_0.Read;
    $80:
      upl_snd_inbyte := ym2203_1.status;
    $81:
      upl_snd_inbyte := ym2203_1.Read;
  end;
end;

procedure upl_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $00:
      ym2203_0.Control(valor);
    $01:
      ym2203_0.Write(valor);
    $80:
      ym2203_1.Control(valor);
    $81:
      ym2203_1.Write(valor);
  end;
end;

procedure upl_snd_irq(irqstate: byte);
begin
  z80_1.change_irq(irqstate);
end;

procedure upl_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
end;

procedure reset_upl;
var
  f: byte;
begin
  z80_0.reset;
  z80_0.im0 := $D7; // rst 10
  z80_1.reset;
  ym2203_0.reset;
  ym2203_1.reset;
  if main_vars.machine_type = 120 then
    dac_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  rom_nbank := 0;
  sprite_overdraw := false;
  sound_latch := 0;
  ninjakid2_pcm_pos := 0;
  for f := 0 to 2 do
  begin
    bg_bank[f] := 0;
    bg_enable[f] := false;
    scroll_x[f] := 0;
    scroll_y[f] := 0;
  end;
end;

function start_ninjakid2: boolean;
var
  f: byte;
  memory_temp: array [0 .. $7FFFF] of byte;
  mem_key: array [0 .. $1FFF] of byte;
const
  pt_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4,
    32 * 8 + 0 * 4, 32 * 8 + 1 * 4, 32 * 8 + 2 * 4, 32 * 8 + 3 * 4, 32 * 8 + 4 * 4, 32 * 8 + 5 * 4,
    32 * 8 + 6 * 4, 32 * 8 + 7 * 4);
  pt_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32,
    64 * 8 + 0 * 32, 64 * 8 + 1 * 32, 64 * 8 + 2 * 32, 64 * 8 + 3 * 32, 64 * 8 + 4 * 32,
    64 * 8 + 5 * 32, 64 * 8 + 6 * 32, 64 * 8 + 7 * 32);
  pt_x_r: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4,
    4 * 8 * 16 + 0 * 4, 4 * 8 * 16 + 1 * 4, 4 * 8 * 16 + 2 * 4, 4 * 8 * 16 + 3 * 4,
    4 * 8 * 16 + 4 * 4, 4 * 8 * 16 + 5 * 4, 4 * 8 * 16 + 6 * 4, 4 * 8 * 16 + 7 * 4);
  pt_y_r: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32,
    7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  procedure lineswap_gfx_roms(length: dword; src: pbyte; bit: byte);
  var
    ptemp, ptemp2, ptemp3: pbyte;
    f, pos, mask: dword;
  begin
    getmem(ptemp, length);
    mask := (1 shl (bit + 1)) - 1;
    for f := 0 to (length - 1) do
    begin
      pos := (f and not(mask)) or ((f shl 1) and mask) or ((f shr bit) and 1);
      ptemp2 := ptemp;
      inc(ptemp2, pos);
      ptemp3 := src;
      inc(ptemp3, f);
      ptemp2^ := ptemp3^;
    end;
    copymemory(src, ptemp, length);
    freemem(ptemp);
  end;
  procedure extract_char(swap: boolean);
  begin
    if swap then
      lineswap_gfx_roms($8000, @memory_temp, 13);
    init_gfx(0, 8, 8, $400);
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memory_temp, @pt_x, @pt_y, false, false);
  end;
  procedure extract_gr2(size: dword; num: byte; size_gr: word; swap: boolean);
  begin
    init_gfx(num, 16, 16, size_gr);
    if swap then
    begin
      lineswap_gfx_roms(size, @memory_temp, 14);
      gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
      convert_gfx(num, 0, @memory_temp, @pt_x, @pt_y, false, false);
    end
    else
    begin
      gfx_set_desc_data(4, 0, 16 * 16 * 4, 0, 1, 2, 3);
      convert_gfx(num, 0, @memory_temp, @pt_x_r, @pt_y_r, false, false);
    end;
  end;

begin
  machine_calls.general_loop := upl_loop;
  machine_calls.reset := reset_upl;
  machine_calls.fps_max := 59.61;
  start_ninjakid2 := false;
  start_audio(false);
  screen_init(1, 256, 256, true); // FG
  screen_init(2, 512, 512); // BG0
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  if main_vars.machine_type = 307 then
  begin
    screen_init(5, 512, 512, true); // BG1
    screen_mod_scroll(5, 512, 256, 511, 512, 256, 511);
    screen_init(6, 512, 512, true); // BG2
    screen_mod_scroll(6, 512, 256, 511, 512, 256, 511);
  end;
  screen_init(3, 512, 256, false, true);
  screen_init(4, 512, 256, true); // Sprites
  start_video(256, 192);
  // Main CPU
  z80_0 := cpu_z80.create(6000000, 256);
  z80_0.change_ram_calls(upl_getbyte, upl_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(5000000, 256);
  z80_1.change_ram_calls(upl_snd_getbyte, upl_snd_putbyte);
  z80_1.change_io_calls(upl_snd_inbyte, upl_snd_outbyte);
  // Que no se me olvide!!! Primero la CPU de sonido y luego el chip de audio!!!!
  if main_vars.machine_type = 120 then
    z80_1.init_sound(ninjakid2_sound_update)
  else
    z80_1.init_sound(upl_sound_update);
  // Sound Chips
  ym2203_0 := ym2203_chip.create(1500000, 0.5, 0.1);
  ym2203_0.change_irq_calls(upl_snd_irq);
  ym2203_1 := ym2203_chip.create(1500000, 0.5, 0.1);
  // Video
  update_video_upl := update_video_ninjakid2;
  update_background := bg_upl;
  sprite_color := $100;
  fg_color := $200;
  xshift := 0;
  yshift := 1;
  sprite_comp := sprite_comp_upl;
  case main_vars.machine_type of
    120:
      begin // Ninja Kid 2
        z80_0.change_ram_calls(ninjakid2_getbyte, ninjakid2_putbyte);
        z80_1.change_ram_calls(ninjakid2_snd_getbyte, ninjakid2_snd_putbyte);
        update_background := bg_ninjakid2;
        // cargar roms y ponerlas en sus bancos
        if not(roms_load(@memory_temp, ninjakid2_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        for f := 0 to 7 do
          copymemory(@rom_bank[f, 0], @memory_temp[(f * $4000) + $8000], $4000);
        // cargar ROMS sonido y desencriptar
        if not(roms_load(@mem_snd, ninjakid2_snd_rom)) then
          exit;
        if not(roms_load(@mem_key, ninjakid2_snd_key)) then
          exit;
        mc8123_decrypt_rom(@mem_key, @mem_snd, @mem_snd_opc, $8000);
        if not(roms_load(@ninjakid2_pcm, ninjakid2_pcm_rom)) then
          exit;
        dac_0 := dac_chip.create(1);
        ninjakid2_timer := timers.init(z80_1.numero_cpu, 5000000 / 16300, ninjakid2_snd_timer,
          nil, false);
        // convertir fg
        if not(roms_load(@memory_temp, ninjakid2_fgtiles)) then
          exit;
        extract_char(true);
        // convertir bg
        if not(roms_load(@memory_temp, ninjakid2_bgtiles)) then
          exit;
        extract_gr2($20000, 1, $400, true);
        // convertir sprites
        if not(roms_load(@memory_temp, ninjakid2_sprites)) then
          exit;
        extract_gr2($20000, 2, $400, true);
        // DIP
        marcade.dswa := $6F;
        marcade.dswb := $FD;
        marcade.dswa_val := @ninjakid2_dip_a;
        marcade.dswb_val := @ninjakid2_dip_b;
      end;
    121:
      begin // Ark Area
        // cargar roms y ponerlas en sus bancos
        if not(roms_load(@memory_temp, aarea_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        for f := 0 to 7 do
          copymemory(@rom_bank[f, 0], @memory_temp[(f * $4000) + $8000], $4000);
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, aarea_snd_rom)) then
          exit;
        // convertir fg
        if not(roms_load(@memory_temp, aarea_fgtiles)) then
          exit;
        extract_char(true);
        // convertir bg
        if not(roms_load(@memory_temp, aarea_bgtiles)) then
          exit;
        extract_gr2($30000, 1, $600, true);
        // convertir sprites
        if not(roms_load(@memory_temp, aarea_sprites)) then
          exit;
        extract_gr2($30000, 2, $600, true);
        // DIP
        marcade.dswa := $EF;
        marcade.dswb := $FF;
        marcade.dswa_val := @aarea_dip_a;
      end;
    122:
      begin // Mutant Night
        // cargar roms y ponerlas en sus bancos
        if not(roms_load(@memory_temp, mnight_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        for f := 0 to 7 do
          copymemory(@rom_bank[f, 0], @memory_temp[(f * $4000) + $8000], $4000);
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, mnight_snd_rom)) then
          exit;
        // convertir fg
        if not(roms_load(@memory_temp, mnight_fgtiles)) then
          exit;
        extract_char(true);
        // convertir bg
        if not(roms_load(@memory_temp, mnight_bgtiles)) then
          exit;
        extract_gr2($30000, 1, $600, true);
        // convertir sprites
        if not(roms_load(@memory_temp, mnight_sprites)) then
          exit;
        extract_gr2($30000, 2, $600, true);
        // DIP
        marcade.dswa := $CF;
        marcade.dswb := $FF;
        marcade.dswa_val := @mnight_dip_a;
        marcade.dswb_val := @mnight_dip_b;
      end;
    307:
      begin // Atomic Robo-kid
        sprite_color := $200;
        fg_color := $300;
        xshift := 1;
        yshift := 0;
        update_video_upl := update_video_robokid;
        sprite_comp := sprite_comp_robokid;
        z80_0.change_ram_calls(robokid_getbyte, robokid_putbyte);
        if not(roms_load(@memory_temp, robokid_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to $F do
          copymemory(@rom_bank[f, 0], @memory_temp[f * $4000], $4000);
        // Parches!
        memory[$5247] := $E6;
        memory[$5248] := $03;
        memory[$5249] := $18;
        memory[$524A] := $F6;
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, robokid_snd_rom)) then
          exit;
        // convertir fg
        if not(roms_load(@memory_temp, robokid_fgtiles)) then
          exit;
        extract_char(false);
        // convertir bg0
        if not(roms_load(@memory_temp, robokid_bgtiles0)) then
          exit;
        extract_gr2($80000, 1, $1000, false);
        // convertir sprites
        if not(roms_load(@memory_temp, robokid_sprites)) then
          exit;
        extract_gr2($40000, 2, $800, false);
        // convertir bg1
        if not(roms_load(@memory_temp, robokid_bgtiles1)) then
          exit;
        extract_gr2($80000, 3, $1000, false);
        gfx[3].trans[15] := true;
        // convertir bg2
        if not(roms_load(@memory_temp, robokid_bgtiles2)) then
          exit;
        extract_gr2($80000, 4, $1000, false);
        gfx[4].trans[15] := true;
        // DIP
        marcade.dswa := $CF;
        marcade.dswb := $FF;
        marcade.dswa_val := @robokid_dip_a;
        marcade.dswb_val := @mnight_dip_b;
      end;
  end;
  gfx[0].trans[15] := true;
  gfx[2].trans[15] := true;
  // final
  reset_upl;
  start_ninjakid2 := true;
end;

end.
