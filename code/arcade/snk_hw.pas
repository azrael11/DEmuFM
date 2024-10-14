unit snk_hw;

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
  ym_3812,
  timer_engine,
  System.SysUtils;

function start_snk: boolean;

implementation

uses
  main;

const
  // ikari
  ikari_main: tipo_roms = (n: '1.rom'; l: $10000; p: 0; crc: $52A8B2DD);
  ikari_sub: tipo_roms = (n: '2.rom'; l: $10000; p: 0; crc: $45364D55);
  ikari_snd: tipo_roms = (n: '3.rom'; l: $10000; p: 0; crc: $56A26699);
  ikari_gfx1: tipo_roms = (n: '7.rom'; l: $4000; p: 0; crc: $A7EB4917);
  ikari_gfx2: array [0 .. 3] of tipo_roms = ((n: '17.rom'; l: $8000; p: 0; crc: $E0DBA976), (n: '18.rom'; l: $8000; p: $8000; crc: $24947D5F), (n: '19.rom'; l: $8000; p: $10000; crc: $9EE59E91),
    (n: '20.rom'; l: $8000; p: $18000; crc: $5DA7EC1A));
  ikari_sprite16: array [0 .. 2] of tipo_roms = ((n: '8.rom'; l: $8000; p: 0; crc: $9827C14A), (n: '9.rom'; l: $8000; p: $8000; crc: $545C790C), (n: '10.rom'; l: $8000; p: $10000; crc: $EC9BA07E));
  ikari_sprite32: array [0 .. 5] of tipo_roms = ((n: '11.rom'; l: $8000; p: 0; crc: $5C75EA8F), (n: '14.rom'; l: $8000; p: $8000; crc: $3293FDE4), (n: '12.rom'; l: $8000; p: $10000; crc: $95138498),
    (n: '15.rom'; l: $8000; p: $18000; crc: $65A61C99), (n: '13.rom'; l: $8000; p: $20000; crc: $315383D7), (n: '16.rom'; l: $8000; p: $28000; crc: $E9B03E07));
  ikari_proms: array [0 .. 2] of tipo_roms = ((n: '7122er.prm'; l: $400; p: 0; crc: $B9BF2C2C), (n: '7122eg.prm'; l: $400; p: $400; crc: $0703A770), (n: '7122eb.prm'; l: $400; p: $800;
    crc: $0A11CDDE));
  ikari_dip_a: array [0 .. 5] of def_dip = ((mask: $1; name: 'Allow killing each other'; number: 2;
    dip: ((dip_val: $1; dip_name: 'No'), (dip_val: $0; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2; name: 'P1 && P2 Fire Buttons'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Separate'), (dip_val: $0; dip_name: 'Common'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Lives'; number: 2;
    dip: ((dip_val: $8; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $10; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $30; dip_name: '1C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C0; name: 'Coin B'; number: 4; dip: ((dip_val: $0; dip_name: '1C 2C'), (dip_val: $40; dip_name: '1C 3C'), (dip_val: $80; dip_name: '1C 4C'), (dip_val: $C0;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  ikari_dip_b: array [0 .. 3] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Game Mode'; number: 4;
    dip: ((dip_val: $C; dip_name: 'Demo Sounds Off'), (dip_val: $8; dip_name: 'Demo Sounds On'), (dip_val: $4; dip_name: 'Freeze'), (dip_val: $0; dip_name: 'Infinite Lives (Cheat)'), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'Yes'), (dip_val: $80; dip_name: 'No'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), ());
  ikari_dip_c: array [0 .. 1] of def_dip = ((mask: $34; name: 'Bonus Life'; number: 7; dip: ((dip_val: $34; dip_name: '50K 100K 100K+'), (dip_val: $24; dip_name: '60K 120K 120K+'), (dip_val: $14;
    dip_name: '100K 200K 200K+'), (dip_val: $30; dip_name: '50K 100K'), (dip_val: $20; dip_name: '60K 120K'), (dip_val: $10; dip_name: '100K 200K'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), (), ())), ());
  // athena
  athena_main: array [0 .. 1] of tipo_roms = ((n: 'up02_p4.rom'; l: $4000; p: 0; crc: $900A113C), (n: 'up02_m4.rom'; l: $8000; p: $4000; crc: $61C69474));
  athena_sub: array [0 .. 1] of tipo_roms = ((n: 'up02_p8.rom'; l: $4000; p: 0; crc: $DF50AF7E), (n: 'up02_m8.rom'; l: $8000; p: $4000; crc: $F3C933DF));
  athena_snd: array [0 .. 1] of tipo_roms = ((n: 'up02_g6.rom'; l: $4000; p: 0; crc: $42DBE029), (n: 'up02_k6.rom'; l: $8000; p: $4000; crc: $596F1C8A));
  athena_gfx1: tipo_roms = (n: 'up01_d2.rom'; l: $4000; p: 0; crc: $18B4BCCA);
  athena_gfx2: tipo_roms = (n: 'up01_b2.rom'; l: $8000; p: 0; crc: $F269C0EB);
  athena_sprite16: array [0 .. 2] of tipo_roms = ((n: 'up01_p2.rom'; l: $8000; p: 0; crc: $C63A871F), (n: 'up01_s2.rom'; l: $8000; p: $8000; crc: $760568D8), (n: 'up01_t2.rom'; l: $8000; p: $10000;
    crc: $57B35C73));
  athena_proms: array [0 .. 2] of tipo_roms = ((n: 'up02_c2.rom'; l: $400; p: 0; crc: $294279AE), (n: 'up02_b1.rom'; l: $400; p: $400; crc: $D25C9099), (n: 'up02_c1.rom'; l: $400; p: $800;
    crc: $A4A4E7DC));
  athena_dip_a: array [0 .. 4] of def_dip = ((mask: $2; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $8; name: 'Lives'; number: 2; dip: ((dip_val: $8; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30;
    name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $10; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $30; dip_name: '1C 1C'), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $C0; name: 'Coin B'; number: 4; dip: ((dip_val: $0; dip_name: '1C 2C'), (dip_val: $40; dip_name: '1C 3C'), (dip_val: $80; dip_name: '1C 4C'), (dip_val: $C0;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  athena_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Freeze'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Energy'; number: 2;
    dip: ((dip_val: $80; dip_name: '12'), (dip_val: $0; dip_name: '14'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  athena_dip_c: array [0 .. 1] of def_dip = ((mask: $34; name: 'Bonus Life'; number: 7; dip: ((dip_val: $34; dip_name: '50K 100K 100K+'), (dip_val: $24; dip_name: '60K 120K 120K+'), (dip_val: $14;
    dip_name: '100K 200K 200K+'), (dip_val: $30; dip_name: '50K 100K'), (dip_val: $20; dip_name: '60K 120K'), (dip_val: $10; dip_name: '100K 200K'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), (), ())), ());
  // tnk3
  tnk3_main: array [0 .. 2] of tipo_roms = ((n: 'tnk3-p1.bin'; l: $4000; p: 0; crc: $0D2A8CA9), (n: 'tnk3-p2.bin'; l: $4000; p: $4000; crc: $0AE0A483), (n: 'tnk3-p3.bin'; l: $4000; p: $8000;
    crc: $D16DD4DB));
  tnk3_sub: array [0 .. 2] of tipo_roms = ((n: 'tnk3-p4.bin'; l: $4000; p: 0; crc: $01B45A90), (n: 'tnk3-p5.bin'; l: $4000; p: $4000; crc: $60DB6667), (n: 'tnk3-p6.bin'; l: $4000; p: $8000;
    crc: $4761FDE7));
  tnk3_snd: array [0 .. 1] of tipo_roms = ((n: 'tnk3-p10.bin'; l: $4000; p: 0; crc: $7BF0A517), (n: 'tnk3-p11.bin'; l: $4000; p: $4000; crc: $0569CE27));
  tnk3_gfx1: tipo_roms = (n: 'tnk3-p14.bin'; l: $2000; p: 0; crc: $1FD18C43);
  tnk3_gfx2: array [0 .. 1] of tipo_roms = ((n: 'tnk3-p12.bin'; l: $4000; p: 0; crc: $FF495A16), (n: 'tnk3-p13.bin'; l: $4000; p: $4000; crc: $F8344843));
  tnk3_sprite16: array [0 .. 2] of tipo_roms = ((n: 'tnk3-p7.bin'; l: $4000; p: 0; crc: $06B92C88), (n: 'tnk3-p8.bin'; l: $4000; p: $4000; crc: $63D0E2EB), (n: 'tnk3-p9.bin'; l: $4000; p: $8000;
    crc: $872E3FAC));
  tnk3_proms: array [0 .. 2] of tipo_roms = ((n: '7122.2'; l: $400; p: 0; crc: $34C06BC6), (n: '7122.1'; l: $400; p: $400; crc: $6D0AC66A), (n: '7122.0'; l: $400; p: $800; crc: $4662B4C8));
  tnk3_dip_a: array [0 .. 4] of def_dip = ((mask: $1; name: 'No BG Collision'; number: 2; dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $2; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Lives'; number: 2; dip: ((dip_val: $4; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $38; name: 'Coinage';
    number: 6; dip: ((dip_val: $20; dip_name: '3C 1C'), (dip_val: $18; dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28;
    dip_name: '1C 3C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), ())), ());
  tnk3_dip_b: array [0 .. 4] of def_dip = ((mask: $6; name: 'Difficulty'; number: 4; dip: ((dip_val: $6; dip_name: 'Easy'), (dip_val: $4; dip_name: 'Normal'), (dip_val: $2;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Game Mode'; number: 4;
    dip: ((dip_val: $18; dip_name: 'Demo Sounds Off'), (dip_val: $10; dip_name: 'Demo Sounds On'), (dip_val: $0; dip_name: 'Freeze'), (dip_val: $8; dip_name: 'Infinite Lives'), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $20; name: 'Flip Screen'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $80; name: 'Allow Continue'; number: 2; dip: ((dip_val: $80; dip_name: 'No'), (dip_val: $0; dip_name: '5 Times'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  tnk3_dip_c: array [0 .. 1] of def_dip = ((mask: $C1; name: 'Bonus Life'; number: 7; dip: ((dip_val: $C1; dip_name: '20K 60K 60K+'), (dip_val: $81; dip_name: '40K 90K 90K+'), (dip_val: $41;
    dip_name: '50K 120K 120K+'), (dip_val: $C0; dip_name: '20K 60K'), (dip_val: $80; dip_name: '40K 90K'), (dip_val: $40; dip_name: '50K 120K'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), (), ())), ());
  // aso
  aso_main: array [0 .. 2] of tipo_roms = ((n: 'p1.8d'; l: $4000; p: 0; crc: $84981F3C), (n: 'p2.7d'; l: $4000; p: $4000; crc: $CFE912A6), (n: 'p3.5d'; l: $4000; p: $8000; crc: $39A666D2));
  aso_sub: array [0 .. 2] of tipo_roms = ((n: 'p4.3d'; l: $4000; p: 0; crc: $A4122355), (n: 'p5.2d'; l: $4000; p: $4000; crc: $9879E506), (n: 'p6.1d'; l: $4000; p: $8000; crc: $C0BFDF1F));
  aso_snd: array [0 .. 2] of tipo_roms = ((n: 'p7.4f'; l: $4000; p: 0; crc: $DBC19736), (n: 'p8.3f'; l: $4000; p: $4000; crc: $537726A9), (n: 'p9.2f'; l: $4000; p: $8000; crc: $AEF5A4F4));
  aso_gfx1: tipo_roms = (n: 'p14.1h'; l: $2000; p: 0; crc: $8BAA2253);
  aso_gfx2: tipo_roms = (n: 'p10.14h'; l: $8000; p: 0; crc: $00DFF996);
  aso_sprite16: array [0 .. 2] of tipo_roms = ((n: 'p11.11h'; l: $8000; p: $4000; crc: $7FEAC86C), (n: 'p12.9h'; l: $8000; p: $C000; crc: $6895990B), (n: 'p13.8h'; l: $8000; p: $14000;
    crc: $87A81CE1));
  aso_proms: array [0 .. 2] of tipo_roms = ((n: 'mb7122h.12f'; l: $400; p: 0; crc: $5B0A0059), (n: 'mb7122h.13f'; l: $400; p: $400; crc: $37E28DD8), (n: 'mb7122h.14f'; l: $400; p: $800;
    crc: $C3FD1DD3));
  aso_dip_a: array [0 .. 4] of def_dip = ((mask: $1; name: 'Allow Continue'; number: 2; dip: ((dip_val: $1; dip_name: 'No'), (dip_val: $0; dip_name: '3 Times'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $2; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Lives'; number: 2; dip: ((dip_val: $4; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $38; name: 'Coinage';
    number: 8; dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $28; dip_name: '3C 1C'), (dip_val: $30; dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $18;
    dip_name: '1C 2C'), (dip_val: $10; dip_name: '1C 3C'), (dip_val: $8; dip_name: '1C 4C'), (dip_val: $0; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), ());
  aso_dip_b: array [0 .. 5] of def_dip = ((mask: $6; name: 'Difficulty'; number: 4; dip: ((dip_val: $6; dip_name: 'Easy'), (dip_val: $4; dip_name: 'Normal'), (dip_val: $2;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'All Ships at Start'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Start Area'; number: 4;
    dip: ((dip_val: $C0; dip_name: '1'), (dip_val: $80; dip_name: '2'), (dip_val: $40; dip_name: '3'), (dip_val: $0; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  aso_dip_c: array [0 .. 1] of def_dip = ((mask: $C1; name: 'Bonus Life'; number: 7; dip: ((dip_val: $C1; dip_name: '50K 100K 100K+'), (dip_val: $81; dip_name: '60K 120K 120K+'), (dip_val: $41;
    dip_name: '100K 200K 200K+'), (dip_val: $C0; dip_name: '50K 100K'), (dip_val: $80; dip_name: '60K 120K'), (dip_val: $40; dip_name: '100K 200K'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 5;

var
  sp16_scroll_x, sp16_scroll_y, sp32_scroll_x, sp32_scroll_y, scroll_x, scroll_y, hf_posx, hf_posy, txt_offset, bg_offset, bg_pal_offset: word;
  sound_status, sound_latch, timer_sound, rot_cont: byte;
  update_video_snk, update_events_snk, tnk3_draw_sprites: procedure;
  txt_ram, sprite_ram: array [0 .. $7FF] of byte;
  bg_ram: array [0 .. $1FFF] of byte;

procedure draw_sprites32;
var
  f, color, atrib: byte;
  nchar: word;
  sx, sy: integer;
begin
  for f := 0 to 24 do
  begin
    atrib := memory[$E003 + (f * 4)];
    nchar := memory[$E001 + (f * 4)] or ((atrib and $40) shl 2);
    color := atrib and $F;
    sx := sp32_scroll_x + 300 - 32 - memory[$E002 + (f * 4)];
    sy := -sp32_scroll_y + 7 - 32 - 8 + memory[$E000 + (f * 4)];
    sx := sx + ((atrib and $80) shl 1);
    sy := sy + ((atrib and $10) shl 4);
    sx := sx and $1FF;
    sy := sy and $1FF;
    put_gfx_sprite_shadow(nchar, (color shl 3) + $80, false, false, 3, $401);
    update_gfx_sprite(sy, (256 - sx) and $1FF, 3, 3);
  end;
end;

procedure update_video_ikari;
  procedure draw_sprites16(pos: byte);
  var
    f, color, atrib: byte;
    nchar: word;
    sx, sy: integer;
  begin
    for f := 0 to 24 do
    begin
      atrib := memory[$E803 + (f * 4) + (pos * 25 * 4)];
      nchar := memory[$E801 + (f * 4) + (pos * 25 * 4)] or ((atrib and $60) shl 3);
      color := atrib and $F;
      sx := sp16_scroll_x + 300 - 16 - memory[$E802 + (f * 4) + (pos * 25 * 4)];
      sy := -sp16_scroll_y + 7 - 16 - 8 + memory[$E800 + (f * 4) + (pos * 25 * 4)];
      sx := sx + ((atrib and $80) shl 1);
      sy := sy + ((atrib and $10) shl 4);
      sx := sx and $1FF;
      sy := sy and $1FF;
      put_gfx_sprite_shadow(nchar, color shl 3, false, false, 2, $401);
      update_gfx_sprite(sy, (272 - sx) and $1FF, 3, 2);
    end;
  end;

var
  g, atrib, color: byte;
  x, y, f, nchar, pos: word;
  col: integer;
begin
  for f := 0 to 27 do
  begin
    for g := 0 to 35 do
    begin
      col := g - 2;
      if (col and $20) <> 0 then
        pos := $400 + f + ((col and $1F) shl 5)
      else
        pos := f + (col shl 5);
      if gfx[0].buffer[pos] then
      begin
        nchar := txt_ram[pos];
        if (pos and $400) <> 0 then
          put_gfx(f * 8, (35 - g) * 8, txt_offset + nchar, $180, 1, 0)
        else
          put_gfx_trans(f * 8, (35 - g) * 8, txt_offset + nchar, $180, 1, 0);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  // Brackgound
  for f := $0 to $3FF do
  begin
    atrib := bg_ram[$1 + (f * 2)];
    color := atrib and $70;
    if gfx[1].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := bg_ram[f * 2] + ((atrib and $3) shl 8);
      put_gfx(x * 16, (31 - y) * 16, nchar, color + $100, 2, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_y, 239 - scroll_x);
  draw_sprites16(0);
  draw_sprites32;
  draw_sprites16(1);
  update_region(0, 0, 224, 288, 1, 0, 0, 224, 288, 3);
  update_final_piece(0, 0, 216, 288, 3);
end;

procedure tnk3_sprites;
var
  atrib, color: byte;
  sx, sy, f, nchar: word;
begin
  // Sprites
  for f := 0 to 49 do
  begin
    atrib := sprite_ram[$3 + (f * 4)];
    nchar := sprite_ram[$1 + (f * 4)] or ((atrib and $40) shl 2);
    color := atrib and $F;
    sx := sp16_scroll_x + 301 - 16 - sprite_ram[$2 + (f * 4)];
    sy := -sp16_scroll_y + 7 - 24 + sprite_ram[$0 + (f * 4)];
    sx := sx + ((atrib and $80) shl 1);
    sy := sy + ((atrib and $10) shl 4);
    sx := sx and $1FF;
    sy := sy and $1FF;
    put_gfx_sprite_shadow(nchar, color shl 3, false, (atrib and $20) <> 0, 2, $401);
    update_gfx_sprite(sx, sy, 3, 2);
  end;
end;

procedure athena_sprites;
var
  atrib, color: byte;
  sx, sy, f, nchar: word;
begin
  // Sprites
  for f := 0 to 49 do
  begin
    atrib := sprite_ram[$3 + (f * 4)];
    nchar := sprite_ram[$1 + (f * 4)] or ((atrib and $40) shl 2) or ((atrib and $20) shl 4);
    color := atrib and $F;
    sx := sp16_scroll_x + 301 - 16 - sprite_ram[$2 + (f * 4)];
    sy := -sp16_scroll_y + 7 - 24 + sprite_ram[$0 + (f * 4)];
    sx := sx + ((atrib and $80) shl 1);
    sy := sy + ((atrib and $10) shl 4);
    sx := sx and $1FF;
    sy := sy and $1FF;
    put_gfx_sprite_shadow(nchar, color shl 3, false, false, 2, $401);
    update_gfx_sprite(sx, sy, 3, 2);
  end;
end;

procedure update_video_tnk3;
var
  g, atrib, color: byte;
  x, y, f, nchar, pos: word;
  col: integer;
begin
  for f := 0 to 27 do
  begin
    for g := 0 to 35 do
    begin
      col := g - 2;
      if (col and $20) <> 0 then
        pos := $400 + f + ((col and $1F) shl 5)
      else
        pos := f + (col shl 5);
      if gfx[0].buffer[pos] then
      begin
        nchar := txt_ram[pos];
        color := (nchar and $E0) shr 1;
        if (pos and $400) <> 0 then
          put_gfx(g * 8, f * 8, txt_offset + nchar, $180, 1, 0)
        else
          put_gfx_trans(g * 8, f * 8, txt_offset + nchar, $180 + color, 1, 0);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  // Brackgound
  for f := $0 to $FFF do
  begin
    atrib := bg_ram[$1 + (f * 2)];
    color := ((atrib and $F) xor 8) shl 4;
    if gfx[1].buffer[f] then
    begin
      x := f div 64;
      y := f mod 64;
      nchar := bg_ram[f * 2] + ((atrib and $30) shl 4);
      put_gfx(x * 8, y * 8, nchar, color + $80, 2, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x - 16, scroll_y);
  tnk3_draw_sprites;
  update_region(0, 0, 288, 224, 1, 0, 0, 288, 224, 3);
  update_final_piece(0, 0, 288, 216, 3);
end;

procedure update_video_aso;
var
  g, color: byte;
  x, y, f, nchar, pos: word;
  col: integer;
begin
  for f := 0 to 27 do
  begin
    for g := 0 to 35 do
    begin
      col := g - 2;
      if (col and $20) <> 0 then
        pos := $400 + f + ((col and $1F) shl 5)
      else
        pos := f + (col shl 5);
      if gfx[0].buffer[pos] then
      begin
        nchar := txt_ram[pos];
        color := (nchar and $E0) shr 1;
        if (pos and $400) <> 0 then
          put_gfx(g * 8, f * 8, nchar, $180, 1, 0)
        else
          put_gfx_trans(g * 8, f * 8, nchar, $180 + color, 1, 0);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  // Brackgound
  for f := $0 to $FFF do
  begin
    if gfx[1].buffer[f] then
    begin
      x := f div 64;
      y := f mod 64;
      nchar := bg_offset + bg_ram[f];
      put_gfx(x * 8, y * 8, nchar, $80 + bg_pal_offset, 2, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x - 16 + 256, scroll_y);
  tnk3_sprites;
  update_region(0, 0, 288, 224, 1, 0, 0, 288, 224, 3);
  update_final_piece(0, 0, 288, 216, 3);
end;

procedure events_ikari;
begin
  if event.arcade then
  begin
    // in0
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // in1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but2[0] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 + $10;
        if (marcade.in1 and $F0) = $C0 then
          marcade.in1 := marcade.in1 and $F;
        rot_cont := $0;
      end;
    end;
    if p_contrls.map_arcade.but3[0] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 - $10;
        if (marcade.in1 and $F0) = $F0 then
          marcade.in1 := $B0 or (marcade.in1 and $F);
        rot_cont := $0;
      end;
    end;
    // in2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := marcade.in2 and $FD
    else
      marcade.in2 := marcade.in2 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := marcade.in2 and $FB
    else
      marcade.in2 := marcade.in2 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := marcade.in2 and $F7
    else
      marcade.in2 := marcade.in2 or 8;
    if p_contrls.map_arcade.but2[1] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 + $10;
        if (marcade.in1 and $F0) = $C0 then
          marcade.in1 := marcade.in1 and $F;
        rot_cont := $0;
      end;
    end;
    if p_contrls.map_arcade.but3[1] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 - $10;
        if (marcade.in1 and $F0) = $F0 then
          marcade.in1 := $B0 or (marcade.in1 and $F);
        rot_cont := $0;
      end;
    end;
    // in3
    if p_contrls.map_arcade.but0[0] then
      marcade.in3 := marcade.in3 and $FE
    else
      marcade.in3 := marcade.in3 or 1;
    if p_contrls.map_arcade.but1[0] then
      marcade.in3 := marcade.in3 and $FD
    else
      marcade.in3 := marcade.in3 or 2;
    if p_contrls.map_arcade.but0[1] then
      marcade.in3 := marcade.in3 and $F7
    else
      marcade.in3 := marcade.in3 or 8;
    if p_contrls.map_arcade.but1[1] then
      marcade.in3 := marcade.in3 and $EF
    else
      marcade.in3 := marcade.in3 or $10;
  end;
end;

procedure events_athena;
begin
  if event.arcade then
  begin
    // in0
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // in1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    // in2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := marcade.in2 and $FD
    else
      marcade.in2 := marcade.in2 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := marcade.in2 and $FB
    else
      marcade.in2 := marcade.in2 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := marcade.in2 and $F7
    else
      marcade.in2 := marcade.in2 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := marcade.in2 and $EF
    else
      marcade.in2 := marcade.in2 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := marcade.in2 and $DF
    else
      marcade.in2 := marcade.in2 or $20;
  end;
end;

procedure events_tnk3;
begin
  if event.arcade then
  begin
    // in0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or $1;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or $8;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    // in1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but2[0] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 + $10;
        if (marcade.in1 and $F0) = $C0 then
          marcade.in1 := marcade.in1 and $F;
        rot_cont := $0;
      end;
    end;
    if p_contrls.map_arcade.but3[0] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 - $10;
        if (marcade.in1 and $F0) = $F0 then
          marcade.in1 := $B0 or (marcade.in1 and $F);
        rot_cont := $0;
      end;
    end;
    // in2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := marcade.in2 and $FD
    else
      marcade.in2 := marcade.in2 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := marcade.in2 and $FB
    else
      marcade.in2 := marcade.in2 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := marcade.in2 and $F7
    else
      marcade.in2 := marcade.in2 or 8;
    if p_contrls.map_arcade.but2[1] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 + $10;
        if (marcade.in1 and $F0) = $C0 then
          marcade.in1 := marcade.in1 and $F;
        rot_cont := $0;
      end;
    end;
    if p_contrls.map_arcade.but3[1] then
    begin
      rot_cont := rot_cont + 1;
      if rot_cont = $F then
      begin
        marcade.in1 := marcade.in1 - $10;
        if (marcade.in1 and $F0) = $F0 then
          marcade.in1 := $B0 or (marcade.in1 and $F);
        rot_cont := $0;
      end;
    end;
    // in3
    if p_contrls.map_arcade.but0[0] then
      marcade.in3 := marcade.in3 and $FE
    else
      marcade.in3 := marcade.in3 or 1;
    if p_contrls.map_arcade.but1[0] then
      marcade.in3 := marcade.in3 and $FD
    else
      marcade.in3 := marcade.in3 or 2;
    if p_contrls.map_arcade.but0[1] then
      marcade.in3 := marcade.in3 and $F7
    else
      marcade.in3 := marcade.in3 or 8;
    if p_contrls.map_arcade.but1[1] then
      marcade.in3 := marcade.in3 and $EF
    else
      marcade.in3 := marcade.in3 or $10;
  end;
end;

procedure events_aso;
begin
  if event.arcade then
  begin
    // in0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or $1;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or $8;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    // in1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    // in2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := marcade.in2 and $FD
    else
      marcade.in2 := marcade.in2 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := marcade.in2 and $FB
    else
      marcade.in2 := marcade.in2 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := marcade.in2 and $F7
    else
      marcade.in2 := marcade.in2 or 8;
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := marcade.in2 and $EF
    else
      marcade.in2 := marcade.in2 or $10;
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := marcade.in2 and $DF
    else
      marcade.in2 := marcade.in2 or $20;
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := marcade.in2 and $BF
    else
      marcade.in2 := marcade.in2 or $40;
  end;
end;

procedure snk_loop;
var
  frame_m, frame_sub, frame_snd: single;
  h, f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_sub := z80_1.tframes;
  frame_snd := z80_2.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 223 do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // Main
          z80_0.run(frame_m);
          frame_m := frame_m + z80_0.tframes - z80_0.contador;
          // Sub
          z80_1.run(frame_sub);
          frame_sub := frame_sub + z80_1.tframes - z80_1.contador;
          // snd
          z80_2.run(frame_snd);
          frame_snd := frame_snd + z80_2.tframes - z80_2.contador;
        end;
        if f = 0 then
        begin
          z80_0.change_irq(HOLD_LINE);
          z80_1.change_irq(HOLD_LINE);
          update_video_snk;
        end;
      end;
      update_events_snk;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function hardflags_check(num: byte): byte;
var
  x, y, dx, dy: word;
  ret: byte;
  sr: array [0 .. 3] of byte;
begin
  copymemory(@sr, @memory[$E800 + 4 * num], 4);
  x := sr[2] + ((sr[3] and $80) shl 1);
  y := sr[0] + ((sr[3] and $10) shl 4);
  dx := (x - hf_posx) and $1FF;
  dy := (y - hf_posy) and $1FF;
  if ((dx > $20) and (dx <= $1E0) and (dy > $20) and (dy <= $1E0)) then
    ret := 0
  else
    ret := 1;
  hardflags_check := ret;
end;

function hardflags_check8(num: byte): byte;
begin
  hardflags_check8 := (hardflags_check(num + 0) shl 0) or (hardflags_check(num + 1) shl 1) or (hardflags_check(num + 2) shl 2) or (hardflags_check(num + 3) shl 3) or (hardflags_check(num + 4) shl 4)
    or (hardflags_check(num + 5) shl 5) or (hardflags_check(num + 6) shl 6) or (hardflags_check(num + 7) shl 7);
end;

function ikari_main_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF, $E000 .. $F7FF:
      ikari_main_getbyte := memory[direccion];
    $C000:
      ikari_main_getbyte := marcade.in0 or ((sound_status and 4) shr 2);
    $C100:
      ikari_main_getbyte := marcade.in1;
    $C200:
      ikari_main_getbyte := marcade.in2;
    $C300:
      ikari_main_getbyte := marcade.in3;
    $C500:
      ikari_main_getbyte := marcade.dswa + (marcade.dswc and $4);
    $C600:
      ikari_main_getbyte := marcade.dswb + (marcade.dswc and $30);
    $C700:
      begin // snk_cpuB_nmi_trigger_r
        z80_1.change_nmi(ASSERT_LINE);
        ikari_main_getbyte := $FF;
      end;
    $CE00:
      ikari_main_getbyte := hardflags_check8(0 * 8); // hardflags1_r
    $CE20:
      ikari_main_getbyte := hardflags_check8(1 * 8); // hardflags2_r
    $CE40:
      ikari_main_getbyte := hardflags_check8(2 * 8); // hardflags3_r
    $CE60:
      ikari_main_getbyte := hardflags_check8(3 * 8); // hardflags4_r
    $CE80:
      ikari_main_getbyte := hardflags_check8(4 * 8); // hardflags5_r
    $CEA0:
      ikari_main_getbyte := hardflags_check8(5 * 8); // hardflags6_r
    $CEE0:
      ikari_main_getbyte := (hardflags_check(6 * 8 + 0) shl 0) or (hardflags_check(6 * 8 + 1) shl 1) or (hardflags_check(6 * 8 + 0) shl 4) or (hardflags_check(6 * 8 + 1) shl 5); // hardflags7_r
    $D000 .. $DFFF:
      ikari_main_getbyte := bg_ram[direccion and $7FF];
    $F800 .. $FFFF:
      ikari_main_getbyte := txt_ram[direccion and $7FF];
  end;
end;

procedure ikari_main_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C400:
      begin // soundlatch_w
        sound_latch := valor;
        sound_status := sound_status or 8 or 4;
        timers.enabled(timer_sound, true);
      end;
    $C700:
      z80_0.change_nmi(CLEAR_LINE); // cpa_nmi_ack
    $C800:
      scroll_y := (scroll_y and $100) or valor; // snk_bg_scrolly_w
    $C880:
      scroll_x := (scroll_x and $100) or valor; // snk_bg_scrollx_w
    $C900:
      begin // ikari_bg_scroll_msb_w
        scroll_x := (scroll_x and $FF) or ((valor and $02) shl 7);
        scroll_y := (scroll_y and $FF) or ((valor and $01) shl 8);
      end;
    $C980:
      begin // ikari_unknown_video_w
        txt_offset := (valor and $10) shl 4;
        fillchar(gfx[0].buffer, $400, 1);
      end;
    $CA00:
      sp16_scroll_y := (sp16_scroll_y and $100) or valor; // snk_sp16_scrolly_w
    $CA80:
      sp16_scroll_x := (sp16_scroll_x and $100) or valor; // snk_sp16_scrollx_w
    $CB00:
      sp32_scroll_y := (sp32_scroll_y and $100) or valor; // snk_sp32_scrolly_w
    $CB80:
      sp32_scroll_x := (sp32_scroll_x and $100) or valor; // snk_sp32_scrollx_w
    $CC00:
      hf_posy := (hf_posy and $100) or valor; // hardflags_scrolly_w
    $CC80:
      hf_posx := (hf_posx and $100) or valor; // hardflags_scrollx_w
    $CD00:
      begin // ikari_sp_scroll_msb_w
        sp32_scroll_x := (sp32_scroll_x and $FF) or ((valor and $20) shl 3);
        sp16_scroll_x := (sp16_scroll_x and $FF) or ((valor and $10) shl 4);
        sp32_scroll_y := (sp32_scroll_y and $FF) or ((valor and $08) shl 5);
        sp16_scroll_y := (sp16_scroll_y and $FF) or ((valor and $04) shl 6);
      end;
    $CD80:
      begin // hardflags_scroll_msb_w
        hf_posx := (hf_posx and $FF) or ((valor and $80) shl 1);
        hf_posy := (hf_posy and $FF) or ((valor and $40) shl 2);
      end;
    $D000 .. $DFFF:
      if bg_ram[direccion and $7FF] <> valor then
      begin // bg
        bg_ram[direccion and $7FF] := valor;
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $E000 .. $F7FF:
      memory[direccion] := valor;
    $F800 .. $FFFF:
      if txt_ram[direccion and $7FF] <> valor then
      begin
        txt_ram[direccion and $7FF] := valor;
        gfx[0].buffer[direccion and $7FF] := true;
      end;
  end;
end;

function ikari_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      ikari_sub_getbyte := mem_misc[direccion];
    $C000:
      begin // snk_cpuA_nmi_trigger_r
        z80_0.change_nmi(ASSERT_LINE);
        ikari_sub_getbyte := $FF;
      end;
    $CE00:
      ikari_sub_getbyte := hardflags_check8(0 * 8); // hardflags1_r
    $CE20:
      ikari_sub_getbyte := hardflags_check8(1 * 8); // hardflags2_r
    $CE40:
      ikari_sub_getbyte := hardflags_check8(2 * 8); // hardflags3_r
    $CE60:
      ikari_sub_getbyte := hardflags_check8(3 * 8); // hardflags4_r
    $CE80:
      ikari_sub_getbyte := hardflags_check8(4 * 8); // hardflags5_r
    $CEA0:
      ikari_sub_getbyte := hardflags_check8(5 * 8); // hardflags6_r
    $CEE0:
      ikari_sub_getbyte := (hardflags_check(6 * 8 + 0) shl 0) or (hardflags_check(6 * 8 + 1) shl 1) or (hardflags_check(6 * 8 + 0) shl 4) or (hardflags_check(6 * 8 + 1) shl 5); // hardflags7_r
    $D000 .. $DFFF:
      ikari_sub_getbyte := bg_ram[direccion and $7FF];
    $E000 .. $F7FF:
      ikari_sub_getbyte := memory[direccion];
    $F800 .. $FFFF:
      ikari_sub_getbyte := txt_ram[direccion and $7FF];
  end;
end;

procedure ikari_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000:
      z80_1.change_nmi(CLEAR_LINE); // snk_cpuB_nmi_ack_w
    $C980:
      begin // ikari_unknown_video_w
        txt_offset := (valor and $10) shl 4;
        fillchar(gfx[0].buffer, $400, 1);
      end;
    $CC00:
      hf_posy := (hf_posy and $100) or valor; // hardflags_scrolly_w
    $CC80:
      hf_posx := (hf_posx and $100) or valor; // hardflags_scrollx_w
    $CD80:
      begin // hardflags_scroll_msb_w
        hf_posx := (hf_posx and $FF) or ((valor and $80) shl 1);
        hf_posy := (hf_posy and $FF) or ((valor and $40) shl 2);
      end;
    $D000 .. $DFFF:
      if bg_ram[direccion and $7FF] <> valor then
      begin // bg
        bg_ram[direccion and $7FF] := valor;
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $E000 .. $F7FF:
      memory[direccion] := valor;
    $F800 .. $FFFF:
      if txt_ram[direccion and $7FF] <> valor then
      begin // tx
        txt_ram[direccion and $7FF] := valor;
        gfx[0].buffer[direccion and $7FF] := true;
      end;
  end;
end;

// TNK III
function tnk3_main_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      tnk3_main_getbyte := memory[direccion];
    $C000:
      tnk3_main_getbyte := marcade.in0 or ((sound_status and 4) shl 3);
    $C100:
      tnk3_main_getbyte := marcade.in1;
    $C200:
      tnk3_main_getbyte := marcade.in2;
    $C300:
      tnk3_main_getbyte := marcade.in3;
    $C500:
      tnk3_main_getbyte := marcade.dswa + (marcade.dswc and $C0);
    $C600:
      tnk3_main_getbyte := marcade.dswb + (marcade.dswc and $1);
    $C700:
      begin // snk_cpuB_nmi_trigger_r
        z80_1.change_nmi(ASSERT_LINE);
        tnk3_main_getbyte := $FF;
      end;
    $D000 .. $D7FF:
      tnk3_main_getbyte := sprite_ram[direccion and $7FF];
    $D800 .. $F7FF:
      tnk3_main_getbyte := bg_ram[direccion - $D800];
    $F800 .. $FFFF:
      tnk3_main_getbyte := txt_ram[direccion and $7FF];
  end;
end;

// ASO
function aso_main_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF, $D800 .. $DFFF:
      aso_main_getbyte := memory[direccion];
    $C000:
      aso_main_getbyte := marcade.in0 or ((sound_status and 4) shl 3);
    $C100:
      aso_main_getbyte := marcade.in1;
    $C200:
      aso_main_getbyte := marcade.in2;
    $C500:
      aso_main_getbyte := marcade.dswa + (marcade.dswc and $C0);
    $C600:
      aso_main_getbyte := marcade.dswb + (marcade.dswc and $1);
    $C700:
      begin // snk_cpuB_nmi_trigger_r
        z80_1.change_nmi(ASSERT_LINE);
        aso_main_getbyte := $FF;
      end;
    $E000 .. $E7FF:
      aso_main_getbyte := sprite_ram[direccion and $7FF];
    $E800 .. $F7FF:
      aso_main_getbyte := bg_ram[direccion - $E800];
    $F800 .. $FFFF:
      aso_main_getbyte := txt_ram[direccion and $7FF];
  end;
end;

procedure aso_main_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C400:
      begin // soundlatch_w
        sound_latch := valor;
        sound_status := sound_status or 8 or 4;
        timers.enabled(timer_sound, true);
      end;
    $C700:
      z80_0.change_nmi(CLEAR_LINE); // cpua_nmi_ack
    $C800:
      begin // aso_videoattrs_w
        main_screen.flip_main_screen := (valor and $20) <> 0;
        scroll_y := (scroll_y and $FF) or ((valor and $10) shl 4);
        sp16_scroll_y := (sp16_scroll_y and $FF) or ((valor and $08) shl 5);
        scroll_x := (scroll_x and $FF) or ((valor and $02) shl 7);
        sp16_scroll_x := (sp16_scroll_x and $FF) or ((valor and $01) shl 8);
      end;
    $C900:
      sp16_scroll_y := (sp16_scroll_y and $100) or valor; // snk_sp16_scrolly_w
    $CA00:
      sp16_scroll_x := (sp16_scroll_x and $100) or valor; // snk_sp16_scrollx_w
    $CB00:
      scroll_y := (scroll_y and $100) or valor; // snk_bg_scrolly_w
    $CC00:
      scroll_x := (scroll_x and $100) or valor; // snk_bg_scrollx_w
    $CF00:
      begin // aso_bg_bank_w
        if bg_pal_offset <> (((valor and $F) xor 8) shl 4) then
        begin
          bg_pal_offset := ((valor and $F) xor 8) shl 4;
          fillchar(gfx[1].buffer, $1000, 1);
        end;
        if (bg_offset <> ((valor and $30) shl 4)) then
        begin
          bg_offset := (valor and $30) shl 4;
          fillchar(gfx[1].buffer, $1000, 1);
        end;
      end;
    $D800 .. $DFFF:
      memory[direccion] := valor; // share
    $E000 .. $E7FF:
      sprite_ram[direccion and $7FF] := valor; // sprites
    $E800 .. $F7FF:
      if bg_ram[direccion - $E800] <> valor then
      begin // bg_ram
        bg_ram[direccion - $E800] := valor;
        gfx[1].buffer[(direccion - $E800) shr 1] := true;
      end;
    $F800 .. $FFFF:
      if txt_ram[direccion and $7FF] <> valor then
      begin // txt_ram
        txt_ram[direccion and $7FF] := valor;
        gfx[0].buffer[direccion and $7FF] := true;
      end;
  end;
end;

function aso_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      aso_sub_getbyte := mem_misc[direccion];
    $C000:
      begin // snk_cpuA_nmi_trigger_r
        z80_0.change_nmi(ASSERT_LINE);
        aso_sub_getbyte := $FF;
      end;
    $C800 .. $CFFF:
      aso_sub_getbyte := memory[direccion + $1000];
    $D000 .. $D7FF:
      aso_sub_getbyte := sprite_ram[direccion and $7FF]; // sprites
    $D800 .. $E7FF:
      aso_sub_getbyte := bg_ram[direccion - $D800]; // bg_ram
    $F800 .. $FFFF:
      aso_sub_getbyte := txt_ram[direccion and $7FF]; // tx_ram
  end;
end;

procedure aso_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000:
      z80_1.change_nmi(CLEAR_LINE); // snk_cpuB_nmi_ack_w
    $C800 .. $CFFF:
      memory[direccion + $1000] := valor;
    $D000 .. $D7FF:
      sprite_ram[direccion and $7FF] := valor; // sprites
    $D800 .. $E7FF:
      if bg_ram[direccion - $D800] <> valor then
      begin // bg_ram
        bg_ram[direccion - $D800] := valor;
        gfx[1].buffer[(direccion - $D800) shr 1] := true;
      end;
    $F800 .. $FFFF:
      if txt_ram[direccion and $7FF] <> valor then
      begin // tx_ram
        txt_ram[direccion and $7FF] := valor;
        gfx[0].buffer[direccion and $7FF] := true;
      end;
  end;
end;

// Athena
function athena_main_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      athena_main_getbyte := memory[direccion];
    $C000:
      athena_main_getbyte := marcade.in0 or ((sound_status and 4) shr 2);
    $C100:
      athena_main_getbyte := marcade.in1;
    $C200:
      athena_main_getbyte := marcade.in2;
    $C500:
      athena_main_getbyte := marcade.dswa + (marcade.dswc and $4);
    $C600:
      athena_main_getbyte := marcade.dswb + (marcade.dswc and $30);
    $C700:
      begin // snk_cpuB_nmi_trigger_r
        z80_1.change_nmi(ASSERT_LINE);
        athena_main_getbyte := $FF;
      end;
    $D000 .. $D7FF:
      athena_main_getbyte := sprite_ram[direccion and $7FF];
    $D800 .. $F7FF:
      athena_main_getbyte := bg_ram[direccion - $D800];
    $F800 .. $FFFF:
      athena_main_getbyte := txt_ram[direccion and $7FF];
  end;
end;

procedure athena_main_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C400:
      begin // soundlatch_w
        sound_latch := valor;
        sound_status := sound_status or 8 or 4;
        timers.enabled(timer_sound, true);
      end;
    $C700:
      z80_0.change_nmi(CLEAR_LINE); // cpua_nmi_ack
    $C800:
      begin // tnk3_videoattrs_w
        main_screen.flip_main_screen := (valor and $80) <> 0;
        if txt_offset <> ((valor and $40) shl 2) then
        begin
          txt_offset := (valor and $40) shl 2;
          fillchar(gfx[0].buffer, $400, 1);
        end;
        scroll_y := (scroll_y and $FF) or ((valor and $10) shl 4);
        sp16_scroll_y := (sp16_scroll_y and $FF) or ((valor and $08) shl 5);
        scroll_x := (scroll_x and $FF) or ((valor and $02) shl 7);
        sp16_scroll_x := (sp16_scroll_x and $FF) or ((valor and $01) shl 8);
      end;
    $C900:
      sp16_scroll_y := (sp16_scroll_y and $100) or valor; // snk_sp16_scrolly_w
    $CA00:
      sp16_scroll_x := (sp16_scroll_x and $100) or valor; // snk_sp16_scrollx_w
    $CB00:
      scroll_y := (scroll_y and $100) or valor; // snk_bg_scrolly_w
    $CC00:
      scroll_x := (scroll_x and $100) or valor; // snk_bg_scrollx_w
    $D000 .. $D7FF:
      sprite_ram[direccion and $7FF] := valor; // sprites
    $D800 .. $F7FF:
      if bg_ram[direccion - $D800] <> valor then
      begin // bg_ram
        bg_ram[direccion - $D800] := valor;
        gfx[1].buffer[(direccion - $D800) shr 1] := true;
      end;
    $F800 .. $FFFF:
      if txt_ram[direccion and $7FF] <> valor then
      begin // txt_ram
        txt_ram[direccion and $7FF] := valor;
        gfx[0].buffer[direccion and $7FF] := true;
      end;
  end;
end;

function athena_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      athena_sub_getbyte := mem_misc[direccion];
    $C000, $C700:
      begin // snk_cpuA_nmi_trigger_r
        z80_0.change_nmi(ASSERT_LINE);
        athena_sub_getbyte := $FF;
      end;
    $C800 .. $CFFF:
      athena_sub_getbyte := sprite_ram[direccion and $7FF]; // sprites
    $D000 .. $EFFF:
      athena_sub_getbyte := bg_ram[direccion - $D000]; // bg_ram
    $F000 .. $F7FF:
      athena_sub_getbyte := mem_misc[direccion];
    $F800 .. $FFFF:
      athena_sub_getbyte := txt_ram[direccion and $7FF]; // tx_ram
  end;
end;

procedure athena_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000, $C700:
      z80_1.change_nmi(CLEAR_LINE); // snk_cpuB_nmi_ack_w
    $C800 .. $CFFF:
      sprite_ram[direccion and $7FF] := valor; // sprites
    $D000 .. $EFFF:
      if bg_ram[direccion - $D000] <> valor then
      begin // bg_ram
        bg_ram[direccion - $D000] := valor;
        gfx[1].buffer[(direccion - $D000) shr 1] := true;
      end;
    $F000 .. $F7FF:
      mem_misc[direccion] := valor;
    $F800 .. $FFFF:
      if txt_ram[direccion and $7FF] <> valor then
      begin // tx_ram
        txt_ram[direccion and $7FF] := valor;
        gfx[0].buffer[direccion and $7FF] := true;
      end;
  end;
end;

// Sound
function ikari_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $CFFF:
      ikari_snd_getbyte := mem_snd[direccion];
    $E000:
      ikari_snd_getbyte := sound_latch;
    $E800:
      ikari_snd_getbyte := ym3812_0.status;
    $F000:
      ikari_snd_getbyte := ym3812_1.status;
    $F800:
      ikari_snd_getbyte := sound_status;
  end;
end;

procedure ikari_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $CFFF:
      mem_snd[direccion] := valor;
    $E800:
      ym3812_0.control(valor);
    $EC00:
      ym3812_0.write(valor);
    $F000:
      ym3812_1.control(valor);
    $F400:
      ym3812_1.write(valor);
    $F800:
      begin // snk_sound_status_w
        if (valor and $10) = 0 then
        begin
          sound_status := sound_status and $FE;
          timers.enabled(timer_sound, true);
        end;
        if (valor and $20) = 0 then
        begin
          sound_status := sound_status and $FD;
          timers.enabled(timer_sound, true);
        end;
        if (valor and $40) = 0 then
        begin
          sound_status := sound_status and $FB;
          timers.enabled(timer_sound, true);
        end;
        if (valor and $80) = 0 then
        begin
          sound_status := sound_status and $F7;
          timers.enabled(timer_sound, true);
        end;
      end;
  end;
end;

procedure snd_irq1(irqstate: byte);
begin
  if irqstate <> 0 then
  begin
    sound_status := sound_status or 1;
    timers.enabled(timer_sound, true);
  end;
end;

procedure snd_irq2(irqstate: byte);
begin
  if irqstate <> 0 then
  begin
    sound_status := sound_status or 2;
    timers.enabled(timer_sound, true);
  end;
end;

procedure ikari_sound_update;
begin
  ym3812_0.update;
  ym3812_1.update;
end;

procedure snk_snd_irq;
begin
  timers.enabled(timer_sound, false);
  if (sound_status and $B) <> 0 then
    z80_2.change_irq(ASSERT_LINE)
  else
    z80_2.change_irq(CLEAR_LINE);
end;

function tnk3_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $87FF:
      tnk3_snd_getbyte := mem_snd[direccion];
    $A000:
      tnk3_snd_getbyte := sound_latch;
    $C000:
      begin // tnk3_busy_clear_r
        sound_latch := 0;
        sound_status := sound_status and $FB;
        timers.enabled(timer_sound, true);
        tnk3_snd_getbyte := $FF;
      end;
    $E000:
      tnk3_snd_getbyte := ym3812_0.status;
    $E004:
      begin // tnk3_cmdirq_ack_r
        sound_status := sound_status and $F7;
        timers.enabled(timer_sound, true);
        tnk3_snd_getbyte := $FF;
      end;
    $E006:
      begin // tnk3_ymirq_ack_r
        sound_status := sound_status and $FE;
        timers.enabled(timer_sound, true);
        tnk3_snd_getbyte := $FF;
      end;
  end;
end;

procedure tnk3_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $E000:
      ym3812_0.control(valor);
    $E001:
      ym3812_0.write(valor);
  end;
end;

function aso_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $C7FF:
      aso_snd_getbyte := mem_snd[direccion];
    $D000:
      aso_snd_getbyte := sound_latch;
    $E000:
      begin // tnk3_busy_clear_r
        sound_latch := 0;
        sound_status := sound_status and $FB;
        timers.enabled(timer_sound, true);
        aso_snd_getbyte := $FF;
      end;
    $F000:
      aso_snd_getbyte := ym3812_0.status;
    $F004:
      begin // tnk3_cmdirq_ack_r
        sound_status := sound_status and $F7;
        timers.enabled(timer_sound, true);
        aso_snd_getbyte := $FF;
      end;
    $F006:
      begin // tnk3_ymirq_ack_r
        sound_status := sound_status and $FE;
        timers.enabled(timer_sound, true);
        aso_snd_getbyte := $FF;
      end;
  end;
end;

procedure aso_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $F000:
      ym3812_0.control(valor);
    $F001:
      ym3812_0.write(valor);
  end;
end;

procedure tnk3_sound_update;
begin
  ym3812_0.update;
end;

// Main
procedure reset_snk;
begin
  z80_0.reset;
  z80_1.reset;
  z80_2.reset;
  reset_audio;
  ym3812_0.reset;
  txt_offset := 0;
  bg_offset := 0;
  bg_pal_offset := 0;
  sound_latch := 0;
  sound_status := 0;
  hf_posx := 0;
  hf_posy := 0;
  rot_cont := 0;
  sp16_scroll_x := 0;
  sp16_scroll_y := 0;
  sp32_scroll_x := 0;
  sp32_scroll_y := 0;
  scroll_x := 0;
  scroll_y := 0;
  case main_vars.machine_type of
    241, 242:
      begin
        marcade.in0 := $FE;
        marcade.in1 := $BF;
        marcade.in2 := $BF;
        ym3812_1.reset;
      end;
    243:
      begin
        marcade.in0 := $DF;
        marcade.in1 := $F;
        marcade.in2 := $F;
      end;
    279:
      begin
        marcade.in0 := $DF;
        marcade.in1 := $FF;
        marcade.in2 := $FF;
      end;
  end;
  marcade.in3 := $FF;
end;

function start_snk: boolean;
const
  pc_y: array [0 .. 31] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32, 16 * 32 + 0 * 32,
    16 * 32 + 1 * 32, 16 * 32 + 2 * 32, 16 * 32 + 3 * 32, 16 * 32 + 4 * 32, 16 * 32 + 5 * 32, 16 * 32 + 6 * 32, 16 * 32 + 7 * 32, 16 * 32 + 8 * 32, 16 * 32 + 9 * 32, 16 * 32 + 10 * 32,
    16 * 32 + 11 * 32, 16 * 32 + 12 * 32, 16 * 32 + 13 * 32, 16 * 32 + 14 * 32, 16 * 32 + 15 * 32);
  pb_x: array [0 .. 15] of dword = (4 * 1, 4 * 0, 4 * 3, 4 * 2, 4 * 5, 4 * 4, 4 * 7, 4 * 6, 32 + 4 * 1, 32 + 4 * 0, 32 + 4 * 3, 32 + 4 * 2, 32 + 4 * 5, 32 + 4 * 4, 32 + 4 * 7, 32 + 4 * 6);
  pb_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
  ps_x: array [0 .. 31] of dword = (7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8, 23, 22, 21, 20, 19, 18, 17, 16, 31, 30, 29, 28, 27, 26, 25, 24);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);

var
  memory_temp: array [0 .. $2FFFF] of byte;
  colores: tpaleta;
  f: word;
  procedure tank3_pal;
  var
    bit0, bit1, bit2, bit3: byte;
    f: word;
  begin
    for f := 0 to $3FF do
    begin
      bit0 := (memory_temp[f + $800] shr 3) and $01;
      bit1 := (memory_temp[f] shr 1) and $01;
      bit2 := (memory_temp[f] shr 2) and $01;
      bit3 := (memory_temp[f] shr 3) and $01;
      colores[f].r := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
      bit0 := (memory_temp[f + $800] shr 2) and $01;
      bit1 := (memory_temp[f + $400] shr 2) and $01;
      bit2 := (memory_temp[f + $400] shr 3) and $01;
      bit3 := (memory_temp[f] shr 0) and $01;
      colores[f].g := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
      bit0 := (memory_temp[f + $800] shr 0) and $01;
      bit1 := (memory_temp[f + $800] shr 1) and $01;
      bit2 := (memory_temp[f + $400] shr 0) and $01;
      bit3 := (memory_temp[f + $400] shr 1) and $01;
      colores[f].b := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    end;
    set_pal(colores, $400);
  end;

begin
  machine_calls.general_loop := snk_loop;
  machine_calls.reset := reset_snk;
  start_snk := false;
  start_audio(false);
  screen_init(1, 288, 288, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 512, 511, 512, 512, 511);
  screen_init(3, 512, 512, false, true);
  case main_vars.machine_type of
    241:
      start_video(216, 288);
    242:
      start_video(288, 216);
    243, 279:
      begin
        main_screen.rot270_screen := true;
        start_video(288, 216);
      end;
  end;
  // Main CPU
  // IMPORTANTE!!! Para que las 3 CPUs funcionen correctamente, es necesario que se ejecuten pocas instrucciones
  // cada vez para que se sincronicen mejor, si no, por ejemplo nada mas arrancar la CPU 2 no carga la pila y la CPU 1
  // genera una NMI...
  z80_0 := cpu_z80.create(3350000, 224 * CPU_SYNC);
  z80_1 := cpu_z80.create(3350000, 224 * CPU_SYNC);
  z80_2 := cpu_z80.create(4000000, 224 * CPU_SYNC);
  timer_sound := timers.init(z80_2.numero_cpu, 120, snk_snd_irq, nil, false);
  case main_vars.machine_type of
    241:
      begin // Ikari Warriors
        update_video_snk := update_video_ikari;
        update_events_snk := events_ikari;
        z80_0.change_ram_calls(ikari_main_getbyte, ikari_main_putbyte);
        // Sub CPU
        z80_1.change_ram_calls(ikari_sub_getbyte, ikari_sub_putbyte);
        // Sound Z80
        z80_2.change_ram_calls(ikari_snd_getbyte, ikari_snd_putbyte);
        z80_2.init_sound(ikari_sound_update);
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 2);
        ym3812_0.change_irq_calls(snd_irq1);
        ym3812_1 := ym3812_chip.create(YM3526_FM, 4000000, 2);
        ym3812_1.change_irq_calls(snd_irq2);
        // cargar roms
        if not(roms_load(@memory, ikari_main)) then
          exit;
        if not(roms_load(@mem_misc, ikari_sub)) then
          exit;
        if not(roms_load(@mem_snd, ikari_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, ikari_gfx1)) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[15] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(0, 0, @memory_temp, @pb_x, @pc_y, false, true);
        // convertir bg
        if not(roms_load(@memory_temp, ikari_gfx2)) then
          exit;
        init_gfx(1, 16, 16, $400);
        gfx_set_desc_data(4, 0, 64 * 16, 0, 1, 2, 3);
        convert_gfx(1, 0, @memory_temp, @pb_x, @pb_y, false, true);
        // sprite 16
        if not(roms_load(@memory_temp, ikari_sprite16)) then
          exit;
        init_gfx(2, 16, 16, $400);
        gfx[2].trans[7] := true;
        gfx[2].shadow[6] := true;
        gfx_set_desc_data(3, 0, 16 * 16, 2 * 1024 * 256, 1 * 1024 * 256, 0 * 1024 * 256);
        convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
        // sprite 32
        if not(roms_load(@memory_temp, ikari_sprite32)) then
          exit;
        init_gfx(3, 32, 32, $200);
        gfx[3].trans[7] := true;
        gfx[3].shadow[6] := true;
        gfx_set_desc_data(3, 0, 16 * 32 * 2, 2 * 2048 * 256, 1 * 2048 * 256, 0 * 2048 * 256);
        convert_gfx(3, 0, @memory_temp, @ps_x, @pc_y, false, true);
        // pal
        if not(roms_load(@memory_temp, ikari_proms)) then
          exit;
        for f := 0 to $3FF do
        begin
          colores[f].r := pal4bit(memory_temp[f]);
          colores[f].g := pal4bit(memory_temp[f + $400]);
          colores[f].b := pal4bit(memory_temp[f + $800]);
        end;
        set_pal(colores, $400);
        marcade.dswa := $3B;
        marcade.dswb := $4B;
        marcade.dswc := $34;
        marcade.dswa_val := @ikari_dip_a;
        marcade.dswb_val := @ikari_dip_b;
        marcade.dswc_val := @ikari_dip_c;
      end;
    242:
      begin // Athena
        update_video_snk := update_video_tnk3;
        tnk3_draw_sprites := athena_sprites;
        update_events_snk := events_athena;
        z80_0.change_ram_calls(athena_main_getbyte, athena_main_putbyte);
        // Sub CPU
        z80_1.change_ram_calls(athena_sub_getbyte, athena_sub_putbyte);
        // Sound Z80
        z80_2.change_ram_calls(ikari_snd_getbyte, ikari_snd_putbyte);
        z80_2.init_sound(ikari_sound_update);
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 2);
        ym3812_0.change_irq_calls(snd_irq1);
        ym3812_1 := ym3812_chip.create(YM3526_FM, 4000000, 2);
        ym3812_1.change_irq_calls(snd_irq2);
        // cargar roms
        if not(roms_load(@memory, athena_main)) then
          exit;
        if not(roms_load(@mem_misc, athena_sub)) then
          exit;
        if not(roms_load(@mem_snd, athena_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, athena_gfx1)) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[15] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(0, 0, @memory_temp, @pb_x, @pc_y, false, false);
        // convertir bg
        if not(roms_load(@memory_temp, athena_gfx2)) then
          exit;
        init_gfx(1, 8, 8, $400);
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(1, 0, @memory_temp, @pb_x, @pc_y, false, false);
        // sprite 16
        if not(roms_load(@memory_temp, athena_sprite16)) then
          exit;
        init_gfx(2, 16, 16, $400);
        gfx[2].trans[7] := true;
        gfx[2].shadow[6] := true;
        gfx_set_desc_data(3, 0, 16 * 16, 2 * 1024 * 256, 1 * 1024 * 256, 0 * 1024 * 256);
        convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // pal
        if not(roms_load(@memory_temp, athena_proms)) then
          exit;
        tank3_pal;
        marcade.dswa := $39;
        marcade.dswb := $CB;
        marcade.dswc := $34;
        marcade.dswa_val := @athena_dip_a;
        marcade.dswb_val := @athena_dip_b;
        marcade.dswc_val := @athena_dip_c;
      end;
    243:
      begin // TNK III
        update_video_snk := update_video_tnk3;
        tnk3_draw_sprites := tnk3_sprites;
        update_events_snk := events_tnk3;
        z80_0.change_ram_calls(tnk3_main_getbyte, athena_main_putbyte);
        // Sub CPU
        z80_1.change_ram_calls(athena_sub_getbyte, athena_sub_putbyte);
        // Sound Z80
        z80_2.change_ram_calls(tnk3_snd_getbyte, tnk3_snd_putbyte);
        z80_2.init_sound(tnk3_sound_update);
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 2);
        ym3812_0.change_irq_calls(snd_irq1);
        // cargar roms
        if not(roms_load(@memory, tnk3_main)) then
          exit;
        if not(roms_load(@mem_misc, tnk3_sub)) then
          exit;
        if not(roms_load(@mem_snd, tnk3_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, tnk3_gfx1)) then
          exit;
        copymemory(@memory_temp[$2000], @memory_temp, $2000);
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[15] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(0, 0, @memory_temp, @pb_x, @pc_y, false, false);
        // convertir bg
        if not(roms_load(@memory_temp, tnk3_gfx2)) then
          exit;
        init_gfx(1, 8, 8, $400);
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(1, 0, @memory_temp, @pb_x, @pc_y, false, false);
        // sprite 16
        if not(roms_load(@memory_temp, tnk3_sprite16)) then
          exit;
        init_gfx(2, 16, 16, $200);
        gfx[2].trans[7] := true;
        gfx[2].shadow[6] := true;
        gfx_set_desc_data(3, 0, 16 * 16, 2 * 512 * 256, 1 * 512 * 256, 0 * 512 * 256);
        convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // pal
        if not(roms_load(@memory_temp, tnk3_proms)) then
          exit;
        tank3_pal;
        marcade.dswa := $3D;
        marcade.dswb := $76;
        marcade.dswc := $C1;
        marcade.dswa_val := @tnk3_dip_a;
        marcade.dswb_val := @tnk3_dip_b;
        marcade.dswc_val := @tnk3_dip_c;
      end;
    279:
      begin // ASO
        update_video_snk := update_video_aso;
        update_events_snk := events_aso;
        z80_0.change_ram_calls(aso_main_getbyte, aso_main_putbyte);
        // Sub CPU
        z80_1.change_ram_calls(aso_sub_getbyte, aso_sub_putbyte);
        // Sound Z80
        z80_2.change_ram_calls(aso_snd_getbyte, aso_snd_putbyte);
        z80_2.init_sound(tnk3_sound_update);
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 2);
        ym3812_0.change_irq_calls(snd_irq1);
        // cargar roms
        if not(roms_load(@memory, aso_main)) then
          exit;
        if not(roms_load(@mem_misc, aso_sub)) then
          exit;
        if not(roms_load(@mem_snd, aso_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, aso_gfx1)) then
          exit;
        copymemory(@memory_temp[$2000], @memory_temp, $2000);
        init_gfx(0, 8, 8, $100);
        gfx[0].trans[15] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(0, 0, @memory_temp, @pb_x, @pc_y, false, false);
        // convertir bg
        if not(roms_load(@memory_temp, aso_gfx2)) then
          exit;
        init_gfx(1, 8, 8, $400);
        gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
        convert_gfx(1, 0, @memory_temp, @pb_x, @pc_y, false, false);
        // sprites
        if not(roms_load(@memory_temp, aso_sprite16)) then
          exit;
        // Los recoloco...
        copymemory(@memory_temp[0], @memory_temp[$8000], $4000);
        copymemory(@memory_temp[$8000], @memory_temp[$10000], $4000);
        copymemory(@memory_temp[$10000], @memory_temp[$18000], $4000);
        init_gfx(2, 16, 16, $400);
        gfx[2].trans[7] := true;
        gfx[2].shadow[6] := true;
        gfx_set_desc_data(3, 0, 16 * 16, 2 * 1024 * 256, 1 * 1024 * 256, 0 * 1024 * 256);
        convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // pal
        if not(roms_load(@memory_temp, aso_proms)) then
          exit;
        tank3_pal;
        marcade.dswa := $3C;
        marcade.dswb := $F6;
        marcade.dswc := $C1;
        marcade.dswa_val := @aso_dip_a;
        marcade.dswb_val := @aso_dip_b;
        marcade.dswc_val := @aso_dip_c;
      end;
  end;
  // final
  reset_snk;
  start_snk := true;
end;

end.
