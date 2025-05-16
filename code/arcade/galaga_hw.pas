unit galaga_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  namco_snd,
  controls_engine,
  gfx_engine,
  namcoio_06xx_5xxx,
  rom_engine,
  pal_engine,
  sound_engine,
  galaga_stars_const,
  samples,
  misc_functions;

function start_galagahw: boolean;

implementation

const
  // Galaga
  galaga_rom: array [0 .. 3] of tipo_roms = ((n: 'gg1_1b.3p'; l: $1000; p: 0; crc: $AB036C9F), (n: 'gg1_2b.3m'; l: $1000; p: $1000; crc: $D9232240), (n: 'gg1_3.2m'; l: $1000; p: $2000; crc: $753CE503), (n: 'gg1_4b.2l'; l: $1000; p: $3000; crc: $499FCC76));
  galaga_sub: tipo_roms = (n: 'gg1_5b.3f'; l: $1000; p: 0; crc: $BB5CAAE3);
  galaga_sub2: tipo_roms = (n: 'gg1_7b.2c'; l: $1000; p: 0; crc: $D016686B);
  galaga_prom: array [0 .. 2] of tipo_roms = ((n: 'prom-5.5n'; l: $20; p: 0; crc: $54603C6B), (n: 'prom-4.2n'; l: $100; p: $20; crc: $59B6EDAB), (n: 'prom-3.1c'; l: $100; p: $120; crc: $4A04BB6B));
  galaga_char: tipo_roms = (n: 'gg1_9.4l'; l: $1000; p: 0; crc: $58B2F47C);
  galaga_sound: tipo_roms = (n: 'prom-1.1d'; l: $100; p: 0; crc: $7A2815B4);
  galaga_sprites: array [0 .. 1] of tipo_roms = ((n: 'gg1_11.4d'; l: $1000; p: 0; crc: $AD447C80), (n: 'gg1_10.4f'; l: $1000; p: $1000; crc: $DD6F1AFC));
  galaga_samples: array [0 .. 1] of tipo_nombre_samples = ((nombre: 'bang.wav'), (nombre: 'init.wav'));
  galaga_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Difficulty'; number: 4; val4: (3, 0, 1, 2); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: 8; name: 'Demo Sounds'; number: 2; val2: (8, 0); name2: ('Off', 'On')), (mask: $10; name: 'Freeze'; number: 2;
    val2: ($10, 0); name2: ('Off', 'On')), (mask: $20; name: 'Rack test'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: ($80, 0); name2: ('Upright', 'Cocktail')), ());
  galaga_dip_b: array [0 .. 3] of def_dip2 = ((mask: 7; name: 'Coinage'; number: 8; val8: (4, 2, 6, 7, 1, 3, 5, 0); name8: ('4C 1C', '3C 1C', '2C 1C', '1C 1C', '2C 3C', '1C 2C', '1C 3C', 'Free Play')), (mask: $38; name: 'Bonus Life'; number: 8;
    val8: ($20, $18, $10, $30, $38, 8, $28, 0); name8: ('20K 60K 60K+', '20K 60K', '20K 70K 70K+', '20K 80K 80K+', '20K 80K', '30K 100K 100K+', '30K 120K 120K+', 'None')), (mask: $C0; name: 'Lives'; number: 4; val4: (0, $80, $40, $C0); name4: ('2', '3', '4', '5')), ());
  // Dig Dug
  digdug_rom: array [0 .. 3] of tipo_roms = ((n: 'dd1a.1'; l: $1000; p: 0; crc: $A80EC984), (n: 'dd1a.2'; l: $1000; p: $1000; crc: $559F00BD), (n: 'dd1a.3'; l: $1000; p: $2000; crc: $8CBC6FE1), (n: 'dd1a.4'; l: $1000; p: $3000; crc: $D066F830));
  digdug_sub: array [0 .. 1] of tipo_roms = ((n: 'dd1a.5'; l: $1000; p: 0; crc: $6687933B), (n: 'dd1a.6'; l: $1000; p: $1000; crc: $843D857F));
  digdug_sub2: tipo_roms = (n: 'dd1.7'; l: $1000; p: 0; crc: $A41BCE72);
  digdug_prom: array [0 .. 2] of tipo_roms = ((n: '136007.113'; l: $20; p: 0; crc: $4CB9DA99), (n: '136007.111'; l: $100; p: $20; crc: $00C7C419), (n: '136007.112'; l: $100; p: $120; crc: $E9B3E08E));
  digdug_sound: tipo_roms = (n: '136007.110'; l: $100; p: 0; crc: $7A2815B4);
  digdug_chars: tipo_roms = (n: 'dd1.9'; l: $800; p: 0; crc: $F14A6FE1);
  digdug_sprites: array [0 .. 3] of tipo_roms = ((n: 'dd1.15'; l: $1000; p: 0; crc: $E22957C8), (n: 'dd1.14'; l: $1000; p: $1000; crc: $2829EC99), (n: 'dd1.13'; l: $1000; p: $2000; crc: $458499E9), (n: 'dd1.12'; l: $1000; p: $3000; crc: $C58252A0));
  digdug_chars2: tipo_roms = (n: 'dd1.11'; l: $1000; p: 0; crc: $7B383983);
  digdug_background: tipo_roms = (n: 'dd1.10b'; l: $1000; p: 0; crc: $2CF399C2);
  digdug_dip_a: array [0 .. 3] of def_dip2 = ((mask: 7; name: 'Coin B'; number: 8; val8: (7, 3, 1, 5, 6, 2, 4, 0); name8: ('3C 1C', '2C 1C', '1C 1C', '2C 3C', '1C 2C', '1C 3C', '1C 6C', '1C 7C')), (mask: $38; name: 'Bonus Life'; number: 8;
    val8: ($20, $10, $30, 8, $28, $18, $38, 0); name8: ('10K 40K 40K+', '10K 50K 50K+', '20K 60K 60K+', '20K 70K 70K+', '10K 40K', '20K 60K', '10K', 'None')), (mask: $C0; name: 'Lives'; number: 4; val4: (0, $40, $80, $C0); name4: ('1', '2', '3', '5')), ());
  digdug_dip_b: array [0 .. 6] of def_dip2 = ((mask: 3; name: 'Difficulty'; number: 4; val4: (0, 2, 1, 3); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: 4; name: 'Cabinet'; number: 2; val2: (4, 0); name2: ('Upright', 'Cocktail')), (mask: 8; name: 'Allow Continue';
    number: 2; val2: (8, 0); name2: ('No', 'Yes')), (mask: $10; name: 'Demo Sounds'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), (mask: $20; name: 'Freeze'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $C0; name: 'Coin A'; number: 4; val4: ($40, 0, $C0, $80);
    name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), ());
  // Xevious
  xevious_rom: array [0 .. 3] of tipo_roms = ((n: 'xvi_1.3p'; l: $1000; p: 0; crc: $09964DDA), (n: 'xvi_2.3m'; l: $1000; p: $1000; crc: $60ECCE84), (n: 'xvi_3.2m'; l: $1000; p: $2000; crc: $79754B7D), (n: 'xvi_4.2l'; l: $1000; p: $3000; crc: $C7D4BBF0));
  xevious_sub: array [0 .. 1] of tipo_roms = ((n: 'xvi_5.3f'; l: $1000; p: $0; crc: $C85B703F), (n: 'xvi_6.3j'; l: $1000; p: $1000; crc: $E18CDAAD));
  xevious_sub2: tipo_roms = (n: 'xvi_7.2c'; l: $1000; p: 0; crc: $DD35CF1C);
  xevious_prom: array [0 .. 6] of tipo_roms = ((n: 'xvi-8.6a'; l: $100; p: 0; crc: $5CC2727F), (n: 'xvi-9.6d'; l: $100; p: $100; crc: $5C8796CC), (n: 'xvi-10.6e'; l: $100; p: $200; crc: $3CB60975), (n: 'xvi-7.4h'; l: $200; p: $300; crc: $22D98032), (n: 'xvi-6.4f'; l: $200;
    p: $500; crc: $3A7599F0), (n: 'xvi-4.3l'; l: $200; p: $700; crc: $FD8B9D91), (n: 'xvi-5.3m'; l: $200; p: $900; crc: $BF906D82));
  xevious_sound: tipo_roms = (n: 'xvi-2.7n'; l: $100; p: 0; crc: $550F06BC);
  xevious_char: tipo_roms = (n: 'xvi_12.3b'; l: $1000; p: 0; crc: $088C8B26);
  xevious_sprites: array [0 .. 3] of tipo_roms = ((n: 'xvi_15.4m'; l: $2000; p: 0; crc: $DC2C0ECB), (n: 'xvi_17.4p'; l: $2000; p: $2000; crc: $DFB587CE), (n: 'xvi_16.4n'; l: $1000; p: $4000; crc: $605CA889), (n: 'xvi_18.4r'; l: $2000; p: $5000; crc: $02417D19));
  xevious_bg: array [0 .. 1] of tipo_roms = ((n: 'xvi_13.3c'; l: $1000; p: $0; crc: $DE60BA25), (n: 'xvi_14.3d'; l: $1000; p: $1000; crc: $535CDBBC));
  xevious_bg_tiles: array [0 .. 2] of tipo_roms = ((n: 'xvi_9.2a'; l: $1000; p: 0; crc: $57ED9879), (n: 'xvi_10.2b'; l: $2000; p: $1000; crc: $AE3BA9E5), (n: 'xvi_11.2c'; l: $1000; p: $3000; crc: $31E244DD));
  xevious_samples: array [0 .. 1] of tipo_nombre_samples = ((nombre: 'explo2.wav'), (nombre: 'explo1.wav'));
  xevious_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (1, 3, 0, 2); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $1C; name: 'Bonus Life'; number: 8; val8: ($18, $14, $10, $1C, $C, 8, 4, 0);
    name8: ('10K 40K 40K+', '10K 50K 50K+', '20K 50K 50K+', '20K 60K 60K+', '20K 70K 70+', '20K 80K 80K+', '20K 60K', 'None')), (mask: $60; name: 'Lives'; number: 4; val4: ($40, $20, $60, 0); name4: ('1', '2', '3', '5')), (mask: $80; name: 'Cabinet'; number: 2; val2: ($80, 0);
    name2: ('Upright', 'Cocktail')), ());
  xevious_dip_b: array [0 .. 4] of def_dip2 = ((mask: 2; name: 'Flags Award Bonus Life'; number: 2; val2: (0, 1); name2: ('No', 'Yes')), (mask: $C; name: 'Coin B'; number: 4; val4: (4, $C, 0, 8); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $60; name: 'Difficulty';
    number: 4; val4: ($40, $60, $20, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $80; name: 'Freeze'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  // Bosconian
  bosco_rom: array [0 .. 3] of tipo_roms = ((n: 'bos3_1.3n'; l: $1000; p: 0; crc: $96021267), (n: 'bos1_2.3m'; l: $1000; p: $1000; crc: $2D8F3EBE), (n: 'bos1_3.3l'; l: $1000; p: $2000; crc: $C80CCFA5), (n: 'bos1_4b.3k'; l: $1000; p: $3000; crc: $A3F7F4AB));
  bosco_sub: array [0 .. 1] of tipo_roms = ((n: 'bos1_5c.3j'; l: $1000; p: $0; crc: $A7C8E432), (n: 'bos3_6.3h'; l: $1000; p: $1000; crc: $4543CF82));
  bosco_sub2: tipo_roms = (n: 'bos1_7.3e'; l: $1000; p: 0; crc: $D45A4911);
  bosco_char: tipo_roms = (n: 'bos1_14.5d'; l: $1000; p: 0; crc: $A956D3C5);
  bosco_sprites: tipo_roms = (n: 'bos1_13.5e'; l: $1000; p: 0; crc: $E869219C);
  bosco_dots: tipo_roms = (n: 'bos1-4.2r'; l: $100; p: 0; crc: $9B69B543);
  bosco_prom: array [0 .. 3] of tipo_roms = ((n: 'bos1-6.6b'; l: $20; p: 0; crc: $D2B96FB0), (n: 'bos1-5.4m'; l: $100; p: $20; crc: $4E15D59C), (n: 'bos1-3.2d'; l: $20; p: $120; crc: $B88D5BA9), (n: 'bos1-7.7h'; l: $20; p: $140; crc: $87D61353));
  bosco_snd: tipo_roms = (n: 'bos1-1.1d'; l: $100; p: $0; crc: $DE2316C6);
  bosco_52xx: array [0 .. 2] of tipo_roms = ((n: 'bos1_9.5n'; l: $1000; p: $0; crc: $09ACC978), (n: 'bos1_10.5m'; l: $1000; p: $1000; crc: $E571E959), (n: 'bos1_11.5k'; l: $1000; p: $2000; crc: $17AC9511));
  bosco_samples: array [0 .. 2] of tipo_nombre_samples = ((nombre: 'bigbang.wav'), (nombre: 'midbang.wav'), (nombre: 'shot.wav'; restart: true; loop: false));
  bosco_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Difficulty'; number: 4; val4: (1, 3, 2, 0); name4: ('Easy', 'Medium', 'Hardest', 'Auto')), (mask: 4; name: 'Allow Continue'; number: 2; val2: (0, 4); name2: ('No', 'Yes')), (mask: 8; name: 'Demo Sounds'; number: 2;
    val2: (8, 0); name2: ('Off', 'On')), (mask: $10; name: 'Freeze'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: ($80, 0); name2: ('Upright', 'Cocktail')), ());
  bosco_dip_b: array [0 .. 3] of def_dip2 = ((mask: 7; name: 'Coinage'; number: 8; val8: (1, 2, 3, 7, 4, 6, 5, 0); name8: ('4C 1C', '3C 1C', '2C 1C', '1C 1C', '2C 3C', '1C 2C', '1C 3C', 'Free Play')), (mask: $38; name: 'Bonus Fighter'; number: 8;
    val8: ($30, $38, 8, $10, $18, $20, $28, 0); name8: ('15K 50K', '20K 70K', '10K 50K 50K+', '15K 50K 50K+', '15K 70K 70+', '20K 70K 70K+', '30K 100K 100K+', 'None')), (mask: $C0; name: 'Lives'; number: 4; val4: (0, $80, $40, $C0); name4: ('2', '3', '4', '5')), ());
  // Super Xevious
  sxevious_rom: array [0 .. 3] of tipo_roms = ((n: 'cpu_3p.rom'; l: $1000; p: 0; crc: $1C8D27D5), (n: 'cpu_3m.rom'; l: $1000; p: $1000; crc: $FD04E615), (n: 'xv3_3.2m'; l: $1000; p: $2000; crc: $294D5404), (n: 'xv3_4.2l'; l: $1000; p: $3000; crc: $6A44BF92));
  sxevious_sub: array [0 .. 1] of tipo_roms = ((n: 'xv3_5.3f'; l: $1000; p: $0; crc: $D4BD3D81), (n: 'xv3_6.3j'; l: $1000; p: $1000; crc: $AF06BE5F));
  sxevious_dip_b: array [0 .. 4] of def_dip2 = ((mask: 2; name: 'Flags Award Bonus Life'; number: 2; val2: (0, 1); name2: ('No', 'Yes')), (mask: $C; name: 'Coin B'; number: 4; val4: ($C, 8, 4, 0); name4: ('1C 1C', '1C 2C', '1C 3C', '1C 6C')), (mask: $60; name: 'Difficulty';
    number: 4; val4: ($40, $60, $20, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $80; name: 'Freeze'; number: 2; val2: (0, $80); name2: ('Off', 'On')), ());
  MAX_STARS = 252;

var
  main_irq, sub_irq, sub2_nmi: boolean;
  scrollx_bg, scrolly_bg: word;
  // Galaga
  galaga_starcontrol: array [0 .. 5] of byte;
  // Dig Dug
  digdug_bg: array [0 .. $FFF] of byte;
  custom_mod, bg_select, bg_color_bank, tx_color_mode: byte;
  bg_disable, bg_repaint: boolean;
  // Xevious
  xevious_tiles: array [0 .. $3FFF] of byte;
  xevious_bs: array [0 .. 1] of byte;
  scrollx_fg, scrolly_fg: word;

procedure update_video_galaga;
  procedure draw_sprites_galaga;
  var
    nchar, f, atrib, a, b, c, d: byte;
    color, x, y: word;
    flipx, flipy: boolean;
  begin
    for f := 0 to $3F do
    begin
      nchar := memory[$8B80 + (f * 2)] and $7F;
      color := (memory[$8B81 + (f * 2)] and $3F) shl 2;
      y := memory[$9381 + (f * 2)] - 40 + $100 * (memory[$9B81 + (f * 2)] and 3);
      x := memory[$9380 + (f * 2)] - 16 - 1; // sprites are buffered and delayed by one scanline
      atrib := memory[$9B80 + (f * 2)];
      flipx := (atrib and 2) <> 0;
      flipy := (atrib and 1) <> 0;
      case (atrib and $C) of
        0:
          begin // 16x16
            put_gfx_sprite_mask(nchar, color, flipx, flipy, 1, $F, $F);
            update_gfx_sprite(x, y, 2, 1);
          end;
        4:
          begin // 16x32
            a := 0 xor byte(flipy);
            b := 1 xor byte(flipy);
            put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, $F, $F, 0, 16);
            put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, $F, $F, 0, 0);
            actualiza_gfx_sprite_size(x, y, 2, 16, 32);
          end;
        8:
          begin // 32x16
            a := 0 xor (byte(flipx) shl 1);
            b := 2 xor (byte(flipx) shl 1);
            put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, $F, $F, 16, 0);
            put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, $F, $F, 0, 0);
            actualiza_gfx_sprite_size(x, y, 2, 32, 16);
          end;
        $C:
          begin // 32x32
            a := 0 xor byte(flipy) xor (byte(flipx) shl 1);
            b := 1 xor byte(flipy) xor (byte(flipx) shl 1);
            c := 2 xor byte(flipy) xor (byte(flipx) shl 1);
            d := 3 xor byte(flipy) xor (byte(flipx) shl 1);
            put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, $F, $F, 16, 0);
            put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, $F, $F, 16, 16);
            put_gfx_sprite_mask_diff(nchar + c, color, flipx, flipy, 1, $F, $F, 0, 0);
            put_gfx_sprite_mask_diff(nchar + d, color, flipx, flipy, 1, $F, $F, 0, 16);
            actualiza_gfx_sprite_size(x, y, 2, 32, 32);
          end;
      end;
    end;
  end;

  procedure update_stars;
  const
    speeds: array [0 .. 7] of integer = (-1, -2, -3, 0, 3, 2, 1, 0);
  var
    s0, s1, s2: byte;
  begin
    s0 := galaga_starcontrol[0] and 1;
    s1 := galaga_starcontrol[1] and 1;
    s2 := galaga_starcontrol[2] and 1;
    scrolly_bg := scrolly_bg + speeds[s0 + s1 * 2 + s2 * 4];
  end;

  procedure draw_stars;
  var
    star_cntr, set_a, set_b: byte;
    x, y, color: word;
  begin
    if (galaga_starcontrol[5] and 1) = 1 then
    begin
      // two sets of stars controlled by these bits
      set_a := galaga_starcontrol[3] and 1;
      set_b := (galaga_starcontrol[4] and 1) or 2;
      for star_cntr := 0 to (MAX_STARS - 1) do
      begin
        if ((set_a = star_seed_tab[star_cntr].set_) or (set_b = star_seed_tab[star_cntr].set_)) then
        begin
          y := (star_seed_tab[star_cntr].y + scrolly_bg) mod 256 + 16;
          x := (112 + star_seed_tab[star_cntr].x + scrollx_bg) mod 256;
          color := paleta[32 + star_seed_tab[star_cntr].col];
          putpixel(x + ADD_SPRITE, y + ADD_SPRITE, 1, @color, 2);
        end;
      end;
    end;
  end;

var
  color, nchar, pos: word;
  sx, sy, x, y: byte;
begin
  fill_full_screen(2, 100);
  draw_stars;
  draw_sprites_galaga;
  for x := 0 to 27 do
  begin
    for y := 0 to 35 do
    begin
      sx := 29 - x;
      sy := y - 2;
      if (sy and $20) <> 0 then
        pos := sx + ((sy and $1F) shl 5)
      else
        pos := sy + (sx shl 5);
      if gfx[0].buffer[pos] then
      begin
        color := (memory[$8400 + pos] and $3F) shl 2;
        nchar := memory[$8000 + pos];
        put_gfx_mask(x * 8, y * 8, nchar, color, 1, 0, $F, $F);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  update_region(0, 0, 224, 288, 1, 0, 0, 224, 288, 2);
  update_final_piece(0, 0, 224, 288, 2);
  update_stars;
end;

procedure events_galaga;
begin
  if event.arcade then
  begin
    // P1 & P2
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // System
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // Extra P1 & P2 (Xevious solo)
    if p_contrls.map_arcade.but1[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure galaga_loop;
var
  frame_m, frame_s1, frame_s2: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s1 := z80_2.tframes;
  frame_s2 := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sub CPU
        z80_2.run(frame_s1);
        frame_s1 := frame_s1 + z80_2.tframes - z80_2.contador;
        // Sub 2 CPU
        z80_1.run(frame_s2);
        frame_s2 := frame_s2 + z80_1.tframes - z80_1.contador;
        // run_namco_51xx;
        run_namco_54xx;
        case f of
          // 8:namco_51xx_vblank(ASSERT_LINE);
          63, 191:
            if sub2_nmi then
              z80_1.change_nmi(PULSE_LINE);
          223:
            begin
              if main_irq then
                z80_0.change_irq(ASSERT_LINE);
              if sub_irq then
                z80_2.change_irq(ASSERT_LINE);
              update_video_galaga;
              copymemory(@buffer_sprites, @memory[$FE00], $200);
            end;
        end;
      end;
      events_galaga;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure galaga_latch(dir, val: byte);
var
  bit: byte;
begin
  bit := val and 1;
  case dir of
    0:
      begin // IRQ1
        main_irq := bit <> 0;
        if not(main_irq) then
          z80_0.change_irq(CLEAR_LINE);
      end;
    1:
      begin // IRQ2
        sub_irq := bit <> 0;
        if not(sub_irq) then
          z80_2.change_irq(CLEAR_LINE);
      end;
    2:
      sub2_nmi := (bit = 0); // NMION
    3:
      if (bit <> 0) then
      begin // RESET
        z80_1.change_reset(CLEAR_LINE);
        z80_2.change_reset(CLEAR_LINE);
        // namco_51xx.mb88.change_reset(CLEAR_LINE);
        // namcoio_51xx_reset(false);
      end
      else
      begin
        z80_1.change_reset(ASSERT_LINE);
        z80_2.change_reset(ASSERT_LINE);
        // namco_51xx.mb88.change_reset(ASSERT_LINE);
      end;
    4:
      ; // n.c.
    5:
      custom_mod := (custom_mod and $FE) or (bit shl 0); // MOD 0
    6:
      custom_mod := (custom_mod and $FD) or (bit shl 1); // MOD 1
    7:
      custom_mod := (custom_mod and $FB) or (bit shl 2); // MOD 2
  end;
end;

function galaxian_dip(direccion: byte): byte;
var
  bit0, bit1: byte;
begin
  bit0 := (marcade.dswb shr direccion) and 1;
  bit1 := (marcade.dswa shr direccion) and 1;
  galaxian_dip := bit0 or (bit1 shl 1);
end;

function galaga_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $8BFF, $9000 .. $93FF, $9800 .. $9BFF:
      galaga_getbyte := memory[direccion];
    $6800 .. $6807:
      galaga_getbyte := galaxian_dip(direccion and 7);
    $7000 .. $70FF:
      galaga_getbyte := namco_06xx_data_r(direccion and $FF, 0);
    $7100:
      galaga_getbyte := namco_06xx_ctrl_r(0);
  end;
end;

procedure galaga_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $6800 .. $681F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $6820 .. $6827:
      galaga_latch(direccion and 7, valor);
    $7000 .. $70FF:
      namco_06xx_data_w(direccion and $FF, 0, valor);
    $7100:
      namco_06xx_ctrl_w(0, valor);
    $8000 .. $87FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $8800 .. $8BFF, $9000 .. $93FF, $9800 .. $9BFF:
      memory[direccion] := valor;
    $A000 .. $A005:
      galaga_starcontrol[direccion and 7] := valor;
    $A007:
      main_screen.flip_main_screen := (valor and 1) <> 0;
  end;
end;

function galaga_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      galaga_sub_getbyte := mem_snd[direccion];
    $4000 .. $FFFF:
      galaga_sub_getbyte := galaga_getbyte(direccion);
  end;
end;

function galaga_sub2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      galaga_sub2_getbyte := mem_misc[direccion];
    $4000 .. $FFFF:
      galaga_sub2_getbyte := galaga_getbyte(direccion);
  end;
end;

procedure galaga_sound_update;
begin
  samples_update;
  namco_snd_0.update;
end;

// DigDug
function namco_53xx_r_r(port: byte): byte;
begin
  case port of
    0:
      namco_53xx_r_r := marcade.dswa and $F;
    1:
      namco_53xx_r_r := marcade.dswa shr 4;
    2:
      namco_53xx_r_r := marcade.dswb and $F;
    3:
      namco_53xx_r_r := marcade.dswb shr 4;
  end;
end;

function namco_53xx_k_r: byte;
begin
  namco_53xx_k_r := custom_mod shl 1;
end;

procedure update_video_digdug;

  procedure draw_sprites_digdug;
  var
    nchar, f, atrib, a, b, c, d: byte;
    color, x: word;
    y: integer;
    flipx, flipy: boolean;
  begin
    for f := 0 to $3F do
    begin
      nchar := memory[$8B80 + (f * 2)];
      color := (memory[$8B81 + (f * 2)] and $3F) shl 2;
      y := memory[$9381 + (f * 2)] - 40 + 1;
      if y <= 0 then
        y := 256 + y;
      x := memory[$9380 + (f * 2)] - 16 - 1; // sprites are buffered and delayed by one scanline
      atrib := memory[$9B80 + (f * 2)];
      flipx := (atrib and 2) <> 0;
      flipy := (atrib and 1) <> 0;
      if (nchar and $80) = 0 then
      begin // 16x16
        put_gfx_sprite_mask(nchar, color, flipx, flipy, 1, $1F, $1F);
        update_gfx_sprite(x, y, 2, 1);
      end
      else
      begin // 32x32
        a := 0 xor byte(flipy) xor (byte(flipx) shl 1);
        b := 1 xor byte(flipy) xor (byte(flipx) shl 1);
        c := 2 xor byte(flipy) xor (byte(flipx) shl 1);
        d := 3 xor byte(flipy) xor (byte(flipx) shl 1);
        nchar := (nchar and $C0) or ((nchar and $3F) shl 2);
        put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, $1F, $1F, 16, 0);
        put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, $1F, $1F, 16, 16);
        put_gfx_sprite_mask_diff(nchar + c, color, flipx, flipy, 1, $1F, $1F, 0, 0);
        put_gfx_sprite_mask_diff(nchar + d, color, flipx, flipy, 1, $1F, $1F, 0, 16);
        actualiza_gfx_sprite_size(x, y, 2, 32, 32);
      end;
    end;
  end;

var
  color, nchar, pos: word;
  sx, sy, x, y: byte;
begin
  if bg_disable then
    fill_full_screen(3, $100);
  for x := 0 to 27 do
  begin
    for y := 0 to 35 do
    begin
      sx := 29 - x;
      sy := y - 2;
      if (sy and $20) <> 0 then
        pos := sx + ((sy and $1F) shl 5)
      else
        pos := sy + (sx shl 5);
      // Background
      if not(bg_disable) and bg_repaint then
      begin
        nchar := digdug_bg[pos or (bg_select shl 10)];
        color := (nchar shr 4);
        put_gfx(x * 8, y * 8, nchar, (color or bg_color_bank) shl 2, 3, 2);
      end;
      // Chars
      if gfx[0].buffer[pos] then
      begin
        nchar := memory[$8000 + pos];
        color := ((nchar shr 4) and $E) or ((nchar shr 3) and 2);
        put_gfx_trans(x * 8, y * 8, nchar and $7F, color shl 1, 1, 0);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  update_region(0, 0, 224, 288, 3, 0, 0, 224, 288, 2);
  update_region(0, 0, 224, 288, 1, 0, 0, 224, 288, 2);
  draw_sprites_digdug;
  update_final_piece(0, 0, 224, 288, 2);
  bg_repaint := false;
end;

procedure digdug_loop;
var
  frame_m, frame_s1, frame_s2: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s1 := z80_2.tframes;
  frame_s2 := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sub CPU
        z80_2.run(frame_s1);
        frame_s1 := frame_s1 + z80_2.tframes - z80_2.contador;
        // Sub 2 CPU
        z80_1.run(frame_s2);
        frame_s2 := frame_s2 + z80_1.tframes - z80_1.contador;
        // IO's
        run_namco_53xx;
        case f of
          63, 191:
            if sub2_nmi then
              z80_1.change_nmi(PULSE_LINE);
          223:
            begin
              if main_irq then
                z80_0.change_irq(ASSERT_LINE);
              if sub_irq then
                z80_2.change_irq(ASSERT_LINE);
              update_video_digdug;
            end;
        end;
      end;
      events_galaga;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Main CPU
procedure digdug_putbyte(direccion: word; valor: byte);
var
  mask, shift: byte;
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $6800 .. $681F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $6820 .. $6827:
      galaga_latch(direccion and 7, valor);
    $7000 .. $70FF:
      namco_06xx_data_w(direccion and $FF, 0, valor);
    $7100:
      namco_06xx_ctrl_w(0, valor);
    $8000 .. $83FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $8400 .. $8BFF, $9000 .. $93FF, $9800 .. $9BFF:
      memory[direccion] := valor;
    $A000 .. $A007:
      case (direccion and 7) of // port_w
        0, 1:
          begin // select background picture
            shift := direccion and 7;
            mask := 1 shl shift;
            if ((bg_select and mask) <> ((valor and 1) shl shift)) then
            begin
              bg_select := (bg_select and not(mask)) or ((valor and 1) shl shift);
              bg_repaint := true;
            end;
          end;
        2:
          if (tx_color_mode <> (valor and 1)) then
            tx_color_mode := valor and 1; // select alpha layer color mode
        3:
          if bg_disable <> ((valor and 1) <> 0) then
          begin // disable background
            bg_disable := (valor and 1) <> 0;
            if not(bg_disable) then
              bg_repaint := true;
          end;
        4, 5:
          begin // background color bank
            shift := direccion and 7;
            mask := 1 shl shift;
            if ((bg_color_bank and mask) <> ((valor and 1) shl shift)) then
            begin
              bg_color_bank := (bg_color_bank and not(mask)) or ((valor and 1) shl shift);
              bg_repaint := true;
            end;
          end;
        6:
          ; // n.c.
        7:
          main_screen.flip_main_screen := (valor and 1) <> 0; // FLIP
      end;
    $B800 .. $B840:
      memory[direccion] := valor; // eeprom
  end;
end;

function digdug_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $8BFF, $9000 .. $93FF, $9800 .. $9BFF:
      digdug_getbyte := memory[direccion];
    $7000 .. $70FF:
      digdug_getbyte := namco_06xx_data_r(direccion and $FF, 0);
    $7100:
      digdug_getbyte := namco_06xx_ctrl_r(0);
    $B800 .. $B83F:
      digdug_getbyte := memory[direccion]; // eeprom
  end;
end;

// Sub1 CPU
function digdug_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      digdug_sub_getbyte := mem_snd[direccion];
    $4000 .. $FFFF:
      digdug_sub_getbyte := digdug_getbyte(direccion);
  end;
end;

// Sub2 CPU
function digdug_sub2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      digdug_sub2_getbyte := mem_misc[direccion];
    $4000 .. $FFFF:
      digdug_sub2_getbyte := digdug_getbyte(direccion);
  end;
end;

procedure digdug_sound_update;
begin
  namco_snd_0.update;
end;

// Xevious
procedure update_video_xevious;
  procedure draw_sprites_xevious;
  var
    f: byte;
    code, color, x, y, sx1, sx2, sy1, sy2: word;
    flipx, flipy: boolean;
  begin
    for f := 0 to $3F do
    begin
      if ((memory[$A781 + (f * 2)] and $40) = 0) then
      begin
        if (memory[$9780 + (f * 2)] and $80) <> 0 then
          code := (memory[$A780 + (f * 2)] and $3F) + $100
        else
          code := memory[$A780 + (f * 2)];
        color := (memory[$A781 + (f * 2)] and $7F) shl 3;
        flipy := (memory[$9780 + (f * 2)] and 4) <> 0;
        flipx := (memory[$9780 + (f * 2)] and 8) <> 0;
        y := memory[$8781 + (f * 2)] - 39 + $100 * (memory[$9781 + (f * 2)] and 1);
        x := memory[$8780 + (f * 2)] - 19;
        case memory[$9780 + (f * 2)] and 3 of
          0:
            begin // normal
              put_gfx_sprite_mask(code, color, flipx, flipy, 2, 0, $F);
              update_gfx_sprite(x, y, 3, 2);
            end;
          1:
            begin // double width
              code := code and $1FE;
              sx1 := 16 * byte(flipx);
              sx2 := 16 * byte(not(flipx));
              put_gfx_sprite_mask_diff(code, color, flipx, flipy, 2, 0, $F, sx1, 0);
              put_gfx_sprite_mask_diff(code + 1, color, flipx, flipy, 2, 0, $F, sx2, 0);
              actualiza_gfx_sprite_size(x, y, 3, 32, 16);
            end;
          2:
            begin // double height
              code := code and $1FD;
              sy1 := 16 * byte(flipy);
              sy2 := 16 * byte(not(flipy));
              put_gfx_sprite_mask_diff(code + 2, color, flipx, flipy, 2, 0, $F, 0, sy1);
              put_gfx_sprite_mask_diff(code, color, flipx, flipy, 2, 0, $F, 0, sy2);
              actualiza_gfx_sprite_size(x, y, 3, 16, 32);
            end;
          3:
            begin // double width, double height
              sx1 := 16 * byte(flipx);
              sx2 := 16 * byte(not(flipx));
              sy1 := 16 * byte(flipy);
              sy2 := 16 * byte(not(flipy));
              code := code and $1FC;
              put_gfx_sprite_mask_diff(code + 3, color, flipx, flipy, 2, 0, $F, sx1, sy2);
              put_gfx_sprite_mask_diff(code + 1, color, flipx, flipy, 2, 0, $F, sx2, sy2);
              put_gfx_sprite_mask_diff(code + 2, color, flipx, flipy, 2, 0, $F, sx1, sy1);
              put_gfx_sprite_mask_diff(code, color, flipx, flipy, 2, 0, $F, sx2, sy1);
              actualiza_gfx_sprite_size(x, y, 3, 32, 32);
            end;
        end;
      end;
    end;
  end;

var
  f, color, nchar: word;
  x, y, atrib: byte;
begin
  for f := 0 to $7FF do
  begin
    x := 63 - (f div 64);
    y := f mod 64;
    if gfx[0].buffer[f] then
    begin
      atrib := memory[$B000 + f];
      color := (((atrib and $3) shl 4) or ((atrib and $3C) shr 2)) shl 1;
      nchar := memory[$C000 + f];
      put_gfx_trans_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
    if gfx[1].buffer[f] then
    begin
      nchar := memory[$C800 + f];
      atrib := memory[$B800 + f];
      color := ((atrib and $3C) shr 2) or ((nchar and $80) shr 3) or ((atrib and 3) shl 5);
      put_gfx_flip(x * 8, y * 8, nchar + ((atrib and 1) shl 8), color shl 2, 2, 1, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scrolly_bg + 20, scrollx_bg + 20, 0, 0, 0, 1);
  draw_sprites_xevious;
  scroll_x_y(1, 3, scrolly_fg + 18, scrollx_fg + 32, 0, 0, 0, 1);
  update_final_piece(0, 0, 224, 288, 3);
end;

procedure xevious_loop;
var
  frame_m, frame_s1, frame_s2: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s1 := z80_2.tframes;
  frame_s2 := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sub CPU
        z80_2.run(frame_s1);
        frame_s1 := frame_s1 + z80_2.tframes - z80_2.contador;
        // Sub 2 CPU
        z80_1.run(frame_s2);
        frame_s2 := frame_s2 + z80_1.tframes - z80_1.contador;
        // IO's
        run_namco_50xx(0);
        run_namco_54xx;
        case f of
          63, 191:
            if sub2_nmi then
              z80_1.change_nmi(PULSE_LINE);
          223:
            begin
              if main_irq then
                z80_0.change_irq(ASSERT_LINE);
              if sub_irq then
                z80_2.change_irq(ASSERT_LINE);
              update_video_xevious;
            end;
        end;
      end;
      events_galaga;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function xevious_getbyte(direccion: word): byte;
  function xevious_dip(direccion: byte): byte;
  var
    bit0, bit1: byte;
  begin
    bit0 := ((marcade.dswb or marcade.in2) shr direccion) and 1;
    bit1 := (marcade.dswa shr direccion) and 1;
    xevious_dip := bit0 or (bit1 shl 1);
  end;

  function xevious_bb_r(direccion: byte): byte;
  var
    dat1, adr_2b, adr_2c: word;
    dat2: byte;
  begin
    // get BS to 12 bit data from 2A,2B
    adr_2b := ((xevious_bs[1] and $7E) shl 6) or ((xevious_bs[0] and $FE) shr 1);
    if (adr_2b and 1) <> 0 then
      dat1 := ((xevious_tiles[0 + (adr_2b shr 1)] and $F0) shl 4) or xevious_tiles[$1000 + adr_2b]
      // high bits select
    else
      dat1 := ((xevious_tiles[0 + (adr_2b shr 1)] and $F) shl 8) or xevious_tiles[$1000 + adr_2b];
    // low bits select
    adr_2c := ((dat1 and $1FF) shl 2) or ((xevious_bs[1] and 1) shl 1) or (xevious_bs[0] and 1);
    if (dat1 and $400) <> 0 then
      adr_2c := adr_2c xor 1;
    if (dat1 and $200) <> 0 then
      adr_2c := adr_2c xor 2;
    if (direccion <> 0) then
      dat2 := xevious_tiles[$3000 + (adr_2c or $800)] // return BB1
    else
    begin // return BB0
      dat2 := xevious_tiles[$3000 + adr_2c];
      // swap bit 6 & 7
      dat2 := BITSWAP8(dat2, 6, 7, 5, 4, 3, 2, 1, 0);
      // flip x & y
      if (dat1 and $400) <> 0 then
        dat2 := dat2 xor $40;
      if (dat1 and $200) <> 0 then
        dat2 := dat2 xor $80;
    end;
    xevious_bb_r := dat2;
  end;

begin
  case direccion of
    0 .. $3FFF, $7800 .. $87FF, $9000 .. $97FF, $A000 .. $A7FF, $B000 .. $CFFF:
      xevious_getbyte := memory[direccion];
    $6800 .. $6807:
      xevious_getbyte := xevious_dip(direccion and 7);
    $7000 .. $70FF:
      xevious_getbyte := namco_06xx_data_r(direccion and $FF, 0);
    $7100:
      xevious_getbyte := namco_06xx_ctrl_r(0);
    $F000 .. $FFFF:
      xevious_getbyte := xevious_bb_r(direccion and 1);
  end;
end;

procedure xevious_putbyte(direccion: word; valor: byte);
var
  scroll: word;
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $6800 .. $681F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $6820 .. $6827:
      galaga_latch(direccion and 7, valor);
    $6830:
      ;
    $7000 .. $70FF:
      namco_06xx_data_w(direccion and $FF, 0, valor);
    $7100:
      namco_06xx_ctrl_w(0, valor);
    $7800 .. $87FF, $9000 .. $97FF, $A000 .. $A7FF:
      memory[direccion] := valor;
    $B000 .. $B7FF, $C000 .. $C7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $B800 .. $BFFF, $C800 .. $CFFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $D000 .. $D07F:
      begin
        scroll := valor + ((direccion and 1) shl 8); // A0 -> D8
        case ((direccion and $F0) shr 4) of
          0:
            scrollx_bg := scroll;
          1:
            scrollx_fg := scroll;
          2:
            scrolly_bg := scroll;
          3:
            scrolly_fg := scroll;
          7:
            main_screen.flip_main_screen := (scroll and 1) <> 0;
        end;
      end;
    $F000 .. $FFFF:
      xevious_bs[direccion and 1] := valor;
  end;
end;

// Sub1 CPU
function xevious_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      xevious_sub_getbyte := mem_snd[direccion];
    $4000 .. $FFFF:
      xevious_sub_getbyte := xevious_getbyte(direccion);
  end;
end;

// Sub2 CPU
function xevious_sub2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      xevious_sub2_getbyte := mem_misc[direccion];
    $4000 .. $FFFF:
      xevious_sub2_getbyte := xevious_getbyte(direccion);
  end;
end;

// Bosconian
procedure update_video_bosco;
  procedure update_stars_bosco;
  const
    speedsx: array [0 .. 7] of integer = (-1, -2, -3, 0, 3, 2, 1, 0);
    speedsy: array [0 .. 7] of integer = (0, -1, -2, -3, 0, 3, 2, 1);
  begin
    scrollx_bg := scrollx_bg + (speedsx[galaga_starcontrol[0] and 7]);
    scrolly_bg := scrolly_bg + (speedsy[(galaga_starcontrol[0] and $38) shr 3]);
  end;

  procedure draw_stars_bosco;
  var
    x, y, star_cntr, set_a, set_b: byte;
    color: word;
  begin
    set_a := galaga_starcontrol[1] and 1;
    set_b := (galaga_starcontrol[2] and 1) or 2;
    for star_cntr := 0 to (MAX_STARS - 1) do
    begin
      if ((set_a = star_seed_tab[star_cntr].set_) or (set_b = star_seed_tab[star_cntr].set_)) then
      begin
        y := (star_seed_tab[star_cntr].y + scrolly_bg) and $FF;
        x := (star_seed_tab[star_cntr].x + scrollx_bg) and $FF;
        color := paleta[32 + star_seed_tab[star_cntr].col];
        putpixel(x, y, 1, @color, 4);
      end;
    end;
  end;

var
  f, pos, x, y: word;
  color, nchar, atrib: byte;
  flipx, flipy: boolean;
begin
  fill_full_screen(4, 100);
  draw_stars_bosco;
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f + $400] then
    begin
      y := f div 32;
      x := f mod 32;
      atrib := memory[$8C00 + f];
      nchar := memory[$8400 + f];
      color := (atrib and $3F) shl 2;
      flipx := (atrib and $40) = 0;
      flipy := (atrib and $80) <> 0;
      put_gfx_mask_flip(x * 8, y * 8, nchar, color, 1, 0, $F, $F, flipx, flipy);
      gfx[0].buffer[f + $400] := false;
    end;
  end;
  // radar
  for x := 0 to 7 do
  begin
    for y := 0 to 31 do
    begin
      pos := x + (y shl 5);
      if gfx[0].buffer[pos] then
      begin
        atrib := memory[$8800 + pos];
        nchar := memory[$8000 + pos];
        color := (atrib and $3F) shl 2;
        put_gfx_flip(x * 8, y * 8, nchar, color, 2, 0, (atrib and $40) = 0, (atrib and $80) <> 0);
        gfx[0].buffer[pos] := false;
      end;
    end;
  end;
  scroll_x_y(4, 3, scrollx_bg, scrolly_bg);
  scroll_x_y(1, 3, scrollx_bg, scrolly_bg);
  // sprites
  for f := 0 to 5 do
  begin
    x := memory[$83D5 + (f * 2)] - 1;
    y := 240 - memory[$8BD4 + (f * 2)];
    flipx := (memory[$83D4 + (f * 2)] and 1) <> 0;
    flipy := (memory[$83D4 + (f * 2)] and 2) <> 0;
    color := (memory[$8BD5 + (f * 2)] and $3F) shl 2;
    nchar := (memory[$83D4 + (f * 2)] and $FC) shr 2;
    put_gfx_sprite_mask(nchar, color, flipx, flipy, 1, $F, $F);
    update_gfx_sprite(x, y, 3, 1);
  end;
  update_region(32, 0, 32, 256, 2, 221, 0, 32, 256, 3);
  update_region(0, 0, 32, 256, 2, 253, 0, 32, 256, 3);
  // dots
  for f := 4 to $F do
  begin
    x := memory[$83F0 + f] + ((not(memory[$9800 + f]) and 1) shl 8) - 2;
    y := 251 - memory[$8BF0 + f];
    nchar := ((memory[$9800 + f] and $E) shr 1) xor 7;
    put_gfx_sprite(nchar, 0, false, false, 2);
    update_gfx_sprite(x, y, 3, 2);
  end;
  update_final_piece(0, 16, 285, 224, 3);
  update_stars_bosco;
end;

procedure bosco_loop;
var
  frame_m, frame_s1, frame_s2: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s1 := z80_2.tframes;
  frame_s2 := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sub CPU
        z80_2.run(frame_s1);
        frame_s1 := frame_s1 + z80_2.tframes - z80_2.contador;
        // Sub 2 CPU
        z80_1.run(frame_s2);
        frame_s2 := frame_s2 + z80_1.tframes - z80_1.contador;
        run_namco_50xx(0);
        run_namco_50xx(1);
        run_namco_54xx;
        case f of
          63, 191:
            if sub2_nmi then
              z80_1.change_nmi(PULSE_LINE);
          223:
            begin
              if main_irq then
                z80_0.change_irq(ASSERT_LINE);
              if sub_irq then
                z80_2.change_irq(ASSERT_LINE);
              update_video_bosco;
            end;
        end;
      end;
      events_galaga;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function bosco_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $7800 .. $8FFF:
      bosco_getbyte := memory[direccion];
    $6800 .. $6807:
      bosco_getbyte := galaxian_dip(direccion and 7);
    $7000 .. $70FF:
      bosco_getbyte := namco_06xx_data_r(direccion and $FF, 0);
    $7100:
      bosco_getbyte := namco_06xx_ctrl_r(0);
    $9000 .. $90FF:
      bosco_getbyte := namco_06xx_data_r(direccion and $FF, 1);
    $9100:
      bosco_getbyte := namco_06xx_ctrl_r(1);
  end;
end;

procedure bosco_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $6800 .. $681F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $6820 .. $6827:
      galaga_latch(direccion and 7, valor);
    $7000 .. $70FF:
      namco_06xx_data_w(direccion and $FF, 0, valor);
    $7100:
      namco_06xx_ctrl_w(0, valor);
    $8000 .. $8FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $7800 .. $7FFF, $9800 .. $980F:
      memory[direccion] := valor;
    $9000 .. $90FF:
      namco_06xx_data_w(direccion and $FF, 1, valor);
    $9100:
      namco_06xx_ctrl_w(1, valor);
    $9810:
      scrollx_bg := valor;
    $9820:
      scrolly_bg := valor;
    $9830:
      galaga_starcontrol[0] := valor;
    $9870:
      main_screen.flip_main_screen := (valor and 1) = 0;
    $9874:
      galaga_starcontrol[1] := valor;
    $9875:
      galaga_starcontrol[2] := valor;
  end;
end;

function bosco_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      bosco_sub_getbyte := mem_snd[direccion];
    $4000 .. $FFFF:
      bosco_sub_getbyte := bosco_getbyte(direccion);
  end;
end;

function bosco_sub2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      bosco_sub2_getbyte := mem_misc[direccion];
    $4000 .. $FFFF:
      bosco_sub2_getbyte := bosco_getbyte(direccion);
  end;
end;

// Namco IO
procedure namco_06xx_nmi;
begin
  z80_0.change_nmi(PULSE_LINE);
end;

procedure namco_06xx_sub_nmi;
begin
  z80_2.change_nmi(PULSE_LINE);
end;

// Main
procedure reset_galagahw;
var
  f: byte;
begin
  z80_0.reset;
  z80_2.reset;
  z80_1.reset;
  namco_snd_0.reset;
  namcoio_06xx_reset(0);
  case main_vars.machine_type of
    65:
      begin
        namcoio_51xx_reset(false);
        namcoio_54xx_reset;
        fillchar(galaga_starcontrol, 6, 0);
        scrollx_bg := 0;
        scrolly_bg := 0;
      end;
    167:
      begin
        namcoio_51xx_reset(false);
        namcoio_53xx_reset;
        custom_mod := 0;
        bg_select := 0;
        bg_color_bank := 0;
        bg_disable := false;
        tx_color_mode := 0;
        bg_repaint := true;
      end;
    231, 350:
      begin
        namcoio_50xx_reset(0);
        namcoio_51xx_reset(true);
        namcoio_54xx_reset;
        scrollx_bg := 0;
        scrolly_bg := 0;
        scrollx_fg := 0;
        scrolly_fg := 0;
        xevious_bs[0] := 0;
        xevious_bs[1] := 0;
      end;
    250:
      begin
        namcoio_50xx_reset(0);
        namcoio_50xx_reset(1);
        namcoio_51xx_reset(false);
        namcoio_54xx_reset;
        fillchar(galaga_starcontrol, 6, 0);
        scrollx_bg := 0;
        scrolly_bg := 0;
        namcoio_06xx_reset(1);
      end;
  end;
 reset_game_general;
  main_irq := false;
  sub_irq := false;
  sub2_nmi := false;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $11;
  for f := 0 to 7 do
    galaga_latch(f, 0);
end;

procedure close_galagahw;
begin
  case main_vars.machine_type of
    65:
      namco_54xx_close;
    167:
      namco_53xx_close;
    231, 350:
      begin
        namco_50xx_close(0);
        namco_54xx_close;
      end;
    250:
      begin
        namco_50xx_close(0);
        namco_50xx_close(1);
        namco_54xx_close;
      end;
  end;
end;

function start_galagahw: boolean;
var
  colores: tpaleta;
  f: word;
  ctemp0, ctemp1, ctemp2, ctemp3: byte;
  memory_temp: array [0 .. $9FFF] of byte;
const
  map: array [0 .. 3] of byte = (0, $47, $97, $DE);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  ps_x_bosco: array [0 .. 15] of dword = (8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3, 0, 1, 2, 3);
  pc_x_digdug: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
  pc_x_xevious: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  pc_x_galaga: array [0 .. 7] of dword = (8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  pc_x_dot: array [0 .. 3] of dword = (3 * 8, 2 * 8, 1 * 8, 0 * 8);
  pc_y_dot: array [0 .. 3] of dword = (3 * 32, 2 * 32, 1 * 32, 0 * 32);

  procedure galaga_chr(ngfx: byte; num: word);
  begin
    init_gfx(ngfx, 8, 8, num);
    gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
    convert_gfx(ngfx, 0, @memory_temp, @pc_x_galaga, @ps_y, true, false);
  end;

  procedure galaga_spr(num: word);
  begin
    init_gfx(1, 16, 16, num);
    gfx_set_desc_data(2, 0, 64 * 8, 0, 4);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;

begin
  start_galagahw := false;
  start_audio(false);
  machine_calls.close := close_galagahw;
  machine_calls.reset := reset_galagahw;
  machine_calls.fps_max := 60.6060606060;
  case main_vars.machine_type of
    65, 167:
      begin
        screen_init(1, 224, 288, true);
        screen_init(2, 256, 512, false, true);
        screen_init(3, 224, 288);
        if main_vars.machine_type = 65 then
          machine_calls.general_loop := galaga_loop
        else
          machine_calls.general_loop := digdug_loop;
      end;
    231, 350:
      begin
        screen_init(1, 256, 512, true);
        screen_mod_scroll(1, 256, 256, 255, 512, 512, 511);
        screen_init(2, 256, 512, true);
        screen_mod_scroll(2, 256, 256, 255, 512, 512, 511);
        screen_init(3, 256, 512, false, true);
        machine_calls.general_loop := xevious_loop;
      end;
    250:
      begin
        screen_init(1, 256, 256, true);
        screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
        screen_init(2, 64, 256, true);
        screen_init(3, 512, 512, false, true);
        screen_init(4, 256, 256);
        screen_mod_scroll(4, 256, 256, 255, 256, 256, 255);
        machine_calls.general_loop := bosco_loop;
      end;
  end;
  if main_vars.machine_type <> 250 then
    start_video(224, 288)
  else
    start_video(285, 224);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 264);
  // Sub CPU
  z80_2 := cpu_z80.create(3072000, 264);
  // Sub2 CPU
  z80_1 := cpu_z80.create(3072000, 264);
  // IO's
  // namcoio_51xx_init(@marcade.in1,@marcade.in0,'galaga.zip');
  namcoio_51xx_init(@marcade.in0, @marcade.in1);
  case main_vars.machine_type of
    65:
      begin // Galaga
        // Main
        z80_0.change_ram_calls(galaga_getbyte, galaga_putbyte);
        z80_0.init_sound(galaga_sound_update);
        // Sub1
        z80_2.change_ram_calls(galaga_sub_getbyte, galaga_putbyte);
        // Sub2
        z80_1.change_ram_calls(galaga_sub2_getbyte, galaga_putbyte);
        // Sound
        namco_snd_0 := namco_snd_chip.create(3);
        // Init IO's
        namco_06xx_init(0, IO51XX, NONE, NONE, IO54XX, namco_06xx_nmi);
        // Namco 54xx
        if not(namcoio_54xx_init('galaga.zip')) then
          exit;
        load_samples(galaga_samples);
        // cargar roms
        if not(roms_load(@memory, galaga_rom)) then
          exit;
        if not(roms_load(@mem_snd, galaga_sub)) then
          exit;
        if not(roms_load(@mem_misc, galaga_sub2)) then
          exit;
        // cargar sonido & iniciar_sonido
        if not(roms_load(namco_snd_0.get_wave_dir, galaga_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, galaga_char)) then
          exit;
        galaga_chr(0, $100);
        // convertir sprites
        if not(roms_load(@memory_temp, galaga_sprites)) then
          exit;
        galaga_spr($80);
        // poner la paleta
        if not(roms_load(@memory_temp, galaga_prom)) then
          exit;
        for f := 0 to $1F do
        begin
          ctemp1 := memory_temp[f];
          colores[f].r := $21 * (ctemp1 and 1) + $47 * ((ctemp1 shr 1) and 1) + $97 * ((ctemp1 shr 2) and 1);
          colores[f].g := $21 * ((ctemp1 shr 3) and 1) + $47 * ((ctemp1 shr 4) and 1) + $97 * ((ctemp1 shr 5) and 1);
          colores[f].b := 0 + $47 * ((ctemp1 shr 6) and 1) + $97 * ((ctemp1 shr 7) and 1);
        end;
        // paleta de las estrellas
        for f := 0 to $3F do
        begin
          ctemp1 := (f shr 0) and 3;
          colores[$20 + f].r := map[ctemp1];
          ctemp1 := (f shr 2) and 3;
          colores[$20 + f].g := map[ctemp1];
          ctemp1 := (f shr 4) and 3;
          colores[$20 + f].b := map[ctemp1];
        end;
        set_pal(colores, 32 + 64);
        // CLUT
        for f := 0 to $FF do
        begin
          gfx[0].colores[f] := memory_temp[$20 + f] + $10;
          gfx[1].colores[f] := memory_temp[$120 + f];
        end;
        // Dip
        marcade.dswa := $F7;
        marcade.dswa_val2 := @galaga_dip_a;
        marcade.dswb := $97;
        marcade.dswb_val2 := @galaga_dip_b;
      end;
    167:
      begin // DigDug
        // Main
        z80_0.change_ram_calls(digdug_getbyte, digdug_putbyte);
        z80_0.init_sound(digdug_sound_update);
        // Sub1
        z80_2.change_ram_calls(digdug_sub_getbyte, digdug_putbyte);
        // Sub2
        z80_1.change_ram_calls(digdug_sub2_getbyte, digdug_putbyte);
        // Sound
        namco_snd_0 := namco_snd_chip.create(3);
        // Init IO's
        namco_06xx_init(0, IO51XX, IO53XX, NONE, NONE, namco_06xx_nmi);
        // Namco 53XX
        if not(namcoio_53xx_init(namco_53xx_k_r, namco_53xx_r_r, 'digdug.zip')) then
          exit;
        // cargar roms
        if not(roms_load(@memory, digdug_rom)) then
          exit;
        if not(roms_load(@mem_snd, digdug_sub)) then
          exit;
        if not(roms_load(@mem_misc, digdug_sub2)) then
          exit;
        // cargar sonido & iniciar_sonido
        if not(roms_load(namco_snd_0.get_wave_dir, digdug_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, digdug_chars)) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(1, 0, 8 * 8, 0);
        convert_gfx(0, 0, @memory_temp, @pc_x_digdug, @ps_y, true, false);
        // sprites
        if not(roms_load(@memory_temp, digdug_sprites)) then
          exit;
        galaga_spr($100);
        // Background
        if not(roms_load(@digdug_bg, digdug_background)) then
          exit;
        if not(roms_load(@memory_temp, digdug_chars2)) then
          exit;
        galaga_chr(2, $100);
        // poner la paleta
        if not(roms_load(@memory_temp, digdug_prom)) then
          exit;
        for f := 0 to $1F do
        begin
          ctemp1 := memory_temp[f];
          colores[f].r := $21 * (ctemp1 and 1) + $47 * ((ctemp1 shr 1) and 1) + $97 * ((ctemp1 shr 2) and 1);
          colores[f].g := $21 * ((ctemp1 shr 3) and 1) + $47 * ((ctemp1 shr 4) and 1) + $97 * ((ctemp1 shr 5) and 1);
          colores[f].b := 0 + $47 * ((ctemp1 shr 6) and 1) + $97 * ((ctemp1 shr 7) and 1);
        end;
        set_pal(colores, 32);
        // CLUT
        for f := 0 to 15 do
        begin // chars
          gfx[0].colores[f * 2 + 0] := 0;
          gfx[0].colores[f * 2 + 1] := f;
        end;
        for f := 0 to $FF do
        begin
          gfx[1].colores[f] := memory_temp[$20 + f] + $10; // sprites
          gfx[2].colores[f] := memory_temp[$120 + f]; // background
        end;
        // Dip
        marcade.dswa := $99;
        marcade.dswa_val2 := @digdug_dip_a;
        marcade.dswb := $24;
        marcade.dswb_val2 := @digdug_dip_b;
      end;
    231, 350:
      begin // Xevious
        // Main
        z80_0.change_ram_calls(xevious_getbyte, xevious_putbyte);
        // Sub1
        z80_2.change_ram_calls(xevious_sub_getbyte, xevious_putbyte);
        // Sub2
        z80_1.change_ram_calls(xevious_sub2_getbyte, xevious_putbyte);
        // Init IO's
        namco_06xx_init(0, IO51XX, NONE, IO50XX_0, IO54XX, namco_06xx_nmi);
        // Namco 50xx - 54xx
        if not(namcoio_50xx_init(0, 'xevious.zip')) then
          exit;
        if not(namcoio_54xx_init('xevious.zip')) then
          exit;
        z80_0.init_sound(galaga_sound_update);
        load_samples(xevious_samples, 1, 'xevious.zip');
        // Sound
        namco_snd_0 := namco_snd_chip.create(3);
        // cargar roms
        if not(roms_load(@mem_misc, xevious_sub2, true, 'xevious.zip')) then
          exit;
        if main_vars.machine_type = 231 then
        begin
          if not(roms_load(@memory, xevious_rom)) then
            exit;
          if not(roms_load(@mem_snd, xevious_sub)) then
            exit;
        end
        else
        begin
          if not(roms_load(@memory, sxevious_rom)) then
            exit;
          if not(roms_load(@mem_snd, sxevious_sub)) then
            exit;
        end;
        // cargar sonido & iniciar_sonido
        if not(roms_load(namco_snd_0.get_wave_dir, xevious_sound, true, 'xevious.zip')) then
          exit;
        // chars
        if not(roms_load(@memory_temp, xevious_char, true, 'xevious.zip')) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(1, 0, 8 * 8, 0);
        convert_gfx(0, 0, @memory_temp, @pc_x_xevious, @ps_y, true, false);
        // convertir sprites
        fillchar(memory_temp, $A000, 0);
        if not(roms_load(@memory_temp, xevious_sprites, true, 'xevious.zip')) then
          exit;
        for f := $5000 to $6FFF do
          memory_temp[f + $2000] := memory_temp[f] shr 4;
        init_gfx(2, 16, 16, $140);
        gfx_set_desc_data(3, 0, 64 * 8, ($140 * 64 * 8) + 4, 0, 4);
        convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, true, false);
        // tiles
        if not(roms_load(@xevious_tiles, xevious_bg_tiles, true, 'xevious.zip')) then
          exit;
        if not(roms_load(@memory_temp, xevious_bg, true, 'xevious.zip')) then
          exit;
        init_gfx(1, 8, 8, $200);
        gfx_set_desc_data(2, 0, 8 * 8, 0, $200 * 8 * 8);
        convert_gfx(1, 0, @memory_temp, @pc_x_xevious, @ps_y, true, false);
        // poner la paleta
        if not(roms_load(@memory_temp, xevious_prom, true, 'xevious.zip')) then
          exit;
        for f := 0 to $FF do
        begin
          ctemp0 := (memory_temp[f] shr 0) and 1;
          ctemp1 := (memory_temp[f] shr 1) and 1;
          ctemp2 := (memory_temp[f] shr 2) and 1;
          ctemp3 := (memory_temp[f] shr 3) and 1;
          colores[f].r := $E * ctemp0 + $1F * ctemp1 + $43 * ctemp2 + $8F * ctemp3;
          ctemp0 := (memory_temp[f + 256] shr 0) and 1;
          ctemp1 := (memory_temp[f + 256] shr 1) and 1;
          ctemp2 := (memory_temp[f + 256] shr 2) and 1;
          ctemp3 := (memory_temp[f + 256] shr 3) and 1;
          colores[f].g := $E * ctemp0 + $1F * ctemp1 + $43 * ctemp2 + $8F * ctemp3;
          ctemp0 := (memory_temp[f + 512] shr 0) and 1;
          ctemp1 := (memory_temp[f + 512] shr 1) and 1;
          ctemp2 := (memory_temp[f + 512] shr 2) and 1;
          ctemp3 := (memory_temp[f + 512] shr 3) and 1;
          colores[f].b := $E * ctemp0 + $1F * ctemp1 + $43 * ctemp2 + $8F * ctemp3;
        end;
        set_pal(colores, 256);
        // CLUT
        for f := 0 to $FF do
          if (f mod 2) <> 0 then
            gfx[0].colores[f] := f shr 1
          else
            gfx[0].colores[f] := $80;
        for f := 0 to $1FF do
        begin
          gfx[1].colores[f] := (memory_temp[$300 + f] and $F) or ((memory_temp[$500 + f] and $F) shl 4);
          ctemp0 := (memory_temp[$700 + f] and $F) or ((memory_temp[$900 + f] and $F) shl 4);
          if (ctemp0 and $80) <> 0 then
            gfx[2].colores[f] := ctemp0 and $7F
          else
            gfx[2].colores[f] := $80;
        end;
        // Dip
        marcade.dswa := $FF;
        if main_vars.machine_type = 231 then
        begin
          marcade.dswa_val2 := @xevious_dip_a;
          marcade.dswb := $EE;
          marcade.dswb_val2 := @xevious_dip_b;
        end
        else
        begin
          // Dip
          marcade.dswa_val2 := @xevious_dip_a;
          marcade.dswb := $62;
          marcade.dswb_val2 := @xevious_dip_b;
        end;
      end;
    250:
      begin // Bosconian
        // Main
        z80_0.change_ram_calls(bosco_getbyte, bosco_putbyte);
        // Sub1
        z80_2.change_ram_calls(bosco_sub_getbyte, bosco_putbyte);
        // Sub2
        z80_1.change_ram_calls(bosco_sub2_getbyte, bosco_putbyte);
        // Init IO's
        namco_06xx_init(0, IO51XX, NONE, IO50XX_0, IO54XX, namco_06xx_nmi);
        namco_06xx_init(1, IO50XX_1, NONE { IO52XX } , NONE, NONE, namco_06xx_sub_nmi);
        // Namco 50xx - 54xx
        if not(namcoio_50xx_init(0, 'bosco.zip')) then
          exit;
        if not(namcoio_50xx_init(1, 'bosco.zip')) then
          exit;
        if not(namcoio_54xx_init('bosco.zip')) then
          exit;
        z80_0.init_sound(galaga_sound_update);
        load_samples(bosco_samples, 0.25);
        // Sound
        namco_snd_0 := namco_snd_chip.create(3);
        // cargar roms
        if not(roms_load(@memory, bosco_rom)) then
          exit;
        if not(roms_load(@mem_snd, bosco_sub)) then
          exit;
        if not(roms_load(@mem_misc, bosco_sub2)) then
          exit;
        // cargar sonido & iniciar_sonido
        if not(roms_load(namco_snd_0.get_wave_dir, bosco_snd)) then
          exit;
        // chars
        if not(roms_load(@memory_temp, bosco_char)) then
          exit;
        init_gfx(0, 8, 8, $100);
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(0, 0, @memory_temp, @pc_x_galaga, @ps_y, false, false);
        // convertir sprites
        if not(roms_load(@memory_temp, bosco_sprites)) then
          exit;
        init_gfx(1, 16, 16, $40);
        gfx_set_desc_data(2, 0, 64 * 8, 0, 4);
        convert_gfx(1, 0, @memory_temp, @ps_x_bosco, @ps_y, false, false);
        // convertir disparos
        if not(roms_load(@memory_temp, bosco_dots)) then
          exit;
        init_gfx(2, 4, 4, 8);
        gfx[2].trans[3] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 6, 7);
        convert_gfx(2, 0, @memory_temp, @pc_x_dot, @pc_y_dot, false, false);
        // poner la paleta
        if not(roms_load(@memory_temp, bosco_prom)) then
          exit;
        for f := 0 to $1F do
        begin
          ctemp0 := (memory_temp[f] shr 0) and 1;
          ctemp1 := (memory_temp[f] shr 1) and 1;
          ctemp2 := (memory_temp[f] shr 2) and 1;
          colores[f].r := $21 * ctemp0 + $47 * ctemp1 + $97 * ctemp2;
          ctemp0 := (memory_temp[f] shr 3) and 1;
          ctemp1 := (memory_temp[f] shr 4) and 1;
          ctemp2 := (memory_temp[f] shr 5) and 1;
          colores[f].g := $21 * ctemp0 + $47 * ctemp1 + $97 * ctemp2;
          ctemp1 := (memory_temp[f] shr 6) and 1;
          ctemp2 := (memory_temp[f] shr 7) and 1;
          colores[f].b := 0 + $47 * ctemp1 + $97 * ctemp2;
        end;
        for f := 0 to 63 do
        begin
          ctemp0 := (f shr 0) and 3;
          colores[f + 32].r := map[ctemp0];
          ctemp0 := (f shr 2) and 3;
          colores[f + 32].g := map[ctemp0];
          ctemp0 := (f shr 4) and 3;
          colores[f + 32].b := map[ctemp0];
        end;
        set_pal(colores, 32 + 64);
        // CLUT
        for f := 0 to $FF do
        begin
          gfx[0].colores[f] := (memory_temp[f + $20] and $F) + $10;
          gfx[1].colores[f] := memory_temp[f + $20] and $F;
        end;
        for f := 0 to 3 do
          gfx[2].colores[f] := 31 - f;
        // Dip
        marcade.dswa := $F7;
        marcade.dswa_val2 := @bosco_dip_a;
        marcade.dswb := $A7;
        marcade.dswb_val2 := @bosco_dip_b;
      end;
  end;
  // final
  reset_galagahw;
  start_galagahw := true;
end;

end.
