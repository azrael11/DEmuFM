unit armedf_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  ym_3812,
  pal_engine,
  sound_engine,
  dac,
  timer_engine,
  nb1414_m4;

function start_armedf: boolean;

implementation

const
  // Armed F
  armedf_rom: array [0 .. 5] of tipo_roms = ((n: '06.3d'; l: $10000; p: 0; crc: $0F9015E2), (n: '01.3f'; l: $10000; p: 1; crc: $816FF7C5), (n: '07.5d'; l: $10000; p: $20000; crc: $5B3144A5), (n: '02.4f'; l: $10000; p: $20001; crc: $FA10C29D), (n: 'af_08.rom'; l: $10000;
    p: $40000; crc: $D1D43600), (n: 'af_03.rom'; l: $10000; p: $40001; crc: $BBE1FE2D));
  armedf_sound: tipo_roms = (n: 'af_10.rom'; l: $10000; p: 0; crc: $C5EACB87);
  armedf_char: tipo_roms = (n: '09.11c'; l: $8000; p: 0; crc: $5C6993D5);
  armedf_bg: array [0 .. 1] of tipo_roms = ((n: 'af_14.rom'; l: $10000; p: 0; crc: $8C5DC5A7), (n: 'af_13.rom'; l: $10000; p: $10000; crc: $136A58A3));
  armedf_fg: array [0 .. 1] of tipo_roms = ((n: 'af_04.rom'; l: $10000; p: 0; crc: $44D3AF4F), (n: 'af_05.rom'; l: $10000; p: $10000; crc: $92076CAB));
  armedf_sprites: array [0 .. 1] of tipo_roms = ((n: 'af_11.rom'; l: $20000; p: 0; crc: $B46C473C), (n: 'af_12.rom'; l: $20000; p: $20000; crc: $23CB6BFE));
  armedf_dip_a: array [0 .. 7] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '6')), (mask: 4; name: '1st Bonus Life'; number: 2; val2: (4, 0); name2: ('20K', '40K')), (mask: 8; name: '2st Bonus Life'; number: 2; val2: (8, 0);
    name2: ('60K', '80K')), (mask: $C; name: 'Bonus Life'; number: 4; val4: ($C, 4, 8, 0); name4: ('20K 60K+', '20K 80K+', '40K 60K+', '40K 80K+')), (mask: $10; name: 'Demo Sounds'; number: 2; val2: (0, $10); name2: ('Off', 'On')), (mask: $20; name: 'Cabinet'; number: 2;
    val2: (0, $20); name2: ('Upright', 'Cocktail')), (mask: $C0; name: 'Difficulty'; number: 4; val4: ($C0, $80, $40, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), ());
  armedf_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (1, 3, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Coin B'; number: 4; val4: (4, $C, 0, 8); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $30;
    name: 'Allow Continue'; number: 4; val4: ($30, $20, $10, 0); name4: ('No', '3 Times', '5 Times', 'Yes')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  // Terra Force
  terraf_rom: array [0 .. 5] of tipo_roms = ((n: '8.6e'; l: $10000; p: 0; crc: $FD58FA06), (n: '3.6h'; l: $10000; p: 1; crc: $54823A7D), (n: '7.4e'; l: $10000; p: $20000; crc: $FDE8DE7E), (n: '2.4h'; l: $10000; p: $20001; crc: $DB987414), (n: '6.3e'; l: $10000; p: $40000;
    crc: $A5BB8C3B), (n: '1.3h'; l: $10000; p: $40001; crc: $D2DE6D28));
  terraf_sound: tipo_roms = (n: '11.17k'; l: $10000; p: 0; crc: $4407D475);
  terraf_nb1414: tipo_roms = (n: '10.11c'; l: $4000; p: 0; crc: $AC705812);
  terraf_char: tipo_roms = (n: '9.11e'; l: $8000; p: 0; crc: $BC6F7CBC);
  terraf_bg: array [0 .. 1] of tipo_roms = ((n: '15.8a'; l: $10000; p: 0; crc: $2144D8E0), (n: '14.6a'; l: $10000; p: $10000; crc: $744F5C9E));
  terraf_fg: array [0 .. 1] of tipo_roms = ((n: '5.15h'; l: $10000; p: 0; crc: $25D23DFD), (n: '4.13h'; l: $10000; p: $10000; crc: $B9B0FE27));
  terraf_sprites: array [0 .. 1] of tipo_roms = ((n: '12.7d'; l: $10000; p: 0; crc: $2D1F2CEB), (n: '13.9d'; l: $10000; p: $10000; crc: $1D2F92D6));
  terraf_dip_a: array [0 .. 7] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '6')), (mask: 4; name: '1st Bonus Life'; number: 2; val2: (4, 0); name2: ('20K', '50K')), (mask: 8; name: '2st Bonus Life'; number: 2; val2: (8, 0);
    name2: ('60K', '90K')), (mask: $C; name: 'Bonus Life'; number: 4; val4: ($C, 4, 8, 0); name4: ('20K 60K+', '20K 90K+', '50K 60K+', '50K 90K+')), (mask: $10; name: 'Demo Sounds'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), (mask: $20; name: 'Cabinet'; number: 2;
    val2: (0, $20); name2: ('Upright', 'Cocktail')), (mask: $C0; name: 'Difficulty'; number: 4; val4: ($C0, $80, $40, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), ());
  terraf_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (1, 3, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Coin B'; number: 4; val4: (4, $C, 0, 8); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $20;
    name: 'Flip Screen'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $C0; name: 'Allow Continue'; number: 4; val4: ($C0, $80, $40, 0); name4: ('No', '3 Times', '5 Times', 'Yes')), ());
  // Crazy Climber 2
  cclimbr2_rom: array [0 .. 5] of tipo_roms = ((n: '4.bin'; l: $10000; p: 0; crc: $7922EA14), (n: '1.bin'; l: $10000; p: 1; crc: $2AC7ED67), (n: '6.bin'; l: $10000; p: $20000; crc: $7905C992), (n: '5.bin'; l: $10000; p: $20001; crc: $47BE6C1E), (n: '3.bin'; l: $10000; p: $40000;
    crc: $1FB110D6), (n: '2.bin'; l: $10000; p: $40001; crc: $0024C15B));
  cclimbr2_sound: array [0 .. 1] of tipo_roms = ((n: '11.bin'; l: $4000; p: 0; crc: $FE0175BE), (n: '12.bin'; l: $8000; p: $4000; crc: $5DDF18F2));
  cclimbr2_nb1414: tipo_roms = (n: '9.bin'; l: $4000; p: 0; crc: $740D260F);
  cclimbr2_char: tipo_roms = (n: '10.bin'; l: $8000; p: 0; crc: $7F475266);
  cclimbr2_bg: array [0 .. 1] of tipo_roms = ((n: '17.bin'; l: $10000; p: 0; crc: $E24BB2D7), (n: '18.bin'; l: $10000; p: $10000; crc: $56834554));
  cclimbr2_fg: array [0 .. 1] of tipo_roms = ((n: '7.bin'; l: $10000; p: 0; crc: $CBDD3906), (n: '8.bin'; l: $10000; p: $10000; crc: $B2A613C0));
  cclimbr2_sprites: array [0 .. 3] of tipo_roms = ((n: '15.bin'; l: $10000; p: 0; crc: $4BF838BE), (n: '13.bin'; l: $10000; p: $20000; crc: $6B6EC999), (n: '16.bin'; l: $10000; p: $10000; crc: $21A265C5), (n: '14.bin'; l: $10000; p: $30000; crc: $F426A4AD));
  cclimbr2_dip_a: array [0 .. 7] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '6')), (mask: 4; name: '1st Bonus Life'; number: 2; val2: (4, 0); name2: ('30K', '60K')), (mask: 8; name: '2st Bonus Life'; number: 2; val2: (8, 0);
    name2: ('70K', 'None')), (mask: $C; name: 'Bonus Life'; number: 4; val4: ($C, 4, 8, 0); name4: ('30K 130K+', '60K 130K+', '30K', '60K')), (mask: $10; name: 'Demo Sounds'; number: 2; val2: (0, $10); name2: ('Off', 'On')), (mask: $20; name: 'Cabinet'; number: 2; val2: (0, $20);
    name2: ('Upright', 'Cocktail')), (mask: $40; name: 'Difficulty'; number: 2; val2: ($40, 0); name2: ('Easy', 'Normal')), ());
  cclimbr2_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (1, 3, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Coin B'; number: 4; val4: (4, $C, 0, 8); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $10;
    name: 'Allow Continue'; number: 2; val2: (0, $10); name2: ('No', '3 Times')), (mask: $20; name: 'Flip Screen'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $40; name: 'Partial Invulnerability'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  // Legion
  legion_rom: array [0 .. 3] of tipo_roms = ((n: 'lg1.bin'; l: $10000; p: 1; crc: $C4AEB724), (n: 'lg3.bin'; l: $10000; p: 0; crc: $777E4935), (n: 'legion.1b'; l: $10000; p: $20001; crc: $C306660A), (n: 'legion.1d'; l: $10000; p: $20000; crc: $C2E45E1E));
  legion_sound: array [0 .. 1] of tipo_roms = ((n: 'legion.1h'; l: $4000; p: 0; crc: $2CA4F7F0), (n: 'legion.1i'; l: $8000; p: $4000; crc: $79F4A827));
  legion_nb1414: tipo_roms = (n: 'lg7.bin'; l: $4000; p: 0; crc: $533E2B58);
  legion_char: tipo_roms = (n: 'lg8.bin'; l: $8000; p: 0; crc: $E0596570);
  legion_bg: tipo_roms = (n: 'legion.1l'; l: $10000; p: 0; crc: $29B8ADAA);
  legion_fg: array [0 .. 1] of tipo_roms = ((n: 'legion.1e'; l: $10000; p: 0; crc: $A9D70FAF), (n: 'legion.1f'; l: $8000; p: $18000; crc: $F018313B));
  legion_sprites: array [0 .. 1] of tipo_roms = ((n: 'legion.1k'; l: $10000; p: 0; crc: $FF5A0DB9), (n: 'legion.1j'; l: $10000; p: $10000; crc: $BAE220C8));
  legion_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '6')), (mask: 4; name: 'Bonus Life'; number: 2; val2: (4, 0); name2: ('30K 100K+', '50K')), (mask: 8; name: 'Demo Sounds'; number: 2; val2: (8, 0);
    name2: ('Off', 'On')), (mask: $10; name: 'Flip Screen'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), (mask: $80; name: 'Allow Invulnerability'; number: 2; val2: ($80, 0); name2: ('No', 'Yes')), ());
  legion_dip_b: array [0 .. 6] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (1, 3, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Coin B'; number: 4; val4: (4, $C, 0, 8); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $10;
    name: 'Coin Slots'; number: 2; val2: ($10, 0); name2: ('Common', 'Individual')), (mask: $20; name: 'Difficulty'; number: 2; val2: ($20, 0); name2: ('Easy', 'Hard')), (mask: $40; name: 'P1 Invulnerability'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80;
    name: 'P2 Invulnerability'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  sprite_num, video_reg, scroll_fg_x, scroll_fg_y, scroll_bg_x, scroll_bg_y: word;
  rom: array [0 .. $2FFFF] of word;
  ram: array [0 .. $63FF] of word;
  ram_txt: array [0 .. $FFF] of byte;
  ram_bg, ram_fg, ram_clut, ram_sprites: array [0 .. $7FF] of word;
  size_x, size_y, irq_level, sound_latch, frame, sprite_offset: byte;
  update_video: procedure;
  calc_pos_txt: function(x, y: byte): word;

procedure draw_sprites(prio: byte);
  procedure armedf_put_gfx_sprite(nchar: dword; color: word; flipx, flipy: boolean; clut: byte);
  var
    x, y, pos_y: byte;
    temp, temp2: pword;
    pos: pbyte;
    punto: word;
    dir_x, dir_y: integer;
  begin
    nchar := nchar mod gfx[3].elements;
    pos := gfx[3].datos;
    inc(pos, nchar * 16 * 16);
    if flipy then
    begin
      pos_y := 15;
      dir_y := -1;
    end
    else
    begin
      pos_y := 0;
      dir_y := 1;
    end;
    if flipx then
    begin
      temp2 := punbuf;
      inc(temp2, 15);
      dir_x := -1;
    end
    else
    begin
      temp2 := punbuf;
      dir_x := 1;
    end;
    for y := 0 to 15 do
    begin
      temp := temp2;
      for x := 0 to 15 do
      begin
        punto := ram_clut[clut * $10 + pos^] and $F;
        if (punto <> 15) then
          temp^ := paleta[punto + color]
        else
          temp^ := paleta[MAX_COLORS];
        inc(temp, dir_x);
        inc(pos);
      end;
      putpixel_gfx_int(0, pos_y, 16, PANT_SPRITES);
      pos_y := pos_y + dir_y;
    end;
  end;

var
  atrib, f, nchar, sx, sy: word;
  flip_x, flip_y: boolean;
  color, clut, pri: byte;
begin
  for f := 0 to sprite_num do
  begin
    pri := (buffer_sprites_w[(f * 4) + 0] and $3000) shr 12;
    if pri <> prio then
      continue;
    nchar := buffer_sprites_w[(f * 4) + 1];
    flip_x := (nchar and $2000) <> 0;
    flip_y := (nchar and $1000) <> 0;
    atrib := buffer_sprites_w[(f * 4) + 2];
    color := (atrib shr 8) and $1F;
    clut := atrib and $7F;
    sx := buffer_sprites_w[(f * 4) + 3] and $1FF;
    sy := sprite_offset + 240 - (buffer_sprites_w[(f * 4) + 0] and $1FF);
    armedf_put_gfx_sprite(nchar, (color shl 4) + $200, flip_x, flip_y, clut);
    update_gfx_sprite(sx, sy, 5, 3);
  end;
end;

procedure draw_fg_bg(f: word; x, y: byte);
var
  color: byte;
  atrib, nchar: word;
begin
  atrib := ram_bg[f];
  color := atrib shr 11;
  if (gfx[1].buffer[f] or buffer_color[color + $40]) then
  begin
    nchar := atrib and $3FF;
    put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $600, 3, 1);
    gfx[1].buffer[f] := false;
  end;
  atrib := ram_fg[f];
  color := atrib shr 11;
  if (gfx[2].buffer[f] or buffer_color[color + $20]) then
  begin
    nchar := atrib and $3FF;
    put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $400, 4, 2);
    gfx[2].buffer[f] := false;
  end;
end;

procedure update_video_armedf;
var
  f, nchar, atrib: word;
  x, y, color: byte;
begin
  for f := 0 to $7FF do
  begin
    x := f div 32;
    y := f mod 32;
    atrib := ram_txt[$800 + f];
    color := atrib shr 4;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := ram_txt[f] + ((atrib and 3) shl 8);
      color := color shl 4;
      put_gfx(x * 8, y * 8, nchar, color, 1, 0);
      if (atrib and 8) = 0 then
        put_gfx_trans(x * 8, y * 8, nchar, color, 2, 0)
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      gfx[0].buffer[f] := false;
    end;
    draw_fg_bg(f, x, y);
  end;
  if (video_reg and $100) <> 0 then
    update_region(0, 0, 512, 256, 1, 0, 0, 512, 256, 5)
  else
    fill_full_screen(5, $800);
  if (video_reg and $800) <> 0 then
    scroll_x_y(3, 5, scroll_bg_x, scroll_bg_y);
  if (video_reg and $200) <> 0 then
    draw_sprites(2);
  if (video_reg and $400) <> 0 then
    scroll_x_y(4, 5, scroll_fg_x, scroll_fg_y);
  if (video_reg and $200) <> 0 then
    draw_sprites(1);
  if (video_reg and $100) <> 0 then
    update_region(0, 0, 512, 256, 2, 0, 0, 512, 256, 5);
  if (video_reg and $200) <> 0 then
    draw_sprites(0);
  update_final_piece(96, 8, 320, 240, 5);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
  copymemory(@buffer_sprites_w[0], @ram_sprites[0], $1000 * 2);
end;

function calc_pos_terraf(x, y: byte): word;
begin
  calc_pos_terraf := 32 * (31 - y) + (x and $1F) + $800 * (x div 32);
end;

function calc_pos_legion(x, y: byte): word;
begin
  calc_pos_legion := (x and $1F) * 32 + y + $800 * (x div 32);
end;

procedure update_video_terraf;
var
  f, nchar, atrib, pos: word;
  x, y, color: byte;
begin
  for f := 0 to $7FF do
  begin
    x := f div 32;
    y := f mod 32;
    pos := calc_pos_txt(x, y);
    if pos < $12 then
    begin
      atrib := 0;
      nchar := 0;
      color := 0;
    end
    else
    begin
      atrib := ram_txt[$400 + pos];
      nchar := ram_txt[pos];
      color := atrib shr 4;
    end;
    if (gfx[0].buffer[pos] or buffer_color[color]) then
    begin
      nchar := nchar or ((atrib and 3) shl 8);
      color := color shl 4;
      put_gfx(x * 8, y * 8, nchar, color, 1, 0);
      if (atrib and 8) = 0 then
        put_gfx_trans(x * 8, y * 8, nchar, color, 2, 0)
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      gfx[0].buffer[pos] := false;
    end;
    draw_fg_bg(f, x, y);
  end;
  if (video_reg and $100) <> 0 then
    scroll__x(1, 5, 512 - 128)
  else
    fill_full_screen(5, $800);
  if (video_reg and $800) <> 0 then
    scroll_x_y(3, 5, scroll_bg_x, scroll_bg_y);
  if (video_reg and $200) <> 0 then
    draw_sprites(2);
  if (video_reg and $400) <> 0 then
    scroll_x_y(4, 5, scroll_fg_x, scroll_fg_y);
  if (video_reg and $200) <> 0 then
    draw_sprites(1);
  if (video_reg and $100) <> 0 then
    scroll__x(2, 5, 512 - 128);
  if (video_reg and $200) <> 0 then
    draw_sprites(0);
  // actualiza_trozo_final(96,8,320,240,5);
  update_final_piece(96 + size_x, 8 + size_y, 320 - (size_x shl 1), 240 - (size_y shl 1), 5);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
  copymemory(@buffer_sprites_w[0], @ram_sprites[0], $1000 * 2);
end;

procedure events_armedf;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFEF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
  end;
end;

procedure armedf_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
 for f:=0 to $ff do begin
    events_armedf;
    if f=248 then begin
      m68000_0.irq[irq_level]:=ASSERT_LINE;
      update_video;
    end;
    //main
    m68000_0.run(frame_main);
    frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
    //sound
    z80_0.run(frame_snd);
    frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
 end;
 frame:=frame+1;
 video_sync;
    end
    else
      pause_action;
  end;
end;

function armedf_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $5FFFF:
      armedf_getword := rom[direccion shr 1];
    $60000 .. $60FFF:
      armedf_getword := ram_sprites[(direccion and $FFF) shr 1];
    $61000 .. $65FFF, $6C008 .. $6C7FF:
      armedf_getword := ram[(direccion - $60000) shr 1];
    $66000 .. $66FFF:
      armedf_getword := ram_bg[(direccion and $FFF) shr 1];
    $67000 .. $67FFF:
      armedf_getword := ram_fg[(direccion and $FFF) shr 1];
    $68000 .. $69FFF:
      armedf_getword := ram_txt[(direccion and $1FFF) shr 1];
    $6A000 .. $6AFFF:
      armedf_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $6B000 .. $6BFFF:
      armedf_getword := ram_clut[(direccion and $FFF) shr 1];
    $6C000:
      armedf_getword := marcade.in0;
    $6C002:
      armedf_getword := marcade.in1;
    $6C004:
      armedf_getword := marcade.dswa;
    $6C006:
      armedf_getword := marcade.dswb;
  end;
end;

procedure change_color(pos, data: word);
var
  color: tcolor;
begin
  color.r := pal4bit(data shr 8);
  color.g := pal4bit(data shr 4);
  color.b := pal4bit(data);
  set_pal_color(color, pos);
  case pos of
    0 .. $1FF:
      buffer_color[pos shr 4] := true; // chars
    $400 .. $5FF:
      buffer_color[((pos and $1FF) shr 4) + $20] := true; // fg
    $600 .. $7FF:
      buffer_color[((pos and $1FF) shr 4) + $40] := true; // bg
  end;
end;

procedure armedf_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $5FFFF:
      ;
    $60000 .. $60FFF:
      ram_sprites[(direccion and $FFF) shr 1] := valor;
    $61000 .. $65FFF, $6C000 .. $6C7FF:
      ram[(direccion - $60000) shr 1] := valor;
    $66000 .. $66FFF:
      if ram_bg[(direccion and $FFF) shr 1] <> valor then
      begin
        ram_bg[(direccion and $FFF) shr 1] := valor;
        gfx[1].buffer[((direccion and $FFF) shr 1) and $7FF] := true;
      end;
    $67000 .. $67FFF:
      if ram_fg[(direccion and $FFF) shr 1] <> valor then
      begin
        ram_fg[(direccion and $FFF) shr 1] := valor;
        gfx[2].buffer[((direccion and $FFF) shr 1) and $7FF] := true;
      end;
    $68000 .. $69FFF:
      if ram_txt[(direccion and $1FFF) shr 1] <> (valor and $FF) then
      begin
        ram_txt[(direccion and $1FFF) shr 1] := valor and $FF;
        gfx[0].buffer[((direccion and $1FFF) shr 1) and $7FF] := true;
      end;
    $6A000 .. $6AFFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color((direccion and $FFF) shr 1, valor);
      end;
    $6B000 .. $6BFFF:
      ram_clut[(direccion and $FFF) shr 1] := valor;
    $6D000:
      video_reg := valor;
    $6D002:
      scroll_bg_x := valor;
    $6D004:
      scroll_bg_y := valor;
    $6D006:
      scroll_fg_x := valor;
    $6D008:
      scroll_fg_y := valor;
    $6D00A:
      sound_latch := ((valor and $7F) shl 1) or 1;
    $6D00E:
      m68000_0.irq[1] := CLEAR_LINE;
  end;
end;

// Terra Force
function terraf_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $5FFFF:
      terraf_getword := rom[direccion shr 1];
    $60000 .. $603FF:
      terraf_getword := ram_sprites[(direccion and $FFF) shr 1];
    $60400 .. $63FFF, $6A000 .. $6A9FF:
      terraf_getword := ram[(direccion - $60000) shr 1];
    $64000 .. $64FFF:
      terraf_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $68000 .. $69FFF:
      terraf_getword := ram_txt[(direccion and $1FFF) shr 1];
    $6C000 .. $6CFFF:
      terraf_getword := ram_clut[(direccion and $FFF) shr 1];
    $70000 .. $70FFF:
      terraf_getword := ram_fg[(direccion and $FFF) shr 1];
    $74000 .. $74FFF:
      terraf_getword := ram_bg[(direccion and $FFF) shr 1];
    $78000:
      terraf_getword := marcade.in0;
    $78002:
      terraf_getword := marcade.in1;
    $78004:
      terraf_getword := marcade.dswa;
    $78006:
      terraf_getword := marcade.dswb;
  end;
end;

procedure terraf_putword(direccion: dword; valor: word);
var
  dir: word;
begin
  case direccion of
    0 .. $5FFFF:
      ;
    $60000 .. $603FF:
      ram_sprites[(direccion and $FFF) shr 1] := valor;
    $60400 .. $63FFF, $6A000 .. $6A9FF:
      ram[(direccion - $60000) shr 1] := valor;
    $64000 .. $64FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color((direccion and $FFF) shr 1, valor);
      end;
    $68000 .. $69FFF:
      if ram_txt[(direccion and $1FFF) shr 1] <> (valor and $FF) then
      begin
        dir := (direccion and $1FFF) shr 1;
        ram_txt[dir] := valor and $FF;
        gfx[0].buffer[(dir and $3FF) + (dir and $800)] := true;
      end;
    $6C000 .. $6CFFF:
      ram_clut[(direccion and $FFF) shr 1] := valor;
    $70000 .. $70FFF:
      if ram_fg[(direccion and $FFF) shr 1] <> valor then
      begin
        ram_fg[(direccion and $FFF) shr 1] := valor;
        gfx[2].buffer[((direccion and $FFF) shr 1) and $7FF] := true;
      end;
    $74000 .. $74FFF:
      if ram_bg[(direccion and $FFF) shr 1] <> valor then
      begin
        ram_bg[(direccion and $FFF) shr 1] := valor;
        gfx[1].buffer[((direccion and $FFF) shr 1) and $7FF] := true;
      end;
    $7C000:
      begin
        if (((valor and $4000) <> 0) and ((video_reg and $4000) = 0)) then
          nb1414m4_0.exec(scroll_fg_x, scroll_fg_y, frame);
        video_reg := valor;
      end;
    $7C002:
      scroll_bg_x := valor;
    $7C004:
      scroll_bg_y := valor;
    $7C00A:
      sound_latch := ((valor and $7F) shl 1) or 1;
    $7C00E:
      m68000_0.irq[irq_level] := CLEAR_LINE;
  end;
end;

// Sound
function armedf_snd_getbyte(direccion: word): byte;
begin
  armedf_snd_getbyte := mem_snd[direccion];
end;

procedure armedf_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $F7FF:
      ;
    $F800 .. $FFFF:
      mem_snd[direccion] := valor;
  end;
end;

function armedf_snd_in(puerto: word): byte;
begin
  case (puerto and $FF) of
    4:
      sound_latch := 0;
    6:
      armedf_snd_in := sound_latch;
  end;
end;

procedure armedf_snd_out(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ym3812_0.control(valor);
    1:
      ym3812_0.write(valor);
    2:
      dac_0.signed_data8_w(valor);
    3:
      dac_1.signed_data8_w(valor);
  end;
end;

procedure armedf_snd_irq;
begin
  z80_0.change_irq(HOLD_LINE);
end;

procedure cclimb2_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $FFFF:
      mem_snd[direccion] := valor;
  end;
end;

procedure armedf_sound_update;
begin
  ym3812_0.update;
  dac_0.update;
  dac_1.update;
end;

// Main
procedure reset_armedf;
begin
  m68000_0.reset;
  z80_0.reset;
  frame_main := m68000_0.tframes;
  frame_snd := z80_0.tframes;
  ym3812_0.reset;
  dac_0.reset;
  dac_1.reset;
  if main_vars.machine_type = 276 then
    nb1414m4_0.reset;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  scroll_fg_x := 0;
  scroll_fg_y := 0;
  scroll_bg_x := 0;
  scroll_bg_y := 0;
  sound_latch := 0;
  frame := 0;
  video_reg := 0;
end;

function start_armedf: boolean;
var
  memory_temp: array [0 .. $5FFFF] of byte;
const
  pf_x: array [0 .. 15] of dword = (4, 0, 12, 8, 20, 16, 28, 24, 32 + 4, 32 + 0, 32 + 12, 32 + 8, 32 + 20, 32 + 16, 32 + 28, 32 + 24);
  pf_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
  ps_x: array [0 .. 15] of dword = (4, 0, $800 * 64 * 8 + 4, $800 * 64 * 8 + 0, 12, 8, $800 * 64 * 8 + 12, $800 * 64 * 8 + 8, 20, 16, $800 * 64 * 8 + 20, $800 * 64 * 8 + 16, 28, 24, $800 * 64 * 8 + 28, $800 * 64 * 8 + 24);
  ps_x_terraf: array [0 .. 15] of dword = (4, 0, $400 * 64 * 8 + 4, $400 * 64 * 8 + 0, 12, 8, $400 * 64 * 8 + 12, $400 * 64 * 8 + 8, 20, 16, $400 * 64 * 8 + 20, $400 * 64 * 8 + 16, 28, 24, $400 * 64 * 8 + 28, $400 * 64 * 8 + 24);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  procedure conv_chars(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[15] := true;
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memory_temp, @pf_x, @ps_y, false, false);
  end;
  procedure conv_tiles(num: word; ngfx: byte);
  begin
    init_gfx(ngfx, 16, 16, num);
    gfx[ngfx].trans[15] := true;
    gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
    convert_gfx(ngfx, 0, @memory_temp, @pf_x, @pf_y, false, false);
  end;
  procedure conv_sprites(num: word);
  begin
    init_gfx(3, 16, 16, num);
    gfx_set_desc_data(4, 0, 64 * 8, 0, 1, 2, 3);
    case main_vars.machine_type of
      275, 277:
        convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
      276, 278:
        convert_gfx(3, 0, @memory_temp, @ps_x_terraf, @ps_y, false, false)
    end;
  end;

begin
  machine_calls.general_loop := armedf_loop;
  machine_calls.reset := reset_armedf;
  machine_calls.fps_max := 59.082012;
  start_armedf := false;
  start_audio(false);
  // Pantallas
  screen_init(1, 512, 256, false);
  screen_mod_scroll(1, 512, 512, 511, 256, 256, 255);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 1024, 512, true);
  screen_mod_scroll(3, 1024, 512, 1023, 512, 256, 511);
  screen_init(4, 1024, 512, true);
  screen_mod_scroll(4, 1024, 512, 1023, 512, 256, 511);
  screen_init(5, 512, 512, false, true);
  if ((main_vars.machine_type = 275) or (main_vars.machine_type = 278)) then
    main_screen.rot270_screen := true;
  size_x := 0;
  size_y := 0;
  case main_vars.machine_type of
    275, 276:
      start_video(320, 240);
    277, 278:
      begin
        start_video(288, 224);
        size_x := 16;
        size_y := 8;
      end;
  end;
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256);
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, 256);
  z80_0.change_ram_calls(armedf_snd_getbyte, armedf_snd_putbyte);
  z80_0.change_io_calls(armedf_snd_in, armedf_snd_out);
  z80_0.init_sound(armedf_sound_update);
  timers.init(z80_0.numero_cpu, 4000000 / (4000000 / 512), armedf_snd_irq, nil, true);
  // Sound Chips
  if (main_vars.machine_type = 278) then
    ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 0.6)
  else
    ym3812_0 := ym3812_chip.create(YM3812_FM, 4000000, 0.6);
  dac_0 := dac_chip.create(1);
  dac_1 := dac_chip.create(1);
  irq_level := 1;
  sprite_offset := $80;
  calc_pos_txt := calc_pos_terraf;
  update_video := update_video_terraf;
  case main_vars.machine_type of
    275:
      begin // Armed F
        m68000_0.change_ram16_calls(armedf_getword, armedf_putword);
        // cargar roms
        if not(roms_load16w(@rom, armedf_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, armedf_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, armedf_char)) then
          exit;
        conv_chars($400);
        // convertir bg
        if not(roms_load(@memory_temp, armedf_bg)) then
          exit;
        conv_tiles($400, 1);
        // convertir fg
        if not(roms_load(@memory_temp, armedf_fg)) then
          exit;
        conv_tiles($400, 2);
        // convertir sprites
        if not(roms_load(@memory_temp, armedf_sprites)) then
          exit;
        conv_sprites($800);
        // DIP
        marcade.dswa := $FFDF;
        marcade.dswa_val2 := @armedf_dip_a;
        marcade.dswb := $FFCF;
        marcade.dswb_val2 := @armedf_dip_b;
        // Misc
        update_video := update_video_armedf;
        sprite_num := $200 - 1;
      end;
    276:
      begin // Terra Force
        m68000_0.change_ram16_calls(terraf_getword, terraf_putword);
        // nb1414
        nb1414m4_0 := tnb1414_m4.create(@ram_txt[0]);
        if not(roms_load(nb1414m4_0.get_internal_rom, terraf_nb1414)) then
          exit;
        // cargar roms
        if not(roms_load16w(@rom, terraf_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, terraf_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, terraf_char)) then
          exit;
        conv_chars($400);
        // convertir bg
        if not(roms_load(@memory_temp, terraf_bg)) then
          exit;
        conv_tiles($400, 1);
        // convertir fg
        if not(roms_load(@memory_temp, terraf_fg)) then
          exit;
        conv_tiles($400, 2);
        // convertir sprites
        if not(roms_load(@memory_temp, terraf_sprites)) then
          exit;
        conv_sprites($400);
        // DIP
        marcade.dswa := $FFCF;
        marcade.dswa_val2 := @terraf_dip_a;
        marcade.dswb := $FF3F;
        marcade.dswb_val2 := @terraf_dip_b;
        // Misc
        sprite_num := $80 - 1;
      end;
    277:
      begin // Crazy Climber 2
        m68000_0.change_ram16_calls(terraf_getword, terraf_putword);
        // nb1414
        nb1414m4_0 := tnb1414_m4.create(@ram_txt[0]);
        if not(roms_load(nb1414m4_0.get_internal_rom, cclimbr2_nb1414)) then
          exit;
        // cargar roms
        if not(roms_load16w(@rom, cclimbr2_rom)) then
          exit;
        // cargar sonido
        z80_0.change_ram_calls(armedf_snd_getbyte, cclimb2_snd_putbyte);
        if not(roms_load(@mem_snd, cclimbr2_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, cclimbr2_char)) then
          exit;
        conv_chars($400);
        // convertir bg
        if not(roms_load(@memory_temp, cclimbr2_bg)) then
          exit;
        conv_tiles($400, 1);
        // convertir fg
        if not(roms_load(@memory_temp, cclimbr2_fg)) then
          exit;
        conv_tiles($400, 2);
        // convertir sprites
        if not(roms_load(@memory_temp, cclimbr2_sprites)) then
          exit;
        conv_sprites($800);
        // DIP
        marcade.dswa := $FFDF;
        marcade.dswa_val2 := @cclimbr2_dip_a;
        marcade.dswb := $FFFF;
        marcade.dswb_val2 := @cclimbr2_dip_b;
        // Misc
        sprite_num := $200 - 1;
        irq_level := 2;
        sprite_offset := 0;
      end;
    278:
      begin // Legion
        m68000_0.change_ram16_calls(terraf_getword, terraf_putword);
        // nb1414
        nb1414m4_0 := tnb1414_m4.create(@ram_txt[0]);
        if not(roms_load(nb1414m4_0.get_internal_rom, legion_nb1414)) then
          exit;
        // cargar roms
        if not(roms_load16w(@rom, legion_rom)) then
          exit;
        // cargar sonido
        z80_0.change_ram_calls(armedf_snd_getbyte, cclimb2_snd_putbyte);
        if not(roms_load(@mem_snd, legion_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, legion_char)) then
          exit;
        conv_chars($400);
        // convertir bg
        if not(roms_load(@memory_temp, legion_bg)) then
          exit;
        conv_tiles($400, 1);
        // convertir fg
        if not(roms_load(@memory_temp, legion_fg)) then
          exit;
        conv_tiles($400, 2);
        // convertir sprites
        if not(roms_load(@memory_temp, legion_sprites)) then
          exit;
        conv_sprites($400);
        // DIP
        marcade.dswa := $FFDF;
        marcade.dswa_val2 := @legion_dip_a;
        marcade.dswb := $FFFF;
        marcade.dswb_val2 := @legion_dip_b;
        // Misc
        calc_pos_txt := calc_pos_legion;
        sprite_num := $80 - 1;
        irq_level := 2;
        sprite_offset := 0;
      end;
  end;
  // final
  start_armedf := true;
end;

end.
