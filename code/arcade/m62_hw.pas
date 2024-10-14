unit m62_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m680x,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  msm5205,
  rom_engine,
  pal_engine,
  sound_engine;

function start_m62: boolean;

implementation

const
  // Kung-Fu Master
  kungfum_rom: array [0 .. 1] of tipo_roms = ((n: 'a-4e-c.bin'; l: $4000; p: 0; crc: $B6E2D083), (n: 'a-4d-c.bin'; l: $4000; p: $4000; crc: $7532918E));
  kungfum_pal: array [0 .. 6] of tipo_roms = ((n: 'g-1j-.bin'; l: $100; p: 0; crc: $668E6BCA), (n: 'g-1f-.bin'; l: $100; p: $100; crc: $964B6495), (n: 'g-1h-.bin'; l: $100; p: $200; crc: $550563E1),
    (n: 'b-1m-.bin'; l: $100; p: $300; crc: $76C05A9C), (n: 'b-1n-.bin'; l: $100; p: $400; crc: $23F06B99), (n: 'b-1l-.bin'; l: $100; p: $500; crc: $35E45021), (n: 'b-5f-.bin'; l: $20; p: $600;
    crc: $7A601C3D));
  kungfum_char: array [0 .. 2] of tipo_roms = ((n: 'g-4c-a.bin'; l: $2000; p: 0; crc: $6B2CC9C8), (n: 'g-4d-a.bin'; l: $2000; p: $2000; crc: $C648F558), (n: 'g-4e-a.bin'; l: $2000; p: $4000;
    crc: $FBE9276E));
  kungfum_sound: array [0 .. 2] of tipo_roms = ((n: 'a-3e-.bin'; l: $2000; p: $A000; crc: $58E87AB0), (n: 'a-3f-.bin'; l: $2000; p: $C000; crc: $C81E31EA), (n: 'a-3h-.bin'; l: $2000; p: $E000;
    crc: $D99FB995));
  kungfum_sprites: array [0 .. 11] of tipo_roms = ((n: 'b-4k-.bin'; l: $2000; p: 0; crc: $16FB5150), (n: 'b-4f-.bin'; l: $2000; p: $2000; crc: $67745A33), (n: 'b-4l-.bin'; l: $2000; p: $4000;
    crc: $BD1C2261), (n: 'b-4h-.bin'; l: $2000; p: $6000; crc: $8AC5ED3A), (n: 'b-3n-.bin'; l: $2000; p: $8000; crc: $28A213AA), (n: 'b-4n-.bin'; l: $2000; p: $A000; crc: $D5228DF3), (n: 'b-4m-.bin';
    l: $2000; p: $C000; crc: $B16DE4F2), (n: 'b-3m-.bin'; l: $2000; p: $E000; crc: $EBA0D66B), (n: 'b-4c-.bin'; l: $2000; p: $10000; crc: $01298885), (n: 'b-4e-.bin'; l: $2000; p: $12000;
    crc: $C77B87D4), (n: 'b-4d-.bin'; l: $2000; p: $14000; crc: $6A70615F), (n: 'b-4a-.bin'; l: $2000; p: $16000; crc: $6189D626));
  // Dip
  kungfum_dip_a: array [0 .. 4] of def_dip = ((mask: $1; name: 'Difficulty'; number: 2; dip: ((dip_val: $1; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $2; name: 'Energy Loss'; number: 2; dip: ((dip_val: $2; dip_name: 'Slow'), (dip_val: $0; dip_name: 'Fast'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $8; dip_name: '2'), (dip_val: $C; dip_name: '3'), (dip_val: $4; dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $F0; name: 'Coinage'; number: 15; dip: ((dip_val: $90; dip_name: '7 Coin - 1 Credit'), (dip_val: $A0; dip_name: '6 Coin - 1 Credit'), (dip_val: $B0;
    dip_name: '5 Coin - 1 Credit'), (dip_val: $C0; dip_name: '4 Coin - 1 Credit'), (dip_val: $D0; dip_name: '3 Coin - 1 Credit'), (dip_val: $E0; dip_name: '2 Coin - 1 Credit'), (dip_val: $F0;
    dip_name: '1 Coin - 1 Credit'), (dip_val: $70; dip_name: '1 Coin - 2 Credit'), (dip_val: $60; dip_name: '1 Coin - 3 Credit'), (dip_val: $50; dip_name: '1 Coin - 4 Credit'), (dip_val: $40;
    dip_name: '1 Coin - 5 Credit'), (dip_val: $30; dip_name: '1 Coin - 6 Credit'), (dip_val: $20; dip_name: '1 Coin - 7 Credit'), (dip_val: $10; dip_name: '1 Coin - 8 Credit'), (dip_val: $0;
    dip_name: 'Free Play'), ())), ());
  kungfum_dip_b: array [0 .. 8] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2; dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $2; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Coin Mode'; number: 2; dip: ((dip_val: $4; dip_name: 'Mode 1'), (dip_val: $0; dip_name: 'Mode 2'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8;
    name: 'Slow Motion Mode (Cheat)'; number: 2; dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10;
    name: 'Freeze (Cheat)'; number: 2; dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Level Selection Mode (Cheat)'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Invulnerability (Cheat)'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Service'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Spelunker
  spl_rom: array [0 .. 3] of tipo_roms = ((n: 'spra.4e'; l: $4000; p: 0; crc: $CF811201), (n: 'spra.4d'; l: $4000; p: $4000; crc: $BB4FAA4F), (n: 'sprm.7c'; l: $4000; p: $8000; crc: $FB6197E2),
    (n: 'sprm.7b'; l: $4000; p: $C000; crc: $26BB25A4));
  spl_pal: array [0 .. 6] of tipo_roms = ((n: 'sprm.2k'; l: $100; p: 0; crc: $FD8FA991), (n: 'sprm.2j'; l: $100; p: $100; crc: $0E3890B4), (n: 'sprm.2h'; l: $100; p: $200; crc: $0478082B),
    (n: 'sprb.1m'; l: $100; p: $300; crc: $8D8CCCAD), (n: 'sprb.1n'; l: $100; p: $400; crc: $C40E1CB2), (n: 'sprb.1l'; l: $100; p: $500; crc: $3EC46248), (n: 'sprb.5p'; l: $20; p: $600;
    crc: $746C6238));
  spl_char: array [0 .. 2] of tipo_roms = ((n: 'sprm.4p'; l: $4000; p: 0; crc: $4DFE2E63), (n: 'sprm.4l'; l: $4000; p: $4000; crc: $239F2CD4), (n: 'sprm.4m'; l: $4000; p: $8000; crc: $D6D07D70));
  spl_sound: array [0 .. 1] of tipo_roms = ((n: 'spra.3d'; l: $4000; p: $8000; crc: $4110363C), (n: 'spra.3f'; l: $4000; p: $C000; crc: $67A9D2E6));
  spl_sprites: array [0 .. 5] of tipo_roms = ((n: 'sprb.4k'; l: $4000; p: 0; crc: $E7F0E861), (n: 'sprb.4f'; l: $4000; p: $4000; crc: $32663097), (n: 'sprb.3p'; l: $4000; p: $8000; crc: $8FBAF373),
    (n: 'sprb.4p'; l: $4000; p: $C000; crc: $37069B76), (n: 'sprb.4c'; l: $4000; p: $10000; crc: $CFE46A88), (n: 'sprb.4e'; l: $4000; p: $14000; crc: $11C48979));
  spl_tiles: array [0 .. 5] of tipo_roms = ((n: 'sprm.1d'; l: $4000; p: 0; crc: $4EF7AE89), (n: 'sprm.1e'; l: $4000; p: $4000; crc: $A3755180), (n: 'sprm.3c'; l: $4000; p: $8000; crc: $B4008E6A),
    (n: 'sprm.3b'; l: $4000; p: $C000; crc: $F61CF012), (n: 'sprm.1c'; l: $4000; p: $10000; crc: $58B21C76), (n: 'sprm.1b'; l: $4000; p: $14000; crc: $A95CB3E5));
  // Spelunker II
  spl2_rom: array [0 .. 4] of tipo_roms = ((n: 'sp2-a.4e'; l: $4000; p: 0; crc: $96C04BBB), (n: 'sp2-a.4d'; l: $4000; p: $4000; crc: $CB38C2FF), (n: 'sp2-r.7d'; l: $8000; p: $8000; crc: $558837EA),
    (n: 'sp2-r.7c'; l: $8000; p: $10000; crc: $4B380162), (n: 'sp2-r.7b'; l: $4000; p: $18000; crc: $7709A1FE));
  spl2_pal: array [0 .. 6] of tipo_roms = ((n: 'sp2-r.1k'; l: $200; p: 0; crc: $31C1BCDC), (n: 'sp2-r.2k'; l: $100; p: $200; crc: $1CF5987E), (n: 'sp2-r.2j'; l: $100; p: $300; crc: $1ACBE2A5),
    (n: 'sp2-b.1m'; l: $100; p: $400; crc: $906104C7), (n: 'sp2-b.1n'; l: $100; p: $500; crc: $5A564C06), (n: 'sp2-b.1l'; l: $100; p: $600; crc: $8F4A2E3C), (n: 'sp2-b.5p'; l: $20; p: $700;
    crc: $CD126F6A));
  spl2_char: array [0 .. 2] of tipo_roms = ((n: 'sp2-r.4l'; l: $4000; p: 0; crc: $6A4B2D8B), (n: 'sp2-r.4m'; l: $4000; p: $4000; crc: $E1368B61), (n: 'sp2-r.4p'; l: $4000; p: $8000; crc: $FC138E13));
  spl2_sound: array [0 .. 1] of tipo_roms = ((n: 'sp2-a.3d'; l: $4000; p: $8000; crc: $839EC7E2), (n: 'sp2-a.3f'; l: $4000; p: $C000; crc: $AD3CE898));
  spl2_sprites: array [0 .. 5] of tipo_roms = ((n: 'sp2-b.4k'; l: $4000; p: 0; crc: $6CB67A17), (n: 'sp2-b.4f'; l: $4000; p: $4000; crc: $E4A1166F), (n: 'sp2-b.3n'; l: $4000; p: $8000;
    crc: $F59E8B76), (n: 'sp2-b.4n'; l: $4000; p: $C000; crc: $FA65BAC9), (n: 'sp2-b.4c'; l: $4000; p: $10000; crc: $1CAF7013), (n: 'sp2-b.4e'; l: $4000; p: $14000; crc: $780A463B));
  spl2_tiles: array [0 .. 2] of tipo_roms = ((n: 'sp2-r.1d'; l: $8000; p: 0; crc: $C19FA4C9), (n: 'sp2-r.3b'; l: $8000; p: $8000; crc: $366604AF), (n: 'sp2-r.1b'; l: $8000; p: $10000;
    crc: $3A0C4D47));
  // Lode Runner
  ldrun_rom: array [0 .. 3] of tipo_roms = ((n: 'lr-a-4e'; l: $2000; p: 0; crc: $5D7E2A4D), (n: 'lr-a-4d'; l: $2000; p: $2000; crc: $96F20473), (n: 'lr-a-4b'; l: $2000; p: $4000; crc: $B041C4A9),
    (n: 'lr-a-4a'; l: $2000; p: $6000; crc: $645E42AA));
  ldrun_pal: array [0 .. 6] of tipo_roms = ((n: 'lr-e-3m'; l: $100; p: 0; crc: $53040416), (n: 'lr-e-3l'; l: $100; p: $100; crc: $67786037), (n: 'lr-e-3n'; l: $100; p: $200; crc: $5B716837),
    (n: 'lr-b-1m'; l: $100; p: $300; crc: $4BAE1C25), (n: 'lr-b-1n'; l: $100; p: $400; crc: $9CD3DB94), (n: 'lr-b-1l'; l: $100; p: $500; crc: $08D8CF9A), (n: 'lr-b-5p'; l: $20; p: $600;
    crc: $E01F69E2));
  ldrun_char: array [0 .. 2] of tipo_roms = ((n: 'lr-e-2d'; l: $2000; p: 0; crc: $24F9B58D), (n: 'lr-e-2j'; l: $2000; p: $2000; crc: $43175E08), (n: 'lr-e-2f'; l: $2000; p: $4000; crc: $E0317124));
  ldrun_sound: array [0 .. 1] of tipo_roms = ((n: 'lr-a-3f'; l: $2000; p: $C000; crc: $7A96ACCD), (n: 'lr-a-3h'; l: $2000; p: $E000; crc: $3F7F3939));
  ldrun_sprites: array [0 .. 2] of tipo_roms = ((n: 'lr-b-4k'; l: $2000; p: 0; crc: $8141403E), (n: 'lr-b-3n'; l: $2000; p: $2000; crc: $55154154), (n: 'lr-b-4c'; l: $2000; p: $4000; crc: $924E34D0));
  // Lode Runner II
  ldrun2_rom: array [0 .. 5] of tipo_roms = ((n: 'lr2-a-4e.a'; l: $2000; p: 0; crc: $22313327), (n: 'lr2-a-4d'; l: $2000; p: $2000; crc: $EF645179), (n: 'lr2-a-4a.a'; l: $2000; p: $4000;
    crc: $B11DDF59), (n: 'lr2-a-4a'; l: $2000; p: $6000; crc: $470CC8A1), (n: 'lr2-h-1c.a'; l: $2000; p: $8000; crc: $7EBCADBC), (n: 'lr2-h-1d.a'; l: $2000; p: $A000; crc: $64CBB7F9));
  ldrun2_pal: array [0 .. 6] of tipo_roms = ((n: 'lr2-h-3m'; l: $100; p: 0; crc: $2C5D834B), (n: 'lr2-h-3l'; l: $100; p: $100; crc: $3AE69ACA), (n: 'lr2-h-3n'; l: $100; p: $200; crc: $2B28AEC5),
    (n: 'lr2-b-1m'; l: $100; p: $300; crc: $4EC9BB3D), (n: 'lr2-b-1n'; l: $100; p: $400; crc: $1DAF1FA4), (n: 'lr2-b-1l'; l: $100; p: $500; crc: $C8FB708A), (n: 'lr2-b-5p'; l: $20; p: $600;
    crc: $E01F69E2));
  ldrun2_char: array [0 .. 2] of tipo_roms = ((n: 'lr2-h-1e'; l: $2000; p: 0; crc: $9D63A8FF), (n: 'lr2-h-1j'; l: $2000; p: $2000; crc: $40332BBD), (n: 'lr2-h-1h'; l: $2000; p: $4000;
    crc: $9404727D));
  ldrun2_sound: array [0 .. 2] of tipo_roms = ((n: 'lr2-a-3e'; l: $2000; p: $A000; crc: $853F3898), (n: 'lr2-a-3f'; l: $2000; p: $C000; crc: $7A96ACCD), (n: 'lr2-a-3h'; l: $2000; p: $E000;
    crc: $2A0E83CA));
  ldrun2_sprites: array [0 .. 5] of tipo_roms = ((n: 'lr2-b-4k'; l: $2000; p: 0; crc: $79909871), (n: 'lr2-b-4f'; l: $2000; p: $2000; crc: $06BA1EF4), (n: 'lr2-b-3n'; l: $2000; p: $4000;
    crc: $3CC5893F), (n: 'lr2-b-4n'; l: $2000; p: $6000; crc: $49C12F42), (n: 'lr2-b-4c'; l: $2000; p: $8000; crc: $FBE6D24C), (n: 'lr2-b-4e'; l: $2000; p: $A000; crc: $75172D1F));

var
  sound_command, val_port1, val_port2, ldrun_color, sprites_sp: byte;
  scroll_x, scroll_y: word;
  memory_sprites: array [0 .. $1F] of byte;
  mem_rom: array [0 .. 3, 0 .. $1FFF] of byte;
  mem_rom2: array [0 .. 15, 0 .. $FFF] of byte;
  rom_bank, rom_bank2, pal_bank, ldrun2_banksw: byte;
  bankcontrol: array [0 .. 1] of byte;
  update_video_m62: procedure;
  calc_nchar_sp: function(color: byte): word;

procedure draw_sprites(pos, col, col_mask, pri_mask, pri: byte);
var
  f, i, h, atrib: byte;
  nchar, x, y, color: word;
  flipy: boolean;
  incr: integer;
begin
  for f := 0 to $1F do
  begin
    if ((memory[$C000 + (f * 8)] and pri_mask) = pri) then
    begin
      atrib := memory[$C005 + (f * 8)];
      nchar := memory[$C004 + (f * 8)] + ((atrib and $07) shl 8);
      color := (memory[$C000 + (f * 8)] and col_mask) shl 3;
      x := ((memory[$C007 + (f * 8)] and 1) shl 8) + memory[$C006 + (f * 8)];
      y := 256 + (128 * pos) - 15 - (256 * (memory[$C003 + (f * 8)] and 1) + memory[$C002 + (f * 8)]);
      flipy := (atrib and $80) <> 0;
      i := memory_sprites[(nchar shr 5) and $1F];
      case i of
        1:
          begin // doble
            y := y - 16;
            // Cojo el sprite base
            nchar := nchar and $FFFE;
          end;
        2:
          begin // Cuadruple
            i := 3;
            nchar := nchar and $FFFC;
            y := y - 3 * 16;
          end;
      end;
      if flipy then
      begin
        incr := -1;
        nchar := nchar + i;
      end
      else
        incr := 1;
      for h := 0 to i do
      begin
        put_gfx_sprite(nchar + (h * incr), color + (256 * col), (atrib and $40) <> 0, flipy, 1);
        update_gfx_sprite(x, y + (16 * h), 2, 1);
      end;
    end;
  end;
end;

procedure update_video_kungfum;
var
  f, nchar, y, x: word;
  atrib, color: byte;
begin
  for f := 0 to $7FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$D800 + f];
      color := atrib and $1F;
      nchar := memory[$D000 + f] + ((atrib and $C0) shl 2);
      put_gfx_flip(x * 8, y * 8, nchar, color shl 3, 1, 0, (atrib and $20) <> 0, false);
      if not((y < 6) or ((color shr 1) > $0C)) then
        put_gfx_block_trans(x * 8, y * 8, 3, 8, 8)
      else
        put_gfx_flip(x * 8, y * 8, nchar, color shl 3, 3, 0, (atrib and $20) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x_part2(1, 2, 464, @scroll_x, 0, 0, 48);
  // Sprites
  draw_sprites(1, 1, $1F, 0, 0);
  // La parte de arriba tiene prioridad sobre los sprites?
  scroll__x_part2(3, 2, 464, @scroll_x, 0, 0, 48);
  update_region(128, 0, 256, 48, 1, 128, 0, 256, 48, 2);
  update_final_piece(128, 0, 256, 256, 2);
end;

procedure update_video_ldrun;
var
  f, nchar, y, x: word;
  atrib, color: byte;
begin
  for f := 0 to $7FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$D001 + (f * 2)];
      color := atrib and $1F;
      nchar := memory[$D000 + (f * 2)] + ((atrib and $C0) shl 2);
      put_gfx_flip(x * 8, y * 8, nchar, color shl 3, 1, 0, (atrib and $20) <> 0, false);
      if not((color shr 1) >= ldrun_color) then
        put_gfx_block_trans(x * 8, y * 8, 3, 8, 8)
      else
        put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 3, 3, 0, (atrib and $20) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(64, 0, 384, 256, 1, 64, 0, 384, 256, 2);
  draw_sprites(1, 1, $F, $10, 0);
  update_region(64, 0, 384, 256, 3, 64, 0, 384, 256, 2);
  draw_sprites(1, 1, $F, $10, $10);
  update_final_piece(64, 0, 384, 256, 2);
end;

function calc_nchar_splunker(color: byte): word;
begin
  calc_nchar_splunker := ((color and $10) shl 4) + ((color and $20) shl 6) + ((color and $C0) shl 3);
end;

function calc_nchar_splunker2(color: byte): word;
begin
  calc_nchar_splunker2 := (color and $F0) shl 4;
end;

procedure update_video_spelunker;
var
  f, x, y: word;
  color, nchar: word;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      color := memory[$C801 + (f * 2)];
      nchar := memory[$C800 + (f * 2)] + ((color and $10) shl 4);
      put_gfx_trans(x * 12, y * 8, nchar, (pal_bank or (color and $F)) shl 3, 3, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  for f := 0 to $FFF do
  begin
    if gfx[2].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      color := memory[$A001 + (f * 2)];
      nchar := memory[$A000 + (f * 2)] + calc_nchar_sp(color);
      put_gfx(x * 8, y * 8, nchar, (pal_bank or (color and $F)) shl 3, 1, 2);
      gfx[2].buffer[f] := false;
    end;
  end;
  scroll_x_y(1, 2, scroll_x, scroll_y);
  draw_sprites(2, sprites_sp, $1F, 0, 0);
  update_region(0, 0, 384, 256, 3, 64, 128, 384, 256, 2);
  update_final_piece(64, 128, 384, 256, 2);
end;

procedure events_irem_m62;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
  end;
end;

procedure irem_m62_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := m6800_0.tframes;
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
        m6800_0.run(frame_s);
        frame_s := frame_s + m6800_0.tframes - m6800_0.contador;
      end;
      z80_0.change_irq(HOLD_LINE);
      update_video_m62;
      events_irem_m62;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// KungFu Master
function kungfum_getbyte(direccion: word): byte;
begin
  kungfum_getbyte := memory[direccion];
end;

procedure kungfum_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $A000:
      scroll_x := (scroll_x and $100) or valor;
    $B000:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $C000 .. $C0FF, $E000 .. $EFFF:
      memory[direccion] := valor;
    $D000 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function kungfum_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $0:
      kungfum_inbyte := marcade.in0;
    $1:
      kungfum_inbyte := marcade.in1;
    $2:
      kungfum_inbyte := marcade.in2;
    $3:
      kungfum_inbyte := marcade.dswa;
    $4:
      kungfum_inbyte := marcade.dswb;
  end;
end;

procedure kungfum_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      if ((valor and $80) = 0) then
        sound_command := valor and $7F
      else
        m6800_0.change_irq(ASSERT_LINE);
  end;
end;

// Spelunker
function spl_getbyte(direccion: word): byte;
begin
  case direccion of
    $8000 .. $9FFF:
      spl_getbyte := mem_rom[rom_bank, direccion and $1FFF];
  else
    spl_getbyte := memory[direccion];
  end;
end;

procedure spl_putbyte(direccion: word; valor: byte);
begin
  if direccion < $A000 then
    exit;
  memory[direccion] := valor;
  case direccion of
    $A000 .. $BFFF:
      gfx[2].buffer[(direccion and $1FFF) shr 1] := true;
    $C800 .. $CFFF:
      gfx[0].buffer[(direccion and $7FF) shr 1] := true;
    $D000:
      scroll_y := (scroll_y and $100) or valor;
    $D001:
      scroll_y := (scroll_y and $FF) or ((valor and 1) shl 8);
    $D002:
      scroll_x := (scroll_x and $100) or valor;
    $D003:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $D004:
      rom_bank := (valor and 3);
    $D005:
      if pal_bank <> ((valor and 1) shl 4) then
      begin
        pal_bank := (valor and 1) shl 4;
        fillchar(gfx[0].buffer[0], $400, 1);
        fillchar(gfx[2].buffer[0], $1000, 1);
      end;
  end;
end;

// Spelunker II
function spl2_getbyte(direccion: word): byte;
begin
  case direccion of
    $8000 .. $8FFF:
      spl2_getbyte := mem_rom[rom_bank, direccion and $FFF];
    $9000 .. $9FFF:
      spl2_getbyte := mem_rom2[rom_bank2, direccion and $FFF];
  else
    spl2_getbyte := memory[direccion];
  end;
end;

procedure spl2_putbyte(direccion: word; valor: byte);
begin
  if direccion < $A000 then
    exit;
  memory[direccion] := valor;
  case direccion of
    $A000 .. $BFFF:
      gfx[2].buffer[(direccion and $1FFF) shr 1] := true;
    $C800 .. $CFFF:
      gfx[0].buffer[(direccion and $7FF) shr 1] := true;
    $D000:
      scroll_y := (scroll_y and $100) or valor;
    $D001:
      scroll_x := (scroll_x and $100) or valor;
    $D002:
      begin
        scroll_x := (scroll_x and $FF) or ((valor and 2) shl 7);
        scroll_y := (scroll_y and $FF) or ((valor and 1) shl 8);
        if (pal_bank <> ((valor and $0C) shl 2)) then
        begin
          pal_bank := (valor and $0C) shl 2;
          fillchar(gfx[0].buffer[0], $400, 1);
          fillchar(gfx[2].buffer[0], $1000, 1);
        end;
      end;
    $D003:
      begin
        rom_bank := (valor and $C0) shr 6;
        rom_bank2 := (valor and $3C) shr 2;
      end;
  end;
end;

// Lode Runner
procedure ldrun_putbyte(direccion: word; valor: byte);
begin
  if direccion < $8000 then
    exit;
  memory[direccion] := valor;
  case direccion of
    $D000 .. $DFFF:
      gfx[0].buffer[(direccion and $FFF) shr 1] := true;
  end;
end;

// Lode Runner II
function ldrun2_getbyte(direccion: word): byte;
begin
  case direccion of
    $8000 .. $9FFF:
      ldrun2_getbyte := mem_rom[rom_bank, direccion and $1FFF];
  else
    ldrun2_getbyte := memory[direccion];
  end;
end;

function ldrun2_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $0:
      ldrun2_inbyte := marcade.in0;
    $1:
      ldrun2_inbyte := marcade.in1;
    $2:
      ldrun2_inbyte := marcade.in2;
    $3:
      ldrun2_inbyte := marcade.dswa;
    $4:
      ldrun2_inbyte := marcade.dswb;
    $80:
      begin
        if (ldrun2_banksw <> 0) then
        begin
          ldrun2_banksw := ldrun2_banksw - 1;
          // swap to bank #1 on second read */
          if (ldrun2_banksw = 0) then
            rom_bank := 1;
        end;
        ldrun2_inbyte := 0;
      end;
  end;
end;

procedure ldrun2_outbyte(puerto: word; valor: byte);
const
  banks: array [1 .. 30] of byte = (0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1);
begin
  case (puerto and $FF) of
    0:
      if ((valor and $80) = 0) then
        sound_command := valor and $7F
      else
        m6800_0.change_irq(ASSERT_LINE);
    $80 .. $81:
      begin
        bankcontrol[puerto and 1] := valor;
        if ((puerto and 1) = 0) then
        begin
          rom_bank := banks[valor];
        end
        else
        begin
          if ((bankcontrol[0] = $1) and (valor = $0D)) then
            ldrun2_banksw := 2
          else
            ldrun2_banksw := 0;
        end;
      end;
  end;
end;

// sonido
function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $4000 .. $FFFF:
      snd_getbyte := mem_snd[direccion];
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $800 .. $8FF:
      case direccion and $3 of
        0:
          m6800_0.change_irq(CLEAR_LINE);
        1:
          msm5205_0.data_w(valor);
        2:
          msm5205_1.data_w(valor);
      end;
    $4000 .. $FFFF:
      ;
  end;
end;

procedure out_port1(valor: byte);
begin
  val_port1 := valor;
end;

procedure out_port2(valor: byte);
begin
  if (((val_port2 and $01) <> 0) and ((not(valor and $01)) <> 0)) then
  begin
    // control or data port? */
    if (val_port2 and $04) <> 0 then
    begin
      // PSG 0 or 1? control */
      if (val_port2 and $08) <> 0 then
        ay8910_0.control(val_port1);
      if (val_port2 and $10) <> 0 then
        ay8910_1.control(val_port1);
    end
    else
    begin
      // PSG 0 or 1? data */
      if (val_port2 and $08) <> 0 then
        ay8910_0.Write(val_port1);
      if (val_port2 and $10) <> 0 then
        ay8910_1.Write(val_port1);
    end;
  end;
  val_port2 := valor;
end;

function in_port1: byte;
begin
  // PSG 0 or 1?
  if (val_port2 and $08) <> 0 then
    in_port1 := ay8910_0.read
  else if (val_port2 and $10) <> 0 then
    in_port1 := ay8910_1.read;
end;

function in_port2: byte;
begin
  in_port2 := 0;
end;

function ay0_porta_r: byte;
begin
  ay0_porta_r := sound_command;
end;

procedure ay0_portb_w(valor: byte);
begin
  // bits 0 and 1 reset the two chips */
  msm5205_0.reset_w((valor and 1) <> 0);
  msm5205_1.reset_w((valor and 2) <> 0);
end;

procedure adpcm_int;
begin
  m6800_0.change_nmi(PULSE_LINE);
end;

procedure irem_m62_play_sound;
begin
  ay8910_0.update;
  ay8910_1.update;
  msm5205_0.update;
  msm5205_1.update;
end;

// Main
procedure reset_irem_m62;
begin
  z80_0.reset;
  m6800_0.reset;
  reset_audio;
  ay8910_0.reset;
  ay8910_1.reset;
  msm5205_0.reset;
  msm5205_1.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  rom_bank := 0;
  rom_bank2 := 0;
  pal_bank := 0;
  sound_command := 0;
  val_port1 := 0;
  val_port2 := 0;
  scroll_x := 0;
  scroll_y := 0;
  ldrun2_banksw := 0;
  bankcontrol[0] := 0;
  bankcontrol[1] := 0;
end;

function start_m62: boolean;
var
  f, x: word;
  memory_temp: array [0 .. $1FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  procedure cargar_paleta;
  var
    colores: tpaleta;
    f: byte;
  begin
    for f := 0 to $FF do
    begin
      // Chars
      colores[f].r := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
      colores[f].g := ((memory_temp[f + $100] and $F) shl 4) or (memory_temp[f + $100] and $F);
      colores[f].b := ((memory_temp[f + $200] and $F) shl 4) or (memory_temp[f + $200] and $F);
      // Sprites
      colores[f + $100].r := ((memory_temp[f + $300] and $F) shl 4) or (memory_temp[f] and $F);
      colores[f + $100].g := ((memory_temp[f + $400] and $F) shl 4) or (memory_temp[f + $100] and $F);
      colores[f + $100].b := ((memory_temp[f + $500] and $F) shl 4) or (memory_temp[f + $200] and $F);
    end;
    set_pal(colores, 512);
  end;
  procedure cargar_paleta_spl2;
  var
    colores: tpaleta;
    f: word;
  begin
    for f := 0 to $1FF do
    begin
      // Chars
      colores[f].r := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
      colores[f].g := ((memory_temp[f] and $F0) shr 4) or (memory_temp[f] and $F0);
      colores[f].b := ((memory_temp[f + $200] and $F) shl 4) or (memory_temp[f + $200] and $F);
    end;
    for f := 0 to $FF do
    begin
      colores[f + $200].r := ((memory_temp[f + $400] and $F) shl 4) or (memory_temp[f + $400] and $F);
      colores[f + $200].g := ((memory_temp[f + $500] and $F) shl 4) or (memory_temp[f + $500] and $F);
      colores[f + $200].b := ((memory_temp[f + $600] and $F) shl 4) or (memory_temp[f + $600] and $F);
    end;
    set_pal(colores, 768);
  end;
  procedure make_chars_spl;
  const
    pc_spl_x: array [0 .. 11] of dword = (0, 1, 2, 3, $2000 * 8 + 0, $2000 * 8 + 1, $2000 * 8 + 2, $2000 * 8 + 3, $2000 * 8 + 4, $2000 * 8 + 5, $2000 * 8 + 6, $2000 * 8 + 7);
  var
    mem_char: array [0 .. $BFFF] of byte;
  begin
    copymemory(@mem_char[$0000], @memory_temp[$0000], $800);
    copymemory(@mem_char[$2000], @memory_temp[$0800], $800);
    copymemory(@mem_char[$0800], @memory_temp[$1000], $800);
    copymemory(@mem_char[$2800], @memory_temp[$1800], $800);
    copymemory(@mem_char[$1000], @memory_temp[$2000], $800);
    copymemory(@mem_char[$3000], @memory_temp[$2800], $800);
    copymemory(@mem_char[$1800], @memory_temp[$3000], $800);
    copymemory(@mem_char[$3800], @memory_temp[$3800], $800);
    copymemory(@mem_char[$4000], @memory_temp[$4000], $800);
    copymemory(@mem_char[$6000], @memory_temp[$4800], $800);
    copymemory(@mem_char[$4800], @memory_temp[$5000], $800);
    copymemory(@mem_char[$6800], @memory_temp[$5800], $800);
    copymemory(@mem_char[$5000], @memory_temp[$6000], $800);
    copymemory(@mem_char[$7000], @memory_temp[$6800], $800);
    copymemory(@mem_char[$5800], @memory_temp[$7000], $800);
    copymemory(@mem_char[$7800], @memory_temp[$7800], $800);
    copymemory(@mem_char[$8000], @memory_temp[$8000], $800);
    copymemory(@mem_char[$A000], @memory_temp[$8800], $800);
    copymemory(@mem_char[$8800], @memory_temp[$9000], $800);
    copymemory(@mem_char[$A800], @memory_temp[$9800], $800);
    copymemory(@mem_char[$9000], @memory_temp[$A000], $800);
    copymemory(@mem_char[$B000], @memory_temp[$A800], $800);
    copymemory(@mem_char[$9800], @memory_temp[$B000], $800);
    copymemory(@mem_char[$B800], @memory_temp[$B800], $800);
    init_gfx(0, 12, 8, $200);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(3, 0, 8 * 8, 0, $4000 * 8, 2 * $4000 * 8);
    convert_gfx(0, 0, @mem_char[0], @pc_spl_x[0], @ps_y[0], false, false);
  end;
  procedure make_chars(num: word; ngfx: byte);
  begin
    init_gfx(ngfx, 8, 8, num);
    gfx_set_desc_data(3, 0, 8 * 8, 2 * num * 8 * 8, num * 8 * 8, 0);
    convert_gfx(ngfx, 0, @memory_temp[0], @ps_x[0], @ps_y[0], false, false);
  end;
  procedure make_sprites(num: word);
  begin
    init_gfx(1, 16, 16, num);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(3, 0, 32 * 8, 2 * num * 32 * 8, num * 32 * 8, 0);
    convert_gfx(1, 0, @memory_temp[0], @ps_x[0], @ps_y[0], false, false);
  end;

begin
  machine_calls.general_loop := irem_m62_loop;
  machine_calls.reset := reset_irem_m62;
  machine_calls.fps_max := 55;
  start_m62 := false;
  fillchar(memory_temp[0], $20000, 0);
  start_audio(false);
  screen_init(1, 512, 512);
  screen_init(2, 512, 512, false, true);
  screen_init(3, 512, 512, true);
  case main_vars.machine_type of
    42:
      begin
        x := 256;
        screen_mod_scroll(1, 512, 256 + 128, 511, 512, 512, 511);
        screen_mod_scroll(3, 512, 256 + 128, 511, 512, 512, 511);
      end;
    72, 73:
      begin
        x := 384;
        screen_mod_scroll(1, 512, 384 + 128, 511, 512, 256 + 128, 511);
      end;
    74, 75:
      x := 384;
  end;
  start_video(x, 256);
  // Sound CPU
  m6800_0 := cpu_m6800.create(3579545, $100, TCPU_M6803);
  m6800_0.change_ram_calls(snd_getbyte, snd_putbyte);
  m6800_0.change_io_calls(in_port1, in_port2, nil, nil, out_port1, out_port2, nil, nil);
  m6800_0.init_sound(irem_m62_play_sound);
  // sound chips
  msm5205_0 := MSM5205_chip.create(384000, MSM5205_S96_4B, 1, 0);
  msm5205_1 := MSM5205_chip.create(384000, MSM5205_SEX_4B, 1, 0);
  msm5205_0.change_advance(adpcm_int);
  msm5205_1.change_advance(nil);
  ay8910_0 := ay8910_chip.create(3579545 div 4, AY8910, 1);
  ay8910_0.change_io_calls(ay0_porta_r, nil, nil, ay0_portb_w);
  ay8910_1 := ay8910_chip.create(3579545 div 4, AY8910, 1);
  marcade.dswa := $FF;
  marcade.dswb := $FD;
  case main_vars.machine_type of
    42:
      begin // KungFu Master
        // Main CPU
        z80_0 := cpu_z80.create(3072000, $100);
        z80_0.change_ram_calls(kungfum_getbyte, kungfum_putbyte);
        z80_0.change_io_calls(kungfum_inbyte, kungfum_outbyte);
        // video
        update_video_m62 := update_video_kungfum;
        // cargar roms
        if not(roms_load(@memory, kungfum_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, kungfum_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, kungfum_char)) then
          exit;
        make_chars(1024, 0);
        gfx[0].trans[0] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, kungfum_sprites)) then
          exit;
        make_sprites(1024);
        // poner la paleta
        if not(roms_load(@memory_temp, kungfum_pal)) then
          exit;
        cargar_paleta;
        copymemory(@memory_sprites[0], @memory_temp[$600], $20);
        marcade.dswa_val := @kungfum_dip_a;
        marcade.dswb_val := @kungfum_dip_b;
      end;
    72:
      begin // Spelunker
        // Main CPU
        z80_0 := cpu_z80.create(4000000, $100);
        z80_0.change_ram_calls(spl_getbyte, spl_putbyte);
        z80_0.change_io_calls(kungfum_inbyte, kungfum_outbyte);
        // video
        update_video_m62 := update_video_spelunker;
        calc_nchar_sp := calc_nchar_splunker;
        sprites_sp := 1;
        // cargar roms y ponerlas en sus bancos
        if not(roms_load(@memory_temp, spl_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$8000 + (f * $2000)], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, spl_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, spl_char)) then
          exit;
        make_chars_spl;
        // convertir sprites
        if not(roms_load(@memory_temp, spl_sprites)) then
          exit;
        make_sprites($400);
        // convertir tiles
        if not(roms_load(@memory_temp, spl_tiles)) then
          exit;
        make_chars(4096, 2);
        // poner la paleta
        if not(roms_load(@memory_temp, spl_pal)) then
          exit;
        cargar_paleta;
        copymemory(@memory_sprites[0], @memory_temp[$600], $20);
      end;
    73:
      begin // Spelunker II
        // Main CPU
        z80_0 := cpu_z80.create(4000000, $100);
        z80_0.change_ram_calls(spl2_getbyte, spl2_putbyte);
        z80_0.change_io_calls(kungfum_inbyte, kungfum_outbyte);
        // video
        update_video_m62 := update_video_spelunker;
        calc_nchar_sp := calc_nchar_splunker2;
        sprites_sp := 2;
        // cargar roms y ponerlas en sus bancos (2)
        if not(roms_load(@memory_temp, spl2_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        for f := 0 to 15 do
          copymemory(@mem_rom2[f, 0], @memory_temp[$8000 + (f * $1000)], $1000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$18000 + (f * $1000)], $1000);
        // cargar sonido
        if not(roms_load(@mem_snd, spl2_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, spl2_char)) then
          exit;
        make_chars_spl;
        // convertir sprites
        if not(roms_load(@memory_temp, spl2_sprites)) then
          exit;
        make_sprites($400);
        // convertir tiles
        if not(roms_load(@memory_temp, spl2_tiles)) then
          exit;
        make_chars(4096, 2);
        // poner la paleta
        if not(roms_load(@memory_temp, spl2_pal)) then
          exit;
        cargar_paleta_spl2;
        copymemory(@memory_sprites[0], @memory_temp[$700], $20);
      end;
    74:
      begin // Lode Runner
        // Main CPU
        z80_0 := cpu_z80.create(4000000, $100);
        z80_0.change_ram_calls(kungfum_getbyte, ldrun_putbyte);
        z80_0.change_io_calls(kungfum_inbyte, kungfum_outbyte);
        // video
        update_video_m62 := update_video_ldrun;
        ldrun_color := $0C;
        // cargar roms
        if not(roms_load(@memory, ldrun_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, ldrun_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, ldrun_char)) then
          exit;
        make_chars($400, 0);
        gfx[0].trans[0] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, ldrun_sprites)) then
          exit;
        make_sprites($100);
        // poner la paleta
        if not(roms_load(@memory_temp, ldrun_pal)) then
          exit;
        cargar_paleta;
        copymemory(@memory_sprites[0], @memory_temp[$600], $20);
      end;
    75:
      begin // Lode Runner II
        // Main CPU
        z80_0 := cpu_z80.create(4000000, $100);
        z80_0.change_ram_calls(ldrun2_getbyte, ldrun_putbyte);
        z80_0.change_io_calls(ldrun2_inbyte, ldrun2_outbyte);
        // video
        update_video_m62 := update_video_ldrun;
        ldrun_color := $04;
        // cargar roms y ponerlas en sus bancos
        if not(roms_load(@memory_temp, ldrun2_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        for f := 0 to 1 do
          copymemory(@mem_rom[f, 0], @memory_temp[$8000 + (f * $2000)], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, ldrun2_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, ldrun2_char)) then
          exit;
        make_chars($400, 0);
        gfx[0].trans[0] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, ldrun2_sprites)) then
          exit;
        make_sprites($200);
        // poner la paleta
        if not(roms_load(@memory_temp, ldrun2_pal)) then
          exit;
        cargar_paleta;
        copymemory(@memory_sprites[0], @memory_temp[$600], $20);
      end;
  end;
  // final
  reset_irem_m62;
  start_m62 := true;
end;

end.
