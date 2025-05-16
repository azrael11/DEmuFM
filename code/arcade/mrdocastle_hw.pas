unit mrdocastle_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  msm5205;

function start_mrdocastle: boolean;

implementation

const
  // Mr Do Castle
  mrdocastle_rom: array [0 .. 3] of tipo_roms = ((n: '01p_a1.bin'; l: $2000; p: 0; crc: $17C6FC24), (n: '01n_a2.bin'; l: $2000; p: $2000; crc: $1D2FC7F4), (n: '01l_a3.bin'; l: $2000; p: $4000; crc: $71A70BA9), (n: '01k_a4.bin'; l: $2000; p: $6000; crc: $479A745E));
  mrdocastle_slave: tipo_roms = (n: '07n_a0.bin'; l: $4000; p: 0; crc: $F23B5CDB);
  mrdocastle_misc: tipo_roms = (n: '01d.bin'; l: $200; p: 0; crc: $2747CA77);
  mrdocastle_char: tipo_roms = (n: '03a_a5.bin'; l: $4000; p: 0; crc: $0636B8F4);
  mrdocastle_sprites: array [0 .. 3] of tipo_roms = ((n: '04m_a6.bin'; l: $2000; p: 0; crc: $3BBC9B26), (n: '04l_a7.bin'; l: $2000; p: $2000; crc: $3DFAA9D1), (n: '04j_a8.bin'; l: $2000; p: $4000; crc: $9AFB16E9), (n: '04h_a9.bin'; l: $2000; p: $6000; crc: $AF24BCE0));
  mrdocastle_pal: tipo_roms = (n: '09c.bin'; l: $200; p: 0; crc: $066F52BC);
  // Do Run Run
  dorunrun_rom: array [0 .. 3] of tipo_roms = ((n: '2764.p1'; l: $2000; p: 0; crc: $95C86F8E), (n: '2764.l1'; l: $2000; p: $4000; crc: $E9A65BA7), (n: '2764.k1'; l: $2000; p: $6000; crc: $B1195D3D), (n: '2764.n1'; l: $2000; p: $8000; crc: $6A8160D1));
  dorunrun_slave: tipo_roms = (n: '27128.p7'; l: $4000; p: 0; crc: $8B06D461);
  dorunrun_misc: tipo_roms = (n: 'bprom2.bin'; l: $200; p: 0; crc: $2747CA77);
  dorunrun_char: tipo_roms = (n: '27128.a3'; l: $4000; p: 0; crc: $4BE96DCF);
  dorunrun_sprites: array [0 .. 3] of tipo_roms = ((n: '2764.m4'; l: $2000; p: 0; crc: $4BB231A0), (n: '2764.l4'; l: $2000; p: $2000; crc: $0C08508A), (n: '2764.j4'; l: $2000; p: $4000; crc: $79287039), (n: '2764.h4'; l: $2000; p: $6000; crc: $523AA999));
  dorunrun_pal: tipo_roms = (n: 'dorunrun.clr'; l: $100; p: 0; crc: $D5BAB5D5);
  // Do Wild Ride
  dowild_rom: array [0 .. 3] of tipo_roms = ((n: 'w1'; l: $2000; p: 0; crc: $097DE78B), (n: 'w3'; l: $2000; p: $4000; crc: $FC6A1CBB), (n: 'w4'; l: $2000; p: $6000; crc: $8AAC1D30), (n: 'w2'; l: $2000; p: $8000; crc: $0914AB69));
  dowild_slave: tipo_roms = (n: 'w10'; l: $4000; p: 0; crc: $D1F37FBA);
  dowild_misc: tipo_roms = (n: '8300b-2'; l: $200; p: 0; crc: $2747CA77);
  dowild_char: tipo_roms = (n: 'w5'; l: $4000; p: 0; crc: $B294B151);
  dowild_sprites: array [0 .. 3] of tipo_roms = ((n: 'w6'; l: $2000; p: 0; crc: $57E0208B), (n: 'w7'; l: $2000; p: $2000; crc: $5001A6F7), (n: 'w8'; l: $2000; p: $4000; crc: $EC503251), (n: 'w9'; l: $2000; p: $6000; crc: $AF7BD7EB));
  dowild_pal: tipo_roms = (n: 'dowild.clr'; l: $100; p: 0; crc: $A703DEA5);
  // Jumping Jack
  jjack_rom: array [0 .. 3] of tipo_roms = ((n: 'j1.bin'; l: $2000; p: 0; crc: $87F29BD2), (n: 'j3.bin'; l: $2000; p: $4000; crc: $35B0517E), (n: 'j4.bin'; l: $2000; p: $6000; crc: $35BB316A), (n: 'j2.bin'; l: $2000; p: $8000; crc: $DEC52E80));
  jjack_slave: tipo_roms = (n: 'j0.bin'; l: $4000; p: 0; crc: $AB042F04);
  jjack_misc: tipo_roms = (n: 'bprom2.bin'; l: $200; p: 0; crc: $2747CA77);
  jjack_char: tipo_roms = (n: 'j5.bin'; l: $4000; p: 0; crc: $75038FF9);
  jjack_sprites: array [0 .. 3] of tipo_roms = ((n: 'j6.bin'; l: $2000; p: 0; crc: $5937BD7B), (n: 'j7.bin'; l: $2000; p: $2000; crc: $CF8AE8E7), (n: 'j8.bin'; l: $2000; p: $4000; crc: $84F6FC8C), (n: 'j9.bin'; l: $2000; p: $6000; crc: $3F9BB09F));
  jjack_pal: tipo_roms = (n: 'bprom1.bin'; l: $200; p: 0; crc: $2F0955F2);
  // Kick Rider
  kickridr_rom: array [0 .. 3] of tipo_roms = ((n: 'k1'; l: $2000; p: 0; crc: $DFDD1AB4), (n: 'k3'; l: $2000; p: $4000; crc: $412244DA), (n: 'k4'; l: $2000; p: $6000; crc: $A67DD2EC), (n: 'k2'; l: $2000; p: $8000; crc: $E193FB5C));
  kickridr_slave: tipo_roms = (n: 'k10'; l: $4000; p: 0; crc: $6843DBC0);
  kickridr_misc: tipo_roms = (n: '8300b-2'; l: $200; p: 0; crc: $2747CA77);
  kickridr_char: tipo_roms = (n: 'k5'; l: $4000; p: 0; crc: $3F7D7E49);
  kickridr_sprites: array [0 .. 3] of tipo_roms = ((n: 'k6'; l: $2000; p: 0; crc: $94252ED3), (n: 'k7'; l: $2000; p: $2000; crc: $7EF2420E), (n: 'k8'; l: $2000; p: $4000; crc: $29BED201), (n: 'k9'; l: $2000; p: $6000; crc: $847584D3));
  kickridr_pal: tipo_roms = (n: 'kickridr.clr'; l: $100; p: 0; crc: $73EC281C);
  // Indoor Soccer
  idsoccer_rom: array [0 .. 3] of tipo_roms = ((n: 'id01'; l: $2000; p: 0; crc: $F1C3BF09), (n: 'id02'; l: $2000; p: $2000; crc: $184E6AF0), (n: 'id03'; l: $2000; p: $6000; crc: $22524661), (n: 'id04'; l: $2000; p: $8000; crc: $E8CD95FD));
  idsoccer_slave: tipo_roms = (n: 'id10'; l: $4000; p: 0; crc: $6C8B2037);
  idsoccer_misc: tipo_roms = (n: 'id_8p'; l: $200; p: 0; crc: $2747CA77);
  idsoccer_char: tipo_roms = (n: 'id05'; l: $4000; p: 0; crc: $A57C7A11);
  idsoccer_sprites: array [0 .. 3] of tipo_roms = ((n: 'id06'; l: $8000; p: 0; crc: $B42A6F4A), (n: 'id07'; l: $8000; p: $8000; crc: $FA2B1C77), (n: 'id08'; l: $8000; p: $10000; crc: $5E97EAB9), (n: 'id09'; l: $8000; p: $18000; crc: $A2A69223));
  idsoccer_pal: tipo_roms = (n: 'id_3d.clr'; l: $200; p: 0; crc: $A433FF62);
  idsoccer_adpcm: array [0 .. 2] of tipo_roms = ((n: 'is1'; l: $4000; p: 0; crc: $9EB76196), (n: 'is3'; l: $4000; p: $8000; crc: $27BEBBA3), (n: 'is4'; l: $4000; p: $C000; crc: $DD5FFAA2));
  // Dip
  mrdocastle_dip_a: array [0 .. 6] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: '1 (Beginner)'), (dip_val: $2; dip_name: '2'), (dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '4 (Advanced)'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'Rack Test (Cheat)'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Advance Level on Getting Diamond'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Difficulty of EXTRA'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Difficult'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $80; dip_name: '4'), (dip_val: $40; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  mrdocastle_dip_b: array [0 .. 2] of def_dip = ((mask: $F0; name: 'Coin A'; number: 11; dip: ((dip_val: $60; dip_name: '4C 1C'), (dip_val: $80; dip_name: '3C 1C'), (dip_val: $A0; dip_name: '2C 1C'), (dip_val: $70; dip_name: '3C 2C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $90; dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $D0; dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), ())), (mask: $0F;
    name: 'Coin B'; number: 11; dip: ((dip_val: $06; dip_name: '4C 1C'), (dip_val: $08; dip_name: '3C 1C'), (dip_val: $0A; dip_name: '2C 1C'), (dip_val: $07; dip_name: '3C 2C'), (dip_val: $0F; dip_name: '1C 1C'), (dip_val: $09; dip_name: '2C 3C'), (dip_val: $0E;
    dip_name: '1C 2C'), (dip_val: $0D; dip_name: '1C 3C'), (dip_val: $0C; dip_name: '1C 4C'), (dip_val: $0B; dip_name: '1C 5C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), ())), ());
  dorunrun_dip_a: array [0 .. 7] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: '1 (Beginner)'), (dip_val: $2; dip_name: '2'), (dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '4 (Advanced)'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $4; name: 'Demo Sounds'; number: 2; dip: ((dip_val: 0; dip_name: 'Off'), (dip_val: $4; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Difficulty of EXTRA'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Difficult'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Special'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Given'), (dip_val: $0; dip_name: 'Not Given'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Lives'; number: 2;
    dip: ((dip_val: $80; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  dowild_dip_a: array [0 .. 7] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: '1 (Beginner)'), (dip_val: $2; dip_name: '2'), (dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '4 (Advanced)'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $4; name: 'Rack Test (Cheat)'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Difficulty of EXTRA'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Difficult'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Special'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Given'), (dip_val: $0; dip_name: 'Not Given'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Lives'; number: 2;
    dip: ((dip_val: $80; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  jjack_dip_a: array [0 .. 6] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Medium'), (dip_val: $1; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Rack Test (Cheat)'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Extra'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $80; dip_name: '4'), (dip_val: $40; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  kickridr_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Medium'), (dip_val: $1; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $4; name: 'Rack Test (Cheat)'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  idsoccer_dip_a: array [0 .. 6] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Medium'), (dip_val: $1; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $4; name: 'One Player vs. Computer'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Player 2 Time Extension'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Player 1 Time Extension'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Real Game Time'; number: 4;
    dip: ((dip_val: $C0; dip_name: '3:00'), (dip_val: $80; dip_name: '2:30'), (dip_val: $40; dip_name: '2:00'), (dip_val: $0; dip_name: '1:00'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 4;

var
  buffer0, buffer1: array [0 .. 8] of byte;
  buf_input, adpcm_status: byte;
  sprite_ram: array [0 .. $1FF] of byte;
  draw_sprites: procedure;

procedure docastle_draw_sprites;
var
  color: word;
  nchar, f, x, y, atrib: byte;
begin
  for f := 0 to $7F do
  begin
    nchar := sprite_ram[(f * 4) + 3];
    atrib := sprite_ram[(f * 4) + 2];
    color := (atrib and $1F) shl 4;
    x := ((sprite_ram[(f * 4) + 1] + 8) and $FF) - 8;
    y := sprite_ram[(f * 4) + 0];
    put_gfx_sprite(nchar, color, (atrib and $40) <> 0, (atrib and $80) <> 0, 1);
    update_gfx_sprite(x, y, 3, 1);
  end;
end;

procedure idsoccer_draw_sprites;
var
  nchar: word;
  color, f, x, y, atrib: byte;
begin
  for f := 0 to $7F do
  begin
    atrib := sprite_ram[(f * 4) + 2];
    nchar := sprite_ram[(f * 4) + 3] + ((atrib and $10) shl 4) + ((atrib and $80) shl 2);
    color := (atrib and $F) shl 4;
    x := ((sprite_ram[(f * 4) + 1] + 8) and $FF) - 8;
    y := sprite_ram[(f * 4) + 0];
    put_gfx_sprite(nchar, color, (atrib and $40) <> 0, false, 1);
    update_gfx_sprite(x, y, 3, 1);
  end;
end;

procedure update_video_mrdocastle;
var
  f, color, nchar: word;
  x, y, atrib: byte;
begin
  for f := $0 to $3FF do
  begin
    x := f mod 32;
    y := f div 32;
    if gfx[0].buffer[f] then
    begin
      atrib := memory[$B400 + f];
      nchar := memory[$B000 + f] + ((atrib and $20) shl 3);
      color := (atrib and $1F) shl 4;
      put_gfx(x * 8, y * 8, nchar, color, 1, 0);
      put_gfx_trans(x * 8, y * 8, nchar, color, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  draw_sprites;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(8, 32, 240, 192, 3);
end;

procedure events_mrdocastle;
begin
  if event.arcade then
  begin
    // joy
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // but
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // joy2
    if p_contrls.map_arcade.right[1] then
      marcade.in3 := (marcade.in3 and $FE)
    else
      marcade.in3 := (marcade.in3 or $1);
    if p_contrls.map_arcade.up[1] then
      marcade.in3 := (marcade.in3 and $FD)
    else
      marcade.in3 := (marcade.in3 or $2);
    if p_contrls.map_arcade.left[1] then
      marcade.in3 := (marcade.in3 and $FB)
    else
      marcade.in3 := (marcade.in3 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in3 := (marcade.in3 and $F7)
    else
      marcade.in3 := (marcade.in3 or $8);
    if p_contrls.map_arcade.right[0] then
      marcade.in3 := (marcade.in3 and $EF)
    else
      marcade.in3 := (marcade.in3 or $10);
    if p_contrls.map_arcade.up[0] then
      marcade.in3 := (marcade.in3 and $DF)
    else
      marcade.in3 := (marcade.in3 or $20);
    if p_contrls.map_arcade.left[0] then
      marcade.in3 := (marcade.in3 and $BF)
    else
      marcade.in3 := (marcade.in3 or $40);
    if p_contrls.map_arcade.down[0] then
      marcade.in3 := (marcade.in3 and $7F)
    else
      marcade.in3 := (marcade.in3 or $80);
  end;
end;

procedure mrdocastle_loop;
var
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        events_mrdocastle;
        case f of
          14, 40, 72, 104, 136, 168, 200, 232:
            z80_1.change_irq(HOLD_LINE);
          224:
            begin
              z80_0.change_irq(HOLD_LINE);
              z80_2.change_nmi(PULSE_LINE);
              update_video_mrdocastle;
            end;
        end;
        for h := 1 to CPU_SYNC do
        begin
          z80_0.run(frame_main);
          frame_main := frame_main + z80_0.tframes - z80_0.contador;
          z80_1.run(frame_snd);
          frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
          z80_2.run(frame_sub);
          frame_sub := frame_sub + z80_2.tframes - z80_2.contador;
        end;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Mr Do Castler
function mrdocastle_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $97FF:
      mrdocastle_getbyte := memory[direccion];
    $9800 .. $99FF:
      mrdocastle_getbyte := sprite_ram[direccion - $9800];
    $A000 .. $A008:
      mrdocastle_getbyte := buffer0[direccion - $A000];
    $B000 .. $B3FF, $B800 .. $BBFF:
      mrdocastle_getbyte := memory[$B000 + (direccion and $3FF)];
    $B400 .. $B7FF, $BC00 .. $BFFF:
      mrdocastle_getbyte := memory[$B400 + (direccion and $3FF)];
  end;
end;

procedure mrdocastle_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $97FF:
      memory[direccion] := valor;
    $9800 .. $99FF:
      sprite_ram[direccion - $9800] := valor;
    $A000 .. $A007:
      buffer1[direccion and 7] := valor;
    $A008:
      begin
        buffer1[8] := valor;
        z80_0.change_halt(ASSERT_LINE);
      end;
    $B000 .. $B3FF, $B800 .. $BBFF:
      if memory[$B000 + (direccion and $3FF)] <> valor then
      begin
        memory[$B000 + (direccion and $3FF)] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $B400 .. $B7FF, $BC00 .. $BFFF:
      if memory[$B400 + (direccion and $3FF)] <> valor then
      begin
        memory[$B400 + (direccion and $3FF)] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $E000:
      z80_1.change_nmi(PULSE_LINE);
  end;
end;

function mrdocastle_getbyte_slave(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $87FF:
      mrdocastle_getbyte_slave := mem_snd[direccion];
    $A000 .. $A008:
      mrdocastle_getbyte_slave := buffer1[direccion - $A000];
    $C000 .. $C007, $C080 .. $C087:
      begin
        case buf_input of
          0:
            mrdocastle_getbyte_slave := marcade.dswb; // dsw2
          1:
            mrdocastle_getbyte_slave := marcade.dswa; // dsw1
          2:
            mrdocastle_getbyte_slave := marcade.in0; // joy
          3:
            mrdocastle_getbyte_slave := marcade.in3; // joy2
          5, 7:
            mrdocastle_getbyte_slave := $FF;
          4:
            mrdocastle_getbyte_slave := marcade.in1; // but
          6:
            mrdocastle_getbyte_slave := marcade.in2; // system
        end;
        main_screen.flip_main_screen := (direccion and $80) <> 0;
        buf_input := (direccion - 1) and $7;
      end;
  end;
end;

procedure mrdocastle_putbyte_slave(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $A000 .. $A007:
      buffer0[direccion and 7] := valor;
    $A008:
      begin
        buffer0[8] := valor;
        z80_0.change_halt(CLEAR_LINE);
      end;
    $E000:
      sn_76496_0.Write(valor);
    $E400:
      sn_76496_1.Write(valor);
    $E800:
      sn_76496_2.Write(valor);
    $EC00:
      sn_76496_3.Write(valor);
  end;
end;

function mrdocastle_getbyte_misc(direccion: word): byte;
begin
  case direccion of
    0 .. $FF, $4000 .. $47FF:
      mrdocastle_getbyte_misc := mem_misc[direccion];
    $8000 .. $8008:
      mrdocastle_getbyte_misc := buffer1[direccion - $8000];
  end;
end;

procedure mrdocastle_putbyte_misc(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FF:
      ;
    $4000 .. $47FF:
      mem_misc[direccion] := valor;
  end;
end;

procedure mrdocastle_update_sound;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
  sn_76496_2.Update;
  sn_76496_3.Update;
end;

// Do Run Run
function dorunrun_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $37FF, $4000 .. $9FFF:
      dorunrun_getbyte := memory[direccion];
    $3800 .. $39FF:
      dorunrun_getbyte := sprite_ram[direccion - $3800];
    $A000 .. $A008:
      dorunrun_getbyte := buffer0[direccion - $A000];
    $B000 .. $B3FF:
      dorunrun_getbyte := memory[direccion and $3FF];
    $B400 .. $B7FF:
      dorunrun_getbyte := memory[direccion and $3FF];
  end;
end;

procedure dorunrun_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF, $4000 .. $9FFF:
      ;
    $2000 .. $37FF:
      memory[direccion] := valor;
    $3800 .. $39FF:
      sprite_ram[direccion - $3800] := valor;
    $A000 .. $A007:
      buffer1[direccion and 7] := valor;
    $A008:
      begin
        buffer1[8] := valor;
        z80_0.change_halt(ASSERT_LINE);
      end;
    $B000 .. $B7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $B800:
      z80_1.change_nmi(PULSE_LINE);
  end;
end;

function dorunrun_getbyte_slave(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $87FF:
      dorunrun_getbyte_slave := mem_snd[direccion];
    $C000 .. $C007, $C080 .. $C087:
      begin
        case buf_input of
          0:
            dorunrun_getbyte_slave := marcade.dswb; // dsw2
          1:
            dorunrun_getbyte_slave := marcade.dswa; // dsw1
          2:
            dorunrun_getbyte_slave := marcade.in0; // joy
          3, 5, 7:
            dorunrun_getbyte_slave := $FF;
          4:
            dorunrun_getbyte_slave := marcade.in1; // but
          6:
            dorunrun_getbyte_slave := marcade.in2; // system
        end;
        main_screen.flip_main_screen := (direccion and $80) <> 0;
        buf_input := (direccion - 1) and $7;
      end;
    $E000 .. $E008:
      dorunrun_getbyte_slave := buffer1[direccion - $E000];
  end;
end;

procedure dorunrun_putbyte_slave(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $A000:
      sn_76496_0.Write(valor);
    $A400:
      sn_76496_1.Write(valor);
    $A800:
      sn_76496_2.Write(valor);
    $AC00:
      sn_76496_3.Write(valor);
    $E000 .. $E007:
      buffer0[direccion and 7] := valor;
    $E008:
      begin
        buffer0[8] := valor;
        z80_0.change_halt(CLEAR_LINE);
      end;
  end;
end;

// Indoor soccer
function idsoccer_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $57FF, $6000 .. $9FFF:
      idsoccer_getbyte := memory[direccion];
    $5800 .. $59FF:
      idsoccer_getbyte := sprite_ram[direccion - $5800];
    $A000 .. $A008:
      idsoccer_getbyte := buffer0[direccion - $A000];
    $B000 .. $B3FF, $B800 .. $BBFF:
      idsoccer_getbyte := memory[$B000 + (direccion and $3FF)];
    $B400 .. $B7FF, $BC00 .. $BFFF:
      idsoccer_getbyte := memory[$B400 + (direccion and $3FF)];
    $C000:
      begin
        adpcm_status := adpcm_status xor $80;
        idsoccer_getbyte := adpcm_status;
      end;
  end;
end;

procedure idsoccer_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF, $6000 .. $9FFF:
      ;
    $4000 .. $57FF:
      memory[direccion] := valor;
    $5800 .. $59FF:
      sprite_ram[direccion - $5800] := valor;
    $A000 .. $A007:
      buffer1[direccion and 7] := valor;
    $A008:
      begin
        buffer1[8] := valor;
        z80_0.change_halt(ASSERT_LINE);
      end;
    $B000 .. $B3FF, $B800 .. $BBFF:
      if memory[$B000 + (direccion and $3FF)] <> valor then
      begin
        memory[$B000 + (direccion and $3FF)] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $B400 .. $B7FF, $BC00 .. $BFFF:
      if memory[$B400 + (direccion and $3FF)] <> valor then
      begin
        memory[$B400 + (direccion and $3FF)] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $C000:
      if (valor and $80) <> 0 then
      begin
        msm5205_0.reset_w(true);
      end
      else
      begin
        msm5205_0.pos := (valor and $7F) * $200;
        msm5205_0.reset_w(false);
      end;
    $E000:
      z80_1.change_nmi(PULSE_LINE);
  end;
end;

procedure snd_adpcm;
begin
  if (msm5205_0.data_val <> -1) then
  begin
    msm5205_0.data_w(msm5205_0.data_val and $F);
    msm5205_0.data_val := -1;
    msm5205_0.pos := msm5205_0.pos + 1;
    if (msm5205_0.pos + 1) = $10000 then
      msm5205_0.reset_w(true)
  end
  else
  begin
    msm5205_0.data_val := msm5205_0.rom_data[msm5205_0.pos];
    msm5205_0.data_w(msm5205_0.data_val shr 4);
  end;
end;

procedure idoor_update_sound;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
  sn_76496_2.Update;
  sn_76496_3.Update;
  msm5205_0.Update;
end;

// Main
procedure reset_mrdocastle;
begin
  z80_0.reset;
  z80_1.reset;
  z80_2.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  sn_76496_2.reset;
  sn_76496_3.reset;
  if (main_vars.machine_type = 313) then
    msm5205_0.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  frame_sub := z80_2.tframes;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  marcade.in3 := $FF;
  fillchar(buffer0, 9, 0);
  fillchar(buffer1, 9, 0);
  buf_input := 0;
  adpcm_status := 0;
end;

function start_mrdocastle: boolean;
var
  colores: tpaleta;
  memory_temp: array [0 .. $1FFFF] of byte;
  f, ctemp1, ctemp2, ctemp3: byte;
  pos: word;
const
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 8 * 4, 9 * 4, 10 * 4, 11 * 4, 12 * 4, 13 * 4, 14 * 4, 15 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
  procedure conv_chars;
  begin
    init_gfx(0, 8, 8, $200);
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, false, false);
  end;
  procedure conv_sprites(size: word);
  begin
    init_gfx(1, 16, 16, size);
    gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := mrdocastle_loop;
  machine_calls.reset := reset_mrdocastle;
  machine_calls.fps_max := 59.659092;
  start_mrdocastle := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  if ((main_vars.machine_type = 308) or (main_vars.machine_type = 311)) then
    main_screen.rot270_screen := true;
  start_video(240, 192);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 264 * CPU_SYNC);
  // Slave CPU
  z80_1 := cpu_z80.create(4000000, 264 * CPU_SYNC);
  if (main_vars.machine_type = 313) then
    z80_1.init_sound(idoor_update_sound)
  else
    z80_1.init_sound(mrdocastle_update_sound);
  // Tercera CPU
  z80_2 := cpu_z80.create(4000000, 264 * CPU_SYNC);
  z80_2.change_ram_calls(mrdocastle_getbyte_misc, mrdocastle_putbyte_misc);
  // Sound Chips
  sn_76496_0 := sn76496_chip.create(4000000);
  sn_76496_1 := sn76496_chip.create(4000000);
  sn_76496_2 := sn76496_chip.create(4000000);
  sn_76496_3 := sn76496_chip.create(4000000);
  draw_sprites := docastle_draw_sprites;
  case main_vars.machine_type of
    308:
      begin // Mr Do Castle
        z80_0.change_ram_calls(mrdocastle_getbyte, mrdocastle_putbyte);
        if not(roms_load(@memory, mrdocastle_rom)) then
          exit;
        z80_1.change_ram_calls(mrdocastle_getbyte_slave, mrdocastle_putbyte_slave);
        if not(roms_load(@mem_snd, mrdocastle_slave)) then
          exit;
        if not(roms_load(@mem_misc, mrdocastle_misc)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, mrdocastle_char)) then
          exit;
        conv_chars;
        for f := 0 to 7 do
          gfx[0].trans[f] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, mrdocastle_sprites)) then
          exit;
        conv_sprites($100);
        gfx[1].trans[0] := true;
        // dip
        marcade.dswa := $DF;
        marcade.dswb := $FF;
        marcade.dswa_val := @mrdocastle_dip_a;
        marcade.dswb_val := @mrdocastle_dip_b;
        if not(roms_load(@memory_temp, mrdocastle_pal)) then
          exit;
      end;
    309:
      begin // Do! Run Run
        z80_0.change_ram_calls(dorunrun_getbyte, dorunrun_putbyte);
        if not(roms_load(@memory, dorunrun_rom)) then
          exit;
        z80_1.change_ram_calls(dorunrun_getbyte_slave, dorunrun_putbyte_slave);
        if not(roms_load(@mem_snd, dorunrun_slave)) then
          exit;
        if not(roms_load(@mem_misc, dorunrun_misc)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, dorunrun_char)) then
          exit;
        conv_chars;
        for f := 8 to $F do
          gfx[0].trans[f] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, dorunrun_sprites)) then
          exit;
        conv_sprites($100);
        gfx[1].trans[7] := true;
        // dip
        marcade.dswa := $DF;
        marcade.dswb := $FF;
        marcade.dswa_val := @dorunrun_dip_a;
        marcade.dswb_val := @mrdocastle_dip_b;
        if not(roms_load(@memory_temp, dorunrun_pal)) then
          exit;
      end;
    310:
      begin // Mr Do wild ride
        z80_0.change_ram_calls(dorunrun_getbyte, dorunrun_putbyte);
        if not(roms_load(@memory, dowild_rom)) then
          exit;
        z80_1.change_ram_calls(dorunrun_getbyte_slave, dorunrun_putbyte_slave);
        if not(roms_load(@mem_snd, dowild_slave)) then
          exit;
        if not(roms_load(@mem_misc, dowild_misc)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, dowild_char)) then
          exit;
        conv_chars;
        for f := 8 to $F do
          gfx[0].trans[f] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, dowild_sprites)) then
          exit;
        conv_sprites($100);
        gfx[1].trans[0] := true;
        gfx[1].trans[7] := true;
        // dip
        marcade.dswa := $DF;
        marcade.dswb := $FF;
        marcade.dswa_val := @dowild_dip_a;
        marcade.dswb_val := @mrdocastle_dip_b;
        if not(roms_load(@memory_temp, dowild_pal)) then
          exit;
      end;
    311:
      begin // Jumping Jack
        z80_0.change_ram_calls(dorunrun_getbyte, dorunrun_putbyte);
        if not(roms_load(@memory, jjack_rom)) then
          exit;
        z80_1.change_ram_calls(dorunrun_getbyte_slave, dorunrun_putbyte_slave);
        if not(roms_load(@mem_snd, jjack_slave)) then
          exit;
        if not(roms_load(@mem_misc, jjack_misc)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, jjack_char)) then
          exit;
        conv_chars;
        for f := 8 to $F do
          gfx[0].trans[f] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, jjack_sprites)) then
          exit;
        conv_sprites($100);
        gfx[1].trans[0] := true;
        gfx[1].trans[$F] := true;
        // dip
        marcade.dswa := $DF;
        marcade.dswb := $FF;
        marcade.dswa_val := @jjack_dip_a;
        marcade.dswb_val := @mrdocastle_dip_b;
        if not(roms_load(@memory_temp, jjack_pal)) then
          exit;
      end;
    312:
      begin // Kick Rider
        z80_0.change_ram_calls(dorunrun_getbyte, dorunrun_putbyte);
        if not(roms_load(@memory, kickridr_rom)) then
          exit;
        z80_1.change_ram_calls(dorunrun_getbyte_slave, dorunrun_putbyte_slave);
        if not(roms_load(@mem_snd, kickridr_slave)) then
          exit;
        if not(roms_load(@mem_misc, kickridr_misc)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, kickridr_char)) then
          exit;
        conv_chars;
        for f := 8 to $F do
          gfx[0].trans[f] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, kickridr_sprites)) then
          exit;
        conv_sprites($100);
        gfx[1].trans[7] := true;
        // dip
        marcade.dswa := $DF;
        marcade.dswb := $FF;
        marcade.dswa_val := @kickridr_dip_a;
        marcade.dswb_val := @mrdocastle_dip_b;
        if not(roms_load(@memory_temp, kickridr_pal)) then
          exit;
      end;
    313:
      begin // Indoor Soccer
        draw_sprites := idsoccer_draw_sprites;
        z80_0.change_ram_calls(idsoccer_getbyte, idsoccer_putbyte);
        if not(roms_load(@memory, idsoccer_rom)) then
          exit;
        z80_1.change_ram_calls(mrdocastle_getbyte_slave, mrdocastle_putbyte_slave);
        if not(roms_load(@mem_snd, idsoccer_slave)) then
          exit;
        if not(roms_load(@mem_misc, idsoccer_misc)) then
          exit;
        msm5205_0 := MSM5205_chip.create(384000, MSM5205_S64_4B, 0.4, $10000);
        msm5205_0.change_advance(snd_adpcm);
        if not(roms_load(msm5205_0.rom_data, idsoccer_adpcm)) then
          exit;
        exit;
        // convertir chars
        if not(roms_load(@memory_temp, idsoccer_char)) then
          exit;
        conv_chars;
        for f := 8 to $F do
          gfx[0].trans[f] := true;
        // convertir sprites
        if not(roms_load(@memory_temp, idsoccer_sprites)) then
          exit;
        conv_sprites($400);
        gfx[1].trans[7] := true;
        // dip
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswa_val := @idsoccer_dip_a;
        marcade.dswb_val := @mrdocastle_dip_b;
        if not(roms_load(@memory_temp, idsoccer_pal)) then
          exit;
      end;
  end;
  // pal
  for f := 0 to 255 do
  begin
    pos := ((f and $F8) shl 1) or (f and $07);
    ctemp1 := (memory_temp[f] shr 5) and 1;
    ctemp2 := (memory_temp[f] shr 6) and 1;
    ctemp3 := (memory_temp[f] shr 7) and 1;
    colores[pos].r := $23 * ctemp1 + $4B * ctemp2 + $91 * ctemp3;
    colores[pos or 8].r := $23 * ctemp1 + $4B * ctemp2 + $91 * ctemp3;
    ctemp1 := (memory_temp[f] shr 2) and 1;
    ctemp2 := (memory_temp[f] shr 3) and 1;
    ctemp3 := (memory_temp[f] shr 4) and 1;
    colores[pos].g := $23 * ctemp1 + $4B * ctemp2 + $91 * ctemp3;
    colores[pos or 8].g := $23 * ctemp1 + $4B * ctemp2 + $91 * ctemp3;
    ctemp1 := 0;
    ctemp2 := (memory_temp[f] shr 0) and 1;
    ctemp3 := (memory_temp[f] shr 1) and 1;
    colores[pos].b := $23 * ctemp1 + $4B * ctemp2 + $91 * ctemp3;
    colores[pos or 8].b := $23 * ctemp1 + $4B * ctemp2 + $91 * ctemp3;
  end;
  set_pal(colores, 512);
  // final
  start_mrdocastle := true;
end;

end.
