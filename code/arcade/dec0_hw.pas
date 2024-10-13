unit dec0_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  ym_2203,
  ym_3812,
  oki6295,
  m6502,
  sound_engine,
  hu6280,
  misc_functions,
  deco_bac06,
  mcs51;

function start_dec0: boolean;

implementation

const
  // Robocop
  robocop_rom: array [0 .. 3] of tipo_roms = ((n: 'ep05-4.11c'; l: $10000; p: 0; crc: $29C35379), (n: 'ep01-4.11b'; l: $10000; p: $1; crc: $77507C69), (n: 'ep04-3'; l: $10000; p: $20000;
    crc: $39181778), (n: 'ep00-3'; l: $10000; p: $20001; crc: $E128541F));
  robocop_mcu: tipo_roms = (n: 'en_24_mb7124e.a2'; l: $200; p: $0; crc: $B8E2CA98);
  robocop_char: array [0 .. 1] of tipo_roms = ((n: 'ep23'; l: $10000; p: 0; crc: $A77E4AB1), (n: 'ep22'; l: $10000; p: $10000; crc: $9FBD6903));
  robocop_sound: tipo_roms = (n: 'ep03-3'; l: $8000; p: $8000; crc: $5B164B24);
  robocop_oki: tipo_roms = (n: 'ep02'; l: $10000; p: 0; crc: $711CE46F);
  robocop_tiles1: array [0 .. 3] of tipo_roms = ((n: 'ep20'; l: $10000; p: 0; crc: $1D8D38B8), (n: 'ep21'; l: $10000; p: $10000; crc: $187929B2), (n: 'ep18'; l: $10000; p: $20000; crc: $B6580B5E),
    (n: 'ep19'; l: $10000; p: $30000; crc: $9BAD01C7));
  robocop_tiles2: array [0 .. 3] of tipo_roms = ((n: 'ep14'; l: $8000; p: 0; crc: $CA56CEDA), (n: 'ep15'; l: $8000; p: $8000; crc: $A945269C), (n: 'ep16'; l: $8000; p: $10000; crc: $E7FA4D58),
    (n: 'ep17'; l: $8000; p: $18000; crc: $84AAE89D));
  robocop_sprites: array [0 .. 7] of tipo_roms = ((n: 'ep07'; l: $10000; p: $00000; crc: $495D75CF), (n: 'ep06'; l: $8000; p: $10000; crc: $A2AE32E2), (n: 'ep11'; l: $10000; p: $20000;
    crc: $62FA425A), (n: 'ep10'; l: $8000; p: $30000; crc: $CCE3BD95), (n: 'ep09'; l: $10000; p: $40000; crc: $11BED656), (n: 'ep08'; l: $8000; p: $50000; crc: $C45C7B4C), (n: 'ep13'; l: $10000;
    p: $60000; crc: $8FCA9F28), (n: 'ep12'; l: $8000; p: $70000; crc: $3CD1D0C3));
  robocop_dip: array [0 .. 10] of def_dip = ((mask: $0003; name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $000C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $0020; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0040;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0080; name: 'Cabinet';
    number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0300; name: 'Player Energy'; number: 4;
    dip: ((dip_val: $100; dip_name: 'Low'), (dip_val: $300; dip_name: 'Medium'), (dip_val: $200; dip_name: 'High'), (dip_val: $0; dip_name: 'Very High'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $0C00; name: 'Difficulty'; number: 4; dip: ((dip_val: $800; dip_name: 'Easy'), (dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'Hard'), (dip_val: $000;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $1000; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $1000; dip_name: 'Yes'), (dip_val: $0; dip_name: 'No'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2000; name: 'Bonus Stage Energy'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Low'), (dip_val: $2000; dip_name: 'High'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4000; name: 'Brink Time'; number: 2;
    dip: ((dip_val: $4000; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Less'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Baddudes
  baddudes_rom: array [0 .. 3] of tipo_roms = ((n: 'ei04-1.3c'; l: $10000; p: 0; crc: $4BF158A7), (n: 'ei01-1.3a'; l: $10000; p: $1; crc: $74F5110C), (n: 'ei06.6c'; l: $10000; p: $40000;
    crc: $3FF8DA57), (n: 'ei03.6a'; l: $10000; p: $40001; crc: $F8F2BD94));
  baddudes_char: array [0 .. 1] of tipo_roms = ((n: 'ei25.15j'; l: $8000; p: 0; crc: $BCF59A69), (n: 'ei26.16j'; l: $8000; p: $8000; crc: $9AFF67B8));
  baddudes_mcu: tipo_roms = (n: 'ei31.9a'; l: $1000; p: $0; crc: $2A8745D2);
  baddudes_sound: tipo_roms = (n: 'ei07.8a'; l: $8000; p: $8000; crc: $9FB1EF4B);
  baddudes_oki: tipo_roms = (n: 'ei08.2c'; l: $10000; p: 0; crc: $3C87463E);
  baddudes_tiles1: array [0 .. 3] of tipo_roms = ((n: 'ei18.14d'; l: $10000; p: 0; crc: $05CFC3E5), (n: 'ei20.17d'; l: $10000; p: $10000; crc: $E11E988F), (n: 'ei22.14f'; l: $10000; p: $20000;
    crc: $B893D880), (n: 'ei24.17f'; l: $10000; p: $30000; crc: $6F226DDA));
  baddudes_tiles2: array [0 .. 1] of tipo_roms = ((n: 'ei30.9j'; l: $10000; p: $20000; crc: $982DA0D1), (n: 'ei28.9f'; l: $10000; p: $30000; crc: $F01EBB3B));
  baddudes_sprites: array [0 .. 7] of tipo_roms = ((n: 'ei15.16c'; l: $10000; p: $00000; crc: $A38A7D30), (n: 'ei16.17c'; l: $8000; p: $10000; crc: $17E42633), (n: 'ei11.16a'; l: $10000; p: $20000;
    crc: $3A77326C), (n: 'ei12.17a'; l: $8000; p: $30000; crc: $FEA2A134), (n: 'ei13.13c'; l: $10000; p: $40000; crc: $E5AE2751), (n: 'ei14.14c'; l: $8000; p: $50000; crc: $E83C760A), (n: 'ei09.13a';
    l: $10000; p: $60000; crc: $6901E628), (n: 'ei10.14a'; l: $8000; p: $70000; crc: $EEEE8A1A));
  baddudes_dip: array [0 .. 7] of def_dip = ((mask: $0003; name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $000C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $0020; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0040;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0300; name: 'Lives';
    number: 4; dip: ((dip_val: $100; dip_name: '1'), (dip_val: $300; dip_name: '3'), (dip_val: $200; dip_name: '5'), (dip_val: $0; dip_name: 'Infinite'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $0C00; name: 'Difficulty'; number: 4; dip: ((dip_val: $800; dip_name: 'Easy'), (dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'Hard'), (dip_val: $000;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $1000; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $1000; dip_name: 'Yes'), (dip_val: $0; dip_name: 'No'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Hippodrome
  hippo_rom: array [0 .. 3] of tipo_roms = ((n: 'ew02'; l: $10000; p: 0; crc: $DF0D7DC6), (n: 'ew01'; l: $10000; p: $1; crc: $D5670AA7), (n: 'ew05'; l: $10000; p: $20000; crc: $C76D65EC), (n: 'ew00';
    l: $10000; p: $20001; crc: $E9B427A6));
  hippo_mcu: tipo_roms = (n: 'ew08'; l: $10000; p: $0; crc: $53010534);
  hippo_char: array [0 .. 1] of tipo_roms = ((n: 'ew14'; l: $10000; p: 0; crc: $71CA593D), (n: 'ew13'; l: $10000; p: $10000; crc: $86BE5FA7));
  hippo_sound: tipo_roms = (n: 'ew04'; l: $8000; p: $8000; crc: $9871B98D);
  hippo_oki: tipo_roms = (n: 'ew03'; l: $10000; p: 0; crc: $B606924D);
  hippo_tiles1: array [0 .. 3] of tipo_roms = ((n: 'ew19'; l: $8000; p: 0; crc: $6B80D7A3), (n: 'ew18'; l: $8000; p: $8000; crc: $78D3D764), (n: 'ew20'; l: $8000; p: $10000; crc: $CE9F5DE3),
    (n: 'ew21'; l: $8000; p: $18000; crc: $487A7BA2));
  hippo_tiles2: array [0 .. 3] of tipo_roms = ((n: 'ew24'; l: $8000; p: 0; crc: $4E1BC2A4), (n: 'ew25'; l: $8000; p: $8000; crc: $9EB47DFB), (n: 'ew23'; l: $8000; p: $10000; crc: $9ECF479E),
    (n: 'ew22'; l: $8000; p: $18000; crc: $E55669AA));
  hippo_sprites: array [0 .. 7] of tipo_roms = ((n: 'ew15'; l: $10000; p: $00000; crc: $95423914), (n: 'ew16'; l: $10000; p: $10000; crc: $96233177), (n: 'ew10'; l: $10000; p: $20000; crc: $4C25DFE8),
    (n: 'ew11'; l: $10000; p: $30000; crc: $F2E007FC), (n: 'ew06'; l: $10000; p: $40000; crc: $E4BB8199), (n: 'ew07'; l: $10000; p: $50000; crc: $470B6989), (n: 'ew17'; l: $10000; p: $60000;
    crc: $8C97C757), (n: 'ew12'; l: $10000; p: $70000; crc: $A2D244BC));
  hippo_dip: array [0 .. 8] of def_dip = ((mask: $0003; name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $000C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $0020; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0040;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0300; name: 'Lives';
    number: 4; dip: ((dip_val: $100; dip_name: '1'), (dip_val: $300; dip_name: '3'), (dip_val: $200; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $0C00; name: 'Difficulty'; number: 4; dip: ((dip_val: $800; dip_name: 'Easy'), (dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'Hard'), (dip_val: $000;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $3000; name: 'Player & Enemy Energy'; number: 4;
    dip: ((dip_val: $1000; dip_name: 'Very Low'), (dip_val: $2000; dip_name: 'Low'), (dip_val: $3000; dip_name: 'Medium'), (dip_val: $0; dip_name: 'High'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $4000; name: 'Enemy Power Decrease on Continue'; number: 2; dip: ((dip_val: $4000; dip_name: '2 Dots'), (dip_val: $0; dip_name: '3 Dots'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), ());
  // Slyspy
  slyspy_rom: array [0 .. 3] of tipo_roms = ((n: 'fa14-4.17l'; l: $10000; p: 0; crc: $60F16E31), (n: 'fa12-4.9l'; l: $10000; p: $1; crc: $B9B9FDCF), (n: 'fa15.19l'; l: $10000; p: $20000;
    crc: $04A79266), (n: 'fa13.11l'; l: $10000; p: $20001; crc: $641CC4B3));
  slyspy_char: array [0 .. 1] of tipo_roms = ((n: 'fa05.11a'; l: $8000; p: 0; crc: $09802924), (n: 'fa04.9a'; l: $8000; p: $8000; crc: $EC25B895));
  slyspy_sound: tipo_roms = (n: 'fa10.5h'; l: $10000; p: $0; crc: $DFD2FF25);
  slyspy_oki: tipo_roms = (n: 'fa11.11k'; l: $20000; p: 0; crc: $4E547BAD);
  slyspy_tiles1: array [0 .. 1] of tipo_roms = ((n: 'fa07.17a'; l: $10000; p: $0; crc: $E932268B), (n: 'fa06.15a'; l: $10000; p: $10000; crc: $C4DD38C0));
  slyspy_tiles2: array [0 .. 1] of tipo_roms = ((n: 'fa09.22a'; l: $20000; p: $0; crc: $1395E9BE), (n: 'fa08.21a'; l: $20000; p: $20000; crc: $4D7464DB));
  slyspy_sprites: array [0 .. 3] of tipo_roms = ((n: 'fa01.4a'; l: $20000; p: $0; crc: $99B0CD92), (n: 'fa03.7a'; l: $20000; p: $20000; crc: $0E7EA74D), (n: 'fa00.2a'; l: $20000; p: $40000;
    crc: $F7DF3FD7), (n: 'fa02.5a'; l: $20000; p: $60000; crc: $84E8DA9D));
  slyspy_dip: array [0 .. 8] of def_dip = ((mask: $0003; name: 'Coin A'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $000C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $0020; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0040;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0080; name: 'Cabinet';
    number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0300; name: 'Energy'; number: 4;
    dip: ((dip_val: $200; dip_name: 'Low - 8 bars'), (dip_val: $300; dip_name: 'Medium - 10 bars'), (dip_val: $100; dip_name: 'High - 12 bars'), (dip_val: $0;
    dip_name: 'Very High - 14 bars'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0C00; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $800; dip_name: 'Easy'), (dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'Hard'), (dip_val: $000; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $1000; name: 'Allow Continue'; number: 2; dip: ((dip_val: $1000; dip_name: 'Yes'), (dip_val: $0; dip_name: 'No'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Boulder Dash
  bouldash_rom: array [0 .. 5] of tipo_roms = ((n: 'fw-15-2.17l'; l: $10000; p: 0; crc: $CA19A967), (n: 'fw-12-2.9l'; l: $10000; p: $1; crc: $242BDC2A), (n: 'fw-16-2.19l'; l: $10000; p: $20000;
    crc: $B7217265), (n: 'fw-13-2.11l'; l: $10000; p: $20001; crc: $19209EF4), (n: 'fw-17-2.20l'; l: $10000; p: $40000; crc: $78A632A1), (n: 'fw-14-2.13l'; l: $10000; p: $40001; crc: $69B6112D));
  bouldash_char: array [0 .. 1] of tipo_roms = ((n: 'fn-04'; l: $10000; p: 0; crc: $40F5A760), (n: 'fn-05'; l: $10000; p: $10000; crc: $824F2168));
  bouldash_sound: tipo_roms = (n: 'fn-10'; l: $10000; p: $0; crc: $C74106E7);
  bouldash_oki: tipo_roms = (n: 'fn-11'; l: $10000; p: 0; crc: $990FD8D9);
  bouldash_tiles1: array [0 .. 1] of tipo_roms = ((n: 'fn-07'; l: $10000; p: $0; crc: $EAC6A3B3), (n: 'fn-06'; l: $10000; p: $10000; crc: $3FEEE292));
  bouldash_tiles2: array [0 .. 1] of tipo_roms = ((n: 'fn-09'; l: $20000; p: $0; crc: $C2B27BD2), (n: 'fn-08'; l: $20000; p: $20000; crc: $5AC97178));
  bouldash_sprites: array [0 .. 3] of tipo_roms = ((n: 'fn-01'; l: $10000; p: $0; crc: $9333121B), (n: 'fn-03'; l: $10000; p: $10000; crc: $254BA60F), (n: 'fn-00'; l: $10000; p: $20000;
    crc: $EC18D098), (n: 'fn-02'; l: $10000; p: $30000; crc: $4F060CBA));
  bouldash_dip: array [0 .. 9] of def_dip = ((mask: $0007; name: 'Coin A'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $07;
    dip_name: '1C 1C'), (dip_val: $06; dip_name: '1C 2C'), (dip_val: $05; dip_name: '1C 3C'), (dip_val: $04; dip_name: '1C 4C'), (dip_val: $03; dip_name: '1C 5C'), (dip_val: $02;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $0038; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $8; dip_name: '2C 1C'), (dip_val: $38;
    dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28; dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 5C'), (dip_val: $10;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $0040; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0080; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0300; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $300; dip_name: '3'), (dip_val: $200; dip_name: '4'), (dip_val: $100; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0C00;
    name: 'Difficulty'; number: 4; dip: ((dip_val: $800; dip_name: 'Easy'), (dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'Hard'), (dip_val: $000;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2000; name: 'Game Change Mode'; number: 2;
    dip: ((dip_val: $2000; dip_name: 'Part 1'), (dip_val: $0; dip_name: 'Part 2'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4000; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $4000; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8000; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $8000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  rom: array [0 .. $2FFFF] of word;
  ram1: array [0 .. $FFF] of word;
  ram2: array [0 .. $1FFF] of word;
  sound_latch, prioridad, hippodrm_lsb, slyspy_state, slyspy_sound_state: byte;
  // HU 6280
  mcu_ram, mcu_shared_ram: array [0 .. $1FFF] of byte;
  // 8751
  i8751_return, i8751_command: word;
  i8751_ports: array [0 .. 3] of byte;

procedure update_video_robocop;
var
  trans: byte;
begin
  trans := (prioridad and $4) shl 1;
  if (prioridad and 1) <> 0 then
  begin
    bac06_0.tile_2.update_pf(1, false, false);
    bac06_0.tile_3.update_pf(2, true, false);
    // Pri pant
    bac06_0.tile_2.show_pf;
    if (prioridad and $02) <> 0 then
      bac06_0.draw_sprites($8, trans, 3);
    bac06_0.tile_3.show_pf;
  end
  else
  begin // invertidas
    bac06_0.tile_3.update_pf(2, false, false);
    bac06_0.tile_2.update_pf(1, true, false);
    // Pri pant
    bac06_0.tile_3.show_pf;
    if (prioridad and $02) <> 0 then
      bac06_0.draw_sprites($8, trans, 3);
    bac06_0.tile_2.show_pf;
  end;
  if (prioridad and $02) <> 0 then
    bac06_0.draw_sprites($8, trans xor $08, 3)
  else
    bac06_0.draw_sprites(0, 0, 3);
  // chars
  bac06_0.tile_1.update_pf(0, true, false);
  bac06_0.tile_1.show_pf;
  update_final_piece(0, 8, 256, 240, 7);
end;

procedure update_video_baddudes;
begin
  if (prioridad and 1) = 0 then
  begin
    bac06_0.tile_2.update_pf(1, false, true);
    bac06_0.tile_3.update_pf(2, true, true);
    // Pri pant
    bac06_0.tile_2.show_pf;
    bac06_0.tile_3.show_pf;
    // prioridades
    if (prioridad and $2) <> 0 then
      bac06_0.tile_2.show_pf_pri;
    bac06_0.draw_sprites(0, 0, 3);
    if (prioridad and $4) <> 0 then
      bac06_0.tile_3.show_pf_pri;
  end
  else
  begin // invertidas
    bac06_0.tile_3.update_pf(2, false, true);
    bac06_0.tile_2.update_pf(1, true, true);
    // Pri pant
    bac06_0.tile_3.show_pf;
    bac06_0.tile_2.show_pf;
    // prioridades
    if (prioridad and $2) <> 0 then
      bac06_0.tile_3.show_pf_pri;
    bac06_0.draw_sprites(0, 0, 3);
    if (prioridad and $4) <> 0 then
      bac06_0.tile_2.show_pf_pri;
  end;
  // chars
  bac06_0.tile_1.update_pf(0, true, false);
  bac06_0.tile_1.show_pf;
  update_final_piece(0, 8, 256, 240, 7);
end;

procedure update_video_hippo;
begin
  if (prioridad and 1) <> 0 then
  begin
    bac06_0.tile_2.update_pf(1, false, false);
    bac06_0.tile_3.update_pf(2, true, false);
    // Pri pant
    bac06_0.tile_2.show_pf;
    bac06_0.tile_3.show_pf;
  end
  else
  begin // invertidas
    bac06_0.tile_3.update_pf(2, false, false);
    bac06_0.tile_2.update_pf(1, true, false);
    // Pri pant
    bac06_0.tile_3.show_pf;
    bac06_0.tile_2.show_pf;
  end;
  bac06_0.draw_sprites(0, 0, 3);
  // chars
  bac06_0.tile_1.update_pf(0, true, false);
  bac06_0.tile_1.show_pf;
  update_final_piece(0, 8, 256, 240, 7);
end;

procedure update_video_slyspy;
begin
  bac06_0.tile_3.update_pf(2, false, false);
  bac06_0.tile_3.show_pf;
  bac06_0.tile_2.update_pf(1, true, true);
  bac06_0.tile_2.show_pf;
  bac06_0.draw_sprites(0, 0, 3);
  if (prioridad and $80) <> 0 then
    bac06_0.tile_2.show_pf_pri;
  bac06_0.tile_1.update_pf(0, true, false);
  bac06_0.tile_1.show_pf;
  update_final_piece(0, 8, 256, 240, 7);
end;

procedure events_dec0;
begin
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $8);
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
    if p_contrls.map_arcade.but3[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.but3[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // SYSTEM
    if p_contrls.map_arcade.but4[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.but4[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
  end;
end;

procedure events_dec1;
begin
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $8);
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
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7EFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or $2);
  end;
end;

procedure baddudes_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        m6502_0.run(frame_s);
        frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        mcs51_0.run(frame_mcu);
        frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        case f of
          7:
            marcade.in1 := marcade.in1 and $7F;
          247:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              update_video_baddudes;
              marcade.in1 := marcade.in1 or $80;
            end;
        end;
      end;
      events_dec0;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure hippodrome_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := h6280_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        m6502_0.run(frame_s);
        frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        h6280_0.run(frame_mcu);
        frame_mcu := frame_mcu + h6280_0.tframes - h6280_0.contador;
        case f of
          7:
            marcade.in1 := marcade.in1 and $7F;
          247:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              h6280_0.set_irq_line(0, HOLD_LINE);
              update_video_hippo;
              marcade.in1 := marcade.in1 or $80;
            end;
        end;
      end;
      events_dec0;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure robocop_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := h6280_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        m6502_0.run(frame_s);
        frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        h6280_0.run(frame_mcu);
        frame_mcu := frame_mcu + h6280_0.tframes - h6280_0.contador;
        case f of
          7:
            marcade.in1 := marcade.in1 and $7F;
          247:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              update_video_robocop;
              marcade.in1 := marcade.in1 or $80;
            end;
        end;
      end;
      events_dec0;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure slyspy_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := h6280_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        h6280_0.run(frame_s);
        frame_s := frame_s + h6280_0.tframes - h6280_0.contador;
        case f of
          7:
            marcade.in1 := marcade.in1 and $F7;
          247:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              update_video_slyspy;
              marcade.in1 := marcade.in1 or 8;
            end;
        end;
      end;
      events_dec1;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure change_color(numero: word);
var
  color: tcolor;
begin
  color.r := buffer_paleta[numero] and $FF;
  color.g := buffer_paleta[numero] shr 8;
  color.b := buffer_paleta[$400 + numero] and $FF;
  set_pal_color(color, numero);
  case numero of
    $000 .. $0FF:
      bac06_0.tile_1.buffer_color[numero shr 4] := true;
    $200 .. $2FF:
      bac06_0.tile_2.buffer_color[(numero shr 4) and $F] := true;
    $300 .. $3FF:
      bac06_0.tile_3.buffer_color[(numero shr 4) and $F] := true;
  end;
end;

function dec0_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $5FFFF:
      dec0_getword := rom[direccion shr 1];
    $180000 .. $180FFF:
      dec0_getword := mcu_shared_ram[(direccion and $FFF) shr 1];
    $242000 .. $24207F:
      dec0_getword := bac06_0.tile_1.colscroll[(direccion and $7F) shr 1];
    $242400 .. $2427FF:
      dec0_getword := bac06_0.tile_1.rowscroll[(direccion and $3FF) shr 1];
    $242800 .. $243FFF:
      dec0_getword := ram1[(direccion - $242800) shr 1];
    $244000 .. $245FFF:
      dec0_getword := bac06_0.tile_1.data[(direccion and $1FFF) shr 1];
    $248000 .. $24807F:
      dec0_getword := bac06_0.tile_2.colscroll[(direccion and $7F) shr 1];
    $248400 .. $2487FF:
      dec0_getword := bac06_0.tile_2.rowscroll[(direccion and $3FF) shr 1];
    $24A000 .. $24A7FF:
      dec0_getword := bac06_0.tile_2.data[(direccion and $7FF) shr 1];
    $24C800 .. $24C87F:
      dec0_getword := bac06_0.tile_3.colscroll[(direccion and $7F) shr 1];
    $24CC00 .. $24CFFF:
      dec0_getword := bac06_0.tile_3.rowscroll[(direccion and $3FF) shr 1];
    $24D000 .. $24D7FF:
      dec0_getword := bac06_0.tile_3.data[(direccion and $7FF) shr 1];
    $30C000:
      dec0_getword := marcade.in0;
    $30C002:
      dec0_getword := marcade.in1;
    $30C004:
      dec0_getword := marcade.dswa;
    $30C006:
      dec0_getword := $FFFF;
    $30C008:
      dec0_getword := i8751_return;
    $310000 .. $3107FF:
      dec0_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $314000 .. $3147FF:
      dec0_getword := buffer_paleta[((direccion and $7FF) shr 1) + $400];
    $FF8000 .. $FFBFFF:
      dec0_getword := ram2[(direccion and $3FFF) shr 1];
    $FFC000 .. $FFCFFF:
      dec0_getword := buffer_sprites_w[(direccion and $7FF) shr 1];
  end;
end;

procedure dec0_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $5FFFF:
      ; // ROM
    $180000 .. $180FFF:
      begin
        mcu_shared_ram[(direccion and $FFF) shr 1] := valor and $FF;
        if ((direccion and $FFF) = $FFE) then
          h6280_0.set_irq_line(0, HOLD_LINE);
      end;
    $240000 .. $240007:
      bac06_0.tile_1.change_control0((direccion and 7) shr 1, valor);
    $240010 .. $240017:
      bac06_0.tile_1.change_control1((direccion and 7) shr 1, valor);
    $242000 .. $24207F:
      bac06_0.tile_1.colscroll[(direccion and $7F) shr 1] := valor;
    $242400 .. $2427FF:
      bac06_0.tile_1.rowscroll[(direccion and $3FF) shr 1] := valor;
    $242800 .. $243FFF:
      ram1[(direccion - $242800) shr 1] := valor;
    $244000 .. $245FFF:
      if bac06_0.tile_1.data[(direccion and $1FFF) shr 1] <> valor then
      begin
        bac06_0.tile_1.data[(direccion and $1FFF) shr 1] := valor;
        bac06_0.tile_1.buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $246000 .. $246007:
      bac06_0.tile_2.change_control0((direccion and 7) shr 1, valor);
    $246010 .. $246017:
      bac06_0.tile_2.change_control1((direccion and 7) shr 1, valor);
    $248000 .. $24807F:
      bac06_0.tile_2.colscroll[(direccion and $7F) shr 1] := valor;
    $248400 .. $2487FF:
      bac06_0.tile_2.rowscroll[(direccion and $3FF) shr 1] := valor;
    $24A000 .. $24A7FF:
      if bac06_0.tile_2.data[(direccion and $7FF) shr 1] <> valor then
      begin
        bac06_0.tile_2.data[(direccion and $7FF) shr 1] := valor;
        bac06_0.tile_2.buffer[(direccion and $7FF) shr 1] := true;
      end;
    $24C000 .. $24C007:
      bac06_0.tile_3.change_control0((direccion and 7) shr 1, valor);
    $24C010 .. $24C017:
      bac06_0.tile_3.change_control1((direccion and 7) shr 1, valor);
    $24C800 .. $24C87F:
      bac06_0.tile_3.colscroll[(direccion and $7F) shr 1] := valor;
    $24CC00 .. $24CFFF:
      bac06_0.tile_3.rowscroll[(direccion and $3FF) shr 1] := valor;
    $24D000 .. $24D7FF:
      if bac06_0.tile_3.data[(direccion and $7FF) shr 1] <> valor then
      begin
        bac06_0.tile_3.data[(direccion and $7FF) shr 1] := valor;
        bac06_0.tile_3.buffer[(direccion and $7FF) shr 1] := true;
      end;
    $30C010 .. $30C01F:
      case (direccion and $F) of
        0:
          if prioridad <> (valor and $FF) then
          begin
            prioridad := valor and $FF;
            fillchar(bac06_0.tile_2.buffer, $1000, 1);
            fillchar(bac06_0.tile_3.buffer, $1000, 1);
          end;
        2:
          bac06_0.update_sprite_data(@buffer_sprites_w);
        4:
          begin
            sound_latch := valor and $FF;
            m6502_0.change_nmi(PULSE_LINE);
          end;
        6:
          begin
            i8751_command := valor;
            if (i8751_ports[2] and 8) <> 0 then
              mcs51_0.change_irq1(ASSERT_LINE);
          end;
        $E:
          begin
            i8751_command := 0;
            i8751_return := 0;
          end;
      end;
    $310000 .. $3107FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color((direccion and $7FF) shr 1);
      end;
    $314000 .. $3147FF:
      if buffer_paleta[((direccion and $7FF) shr 1) + $400] <> valor then
      begin
        buffer_paleta[((direccion and $7FF) shr 1) + $400] := valor;
        change_color((direccion and $7FF) shr 1);
      end;
    $FF8000 .. $FFBFFF:
      ram2[(direccion and $3FFF) shr 1] := valor;
    $FFC000 .. $FFCFFF:
      buffer_sprites_w[(direccion and $7FF) shr 1] := valor;
  end;
end;

function dec0_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF, $8000 .. $FFFF:
      dec0_snd_getbyte := mem_snd[direccion];
    $3000:
      dec0_snd_getbyte := sound_latch;
    $3800:
      dec0_snd_getbyte := oki_6295_0.read;
  end;
end;

procedure dec0_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $800:
      ym2203_0.Control(valor);
    $801:
      ym2203_0.Write(valor);
    $1000:
      ym3812_0.Control(valor);
    $1001:
      ym3812_0.Write(valor);
    $3800:
      oki_6295_0.Write(valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

procedure dec0_sound_update;
begin
  ym3812_0.update;
  ym2203_0.update;
  oki_6295_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  m6502_0.change_irq(irqstate);
end;

procedure slyspy_snd_irq(irqstate: byte);
begin
  h6280_0.set_irq_line(1, irqstate);
end;

function in_port1: byte;
begin
  in_port1 := $FF;
end;

function in_port0: byte;
var
  res: byte;
begin
  res := $FF;
  // P0 connected to latches
  if (i8751_ports[2] and $10) = 0 then
    res := res and (i8751_command shr 8);
  if (i8751_ports[2] and $20) = 0 then
    res := res and (i8751_command and $FF);
  in_port0 := res;
end;

procedure out_port0(valor: byte);
begin
  i8751_ports[0] := valor;
end;

procedure out_port1(valor: byte);
begin
  i8751_ports[1] := valor;
end;

procedure out_port3(valor: byte);
begin
  i8751_ports[3] := valor;
end;

procedure out_port2(valor: byte);
begin
  if (((valor and 4) = 0) and ((i8751_ports[2] and 4) <> 0)) then
    m68000_0.irq[5] := HOLD_LINE;
  if (valor and 8) = 0 then
    mcs51_0.change_irq1(CLEAR_LINE);
  if (((valor and $40) <> 0) and ((i8751_ports[2] and $40) = 0)) then
    i8751_return := (i8751_return and $FF00) or i8751_ports[0];
  if (((valor and $80) <> 0) and ((i8751_ports[2] and $80) = 0)) then
    i8751_return := (i8751_return and $00FF) or (i8751_ports[0] shl 8);
  i8751_ports[2] := valor;
end;

// Robocop
function robocop_mcu_getbyte(direccion: dword): byte;
begin
  case direccion of
    $1E00 .. $1FFF:
      robocop_mcu_getbyte := mem_misc[direccion and $1FF];
    $1F0000 .. $1F1FFF:
      robocop_mcu_getbyte := mcu_ram[direccion and $1FFF];
    $1F2000 .. $1F3FFF:
      robocop_mcu_getbyte := mcu_shared_ram[direccion and $1FFF];
  end;
end;

procedure robocop_mcu_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    0 .. $FFFF:
      ; // ROM
    $1F0000 .. $1F1FFF:
      mcu_ram[direccion and $1FFF] := valor;
    $1F2000 .. $1F3FFF:
      mcu_shared_ram[direccion and $1FFF] := valor;
    $1FF400 .. $1FF403:
      h6280_0.irq_status_w(direccion and $3, valor);
  end;
end;

// Hippodrome
function hippo_mcu_getbyte(direccion: dword): byte;
var
  tempw: word;
begin
  case direccion of
    0 .. $FFFF:
      hippo_mcu_getbyte := mem_misc[direccion];
    $180000 .. $1800FF:
      hippo_mcu_getbyte := mcu_shared_ram[direccion and $FF];
    $1807FF:
      hippo_mcu_getbyte := $FF;
    $1D0000 .. $1D00FF:
      case hippodrm_lsb of // protecction
        $45:
          hippo_mcu_getbyte := $4E;
        $92:
          hippo_mcu_getbyte := $15;
      end;
    $1A1000 .. $1A17FF:
      begin
        tempw := bac06_0.tile_3.data[(direccion and $7FF) shr 1];
        if (direccion and 1) <> 0 then
          hippo_mcu_getbyte := tempw shr 8
        else
          hippo_mcu_getbyte := tempw;
      end;
    $1F0000 .. $1F1FFF:
      hippo_mcu_getbyte := mcu_ram[direccion and $1FFF];
    $1FF402 .. $1FF403:
      hippo_mcu_getbyte := marcade.in1 shr 7;
  end;
end;

procedure hippo_mcu_putbyte(direccion: dword; valor: byte);
var
  tempw: word;
begin
  case direccion of
    0 .. $FFFF:
      ; // ROM
    $180000 .. $1800FF:
      mcu_shared_ram[direccion and $FF] := valor;
    $1A0000 .. $1A0007:
      begin
        if (direccion and 1) <> 0 then
          tempw := (bac06_0.tile_3.control_0[(direccion and 7) shr 1] and $00FF) or (valor shl 8)
        else
          tempw := (bac06_0.tile_3.control_0[(direccion and 7) shr 1] and $FF00) or valor;
        bac06_0.tile_3.change_control0((direccion and 7) shr 1, tempw);
      end;
    $1A0010 .. $1A001F:
      begin
        if (direccion and 1) <> 0 then
          tempw := (bac06_0.tile_3.control_1[(direccion and 7) shr 1] and $00FF) or (valor shl 8)
        else
          tempw := (bac06_0.tile_3.control_1[(direccion and 7) shr 1] and $FF00) or valor;
        bac06_0.tile_3.change_control1((direccion and 7) shr 1, tempw);
      end;
    $1A1000 .. $1A17FF:
      begin
        if (direccion and 1) <> 0 then
          tempw := (bac06_0.tile_3.data[(direccion and $7FF) shr 1] and $00FF) or (valor shl 8)
        else
          tempw := (bac06_0.tile_3.data[(direccion and $7FF) shr 1] and $FF00) or valor;
        if bac06_0.tile_3.data[(direccion and $7FF) shr 1] <> tempw then
        begin
          bac06_0.tile_3.data[(direccion and $7FF) shr 1] := tempw;
          bac06_0.tile_3.buffer[(direccion and $7FF) shr 1] := true;
        end;
      end;
    $1D0000 .. $1D00FF:
      hippodrm_lsb := valor;
    $1F0000 .. $1F1FFF:
      mcu_ram[direccion and $1FFF] := valor;
    $1FF400 .. $1FF403:
      h6280_0.irq_status_w(direccion and $3, valor);
  end;
end;

// Sly spy
procedure change_color_dec1(numero: word);
var
  color: tcolor;
begin
  color.r := pal4bit(buffer_paleta[numero]);
  color.g := pal4bit(buffer_paleta[numero] shr 4);
  color.b := pal4bit(buffer_paleta[numero] shr 8);
  set_pal_color(color, numero);
  case numero of
    $000 .. $0FF:
      bac06_0.tile_1.buffer_color[numero shr 4] := true;
    $200 .. $2FF:
      bac06_0.tile_2.buffer_color[(numero shr 4) and $F] := true;
    $300 .. $3FF:
      bac06_0.tile_3.buffer_color[(numero shr 4) and $F] := true;
  end;
end;

function slyspy_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $5FFFF:
      slyspy_getword := rom[direccion shr 1];
    $240000 .. $24FFFF:
      case ((direccion and $FFFF) or (slyspy_state * $10000)) of
        $4000, $14000, $24000, $34000:
          slyspy_state := (slyspy_state + 1) and 3;
      end;
    $300800 .. $30087F:
      slyspy_getword := bac06_0.tile_3.colscroll[(direccion and $7F) shr 1];
    $300C00 .. $300FFF:
      slyspy_getword := bac06_0.tile_3.rowscroll[(direccion and $3FF) shr 1];
    $301000 .. $3017FF:
      slyspy_getword := bac06_0.tile_3.data[(direccion and $7FF) shr 1];
    $304000 .. $307FFF:
      slyspy_getword := ram2[(direccion and $3FFF) shr 1];
    $308000 .. $3087FF:
      slyspy_getword := buffer_sprites_w[(direccion and $7FF) shr 1];
    $310000 .. $3107FF:
      slyspy_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $314008 .. $31400F:
      case ((direccion and 7) shr 1) of
        0:
          slyspy_getword := marcade.dswa;
        1:
          slyspy_getword := marcade.in0;
        2:
          slyspy_getword := marcade.in1;
        3:
          slyspy_getword := $FFFF;
      end;
    $31C000 .. $31C00F:
      case (direccion and $E) of
        0, 4:
          slyspy_getword := 0;
        2:
          slyspy_getword := $13;
        6:
          slyspy_getword := 2;
        $C:
          slyspy_getword := ram2[$2028 shr 1] shr 8;
      end;
  end;
end;

procedure slyspy_putword(direccion: dword; valor: word);
var
  tempw: word;
begin
  case direccion of
    0 .. $5FFFF:
      ; // ROM
    $240000 .. $24FFFF:
      case ((direccion and $FFFF) or (slyspy_state * $10000)) of
        $A000, $1A000, $2A000, $3A000:
          slyspy_state := 0;
        // State 0
        $0 .. $7:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_2.control_0[(direccion and 7) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_2.control_0[(direccion and 7) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            bac06_0.tile_2.change_control0((direccion and 7) shr 1, tempw);
          end;
        $10 .. $17:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_2.control_1[(direccion and 7) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_2.control_1[(direccion and 7) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            bac06_0.tile_2.change_control1((direccion and 7) shr 1, tempw, true);
          end;
        $2000 .. $207F:
          bac06_0.tile_2.colscroll[(direccion and $7F) shr 1] := valor;
        $2400 .. $27FF:
          bac06_0.tile_2.rowscroll[(direccion and $3FF) shr 1] := valor;
        $6000 .. $7FFF:
          if bac06_0.tile_2.data[(direccion and $1FFF) shr 1] <> valor then
          begin
            bac06_0.tile_2.data[(direccion and $1FFF) shr 1] := valor;
            bac06_0.tile_2.buffer[(direccion and $1FFF) shr 1] := true;
          end;
        $8000 .. $8007:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.control_0[(direccion and 7) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.control_0[(direccion and 7) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            bac06_0.tile_1.change_control0((direccion and 7) shr 1, tempw);
          end;
        $8010 .. $8017:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.control_1[(direccion and 7) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.control_1[(direccion and 7) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            bac06_0.tile_1.change_control1((direccion and 7) shr 1, tempw);
          end;
        $C000 .. $C07F:
          bac06_0.tile_1.colscroll[(direccion and $7F) shr 1] := valor;
        $C400 .. $C7FF:
          bac06_0.tile_1.rowscroll[(direccion and $3FF) shr 1] := valor;
        $E000 .. $FFFF:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            if bac06_0.tile_1.data[(direccion and $1FFF) shr 1] <> tempw then
            begin
              bac06_0.tile_1.data[(direccion and $1FFF) shr 1] := tempw;
              bac06_0.tile_1.buffer[(direccion and $1FFF) shr 1] := true;
            end;
          end;
        // State 1
        $18000 .. $19FFF:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            if bac06_0.tile_1.data[(direccion and $1FFF) shr 1] <> tempw then
            begin
              bac06_0.tile_1.data[(direccion and $1FFF) shr 1] := tempw;
              bac06_0.tile_1.buffer[(direccion and $1FFF) shr 1] := true;
            end;
          end;
        $1C000 .. $1DFFF:
          if bac06_0.tile_2.data[(direccion and $1FFF) shr 1] <> valor then
          begin
            bac06_0.tile_2.data[(direccion and $1FFF) shr 1] := valor;
            bac06_0.tile_2.buffer[(direccion and $1FFF) shr 1] := true;
          end;
        // State 2
        $20000 .. $21FFF:
          if bac06_0.tile_2.data[(direccion and $1FFF) shr 1] <> valor then
          begin
            bac06_0.tile_2.data[(direccion and $1FFF) shr 1] := valor;
            bac06_0.tile_2.buffer[(direccion and $1FFF) shr 1] := true;
          end;
        $22000 .. $23FFF:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            if bac06_0.tile_1.data[(direccion and $1FFF) shr 1] <> tempw then
            begin
              bac06_0.tile_1.data[(direccion and $1FFF) shr 1] := tempw;
              bac06_0.tile_1.buffer[(direccion and $1FFF) shr 1] := true;
            end;
          end;
        $2E000 .. $2FFFF:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            if bac06_0.tile_1.data[(direccion and $1FFF) shr 1] <> tempw then
            begin
              bac06_0.tile_1.data[(direccion and $1FFF) shr 1] := tempw;
              bac06_0.tile_1.buffer[(direccion and $1FFF) shr 1] := true;
            end;
          end;
        // State 3
        $30000 .. $31FFF:
          begin
            if m68000_0.write_8bits_lo_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF) or (valor and $FF00)
            else if m68000_0.write_8bits_hi_dir then
              tempw := (bac06_0.tile_1.data[(direccion and $1FFF) shr 1] and $FF00) or (valor and $FF)
            else
              tempw := valor;
            if bac06_0.tile_1.data[(direccion and $1FFF) shr 1] <> tempw then
            begin
              bac06_0.tile_1.data[(direccion and $1FFF) shr 1] := tempw;
              bac06_0.tile_1.buffer[(direccion and $1FFF) shr 1] := true;
            end;
          end;
        $38000 .. $39FFF:
          if bac06_0.tile_2.data[(direccion and $1FFF) shr 1] <> valor then
          begin
            bac06_0.tile_2.data[(direccion and $1FFF) shr 1] := valor;
            bac06_0.tile_2.buffer[(direccion and $1FFF) shr 1] := true;
          end;
      end;
    $300000 .. $300007:
      bac06_0.tile_3.change_control0((direccion and 7) shr 1, valor);
    $300010 .. $300017:
      bac06_0.tile_3.change_control1((direccion and 7) shr 1, valor);
    $300800 .. $30087F:
      bac06_0.tile_3.colscroll[(direccion and $7F) shr 1] := valor;
    $300C00 .. $300FFF:
      bac06_0.tile_3.rowscroll[(direccion and $3FF) shr 1] := valor;
    $301000 .. $3017FF:
      if bac06_0.tile_3.data[(direccion and $7FF) shr 1] <> valor then
      begin
        bac06_0.tile_3.data[(direccion and $7FF) shr 1] := valor;
        bac06_0.tile_3.buffer[(direccion and $7FF) shr 1] := true;
      end;
    $304000 .. $307FFF:
      ram2[(direccion and $3FFF) shr 1] := valor;
    $308000 .. $3087FF:
      bac06_0.sprite_ram[(direccion and $7FF) shr 1] := valor;
    $310000 .. $3107FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color_dec1((direccion and $7FF) shr 1);
      end;
    $314000 .. $31400F:
      case ((direccion and $F) shr 1) of
        0:
          begin
            sound_latch := valor and $FF;
            h6280_0.set_irq_line(0, HOLD_LINE);
          end;
        1:
          prioridad := valor and $FF;
      end;
  end;
end;

function slyspy_snd_getbyte(direccion: dword): byte;
begin
  case direccion of
    $0 .. $FFFF:
      slyspy_snd_getbyte := mem_snd[direccion];
    $80000 .. $FFFFF:
      case ((direccion and $7FFFF) or (slyspy_sound_state * $80000)) of
        $20000, $A0000, $120000, $1A0000:
          slyspy_sound_state := (slyspy_sound_state + 1) and 3;
        $50000, $D0000, $150000, $1D0000:
          slyspy_sound_state := 0;
        // State 0
        $60000:
          slyspy_snd_getbyte := oki_6295_0.read;
        $70000:
          slyspy_snd_getbyte := sound_latch;
        // State 1
        $90000:
          slyspy_snd_getbyte := oki_6295_0.read;
        $C0000:
          slyspy_snd_getbyte := sound_latch;
        // State 2
        $110000:
          slyspy_snd_getbyte := sound_latch;
        $130000:
          slyspy_snd_getbyte := oki_6295_0.read;
        // State 3
        $1E0000:
          slyspy_snd_getbyte := sound_latch;
        $1F0000:
          slyspy_snd_getbyte := oki_6295_0.read;
      end;
    $1F0000 .. $1FFFFF:
      slyspy_snd_getbyte := mcu_ram[direccion and $1FFF];
  end;
end;

procedure slyspy_snd_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    0 .. $FFFF:
      ; // ROM
    $80000 .. $FFFFF:
      case ((direccion and $7FFFF) or (slyspy_sound_state * $80000)) of
        // State 0
        $10000:
          ym3812_0.Control(valor);
        $10001:
          ym3812_0.Write(valor);
        $30000:
          ym2203_0.Control(valor);
        $30001:
          ym2203_0.Write(valor);
        $60000:
          oki_6295_0.Write(valor);
        // State 1
        $90000:
          oki_6295_0.Write(valor);
        $E0000:
          ym2203_0.Control(valor);
        $E0001:
          ym2203_0.Write(valor);
        $F0000:
          ym3812_0.Control(valor);
        $F0001:
          ym3812_0.Write(valor);
        // State 2
        $130000:
          oki_6295_0.Write(valor);
        $140000:
          ym2203_0.Control(valor);
        $140001:
          ym2203_0.Write(valor);
        $170000:
          ym3812_0.Control(valor);
        $170001:
          ym3812_0.Write(valor);
        // State 3
        $190000:
          ym3812_0.Control(valor);
        $190001:
          ym3812_0.Write(valor);
        $1C0000:
          ym2203_0.Control(valor);
        $1C0001:
          ym2203_0.Write(valor);
        $1F0000:
          oki_6295_0.Write(valor);
      end;
    $1F0000 .. $1FFFFF:
      mcu_ram[direccion and $1FFF] := valor;
  end;
end;

// Main
procedure reset_dec0;
begin
  m68000_0.reset;
  case main_vars.machine_type of
    157:
      begin
        mcs51_0.reset;
        i8751_return := 0;
        i8751_command := 0;
        i8751_ports[0] := 0;
        i8751_ports[1] := 0;
        i8751_ports[2] := 0;
        i8751_ports[3] := 0;
        m6502_0.reset;
      end;
    156, 158:
      begin
        h6280_0.reset;
        m6502_0.reset;
      end;
    316, 317:
      begin
        h6280_0.reset;
        slyspy_state := 0;
        slyspy_sound_state := 0;
      end;
  end;
  ym3812_0.reset;
  ym2203_0.reset;
  oki_6295_0.reset;
  bac06_0.reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $F7;
  sound_latch := 0;
end;

function start_dec0: boolean;
const
  ps_x: array [0 .. 15] of dword = (16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  memory_temp: array [0 .. $7FFFF] of byte;
  memory_temp2: array [0 .. $1FFFF] of byte;
  f: word;
  procedure convert_chars(ch_num: word);
  begin
    init_gfx(0, 8, 8, ch_num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 8 * 8, 0, ch_num * 8 * 8 * 2, ch_num * 8 * 8 * 1, ch_num * 8 * 8 * 3);
    convert_gfx(0, 0, @memory_temp, @ps_x[8], @ps_y, false, false);
  end;
  procedure convert_tiles(num_gfx: byte; tl_num: word);
  begin
    init_gfx(num_gfx, 16, 16, tl_num);
    gfx[num_gfx].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 16, tl_num * 16 * 16 * 1, tl_num * 16 * 16 * 3, tl_num * 16 * 16 * 0, tl_num * 16 * 16 * 2);
    convert_gfx(num_gfx, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;
  procedure init_sound_chips;
  begin
    ym3812_0 := ym3812_chip.create(YM3812_FM, 3000000);
    ym3812_0.change_irq_calls(snd_irq);
    ym2203_0 := ym2203_chip.create(1500000);
    oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH);
  end;

begin
  case main_vars.machine_type of
    156:
      machine_calls.general_loop := robocop_loop;
    157:
      machine_calls.general_loop := baddudes_loop;
    158:
      machine_calls.general_loop := hippodrome_loop;
    316, 317:
      machine_calls.general_loop := slyspy_loop;
  end;
  machine_calls.reset := reset_dec0;
  machine_calls.fps_max := 57.444885;
  start_dec0 := false;
  start_audio(false);
  // El video se inicia en el chip bac06!!!
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 272);
  m68000_0.change_ram16_calls(dec0_getword, dec0_putword);
  case main_vars.machine_type of
    156:
      begin // Robocop
        bac06_0 := bac06_chip.create(false, false, false, $000, $200, $300, $FFF, $7FF, $3FF, 1, 1, 1, $100);
        // cargar roms
        if not(roms_load16w(@rom, robocop_rom)) then
          exit;
        // cargar sonido
        m6502_0 := cpu_m6502.create(1500000, 272, TCPU_M6502);
        m6502_0.change_ram_calls(dec0_snd_getbyte, dec0_snd_putbyte);
        m6502_0.init_sound(dec0_sound_update);
        init_sound_chips;
        if not(roms_load(@mem_snd, robocop_sound)) then
          exit;
        // MCU
        h6280_0 := cpu_h6280.create(21477200 div 16, 272);
        h6280_0.change_ram_calls(robocop_mcu_getbyte, robocop_mcu_putbyte);
        if not(roms_load(@mem_misc, robocop_mcu)) then
          exit;
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, robocop_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, robocop_char)) then
          exit;
        convert_chars($1000);
        // tiles 1
        if not(roms_load(@memory_temp, robocop_tiles1)) then
          exit;
        convert_tiles(1, $800);
        // tiles 2
        if not(roms_load(@memory_temp, robocop_tiles2)) then
          exit;
        convert_tiles(2, $400);
        // sprites
        if not(roms_load(@memory_temp, robocop_sprites)) then
          exit;
        convert_tiles(3, $1000);
        // Dip
        marcade.dswa := $FF7F;
        marcade.dswa_val := @robocop_dip;
      end;
    157:
      begin // Baddudes
        bac06_0 := bac06_chip.create(false, true, true, $000, $200, $300, $7FF, $7FF, $3FF, 1, 1, 1, $100);
        // cargar roms
        if not(roms_load16w(@rom, baddudes_rom)) then
          exit;
        // cargar sonido
        m6502_0 := cpu_m6502.create(1500000, 272, TCPU_M6502);
        m6502_0.change_ram_calls(dec0_snd_getbyte, dec0_snd_putbyte);
        m6502_0.init_sound(dec0_sound_update);
        init_sound_chips;
        if not(roms_load(@mem_snd, baddudes_sound)) then
          exit;
        // MCU
        mcs51_0 := cpu_mcs51.create(I8X51, 8000000, 272);
        mcs51_0.change_io_calls(in_port0, in_port1, in_port1, in_port1, out_port0, out_port1, out_port2, out_port3);
        if not(roms_load(mcs51_0.get_rom_addr, baddudes_mcu)) then
          exit;
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, baddudes_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, baddudes_char)) then
          exit;
        convert_chars($800);
        // tiles 1
        if not(roms_load(@memory_temp, baddudes_tiles1)) then
          exit;
        convert_tiles(1, $800);
        // tiles 2, ordenar
        if not(roms_load(@memory_temp, baddudes_tiles2)) then
          exit;
        copymemory(@memory_temp[$8000], @memory_temp[$20000], $8000);
        copymemory(@memory_temp[$0], @memory_temp[$28000], $8000);
        copymemory(@memory_temp[$18000], @memory_temp[$30000], $8000);
        copymemory(@memory_temp[$10000], @memory_temp[$38000], $8000);
        convert_tiles(2, $400);
        // sprites
        if not(roms_load(@memory_temp, baddudes_sprites)) then
          exit;
        convert_tiles(3, $1000);
        // Dip
        marcade.dswa := $FFFF;
        marcade.dswa_val := @baddudes_dip;
      end;
    158:
      begin // Hippodrome
        bac06_0 := bac06_chip.create(false, false, false, $000, $200, $300, $FFF, $3FF, $3FF, 1, 1, 1, $100);
        // cargar roms
        if not(roms_load16w(@rom, hippo_rom)) then
          exit;
        // cargar sonido
        m6502_0 := cpu_m6502.create(1500000, 272, TCPU_M6502);
        m6502_0.change_ram_calls(dec0_snd_getbyte, dec0_snd_putbyte);
        m6502_0.init_sound(dec0_sound_update);
        init_sound_chips;
        if not(roms_load(@mem_snd, hippo_sound)) then
          exit;
        // MCU+decrypt
        h6280_0 := cpu_h6280.create(21477200 div 16, 272);
        h6280_0.change_ram_calls(hippo_mcu_getbyte, hippo_mcu_putbyte);
        if not(roms_load(@mem_misc, hippo_mcu)) then
          exit;
        for f := 0 to $FFFF do
          mem_misc[f] := bitswap8(mem_misc[f], 0, 6, 5, 4, 3, 2, 1, 7);
        mem_misc[$189] := $60; // RTS prot area
        mem_misc[$1AF] := $60; // RTS prot area
        mem_misc[$1DB] := $60; // RTS prot area
        mem_misc[$21A] := $60; // RTS prot area
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, hippo_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, hippo_char)) then
          exit;
        convert_chars($1000);
        // tiles 1
        if not(roms_load(@memory_temp, hippo_tiles1)) then
          exit;
        convert_tiles(1, $400);
        // tiles 2
        if not(roms_load(@memory_temp, hippo_tiles2)) then
          exit;
        convert_tiles(2, $400);
        // sprites
        if not(roms_load(@memory_temp, hippo_sprites)) then
          exit;
        convert_tiles(3, $1000);
        // Dip
        marcade.dswa := $FFFF;
        marcade.dswa_val := @hippo_dip;
      end;
    316:
      begin // Sly Spy
        bac06_0 := bac06_chip.create(false, true, false, $000, $200, $300, $7FF, $3FF, $7FF, 1, 1, 1, $100);
        // cargar roms
        m68000_0.change_ram16_calls(slyspy_getword, slyspy_putword);
        if not(roms_load16w(@rom, slyspy_rom)) then
          exit;
        // cargar sonido
        h6280_0 := cpu_h6280.create(12000000 div 4, 272);
        h6280_0.change_ram_calls(slyspy_snd_getbyte, slyspy_snd_putbyte);
        h6280_0.init_sound(dec0_sound_update);
        init_sound_chips;
        ym3812_0.change_irq_calls(slyspy_snd_irq);
        if not(roms_load(@mem_snd, slyspy_sound)) then
          exit;
        for f := 0 to $FFFF do
          mem_snd[f] := bitswap8(mem_snd[f], 0, 6, 5, 4, 3, 2, 1, 7);
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, slyspy_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp2, slyspy_char)) then
          exit;
        copymemory(@memory_temp[$0], @memory_temp2[$4000], $4000);
        copymemory(@memory_temp[$4000], @memory_temp2[$0], $4000);
        copymemory(@memory_temp[$8000], @memory_temp2[$C000], $4000);
        copymemory(@memory_temp[$C000], @memory_temp2[$8000], $4000);
        convert_chars($800);
        // tiles 1
        if not(roms_load(@memory_temp, slyspy_tiles1)) then
          exit;
        convert_tiles(1, $400);
        // tiles 2, ordenar
        if not(roms_load(@memory_temp, slyspy_tiles2)) then
          exit;
        convert_tiles(2, $800);
        // sprites
        if not(roms_load(@memory_temp, slyspy_sprites)) then
          exit;
        convert_tiles(3, $1000);
        // Dip
        marcade.dswa := $FF7F;
        marcade.dswa_val := @slyspy_dip;
      end;
    317:
      begin // Boulder Dash
        bac06_0 := bac06_chip.create(false, true, false, $000, $200, $300, $FFF, $3FF, $7FF, 1, 1, 1, $100);
        // cargar roms
        m68000_0.change_ram16_calls(slyspy_getword, slyspy_putword);
        if not(roms_load16w(@rom, bouldash_rom)) then
          exit;
        // cargar sonido
        h6280_0 := cpu_h6280.create(12000000 div 4, 272);
        h6280_0.change_ram_calls(slyspy_snd_getbyte, slyspy_snd_putbyte);
        h6280_0.init_sound(dec0_sound_update);
        init_sound_chips;
        ym3812_0.change_irq_calls(slyspy_snd_irq);
        if not(roms_load(@mem_snd, bouldash_sound)) then
          exit;
        for f := 0 to $FFFF do
          mem_snd[f] := bitswap8(mem_snd[f], 0, 6, 5, 4, 3, 2, 1, 7);
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, bouldash_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp2, bouldash_char)) then
          exit;
        copymemory(@memory_temp[$0], @memory_temp2[$8000], $8000);
        copymemory(@memory_temp[$8000], @memory_temp2[$0], $8000);
        copymemory(@memory_temp[$18000], @memory_temp2[$10000], $8000);
        copymemory(@memory_temp[$10000], @memory_temp2[$18000], $8000);
        convert_chars($1000);
        // tiles 1
        if not(roms_load(@memory_temp, bouldash_tiles1)) then
          exit;
        convert_tiles(1, $400);
        // tiles 2, ordenar
        if not(roms_load(@memory_temp, bouldash_tiles2)) then
          exit;
        convert_tiles(2, $800);
        // sprites
        if not(roms_load(@memory_temp, bouldash_sprites)) then
          exit;
        convert_tiles(3, $800);
        // Dip
        marcade.dswa := $7F7F;
        marcade.dswa_val := @bouldash_dip;
      end;
  end;
  // final
  reset_dec0;
  start_dec0 := true;
end;

end.
