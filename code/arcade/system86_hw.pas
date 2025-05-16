unit system86_hw;

interface

uses
  WinApi.Windows,
  m6809,
  m680x,
  namco_snd,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine;

function start_system86: boolean;

implementation

const
  // Rolling Thunder
  rthunder_rom: tipo_roms = (n: 'rt3_1b.9c'; l: $8000; p: $8000; crc: $7D252A1B);
  rthunder_rom_bank: array [0 .. 3] of tipo_roms = ((n: 'rt1_17.f1'; l: $10000; p: 0; crc: $766AF455), (n: 'rt1_18.h1'; l: $10000; p: $10000; crc: $3F9F2F5D), (n: 'rt3_19.k1'; l: $10000; p: $20000; crc: $C16675E9), (n: 'rt3_20.m1'; l: $10000; p: $30000; crc: $C470681B));
  rthunder_sub_rom: array [0 .. 1] of tipo_roms = ((n: 'rt3_2b.12c'; l: $8000; p: 0; crc: $A7EA46EE), (n: 'rt3_3.12d'; l: $8000; p: $8000; crc: $A13F601C));
  rthunder_chars: array [0 .. 1] of tipo_roms = ((n: 'rt1_7.7r'; l: $10000; p: 0; crc: $A85EFA39), (n: 'rt1_8.7s'; l: $8000; p: $10000; crc: $F7A95820));
  rthunder_tiles: array [0 .. 1] of tipo_roms = ((n: 'rt1_5.4r'; l: $8000; p: 0; crc: $D0FC470B), (n: 'rt1_6.4s'; l: $4000; p: $8000; crc: $6B57EDB2));
  rthunder_sprites: array [0 .. 7] of tipo_roms = ((n: 'rt1_9.12h'; l: $10000; p: 0; crc: $8E070561), (n: 'rt1_10.12k'; l: $10000; p: $10000; crc: $CB8FB607), (n: 'rt1_11.12l'; l: $10000; p: $20000; crc: $2BDF5ED9), (n: 'rt1_12.12m'; l: $10000; p: $30000; crc: $E6C6C7DC),
    (n: 'rt1_13.12p'; l: $10000; p: $40000; crc: $489686D7), (n: 'rt1_14.12r'; l: $10000; p: $50000; crc: $689E56A8), (n: 'rt1_15.12t'; l: $10000; p: $60000; crc: $1D8BF2CA), (n: 'rt1_16.12u'; l: $10000; p: $70000; crc: $1BBCF37B));
  rthunder_mcu: array [0 .. 1] of tipo_roms = ((n: 'rt3_4.6b'; l: $8000; p: $1000; crc: $00CF293F), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  rthunder_prom: array [0 .. 4] of tipo_roms = ((n: 'rt1-1.3r'; l: $200; p: 0; crc: $8EF3BB9D), (n: 'rt1-2.3s'; l: $200; p: $200; crc: $6510A8F2), (n: 'rt1-3.4v'; l: $800; p: $400; crc: $95C7D944), (n: 'rt1-4.5v'; l: $800; p: $C00; crc: $1391FEC9), (n: 'rt1-5.6u'; l: $20;
    p: $1400; crc: $E4130804));
  rthunder_adpcm: array [0 .. 1] of tipo_roms = ((n: 'rt1_21.f3'; l: $10000; p: 0; crc: $454968F3), (n: 'rt2_22.h3'; l: $10000; p: $20000; crc: $FE963E72));
  rthunder_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin B'; number: 4; dip: ((dip_val: 0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Freeze'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Invulnerability'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $60; dip_name: '1C 1C'), (dip_val: $40; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  rthunder_dip_b: array [0 .. 7] of def_dip = ((mask: $1; name: 'Continues'; number: 2; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $6; name: 'Cabinet'; number: 4;
    dip: ((dip_val: $6; dip_name: 'Upright 1 Player'), (dip_val: $2; dip_name: 'Upright 1 Player'), (dip_val: $4; dip_name: 'Upright 2 Players'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Level Select (Cheat)';
    number: 2; dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Easy'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Timer value'; number: 2;
    dip: ((dip_val: $20; dip_name: '120secs'), (dip_val: $0; dip_name: '150secs'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $40; dip_name: '70K 200K'), (dip_val: $0; dip_name: '100K 300K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Lives'; number: 2;
    dip: ((dip_val: $80; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Hopping Mappy
  hopmappy_rom: tipo_roms = (n: 'hm1_1.9c'; l: $8000; p: $8000; crc: $1A83914E);
  hopmappy_sub_rom: tipo_roms = (n: 'hm1_2.12c'; l: $4000; p: $C000; crc: $C46CDA65);
  hopmappy_chars: tipo_roms = (n: 'hm1_6.7r'; l: $4000; p: 0; crc: $FD0E8887);
  hopmappy_tiles: tipo_roms = (n: 'hm1_5.4r'; l: $4000; p: 0; crc: $9C4F31AE);
  hopmappy_sprites: tipo_roms = (n: 'hm1_4.12h'; l: $8000; p: 0; crc: $78719C52);
  hopmappy_mcu: array [0 .. 1] of tipo_roms = ((n: 'hm1_3.6b'; l: $2000; p: $1000; crc: $6496E1DB), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  hopmappy_prom: array [0 .. 4] of tipo_roms = ((n: 'hm1-1.3r'; l: $200; p: 0; crc: $CC801088), (n: 'hm1-2.3s'; l: $200; p: $200; crc: $A1CB71C5), (n: 'hm1-3.4v'; l: $800; p: $400; crc: $E362D613), (n: 'hm1-4.5v'; l: $800; p: $C00; crc: $678252B4), (n: 'hm1-5.6u'; l: $20;
    p: $1400; crc: $475BF500));
  hopmappy_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coin B'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $4; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Lives'; number: 4;
    dip: ((dip_val: $10; dip_name: '1'), (dip_val: $8; dip_name: '2'), (dip_val: $18; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $60; dip_name: '1C 1C'), (dip_val: $40; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  hopmappy_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2; dip: ((dip_val: $1; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Level Select (Cheat)'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Sky Kid Deluxe
  skykiddx_rom: array [0 .. 1] of tipo_roms = ((n: 'sk3_2.9d'; l: $8000; p: 0; crc: $74B8F8E2), (n: 'sk3_1b.9c'; l: $8000; p: $8000; crc: $767B3514));
  skykiddx_sub_rom: tipo_roms = (n: 'sk3_3.12c'; l: $8000; p: $8000; crc: $6D1084C4);
  skykiddx_chars: array [0 .. 1] of tipo_roms = ((n: 'sk3_9.7r'; l: $8000; p: 0; crc: $48675B17), (n: 'sk3_10.7s'; l: $4000; p: $8000; crc: $7418465A));
  skykiddx_tiles: array [0 .. 1] of tipo_roms = ((n: 'sk3_7.4r'; l: $8000; p: 0; crc: $4036B735), (n: 'sk3_8.4s'; l: $4000; p: $8000; crc: $044BFD21));
  skykiddx_sprites: array [0 .. 1] of tipo_roms = ((n: 'sk3_5.12h'; l: $8000; p: 0; crc: $5C7D4399), (n: 'sk3_6.12k'; l: $8000; p: $8000; crc: $C908A3B2));
  skykiddx_mcu: array [0 .. 1] of tipo_roms = ((n: 'sk3_4.6b'; l: $4000; p: $1000; crc: $E6CAE2D6), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  skykiddx_prom: array [0 .. 4] of tipo_roms = ((n: 'sk3-1.3r'; l: $200; p: 0; crc: $9E81DEDD), (n: 'sk3-2.3s'; l: $200; p: $200; crc: $CBFEC4DD), (n: 'sk3-3.4v'; l: $800; p: $400; crc: $81714109), (n: 'sk3-4.5v'; l: $800; p: $C00; crc: $1BF25ACC), (n: 'sk3-5.6u'; l: $20;
    p: $1400; crc: $E4130804));
  skykiddx_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin B'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Freeze'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Level Select (Cheat)'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $60; dip_name: '1C 1C'), (dip_val: $40; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  skykiddx_dip_b: array [0 .. 3] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $1; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $0; dip_name: '20K 80K+'), (dip_val: $10; dip_name: '20K 80K'), (dip_val: $20; dip_name: '30K 90K+'), (dip_val: $30; dip_name: '30K 90K'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $80; dip_name: '1'), (dip_val: $40; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // The Return to Istahar
  roishtar_rom: array [0 .. 1] of tipo_roms = ((n: 'ri1_2.9d'; l: $2000; p: $4000; crc: $FCD58D91), (n: 'ri1_1c.9c'; l: $8000; p: $8000; crc: $14ACBACB));
  roishtar_sub_rom: tipo_roms = (n: 'ri1_3.12c'; l: $8000; p: $8000; crc: $A39829F7);
  roishtar_chars: array [0 .. 1] of tipo_roms = ((n: 'ri1_14.7r'; l: $4000; p: 0; crc: $DE8154B4), (n: 'ri1_15.7s'; l: $2000; p: $4000; crc: $4298822B));
  roishtar_tiles: array [0 .. 1] of tipo_roms = ((n: 'ri1_12.4r'; l: $4000; p: 0; crc: $557E54D3), (n: 'ri1_13.4s'; l: $2000; p: $4000; crc: $9EBE8E32));
  roishtar_sprites: array [0 .. 6] of tipo_roms = ((n: 'ri1_5.12h'; l: $8000; p: 0; crc: $46B59239), (n: 'ri1_6.12k'; l: $8000; p: $8000; crc: $94D9EF48), (n: 'ri1_7.12l'; l: $8000; p: $10000; crc: $DA802B59), (n: 'ri1_8.12m'; l: $8000; p: $18000; crc: $16B88B74),
    (n: 'ri1_9.12p'; l: $8000; p: $20000; crc: $F3DE3C2A), (n: 'ri1_10.12r'; l: $8000; p: $28000; crc: $6DACC70D), (n: 'ri1_11.12t'; l: $8000; p: $30000; crc: $FB6BC533));
  roishtar_mcu: array [0 .. 1] of tipo_roms = ((n: 'ri1_4.6b'; l: $8000; p: $1000; crc: $552172B8), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  roishtar_prom: array [0 .. 4] of tipo_roms = ((n: 'ri1-1.3r'; l: $200; p: 0; crc: $29CD0400), (n: 'ri1-2.3s'; l: $200; p: $200; crc: $02FD278D), (n: 'ri1-3.4v'; l: $800; p: $400; crc: $CBD7E53F), (n: 'ri1-4.5v'; l: $800; p: $C00; crc: $22921617), (n: 'ri1-5.6u'; l: $20;
    p: $1400; crc: $E2188075));
  roishtar_dip_a: array [0 .. 2] of def_dip = ((mask: $7; name: 'Coin A'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $2; dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $1; dip_name: '2C 3C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5;
    dip_name: '1C 3C'), (dip_val: $4; dip_name: '1C 5C'), (dip_val: $3; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  roishtar_dip_b: array [0 .. 3] of def_dip = ((mask: $7; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $2; dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $1; dip_name: '2C 3C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5;
    dip_name: '1C 3C'), (dip_val: $4; dip_name: '1C 5C'), (dip_val: $3; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Freeze'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Genpei ToumaDen
  genpeitd_rom: tipo_roms = (n: 'gt1_1b.9c'; l: $8000; p: $8000; crc: $75396194);
  genpeitd_rom_bank: tipo_roms = (n: 'gt1_10b.f1'; l: $10000; p: 0; crc: $5721AD0D);
  genpeitd_sub_rom: tipo_roms = (n: 'gt1_2.12c'; l: $4000; p: $C000; crc: $302F2CB6);
  genpeitd_chars: array [0 .. 1] of tipo_roms = ((n: 'gt1_7.7r'; l: $10000; p: 0; crc: $EA77A211), (n: 'gt1_4.4s'; l: $8000; p: $10000; crc: $1B128A2E));
  genpeitd_tiles: array [0 .. 1] of tipo_roms = ((n: 'gt1_5.4r'; l: $8000; p: 0; crc: $44D58B06), (n: 'rt1_6.4s'; l: $4000; p: $8000; crc: $DB8D45B0));
  genpeitd_sprites: array [0 .. 7] of tipo_roms = ((n: 'gt1_11.12h'; l: $20000; p: 0; crc: $3181A5FE), (n: 'gt1_12.12k'; l: $20000; p: $20000; crc: $76B729AB), (n: 'gt1_13.12l'; l: $20000; p: $40000; crc: $E332A36E), (n: 'gt1_14.12m'; l: $20000; p: $60000; crc: $E5FFAEF5),
    (n: 'gt1_15.12p'; l: $20000; p: $80000; crc: $198B6878), (n: 'gt1_16.12r'; l: $20000; p: $A0000; crc: $801E29C7), (n: 'gt1_8.12t'; l: $10000; p: $C0000; crc: $AD7BC770), (n: 'gt1_9.12u'; l: $10000; p: $E0000; crc: $D95A5FD7));
  genpeitd_mcu: array [0 .. 1] of tipo_roms = ((n: 'gt1_3.6b'; l: $8000; p: $1000; crc: $315CD988), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  genpeitd_prom: array [0 .. 4] of tipo_roms = ((n: 'gt1-1.3r'; l: $200; p: 0; crc: $2F0DDDDB), (n: 'gt1-2.3s'; l: $200; p: $200; crc: $87D27025), (n: 'gt1-3.4v'; l: $800; p: $400; crc: $C178DE99), (n: 'gt1-4.5v'; l: $800; p: $C00; crc: $9F48EF17), (n: 'gt1-5.6u'; l: $20;
    p: $1400; crc: $E4130804));
  genpeitd_adpcm: array [0 .. 2] of tipo_roms = ((n: 'gt1_17.f3'; l: $20000; p: 0; crc: $26181FF8), (n: 'gt1_18.h3'; l: $20000; p: $20000; crc: $7EF9E5EA), (n: 'gt1_19.k3'; l: $20000; p: $40000; crc: $38E11F6C));
  genpeitd_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin B'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Freeze'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $8; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $60; dip_name: '1C 1C'), (dip_val: $40; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  genpeitd_dip_b: array [0 .. 4] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $1; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $20; dip_name: 'Easy'), (dip_val: $30; dip_name: 'Normal'), (dip_val: $10; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Candle'; number: 4;
    dip: ((dip_val: $80; dip_name: '40'), (dip_val: $C0; dip_name: '50'), (dip_val: $40; dip_name: '60'), (dip_val: $0; dip_name: '70'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Wonder Momo
  wndrmomo_rom: tipo_roms = (n: 'wm1_1.9c'; l: $8000; p: $8000; crc: $34B50BF0);
  wndrmomo_rom_bank: tipo_roms = (n: 'wm1_16.f1'; l: $10000; p: 0; crc: $E565F8F3);
  wndrmomo_sub_rom: tipo_roms = (n: 'wm1_2.12c'; l: $8000; p: $8000; crc: $3181EFD0);
  wndrmomo_chars: array [0 .. 1] of tipo_roms = ((n: 'wm1_6.7r'; l: $8000; p: 0; crc: $93955FBB), (n: 'wm1_7.7s'; l: $4000; p: $8000; crc: $7D662527));
  wndrmomo_tiles: array [0 .. 1] of tipo_roms = ((n: 'wm1_4.4r'; l: $8000; p: 0; crc: $BBE67836), (n: 'wm1_5.4s'; l: $4000; p: $8000; crc: $A81B481F));
  wndrmomo_sprites: array [0 .. 7] of tipo_roms = ((n: 'wm1_8.12h'; l: $10000; p: 0; crc: $14F52E72), (n: 'wm1_9.12k'; l: $10000; p: $10000; crc: $16F8CDAE), (n: 'wm1_10.12l'; l: $10000; p: $20000; crc: $BFBC1896), (n: 'wm1_11.12m'; l: $10000; p: $30000; crc: $D775DDB2),
    (n: 'wm1_12.12p'; l: $10000; p: $40000; crc: $DE64C12F), (n: 'wm1_13.12r'; l: $10000; p: $50000; crc: $CFE589AD), (n: 'wm1_14.12t'; l: $10000; p: $60000; crc: $2AE21A53), (n: 'wm1_15.12u'; l: $10000; p: $70000; crc: $B5C98BE0));
  wndrmomo_mcu: array [0 .. 1] of tipo_roms = ((n: 'wm1_3.6b'; l: $8000; p: $1000; crc: $55F01DF7), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  wndrmomo_prom: array [0 .. 4] of tipo_roms = ((n: 'wm1-1.3r'; l: $200; p: 0; crc: $1AF8ADE8), (n: 'wm1-2.3s'; l: $200; p: $200; crc: $8694E213), (n: 'wm1-3.4v'; l: $800; p: $400; crc: $2FFAF9A4), (n: 'wm1-4.5v'; l: $800; p: $C00; crc: $F4E83E0B), (n: 'wm1-5.6u'; l: $20;
    p: $1400; crc: $E4130804));
  wndrmomo_adpcm: array [0 .. 3] of tipo_roms = ((n: 'wm1_17.f3'; l: $10000; p: 0; crc: $BEA3C318), (n: 'wm1_18.h3'; l: $10000; p: $20000; crc: $6D73BCC5), (n: 'wm1_19.k3'; l: $10000; p: $40000; crc: $D288E912), (n: 'wm1_20.m3'; l: $10000; p: $60000; crc: $076A72CB));
  wndrmomo_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin B'; number: 4; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Freeze'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Level Select'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Coin A'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $60; dip_name: '1C 1C'), (dip_val: $40; dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  wndrmomo_dip_b: array [0 .. 2] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2; dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $6; name: 'Cabinet'; number: 4;
    dip: ((dip_val: $2; dip_name: 'Upright 1 Player'), (dip_val: $4; dip_name: 'Upright 2 Players'), (dip_val: $6; dip_name: 'Cocktail'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 4;

var
  rom_bank: array [0 .. $1F, 0 .. $1FFF] of byte;
  rom_sub_bank: array [0 .. 3, 0 .. $1FFF] of byte;
  nchar_prom: array [0 .. $1F] of byte;
  rom_nbank, rom_sub_nbank, tile_bank, back_color: byte;
  scroll_y, prior: array [0 .. 3] of byte;
  scroll_x: array [0 .. 3] of word;
  copy_sprites: boolean;
  bank_sprites: word;

procedure draw_sprites(prior: byte);
var
  sy, sizex, sizey, tx, ty, f, atrib1, atrib2, sprite_yoffs: byte;
  nchar, sx, color, sprite_xoffs: word;
  flipx, flipy: boolean;
const
  sprite_size: array [0 .. 3] of byte = (16, 8, 32, 4);
begin
  sprite_xoffs := memory[$5800 + $7F5] + ((memory[$5800 + $7F4] and 1) shl 8);
  sprite_yoffs := memory[$5800 + $7F7];
  for f := 0 to $7E do
  begin
    atrib2 := memory[$580E + (f * $10)];
    if prior <> ((atrib2 and $E0) shr 5) then
      continue;
    atrib1 := memory[$580A + (f * $10)];
    color := memory[$580C + (f * $10)];
    flipx := (atrib1 and $20) <> 0;
    flipy := (atrib2 and 1) <> 0;
    sizex := sprite_size[(atrib1 and $C0) shr 6];
    sizey := sprite_size[(atrib2 and 6) shr 1];
    tx := (atrib1 and $18) and (not(sizex - 1));
    ty := (atrib2 and $18) and (not(sizey - 1));
    if flipx then
      tx := (tx - sizex) and $1F;
    if flipy then
      ty := (ty - sizey) and $1F;
    sx := (memory[$580D + (f * $10)] + ((color and 1) shl 8)) + sprite_xoffs;
    sy := (256 - (memory[$580F + (f * $10)]) - sizey) - (sprite_yoffs + 1);
    nchar := memory[$580B + (f * $10)] and (bank_sprites - 1);
    nchar := nchar + ((atrib1 and $7) * bank_sprites);
    color := (color and $FE) shl 3;
    put_gfx_sprite(nchar, color, flipx, flipy, 2);
    actualiza_gfx_sprite_size(sx - 67, sy + 11, 5, sizex, sizey, tx, ty);
  end;
end;

procedure update_video_screen;
var
  f, color, nchar, offs: word;
  x, y: byte;
begin
  fill_full_screen(5, gfx[0].colores[8 * back_color + 7]);
  for f := 0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    // Screen 0
    if gfx[0].buffer[f] then
    begin
      color := memory[1 + (f * 2)];
      offs := ((nchar_prom[((0 and 1) shl 4) + ((color and 3) shl 2)] and $E) shr 1) * $100 + tile_bank * $800;
      nchar := memory[0 + (f * 2)] + offs;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 1, 0);
      gfx[0].buffer[f] := false;
    end;
    // Screen 1
    if gfx[0].buffer[$800 + f] then
    begin
      color := memory[$1001 + (f * 2)];
      offs := ((nchar_prom[((1 and 1) shl 4) + ((color and 3) shl 2)] and $E) shr 1) * $100 + tile_bank * $800;
      nchar := memory[$1000 + (f * 2)] + offs;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 2, 0);
      gfx[0].buffer[$800 + f] := false;
    end;
    // Screen 2
    if gfx[1].buffer[f] then
    begin
      color := memory[$2001 + (f * 2)];
      offs := ((nchar_prom[((2 and 1) shl 4) + (color and 3)] and $E0) shr 5) * $100;
      nchar := memory[$2000 + (f * 2)] + offs;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 3, 1);
      gfx[1].buffer[f] := false;
    end;
    // Screen 3
    if gfx[1].buffer[$800 + f] then
    begin
      color := memory[$3001 + (f * 2)];
      offs := ((nchar_prom[((3 and 1) shl 4) + (color and 3)] and $E0) shr 5) * $100;
      nchar := memory[$3000 + (f * 2)] + offs;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 4, 1);
      gfx[1].buffer[$800 + f] := false;
    end;
  end;
end;

procedure update_video_system86;
var
  layer, f: byte;
const
  diff_x: array [0 .. 3] of byte = (20, 18, 21, 19);
begin
  update_video_screen;
  for layer := 0 to 7 do
  begin
    for f := 3 downto 0 do
      if (prior[f] = layer) then
        scroll_x_y(f + 1, 5, scroll_x[f] + diff_x[f], scroll_y[f]);
    draw_sprites(layer);
  end;
  update_final_piece(0, 25, 288, 224, 5);
end;

procedure events_system86;
begin
  if event.arcade then
  begin
    // marcade.in0
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // marcade.in1
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // marcade.in2
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.left[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.right[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
  end;
end;

procedure copy_sprites_hw;
var
  i, j: byte;
begin
  for i := 0 to $7F do
  begin
    for j := 10 to 15 do
      memory[$5800 + (i * $10) + j] := memory[$5800 + (i * $10) + j - 6];
  end;
  copy_sprites := false;
end;

procedure system86_loop;
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
        events_system86;
        if f = 240 then
        begin
          update_video_system86;
          m6809_0.change_irq(ASSERT_LINE);
          m6809_1.change_irq(ASSERT_LINE);
          m6800_0.change_irq(HOLD_LINE);
          if copy_sprites then
            copy_sprites_hw;
        end;
        for h := 1 to CPU_SYNC do
        begin
          // Main CPU
          m6809_0.run(frame_main);
          frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
          // Sub CPU
          m6809_1.run(frame_snd);
          frame_snd := frame_snd + m6809_1.tframes - m6809_1.contador;
          // Sound CPU
          m6800_0.run(frame_mcu);
          frame_mcu := frame_mcu + m6800_0.tframes - m6800_0.contador;
        end;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function system86_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $4400 .. $5FFF, $8000 .. $FFFF:
      system86_getbyte := memory[direccion];
    $4000 .. $43FF:
      system86_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
    $6000 .. $7FFF:
      system86_getbyte := rom_bank[rom_nbank, direccion and $1FFF];
  end;
end;

procedure system86_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion shr 1] := true;
        memory[direccion] := valor;
      end;
    $2000 .. $3FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $43FF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $4400 .. $5FFF:
      begin
        memory[direccion] := valor;
        if direccion = $5FF2 then
          copy_sprites := true;
      end;
    $8400:
      m6809_0.change_irq(CLEAR_LINE);
    $8800 .. $8FFF:
      if tile_bank <> ((direccion shr 10) and 1) then
      begin
        tile_bank := (direccion shr 10) and 1;
        fillchar(gfx[0].buffer, $800, 1);
        fillchar(gfx[1].buffer, $800, 1);
      end;
    $9000:
      begin
        prior[0] := (valor and $E) shr 1;
        scroll_x[0] := (scroll_x[0] and $FF) or ((valor and 1) shl 8);
      end;
    $9001:
      scroll_x[0] := (scroll_x[0] and $FF00) or valor;
    $9002:
      scroll_y[0] := valor;
    $9003:
      rom_nbank := valor and 3;
    $9004:
      begin
        prior[1] := (valor and $E) shr 1;
        scroll_x[1] := (scroll_x[1] and $FF) or ((valor and 1) shl 8);
      end;
    $9005:
      scroll_x[1] := (scroll_x[1] and $FF00) or valor;
    $9006:
      scroll_y[1] := valor;
    $9400:
      begin
        prior[2] := (valor and $E) shr 1;
        scroll_x[2] := (scroll_x[2] and $FF) or ((valor and 1) shl 8);
      end;
    $9401:
      scroll_x[2] := (scroll_x[2] and $FF00) or valor;
    $9402:
      scroll_y[2] := valor;
    $9404:
      begin
        prior[3] := (valor and $E) shr 1;
        scroll_x[3] := (scroll_x[3] and $FF) or ((valor and 1) shl 8);
      end;
    $9405:
      scroll_x[3] := (scroll_x[3] and $FF00) or valor;
    $9406:
      scroll_y[3] := valor;
    $A000:
      back_color := valor;
    $6000 .. $83FF, $8401 .. $87FF, $9007 .. $93FF, $9403, $9407 .. $9FFF, $A001 .. $FFFF:
      ; // ROM
  end;
end;

function system86_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    $8000 .. $FFFF:
      system86_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure system86_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $9400:
      m6809_1.change_irq(CLEAR_LINE);
    $8000 .. $93FF, $9401 .. $FFFF:
      ;
  end;
end;

function ret_dsw0: byte;
begin
  ret_dsw0 := ((marcade.dswa and 1) shl 4) or ((marcade.dswa and 4) shl 3) or ((marcade.dswa and $10) shl 2) or ((marcade.dswa and $40) shl 1) or (marcade.dswb and 1) or ((marcade.dswb and 4) shr 1) or ((marcade.dswb and $10) shr 2) or ((marcade.dswb and $40) shr 3);
end;

function ret_dsw1: byte;
begin
  ret_dsw1 := ((marcade.dswa and 2) shl 3) or ((marcade.dswa and 8) shl 2) or ((marcade.dswa and $20) shl 1) or (marcade.dswa and $80) or ((marcade.dswb and 2) shr 1) or ((marcade.dswb and 8) shr 2) or ((marcade.dswb and $20) shr 3) or ((marcade.dswb and $80) shr 4);
end;

function system86_mcu_getbyte(direccion: word): byte;
begin
  case direccion of
    $1000 .. $13FF:
      system86_mcu_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
    $1400 .. $1FFF, $4000 .. $BFFF:
      system86_mcu_getbyte := mem_snd[direccion];
    $2001, $2801, $3801:
      system86_mcu_getbyte := ym2151_0.status;
    $2020, $2820, $3820:
      system86_mcu_getbyte := marcade.in0;
    $2021, $2821, $3821:
      system86_mcu_getbyte := marcade.in1;
    $2030, $2830, $3830:
      system86_mcu_getbyte := ret_dsw0;
    $2031, $2831, $3831:
      system86_mcu_getbyte := ret_dsw1;
  end;
end;

procedure system86_mcu_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $1000 .. $13FF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $1400 .. $1FFF:
      mem_snd[direccion] := valor;
    $2000, $2800, $3800:
      ym2151_0.reg(valor);
    $2001, $2801, $3801:
      ym2151_0.write(valor);
    $4000 .. $BFFF:
      ;
  end;
end;

function system86_in_port1: byte;
begin
  system86_in_port1 := marcade.in2;
end;

function system86_in_port2: byte;
begin
  system86_in_port2 := $FF;
end;

procedure sound_update_system86;
begin
  ym2151_0.update;
  namco_snd_0.update;
end;

// Rolling Thunder
procedure rthunder_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF, $8000 .. $FFFF:
      system86_putbyte(direccion, valor);
    $6000 .. $7FFF:
      case ((direccion and $1E00) shr 9) of
        0, 1, 2, 3:
          namco_63701x_w((direccion and $1E00) shr 9, valor);
        4:
          rom_nbank := valor and $1F;
      end;
  end;
end;

function rthunder_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      rthunder_sub_getbyte := memory[$4000 + direccion]; // sprite ram
    $2000 .. $3FFF:
      rthunder_sub_getbyte := memory[0 + (direccion and $1FFF)]; // video 1 ram
    $4000 .. $5FFF:
      rthunder_sub_getbyte := memory[$2000 + (direccion and $1FFF)]; // video 2 ram
    $6000 .. $7FFF:
      rthunder_sub_getbyte := rom_sub_bank[rom_sub_nbank, direccion and $1FFF];
    $8000 .. $FFFF:
      rthunder_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure rthunder_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      begin // sprite ram
        memory[$4000 + direccion] := valor;
        if direccion = $1FF2 then
          copy_sprites := true;
      end;
    $2000 .. $3FFF:
      if memory[0 + (direccion and $1FFF)] <> valor then
      begin // video 1 ram
        memory[0 + (direccion and $1FFF)] := valor;
        gfx[0].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $4000 .. $5FFF:
      if memory[$2000 + (direccion and $1FFF)] <> valor then
      begin // video 2 ram
        memory[$2000 + (direccion and $1FFF)] := valor;
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $8800:
      m6809_1.change_irq(CLEAR_LINE);
    $D803:
      rom_sub_nbank := valor and 3;
    $8000 .. $87FF, $8801 .. $D802, $D804 .. $FFFF:
      ;
  end;
end;

procedure sound_update_rthunder;
begin
  ym2151_0.update;
  namco_63701x_update;
  namco_snd_0.update;
end;

// The Return to Ishtar
function roishtar_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      roishtar_sub_getbyte := memory[$4000 + direccion]; // sprite ram
    $4000 .. $5FFF:
      roishtar_sub_getbyte := memory[$2000 + (direccion and $1FFF)]; // video 2 ram
    $6000 .. $7FFF:
      roishtar_sub_getbyte := memory[0 + (direccion and $1FFF)]; // video 1 ram
    $8000 .. $FFFF:
      roishtar_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure roishtar_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      begin // sprite ram
        memory[$4000 + direccion] := valor;
        if direccion = $1FF2 then
          copy_sprites := true;
      end;
    $4000 .. $5FFF:
      if memory[$2000 + (direccion and $1FFF)] <> valor then
      begin // video 2 ram
        memory[$2000 + (direccion and $1FFF)] := valor;
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $6000 .. $7FFF:
      if memory[0 + (direccion and $1FFF)] <> valor then
      begin // video 1 ram
        memory[0 + (direccion and $1FFF)] := valor;
        gfx[0].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $B000:
      m6809_1.change_irq(CLEAR_LINE);
    $8000 .. $AFFF, $B002 .. $FFFF:
      ;
  end;
end;

function roishtar_mcu_getbyte(direccion: word): byte;
begin
  case direccion of
    $1000 .. $13FF:
      roishtar_mcu_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
    $1400 .. $1FFF, $2000 .. $3FFF, $8000 .. $BFFF, $F000 .. $FFFF:
      roishtar_mcu_getbyte := mem_snd[direccion];
    $6001:
      roishtar_mcu_getbyte := ym2151_0.status;
    $6020:
      roishtar_mcu_getbyte := marcade.in0;
    $6021:
      roishtar_mcu_getbyte := marcade.in1;
    $6030:
      roishtar_mcu_getbyte := ret_dsw0;
    $6031:
      roishtar_mcu_getbyte := ret_dsw1;
  end;
end;

procedure roishtar_mcu_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $1000 .. $13FF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $1400 .. $1FFF:
      mem_snd[direccion] := valor;
    $6000:
      ym2151_0.reg(valor);
    $6001:
      ym2151_0.write(valor);
    $2000 .. $3FFF, $8000 .. $BFFF, $F000 .. $FFFF:
      ;
  end;
end;

// Genpei ToumaDen
function genpeitd_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $5FFF:
      genpeitd_sub_getbyte := memory[direccion]; // video 1 + video 2 ram + sprite ram
    $8000 .. $FFFF:
      genpeitd_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure genpeitd_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $1FFF:
      if memory[direccion] <> valor then
      begin // video 1 ram
        memory[direccion] := valor;
        gfx[0].buffer[direccion shr 1] := true;
      end;
    $2000 .. $3FFF:
      if memory[direccion] <> valor then
      begin // video 2 ram
        memory[direccion] := valor;
        gfx[1].buffer[direccion shr 1] := true;
      end;
    $4000 .. $5FFF:
      begin // sprite ram
        memory[direccion] := valor;
        if direccion = $5FF2 then
          copy_sprites := true;
      end;
    $8800:
      m6809_1.change_irq(CLEAR_LINE);
    $8000 .. $87FF, $8801 .. $FFFF:
      ;
  end;
end;

// Wonder Momo
function wndrmomo_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    $2000 .. $3FFF:
      wndrmomo_sub_getbyte := memory[$4000 + (direccion and $1FFF)]; // sprite ram
    $4000 .. $5FFF:
      wndrmomo_sub_getbyte := memory[0 + (direccion and $1FFF)]; // video 1 ram
    $6000 .. $7FFF:
      wndrmomo_sub_getbyte := memory[$2000 + (direccion and $1FFF)]; // video 2 ram
    $8000 .. $FFFF:
      wndrmomo_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure wndrmomo_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $2000 .. $3FFF:
      begin // sprite ram
        memory[$4000 + (direccion and $1FFF)] := valor;
        if direccion = $3FF2 then
          copy_sprites := true;
      end;
    $4000 .. $5FFF:
      if memory[0 + (direccion and $1FFF)] <> valor then
      begin // video 1 ram
        memory[0 + (direccion and $1FFF)] := valor;
        gfx[0].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $6000 .. $7FFF:
      if memory[$2000 + (direccion and $1FFF)] <> valor then
      begin // video 2 ram
        memory[$2000 + (direccion and $1FFF)] := valor;
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $C800:
      m6809_1.change_irq(CLEAR_LINE);
    $8000 .. $C7FF, $C801 .. $FFFF:
      ;
  end;
end;

// Main
procedure reset_system86;
var
  f: byte;
begin
  m6809_0.reset;
  m6809_1.reset;
  m6800_0.reset;
  namco_snd_0.reset;
  ym2151_0.reset;
  if ((main_vars.machine_type = 124) or (main_vars.machine_type = 290) or (main_vars.machine_type = 291)) then
    namco_63701x_reset;
  frame_main := m6809_0.tframes;
  frame_snd := m6809_1.tframes;
  frame_mcu := m6800_0.tframes;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $F;
  rom_nbank := 0;
  rom_sub_nbank := 0;
  for f := 0 to 3 do
    scroll_y[f] := 0;
  for f := 0 to 3 do
    scroll_x[f] := 0;
  for f := 0 to 3 do
    prior[f] := 0;
  tile_bank := 0;
  copy_sprites := false;
end;

procedure close_system86;
begin
  if ((main_vars.machine_type = 124) or (main_vars.machine_type = 290) or (main_vars.machine_type = 291)) then
    namco_63701x_close;
end;

function start_system86: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: pbyte;
  ptemp: pbyte;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  ps_x: array [0 .. 31] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 8 * 4, 9 * 4, 10 * 4, 11 * 4, 12 * 4, 13 * 4, 14 * 4, 15 * 4, 16 * 64 + 0 * 4, 16 * 64 + 1 * 4, 16 * 64 + 2 * 4, 16 * 64 + 3 * 4, 16 * 64 + 4 * 4, 16 * 64 + 5 * 4, 16 * 64 + 6 * 4,
    16 * 64 + 7 * 4, 16 * 64 + 8 * 4, 16 * 64 + 9 * 4, 16 * 64 + 10 * 4, 16 * 64 + 11 * 4, 16 * 64 + 12 * 4, 16 * 64 + 13 * 4, 16 * 64 + 14 * 4, 16 * 64 + 15 * 4);
  ps_y: array [0 .. 31] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64, 32 * 64, 33 * 64, 34 * 64, 35 * 64, 36 * 64, 37 * 64, 38 * 64, 39 * 64, 40 * 64, 41 * 64, 42 * 64, 43 * 64,
    44 * 64, 45 * 64, 46 * 64, 47 * 64);
  procedure convert_data(long: dword);
  var
    size, i: dword;
    dest1, dest2, mono: dword;
    buffer: array [0 .. $1FFFF] of byte;
    data1, data2: byte;
  begin
    size := (long * 2) div 3;
    dest1 := 0;
    dest2 := 0 + (size div 2);
    mono := 0 + size;
    copymemory(@buffer[0], @memory_temp[0], size);
    for i := 0 to ((size div 2) - 1) do
    begin
      data1 := buffer[i * 2];
      data2 := buffer[(i * 2) + 1];
      memory_temp[dest1] := (data1 shl 4) or (data2 and $F);
      dest1 := dest1 + 1;
      memory_temp[dest2] := (data1 and $F0) or (data2 shr 4);
      dest2 := dest2 + 1;
      memory_temp[mono] := memory_temp[mono] xor $FF;
      mono := mono + 1;
    end;
  end;
  procedure convert_chars(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[7] := true;
    gfx_set_desc_data(3, 0, 8 * 8, 2 * num * 8 * 8, num * 8 * 8, 0);
    convert_gfx(0, 0, memory_temp, @pc_x, @pc_y, false, false);
  end;
  procedure convert_tiles(num: word);
  begin
    init_gfx(1, 8, 8, num);
    gfx[1].trans[7] := true;
    gfx_set_desc_data(3, 0, 8 * 8, 2 * num * 8 * 8, num * 8 * 8, 0);
    convert_gfx(1, 0, memory_temp, @pc_x, @pc_y, false, false);
  end;
  procedure convert_sprites(num: word);
  begin
    init_gfx(2, 32, 32, num);
    gfx[2].trans[15] := true;
    gfx_set_desc_data(4, 0, 64 * 64, 0, 1, 2, 3);
    convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
    bank_sprites := num div 8;
  end;

begin
  machine_calls.general_loop := system86_loop;
  machine_calls.close := close_system86;
  machine_calls.reset := reset_system86;
  machine_calls.fps_max := 60.606060;
  start_system86 := false;
  start_audio(false);
  screen_init(1, 512, 256, true);
  screen_mod_scroll(1, 512, 512, 511, 256, 256, 255);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 512, 256, true);
  screen_mod_scroll(4, 512, 512, 511, 256, 256, 255);
  screen_init(5, 512, 256, false, true);
  start_video(288, 224);
  getmem(memory_temp, $100000);
  // Main CPU
  m6809_0 := cpu_m6809.Create(49152000 div 32, 264 * CPU_SYNC, TCPU_M6809);
  // Sub CPU
  m6809_1 := cpu_m6809.Create(49152000 div 32, 264 * CPU_SYNC, TCPU_M6809);
  // MCU CPU
  m6800_0 := cpu_m6800.Create(49152000 div 8, 264 * CPU_SYNC, TCPU_HD63701V);
  m6800_0.change_io_calls(system86_in_port1, system86_in_port2, nil, nil, nil, nil, nil, nil);
  if ((main_vars.machine_type = 124) or (main_vars.machine_type = 290) or (main_vars.machine_type = 291)) then
    m6800_0.init_sound(sound_update_rthunder)
  else
    m6800_0.init_sound(sound_update_system86);
  // Sound
  namco_snd_0 := namco_snd_chip.Create(8, true);
  ym2151_0 := ym2151_chip.Create(3579580, 0.4);
  case main_vars.machine_type of
    124:
      begin // Rolling Thunder
        // cargar roms main CPU
        if not(roms_load(@memory, rthunder_rom)) then
          exit;
        m6809_0.change_ram_calls(system86_getbyte, rthunder_putbyte);
        // Pongo las ROMs en su banco
        if not(roms_load(memory_temp, rthunder_rom_bank)) then
          exit;
        for f := 0 to $1F do
          copymemory(@rom_bank[f, 0], @memory_temp[f * $2000], $2000);
        // cargar roms sub CPU
        if not(roms_load(memory_temp, rthunder_sub_rom)) then
          exit;
        m6809_1.change_ram_calls(rthunder_sub_getbyte, rthunder_sub_putbyte);
        // Pongo las ROMs en su banco
        copymemory(@mem_misc[$8000], @memory_temp[$0], $8000);
        for f := 0 to 3 do
          copymemory(@rom_sub_bank[f, 0], @memory_temp[(f * $2000) + $8000], $2000);
        // Cargar MCU
        if not(roms_load(memory_temp, rthunder_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$4000], @memory_temp[$1000], $8000);
        m6800_0.change_ram_calls(system86_mcu_getbyte, system86_mcu_putbyte);
        // Cargar ADPCM
        namco_63701x_start(6000000);
        if not(roms_load(namco_63701_rom, rthunder_adpcm)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, rthunder_chars)) then
          exit;
        convert_data($18000);
        convert_chars($1000);
        // tiles
        if not(roms_load(memory_temp, rthunder_tiles)) then
          exit;
        convert_data($C000);
        convert_tiles($800);
        // sprites
        if not(roms_load(memory_temp, rthunder_sprites)) then
          exit;
        convert_sprites($400);
        // Paleta
        if not(roms_load(memory_temp, rthunder_prom)) then
          exit;
        marcade.dswa := $FF;
        marcade.dswb := $DF;
        marcade.dswa_val := @rthunder_dip_a;
        marcade.dswb_val := @rthunder_dip_b;
      end;
    125:
      begin // Hop Mappy
        // cargar roms main CPU
        if not(roms_load(@memory, hopmappy_rom)) then
          exit;
        m6809_0.change_ram_calls(system86_getbyte, system86_putbyte);
        // cargar roms sub CPU
        if not(roms_load(@mem_misc, hopmappy_sub_rom)) then
          exit;
        m6809_1.change_ram_calls(system86_sub_getbyte, system86_sub_putbyte);
        // Cargar MCU
        if not(roms_load(memory_temp, hopmappy_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$8000], @memory_temp[$1000], $2000);
        m6800_0.change_ram_calls(system86_mcu_getbyte, system86_mcu_putbyte);
        // convertir chars
        fillchar(memory_temp[0], $6000, 0);
        if not(roms_load(memory_temp, hopmappy_chars)) then
          exit;
        convert_data($6000);
        init_gfx(0, 8, 8, $400);
        convert_chars($400);
        // tiles
        fillchar(memory_temp[0], $6000, 0);
        if not(roms_load(memory_temp, hopmappy_tiles)) then
          exit;
        convert_data($6000);
        convert_tiles($400);
        // sprites
        if not(roms_load(memory_temp, hopmappy_sprites)) then
          exit;
        convert_sprites($200);
        // Paleta
        if not(roms_load(memory_temp, hopmappy_prom)) then
          exit;
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswa_val := @hopmappy_dip_a;
        marcade.dswb_val := @hopmappy_dip_b;
      end;
    126:
      begin // Skykid X
        // cargar roms main CPU
        if not(roms_load(memory_temp, skykiddx_rom)) then
          exit;
        copymemory(@memory[$8000], @memory_temp[$8000], $8000);
        for f := 0 to 3 do
          copymemory(@rom_bank[f, 0], @memory_temp[f * $2000], $2000);
        m6809_0.change_ram_calls(system86_getbyte, system86_putbyte);
        // cargar roms sub CPU
        if not(roms_load(@mem_misc, skykiddx_sub_rom)) then
          exit;
        m6809_1.change_ram_calls(system86_sub_getbyte, system86_sub_putbyte);
        // Cargar MCU
        if not(roms_load(memory_temp, skykiddx_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$8000], @memory_temp[$1000], $4000);
        m6800_0.change_ram_calls(system86_mcu_getbyte, system86_mcu_putbyte);
        // convertir chars
        if not(roms_load(memory_temp, skykiddx_chars)) then
          exit;
        convert_data($C000);
        convert_chars($800);
        // tiles
        if not(roms_load(memory_temp, skykiddx_tiles)) then
          exit;
        convert_data($C000);
        convert_tiles($800);
        // sprites
        if not(roms_load(memory_temp, skykiddx_sprites)) then
          exit;
        convert_sprites($200);
        // Paleta
        if not(roms_load(memory_temp, skykiddx_prom)) then
          exit;
        marcade.dswa := $FF;
        // ATENCION! Invierto la pantalla! De verdad el original la tiene invertida???
        marcade.dswb := $FE;
        marcade.dswa_val := @skykiddx_dip_a;
        marcade.dswb_val := @skykiddx_dip_b;
      end;
    289:
      begin // The Return Of Ishtar
        // cargar roms main CPU
        if not(roms_load(memory_temp, roishtar_rom)) then
          exit;
        copymemory(@memory[$8000], @memory_temp[$8000], $8000);
        for f := 0 to 3 do
          copymemory(@rom_bank[f, 0], @memory_temp[f * $2000], $2000);
        m6809_0.change_ram_calls(system86_getbyte, system86_putbyte);
        // cargar roms sub CPU
        if not(roms_load(@mem_misc, roishtar_sub_rom)) then
          exit;
        m6809_1.change_ram_calls(roishtar_sub_getbyte, roishtar_sub_putbyte);
        // Cargar MCU
        if not(roms_load(memory_temp, roishtar_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$8000], @memory_temp[$5000], $4000);
        m6800_0.change_ram_calls(roishtar_mcu_getbyte, roishtar_mcu_putbyte);
        // convertir chars
        fillchar(memory_temp[0], $6000, 0);
        if not(roms_load(memory_temp, roishtar_chars)) then
          exit;
        convert_data($6000);
        init_gfx(0, 8, 8, $400);
        convert_chars($400);
        // tiles
        fillchar(memory_temp[0], $6000, 0);
        if not(roms_load(memory_temp, roishtar_tiles)) then
          exit;
        convert_data($6000);
        convert_tiles($400);
        // sprites
        if not(roms_load(memory_temp, roishtar_sprites)) then
          exit;
        convert_sprites($200);
        // Paleta
        if not(roms_load(memory_temp, roishtar_prom)) then
          exit;
        marcade.dswa := $FF;
        marcade.dswb := $BF;
        marcade.dswa_val := @roishtar_dip_a;
        marcade.dswb_val := @roishtar_dip_b;
      end;
    290:
      begin // Genpei ToumaDen
        // cargar roms main CPU
        if not(roms_load(@memory, genpeitd_rom)) then
          exit;
        m6809_0.change_ram_calls(system86_getbyte, rthunder_putbyte);
        // Pongo las ROMs en su banco
        if not(roms_load(memory_temp, genpeitd_rom_bank)) then
          exit;
        for f := 0 to $7 do
          copymemory(@rom_bank[f, 0], @memory_temp[f * $2000], $2000);
        // cargar roms sub CPU
        if not(roms_load(@mem_misc, genpeitd_sub_rom)) then
          exit;
        m6809_1.change_ram_calls(genpeitd_sub_getbyte, genpeitd_sub_putbyte);
        // Cargar MCU
        if not(roms_load(memory_temp, genpeitd_mcu)) then
          exit;
        m6800_0.change_ram_calls(system86_mcu_getbyte, system86_mcu_putbyte);
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$4000], @memory_temp[$1000], $8000);
        // Cargar ADPCM
        namco_63701x_start(6000000);
        if not(roms_load(namco_63701_rom, genpeitd_adpcm)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, genpeitd_chars)) then
          exit;
        convert_data($18000);
        convert_chars($1000);
        // tiles
        if not(roms_load(memory_temp, genpeitd_tiles)) then
          exit;
        convert_data($C000);
        convert_tiles($800);
        // sprites
        if not(roms_load(memory_temp, genpeitd_sprites)) then
          exit;
        convert_sprites($800);
        // Paleta
        if not(roms_load(memory_temp, genpeitd_prom)) then
          exit;
        marcade.dswa := $FF;
        marcade.dswb := $FC;
        marcade.dswa_val := @genpeitd_dip_a;
        marcade.dswb_val := @genpeitd_dip_b;
      end;
    291:
      begin // Wonder Momo
        // cargar roms main CPU
        if not(roms_load(@memory, wndrmomo_rom)) then
          exit;
        m6809_0.change_ram_calls(system86_getbyte, rthunder_putbyte);
        // Pongo las ROMs en su banco
        if not(roms_load(memory_temp, wndrmomo_rom_bank)) then
          exit;
        for f := 0 to $7 do
          copymemory(@rom_bank[f, 0], @memory_temp[f * $2000], $2000);
        // cargar roms sub CPU
        if not(roms_load(@mem_misc, wndrmomo_sub_rom)) then
          exit;
        m6809_1.change_ram_calls(wndrmomo_sub_getbyte, wndrmomo_sub_putbyte);
        // Cargar MCU
        if not(roms_load(memory_temp, wndrmomo_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$4000], @memory_temp[$1000], $8000);
        m6800_0.change_ram_calls(system86_mcu_getbyte, system86_mcu_putbyte);
        // Cargar ADPCM
        namco_63701x_start(6000000);
        if not(roms_load(namco_63701_rom, wndrmomo_adpcm)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, wndrmomo_chars)) then
          exit;
        convert_data($C000);
        convert_chars($800);
        // tiles
        if not(roms_load(memory_temp, wndrmomo_tiles)) then
          exit;
        convert_data($C000);
        convert_tiles($800);
        // sprites
        if not(roms_load(memory_temp, wndrmomo_sprites)) then
          exit;
        convert_sprites($400);
        // Paleta
        if not(roms_load(memory_temp, wndrmomo_prom)) then
          exit;
        marcade.dswa := $F7;
        marcade.dswb := $FB;
        marcade.dswa_val := @wndrmomo_dip_a;
        marcade.dswb_val := @wndrmomo_dip_b;
      end;
  end;
  for f := 0 to $1FF do
  begin
    colores[f].r := ((memory_temp[f] shr 0) and 1) * $E + ((memory_temp[f] shr 1) and 1) * $1F + ((memory_temp[f] shr 2) and 1) * $43 + ((memory_temp[f] shr 3) and 1) * $8F;
    colores[f].g := ((memory_temp[f] shr 4) and 1) * $E + ((memory_temp[f] shr 5) and 1) * $1F + ((memory_temp[f] shr 6) and 1) * $43 + ((memory_temp[f] shr 7) and 1) * $8F;
    colores[f].b := ((memory_temp[f + $200] shr 0) and 1) * $E + ((memory_temp[f + $200] shr 1) and 1) * $1F + ((memory_temp[f + $200] shr 2) and 1) * $43 + ((memory_temp[f + $200] shr 3) and 1) * $8F;
  end;
  set_pal(colores, $200);
  // tiles/sprites color table
  for f := 0 to $7FF do
  begin
    gfx[0].colores[f] := memory_temp[$400 + f];
    gfx[1].colores[f] := memory_temp[$400 + f];
    gfx[2].colores[f] := memory_temp[$400 + $800 + f];
  end;
  // color prom used at run time
  copymemory(@nchar_prom[0], @memory_temp[$1400], $20);
  // final
  freemem(memory_temp);
  start_system86 := true;
end;

end.
