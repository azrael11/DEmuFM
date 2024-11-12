unit galaxian_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  ay_8910,
  controls_engine,
  gfx_engine,
  timer_engine,
  samples,
  rom_engine,
  file_engine,
  pal_engine,
  sound_engine,
  ppi8255,
  misc_functions,
  konami_snd,
  galaxian_stars;

function start_galaxian: boolean;

implementation

const
  // Galaxian
  galaxian_rom: array [0 .. 4] of tipo_roms = ((n: 'galmidw.u'; l: $800; p: 0; crc: $745E2D61), (n: 'galmidw.v'; l: $800; p: $800; crc: $9C999A40), (n: 'galmidw.w'; l: $800; p: $1000; crc: $B5894925), (n: 'galmidw.y'; l: $800; p: $1800; crc: $6B3CA10B), (n: '7l'; l: $800;
    p: $2000; crc: $1B933207));
  galaxian_char: array [0 .. 1] of tipo_roms = ((n: '1h.bin'; l: $800; p: 0; crc: $39FB43A4), (n: '1k.bin'; l: $800; p: $800; crc: $7E3F56A2));
  galaxian_pal: tipo_roms = (n: '6l.bpr'; l: $20; p: 0; crc: $C3AC9467);
  galaxian_samples: array [0 .. 8] of tipo_nombre_samples = ((nombre: 'fire.wav'), (nombre: 'death.wav'), (nombre: 'back1.wav'), (nombre: 'back2.wav'), (nombre: 'back3.wav'), (nombre: 'kill.wav'; restart: true), (nombre: 'coin.wav'), (nombre: 'music.wav'), (nombre: 'extra.wav'));
  galaxian_dip_a: array [0 .. 1] of def_dip2 = ((mask: $20; name: 'Cabinet'; number: 2; val2: (0, $20); name2: ('Upright', 'Cocktail')), ());
  galaxian_dip_b: array [0 .. 1] of def_dip2 = ((mask: $C0; name: 'Coinage'; number: 4; val4: ($40, 0, $80, $C0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), ());
  galaxian_dip_c: array [0 .. 2] of def_dip2 = ((mask: 3; name: 'Bonus Life'; number: 4; val4: (0, 1, 2, 3); name4: ('7K', '10K', '12K', '20K')), (mask: 4; name: 'Lives'; number: 2; val2: (0, 4); name2: ('2', '3')), ());
  // Jump Bug
  jumpbug_rom: array [0 .. 6] of tipo_roms = ((n: 'jb1'; l: $1000; p: 0; crc: $415AA1B7), (n: 'jb2'; l: $1000; p: $1000; crc: $B1C27510), (n: 'jb3'; l: $1000; p: $2000; crc: $97C24BE2), (n: 'jb4'; l: $1000; p: $3000; crc: $66751D12), (n: 'jb5'; l: $1000; p: $8000;
    crc: $E2D66FAF), (n: 'jb6'; l: $1000; p: $9000; crc: $49E0BDFD), (n: 'jb7'; l: $800; p: $A000; crc: $83D71302));
  jumpbug_char: array [0 .. 5] of tipo_roms = ((n: 'jbl'; l: $800; p: 0; crc: $9A091B0A), (n: 'jbm'; l: $800; p: $800; crc: $8A0FC082), (n: 'jbn'; l: $800; p: $1000; crc: $155186E0), (n: 'jbi'; l: $800; p: $1800; crc: $7749B111), (n: 'jbj'; l: $800; p: $2000; crc: $06E8D7DF),
    (n: 'jbk'; l: $800; p: $2800; crc: $B8DBDDF3));
  jumpbug_pal: tipo_roms = (n: 'l06_prom.bin'; l: $20; p: 0; crc: $6A0C7D87);
  jumpbug_dip_b: array [0 .. 1] of def_dip2 = ((mask: $40; name: 'Difficulty'; number: 2; val2: (0, 1); name2: ('Easy', 'Hard')), ());
  jumpbug_dip_c: array [0 .. 2] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 2, 3, 0); name4: ('3', '4', '5', 'Infinite')), (mask: $C; name: 'Coinage'; number: 4; val4: (4, 8, 0, $C);
    name4: ('A 2C 1C-B 2C 1C', 'A 2C 1C-B 1C 3C', 'A 1C 1C-B 1C 1C', 'A 1C 1C-B 1C 6C')), ());
  // Moon Cresta
  mooncrst_rom: array [0 .. 7] of tipo_roms = ((n: 'mc1'; l: $800; p: 0; crc: $7D954A7A), (n: 'mc2'; l: $800; p: $800; crc: $44BB7CFA), (n: 'mc3'; l: $800; p: $1000; crc: $9C412104), (n: 'mc4'; l: $800; p: $1800; crc: $7E9B1AB5), (n: 'mc5.7r'; l: $800; p: $2000; crc: $16C759AF),
    (n: 'mc6.8d'; l: $800; p: $2800; crc: $69BCAFDB), (n: 'mc7.8e'; l: $800; p: $3000; crc: $B50DBC46), (n: 'mc8'; l: $800; p: $3800; crc: $18CA312B));
  mooncrst_char: array [0 .. 3] of tipo_roms = ((n: 'mcs_b'; l: $800; p: 0; crc: $FB0F1F81), (n: 'mcs_d'; l: $800; p: $800; crc: $13932A15), (n: 'mcs_a'; l: $800; p: $1000; crc: $631EBB5A), (n: 'mcs_c'; l: $800; p: $1800; crc: $24CFD145));
  mooncrst_pal: tipo_roms = (n: 'l06_prom.bin'; l: $20; p: 0; crc: $6A0C7D87);
  mooncrst_samples: array [0 .. 4] of tipo_nombre_samples = ((nombre: 'fire.wav'), (nombre: 'death.wav'), (nombre: 'ship1.wav'), (nombre: 'enemy1.wav'), (nombre: 'back3.wav'));
  mooncrst_dip_b: array [0 .. 2] of def_dip2 = ((mask: $40; name: 'Bonus Life'; number: 2; val2: (0, $40); name2: ('30K', '50K')), (mask: $80; name: 'Language'; number: 2; val2: ($80, 0); name2: ('English', 'Japanese')), ());
  mooncrst_dip_c: array [0 .. 2] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (3, 2, 1, 0); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, 4, 8, $C); name4: ('1C 1C', '1C 2C', '1C 3C', 'Free Play')), ());
  // Scramble
  scramble_rom: array [0 .. 7] of tipo_roms = ((n: 's1.2d'; l: $800; p: 0; crc: $EA35CCAA), (n: 's2.2e'; l: $800; p: $800; crc: $E7BBA1B3), (n: 's3.2f'; l: $800; p: $1000; crc: $12D7FC3E), (n: 's4.2h'; l: $800; p: $1800; crc: $B59360EB), (n: 's5.2j'; l: $800; p: $2000;
    crc: $4919A91C), (n: 's6.2l'; l: $800; p: $2800; crc: $26A4547B), (n: 's7.2m'; l: $800; p: $3000; crc: $0BB49470), (n: 's8.2p'; l: $800; p: $3800; crc: $6A5740E5));
  scramble_char: array [0 .. 1] of tipo_roms = ((n: 'c2.5f'; l: $800; p: 0; crc: $4708845B), (n: 'c1.5h'; l: $800; p: $800; crc: $11FD2887));
  scramble_sound: array [0 .. 2] of tipo_roms = ((n: 'ot1.5c'; l: $800; p: 0; crc: $BCD297F0), (n: 'ot2.5d'; l: $800; p: $800; crc: $DE7912DA), (n: 'ot3.5e'; l: $800; p: $1000; crc: $BA2FA933));
  scramble_pal: tipo_roms = (n: 'c01s.6e'; l: $20; p: 0; crc: $4E3CAEAB);
  scramble_dip_a: array [0 .. 1] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (0, 1, 2, 3); name4: ('3', '4', '5', '255')), ());
  scramble_dip_b: array [0 .. 2] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (0, 2, 4, 6); name4: ('A 1C/1C-B 2C/1C-C 1C/1C', 'A 1C/2C-B 1C/1C-C 1C/2C', 'A 1C/3C-B 3C/1C-C 1C/3C', 'A 1C/4C-B 4C/1C-C 1C/4C')), (mask: 8; name: 'Cabinet'; number: 2; val2: (0, 8);
    name2: ('Upright', 'Cocktail')), ());
  // Super Cobra
  scobra_rom: array [0 .. 5] of tipo_roms = ((n: 'epr1265.2c'; l: $1000; p: 0; crc: $A0744B3F), (n: '2e'; l: $1000; p: $1000; crc: $8E7245CD), (n: 'epr1267.2f'; l: $1000; p: $2000; crc: $47A4E6FB), (n: '2h'; l: $1000; p: $3000; crc: $7244F21C), (n: 'epr1269.2j'; l: $1000;
    p: $4000; crc: $E1F8A801), (n: '2l'; l: $1000; p: $5000; crc: $D52AFFDE));
  scobra_sound: array [0 .. 2] of tipo_roms = ((n: '5c'; l: $800; p: 0; crc: $D4346959), (n: '5d'; l: $800; p: $800; crc: $CC025D95), (n: '5e'; l: $800; p: $1000; crc: $1628C53F));
  scobra_char: array [0 .. 1] of tipo_roms = ((n: 'epr1274.5h'; l: $800; p: 0; crc: $64D113B4), (n: 'epr1273.5f'; l: $800; p: $800; crc: $A96316D3));
  scobra_pal: tipo_roms = (n: '82s123.6e'; l: $20; p: 0; crc: $9B87F90D);
  scobra_dip_a: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Allow Continue'; number: 2; val2: (0, 1); name2: ('No', '4 Times')), (mask: 2; name: 'Lives'; number: 2; val2: (0, 2); name2: ('3', '4')), ());
  scobra_dip_b: array [0 .. 2] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (4, 6, 2, 0); name4: ('2C 1C', '4C 3C', '1C 1C', '99 Credits')), (mask: 8; name: 'Cabinet'; number: 2; val2: (0, 8); name2: ('Upright', 'Cocktail')), ());
  // Frogger
  frogger_rom: array [0 .. 2] of tipo_roms = ((n: 'frogger.26'; l: $1000; p: 0; crc: $597696D6), (n: 'frogger.27'; l: $1000; p: $1000; crc: $B6E6FCC3), (n: 'frsm3.7'; l: $1000; p: $2000; crc: $ACA22AE0));
  frogger_char: array [0 .. 1] of tipo_roms = ((n: 'frogger.607'; l: $800; p: 0; crc: $05F7D883), (n: 'frogger.606'; l: $800; p: $800; crc: $F524EE30));
  frogger_pal: tipo_roms = (n: 'pr-91.6l'; l: 32; p: 0; crc: $413703BF);
  frogger_sound: array [0 .. 2] of tipo_roms = ((n: 'frogger.608'; l: $800; p: 0; crc: $E8AB0256), (n: 'frogger.609'; l: $800; p: $800; crc: $7380A48F), (n: 'frogger.610'; l: $800; p: $1000; crc: $31D7EB27));
  frogger_dip_a: array [0 .. 1] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (0, 1, 2, 3); name4: ('3', '5', '7', '256')), ());
  frogger_dip_b: array [0 .. 2] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (2, 4, 0, 6); name4: ('A 2C/1C-B 2C/1C-C 2C/1C', 'A 2C/1C-B 1C/3C-C 2C/1C', 'A 1C/1C-B 1C/1C-C 1C/1C', 'A 1C/1C-B 1C/6C-C 1C/1C')), (mask: 8; name: 'Cabinet'; number: 2; val2: (0, 8);
    name2: ('Upright', 'Cocktail')), ());
  // Amidar
  amidar_rom: array [0 .. 4] of tipo_roms = ((n: '1.2c'; l: $1000; p: 0; crc: $621B74DE), (n: '2.2e'; l: $1000; p: $1000; crc: $38538B98), (n: '3.2f'; l: $1000; p: $2000; crc: $099ECB24), (n: '4.2h'; l: $1000; p: $3000; crc: $BA149A93), (n: '5.2j'; l: $1000; p: $4000;
    crc: $EECC1ABF));
  amidar_char: array [0 .. 1] of tipo_roms = ((n: 'c2.5f'; l: $800; p: 0; crc: $2CFE5EDE), (n: 'c2.5d'; l: $800; p: $800; crc: $57C4FD0D));
  amidar_pal: tipo_roms = (n: 'amidar.clr'; l: 32; p: 0; crc: $F940DCC3);
  amidar_sound: array [0 .. 1] of tipo_roms = ((n: 's1.5c'; l: $1000; p: 0; crc: $8CA7B750), (n: 's2.5d'; l: $1000; p: $1000; crc: $9B5BDC0A));
  amidar_dip_a: array [0 .. 1] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '255')), ());
  amidar_dip_b: array [0 .. 3] of def_dip2 = ((mask: 2; name: 'Demo Sounds'; number: 2; val2: (2, 0); name2: ('Off', 'On')), (mask: 4; name: 'Bonus Life'; number: 2; val2: (0, 4); name2: ('30K 70K+', '50K 80K+')), (mask: 8; name: 'Cabinet'; number: 2; val2: (0, 8);
    name2: ('Upright', 'Cocktail')), ());
  amidar_dip_c: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (4, $A, 1, 2, 8, $F, $C, $E, 7, 6, $B, 3, $D, 5, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($40, $A0, $10, $20, $80, $F0, $C0, $E0, $70, $60, $B0, $30, $D0, $50, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Invalid')), ());
  // Ant Eater
  anteater_rom: array [0 .. 3] of tipo_roms = ((n: 'ra1-2c'; l: $1000; p: 0; crc: $58BC9393), (n: 'ra1-2e'; l: $1000; p: $1000; crc: $574FC6F6), (n: 'ra1-2f'; l: $1000; p: $2000; crc: $2F7C1FE5), (n: 'ra1-2h'; l: $1000; p: $3000; crc: $AE8A5DA3));
  anteater_char: array [0 .. 1] of tipo_roms = ((n: 'ra6-5f'; l: $800; p: 0; crc: $4C3F8A08), (n: 'ra6-5h'; l: $800; p: $800; crc: $B30C7C9F));
  anteater_sound: array [0 .. 1] of tipo_roms = ((n: 'ra4-5c'; l: $800; p: 0; crc: $87300B4F), (n: 'ra4-5d'; l: $800; p: $800; crc: $AF4E5FFE));
  anteater_pal: tipo_roms = (n: 'colr6f.cpu'; l: $20; p: 0; crc: $FCE333C7);
  anteater_dip_a: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Lives'; number: 2; val2: (1, 0); name2: ('3', '5')), (mask: 2; name: 'Demo Sounds'; number: 2; val2: (2, 0); name2: ('Off', 'On')), ());
  anteater_dip_b: array [0 .. 1] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (2, 0, 4, 6); name4: ('A 1C/1C-B 1C/1C', 'A 1C/2C-B 2C/1C', 'A 1C/3C-B 3C/1C', 'A 1C/4C-B 4C/1C')), ());
  // Armored Car
  armoredcar_rom: array [0 .. 4] of tipo_roms = ((n: 'cpu.2c'; l: $1000; p: 0; crc: $0D7BFDFB), (n: 'cpu.2e'; l: $1000; p: $1000; crc: $76463213), (n: 'cpu.2f'; l: $1000; p: $2000; crc: $2CC6D5F0), (n: 'cpu.2h'; l: $1000; p: $3000; crc: $61278DBB), (n: 'cpu.2j'; l: $1000;
    p: $4000; crc: $FB158D8C));
  armoredcar_char: array [0 .. 1] of tipo_roms = ((n: 'cpu.5f'; l: $800; p: 0; crc: $8A3DA4D1), (n: 'cpu.5h'; l: $800; p: $800; crc: $85BDB113));
  armoredcar_sound: array [0 .. 1] of tipo_roms = ((n: 'sound.5c'; l: $800; p: 0; crc: $54EE7753), (n: 'sound.5d'; l: $800; p: $800; crc: $5218FEC0));
  armoredcar_pal: tipo_roms = (n: '82s123.6e'; l: $20; p: 0; crc: $9B87F90D);
  armoredcar_dip_b: array [0 .. 2] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (2, 0, 4, 6); name4: ('A 1C/1C-B 1C/1C', 'A 1C/2C-B 2C/1C', 'A 1C/3C-B 3C/1C', 'A 1C/4C-B 4C/1C')), (mask: 8; name: 'Cabinet'; number: 2; val2: (8, 0);
    name2: ('Upright', 'Cocktail')), ());
  // The End
  theend_rom: array [0 .. 5] of tipo_roms = ((n: 'ic13_1t.bin'; l: $800; p: 0; crc: $93E555BA), (n: 'ic14_2t.bin'; l: $800; p: $800; crc: $2DE7AD27), (n: 'ic15_3t.bin'; l: $800; p: $1000; crc: $035F750B), (n: 'ic16_4t.bin'; l: $800; p: $1800; crc: $61286B5C), (n: 'ic17_5t.bin';
    l: $800; p: $2000; crc: $434A8F68), (n: 'ic18_6t.bin'; l: $800; p: $2800; crc: $DC4CC786));
  theend_char: array [0 .. 1] of tipo_roms = ((n: 'ic30_2c.bin'; l: $800; p: 0; crc: $68CCF7BF), (n: 'ic31_1c.bin'; l: $800; p: $800; crc: $4A48C999));
  theend_sound: array [0 .. 1] of tipo_roms = ((n: 'ic56_1.bin'; l: $800; p: 0; crc: $7A141F29), (n: 'ic55_2.bin'; l: $800; p: $800; crc: $218497C1));
  theend_pal: tipo_roms = (n: '6331-1j.86'; l: $20; p: 0; crc: $24652BC4);
  theend_dip_a: array [0 .. 1] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (0, 1, 2, 3); name4: ('3', '4', '5', '256')), ());
  theend_dip_b: array [0 .. 2] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (4, 2, 0, 6); name4: ('3C 1C', '2C 1C', '1C 1C', '1C 2C')), (mask: 8; name: 'Cabinet'; number: 2; val2: (0, 8); name2: ('Upright', 'Cocktail')), ());
  // Battle of Atlantis
  atlantis_rom: array [0 .. 5] of tipo_roms = ((n: '2c'; l: $800; p: 0; crc: $0E485B9A), (n: '2e'; l: $800; p: $800; crc: $C1640513), (n: '2f'; l: $800; p: $1000; crc: $EEC265EE), (n: '2h'; l: $800; p: $1800; crc: $A5D2E442), (n: '2j'; l: $800; p: $2000; crc: $45F7CF34),
    (n: '2l'; l: $800; p: $2800; crc: $F335B96B));
  atlantis_char: array [0 .. 1] of tipo_roms = ((n: '5f'; l: $800; p: 0; crc: $57F9C6B9), (n: '5h'; l: $800; p: $800; crc: $E989F325));
  atlantis_sound: array [0 .. 2] of tipo_roms = ((n: 'ot1.5c'; l: $800; p: 0; crc: $BCD297F0), (n: 'ot2.5d'; l: $800; p: $800; crc: $DE7912DA), (n: 'ot3.5e'; l: $800; p: $1000; crc: $BA2FA933));
  atlantis_pal: tipo_roms = (n: 'c01s.6e'; l: $20; p: 0; crc: $4E3CAEAB);
  atlantis_dip_a: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Cabinet'; number: 2; val2: (0, 1); name2: ('Upright', 'Cocktail')), (mask: 2; name: 'Lives'; number: 2; val2: (2, 0); name2: ('3', '5')), ());
  atlantis_dip_b: array [0 .. 1] of def_dip2 = ((mask: $E; name: 'Coinage'; number: 4; val4: (2, 0, 4, 8); name4: ('A 1C/3C-B 2C/1C', 'A 1C/6C-B 1C/1C', 'A 1C/99C-B 1C/99C', 'Invalid')), ());
  // Calipso
  calipso_rom: array [0 .. 5] of tipo_roms = ((n: 'calipso.2c'; l: $1000; p: 0; crc: $0FCB703C), (n: 'calipso.2e'; l: $1000; p: $1000; crc: $C6622F14), (n: 'calipso.2f'; l: $1000; p: $2000; crc: $7BACBABA), (n: 'calipso.2h'; l: $1000; p: $3000; crc: $A3A8111B), (n: 'calipso.2j';
    l: $1000; p: $4000; crc: $FCBD7B9E), (n: 'calipso.2l'; l: $1000; p: $5000; crc: $F7630CAB));
  calipso_char: array [0 .. 1] of tipo_roms = ((n: 'calipso.5f'; l: $2000; p: 0; crc: $FD4252E9), (n: 'calipso.5h'; l: $2000; p: $2000; crc: $1663A73A));
  calipso_sound: array [0 .. 1] of tipo_roms = ((n: 'calipso.5c'; l: $800; p: 0; crc: $9CBC65AB), (n: 'calipso.5d'; l: $800; p: $800; crc: $A225EE3B));
  calipso_pal: tipo_roms = (n: 'calipso.clr'; l: $20; p: 0; crc: $01165832);
  calipso_dip_a: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Lives'; number: 2; val2: (1, 0); name2: ('3', '5')), (mask: 2; name: 'Demo Sounds'; number: 2; val2: (2, 0); name2: ('Off', 'On')), ());
  calipso_dip_b: array [0 .. 1] of def_dip2 = ((mask: 6; name: 'Coinage'; number: 4; val4: (2, 0, 4, 6); name4: ('A 1C/1C-B 1C/1C', 'A 1C/2C-B 2C/1C', 'A 1C/3C-B 3C/1C', 'A 1C/4C-B 4C/1C')), ());
  // Cavelon
  cavelon_rom: array [0 .. 2] of tipo_roms = ((n: '2.bin'; l: $2000; p: 0; crc: $A3B353AC), (n: '1.bin'; l: $2000; p: $2000; crc: $3F62EFD6), (n: '3.bin'; l: $2000; p: $4000; crc: $39D74E4E));
  cavelon_char: array [0 .. 1] of tipo_roms = ((n: 'h.bin'; l: $1000; p: 0; crc: $D44FCD6F), (n: 'k.bin'; l: $1000; p: $1000; crc: $59BC7F9E));
  cavelon_sound: tipo_roms = (n: '1c_snd.bin'; l: $800; p: 0; crc: $F58DCF55);
  cavelon_pal: tipo_roms = (n: 'cavelon.clr'; l: $20; p: 0; crc: $D133356B);
  cavelon_dip_a: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Cabinet'; number: 2; val2: (1, 0); name2: ('Upright', 'Cocktail')), (mask: 2; name: 'Coinage'; number: 2; val2: (0, 2); name2: ('A 1C/1C-B 1C/6C', 'A 2C/1C-B 1C/3C')), ());
  cavelon_dip_b: array [0 .. 1] of def_dip2 = ((mask: 6; name: 'Lives'; number: 4; val4: (0, 4, 2, 6); name4: ('5', '4', '3', '2')), ());
  BACK_COLOR = 35;

var
  // variables de funciones especificas
  events_hardware_galaxian: procedure;
  calc_nchar: function(direccion: word): word;
  calc_sprite: procedure(direccion: byte; var nchar: word; var flipx: boolean; var flipy: boolean);
  draw_stars: procedure(screen: byte);
  draw_bullet: procedure;
  galaxian_update_video: procedure;
  sound1_pos, sound2_pos, sound3_pos, sound4_pos, sound_pos: byte;
  sound_data: array [0 .. 3] of byte;
  // Variables globales
  haz_nmi: boolean;
  backgroud_type, port_b_latch: byte;
  videoram_mem: array [0 .. $3FF] of byte;
  sprite_mem, disparo_mem: array [0 .. $1F] of byte;
  atributos_mem: array [0 .. $3F] of byte;
  gfx_bank: array [0 .. 4] of byte;
  // scramble
  scramble_background: boolean;
  scramble_prot_state: word;
  scramble_prot: byte;
  // amidar
  amidar_back_r, amidar_back_g, amidar_back_b: byte;
  // frogger
  timer_hs_frogger: byte;
  // cavelon
  roms: array [0 .. 1, 0 .. $3FFF] of byte;
  num_rom: byte;

  // Keys
procedure events_galaxian;
begin
  if event.arcade then
  begin
    // P1 & System
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // P2 & System
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
  end;
end;

procedure events_frogger;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
    // P1 & P2
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := marcade.in2 and $EF
    else
      marcade.in2 := marcade.in2 or $10;
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := marcade.in2 and $BF
    else
      marcade.in2 := marcade.in2 or $40;
  end;
end;

procedure events_jumpbug;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // P2
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
  end;
end;

procedure events_scramble;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // P1 & P2
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
  end;
end;

procedure events_anteater;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // Start
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
  end;
end;

procedure events_amidar;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
    // Misc
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := marcade.in2 and $EF
    else
      marcade.in2 := marcade.in2 or $10;
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := marcade.in2 and $BF
    else
      marcade.in2 := marcade.in2 or $40;
  end;
end;

procedure events_calipso;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // Start
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
  end;
end;

function galaxian_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      galaxian_getbyte := memory[direccion];
    $4000 .. $47FF:
      galaxian_getbyte := memory[$4000 + (direccion and $3FF)];
    $5000 .. $57FF:
      galaxian_getbyte := videoram_mem[direccion and $3FF];
    $5800 .. $5FFF:
      case (direccion and $FF) of
        0 .. $3F:
          galaxian_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          galaxian_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          galaxian_getbyte := disparo_mem[direccion and $1F];
      else
        galaxian_getbyte := memory[$5800 + (direccion and $FF)];
      end;
    $6000 .. $67FF:
      galaxian_getbyte := marcade.in0 or marcade.dswa;
    $6800 .. $6FFF:
      galaxian_getbyte := marcade.in1 or marcade.dswb;
    $7000 .. $77FF:
      galaxian_getbyte := marcade.dswc;
  else
    galaxian_getbyte := $FF;
  end;
end;

procedure galaxian_putbyte(direccion: word; valor: byte);
var
  f, dir: byte;
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $47FF:
      memory[$4000 + (direccion and $3FF)] := valor;
    $5000 .. $57FF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $5800 .. $5FFF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
      else
        memory[$5800 + (direccion and $FF)] := valor;
      end;
    $6800 .. $6FFF:
      case (direccion and 7) of
        0:
          if (valor <> 0) then
            start_sample(2);
        1:
          if (valor <> 0) then
            start_sample(3);
        2:
          if (valor <> 0) then
            start_sample(4);
        3:
          if (valor <> 0) then
            start_sample(1);
        5:
          if (valor <> 0) then
            start_sample(0);
      end;
    $7000 .. $77FF:
      case (direccion and 7) of
        1:
          begin
            haz_nmi := ((valor and 1) <> 0);
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        4:
          galaxian_stars_0.enable_w((valor and 1) <> 0);
      end;
    $7800:
      begin
        sound_data[sound_pos] := valor;
        case valor of
          0:
            sound4_pos := sound_pos;
          4:
            sound2_pos := sound_pos;
          142:
            sound1_pos := sound_pos;
          208:
            sound3_pos := sound_pos;
          255:
            exit;
        end;
        sound_pos := (sound_pos + 1) and 3;
        if ((sound_data[sound1_pos] = 142) and (sound_data[(sound1_pos + 1) and 3] = 128) and (sound_data[(sound1_pos + 2) and 3] = 112) and (sound_data[(sound1_pos + 3) and 3] = 104)) then
          start_sample(5);
        if ((sound_data[sound2_pos] = 4) and (sound_data[(sound2_pos + 1) and 3] = 8) and (sound_data[(sound2_pos + 2) and 3] = 12) and (sound_data[(sound2_pos + 3) and 3] = 16)) then
          start_sample(6);
        if ((sound_data[sound3_pos] = 208) and (sound_data[(sound3_pos + 1) and 3] = 205) and (sound_data[(sound3_pos + 2) and 3] = 199) and (sound_data[(sound3_pos + 3) and 3] = 192)) then
          start_sample(7);
        if ((sound_data[sound4_pos] = 0) and (sound_data[(sound4_pos + 1) and 3] = 28) and (sound_data[(sound4_pos + 2) and 3] = 64) and (sound_data[(sound4_pos + 3) and 3] = 85)) then
          start_sample(8);
        // principal1.statusbar1.panels[2].text:=inttostr(valor);
      end;
  end;
end;

function galaxian_calc_nchar(direccion: word): word;
begin
  galaxian_calc_nchar := videoram_mem[direccion and $3FF];
end;

procedure galaxian_calc_sprite(direccion: byte; var nchar: word; var flipx: boolean; var flipy: boolean);
var
  tempb: byte;
begin
  tempb := sprite_mem[1 + (direccion * 4)];
  nchar := tempb and $3F;
  flipx := (tempb and $80) <> 0;
  flipy := (tempb and $40) <> 0;
end;

procedure galaxian_draw_bullet;
var
  f, x, y: byte;
  tempw: word;
begin
  for f := 0 to 7 do
  begin
    y := 250 - disparo_mem[3 + (f * 4)];
    if f > 2 then
      y := y + 1;
    if f = 7 then
      tempw := paleta[33]
    else
      tempw := paleta[32];
    x := disparo_mem[1 + (f * 4)];
    putpixel(x + ADD_SPRITE, y + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 1 + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 2 + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 3 + ADD_SPRITE, 1, @tempw, 1);
  end;
end;

procedure galaxian_update_sound;
begin
  samples_update;
end;

// Jump Bug
function jumpbug_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF, $8000 .. $AFFF:
      jumpbug_getbyte := memory[direccion];
    $4800 .. $4FFF:
      jumpbug_getbyte := videoram_mem[direccion and $3FF];
    $5000 .. $57FF:
      case (direccion and $FF) of
        0 .. $3F:
          jumpbug_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          jumpbug_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          jumpbug_getbyte := disparo_mem[direccion and $1F];
      else
        jumpbug_getbyte := memory[$5000 + (direccion and $FF)];
      end;
    $6000 .. $67FF:
      jumpbug_getbyte := marcade.in0 or marcade.dswa;
    $6800 .. $6FFF:
      jumpbug_getbyte := marcade.in1 or marcade.dswb;
    $7000 .. $77FF:
      jumpbug_getbyte := marcade.dswc;
    $B000 .. $BFFF:
      case (direccion and $FFF) of // proteccion
        $114:
          jumpbug_getbyte := $4F;
        $118:
          jumpbug_getbyte := $D3;
        $214:
          jumpbug_getbyte := $CF;
        $235:
          jumpbug_getbyte := 2;
        $311:
          jumpbug_getbyte := $FF;
      else
        jumpbug_getbyte := 0;
      end;
  else
    jumpbug_getbyte := $FF;
  end;
end;

procedure jumpbug_putbyte(direccion: word; valor: byte);
var
  f, dir: byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $AFFF:
      ;
    $4000 .. $47FF:
      memory[direccion] := valor;
    $4800 .. $4FFF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $5000 .. $57FF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
      else
        memory[$5000 + (direccion and $FF)] := valor;
      end;
    $5800 .. $58FF:
      ay8910_0.Write(valor);
    $5900 .. $59FF:
      ay8910_0.Control(valor);
    $6000 .. $67FF:
      case direccion and 7 of
        2 .. 6:
          if gfx_bank[(direccion and 7) - 2] <> valor then
          begin
            gfx_bank[(direccion and 7) - 2] := valor;
            fillchar(gfx[0].buffer[0], $400, 1);
          end;
      end;
    $7000 .. $77FF:
      case direccion and 7 of
        1:
          begin
            haz_nmi := (valor and 1) <> 0;
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        4:
          galaxian_stars_0.enable_w((valor and 1) <> 0);
      end;
  end;
end;

function jumpbug_calc_nchar(direccion: word): word;
var
  charcode: word;
begin
  charcode := videoram_mem[direccion and $3FF];
  if (((charcode and $C0) = $80) and ((gfx_bank[2] and 1) <> 0)) then
  begin
    charcode := charcode + (128 + ((gfx_bank[0] and 1) shl 6) + ((gfx_bank[1] and 1) shl 7) + ((not(gfx_bank[4]) and 1) shl 8));
  end;
  jumpbug_calc_nchar := charcode;
end;

procedure jumpbug_calc_sprite(direccion: byte; var nchar: word; var flipx: boolean; var flipy: boolean);
var
  tempb: byte;
begin
  tempb := sprite_mem[1 + (direccion * 4)];
  flipx := (tempb and $80) <> 0;
  flipy := (tempb and $40) <> 0;
  nchar := tempb and $3F;
  if (((nchar and $30) = $20) and ((gfx_bank[2] and 1) <> 0)) then
    nchar := nchar + (32 + ((gfx_bank[0] and 1) shl 4) + ((gfx_bank[1] and 1) shl 5) + ((not(gfx_bank[4]) and 1) shl 6));
end;

procedure jumpbug_update_sound;
begin
  ay8910_0.update;
end;

// Moon Cresta
function mooncrst_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      mooncrst_getbyte := memory[direccion];
    $8000 .. $87FF:
      mooncrst_getbyte := memory[$8000 + (direccion and $3FF)];
    $9000 .. $97FF:
      mooncrst_getbyte := videoram_mem[direccion and $3FF];
    $9800 .. $9FFF:
      case (direccion and $FF) of
        0 .. $3F:
          mooncrst_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          mooncrst_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          mooncrst_getbyte := disparo_mem[direccion and $1F];
      else
        mooncrst_getbyte := memory[$9800 + (direccion and $FF)];
      end;
    $A000 .. $A7FF:
      mooncrst_getbyte := marcade.in0 or marcade.dswa;
    $A800 .. $AFFF:
      mooncrst_getbyte := marcade.in1 or marcade.dswb;
    $B000 .. $B7FF:
      mooncrst_getbyte := marcade.dswc;
  else
    mooncrst_getbyte := $FF;
  end;
end;

procedure mooncrst_putbyte(direccion: word; valor: byte);
var
  f, dir: byte;
begin
  case direccion of
    0 .. $3FFF:
      ;
    $8000 .. $87FF:
      memory[$8000 + (direccion and $3FF)] := valor;
    $9000 .. $97FF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $9800 .. $9FFF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
      else
        memory[$9800 + (direccion and $FF)] := valor;
      end;
    $A000 .. $A7FF:
      case (direccion and 7) of
        0 .. 2:
          if gfx_bank[direccion - $A000] <> valor then
          begin
            gfx_bank[direccion - $A000] := valor;
            fillchar(gfx[0].buffer[0], $400, 1);
          end;
      end;
    $A800 .. $AFFF:
      case (direccion and 7) of
        0, 1, 2:
          ;
        3:
          if (valor <> 0) then
            start_sample(1);
        5:
          if (valor <> 0) then
            start_sample(0);
        6:
          sound1_pos := valor;
        7:
          begin
            if ((sound1_pos = 7) and (valor = 131)) then
              start_sample(2);
            if ((sound1_pos = 2) and (valor = 1)) then
              start_sample(3);
          end;
      end;
    $B000 .. $B7FF:
      case (direccion and 7) of
        0:
          begin
            haz_nmi := (valor and 1) <> 0;
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        4:
          galaxian_stars_0.enable_w((valor and 1) <> 0);
      end;
  end;
end;

function mooncrst_calc_nchar(direccion: word): word;
var
  charcode: word;
begin
  charcode := videoram_mem[direccion and $3FF];
  if ((gfx_bank[2] <> 0) and ((charcode and $C0) = $80)) then
    charcode := (charcode and $3F) or (gfx_bank[0] shl 6) or (gfx_bank[1] shl 7) or $100;
  mooncrst_calc_nchar := charcode and $1FF;
end;

procedure mooncrst_calc_sprite(direccion: byte; var nchar: word; var flipx: boolean; var flipy: boolean);
var
  tempb: byte;
begin
  tempb := sprite_mem[1 + (direccion * 4)];
  flipx := (tempb and $80) <> 0;
  flipy := (tempb and $40) <> 0;
  nchar := tempb and $3F;
  if ((gfx_bank[2] <> 0) and ((nchar and $30) = $20)) then
    nchar := (nchar and $F) or (gfx_bank[0] shl 4) or (gfx_bank[1] shl 5) or $40;
  nchar := nchar and $7F;
end;

// Scramble
procedure scramble_draw_bullet;
var
  f, x, y: byte;
  tempw: word;
begin
  for f := 0 to 7 do
  begin
    y := 249 - disparo_mem[3 + (f * 4)];
    if f > 2 then
      y := y + 1;
    tempw := paleta[32];
    x := disparo_mem[$1 + (f * 4)];
    putpixel(x + ADD_SPRITE, y + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 1 + ADD_SPRITE, 1, @tempw, 1);
  end;
end;

function scramble_ppi8255_r(direccion: word): byte;
var
  res: byte;
begin
  res := $FF;
  if (direccion and $100) <> 0 then
    res := res and pia8255_0.read(direccion and 3);
  if (direccion and $200) <> 0 then
    res := res and pia8255_1.read(direccion and 3);
  scramble_ppi8255_r := res;
end;

function scramble_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF:
      scramble_getbyte := memory[direccion];
    $4800 .. $4FFF:
      scramble_getbyte := videoram_mem[direccion and $3FF];
    $5000 .. $5FFF:
      case (direccion and $FF) of
        0 .. $3F:
          scramble_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          scramble_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          scramble_getbyte := disparo_mem[direccion and $1F];
        $80 .. $FF:
          scramble_getbyte := memory[$5000 + (direccion and $FF)];
      end;
    $8000 .. $FFFF:
      scramble_getbyte := scramble_ppi8255_r(direccion);
  else
    scramble_getbyte := $FF;
  end;
end;

procedure scramble_ppi8255_w(direccion: word; valor: byte);
begin
  if (direccion and $100) <> 0 then
    pia8255_0.Write(direccion and 3, valor);
  if (direccion and $200) <> 0 then
    pia8255_1.Write(direccion and 3, valor);
end;

procedure scramble_putbyte(direccion: word; valor: byte);
var
  f, dir: byte;
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $47FF:
      memory[direccion] := valor;
    $4800 .. $4FFF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $5000 .. $5FFF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
        $80 .. $FF:
          memory[$5000 + (direccion and $FF)] := valor;
      end;
    $6800 .. $6FFF:
      case (direccion and 7) of
        1:
          begin
            haz_nmi := (valor and 1) <> 0;
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        3:
          scramble_background := ((valor and 1) <> 0);
        4:
          galaxian_stars_0.enable_w((valor and 1) <> 0);
      end;
    $8000 .. $FFFF:
      scramble_ppi8255_w(direccion, valor);
  end;
end;

// 8255 scramble
function scramble_port_1_c_read: byte;
begin
  scramble_port_1_c_read := scramble_prot;
end;

procedure scramble_port_1_a_write(valor: byte);
begin
  konamisnd_0.sound_latch := valor;
end;

function scramble_port_0_c_read: byte;
begin
  scramble_port_0_c_read := marcade.in2 or marcade.dswb or (scramble_prot and $80) or ((scramble_prot and $80) shr 2);
end;

procedure scramble_port_1_b_write(valor: byte);
var
  old: byte;
begin
  old := port_b_latch;
  port_b_latch := valor;
  if (((old and 8) <> 0) and ((not(valor and 8)) <> 0)) then
    konamisnd_0.pedir_irq := HOLD_LINE;
  // device->machine().sound().system_mute(data & 0x10);
end;

procedure scramble_port_1_c_write(valor: byte);
var
  num1, num2, op: byte;
  res: integer;
begin
  scramble_prot_state := (scramble_prot_state shl 4) or (valor and $F);
  num1 := (scramble_prot_state shr 8) and $F;
  num2 := (scramble_prot_state shr 4) and $F;
  op := scramble_prot_state and $F;
  case op of
    6:
      scramble_prot := scramble_prot xor $80;
    9:
      begin
        scramble_prot := num1 + 1;
        if scramble_prot > $F then
          scramble_prot := $F0
        else
          scramble_prot := scramble_prot shl 4;
      end;
    $A:
      scramble_prot := 0;
    $B:
      begin
        res := num2 - num1;
        if res < 0 then
          scramble_prot := 0
        else
          scramble_prot := res shl 4;
      end;
    $F:
      begin
        res := num1 - num2;
        if res < 0 then
          scramble_prot := 0
        else
          scramble_prot := res shl 4;
      end;
  end;
end;

function scobra_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      scobra_getbyte := memory[direccion];
    $8000 .. $87FF, $C000 .. $C7FF:
      scobra_getbyte := memory[$8000 + (direccion and $7FF)];
    $8800 .. $8BFF, $C800 .. $CBFF:
      scobra_getbyte := videoram_mem[direccion and $3FF];
    $9000 .. $97FF, $D000 .. $D7FF:
      case (direccion and $FF) of
        0 .. $3F:
          scobra_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          scobra_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          scobra_getbyte := disparo_mem[direccion and $1F];
        $80 .. $FF:
          scobra_getbyte := memory[$9000 + (direccion and $FF)];
      end;
    $9800 .. $9FFF, $D800 .. $DFFF:
      scobra_getbyte := pia8255_0.read(direccion and 3);
    $A000 .. $A7FF, $E000 .. $E7FF:
      scobra_getbyte := pia8255_1.read(direccion and 3);
  else
    scobra_getbyte := $FF;
  end;
end;

procedure scobra_putbyte(direccion: word; valor: byte);
var
  f, dir: byte;
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF, $C000 .. $C7FF:
      memory[$8000 + (direccion and $7FF)] := valor;
    $8800 .. $8BFF, $C800 .. $CBFF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $9000 .. $97FF, $D000 .. $D7FF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
        $80 .. $FF:
          memory[$9000 + (direccion and $FF)] := valor;
      end;
    $9800 .. $9FFF, $D800 .. $DFFF:
      pia8255_0.Write(direccion and 3, valor);
    $A000 .. $A7FF, $E000 .. $E7FF:
      pia8255_1.Write(direccion and 3, valor);
    $A800 .. $AFFF, $E800 .. $EFFF:
      case (direccion and 7) of
        1:
          begin
            haz_nmi := (valor and 1) <> 0;
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        3:
          scramble_background := ((valor and 1) <> 0);
        4:
          galaxian_stars_0.enable_w((valor and 1) <> 0);
      end;
  end;
end;

// Frogger
function frogger_ppi8255_r(direccion: word): byte;
var
  res: byte;
begin
  // the decoding here is very simplistic, and you can address both simultaneously
  res := $FF;
  if (direccion and $1000) <> 0 then
    res := res and pia8255_1.read((direccion shr 1) and 3);
  if (direccion and $2000) <> 0 then
    res := res and pia8255_0.read((direccion shr 1) and 3);
  frogger_ppi8255_r := res;
end;

function frogger_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $87FF:
      frogger_getbyte := memory[direccion];
    $A800 .. $AFFF:
      frogger_getbyte := videoram_mem[direccion and $3FF];
    $B000 .. $B7FF:
      case (direccion and $FF) of
        0 .. $3F:
          frogger_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          frogger_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          frogger_getbyte := disparo_mem[direccion and $1F];
      else
        frogger_getbyte := memory[$B000 + (direccion and $FF)];
      end;
    $C000 .. $FFFF:
      frogger_getbyte := frogger_ppi8255_r(direccion);
  else
    frogger_getbyte := $FF;
  end;
end;

procedure frogger_ppi8255_w(direccion: word; valor: byte);
begin
  if (direccion and $1000) <> 0 then
    pia8255_1.Write((direccion shr 1) and 3, valor);
  if (direccion and $2000) <> 0 then
    pia8255_0.Write((direccion shr 1) and 3, valor);
end;

procedure frogger_putbyte(direccion: word; valor: byte);
var
  dir, f: byte;
begin
  case direccion of
    0 .. $3FFF:
      ;
    $8000 .. $87FF:
      memory[direccion] := valor;
    $A800 .. $AFFF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $B000 .. $B7FF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
      else
        memory[$B000 + (direccion and $FF)] := valor;
      end;
    $B800 .. $BFFF:
      case (direccion and $1F) of
        8:
          begin
            haz_nmi := (valor and 1) <> 0;
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
      end;
    $C000 .. $FFFF:
      frogger_ppi8255_w(direccion, valor);
  end;
end;

function frogger_port_0_a_read: byte;
begin
  frogger_port_0_a_read := marcade.in0;
end;

function frogger_port_0_b_read: byte;
begin
  frogger_port_0_b_read := marcade.in1 or marcade.dswa;
end;

function frogger_port_0_c_read: byte;
begin
  frogger_port_0_c_read := marcade.in2 or marcade.dswb;
end;

procedure frogger_port_1_a_write(valor: byte);
begin
  konamisnd_0.sound_latch := valor;
end;

procedure frogger_port_1_b_write(valor: byte);
begin
  if ((port_b_latch = 0) and (valor <> 0)) then
    konamisnd_0.pedir_irq := HOLD_LINE;
  port_b_latch := valor;
end;

procedure frogger_hi_score;
begin
  if ((memory[$83F1] = $63) and (memory[$83F2] = 4)) then
  begin
    load_hi('frogger.hi', @memory[$83F1], 10);
    copymemory(@memory[$83EF], @memory[$83F1], 2);
    timers.enabled(timer_hs_frogger, false);
  end;
end;

// amidar
function amidar_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      amidar_getbyte := memory[direccion];
    $8000 .. $87FF, $C000 .. $C7FF:
      amidar_getbyte := memory[$8000 - (direccion and $7FF)];
    $9000 .. $97FF, $D000 .. $D7FF:
      amidar_getbyte := videoram_mem[direccion and $3FF];
    $9800 .. $9FFF, $D800 .. $DFFF:
      case (direccion and $FF) of
        0 .. $3F:
          amidar_getbyte := atributos_mem[direccion and $3F];
        $40 .. $5F:
          amidar_getbyte := sprite_mem[direccion and $1F];
        $60 .. $7F:
          amidar_getbyte := disparo_mem[direccion and $1F];
      else
        amidar_getbyte := memory[$9800 + (direccion and $FF)];
      end;
    $B000 .. $B7FF, $F000 .. $F7FF:
      amidar_getbyte := pia8255_0.read((direccion shr 4) and 3);
    $B800 .. $BFFF, $F800 .. $FFFF:
      amidar_getbyte := pia8255_1.read((direccion shr 4) and 3);
  else
    amidar_getbyte := $FF;
  end;
end;

procedure amidar_putbyte(direccion: word; valor: byte);
var
  dir, f: byte;
  color: tcolor;
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF, $C000 .. $C7FF:
      memory[$8000 - (direccion and $7FF)] := valor;
    $9000 .. $97FF, $D000 .. $D7FF:
      if videoram_mem[direccion and $3FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        videoram_mem[direccion and $3FF] := valor;
      end;
    $9800 .. $9FFF, $D800 .. $DFFF:
      case (direccion and $FF) of
        0 .. $3F:
          if atributos_mem[direccion and $3F] <> valor then
          begin
            atributos_mem[direccion and $3F] := valor;
            dir := ((direccion and $3F) shr 1);
            for f := 0 to $1F do
              gfx[0].buffer[dir + (f shl 5)] := true;
          end;
        $40 .. $5F:
          sprite_mem[direccion and $1F] := valor;
        $60 .. $7F:
          disparo_mem[direccion and $1F] := valor;
      else
        memory[$9800 + (direccion and $FF)] := valor;
      end;
    $A000 .. $A7FF, $E000 .. $E7FF:
      case (direccion and $3F) of
        0, $20, $28:
          begin
            case (direccion and $3F) of
              0:
                amidar_back_r := (valor and 1) * $55;
              $20:
                amidar_back_g := (valor and 1) * $47;
              $28:
                amidar_back_b := (valor and 1) * $55;
            end;
            color.r := amidar_back_r;
            color.g := amidar_back_g;
            color.b := amidar_back_b;
            set_pal_color(color, BACK_COLOR);
          end;
        8:
          begin
            haz_nmi := (valor and 1) <> 0;
            if not(haz_nmi) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
      end;
    $B000 .. $B7FF, $F000 .. $F7FF:
      pia8255_0.Write((direccion shr 4) and 3, valor);
    $B800 .. $BFFF, $F800 .. $FFFF:
      pia8255_1.Write((direccion shr 4) and 3, valor);
  end;
end;

function amidar_port_1_c_read: byte;
begin
  amidar_port_1_c_read := marcade.dswc;
end;

// The End
procedure theend_draw_bullet;
var
  f, x, y: byte;
  tempw: word;
begin
  for f := 0 to 7 do
  begin
    y := 250 - disparo_mem[3 + (f * 4)];
    if f > 2 then
      y := y + 1;
    if f = 7 then
      tempw := paleta[34]
    else
      tempw := paleta[32];
    x := disparo_mem[1 + (f * 4)];
    putpixel(x + ADD_SPRITE, y + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 1 + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 2 + ADD_SPRITE, 1, @tempw, 1);
    putpixel(x + ADD_SPRITE, y + 3 + ADD_SPRITE, 1, @tempw, 1);
  end;
end;

// Calipso
procedure calipso_calc_sprite(direccion: byte; var nchar: word; var flipx: boolean; var flipy: boolean);
begin
  nchar := sprite_mem[1 + (direccion * 4)];
  flipx := false;
  flipy := false;
end;

// Cavelon
function cavelon_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      cavelon_getbyte := roms[num_rom, direccion];
    $4000 .. $7FFF:
      cavelon_getbyte := scramble_getbyte(direccion);
    $8000 .. $FFFF:
      begin
        num_rom := num_rom xor 1;
        cavelon_getbyte := scramble_ppi8255_r(direccion);
      end;
  end;
end;

procedure cavelon_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $7FFF:
      scramble_putbyte(direccion, valor);
    $8000 .. $FFFF:
      begin
        num_rom := num_rom xor 1;
        scramble_ppi8255_w(direccion, valor);
      end;
  end;
end;

procedure cavelon_calc_sprite(direccion: byte; var nchar: word; var flipx: boolean; var flipy: boolean);
var
  tempb: byte;
begin
  tempb := sprite_mem[1 + (direccion * 4)];
  nchar := tempb and $3F;
  flipx := (tempb and $80) <> 0;
  flipy := (tempb and $40) <> 0;
  nchar := nchar or ((sprite_mem[2 + (direccion * 4)] and $30) shl 2);
end;

// Video
procedure update_video_frogger;
var
  f, color, nchar: word;
  scroll, x, y, atrib: byte;
begin
  // Fondo con la mitad azul...
  for f := 0 to 127 do
    single_line(ADD_SPRITE, f + ADD_SPRITE, paleta[BACK_COLOR], 256, 1);
  for f := 128 to 255 do
    single_line(ADD_SPRITE, f + ADD_SPRITE, paleta[0], 256, 1);
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := atributos_mem[y * 2];
      color := atributos_mem[1 + (y * 2)];
      color := (((color shr 1) and 3) + ((color shl 2) and 4)) shl 2;
      scroll := (x * 8) + ((atrib and $F) shl 4) + (atrib shr 4);
      nchar := videoram_mem[f];
      put_gfx_trans(scroll, y * 8, nchar, color, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 1);
  for f := 7 downto 0 do
  begin
    y := sprite_mem[3 + (f * 4)] + 1;
    if y < 16 then
      continue;
    atrib := sprite_mem[1 + (f * 4)];
    nchar := atrib and $3F;
    color := sprite_mem[2 + (f * 4)];
    color := (((color shr 1) and 3) + ((color shl 2) and 4)) shl 2;
    x := ((sprite_mem[f * 4] and $F) shl 4) + (sprite_mem[f * 4] shr 4);
    put_gfx_sprite(nchar, color, (atrib and $80) <> 0, (atrib and $40) <> 0, 1);
    update_gfx_sprite(x, y, 1, 1);
  end;
  update_final_piece(16, 0, 224, 256, 1);
end;

procedure update_video_galaxian;
var
  f, color, nchar: word;
  scroll, x, y: byte;
  flipx, flipy: boolean;
begin
  case backgroud_type of
    0:
      fill_full_screen(1, 150);
    1:
      if scramble_background then
        fill_full_screen(1, BACK_COLOR)
      else
        fill_full_screen(1, 150);
    2:
      fill_full_screen(1, BACK_COLOR);
    3:
      if scramble_background then
      begin
        for f := 0 to 55 do
          single_line(ADD_SPRITE, f + ADD_SPRITE, paleta[BACK_COLOR], 256, 1);
        for f := 56 to 255 do
          single_line(ADD_SPRITE, f + ADD_SPRITE, paleta[0], 256, 1)
      end
      else
        fill_full_screen(1, 150);
  end;
  draw_stars(1);
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      color := (atributos_mem[1 + (y shl 1)] and 7) shl 2;
      scroll := (x * 8) + atributos_mem[y shl 1];
      nchar := calc_nchar(f);
      put_gfx_trans(scroll, y * 8, nchar, color, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 24, 256, 232, 2, 0, 24, 256, 232, 1);
  draw_bullet;
  for f := 7 downto 0 do
  begin
    y := sprite_mem[3 + (f * 4)] + 1;
    if y < 16 then
      continue;
    calc_sprite(f, nchar, flipx, flipy);
    color := (sprite_mem[2 + (f * 4)] and 7) shl 2;
    x := sprite_mem[f * 4];
    put_gfx_sprite(nchar, color, flipx, flipy, 1);
    update_gfx_sprite(x, y, 1, 1);
  end;
  update_region(0, 0, 256, 24, 2, 0, 0, 256, 24, 1);
  update_final_piece(16, 0, 224, 256, 1);
end;

procedure frogger_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 248 then
        begin
          if haz_nmi then
            z80_0.change_nmi(ASSERT_LINE);
          galaxian_update_video;
        end;
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // SND
        konamisnd_0.run;
      end;

      events_hardware_galaxian;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure galaxian_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 248 then
        begin
          if haz_nmi then
            z80_0.change_nmi(ASSERT_LINE);
          update_video_galaxian;
        end;
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
      end;
      events_hardware_galaxian;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Main
procedure reset_galaxian;
begin
  z80_0.reset;
  frame_main := z80_0.tframes;
  reset_audio;
  haz_nmi := false;
  if main_vars.machine_type <> 14 then
    galaxian_stars_0.reset;
  scramble_background := false;
  port_b_latch := 0;
  sound1_pos := 0;
  sound2_pos := 0;
  sound3_pos := 0;
  sound4_pos := 0;
  sound_pos := 0;
  scramble_prot := 0;
  scramble_prot_state := 0;
  amidar_back_r := 0;
  amidar_back_g := 0;
  amidar_back_b := 0;
  num_rom := 0;
  case main_vars.machine_type of
    14, 143, 144, 145, 363, 366, 369, 370, 382, 385:
      begin
        konamisnd_0.reset;
        pia8255_0.reset;
        pia8255_1.reset;
        marcade.in0 := $FF;
        marcade.in1 := $FC;
        marcade.in2 := $51;
      end;
    47, 49:
      begin
        marcade.in0 := 0;
        marcade.in1 := 0;
        reset_samples;
      end;
    48:
      begin
        marcade.in0 := 0;
        marcade.in1 := 0;
        ay8910_0.reset;
      end;
  end;
end;

procedure close_galaxian;
begin
  if main_vars.machine_type = 14 then
    save_hi('frogger.hi', @memory[$83F1], 10);
end;

function start_galaxian: boolean;
var
  colores: tpaleta;
  f, x: word;
  ctemp1, ctemp2: byte;
  memory_temp: array [0 .. $AFFF] of byte;
  memory_temp2: array [0 .. $FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
  procedure convert_chars(n: word);
  begin
    init_gfx(0, 8, 8, n);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(2, 0, 8 * 8, 0, n * 8 * 8);
    convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;
  procedure convert_sprt(n: word);
  begin
    init_gfx(1, 16, 16, n);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(2, 0, 16 * 16, 0, n * 16 * 16);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;

begin
  machine_calls.general_loop := frogger_loop;
  events_hardware_galaxian := events_scramble;
  calc_nchar := galaxian_calc_nchar;
  calc_sprite := galaxian_calc_sprite;
  draw_bullet := scramble_draw_bullet;
  galaxian_update_video := update_video_galaxian;
  machine_calls.close := close_galaxian;
  machine_calls.reset := reset_galaxian;
  backgroud_type := 0;
  machine_calls.fps_max := 60.6060606060;
  start_galaxian := false;
  start_audio(false);
  screen_init(1, 256, 512, true, true);
  screen_init(2, 512, 256, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 256);
  marcade.dswc := $FF;
  case main_vars.machine_type of
    14:
      begin // frogger
        events_hardware_galaxian := events_frogger;
        calc_nchar := nil;
        calc_sprite := nil;
        galaxian_update_video := update_video_frogger;
        // Main CPU
        z80_0.change_ram_calls(frogger_getbyte, frogger_putbyte);
        if not(roms_load(@memory, frogger_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_FROGGER, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, frogger_sound)) then
          exit;
        // Las ROMS tienen lineas movidas...
        for f := 0 to $7FF do
          konamisnd_0.memory[f] := BITSWAP8(konamisnd_0.memory[f], 7, 6, 5, 4, 3, 2, 0, 1);
        // Hi-Score
        timer_hs_frogger := timers.init(z80_0.numero_cpu, 10000, frogger_hi_score, nil, true);
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, nil, frogger_port_1_a_write, frogger_port_1_b_write, nil);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, frogger_char)) then
          exit;
        // la rom tiene cambiadas D1 y D0
        for f := $800 to $FFF do
          memory_temp[f] := BITSWAP8(memory_temp[f], 7, 6, 5, 4, 3, 2, 0, 1);
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, frogger_pal)) then
          exit;
        // DIP
        marcade.dswa := 0;
        marcade.dswa_val2 := @scramble_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @scramble_dip_b;
      end;
    47:
      begin // galaxian
        machine_calls.general_loop := galaxian_loop;
        events_hardware_galaxian := events_galaxian;
        draw_bullet := galaxian_draw_bullet;
        // Main CPU
        z80_0.change_ram_calls(galaxian_getbyte, galaxian_putbyte);
        // cargar roms
        if not(roms_load(@memory, galaxian_rom)) then
          exit;
        // cargar samples
        if load_samples(galaxian_samples) then
          z80_0.init_sound(galaxian_update_sound);
        // convertir chars &sprites
        if not(roms_load(@memory_temp, galaxian_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, galaxian_pal)) then
          exit;
        // DIP
        marcade.dswa := 0;
        marcade.dswa_val2 := @galaxian_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @galaxian_dip_b;
        marcade.dswc := 4;
        marcade.dswc_val2 := @galaxian_dip_c;
      end;
    48:
      begin // Jump Bug
        machine_calls.general_loop := galaxian_loop;
        events_hardware_galaxian := events_jumpbug;
        calc_nchar := jumpbug_calc_nchar;
        calc_sprite := jumpbug_calc_sprite;
        // Main CPU
        z80_0.change_ram_calls(jumpbug_getbyte, jumpbug_putbyte);
        if not(roms_load(@memory, jumpbug_rom)) then
          exit;
        // chip de sonido
        z80_0.init_sound(jumpbug_update_sound);
        ay8910_0 := ay8910_chip.create(1500000, AY8910, 1);
        // convertir chars &sprites
        if not(roms_load(@memory_temp, jumpbug_char)) then
          exit;
        convert_chars($300);
        convert_sprt($C0);
        if not(roms_load(@memory_temp, jumpbug_pal)) then
          exit;
        marcade.dswa := 0;
        marcade.dswa_val2 := @galaxian_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @jumpbug_dip_b;
        marcade.dswc := 1;
        marcade.dswc_val2 := @jumpbug_dip_c;
      end;
    49:
      begin // mooncrst
        machine_calls.general_loop := galaxian_loop;
        events_hardware_galaxian := events_galaxian;
        calc_nchar := mooncrst_calc_nchar;
        calc_sprite := mooncrst_calc_sprite;
        draw_bullet := galaxian_draw_bullet;
        // Main CPU
        z80_0.change_ram_calls(mooncrst_getbyte, mooncrst_putbyte);
        if not(roms_load(@memory, mooncrst_rom)) then
          exit;
        // Desencriptarlas
        for f := 0 to $3FFF do
        begin
          ctemp1 := memory[f];
          ctemp2 := ctemp1;
          if ((ctemp1 and 2) <> 0) then
            ctemp2 := ctemp2 xor $40;
          if ((ctemp1 and $20) <> 0) then
            ctemp2 := ctemp2 xor 4;
          if ((f and 1) = 0) then
            ctemp1 := BITSWAP8(ctemp2, 7, 2, 5, 4, 3, 6, 1, 0)
          else
            ctemp1 := ctemp2;
          memory[f] := ctemp1;
        end;
        if load_samples(mooncrst_samples) then
          z80_0.init_sound(galaxian_update_sound);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, mooncrst_char)) then
          exit;
        convert_chars($200);
        convert_sprt($80);
        if not(roms_load(@memory_temp, mooncrst_pal)) then
          exit;
        marcade.dswa := 0;
        marcade.dswa_val2 := @galaxian_dip_a;
        marcade.dswb := $80;
        marcade.dswb_val2 := @mooncrst_dip_b;
        marcade.dswc := 0;
        marcade.dswc_val2 := @mooncrst_dip_c;
      end;
    143:
      begin // scramble
        backgroud_type := 1;
        // Main CPU
        z80_0.change_ram_calls(scramble_getbyte, scramble_putbyte);
        if not(roms_load(@memory, scramble_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, scramble_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, scramble_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, scramble_port_1_c_read, scramble_port_1_a_write, scramble_port_1_b_write, scramble_port_1_c_write);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, scramble_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, scramble_pal)) then
          exit;
        marcade.dswa := 0;
        marcade.dswa_val2 := @scramble_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @scramble_dip_b;
        marcade.dswc := $FF;
      end;
    144:
      begin // super cobra
        backgroud_type := 1;
        // Main CPU
        z80_0.change_ram_calls(scobra_getbyte, scobra_putbyte);
        if not(roms_load(@memory, scobra_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, scobra_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, nil, scramble_port_1_a_write, scramble_port_1_b_write, nil);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, scobra_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, scobra_pal)) then
          exit;
        marcade.dswa := 1;
        marcade.dswa_val2 := @scobra_dip_a;
        marcade.dswb := 2;
        marcade.dswb_val2 := @scobra_dip_b;
        marcade.dswc := $FF;
      end;
    145:
      begin // amidar
        backgroud_type := 2;
        // Main CPU
        z80_0.change_ram_calls(amidar_getbyte, amidar_putbyte);
        if not(roms_load(@memory, amidar_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, amidar_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, amidar_port_1_c_read, scramble_port_1_a_write, scramble_port_1_b_write, nil);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, amidar_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, amidar_pal)) then
          exit;
        // DIP
        marcade.dswa := 3;
        marcade.dswa_val2 := @amidar_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @amidar_dip_b;
        marcade.dswc := $FF;
        marcade.dswc_val2 := @amidar_dip_c;
      end;
    363:
      begin // ant eater
        backgroud_type := 3;
        events_hardware_galaxian := events_anteater;
        // Main CPU
        z80_0.change_ram_calls(scobra_getbyte, scobra_putbyte);
        if not(roms_load(@memory, anteater_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, anteater_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, nil, scramble_port_1_a_write, scramble_port_1_b_write, nil);
        // convertir chars & sprites
        if not(roms_load(@memory_temp2, anteater_char)) then
          exit;
        for f := 0 to $FFF do
        begin
          x := f and $9BF;
          x := x or ((BIT_n(f, 4) xor BIT_n(f, 9) xor (BIT_n(f, 2) and BIT_n(f, 10))) shl 6);
          x := x or ((BIT_n(f, 2) xor BIT_n(f, 10)) shl 9);
          x := x or ((BIT_n(f, 0) xor BIT_n(f, 6) xor 1) shl 10);
          memory_temp[f] := memory_temp2[x];
        end;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, anteater_pal)) then
          exit;
        // DIP
        marcade.dswa := 1;
        marcade.dswa_val2 := @anteater_dip_a;
        marcade.dswb := 2;
        marcade.dswb_val2 := @anteater_dip_b;
      end;
    366:
      begin // armored car
        backgroud_type := 1;
        // Main CPU
        z80_0.change_ram_calls(scobra_getbyte, scobra_putbyte);
        if not(roms_load(@memory, armoredcar_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, armoredcar_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, nil, scramble_port_1_a_write, scramble_port_1_b_write, nil);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, armoredcar_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, armoredcar_pal)) then
          exit;
        // DIP
        marcade.dswa := 1;
        marcade.dswa_val2 := @anteater_dip_a;
        marcade.dswb := $A;
        marcade.dswb_val2 := @armoredcar_dip_b;
      end;
    369:
      begin // the end
        backgroud_type := 1;
        draw_bullet := theend_draw_bullet;
        // Main CPU
        z80_0.change_ram_calls(scramble_getbyte, scramble_putbyte);
        if not(roms_load(@memory, theend_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, theend_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, scramble_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, scramble_port_1_c_read, scramble_port_1_a_write, scramble_port_1_b_write, scramble_port_1_c_write);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, theend_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, theend_pal)) then
          exit;
        // DIP
        marcade.dswa := 0;
        marcade.dswa_val2 := @theend_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @theend_dip_b;
      end;
    370:
      begin // Battle of Atlantis
        backgroud_type := 1;
        // Main CPU
        z80_0.change_ram_calls(scramble_getbyte, scramble_putbyte);
        if not(roms_load(@memory, atlantis_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, atlantis_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, scramble_port_1_c_read, scramble_port_1_a_write, scramble_port_1_b_write, scramble_port_1_c_write);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, atlantis_char)) then
          exit;
        convert_chars($100);
        convert_sprt($40);
        if not(roms_load(@memory_temp, atlantis_pal)) then
          exit;
        // DIP
        marcade.dswa := 2;
        marcade.dswa_val2 := @atlantis_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val2 := @atlantis_dip_b;
      end;
    382:
      begin // calipso
        backgroud_type := 1;
        events_hardware_galaxian := events_calipso;
        calc_sprite := calipso_calc_sprite;
        // Main CPU
        z80_0.change_ram_calls(scobra_getbyte, scobra_putbyte);
        if not(roms_load(@memory, calipso_rom)) then
          exit;
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, calipso_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, frogger_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, nil, scramble_port_1_a_write, scramble_port_1_b_write, nil);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, calipso_char)) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 8 * 8, 0, $200 * 2 * 8 * 8);
        convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
        convert_sprt($100);
        if not(roms_load(@memory_temp, calipso_pal)) then
          exit;
        // DIP
        marcade.dswa := 1;
        marcade.dswa_val2 := @calipso_dip_a;
        marcade.dswb := $FA;
        marcade.dswb_val2 := @calipso_dip_b;
      end;
    385:
      begin // cavelon
        backgroud_type := 1;
        calc_sprite := cavelon_calc_sprite;
        // Main CPU
        z80_0.change_ram_calls(cavelon_getbyte, cavelon_putbyte);
        if not(roms_load(@memory_temp, cavelon_rom)) then
          exit;
        copymemory(@roms[0, 0], @memory_temp[0], $4000);
        copymemory(@roms[1, $2000], @memory_temp[$2000], $2000);
        copymemory(@roms[1, 0], @memory_temp[$4000], $2000);
        // Sound
        konamisnd_0 := konamisnd_chip.create(2, TIPO_SCRAMBLE, 1789750, 256);
        if not(roms_load(@konamisnd_0.memory, cavelon_sound)) then
          exit;
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(frogger_port_0_a_read, frogger_port_0_b_read, scramble_port_0_c_read, nil, nil, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(nil, nil, scramble_port_1_c_read, scramble_port_1_a_write, scramble_port_1_b_write, scramble_port_1_c_write);
        // convertir chars & sprites
        if not(roms_load(@memory_temp, cavelon_char)) then
          exit;
        convert_chars($200);
        convert_sprt($80);
        if not(roms_load(@memory_temp, cavelon_pal)) then
          exit;
        // DIP
        marcade.dswa := 1;
        marcade.dswa_val2 := @cavelon_dip_a;
        marcade.dswb := 2;
        marcade.dswb_val2 := @cavelon_dip_b;
      end;
  end;
  // iniciar las estrellas de fondo
  case main_vars.machine_type of
    14:
      draw_stars := nil;
    47, 48, 49, 369, 370:
      begin
        draw_stars := stars_galaxian;
        galaxian_stars_0 := gal_stars.create(z80_0.numero_cpu, z80_0.clock, GALAXIANS);
        // y la de las estrellas de fondo
        galaxian_stars_0.create_pal(36);
      end;
    143, 144, 145, 363, 366, 382, 385:
      begin
        draw_stars := stars_scramble;
        galaxian_stars_0 := gal_stars.create(z80_0.numero_cpu, z80_0.clock, SCRAMBLE);
        // y la de las estrellas de fondo
        galaxian_stars_0.create_pal(36);
      end;
  end;
  // poner la paleta
  for f := 0 to 31 do
  begin
    ctemp1 := memory_temp[f];
    colores[f].r := $21 * (ctemp1 and 1) + $47 * ((ctemp1 shr 1) and 1) + $97 * ((ctemp1 shr 2) and 1);
    colores[f].g := $21 * ((ctemp1 shr 3) and 1) + $47 * ((ctemp1 shr 4) and 1) + $97 * ((ctemp1 shr 5) and 1);
    colores[f].b := 0 + $47 * ((ctemp1 shr 6) and 1) + $97 * ((ctemp1 shr 7) and 1);
  end;
  // y la paleta del disparo
  colores[32].r := $FF;
  colores[32].g := $FF;
  colores[32].b := 0;
  colores[33].r := $FF;
  colores[33].g := $FF;
  colores[33].b := $FF;
  colores[34].r := $FF;
  colores[34].g := 0;
  colores[34].b := $FF;
  // Color especial fondo de azul
  colores[BACK_COLOR].r := 0;
  colores[BACK_COLOR].g := 0;
  colores[BACK_COLOR].b := $56;
  // 32 paleta, 3 disparo
  set_pal(colores, 32 + 3 + 1);
  // final
  reset_galaxian;
  start_galaxian := true;
end;

end.
