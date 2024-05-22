unit system1_hw_misc;

interface

uses
  main_engine,
  gfx_engine,
  nz80,
  sn_76496,
  controls_engine,
  sega_decrypt,
  z80pio,
  ppi8255,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  sega_decrypt_2;

function start_system1: boolean;
procedure system1_loop;
procedure change_color_system1(valor: byte; pos: word);

implementation

uses
  system1_hw;

const
  // Pitfall 2
  pitfall2_rom: array [0 .. 2] of tipo_roms = ((n: 'epr6456a.116'; l: $4000; p: 0; crc: $BCC8406B),
    (n: 'epr6457a.109'; l: $4000; p: $4000; crc: $A016FD2A), (n: 'epr6458a.96'; l: $4000; p: $8000;
    crc: $5C30B3E8));
  pitfall2_char: array [0 .. 5] of tipo_roms = ((n: 'epr6474a.62'; l: $2000; p: 0; crc: $9F1711B9),
    (n: 'epr6473a.61'; l: $2000; p: $2000; crc: $8E53B8DD), (n: 'epr6472a.64'; l: $2000; p: $4000;
    crc: $E0F34A11), (n: 'epr6471a.63'; l: $2000; p: $6000; crc: $D5BC805C), (n: 'epr6470a.66'; l: $2000;
    p: $8000; crc: $1439729F), (n: 'epr6469a.65'; l: $2000; p: $A000; crc: $E4AC6921));
  pitfall2_sound: tipo_roms = (n: 'epr-6462.120'; l: $2000; p: 0; crc: $86BB9185);
  pitfall2_sprites: array [0 .. 1] of tipo_roms = ((n: 'epr6454a.117'; l: $4000; p: 0; crc: $A5D96780),
    (n: 'epr-6455.05'; l: $4000; p: $4000; crc: $32EE64A1));
  pitfall2_video_prom: tipo_roms = (n: 'pr-5317.76'; l: $100; p: 0; crc: $648350B8);
  pitfall2_dip_b: array [0 .. 6] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (dip_val: $0; dip_name: 'Infinite'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Bonus Life'; number: 2; dip: ((dip_val: $10; dip_name: '20K 50K'), (dip_val: $0;
    dip_name: '30K 70K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Allow Continue'; number: 2; dip: ((dip_val: $20; dip_name: 'No'), (dip_val: $0;
    dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Time';
    number: 2; dip: ((dip_val: $0; dip_name: '2 Minutes'), (dip_val: $40; dip_name: '3 Minutes'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), ());
  // Teddy Boy Blues
  teddy_rom: array [0 .. 2] of tipo_roms = ((n: 'epr-6768.116'; l: $4000; p: 0; crc: $5939817E),
    (n: 'epr-6769.109'; l: $4000; p: $4000; crc: $14A98DDD), (n: 'epr-6770.96'; l: $4000; p: $8000;
    crc: $67B0C7C2));
  teddy_char: array [0 .. 5] of tipo_roms = ((n: 'epr-6747.62'; l: $2000; p: 0; crc: $A0E5ACA7),
    (n: 'epr-6746.61'; l: $2000; p: $2000; crc: $CDB77E51), (n: 'epr-6745.64'; l: $2000; p: $4000;
    crc: $0CAB75C3), (n: 'epr-6744.63'; l: $2000; p: $6000; crc: $0EF8D2CD), (n: 'epr-6743.66'; l: $2000;
    p: $8000; crc: $C33062B5), (n: 'epr-6742.65'; l: $2000; p: $A000; crc: $C457E8C5));
  teddy_sound: tipo_roms = (n: 'epr6748x.120'; l: $2000; p: 0; crc: $C2A1B89D);
  teddy_sprites: array [0 .. 3] of tipo_roms = ((n: 'epr-6735.117'; l: $4000; p: 0; crc: $1BE35A97),
    (n: 'epr-6737.04'; l: $4000; p: $4000; crc: $6B53AA7A), (n: 'epr-6736.110'; l: $4000; p: $8000;
    crc: $565C25D0), (n: 'epr-6738.05'; l: $4000; p: $C000; crc: $E116285F));
  teddy_video_prom: tipo_roms = (n: 'pr-5317.76'; l: $100; p: 0; crc: $648350B8);
  teddy_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $2; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $8; dip_name: '2'), (dip_val: $C; dip_name: '3'), (dip_val: $4;
    dip_name: '4'), (dip_val: $0; dip_name: '252'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $30; name: 'Bonus Life'; number: 4; dip: ((dip_val: $30; dip_name: '100K 400K'), (dip_val: $20;
    dip_name: '200K 600K'), (dip_val: $10; dip_name: '400K 800K'), (dip_val: $0; dip_name: '600K'), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (),
    (), (), (), (), (), ())), ());
  // Wonder Boy
  wboy_rom: array [0 .. 2] of tipo_roms = ((n: 'epr-7489.116'; l: $4000; p: 0; crc: $130F4B70),
    (n: 'epr-7490.109'; l: $4000; p: $4000; crc: $9E656733), (n: 'epr-7491.96'; l: $4000; p: $8000;
    crc: $1F7D0EFE));
  wboy_char: array [0 .. 5] of tipo_roms = ((n: 'epr-7497.62'; l: $2000; p: 0; crc: $08D609CA),
    (n: 'epr-7496.61'; l: $2000; p: $2000; crc: $6F61FDF1), (n: 'epr-7495.64'; l: $2000; p: $4000;
    crc: $6A0D2C2D), (n: 'epr-7494.63'; l: $2000; p: $6000; crc: $A8E281C7), (n: 'epr-7493.66'; l: $2000;
    p: $8000; crc: $89305DF4), (n: 'epr-7492.65'; l: $2000; p: $A000; crc: $60F806B1));
  wboy_sound: tipo_roms = (n: 'epr-7498.120'; l: $2000; p: 0; crc: $78AE1E7B);
  wboy_sprites: array [0 .. 3] of tipo_roms = ((n: 'epr-7485.117'; l: $4000; p: 0; crc: $C2891722),
    (n: 'epr-7487.04'; l: $4000; p: $4000; crc: $2D3A421B), (n: 'epr-7486.110'; l: $4000; p: $8000;
    crc: $8D622C50), (n: 'epr-7488.05'; l: $4000; p: $C000; crc: $007C2F1B));
  wboy_video_prom: tipo_roms = (n: 'pr-5317.76'; l: $100; p: 0; crc: $648350B8);
  wboy_dip_b: array [0 .. 6] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Bonus Life'; number: 2; dip: ((dip_val: $10; dip_name: '30K 100K 170K 240K'),
    (dip_val: $0; dip_name: '30K 120K 210K 300K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20;
    dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Difficulty'; number: 2; dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $0;
    dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Mr Viking
  mrviking_rom: array [0 .. 5] of tipo_roms = ((n: 'epr-5873.129'; l: $2000; p: 0; crc: $14D21624),
    (n: 'epr-5874.130'; l: $2000; p: $2000; crc: $6DF7DE87), (n: 'epr-5875.131'; l: $2000; p: $4000;
    crc: $AC226100), (n: 'epr-5876.132'; l: $2000; p: $6000; crc: $E77DB1DC), (n: 'epr-5755.133'; l: $2000;
    p: $8000; crc: $EDD62AE1), (n: 'epr-5756.134'; l: $2000; p: $A000; crc: $11974040));
  mrviking_sprites: array [0 .. 1] of tipo_roms = ((n: 'epr-5749.86'; l: $4000; p: $0; crc: $E24682CD),
    (n: 'epr-5750.93'; l: $4000; p: $4000; crc: $6564D1AD));
  mrviking_sound: tipo_roms = (n: 'epr-5763.3'; l: $2000; p: 0; crc: $D712280D);
  mrviking_char: array [0 .. 5] of tipo_roms = ((n: 'epr-5762.82'; l: $2000; p: 0; crc: $4A91D08A),
    (n: 'epr-5761.65'; l: $2000; p: $2000; crc: $F7D61B65), (n: 'epr-5760.81'; l: $2000; p: $4000;
    crc: $95045820), (n: 'epr-5759.64'; l: $2000; p: $6000; crc: $5F9BAE4E), (n: 'epr-5758.80'; l: $2000;
    p: $8000; crc: $808EE706), (n: 'epr-5757.63'; l: $2000; p: $A000; crc: $480F7074));
  mrviking_video_prom: tipo_roms = (n: 'pr-5317.106'; l: $100; p: 0; crc: $648350B8);
  mrviking_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Maximum Credits'; number: 2;
    dip: ((dip_val: $2; dip_name: '9'), (dip_val: $0; dip_name: '99'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $C; dip_name: '3'), (dip_val: $8;
    dip_name: '4'), (dip_val: $4; dip_name: '5'), (dip_val: $0; dip_name: 'Infinite'), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $30; dip_name: '10K 30K 30K+'), (dip_val: $20; dip_name: '20K 40K 30K+'), (dip_val: $10;
    dip_name: '30K 30K+'), (dip_val: $0; dip_name: '40K 30K+'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $40; name: 'Difficulty'; number: 2; dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $0;
    dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Sega Ninja
  seganinj_rom: array [0 .. 2] of tipo_roms = ((n: 'epr-.116'; l: $4000; p: 0; crc: $A5D0C9D0),
    (n: 'epr-.109'; l: $4000; p: $4000; crc: $B9E6775C), (n: 'epr-6552.96'; l: $4000; p: $8000;
    crc: $F2EEB0D8));
  seganinj_sprites: array [0 .. 3] of tipo_roms = ((n: 'epr-6546.117'; l: $4000; p: $0; crc: $A4785692),
    (n: 'epr-6548.04'; l: $4000; p: $4000; crc: $BDF278C1), (n: 'epr-6547.110'; l: $4000; p: $8000;
    crc: $34451B08), (n: 'epr-6549.05'; l: $4000; p: $C000; crc: $D2057668));
  seganinj_sound: tipo_roms = (n: 'epr-6559.120'; l: $2000; p: 0; crc: $5A1570EE);
  seganinj_char: array [0 .. 5] of tipo_roms = ((n: 'epr-6558.62'; l: $2000; p: 0; crc: $2AF9EAEB),
    (n: 'epr-6592.61'; l: $2000; p: $2000; crc: $7804DB86), (n: 'epr-6556.64'; l: $2000; p: $4000;
    crc: $79FD26F7), (n: 'epr-6590.63'; l: $2000; p: $6000; crc: $BF858CAD), (n: 'epr-6554.66'; l: $2000;
    p: $8000; crc: $5AC9D205), (n: 'epr-6588.65'; l: $2000; p: $A000; crc: $DC931DBB));
  seganinj_video_prom: tipo_roms = (n: 'pr-5317.76'; l: $100; p: 0; crc: $648350B8);
  seganinj_dip_b: array [0 .. 6] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $8; dip_name: '2'), (dip_val: $C; dip_name: '3'), (dip_val: $4;
    dip_name: '4'), (dip_val: $0; dip_name: '240'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Bonus Life'; number: 2; dip: ((dip_val: $10; dip_name: '20K 70K 120K 170K'),
    (dip_val: $0; dip_name: '50K 100K 150K 200K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Allow Continue'; number: 2; dip: ((dip_val: $20; dip_name: 'No'), (dip_val: $0;
    dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Difficulty'; number: 2; dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $0;
    dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Up and Down
  upndown_rom: array [0 .. 5] of tipo_roms = ((n: 'epr5516a.129'; l: $2000; p: 0; crc: $038C82DA),
    (n: 'epr5517a.130'; l: $2000; p: $2000; crc: $6930E1DE), (n: 'epr-5518.131'; l: $2000; p: $4000;
    crc: $2A370C99), (n: 'epr-5519.132'; l: $2000; p: $6000; crc: $9D664A58), (n: 'epr-5520.133'; l: $2000;
    p: $8000; crc: $208DFBDF), (n: 'epr-5521.134'; l: $2000; p: $A000; crc: $E7B8D87A));
  upndown_sprites: array [0 .. 1] of tipo_roms = ((n: 'epr-5514.86'; l: $4000; p: $0; crc: $FCC0A88B),
    (n: 'epr-5515.93'; l: $4000; p: $4000; crc: $60908838));
  upndown_sound: tipo_roms = (n: 'epr-5535.3'; l: $2000; p: 0; crc: $CF4E4C45);
  upndown_char: array [0 .. 5] of tipo_roms = ((n: 'epr-5527.82'; l: $2000; p: 0; crc: $B2D616F1),
    (n: 'epr-5526.65'; l: $2000; p: $2000; crc: $8A8B33C2), (n: 'epr-5525.81'; l: $2000; p: $4000;
    crc: $E749C5EF), (n: 'epr-5524.64'; l: $2000; p: $6000; crc: $8B886952), (n: 'epr-5523.80'; l: $2000;
    p: $8000; crc: $DEDE35D9), (n: 'epr-5522.63'; l: $2000; p: $A000; crc: $5E6D9DFF));
  upndown_video_prom: tipo_roms = (n: 'pr-5317.106'; l: $100; p: 0; crc: $648350B8);
  upndown_dip_b: array [0 .. 4] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $6; name: 'Lives'; number: 4;
    dip: ((dip_val: $6; dip_name: '3'), (dip_val: $4; dip_name: '4'), (dip_val: $2;
    dip_name: '5'), (dip_val: $0; dip_name: 'Infinite'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $38; name: 'Bonus Life'; number: 8; dip: ((dip_val: $38; dip_name: '10K'), (dip_val: $30;
    dip_name: '20K'), (dip_val: $28; dip_name: '30K'), (dip_val: $20; dip_name: '40K'), (dip_val: $18;
    dip_name: '50K'), (dip_val: $10; dip_name: '60K'), (dip_val: $8; dip_name: '70K'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $C0; dip_name: 'Easy'), (dip_val: $80; dip_name: 'Medium'), (dip_val: $40;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  // Flicky
  flicky_rom: array [0 .. 1] of tipo_roms = ((n: 'epr5978a.116'; l: $4000; p: 0; crc: $296F1492),
    (n: 'epr5979a.109'; l: $4000; p: $4000; crc: $64B03EF9));
  flicky_sprites: array [0 .. 1] of tipo_roms = ((n: 'epr-5855.117'; l: $4000; p: $0; crc: $B5F894A1),
    (n: 'epr-5856.110'; l: $4000; p: $4000; crc: $266AF78F));
  flicky_sound: tipo_roms = (n: 'epr-5869.120'; l: $2000; p: 0; crc: $6D220D4E);
  flicky_char: array [0 .. 5] of tipo_roms = ((n: 'epr-5868.62'; l: $2000; p: 0; crc: $7402256B),
    (n: 'epr-5867.61'; l: $2000; p: $2000; crc: $2F5CE930), (n: 'epr-5866.64'; l: $2000; p: $4000;
    crc: $967F1D9A), (n: 'epr-5865.63'; l: $2000; p: $6000; crc: $03D9A34C), (n: 'epr-5864.66'; l: $2000;
    p: $8000; crc: $E659F358), (n: 'epr-5863.65'; l: $2000; p: $A000; crc: $A496CA15));
  flicky_video_prom: tipo_roms = (n: 'pr-5317.76'; l: $100; p: 0; crc: $648350B8);
  flicky_dip_b: array [0 .. 4] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (dip_val: $0; dip_name: 'Infinite'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $30; name: 'Bonus Life'; number: 4; dip: ((dip_val: $30; dip_name: '30K 80K 160K'), (dip_val: $20;
    dip_name: '30K 100K 200K'), (dip_val: $10; dip_name: '40K 120K 240K'), (dip_val: $0;
    dip_name: '40K 140K 280K'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Difficulty'; number: 2; dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $0;
    dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Gardia
  gardia_rom: array [0 .. 2] of tipo_roms = ((n: 'epr-10255.1'; l: $8000; p: 0; crc: $89282A6B),
    (n: 'epr-10254.2'; l: $8000; p: $8000; crc: $2826B6D8), (n: 'epr-10253.3'; l: $8000; p: $10000;
    crc: $7911260F));
  gardia_char: array [0 .. 2] of tipo_roms = ((n: 'epr-10249.61'; l: $4000; p: 0; crc: $4E0AD0F2),
    (n: 'epr-10248.64'; l: $4000; p: $4000; crc: $3515D124), (n: 'epr-10247.66'; l: $4000; p: $8000;
    crc: $541E1555));
  gardia_sound: tipo_roms = (n: 'epr-10243.120'; l: $4000; p: 0; crc: $87220660);
  gardia_sprites: array [0 .. 3] of tipo_roms = ((n: 'epr-10234.117'; l: $8000; p: 0; crc: $8A6AED33),
    (n: 'epr-10233.110'; l: $8000; p: $8000; crc: $C52784D3), (n: 'epr-10236.04'; l: $8000; p: $10000;
    crc: $B35AB227), (n: 'epr-10235.5'; l: $8000; p: $18000; crc: $006A3151));
  gardia_proms: array [0 .. 2] of tipo_roms = ((n: 'pr-7345.3'; l: $100; p: 0; crc: $8EEE0F72),
    (n: 'pr-7344.2'; l: $100; p: $100; crc: $3E7BABD7), (n: 'pr-7343.1'; l: $100; p: $200; crc: $371C44A6));
  gardia_video_prom: tipo_roms = (n: 'pr5317.4'; l: $100; p: 0; crc: $648350B8);
  gardia_dip_b: array [0 .. 6] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Bonus Life'; number: 2; dip: ((dip_val: $10; dip_name: '30K 100K 170K 240K'),
    (dip_val: $0; dip_name: '30K 120K 210K 300K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20;
    dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Difficulty'; number: 2; dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $0;
    dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  rweights, gweights, bweights: array [0 .. 2] of single;

procedure update_video;
var
  x_temp: word;
begin
  update_backgroud(0);
  update_backgroud(1);
  x_temp := (bg_ram[$FFC] + (bg_ram[$FFD] shl 8)) div 2 + 14;
  fillword(@xscroll[0], 32, x_temp);
  yscroll := bg_ram[$FBD];
  update_video_system1;
end;

procedure system1_loop;
var
  f: word;
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 259 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound CPU
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 223 then
        begin
          z80_0.change_irq(HOLD_LINE);
          update_video;
          events_system1;
        end;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function system1_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF:
      if z80_0.opcode then
        system1_getbyte := mem_dec[direccion]
      else
        system1_getbyte := memory[direccion];
    $8000 .. $D7FF, $DE00 .. $DFFF:
      system1_getbyte := memory[direccion];
    $D800 .. $DDFF:
      system1_getbyte := buffer_paleta[direccion and $7FF];
    $E000 .. $EFFF:
      system1_getbyte := bg_ram[direccion and $FFF];
    $F000 .. $F3FF:
      system1_getbyte := mix_collide[direccion and $3F] or $7E or (mix_collide_summary shl 7);
    $F800 .. $FBFF:
      system1_getbyte := sprite_collide[direccion and $3FF] or $7E or (sprite_collide_summary shl 7);
  end;
end;

procedure change_color_system1(valor: byte; pos: word);
var
  color: tcolor;
  bit0, bit1, bit2: byte;
begin
  bit0 := (valor shr 0) and 1;
  bit1 := (valor shr 1) and 1;
  bit2 := (valor shr 2) and 1;
  color.r := combine_3_weights(@rweights, bit0, bit1, bit2);
  bit0 := (valor shr 3) and 1;
  bit1 := (valor shr 4) and 1;
  bit2 := (valor shr 5) and 1;
  color.g := combine_3_weights(@gweights, bit0, bit1, bit2);
  bit0 := (valor shr 6) and 1;
  bit1 := (valor shr 7) and 1;
  color.b := combine_2_weights(@bweights, bit0, bit1);
  set_pal_color(color, pos);
end;

procedure system1_putbyte(direccion: word; valor: byte);
var
  pos_bg: word;
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $D7FF, $DE00 .. $DFFF:
      memory[direccion] := valor;
    $D800 .. $DDFF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        change_color_system1(valor, direccion and $7FF);
        buffer_paleta[direccion and $7FF] := valor;
      end;
    $E000 .. $EFFF:
      begin
        pos_bg := direccion and $FFF;
        if bg_ram[pos_bg] <> valor then
        begin
          bg_ram[pos_bg] := valor;
          bg_ram_w[pos_bg shr 1] := true;
        end;
      end;
    $F000 .. $F3FF:
      mix_collide[direccion and $3F] := 0;
    $F400 .. $F7FF:
      mix_collide_summary := 0;
    $F800 .. $FBFF:
      sprite_collide[direccion and $3FF] := 0;
    $FC00 .. $FFFF:
      sprite_collide_summary := 0;
  end;
end;

function system1_snd_getbyte_pio(direccion: word): byte;
begin
  case direccion of
    $0000 .. $7FFF:
      system1_snd_getbyte_pio := mem_snd[direccion];
    $8000 .. $9FFF:
      system1_snd_getbyte_pio := mem_snd[(direccion and $7FF) + $8000];
    $E000 .. $EFFF:
      begin
        system1_snd_getbyte_pio := z80pio_port_read(0, PORT_A);
        z80pio_astb_w(0, false);
        z80pio_astb_w(0, true);
      end;
  end;
end;

function system1_inbyte_pio(puerto: word): byte;
begin
  case (puerto and $1F) of
    $0 .. $3:
      system1_inbyte_pio := marcade.in1;
    $4 .. $7:
      system1_inbyte_pio := marcade.in2;
    $8 .. $B:
      system1_inbyte_pio := marcade.in0;
    $C, $E:
      system1_inbyte_pio := marcade.dswa;
    $D, $F, $10 .. $13:
      system1_inbyte_pio := marcade.dswb;
    $18 .. $1B:
      system1_inbyte_pio := z80pio_cd_ba_r(0, puerto and $1F);
  end;
end;

procedure system1_outbyte_pio(puerto: word; valor: byte);
begin
  case (puerto and $1F) of
    $18 .. $1B:
      z80pio_cd_ba_w(0, puerto and $1F, valor);
  end;
end;

function system1_getbyte_bank(direccion: word): byte;
begin
  case direccion of
    $8000 .. $BFFF:
      system1_getbyte_bank := roms[rom_bank, direccion and $3FFF];
  else
    system1_getbyte_bank := system1_getbyte(direccion);
  end;
end;

procedure system1_putbyte_bank(direccion: word; valor: byte);
var
  pos_bg: word;
begin
  case direccion of
    0 .. $BFFF:
      ;
    $D800 .. $DDFF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        change_color_system2(valor, direccion and $7FF);
        buffer_paleta[direccion and $7FF] := valor;
      end;
  else
    system1_putbyte(direccion, valor);
  end;
end;

// PIO
procedure system1_pio_porta_nmi(state: boolean);
begin
  z80_1.change_nmi(PULSE_LINE);
end;

// Main
function start_system1: boolean;
const
  resistances: array [0 .. 2] of integer = (995, 495, 250);
var
  memory_temp: array [0 .. $1FFFF] of byte;
  f: byte;
  procedure convert_gfx_system1;
  begin
    init_gfx(0, 8, 8, 2048);
    gfx_set_desc_data(3, 0, 8 * 8, 0, $4000 * 8, $8000 * 8);
    convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  end;

begin
  start_system1 := false;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256, false, true);
  case main_vars.machine_type of
    152, 154, 384:
      begin
        main_screen.rot270_screen := true;
        start_video(240, 224);
      end;
  else
    start_video(256, 224);
  end;
  // Main CPU
  // It should be 20Mhz, but if I put it, it fails Pitfall II
  z80_0 := cpu_z80.create(20000000 div 5, 260);
  z80_0.change_ram_calls(system1_getbyte, system1_putbyte);
  z80_0.change_misc_calls(nil, nil, system1_adjust_cycle);
  // Sound CPU
  z80_1 := cpu_z80.create(4000000, 260);
  z80_1.init_sound(system1_sound_update);
  timers.init(z80_1.numero_cpu, 4000000 / machine_calls.fps_max / (260 / 64), system1_sound_irq, nil, true);
  // Sound Chip
  sn_76496_0 := sn76496_chip.create(2000000, 0.5);
  sn_76496_1 := sn76496_chip.create(4000000);
  sprite_num_banks := 1;
  marcade.dswa := $FF;
  marcade.dswa_val := @system1_dip_credit;
  case main_vars.machine_type of
    27:
      begin // Pitfall II
        // cargar roms
        if not(roms_load(@memory, pitfall2_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 0); // Sega Decypt
        // cargar sonido
        if not(roms_load(@mem_snd, pitfall2_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, pitfall2_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, pitfall2_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, pitfall2_video_prom)) then
          exit;
        // dip
        marcade.dswb := $DC;
        marcade.dswb_val := @pitfall2_dip_b;
      end;
    35:
      begin // Teddy Boy Blues
        sprite_num_banks := 2;
        // cargar roms
        if not(roms_load(@memory, teddy_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 1); // Sega Decypt
        // cargar sonido
        if not(roms_load(@mem_snd, teddy_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, teddy_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, teddy_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, teddy_video_prom)) then
          exit;
        // dip
        marcade.dswa := $FF;
        marcade.dswa_val := @system1_dip_credit;
        marcade.dswb := $FE;
        marcade.dswb_val := @teddy_dip_b;
      end;
    36:
      begin // Wonder boy
        sprite_num_banks := 2;
        // cargar roms
        if not(roms_load(@memory, wboy_rom)) then
          exit;
        decode_sega_type2(@memory, @mem_dec, S315_5177);
        // cargar sonido
        if not(roms_load(@mem_snd, wboy_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, wboy_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, wboy_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, wboy_video_prom)) then
          exit;
        // dip
        marcade.dswb := $EC;
        marcade.dswb_val := @wboy_dip_b;
      end;
    152:
      begin // Mr Viking
        // cargar roms
        if not(roms_load(@memory, mrviking_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 3); // Sega Decypt
        // cargar sonido
        if not(roms_load(@mem_snd, mrviking_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, mrviking_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, mrviking_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, mrviking_video_prom)) then
          exit;
        // dip
        marcade.dswb := $FC;
        marcade.dswb_val := @mrviking_dip_b;
      end;
    153:
      begin // Sega Ninja
        sprite_num_banks := 2;
        // cargar roms
        if not(roms_load(@memory, seganinj_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 4); // Sega Decypt
        // cargar sonido
        if not(roms_load(@mem_snd, seganinj_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, seganinj_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, seganinj_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, seganinj_video_prom)) then
          exit;
        // dip
        marcade.dswb := $DC;
        marcade.dswb_val := @seganinj_dip_b;
      end;
    154:
      begin // Up and Down
        // cargar roms
        if not(roms_load(@memory, upndown_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 5); // Sega Decypt
        // cargar sonido
        if not(roms_load(@mem_snd, upndown_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, upndown_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, upndown_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, upndown_video_prom)) then
          exit;
        // dip
        marcade.dswa := $FF;
        marcade.dswb_val := @system1_dip_credit;
        marcade.dswb := $FE;
        marcade.dswb_val := @upndown_dip_b;
      end;
    155:
      begin // Flicky
        // cargar roms
        if not(roms_load(@memory, flicky_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 6); // Sega Decypt
        // cargar sonido
        if not(roms_load(@mem_snd, flicky_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, flicky_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memory
        if not(roms_load(@memory_sprites, flicky_sprites)) then
          exit;
        // Cargar PROM
        if not(roms_load(@lookup_memory, flicky_video_prom)) then
          exit;
        // dip
        marcade.dswb := $FE;
        marcade.dswb_val := @flicky_dip_b;
      end;
    384:
      begin // Gardia
        z80_0.change_ram_calls(system1_getbyte_bank, system1_putbyte_bank);
        sprite_num_banks := 4;
        // cargar roms
        if not(roms_load(@memory_temp, gardia_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to 3 do
          copymemory(@roms[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
        decode_sega_type2(@memory, @mem_dec, S317_000X, 2);
        // cargar sonido
        if not(roms_load(@mem_snd, gardia_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, gardia_char)) then
          exit;
        convert_gfx_system1;
        // Meter los sprites en memoria
        if not(roms_load(@memory_sprites, gardia_sprites)) then
          exit;
        // Cargar PROMs
        if not(roms_load(@memory_proms, gardia_proms)) then
          exit;
        if not(roms_load(@lookup_memory, gardia_video_prom)) then
          exit;
        // dip
        marcade.dswb := $7C;
        marcade.dswb_val := @wboy_dip_b;
      end;
  end;
  // Paleta
  compute_resistor_weights(0, 255, -1.0, 3, @resistances, @rweights, 0, 0, 3, @resistances, @gweights, 0, 0,
    2, @resistances[1], @bweights, 0, 0);
  case main_vars.machine_type of
    152, 154:
      begin
        // Main CPU
        z80_0.change_io_calls(system1_inbyte_ppi, system1_outbyte_ppi);
        // Sound CPU
        z80_1.change_ram_calls(system1_snd_getbyte_ppi, system1_snd_putbyte);
        // PPI 8255
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(nil, nil, nil, system1_port_a_write, system1_port_b_write,
          system1_port_c_write);
      end;
  else
    begin
      // Main CPU
      z80_0.change_io_calls(system1_inbyte_pio, system1_outbyte_pio);
      // Sound CPU
      z80_1.change_ram_calls(system1_snd_getbyte_pio, system1_snd_putbyte);
      // Z80 PIO
      z80pio_init(0, nil, nil, system1_port_a_write, system1_pio_porta_nmi, nil, system1_port_b_write, nil);
    end;
  end;
  char_screen := 1;
  sprite_offset := 0;
  mask_char := $7FF;
  machine_calls.reset;
  start_system1 := true;
end;

end.
