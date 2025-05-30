unit dec8_hw;

interface

uses
  WinApi.Windows,
  m6502,
  m6809,
  hd6309,
  main_engine,
  controls_engine,
  ym_2203,
  ym_3812,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  misc_functions,
  mcs51,
  timer_engine,
  deco_bac06,
  msm5205;

function start_dec8: boolean;

implementation

const
  // Super Real Darwin
  srd_rom: array [0 .. 1] of tipo_roms = ((n: 'dy01-e.b14'; l: $10000; p: 0; crc: $176E9299), (n: 'dy00.b16'; l: $10000; p: $10000; crc: $2BF6B461));
  srd_snd: tipo_roms = (n: 'dy04.d7'; l: $8000; p: $8000; crc: $2AE3591C);
  srd_mcu: tipo_roms = (n: 'id8751h.mcu'; l: $1000; p: 0; crc: $11CD6CA4);
  srd_char: tipo_roms = (n: 'dy05.b6'; l: $4000; p: 0; crc: $8780E8A3);
  srd_tiles: array [0 .. 1] of tipo_roms = ((n: 'dy03.b4'; l: $10000; p: 0; crc: $44F2A4F9), (n: 'dy02.b5'; l: $10000; p: $10000; crc: $522D9A9E));
  srd_sprites: array [0 .. 5] of tipo_roms = ((n: 'dy07.h16'; l: $8000; p: 0; crc: $97EABA60), (n: 'dy06.h14'; l: $8000; p: $8000; crc: $C279541B), (n: 'dy09.k13'; l: $8000; p: $10000; crc: $D30D1745), (n: 'dy08.k11'; l: $8000; p: $18000; crc: $71D645FD), (n: 'dy11.k16';
    l: $8000; p: $20000; crc: $FD9CCC5B), (n: 'dy10.k14'; l: $8000; p: $28000; crc: $88770AB8));
  srd_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (3, 2, 1, 0); name4: ('1C 2C', '1C 3C', '1C 4C', '1C 6C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, 4, 8, $C); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $20;
    name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: (0, $80); name2: ('Upright', 'Cocktail')), ());
  srd_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 3, 2, 0); name4: ('1', '3', '5', '28')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $10; name: 'Bonus Life';
    number: 2; val2: ($10, 0); name2: ('Every 50K', 'Every 100K')), (mask: $20; name: 'After Stage 10'; number: 2; val2: ($20, 0); name2: ('Back to Stager 1', 'Game Over')), (mask: $80; name: 'Allow Continue'; number: 2; val2: ($80, 0); name2: ('No', 'Yes')), ());
  // Last Mission
  lastmisn_rom: array [0 .. 1] of tipo_roms = ((n: 'last_mission_dl03-8.13h'; l: $8000; p: 0; crc: $A4F8D54B), (n: 'last_mission_dl04-5.7h'; l: $10000; p: $8000; crc: $7DEA1552));
  lastmisn_sub: tipo_roms = (n: 'last_mission_dl02-5.18h'; l: $10000; p: 0; crc: $EC9B5DAF);
  lastmisn_snd: tipo_roms = (n: 'last_mission_dl05-.5h'; l: $8000; p: $8000; crc: $1A5DF8C0);
  lastmisn_mcu: tipo_roms = (n: 'last_mission_dl00-e.18a'; l: $1000; p: 0; crc: $E97481C6);
  lastmisn_char: tipo_roms = (n: 'last_mission_dl01-.2a'; l: $8000; p: $8000; crc: $F3787A5D);
  lastmisn_sprites: array [0 .. 3] of tipo_roms = ((n: 'last_mission_dl11-.13f'; l: $8000; p: 0; crc: $36579D3B), (n: 'last_mission_dl12-.9f'; l: $8000; p: $20000; crc: $2BA6737E), (n: 'last_mission_dl13-.8f'; l: $8000; p: $40000; crc: $39A7DC93), (n: 'last_mission_dl10-.16f';
    l: $8000; p: $60000; crc: $FE275EA8));
  lastmisn_tiles: array [0 .. 3] of tipo_roms = ((n: 'last_mission_dl09-.12k'; l: $10000; p: 0; crc: $6A5A0C5D), (n: 'last_mission_dl08-.14k'; l: $10000; p: $20000; crc: $3B38CFCE), (n: 'last_mission_dl07-.15k'; l: $10000; p: $40000; crc: $1B60604D), (n: 'last_mission_dl06-.17k';
    l: $10000; p: $60000; crc: $C43C26A7));
  lastmisn_dip_a: array [0 .. 6] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (0, 3, 2, 1); name4: ('1C 5C', '1C 1C', '1C 2C', '1C 3C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, $C, 8, 4); name4: ('4C 1C', '1C 1C', '2C 1C', '3C 1C')), (mask: $10;
    name: 'Demo Sounds'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), (mask: $20; name: 'Cabinet'; number: 2; val2: (0, $20); name2: ('Upright', 'Cocktail')), (mask: $40; name: 'Invulnerability'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80;
    name: 'Infinite Lives'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  lastmisn_dip_b: array [0 .. 4] of def_dip2 = ((mask: $1; name: 'Lives'; number: 2; val2: (1, 0); name2: ('3', '5')), (mask: $6; name: 'Bonus Life'; number: 4; val4: (6, 4, 2, 0); name4: ('30K 70K 70K+', '40K 90K 90K+', '40K 80K', '50K')), (mask: $18; name: 'Difficulty';
    number: 4; val4: ($18, $10, 8, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $80; name: 'Allow Continue'; number: 2; val2: ($80, 0); name2: ('No', 'Yes')), ());
  // Shackled
  shackled_rom: array [0 .. 4] of tipo_roms = ((n: 'dk-02.13h'; l: $8000; p: 0; crc: $87F8FA85), (n: 'dk-06.7h'; l: $10000; p: $8000; crc: $69AD62D1), (n: 'dk-05.8h'; l: $10000; p: $18000; crc: $598DD128), (n: 'dk-04.10h'; l: $10000; p: $28000; crc: $36D305D4), (n: 'dk-03.11h';
    l: $8000; p: $38000; crc: $6FD90FD1));
  shackled_sub: tipo_roms = (n: 'dk-01.18h'; l: $10000; p: 0; crc: $71FE3BDA);
  shackled_snd: tipo_roms = (n: 'dk-07.5h'; l: $8000; p: $8000; crc: $887E4BCC);
  shackled_mcu: tipo_roms = (n: 'dk-e.18a'; l: $1000; p: 0; crc: $1AF06149);
  shackled_char: tipo_roms = (n: 'dk-00.2a'; l: $8000; p: 0; crc: $69B975AA);
  shackled_sprites: array [0 .. 7] of tipo_roms = ((n: 'dk-12.15k'; l: $10000; p: 0; crc: $615C2371), (n: 'dk-13.14k'; l: $10000; p: $10000; crc: $479AA503), (n: 'dk-14.13k'; l: $10000; p: $20000; crc: $CDC24246), (n: 'dk-15.11k'; l: $10000; p: $30000; crc: $88DB811B),
    (n: 'dk-16.10k'; l: $10000; p: $40000; crc: $061A76BD), (n: 'dk-17.9k'; l: $10000; p: $50000; crc: $A6C5D8AF), (n: 'dk-18.8k'; l: $10000; p: $60000; crc: $4D466757), (n: 'dk-19.6k'; l: $10000; p: $70000; crc: $1911E83E));
  shackled_tiles: array [0 .. 3] of tipo_roms = ((n: 'dk-11.12k'; l: $10000; p: 0; crc: $5CF5719F), (n: 'dk-10.14k'; l: $10000; p: $20000; crc: $408E6D08), (n: 'dk-09.15k'; l: $10000; p: $40000; crc: $C1557FAC), (n: 'dk-08.17k'; l: $10000; p: $60000; crc: $5E54E9F5));
  shackled_dip_a: array [0 .. 2] of def_dip2 = ((mask: $1; name: 'Flip Screen'; number: 2; val2: (1, 0); name2: ('Off', 'On')), (mask: $80; name: 'Freeze'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  shackled_dip_b: array [0 .. 3] of def_dip2 = ((mask: $6; name: 'Coin/Heart/Help/6-Help'; number: 8; val8: (0, 1, 2, 3, 7, 6, 5, 4); name8: ('2/100/50/200', '4/100/60/300', '6/200/70/300', '8/200/80/400', '10/200/100/500', '12/300/100/600', '18/400/200/700', '20/500/200/800')),
    (mask: $30; name: 'Difficulty'; number: 4; val4: ($30, $20, $10, 0); name4: ('Normal', 'Hard', 'very Hard', 'Hardest')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  // Gondomania
  gondo_rom: array [0 .. 3] of tipo_roms = ((n: 'dt00-e.f3'; l: $8000; p: 0; crc: $912A7EEE), (n: 'dt01.f5'; l: $10000; p: $8000; crc: $C39BB877), (n: 'dt02.f6'; l: $10000; p: $18000; crc: $925307A4), (n: 'dt03-e.f7'; l: $10000; p: $28000; crc: $EE7475EB));
  gondo_snd: tipo_roms = (n: 'dt05-e.h5'; l: $8000; p: $8000; crc: $EC08AA29);
  gondo_mcu: tipo_roms = (n: 'dt-e.b1'; l: $1000; p: 0; crc: $0D0532EC);
  gondo_char: tipo_roms = (n: 'dt14-e.b18'; l: $8000; p: 0; crc: $00CBE9C8);
  gondo_tiles: array [0 .. 7] of tipo_roms = ((n: 'dt08.h10'; l: $10000; p: 0; crc: $AEC483F5), (n: 'dt09.h12'; l: $8000; p: $10000; crc: $446F0CE0), (n: 'dt06.h7'; l: $10000; p: $18000; crc: $3FE1527F), (n: 'dt07.h9'; l: $8000; p: $28000; crc: $61F9BCE5), (n: 'dt12.h16';
    l: $10000; p: $30000; crc: $1A72CA8D), (n: 'dt13.h18'; l: $8000; p: $40000; crc: $CCB81AEC), (n: 'dt10.h13'; l: $10000; p: $48000; crc: $CFCFC9ED), (n: 'dt11.h15'; l: $8000; p: $58000; crc: $53E9CF17));
  gondo_sprites: array [0 .. 7] of tipo_roms = ((n: 'dt19.f13'; l: $10000; p: 0; crc: $DA2ABE4B), (n: 'dt20-e.f15'; l: $8000; p: $10000; crc: $0EEF7F56), (n: 'dt16.f9'; l: $10000; p: $20000; crc: $E9955D8F), (n: 'dt18-e.f12'; l: $8000; p: $30000; crc: $2B2D1468), (n: 'dt15.f8';
    l: $10000; p: $40000; crc: $A54B2EB6), (n: 'dt17-e.f11'; l: $8000; p: $50000; crc: $75AE349A), (n: 'dt21.f16'; l: $10000; p: $60000; crc: $1C5F682D), (n: 'dt22-e.f18'; l: $8000; p: $70000; crc: $C8FFB148));
  gondo_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (0, 2, 3, 1); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, $C, 8, 4); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $20;
    name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Swap buttons'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  gondo_dip_b: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 3, 2, 0); name4: ('1', '3', '5', '99')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $10; name: 'Allow Continue';
    number: 2; val2: ($10, 0); name2: ('No', 'Yes')), ());
  // Garyo Retsuden
  garyoret_rom: array [0 .. 4] of tipo_roms = ((n: 'dv00'; l: $8000; p: 0; crc: $CCEAAF05), (n: 'dv01'; l: $10000; p: $8000; crc: $C33FC18A), (n: 'dv02'; l: $10000; p: $18000; crc: $F9E26CE7), (n: 'dv03'; l: $10000; p: $28000; crc: $55D8D699), (n: 'dv04'; l: $10000; p: $38000;
    crc: $ED3D00EE));
  garyoret_snd: tipo_roms = (n: 'dv05'; l: $8000; p: $8000; crc: $C97C347F);
  garyoret_mcu: tipo_roms = (n: 'dv__.mcu'; l: $1000; p: 0; crc: $37CACEC6);
  garyoret_char: tipo_roms = (n: 'dv14'; l: $8000; p: 0; crc: $FB2BC581);
  garyoret_tiles: array [0 .. 7] of tipo_roms = ((n: 'dv08'; l: $10000; p: 0; crc: $89C13E15), (n: 'dv09'; l: $10000; p: $10000; crc: $6A345A23), (n: 'dv06'; l: $10000; p: $20000; crc: $1EB52A20), (n: 'dv07'; l: $10000; p: $30000; crc: $E7346EF8), (n: 'dv12'; l: $10000;
    p: $40000; crc: $46BA5AF4), (n: 'dv13'; l: $10000; p: $50000; crc: $A7AF6DFD), (n: 'dv10'; l: $10000; p: $60000; crc: $68B6D75C), (n: 'dv11'; l: $10000; p: $70000; crc: $B5948AEE));
  garyoret_sprites: array [0 .. 7] of tipo_roms = ((n: 'dv22'; l: $10000; p: 0; crc: $CEF0367E), (n: 'dv21'; l: $8000; p: $10000; crc: $90042FB7), (n: 'dv20'; l: $10000; p: $20000; crc: $451A2D8C), (n: 'dv19'; l: $8000; p: $30000; crc: $14E1475B), (n: 'dv18'; l: $10000;
    p: $40000; crc: $7043BEAD), (n: 'dv17'; l: $8000; p: $50000; crc: $28F449D7), (n: 'dv16'; l: $10000; p: $60000; crc: $37E4971E), (n: 'dv15'; l: $8000; p: $70000; crc: $CA41B6AC));
  garyoret_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (0, 2, 3, 1); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, $C, 8, 4); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $20;
    name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  garyoret_dip_b: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Lives'; number: 2; val2: (1, 0); name2: ('3', '5')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), ());
  // Captain Silver
  csilver_rom: array [0 .. 2] of tipo_roms = ((n: 'dx03-12.18d'; l: $8000; p: 0; crc: $2D926E7C), (n: 'dx01.12d'; l: $10000; p: $8000; crc: $570FB50C), (n: 'dx02.13d'; l: $10000; p: $18000; crc: $58625890));
  csilver_sub: tipo_roms = (n: 'dx04-1.19d'; l: $10000; p: 0; crc: $29432691);
  csilver_snd: tipo_roms = (n: 'dx05.3f'; l: $10000; p: 0; crc: $EB32CF25);
  csilver_mcu: tipo_roms = (n: 'dx-8.19a'; l: $1000; p: 0; crc: $C0266263);
  csilver_char: tipo_roms = (n: 'dx00.3d'; l: $8000; p: 0; crc: $F01EF985);
  csilver_sprites: array [0 .. 2] of tipo_roms = ((n: 'dx14.15k'; l: $10000; p: 0; crc: $80F07915), (n: 'dx13.13k'; l: $10000; p: $20000; crc: $D32C02E7), (n: 'dx12.10k'; l: $10000; p: $40000; crc: $AC78B76B));
  csilver_tiles: array [0 .. 5] of tipo_roms = ((n: 'dx06.5f'; l: $10000; p: 0; crc: $B6FB208C), (n: 'dx07.7f'; l: $10000; p: $10000; crc: $EE3E1817), (n: 'dx08.8f'; l: $10000; p: $20000; crc: $705900FE), (n: 'dx09.10f'; l: $10000; p: $30000; crc: $3192571D), (n: 'dx10.12f';
    l: $10000; p: $40000; crc: $3EF77A32), (n: 'dx11.13f'; l: $10000; p: $50000; crc: $9CF3D5B8));
  csilver_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (3, 2, 1, 0); name4: ('1C 2C', '1C 3C', '1C 4C', '1C 6C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, 4, 8, $C); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $20;
    name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: (0, $80); name2: ('Upright', 'Cocktail')), ());
  csilver_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 3, 2, 0); name4: ('1', '3', '5', '255')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $10; name: 'Allow Continue';
    number: 2; val2: (0, $10); name2: ('No', 'Yes')), (mask: $40; name: 'No key for door'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  // Cobra Command
  cobracom_rom: array [0 .. 2] of tipo_roms = ((n: 'el11-5.5j'; l: $8000; p: 0; crc: $AF0A8B05), (n: 'el12-4.7j'; l: $10000; p: $8000; crc: $7A44EF38), (n: 'el13.9j'; l: $10000; p: $18000; crc: $04505ACB));
  cobracom_snd: tipo_roms = (n: 'el10-4.1f'; l: $8000; p: $8000; crc: $EDFAD118);
  cobracom_char: tipo_roms = (n: 'el14.14j'; l: $8000; p: 0; crc: $47246177);
  cobracom_sprites: array [0 .. 3] of tipo_roms = ((n: 'el00-4.2a'; l: $10000; p: 0; crc: $122DA2A8), (n: 'el01-4.3a'; l: $10000; p: $20000; crc: $27BF705B), (n: 'el02-4.5a'; l: $10000; p: $40000; crc: $C86FEDE6), (n: 'el03-4.6a'; l: $10000; p: $60000; crc: $1D8A855B));
  cobracom_tiles1: array [0 .. 3] of tipo_roms = ((n: 'el05.15a'; l: $10000; p: 0; crc: $1C4F6033), (n: 'el06.16a'; l: $10000; p: $20000; crc: $D24BA794), (n: 'el04.13a'; l: $10000; p: $40000; crc: $D80A49CE), (n: 'el07.18a'; l: $10000; p: $60000; crc: $6D771FC3));
  cobracom_tiles2: array [0 .. 1] of tipo_roms = ((n: 'el08.7d'; l: $10000; p: 0; crc: $CB0DCF4C), (n: 'el09.9d'; l: $10000; p: $10000; crc: $1FAE5BE7));
  cobracom_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (0, 1, 3, 2); name4: ('3C 1C', '2C 1C', '1C 1C', '1C 2C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, 4, $C, 8); name4: ('3C 1C', '2C 1C', '1C 1C', '1C 2C')), (mask: $20;
    name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: (0, $80); name2: ('Upright', 'Cocktail')), ());
  cobracom_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '99')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $10; name: 'Allow Continue';
    number: 2; val2: (0, $10); name2: ('No', 'Yes')), (mask: $20; name: 'Bonus Life'; number: 2; val2: ($20, 0); name2: ('50K 150K', '100K 200K')), ());
  // The Real Ghostbusters
  ghostb_rom: array [0 .. 4] of tipo_roms = ((n: 'dz01-22.1d'; l: $8000; p: 0; crc: $FC65FDF2), (n: 'dz02.3d'; l: $10000; p: $8000; crc: $8E117541), (n: 'dz03.4d'; l: $10000; p: $18000; crc: $5606A8F4), (n: 'dz04-21.6d'; l: $10000; p: $28000; crc: $7D46582F), (n: 'dz05-21.7d';
    l: $10000; p: $38000; crc: $23E1C758));
  ghostb_snd: tipo_roms = (n: 'dz06.5f'; l: $8000; p: $8000; crc: $798F56DF);
  ghostb_mcu: tipo_roms = (n: 'dz-1.1b'; l: $1000; p: 0; crc: $9F5F3CB5);
  ghostb_char: tipo_roms = (n: 'dz00.16b'; l: $8000; p: 0; crc: $992B4F31);
  ghostb_sprites: array [0 .. 7] of tipo_roms = ((n: 'dz15.14f'; l: $10000; p: 0; crc: $A01A5FD9), (n: 'dz16.15f'; l: $10000; p: $10000; crc: $5A9A344A), (n: 'dz12.9f'; l: $10000; p: $20000; crc: $817FAE99), (n: 'dz14.12f'; l: $10000; p: $30000; crc: $0ABBF76D), (n: 'dz11.8f';
    l: $10000; p: $40000; crc: $A5E19C24), (n: 'dz13.1f'; l: $10000; p: $50000; crc: $3E7C0405), (n: 'dz17.17f'; l: $10000; p: $60000; crc: $40361B8B), (n: 'dz18.18f'; l: $10000; p: $70000; crc: $8D219489));
  ghostb_tiles: array [0 .. 3] of tipo_roms = ((n: 'dz07.12f'; l: $10000; p: 0; crc: $E7455167), (n: 'dz08.14f'; l: $10000; p: $10000; crc: $32F9DDFE), (n: 'dz09.15f'; l: $10000; p: $20000; crc: $BB6EFC02), (n: 'dz10.17f'; l: $10000; p: $30000; crc: $6EF9963B));
  ghostb_proms: array [0 .. 1] of tipo_roms = ((n: 'dz19a.10d'; l: $400; p: 0; crc: $47E1F83B), (n: 'dz20a.11d'; l: $400; p: $400; crc: $D8FE2D99));
  ghostb_dip_a: array [0 .. 2] of def_dip2 = ((mask: $20; name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  ghostb_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 3, 2, 0); name4: ('1', '3', '5', 'Invulnerability')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $30;
    name: 'Max Scene Time'; number: 4; val4: (0, $10, $30, $20); name4: ('4:00', '4:30', '5:00', '6:00')), (mask: $40; name: 'Allow Continue'; number: 2; val2: ($40, 0); name2: ('No', 'Yes')), (mask: $80; name: 'Energy Bonus'; number: 2; val2: ($80, 0);
    name2: ('None', '+25%')), ());
  // Psycho-Nicks Oscar
  oscar_rom: array [0 .. 1] of tipo_roms = ((n: 'du10'; l: $8000; p: 0; crc: $120040D8), (n: 'ed09'; l: $10000; p: $8000; crc: $E2D4BBA9));
  oscar_sub: tipo_roms = (n: 'du11'; l: $10000; p: 0; crc: $FF45C440);
  oscar_snd: tipo_roms = (n: 'ed12'; l: $8000; p: $8000; crc: $432031C5);
  oscar_char: tipo_roms = (n: 'ed08'; l: $4000; p: 0; crc: $308AC264);
  oscar_sprites: array [0 .. 3] of tipo_roms = ((n: 'ed04'; l: $10000; p: 0; crc: $416A791B), (n: 'ed05'; l: $10000; p: $20000; crc: $FCDBA431), (n: 'ed06'; l: $10000; p: $40000; crc: $7D50BEBC), (n: 'ed07'; l: $10000; p: $60000; crc: $8FDF0FA5));
  oscar_tiles: array [0 .. 3] of tipo_roms = ((n: 'ed01'; l: $10000; p: 0; crc: $D3A58E9E), (n: 'ed03'; l: $10000; p: $20000; crc: $4FC4FB0F), (n: 'ed00'; l: $10000; p: $40000; crc: $AC201F2D), (n: 'ed02'; l: $10000; p: $60000; crc: $7DDC5651));
  oscar_dip_a: array [0 .. 6] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (3, 2, 1, 0); name4: ('1C 2C', '1C 3C', '1C 4C', '1C 6C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, 4, 8, $C); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $10;
    name: 'Freeze Mode'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), (mask: $20; name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2;
    val2: (0, $80); name2: ('Upright', 'Cocktail')), ());
  oscar_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 3, 2, 0); name4: ('1', '3', '5', 'Infinite')), (mask: $C; name: 'Difficulty'; number: 4; val4: (8, $C, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: ($30, $20, $10, 0); name4: ('40K 100K 60K+', '60K 160K 100K+', '90K 240K 150K+', '50K')), (mask: $40; name: 'Invulnerability'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Allow Continue'; number: 2; val2: (0, $80);
    name2: ('No', 'Yes')), ());
  CPU_SYNC = 10;

var
  scroll_y, scroll_x, i8751_return, i8751_value: word;
  last_p2, i8751_port0, i8751_port1, rom_bank, sound_latch, vblank: byte;
  screen_prio, secclr, sub_nmi, main_nmi: boolean;
  rom: array [0 .. $F, 0 .. $3FFF] of byte;
  snd_dec: array [0 .. $7FFF] of byte;
  eventos_gondo_call: procedure;
  video_update_gondo: procedure;
  call_io_mcu_read: function(direccion: word): byte;
  call_io_mcu_write: procedure(direccion: word; valor: byte);
  // Captain Silver
  msm5205_next, sound_rom_bank: byte;
  msm5205_toggle: boolean;
  sound_rom: array [0 .. 1, 0 .. $3FFF] of byte;

procedure update_video_srd;
var
  f, nchar, color, atrib, x, y: word;
  procedure draw_sprites_srd(pri: byte);
  var
    x, y, f, nchar: word;
    color, atrib: byte;
    flipx: boolean;
  begin
    for f := 0 to $7F do
    begin
      atrib := buffer_sprites[(f * 4) + 1];
      color := (atrib and 3) + ((atrib and 8) shr 1);
      if ((pri = 0) and (color <> 0)) then
        continue;
      if ((pri = 1) and (color = 0)) then
        continue;
      nchar := buffer_sprites[(f * 4) + 3] + ((atrib and $E0) shl 3);
      if (nchar = 0) then
        continue;
      y := buffer_sprites[f * 4];
      if y = 248 then
        continue;
      x := 241 - buffer_sprites[(f * 4) + 2];
      flipx := (atrib and 4) <> 0;
      if (atrib and $10) <> 0 then
      begin
        put_gfx_sprite_diff(nchar, $40 + (color shl 3), flipx, false, 2, 0, 0);
        put_gfx_sprite_diff(nchar + 1, $40 + (color shl 3), flipx, false, 2, 0, 16);
        actualiza_gfx_sprite_size(x, y, 4, 16, 32);
      end
      else
      begin
        put_gfx_sprite(nchar, $40 + (color shl 3), flipx, false, 2);
        update_gfx_sprite(x, y, 4, 2);
      end;
    end;
  end;

begin
  for f := 0 to $1FF do
  begin
    atrib := memory[$1400 + (f * 2)];
    color := (atrib and $F0) shr 4;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$1401 + (f * 2)] + ((atrib and 3) shl 8);
      put_gfx(x * 16, y * 16, nchar, color shl 4, 2, 1);
      if color = 0 then
        put_gfx_block_trans(x * 16, y * 16, 3, 16, 16)
      else
        put_gfx_trans(x * 16, y * 16, nchar, color shl 4, 3, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  // Foreground
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$800 + f];
      put_gfx_trans(x * 8, y * 8, nchar, $80, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x(2, 4, scroll_x);
  draw_sprites_srd(0);
  scroll__x(3, 4, scroll_x);
  draw_sprites_srd(1);
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(0, 8, 256, 240, 4);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure draw_sprites(npant: byte);
var
  extra, fx, fy: boolean;
  atrib, x, y, f, nchar, nchar2: word;
  color: byte;
begin
  for f := 0 to $1FF do
  begin
    y := buffer_sprites_w[f * 4];
    if ((y and $8000) = 0) then
      continue;
    atrib := buffer_sprites_w[(f * 4) + 1];
    if ((atrib and 1) = 0) then
      continue;
    y := y and $1FF;
    nchar := buffer_sprites_w[(f * 4) + 3];
    color := nchar shr 12;
    nchar := nchar and $FFF;
    x := buffer_sprites_w[(f * 4) + 2] and $1FF;
    extra := (atrib and $10) <> 0;
    fy := (atrib and 2) <> 0;
    fx := (atrib and 4) <> 0;
    if extra then
    begin
      y := y + 16;
      nchar := nchar and $FFE;
    end;
    x := (x + 16) and $1FF;
    y := (y + 16) and $1FF;
    x := (256 - x) and $1FF;
    y := (256 - y) and $1FF;
    if (extra and fy) then
    begin
      nchar2 := nchar;
      nchar := nchar + 1;
    end
    else
      nchar2 := nchar + 1;
    put_gfx_sprite(nchar, (color shl 4) + 256, fx, fy, 2);
    update_gfx_sprite(x, y, npant, 2);
    if extra then
    begin
      put_gfx_sprite(nchar2, (color shl 4) + 256, fx, fy, 2);
      update_gfx_sprite(x, y + 16, npant, 2);
    end;
  end;
end;

procedure update_video_lastmissn;
var
  pos, f, nchar, x, y: word;
  color, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    x := f mod 32;
    y := f div 32;
    // Foreground
    atrib := memory[$2000 + (f * 2)];
    color := (atrib and $C0) shr 6;
    if ((gfx[0].buffer[f]) or buffer_color[color + $10]) then
    begin
      nchar := memory[$2001 + (f * 2)] + ((atrib and 3) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 1, 0);
      gfx[0].buffer[f] := false;
    end;
    // Background
    pos := ((x and $F) + ((y and $F) shl 4)) + ((x and $10) shl 4) + ((y and $10) shl 5);
    atrib := memory[$3800 + (pos * 2)];
    color := (atrib and $F0) shr 4;
    if (gfx[1].buffer[pos] or buffer_color[color]) then
    begin
      nchar := memory[$3801 + (pos * 2)] + ((atrib and $F) shl 8);
      put_gfx(x * 16, y * 16, nchar, (color shl 4) + $300, 2, 1);
      if screen_prio then
      begin
        if (color and 8) = 0 then
          put_gfx_block_trans(x * 16, y * 16, 3, 16, 16)
        else
          put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $300, 3, 1);
      end;
      gfx[1].buffer[pos] := false;
    end;
  end;
  scroll_x_y(2, 4, scroll_x, scroll_y);
  draw_sprites(4);
  if screen_prio then
    scroll_x_y(3, 4, scroll_x, scroll_y);
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(0, 8, 256, 240, 4);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure update_video_gondo;
var
  f, nchar, x, y: word;
  color, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    x := f mod 32;
    y := f div 32;
    // Foreground
    atrib := memory[$1800 + (f * 2)];
    color := (atrib and $70) shr 4;
    if ((gfx[0].buffer[f]) or buffer_color[color + $10]) then
    begin
      nchar := memory[$1801 + (f * 2)] + ((atrib and 3) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 1, 0);
      gfx[0].buffer[f] := false;
    end;
    // Background
    atrib := memory[$2000 + (f * 2)];
    color := (atrib and $F0) shr 4;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      nchar := memory[$2001 + (f * 2)] + ((atrib and $F) shl 8);
      put_gfx(x * 16, y * 16, nchar, (color shl 4) + $300, 2, 1);
      if screen_prio then
      begin
        if (color and 8) = 0 then
          put_gfx_block_trans(x * 16, y * 16, 3, 16, 16)
        else
          put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $300, 3, 1);
      end;
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 4, scroll_x, scroll_y);
  draw_sprites(4);
  if screen_prio then
    scroll_x_y(3, 4, scroll_x, scroll_y);
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(0, 8, 256, 240, 4);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure update_video_cobracom;
var
  f, nchar, x, y: word;
  color, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    // Foreground
    atrib := memory[$2000 + (f * 2)];
    color := (atrib and $E0) shr 5;
    if ((gfx[0].buffer[f]) or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$2001 + (f * 2)] + ((atrib and 3) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color shl 2, 8, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  bac06_0.tile_1.update_pf(1, false, false);
  bac06_0.tile_2.update_pf(2, true, false);
  bac06_0.tile_1.show_pf;
  bac06_0.draw_sprites(4, 0, 3);
  bac06_0.tile_2.show_pf;
  bac06_0.draw_sprites(4, 4, 3);
  update_region(0, 0, 256, 256, 8, 0, 0, 256, 256, 7);
  update_final_piece(0, 8, 256, 240, 7);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure update_video_ghostb;
var
  f, nchar, x, y: word;
  color, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      atrib := memory[$1800 + (f * 2)];
      color := (atrib and $C) shr 2;
      nchar := memory[$1801 + (f * 2)] + ((atrib and 3) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 8, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  bac06_0.tile_1.update_pf(1, false, false);
  bac06_0.tile_1.show_pf;
  draw_sprites(7);
  update_region(0, 0, 256, 256, 8, 0, 0, 256, 256, 7);
  update_final_piece(0, 8, 256, 240, 7);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure update_video_oscar;
var
  f, nchar, x, y: word;
  color, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    // Foreground
    atrib := memory[$2000 + (f * 2)];
    color := (atrib and $F0) shr 6;
    if ((gfx[0].buffer[f]) or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$2001 + (f * 2)] + ((atrib and 3) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, (color shl 3) + $100, 8, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  bac06_0.tile_1.update_pf(1, false, true);
  bac06_0.tile_1.show_pf;
  bac06_0.draw_sprites(0, 0, 2);
  bac06_0.tile_1.show_pf_pri;
  update_region(0, 0, 256, 256, 8, 0, 0, 256, 256, 7);
  update_final_piece(0, 8, 256, 240, 7);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure eventos_srd;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    // i8751
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := marcade.in2 and $DF
    else
      marcade.in2 := marcade.in2 or $20;
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := marcade.in2 and $BF
    else
      marcade.in2 := marcade.in2 or $40;
  end;
end;

procedure eventos_lastmisn;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    // i8751
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in2 := marcade.in2 and $DF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $20;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    if p_contrls.map_arcade.coin[1] then
    begin
      marcade.in2 := marcade.in2 and $BF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $40;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in3 := marcade.in3 and $FB
    else
      marcade.in3 := marcade.in3 or 4;
    if p_contrls.map_arcade.start[1] then
      marcade.in3 := marcade.in3 and $F7
    else
      marcade.in3 := marcade.in3 or 8;
  end;
end;

procedure eventos_gondo;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    // i8751
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in2 := marcade.in2 and $DF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $20;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    if p_contrls.map_arcade.coin[1] then
    begin
      marcade.in2 := marcade.in2 and $BF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $40;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in3 := marcade.in3 and $FE
    else
      marcade.in3 := marcade.in3 or 1;
    if p_contrls.map_arcade.start[1] then
      marcade.in3 := marcade.in3 and $FD
    else
      marcade.in3 := marcade.in3 or 2;
    // But
    if p_contrls.map_arcade.but0[0] then
      marcade.in4 := marcade.in4 and $FE
    else
      marcade.in4 := marcade.in4 or 1;
    if p_contrls.map_arcade.but1[0] then
      marcade.in4 := marcade.in4 and $FD
    else
      marcade.in4 := marcade.in4 or 2;
    if p_contrls.map_arcade.but0[1] then
      marcade.in4 := marcade.in4 and $FB
    else
      marcade.in4 := marcade.in4 or 4;
    if p_contrls.map_arcade.but1[1] then
      marcade.in4 := marcade.in4 and $F7
    else
      marcade.in4 := marcade.in4 or 8;
  end;
end;

procedure eventos_garyoret;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    // i8751
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in2 := marcade.in2 and $DF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $20;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    if p_contrls.map_arcade.coin[1] then
    begin
      marcade.in2 := marcade.in2 and $BF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $40;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
  end;
end;

procedure eventos_cobracom;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in3 := marcade.in3 and $FE
    else
      marcade.in3 := marcade.in3 or 1;
    if p_contrls.map_arcade.coin[1] then
      marcade.in3 := marcade.in3 and $FD
    else
      marcade.in3 := marcade.in3 or 2;
  end;
end;

procedure eventos_ghostb;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    // i8751
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in2 := marcade.in2 and $DF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $20;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    if p_contrls.map_arcade.coin[1] then
    begin
      marcade.in2 := marcade.in2 and $BF;
    end
    else
    begin
      marcade.in2 := marcade.in2 or $40;
      mcs51_0.change_irq0(ASSERT_LINE);
    end;
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in3 := marcade.in3 and $FE
    else
      marcade.in3 := marcade.in3 or 1;
    if p_contrls.map_arcade.start[1] then
      marcade.in3 := marcade.in3 and $FD
    else
      marcade.in3 := marcade.in3 or 2;
  end;
end;

procedure eventos_oscar;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
    // System
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in3 := marcade.in3 and $FE
    end
    else
    begin
      marcade.in3 := marcade.in3 or 1;
      hd6309_0.change_nmi(ASSERT_LINE);
    end;
    if p_contrls.map_arcade.coin[1] then
    begin
      marcade.in3 := marcade.in3 and $FD;
    end
    else
    begin
      marcade.in3 := marcade.in3 or 2;
      hd6309_0.change_nmi(ASSERT_LINE);
    end;
  end;
end;

procedure srd_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 271 do
    begin
      case f of
        8:
          vblank := 0;
        248:
          begin
            m6809_0.change_nmi(PULSE_LINE);
            update_video_srd;
            vblank := $40;
          end;
      end;
      for h := 1 to CPU_SYNC do
      begin
        // Main
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Sound
        m6502_0.run(frame_s);
        frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        // MCU
        mcs51_0.run(frame_mcu);
        frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
      end;
    end;
    eventos_srd;
    video_sync;
  end;
end;

procedure lastmisn_loop;
var
  frame_m, frame_sub, frame_s, frame_mcu: single;
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_sub := m6809_1.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 271 do
      begin
        case f of
          8:
            vblank := 0;
          248:
            begin
              update_video_lastmissn;
              vblank := $80;
              if sub_nmi then
                m6809_1.change_nmi(PULSE_LINE);
            end;
        end;
        for h := 1 to CPU_SYNC do
        begin
          // Main
          m6809_0.run(frame_m);
          frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
          // SUB
          m6809_1.run(frame_sub);
          frame_sub := frame_sub + m6809_1.tframes - m6809_1.contador;
          // Sound
          m6502_0.run(frame_s);
          frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
          // MCU
          mcs51_0.run(frame_mcu);
          frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        end;
      end;
      eventos_lastmisn;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure gondo_loop;
var
  frame_m, frame_s, frame_mcu: single;
  s, f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := hd6309_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 271 do
      begin
        case f of
          8:
            vblank := 0;
          248:
            begin
              video_update_gondo;
              for s := 0 to $3FF do
                buffer_sprites_w[s] := memory[$3001 + (s * 2)] + (memory[$3000 + (s * 2)] shl 8);
              vblank := $80;
              if main_nmi then
                hd6309_0.change_nmi(PULSE_LINE);
            end;
        end;
        for h := 1 to CPU_SYNC do
        begin
          // Main
          hd6309_0.run(frame_m);
          frame_m := frame_m + hd6309_0.tframes - hd6309_0.contador;
          // Sound
          m6502_0.run(frame_s);
          frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
          // MCU
          mcs51_0.run(frame_mcu);
          frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        end;
      end;
      eventos_gondo_call;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure cobracom_loop;
var
  frame_m, frame_s: single;
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 271 do
      begin
        case f of
          8:
            vblank := $80;
          248:
            begin
              update_video_cobracom;
              vblank := 0;
              m6809_0.change_nmi(PULSE_LINE);
            end;
        end;
        for h := 1 to CPU_SYNC do
        begin
          // Main
          m6809_0.run(frame_m);
          frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
          // Sound
          m6502_0.run(frame_s);
          frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        end;
      end;
      eventos_cobracom;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure oscar_loop;
var
  frame_m, frame_sub, frame_s: single;
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := hd6309_0.tframes;
  frame_sub := hd6309_1.tframes;
  frame_s := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 271 do
      begin
        case f of
          8:
            vblank := 0;
          248:
            begin
              update_video_oscar;
              vblank := $80;
            end;
        end;
        for h := 1 to CPU_SYNC do
        begin
          // Main
          hd6309_0.run(frame_m);
          frame_m := frame_m + hd6309_0.tframes - hd6309_0.contador;
          // Sub
          hd6309_1.run(frame_sub);
          frame_sub := frame_sub + hd6309_1.tframes - hd6309_1.contador;
          // Sound
          m6502_0.run(frame_s);
          frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        end;
      end;
      eventos_oscar;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Super Real Darwin
function getbyte_srd(direccion: word): byte;
begin
  case direccion of
    0 .. $17FF, $8000 .. $FFFF:
      getbyte_srd := memory[direccion];
    $2000:
      getbyte_srd := i8751_return shr 8;
    $2001:
      getbyte_srd := i8751_return and $FF;
    $2800 .. $288F:
      getbyte_srd := buffer_paleta[direccion and $FF];
    $3000 .. $308F:
      getbyte_srd := buffer_paleta[(direccion and $FF) + $400];
    $3800:
      getbyte_srd := marcade.dswa;
    $3801:
      getbyte_srd := marcade.in0;
    $3802:
      getbyte_srd := marcade.in1 or vblank;
    $3803:
      getbyte_srd := marcade.dswb;
    $4000 .. $7FFF:
      getbyte_srd := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure cambiar_color(dir: word);
var
  tmp_color: byte;
  color: tcolor;
  bit0, bit1, bit2, bit3: byte;
begin
  tmp_color := buffer_paleta[dir];
  bit0 := (tmp_color and 1) shr 0;
  bit1 := (tmp_color and 2) shr 1;
  bit2 := (tmp_color and 4) shr 2;
  bit3 := (tmp_color and 8) shr 3;
  color.r := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  bit0 := (tmp_color and $10) shr 4;
  bit1 := (tmp_color and $20) shr 5;
  bit2 := (tmp_color and $40) shr 6;
  bit3 := (tmp_color and $80) shr 7;
  color.g := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  tmp_color := buffer_paleta[dir + $400];
  bit0 := tmp_color and 1;
  bit1 := (tmp_color and 2) shr 1;
  bit2 := (tmp_color and 4) shr 2;
  bit3 := (tmp_color and 8) shr 3;
  color.b := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  set_pal_color(color, dir);
end;

procedure i8751_irq;
begin
  mcs51_0.change_irq1(CLEAR_LINE);
end;

procedure snd_clear_nmi;
begin
  m6502_0.change_nmi(CLEAR_LINE);
end;

procedure putbyte_srd(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF, $1000 .. $13FF:
      memory[direccion] := valor;
    $800 .. $FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $1400 .. $17FF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $3FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $1800:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $1801:
      i8751_value := (i8751_value and $FF00) or valor;
    $1802:
      ; // i8751_return:=0;
    $1804:
      copymemory(@buffer_sprites, @memory[$600], $200);
    $1805:
      begin
        rom_bank := valor shr 5;
        scroll_x := (scroll_x and $FF) or ((valor and $F) shl 8);
      end;
    $1806:
      scroll_x := (scroll_x and $F00) or valor;
    $2000:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $2001:
      main_screen.flip_main_screen := valor <> 0;
    $2800 .. $288F:
      if buffer_paleta[direccion and $FF] <> valor then
      begin
        direccion := direccion and $FF;
        buffer_paleta[direccion] := valor;
        cambiar_color(direccion);
        case direccion of
          $80 .. $83:
            fillchar(gfx[0].buffer, $400, 1);
          $F0 .. $FF:
            buffer_color[(direccion shr 4) and $F] := true;
        end;
      end;
    $3000 .. $308F:
      if buffer_paleta[(direccion and $FF) + $400] <> valor then
      begin
        direccion := direccion and $FF;
        buffer_paleta[direccion + $400] := valor;
        cambiar_color(direccion);
        case direccion of
          $80 .. $83:
            fillchar(gfx[0].buffer, $400, 1);
          $F0 .. $FF:
            buffer_color[(direccion shr 4) and $F] := true;
        end;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

// MCU
function in_port0: byte;
begin
  in_port0 := i8751_port0;
end;

function in_port1: byte;
begin
  in_port1 := i8751_port1;
end;

function in_port3: byte;
begin
  in_port3 := marcade.in2;
end;

procedure out_port0(valor: byte);
begin
  i8751_port0 := valor;
end;

procedure out_port1(valor: byte);
begin
  i8751_port1 := valor;
end;

procedure out_port2(valor: byte);
begin
  if (valor and $10) = 0 then
    i8751_port0 := i8751_value shr 8;
  if (valor and $20) = 0 then
    i8751_port0 := i8751_value;
  if (valor and $40) = 0 then
    i8751_return := (i8751_return and $FF) or (i8751_port0 shl 8);
  if (valor and $80) = 0 then
    i8751_return := (i8751_return and $FF00) or i8751_port0;
  if (valor and 4) = 0 then
    m6809_0.change_irq(ASSERT_LINE);
  if (valor and 2) = 0 then
    mcs51_0.change_irq1(CLEAR_LINE);
end;

// Last Mission
function lastmisn_io_mcu_read(direccion: word): byte;
begin
  case direccion of
    $1800:
      lastmisn_io_mcu_read := marcade.in0;
    $1801:
      lastmisn_io_mcu_read := marcade.in1;
    $1802:
      lastmisn_io_mcu_read := marcade.in3 or vblank;
    $1803:
      lastmisn_io_mcu_read := marcade.dswa;
    $1804:
      lastmisn_io_mcu_read := marcade.dswb;
    $1806:
      lastmisn_io_mcu_read := i8751_return shr 8;
    $1807:
      lastmisn_io_mcu_read := i8751_return and $FF;
  end;
end;

function getbyte_lastmisn(direccion: word): byte;
begin
  case direccion of
    0 .. $FFF, $2000 .. $3FFF, $8000 .. $FFFF:
      getbyte_lastmisn := memory[direccion];
    $1000 .. $17FF:
      getbyte_lastmisn := buffer_paleta[direccion and $7FF];
    $1800 .. $1FFF:
      getbyte_lastmisn := call_io_mcu_read(direccion);
    $4000 .. $7FFF:
      getbyte_lastmisn := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure lastmisn_io_mcu_write(direccion: word; valor: byte);
var
  f: word;
begin
  case direccion of
    $1800:
      m6809_1.change_irq(CLEAR_LINE);
    $1801:
      m6809_0.change_irq(CLEAR_LINE);
    $1802:
      m6809_0.change_firq(CLEAR_LINE);
    $1803:
      m6809_0.change_irq(ASSERT_LINE);
    $1804:
      m6809_1.change_irq(ASSERT_LINE);
    $1805:
      for f := 0 to $3FF do
        buffer_sprites_w[f] := memory[$2801 + (f * 2)] + (memory[$2800 + (f * 2)] shl 8);
    $1807:
      main_screen.flip_main_screen := valor <> 0;
    $1809:
      scroll_x := (scroll_x and $100) or valor;
    $180B:
      scroll_y := (scroll_y and $100) or valor;
    $180C:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $180D:
      begin
        rom_bank := valor and $F;
        scroll_x := (scroll_x and $FF) or ((valor and $20) shl 3);
        scroll_y := (scroll_y and $FF) or ((valor and $40) shl 2);
        if (valor and $80) <> 0 then
          m6809_1.change_reset(CLEAR_LINE)
        else
          m6809_1.change_reset(ASSERT_LINE);
      end;
    $180E:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $180F:
      i8751_value := (i8751_value and $FF00) or valor;
  end;
end;

procedure putbyte_lastmisn(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FFF, $2800 .. $37FF:
      memory[direccion] := valor;
    $1000 .. $17FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        direccion := direccion and $3FF;
        cambiar_color(direccion);
        case direccion of
          0 .. $1F:
            buffer_color[(direccion shr 3) + $10] := true;
          $300 .. $3FF:
            buffer_color[(direccion shr 4) and $F] := true;
        end;
      end;
    $1800 .. $1FFF:
      call_io_mcu_write(direccion, valor);
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $3800 .. $3FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function getbyte_sublastmisn(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      getbyte_sublastmisn := getbyte_lastmisn(direccion);
    $4000 .. $FFFF:
      getbyte_sublastmisn := mem_misc[direccion];
  end;
end;

procedure lastmisn_out_port2(valor: byte);
begin
  if (valor and $10) = 0 then
  begin
    i8751_port0 := i8751_value shr 8;
    mcs51_0.set_port_forced_input(0, i8751_port0);
  end;
  if (valor and $20) = 0 then
    i8751_port1 := i8751_value;
  if (valor and $40) = 0 then
    i8751_return := (i8751_return and $FF) or (i8751_port0 shl 8);
  if (valor and $80) = 0 then
    i8751_return := (i8751_return and $FF00) or i8751_port1;
  if (((valor and 4) <> 0) and ((last_p2 and 4) = 0)) then
    m6809_1.change_firq(ASSERT_LINE);
  if (valor and 2) = 0 then
    mcs51_0.change_irq1(CLEAR_LINE);
  if (valor and 1) = 0 then
    mcs51_0.change_irq0(CLEAR_LINE);
  last_p2 := valor;
end;

// Shackled
procedure shackled_io_mcu_write(direccion: word; valor: byte);
var
  f: word;
begin
  case direccion of
    $1800:
      m6809_1.change_irq(CLEAR_LINE);
    $1801:
      m6809_0.change_irq(CLEAR_LINE);
    $1802:
      m6809_1.change_firq(CLEAR_LINE);
    $1803:
      m6809_0.change_irq(ASSERT_LINE);
    $1804:
      m6809_1.change_irq(ASSERT_LINE);
    $1805:
      for f := 0 to $3FF do
        buffer_sprites_w[f] := memory[$2801 + (f * 2)] + (memory[$2800 + (f * 2)] shl 8);
    $1807:
      main_screen.flip_main_screen := valor <> 0;
    $1809:
      scroll_x := (scroll_x and $100) or valor;
    $180B:
      scroll_y := (scroll_y and $100) or valor;
    $180C:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $180D:
      begin
        rom_bank := valor and $F;
        scroll_x := (scroll_x and $FF) or ((valor and $20) shl 3);
        scroll_y := (scroll_y and $FF) or ((valor and $40) shl 2);
      end;
    $180E:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $180F:
      i8751_value := (i8751_value and $FF00) or valor;
  end;
end;

// Gondomania
function gondo_io_mcu_read(direccion: word): byte;
begin
  case direccion of
    $3800:
      gondo_io_mcu_read := marcade.dswa;
    $3801:
      gondo_io_mcu_read := marcade.dswb;
    $380A:
      gondo_io_mcu_read := $FF;
    $380B:
      gondo_io_mcu_read := $70 or marcade.in0;
    $380C:
      gondo_io_mcu_read := $FF;
    $380D:
      gondo_io_mcu_read := $70 or marcade.in1;
    $380E:
      gondo_io_mcu_read := marcade.in3 or vblank;
    $380F:
      gondo_io_mcu_read := marcade.in4;
    $3838:
      gondo_io_mcu_read := i8751_return shr 8;
    $3839:
      gondo_io_mcu_read := i8751_return and $FF;
  end;
end;

function getbyte_gondo(direccion: word): byte;
begin
  case direccion of
    0 .. $27FF, $3000 .. $37FF, $8000 .. $FFFF:
      getbyte_gondo := memory[direccion];
    $2800 .. $2FFF:
      getbyte_gondo := buffer_paleta[direccion and $7FF];
    $3800 .. $38FF:
      getbyte_gondo := call_io_mcu_read(direccion);
    $4000 .. $7FFF:
      getbyte_gondo := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure gondo_io_mcu_write(direccion: word; valor: byte);
begin
  case direccion of
    $3810:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $3818:
      scroll_x := (scroll_x and $100) or valor;
    $3820:
      scroll_y := (scroll_y and $100) or valor;
    $3828:
      begin
        scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
        scroll_y := (scroll_y and $FF) or ((valor and 2) shl 7);
      end;
    $3830:
      begin
        rom_bank := valor shr 4;
        main_screen.flip_main_screen := (valor and 8) <> 0;
        secclr := (valor and 1) <> 0;
        if not(secclr) then
          hd6309_0.change_irq(CLEAR_LINE);
        main_nmi := (valor and 2) <> 0;
      end;
    $383A:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $383B:
      i8751_value := (i8751_value and $FF00) or valor;
  end;
end;

procedure putbyte_gondo(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $17FF, $3000 .. $37FF:
      memory[direccion] := valor;
    $1800 .. $1FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $2800 .. $2FFF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        direccion := direccion and $3FF;
        cambiar_color(direccion);
        case direccion of
          0 .. $3F:
            buffer_color[(direccion shr 3) + $10] := true;
          $300 .. $3FF:
            buffer_color[(direccion shr 4) and $F] := true;
        end;
      end;
    $3800 .. $38FF:
      call_io_mcu_write(direccion, valor);
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

procedure gondo_out_port2(valor: byte);
begin
  if (valor and $10) = 0 then
    i8751_port0 := i8751_value shr 8;
  if (valor and $20) = 0 then
    i8751_port1 := i8751_value;
  if (valor and $40) = 0 then
    i8751_return := (i8751_return and $FF) or (i8751_port0 shl 8);
  if (valor and $80) = 0 then
    i8751_return := (i8751_return and $FF00) or i8751_port1;
  if (((valor and 4) <> 0) and ((last_p2 and 4) = 0) and secclr) then
    hd6309_0.change_irq(ASSERT_LINE);
  last_p2 := valor;
end;

// Garyo Ret
function garyoret_io_mcu_read(direccion: word): byte;
begin
  case direccion of
    $3800:
      garyoret_io_mcu_read := marcade.dswa;
    $3801:
      garyoret_io_mcu_read := marcade.dswb;
    $380A:
      garyoret_io_mcu_read := marcade.in1 or vblank;
    $380B:
      garyoret_io_mcu_read := marcade.in0;
    $383A:
      garyoret_io_mcu_read := i8751_return shr 8;
    $383B:
      garyoret_io_mcu_read := i8751_return and $FF;
  end;
end;

procedure garyoret_io_mcu_write(direccion: word; valor: byte);
begin
  case direccion of
    $3810:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $3818:
      scroll_x := (scroll_x and $100) or valor;
    $3820:
      scroll_y := (scroll_y and $100) or valor;
    $3828:
      begin
        scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
        scroll_y := (scroll_y and $FF) or ((valor and 2) shl 7);
      end;
    $3830:
      begin
        rom_bank := valor shr 4;
        main_screen.flip_main_screen := (valor and 8) <> 0;
        secclr := (valor and 1) <> 0;
        if not(secclr) then
          hd6309_0.change_irq(CLEAR_LINE);
        main_nmi := (valor and 2) <> 0;
      end;
    $3838:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $3839:
      i8751_value := (i8751_value and $FF00) or valor;
  end;
end;

// Captain Silver
procedure csilver_out_port2(valor: byte);
begin
  if (valor and $10) = 0 then
    i8751_port0 := i8751_value shr 8;
  if (valor and $20) = 0 then
    i8751_port1 := i8751_value;
  if (valor and $40) = 0 then
  begin
    i8751_return := (i8751_return and $FF) or (i8751_port0 shl 8);
    m6809_0.change_firq(ASSERT_LINE);
  end;
  if (valor and $80) = 0 then
    i8751_return := (i8751_return and $FF00) or i8751_port1;
end;

function csilver_io_mcu_read(direccion: word): byte;
begin
  case direccion of
    $1800:
      csilver_io_mcu_read := marcade.in1;
    $1801:
      csilver_io_mcu_read := marcade.in0;
    $1803:
      csilver_io_mcu_read := marcade.in3 or vblank;
    $1804:
      csilver_io_mcu_read := marcade.dswb;
    $1805:
      csilver_io_mcu_read := marcade.dswa;
    $1C00:
      csilver_io_mcu_read := i8751_return shr 8;
    $1E00:
      csilver_io_mcu_read := i8751_return and $FF;
  end;
end;

procedure csilver_io_mcu_write(direccion: word; valor: byte);
var
  f: word;
begin
  case direccion of
    $1800:
      m6809_1.change_irq(CLEAR_LINE);
    $1801:
      m6809_0.change_irq(CLEAR_LINE);
    $1802:
      m6809_0.change_firq(CLEAR_LINE);
    $1803:
      m6809_0.change_irq(ASSERT_LINE);
    $1804:
      m6809_1.change_irq(ASSERT_LINE);
    $1805:
      for f := 0 to $3FF do
        buffer_sprites_w[f] := memory[$2801 + (f * 2)] + (memory[$2800 + (f * 2)] shl 8);
    $1807:
      main_screen.flip_main_screen := valor <> 0;
    $1808:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $1809:
      scroll_x := (scroll_x and $100) or valor;
    $180A:
      scroll_y := (scroll_y and $FF) or ((valor and 1) shl 8);
    $180B:
      scroll_y := (scroll_y and $100) or valor;
    $180C:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $180D:
      rom_bank := valor and $F;
    $180E:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $180F:
      i8751_value := (i8751_value and $FF00) or valor;
  end;
end;

// Cobra Command
function getbyte_cobracom(direccion: word): byte;
begin
  case direccion of
    0 .. $2FFF, $8000 .. $FFFF:
      getbyte_cobracom := memory[direccion];
    $3000 .. $31FF:
      getbyte_cobracom := buffer_paleta[direccion and $1FF];
    $3800:
      getbyte_cobracom := marcade.in0;
    $3801:
      getbyte_cobracom := marcade.in1;
    $3802:
      getbyte_cobracom := marcade.dswa;
    $3803:
      getbyte_cobracom := marcade.dswb;
    $3A00:
      getbyte_cobracom := marcade.in3 or vblank;
    $4000 .. $7FFF:
      getbyte_cobracom := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure cambiar_color_cobra(dir: word);
var
  tmp_color: byte;
  color: tcolor;
  bit0, bit1, bit2, bit3: byte;
begin
  tmp_color := buffer_paleta[dir + 1];
  bit0 := (tmp_color and 1) shr 0;
  bit1 := (tmp_color and 2) shr 1;
  bit2 := (tmp_color and 4) shr 2;
  bit3 := (tmp_color and 8) shr 3;
  color.r := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  bit0 := (tmp_color and $10) shr 4;
  bit1 := (tmp_color and $20) shr 5;
  bit2 := (tmp_color and $40) shr 6;
  bit3 := (tmp_color and $80) shr 7;
  color.g := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  tmp_color := buffer_paleta[dir];
  bit0 := tmp_color and 1;
  bit1 := (tmp_color and 2) shr 1;
  bit2 := (tmp_color and 4) shr 2;
  bit3 := (tmp_color and 8) shr 3;
  color.b := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  dir := dir shr 1;
  set_pal_color(color, dir);
end;

procedure putbyte_cobracom(direccion: word; valor: byte);
var
  f: word;
begin
  case direccion of
    0 .. $7FF, $1800 .. $1FFF, $2800 .. $2FFF:
      memory[direccion] := valor;
    $800 .. $FFF:
      begin
        bac06_0.tile_1.write_tile_data_8b(direccion, valor, $7FF);
        memory[direccion] := valor;
      end;
    $1000 .. $17FF:
      begin
        bac06_0.tile_2.write_tile_data_8b(direccion, valor, $7FF);
        memory[direccion] := valor;
      end;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $3000 .. $31FF:
      if buffer_paleta[direccion and $1FF] <> valor then
      begin
        buffer_paleta[direccion and $1FF] := valor;
        cambiar_color_cobra(direccion and $1FE);
        direccion := (direccion and $1FE) shr 1;
        case direccion of
          0 .. $1F:
            buffer_color[direccion shr 2] := true;
          $80 .. $BF:
            bac06_0.tile_1.buffer_color[(direccion shr 4) and 3] := true;
          $C0 .. $FF:
            begin
              bac06_0.tile_1.buffer_color[(direccion shr 4) and 3] := true;
              bac06_0.tile_2.buffer_color[(direccion shr 4) and 3] := true;
            end;
        end;
      end;
    $3800 .. $3807:
      bac06_0.tile_1.change_control0_8b(direccion, valor);
    $3810 .. $381F:
      bac06_0.tile_1.change_control1_8b(direccion, valor);
    $3A00 .. $3A07:
      bac06_0.tile_2.change_control0_8b(direccion, valor);
    $3A10 .. $3A1F:
      bac06_0.tile_2.change_control1_8b(direccion, valor);
    $3C00:
      rom_bank := valor and $F;
    $3C02:
      for f := 0 to $3FF do
        bac06_0.sprite_ram[f] := memory[$2801 + (f * 2)] + (memory[$2800 + (f * 2)] shl 8);
    $3E00:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

// The Real Ghostbusters
function getbyte_ghostb(direccion: word): byte;
begin
  case direccion of
    0 .. $37FF, $8000 .. $FFFF:
      getbyte_ghostb := memory[direccion];
    $3800:
      getbyte_ghostb := marcade.in0;
    $3801:
      getbyte_ghostb := marcade.in1;
    $3802:
      getbyte_ghostb := $FF;
    $3803:
      getbyte_ghostb := marcade.dswa or marcade.in3 or (vblank shr 4);
    $3820:
      getbyte_ghostb := marcade.dswb;
    $3840:
      getbyte_ghostb := i8751_return shr 8;
    $3860:
      getbyte_ghostb := i8751_return and $FF;
    $4000 .. $7FFF:
      getbyte_ghostb := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure putbyte_ghostb(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $17FF, $2800 .. $2BFF, $3000 .. $37FF:
      memory[direccion] := valor;
    $1800 .. $1FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $2000 .. $27FF:
      begin
        bac06_0.tile_1.write_tile_data_8b(direccion, valor, $7FF);
        memory[direccion] := valor;
      end;
    $2C00 .. $2FFF:
      begin
        bac06_0.tile_1.write_rowscroll_8b(direccion, valor);
        memory[direccion] := valor;
      end;
    $3800:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(ASSERT_LINE);
        one_shot_timer_1(m6502_0.numero_cpu, 3, snd_clear_nmi);
      end;
    $3820 .. $3827:
      bac06_0.tile_1.change_control0_8b(direccion, valor);
    $3830 .. $383F:
      bac06_0.tile_1.change_control1_8b(direccion, valor);
    $3840:
      begin
        rom_bank := valor shr 4;
        main_screen.flip_main_screen := (valor and 8) <> 0;
        secclr := (valor and 1) <> 0;
        if not(secclr) then
          hd6309_0.change_irq(CLEAR_LINE);
        main_nmi := (valor and 2) <> 0;
      end;
    $3860:
      begin
        i8751_value := (i8751_value and $FF) or (valor shl 8);
        mcs51_0.change_irq1(ASSERT_LINE);
        one_shot_timer_0(mcs51_0.numero_cpu, 64, i8751_irq);
      end;
    $3861:
      i8751_value := (i8751_value and $FF00) or valor;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

// Psycho-Nics Oscar
function getbyte_oscar(direccion: word): byte;
begin
  case direccion of
    0 .. $37FF, $8000 .. $FFFF:
      getbyte_oscar := memory[direccion];
    $3800 .. $3BFF:
      getbyte_oscar := buffer_paleta[direccion and $3FF];
    $3C00:
      getbyte_oscar := marcade.in0;
    $3C01:
      getbyte_oscar := marcade.in1;
    $3C02:
      getbyte_oscar := marcade.in3 or vblank;
    $3C03:
      getbyte_oscar := $7F;
    $3C04:
      getbyte_oscar := $FF;
    $4000 .. $7FFF:
      getbyte_oscar := rom[rom_bank, direccion and $3FFF];
  end;
end;

procedure putbyte_oscar(direccion: word; valor: byte);
var
  f: word;
begin
  case direccion of
    0 .. $1FFF, $3000 .. $37FF:
      memory[direccion] := valor;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $2800 .. $2FFF:
      begin
        bac06_0.tile_1.write_tile_data_8b(direccion, valor, $7FF);
        memory[direccion] := valor;
      end;
    $3800 .. $3BFF:
      begin
        buffer_paleta[direccion and $3FF] := valor;
        cambiar_color_cobra(direccion and $3FE);
        direccion := (direccion and $3FF) shr 1;
        case direccion of
          $100 .. $11F:
            buffer_color[(direccion shr 3) and 3] := true;
          $180 .. $1FF:
            bac06_0.tile_1.buffer_color[(direccion shr 4) and 3] := true;
        end;
      end;
    $3C00 .. $3C07:
      bac06_0.tile_1.change_control0_8b(direccion, valor);
    $3C10 .. $3C1F:
      bac06_0.tile_1.change_control1_8b(direccion, valor);
    $3C80:
      for f := 0 to $3FF do
        bac06_0.sprite_ram[f] := memory[$3001 + (f * 2)] + (memory[$3000 + (f * 2)] shl 8);
    $3D00:
      rom_bank := valor and $F;
    $3D80:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(PULSE_LINE);
      end;
    $3E00:
      hd6309_0.change_nmi(CLEAR_LINE);
    $3E80:
      hd6309_1.change_irq(ASSERT_LINE);
    $3E81:
      hd6309_0.change_irq(CLEAR_LINE);
    $3E82:
      hd6309_0.change_irq(ASSERT_LINE);
    $3E83:
      hd6309_1.change_irq(CLEAR_LINE);
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function getbyte_suboscar(direccion: word): byte;
begin
  case direccion of
    0 .. $EFF, $1000 .. $3FFF:
      getbyte_suboscar := getbyte_oscar(direccion);
    $F00 .. $FFF, $4000 .. $FFFF:
      getbyte_suboscar := mem_misc[direccion];
  end;
end;

procedure putbyte_suboscar(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $EFF, $1000 .. $3FFF:
      putbyte_oscar(direccion, valor);
    $F00 .. $FFF:
      mem_misc[direccion] := valor;
    $4000 .. $FFFF:
      ;
  end;
end;

// Sound
function getbyte_snd_deco222(direccion: word): byte;
begin
  case direccion of
    0 .. $5FF:
      getbyte_snd_deco222 := mem_snd[direccion];
    $2000:
      getbyte_snd_deco222 := ym2203_0.status;
    $2001:
      getbyte_snd_deco222 := ym2203_0.read;
    $4000:
      getbyte_snd_deco222 := ym3812_0.status;
    $6000:
      getbyte_snd_deco222 := sound_latch;
    $8000 .. $FFFF:
      if m6502_0.opcode then
        getbyte_snd_deco222 := snd_dec[direccion and $7FFF]
      else
        getbyte_snd_deco222 := mem_snd[direccion];
  end;
end;

procedure putbyte_snd_deco222(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FF:
      mem_snd[direccion] := valor;
    $2000:
      ym2203_0.control(valor);
    $2001:
      ym2203_0.write(valor);
    $4000:
      ym3812_0.control(valor);
    $4001:
      ym3812_0.write(valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

function getbyte_snd_lastmisn(direccion: word): byte;
begin
  case direccion of
    0 .. $5FF:
      getbyte_snd_lastmisn := mem_snd[direccion];
    $800:
      getbyte_snd_lastmisn := ym2203_0.status;
    $801:
      getbyte_snd_lastmisn := ym2203_0.read;
    $1000:
      getbyte_snd_lastmisn := ym3812_0.status;
    $3000:
      getbyte_snd_lastmisn := sound_latch;
    $8000 .. $FFFF:
      getbyte_snd_lastmisn := mem_snd[direccion];
  end;
end;

procedure putbyte_snd_lastmisn(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FF:
      mem_snd[direccion] := valor;
    $800:
      ym2203_0.control(valor);
    $801:
      ym2203_0.write(valor);
    $1000:
      ym3812_0.control(valor);
    $1001:
      ym3812_0.write(valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

function getbyte_snd_csilver(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF:
      getbyte_snd_csilver := mem_snd[direccion];
    $800:
      getbyte_snd_csilver := ym2203_0.status;
    $801:
      getbyte_snd_csilver := ym2203_0.read;
    $1000:
      getbyte_snd_csilver := ym3812_0.status;
    $3000:
      getbyte_snd_csilver := sound_latch;
    $3400:
      begin
        getbyte_snd_csilver := 0;
        msm5205_0.reset_w(false);
      end;
    $4000 .. $7FFF:
      getbyte_snd_csilver := sound_rom[sound_rom_bank, direccion and $3FFF];
    $8000 .. $FFFF:
      getbyte_snd_csilver := mem_snd[direccion];
  end;
end;

procedure putbyte_snd_csilver(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $800:
      ym2203_0.control(valor);
    $801:
      ym2203_0.write(valor);
    $1000:
      ym3812_0.control(valor);
    $1001:
      ym3812_0.write(valor);
    $1800:
      msm5205_next := valor;
    $2000:
      sound_rom_bank := (valor and 8) shr 3;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

procedure snd_adpcm;
begin
  msm5205_toggle := not(msm5205_toggle);
  if msm5205_toggle then
    m6502_0.change_irq(HOLD_LINE);
  msm5205_0.data_w(msm5205_next shr 4);
  msm5205_next := msm5205_next shl 4;
end;

procedure dec8_sound_update;
begin
  ym2203_0.update;
  ym3812_0.update;
end;

procedure csilver_sound_update;
begin
  ym2203_0.update;
  ym3812_0.update;
  msm5205_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  m6502_0.change_irq(irqstate);
end;

// Main
procedure reset_dec8;
begin
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  marcade.in3 := $7F;
  case main_vars.machine_type of
    91:
      begin
        m6809_0.reset;
        mcs51_0.reset;
        marcade.in1 := $BF;
      end;
    392, 393:
      begin
        m6809_0.reset;
        m6809_1.reset;
        mcs51_0.reset;
      end;
    394:
      begin
        hd6309_0.reset;
        mcs51_0.reset;
        marcade.in4 := $FF;
      end;
    395:
      begin
        hd6309_0.reset;
        mcs51_0.reset;
        marcade.in1 := $7F;
      end;
    396:
      begin
        m6809_0.reset;
        m6809_1.reset;
        mcs51_0.reset;
        msm5205_0.reset;
      end;
    397:
      begin
        m6809_0.reset;
        bac06_0.reset;
      end;
    398:
      begin
        hd6309_0.reset;
        mcs51_0.reset;
        bac06_0.reset;
        marcade.in3 := 3;
      end;
    399:
      begin
        hd6309_0.reset;
        hd6309_1.reset;
        bac06_0.reset;
        // No inicializa bien el chip de video!! Y confia que los valores esten asi de inicio
        bac06_0.tile_1.control_0[0] := 2;
        bac06_0.tile_1.change_control0(3, 1);
      end;
  end;
  m6502_0.reset;
  ym2203_0.reset;
  ym3812_0.reset;
  sound_latch := 0;
  rom_bank := 0;
  i8751_return := 0;
  i8751_value := 0;
  i8751_port0 := 0;
  i8751_port1 := 0;
  last_p2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  secclr := false;
  main_nmi := false;
  vblank := 0;
  sound_rom_bank := 0;
  msm5205_next := 0;
  msm5205_toggle := false;
end;

function start_dec8: boolean;
const
  pc_x: array [0 .. 7] of dword = ($2000 * 8 + 0, $2000 * 8 + 1, $2000 * 8 + 2, $2000 * 8 + 3, 0, 1, 2, 3);
  ps_x: array [0 .. 15] of dword = (16 * 8, 1 + (16 * 8), 2 + (16 * 8), 3 + (16 * 8), 4 + (16 * 8), 5 + (16 * 8), 6 + (16 * 8), 7 + (16 * 8), 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 1024 * 8 * 8 + 0, 1024 * 8 * 8 + 1, 1024 * 8 * 8 + 2, 1024 * 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 1024 * 8 * 8 + 0, 16 * 8 + 1024 * 8 * 8 + 1, 16 * 8 + 1024 * 8 * 8 + 2, 16 * 8 + 1024 * 8 * 8 + 3);
  gb_pt_x: array [0 .. 15] of dword = (7, 6, 5, 4, 3, 2, 1, 0, 7 + (16 * 8), 6 + (16 * 8), 5 + (16 * 8), 4 + (16 * 8), 3 + (16 * 8), 2 + (16 * 8), 1 + (16 * 8), 0 + (16 * 8));
var
  f: word;
  memoria_temp: array [0 .. $47FFF] of byte;
  ptemp, ptemp2: pbyte;
  colores: tpaleta;
  procedure lastmissn_convert_chars;
  begin
    init_gfx(0, 8, 8, $400);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(3, 0, 8 * 8, $6000 * 8, $4000 * 8, $2000 * 8);
    convert_gfx(0, 0, @memoria_temp, @ps_x[8], @ps_y, false, false);
  end;
  procedure lastmissn_convert_tiles_sprites(gfx_num: byte);
  begin
    init_gfx(gfx_num, 16, 16, $1000);
    gfx_set_desc_data(4, 0, 16 * 16, $60000 * 8, $40000 * 8, $20000 * 8, 0 * 8);
    convert_gfx(gfx_num, 0, ptemp, @ps_x, @ps_y, false, false);
  end;

begin
  start_dec8 := false;
  machine_calls.reset := reset_dec8;
  machine_calls.fps_max := 57.444853;
  start_audio(false);
  case main_vars.machine_type of
    91, 392, 393, 394, 395, 396:
      begin
        screen_init(1, 256, 256, true);
        screen_init(2, 512, 512);
        screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
        screen_init(3, 512, 512, true);
        screen_mod_scroll(3, 512, 256, 511, 512, 256, 511);
        if ((main_vars.machine_type <> 393) and (main_vars.machine_type <> 395) and (main_vars.machine_type <> 396)) then
          main_screen.rot270_screen := true;
        screen_init(4, 512, 512, false, true);
        start_video(256, 240);
      end;
    397:
      begin
        screen_init(8, 256, 256, true);
        bac06_0 := bac06_chip.create(false, false, false, $80, $C0, 0, 1, 1, 1, $40, 3);
      end;
    398:
      begin
        screen_init(8, 256, 256, true);
        bac06_0 := bac06_chip.create(false, false, false, $200, 0, 0, 1, 1, 1, 0);
      end;
    399:
      begin
        screen_init(8, 256, 256, true);
        bac06_0 := bac06_chip.create(true, false, false, $180, 0, 0, 1, 1, 1, 0, 7);
      end;
  end;
  // Main CPU
  case main_vars.machine_type of
    91, 392, 393, 397:
      m6809_0 := cpu_m6809.create(2000000, 272 * CPU_SYNC, TCPU_MC6809E);
    394, 395, 398:
      hd6309_0 := cpu_hd6309.create(3000000, 272 * CPU_SYNC, TCPU_HD6309E);
    396:
      m6809_0 := cpu_m6809.create(1500000, 272 * CPU_SYNC, TCPU_MC6809E);
    399:
      hd6309_0 := cpu_hd6309.create(6000000, 272 * CPU_SYNC, TCPU_HD6309E);
  end;
  // Sound CPU
  m6502_0 := cpu_m6502.create(1500000, 272 * CPU_SYNC, TCPU_M6502);
  if (main_vars.machine_type = 396) then
    m6502_0.init_sound(csilver_sound_update)
  else
    m6502_0.init_sound(dec8_sound_update);
  // Sound Chip
  ym2203_0 := ym2203_chip.create(1500000);
  case main_vars.machine_type of
    91, 397, 398:
      ym3812_0 := ym3812_chip.create(YM3812_FM, 3000000, 0.7);
    392, 393, 394, 395, 396, 399:
      ym3812_0 := ym3812_chip.create(YM3526_FM, 3000000, 0.7);
  end;
  ym3812_0.change_irq_calls(snd_irq);
  // MCU
  if ((main_vars.machine_type <> 397) and (main_vars.machine_type <> 399)) then
    mcs51_0 := cpu_mcs51.create(I8X51, 8000000, 272 * CPU_SYNC);
  case main_vars.machine_type of
    91:
      begin
        machine_calls.general_loop := srd_loop;
        // Main CPU
        m6809_0.change_ram_calls(getbyte_srd, putbyte_srd);
        if not(roms_load(@memoria_temp, srd_rom)) then
          exit;
        copymemory(@rom[4, 0], @memoria_temp[0], $4000);
        copymemory(@rom[5, 0], @memoria_temp[$4000], $4000);
        copymemory(@memory[$8000], @memoria_temp[$8000], $8000);
        // memoria[$96e4]:=$39; //Cheat!
        for f := 0 to 3 do
          copymemory(@rom[f, 0], @memoria_temp[$10000 + (f * $4000)], $4000);
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_deco222, putbyte_snd_deco222);
        if not(roms_load(@mem_snd, srd_snd)) then
          exit;
        for f := $8000 to $FFFF do
          snd_dec[f - $8000] := bitswap8(mem_snd[f], 7, 5, 6, 4, 3, 2, 1, 0);
        // MCU
        mcs51_0.change_io_calls(in_port0, nil, nil, in_port3, out_port0, nil, out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, srd_mcu)) then
          exit;
        // Cargar chars
        if not(roms_load(@memoria_temp, srd_char)) then
          exit;
        init_gfx(0, 8, 8, $400);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 8 * 8, 0, 4);
        convert_gfx(0, 0, @memoria_temp, @pc_x, @ps_y, false, false);
        // Cargar tiles y ponerlas en su sitio
        getmem(ptemp, $100000);
        if not(roms_load(ptemp, srd_tiles)) then
          exit;
        for f := 0 to 3 do
        begin
          copymemory(@memoria_temp[$10000 * f], @ptemp[$4000 * f], $4000);
          copymemory(@memoria_temp[$8000 + ($10000 * f)], @ptemp[$10000 + ($4000 * f)], $4000);
        end;
        init_gfx(1, 16, 16, $400);
        for f := 0 to 7 do
          gfx[1].trans[f] := true;
        gfx_set_desc_data(4, 4, 32 * 8, $8000 * 8, $8000 * 8 + 4, 0, 4);
        for f := 0 to 3 do
          convert_gfx(1, $100 * f * 16 * 16, @memoria_temp[$10000 * f], @pt_x, @ps_y, false, false);
        freemem(ptemp);
        // Cargar sprites
        if not(roms_load(@memoria_temp, srd_sprites)) then
          exit;
        init_gfx(2, 16, 16, $800);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(3, 0, 16 * 16, $10000 * 8, $20000 * 8, 0 * 8);
        convert_gfx(2, 0, @memoria_temp, @ps_x, @ps_y, false, false);
        // DIP
        marcade.dswa := $7F;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @srd_dip_a;
        marcade.dswb_val2 := @srd_dip_b;
      end;
    392:
      begin
        machine_calls.general_loop := lastmisn_loop;
        call_io_mcu_read := lastmisn_io_mcu_read;
        call_io_mcu_write := lastmisn_io_mcu_write;
        // Main CPU
        m6809_0.change_ram_calls(getbyte_lastmisn, putbyte_lastmisn);
        if not(roms_load(@memoria_temp, lastmisn_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        copymemory(@rom[0, 0], @memoria_temp[$8000], $4000);
        copymemory(@rom[1, 0], @memoria_temp[$C000], $4000);
        copymemory(@rom[2, 0], @memoria_temp[$10000], $4000);
        copymemory(@rom[3, 0], @memoria_temp[$14000], $4000);
        // Sub CPU
        m6809_1 := cpu_m6809.create(2000000, 272 * CPU_SYNC, TCPU_MC6809E);
        m6809_1.change_ram_calls(getbyte_sublastmisn, putbyte_lastmisn);
        if not(roms_load(@mem_misc, lastmisn_sub)) then
          exit;
        sub_nmi := false;
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_lastmisn, putbyte_snd_lastmisn);
        if not(roms_load(@mem_snd, lastmisn_snd)) then
          exit;
        // MCU
        mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, lastmisn_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, lastmisn_mcu)) then
          exit;
        screen_prio := false;
        // Cargar chars
        if not(roms_load(@memoria_temp, lastmisn_char)) then
          exit;
        copymemory(@memoria_temp[0], @memoria_temp[$8000], $2000);
        copymemory(@memoria_temp[$6000], @memoria_temp[$A000], $2000);
        copymemory(@memoria_temp[$4000], @memoria_temp[$C000], $2000);
        copymemory(@memoria_temp[$2000], @memoria_temp[$E000], $2000);
        lastmissn_convert_chars;
        // Cargar tiles
        getmem(ptemp, $100000);
        if not(roms_load(ptemp, lastmisn_tiles)) then
          exit;
        lastmissn_convert_tiles_sprites(1);
        // Cargar sprites
        if not(roms_load(ptemp, lastmisn_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        // DIP
        marcade.dswa := $CF;
        marcade.dswb := $7F;
        marcade.dswa_val2 := @lastmisn_dip_a;
        marcade.dswb_val2 := @lastmisn_dip_b;
      end;
    393:
      begin
        machine_calls.general_loop := lastmisn_loop;
        call_io_mcu_read := lastmisn_io_mcu_read;
        call_io_mcu_write := shackled_io_mcu_write;
        // Main CPU
        m6809_0.change_ram_calls(getbyte_lastmisn, putbyte_lastmisn);
        if not(roms_load(@memoria_temp, shackled_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to $D do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        copymemory(@rom[$E, 0], @memoria_temp[$38000], $4000);
        copymemory(@rom[$F, 0], @memoria_temp[$3C000], $4000);
        // Sub CPU
        m6809_1 := cpu_m6809.create(2000000, 272 * CPU_SYNC, TCPU_MC6809E);
        m6809_1.change_ram_calls(getbyte_sublastmisn, putbyte_lastmisn);
        if not(roms_load(@mem_misc, shackled_sub)) then
          exit;
        sub_nmi := false;
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_lastmisn, putbyte_snd_lastmisn);
        if not(roms_load(@mem_snd, shackled_snd)) then
          exit;
        // MCU
        mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, lastmisn_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, shackled_mcu)) then
          exit;
        screen_prio := true;
        // Cargar chars
        if not(roms_load(@memoria_temp, shackled_char)) then
          exit;
        lastmissn_convert_chars;
        // Cargar tiles
        getmem(ptemp, $100000);
        if not(roms_load(ptemp, shackled_tiles)) then
          exit;
        lastmissn_convert_tiles_sprites(1);
        for f := 0 to 3 do
          gfx[1].trans[f] := true;
        // Cargar sprites
        if not(roms_load(ptemp, shackled_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $7F;
        marcade.dswa_val2 := @shackled_dip_a;
        marcade.dswb_val2 := @shackled_dip_b;
      end;
    394:
      begin
        machine_calls.general_loop := gondo_loop;
        eventos_gondo_call := eventos_gondo;
        call_io_mcu_read := gondo_io_mcu_read;
        call_io_mcu_write := gondo_io_mcu_write;
        video_update_gondo := update_video_gondo;
        // Main CPU
        hd6309_0.change_ram_calls(getbyte_gondo, putbyte_gondo);
        if not(roms_load(@memoria_temp, gondo_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to $B do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_deco222, putbyte_snd_deco222);
        if not(roms_load(@mem_snd, gondo_snd)) then
          exit;
        copymemory(@snd_dec, @mem_snd[$8000], $8000);
        // MCU
        mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, gondo_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, gondo_mcu)) then
          exit;
        screen_prio := false;
        // Cargar chars
        if not(roms_load(@memoria_temp, gondo_char)) then
          exit;
        lastmissn_convert_chars;
        // Cargar tiles
        getmem(ptemp, $100000);
        getmem(ptemp2, $100000);
        if not(roms_load(ptemp2, gondo_tiles)) then
          exit;
        copymemory(@ptemp[0], @ptemp2[0], $8000);
        copymemory(@ptemp[$10000], @ptemp2[$8000], $8000);
        copymemory(@ptemp[$8000], @ptemp2[$10000], $8000);
        copymemory(@ptemp[$20000], @ptemp2[$18000], $8000);
        copymemory(@ptemp[$30000], @ptemp2[$20000], $8000);
        copymemory(@ptemp[$28000], @ptemp2[$28000], $8000);
        copymemory(@ptemp[$40000], @ptemp2[$30000], $8000);
        copymemory(@ptemp[$50000], @ptemp2[$38000], $8000);
        copymemory(@ptemp[$48000], @ptemp2[$40000], $8000);
        copymemory(@ptemp[$60000], @ptemp2[$48000], $8000);
        copymemory(@ptemp[$70000], @ptemp2[$50000], $8000);
        copymemory(@ptemp[$68000], @ptemp2[$58000], $8000);
        lastmissn_convert_tiles_sprites(1);
        freemem(ptemp2);
        // Cargar sprites
        if not(roms_load(ptemp, gondo_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $EF;
        marcade.dswa_val2 := @gondo_dip_a;
        marcade.dswb_val2 := @gondo_dip_b;
      end;
    395:
      begin
        machine_calls.general_loop := gondo_loop;
        eventos_gondo_call := eventos_garyoret;
        call_io_mcu_read := garyoret_io_mcu_read;
        call_io_mcu_write := garyoret_io_mcu_write;
        video_update_gondo := update_video_gondo;
        // Main CPU
        hd6309_0.change_ram_calls(getbyte_gondo, putbyte_gondo);
        if not(roms_load(@memoria_temp, garyoret_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to $F do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_deco222, putbyte_snd_deco222);
        if not(roms_load(@mem_snd, garyoret_snd)) then
          exit;
        copymemory(@snd_dec, @mem_snd[$8000], $8000);
        // MCU
        mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, gondo_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, garyoret_mcu)) then
          exit;
        screen_prio := true;
        // Cargar chars
        if not(roms_load(@memoria_temp, garyoret_char)) then
          exit;
        lastmissn_convert_chars;
        // Cargar tiles
        getmem(ptemp, $100000);
        getmem(ptemp2, $100000);
        if not(roms_load(ptemp2, garyoret_tiles)) then
          exit;
        copymemory(@ptemp[0], @ptemp2[0], $8000);
        copymemory(@ptemp[$10000], @ptemp2[$8000], $8000);
        copymemory(@ptemp[$8000], @ptemp2[$10000], $8000);
        copymemory(@ptemp[$18000], @ptemp2[$18000], $8000);
        copymemory(@ptemp[$20000], @ptemp2[$20000], $8000);
        copymemory(@ptemp[$30000], @ptemp2[$28000], $8000);
        copymemory(@ptemp[$28000], @ptemp2[$30000], $8000);
        copymemory(@ptemp[$38000], @ptemp2[$38000], $8000);
        copymemory(@ptemp[$40000], @ptemp2[$40000], $8000);
        copymemory(@ptemp[$50000], @ptemp2[$48000], $8000);
        copymemory(@ptemp[$48000], @ptemp2[$50000], $8000);
        copymemory(@ptemp[$58000], @ptemp2[$58000], $8000);
        copymemory(@ptemp[$60000], @ptemp2[$60000], $8000);
        copymemory(@ptemp[$70000], @ptemp2[$68000], $8000);
        copymemory(@ptemp[$68000], @ptemp2[$70000], $8000);
        copymemory(@ptemp[$78000], @ptemp2[$78000], $8000);
        lastmissn_convert_tiles_sprites(1);
        for f := 0 to 3 do
          gfx[1].trans[f] := true;
        freemem(ptemp2);
        // Cargar sprites
        if not(roms_load(ptemp, garyoret_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @garyoret_dip_a;
        marcade.dswb_val2 := @garyoret_dip_b;
      end;
    396:
      begin
        machine_calls.general_loop := lastmisn_loop;
        call_io_mcu_read := csilver_io_mcu_read;
        call_io_mcu_write := csilver_io_mcu_write;
        // Main CPU
        m6809_0.change_ram_calls(getbyte_lastmisn, putbyte_lastmisn);
        if not(roms_load(@memoria_temp, csilver_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to 7 do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        // Sub CPU
        m6809_1 := cpu_m6809.create(1500000, 272 * CPU_SYNC, TCPU_MC6809E);
        m6809_1.change_ram_calls(getbyte_sublastmisn, putbyte_lastmisn);
        if not(roms_load(@mem_misc, csilver_sub)) then
          exit;
        sub_nmi := true;
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_csilver, putbyte_snd_csilver);
        if not(roms_load(@memoria_temp, csilver_snd)) then
          exit;
        copymemory(@mem_snd[$8000], @memoria_temp[$8000], $8000);
        copymemory(@sound_rom[0, 0], @memoria_temp[0], $4000);
        copymemory(@sound_rom[1, 0], @memoria_temp[$4000], $4000);
        msm5205_0 := MSM5205_chip.create(384000, MSM5205_S48_4B, 1, 0);
        msm5205_0.change_advance(snd_adpcm);
        // MCU
        mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, csilver_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, csilver_mcu)) then
          exit;
        screen_prio := false;
        // Cargar chars
        if not(roms_load(@memoria_temp, csilver_char)) then
          exit;
        lastmissn_convert_chars;
        // Cargar tiles
        getmem(ptemp, $100000);
        if not(roms_load(ptemp, csilver_tiles)) then
          exit;
        lastmissn_convert_tiles_sprites(1);
        // Cargar sprites
        if not(roms_load(ptemp, csilver_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        // DIP
        marcade.dswa := $7F;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @csilver_dip_a;
        marcade.dswb_val2 := @csilver_dip_b;
      end;
    397:
      begin
        machine_calls.general_loop := cobracom_loop;
        // Main CPU
        m6809_0.change_ram_calls(getbyte_cobracom, putbyte_cobracom);
        if not(roms_load(@memoria_temp, cobracom_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to 7 do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_deco222, putbyte_snd_deco222);
        if not(roms_load(@mem_snd, cobracom_snd)) then
          exit;
        copymemory(@snd_dec, @mem_snd[$8000], $8000);
        // Cargar chars
        if not(roms_load(@memoria_temp, cobracom_char)) then
          exit;
        init_gfx(0, 8, 8, $400);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 8 * 8, $4000 * 8, 0 * 8);
        convert_gfx(0, 0, @memoria_temp, @ps_x[8], @ps_y, false, false);
        // Cargar tiles
        getmem(ptemp, $100000);
        getmem(ptemp2, $100000);
        if not(roms_load(ptemp2, cobracom_tiles2)) then
          exit;
        copymemory(@ptemp[0], @ptemp2[0], $8000);
        copymemory(@ptemp[$40000], @ptemp2[$8000], $8000);
        copymemory(@ptemp[$20000], @ptemp2[$10000], $8000);
        copymemory(@ptemp[$60000], @ptemp2[$18000], $8000);
        lastmissn_convert_tiles_sprites(1);
        gfx[1].trans[0] := true;
        if not(roms_load(ptemp, cobracom_tiles1)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        // Cargar sprites
        if not(roms_load(ptemp, cobracom_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(3);
        gfx[3].trans[0] := true;
        freemem(ptemp);
        freemem(ptemp2);
        // DIP
        marcade.dswa := $7F;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @cobracom_dip_a;
        marcade.dswb_val2 := @cobracom_dip_b;
      end;
    398:
      begin
        machine_calls.general_loop := gondo_loop;
        eventos_gondo_call := eventos_ghostb;
        video_update_gondo := update_video_ghostb;
        // Main CPU
        hd6309_0.change_ram_calls(getbyte_ghostb, putbyte_ghostb);
        if not(roms_load(@memoria_temp, ghostb_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to $F do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_deco222, putbyte_snd_deco222);
        if not(roms_load(@mem_snd, ghostb_snd)) then
          exit;
        for f := $8000 to $FFFF do
          snd_dec[f - $8000] := bitswap8(mem_snd[f], 7, 5, 6, 4, 3, 2, 1, 0);
        // MCU
        mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, gondo_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, ghostb_mcu)) then
          exit;
        screen_prio := false;
        // Cargar chars
        if not(roms_load(@memoria_temp, ghostb_char)) then
          exit;
        lastmissn_convert_chars;
        // Cargar tiles
        getmem(ptemp, $100000);
        if not(roms_load(ptemp, ghostb_tiles)) then
          exit;
        init_gfx(1, 16, 16, $1000);
        gfx_set_desc_data(4, 0, 16 * 16, $20000 * 8, 0 * 8, $30000 * 8, $10000 * 8);
        convert_gfx(1, 0, ptemp, @gb_pt_x, @ps_y, false, false);
        // Cargar sprites
        if not(roms_load(ptemp, ghostb_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        if not(roms_load(@memoria_temp, ghostb_proms)) then
          exit;
        for f := 0 to $3FF do
        begin
          colores[f].r := (memoria_temp[f] and 1) * $E + ((memoria_temp[f] and 2) shr 1) * $1F + ((memoria_temp[f] and 4) shr 2) * $43 + ((memoria_temp[f] and 8) shr 3) * $8F;
          colores[f].g := ((memoria_temp[f] and $10) shr 4) * $E + ((memoria_temp[f] and $20) shr 5) * $1F + ((memoria_temp[f] and $40) shr 6) * $43 + ((memoria_temp[f] and $80) shr 7) * $8F;
          colores[f].b := (memoria_temp[f + $400] and 1) * $E + ((memoria_temp[f + $400] and 2) shr 1) * $1F + ((memoria_temp[f + $400] and 4) shr 2) * $43 + ((memoria_temp[f + $400] and 8) shr 3) * $8F;
        end;
        set_pal(colores, $400);
        // DIP
        marcade.dswa := $F0;
        marcade.dswb := $BF;
        marcade.dswa_val2 := @ghostb_dip_a;
        marcade.dswb_val2 := @ghostb_dip_b;
      end;
    399:
      begin
        machine_calls.general_loop := oscar_loop;
        // Main CPU
        hd6309_0.change_ram_calls(getbyte_oscar, putbyte_oscar);
        if not(roms_load(@memoria_temp, oscar_rom)) then
          exit;
        copymemory(@memory[$8000], @memoria_temp[0], $8000);
        for f := 0 to 3 do
          copymemory(@rom[f, 0], @memoria_temp[$8000 + (f * $4000)], $4000);
        // Sub CPU
        hd6309_1 := cpu_hd6309.create(6000000, 272 * CPU_SYNC, TCPU_HD6309E);
        hd6309_1.change_ram_calls(getbyte_suboscar, putbyte_suboscar);
        if not(roms_load(@mem_misc, oscar_sub)) then
          exit;
        // Sound CPU
        m6502_0.change_ram_calls(getbyte_snd_deco222, putbyte_snd_deco222);
        if not(roms_load(@mem_snd, oscar_snd)) then
          exit;
        for f := $8000 to $FFFF do
          snd_dec[f - $8000] := bitswap8(mem_snd[f], 7, 5, 6, 4, 3, 2, 1, 0);
        // Cargar chars
        if not(roms_load(@memoria_temp, oscar_char)) then
          exit;
        init_gfx(0, 8, 8, $400);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(3, 0, 8 * 8, $3000 * 8, $2000 * 8, $1000 * 8);
        convert_gfx(0, 0, @memoria_temp, @ps_x[8], @ps_y, false, false);
        // Cargar tiles
        getmem(ptemp, $100000);
        if not(roms_load(ptemp, oscar_tiles)) then
          exit;
        lastmissn_convert_tiles_sprites(1);
        // Cargar sprites
        if not(roms_load(ptemp, oscar_sprites)) then
          exit;
        lastmissn_convert_tiles_sprites(2);
        gfx[2].trans[0] := true;
        freemem(ptemp);
        // DIP
        marcade.dswa := $7F;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @oscar_dip_a;
        marcade.dswb_val2 := @oscar_dip_b;
      end;
  end;
  // final
  reset_dec8;
  start_dec8 := true;
end;

end.
