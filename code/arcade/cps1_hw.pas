unit cps1_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  timer_engine,
  ym_2151,
  oki6295,
  kabuki_decript,
  qsound,
  rom_engine,
  misc_functions,
  pal_engine,
  sound_engine,
  eeprom;

function start_cps1: boolean;

implementation

type
  cps1_games_def = record
    layerctrl: word;
    palctrl: word;
    testaddr, testval: word;
    mula, mulb, mull, mulh: word;
    mask_sc1, mask_sc2, mask_sc3, mask_sc4: byte;
    pri_mask1, pri_mask2, pri_mask3, pri_mask4: word;
  end;

  cps1_long_bank_def = record
    tipo: byte;
    start_bank, end_bank: dword;
    num_bank: byte;
  end;

  cps1_bank_def = record
    lbank: array [0 .. 3] of dword;
    bank: array [0 .. 6] of cps1_long_bank_def;
  end;

const
  GFXTYPE_SPRITES = 1 shl 0;
  GFXTYPE_SCROLL1 = 1 shl 1;
  GFXTYPE_SCROLL2 = 1 shl 2;
  GFXTYPE_SCROLL3 = 1 shl 3;
  GFXTYPE_STARS = 1 shl 4;
  // Banks
  cps1_banks: array [0 .. 10] of cps1_bank_def = (
    { DM620 } (lbank: ($8000, $2000, $2000, 0); bank: ((tipo: GFXTYPE_SCROLL3; start_bank: $8000; end_bank: $BFFF; num_bank: 1), (tipo: GFXTYPE_SPRITES; start_bank: $2000; end_bank: $3FFF;
    num_bank: 2), (tipo: GFXTYPE_STARS or GFXTYPE_SPRITES or GFXTYPE_SCROLL1 or GFXTYPE_SCROLL2 or GFXTYPE_SCROLL3; start_bank: $0; end_bank: $1FFFF; num_bank: 0), (), (), (), ())),
    { S224B } (lbank: ($8000, $0000, $0000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $43FF; num_bank: 0), (tipo: GFXTYPE_SCROLL1; start_bank: $4400; end_bank: $4BFF;
    num_bank: 0), (tipo: GFXTYPE_SCROLL3; start_bank: $4C00; end_bank: $5FFF; num_bank: 0), (tipo: GFXTYPE_SCROLL2; start_bank: $6000; end_bank: $7FFF; num_bank: 0), (), (), ())),
    { KD29B } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $8000; end_bank: $8FFF;
    num_bank: 1), (tipo: GFXTYPE_SCROLL2; start_bank: $9000; end_bank: $BFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL1; start_bank: $C000; end_bank: $D7FF; num_bank: 1), (tipo: GFXTYPE_SCROLL3;
    start_bank: $D800; end_bank: $FFFF; num_bank: 1), (), ())),
    { STF29 } (lbank: ($8000, $8000, $8000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $8000; end_bank: $FFFF;
    num_bank: 1), (tipo: GFXTYPE_SPRITES; start_bank: $10000; end_bank: $11FFF; num_bank: 2), (tipo: GFXTYPE_SCROLL3; start_bank: $2000; end_bank: $3FFF; num_bank: 2), (tipo: GFXTYPE_SCROLL1;
    start_bank: $4000; end_bank: $4FFF; num_bank: 2), (tipo: GFXTYPE_SCROLL2; start_bank: $5000; end_bank: $7FFF; num_bank: 2), ())),
    { ST24M1 } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_STARS; start_bank: $0000; end_bank: $03FF; num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $4FFF;
    num_bank: 0), (tipo: GFXTYPE_SCROLL2; start_bank: $4000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SCROLL3; start_bank: $0000; end_bank: $7FFF; num_bank: 1), (tipo: GFXTYPE_SCROLL1;
    start_bank: $7000; end_bank: $7FFF; num_bank: 1), (), ())),
    { RT24B } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $53FF; num_bank: 0), (tipo: GFXTYPE_SCROLL1; start_bank: $5400; end_bank: $6FFF;
    num_bank: 0), (tipo: GFXTYPE_SCROLL3; start_bank: $7000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SCROLL3; start_bank: $0000; end_bank: $3FFF; num_bank: 1), (tipo: GFXTYPE_SCROLL2;
    start_bank: $2800; end_bank: $7FFF; num_bank: 1), (tipo: GFXTYPE_SPRITES; start_bank: $5400; end_bank: $7FFF; num_bank: 1), ())),
    { CC63B } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SCROLL2; start_bank: $0000; end_bank: $7FFF;
    num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $8000; end_bank: $FFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL1; start_bank: $8000; end_bank: $FFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL2;
    start_bank: $8000; end_bank: $FFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL3; start_bank: $8000; end_bank: $FFFF; num_bank: 1), ())),
    { KR63B } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SCROLL2; start_bank: $0000; end_bank: $7FFF;
    num_bank: 0), (tipo: GFXTYPE_SCROLL1; start_bank: $8000; end_bank: $9FFF; num_bank: 1), (tipo: GFXTYPE_SPRITES; start_bank: $8000; end_bank: $CFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL2;
    start_bank: $8000; end_bank: $CFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL3; start_bank: $D000; end_bank: $FFFF; num_bank: 1), ())),
    { S9263B } (lbank: ($8000, $8000, $8000, 0); bank: ((tipo: GFXTYPE_SPRITES; start_bank: $0000; end_bank: $7FFF; num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $8000; end_bank: $FFFF;
    num_bank: 1), (tipo: GFXTYPE_SPRITES; start_bank: $10000; end_bank: $11FFF; num_bank: 2), (tipo: GFXTYPE_SCROLL3; start_bank: $2000; end_bank: $3FFF; num_bank: 2), (tipo: GFXTYPE_SCROLL1;
    start_bank: $4000; end_bank: $4FFF; num_bank: 2), (tipo: GFXTYPE_SCROLL2; start_bank: $5000; end_bank: $7FFF; num_bank: 2), ())),
    { CD63B } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_SCROLL1; start_bank: $0000; end_bank: $0FFF; num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $1000; end_bank: $7FFF;
    num_bank: 0), (tipo: GFXTYPE_SPRITES OR GFXTYPE_SCROLL2; start_bank: $8000; end_bank: $DFFF; num_bank: 1), (tipo: GFXTYPE_SCROLL3; start_bank: $E000; end_bank: $FFFF; num_bank: 1), (), (), ())),
    { PS63B } (lbank: ($8000, $8000, $0000, 0); bank: ((tipo: GFXTYPE_SCROLL1; start_bank: $0000; end_bank: $0FFF; num_bank: 0), (tipo: GFXTYPE_SPRITES; start_bank: $1000; end_bank: $7FFF;
    num_bank: 0), (tipo: GFXTYPE_SPRITES OR GFXTYPE_SCROLL2; start_bank: $8000; end_bank: $DBFF; num_bank: 1), (tipo: GFXTYPE_SCROLL3; start_bank: $DC00; end_bank: $FFFF; num_bank: 1), (), (), ())));
  // Games $140
  cps1_cps_b: array [0 .. 9] of cps1_games_def = (
    { CPS_B_01 } (layerctrl: $166; palctrl: $170; testaddr: $1FF; testval: $0000; mula: $1FF; mulb: $1FF; mull: $1FF; mulh: $1FF; mask_sc1: $02; mask_sc2: $04; mask_sc3: $08; mask_sc4: $30;
    pri_mask1: $168; pri_mask2: $16A; pri_mask3: $16C; pri_mask4: $16E),
    { CPS_B_04 } (layerctrl: $16E; palctrl: $16A; testaddr: $160; testval: $0004; mula: $1FF; mulb: $1FF; mull: $1FF; mulh: $1FF; mask_sc1: $02; mask_sc2: $04; mask_sc3: $08; mask_sc4: $0;
    pri_mask1: $166; pri_mask2: $170; pri_mask3: $168; pri_mask4: $172),
    { CPS_B_21_BT2 } (layerctrl: $160; palctrl: $170; testaddr: $1FF; testval: $0000; mula: $15E; mulb: $15C; mull: $15A; mulh: $158; mask_sc1: $30; mask_sc2: $08; mask_sc3: $30; mask_sc4: $0;
    pri_mask1: $16E; pri_mask2: $16C; pri_mask3: $16A; pri_mask4: $168),
    { CPS_B_11 } (layerctrl: $166; palctrl: $170; testaddr: $172; testval: $0401; mula: $1FF; mulb: $1FF; mull: $1FF; mulh: $1FF; mask_sc1: $08; mask_sc2: $10; mask_sc3: $20; mask_sc4: $0;
    pri_mask1: $168; pri_mask2: $16A; pri_mask3: $16C; pri_mask4: $16E),
    { CPS_B_21_BT1 } (layerctrl: $168; palctrl: $170; testaddr: $172; testval: $0800; mula: $14E; mulb: $14C; mull: $14A; mulh: $148; mask_sc1: $20; mask_sc2: $04; mask_sc3: $08; mask_sc4: $12;
    pri_mask1: $166; pri_mask2: $164; pri_mask3: $162; pri_mask4: $160),
    { CPS_B_21_BT3 } (layerctrl: $160; palctrl: $170; testaddr: $1FF; testval: $0000; mula: $146; mulb: $144; mull: $142; mulh: $140; mask_sc1: $20; mask_sc2: $12; mask_sc3: $12; mask_sc4: $0;
    pri_mask1: $16E; pri_mask2: $16C; pri_mask3: $16A; pri_mask4: $168),
    { CPS_B_21_BT4 } (layerctrl: $168; palctrl: $170; testaddr: $1FF; testval: $0000; mula: $146; mulb: $144; mull: $142; mulh: $140; mask_sc1: $20; mask_sc2: $10; mask_sc3: $82; mask_sc4: $0;
    pri_mask1: $166; pri_mask2: $164; pri_mask3: $162; pri_mask4: $160),
    { CPS_B_21_DEF } (layerctrl: $166; palctrl: $170; testaddr: $172; testval: $0000; mula: $140; mulb: $142; mull: $144; mulh: $146; mask_sc1: $02; mask_sc2: $04; mask_sc3: $08; mask_sc4: $30;
    pri_mask1: $168; pri_mask2: $16A; pri_mask3: $16C; pri_mask4: $16E),
    { CPS_B_21_QS2 } (layerctrl: $14A; palctrl: $144; testaddr: $1FF; testval: $0000; mula: $1FF; mulb: $1FF; mull: $1FF; mulh: $1FF; mask_sc1: $16; mask_sc2: $16; mask_sc3: $16; mask_sc4: $0;
    pri_mask1: $14C; pri_mask2: $14E; pri_mask3: $140; pri_mask4: $142),
    { CPS_B_21_QS3 } (layerctrl: $152; palctrl: $14C; testaddr: $14E; testval: $0C00; mula: $1FF; mulb: $1FF; mull: $1FF; mulh: $1FF; mask_sc1: $04; mask_sc2: $02; mask_sc3: $20; mask_sc4: $0;
    pri_mask1: $154; pri_mask2: $156; pri_mask3: $148; pri_mask4: $14A));

  // Ghouls and ghosts
  ghouls_rom1: array [0 .. 3] of tipo_roms = ((n: 'dme_29.10h'; l: $20000; p: 0; crc: $166A58A2), (n: 'dme_30.10j'; l: $20000; p: $1; crc: $7AC8407A), (n: 'dme_27.9h'; l: $20000; p: $40000;
    crc: $F734B2BE), (n: 'dme_28.9j'; l: $20000; p: $40001; crc: $03D3E714));
  ghouls_rom2: tipo_roms = (n: 'dm-17.7j'; l: $80000; p: $80000; crc: $3EA1B0F2);
  ghouls_sound: tipo_roms = (n: 'dm_26.10a'; l: $10000; p: 0; crc: $3692F6E5);
  ghouls_gfx1: array [0 .. 3] of tipo_roms = ((n: 'dm-05.3a'; l: $80000; p: 0; crc: $0BA9C0B0), (n: 'dm-07.3f'; l: $80000; p: 2; crc: $5D760AB9), (n: 'dm-06.3c'; l: $80000; p: 4; crc: $4BA90B59),
    (n: 'dm-08.3g'; l: $80000; p: 6; crc: $4BDEE9DE));
  ghouls_gfx2: array [0 .. 15] of tipo_roms = ((n: '09.4a'; l: $10000; p: $200000; crc: $AE24BB19), (n: '18.7a'; l: $10000; p: $200001; crc: $D34E271A), (n: '13.4e'; l: $10000; p: $200002;
    crc: $3F70DD37), (n: '22.7e'; l: $10000; p: $200003; crc: $7E69E2E6), (n: '11.4c'; l: $10000; p: $200004; crc: $37C9B6C6), (n: '20.7c'; l: $10000; p: $200005; crc: $2F1345B4), (n: '15.4g';
    l: $10000; p: $200006; crc: $3C2A212A), (n: '24.7g'; l: $10000; p: $200007; crc: $889AAC05), (n: '10.4b'; l: $10000; p: $280000; crc: $BCC0F28C), (n: '19.7b'; l: $10000; p: $280001;
    crc: $2A40166A), (n: '14.4f'; l: $10000; p: $280002; crc: $20F85C03), (n: '23.7f'; l: $10000; p: $280003; crc: $8426144B), (n: '12.4d'; l: $10000; p: $280004; crc: $DA088D61), (n: '21.7d';
    l: $10000; p: $280005; crc: $17E11DF0), (n: '16.4h'; l: $10000; p: $280006; crc: $F187BA1C), (n: '25.7h'; l: $10000; p: $280007; crc: $29F79C78));
  // Final Fight
  ffight_rom1: array [0 .. 3] of tipo_roms = ((n: 'ff_36.11f'; l: $20000; p: 0; crc: $F9A5CE83), (n: 'ff_42.11h'; l: $20000; p: $1; crc: $65F11215), (n: 'ff_37.12f'; l: $20000; p: $40000;
    crc: $E1033784), (n: 'ffe_43.12h'; l: $20000; p: $40001; crc: $995E968A));
  ffight_rom2: tipo_roms = (n: 'ff-32m.8h'; l: $80000; p: $80000; crc: $C747696E);
  ffight_sound: tipo_roms = (n: 'ff_09.12b'; l: $10000; p: 0; crc: $B8367EB5);
  ffight_gfx1: array [0 .. 3] of tipo_roms = ((n: 'ff-5m.7a'; l: $80000; p: 0; crc: $9C284108), (n: 'ff-7m.9a'; l: $80000; p: 2; crc: $A7584DFB), (n: 'ff-1m.3a'; l: $80000; p: 4; crc: $0B605E44),
    (n: 'ff-3m.5a'; l: $80000; p: 6; crc: $52291CD2));
  ffight_oki: array [0 .. 1] of tipo_roms = ((n: 'ff_18.11c'; l: $20000; p: 0; crc: $375C66E7), (n: 'ff_19.12c'; l: $20000; p: $20000; crc: $1EF137F9));
  // King of Dragons
  kod_rom1: array [0 .. 7] of tipo_roms = ((n: 'kde_30.11e'; l: $20000; p: $00000; crc: $C7414FD4), (n: 'kde_37.11f'; l: $20000; p: $00001; crc: $A5BF40D2), (n: 'kde_31.12e'; l: $20000; p: $40000;
    crc: $1FFFC7BD), (n: 'kde_38.12f'; l: $20000; p: $40001; crc: $89E57A82), (n: 'kde_28.9e'; l: $20000; p: $80000; crc: $9367BCD9), (n: 'kde_35.9f'; l: $20000; p: $80001; crc: $4CA6A48A),
    (n: 'kde_29.10e'; l: $20000; p: $C0000; crc: $6A0BA878), (n: 'kde_36.10f'; l: $20000; p: $C0001; crc: $B509B39D));
  kod_sound: tipo_roms = (n: 'kd_9.12a'; l: $10000; p: 0; crc: $BAC6EC26);
  kod_gfx1: array [0 .. 7] of tipo_roms = ((n: 'kd-5m.4a'; l: $80000; p: $000000; crc: $E45B8701), (n: 'kd-7m.6a'; l: $80000; p: $000002; crc: $A7750322), (n: 'kd-1m.3a'; l: $80000; p: $000004;
    crc: $5F74BF78), (n: 'kd-3m.5a'; l: $80000; p: $000006; crc: $5E5303BF), (n: 'kd-6m.4c'; l: $80000; p: $200000; crc: $113358F3), (n: 'kd-8m.6c'; l: $80000; p: $200002; crc: $38853C44),
    (n: 'kd-2m.3c'; l: $80000; p: $200004; crc: $9EF36604), (n: 'kd-4m.5c'; l: $80000; p: $200006; crc: $402B9B4F));
  kod_oki: array [0 .. 1] of tipo_roms = ((n: 'kd_18.11c'; l: $20000; p: 0; crc: $69ECB2C8), (n: 'kd_19.12c'; l: $20000; p: $20000; crc: $02D851C1));
  // Street Fighter 2
  sf2_rom1: array [0 .. 7] of tipo_roms = ((n: 'sf2e_30g.11e'; l: $20000; p: $00000; crc: $FE39EE33), (n: 'sf2e_37g.11f'; l: $20000; p: $00001; crc: $FB92CD74), (n: 'sf2e_31g.12e'; l: $20000;
    p: $40000; crc: $69A0A301), (n: 'sf2e_38g.12f'; l: $20000; p: $40001; crc: $5E22DB70), (n: 'sf2e_28g.9e'; l: $20000; p: $80000; crc: $8BF9F1E5), (n: 'sf2e_35g.9f'; l: $20000; p: $80001;
    crc: $626EF934), (n: 'sf2_29b.10e'; l: $20000; p: $C0000; crc: $BB4AF315), (n: 'sf2_36b.10f'; l: $20000; p: $C0001; crc: $C02A13EB));
  sf2_sound: tipo_roms = (n: 'sf2_09.12a'; l: $10000; p: 0; crc: $A4823A1B);
  sf2_gfx1: array [0 .. 11] of tipo_roms = ((n: 'sf2-5m.4a'; l: $80000; p: $000000; crc: $22C9CC8E), (n: 'sf2-7m.6a'; l: $80000; p: $000002; crc: $57213BE8), (n: 'sf2-1m.3a'; l: $80000; p: $000004;
    crc: $BA529B4F), (n: 'sf2-3m.5a'; l: $80000; p: $000006; crc: $4B1B33A8), (n: 'sf2-6m.4c'; l: $80000; p: $200000; crc: $2C7E2229), (n: 'sf2-8m.6c'; l: $80000; p: $200002; crc: $B5548F17),
    (n: 'sf2-2m.3c'; l: $80000; p: $200004; crc: $14B84312), (n: 'sf2-4m.5c'; l: $80000; p: $200006; crc: $5E9CD89A), (n: 'sf2-13m.4d'; l: $80000; p: $400000; crc: $994BFA58), (n: 'sf2-15m.6d';
    l: $80000; p: $400002; crc: $3E66AD9D), (n: 'sf2-9m.3d'; l: $80000; p: $400004; crc: $C1BEFAA8), (n: 'sf2-11m.5d'; l: $80000; p: $400006; crc: $0627C831));
  sf2_oki: array [0 .. 1] of tipo_roms = ((n: 'sf2_18.11c'; l: $20000; p: 0; crc: $7F162009), (n: 'sf2_19.12c'; l: $20000; p: $20000; crc: $BEADE53F));
  // Strider
  strider_rom1: array [0 .. 3] of tipo_roms = ((n: '30.11f'; l: $20000; p: $00000; crc: $DA997474), (n: '35.11h'; l: $20000; p: $00001; crc: $5463AAA3), (n: '31.12f'; l: $20000; p: $40000;
    crc: $D20786DB), (n: '36.12h'; l: $20000; p: $40001; crc: $21AA2863));
  strider_rom2: tipo_roms = (n: 'st-14.8h'; l: $80000; p: $80000; crc: $9B3CFC08);
  strider_sound: tipo_roms = (n: '09.12b'; l: $10000; p: 0; crc: $2ED403BC);
  strider_gfx1: array [0 .. 7] of tipo_roms = ((n: 'st-2.8a'; l: $80000; p: $000000; crc: $4EEE9AEA), (n: 'st-11.10a'; l: $80000; p: $000002; crc: $2D7F21E4), (n: 'st-5.4a'; l: $80000; p: $000004;
    crc: $7705AA46), (n: 'st-9.6a'; l: $80000; p: $000006; crc: $5B18B722), (n: 'st-1.7a'; l: $80000; p: $200000; crc: $005F000B), (n: 'st-10.9a'; l: $80000; p: $200002; crc: $B9441519),
    (n: 'st-4.3a'; l: $80000; p: $200004; crc: $B7D04E8B), (n: 'st-8.5a'; l: $80000; p: $200006; crc: $6B4713B4));
  strider_oki: array [0 .. 1] of tipo_roms = ((n: '18.11c'; l: $20000; p: 0; crc: $4386BC80), (n: '19.12c'; l: $20000; p: $20000; crc: $444536D7));
  // 3 Wonder
  wonder3_rom1: array [0 .. 7] of tipo_roms = ((n: 'rte_30a.11f'; l: $20000; p: $00000; crc: $EF5B8B33), (n: 'rte_35a.11h'; l: $20000; p: $00001; crc: $7D705529), (n: 'rte_31a.12f'; l: $20000;
    p: $40000; crc: $32835E5E), (n: 'rte_36a.12h'; l: $20000; p: $40001; crc: $7637975F), (n: 'rt_28a.9f'; l: $20000; p: $80000; crc: $054137C8), (n: 'rt_33a.9h'; l: $20000; p: $80001;
    crc: $7264CB1B), (n: 'rte_29a.10f'; l: $20000; p: $C0000; crc: $CDDAA919), (n: 'rte_34a.10h'; l: $20000; p: $C0001; crc: $ED52E7E5));
  wonder3_sound: tipo_roms = (n: 'rt_9.12b'; l: $10000; p: 0; crc: $ABFCA165);
  wonder3_gfx1: array [0 .. 7] of tipo_roms = ((n: 'rt-5m.7a'; l: $80000; p: $000000; crc: $86AEF804), (n: 'rt-7m.9a'; l: $80000; p: $000002; crc: $4F057110), (n: 'rt-1m.3a'; l: $80000; p: $000004;
    crc: $902489D0), (n: 'rt-3m.5a'; l: $80000; p: $000006; crc: $E35CE720), (n: 'rt-6m.8a'; l: $80000; p: $200000; crc: $13CB0E7C), (n: 'rt-8m.10a'; l: $80000; p: $200002; crc: $1F055014),
    (n: 'rt-2m.4a'; l: $80000; p: $200004; crc: $E9A034F4), (n: 'rt-4m.6a'; l: $80000; p: $200006; crc: $DF0EEA8B));
  wonder3_oki: array [0 .. 1] of tipo_roms = ((n: 'rt_18.11c'; l: $20000; p: 0; crc: $26B211AB), (n: 'rt_19.12c'; l: $20000; p: $20000; crc: $DBE64AD0));
  // Captain Commando
  ccommando_rom1: array [0 .. 1] of tipo_roms = ((n: 'cce_23d.8f'; l: $80000; p: $00000; crc: $42C814C5), (n: 'cc_22d.7f'; l: $80000; p: $80000; crc: $0FD34195));
  ccommando_rom2: array [0 .. 1] of tipo_roms = ((n: 'cc_24d.9e'; l: $20000; p: $100000; crc: $3A794F25), (n: 'cc_28d.9f'; l: $20000; p: $100001; crc: $FC3C2906));
  ccommando_sound: tipo_roms = (n: 'cc_09.11a'; l: $10000; p: 0; crc: $698E8B58);
  ccommando_gfx1: array [0 .. 7] of tipo_roms = ((n: 'cc-5m.3a'; l: $80000; p: $000000; crc: $7261D8BA), (n: 'cc-7m.5a'; l: $80000; p: $000002; crc: $6A60F949), (n: 'cc-1m.4a'; l: $80000; p: $000004;
    crc: $00637302), (n: 'cc-3m.6a'; l: $80000; p: $000006; crc: $CC87CF61), (n: 'cc-6m.7a'; l: $80000; p: $200000; crc: $28718BED), (n: 'cc-8m.9a'; l: $80000; p: $200002; crc: $D4ACC53A),
    (n: 'cc-2m.8a'; l: $80000; p: $200004; crc: $0C69F151), (n: 'cc-4m.10a'; l: $80000; p: $200006; crc: $1F9EBB97));
  ccommando_oki: array [0 .. 1] of tipo_roms = ((n: 'cc_18.11c'; l: $20000; p: 0; crc: $6DE2C2DB), (n: 'cc_19.12c'; l: $20000; p: $20000; crc: $B99091AE));
  // Knights of the round
  knights_rom1: array [0 .. 1] of tipo_roms = ((n: 'kr_23e.8f'; l: $80000; p: $00000; crc: $1B3997EB), (n: 'kr_22.7f'; l: $80000; p: $80000; crc: $D0B671A9));
  knights_sound: tipo_roms = (n: 'kr_09.11a'; l: $10000; p: 0; crc: $5E44D9EE);
  knights_gfx1: array [0 .. 7] of tipo_roms = ((n: 'kr-5m.3a'; l: $80000; p: $000000; crc: $9E36C1A4), (n: 'kr-7m.5a'; l: $80000; p: $000002; crc: $C5832CAE), (n: 'kr-1m.4a'; l: $80000; p: $000004;
    crc: $F095BE2D), (n: 'kr-3m.6a'; l: $80000; p: $000006; crc: $179DFD96), (n: 'kr-6m.7a'; l: $80000; p: $200000; crc: $1F4298D2), (n: 'kr-8m.9a'; l: $80000; p: $200002; crc: $37FA8751),
    (n: 'kr-2m.8a'; l: $80000; p: $200004; crc: $0200BC3D), (n: 'kr-4m.10a'; l: $80000; p: $200006; crc: $0BB2B4E7));
  knights_oki: array [0 .. 1] of tipo_roms = ((n: 'kr_18.11c'; l: $20000; p: 0; crc: $DA69D15F), (n: 'kr_19.12c'; l: $20000; p: $20000; crc: $BFC654E9));
  // Street Fighter II': Champion Edition
  sf2ce_rom1: array [0 .. 2] of tipo_roms = ((n: 's92e_23b.8f'; l: $80000; p: $00000; crc: $0AAA1A3A), (n: 's92_22b.7f'; l: $80000; p: $80000; crc: $2BBE15ED), (n: 's92_21a.6f'; l: $80000; p: $100000;
    crc: $925A7877));
  sf2ce_sound: tipo_roms = (n: 's92_09.11a'; l: $10000; p: 0; crc: $08F6B60E);
  sf2ce_gfx1: array [0 .. 11] of tipo_roms = ((n: 's92-1m.3a'; l: $80000; p: $000000; crc: $03B0D852), (n: 's92-3m.5a'; l: $80000; p: $000002; crc: $840289EC), (n: 's92-2m.4a'; l: $80000; p: $000004;
    crc: $CDB5F027), (n: 's92-4m.6a'; l: $80000; p: $000006; crc: $E2799472), (n: 's92-5m.7a'; l: $80000; p: $200000; crc: $BA8A2761), (n: 's92-7m.9a'; l: $80000; p: $200002; crc: $E584BFB5),
    (n: 's92-6m.8a'; l: $80000; p: $200004; crc: $21E3F87D), (n: 's92-8m.10a'; l: $80000; p: $200006; crc: $BEFC47DF), (n: 's92-10m.3c'; l: $80000; p: $400000; crc: $960687D5), (n: 's92-12m.5c';
    l: $80000; p: $400002; crc: $978ECD18), (n: 's92-11m.4c'; l: $80000; p: $400004; crc: $D6EC9A0A), (n: 's92-13m.6c'; l: $80000; p: $400006; crc: $ED2C67F6));
  sf2ce_oki: array [0 .. 1] of tipo_roms = ((n: 's92_18.11c'; l: $20000; p: 0; crc: $7F162009), (n: 's92_19.12c'; l: $20000; p: $20000; crc: $BEADE53F));
  // Cadillacs and Dinosaurs
  dino_rom1: array [0 .. 2] of tipo_roms = ((n: 'cde_23a.8f'; l: $80000; p: $00000; crc: $8F4E585E), (n: 'cde_22a.7f'; l: $80000; p: $80000; crc: $9278AA12), (n: 'cde_21a.6f'; l: $80000; p: $100000;
    crc: $66D23DE2));
  dino_sound: tipo_roms = (n: 'cd_q.5k'; l: $20000; p: 0; crc: $605FDB0B);
  dino_gfx1: array [0 .. 7] of tipo_roms = ((n: 'cd-1m.3a'; l: $80000; p: $000000; crc: $8DA4F917), (n: 'cd-3m.5a'; l: $80000; p: $000002; crc: $6C40F603), (n: 'cd-2m.4a'; l: $80000; p: $000004;
    crc: $09C8FC2D), (n: 'cd-4m.6a'; l: $80000; p: $000006; crc: $637FF38F), (n: 'cd-5m.7a'; l: $80000; p: $200000; crc: $470BEFEE), (n: 'cd-7m.9a'; l: $80000; p: $200002; crc: $22BFB7A3),
    (n: 'cd-6m.8a'; l: $80000; p: $200004; crc: $E7599AC4), (n: 'cd-8m.10a'; l: $80000; p: $200006; crc: $211B4B15));
  dino_qsound1: array [0 .. 3] of tipo_roms = ((n: 'cd-q1.1k'; l: $80000; p: $00000; crc: $60927775), (n: 'cd-q2.2k'; l: $80000; p: $80000; crc: $770F4C47), (n: 'cd-q3.3k'; l: $80000; p: $100000;
    crc: $2F273FFC), (n: 'cd-q4.4k'; l: $80000; p: $180000; crc: $2C67821D));
  // The Punisher
  punisher_rom1: array [0 .. 7] of tipo_roms = ((n: 'pse_26.11e'; l: $20000; p: $000000; crc: $389A99D2), (n: 'pse_30.11f'; l: $20000; p: $000001; crc: $68FB06AC), (n: 'pse_27.12e'; l: $20000;
    p: $040000; crc: $3EB181C3), (n: 'pse_31.12f'; l: $20000; p: $040001; crc: $37108E7B), (n: 'pse_24.9e'; l: $20000; p: $080000; crc: $0F434414), (n: 'pse_28.9f'; l: $20000; p: $080001;
    crc: $B732345D), (n: 'pse_25.10e'; l: $20000; p: $0C0000; crc: $B77102E2), (n: 'pse_29.10f'; l: $20000; p: $0C0001; crc: $EC037BCE));
  punisher_rom2: tipo_roms = (n: 'ps_21.6f'; l: $80000; p: $100000; crc: $8AFFA5A9);
  punisher_sound: tipo_roms = (n: 'ps_q.5k'; l: $20000; p: 0; crc: $49FF4446);
  punisher_gfx1: array [0 .. 7] of tipo_roms = ((n: 'ps-1m.3a'; l: $80000; p: $000000; crc: $77B7CCAB), (n: 'ps-3m.5a'; l: $80000; p: $000002; crc: $0122720B), (n: 'ps-2m.4a'; l: $80000; p: $000004;
    crc: $64FA58D4), (n: 'ps-4m.6a'; l: $80000; p: $000006; crc: $60DA42C8), (n: 'ps-5m.7a'; l: $80000; p: $200000; crc: $C54EA839), (n: 'ps-7m.9a'; l: $80000; p: $200002; crc: $04C5ACBD),
    (n: 'ps-6m.8a'; l: $80000; p: $200004; crc: $A544F4CC), (n: 'ps-8m.10a'; l: $80000; p: $200006; crc: $8F02F436));
  punisher_qsound1: array [0 .. 3] of tipo_roms = ((n: 'ps-q1.1k'; l: $80000; p: $00000; crc: $31FD8726), (n: 'ps-q2.2k'; l: $80000; p: $80000; crc: $980A9EEF), (n: 'ps-q3.3k'; l: $80000; p: $100000;
    crc: $0DD44491), (n: 'ps-q4.4k'; l: $80000; p: $180000; crc: $BED42F03));

var
  nbank, cps_b: byte;
  scroll_x1, scroll_y1, scroll_x2, scroll_y2, scroll_x3, scroll_y3: word;
  rom: array [0 .. $1FFFFF] of word;
  ram: array [0 .. $7FFF] of word;
  vram: array [0 .. $17FFF] of word;
  snd_rom: array [0 .. 5, 0 .. $3FFF] of byte;
  sprite_buffer: array [0 .. $3FF] of word;
  qram1, qram2: array [0 .. $FFF] of byte;
  qsnd_opcode, qsnd_data: array [0 .. $7FFF] of byte;
  sound_latch, sound_latch2, sound_bank, dswa, dswb, dswc: byte;
  cps1_sprites, cps1_scroll1, cps1_scroll2, cps1_scroll3, cps1_rowscroll, cps1_pal: dword;
  cps1_mula, cps1_mulb, cps1_layer, scroll_pri_x, scroll_pri_y: word;
  cps1_palcltr, pri_mask0, pri_mask1, pri_mask2, pri_mask3: word;
  cps1_rowscrollstart: word;
  pal_change, mask_change, sprites_pri_draw, rowscroll_ena: boolean;

procedure update_video_cps1;
var
  l0, l1, l2, l3: byte;
  procedure pal_calc;
  var
    page, bright: byte;
    color: tcolor;
    offset, palette, pos_buf: word;
    pos: dword;
  begin
    pos_buf := 0;
    for page := 0 to 5 do
    begin
      if BIT(cps1_palcltr, page) then
      begin
        for offset := 0 to $1FF do
        begin
          if buffer_paleta[pos_buf] <> 0 then
          begin
            buffer_paleta[pos_buf] := 0;
            pos := (pos_buf * 2) + cps1_pal;
            palette := vram[pos shr 1];
            bright := $F + ((palette shr 12) shl 1);
            color.r := (((palette shr 8) and $F) * $11 * bright) div $2D;
            color.g := (((palette shr 4) and $F) * $11 * bright) div $2D;
            color.b := (((palette shr 0) and $F) * $11 * bright) div $2D;
            set_pal_color(color, ($200 * page) + offset);
            if page < 4 then
              buffer_color[(offset shr 4) + ((page - 1) * $20)] := true;
          end;
          pos_buf := pos_buf + 1;
        end; // skip page in gfxram, but only if we have already copied at least one page
      end
      else if (pos_buf <> 0) then
        pos_buf := pos_buf + $200;
    end;
    pal_change := false;
  end;
  function gfx_bank(tipo: byte; nchar: word): integer;
  var
    shift, pos: byte;
    code, base: dword;
    i: integer;
  begin
    case tipo of
      GFXTYPE_SPRITES:
        shift := 1;
      GFXTYPE_SCROLL1:
        shift := 0;
      GFXTYPE_SCROLL2:
        shift := 1;
      GFXTYPE_SCROLL3:
        shift := 3;
    end;
    code := nchar shl shift;
    pos := 0;
    while (cps1_banks[nbank].bank[pos].tipo <> 0) do
    begin
      if ((code >= cps1_banks[nbank].bank[pos].start_bank) and (code <= cps1_banks[nbank].bank[pos].end_bank)) then
      begin
        if (cps1_banks[nbank].bank[pos].tipo and tipo) <> 0 then
        begin
          base := 0;
          for i := 0 to (cps1_banks[nbank].bank[pos].num_bank - 1) do
            base := base + (cps1_banks[nbank].lbank[i]);
          gfx_bank := (base + (code and (cps1_banks[nbank].lbank[cps1_banks[nbank].bank[pos].num_bank] - 1))) shr shift;
          exit;
        end;
      end;
      pos := pos + 1;
    end;
    gfx_bank := -1;
  end;
  procedure draw_sprites;
  var
    f, color, col, rx, ry, yy, xx: word;
    flipx, flipy: boolean;
    x, y, nchar, dx, dy, dxx: integer;
    last_sprite: byte;
  begin
    // Find last sprite
    last_sprite := $FE;
    for f := 0 to $FE do
    begin
      color := sprite_buffer[(f * 4) + 3];
      if color = $FF00 then
      begin
        last_sprite := f;
        break;
      end;
    end;
    for f := last_sprite downto 0 do
    begin
      nchar := sprite_buffer[(f shl 2) + 2];
      nchar := gfx_bank(GFXTYPE_SPRITES, nchar);
      if nchar <> -1 then
      begin
        color := sprite_buffer[(f shl 2) + 3];
        x := sprite_buffer[(f shl 2) + 0];
        y := sprite_buffer[(f shl 2) + 1];
        col := (color and $1F) shl 4;
        rx := (color shr 8) and $F;
        ry := (color shr 12) and $F;
        if (color and $20) <> 0 then
        begin // flip_x
          flipx := true;
          x := x + (rx shl 4);
          dx := -16;
          dxx := (rx + 1) shl 4;
        end
        else
        begin
          flipx := false;
          dx := 16;
          dxx := -((rx + 1) shl 4);
        end;
        if (color and $40) <> 0 then
        begin // flip_y
          flipy := true;
          y := y + (ry shl 4);
          dy := -16;
        end
        else
        begin
          flipy := false;
          dy := 16;
        end;
        for yy := 0 to ry do
        begin
          for xx := 0 to rx do
          begin
            put_gfx_sprite(nchar + xx + (yy shl 4), col, flipx, flipy, 2);
            update_gfx_sprite(x, y, 5, 2);
            x := x + dx;
          end;
          x := x + dxx;
          y := y + dy;
        end;
      end;
    end;
    // Prioridad de los sprites
    if sprites_pri_draw then
    begin
      scroll_x_y(4, 5, scroll_pri_x, scroll_pri_y);
      sprites_pri_draw := false;
    end;
  end;
  procedure draw_layer(nlayer: byte; sprite_next: boolean);
  var
    f, atrib, color, sx, sy, pos: word;
    x, y, pant: byte;
    address: dword;
    nchar: integer;
    flipx, flipy: boolean;
    scroll_data_x: array [0 .. $3FF] of word;
  begin
    case nlayer of
      0:
        draw_sprites;
      1:
        if (cps1_layer and cps1_cps_b[cps_b].mask_sc1) = cps1_cps_b[cps_b].mask_sc1 then
        begin
          if (sprite_next and mask_change) then
          begin
            mask_change := false;
            fillchar(gfx[0].buffer, $1000, 1);
          end;
          for f := 0 to $6C7 do
          begin
            x := f mod 56;
            y := f div 56;
            sx := x + ((scroll_x1 and $1F8) div 8);
            sy := y + ((scroll_y1 and $1F8) div 8);
            pos := (sy and $1F) + ((sx and $3F) shl 5) + ((sy and $20) shl 6);
            address := cps1_scroll1 + (pos * 4);
            atrib := vram[(address + 2) shr 1];
            color := atrib and $1F;
            if (gfx[0].buffer[pos] or buffer_color[color]) then
            begin
              nchar := vram[address shr 1];
              nchar := gfx_bank(GFXTYPE_SCROLL1, nchar);
              if nchar = -1 then
              begin
                put_gfx_block_trans(x * 8, y * 8, 1, 8, 8);
                if sprite_next then
                  put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
              end
              else
              begin
                flipx := (atrib and $20) <> 0;
                flipy := (atrib and $40) <> 0;
                color := (color + $20) shl 4;
                put_gfx_trans_flip(x * 8, y * 8, nchar, color, 1, (pos and $20) shr 5, flipx, flipy);
                if sprite_next then
                begin
                  pant := (atrib and $180) shr 7;
                  put_gfx_trans_flip_alt(x * 8, y * 8, nchar, color, 4, (pos and $20) shr 5, flipx, flipy, pant);
                end;
              end;
              gfx[0].buffer[pos] := false;
            end;
          end;
          if sprite_next then
          begin
            sprites_pri_draw := true;
            scroll_pri_x := scroll_x1 and $7;
            scroll_pri_y := scroll_y1 and $7;
          end;
          scroll_x_y(1, 5, scroll_x1 and $7, scroll_y1 and $7);
        end;
      2:
        if (cps1_layer and cps1_cps_b[cps_b].mask_sc2) <> 0 then
        begin
          if (sprite_next and mask_change) then
          begin
            mask_change := false;
            fillchar(gfx[2].buffer, $1000, 1);
          end;
          for f := 0 to $1CF do
          begin
            x := f mod 29;
            y := f div 29;
            sx := x + ((scroll_x2 and $3F0) div 16);
            sy := y + ((scroll_y2 and $3F0) div 16);
            pos := (sy and $0F) + ((sx and $3F) shl 4) + ((sy and $30) shl 6);
            address := cps1_scroll2 + (pos * 4);
            atrib := vram[(address + 2) shr 1];
            color := atrib and $1F;
            if (gfx[2].buffer[pos] or buffer_color[color + $20]) then
            begin
              nchar := vram[address shr 1];
              nchar := gfx_bank(GFXTYPE_SCROLL2, nchar);
              if nchar = -1 then
              begin
                put_gfx_block_trans(x * 16, y * 16, 2, 16, 16);
                if sprite_next then
                  put_gfx_block_trans(x * 16, y * 16, 4, 16, 16);
              end
              else
              begin
                color := (color + $40) shl 4;
                flipx := (atrib and $20) <> 0;
                flipy := (atrib and $40) <> 0;
                put_gfx_trans_flip(x * 16, y * 16, nchar, color, 2, 2, flipx, flipy);
                if sprite_next then
                begin
                  pant := ((atrib and $180) shr 7);
                  put_gfx_trans_flip_alt(x * 16, y * 16, nchar, color, 4, 2, flipx, flipy, pant);
                end;
              end;
              gfx[2].buffer[pos] := false;
            end;
          end;
          if sprite_next then
          begin
            sprites_pri_draw := true;
            scroll_pri_x := scroll_x2 and $F;
            scroll_pri_y := scroll_y2 and $F;
          end;
          if not(rowscroll_ena) then
            scroll_x_y(2, 5, scroll_x2 and $F, scroll_y2 and $F)
          else
          begin
            for f := 0 to $3FF do
              scroll_data_x[(f - scroll_y2) and $3FF] := vram[((cps1_rowscroll + cps1_rowscrollstart) shr 1) + f] and $3FF;
            scroll__x_part2(2, 5, 1, @scroll_data_x, scroll_x2 and $F, scroll_y2 and $F);
          end;
        end;
      3:
        if (cps1_layer and cps1_cps_b[cps_b].mask_sc3) <> 0 then
        begin
          if (sprite_next and mask_change) then
          begin
            // Si se ha modificado las mascaras, y es la pantalla de las prioridades borrar todo
            mask_change := false;
            fillchar(gfx[3].buffer, $1000, 1);
          end;
          for f := 0 to $95 do
          begin
            x := f mod 15;
            y := f div 15;
            sx := x + ((scroll_x3 and $7E0) div 32);
            sy := y + ((scroll_y3 and $7E0) div 32);
            pos := (sy and $07) + ((sx and $3F) shl 3) + ((sy and $38) shl 6);
            address := cps1_scroll3 + (pos * 4);
            atrib := vram[(address + 2) shr 1];
            color := atrib and $1F;
            if (gfx[3].buffer[pos] or buffer_color[color + $40]) then
            begin
              nchar := vram[address shr 1];
              nchar := gfx_bank(GFXTYPE_SCROLL3, nchar);
              if nchar = -1 then
              begin
                // Si esta fuera de rango, poner un tile vacio (incluida la pantalla de las prioridades)
                put_gfx_block_trans(x * 32, y * 32, 3, 32, 32);
                if sprite_next then
                  put_gfx_block_trans(x * 32, y * 32, 4, 32, 32);
              end
              else
              begin
                color := (color + $60) shl 4;
                flipx := (atrib and $20) <> 0;
                flipy := (atrib and $40) <> 0;
                put_gfx_trans_flip(x * 32, y * 32, nchar, color, 3, 3, flipx, flipy);
                if sprite_next then
                begin
                  // ¿Es la pantalla de prioridad? Actualizarla
                  pant := ((atrib and $180) shr 7);
                  put_gfx_trans_flip_alt(x * 32, y * 32, nchar, color, 4, 3, flipx, flipy, pant);
                end;
              end;
              gfx[3].buffer[pos] := false;
            end;
          end;
          if sprite_next then
          begin
            // Si es la pantalla de prioridades de sprites, poner las variables...
            sprites_pri_draw := true;
            scroll_pri_x := scroll_x3 and $1F;
            scroll_pri_y := scroll_y3 and $1F;
          end;
          scroll_x_y(3, 5, scroll_x3 and $1F, scroll_y3 and $1F);
        end;
    end;
  end;

begin
  if pal_change then
    pal_calc;
  fill_full_screen(5, $BFF);
  l0 := (cps1_layer shr 6) and 3;
  l1 := (cps1_layer shr 8) and 3;
  l2 := (cps1_layer shr $A) and 3;
  l3 := (cps1_layer shr $C) and 3;
  // draw_starts
  draw_layer(l0, l1 = 0);
  draw_layer(l1, l2 = 0);
  draw_layer(l2, l3 = 0);
  draw_layer(l3, false);
  update_final_piece(64, 16, 384, 224, 5);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_cps1;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but3[0] then
      marcade.in2 := (marcade.in2 and $FFFE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.but4[0] then
      marcade.in2 := (marcade.in2 and $FFFD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.but5[0] then
      marcade.in2 := (marcade.in2 and $FFFB)
    else
      marcade.in2 := (marcade.in2 or $4);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FEFF)
    else
      marcade.in1 := (marcade.in1 or $100);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $200);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FBFF)
    else
      marcade.in1 := (marcade.in1 or $400);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7FF)
    else
      marcade.in1 := (marcade.in1 or $800);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.but3[1] then
      marcade.in2 := (marcade.in2 and $FFEF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but4[1] then
      marcade.in2 := (marcade.in2 and $FFDF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but5[1] then
      marcade.in2 := (marcade.in2 and $FFBF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
  end;
end;

procedure calc_mask(mask: word; index: byte);
var
  f: byte;
  val: boolean;
begin
  for f := 0 to 15 do
  begin
    val := ((mask shr f) and 1) = 0;
    gfx[0].trans_alt[index][f] := val;
  end;
  copymemory(@gfx[1].trans_alt[index][0], @gfx[0].trans_alt[index][0], 16);
  copymemory(@gfx[2].trans_alt[index][0], @gfx[0].trans_alt[index][0], 16);
  copymemory(@gfx[3].trans_alt[index][0], @gfx[0].trans_alt[index][0], 16);
end;

procedure cps1_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 261 do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          m68000_0.irq[2] := HOLD_LINE;
          update_video_cps1;
          copymemory(@sprite_buffer, @vram[cps1_sprites shr 1], $400 * 2);
        end;
      end;
      events_cps1;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function cps1_read_io_w(dir: word): word;
var
  res: word;
begin
  res := $FFFF;
  case dir of
    $000, $002, $004, $006:
      res := marcade.in1; // P1+P2
    $018:
      res := marcade.in0; // SYS
    $01A:
      res := (dswa shl 8) + $FF; // DSWA
    $01C:
      res := (dswb shl 8) + $FF; // DSWB
    $01E:
      res := (dswc shl 8) + $FF; // DSWC
    $176:
      res := marcade.in2; // Extra buttons
  end;
  if (dir = cps1_cps_b[cps_b].testaddr) then
    res := cps1_cps_b[cps_b].testval;
  if (dir = cps1_cps_b[cps_b].mull) then
    res := (cps1_mula * cps1_mulb) and $FFFF;
  if (dir = cps1_cps_b[cps_b].mulh) then
    res := (cps1_mula * cps1_mulb) shr 16;
  cps1_read_io_w := res;
end;

function cps1_getword(direccion: dword): word;
begin
  case direccion of
    $000000 .. $3FFFFF:
      cps1_getword := rom[direccion shr 1];
    $800000 .. $8001FF:
      cps1_getword := cps1_read_io_w(direccion and $1FE);
    $900000 .. $92FFFF:
      cps1_getword := vram[(direccion and $3FFFF) shr 1];
    $FF0000 .. $FFFFFF:
      cps1_getword := ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure cps1_write_io_w(dir, val: word);
begin
  case dir of
    $100:
      cps1_sprites := (val * 256) - $900000;
    $102:
      if cps1_scroll1 <> (((val * 256) and not($3FFF)) - $900000) then
      begin
        cps1_scroll1 := ((val * 256) and not($3FFF)) - $900000;
        fillchar(gfx[0].buffer, $1000, 1);
      end;
    $104:
      if cps1_scroll2 <> (((val * 256) and not($3FFF)) - $900000) then
      begin
        cps1_scroll2 := ((val * 256) and not($3FFF)) - $900000;
        fillchar(gfx[2].buffer, $1000, 1);
      end;
    $106:
      if cps1_scroll3 <> (((val * 256) and not($3FFF)) - $900000) then
      begin
        cps1_scroll3 := ((val * 256) and not($3FFF)) - $900000;
        fillchar(gfx[3].buffer, $1000, 1);
      end;
    $108:
      cps1_rowscroll := ((val * 256) and not($3FFF)) - $900000;
    $10A:
      if cps1_pal <> (((val * 256) - $900000) and $1FFFF) then
      begin
        cps1_pal := ((val * 256) - $900000) and $1FFFF;
        fillchar(buffer_paleta, $200 * 6, 1);
        pal_change := true;
      end;
    $10C:
      if scroll_x1 <> (val and $1FF) then
      begin
        if abs((scroll_x1 and $1F8) - (val and $1F8)) > 7 then
          fillchar(gfx[0].buffer, $1000, 1);
        scroll_x1 := val and $1FF;
      end;
    $10E:
      if scroll_y1 <> (val and $1FF) then
      begin
        if abs((scroll_y1 and $1F8) - (val and $1F8)) > 7 then
          fillchar(gfx[0].buffer, $1000, 1);
        scroll_y1 := val and $1FF;
      end;
    $110:
      if scroll_x2 <> (val and $3FF) then
      begin
        if abs((scroll_x2 and $3F0) - (val and $3F0)) > 15 then
          fillchar(gfx[2].buffer, $1000, 1);
        scroll_x2 := val and $3FF;
      end;
    $112:
      if scroll_y2 <> (val and $3FF) then
      begin
        if abs((scroll_y2 and $3F0) - (val and $3F0)) > 15 then
          fillchar(gfx[2].buffer, $1000, 1);
        scroll_y2 := val and $3FF;
      end;
    $114:
      if scroll_x3 <> (val and $7FF) then
      begin
        if abs((scroll_x3 and $7E0) - (val and $7E0)) > 31 then
          fillchar(gfx[3].buffer, $1000, 1);
        scroll_x3 := val and $7FF;
      end;
    $116:
      if scroll_y3 <> (val and $7FF) then
      begin
        if abs((scroll_y3 and $7E0) - (val and $7E0)) > 31 then
          fillchar(gfx[3].buffer, $1000, 1);
        scroll_y3 := (val and $7FF);
      end;
    $120:
      cps1_rowscrollstart := val;
    $122:
      begin // cps1_vidctrl
        main_screen.flip_main_screen := (val and $8000) <> 0;
        rowscroll_ena := (val and 1) <> 0;
      end;
    $180, $182, $184, $186:
      sound_latch := val and $FF;
    $188, $18A, $18C, $18E:
      sound_latch2 := val and $FF;
  end;
  if (dir = cps1_cps_b[cps_b].palctrl) then
  begin
    if cps1_palcltr <> val then
    begin
      cps1_palcltr := val;
      pal_change := true;
      fillchar(buffer_paleta, $200 * 6, 1);
    end;
  end;
  if (dir = cps1_cps_b[cps_b].mula) then
    cps1_mula := val;
  if (dir = cps1_cps_b[cps_b].mulb) then
    cps1_mulb := val;
  if (dir = cps1_cps_b[cps_b].layerctrl) then
    cps1_layer := val;
  if (dir = cps1_cps_b[cps_b].pri_mask1) then
  begin
    if pri_mask0 <> val then
    begin
      calc_mask(val, 0);
      pri_mask0 := val;
      mask_change := true;
    end;
  end;
  if (dir = cps1_cps_b[cps_b].pri_mask2) then
  begin
    if pri_mask1 <> val then
    begin
      calc_mask(val, 1);
      pri_mask1 := val;
      mask_change := true;
    end;
  end;
  if (dir = cps1_cps_b[cps_b].pri_mask3) then
  begin
    if pri_mask2 <> val then
    begin
      calc_mask(val, 2);
      pri_mask2 := val;
      mask_change := true;
    end;
  end;
  if (dir = cps1_cps_b[cps_b].pri_mask4) then
  begin
    if pri_mask3 <> val then
    begin
      calc_mask(val, 3);
      pri_mask3 := val;
      mask_change := true;
    end;
  end;
end;

procedure test_buffers(direccion: dword);
begin
  if ((direccion >= cps1_pal) and (direccion < (cps1_pal + $1800))) then
  begin
    pal_change := true;
    buffer_paleta[(direccion - cps1_pal) shr 1] := 1;
    exit;
  end;
  if ((direccion >= cps1_scroll1) and (direccion < (cps1_scroll1 + $4000))) then
    gfx[0].buffer[(direccion - cps1_scroll1) shr 2] := true;
  if ((direccion >= cps1_scroll2) and (direccion < (cps1_scroll2 + $4000))) then
    gfx[2].buffer[(direccion - cps1_scroll2) shr 2] := true;
  if ((direccion >= cps1_scroll3) and (direccion < (cps1_scroll3 + $4000))) then
    gfx[3].buffer[(direccion - cps1_scroll3) shr 2] := true;
end;

procedure cps1_putword(direccion: dword; valor: word);
begin
  case direccion of
    $000000 .. $3FFFFF:
      ; // ROM
    $800000 .. $8001FF:
      cps1_write_io_w(direccion and $1FE, valor);
    $900000 .. $92FFFF:
      if (vram[(direccion and $3FFFF) shr 1] <> valor) then
      begin
        vram[(direccion and $3FFFF) shr 1] := valor;
        test_buffers(direccion and $3FFFF);
      end;
    $FF0000 .. $FFFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

// Sonido
function cps1_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF, $D000 .. $D7FF:
      cps1_snd_getbyte := mem_snd[direccion];
    $8000 .. $BFFF:
      cps1_snd_getbyte := snd_rom[sound_bank, direccion and $3FFF];
    $F001:
      cps1_snd_getbyte := ym2151_0.status;
    $F002:
      cps1_snd_getbyte := oki_6295_0.read;
    $F008:
      cps1_snd_getbyte := sound_latch;
    $F00A:
      cps1_snd_getbyte := sound_latch2;
  end;
end;

procedure cps1_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $D000 .. $D7FF:
      mem_snd[direccion] := valor;
    $F000:
      ym2151_0.reg(valor);
    $F001:
      ym2151_0.write(valor);
    $F002:
      oki_6295_0.write(valor);
    $F004:
      sound_bank := valor and $1;
    $F006:
      oki_6295_0.change_pin7(valor and 1);
  end;
end;

procedure cps1_ym2151_snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

procedure cps1_sound_update;
begin
  ym2151_0.update;
  oki_6295_0.update;
end;

// Qsound
function cps1_qsnd_getword(direccion: dword): word;
begin
  case direccion of
    $000000 .. $17FFFF:
      cps1_qsnd_getword := rom[direccion shr 1];
    $800000 .. $8001FF:
      cps1_qsnd_getword := cps1_read_io_w(direccion and $1FE);
    $900000 .. $92FFFF:
      cps1_qsnd_getword := vram[(direccion and $3FFFF) shr 1];
    // qsound
    $F18000 .. $F19FFF:
      cps1_qsnd_getword := $FF00 + qram1[(direccion shr 1) and $FFF];
    $F1C000, $F1C002:
      cps1_qsnd_getword := $FF;
    $F1C006:
      cps1_qsnd_getword := eeprom_0.readbit;
    $F1E000 .. $F1FFFF:
      cps1_qsnd_getword := $FF00 + qram2[(direccion shr 1) and $FFF];
    $FF0000 .. $FFFFFF:
      cps1_qsnd_getword := ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure cps1_qsnd_putword(direccion: dword; valor: word);
begin
  case direccion of
    $000000 .. $17FFFF:
      ; // ROM
    $800000 .. $8001FF:
      cps1_write_io_w(direccion and $1FE, valor);
    $900000 .. $92FFFF:
      if (vram[(direccion and $3FFFF) shr 1] <> valor) then
      begin
        vram[(direccion and $3FFFF) shr 1] := valor;
        test_buffers(direccion and $3FFFF);
      end;
    $F18000 .. $F19FFF:
      qram1[(direccion shr 1) and $FFF] := valor and $FF;
    $F1C006:
      begin
        eeprom_0.write_bit(valor and 1);
        eeprom_0.set_clock_line(valor and $40);
        eeprom_0.set_cs_line(not(valor) and $80);
      end;
    $F1E000 .. $F1FFFF:
      qram2[(direccion shr 1) and $FFF] := valor and $FF;
    $FF0000 .. $FFFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

function cps1_qz80_getbyte(direccion: word): byte;
begin
  case direccion of
    $0000 .. $7FFF:
      if z80_0.opcode then
        cps1_qz80_getbyte := qsnd_opcode[direccion]
      else
        cps1_qz80_getbyte := qsnd_data[direccion];
    $8000 .. $BFFF:
      cps1_qz80_getbyte := snd_rom[sound_bank, direccion and $3FFF];
    $C000 .. $CFFF:
      cps1_qz80_getbyte := qram1[direccion and $FFF];
    $D007:
      cps1_qz80_getbyte := qsound_r;
    $F000 .. $FFFF:
      cps1_qz80_getbyte := qram2[direccion and $FFF];
  end;
end;

procedure cps1_qz80_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF:
      qram1[direccion and $FFF] := valor;
    $D000 .. $D002:
      qsound_w(direccion and $3, valor);
    $D003:
      sound_bank := valor and $F;
    $F000 .. $FFFF:
      qram2[direccion and $FFF] := valor;
  end;
end;

procedure cps1_qsnd_int;
begin
  z80_0.change_irq(HOLD_LINE);
end;

// Main
procedure reset_cps1;
begin
  m68000_0.reset;
  z80_0.reset;
  reset_audio;
  case main_vars.machine_type of
    103 .. 111:
      begin
        ym2151_0.reset;
        oki_6295_0.reset;
      end;
    112, 113:
      begin
        qsound_reset;
        eeprom_0.reset;
      end;
  end;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  marcade.in2 := $FFFF;
  sound_latch := 0;
  sound_latch2 := 0;
  sound_bank := 0;
  scroll_x1 := 0;
  scroll_y1 := 0;
  scroll_x2 := 0;
  scroll_y2 := 0;
  scroll_x3 := 0;
  scroll_y3 := 0;
  cps1_sprites := $FFFF;
  cps1_scroll1 := $FFFF;
  cps1_scroll2 := $FFFF;
  cps1_scroll3 := $FFFF;
  cps1_rowscroll := 0;
  cps1_pal := $FFFF;
  // cps1_rowscrollstart:=0;
  cps1_mula := 0;
  cps1_mulb := 0;
  cps1_layer := 0;
  cps1_palcltr := 0;
  pal_change := false;
  scroll_pri_x := 0;
  scroll_pri_y := 0;
  sprites_pri_draw := false;
  fillchar(buffer_paleta, $200 * 6, 1);
  pri_mask0 := 0;
  pri_mask1 := 0;
  pri_mask2 := 0;
  pri_mask3 := 0;
  calc_mask(0, 0);
  calc_mask(0, 1);
  calc_mask(0, 2);
  calc_mask(0, 3);
end;

procedure cps1_gfx_decode(memory_temp: pbyte; gfxsize: dword);
var
  i: dword;
  src, dwval, mask: dword;
  j, n: byte;
  ptemp: pbyte;
begin
  gfxsize := gfxsize div 4;
  for i := 0 to (gfxsize - 1) do
  begin
    ptemp := memory_temp;
    inc(ptemp, 4 * i);
    src := ptemp^;
    ptemp := memory_temp;
    inc(ptemp, (4 * i) + 1);
    src := src or (ptemp^ shl 8);
    ptemp := memory_temp;
    inc(ptemp, (4 * i) + 2);
    src := src or (ptemp^ shl 16);
    ptemp := memory_temp;
    inc(ptemp, (4 * i) + 3);
    src := src or (ptemp^ shl 24);
    dwval := 0;
    for j := 0 to 7 do
    begin
      n := 0;
      mask := ($80808080 shr j) and src;
      if (mask and $000000FF) <> 0 then
        n := n or 1;
      if (mask and $0000FF00) <> 0 then
        n := n or 2;
      if (mask and $00FF0000) <> 0 then
        n := n or 4;
      if (mask and $FF000000) <> 0 then
        n := n or 8;
      dwval := dwval or (n shl (j * 4));
    end;
    ptemp := memory_temp;
    inc(ptemp, 4 * i);
    ptemp^ := (dwval shr 0) and $FF;
    ptemp := memory_temp;
    inc(ptemp, (4 * i) + 1);
    ptemp^ := (dwval shr 8) and $FF;
    ptemp := memory_temp;
    inc(ptemp, (4 * i) + 2);
    ptemp^ := (dwval shr 16) and $FF;
    ptemp := memory_temp;
    inc(ptemp, (4 * i) + 3);
    ptemp^ := (dwval shr 24) and $FF;
  end;
end;

procedure close_cps1;
begin
  case main_vars.machine_type of
    112 .. 113:
      qsound_close;
  end;
end;

function start_cps1: boolean;
var
  memory_temp, ptemp: pbyte;
  f: byte;
const
  pt_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
  pt2_x: array [0 .. 31] of dword = (1 * 4, 0 * 4, 3 * 4, 2 * 4, 5 * 4, 4 * 4, 7 * 4, 6 * 4, 9 * 4, 8 * 4, 11 * 4, 10 * 4, 13 * 4, 12 * 4, 15 * 4, 14 * 4, 17 * 4, 16 * 4, 19 * 4, 18 * 4, 21 * 4,
    20 * 4, 23 * 4, 22 * 4, 25 * 4, 24 * 4, 27 * 4, 26 * 4, 29 * 4, 28 * 4, 31 * 4, 30 * 4);
  pt2_y: array [0 .. 31] of dword = (0 * 128, 1 * 128, 2 * 128, 3 * 128, 4 * 128, 5 * 128, 6 * 128, 7 * 128, 8 * 128, 9 * 128, 10 * 128, 11 * 128, 12 * 128, 13 * 128, 14 * 128, 15 * 128, 16 * 128,
    17 * 128, 18 * 128, 19 * 128, 20 * 128, 21 * 128, 22 * 128, 23 * 128, 24 * 128, 25 * 128, 26 * 128, 27 * 128, 28 * 128, 29 * 128, 30 * 128, 31 * 128);

  procedure poner_roms_word;
  var
    rom_size: dword;
    tempw: word;
  begin
    // es una mierda... Lo cargo todo como bytes y luego lo convierto a word...
    ptemp := memory_temp;
    for rom_size := 0 to $BFFFF do
    begin
      tempw := ptemp^ shl 8;
      inc(ptemp);
      tempw := tempw or ptemp^;
      inc(ptemp);
      rom[rom_size] := tempw;
    end;
  end;

  procedure convert_chars(n: dword);
  begin
    init_gfx(0, 8, 8, n);
    init_gfx(1, 8, 8, n);
    gfx[0].trans[15] := true;
    gfx[1].trans[15] := true;
    gfx_set_desc_data(4, 0, 64 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, memory_temp, @pt2_x, @pt_y, false, false);
    convert_gfx(1, 0, memory_temp, @pt2_x[8], @pt_y, false, false);
  end;

  procedure convert_tiles16(n: dword);
  begin
    init_gfx(2, 16, 16, n);
    gfx[2].trans[15] := true;
    gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
    convert_gfx(2, 0, memory_temp, @pt2_x, @pt_y, false, false);
  end;

  procedure convert_tiles32(n: dword);
  begin
    init_gfx(3, 32, 32, n);
    gfx[3].trans[15] := true;
    gfx_set_desc_data(4, 0, 512 * 8, 0, 1, 2, 3);
    convert_gfx(3, 0, memory_temp, @pt2_x, @pt2_y, false, false);
  end;

begin
  machine_calls.general_loop := cps1_loop;
  machine_calls.close := close_cps1;
  machine_calls.reset := reset_cps1;
  machine_calls.fps_max := 59.61;
  start_cps1 := false;
  // 8x8
  screen_init(1, 448, 248, true, false);
  screen_mod_scroll(1, 448, 448, 511, 248, 248, 255);
  // 16x16
  screen_init(2, 464, 256, true, false);
  screen_mod_scroll(2, 464, 464, 511, 256, 256, 255);
  for f := 3 to 4 do
  begin
    screen_init(f, 480, 320, true, false);
    screen_mod_scroll(f, 480, 448, 511, 320, 288, 511);
  end;
  screen_init(5, 512, 512, false, true);
  start_video(384, 224);
  getmem(memory_temp, $600000);
  case main_vars.machine_type of
    103 .. 111:
      begin
        start_audio(false);
        if (main_vars.machine_type = 111) then
          m68000_0 := cpu_m68000.create(12000000, 262)
        else
          m68000_0 := cpu_m68000.create(10000000, 262);
        m68000_0.change_ram16_calls(cps1_getword, cps1_putword);
        // Sound CPU
        z80_0 := cpu_z80.create(3579545, 262);
        z80_0.change_ram_calls(cps1_snd_getbyte, cps1_snd_putbyte);
        z80_0.init_sound(cps1_sound_update);
        // Sound chips
        ym2151_0 := ym2151_chip.create(3579545);
        ym2151_0.change_irq_func(cps1_ym2151_snd_irq);
        oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH, 0.50);
      end;
    112, 113:
      begin // Qsound
        start_audio(true);
        m68000_0 := cpu_m68000.create(12000000, 262);
        m68000_0.change_ram16_calls(cps1_qsnd_getword, cps1_qsnd_putword);
        // Sound CPU
        z80_0 := cpu_z80.create(8000000, 262);
        z80_0.change_ram_calls(cps1_qz80_getbyte, cps1_qz80_putbyte);
        z80_0.init_sound(qsound_sound_update);
        // Sound Chip
        qsound_init($200000);
        timers.init(z80_0.numero_cpu, 8000000 / 250, cps1_qsnd_int, nil, true);
      end;
  end;
  case main_vars.machine_type of
    103:
      begin
        nbank := 0;
        cps_b := 0;
        // cargar roms
        if not(roms_load16b(memory_temp, ghouls_rom1)) then
          exit;
        if not(roms_load(memory_temp, ghouls_rom2)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, ghouls_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, ghouls_gfx1)) then
          exit;
        if not(roms_load64b_b(memory_temp, ghouls_gfx2)) then
          exit;
        cps1_gfx_decode(memory_temp, $300000);
        // Chars
        convert_chars($C000);
        // Tiles 16x16
        convert_tiles16($6000);
        // Tiles 32x32
        convert_tiles32($1800);
        dswa := $FF;
        dswb := $FF;
        dswc := $FF;
      end;
    104:
      begin
        nbank := 1;
        cps_b := 1;
        // cargar roms
        if not(roms_load16b(memory_temp, ffight_rom1)) then
          exit;
        if not(roms_load_swap_word(memory_temp, ffight_rom2)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, ffight_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, ffight_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, ffight_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $200000);
        // Chars
        convert_chars($8000);
        // Tiles 16x16
        convert_tiles16($4000);
        // Tiles 32x32
        convert_tiles32($1000);
        dswa := $FF;
        dswb := $F4;
        dswc := $9F;
      end;
    105:
      begin
        nbank := 2;
        cps_b := 2;
        // cargar roms
        if not(roms_load16b(memory_temp, kod_rom1)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, kod_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, kod_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, kod_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $FC;
        dswc := $9F;
      end;
    106:
      begin
        nbank := 3;
        cps_b := 3;
        // cargar roms
        if not(roms_load16b(memory_temp, sf2_rom1)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, sf2_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, sf2_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, sf2_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $600000);
        // Chars
        convert_chars($18000);
        // Tiles 16x16
        convert_tiles16($C000);
        // Tiles 32x32
        convert_tiles32($3000);
        dswa := $FF;
        dswb := $FC;
        dswc := $9F;
      end;
    107:
      begin // Strider
        nbank := 4;
        cps_b := 0;
        // cargar roms
        if not(roms_load16b(memory_temp, strider_rom1)) then
          exit;
        if not(roms_load_swap_word(memory_temp, strider_rom2)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, strider_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, strider_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, strider_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $BF;
        dswc := $FF;
      end;
    108:
      begin // 3 Wonders
        nbank := 5;
        cps_b := 4;
        // cargar roms
        if not(roms_load16b(memory_temp, wonder3_rom1)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, wonder3_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, wonder3_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, wonder3_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $9A;
        dswc := $99;
      end;
    109:
      begin // Captain Commando
        nbank := 6;
        cps_b := 5;
        // cargar roms
        if not(roms_load_swap_word(memory_temp, ccommando_rom1)) then
          exit;
        if not(roms_load16b(memory_temp, ccommando_rom2)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, ccommando_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, ccommando_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, ccommando_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $F4;
        dswc := $9F;
      end;
    110:
      begin // Knights of the Round
        nbank := 7;
        cps_b := 6;
        // cargar roms
        if not(roms_load_swap_word(memory_temp, knights_rom1)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, knights_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, knights_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, knights_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $FC;
        dswc := $9F;
      end;
    111:
      begin // SF II' CE
        nbank := 8;
        cps_b := 7;
        // cargar roms
        if not(roms_load_swap_word(memory_temp, sf2ce_rom1)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, sf2ce_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        copymemory(@snd_rom[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@snd_rom[1, 0], ptemp, $4000);
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, sf2ce_oki)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, sf2ce_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $600000);
        // Chars
        convert_chars($18000);
        // Tiles 16x16
        convert_tiles16($C000);
        // Tiles 32x32
        convert_tiles32($3000);
        dswa := $FF;
        dswb := $FC;
        dswc := $9F;
      end;
    112:
      begin // Cadillacs and Dinosaurs
        nbank := 9;
        cps_b := 8;
        // eeprom
        eeprom_0 := eeprom_class.create(7, 8, '0110', '0101', '0111');
        // cargar roms
        if not(roms_load_swap_word(memory_temp, dino_rom1)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, dino_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        for f := 0 to 5 do
        begin
          copymemory(@snd_rom[f, 0], ptemp, $4000);
          inc(ptemp, $4000);
        end;
        kabuki_cps1_decode(@mem_snd, @qsnd_opcode, @qsnd_data, $76543210, $24601357, $4343, $43);
        // Cargar ROMS Qsound
        if not(roms_load(qsound_state.sample_rom, dino_qsound1)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, dino_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $FF;
        dswc := $FF;
      end;
    113:
      begin // The Punisher
        nbank := 10;
        cps_b := 9;
        // eeprom
        eeprom_0 := eeprom_class.create(7, 8, '0110', '0101', '0111');
        // cargar roms
        if not(roms_load16b(memory_temp, punisher_rom1)) then
          exit;
        if not(roms_load_swap_word(memory_temp, punisher_rom2)) then
          exit;
        poner_roms_word;
        // roms sonido y poner en su banco
        if not(roms_load(memory_temp, punisher_sound)) then
          exit;
        ptemp := memory_temp;
        copymemory(@mem_snd, ptemp, $8000);
        inc(ptemp, $8000);
        for f := 0 to 5 do
        begin
          copymemory(@snd_rom[f, 0], ptemp, $4000);
          inc(ptemp, $4000);
        end;
        kabuki_cps1_decode(@mem_snd, @qsnd_opcode, @qsnd_data, $67452103, $75316024, $2222, $22);
        // Cargar ROMS Qsound
        if not(roms_load(qsound_state.sample_rom, punisher_qsound1)) then
          exit;
        // convertir gfx (salen todos de los mismos datos)
        if not(roms_load64b(memory_temp, punisher_gfx1)) then
          exit;
        cps1_gfx_decode(memory_temp, $400000);
        // Chars
        convert_chars($10000);
        // Tiles 16x16
        convert_tiles16($8000);
        // Tiles 32x32
        convert_tiles32($2000);
        dswa := $FF;
        dswb := $FF;
        dswc := $FF;
      end;
  end;
  // final
  freemem(memory_temp);
  reset_cps1;
  start_cps1 := true;
end;

end.
