unit init_games;

interface

uses
  sysutils,
  main_engine,
  rom_engine,
  rom_export,
  language,
  // Computer
  spectrum_48k,
  spectrum_128k,
  spectrum_3,
  amstrad_cpc,
  commodore64,
  // Console
  nes,
  coleco,
  gb,
  sms,
  sg1000,
  sega_gg,
  super_cassette_vision,
  // Arcade
  phoenix_hw,
  bombjack_hw,
  pacman_hw,
  mysteriousstones_hw,
  donkeykong_hw,
  greenberet_hw,
  blacktiger_hw,
  commando_hw,
  gng_hw,
  mikie_hw,
  shaolinsroad_hw,
  yiearkungfu_hw,
  asteroids_hw,
  sonson_hw,
  starforce_hw,
  tecmo_hw,
  system1_hw,
  rallyx_hw,
  pooyan_hw,
  cityconnection_hw,
  burgertime_hw,
  expressraider_hw,
  superbasketball_hw,
  ladybug_hw,
  tehkanworldcup_hw,
  popeye_hw,
  psychic5_hw,
  terracresta_hw,
  m62_hw,
  shootout_hw,
  vigilante_hw,
  jackal_hw,
  bubblebobble_hw,
  galaxian_hw,
  prehistoricisle_hw,
  tigerroad_hw,
  snowbros_hw,
  toki_hw,
  contra_hw,
  mappy_hw,
  rastan_hw,
  legendarywings_hw,
  streetfighter_hw,
  galaga_hw,
  xaindsleena_hw,
  suna8_hw,
  nmk16_hw,
  knucklejoe_hw,
  wardner_hw,
  gaelco_hw,
  exedexes_hw,
  gunsmoke_hw,
  hw_1942,
  jailbreak_hw,
  circuscharlie_hw,
  ironhorse_hw,
  m72_hw,
  breakthru_hw,
  dec8_hw,
  doubledragon_hw,
  mrdo_hw,
  epos_hw,
  slapfight_hw,
  legendkage_hw,
  cabal_hw,
  cps1_hw,
  system16a_hw,
  timepilot84_hw,
  tutankham_hw,
  pang_hw,
  ninjakid2_hw,
  skykid_hw,
  system86_hw,
  rocnrope_hw,
  kyugo_hw,
  thenewzealandstory_hw,
  pacland_hw,
  mariobros_hw,
  solomonkey_hw,
  combatschool_hw,
  heavyunit_hw,
  snk68_hw,
  megasys1_hw,
  timepilot_hw,
  pengo_hw,
  twincobra_hw,
  jrpacman_hw,
  dec0_hw,
  funkyjet_hw,
  superburgertime_hw,
  cavemanninja_hw,
  dietgogo_hw,
  actfancer_hw,
  arabian_hw,
  higemaru_hw,
  bagman_hw,
  chip8_hw,
  zaxxon_hw,
  kangaroo_hw,
  bioniccommando_hw,
  wwfsuperstars_hw,
  rainbowislands_hw,
  volfied_hw,
  operationwolf_hw,
  outrun_hw,
  taitosj_hw,
  vulgus_hw,
  ddragon3_hw,
  blockout_hw,
  foodfight_hw,
  nemesis_hw,
  pirates_hw,
  junofirst_hw,
  gyruss_hw,
  freekick_hw,
  boogiewings_hw,
  pinballaction_hw,
  renegade_hw,
  tmnt_hw,
  gradius3_hw,
  spaceinvaders_hw,
  centipede_hw,
  karnov_hw,
  aliens_hw,
  thunderx_hw,
  simpsons_hw,
  trackandfield_hw,
  hypersports_hw,
  megazone_hw,
  spacefirebird_hw,
  ajax_hw,
  vendetta_hw,
  gauntlet_hw,
  sauro_hw,
  crazyclimber_hw,
  returnofinvaders_hw,
  gnw_510,
  tetris_atari_hw,
  snk_hw,
  atari_system1,
  williams_hw,
  systeme_hw,
  route16_hw,
  badlands_hw,
  galivan_hw,
  lastduel_hw,
  armedf_hw,
  firetrap_hw,
  hw_3x3puzzle,
  hw_1945k3,
  bloodbros_hw,
  baraduke_hw,
  system16b_hw,
  toaplan1_hw,
  karatechamp_hw,
  seta_hw,
  genesis,
  mrdocastle_hw,
  crystalcastles_hw,
  flower_hw,
  superdodgeball_hw,
  mcr_hw,
  arkanoid_hw,
  sidearms_hw,
  speedrumbler_hw,
  chinagate_hw,
  magmax_hw,
  ambush_hw,
  superduck_hw,
  hangon_hw,
  shadow_warriors_hw,
  raiden_hw,
  twins_hw,
  oric_hw,
  missilecommand_hw,
  gaplus_hw,
  pv1000,
  pv2000,
  m63_hw,
  diverboy_hw,
  mugsmashers_hw,
  steelforce_hw,
  bankpanic_hw,
  appoooh_hw,
  hw_88games,
  dooyong_hw,
  blueprint_hw,
  unico_hw,
  kikikaikai_hw,
  lasso_hw,
  finalstarforce_hw,
  wyvernf0_hw;

type
  tgame_desc = record
    name, year: string;
    snd: byte;
    hi: boolean;
    zip: string;
    grid: word;
    company: string;
    rom: ptipo_roms;
    samples: ptsample_file;
    tipo: word;
  end;

const
  // Tipos diferentes
  ARCADE = 1;
  COMPUTER = 2;
  GNW = 4;
  CONSOLE = 8;
  SPORT = $10;
  RUN_GUN = $20;
  SHOT = $40;
  MAZE = $80;
  FIGHT = $100;
  DRIVE = $200;
  SOUND_TIPO: array [0 .. 4] of string = ('NO', 'YES', 'SAMPLES', 'YES+SAMPLES', 'PARTIAL');
  GAMES_CONT = 425;
  GAMES_DESC: array [1 .. GAMES_CONT] of tgame_desc = (
    // Computers
    (name: 'Spectrum 48K'; year: '1982'; snd: 1; hi: false; zip: 'spectrum'; grid: 0; company: 'Sinclair'; rom: @spectrum; tipo: COMPUTER), (name: 'Spectrum 128K'; year: '1986'; snd: 1; hi: false; zip: 'spec128'; grid: 1; company: 'Sinclair'; rom: @spec128; tipo: COMPUTER),
    (name: 'Spectrum +3'; year: '1987'; snd: 1; hi: false; zip: 'plus3'; grid: 2; company: 'Amstrad'; rom: @plus3; tipo: COMPUTER), (name: 'Spectrum +2A'; year: '1987'; snd: 1; hi: false; zip: 'plus3'; grid: 3; company: 'Amstrad'; rom: @plus3; tipo: COMPUTER),
    (name: 'Spectrum +2'; year: '1986'; snd: 1; hi: false; zip: 'plus2'; grid: 4; company: 'Amstrad'; rom: @spec_plus2; tipo: COMPUTER), (name: 'Spectrum 16K'; year: '1982'; snd: 1; hi: false; zip: 'spectrum'; grid: 5; company: 'Sinclair'; rom: @spectrum; tipo: COMPUTER),
    (name: 'Amstrad CPC 464'; year: '1984'; snd: 1; hi: false; zip: 'cpc464'; grid: 7; company: 'Amstrad'; rom: @cpc464; tipo: COMPUTER), (name: 'Amstrad CPC 664'; year: '1984'; snd: 1; hi: false; zip: 'cpc664'; grid: 8; company: 'Amstrad'; rom: @cpc664; tipo: COMPUTER),
    (name: 'Amstrad CPC 6128'; year: '1985'; snd: 1; hi: false; zip: 'cpc6128'; grid: 9; company: 'Amstrad'; rom: @cpc6128; tipo: COMPUTER), (name: 'Commodore 64'; year: '1982'; snd: 1; hi: false; zip: 'c64'; grid: 3000; company: 'Commodore'; rom: @c64; tipo: COMPUTER),
    (name: 'Oric Atmos'; year: '1984'; snd: 1; hi: false; zip: 'orica'; grid: 3001; company: 'Tangerine'; rom: @orica; tipo: COMPUTER), (name: 'Oric 1'; year: '1983'; snd: 1; hi: false; zip: 'oric1'; grid: 3002; company: 'Tangerine'; rom: @oric1; tipo: COMPUTER),
    // Arcade
    (name: 'Pacman'; year: '1980'; snd: 1; hi: false; zip: 'pacman'; grid: 10; company: 'Namco'; rom: @pacman; tipo: ARCADE or MAZE), (name: 'Phoenix'; year: '1980'; snd: 1; hi: false; zip: 'phoenix'; grid: 11; company: 'Amstar Electronics'; rom: @phoenix; tipo: ARCADE or SHOT),
    (name: 'Mysterious Stones'; year: '1984'; snd: 1; hi: false; zip: 'mystston'; grid: 12; company: 'Technos'; rom: @mystston; tipo: ARCADE or RUN_GUN), (name: 'Bomb Jack'; year: '1984'; snd: 1; hi: false; zip: 'bombjack'; grid: 13; company: 'Tehkan'; rom: @bombjack;
    tipo: ARCADE or MAZE), (name: 'Frogger'; year: '1981'; snd: 1; hi: true; zip: 'frogger'; grid: 14; company: 'Konami'; rom: @frogger; tipo: ARCADE or MAZE), (name: 'Donkey Kong'; year: '1981'; snd: 2; hi: false; zip: 'dkong'; grid: 15; company: 'Nintendo'; rom: @dkong;
    samples: @dkong_samples; tipo: ARCADE or MAZE), (name: 'Black Tiger'; year: '1986'; snd: 1; hi: true; zip: 'blktiger'; grid: 16; company: 'Capcom'; rom: @blktiger; tipo: ARCADE or RUN_GUN), (name: 'Green Beret'; year: '1985'; snd: 1; hi: true; zip: 'gberet'; grid: 17;
    company: 'Konami'; rom: @gberet; tipo: ARCADE or RUN_GUN), (name: 'Commando'; year: '1985'; snd: 1; hi: false; zip: 'commando'; grid: 18; company: 'Capcom'; rom: @commando; tipo: ARCADE or RUN_GUN or SHOT), (name: 'Ghosts''n Goblins'; year: '1985'; snd: 1; hi: false;
    zip: 'gng'; grid: 19; company: 'Capcom'; rom: @gng; tipo: ARCADE or RUN_GUN), (name: 'Mikie'; year: '1985'; snd: 1; hi: false; zip: 'mikie'; grid: 20; company: 'Konami'; rom: @mikie; tipo: ARCADE or MAZE), (name: 'Shaolin''s Road'; year: '1985'; snd: 1; hi: false;
    zip: 'shaolins'; grid: 21; company: 'Konami'; rom: @shaolins; tipo: ARCADE or MAZE), (name: 'Yie Ar Kung-Fu'; year: '1985'; snd: 1; hi: false; zip: 'yiear'; grid: 22; company: 'Konami'; rom: @yiear; tipo: ARCADE or FIGHT), (name: 'Asteroids'; year: '1979'; snd: 2; hi: false;
    zip: 'asteroid'; grid: 23; company: 'Atari'; rom: @asteroid; samples: @asteroid_samples; tipo: ARCADE or SHOT), (name: 'Son Son'; year: '1984'; snd: 1; hi: false; zip: 'sonson'; grid: 24; company: 'Capcom'; rom: @sonson; tipo: ARCADE or RUN_GUN), (name: 'Star Force';
    year: '1984'; snd: 1; hi: false; zip: 'starforc'; grid: 25; company: 'Tehkan'; rom: @starforc; tipo: ARCADE or SHOT), (name: 'Rygar'; year: '1986'; snd: 1; hi: false; zip: 'rygar'; grid: 26; company: 'Tecmo'; rom: @rygar; tipo: ARCADE or RUN_GUN), (name: 'Pitfall II';
    year: '1984'; snd: 1; hi: false; zip: 'pitfall2'; grid: 27; company: 'Sega'; rom: @pitfall2; tipo: ARCADE or RUN_GUN), (name: 'Pooyan'; year: '1982'; snd: 1; hi: false; zip: 'pooyan'; grid: 28; company: 'Konami'; rom: @pooyan; tipo: ARCADE or SHOT), (name: 'Jungler';
    year: '1981'; snd: 1; hi: false; zip: 'jungler'; grid: 29; company: 'Konami'; rom: @jungler; tipo: ARCADE or MAZE), (name: 'City Connection'; year: '1986'; snd: 1; hi: false; zip: 'citycon'; grid: 30; company: 'Jaleco'; rom: @citycon; tipo: ARCADE or DRIVE),
    (name: 'Burger Time'; year: '1982'; snd: 1; hi: false; zip: 'btime'; grid: 31; company: 'Deco'; rom: @btime; tipo: ARCADE or MAZE), (name: 'Express Raider'; year: '1986'; snd: 1; hi: false; zip: 'exprraid'; grid: 32; company: 'Deco'; rom: @exprraid; tipo: ARCADE or RUN_GUN),
    (name: 'Super Basketball'; year: '1984'; snd: 1; hi: false; zip: 'sbasketb'; grid: 33; company: 'Konami'; rom: @sbasketb; tipo: ARCADE or SPORT), (name: 'Lady Bug'; year: '1981'; snd: 1; hi: false; zip: 'ladybug'; grid: 34; company: 'Universal'; rom: @ladybug;
    tipo: ARCADE or MAZE), (name: 'Teddy Boy Blues'; year: '1985'; snd: 1; hi: false; zip: 'teddybb'; grid: 35; company: 'Sega'; rom: @teddybb; tipo: ARCADE or RUN_GUN), (name: 'Wonder Boy'; year: '1986'; snd: 1; hi: false; zip: 'wboy'; grid: 36; company: 'Sega'; rom: @wboy;
    tipo: ARCADE or RUN_GUN), (name: 'Wonder Boy in Monster Land'; year: '1987'; snd: 1; hi: false; zip: 'wbml'; grid: 37; company: 'Sega'; rom: @wbml; tipo: ARCADE or RUN_GUN), (name: 'Tehkan World Cup'; year: '1985'; snd: 1; hi: false; zip: 'tehkanwc'; grid: 38;
    company: 'Tehkan'; rom: @tehkanwc; tipo: ARCADE or SPORT), (name: 'Popeye'; year: '1982'; snd: 1; hi: false; zip: 'popeye'; grid: 39; company: 'Nintendo'; rom: @popeye; tipo: ARCADE or MAZE), (name: 'Psychic 5'; year: '1987'; snd: 1; hi: false; zip: 'psychic5'; grid: 40;
    company: 'Jaleco'; rom: @psychic5; tipo: ARCADE or RUN_GUN), (name: 'Terra Cresta'; year: '1985'; snd: 1; hi: false; zip: 'terracre'; grid: 41; company: 'Nichibutsu'; rom: @terracre; tipo: ARCADE or SHOT), (name: 'Kung-Fu Master'; year: '1987'; snd: 1; hi: false;
    zip: 'kungfum'; grid: 42; company: 'Irem'; rom: @kungfum; tipo: ARCADE or FIGHT), (name: 'Shoot Out'; year: '1985'; snd: 1; hi: false; zip: 'shootout'; grid: 43; company: 'Data East'; rom: @shootout; tipo: ARCADE or SHOT), (name: 'Vigilante'; year: '1988'; snd: 1; hi: false;
    zip: 'vigilant'; grid: 44; company: 'Irem'; rom: @vigilant; tipo: ARCADE or FIGHT), (name: 'Jackal'; year: '1986'; snd: 1; hi: false; zip: 'jackal'; grid: 45; company: 'Konami'; rom: @jackal; tipo: ARCADE or RUN_GUN), (name: 'Bubble Bobble'; year: '1986'; snd: 1; hi: false;
    zip: 'bublbobl'; grid: 46; company: 'Taito'; rom: @bublbobl; tipo: ARCADE or MAZE), (name: 'Galaxian'; year: '1979'; snd: 4; hi: false; zip: 'galaxian'; grid: 47; company: 'Namco'; rom: @galaxian; samples: @galaxian_samples; tipo: ARCADE or SHOT), (name: 'Jump Bug';
    year: '1981'; snd: 1; hi: false; zip: 'jumpbug'; grid: 48; company: 'Rock-Ola'; rom: @jumpbug; tipo: ARCADE or RUN_GUN), (name: 'Moon Cresta'; year: '1980'; snd: 4; hi: false; zip: 'mooncrst'; grid: 49; company: 'Nichibutsu'; rom: @mooncrst; samples: @mooncrst_samples;
    tipo: ARCADE or SHOT), (name: 'Rally X'; year: '1980'; snd: 3; hi: false; zip: 'rallyx'; grid: 50; company: 'Namco'; rom: @rallyx; samples: @rallyx_samples; tipo: ARCADE or DRIVE or MAZE), (name: 'Prehistoric Isle in 1930'; year: '1989'; snd: 1; hi: false; zip: 'prehisle';
    grid: 51; company: 'SNK'; rom: @prehisle; tipo: ARCADE or SHOT), (name: 'Tiger Road'; year: '1987'; snd: 1; hi: false; zip: 'tigeroad'; grid: 52; company: 'Capcom'; rom: @tigeroad; tipo: ARCADE or RUN_GUN), (name: 'F1 Dream'; year: '1988'; snd: 1; hi: false; zip: 'f1dream';
    grid: 53; company: 'Capcom'; rom: @f1dream; tipo: ARCADE or SPORT or DRIVE), (name: 'Snowbros'; year: '1990'; snd: 1; hi: false; zip: 'snowbros'; grid: 54; company: 'Toaplan'; rom: @snowbros; tipo: ARCADE or MAZE), (name: 'Toki'; year: '1989'; snd: 1; hi: false; zip: 'toki';
    grid: 55; company: 'TAD'; rom: @toki; tipo: ARCADE or RUN_GUN), (name: 'Contra'; year: '1987'; snd: 1; hi: false; zip: 'contra'; grid: 56; company: 'Konami'; rom: @contra; tipo: ARCADE or RUN_GUN), (name: 'Mappy'; year: '1983'; snd: 1; hi: false; zip: 'mappy'; grid: 57;
    company: 'Namco'; rom: @mappy; tipo: ARCADE or MAZE), (name: 'Rastan'; year: '1987'; snd: 1; hi: false; zip: 'rastan'; grid: 58; company: 'Taito'; rom: @rastan; tipo: ARCADE or RUN_GUN), (name: 'Legendary Wings'; year: '1986'; snd: 1; hi: false; zip: 'lwings'; grid: 59;
    company: 'Capcom'; rom: @lwings_roms; tipo: ARCADE or RUN_GUN or SHOT), (name: 'Section Z'; year: '1985'; snd: 1; hi: false; zip: 'sectionz'; grid: 60; company: 'Capcom'; rom: @sectionz_roms; tipo: ARCADE or RUN_GUN or SHOT), (name: 'Trojan'; year: '1986'; snd: 1; hi: false;
    zip: 'trojan'; grid: 61; company: 'Capcom'; rom: @trojan_roms; tipo: ARCADE or RUN_GUN), (name: 'Street Fighter'; year: '1987'; snd: 1; hi: false; zip: 'sf'; grid: 62; company: 'Capcom'; rom: @sfighter; tipo: ARCADE or FIGHT), (name: 'DigDug II'; year: '1985'; snd: 1;
    hi: false; zip: 'digdug2'; grid: 63; company: 'Namco'; rom: @digdug2; tipo: ARCADE or MAZE), (name: 'Super Pacman'; year: '1985'; snd: 1; hi: false; zip: 'superpac'; grid: 64; company: 'Namco'; rom: @spacman; tipo: ARCADE or MAZE), (name: 'Galaga'; year: '1981'; snd: 3;
    hi: false; zip: 'galaga'; grid: 65; company: 'Namco'; rom: @galaga; samples: @galaga_samples; tipo: ARCADE or SHOT), (name: 'Xain''d Sleena'; year: '1986'; snd: 1; hi: false; zip: 'xsleena'; grid: 66; company: 'Technos'; rom: @xsleena; tipo: ARCADE or RUN_GUN),
    (name: 'Hard Head'; year: '1988'; snd: 1; hi: false; zip: 'hardhead'; grid: 67; company: 'Suna'; rom: @hardhead; tipo: ARCADE or RUN_GUN), (name: 'Hard Head 2'; year: '1989'; snd: 1; hi: false; zip: 'hardhea2'; grid: 68; company: 'Suna'; rom: @hardhea2;
    tipo: ARCADE or RUN_GUN), (name: 'Saboten Bombers'; year: '1992'; snd: 1; hi: false; zip: 'sabotenb'; grid: 69; company: 'NMK'; rom: @sabotenb; tipo: ARCADE or MAZE), (name: 'New Rally X'; year: '1981'; snd: 3; hi: false; zip: 'nrallyx'; grid: 70; company: 'Namco';
    rom: @nrallyx; samples: @rallyx_samples; tipo: ARCADE or DRIVE or MAZE), (name: 'Bomb Jack Twin'; year: '1993'; snd: 1; hi: false; zip: 'bjtwin'; grid: 71; company: 'NMK'; rom: @bjtwin; tipo: ARCADE or MAZE), (name: 'Spelunker'; year: '1985'; snd: 1; hi: false;
    zip: 'spelunkr'; grid: 72; company: 'Broderbound'; rom: @spelunkr; tipo: ARCADE or MAZE), (name: 'Spelunker II'; year: '1986'; snd: 1; hi: false; zip: 'spelunk2'; grid: 73; company: 'Broderbound'; rom: @spelunk2; tipo: ARCADE or MAZE), (name: 'Lode Runner'; year: '1984';
    snd: 1; hi: false; zip: 'ldrun'; grid: 74; company: 'Irem'; rom: @ldrun; tipo: ARCADE or MAZE), (name: 'Lode Runner II'; year: '1984'; snd: 1; hi: false; zip: 'ldrun2'; grid: 75; company: 'Irem'; rom: @ldrun2; tipo: ARCADE or MAZE), (name: 'Knuckle Joe'; year: '1985'; snd: 1;
    hi: false; zip: 'kncljoe'; grid: 76; company: 'Taito'; rom: @kncljoe; tipo: ARCADE or FIGHT), (name: 'Wardner'; year: '1987'; snd: 1; hi: false; zip: 'wardner'; grid: 77; company: 'Taito'; rom: @wardner; tipo: ARCADE or RUN_GUN), (name: 'Big Karnak'; year: '1991'; snd: 1;
    hi: false; zip: 'bigkarnk'; grid: 78; company: 'Gaelco'; rom: @bigkarnk; tipo: ARCADE or RUN_GUN), (name: 'Exed-Exes'; year: '1985'; snd: 1; hi: false; zip: 'exedexes'; grid: 79; company: 'Capcom'; rom: @exedexes; tipo: ARCADE or SHOT), (name: 'Gun.Smoke'; year: '1985';
    snd: 1; hi: false; zip: 'gunsmoke'; grid: 80; company: 'Capcom'; rom: @gunsmoke; tipo: ARCADE or RUN_GUN or SHOT), (name: '1942'; year: '1984'; snd: 1; hi: false; zip: '1942'; grid: 81; company: 'Capcom'; rom: @hw1942; tipo: ARCADE or SHOT), (name: '1943'; year: '1987';
    snd: 1; hi: false; zip: '1943'; grid: 82; company: 'Capcom'; rom: @hw1943; tipo: ARCADE or SHOT), (name: '1943 Kai'; year: '1987'; snd: 1; hi: false; zip: '1943kai'; grid: 83; company: 'Capcom'; rom: @hw1943kai; tipo: ARCADE or SHOT), (name: 'Jail Break'; year: '1986';
    snd: 1; hi: false; zip: 'jailbrek'; grid: 84; company: 'Konami'; rom: @jailbrek; tipo: ARCADE or RUN_GUN), (name: 'Circus Charlie'; year: '1984'; snd: 1; hi: false; zip: 'circusc'; grid: 85; company: 'Konami'; rom: @circusc; tipo: ARCADE or RUN_GUN), (name: 'Iron Horse';
    year: '1986'; snd: 1; hi: false; zip: 'ironhors'; grid: 86; company: 'Konami'; rom: @ironhors; tipo: ARCADE or RUN_GUN), (name: 'R-Type'; year: '1987'; snd: 1; hi: false; zip: 'rtype'; grid: 87; company: 'Irem'; rom: @rtype; tipo: ARCADE or SHOT), (name: 'MS. Pac-man';
    year: '1981'; snd: 1; hi: false; zip: 'mspacman'; grid: 88; company: 'Namco'; rom: @mspacman; tipo: ARCADE or MAZE), (name: 'Break Thru'; year: '1986'; snd: 1; hi: false; zip: 'brkthru'; grid: 89; company: 'Data East'; rom: @brkthru; tipo: ARCADE or RUN_GUN or DRIVE),
    (name: 'Darwin 4078'; year: '1986'; snd: 1; hi: false; zip: 'darwin'; grid: 90; company: 'Data East'; rom: @darwin; tipo: ARCADE or SHOT), (name: 'Super Real Darwin'; year: '1987'; snd: 1; hi: false; zip: 'srdarwin'; grid: 91; company: 'Data East'; rom: @srdarwin_roms;
    tipo: ARCADE or SHOT), (name: 'Double Dragon'; year: '1987'; snd: 1; hi: false; zip: 'ddragon'; grid: 92; company: 'Taito'; rom: @ddragon_roms; tipo: ARCADE or FIGHT), (name: 'Mr. Do!'; year: '1982'; snd: 1; hi: false; zip: 'mrdo'; grid: 93; company: 'Universal';
    rom: @mrdo_roms; tipo: ARCADE or MAZE), (name: 'The Glob'; year: '1983'; snd: 1; hi: false; zip: 'theglob'; grid: 94; company: 'Epos'; rom: @theglob_roms; tipo: ARCADE or MAZE), (name: 'Super Glob'; year: '1983'; snd: 1; hi: false; zip: 'suprglob'; grid: 95; company: 'Epos';
    rom: @suprglob_roms; tipo: ARCADE or MAZE), (name: 'Double Dragon II - The Revenge'; year: '1988'; snd: 1; hi: false; zip: 'ddragon2'; grid: 96; company: 'Technos'; rom: @ddragon2_roms; tipo: ARCADE or FIGHT), (name: 'Silk Worm'; year: '1988'; snd: 1; hi: false;
    zip: 'silkworm'; grid: 97; company: 'Tecmo'; rom: @silkworm; tipo: ARCADE or RUN_GUN or SHOT or DRIVE), (name: 'Tiger Heli'; year: '1985'; snd: 1; hi: false; zip: 'tigerh'; grid: 98; company: 'Taito'; rom: @tigerh_roms; tipo: ARCADE or SHOT), (name: 'Slap Fight';
    year: '1986'; snd: 1; hi: false; zip: 'slapfigh'; grid: 99; company: 'Taito'; rom: @slapfigh_roms; tipo: ARCADE or SHOT), (name: 'The Legend of Kage'; year: '1984'; snd: 1; hi: false; zip: 'lkage'; grid: 100; company: 'Taito'; rom: @lkage; tipo: ARCADE or RUN_GUN),
    (name: 'Thunder Hoop'; year: '1992'; snd: 1; hi: false; zip: 'thoop'; grid: 101; company: 'Gaelco'; rom: @thoop; tipo: ARCADE or RUN_GUN), (name: 'Cabal'; year: '1988'; snd: 1; hi: false; zip: 'cabal'; grid: 102; company: 'TAD'; rom: @cabal; tipo: ARCADE or RUN_GUN),
    (name: 'Ghouls''n Ghosts'; year: '1988'; snd: 1; hi: false; zip: 'ghouls'; grid: 103; company: 'Capcom'; rom: @ghouls; tipo: ARCADE or RUN_GUN), (name: 'Final Fight'; year: '1989'; snd: 1; hi: false; zip: 'ffight'; grid: 104; company: 'Capcom'; rom: @ffight;
    tipo: ARCADE or FIGHT), (name: 'The King of Dragons'; year: '1991'; snd: 1; hi: false; zip: 'kod'; grid: 105; company: 'Capcom'; rom: @kod; tipo: ARCADE or FIGHT), (name: 'Street Fighter II - The World Warrior'; year: '1991'; snd: 1; hi: false; zip: 'sf2'; grid: 106;
    company: 'Capcom'; rom: @sf2; tipo: ARCADE or FIGHT), (name: 'Strider'; year: '1989'; snd: 1; hi: false; zip: 'strider'; grid: 107; company: 'Capcom'; rom: @strider; tipo: ARCADE or RUN_GUN), (name: 'Three Wonders'; year: '1991'; snd: 1; hi: false; zip: '3wonders'; grid: 108;
    company: 'Capcom'; rom: @wonder3; tipo: ARCADE or RUN_GUN), (name: 'Captain Commando'; year: '1991'; snd: 1; hi: false; zip: 'captcomm'; grid: 109; company: 'Capcom'; rom: @captcomm; tipo: ARCADE or RUN_GUN), (name: 'Knights of the Round'; year: '1991'; snd: 1; hi: false;
    zip: 'knights'; grid: 110; company: 'Capcom'; rom: @knights; tipo: ARCADE or RUN_GUN), (name: 'Street Fighter II'': Champion Edition'; year: '1992'; snd: 1; hi: false; zip: 'sf2ce'; grid: 111; company: 'Capcom'; rom: @sf2ce; tipo: ARCADE or FIGHT),
    (name: 'Cadillacs and Dinosaurs'; year: '1992'; snd: 1; hi: false; zip: 'dino'; grid: 112; company: 'Capcom'; rom: @dino; tipo: ARCADE or FIGHT), (name: 'The Punisher'; year: '1993'; snd: 1; hi: false; zip: 'punisher'; grid: 113; company: 'Capcom'; rom: @punisher;
    tipo: ARCADE or FIGHT), (name: 'Shinobi'; year: '1987'; snd: 1; hi: false; zip: 'shinobi'; grid: 114; company: 'Sega'; rom: @shinobi; tipo: ARCADE or RUN_GUN), (name: 'Alex Kidd'; year: '1986'; snd: 1; hi: false; zip: 'alexkidd'; grid: 115; company: 'Sega'; rom: @alexkidd;
    tipo: ARCADE or RUN_GUN), (name: 'Fantasy Zone'; year: '1986'; snd: 1; hi: false; zip: 'fantzone'; grid: 116; company: 'Sega'; rom: @fantzone; tipo: ARCADE or RUN_GUN), (name: 'Time Pilot ''84'; year: '1984'; snd: 1; hi: false; zip: 'tp84'; grid: 117; company: 'Konami';
    rom: @tp84; tipo: ARCADE or SHOT), (name: 'Tutankham'; year: '1982'; snd: 1; hi: false; zip: 'tutankhm'; grid: 118; company: 'Konami'; rom: @tutankhm; tipo: ARCADE or MAZE), (name: 'Pang'; year: '1989'; snd: 1; hi: false; zip: 'pang'; grid: 119; company: 'Capcom'; rom: @pang;
    tipo: ARCADE or SHOT), (name: 'Ninja Kid II'; year: '1987'; snd: 1; hi: false; zip: 'ninjakd2'; grid: 120; company: 'UPL'; rom: @ninjakd2; tipo: ARCADE or RUN_GUN), (name: 'Ark Area'; year: '1988'; snd: 1; hi: false; zip: 'arkarea'; grid: 121; company: 'UPL'; rom: @arkarea;
    tipo: ARCADE or SHOT), (name: 'Mutant Night'; year: '1987'; snd: 1; hi: false; zip: 'mnight'; grid: 122; company: 'UPL'; rom: @mnight; tipo: ARCADE or RUN_GUN), (name: 'Sky Kid'; year: '1985'; snd: 1; hi: false; zip: 'skykid'; grid: 123; company: 'Namco'; rom: @skykid;
    tipo: ARCADE or SHOT), (name: 'Rolling Thunder'; year: '1986'; snd: 1; hi: false; zip: 'rthunder'; grid: 124; company: 'Namco'; rom: @rthunder; tipo: ARCADE or RUN_GUN), (name: 'Hopping Mappy'; year: '1986'; snd: 1; hi: false; zip: 'hopmappy'; grid: 125; company: 'Namco';
    rom: @hopmappy; tipo: ARCADE or MAZE), (name: 'Sky Kid Deluxe'; year: '1986'; snd: 1; hi: false; zip: 'skykiddx'; grid: 126; company: 'Namco'; rom: @skykiddx; tipo: ARCADE or SHOT), (name: 'Roc''n Rope'; year: '1983'; snd: 1; hi: false; zip: 'rocnrope'; grid: 127;
    company: 'Konami'; rom: @rocnrope; tipo: ARCADE or MAZE), (name: 'Repulse'; year: '1985'; snd: 1; hi: false; zip: 'repulse'; grid: 128; company: 'Crux/Sega'; rom: @repulse; tipo: ARCADE or SHOT), (name: 'The NewZealand Story'; year: '1988'; snd: 1; hi: false; zip: 'tnzs';
    grid: 129; company: 'Taito'; rom: @tnzs; tipo: ARCADE or RUN_GUN), (name: 'Insector X'; year: '1989'; snd: 1; hi: false; zip: 'insectx'; grid: 130; company: 'Taito'; rom: @insectx; tipo: ARCADE or SHOT), (name: 'Pacland'; year: '1984'; snd: 1; hi: false; zip: 'pacland';
    grid: 131; company: 'Namco'; rom: @pacland; tipo: ARCADE or RUN_GUN), (name: 'Mario Bros.'; year: '1983'; snd: 2; hi: false; zip: 'mario'; grid: 132; company: 'Nintendo'; rom: @mario; samples: @mario_samples; tipo: ARCADE or MAZE), (name: 'Solomon''s Key'; year: '1986';
    snd: 1; hi: false; zip: 'solomon'; grid: 133; company: 'Tecmo'; rom: @solomon; tipo: ARCADE or MAZE), (name: 'Combat School'; year: '1988'; snd: 1; hi: false; zip: 'combatsc'; grid: 134; company: 'Konami'; rom: @combatsc; tipo: ARCADE or RUN_GUN), (name: 'Heavy Unit';
    year: '1988'; snd: 1; hi: false; zip: 'hvyunit'; grid: 135; company: 'Taito'; rom: @hvyunit; tipo: ARCADE or SHOT), (name: 'P.O.W. - Prisoners of War'; year: '1988'; snd: 1; hi: false; zip: 'pow'; grid: 136; company: 'SNK'; rom: @pow; tipo: ARCADE or RUN_GUN),
    (name: 'Street Smart'; year: '1988'; snd: 1; hi: false; zip: 'streetsm'; grid: 137; company: 'SNK'; rom: @streetsm; tipo: ARCADE or FIGHT), (name: 'P47 - Phantom Fighter'; year: '1989'; snd: 1; hi: false; zip: 'p47'; grid: 138; company: 'Jaleco'; rom: @p47;
    tipo: ARCADE or SHOT), (name: 'Rod-Land'; year: '1990'; snd: 1; hi: false; zip: 'rodland'; grid: 139; company: 'Jaleco'; rom: @rodland; tipo: ARCADE or MAZE), (name: 'Saint Dragon'; year: '1989'; snd: 1; hi: false; zip: 'stdragon'; grid: 140; company: 'Jaleco';
    rom: @stdragon; tipo: ARCADE or SHOT), (name: 'Time Pilot'; year: '1982'; snd: 1; hi: false; zip: 'timeplt'; grid: 141; company: 'Konami'; rom: @timeplt; tipo: ARCADE or SHOT), (name: 'Pengo'; year: '1982'; snd: 1; hi: false; zip: 'pengo'; grid: 142; company: 'Sega';
    rom: @pengo; tipo: ARCADE or MAZE), (name: 'Scramble'; year: '1981'; snd: 1; hi: false; zip: 'scramble'; grid: 143; company: 'Konami'; rom: @scramble; tipo: ARCADE or SHOT), (name: 'Super Cobra'; year: '1981'; snd: 1; hi: false; zip: 'scobra'; grid: 144; company: 'Konami';
    rom: @scobra; tipo: ARCADE or SHOT), (name: 'Amidar'; year: '1982'; snd: 1; hi: false; zip: 'amidar'; grid: 145; company: 'Konami'; rom: @amidar_roms; tipo: ARCADE or MAZE), (name: 'Twin Cobra'; year: '1987'; snd: 1; hi: false; zip: 'twincobr'; grid: 146; company: 'Taito';
    rom: @twincobr; tipo: ARCADE or SHOT), (name: 'Flying Shark'; year: '1987'; snd: 1; hi: false; zip: 'fshark'; grid: 147; company: 'Taito'; rom: @fshark; tipo: ARCADE or SHOT), (name: 'Jr. Pac-Man'; year: '1983'; snd: 1; hi: false; zip: 'jrpacman'; grid: 148;
    company: 'Bally Midway'; rom: @jrpacman; tipo: ARCADE or MAZE), (name: 'Ikari III - The Rescue'; year: '1989'; snd: 1; hi: false; zip: 'ikari3'; grid: 149; company: 'SNK'; rom: @ikari3; tipo: ARCADE or FIGHT), (name: 'Search and Rescue'; year: '1989'; snd: 1; hi: false;
    zip: 'searchar'; grid: 150; company: 'SNK'; rom: @searchar; tipo: ARCADE or RUN_GUN), (name: 'Choplifter'; year: '1985'; snd: 1; hi: false; zip: 'choplift'; grid: 151; company: 'Sega'; rom: @choplift; tipo: ARCADE or SHOT or RUN_GUN), (name: 'Mister Viking'; year: '1983';
    snd: 1; hi: false; zip: 'mrviking'; grid: 152; company: 'Sega'; rom: @mrviking; tipo: ARCADE or RUN_GUN), (name: 'Sega Ninja'; year: '1985'; snd: 1; hi: false; zip: 'seganinj'; grid: 153; company: 'Sega'; rom: @seganinj; tipo: ARCADE or RUN_GUN), (name: 'Up''n Down';
    year: '1983'; snd: 1; hi: false; zip: 'upndown'; grid: 154; company: 'Sega'; rom: @upndown; tipo: ARCADE or DRIVE or MAZE), (name: 'Flicky'; year: '1984'; snd: 1; hi: false; zip: 'flicky'; grid: 155; company: 'Sega'; rom: @flicky; tipo: ARCADE or MAZE), (name: 'Robocop';
    year: '1988'; snd: 1; hi: false; zip: 'robocop'; grid: 156; company: 'Data East'; rom: @robocop; tipo: ARCADE or RUN_GUN), (name: 'Baddudes vs. DragonNinja'; year: '1988'; snd: 1; hi: false; zip: 'baddudes'; grid: 157; company: 'Data East'; rom: @baddudes;
    tipo: ARCADE or FIGHT or RUN_GUN), (name: 'Hippodrome'; year: '1989'; snd: 1; hi: false; zip: 'hippodrm'; grid: 158; company: 'Data East'; rom: @hippodrm; tipo: ARCADE or FIGHT), (name: 'Tumble Pop'; year: '1991'; snd: 1; hi: false; zip: 'tumblep'; grid: 159;
    company: 'Data East'; rom: @tumblep; tipo: ARCADE or MAZE), (name: 'Funky Jet'; year: '1992'; snd: 1; hi: false; zip: 'funkyjet'; grid: 160; company: 'Mitchell'; rom: @funkyjet; tipo: ARCADE or MAZE), (name: 'Super Burger Time'; year: '1990'; snd: 1; hi: false;
    zip: 'supbtime'; grid: 161; company: 'Data East'; rom: @supbtime; tipo: ARCADE), (name: 'Caveman Ninja'; year: '1991'; snd: 1; hi: false; zip: 'cninja'; grid: 162; company: 'Data East'; rom: @cninja; tipo: ARCADE or RUN_GUN), (name: 'Robocop 2'; year: '1991'; snd: 1;
    hi: false; zip: 'robocop2'; grid: 163; company: 'Data East'; rom: @robocop2; tipo: ARCADE or RUN_GUN), (name: 'Diet Go Go'; year: '1992'; snd: 1; hi: false; zip: 'dietgo'; grid: 164; company: 'Data East'; rom: @dietgo; tipo: ARCADE or MAZE),
    (name: 'Act-Fancer Cybernetick Hyper Weapon'; year: '1989'; snd: 1; hi: false; zip: 'actfancr'; grid: 165; company: 'Data East'; rom: @actfancer; tipo: ARCADE or RUN_GUN), (name: 'Arabian'; year: '1983'; snd: 1; hi: false; zip: 'arabian'; grid: 166;
    company: 'Sun Electronics'; rom: @arabian; tipo: ARCADE or MAZE), (name: 'Dig Dug'; year: '1982'; snd: 1; hi: false; zip: 'digdug'; grid: 167; company: 'Namco'; rom: @digdug; tipo: ARCADE or MAZE), (name: 'Donkey Kong Jr.'; year: '1982'; snd: 2; hi: false; zip: 'dkongjr';
    grid: 168; company: 'Nintendo'; rom: @dkongjr; samples: @dkjr_samples; tipo: ARCADE or MAZE), (name: 'Donkey Kong 3'; year: '1983'; snd: 1; hi: false; zip: 'dkong3'; grid: 169; company: 'Nintendo'; rom: @dkong3; tipo: ARCADE or MAZE), (name: 'Pirate Ship Higemaru';
    year: '1984'; snd: 1; hi: false; zip: 'higemaru'; grid: 170; company: 'Capcom'; rom: @higemaru; tipo: ARCADE or MAZE), (name: 'Bagman'; year: '1982'; snd: 4; hi: false; zip: 'bagman'; grid: 171; company: 'Valadon Automation'; rom: @bagman; tipo: ARCADE or MAZE),
    (name: 'Super Bagman'; year: '1984'; snd: 4; hi: false; zip: 'sbagman'; grid: 172; company: 'Valadon Automation'; rom: @sbagman; tipo: ARCADE or MAZE), (name: 'Squash'; year: '1992'; snd: 1; hi: false; zip: 'squash'; grid: 173; company: 'Gaelco'; rom: @squash;
    tipo: ARCADE or SPORT), (name: 'Biomechanical Toy'; year: '1995'; snd: 1; hi: false; zip: 'biomtoy'; grid: 174; company: 'Gaelco'; rom: @biomtoy; tipo: ARCADE or RUN_GUN), (name: 'Congo Bongo'; year: '1983'; snd: 3; hi: false; zip: 'congo'; grid: 175; company: 'Sega';
    rom: @congo; samples: @congo_samples; tipo: ARCADE or MAZE), (name: 'Kangaroo'; year: '1982'; snd: 1; hi: false; zip: 'kangaroo'; grid: 176; company: 'Sun Electronics'; rom: @kangaroo; tipo: ARCADE or MAZE), (name: 'Bionic Commando'; year: '1987'; snd: 1; hi: false;
    zip: 'bionicc'; grid: 177; company: 'Capcom'; rom: @bionicc; tipo: ARCADE or RUN_GUN), (name: 'WWF Superstar'; year: '1989'; snd: 1; hi: false; zip: 'wwfsstar'; grid: 178; company: 'Technos Japan'; rom: @wwfsstar; tipo: ARCADE or SPORT), (name: 'Rainbow Islands';
    year: '1987'; snd: 1; hi: false; zip: 'rbisland'; grid: 179; company: 'Taito'; rom: @rbisland; tipo: ARCADE or RUN_GUN), (name: 'Rainbow Islands Extra'; year: '1987'; snd: 1; hi: false; zip: 'rbislande'; grid: 180; company: 'Taito'; rom: @rbislande; tipo: ARCADE or RUN_GUN),
    (name: 'Volfied'; year: '1989'; snd: 1; hi: false; zip: 'volfied'; grid: 181; company: 'Taito'; rom: @volfied; tipo: ARCADE or MAZE), (name: 'Operation Wolf'; year: '1987'; snd: 1; hi: false; zip: 'opwolf'; grid: 182; company: 'Taito'; rom: @opwolf;
    tipo: ARCADE or RUN_GUN or SHOT), (name: 'Super Pang'; year: '1990'; snd: 1; hi: false; zip: 'spang'; grid: 183; company: 'Capcom'; rom: @spang; tipo: ARCADE or SHOT), (name: 'Outrun'; year: '1989'; snd: 1; hi: false; zip: 'outrun'; grid: 184; company: 'Sega'; rom: @outrun;
    tipo: ARCADE or DRIVE), (name: 'Elevator Action'; year: '1983'; snd: 1; hi: false; zip: 'elevator'; grid: 185; company: 'Taito'; rom: @elevator; tipo: ARCADE or RUN_GUN), (name: 'Alien Syndrome'; year: '1988'; snd: 1; hi: false; zip: 'aliensyn'; grid: 186; company: 'Sega';
    rom: @aliensyn; tipo: ARCADE or RUN_GUN), (name: 'Wonder Boy III - Monster Lair'; year: '1987'; snd: 1; hi: false; zip: 'wb3'; grid: 187; company: 'Sega'; rom: @wb3; tipo: ARCADE or RUN_GUN), (name: 'Zaxxon'; year: '1982'; snd: 2; hi: false; zip: 'zaxxon'; grid: 188;
    company: 'Sega'; rom: @zaxxon; samples: @zaxxon_samples; tipo: ARCADE or SHOT), (name: 'Jungle King'; year: '1982'; snd: 1; hi: false; zip: 'junglek'; grid: 189; company: 'Taito'; rom: @junglek; tipo: ARCADE or RUN_GUN), (name: 'Hammerin'' Harry'; year: '1990'; snd: 1;
    hi: false; zip: 'hharry'; grid: 190; company: 'Irem'; rom: @hharry; tipo: ARCADE or RUN_GUN), (name: 'R-Type 2'; year: '1989'; snd: 1; hi: false; zip: 'rtype2'; grid: 191; company: 'Irem'; rom: @rtype2; tipo: ARCADE or SHOT), (name: 'The Tower of Druaga'; year: '1984';
    snd: 1; hi: false; zip: 'todruaga'; grid: 192; company: 'Namco'; rom: @todruaga; tipo: ARCADE or MAZE), (name: 'Motos'; year: '1985'; snd: 1; hi: false; zip: 'motos'; grid: 193; company: 'Namco'; rom: @motos; tipo: ARCADE or MAZE), (name: 'Dragon Buster'; year: '1984';
    snd: 1; hi: false; zip: 'drgnbstr'; grid: 194; company: 'Namco'; rom: @drgnbstr; tipo: ARCADE or MAZE or RUN_GUN), (name: 'Vulgus'; year: '1984'; snd: 1; hi: false; zip: 'vulgus'; grid: 195; company: 'Capcom'; rom: @vulgus; tipo: ARCADE or SHOT),
    (name: 'Double Dragon 3 - The Rosetta Stone'; year: '1990'; snd: 1; hi: false; zip: 'ddragon3'; grid: 196; company: 'Technos'; rom: @ddragon3; tipo: ARCADE or FIGHT), (name: 'Block Out'; year: '1990'; snd: 1; hi: false; zip: 'blockout'; grid: 197; company: 'Technos';
    rom: @blockout; tipo: ARCADE), (name: 'Tetris (Sega)'; year: '1988'; snd: 1; hi: false; zip: 'tetris'; grid: 198; company: 'Sega'; rom: @tetris; tipo: ARCADE), (name: 'Food Fight'; year: '1982'; snd: 1; hi: false; zip: 'foodf'; grid: 199; company: 'Atari'; rom: @foodf;
    tipo: ARCADE or MAZE), (name: 'Snap Jack'; year: '1982'; snd: 1; hi: false; zip: 'snapjack'; grid: 200; company: 'Universal'; rom: @snapjack; tipo: ARCADE or RUN_GUN), (name: 'Cosmic Avenger'; year: '1981'; snd: 1; hi: false; zip: 'cavenger'; grid: 201; company: 'Universal';
    rom: @cavenger; tipo: ARCADE or SHOT), (name: 'Pleiads'; year: '1981'; snd: 0; hi: false; zip: 'pleiads'; grid: 202; company: 'Tehkan'; rom: @pleiads; tipo: ARCADE or SHOT), (name: 'Mr. Goemon'; year: '1986'; snd: 1; hi: false; zip: 'mrgoemon'; grid: 203; company: 'Konami';
    rom: @mrgoemon; tipo: ARCADE or RUN_GUN), (name: 'Nemesis'; year: '1985'; snd: 1; hi: false; zip: 'nemesis'; grid: 204; company: 'Konami'; rom: @nemesis; tipo: ARCADE or SHOT), (name: 'Twinbee'; year: '1985'; snd: 1; hi: false; zip: 'twinbee'; grid: 205; company: 'Konami';
    rom: @twinbee; tipo: ARCADE or SHOT), (name: 'Pirates'; year: '1994'; snd: 1; hi: false; zip: 'pirates'; grid: 206; company: 'NIX'; rom: @pirates; tipo: ARCADE or RUN_GUN), (name: 'Genix Family'; year: '1994'; snd: 1; hi: false; zip: 'genix'; grid: 207; company: 'NIX';
    rom: @genix; tipo: ARCADE or RUN_GUN), (name: 'Juno First'; year: '1983'; snd: 1; hi: false; zip: 'junofrst'; grid: 208; company: 'Konami'; rom: @junofrst; tipo: ARCADE or SHOT), (name: 'Gyruss'; year: '1983'; snd: 1; hi: false; zip: 'gyruss'; grid: 209; company: 'Konami';
    rom: @gyruss; tipo: ARCADE or SHOT), (name: 'Boogie Wings'; year: '1992'; snd: 1; hi: false; zip: 'boogwing'; grid: 210; company: 'Data East'; rom: @boogwing; tipo: ARCADE or RUN_GUN), (name: 'Free Kick'; year: '1987'; snd: 1; hi: false; zip: 'freekick'; grid: 211;
    company: 'Nihon System'; rom: @freekick; tipo: ARCADE or SPORT), (name: 'Pinball Action'; year: '1985'; snd: 1; hi: false; zip: 'pbaction'; grid: 212; company: 'Tehkan'; rom: @pbaction; tipo: ARCADE), (name: 'Renegade'; year: '1986'; snd: 1; hi: false; zip: 'renegade';
    grid: 213; company: 'Technos Japan'; rom: @renegade; tipo: ARCADE or FIGHT), (name: 'Teenage Mutant Ninja Turtles'; year: '1989'; snd: 1; hi: false; zip: 'tmnt'; grid: 214; company: 'Konami'; rom: @tmnt; tipo: ARCADE or FIGHT), (name: 'Sunset Riders'; year: '1991'; snd: 1;
    hi: false; zip: 'ssriders'; grid: 215; company: 'Konami'; rom: @ssriders; tipo: ARCADE or RUN_GUN or FIGHT), (name: 'Gradius III'; year: '1991'; snd: 1; hi: false; zip: 'gradius3'; grid: 216; company: 'Konami'; rom: @gradius3; tipo: ARCADE or SHOT), (name: 'Space Invaders';
    year: '1978'; snd: 2; hi: false; zip: 'invaders'; grid: 217; company: 'Taito'; rom: @spaceinv; samples: @spaceinv_samples; tipo: ARCADE or SHOT), (name: 'Centipede'; year: '1980'; snd: 1; hi: false; zip: 'centiped'; grid: 218; company: 'Atari'; rom: @centipede;
    tipo: ARCADE or SHOT), (name: 'Karnov'; year: '1987'; snd: 1; hi: false; zip: 'karnov'; grid: 219; company: 'Data East'; rom: @karnov; tipo: ARCADE or RUN_GUN), (name: 'Chelnov'; year: '1987'; snd: 1; hi: false; zip: 'chelnov'; grid: 220; company: 'Data East'; rom: @chelnov;
    tipo: ARCADE or RUN_GUN), (name: 'Aliens'; year: '1990'; snd: 1; hi: false; zip: 'aliens'; grid: 221; company: 'Konami'; rom: @aliens; tipo: ARCADE or RUN_GUN), (name: 'Super Contra'; year: '1988'; snd: 1; hi: false; zip: 'scontra'; grid: 222; company: 'Konami';
    rom: @scontra; tipo: ARCADE or RUN_GUN), (name: 'Gang Busters'; year: '1988'; snd: 1; hi: false; zip: 'gbusters'; grid: 223; company: 'Konami'; rom: @gbusters; tipo: ARCADE or RUN_GUN), (name: 'Thunder Cross'; year: '1988'; snd: 1; hi: false; zip: 'thunderx'; grid: 224;
    company: 'Konami'; rom: @thunderx; tipo: ARCADE or SHOT), (name: 'The Simpsons'; year: '1991'; snd: 1; hi: false; zip: 'simpsons'; grid: 225; company: 'Konami'; rom: @simpsons; tipo: ARCADE or RUN_GUN or FIGHT), (name: 'Track & Field'; year: '1983'; snd: 1; hi: false;
    zip: 'trackfld'; grid: 226; company: 'Konami'; rom: @trackfield; tipo: ARCADE or SPORT), (name: 'Hyper Sports'; year: '1984'; snd: 1; hi: false; zip: 'hyperspt'; grid: 227; company: 'Konami'; rom: @hypersports; tipo: ARCADE or SPORT), (name: 'Megazone'; year: '1983'; snd: 1;
    hi: false; zip: 'megazone'; grid: 228; company: 'Konami'; rom: @megazone; tipo: ARCADE or SHOT), (name: 'Space Fire Bird'; year: '1980'; snd: 4; hi: false; zip: 'spacefb'; grid: 229; company: 'Nintendo'; rom: @spacefb; samples: @spacefb_samples; tipo: ARCADE or SHOT),
    (name: 'Ajax'; year: '1987'; snd: 1; hi: false; zip: 'ajax'; grid: 230; company: 'Konami'; rom: @ajax; tipo: ARCADE or SHOT), (name: 'Xevious'; year: '1982'; snd: 3; hi: false; zip: 'xevious'; grid: 231; company: 'Namco'; rom: @xevious; samples: @xevious_samples;
    tipo: ARCADE or SHOT), (name: 'The Combatribes'; year: '1990'; snd: 1; hi: false; zip: 'ctribe'; grid: 232; company: 'Technos'; rom: @ctribe; tipo: ARCADE or FIGHT), (name: 'Lunar Lander'; year: '1979'; snd: 0; hi: false; zip: 'llander'; grid: 233; company: 'Atari';
    rom: @llander; tipo: ARCADE), (name: 'Crush Roller'; year: '1981'; snd: 1; hi: false; zip: 'crush'; grid: 234; company: 'Alpha Denshi Co./Kural Samno Electric, Ltd.'; rom: @crush; tipo: ARCADE or MAZE), (name: 'Vendetta'; year: '1991'; snd: 1; hi: false; zip: 'vendetta';
    grid: 235; company: 'Konami'; rom: @vendetta; tipo: ARCADE or FIGHT), (name: 'Gauntlet'; year: '1985'; snd: 1; hi: false; zip: 'gauntlet'; grid: 236; company: 'Atari'; rom: @gauntlet2p; tipo: ARCADE or RUN_GUN), (name: 'Sauro'; year: '1987'; snd: 1; hi: false; zip: 'sauro';
    grid: 237; company: 'Tecfri'; rom: @sauro; tipo: ARCADE or SHOT), (name: 'Crazy Climber'; year: '1980'; snd: 1; hi: false; zip: 'cclimber'; grid: 238; company: 'Nichibutsu'; rom: @cclimber; tipo: ARCADE), (name: 'Return of the Invaders'; year: '1985'; snd: 1; hi: false;
    zip: 'retofinv'; grid: 239; company: 'Taito'; rom: @retofinv; tipo: ARCADE or SHOT), (name: 'Tetris (Atari)'; year: '1988'; snd: 1; hi: false; zip: 'atetris'; grid: 240; company: 'Atari Games'; rom: @tetris_atari; tipo: ARCADE), (name: 'Ikari Warriors'; year: '1986'; snd: 1;
    hi: false; zip: 'ikari'; grid: 241; company: 'SNK'; rom: @ikari; tipo: ARCADE or RUN_GUN), (name: 'Athena'; year: '1986'; snd: 1; hi: false; zip: 'athena'; grid: 242; company: 'SNK'; rom: @athena; tipo: ARCADE or RUN_GUN), (name: 'T.N.K III'; year: '1986'; snd: 1; hi: false;
    zip: 'tnk3'; grid: 243; company: 'SNK'; rom: @tnk3; tipo: ARCADE or RUN_GUN), (name: 'Peter Pack Rat'; year: '1984'; snd: 1; hi: false; zip: 'peterpak'; grid: 244; company: 'Atari'; rom: @peterpak; tipo: ARCADE), (name: 'Gauntlet II'; year: '1986'; snd: 1; hi: false;
    zip: 'gaunt2'; grid: 245; company: 'Atari'; rom: @gaunt2; tipo: ARCADE or RUN_GUN), (name: 'Defender'; year: '1980'; snd: 1; hi: false; zip: 'defender'; grid: 246; company: 'Williams'; rom: @defender; tipo: ARCADE or SHOT), (name: 'Fire Ball'; year: '1992'; snd: 1; hi: false;
    zip: 'fball'; grid: 247; company: 'FM Works'; rom: @fball_roms; tipo: ARCADE or MAZE), (name: 'Mayday'; year: '1980'; snd: 1; hi: false; zip: 'mayday'; grid: 248; company: 'Williams'; rom: @mayday; tipo: ARCADE or SHOT), (name: 'Colony 7'; year: '1981'; snd: 1; hi: false;
    zip: 'colony7'; grid: 249; company: 'Williams'; rom: @colony7; tipo: ARCADE or SHOT), (name: 'Bosconian'; year: '1981'; snd: 3; hi: false; zip: 'bosco'; grid: 250; company: 'Namco'; rom: @bosco; samples: @bosco_samples; tipo: ARCADE or SHOT), (name: 'HangOn Jr.';
    year: '1985'; snd: 1; hi: false; zip: 'hangonjr'; grid: 251; company: 'Sega'; rom: @hangonjr; tipo: ARCADE or SPORT or DRIVE), (name: 'Slap Shooter'; year: '1986'; snd: 1; hi: false; zip: 'slapshtr'; grid: 252; company: 'Sega'; rom: @slapshtr; tipo: ARCADE or SPORT),
    (name: 'Fantasy Zone II: The Tears of Opa-Opa'; year: '1988'; snd: 1; hi: false; zip: 'fantzn2'; grid: 253; company: 'Sega'; rom: @fantzn2; tipo: ARCADE or SHOT), (name: 'Opa Opa'; year: '1987'; snd: 1; hi: false; zip: 'opaopa'; grid: 254; company: 'Sega'; rom: @opaopa;
    tipo: ARCADE or MAZE), (name: 'Tetris (Sega System E)'; year: '1988'; snd: 1; hi: false; zip: 'tetrisse'; grid: 255; company: 'Sega'; rom: @tetrisse; tipo: ARCADE), (name: 'Transformer'; year: '1986'; snd: 1; hi: false; zip: 'transfrm'; grid: 256; company: 'Sega';
    rom: @transfrm; tipo: ARCADE or SHOT), (name: 'Riddle of Pythagoras'; year: '1986'; snd: 1; hi: false; zip: 'ridleofp'; grid: 257; company: 'Sega'; rom: @ridleofp; tipo: ARCADE or MAZE), (name: 'Route 16'; year: '1981'; snd: 1; hi: false; zip: 'route16'; grid: 258;
    company: 'Sun Electronics'; rom: @route16; tipo: ARCADE or DRIVE), (name: 'Speak & Rescue'; year: '1980'; snd: 1; hi: false; zip: 'speakres'; grid: 259; company: 'Sun Electronics'; rom: @speakres; tipo: ARCADE or SHOT), (name: 'Galactic Warriors'; year: '1985'; snd: 1;
    hi: false; zip: 'gwarrior'; grid: 260; company: 'Konami'; rom: @gwarrior; tipo: ARCADE or FIGHT), (name: 'Salamander'; year: '1986'; snd: 1; hi: false; zip: 'salamand'; grid: 261; company: 'Konami'; rom: @salamander; tipo: ARCADE or SHOT), (name: 'Bad Lands'; year: '1989';
    snd: 1; hi: false; zip: 'badlands'; grid: 262; company: 'Atari'; rom: @badlands; tipo: ARCADE or DRIVE), (name: 'Indiana Jones and the Temple of Doom'; year: '1985'; snd: 1; hi: false; zip: 'indytemp'; grid: 263; company: 'Atari'; rom: @indytemp; tipo: ARCADE or RUN_GUN),
    (name: 'Marble Madness'; year: '1984'; snd: 1; hi: false; zip: 'marble'; grid: 264; company: 'Atari'; rom: @marble; tipo: ARCADE or MAZE), (name: 'Soldier Girl Amazon'; year: '1986'; snd: 1; hi: false; zip: 'amazon'; grid: 265; company: 'Nichibutsu'; rom: @amazon;
    tipo: ARCADE or RUN_GUN), (name: 'Cosmo Police Galivan'; year: '1985'; snd: 1; hi: false; zip: 'galivan'; grid: 266; company: 'Nichibutsu'; rom: @galivan; tipo: ARCADE or RUN_GUN), (name: 'Ufo Robo Dangar'; year: '1986'; snd: 1; hi: false; zip: 'dangar'; grid: 267;
    company: 'Nichibutsu'; rom: @dangar; tipo: ARCADE or SHOT), (name: 'Last Duel'; year: '1988'; snd: 1; hi: false; zip: 'lastduel'; grid: 268; company: 'Capcom'; rom: @lastduel; tipo: ARCADE or DRIVE), (name: 'Mad Gear'; year: '1989'; snd: 1; hi: false; zip: 'madgear';
    grid: 269; company: 'Capcom'; rom: @madgear; tipo: ARCADE or DRIVE), (name: 'Led Storm Rally 2011'; year: '1989'; snd: 1; hi: false; zip: 'leds2011'; grid: 270; company: 'Capcom'; rom: @leds2011; tipo: ARCADE or DRIVE), (name: 'Gigas'; year: '1986'; snd: 1; hi: false;
    zip: 'gigas'; grid: 271; company: 'Sega'; rom: @gigas; tipo: ARCADE or MAZE), (name: 'Gigas Mark II'; year: '1986'; snd: 1; hi: false; zip: 'gigasm2'; grid: 272; company: 'Sega'; rom: @gigasm2; tipo: ARCADE or MAZE), (name: 'Omega'; year: '1986'; snd: 1; hi: false;
    zip: 'omega'; grid: 273; company: 'Nihon System'; rom: @omega; tipo: ARCADE or MAZE), (name: 'Perfect Billard'; year: '1987'; snd: 1; hi: false; zip: 'pbillrd'; grid: 274; company: 'Nihon System'; rom: @pbillrd; tipo: ARCADE or SPORT), (name: 'Armed F'; year: '1988'; snd: 1;
    hi: false; zip: 'armedf'; grid: 275; company: 'Nichibutsu'; rom: @armedf; tipo: ARCADE or SHOT), (name: 'Terra Force'; year: '1987'; snd: 1; hi: false; zip: 'terraf'; grid: 276; company: 'Nichibutsu'; rom: @terraf; tipo: ARCADE or SHOT), (name: 'Crazy Climber 2';
    year: '1988'; snd: 1; hi: false; zip: 'cclimbr2'; grid: 277; company: 'Nichibutsu'; rom: @cclimbr2; tipo: ARCADE), (name: 'Legion - Spinner-87'; year: '1987'; snd: 1; hi: false; zip: 'legion'; grid: 278; company: 'Nichibutsu'; rom: @legion; tipo: ARCADE or SHOT or DRIVE),
    (name: 'ASO - Armored Scrum Object'; year: '1985'; snd: 1; hi: false; zip: 'aso'; grid: 279; company: 'SNK'; rom: @aso; tipo: ARCADE or SHOT), (name: 'Fire Trap'; year: '1986'; snd: 1; hi: false; zip: 'firetrap'; grid: 280; company: 'Woodplace Inc.'; rom: @firetrap;
    tipo: ARCADE), (name: '3x3 Puzzle'; year: '1998'; snd: 1; hi: false; zip: '3x3puzzl'; grid: 281; company: 'Ace Enterprise'; rom: @puzz3x3; tipo: ARCADE), (name: 'Casanova'; year: '199?'; snd: 1; hi: false; zip: 'casanova'; grid: 282; company: 'Promat'; rom: @casanova;
    tipo: ARCADE or MAZE), (name: '1945k III'; year: '2000'; snd: 1; hi: false; zip: '1945kiii'; grid: 283; company: 'Oriental Soft'; rom: @k31945; tipo: ARCADE or SHOT), (name: '96 Flag Rally'; year: '2000'; snd: 1; hi: false; zip: 'flagrall'; grid: 284; company: 'Promat';
    rom: @flagrall; tipo: ARCADE or DRIVE), (name: 'Blood Bros.'; year: '1990'; snd: 1; hi: false; zip: 'bloodbro'; grid: 285; company: 'TAD Corporation'; rom: @bloodbros; tipo: ARCADE or RUN_GUN), (name: 'Sky Smasher'; year: '1990'; snd: 1; hi: false; zip: 'skysmash'; grid: 286;
    company: 'Nihon System'; rom: @skysmash; tipo: ARCADE or SHOT), (name: 'Baraduke'; year: '1985'; snd: 1; hi: false; zip: 'baraduke'; grid: 287; company: 'Namco'; rom: @baraduke; tipo: ARCADE or RUN_GUN), (name: 'Metro-Cross'; year: '1985'; snd: 1; hi: false; zip: 'metrocrs';
    grid: 288; company: 'Namco'; rom: @metrocross; tipo: ARCADE or RUN_GUN), (name: 'The Return of Ishtar'; year: '1986'; snd: 1; hi: false; zip: 'roishtar'; grid: 289; company: 'Namco'; rom: @roishtar; tipo: ARCADE or MAZE), (name: 'Genpei ToumaDen'; year: '1986'; snd: 1;
    hi: false; zip: 'genpeitd'; grid: 290; company: 'Namco'; rom: @genpeitd; tipo: ARCADE or RUN_GUN), (name: 'Wonder Momo'; year: '1987'; snd: 1; hi: false; zip: 'wndrmomo'; grid: 291; company: 'Namco'; rom: @wndrmomo; tipo: ARCADE or RUN_GUN), (name: 'Altered Beast';
    year: '1988'; snd: 1; hi: false; zip: 'altbeast'; grid: 292; company: 'Sega'; rom: @altbeast; tipo: ARCADE or FIGHT), (name: 'Golden Axe'; year: '1989'; snd: 1; hi: false; zip: 'goldnaxe'; grid: 293; company: 'Sega'; rom: @goldnaxe; tipo: ARCADE or FIGHT),
    (name: 'Dynamite Dux'; year: '1988'; snd: 1; hi: false; zip: 'ddux1'; grid: 294; company: 'Sega'; rom: @ddux; tipo: ARCADE or FIGHT), (name: 'E-Swat - Cyber Police'; year: '1989'; snd: 1; hi: false; zip: 'eswat'; grid: 295; company: 'Sega'; rom: @eswat;
    tipo: ARCADE or RUN_GUN or FIGHT), (name: 'Passing Shot'; year: '1988'; snd: 1; hi: false; zip: 'passsht'; grid: 296; company: 'Sega'; rom: @passsht; tipo: ARCADE or SPORT), (name: 'Aurail'; year: '1990'; snd: 1; hi: false; zip: 'aurail'; grid: 297; company: 'Sega';
    rom: @aurail; tipo: ARCADE or RUN_GUN), (name: 'Hellfire'; year: '1989'; snd: 1; hi: false; zip: 'hellfire'; grid: 298; company: 'Toaplan'; rom: @hellfire; tipo: ARCADE or RUN_GUN), (name: 'Lock''n''Chase'; year: '1981'; snd: 1; hi: false; zip: 'lnc'; grid: 299;
    company: 'Deco'; rom: @lnc; tipo: ARCADE or MAZE), (name: 'Minky Monkey'; year: '1982'; snd: 1; hi: false; zip: 'mmonkey'; grid: 300; company: 'Deco'; rom: @mmonkey; tipo: ARCADE or MAZE), (name: 'Karate Champ'; year: '1984'; snd: 1; hi: false; zip: 'kchamp'; grid: 301;
    company: 'Data East'; rom: @karatechamp; tipo: ARCADE or SPORT or FIGHT), (name: 'Thundercade'; year: '1987'; snd: 1; hi: false; zip: 'tndrcade'; grid: 302; company: 'Seta'; rom: @tndrcade; tipo: ARCADE or SHOT), (name: 'Twin Eagle - Revenge Joe''s Brother'; year: '1988';
    snd: 1; hi: false; zip: 'twineagl'; grid: 303; company: 'Seta'; rom: @twineagl; tipo: ARCADE or SHOT), (name: 'Thunder & Lightning'; year: '1990'; snd: 1; hi: false; zip: 'thunderl'; grid: 304; company: 'Seta'; rom: @thunderl; tipo: ARCADE or MAZE), (name: 'Ms Pac Man Twin';
    year: '1992'; snd: 1; hi: false; zip: 'mspactwin'; grid: 305; company: 'Susilu'; rom: @mspactwin; tipo: ARCADE or MAZE), (name: 'Extermination'; year: '1987'; snd: 1; hi: false; zip: 'extrmatn'; grid: 306; company: 'Taito'; rom: @extrmatn; tipo: ARCADE or RUN_GUN),
    (name: 'Atomic Robo-kid'; year: '1988'; snd: 1; hi: false; zip: 'robokid'; grid: 307; company: 'UPL'; rom: @robokid; tipo: ARCADE or RUN_GUN), (name: 'Mr. Do''s Castle'; year: '1983'; snd: 1; hi: false; zip: 'docastle'; grid: 308; company: 'Universal'; rom: @docastle;
    tipo: ARCADE or MAZE), (name: 'Do! Run Run'; year: '1984'; snd: 1; hi: false; zip: 'dorunrun'; grid: 309; company: 'Universal'; rom: @dorunrun; tipo: ARCADE or MAZE), (name: 'Mr. Do''s Wild Ride'; year: '1984'; snd: 1; hi: false; zip: 'dowild'; grid: 310;
    company: 'Universal'; rom: @dowild; tipo: ARCADE or MAZE), (name: 'Jumping Jack'; year: '1984'; snd: 1; hi: false; zip: 'jjack'; grid: 311; company: 'Universal'; rom: @jjack; tipo: ARCADE), (name: 'Kick Rider'; year: '1984'; snd: 1; hi: false; zip: 'kickridr'; grid: 312;
    company: 'Universal'; rom: @kickridr; tipo: ARCADE or SPORT or DRIVE), (name: 'Indoor Soccer'; year: '1985'; snd: 1; hi: false; zip: 'idsoccer'; grid: 313; company: 'Universal'; rom: @idsoccer; tipo: ARCADE or SPORT), (name: 'Crystal Castles'; year: '1983'; snd: 1; hi: false;
    zip: 'ccastles'; grid: 314; company: 'Atari'; rom: @ccastles; tipo: ARCADE or MAZE), (name: 'Flower'; year: '1986'; snd: 1; hi: false; zip: 'flower'; grid: 315; company: 'Clarue'; rom: @flower; tipo: ARCADE or SHOT), (name: 'SlySpy'; year: '1989'; snd: 1; hi: false;
    zip: 'slyspy'; grid: 316; company: 'Data East'; rom: @slyspy; tipo: ARCADE or RUN_GUN), (name: 'Boulder Dash I-II'; year: '1990'; snd: 1; hi: false; zip: 'bouldash'; grid: 317; company: 'Data East'; rom: @bouldash; tipo: ARCADE or MAZE), (name: 'Super Dodge Ball';
    year: '1987'; snd: 1; hi: false; zip: 'spdodgeb'; grid: 318; company: 'Technos'; rom: @sdodgeball; tipo: ARCADE or SPORT), (name: 'Senjyo'; year: '1983'; snd: 1; hi: false; zip: 'senjyo'; grid: 319; company: 'Tehkan'; rom: @senjyo; tipo: ARCADE or SHOT),
    (name: 'Baluba-louk no Densetsu'; year: '1986'; snd: 1; hi: false; zip: 'baluba'; grid: 320; company: 'Able Corp, Ltd.'; rom: @baluba; tipo: ARCADE or RUN_GUN), (name: 'Joust'; year: '1982'; snd: 1; hi: false; zip: 'joust'; grid: 321; company: 'Williams'; rom: @joust;
    tipo: ARCADE or MAZE), (name: 'Robotron'; year: '1982'; snd: 1; hi: false; zip: 'robotron'; grid: 322; company: 'Williams'; rom: @robotron; tipo: ARCADE or RUN_GUN), (name: 'Stargate'; year: '1981'; snd: 1; hi: false; zip: 'stargate'; grid: 323; company: 'Williams';
    rom: @stargate; tipo: ARCADE or SHOT), (name: 'Tapper'; year: '1983'; snd: 1; hi: false; zip: 'tapper'; grid: 324; company: 'Bally Midway'; rom: @tapper; tipo: ARCADE), (name: 'Arkanoid'; year: '1986'; snd: 1; hi: false; zip: 'arkanoid'; grid: 325; company: 'Taito';
    rom: @arkanoid; tipo: ARCADE or MAZE), (name: 'Side Arms - Hyper Dyne'; year: '1986'; snd: 1; hi: false; zip: 'sidearms'; grid: 326; company: 'Capcom'; rom: @sidearms; tipo: ARCADE or RUN_GUN or SHOT), (name: 'The Speed Rumbler'; year: '1986'; snd: 1; hi: false;
    zip: 'srumbler'; grid: 327; company: 'Capcom'; rom: @speedr; tipo: ARCADE or RUN_GUN), (name: 'China Gate'; year: '1988'; snd: 1; hi: false; zip: 'chinagat'; grid: 328; company: 'Technos Japan'; rom: @chinagate; tipo: ARCADE or RUN_GUN or FIGHT), (name: 'Mag Max';
    year: '1985'; snd: 1; hi: false; zip: 'magmax'; grid: 329; company: 'Nichibutsu'; rom: @magmax; tipo: ARCADE or RUN_GUN), (name: 'S.R.D. Mission'; year: '1986'; snd: 1; hi: false; zip: 'srdmissn'; grid: 330; company: 'Kyugo/Sega'; rom: @srdmission; tipo: ARCADE or SHOT),
    (name: 'Airwolf'; year: '1987'; snd: 1; hi: false; zip: 'airwolf'; grid: 331; company: 'Kyugo'; rom: @airwolf; tipo: ARCADE or SHOT), (name: 'Ambush'; year: '1983'; snd: 1; hi: false; zip: 'ambush'; grid: 332; company: 'Tecfri'; rom: @ambush; tipo: ARCADE or SHOT),
    (name: 'Super Duck'; year: '1992'; snd: 1; hi: false; zip: 'supduck'; grid: 333; company: 'Comad'; rom: @superduck; tipo: ARCADE or MAZE), (name: 'Hang-On'; year: '1985'; snd: 1; hi: false; zip: 'hangon'; grid: 334; company: 'Sega'; rom: @hangon; tipo: ARCADE or SPORT),
    (name: 'Enduro Racer'; year: '1986'; snd: 1; hi: false; zip: 'enduror'; grid: 335; company: 'Sega'; rom: @enduror; tipo: ARCADE or SPORT), (name: 'Space Harrier'; year: '1985'; snd: 1; hi: false; zip: 'sharrier'; grid: 336; company: 'Sega'; rom: @sharrier_roms;
    tipo: ARCADE or RUN_GUN), (name: '64th Street - A detective story'; year: '1991'; snd: 1; hi: false; zip: '64street'; grid: 337; company: 'Jaleco'; rom: @th64_roms; tipo: ARCADE or FIGHT), (name: 'Shadow Warriors'; year: '1988'; snd: 1; hi: false; zip: 'shadoww'; grid: 338;
    company: 'Tecmo'; rom: @shadoww_roms; tipo: ARCADE or FIGHT), (name: 'Wild Fang/Tecmo Knight'; year: '1989'; snd: 1; hi: false; zip: 'wildfang'; grid: 339; company: 'Tecmo'; rom: @wildfang_roms; tipo: ARCADE or FIGHT), (name: 'Raiden'; year: '1990'; snd: 1; hi: false;
    zip: 'raiden'; grid: 340; company: 'Seibu Kaihatsu'; rom: @raiden_roms; tipo: ARCADE or SHOT), (name: 'Twins'; year: '1993'; snd: 1; hi: false; zip: 'twins'; grid: 341; company: 'Ecogames'; rom: @twins_roms; tipo: ARCADE or MAZE), (name: 'Twins (Electronic Devices)';
    year: '1994'; snd: 1; hi: false; zip: 'twinsed1'; grid: 342; company: 'Ecogames'; rom: @twinsed1_roms; tipo: ARCADE or MAZE), (name: 'Hot Blocks - Tetirx II'; year: '1993'; snd: 1; hi: false; zip: 'hotblock'; grid: 343; company: 'NIX?'; rom: @hotblock_roms;
    tipo: ARCADE or MAZE), (name: 'Missile Command'; year: '1980'; snd: 1; hi: false; zip: 'missile'; grid: 344; company: 'Atari'; rom: @missile_roms; tipo: ARCADE or SHOT), (name: 'Super Missile Attack'; year: '1981'; snd: 1; hi: false; zip: 'suprmatk'; grid: 345;
    company: 'Atari'; rom: @suprmatk_roms; tipo: ARCADE or SHOT), (name: 'Super Zaxxon'; year: '1982'; snd: 3; hi: false; zip: 'szaxxon'; grid: 346; company: 'Sega'; rom: @szaxxon_roms; samples: @zaxxon_samples; tipo: ARCADE or SHOT), (name: 'Future Spy'; year: '1984'; snd: 3;
    hi: false; zip: 'futspy'; grid: 347; company: 'Sega'; rom: @futspy_roms; samples: @zaxxon_samples; tipo: ARCADE or SHOT), (name: 'Millipede'; year: '1982'; snd: 1; hi: false; zip: 'milliped'; grid: 348; company: 'Atari'; rom: @milliped_roms; tipo: ARCADE or SHOT),
    (name: 'Gaplus'; year: '1984'; snd: 3; hi: false; zip: 'gaplus'; grid: 349; company: 'Namco'; rom: @gaplus_roms; samples: @gaplus_samples; tipo: ARCADE or SHOT), (name: 'Super Xevious'; year: '1984'; snd: 3; hi: false; zip: 'sxevious'; grid: 350; company: 'Namco';
    rom: @sxevious_roms; samples: @xevious_samples; tipo: ARCADE or SHOT), (name: 'Grobda'; year: '1984'; snd: 1; hi: false; zip: 'grobda'; grid: 351; company: 'Namco'; rom: @grobda_roms; tipo: ARCADE or SHOT), (name: 'Pac & Pal'; year: '1983'; snd: 1; hi: false; zip: 'pacnpal';
    grid: 352; company: 'Namco'; rom: @pacnpal_roms; tipo: ARCADE or MAZE), (name: 'Birdiy'; year: '1983'; snd: 1; hi: false; zip: 'birdiy'; grid: 353; company: 'Mama Top'; rom: @birdiy_roms; tipo: ARCADE or MAZE), (name: 'Wily Tower'; year: '1984'; snd: 1; hi: false;
    zip: 'wilytowr'; grid: 354; company: 'Irem'; rom: @wilytower_roms; tipo: ARCADE or MAZE), (name: 'Fighting Basketball'; year: '1984'; snd: 1; hi: false; zip: 'fghtbskt'; grid: 355; company: 'Irem'; rom: @fightbasket_roms; tipo: ARCADE or SPORT), (name: 'Diverboy';
    year: '1992'; snd: 1; hi: false; zip: 'diverboy'; grid: 356; company: 'Gamart'; rom: @diverboy_roms; tipo: ARCADE or MAZE), (name: 'Mug Smashers'; year: '1990'; snd: 1; hi: false; zip: 'mugsmash'; grid: 357; company: 'Electronic Devices Italy'; rom: @mugsmash_roms;
    tipo: ARCADE or FIGHT), (name: 'Steel Force'; year: '1994'; snd: 1; hi: false; zip: 'stlforce'; grid: 358; company: 'Ecogames'; rom: @steelforce_roms; tipo: ARCADE or MAZE or SHOT), (name: 'Twin Brats'; year: '1995'; snd: 1; hi: false; zip: 'twinbrat'; grid: 359;
    company: 'Ecogames'; rom: @twinbrats_roms; tipo: ARCADE or MAZE), (name: 'Mortal Race'; year: '1995'; snd: 1; hi: false; zip: 'mortalr'; grid: 360; company: 'Ecogames'; rom: @mortalrace_roms; tipo: ARCADE or DRIVE), (name: 'Bank Panic'; year: '1985'; snd: 1; hi: false;
    zip: 'bankp'; grid: 361; company: 'Sanritsu/Sega'; rom: @bankpanic_roms; tipo: ARCADE or SHOT), (name: 'Combat Hawk'; year: '1987'; snd: 1; hi: false; zip: 'combh'; grid: 362; company: 'Sanritsu/Sega'; rom: @combathawk_roms; tipo: ARCADE or SHOT), (name: 'Ant Eater';
    year: '1982'; snd: 1; hi: false; zip: 'anteater'; grid: 363; company: 'Tago Electronics'; rom: @anteater_roms; tipo: ARCADE or MAZE), (name: 'Appoooh'; year: '1984'; snd: 1; hi: false; zip: 'appoooh'; grid: 364; company: 'Sanritsu/Sega'; rom: @appoooh_roms;
    tipo: ARCADE or SPORT), (name: 'Robo Wres 2001'; year: '1986'; snd: 1; hi: false; zip: 'robowres'; grid: 365; company: 'Sanritsu/Sega'; rom: @robowres_roms; tipo: ARCADE or SPORT), (name: 'Armored Car'; year: '1981'; snd: 1; hi: false; zip: 'armorcar'; grid: 366;
    company: 'Stern Electronics'; rom: @armoredcar_roms; tipo: ARCADE or MAZE), (name: '88 Games'; year: '1988'; snd: 1; hi: false; zip: '88games'; grid: 367; company: 'Konami'; rom: @hw88games_roms; tipo: ARCADE or SPORT), (name: 'Avengers'; year: '1987'; snd: 1; hi: false;
    zip: 'avengers'; grid: 368; company: 'Capcom'; rom: @avengers_roms; tipo: ARCADE or RUN_GUN), (name: 'The End'; year: '1980'; snd: 1; hi: false; zip: 'theend'; grid: 369; company: 'Konami'; rom: @theend_roms; tipo: ARCADE or SHOT), (name: 'Battle of Atlantis'; year: '1981';
    snd: 1; hi: false; zip: 'atlantis'; grid: 370; company: 'Comsoft'; rom: @atlantis_roms; tipo: ARCADE or SHOT), (name: 'Blue Hawk'; year: '1993'; snd: 1; hi: false; zip: 'bluehawk'; grid: 371; company: 'Dooyong'; rom: @bluehawk_roms; tipo: ARCADE or SHOT),
    (name: 'The Last Day'; year: '1990'; snd: 1; hi: false; zip: 'lastday'; grid: 372; company: 'Dooyong'; rom: @lastday_roms; tipo: ARCADE or SHOT), (name: 'Gulf Storm'; year: '1991'; snd: 1; hi: false; zip: 'gulfstrm'; grid: 373; company: 'Dooyong'; rom: @gulfstorm_roms;
    tipo: ARCADE or SHOT), (name: 'Pollux'; year: '1991'; snd: 1; hi: false; zip: 'pollux'; grid: 374; company: 'Dooyong'; rom: @pollux_roms; tipo: ARCADE or SHOT), (name: 'Flying Tiger'; year: '1992'; snd: 1; hi: false; zip: 'flytiger'; grid: 375; company: 'Dooyong';
    rom: @flytiger_roms; tipo: ARCADE or SHOT), (name: 'Sky Skipper'; year: '1981'; snd: 1; hi: false; zip: 'skyskipr'; grid: 376; company: 'Nintendo'; rom: @skyskipper_roms; tipo: ARCADE or SHOT), (name: 'Blue Print'; year: '1982'; snd: 1; hi: false; zip: 'blueprnt'; grid: 377;
    company: 'Zilec Electronics/Bally Midway'; rom: @blueprint_roms; tipo: ARCADE or MAZE), (name: 'Saturn'; year: '1983'; snd: 1; hi: false; zip: 'saturnzi'; grid: 378; company: 'Zilec Electronics/Jaleco'; rom: @saturnzi_roms; tipo: ARCADE or SHOT), (name: 'Grasspin';
    year: '1983'; snd: 1; hi: false; zip: 'grasspin'; grid: 379; company: 'Zilec Electronics/Jaleco'; rom: @grasspin_roms; tipo: ARCADE or MAZE), (name: 'BurglarX'; year: '1997'; snd: 1; hi: false; zip: 'burglarx'; grid: 380; company: 'Unico'; rom: @burglarx_roms;
    tipo: ARCADE or MAZE), (name: 'Zero Point'; year: '1998'; snd: 1; hi: false; zip: 'zeropnt'; grid: 381; company: 'Unico'; rom: @zeropnt_roms; tipo: ARCADE or SHOT), (name: 'Calipso'; year: '1982'; snd: 1; hi: false; zip: 'calipso'; grid: 382; company: 'Tago Electronics';
    rom: @calipso_roms; tipo: ARCADE or MAZE), (name: 'Calorie Kun vs Moguranian'; year: '1986'; snd: 1; hi: false; zip: 'calorie'; grid: 383; company: 'Sega'; rom: @caloriekun_roms; tipo: ARCADE or MAZE), (name: 'Gardia'; year: '1986'; snd: 1; hi: false; zip: 'gardia';
    grid: 384; company: 'Sega'; rom: @gardia_roms; tipo: ARCADE or SHOT), (name: 'Cavelon'; year: '1983'; snd: 1; hi: false; zip: 'cavelon'; grid: 385; company: 'Jetsoft'; rom: @cavelon_roms; tipo: ARCADE or MAZE), (name: 'Come Back Toto'; year: '1996'; snd: 1; hi: false;
    zip: 'toto'; grid: 386; company: 'SoftClub'; rom: @toto_roms; tipo: ARCADE or MAZE), (name: 'Hyper Pacman'; year: '1995'; snd: 1; hi: false; zip: 'hyperpac'; grid: 387; company: 'SemiCom'; rom: @hyperpac_roms; tipo: ARCADE or MAZE), (name: 'KiKi KaiKai'; year: '1986'; snd: 1;
    hi: false; zip: 'kikikai'; grid: 388; company: 'Taito'; rom: @kikikaikai_roms; tipo: ARCADE or RUN_GUN OR MAZE), (name: 'Kick and Run'; year: '1986'; snd: 1; hi: false; zip: 'kicknrun'; grid: 389; company: 'Taito'; rom: @kickrun_roms; tipo: ARCADE or SPORT), (name: 'Lasso';
    year: '1982'; snd: 1; hi: false; zip: 'lasso'; grid: 390; company: 'SNK'; rom: @lasso_roms; tipo: ARCADE or RUN_GUN), (name: 'Chameleon'; year: '1983'; snd: 1; hi: false; zip: 'chameleo'; grid: 391; company: 'Jaleco'; rom: @chameleo_roms; tipo: ARCADE or MAZE),
    (name: 'Last Mission'; year: '1986'; snd: 1; hi: false; zip: 'lastmisn'; grid: 392; company: 'Data East'; rom: @lastmisn_roms; tipo: ARCADE or SHOT), (name: 'Shackled'; year: '1986'; snd: 1; hi: false; zip: 'shackled'; grid: 393; company: 'Data East'; rom: @shackled_roms;
    tipo: ARCADE or MAZE), (name: 'Gondomania'; year: '1987'; snd: 1; hi: false; zip: 'gondo'; grid: 394; company: 'Data East'; rom: @gondo_roms; tipo: ARCADE or SHOT), (name: 'Garyo Retsuden'; year: '1987'; snd: 1; hi: false; zip: 'garyoret'; grid: 395; company: 'Data East';
    rom: @garyoret_roms; tipo: ARCADE or SHOT), (name: 'Captain Silver'; year: '1987'; snd: 1; hi: false; zip: 'csilver'; grid: 396; company: 'Data East'; rom: @csilver_roms; tipo: ARCADE or RUN_GUN), (name: 'Cobra-Command'; year: '1988'; snd: 1; hi: false; zip: 'cobracom';
    grid: 397; company: 'Data East'; rom: @cobracom_roms; tipo: ARCADE or SHOT), (name: 'The Real Ghostbusters'; year: '1987'; snd: 1; hi: false; zip: 'ghostb'; grid: 398; company: 'Data East'; rom: @ghostb_roms; tipo: ARCADE or RUN_GUN), (name: 'Psycho-Nics Oscar'; year: '1987';
    snd: 1; hi: false; zip: 'oscar'; grid: 399; company: 'Data East'; rom: @oscar_roms; tipo: ARCADE or RUN_GUN), (name: 'Road Fighter'; year: '1984'; snd: 1; hi: false; zip: 'roadf'; grid: 400; company: 'Konami'; rom: @roadf_roms; tipo: ARCADE or DRIVE), (name: 'Ponpoko';
    year: '1982'; snd: 1; hi: false; zip: 'ponpoko'; grid: 401; company: 'Sigma'; rom: @ponpoko_roms; tipo: ARCADE or MAZE), (name: 'Woodpecker'; year: '1981'; snd: 1; hi: false; zip: 'woodpeck'; grid: 402; company: 'Amenip'; rom: @woodpeck_roms; tipo: ARCADE or MAZE),
    (name: 'Eyes'; year: '1982'; snd: 1; hi: false; zip: 'eyes'; grid: 403; company: 'Techstar'; rom: @eyes_roms; tipo: ARCADE or MAZE), (name: 'Ali Baba and 40 Thieves'; year: '1982'; snd: 1; hi: false; zip: 'alibaba'; grid: 404; company: 'Sega'; rom: @alibaba_roms;
    tipo: ARCADE or MAZE), (name: 'Piranha'; year: '1981'; snd: 1; hi: false; zip: 'piranha'; grid: 405; company: 'GL'; rom: @piranha_roms; tipo: ARCADE or MAZE), (name: 'Final Star Force'; year: '1992'; snd: 1; hi: false; zip: 'fstarfrc'; grid: 406; company: 'Tecmo';
    rom: @finalstarforce_roms; tipo: ARCADE or SHOT), (name: 'Wyvern F-0'; year: '1985'; snd: 1; hi: false; zip: 'wyvernf0'; grid: 407; company: 'Taito'; rom: @wyvernf0_roms; tipo: ARCADE or SHOT),
    // *** Consoles
    (name: 'NES'; year: '198X'; snd: 1; hi: false; zip: ''; grid: 1000; company: 'Nintendo'; tipo: CONSOLE), (name: 'ColecoVision'; year: '1980'; snd: 1; hi: false; zip: 'coleco'; grid: 1001; company: 'Coleco'; rom: @coleco_; tipo: CONSOLE), (name: 'GameBoy'; year: '198X';
    snd: 1; hi: false; zip: 'gameboy'; grid: 1002; company: 'Nintendo'; rom: @gameboy; tipo: CONSOLE), (name: 'GameBoy Color'; year: '198X'; snd: 1; hi: false; zip: 'gbcolor'; grid: 1002; company: 'Nintendo'; rom: @gbcolor; tipo: CONSOLE), (name: 'CHIP 8'; year: '197X'; snd: 1;
    hi: false; zip: ''; grid: 1003; company: '-'; tipo: CONSOLE), (name: 'Master System'; year: '1986'; snd: 1; hi: false; zip: 'sms'; grid: 1004; company: 'Sega'; rom: @sms_; tipo: CONSOLE), (name: 'SG-1000'; year: '1985'; snd: 1; hi: false; zip: ''; grid: 1005; company: 'Sega';
    tipo: CONSOLE), (name: 'GameGear'; year: '1990'; snd: 1; hi: false; zip: ''; grid: 1006; company: 'Sega'; tipo: CONSOLE), (name: 'Super Cassette Vision'; year: '1984'; snd: 1; hi: false; zip: 'scv'; grid: 1007; company: 'Epoch'; rom: @scv; tipo: CONSOLE),
    (name: 'Genesis/Megadrive'; year: '1988'; snd: 1; hi: false; zip: ''; grid: 1008; company: 'Sega'; tipo: CONSOLE), (name: 'PV-1000'; year: '1983'; snd: 1; hi: false; zip: ''; grid: 1009; company: 'Casio'; tipo: CONSOLE), (name: 'PV-2000'; year: '1983'; snd: 1; hi: false;
    zip: 'pv2000'; grid: 1010; company: 'Casio'; rom: @pv2000_rom; tipo: CONSOLE),
    // G&W
    (name: 'Donkey Kong Jr'; year: '1983'; snd: 1; hi: false; zip: 'gnw_dj101'; grid: 2000; company: 'Nintendo'; rom: @gnw_dj101; tipo: GNW), (name: 'Donkey Kong II'; year: '1983'; snd: 1; hi: false; zip: 'gnw_jr55'; grid: 2001; company: 'Nintendo'; rom: @gnw_jr55;
    tipo: GNW), (name: 'Mario Bros'; year: '1983'; snd: 1; hi: false; zip: 'gnw_mw56'; grid: 2002; company: 'Nintendo'; rom: @gnw_mw56; tipo: GNW));

var
  orden_games: array [1 .. GAMES_CONT] of word;

procedure load_machine(vMachine: word);

implementation

uses
  main,
  misc_functions;

procedure load_machine(vMachine: word);
begin
  case vMachine of
    0, 5:
      machine_calls.start := start_spectrum_48k;
    1, 4:
      machine_calls.start := start_spectrum_128k;
    2, 3:
      machine_calls.start := start_spectrum_3;
    7, 8, 9:
      machine_calls.start := start_amstrad_cpc;
    3000:
      machine_calls.start := start_commodore_64;
    3001, 3002:
      machine_calls.start := start_oric;
    // arcade
    10, 88, 234, 305, 353, 401, 402, 403, 404, 405:
      { 10 : pacman
        88 : mspacman
        234: chrush roller
        305: ms pacman twin
        353: birdiy
        401: pon pokopon
        402: wood pecker
        403: eyes
        404: ali baba and the 40 thieves
        405: piranha
      }
      machine_calls.start := start_pacman;
    11, 202:
      { 11 : phoenix
        202 : pleiades }
      machine_calls.start := start_phoenix;
    12:
      { 12 : mysterious stones }
      machine_calls.start := start_mysteriousstones;
    13, 383:
      { 13 : bombjack
        383 : caloriekun }
      machine_calls.start := start_bombjack;
    14, 47, 48, 49, 143, 144, 145, 363, 366, 369, 370, 382, 385:
      { 14 : frogger
        47 : galaxian
        48 : jump bug
        49 : moon cresta
        143: scramble
        144: super cobra
        145: amidar
        363: anteater
        366: armored car
        369: the end
        370: battle of atlantis
        382: calipso
        385: cavelon }
      machine_calls.start := start_galaxian;
    15, 168, 169:
      { 15 : donkey kong
        168: donkey kong jr.
        169: donkey kong 3 }
      machine_calls.start := start_donkeykong;
    16:
      { 16 : black tiger }
      machine_calls.start := start_blacktiger;
    17, 203:
      { 17 : green beret
        203: mr. goemon }
      machine_calls.start := start_greenberet;
    18:
      { 18 : commando }
      machine_calls.start := start_commando;
    19:
      { 19 : ghosts 'n goblins }
      machine_calls.start := start_ghostsngoblins;
    20:
      { 20 : mikie }
      machine_calls.start := start_mikie;
    21:
      { 21 : shaolins road }
      machine_calls.start := start_shaolinsroad;
    22:
      { 22 : yier kung fu }
      machine_calls.start := start_yiearkungfu;
    23, 233:
      { 23 : asteroids
        233: lunar lander }
      machine_calls.start := start_asteroids;
    24:
      { 24 : sonson }
      machine_calls.start := start_sonson;
    25, 319, 320:
      { 25 : star force
        319: senjyo
        320: baluba-look no densetsu }
      machine_calls.start := start_starforce;
    26, 97:
      { 26 : rygar
        97 : silkworm }
      machine_calls.start := start_tecmo;
    27, 35, 36, 37, 151, 152, 153, 154, 155, 384:
      { 27 : pitfall 2
        35 : teddy boy blues
        36 : wonder boy
        37 : wonder boy in monster land
        151: choplifter
        152: mr. viking
        153: sega ninja
        154: up and down
        155: flicky
        384: gardia }
      load_system1; // i need to make some changes
    28:
      { 28 : pooyan }
      machine_calls.start := start_pooyan;
    29, 50, 70:
      { 29 : jungler
        50 :  rally x
        70 : new rally x }
      machine_calls.start := start_rallyx;
    30:
      { 30 : city connection }
      machine_calls.start := start_cityconnection;
    31, 299, 300:
      { 31 : burger time
        299: lock 'n chase
        300: minky monkey }
      machine_calls.start := start_burgertime;
    32:
      { 32 : express raider }
      machine_calls.start := start_expressraider;
    33:
      { 33 : super basketball }
      machine_calls.start := start_superbasketball;
    34, 200, 201:
      { 34 : lady bug
        200 : snapjack
        201 : cosmic avenger }
      machine_calls.start := start_ladybug;
    38:
      { 38 : tehkan world cup }
      machine_calls.start := start_tehkanworldcup;
    39, 376:
      { 39 : popeye
        376: sky skipper }
      machine_calls.start := start_popeye;
    40:
      { 40 : psychic 5 }
      machine_calls.start := start_psychic5;
    41, 265:
      { 41 : terra cresta
        265: soldier girl amazon }
      machine_calls.start := start_terracresta;
    42, 72, 73, 74, 75:
      { 42 : kungfu master
        72 : spelunker
        73 : spelunker 2
        74 : load runner
        75 : load runner 2 }
      machine_calls.start := start_m62;
    43:
      { 43 : shootout }
      machine_calls.start := start_shootout;
    44:
      { 44 : vigilante }
      machine_calls.start := start_vigilante;
    45:
      { 45 : jackal }
      machine_calls.start := start_jackal;
    46:
      { 46 : bubble bobble }
      machine_calls.start := start_bubblebobble;
    51:
      { 51 : prehistoric isle }
      machine_calls.start := start_prehistoricisle;
    52, 53:
      { 52 : tiger road
        53 : f1 dream }
      machine_calls.start := start_tigerroad;
    54, 386, 387:
      { 54 : snow bros
        386: come back toto
        387: hyper pacman }
      machine_calls.start := start_snowbros;
    55:
      { 55 : toki }
      machine_calls.start := start_toki;
    56:
      { 56 : kontra }
      machine_calls.start := start_contra;
    57, 63, 64, 192, 193, 351, 352:
      { 57 : mappy
        63 : dig-dug 2
        64: super pacman
        192: the tower of druaga
        193: motos
        351: grobda
        352: pac & pal }
      machine_calls.start := start_mappy;
    58:
      { 58 : rastan }
      machine_calls.start := start_rastan;
    59, 60, 61, 247, 368:
      { 59 : lagendary wings
        60 : section z
        61 : trojan
        247: fire ball
        368: avengers }
      machine_calls.start := start_lwings;
    62:
      { 62 : street fighter }
      machine_calls.start := start_streetfighter;
    65, 167, 231, 250:
      { 65 : galaga
        167: dig-dug
        231: xevious
        250: bosconian }
      machine_calls.start := start_galagahw;
    66:
      { 66 : xain 'n sleena }
      machine_calls.start := start_xainnsleena;
    67, 68:
      { 67 : hardhead
        68 : hardhead 2 }
      machine_calls.start := start_suna;
    69, 71:
      { 69 : saboten bombers
        71 : bombjack twin }
      machine_calls.start := start_nmk16;
    76:
      { 76 : knuckle joe }
      machine_calls.start := start_knucklejoe;
    77:
      { 77 : wardner }
      machine_calls.start := start_wardner;
    78, 101, 173, 174:
      { 78 : big karnak
        101: thunder hoop
        173: squash
        174: biomechanical toy }
      machine_calls.start := start_gaelco;
    79:
      { 79 : exed exes }
      machine_calls.start := start_exedexes;
    80, 82, 83:
      { 80 : gunsmoke
        82 : 1943
        83 : 1943 kai }
      machine_calls.start := start_gunsmoke;
    81:
      { 81 : 1042 }
      machine_calls.start := start_1942;
    84:
      { 84 : jailbreak }
      machine_calls.start := start_jailbreak;
    85:
      { 85 : circus charlie }
      machine_calls.start := start_circuscharlie;
    86:
      { 86 : iron horse }
      machine_calls.start := start_ironhorse;
    87, 190, 191:
      { 87 : r-type
        190: hammerin' harry
        191: r-type 2 }
      machine_calls.start := start_m72;
    89, 90:
      { 89 : breakthru
        90 : darwin 4078 }
      machine_calls.start := start_breakthru;
    91, 392, 393, 394, 395, 396, 397, 398, 399:
      { 91 : super real darwin
        392: last mission
        393: shackled
        394: gondomania
        395: garyo retsuden
        396: captain silver
        397: cobra command
        398: the real ghostbusters
        399: psycho-nicks oscar }
      machine_calls.start := start_dec8;
    92, 96:
      { 92 : double dragon
        96 : double dragon 2 }
      machine_calls.start := start_doubledragon;
    93:
      { 93 : mr. do }
      machine_calls.start := start_mrdo;
    94, 95:
      { 94 : the glob
        95 : super glob }
      machine_calls.start := start_epos;
    98, 99:
      { 98 : tiger heli
        99 : slap fight }
      machine_calls.start := start_slapfight;
    100:
      { 100: the legend of kage }
      machine_calls.start := start_thelegendofkage;
    102:
      { 102: cabal }
      machine_calls.start := start_cabal;
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113:
      { 103: ghouls 'n ghosts
        104: final fight
        105: king of dragons
        106: street fighter 2
        107: strider
        108: 3 wonders
        109: captain commando
        110: knights of the round
        111: street fighter 2 champion edition
        112: cadillacs and dinosaurs
        113: the punisher }
      machine_calls.start := start_cps1;
    114, 115, 116, 186, 187, 198:
      { 114: shinobi
        115: alex kid
        116: fantasy zone
        186: alien syndrome
        187: wonder boy 3
        198: tetris }
      machine_calls.start := start_system16a;
    117:
      { 117: time pilot '84 }
      machine_calls.start := start_timepilot84;
    118:
      { 118: tutankham }
      machine_calls.start := start_tutankham;
    119, 183:
      { 119: pang
        183: super pang }
      machine_calls.start := start_pang;
    120, 121, 122, 307:
      { 120: ninja kid 2
        121: ark area
        122: mutant night
        307: atomic robokid }
      machine_calls.start := start_ninjakid2;
    123, 194:
      { 123: skykid
        194: dragon blaster }
      machine_calls.start := start_skykid;
    124, 125, 126, 289, 290, 291:
      { 124: rolling thunder
        125: hop mappy
        126: skykid x
        289: the return of ishtar
        290: genpei toumaden
        291: wonder momo }
      machine_calls.start := start_system86;
    127:
      { 127: rock 'n rope }
      machine_calls.start := start_rocnrope;
    128, 330, 331:
      { 128: repulse
        330: srd mission
        331: airwolf }
      machine_calls.start := start_kyugo;
    129, 130, 306:
      { 129: the new zealand story
        130: insector x
        306: extermination }
      machine_calls.start := start_thenewzealandstory;
    131:
      { 131: pacland }
      machine_calls.start := start_pacland;
    132:
      { 132: mario }
      machine_calls.start := start_mario;
    133:
      { 133: solomon's key }
      machine_calls.start := start_solomonskey;
    134:
      { 134: combat school }
      machine_calls.start := start_combatschool;
    135:
      { 135: heavy unit }
      machine_calls.start := start_heavyunit;
    136, 137, 149, 150:
      { 136: prisoners of war
        137: street smart
        149: ikari 3
        150: search and rescue }
      machine_calls.start := start_snk68;
    138, 139, 140:
      { 138: p-47 freedom fighter
        139: rodland
        140: saint dragon
        337: 64th street - a detective story }
      machine_calls.start := start_megasystem1;
    141:
      { 141: time pilot }
      machine_calls.start := start_timepilot;
    142:
      { 142: pengo }
      machine_calls.start := start_pengo;
    146, 147:
      { 146: twin cobra
        147: flying shark }
      machine_calls.start := start_twincobra;
    148:
      { 148: jr. pacman }
      machine_calls.start := start_jrpacman;
    156, 157, 158, 316, 317:
      { 156: robocop
        157: bad dudes vs dragon ninja
        158: hippodrome
        316: sly spy
        317: boulder dash }
      machine_calls.start := start_dec0;
    160:
      { 160: funky jet }
      machine_calls.start := start_funkyjet;
    159, 161:
      { 159: super burger time
        161: thumble pop }
      machine_calls.start := start_superbergertime;
    162, 163:
      { 162: caveman ninja
        163: robocop2 }
      machine_calls.start := start_cavemanninja;
    164:
      { 164: diet go go }
      machine_calls.start := start_dietgogo;
    165:
      { 165: actfancer }
      machine_calls.start := start_actfancer;
    166:
      { 166: arabian }
      machine_calls.start := start_arabian;
    170:
      { 170: pirate ship higemaru }
      machine_calls.start := start_pirateshiphigemaru;
    171, 172:
      { 171: bagman
        172: super bagman }
      machine_calls.start := start_bagman;
    175, 188, 346, 347:
      { 175: congo bongo
        188: zaxxon
        346: super zaxxon
        347: Future Spy }
      machine_calls.start := start_zaxxon;
    176:
      { 176: kangaroo }
      machine_calls.start := start_kangaroo;
    177:
      { 177: bionic commando }
      machine_calls.start := start_bioniccommando;
    178:
      { 178: wwf superstars }
      machine_calls.start := start_wwfsuperstars;
    179, 180:
      { 179: rainbow islands
        180: rainbow islands extra }
      machine_calls.start := start_rainbowislands;
    181:
      { 181: volfied }
      machine_calls.start := start_volfied;
    182:
      { 182: operation wolf }
      machine_calls.start := start_operationwolf;
    184:
      { 184: outrun }
      machine_calls.start := start_outrun;
    185, 189:
      { 185: elevator action
        189: jungle king }
      machine_calls.start := start_taitosj;
    195:
      { 195: vulgus }
      machine_calls.start := start_vulgus;
    196, 232:
      { 196: double dragon 3
        232: the combatribes }
      machine_calls.start := start_doubledragon3;
    197:
      { 197: blockout }
      machine_calls.start := start_blockout;
    199:
      { 199: food fight }
      machine_calls.start := start_foodfight;
    204, 205, 260, 261:
      { 204: nemesis
        205: tween bee
        260: galactic warriors
        261: salamander }
      machine_calls.start := start_nemesis;
    206, 207:
      { 206: pirates
        207: genix family }
      machine_calls.start := start_pirates;
    208:
      { 208 : juno first }
      machine_calls.start := start_junofirst;
    209:
      { 209: gyruss }
      machine_calls.start := start_gyruss;
    210:
      { 210: boogie wings }
      machine_calls.start := start_boogiewings;
    211, 271, 272, 273, 274:
      { 211: free kick
        271: gigas
        272: gigas mark 2
        273: omega
        274: perfect billiard }
      machine_calls.start := start_freekick;
    212:
      { 212: pinball action }
      machine_calls.start := start_pinballaction;
    213:
      { 213: renegade }
      machine_calls.start := start_renegade;
    214, 215:
      { 214: teenage mutant ninja turtles
        215: sunset riders }
      machine_calls.start := start_teenagemutantnijaturtles;
    216:
      { 216: gradius 3 }
      machine_calls.start := start_gradius3;
    217:
      { 217: space invaders }
      machine_calls.start := start_spaceinvaders;
    218, 348:
      { 218: centipede
        348: Millipede }
      machine_calls.start := start_centipede;
    219, 220:
      { 219: karnov
        220: chelnov }
      machine_calls.start := start_karnov;
    221:
      { 221: aliens }
      machine_calls.start := start_aliens;
    222, 223, 224:
      { 222: super cobra
        223: gang busters
        224: thunder cross }
      machine_calls.start := start_thunderx;
    225:
      { 225: the simpsons }
      machine_calls.start := start_simpsons;
    226:
      { 226: track 'n field }
      machine_calls.start := start_tracknfield;
    227, 400:
      { 227: hyper sports }
      { 400: road fighter }
      machine_calls.start := start_hypersports;
    228:
      { 228: megazone }
      machine_calls.start := start_megazone;
    229:
      { 229: space fire bird }
      machine_calls.start := start_spacefirebird;
    230:
      { 230: ajax }
      machine_calls.start := start_ajax;
    235:
      { 235: vendetta }
      machine_calls.start := start_vendetta;
    236, 245:
      { 236: gauntlet
        245: gauntlet 2 }
      machine_calls.start := start_gauntlet;
    237:
      { 237: sauro }
      machine_calls.start := start_sauro;
    238:
      { 238: cray climber }
      machine_calls.start := start_crazyclimber;
    239:
      { 239: return of the invaders }
      machine_calls.start := start_returnoftheinvaders;
    240:
      { 240: tetris }
      machine_calls.start := start_tetris;
    241, 242, 243, 279:
      { 241: ikari warriors
        242: athena
        243: tnk 3
        279: aso }
      machine_calls.start := start_snk;
    244, 263, 264:
      { 244: peter pack rat
        263: indiana jones and the temple of doom
        264: marble madness }
      machine_calls.start := start_atarisystem1;
    246, 248, 249, 321, 322, 323:
      { 246: defender
        248: mayday
        249: colony 7
        321: joust
        322: robotron
        323: stargate }
      machine_calls.start := start_williams;
    251, 252, 253, 254, 255, 256, 257:
      { 251: hangon jr.
        252: slap shooter
        253: fantasy zone 2
        254: opa opa
        255: tetris
        256: transformer
        257: riddle of pythagoras }
      machine_calls.start := start_systeme;
    258, 259:
      { 258: route 16
        259: speakres }
      machine_calls.start := start_route16;
    262:
      { 262: badlands }
      machine_calls.start := start_badlands;
    266, 267:
      { 266: galivan
        267: dangar }
      machine_calls.start := start_galivan;
    268, 269, 270:
      { 268: last duel
        269: mad gear
        270: led storm 2011 }
      machine_calls.start := start_lastduel;
    275, 276, 277, 278:
      { 275: armed formation
        276: terra cresta
        277: crazy climber 2
        278: legion }
      machine_calls.start := start_armedf;
    280:
      { 280: fire trap }
      machine_calls.start := start_firetrap;
    281, 282:
      { 281: 3x3 puzzle
        282: casanova }
      machine_calls.start := start_puzz3x3;
    283, 284:
      { 283: 1945k 3
        284: 96 flag rally }
      machine_calls.start := start_1945kiii;
    285, 286:
      { 285: blood bros
        286: sky smasher }
      machine_calls.start := start_bloodbros;
    287, 288:
      { 287: baraduke
        288: metro cross }
      machine_calls.start := start_baraduke;
    292, 293, 294, 295, 296, 297:
      { 292: altered beast
        293: golden axe
        294: dunamite dux
        295: e-swat
        296: passing shot
        297: aurail }
      machine_calls.start := start_system16b;
    298:
      { 298: taoplan1 }
      machine_calls.start := start_toaplan1;
    301:
      { 301: karate champ }
      machine_calls.start := start_karatechamp;
    302, 303, 304:
      { 302: thundercade
        303: twin eagle - revenge joe's brother
        304: thunder & lightning }
      machine_calls.start := start_seta;
    308, 309, 310, 311, 312, 313:
      { 308: mr. do castle
        309: do! run run
        310: mr. do wild ride
        311: jumpink jack
        312: kick rider
        313: indoor soccer }
      machine_calls.start := start_mrdocastle;
    314:
      { 314: crystal castles }
      machine_calls.start := start_crystalcastles;
    315:
      { 315: flower }
      machine_calls.start := start_flower;
    318:
      { 318: super dodge ball }
      machine_calls.start := start_superdodgeball;
    324:
      { 324: tapper }
      machine_calls.start := start_mcr;
    325:
      { 325: arkanoid }
      machine_calls.start := start_arkanoid;
    326:
      { 326: side arms }
      machine_calls.start := start_sidearms;
    327:
      { 327: the speed rumbler }
      machine_calls.start := start_speedrumbler;
    328:
      { 328: china gate }
      machine_calls.start := start_chinagate;
    329:
      { 329 : mag max }
      machine_calls.start := start_magmax;
    332:
      { 332: ambush }
      machine_calls.start := start_ambush;
    333:
      { 333: super duck }
      machine_calls.start := start_superduck;
    334, 335, 336:
      { 334: hang-on
        335: enduro racer
        336: space harrier }
      machine_calls.start := start_hangon;
    337:
      { 337: 64th Street - A detective story }
      machine_calls.start := start_megasystem1;
    338, 339:
      { 338: Shadow Warriors
        339: Wild Fang / Tecmo Knight
      }
      machine_calls.start := start_shadoww;
    340:
      { 340: Raiden }
      machine_calls.start := start_raiden;
    341, 342, 343:
      { 341 : Twins
        342 : Twins ED
        343 : Hot Block }
      machine_calls.start := start_twins;
    344, 345:
      { 344 : missile command
        345 : super missile attack }
      machine_calls.start := start_missilec;
    349:
      { 349 : gaplus }
      machine_calls.start := start_gaplus;
    354, 355:
      { 354 : wily tower
        355 : fighting basketball }
      machine_calls.start := start_irem_m63;
    356:
      { 356 : diverboy }
      machine_calls.start := start_diverboy;
    357:
      { 357 : mag smashers }
      machine_calls.start := start_mugsmash;
    358, 359, 360:
      { 358 : steel force
        359 : twin brats
        360 : mortal race }
      machine_calls.start := start_steelforce;
    361, 362:
      { 361 : bank panic
        362 : combat hawk }
      machine_calls.start := start_bankpanic;
    364, 365:
      { 364 : appoooh
        365 : robo wres 2001 }
      machine_calls.start := start_appoooh;
    367:
      { 367 : 88 games }
      machine_calls.start := start_hw88games;
    371, 372, 373, 374, 375:
      { 371 : bluehawk
        372 : the last day
        373 : gulf storm
        374 : pollux
        375 : flying tiger }
      machine_calls.start := start_dooyong;
    377, 378, 379:
      { 377 : blue print
        378 : saturn
        379 : grasspin }
      machine_calls.start := start_blueprint;
    380, 381:
      { 380 : bulgarx
        381 : zero point }
      machine_calls.start := start_unico;
    388, 389:
      { 388: kikikaikai
        389: kick and run
      }
      machine_calls.start := start_kikikaikai;
    390, 391:
      { 390: lasso
        391: chameleon }
      machine_calls.start := start_lasso;
    406:
      machine_calls.start := start_finalstarforce;
    { 406: final star force }
    407:
      machine_calls.start := start_wyvernf0;
    { 407: wyvern }
    // consolas
    1000:
      machine_calls.start := start_nes;
    1001:
      machine_calls.start := start_coleco;
    1003:
      machine_calls.start := start_chip8;
    1004:
      machine_calls.start := start_sms;
    1005:
      machine_calls.start := start_sg1000;
    1007:
      machine_calls.start := start_scv;
    1008:
      Cargar_genesis;
    // handhelds
    1002:
      machine_calls.start := start_gb;
    1006:
      machine_calls.start := start_gg;
    // gnw
    2000 .. 2002:
      cargar_gnw_510;
  end;
end;

end.
