unit legendarywings_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ym_2203,
  gfx_engine,
  msm5205,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  mcs51,
  oki6295;

function start_lwings: boolean;

implementation

const
  // legendary wings
  lwings_rom: array [0 .. 2] of tipo_roms = ((n: '6c_lw01.bin'; l: $8000; p: 0; crc: $B55A7F60), (n: '7c_lw02.bin'; l: $8000; p: $8000; crc: $A5EFBB1B), (n: '9c_lw03.bin'; l: $8000; p: $10000; crc: $EC5CC201));
  lwings_snd_rom: tipo_roms = (n: '11e_lw04.bin'; l: $8000; p: 0; crc: $A20337A2);
  lwings_char: tipo_roms = (n: '9h_lw05.bin'; l: $4000; p: 0; crc: $091D923C);
  lwings_sprites: array [0 .. 3] of tipo_roms = ((n: '3j_lw17.bin'; l: $8000; p: 0; crc: $5ED1BC9B), (n: '1j_lw11.bin'; l: $8000; p: $8000; crc: $2A0790D6), (n: '3h_lw16.bin'; l: $8000; p: $10000; crc: $E8834006), (n: '1h_lw10.bin'; l: $8000; p: $18000; crc: $B693F5A5));
  lwings_tiles: array [0 .. 7] of tipo_roms = ((n: '3e_lw14.bin'; l: $8000; p: 0; crc: $5436392C), (n: '1e_lw08.bin'; l: $8000; p: $8000; crc: $B491BBBB), (n: '3d_lw13.bin'; l: $8000; p: $10000; crc: $FDD1908A), (n: '1d_lw07.bin'; l: $8000; p: $18000; crc: $5C73D406),
    (n: '3b_lw12.bin'; l: $8000; p: $20000; crc: $32E17B3C), (n: '1b_lw06.bin'; l: $8000; p: $28000; crc: $52E533C1), (n: '3f_lw15.bin'; l: $8000; p: $30000; crc: $99E134BA), (n: '1f_lw09.bin'; l: $8000; p: $38000; crc: $C8F28777));
  // section Z
  sectionz_rom: array [0 .. 2] of tipo_roms = ((n: '6c_sz01.bin'; l: $8000; p: 0; crc: $69585125), (n: '7c_sz02.bin'; l: $8000; p: $8000; crc: $22F161B8), (n: '9c_sz03.bin'; l: $8000; p: $10000; crc: $4C7111ED));
  sectionz_snd_rom: tipo_roms = (n: '11e_sz04.bin'; l: $8000; p: 0; crc: $A6073566);
  sectionz_char: tipo_roms = (n: '9h_sz05.bin'; l: $4000; p: 0; crc: $3173BA2E);
  sectionz_sprites: array [0 .. 3] of tipo_roms = ((n: '3j_sz17.bin'; l: $8000; p: 0; crc: $8DF7B24A), (n: '1j_sz11.bin'; l: $8000; p: $8000; crc: $685D4C54), (n: '3h_sz16.bin'; l: $8000; p: $10000; crc: $500FF2BB), (n: '1h_sz10.bin'; l: $8000; p: $18000; crc: $00B3D244));
  sectionz_tiles: array [0 .. 7] of tipo_roms = ((n: '3e_sz14.bin'; l: $8000; p: 0; crc: $63782E30), (n: '1e_sz08.bin'; l: $8000; p: $8000; crc: $D57D9F13), (n: '3d_sz13.bin'; l: $8000; p: $10000; crc: $1B3D4D7F), (n: '1d_sz07.bin'; l: $8000; p: $18000; crc: $F5B3A29F),
    (n: '3b_sz12.bin'; l: $8000; p: $20000; crc: $11D47DFD), (n: '1b_sz06.bin'; l: $8000; p: $28000; crc: $DF703B68), (n: '3f_sz15.bin'; l: $8000; p: $30000; crc: $36BB9BF7), (n: '1f_sz09.bin'; l: $8000; p: $38000; crc: $DA8F06C9));
  // y mi favorito... TROJAN!!!, pues no me he dajao pasta ni na...
  trojan_rom: array [0 .. 2] of tipo_roms = ((n: 't4.10n'; l: $8000; p: 0; crc: $C1BBEB4E), (n: 't6.13n'; l: $8000; p: $8000; crc: $D49592EF), (n: 'tb_05.12n'; l: $8000; p: $10000; crc: $9273B264));
  trojan_snd_rom: tipo_roms = (n: 'tb_02.15h'; l: $8000; p: 0; crc: $21154797);
  trojan_adpcm: tipo_roms = (n: 'tb_01.6d'; l: $4000; p: 0; crc: $1C0F91B2);
  trojan_char: tipo_roms = (n: 'tb_03.8k'; l: $4000; p: 0; crc: $581A2B4C);
  trojan_sprites: array [0 .. 7] of tipo_roms = ((n: 'tb_18.7l'; l: $8000; p: 0; crc: $862C4713), (n: 'tb_16.3l'; l: $8000; p: $8000; crc: $D86F8CBD), (n: 'tb_17.5l'; l: $8000; p: $10000; crc: $12A73B3F), (n: 'tb_15.2l'; l: $8000; p: $18000; crc: $BB1A2769), (n: 'tb_22.7n';
    l: $8000; p: $20000; crc: $39DAAFD4), (n: 'tb_20.3n'; l: $8000; p: $28000; crc: $94615D2A), (n: 'tb_21.5n'; l: $8000; p: $30000; crc: $66C642BD), (n: 'tb_19.2n'; l: $8000; p: $38000; crc: $81D5AB36));
  trojan_tiles: array [0 .. 7] of tipo_roms = ((n: 'tb_13.6b'; l: $8000; p: 0; crc: $285A052B), (n: 'tb_09.6a'; l: $8000; p: $8000; crc: $AEB693F7), (n: 'tb_12.4b'; l: $8000; p: $10000; crc: $DFB0FE5C), (n: 'tb_08.4a'; l: $8000; p: $18000; crc: $D3A4C9D1), (n: 'tb_11.3b';
    l: $8000; p: $20000; crc: $00F0F4FD), (n: 'tb_07.3a'; l: $8000; p: $28000; crc: $DFF2EE02), (n: 'tb_14.8b'; l: $8000; p: $30000; crc: $14BFAC18), (n: 'tb_10.8a'; l: $8000; p: $38000; crc: $71BA8A6D));
  trojan_tiles2: array [0 .. 1] of tipo_roms = ((n: 'tb_25.15n'; l: $8000; p: 0; crc: $6E38C6FA), (n: 'tb_24.13n'; l: $8000; p: $8000; crc: $14FC6CF2));
  trojan_tile_map: tipo_roms = (n: 'tb_23.9n'; l: $8000; p: 0; crc: $EDA13C0E);
  // Avengers
  avengers_rom: array [0 .. 2] of tipo_roms = ((n: 'avu_04d.10n'; l: $8000; p: 0; crc: $A94AADCC), (n: 'avu_06d.13n'; l: $8000; p: $8000; crc: $39CD80BD), (n: 'avu_05d.12n'; l: $8000; p: $10000; crc: $06B1CEC9));
  avengers_snd_rom: tipo_roms = (n: 'av_02.15h'; l: $8000; p: 0; crc: $107A2E17);
  avengers_mcu: tipo_roms = (n: 'av.13k'; l: $1000; p: 0; crc: $505A0987);
  avengers_adpcm: tipo_roms = (n: 'av_01.6d'; l: $8000; p: 0; crc: $C1E5D258);
  avengers_char: tipo_roms = (n: 'av_03.8k'; l: $8000; p: 0; crc: $EFB5883E);
  avengers_sprites: array [0 .. 7] of tipo_roms = ((n: 'av_18.7l'; l: $8000; p: 0; crc: $3C876A17), (n: 'av_16.3l'; l: $8000; p: $8000; crc: $4B1FF3AC), (n: 'av_17.5l'; l: $8000; p: $10000; crc: $4EB543EF), (n: 'av_15.2l'; l: $8000; p: $18000; crc: $8041DE7F), (n: 'av_22.7n';
    l: $8000; p: $20000; crc: $BDAA8B22), (n: 'av_20.3n'; l: $8000; p: $28000; crc: $566E3059), (n: 'av_21.5n'; l: $8000; p: $30000; crc: $301059AA), (n: 'av_19.2n'; l: $8000; p: $38000; crc: $A00485EC));
  avengers_tiles: array [0 .. 7] of tipo_roms = ((n: 'av_13.6b'; l: $8000; p: 0; crc: $9B5FF305), (n: 'av_09.6a'; l: $8000; p: $8000; crc: $08323355), (n: 'av_12.4b'; l: $8000; p: $10000; crc: $6D5261BA), (n: 'av_08.4a'; l: $8000; p: $18000; crc: $A13D9F54), (n: 'av_11.3b';
    l: $8000; p: $20000; crc: $A2911D8B), (n: 'av_07.3a'; l: $8000; p: $28000; crc: $CDE78D32), (n: 'av_14.8b'; l: $8000; p: $30000; crc: $44AC2671), (n: 'av_10.8a'; l: $8000; p: $38000; crc: $B1A717CB));
  avengers_tiles2: array [0 .. 1] of tipo_roms = ((n: 'avu_25.15n'; l: $8000; p: 0; crc: $230D9E30), (n: 'avu_24.13n'; l: $8000; p: $8000; crc: $A6354024));
  avengers_tile_map: tipo_roms = (n: 'av_23.9n'; l: $8000; p: 0; crc: $C0A93EF6);
  // Fire Ball
  fball_rom: tipo_roms = (n: 'd4.bin'; l: $20000; p: 0; crc: $6122B3DC);
  fball_snd_rom: tipo_roms = (n: 'a05.bin'; l: $10000; p: 0; crc: $474DD19E);
  fball_char: tipo_roms = (n: 'j03.bin'; l: $10000; p: 0; crc: $BE11627F);
  fball_tiles: array [0 .. 3] of tipo_roms = ((n: 'e15.bin'; l: $20000; p: 0; crc: $89A761D2), (n: 'c15.bin'; l: $20000; p: $10000; crc: $0F77B03E), (n: 'b15.bin'; l: $20000; p: $20000; crc: $2169AD3E), (n: 'f15.bin'; l: $20000; p: $30000; crc: $34B3F9A2));
  fball_sprites: array [0 .. 1] of tipo_roms = ((n: 'j15.bin'; l: $20000; p: 0; crc: $ED7BE8E7), (n: 'h15.bin'; l: $20000; p: $20000; crc: $6FFB5433));
  fball_oki: array [0 .. 2] of tipo_roms = ((n: 'a03.bin'; l: $40000; p: 0; crc: $22B0D089), (n: 'a02.bin'; l: $40000; p: $40000; crc: $951D6579), (n: 'a01.bin'; l: $40000; p: $80000; crc: $020B5261));
  // DIPs
  lwings_dip_a: array [0 .. 4] of def_dip2 = ((mask: 2; name: 'Flip Screen'; number: 2; val2: (2, 0); name2: ('Off', 'On')), (mask: $C; name: 'Lives'; number: 4; val4: ($C, 4, 8, 0); name4: ('3', '4', '5', '6')), (mask: $30; name: 'Coin A'; number: 4; val4: (0, $20, $10, $30);
    name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $C0; name: 'Coin B'; number: 4; val4: (0, $C0, $40, $80); name4: ('2C 4C', '1C 1C', '1C 2C', '1C 3C')), ());
  lwings_dip_b: array [0 .. 4] of def_dip2 = ((mask: 6; name: 'Difficulty'; number: 4; val4: (2, 6, 4, 0); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: 8; name: 'Demo Sounds'; number: 2; val2: (0, 8); name2: ('Off', 'On')), (mask: $10; name: 'Allow Continue'; number: 2;
    val2: (0, $10); name2: ('No', 'Yes')), (mask: $E0; name: 'Bonus Life'; number: 8; val8: ($E0, $60, $A0, $20, $C0, $40, $80, 0); name8: ('20K 50K+', '20K 60K+', '20K 70K+', '30K 60K+', '30k 70k+', '30k 80k+', '40k 100k+', 'None')), ());
  sectionz_dip_a: array [0 .. 4] of def_dip2 = ((mask: 2; name: 'Flip Screen'; number: 2; val2: (2, 0); name2: ('Off', 'On')), (mask: $C; name: 'Lives'; number: 4; val4: (4, $C, 8, 0); name4: ('2', '3', '4', '5')), (mask: $30; name: 'Coin A'; number: 4; val4: (0, $20, $10, $30);
    name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $C0; name: 'Coin B'; number: 4; val4: (0, $C0, $40, $80); name4: ('2C 4C', '1C 1C', '1C 2C', '1C 3C')), ());
  sectionz_dip_b: array [0 .. 4] of def_dip2 = ((mask: 1; name: 'Allow Continue'; number: 2; val2: (0, 1); name2: ('No', 'Yes')), (mask: 6; name: 'Difficulty'; number: 4; val4: (2, 6, 4, 0); name4: ('Easy', 'Normal', 'Hard', 'Very Hard')), (mask: $38; name: 'Bonus Life';
    number: 8; val8: ($38, $18, $28, 8, $30, $10, $20, 0); name8: ('20K 50K', '20K 60K', '20K 70K', '30K 60K', '30K 70K', '30K 80K', '40K 100K', 'None')), (mask: $C0; name: 'Cabinet'; number: 4; val4: (0, $40, $C0, $80);
    name4: ('Upright One Player', 'Upright Two Player', 'Cocktail', 'Invalid')), ());
  trojan_dip_a: array [0 .. 2] of def_dip2 = ((mask: 3; name: 'Cabinet'; number: 4; val4: (0, 2, 3, 1); name4: ('Upright One Player', 'Upright Two Player', 'Cocktail', 'Invalid')), (mask: $1C; name: 'Bonus Life'; number: 8; val8: ($10, $C, 8, $1C, $18, $14, 4, 0);
    name8: ('20K 60K', '20K 70K', '20K 80K', '30K 60K', '30K 70K', '30K 80K', '40K 80K', 'None')), ());
  trojan_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coin A'; number: 4; val4: (0, 3, 2, 1); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $C; name: 'Coin B'; number: 4; val4: (0, 4, 8, $C); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $30; name: 'Lives';
    number: 4; val4: ($20, $30, $10, 0); name4: ('2', '3', '4', '5')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Allow Continue'; number: 2; val2: (0, $80); name2: ('No', 'Yes')), ());
  avengers_dip_a: array [0 .. 3] of def_dip2 = ((mask: 2; name: 'Flip Screen'; number: 2; val2: (2, 0); name2: ('Off', 'On')), (mask: $1C; name: 'Coin A'; number: 8; val8: (0, $10, 8, $1C, $C, $14, 4, $18);
    name8: ('4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 6C')), (mask: $E0; name: 'Coin B'; number: 8; val8: (0, $80, $40, $E0, $60, $A0, $20, $C0); name8: ('4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 6C')), ());
  avengers_dip_b: array [0 .. 5] of def_dip2 = ((mask: 1; name: 'Allow Continue'; number: 2; val2: (0, 1); name2: ('No', 'Yes')), (mask: 2; name: 'Demo Sounds'; number: 2; val2: (0, 2); name2: ('Off', 'On')), (mask: $C; name: 'Difficulty'; number: 4; val4: (4, $C, 8, 0);
    name4: ('Easy', 'Normal', 'Hard', 'Very Hard')), (mask: $30; name: 'Bonus Life'; number: 4; val4: ($30, $10, $20, 0); name4: ('20K 60K', '20K 70K', '20K 80K', '30K 80K')), (mask: $C0; name: 'Lives'; number: 4; val4: ($C0, $40, $80, 0); name4: ('3', '4', '5', '6')), ());
  fball_dip_a: array [0 .. 4] of def_dip2 = ((mask: 1; name: 'Difficulty'; number: 2; val2: (1, 0); name2: ('Easy', 'Hard')), (mask: 6; name: 'Lives'; number: 4; val4: (0, 2, 4, 6); name4: ('1', '2', '3', '4')), (mask: $18; name: 'Coinage'; number: 4; val4: (0, 8, $10, $18);
    name4: ('1C 1C', '1C 1C', '1C 2C', '1C 4C')), (mask: $20; name: 'Flip Screen'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), ());
  CPU_SYNC = 4;

var
  scroll_x, scroll_y: word;
  bank, sound_command, sound2_command: byte;
  mem_rom: array [0 .. 3, 0 .. $3FFF] of byte;
  irq_ena: boolean;
  // trojan
  trojan_map: array [0 .. $7FFF] of byte;
  scroll_x2, image: byte;
  pintar_image: boolean;
  // avengers mcu
  mcu_data: array [0 .. 1] of byte;
  mcu_latch: array [0 .. 2] of byte;
  soundstate, avengers_linea, mcu_control, adpcm_command: byte;
  sprt_avenger: boolean;
  // Fire ball
  sprite_bank, oki_bank: byte;
  oki_roms: array [0 .. 7, 0 .. $1FFFF] of byte;

procedure events_lwings;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure update_video_lw;
var
  f, color, nchar, x, y: word;
  attr: byte;
begin
  for f := $3FF downto 0 do
  begin
    // tiles
    attr := memory[$EC00 + f];
    color := attr and 7;
    if (gfx[2].buffer[f] or buffer_color[color + $10]) then
    begin
      x := f div 32;
      y := f mod 32;
      nchar := memory[$E800 + f] + ((attr and $E0) shl 3);
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 2, 2, (attr and 8) <> 0, (attr and $10) <> 0);
      gfx[2].buffer[f] := false;
    end;
    // Chars
    attr := memory[f + $E400];
    color := attr and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[f + $E000] + ((attr and $C0) shl 2);
      put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 2) + 512, 3, 0, (attr and $20) <> 0, (attr and $10) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 1, scroll_y, scroll_x);
  for f := $7F downto 0 do
  begin
    x := (buffer_sprites[3 + (f * 4)] + ((buffer_sprites[1 + (f * 4)] and 1) shl 8));
    y := buffer_sprites[2 + (f * 4)];
    if ((x or y) <> 0) then
    begin
      attr := buffer_sprites[1 + (f * 4)];
      nchar := buffer_sprites[(f * 4)] + ((attr and $C0) shl 2) + (sprite_bank * $400);
      color := (attr and $38) shl 1;
      put_gfx_sprite(nchar, color + 384, (attr and 2) <> 0, (attr and 4) <> 0, 1);
      update_gfx_sprite(x, y, 1, 1);
    end;
  end;
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 1);
  update_final_piece(0, 8, 256, 240, 1);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure lwings_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 255 do
      begin
        if f = 248 then
        begin
          if irq_ena then
            z80_0.change_irq_vector(HOLD_LINE, $D7);
          update_video_lw;
          copymemory(@buffer_sprites[0], @memory[$DE00], $200);
        end;
        // Main CPU
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // Sound CPU
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
      end;
      events_lwings;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Main CPU
function lwings_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $EFFF:
      lwings_getbyte := memory[direccion];
    $8000 .. $BFFF:
      lwings_getbyte := mem_rom[bank, direccion and $3FFF];
    $F000 .. $F7FF:
      lwings_getbyte := buffer_paleta[direccion and $7FF];
    $F808:
      lwings_getbyte := marcade.in0;
    $F809:
      lwings_getbyte := marcade.in1;
    $F80A:
      lwings_getbyte := marcade.in2;
    $F80B:
      lwings_getbyte := marcade.dswa;
    $F80C:
      lwings_getbyte := marcade.dswb;
    $F80D, $F80E:
      lwings_getbyte := $FF; // P3 y P4
  end;
end;

procedure lwings_putbyte(direccion: word; valor: byte);
  procedure cambiar_color(dir: word);
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[dir];
    color.r := pal4bit(tmp_color shr 4);
    color.g := pal4bit(tmp_color);
    tmp_color := buffer_paleta[dir + $400];
    color.b := pal4bit(tmp_color shr 4);
    set_pal_color(color, dir);
    case dir of
      0 .. $7F:
        buffer_color[((dir shr 4) and 7) + $10] := true;
      $200 .. $23F:
        buffer_color[(dir shr 2) and $F] := true;
    end;
  end;

begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $DFFF:
      memory[direccion] := valor;
    $E000 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $E800 .. $EFFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $F000 .. $F7FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        cambiar_color(direccion and $3FF);
      end;
    $F808:
      scroll_y := (scroll_y and $100) or valor;
    $F809:
      scroll_y := (scroll_y and $FF) or ((valor and 1) shl 8);
    $F80A:
      scroll_x := (scroll_x and $100) or valor;
    $F80B:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $F80C:
      sound_command := valor;
    $F80E:
      begin
        bank := (valor and 6) shr 1;
        irq_ena := (valor and 8) <> 0;
        main_screen.flip_main_screen := (valor and 1) = 0;
        if (valor and $20) <> 0 then
          z80_1.change_reset(ASSERT_LINE)
        else
          z80_1.change_reset(CLEAR_LINE);
        sprite_bank := (valor and $10) shr 4;
      end;
  end;
end;

// Sound CPU
function lwings_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $C7FF:
      lwings_snd_getbyte := mem_snd[direccion];
    $C800:
      lwings_snd_getbyte := sound_command;
    $E006:
      begin
        lwings_snd_getbyte := sound2_command or soundstate;
        soundstate := 0;
      end;
  end;
end;

procedure lwings_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $E000:
      ym2203_0.control(valor);
    $E001:
      ym2203_0.write(valor);
    $E002:
      ym2203_1.control(valor);
    $E003:
      ym2203_1.write(valor);
    $E006:
      sound2_command := valor;
  end;
end;

procedure lwings_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

procedure lwings_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
end;

// trojan
procedure update_video_trojan;
var
  f, color, nchar, x, y, tile_index, offsy: word;
  attr: byte;
  flipx, flipy: boolean;
begin
  // final 1  512x512 (por sprites)
  // tiles 2  512x512 pri 0
  // chars 3
  // tiles 4  pri 1
  // back 5
  if pintar_image then
  begin
    offsy := image * $20;
    for y := 0 to $F do
    begin
      offsy := offsy and $7FFF;
      for x := 0 to $1F do
      begin
        tile_index := offsy + (2 * x);
        attr := trojan_map[tile_index + 1];
        color := (attr and 7) shl 4;
        nchar := trojan_map[tile_index] + ((attr and $80) shl 1);
        put_gfx_flip(x * 16, y * 16, nchar, color, 5, 3, (attr and $30) <> 0, false);
      end;
      offsy := offsy + $800;
    end;
    pintar_image := false;
  end;
  scroll__x(5, 1, scroll_x2);
  for f := $3FF downto 0 do
  begin
    // tiles
    attr := memory[$EC00 + f];
    color := attr and 7;
    if (gfx[2].buffer[f] or buffer_color[color + $10]) then
    begin
      x := f div 32;
      y := f mod 32;
      nchar := memory[$E800 + f] + ((attr and $E0) shl 3);
      put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + 256, 2, 2, (attr and $10) <> 0, false);
      if (attr and 8) <> 0 then
        put_gfx_trans_flip_alt(x * 16, y * 16, nchar, (color shl 4) + 256, 4, 2, (attr and $10) <> 0, false, 0)
      else
        put_gfx_block_trans(x * 16, y * 16, 4, 16, 16);
      gfx[2].buffer[f] := false;
    end;
    // Chars
    attr := memory[f + $E400];
    color := attr and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[f + $E000] + ((attr and $C0) shl 2);
      put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 2) + 768, 3, 0, (attr and $20) <> 0, (attr and $10) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // Fondo con prioridad 0
  scroll_x_y(2, 1, scroll_x, scroll_y);
  // sprites
  for f := $5F downto 0 do
  begin
    x := (buffer_sprites[3 + (f * 4)] + ((buffer_sprites[1 + (f * 4)] and 1) shl 8));
    y := buffer_sprites[2 + (f * 4)];
    if (x or y) <> 0 then
    begin
      attr := buffer_sprites[1 + (f * 4)];
      nchar := buffer_sprites[(f * 4)] + ((attr and $20) shl 4) + ((attr and $40) shl 2) + ((attr and $80) shl 3);
      color := (attr and $E) shl 3;
      if sprt_avenger then
      begin
        flipx := false;
        flipy := (attr and $10) = 0;
      end
      else
      begin
        flipx := (attr and $10) <> 0;
        flipy := true;
      end;
      put_gfx_sprite(nchar, color + 640, flipx, flipy, 1);
      update_gfx_sprite(x, y, 1, 1);
    end;
  end;
  // Fondo con prioridad 1
  scroll_x_y(4, 1, scroll_x, scroll_y);
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 1);
  update_final_piece(0, 8, 256, 240, 1);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure trojan_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 255 do
      begin
        if f = 248 then
        begin
          if irq_ena then
            z80_0.change_irq_vector(HOLD_LINE, $D7);
          update_video_trojan;
          copymemory(@buffer_sprites[0], @memory[$DE00], $200);
        end;
        // Main Z80
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // Sound Z80
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
        // ADPCM Z80
        z80_2.run(frame_snd2);
        frame_snd2 := frame_snd2 + z80_2.tframes - z80_2.contador;
      end;
      events_lwings;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure cambiar_color_trojan(dir: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[dir];
  color.r := pal4bit(tmp_color shr 4);
  color.g := pal4bit(tmp_color);
  tmp_color := buffer_paleta[dir + $400];
  color.b := pal4bit(tmp_color shr 4);
  set_pal_color(color, dir);
  case dir of
    0 .. $7F:
      pintar_image := true;
    $100 .. $17F:
      buffer_color[((dir shr 4) and 7) + $10] := true;
    $300 .. $33F:
      buffer_color[(dir shr 2) and $F] := true;
  end;
end;

procedure trojan_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $DFFF:
      memory[direccion] := valor;
    $E000 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $E800 .. $EFFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $F000 .. $F7FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        cambiar_color_trojan(direccion and $3FF);
      end;
    $F800:
      scroll_x := (scroll_x and $100) or valor;
    $F801:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $F802:
      scroll_y := (scroll_y and $100) or valor;
    $F803:
      scroll_y := (scroll_y and $FF) or ((valor and 1) shl 8);
    $F804:
      scroll_x2 := valor;
    $F805:
      if image <> valor then
      begin
        image := valor;
        pintar_image := true;
      end;
    $F80C:
      sound_command := valor;
    $F80D:
      sound2_command := valor;
    $F80E:
      begin
        bank := (valor and 6) shr 1;
        irq_ena := (valor and 8) <> 0;
        main_screen.flip_main_screen := (valor and 1) = 0;
        if (valor and $20) <> 0 then
          z80_1.change_reset(ASSERT_LINE)
        else
          z80_1.change_reset(CLEAR_LINE);
      end;
  end;
end;

function trojan_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 0 then
    trojan_inbyte := sound2_command;
end;

procedure trojan_outbyte(puerto: word; valor: byte);
begin
  if (puerto and $FF) = 1 then
  begin
    msm5205_0.reset_w((valor and $80) <> 0);
    msm5205_0.data_w(valor);
    msm5205_0.vclk_w(true);
    msm5205_0.vclk_w(false);
  end;
end;

function trojan_misc_getbyte(direccion: word): byte;
begin
  trojan_misc_getbyte := msm5205_0.rom_data[direccion];
end;

procedure trojan_misc_putbyte(direccion: word; valor: byte);
begin
  // Nada que hacer!!!
end;

procedure trojan_adpcm_instruccion;
begin
  z80_2.change_irq(HOLD_LINE);
end;

procedure trojan_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
  msm5205_0.Update;
end;

// Avengers
procedure avengers_loop;
var
  h: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for avengers_linea := 0 to 255 do
      begin
        if avengers_linea = 248 then
        begin
          if irq_ena then
            z80_0.change_nmi(PULSE_LINE);
          update_video_trojan;
          copymemory(@buffer_sprites[0], @memory[$DE00], $200);
        end;
        for h := 1 to CPU_SYNC do
        begin
          // Main Z80
          z80_0.run(frame_main);
          frame_main := frame_main + z80_0.tframes - z80_0.contador;
          // Sound Z80
          z80_1.run(frame_snd);
          frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
          // ADPCM Z80
          z80_2.run(frame_snd2);
          frame_snd2 := frame_snd2 + z80_2.tframes - z80_2.contador;
          // MCU
          mcs51_0.run(frame_mcu);
          frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        end;
      end;
      events_lwings;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function avengers_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $EFFF:
      avengers_getbyte := memory[direccion];
    $8000 .. $BFFF:
      avengers_getbyte := mem_rom[bank, direccion and $3FFF];
    $F000 .. $F7FF:
      avengers_getbyte := buffer_paleta[direccion and $7FF];
    $F808:
      avengers_getbyte := marcade.in0;
    $F809:
      avengers_getbyte := marcade.in1;
    $F80A:
      avengers_getbyte := marcade.in2;
    $F80B:
      avengers_getbyte := marcade.dswa;
    $F80C:
      avengers_getbyte := marcade.dswb;
    $F80D:
      avengers_getbyte := mcu_latch[2];
  end;
end;

procedure avengers_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $DFFF:
      memory[direccion] := valor;
    $E000 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $E800 .. $EFFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $F000 .. $F7FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        cambiar_color_trojan(direccion and $3FF);
      end;
    $F800:
      scroll_x := (scroll_x and $100) or valor;
    $F801:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $F802:
      scroll_y := (scroll_y and $100) or valor;
    $F803:
      scroll_y := (scroll_y and $FF) or ((valor and 1) shl 8);
    $F804:
      scroll_x2 := valor;
    $F805:
      if image <> valor then
      begin
        image := valor;
        pintar_image := true;
      end;
    $F809:
      begin
        mcu_latch[0] := valor;
        mcs51_0.change_irq0(ASSERT_LINE);
      end;
    $F80C:
      mcu_latch[1] := valor;
    $F80D:
      adpcm_command := valor;
    $F80E:
      begin
        bank := (valor and 6) shr 1;
        irq_ena := (valor and 8) <> 0;
        main_screen.flip_main_screen := (valor and 1) = 0;
        if (valor and $20) <> 0 then
          z80_1.change_reset(ASSERT_LINE)
        else
          z80_1.change_reset(CLEAR_LINE);
      end;
  end;
end;

function avengers_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 0 then
    avengers_inbyte := adpcm_command;
end;

function avengers_in_port0: byte;
begin
  if (mcu_control and $80) = 0 then
    avengers_in_port0 := mcu_latch[0]
  else
    avengers_in_port0 := $FF;
end;

procedure avengers_out_port0(valor: byte);
begin
  mcu_data[0] := valor;
end;

function avengers_in_port1: byte;
begin
  avengers_in_port1 := avengers_linea;
end;

function avengers_in_port2: byte;
begin
  if (mcu_control and $80) = 0 then
    avengers_in_port2 := mcu_latch[1]
  else
    avengers_in_port2 := $FF;
end;

procedure avengers_out_port2(valor: byte);
begin
  mcu_data[1] := valor;
end;

procedure avengers_out_port3(valor: byte);
begin
  if (((mcu_control and $40) = 0) and ((valor and $40) <> 0)) then
  begin
    mcu_latch[2] := mcu_data[0];
    sound_command := mcu_data[1];
    soundstate := $80;
  end;
  if ((mcu_control and $80) <> (valor and $80)) then
    mcs51_0.change_irq0(CLEAR_LINE);
  mcu_control := valor;
end;

procedure avenger_m1(opcode: byte);
begin
  // Esto es importante para sincronizar el Z80 con la MCU... Si no, la paleta no va bien
  z80_0.contador := z80_0.contador + 2;
end;

// Fire Ball
procedure fball_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 248 then
        begin
          if irq_ena then
            z80_0.change_nmi(PULSE_LINE);
          update_video_lw;
          copymemory(@buffer_sprites[0], @memory[$DE00], $200);
        end;
        // Main CPU
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // Sound CPU
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
      end;
      events_lwings;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function fball_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $FFF, $C000 .. $C7FF:
      fball_snd_getbyte := mem_snd[direccion];
    $8000:
      fball_snd_getbyte := sound_command;
    $E000:
      fball_snd_getbyte := oki_6295_0.read;
  end;
end;

procedure fball_snd_putbyte(direccion: word; valor: byte);
var
  ptemp: pbyte;
begin
  case direccion of
    0 .. $FFF:
      ;
    $A000:
      begin
        oki_bank := (valor shr 1) and 7;
        ptemp := oki_6295_0.get_rom_addr;
        copymemory(@ptemp[$20000], @oki_roms[oki_bank, 0], $20000);
      end;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $E000:
      oki_6295_0.write(valor);
  end;
end;

procedure fball_sound_update;
begin
  oki_6295_0.Update;
end;

// Main
procedure reset_lwings;
begin
  z80_0.reset;
  z80_1.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  if main_vars.machine_type <> 247 then
  begin
    ym2203_0.reset;
    ym2203_1.reset;
  end
  else
  begin
    oki_6295_0.reset;
  end;
  if ((main_vars.machine_type = 61) or (main_vars.machine_type = 368)) then
  begin
    z80_2.reset;
    msm5205_0.reset;
    frame_snd2 := z80_2.tframes;
  end;
  if main_vars.machine_type = 368 then
  begin
    mcs51_0.reset;
    frame_mcu := mcs51_0.tframes;
  end;
 reset_game_general;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  irq_ena := false;
  // trojan
  image := $FF;
  pintar_image := true;
  scroll_x2 := 0;
  adpcm_command := 0;
  // Avengers
  mcu_data[0] := 0;
  mcu_data[1] := 0;
  mcu_latch[0] := 0;
  mcu_latch[1] := 0;
  mcu_latch[2] := 0;
  soundstate := 0;
  mcu_control := 0;
  // Fire Ball
  oki_bank := 0;
  sprite_bank := 0;
end;

function start_lwings: boolean;
var
  f: word;
  memory_temp: array [0 .. $BFFFF] of byte;
  ptemp: pbyte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  procedure convert_chars_lw(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[3] := true;
    gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
    convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;
  procedure convert_sprites_lw(num: word);
  begin
    init_gfx(1, 16, 16, num);
    gfx[1].trans[15] := true;
    gfx_set_desc_data(4, 0, 64 * 8, num * 64 * 8 + 4, num * 64 * 8 + 0, 4, 0);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;
  procedure convert_tiles_lw;
  begin
    init_gfx(2, 16, 16, $800);
    gfx_set_desc_data(4, 0, 32 * 8, $30000 * 8, $20000 * 8, $10000 * 8, 0 * 8);
    convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
  end;
  procedure convert_tiles2_lw;
  begin
    init_gfx(3, 16, 16, $200);
    gfx_set_desc_data(4, 0, 64 * 8, $8000 * 8 + 0, $8000 * 8 + 4, 0, 4);
    convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.reset := reset_lwings;
  start_lwings := false;
  start_audio(false);
  // final 1  512x512 (por sprites)
  // tiles 2  512x512 pri 0
  // chars 3
  // tiles 4  pri 1
  screen_init(1, 512, 512, false, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 256, 256, true);
  case main_vars.machine_type of
    59:
      main_screen.rot270_screen := true;
    60:
      machine_calls.fps_max := 55.37;
    61, 368:
      begin
        // La pantallas 2 (la cambio) y 4 son transparentes
        screen_init(2, 512, 512, true);
        screen_init(4, 512, 512, true);
        screen_mod_scroll(4, 512, 256, 511, 512, 256, 511);
        // La pantalla 5 es el fondo
        screen_init(5, 512, 256);
        screen_mod_scroll(5, 512, 256, 511, 256, 256, 255);
        if main_vars.machine_type = 368 then
          main_screen.rot270_screen := true;
      end;
  end;
  start_video(256, 240);
  // Sound CPU
  case main_vars.machine_type of
    59, 60:
      begin
        z80_1 := cpu_z80.create(3000000, 256);
        z80_1.init_sound(lwings_sound_update);
      end;
    61:
      begin
        z80_1 := cpu_z80.create(3000000, 256);
        z80_1.init_sound(trojan_sound_update);
      end;
    247:
      begin
        z80_1 := cpu_z80.create(3000000, 256);
        z80_1.init_sound(fball_sound_update);
      end;
    368:
      begin
        z80_1 := cpu_z80.create(3000000, 256 * CPU_SYNC);
        z80_1.init_sound(trojan_sound_update);
      end;
  end;
  if main_vars.machine_type <> 247 then
  begin
    z80_1.change_ram_calls(lwings_snd_getbyte, lwings_snd_putbyte);
    timers.init(z80_1.numero_cpu, 3000000 / 222, lwings_snd_irq, nil, true);
    // Sound Chips
    ym2203_0 := ym2203_chip.create(1500000, 0.50, 1);
    ym2203_1 := ym2203_chip.create(1500000, 0.50, 1);
  end
  else
  begin
    z80_1.change_ram_calls(fball_snd_getbyte, fball_snd_putbyte);
    oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH);
  end;
  sprt_avenger := false;
  case main_vars.machine_type of
    59:
      begin // Legendary Wings
        machine_calls.general_loop := lwings_loop;
        // Main CPU
        z80_0 := cpu_z80.create(6000000, 256);
        z80_0.change_ram_calls(lwings_getbyte, lwings_putbyte);
        if not(roms_load(@memory_temp, lwings_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, lwings_snd_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, lwings_char)) then
          exit;
        convert_chars_lw($400);
        // convertir sprites
        if not(roms_load(@memory_temp, lwings_sprites)) then
          exit;
        convert_sprites_lw($400);
        // tiles
        if not(roms_load(@memory_temp, lwings_tiles)) then
          exit;
        convert_tiles_lw; // $800
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @lwings_dip_a;
        marcade.dswb_val2 := @lwings_dip_b;
      end;
    60:
      begin // Section Z
        machine_calls.general_loop := lwings_loop;
        // Main CPU
        z80_0 := cpu_z80.create(3000000, 256);
        z80_0.change_ram_calls(lwings_getbyte, lwings_putbyte);
        if not(roms_load(@memory_temp, sectionz_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, sectionz_snd_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, sectionz_char)) then
          exit;
        convert_chars_lw($400);
        // convertir sprites
        if not(roms_load(@memory_temp, sectionz_sprites)) then
          exit;
        convert_sprites_lw($400);
        // tiles
        if not(roms_load(@memory_temp, sectionz_tiles)) then
          exit;
        convert_tiles_lw; // $800
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $3F;
        marcade.dswa_val2 := @sectionz_dip_a;
        marcade.dswb_val2 := @sectionz_dip_b;
      end;
    61:
      begin // Trojan
        machine_calls.general_loop := trojan_loop;
        // Main CPU
        z80_0 := cpu_z80.create(3000000, 256);
        z80_0.change_ram_calls(lwings_getbyte, trojan_putbyte);
        // ADPCM Z80
        z80_2 := cpu_z80.create(3000000, 256);
        z80_2.change_ram_calls(trojan_misc_getbyte, trojan_misc_putbyte);
        z80_2.change_io_calls(trojan_inbyte, trojan_outbyte);
        msm5205_0 := MSM5205_chip.create(384000, MSM5205_SEX_4B, 0.50, $4000);
        if not(roms_load(msm5205_0.rom_data, trojan_adpcm)) then
          exit;
        msm5205_0.change_advance(nil);
        timers.init(z80_2.numero_cpu, 3000000 / 4000, trojan_adpcm_instruccion, nil, true);
        // Graficos
        if not(roms_load(@memory_temp, trojan_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, trojan_snd_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, trojan_char)) then
          exit;
        convert_chars_lw($400);
        // convertir sprites
        if not(roms_load(@memory_temp, trojan_sprites)) then
          exit;
        convert_sprites_lw($800);
        // tiles
        if not(roms_load(@memory_temp, trojan_tiles)) then
          exit;
        convert_tiles_lw; // $800
        for f := 0 to 6 do
          gfx[2].trans_alt[0, f] := true;
        for f := 12 to 15 do
          gfx[2].trans_alt[0, f] := true;
        gfx[2].trans[0] := true;
        // tiles 2
        if not(roms_load(@memory_temp, trojan_tiles2)) then
          exit;
        convert_tiles2_lw;
        // Map
        if not(roms_load(@trojan_map, trojan_tile_map)) then
          exit;
        // DIP
        marcade.dswa := $FC;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @trojan_dip_a;
        marcade.dswb_val2 := @trojan_dip_b;
      end;
    247:
      begin // Fire ball
        machine_calls.general_loop := fball_loop;
        // Main CPU
        z80_0 := cpu_z80.create(6000000, 256);
        z80_0.change_ram_calls(lwings_getbyte, lwings_putbyte);
        if not(roms_load(@memory_temp, fball_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$10000 + (f * $4000)], $4000);
        // cargar ROMS sonido
        if not(roms_load(@memory_temp, fball_snd_rom)) then
          exit;
        copymemory(@mem_snd, @memory_temp, $1000);
        if not(roms_load(@memory_temp, fball_oki)) then
          exit;
        ptemp := oki_6295_0.get_rom_addr;
        copymemory(ptemp, @memory_temp[0], $40000);
        copymemory(@oki_roms[0, 0], @memory_temp[0], $20000);
        copymemory(@oki_roms[1, 0], @memory_temp[$20000], $20000);
        copymemory(@oki_roms[2, 0], @memory_temp[0], $20000);
        copymemory(@oki_roms[3, 0], @memory_temp[$20000], $20000);
        for f := 4 to 7 do
          copymemory(@oki_roms[f, 0], @memory_temp[$40000 + ((f - 4) * $20000)], $20000);
        // convertir chars
        if not(roms_load(@memory_temp, fball_char)) then
          exit;
        fillchar(memory_temp[$4000], $C000, $FF);
        convert_chars_lw($400);
        // convertir sprites
        if not(roms_load(@memory_temp, fball_sprites)) then
          exit;
        convert_sprites_lw($800);
        // tiles
        if not(roms_load(@memory_temp, fball_tiles)) then
          exit;
        convert_tiles_lw; // $800
        // DIP
        marcade.dswa := $6D;
        marcade.dswb := 0;
        marcade.dswa_val2 := @fball_dip_a;
      end;
    368:
      begin
        machine_calls.general_loop := avengers_loop;
        // Main CPU
        z80_0 := cpu_z80.create(6000000, 256 * CPU_SYNC);
        z80_0.change_ram_calls(avengers_getbyte, avengers_putbyte);
        z80_0.change_misc_calls(nil, nil, avenger_m1);
        if not(roms_load(@memory_temp, avengers_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        for f := 0 to 3 do
          copymemory(@mem_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
        // MCU
        mcs51_0 := cpu_mcs51.create(I8X51, 6000000, 256 * CPU_SYNC);
        mcs51_0.change_io_calls(avengers_in_port0, avengers_in_port1, avengers_in_port2, nil, avengers_out_port0, nil, avengers_out_port2, avengers_out_port3);
        if not(roms_load(@memory_temp, avengers_mcu)) then
          exit;
        memory_temp[$B84] := 2;
        copymemory(mcs51_0.get_rom_addr, @memory_temp, $1000);
        // ADPCM Z80
        z80_2 := cpu_z80.create(3000000, 256 * CPU_SYNC);
        z80_2.change_ram_calls(trojan_misc_getbyte, trojan_misc_putbyte);
        z80_2.change_io_calls(avengers_inbyte, trojan_outbyte);
        msm5205_0 := MSM5205_chip.create(384000, MSM5205_SEX_4B, 0.50, $8000);
        if not(roms_load(msm5205_0.rom_data, avengers_adpcm)) then
          exit;
        msm5205_0.change_advance(nil);
        timers.init(z80_2.numero_cpu, 3000000 / 4000, trojan_adpcm_instruccion, nil, true);
        // cargar ROMS sonido
        if not(roms_load(@mem_snd, avengers_snd_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, avengers_char)) then
          exit;
        convert_chars_lw($800);
        // convertir sprites
        if not(roms_load(@memory_temp, avengers_sprites)) then
          exit;
        convert_sprites_lw($800);
        sprt_avenger := true;
        // tiles
        if not(roms_load(@memory_temp, avengers_tiles)) then
          exit;
        convert_tiles_lw;
        for f := 0 to 6 do
          gfx[2].trans_alt[0, f] := true;
        for f := 12 to 15 do
          gfx[2].trans_alt[0, f] := true;
        gfx[2].trans[0] := true;
        // tiles 2
        if not(roms_load(@memory_temp, avengers_tiles2)) then
          exit;
        convert_tiles2_lw;
        // Map
        if not(roms_load(@trojan_map, avengers_tile_map)) then
          exit;
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $FF;
        marcade.dswa_val2 := @avengers_dip_a;
        marcade.dswb_val2 := @avengers_dip_b;
      end;
  end;
  // final
  reset_lwings;
  start_lwings := true;
end;

end.
