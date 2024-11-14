unit thunderx_hw;

interface

uses
  WinApi.Windows,
  nz80,
  konami,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ym_2151,
  k052109,
  k051960,
  k007232,
  timer_engine;

function start_thunderx: boolean;

implementation

type
  tvideo_bank_func = procedure(valor: byte);
  tfunction_1f98 = procedure(valor: byte);

const
  // Super Contra
  scontra_rom: array [0 .. 1] of tipo_roms = ((n: '775-e02.k11'; l: $10000; p: 0; crc: $A61C0EAD),
    (n: '775-e03.k13'; l: $10000; p: $10000; crc: $00B02622));
  scontra_sound: tipo_roms = (n: '775-c01.bin'; l: $8000; p: 0; crc: $0CED785A);
  scontra_tiles: array [0 .. 11] of tipo_roms = ((n: '775-a07a.bin'; l: $20000; p: 0; crc: $E716BDF3),
    (n: '775-a07e.bin'; l: $20000; p: 1; crc: $0986E3A5), (n: '775-a08a.bin'; l: $20000; p: 2;
    crc: $3DDD11A4), (n: '775-a08e.bin'; l: $20000; p: 3; crc: $1007D963), (n: '775-f07c.bin'; l: $10000;
    p: $80000; crc: $B0B30915), (n: '775-f07g.bin'; l: $10000; p: $80001; crc: $FBED827D), (n: '775-f08c.bin';
    l: $10000; p: $80002; crc: $53ABDAEC), (n: '775-f08g.bin'; l: $10000; p: $80003; crc: $3DF85A6E),
    (n: '775-f07d.bin'; l: $10000; p: $C0000; crc: $F184BE8E), (n: '775-f07h.bin'; l: $10000; p: $C0001;
    crc: $7B56C348), (n: '775-f08d.bin'; l: $10000; p: $C0002; crc: $102DCACE), (n: '775-f08h.bin'; l: $10000;
    p: $C0003; crc: $AD9D7016));
  scontra_sprites: array [0 .. 15] of tipo_roms = ((n: '775-a05a.bin'; l: $10000; p: 0; crc: $A0767045),
    (n: '775-a05e.bin'; l: $10000; p: 1; crc: $2F656F08), (n: '775-a06a.bin'; l: $10000; p: 2;
    crc: $77A34AD0), (n: '775-a06e.bin'; l: $10000; p: 3; crc: $8A910C94), (n: '775-a05b.bin'; l: $10000;
    p: $40000; crc: $AB8AD4FD), (n: '775-a05f.bin'; l: $10000; p: $40001; crc: $1C0EB1B6), (n: '775-a06b.bin';
    l: $10000; p: $40002; crc: $563FB565), (n: '775-a06f.bin'; l: $10000; p: $40003; crc: $E14995C0),
    (n: '775-f05c.bin'; l: $10000; p: $80000; crc: $5647761E), (n: '775-f05g.bin'; l: $10000; p: $80001;
    crc: $A1692CCA), (n: '775-f06c.bin'; l: $10000; p: $80002; crc: $5EE6F3C1), (n: '775-f06g.bin'; l: $10000;
    p: $80003; crc: $2645274D), (n: '775-f05d.bin'; l: $10000; p: $C0000; crc: $AD676A6F), (n: '775-f05h.bin';
    l: $10000; p: $C0001; crc: $3F925BCF), (n: '775-f06d.bin'; l: $10000; p: $C0002; crc: $C8B764FA),
    (n: '775-f06h.bin'; l: $10000; p: $C0003; crc: $D6595F59));
  scontra_k007232: array [0 .. 7] of tipo_roms = ((n: '775-a04a.bin'; l: $10000; p: $0; crc: $7EFB2E0F),
    (n: '775-a04b.bin'; l: $10000; p: $10000; crc: $F41A2B33), (n: '775-a04c.bin'; l: $10000; p: $20000;
    crc: $E4E58F14), (n: '775-a04d.bin'; l: $10000; p: $30000; crc: $D46736F6), (n: '775-f04e.bin'; l: $10000;
    p: $40000; crc: $FBF7E363), (n: '775-f04f.bin'; l: $10000; p: $50000; crc: $B031EF2D), (n: '775-f04g.bin';
    l: $10000; p: $60000; crc: $EE107BBB), (n: '775-f04h.bin'; l: $10000; p: $70000; crc: $FB0FAB46));
  // Gang Busters
  gbusters_rom: array [0 .. 1] of tipo_roms = ((n: '878n02.k13'; l: $10000; p: 0; crc: $51697AAA),
    (n: '878j03.k15'; l: $10000; p: $10000; crc: $3943A065));
  gbusters_sound: tipo_roms = (n: '878h01.f8'; l: $8000; p: 0; crc: $96FEAFAA);
  gbusters_tiles: array [0 .. 1] of tipo_roms = ((n: '878c07.h27'; l: $40000; p: 0; crc: $EEED912C),
    (n: '878c08.k27'; l: $40000; p: 2; crc: $4D14626D));
  gbusters_sprites: array [0 .. 1] of tipo_roms = ((n: '878c05.h5'; l: $40000; p: 0; crc: $01F4AEA5),
    (n: '878c06.k5'; l: $40000; p: 2; crc: $EDFAAAAF));
  gbusters_k007232: tipo_roms = (n: '878c04.d5'; l: $40000; p: 0; crc: $9E982D1C);
  // Thunder Cross
  thunderx_rom: array [0 .. 1] of tipo_roms = ((n: '873-s02.k13'; l: $10000; p: 0; crc: $6619333A),
    (n: '873-s03.k15'; l: $10000; p: $10000; crc: $2AEC2699));
  thunderx_sound: tipo_roms = (n: '873-f01.f8'; l: $8000; p: 0; crc: $EA35FFA3);
  thunderx_tiles: array [0 .. 7] of tipo_roms = ((n: '873c06a.f6'; l: $10000; p: 0; crc: $0E340B67),
    (n: '873c06c.f5'; l: $10000; p: 1; crc: $EF0E72CD), (n: '873c07a.f4'; l: $10000; p: 2; crc: $A8AAB84F),
    (n: '873c07c.f3'; l: $10000; p: 3; crc: $2521009A), (n: '873c06b.e6'; l: $10000; p: $40000;
    crc: $97AD202E), (n: '873c06d.e5'; l: $10000; p: $40001; crc: $8393D42E), (n: '873c07b.e4'; l: $10000;
    p: $40002; crc: $12A2B8BA), (n: '873c07d.e3'; l: $10000; p: $40003; crc: $FAE9F965));
  thunderx_sprites: array [0 .. 7] of tipo_roms = ((n: '873c04a.f11'; l: $10000; p: 0; crc: $F7740BF3),
    (n: '873c04c.f10'; l: $10000; p: 1; crc: $5DACBD2B), (n: '873c05a.f9'; l: $10000; p: 2; crc: $D73E107D),
    (n: '873c05c.f8'; l: $10000; p: 3; crc: $59903200), (n: '873c04b.e11'; l: $10000; p: $40000;
    crc: $9AC581DA), (n: '873c04d.e10'; l: $10000; p: $40001; crc: $44A4668C), (n: '873c05b.e9'; l: $10000;
    p: $40002; crc: $81059B99), (n: '873c05d.e8'; l: $10000; p: $40003; crc: $7FA3D7DF));
  // DIP
  scontra_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
    dip: ((dip_val: $02; dip_name: '4C 1C'), (dip_val: $05; dip_name: '3C 1C'), (dip_val: $08;
    dip_name: '2C 1C'), (dip_val: $04; dip_name: '3C 2C'), (dip_val: $01; dip_name: '4C 3C'), (dip_val: $0F;
    dip_name: '1C 1C'), (dip_val: $03; dip_name: '3C 4C'), (dip_val: $07; dip_name: '2C 3C'), (dip_val: $0E;
    dip_name: '1C 2C'), (dip_val: $06; dip_name: '2C 5C'), (dip_val: $0D; dip_name: '1C 3C'), (dip_val: $0C;
    dip_name: '1C 4C'), (dip_val: $0B; dip_name: '1C 5C'), (dip_val: $0A; dip_name: '1C 6C'), (dip_val: $09;
    dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80;
    dip_name: '2C 1C'), (dip_val: $40; dip_name: '3C 2C'), (dip_val: $10; dip_name: '4C 3C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $30; dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0;
    dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0; dip_name: '1C 3C'), (dip_val: $C0;
    dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90;
    dip_name: '1C 7C'), (dip_val: $0; dip_name: 'No Coin'))), ());
  scontra_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '2'), (dip_val: $2; dip_name: '3'), (dip_val: $1;
    dip_name: '5'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $18; name: 'Bonus Life'; number: 4; dip: ((dip_val: $18; dip_name: '30K 200K'), (dip_val: $10;
    dip_name: '50K 300K'), (dip_val: $8; dip_name: '30K'), (dip_val: $0; dip_name: '50K'), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  scontra_dip_c: array [0 .. 2] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $8; name: 'Continue Limit 1P/2P'; number: 2;
    dip: ((dip_val: $8; dip_name: '3 Times/2 altogether'), (dip_val: $0; dip_name: '5 Times/4 altogether'),
    (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  gbusters_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '2'), (dip_val: $2; dip_name: '3'), (dip_val: $1;
    dip_name: '5'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4;
    name: 'Bullets'; number: 2; dip: ((dip_val: $4; dip_name: '50'), (dip_val: $0; dip_name: '60'), (), (),
    (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $18; dip_name: '30K 200K 400K+'), (dip_val: $10; dip_name: '70K 250K 500K+'),
    (dip_val: $8; dip_name: '50K'), (dip_val: $0; dip_name: '70K'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  gbusters_dip_c: array [0 .. 1] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  thunderx_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '2'), (dip_val: $2; dip_name: '3'), (dip_val: $1;
    dip_name: '5'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4;
    name: 'Award Bonus Life'; number: 2; dip: ((dip_val: $4; dip_name: 'No'), (dip_val: $0;
    dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $18; dip_name: '30K 200K'), (dip_val: $10;
    dip_name: '50K 300K'), (dip_val: $8; dip_name: '30K'), (dip_val: $0; dip_name: '50K'), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  layer_colorbase: array [0 .. 2] of byte = (48, 0, 16);

var
  tiles_rom, sprite_rom, k007232_rom: pbyte;
  sound_latch, sprite_colorbase, bank0_bank, rom_bank1, latch_1f98, priority, thunderx_timer: byte;
  rom_bank: array [0 .. $F, 0 .. $1FFF] of byte;
  pmc_ram: array [0 .. $7FF] of byte;
  video_bank_call: tvideo_bank_func;
  call_function_1f98: tfunction_1f98;

procedure scontra_videobank(valor: byte);
begin
  rom_bank1 := valor and $F;
  bank0_bank := (valor and $10) shr 4;
  priority := valor and $80;
end;

procedure gbusters_videobank(valor: byte);
begin
  bank0_bank := valor and $1;
  priority := valor and $8;
end;

procedure thunderx_videobank(valor: byte);
begin
  if (valor and $10) <> 0 then
    bank0_bank := 2 // PCM
  else
    bank0_bank := valor and $1; // 1 --> RAM 0 --> Paleta
  priority := valor and $8;
end;

procedure scontra_1f98_call(valor: byte);
begin
  if (valor and $1) <> 0 then
    k052109_0.set_rmrd_line(ASSERT_LINE)
  else
    k052109_0.set_rmrd_line(CLEAR_LINE);
  latch_1f98 := valor;
end;

procedure calculate_collisions;
var
  p0, p1, f, h, e0: word;
  s0, s1, e1, cm, hm, p0_lim: byte;
begin
  e0 := (pmc_ram[0] shl 8) + pmc_ram[1];
  e0 := (e0 - 15) div 5;
  e1 := (pmc_ram[2] - 15) div 5;
  s0 := (pmc_ram[5] - 16) div 5;
  s1 := (pmc_ram[6] - 16) div 5;
  cm := pmc_ram[3];
  hm := pmc_ram[4];
  p0 := (16 + 5 * s0) - 5;
  p0_lim := $E6;
  for f := s0 to (e0 - 1) do
  begin
    p0 := p0 + 5;
    if ((pmc_ram[p0 + 0] and cm) = 0) then
      continue;
    p1 := (16 + 5 * s1) - 5;
    for h := s1 to (e1 - 1) do
    begin
      p1 := p1 + 5;
      if ((pmc_ram[p1 + 0] and hm) = 0) then
        continue;
      if ((pmc_ram[p1 + 1] + pmc_ram[p0 + 1]) < abs(pmc_ram[p1 + 3] - pmc_ram[p0 + 3])) then
        continue;
      if ((pmc_ram[p1 + 2] + pmc_ram[p0 + 2]) < abs(pmc_ram[p1 + 4] - pmc_ram[p0 + 4])) then
        continue;
      // set flags
      pmc_ram[p1 + 0] := (pmc_ram[p1 + 0] and $8F) or $10;
      if (p0 + 4) >= p0_lim then
        pmc_ram[p0 + 0] := (pmc_ram[p0 + 0] and $9B) or (pmc_ram[p1 + 0] and $04) or $10;
      break;
    end;
  end;
end;

procedure thunderx_1f98_call(valor: byte);
begin
  if (valor and $1) <> 0 then
    k052109_0.set_rmrd_line(ASSERT_LINE)
  else
    k052109_0.set_rmrd_line(CLEAR_LINE);
  if (((valor and 4) <> 0) and ((latch_1f98 and 4) = 0)) then
  begin
    calculate_collisions;
    timers.enabled(thunderx_timer, true);
  end;
  latch_1f98 := valor;
end;

procedure thunderx_firq;
begin
  konami_0.change_firq(HOLD_LINE);
  timers.enabled(thunderx_timer, false);
end;

procedure thunderx_cb(layer, bank: word; var code: dword; var color: word; var flags: word;
  var priority: word);
begin
  code := code or (((color and $1F) shl 8) or (bank shl 13));
  color := layer_colorbase[layer] + ((color and $E0) shr 5);
end;

procedure gbusters_cb(layer, bank: word; var code: dword; var color: word; var flags: word;
  var priority: word);
begin
  // (color & 0x02) is flip y handled internally by the 052109
  code := code or (((color and $D) shl 8) or ((color and $10) shl 5) or (bank shl 12));
  color := layer_colorbase[layer] + ((color and $E0) shr 5);
end;

procedure thunderx_sprite_cb(var code: word; var color: word; var pri: word; var shadow: word);
begin
  // The PROM allows for mixed priorities, where sprites would have */
  // priority over text but not on one or both of the other two planes. */
  case (color and $30) of
    $0:
      pri := 0;
    $10:
      pri := 3;
    $20:
      pri := 2;
    $30:
      pri := 4;
  end;
  color := sprite_colorbase + (color and $F);
end;

procedure scontra_k007232_cb(valor: byte);
begin
  k007232_0.set_volume(0, (valor shr 4) * $11, 0);
  k007232_0.set_volume(1, 0, (valor and $F) * $11);
end;

procedure update_video_thunderx;
begin
  k052109_0.draw_tiles;
  k051960_0.update_sprites;
  fill_full_screen(4, layer_colorbase[1] * 16);
  k051960_0.draw_sprites(4, -1);
  if priority <> 0 then
  begin
    k052109_0.draw_layer(2, 4);
    k051960_0.draw_sprites(2, -1);
    k052109_0.draw_layer(1, 4);
  end
  else
  begin
    k052109_0.draw_layer(1, 4);
    k051960_0.draw_sprites(2, -1);
    k052109_0.draw_layer(2, 4);
  end;
  k051960_0.draw_sprites(0, -1);
  k052109_0.draw_layer(0, 4);
  k051960_0.draw_sprites(3, -1);
  update_final_piece(112, 16, 288, 224, 4);
end;

procedure events_thunderx;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[1] then
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
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure thunderx_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := konami_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        konami_0.run(frame_m);
        frame_m := frame_m + konami_0.tframes - konami_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          update_video_thunderx;
          if k052109_0.is_irq_enabled then
            konami_0.change_irq(HOLD_LINE);
        end;
      end;
      events_thunderx;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Main CPU
function thunderx_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $3FFF:
      case direccion of
        $1F90:
          thunderx_getbyte := marcade.in2; // system
        $1F91:
          thunderx_getbyte := marcade.in0; // p1
        $1F92:
          thunderx_getbyte := marcade.in1; // p2
        $1F93:
          thunderx_getbyte := marcade.dswc; // dsw3
        $1F94:
          thunderx_getbyte := marcade.dswa; // dsw1
        $1F95:
          thunderx_getbyte := marcade.dswb; // dsw2
        $1F98:
          thunderx_getbyte := latch_1f98;
      else
        if k052109_0.get_rmrd_line = CLEAR_LINE then
        begin
          case direccion of
            0 .. $1F8F, $1F96 .. $1F97, $1F99 .. $37FF, $3808 .. $3BFF:
              thunderx_getbyte := k052109_0.read(direccion);
            $3800 .. $3807:
              thunderx_getbyte := k051960_0.k051937_read(direccion - $3800);
            $3C00 .. $3FFF:
              thunderx_getbyte := k051960_0.read(direccion - $3C00);
          end;
        end
        else
          thunderx_getbyte := k052109_0.read(direccion);
      end;
    $4000 .. $57FF, $8000 .. $FFFF:
      thunderx_getbyte := memory[direccion];
    $5800 .. $5FFF:
      begin
        direccion := direccion and $7FF;
        case bank0_bank of
          0:
            thunderx_getbyte := buffer_paleta[direccion];
          1:
            thunderx_getbyte := memory[$5800 + direccion];
          2:
            if (latch_1f98 and 2) <> 0 then
              thunderx_getbyte := pmc_ram[direccion]
            else
              thunderx_getbyte := 0;
        end;
      end;
    $6000 .. $7FFF:
      thunderx_getbyte := rom_bank[rom_bank1, direccion and $1FFF];
  end;
end;

procedure thunderx_putbyte(direccion: word; valor: byte);
  procedure change_color(pos: word);
  var
    color: tcolor;
    valor: word;
  begin
    valor := (buffer_paleta[pos * 2] shl 8) + buffer_paleta[(pos * 2) + 1];
    color.b := pal5bit(valor shr 10);
    color.g := pal5bit(valor shr 5);
    color.r := pal5bit(valor);
    set_pal_color_alpha(color, pos);
    k052109_0.clean_video_buffer;
  end;

begin
  case direccion of
    $0 .. $3FFF:
      case direccion of
        0 .. $1F7F, $1F81 .. $1F83, $1F85 .. $1F87, $1F89 .. $1F97, $1F99 .. $37FF, $3808 .. $3BFF:
          k052109_0.write(direccion, valor);
        $1F80:
          video_bank_call(valor);
        $1F84:
          sound_latch := valor;
        $1F88:
          z80_0.change_irq(HOLD_LINE);
        $1F98:
          call_function_1f98(valor);
        $3800 .. $3807:
          k051960_0.k051937_write(direccion - $3800, valor);
        $3C00 .. $3FFF:
          k051960_0.write(direccion - $3C00, valor);
      end;
    $4000 .. $57FF:
      memory[direccion] := valor;
    $5800 .. $5FFF:
      begin
        direccion := direccion and $7FF;
        case bank0_bank of
          0:
            if buffer_paleta[direccion] <> valor then
            begin
              buffer_paleta[direccion] := valor;
              change_color(direccion shr 1);
            end;
          1:
            memory[$5800 + direccion] := valor;
          2:
            if (latch_1f98 and 2) <> 0 then
              pmc_ram[direccion] := valor;
        end;
      end;
    $6000 .. $FFFF:
      ; // ROM
  end;
end;

procedure thunderx_bank(valor: byte);
begin
  rom_bank1 := valor and $F;
end;

// Audio CPU
function thunderx_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      thunderx_snd_getbyte := mem_snd[direccion];
    $A000:
      thunderx_snd_getbyte := sound_latch;
    $C001:
      thunderx_snd_getbyte := ym2151_0.status;
  end;
end;

procedure thunderx_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $C000:
      ym2151_0.reg(valor);
    $C001:
      ym2151_0.write(valor);
  end;
end;

procedure thunderx_sound_update;
begin
  ym2151_0.update;
end;

function scontra_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      scontra_snd_getbyte := mem_snd[direccion];
    $A000:
      scontra_snd_getbyte := sound_latch;
    $B000 .. $B00D:
      scontra_snd_getbyte := k007232_0.read(direccion and $F);
    $C001:
      scontra_snd_getbyte := ym2151_0.status;
  end;
end;

procedure scontra_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $B000 .. $B00D:
      k007232_0.write(direccion and $F, valor);
    $C000:
      ym2151_0.reg(valor);
    $C001:
      ym2151_0.write(valor);
    $F000:
      k007232_0.set_bank(valor and $3, (valor shr 2) and $3);
  end;
end;

procedure scontra_sound_update;
begin
  ym2151_0.update;
  k007232_0.update;
end;

// Main
procedure reset_thunderx;
begin
  konami_0.reset;
  z80_0.reset;
  k052109_0.reset;
  ym2151_0.reset;
  k051960_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_latch := 0;
  bank0_bank := 0;
  rom_bank1 := 0;
  latch_1f98 := 0;
  priority := 0;
end;

procedure close_thunderx;
begin
  if main_vars.machine_type <> 224 then
    if k007232_rom <> nil then
      freemem(k007232_rom);
  if sprite_rom <> nil then
    freemem(sprite_rom);
  if tiles_rom <> nil then
    freemem(tiles_rom);
  k007232_rom := nil;
  sprite_rom := nil;
  tiles_rom := nil;
end;

function start_thunderx: boolean;
var
  temp_mem: array [0 .. $1FFFF] of byte;
  f: byte;
begin
  start_thunderx := false;
  machine_calls.close := close_thunderx;
  machine_calls.reset := reset_thunderx;
  machine_calls.general_loop := thunderx_loop;
  machine_calls.fps_max := 59.17;
  // Pantallas para el K052109
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 1024, 1024, false, true);
  if main_vars.machine_type <> 224 then
    main_screen.rot90_screen := true;
  start_video(288, 224, true);
  start_audio(false);
  // Main CPU
  konami_0 := cpu_konami.create(12000000, 256);
  konami_0.change_ram_calls(thunderx_getbyte, thunderx_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  case main_vars.machine_type of
    222:
      begin // Super contra
        call_function_1f98 := scontra_1f98_call;
        // cargar roms y ponerlas en su sitio...
        if not(roms_load(@temp_mem, scontra_rom)) then
          exit;
        copymemory(@memory[$8000], @temp_mem[$8000], $8000);
        for f := 0 to 3 do
        begin
          copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
          copymemory(@rom_bank[4 + f, 0], @temp_mem[f * $2000], $2000);
          // Estas son un mirror de las otras tres...
        end;
        for f := 8 to $F do
          copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, scontra_sound)) then
          exit;
        // Sound CPU
        z80_0.change_ram_calls(scontra_snd_getbyte, scontra_snd_putbyte);
        z80_0.init_sound(scontra_sound_update);
        // Sound Chips
        ym2151_0 := ym2151_chip.create(3579545);
        getmem(k007232_rom, $80000);
        if not(roms_load(k007232_rom, scontra_k007232)) then
          exit;
        k007232_0 := k007232_chip.create(3579545, k007232_rom, $80000, 0.20, scontra_k007232_cb);
        // Iniciar video
        video_bank_call := scontra_videobank;
        getmem(tiles_rom, $100000);
        if not(roms_load32b_b(tiles_rom, scontra_tiles)) then
          exit;
        k052109_0 := k052109_chip.create(1, 2, 3, 0, thunderx_cb, tiles_rom, $100000);
        getmem(sprite_rom, $100000);
        if not(roms_load32b_b(sprite_rom, scontra_sprites)) then
          exit;
        k051960_0 := k051960_chip.create(4, 1, sprite_rom, $100000, thunderx_sprite_cb, 2);
        // DIP
        marcade.dswa := $FF;
        marcade.dswa_val := @scontra_dip_a;
        marcade.dswb := $5A;
        marcade.dswb_val := @scontra_dip_b;
        marcade.dswc := $F7;
        marcade.dswc_val := @scontra_dip_c;
      end;
    223:
      begin // Gang Busters
        konami_0.change_set_lines(thunderx_bank);
        call_function_1f98 := scontra_1f98_call;
        // cargar roms y ponerlas en su sitio...
        if not(roms_load(@temp_mem, gbusters_rom)) then
          exit;
        copymemory(@memory[$8000], @temp_mem[$8000], $8000);
        for f := 0 to 3 do
        begin
          copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
          copymemory(@rom_bank[4 + f, 0], @temp_mem[f * $2000], $2000);
          // Estas son un mirror de las otras tres...
        end;
        for f := 8 to $F do
          copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, gbusters_sound)) then
          exit;
        // Sound CPU
        z80_0.change_ram_calls(scontra_snd_getbyte, scontra_snd_putbyte);
        z80_0.init_sound(scontra_sound_update);
        // Sound Chips
        ym2151_0 := ym2151_chip.create(3579545);
        getmem(k007232_rom, $40000);
        if not(roms_load(k007232_rom, gbusters_k007232)) then
          exit;
        k007232_0 := k007232_chip.create(3579545, k007232_rom, $40000, 0.20, scontra_k007232_cb);
        // Iniciar video
        video_bank_call := gbusters_videobank;
        getmem(tiles_rom, $80000);
        if not(roms_load32b(tiles_rom, gbusters_tiles)) then
          exit;
        k052109_0 := k052109_chip.create(1, 2, 3, 0, gbusters_cb, tiles_rom, $80000);
        getmem(sprite_rom, $80000);
        if not(roms_load32b(sprite_rom, gbusters_sprites)) then
          exit;
        k051960_0 := k051960_chip.create(4, 1, sprite_rom, $80000, thunderx_sprite_cb, 2);
        // DIP
        marcade.dswa := $FF;
        marcade.dswa_val := @scontra_dip_a;
        marcade.dswb := $56;
        marcade.dswb_val := @gbusters_dip_b;
        marcade.dswc := $FF;
        marcade.dswc_val := @gbusters_dip_c;
      end;
    224:
      begin // Thunder Cross
        konami_0.change_set_lines(thunderx_bank);
        call_function_1f98 := thunderx_1f98_call;
        // cargar roms y ponerlas en su sitio...
        if not(roms_load(@temp_mem, thunderx_rom)) then
          exit;
        copymemory(@memory[$8000], @temp_mem[$8000], $8000);
        for f := 0 to 3 do
        begin
          copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
          copymemory(@rom_bank[4 + f, 0], @temp_mem[f * $2000], $2000);
          // Estas son un mirror de las otras tres...
        end;
        for f := 8 to $F do
          copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
        // Despues de calcular las colisiones hay que llamar a FIRQ, pero hay que retrasarla 100T o se cuelga...
        thunderx_timer := timers.init(konami_0.numero_cpu, 100, thunderx_firq, nil, false);
        // cargar sonido
        if not(roms_load(@mem_snd, thunderx_sound)) then
          exit;
        // Sound CPU
        z80_0.change_ram_calls(thunderx_snd_getbyte, thunderx_snd_putbyte);
        z80_0.init_sound(thunderx_sound_update);
        // Sound Chips
        ym2151_0 := ym2151_chip.create(3579545);
        // Iniciar video
        video_bank_call := thunderx_videobank;
        getmem(tiles_rom, $80000);
        if not(roms_load32b_b(tiles_rom, thunderx_tiles)) then
          exit;
        k052109_0 := k052109_chip.create(1, 2, 3, 0, thunderx_cb, tiles_rom, $80000);
        getmem(sprite_rom, $80000);
        if not(roms_load32b_b(sprite_rom, thunderx_sprites)) then
          exit;
        k051960_0 := k051960_chip.create(4, 1, sprite_rom, $80000, thunderx_sprite_cb, 2);
        // DIP
        marcade.dswa := $FF;
        marcade.dswa_val := @scontra_dip_a;
        marcade.dswb := $7A;
        marcade.dswb_val := @thunderx_dip_b;
        marcade.dswc := $FF;
        marcade.dswc_val := @gbusters_dip_c;
      end;
  end;
  sprite_colorbase := 32;
  // final
  reset_thunderx;
  start_thunderx := true;
end;

end.
