unit pacman_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  namco_snd,
  controls_engine,
  gfx_engine,
  rom_engine,
  misc_functions,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_pacman: boolean;

implementation

const
  // Pacman
  pacman_rom: array [0 .. 3] of tipo_roms = ((n: 'pacman.6e'; l: $1000; p: 0; crc: $C1E6AB10), (n: 'pacman.6f'; l: $1000; p: $1000; crc: $1A6FB2D4), (n: 'pacman.6h'; l: $1000; p: $2000; crc: $BCDD1BEB), (n: 'pacman.6j'; l: $1000; p: $3000; crc: $817D94E3));
  pacman_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: '82s126.4a'; l: $100; p: $20; crc: $3EB3A8E4));
  pacman_char: tipo_roms = (n: 'pacman.5e'; l: $1000; p: 0; crc: $0C944964);
  pacman_sound: tipo_roms = (n: '82s126.1m'; l: $100; p: 0; crc: $A9CC86BF);
  pacman_sprites: tipo_roms = (n: 'pacman.5f'; l: $1000; p: 0; crc: $958FEDF9);
  // MS-Pacman
  mspacman_rom: array [0 .. 6] of tipo_roms = ((n: 'pacman.6e'; l: $1000; p: 0; crc: $C1E6AB10), (n: 'pacman.6f'; l: $1000; p: $1000; crc: $1A6FB2D4), (n: 'pacman.6h'; l: $1000; p: $2000; crc: $BCDD1BEB), (n: 'pacman.6j'; l: $1000; p: $3000; crc: $817D94E3), (n: 'u5'; l: $800;
    p: $8000; crc: $F45FBBCD), (n: 'u6'; l: $1000; p: $9000; crc: $A90E7000), (n: 'u7'; l: $1000; p: $B000; crc: $C82CD714));
  mspacman_char: tipo_roms = (n: '5e'; l: $1000; p: 0; crc: $5C281D01);
  mspacman_sprites: tipo_roms = (n: '5f'; l: $1000; p: 0; crc: $615AF909);
  // Crush Roller
  crush_rom: array [0 .. 3] of tipo_roms = ((n: 'crushkrl.6e'; l: $1000; p: 0; crc: $A8DD8F54), (n: 'crushkrl.6f'; l: $1000; p: $1000; crc: $91387299), (n: 'crushkrl.6h'; l: $1000; p: $2000; crc: $D4455F27), (n: 'crushkrl.6j'; l: $1000; p: $3000; crc: $D59FC251));
  crush_char: tipo_roms = (n: 'maketrax.5e'; l: $1000; p: 0; crc: $91BAD2DA);
  crush_sprites: tipo_roms = (n: 'maketrax.5f'; l: $1000; p: 0; crc: $AEA79F55);
  crush_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: '2s140.4a'; l: $100; p: $20; crc: $63EFB927));
  // Ms Pac Man Twin
  mspactwin_rom: tipo_roms = (n: 'm27256.bin'; l: $8000; p: 0; crc: $77A99184);
  mspactwin_char: array [0 .. 1] of tipo_roms = ((n: '4__2716.5d'; l: $800; p: 0; crc: $483C1D1C), (n: '2__2716.5g'; l: $800; p: $800; crc: $C08D73A2));
  mspactwin_sprites: array [0 .. 1] of tipo_roms = ((n: '3__2516.5f'; l: $800; p: 0; crc: $22B0188A), (n: '1__2516.5j'; l: $800; p: $800; crc: $0A8C46A0));
  mspactwin_pal: array [0 .. 1] of tipo_roms = ((n: 'mb7051.8h'; l: $20; p: 0; crc: $FF344446), (n: '82s129.4a'; l: $100; p: $20; crc: $A8202D0D));
  // Birdiy
  birdiy_rom: array [0 .. 3] of tipo_roms = ((n: 'a6.6a'; l: $1000; p: 0; crc: $3A58F8AD), (n: 'c6.6c'; l: $1000; p: $1000; crc: $FEC61EA2), (n: 'a4.4a'; l: $1000; p: $2000; crc: $3392783B), (n: 'c4.4c'; l: $1000; p: $3000; crc: $2391D83D));
  birdiy_pal: array [0 .. 1] of tipo_roms = ((n: 'n82s123n.10n'; l: $20; p: 0; crc: $FF344446), (n: 'n82s129n.9m'; l: $100; p: $20; crc: $63EFB927));
  birdiy_char: tipo_roms = (n: 'c1.1c'; l: $1000; p: 0; crc: $8F6BF54F);
  birdiy_sprites: tipo_roms = (n: 'c3.3c'; l: $1000; p: 0; crc: $10B55440);
  // Ponpoko
  ponpoko_rom: array [0 .. 7] of tipo_roms = ((n: 'ppokoj1.bin'; l: $1000; p: 0; crc: $FFA3C004), (n: 'ppokoj2.bin'; l: $1000; p: $1000; crc: $4A496866), (n: 'ppokoj3.bin'; l: $1000; p: $2000; crc: $17DA6CA3), (n: 'ppokoj4.bin'; l: $1000; p: $3000; crc: $9D39A565),
    (n: 'ppoko5.bin'; l: $1000; p: $8000; crc: $54CA3D7D), (n: 'ppoko6.bin'; l: $1000; p: $9000; crc: $3055C7E0), (n: 'ppoko7.bin'; l: $1000; p: $A000; crc: $3CBE47CA), (n: 'ppokoj8.bin'; l: $1000; p: $B000; crc: $04B63FC6));
  ponpoko_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: '82s126.4a'; l: $100; p: $20; crc: $3EB3A8E4));
  ponpoko_char: tipo_roms = (n: 'ppoko9.bin'; l: $1000; p: 0; crc: $B73E1A06);
  ponpoko_sprites: tipo_roms = (n: 'ppoko10.bin'; l: $1000; p: 0; crc: $62069B5D);
  // Woodpecker
  woodpeck_rom: array [0 .. 4] of tipo_roms = ((n: 'f.bin'; l: $1000; p: 0; crc: $37EA66CA), (n: 'i.bin'; l: $1000; p: $8000; crc: $CD115DBA), (n: 'e.bin'; l: $1000; p: $9000; crc: $D40B2321), (n: 'g.bin'; l: $1000; p: $A000; crc: $024092F4), (n: 'h.bin'; l: $1000; p: $B000;
    crc: $18EF0FC8));
  woodpeck_pal: array [0 .. 1] of tipo_roms = ((n: 'pr.8h'; l: $20; p: 0; crc: $2FC650BD), (n: 'pr.4a'; l: $100; p: $20; crc: $D8772167));
  woodpeck_char: array [0 .. 1] of tipo_roms = ((n: 'a.5e'; l: $800; p: 0; crc: $15A87F62), (n: 'c.5h'; l: $800; p: $800; crc: $AB4ABD88));
  woodpeck_sprites: array [0 .. 1] of tipo_roms = ((n: 'b.5f'; l: $800; p: 0; crc: $5B9BA95B), (n: 'd.5j'; l: $800; p: $800; crc: $D7B80A45));
  // Eyes
  eyes_rom: array [0 .. 3] of tipo_roms = ((n: 'd7'; l: $1000; p: 0; crc: $3B09AC89), (n: 'e7'; l: $1000; p: $1000; crc: $97096855), (n: 'f7'; l: $1000; p: $2000; crc: $731E294E), (n: 'h7'; l: $1000; p: $3000; crc: $22F7A719));
  eyes_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: '82s129.4a'; l: $100; p: $20; crc: $D8D78829));
  eyes_char: tipo_roms = (n: 'd5'; l: $1000; p: 0; crc: $D6AF0030);
  eyes_sprites: tipo_roms = (n: 'e5'; l: $1000; p: 0; crc: $A42B5201);
  // Alibaba
  alibaba_rom: array [0 .. 5] of tipo_roms = ((n: '6e'; l: $1000; p: 0; crc: $38D701AA), (n: '6f'; l: $1000; p: $1000; crc: $3D0E35F3), (n: '6h'; l: $1000; p: $2000; crc: $823BEE89), (n: '6k'; l: $1000; p: $3000; crc: $474D032F), (n: '6l'; l: $1000; p: $8000; crc: $5AB315C1),
    (n: '6m'; l: $800; p: $A000; crc: $438D0357));
  alibaba_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.e7'; l: $20; p: 0; crc: $2FC650BD), (n: '82s129.a4'; l: $100; p: $20; crc: $3EB3A8E4));
  alibaba_char: array [0 .. 1] of tipo_roms = ((n: '5e'; l: $800; p: 0; crc: $85BCB8F8), (n: '5h'; l: $800; p: $800; crc: $38E50862));
  alibaba_sprites: array [0 .. 1] of tipo_roms = ((n: '5f'; l: $800; p: 0; crc: $B5715C86), (n: '5k'; l: $800; p: $800; crc: $713086B3));
  // Piranha
  piranha_rom: array [0 .. 7] of tipo_roms = ((n: 'pir1.7e'; l: $800; p: 0; crc: $69A3E6EA), (n: 'pir5.6e'; l: $800; p: $800; crc: $245E753F), (n: 'pir2.7f'; l: $800; p: $1000; crc: $62CB6954), (n: 'pir6.6f'; l: $800; p: $1800; crc: $CB0700BC), (n: 'pir3.7h'; l: $800; p: $2000;
    crc: $843FBFE5), (n: 'pir7.6h'; l: $800; p: $2800; crc: $73084D5E), (n: 'pir4.7j'; l: $800; p: $3000; crc: $4CDF6704), (n: 'pir8.6j'; l: $800; p: $3800; crc: $B86FEDB3));
  piranha_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: 'piranha.4a'; l: $100; p: $20; crc: $08C9447B));
  piranha_char: array [0 .. 1] of tipo_roms = ((n: 'pir9.5e'; l: $800; p: 0; crc: $0F19EB28), (n: 'pir11.5h'; l: $800; p: $800; crc: $5F8BDABE));
  piranha_sprites: array [0 .. 1] of tipo_roms = ((n: 'pir10.5f'; l: $800; p: 0; crc: $D19399FB), (n: 'pir12.5j'; l: $800; p: $800; crc: $CFB4403D));
  // DIP
  pacman_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: (0, $10, $20, $30); name4: ('10K', '15K', '20K', 'None')), (mask: $40; name: 'Difficulty'; number: 2; val2: ($40, 0); name2: ('Normal', 'Hard')), (mask: $80; name: 'Ghost Names'; number: 2; val2: ($80, 0); name2: ('Normal', 'Alternate')), ());
  pacman_dip_b: array [0 .. 1] of def_dip2 = ((mask: $10; name: 'Rack Test'; number: 2; val2: ($10, 0); name2: ('Off', 'On')), ());
  pacman_dip_c: array [0 .. 1] of def_dip2 = ((mask: $80; name: 'Cabinet'; number: 2; val2: ($80, 0); name2: ('Upright', 'Cocktail')), ());
  mspacman_dip: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: (0, $10, $20, $30); name4: ('10K', '15K', '20K', 'None')), (mask: $40; name: 'Difficulty'; number: 2; val2: ($40, 0); name2: ('Normal', 'Hard')), ());
  crush_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('3', '4', '5', '6')), (mask: $10; name: 'First Pattern';
    number: 2; val2: ($10, 0); name2: ('Easy', 'Hard')), (mask: $20; name: 'Teleport Holes'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), ());
  crush_dip_b: array [0 .. 1] of def_dip2 = ((mask: $10; name: 'Cabinet'; number: 2; val2: (0, $10); name2: ('Upright', 'Cocktail')), ());
  mspactwin_dip_a: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: (0, $10, $20, $30); name4: ('10K', '15K', '20K', 'None')), ());
  mspactwin_dip_b: array [0 .. 1] of def_dip2 = ((mask: $10; name: 'Jama'; number: 2; val2: ($10, 0); name2: ('Slow', 'Fast')), ());
  mspactwin_dip_c: array [0 .. 1] of def_dip2 = ((mask: $80; name: 'Skip Screen'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  birdiy_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('1', '2', '3', '5')), (mask: $10; name: 'Cabinet';
    number: 2; val2: (0, $10); name2: ('Upright', 'Cocktail')), (mask: $20; name: 'Skip Screen'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), ());
  ponpoko_dip_a: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Bonus Life'; number: 4; val4: (1, 2, 3, 0); name4: ('10K', '30K', '50K', 'None')), (mask: $30; name: 'Lives'; number: 4; val4: (0, $10, $20, $30); name4: ('2', '3', '4', '5')), (mask: $40; name: 'Cabinet'; number: 2;
    val2: ($40, 0); name2: ('Upright', 'Cocktail')), ());
  ponpoko_dip_b: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coinage'; number: 16; val16: (4, $E, $F, 2, $D, 7, $B, $C, 1, 6, 5, $A, 8, 9, 3, 0);
    name16: ('A 3/1 B 3/1', 'A 3/1 B 1/2', 'A 3/1 B 1/4', 'A 2/1 B 2/1', 'A 2/1 B 1/1', 'A 2/1 B 1/3', 'A 2/1 B 1/5', 'A 2/1 B 1/6', 'A 1/1 B 1/1', 'A 1/1 B 4/5', 'A 1/1 B 2/3', 'A 1/1 B 1/3', 'A 1/1 B 1/5', 'A 1/1 B 1/6', 'A 1/2 B 1/2', 'Free Play')), (mask: $40;
    name: 'Demo Sounds'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), ());
  woodpeck_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: (0, $10, $20, $30); name4: ('5K', '10K', '15K', 'None')), (mask: $40; name: 'Cabinet'; number: 2; val2: ($40, 0); name2: ('Upright', 'Cocktail')), ());
  eyes_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (1, 3, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: ($C, 8, 4, 0); name4: ('2', '3', '4', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: ($30, $20, $10, 0); name4: ('50K', '75K', '100K', '125K')), (mask: $40; name: 'Cabinet'; number: 2; val2: ($40, 0); name2: ('Upright', 'Cocktail')), ());
  alibaba_dip_a: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 1, 2, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: (0, $10, $20, $30); name4: ('10K', '15K', '20K', 'None')), (mask: $40; name: 'Difficulty'; number: 2; val2: ($40, 0); name2: ('Normal', 'Hard')), ());
  piranha_dip_a: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (0, 1, 2, 3); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Lives'; number: 4; val4: ($C, 4, 8, 0); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: ($30, $10, $20, 0); name4: ('10K', '15K', '20K', 'None')), ());

var
  irq_vblank, dec_enable, croller_disable_protection: boolean;
  rom_decode: array [0 .. $BFFF] of byte;
  read_events: procedure;
  read_io: function(direccion: byte): byte;
  write_io: procedure(direccion, valor: byte);
  croller_counter, croller_offset, unk_latch: byte;
  sprite_ram: array [0 .. 1, 0 .. $F] of byte;
  x_hack, y_hack: integer;
  alibaba_mystery: word;
  irq_vector: byte;

procedure update_video_pacman;
var
  color, offs: word;
  nchar, f, sx, sy, atrib, x, y: byte;
  flip_x, flip_y: boolean;
begin
  for y := 0 to 27 do
  begin
    for x := 0 to 35 do
    begin
      sx := x - 2;
      sy := y + 2;
      if (sx and $20) <> 0 then
        offs := sy + ((sx and $1F) shl 5)
      else
        offs := sx + (sy shl 5);
      if gfx[0].buffer[offs] then
      begin
        color := ((memory[$4400 + offs]) and $1F) shl 2;
        put_gfx(x * 8, y * 8, memory[$4000 + offs], color, 1, 0);
        gfx[0].buffer[offs] := false;
      end;
    end;
  end;
  update_region(0, 0, 288, 224, 1, 0, 0, 288, 224, 2);
  for f := 7 downto 0 do
  begin
    atrib := sprite_ram[0, f * 2];
    nchar := atrib shr 2;
    color := (sprite_ram[0, 1 + (f * 2)] and $1F) shl 2;
    if main_screen.flip_main_screen then
    begin
      x := sprite_ram[1, 1 + (f * 2)] - 32;
      y := sprite_ram[1, (f * 2)];
      flip_y := (atrib and 1) = 0;
      flip_x := (atrib and 2) = 0;
    end
    else
    begin
      x := (270 + x_hack) - sprite_ram[1, 1 + (f * 2)];
      y := sprite_ram[1, f * 2] - 31;
      flip_x := (atrib and 1) <> 0;
      flip_y := (atrib and 2) <> 0;
    end;
    put_gfx_sprite_mask(nchar, color, flip_x, flip_y, 1, 0, $F);
    if (f < 2) then
      update_gfx_sprite(x, y + y_hack, 2, 1)
    else
      update_gfx_sprite(x, y, 2, 1)
  end;
  update_final_piece(0, 0, 288, 224, 2);
end;

procedure events_pacman;
begin
  if event.arcade then
  begin
    // in 0
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // in 1
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but0[0] then
    begin
      if (memory[$180B] <> 1) then
      begin
        memory[$180B] := 1;
        memory[$1FFD] := $BD;
      end
    end
    else
    begin
      if (memory[$180B] <> $BE) then
      begin
        memory[$180B] := $BE;
        memory[$1FFD] := 0;
      end
    end;
  end;
end;

procedure events_mspacman;
begin
  if event.arcade then
  begin
    // in 0
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // in 1
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure events_ponpoko;
begin
  if event.arcade then
  begin
    // in 0
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // in 1
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
  end;
end;

procedure pacman_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to 263 do begin
    read_events;
    //Si no pinto la pantalla aqui, Ms Pac Man Twin no hace el efecto de la pantalla...
    //Los timings del Z80 estan bien, supongo que es correcto (parece que no hay danos colaterales!)
    case f of
      96:update_video_pacman;
      224:if irq_vblank then z80_0.change_irq_vector(ASSERT_LINE,irq_vector);
    end;
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

function pacman_gen_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $BFFF:
      pacman_gen_getbyte := memory[direccion];
    $4000 .. $47FF, $6000 .. $67FF, $C000 .. $C7FF, $E000 .. $E7FF:
      pacman_gen_getbyte := memory[(direccion and $7FF) + $4000];
    $4800 .. $4BFF, $6800 .. $6BFF, $C800 .. $CBFF, $E800 .. $EBFF:
      pacman_gen_getbyte := $BF;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      pacman_gen_getbyte := memory[(direccion and $3FF) + $4C00];
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      pacman_gen_getbyte := read_io(direccion and $FF);
  end;
end;

procedure pacman_gen_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF, $8000 .. $BFFF:
      ; // ROM
    $4000 .. $47FF, $6000 .. $67FF, $C000 .. $C7FF, $E000 .. $E7FF:
      if memory[(direccion and $7FF) + $4000] <> valor then
      begin
        memory[(direccion and $7FF) + $4000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      begin
        memory[(direccion and $3FF) + $4C00] := valor;
        if (((direccion and $3FF) > $3EF) and ((direccion and $3FF) <= $3FF)) then
          sprite_ram[0, direccion and $F] := valor;
      end;
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      write_io(direccion and $FF, valor);
  end;
end;

function pacman_read_io(direccion: byte): byte;
begin
  case direccion of
    0 .. $3F:
      pacman_read_io := marcade.in0 or marcade.dswb;
    $40 .. $7F:
      pacman_read_io := marcade.in1 or marcade.dswc;
    $80 .. $BF:
      pacman_read_io := marcade.dswa;
    $C0 .. $FF:
      pacman_read_io := 0;
  end;
end;

procedure pacman_write_io(direccion, valor: byte);
begin
  case direccion of
    0:
      begin
        irq_vblank := valor <> 0;
        if not(irq_vblank) then
          z80_0.change_irq(CLEAR_LINE);
      end;
    1:
      namco_snd_0.enabled := valor <> 0;
    3:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $40 .. $5F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $60 .. $6F:
      sprite_ram[1, direccion and $F] := valor;
  end;
end;

procedure pacman_outbyte(puerto: word; valor: byte);
begin
  if (puerto and $FF) = 0 then
    irq_vector := valor;
end;

procedure pacman_sound_update;
begin
  namco_snd_0.update;
end;

// MS Pacman
function mspacman_getbyte(direccion: word): byte;
begin
  case direccion of
    $38 .. $3F, $3B0 .. $3B7, $1600 .. $1607, $2120 .. $2127, $3FF0 .. $3FF7, $8000 .. $8007, $97F0 .. $97F7:
      dec_enable := false;
    $3FF8 .. $3FFF:
      dec_enable := true;
  end;
  case direccion of
    0 .. $3FFF, $8000 .. $BFFF:
      if dec_enable then
        mspacman_getbyte := rom_decode[direccion]
      else
        mspacman_getbyte := memory[direccion and $3FFF];
  else
    mspacman_getbyte := pacman_gen_getbyte(direccion);
  end;
end;

procedure mspacman_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $38 .. $3F, $3B0 .. $3B7, $1600 .. $1607, $2120 .. $2127, $3FF0 .. $3FF7, $8000 .. $8007, $97F0 .. $97F7:
      dec_enable := false;
    $3FF8 .. $3FFF:
      dec_enable := true;
  else
    pacman_gen_putbyte(direccion, valor);
  end;
end;

// Crush Roller
function crush_read_io(direccion: byte): byte;
const
  protdata_odd: array [0 .. $1D] of byte = ( // table at $ebd (odd entries)
    $00, $C0, $00, $40, $C0, $40, $00, $C0, $00, $40, $00, $C0, $00, $40, $C0, $40, $00, $C0, $00, $40, $00, $C0, $00, $40, $C0, $40, $00, $C0, $00, $40);
  protdata_even: array [0 .. $1D] of byte = ( // table at $ebd (even entries)
    $1F, $3F, $2F, $2F, $0F, $0F, $0F, $3F, $0F, $0F, $1C, $3C, $2C, $2C, $0C, $0C, $0C, $3C, $0C, $0C, $11, $31, $21, $21, $01, $01, $01, $31, $01, $01);
var
  tempb: byte;
begin
  case direccion of
    0 .. $3F:
      crush_read_io := marcade.in0 or marcade.dswb;
    $40 .. $7F:
      crush_read_io := marcade.in1;
    $80 .. $BF:
      begin // proteccion 1
        tempb := marcade.dswa and $3F;
        if not(croller_disable_protection) then
        begin
          crush_read_io := protdata_odd[croller_offset] or tempb;
          exit;
        end;
        case (direccion and $3F) of
          1, 4:
            crush_read_io := tempb or $40;
          5, $E, $10:
            crush_read_io := tempb or $C0;
        else
          crush_read_io := tempb;
        end;
      end;
    $C0 .. $CF:
      begin // proteccion 2
        if not(croller_disable_protection) then
        begin
          crush_read_io := protdata_even[croller_offset];
          exit;
        end;
        case (direccion and $F) of
          0:
            crush_read_io := $1F;
          9:
            crush_read_io := $30;
          $C:
            crush_read_io := 0;
        else
          crush_read_io := $20;
        end;
      end;
    $D0 .. $FF:
      crush_read_io := 0;
  end;
end;

procedure crush_write_io(direccion, valor: byte);
begin
  case direccion of
    0:
      begin
        irq_vblank := valor <> 0;
        if not(irq_vblank) then
          z80_0.change_irq(CLEAR_LINE);
      end;
    1:
      namco_snd_0.enabled := valor <> 0;
    3:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    4:
      case valor of // proteccion
        0:
          begin // disable protection / reset?
            croller_counter := 0;
            croller_offset := 0;
            croller_disable_protection := true;
          end;
        1:
          begin
            croller_disable_protection := false;
            croller_counter := croller_counter + 1;
            if (croller_counter = $3C) then
            begin
              croller_counter := 0;
              croller_offset := croller_offset + 1;
              if (croller_offset = $1E) then
                croller_offset := 0;
            end;
          end;
      end;
    $40 .. $5F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $60 .. $6F:
      sprite_ram[1, direccion and $F] := valor;
  end;
end;

// Ms Pac Man Twin
function mspactwin_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $BFFF:
      if z80_0.opcode then
        mspactwin_getbyte := rom_decode[direccion]
      else
        mspactwin_getbyte := memory[direccion];
    $6000 .. $67FF:
      if z80_0.opcode then
        mspactwin_getbyte := rom_decode[(direccion and $1FFF) + $2000]
      else
        mspactwin_getbyte := memory[(direccion and $1FFF) + $2000];
    $4000 .. $47FF, $C000 .. $C7FF:
      mspactwin_getbyte := memory[(direccion and $7FF) + $4000];
    $4800 .. $4BFF, $6800 .. $6BFF, $C800 .. $CBFF:
      mspactwin_getbyte := 0;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      mspactwin_getbyte := memory[(direccion and $3FF) + $4C00];
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      case (direccion and $FF) of
        0 .. $3F:
          mspactwin_getbyte := marcade.in0 or marcade.dswb;
        $40 .. $7F:
          mspactwin_getbyte := marcade.in1 or marcade.dswc;
        $80 .. $BF:
          mspactwin_getbyte := marcade.dswa;
        $C0 .. $FF:
          mspactwin_getbyte := unk_latch;
      end;
  end;
end;

procedure mspactwin_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF, $6000 .. $67FF, $8000 .. $BFFF:
      ;
    $4000 .. $47FF, $C000 .. $C7FF:
      if memory[(direccion and $7FF) + $4000] <> valor then
      begin
        memory[(direccion and $7FF) + $4000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      begin
        memory[(direccion and $3FF) + $4C00] := valor;
        if (((direccion and $3FF) > $3EF) and ((direccion and $3FF) <= $3FF)) then
          sprite_ram[0, direccion and $F] := valor;
      end;
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      case (direccion and $FF) of
        0:
          begin
            irq_vblank := valor <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        1:
          namco_snd_0.enabled := valor <> 0;
        3:
          main_screen.flip_main_screen := (valor and 1) <> 0;
        $40 .. $5F:
          namco_snd_0.regs[direccion and $1F] := valor;
        $60 .. $6F:
          sprite_ram[1, direccion and $F] := valor;
        $80 .. $BF:
          unk_latch := valor;
        $C0 .. $FF:
          ; // WD
      end;
  end;
end;

// Birdiy
function birdiy_read_io(direccion: byte): byte;
begin
  case direccion of
    0 .. $3F:
      birdiy_read_io := marcade.in0;
    $40 .. $7F:
      birdiy_read_io := marcade.in1;
    $80 .. $BF:
      birdiy_read_io := marcade.dswa;
    $C0 .. $FF:
      birdiy_read_io := marcade.dswb;
  end;
end;

procedure birdiy_write_io(direccion, valor: byte);
begin
  case direccion of
    1:
      begin
        irq_vblank := valor <> 0;
        if not(irq_vblank) then
          z80_0.change_irq(CLEAR_LINE);
      end;
    $80 .. $9F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $A0 .. $AF:
      sprite_ram[1, direccion and $F] := valor;
  end;
end;

// Alibaba
function alibaba_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $8FFF:
      alibaba_getbyte := memory[direccion];
    $4000 .. $47FF, $6000 .. $67FF, $C000 .. $C7FF:
      alibaba_getbyte := memory[(direccion and $7FF) + $4000];
    $4800 .. $4BFF, $6800 .. $6BFF, $C800 .. $CBFF:
      alibaba_getbyte := $BF;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      alibaba_getbyte := memory[(direccion and $3FF) + $4C00];
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      case (direccion and $FF) of
        0 .. $3F:
          alibaba_getbyte := marcade.in0 or marcade.dswb;
        $40 .. $7F:
          alibaba_getbyte := marcade.in1 or marcade.dswc;
        $80 .. $BF:
          alibaba_getbyte := marcade.dswa;
        $C0:
          alibaba_getbyte := random(16);
        $C1:
          begin
            alibaba_mystery := alibaba_mystery + 1;
            alibaba_getbyte := (alibaba_mystery shr 10) and 1;
          end;
        $C2 .. $FF:
          alibaba_getbyte := $BF;
      end;
    $9000 .. $9FFF:
      alibaba_getbyte := memory[(direccion and $3FF) + $9000];
    $A000 .. $BFFF:
      alibaba_getbyte := memory[(direccion and $7FF) + $A000];
  end;
end;

procedure alibaba_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF, $8000 .. $8FFF, $A000 .. $BFFF:
      ;
    $4000 .. $47FF, $6000 .. $67FF, $C000 .. $C7FF:
      if memory[(direccion and $7FF) + $4000] <> valor then
      begin
        memory[(direccion and $7FF) + $4000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      begin
        memory[(direccion and $3FF) + $4C00] := valor;
        if (((direccion and $3FF) > $2EF) and ((direccion and $3FF) < $2FF)) then
          sprite_ram[0, direccion and $F] := valor;
      end;
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      case (direccion and $FF) of
        $40 .. $4F:
          namco_snd_0.regs[direccion and $F] := valor;
        $50 .. $5F:
          sprite_ram[1, direccion and $F] := valor;
        $60 .. $6F:
          namco_snd_0.regs[(direccion and $F) or $10] := valor;
        $C0:
          namco_snd_0.enabled := valor <> 0;
        $C1:
          main_screen.flip_main_screen := (valor and 1) <> 0;
        $C2:
          begin
            irq_vblank := valor <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;

      end;
    $9000 .. $9FFF:
      memory[(direccion and $3FF) + $9000] := valor;
  end;
end;

// Piranha
procedure piranha_outbyte(puerto: word; valor: byte);
begin
  if (puerto and $FF) = 0 then
  begin
    if valor = $FA then
      irq_vector := $78
    else
      irq_vector := valor;
  end;
end;

procedure pacman_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    10:
      open_qsnapshot_save('pacman' + nombre);
    88:
      open_qsnapshot_save('mspacman' + nombre);
    234:
      open_qsnapshot_save('crushroller' + nombre);
    305:
      open_qsnapshot_save('mspactwin' + nombre);
    353:
      open_qsnapshot_save('birdiy' + nombre);
    401:
      open_qsnapshot_save('ponpoko' + nombre);
    402:
      open_qsnapshot_save('woodpeck' + nombre);
    403:
      open_qsnapshot_save('eyes' + nombre);
    404:
      open_qsnapshot_save('alibaba' + nombre);
    405:
      open_qsnapshot_save('piranha' + nombre);
  end;
  getmem(data, 2000);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := namco_snd_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@sprite_ram[0], $10);
  savedata_qsnapshot(@memory[$4000], $4000);
  if main_vars.machine_type = 88 then
    savedata_qsnapshot(@memory[$C000], $4000);
  // MISC
  buffer[0] := byte(irq_vblank);
  buffer[1] := byte(dec_enable);
  buffer[2] := croller_counter;
  buffer[3] := croller_offset;
  buffer[4] := byte(croller_disable_protection);
  buffer[5] := unk_latch;
  savedata_qsnapshot(@buffer, 6);
  freemem(data);
  close_qsnapshot;
end;

procedure pacman_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    10:
      if not(open_qsnapshot_load('pacman' + nombre)) then
        exit;
    88:
      if not(open_qsnapshot_load('mspacman' + nombre)) then
        exit;
    234:
      if not(open_qsnapshot_load('crushroller' + nombre)) then
        exit;
    305:
      if not(open_qsnapshot_load('mspactwin' + nombre)) then
        exit;
    353:
      if not(open_qsnapshot_load('birdiy' + nombre)) then
        exit;
    401:
      if not(open_qsnapshot_load('ponpoko' + nombre)) then
        exit;
    402:
      if not(open_qsnapshot_load('woodpeck' + nombre)) then
        exit;
    403:
      if not(open_qsnapshot_load('eyes' + nombre)) then
        exit;
    404:
      if not(open_qsnapshot_load('alibaba' + nombre)) then
        exit;
    405:
      if not(open_qsnapshot_load('piranha' + nombre)) then
        exit;
  end;
  getmem(data, 2000);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  namco_snd_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@sprite_ram[0]);
  loaddata_qsnapshot(@memory[$4000]);
  if main_vars.machine_type = 88 then
    loaddata_qsnapshot(@memory[$C000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  irq_vblank := buffer[0] <> 0;
  dec_enable := buffer[1] <> 0;
  croller_counter := buffer[2];
  croller_offset := buffer[3];
  croller_disable_protection := buffer[4] <> 0;
  unk_latch := buffer[5];
  freemem(data);
  close_qsnapshot;
  fillchar(gfx[0].buffer, $400, 1);
end;

// Main
procedure reset_pacman;
begin
  z80_0.reset;
  frame_main := z80_0.tframes;
  namco_snd_0.reset;
  irq_vblank := false;
  irq_vector := $FF;
  dec_enable := false;
  marcade.in0 := $EF;
  marcade.in1 := $7F;
  case main_vars.machine_type of
    234:
      marcade.in1 := $6F;
    401:
      begin
        marcade.in0 := $E0;
        marcade.in1 := 0;
      end;
    353, 402, 405:
      begin
        marcade.in0 := $FF;
        marcade.in1 := $FF;
      end;
  end;
  croller_counter := 0;
  croller_offset := 0;
  croller_disable_protection := false;
  unk_latch := 0;
  alibaba_mystery := 0;
end;

procedure mspacman_install_patches;
var
  i: byte;
begin
  // copy forty 8-byte patches into Pac-Man code
  for i := 0 to 7 do
  begin
    rom_decode[$410 + i] := rom_decode[$8008 + i];
    rom_decode[$8E0 + i] := rom_decode[$81D8 + i];
    rom_decode[$A30 + i] := rom_decode[$8118 + i];
    rom_decode[$BD0 + i] := rom_decode[$80D8 + i];
    rom_decode[$C20 + i] := rom_decode[$8120 + i];
    rom_decode[$E58 + i] := rom_decode[$8168 + i];
    rom_decode[$EA8 + i] := rom_decode[$8198 + i];
    rom_decode[$1000 + i] := rom_decode[$8020 + i];
    rom_decode[$1008 + i] := rom_decode[$8010 + i];
    rom_decode[$1288 + i] := rom_decode[$8098 + i];
    rom_decode[$1348 + i] := rom_decode[$8048 + i];
    rom_decode[$1688 + i] := rom_decode[$8088 + i];
    rom_decode[$16B0 + i] := rom_decode[$8188 + i];
    rom_decode[$16D8 + i] := rom_decode[$80C8 + i];
    rom_decode[$16F8 + i] := rom_decode[$81C8 + i];
    rom_decode[$19A8 + i] := rom_decode[$80A8 + i];
    rom_decode[$19B8 + i] := rom_decode[$81A8 + i];
    rom_decode[$2060 + i] := rom_decode[$8148 + i];
    rom_decode[$2108 + i] := rom_decode[$8018 + i];
    rom_decode[$21A0 + i] := rom_decode[$81A0 + i];
    rom_decode[$2298 + i] := rom_decode[$80A0 + i];
    rom_decode[$23E0 + i] := rom_decode[$80E8 + i];
    rom_decode[$2418 + i] := rom_decode[$8000 + i];
    rom_decode[$2448 + i] := rom_decode[$8058 + i];
    rom_decode[$2470 + i] := rom_decode[$8140 + i];
    rom_decode[$2488 + i] := rom_decode[$8080 + i];
    rom_decode[$24B0 + i] := rom_decode[$8180 + i];
    rom_decode[$24D8 + i] := rom_decode[$80C0 + i];
    rom_decode[$24F8 + i] := rom_decode[$81C0 + i];
    rom_decode[$2748 + i] := rom_decode[$8050 + i];
    rom_decode[$2780 + i] := rom_decode[$8090 + i];
    rom_decode[$27B8 + i] := rom_decode[$8190 + i];
    rom_decode[$2800 + i] := rom_decode[$8028 + i];
    rom_decode[$2B20 + i] := rom_decode[$8100 + i];
    rom_decode[$2B30 + i] := rom_decode[$8110 + i];
    rom_decode[$2BF0 + i] := rom_decode[$81D0 + i];
    rom_decode[$2CC0 + i] := rom_decode[$80D0 + i];
    rom_decode[$2CD8 + i] := rom_decode[$80E0 + i];
    rom_decode[$2CF0 + i] := rom_decode[$81E0 + i];
    rom_decode[$2D60 + i] := rom_decode[$8160 + i];
  end;
end;

function start_pacman: boolean;
var
  colores: tpaleta;
  f: word;
  j, bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $7FFF] of byte;
  buffer: array [0 .. 7] of byte;
  rweights, gweights, bweights: array [0 .. 2] of single;
const
  ps_x: array [0 .. 15] of dword = (8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3, 0, 1, 2, 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  pc_x: array [0 .. 7] of dword = (8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  resistances: array [0 .. 2] of integer = (1000, 470, 220);
  procedure conv_chars;
  begin
    init_gfx(0, 8, 8, 256);
    gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
    convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  end;
  procedure conv_sprites;
  begin
    init_gfx(1, 16, 16, 64);
    gfx_set_desc_data(2, 0, 64 * 8, 0, 4);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;
  procedure decode_eyes;
  var
    f: word;
    j: byte;
  begin
    for f := 0 to $1FF do
    begin
      for j := 0 to 7 do
        buffer[j] := memory_temp[BITSWAP16((f * 8) + j, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 0, 1, 2)];
      for j := 0 to 7 do
        memory_temp[(f * 8) + j] := BITSWAP8(buffer[j], 7, 4, 5, 6, 3, 2, 1, 0);
    end;
  end;

begin
  machine_calls.general_loop := pacman_loop;
  machine_calls.reset := reset_pacman;
  machine_calls.fps_max := 60.6060606060;
  machine_calls.save_qsnap := pacman_qsave;
  machine_calls.load_qsnap := pacman_qload;
  start_pacman := false;
  start_audio(false);
  screen_init(1, 288, 244);
  if (main_vars.machine_type <> 401) then
    main_screen.rot90_screen := true;
  screen_init(2, 512, 256, false, true);
  start_video(288, 224);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 264);
  z80_0.change_ram_calls(pacman_gen_getbyte, pacman_gen_putbyte);
  z80_0.change_io_calls(nil, pacman_outbyte);
  z80_0.init_sound(pacman_sound_update);
  namco_snd_0 := namco_snd_chip.create(3);
  x_hack := 2;
  y_hack := -1;
  case main_vars.machine_type of
    10:
      begin // Pacman
        read_io := pacman_read_io;
        write_io := pacman_write_io;
        // cargar roms
        if not(roms_load(@memory, pacman_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, pacman_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, pacman_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, pacman_pal)) then
          exit;
        // DIP
        read_events := events_pacman;
        marcade.dswa := $C9;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val2 := @pacman_dip_a;
        marcade.dswb_val2 := @pacman_dip_b;
        marcade.dswc_val2 := @pacman_dip_c;
      end;
    88:
      begin // MS Pacman
        z80_0.change_ram_calls(mspacman_getbyte, mspacman_putbyte);
        read_io := pacman_read_io;
        write_io := pacman_write_io;
        // cargar y desencriptar roms
        if not(roms_load(@memory, mspacman_rom)) then
          exit;
        copymemory(@rom_decode, @memory, $1000);
        copymemory(@rom_decode[$1000], @memory[$1000], $1000);
        copymemory(@rom_decode[$2000], @memory[$2000], $1000);
        for f := 0 to $FFF do
          rom_decode[$3000 + f] := BITSWAP8(memory[$B000 + BITSWAP16(f, 15, 14, 13, 12, 11, 3, 7, 9, 10, 8, 6, 5, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt u7
        for f := 0 to $7FF do
        begin
          rom_decode[$8000 + f] := BITSWAP8(memory[$8000 + BITSWAP16(f, 15, 14, 13, 12, 11, 8, 7, 5, 9, 10, 6, 3, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt u5
          rom_decode[$8800 + f] := BITSWAP8(memory[$9800 + BITSWAP16(f, 15, 14, 13, 12, 11, 3, 7, 9, 10, 8, 6, 5, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt half of u6
          rom_decode[$9000 + f] := BITSWAP8(memory[$9000 + BITSWAP16(f, 15, 14, 13, 12, 11, 3, 7, 9, 10, 8, 6, 5, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt half of u6
        end;
        copymemory(@rom_decode[$9800], @memory[$1800], $800);
        copymemory(@rom_decode[$A000], @memory[$2000], $1000);
        copymemory(@rom_decode[$B000], @memory[$3000], $1000);
        mspacman_install_patches;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, mspacman_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, mspacman_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, pacman_pal)) then
          exit;
        // DIP
        read_events := events_mspacman;
        marcade.dswa := $C9;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val2 := @mspacman_dip;
        marcade.dswb_val2 := @pacman_dip_b;
        marcade.dswc_val2 := @pacman_dip_c;
      end;
    234:
      begin // Crush Roller
        read_io := crush_read_io;
        write_io := crush_write_io;
        // cargar roms
        if not(roms_load(@memory, crush_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, crush_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, crush_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, crush_pal)) then
          exit;
        // DIP
        read_events := events_mspacman;
        marcade.dswa := $31;
        marcade.dswb := 0;
        marcade.dswa_val2 := @crush_dip_a;
        marcade.dswb_val2 := @crush_dip_b;
      end;
    305:
      begin // MS Pacman Twin
        z80_0.change_ram_calls(mspactwin_getbyte, mspactwin_putbyte);
        // cargar y desencriptar roms
        if not(roms_load(@memory_temp, mspactwin_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $4000);
        copymemory(@memory[$8000], @memory_temp[$4000], $4000);
        for f := 0 to $1FFF do
        begin
          // decode opcode
          rom_decode[f * 2] := BITSWAP8(memory[f * 2], 4, 5, 6, 7, 0, 1, 2, 3);
          rom_decode[(f * 2) + 1] := BITSWAP8(memory[(f * 2) + 1] xor $9A, 6, 4, 5, 7, 2, 0, 3, 1);
          rom_decode[$8000 + (f * 2)] := BITSWAP8(memory[$8000 + (f * 2)], 4, 5, 6, 7, 0, 1, 2, 3);
          rom_decode[$8001 + (f * 2)] := BITSWAP8(memory[$8001 + (f * 2)] xor $9A, 6, 4, 5, 7, 2, 0, 3, 1);
          // decode operand
          memory[f * 2] := BITSWAP8(memory[f * 2], 0, 1, 2, 3, 4, 5, 6, 7);
          memory[(f * 2) + 1] := BITSWAP8(memory[(f * 2) + 1] xor $A3, 2, 4, 6, 3, 7, 0, 5, 1);
          memory[$8000 + (f * 2)] := BITSWAP8(memory[$8000 + (f * 2)], 0, 1, 2, 3, 4, 5, 6, 7);
          memory[$8001 + (f * 2)] := BITSWAP8(memory[$8001 + (f * 2)] xor $A3, 2, 4, 6, 3, 7, 0, 5, 1);
        end;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, mspactwin_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, mspactwin_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, mspactwin_pal)) then
          exit;
        // DIP
        read_events := events_mspacman;
        marcade.dswa := $C9;
        marcade.dswa_val2 := @mspactwin_dip_a;
        marcade.dswb := $10;
        marcade.dswb_val2 := @mspactwin_dip_b;
        marcade.dswc := $80;
        marcade.dswc_val2 := @mspactwin_dip_c;
      end;
    353:
      begin // Birdiy
        read_io := birdiy_read_io;
        write_io := birdiy_write_io;
        if not(roms_load(@memory, birdiy_rom)) then
          exit;
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, birdiy_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, birdiy_sprites)) then
          exit;
        conv_sprites;
        if not(roms_load(@memory_temp, birdiy_pal)) then
          exit;
        read_events := events_mspacman;
        y_hack := 0;
        marcade.dswa := $E9;
        marcade.dswa_val2 := @birdiy_dip_a;
        marcade.dswb := $FF;
      end;
    401:
      begin // Ponpoko
        read_io := birdiy_read_io;
        write_io := pacman_write_io;
        if not(roms_load(@memory, ponpoko_rom)) then
          exit;
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, ponpoko_char)) then
          exit;
        for f := 0 to $FF do
        begin
          for j := 0 to 7 do
          begin
            bit0 := memory_temp[(f * $10) + j + 8];
            memory_temp[(f * $10) + j + 8] := memory_temp[(f * $10) + j];
            memory_temp[(f * $10) + j] := bit0;
          end;
        end;
        conv_chars;
        if not(roms_load(@memory_temp, ponpoko_sprites)) then
          exit;
        for f := 0 to $7F do
        begin
          for j := 0 to 7 do
          begin
            bit0 := memory_temp[(f * $20) + j + $18];
            memory_temp[(f * $20) + j + $18] := memory_temp[(f * $20) + j + $10];
            memory_temp[(f * $20) + j + $10] := memory_temp[(f * $20) + j + $8];
            memory_temp[(f * $20) + j + $8] := memory_temp[(f * $20) + j];
            memory_temp[(f * $20) + j] := bit0;
          end;
        end;
        conv_sprites;
        if not(roms_load(@memory_temp, ponpoko_pal)) then
          exit;
        read_events := events_ponpoko;
        x_hack := 0;
        y_hack := +1;
        marcade.dswa := $E1;
        marcade.dswa_val2 := @ponpoko_dip_a;
        marcade.dswb := $B1;
        marcade.dswb_val2 := @ponpoko_dip_b;
      end;
    402:
      begin // Woodpecker
        read_io := birdiy_read_io;
        write_io := pacman_write_io;
        if not(roms_load(@memory, woodpeck_rom)) then
          exit;
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, woodpeck_char)) then
          exit;
        decode_eyes;
        conv_chars;
        if not(roms_load(@memory_temp, woodpeck_sprites)) then
          exit;
        decode_eyes;
        conv_sprites;
        if not(roms_load(@memory_temp, woodpeck_pal)) then
          exit;
        read_events := events_mspacman;
        marcade.dswa := $C9;
        marcade.dswa_val2 := @woodpeck_dip_a;
        marcade.dswb := 0;
      end;
    403:
      begin // Eyes
        read_io := pacman_read_io;
        write_io := pacman_write_io;
        if not(roms_load(@memory, eyes_rom)) then
          exit;
        for f := 0 to $BFFF do
          memory[f] := BITSWAP8(memory[f], 7, 6, 3, 4, 5, 2, 1, 0);
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, eyes_char)) then
          exit;
        decode_eyes;
        conv_chars;
        if not(roms_load(@memory_temp, eyes_sprites)) then
          exit;
        decode_eyes;
        conv_sprites;
        if not(roms_load(@memory_temp, eyes_pal)) then
          exit;
        read_events := events_mspacman;
        marcade.dswa := $FB;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val2 := @eyes_dip_a;
        marcade.dswb_val2 := @pacman_dip_b;
        marcade.dswc_val2 := @pacman_dip_c;
      end;
    404:
      begin // Alibaba
        z80_0.change_ram_calls(alibaba_getbyte, alibaba_putbyte);
        if not(roms_load(@memory, alibaba_rom)) then
          exit;
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, alibaba_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, alibaba_sprites)) then
          exit;
        conv_sprites;
        if not(roms_load(@memory_temp, alibaba_pal)) then
          exit;
        read_events := events_pacman;
        marcade.dswa := $C9;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val2 := @alibaba_dip_a;
        marcade.dswb_val2 := @pacman_dip_b;
        marcade.dswc_val2 := @pacman_dip_c;
      end;
    405:
      begin // Piranha
        read_io := birdiy_read_io;
        write_io := pacman_write_io;
        z80_0.change_io_calls(nil, piranha_outbyte);
        if not(roms_load(@memory, piranha_rom)) then
          exit;
        for f := 0 to $BFFF do
          memory[f] := BITSWAP8(memory[f], 7, 6, 3, 4, 5, 2, 1, 0);
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, piranha_char)) then
          exit;
        decode_eyes;
        conv_chars;
        if not(roms_load(@memory_temp, piranha_sprites)) then
          exit;
        decode_eyes;
        conv_sprites;
        if not(roms_load(@memory_temp, piranha_pal)) then
          exit;
        read_events := events_mspacman;
        marcade.dswa := $C9;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val2 := @piranha_dip_a;
        marcade.dswb_val2 := @pacman_dip_b;
        marcade.dswc_val2 := @pacman_dip_c;
      end;
  end;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances, @rweights, 0, 0, 3, @resistances, @gweights, 0, 0, 2, @resistances[1], @bweights, 0, 0);
  for f := 0 to $1F do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    // green component
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // blue component
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to 255 do
  begin
    gfx[0].colores[f] := memory_temp[$20 + f] and $F;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
  end;
  // final
  start_pacman := true;
end;

end.
