unit zaxxon_hw;

interface

uses
  WinApi.Windows,
  nz80, main_engine, controls_engine, gfx_engine, rom_engine, pal_engine,
  sound_engine, sn_76496, timer_engine, ppi8255, samples, sega_decrypt;

function start_zaxxon: boolean;

implementation

const
  // Congo
  congo_rom: array [0 .. 3] of tipo_roms = ((n: 'congo_rev_c_rom1.u21'; l: $2000; p: 0; crc: $09355B5B), (n: 'congo_rev_c_rom2a.u22'; l: $2000; p: $2000; crc: $1C5E30AE), (n: 'congo_rev_c_rom3.u23'; l: $2000; p: $4000; crc: $5EE1132C), (n: 'congo_rev_c_rom4.u24'; l: $2000;
    p: $6000; crc: $5332B9BF));
  congo_pal: array [0 .. 1] of tipo_roms = ((n: 'mr019.u87'; l: $100; p: 0; crc: $B788D8AE), (n: 'mr019.u87'; l: $100; p: $100; crc: $B788D8AE));
  congo_char: tipo_roms = (n: 'tip_top_rom_5.u76'; l: $1000; p: 0; crc: $7BF6BA2B);
  congo_bg: array [0 .. 2] of tipo_roms = ((n: 'tip_top_rom_8.u93'; l: $2000; p: 0; crc: $DB99A619), (n: 'tip_top_rom_9.u94'; l: $2000; p: $2000; crc: $93E2309E), (n: 'tip_top_rom_10.u95'; l: $2000; p: $4000; crc: $F27A9407));
  congo_sprites: array [0 .. 5] of tipo_roms = ((n: 'tip_top_rom_12.u78'; l: $2000; p: 0; crc: $15E3377A), (n: 'tip_top_rom_13.u79'; l: $2000; p: $2000; crc: $1D1321C8), (n: 'tip_top_rom_11.u77'; l: $2000; p: $4000; crc: $73E2709F), (n: 'tip_top_rom_14.u104'; l: $2000; p: $6000;
    crc: $BF9169FE), (n: 'tip_top_rom_16.u106'; l: $2000; p: $8000; crc: $CB6D5775), (n: 'tip_top_rom_15.u105'; l: $2000; p: $A000; crc: $7B15A7A4));
  congo_sound: tipo_roms = (n: 'tip_top_rom_17.u19'; l: $2000; p: 0; crc: $5024E673);
  congo_tilemap: array [0 .. 1] of tipo_roms = ((n: 'congo6.u57'; l: $2000; p: 0; crc: $D637F02B), (n: 'congo7.u58'; l: $2000; p: $2000; crc: $80927943));
  congo_samples: array [0 .. 4] of tipo_nombre_samples = ((nombre: 'gorilla.wav'), (nombre: 'bass.wav'), (nombre: 'congal.wav'), (nombre: 'congah.wav'), (nombre: 'rim.wav'));
  congo_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Bonus Life'; number: 4; dip: ((dip_val: $3; dip_name: '10000'), (dip_val: $1; dip_name: '20000'), (dip_val: $2; dip_name: '30000'), (dip_val: $0; dip_name: '40000'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C; name: 'Difficulty'; number: 4; dip: ((dip_val: $C; dip_name: 'Easy'), (dip_val: $4; dip_name: 'Medium'), (dip_val: $8; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Lives'; number: 4;
    dip: ((dip_val: $30; dip_name: '3'), (dip_val: $10; dip_name: '4'), (dip_val: $20; dip_name: '5'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Sound'; number: 2;
    dip: ((dip_val: $40; dip_name: 'On'), (dip_val: $0; dip_name: 'Off'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Zaxxon
  zaxxon_rom: array [0 .. 2] of tipo_roms = ((n: 'zaxxon3.u27'; l: $2000; p: 0; crc: $6E2B4A30), (n: 'zaxxon2.u28'; l: $2000; p: $2000; crc: $1C9EA398), (n: 'zaxxon1.u29'; l: $1000; p: $4000; crc: $1C123EF9));
  zaxxon_pal: array [0 .. 1] of tipo_roms = ((n: 'zaxxon.u98'; l: $100; p: 0; crc: $6CC6695B), (n: 'zaxxon.u72'; l: $100; p: $100; crc: $DEAA21F7));
  zaxxon_char: array [0 .. 1] of tipo_roms = ((n: 'zaxxon14.u68'; l: $800; p: 0; crc: $07BF8C52), (n: 'zaxxon15.u69'; l: $800; p: $800; crc: $C215EDCB));
  zaxxon_bg: array [0 .. 2] of tipo_roms = ((n: 'zaxxon6.u113'; l: $2000; p: 0; crc: $6E07BB68), (n: 'zaxxon5.u112'; l: $2000; p: $2000; crc: $0A5BCE6A), (n: 'zaxxon4.u111'; l: $2000; p: $4000; crc: $A5BF1465));
  zaxxon_sprites: array [0 .. 2] of tipo_roms = ((n: 'zaxxon11.u77'; l: $2000; p: 0; crc: $EAF0DD4B), (n: 'zaxxon12.u78'; l: $2000; p: $2000; crc: $1C5369C7), (n: 'zaxxon13.u79'; l: $2000; p: $4000; crc: $AB4E8A9A));
  zaxxon_tilemap: array [0 .. 3] of tipo_roms = ((n: 'zaxxon8.u91'; l: $2000; p: 0; crc: $28D65063), (n: 'zaxxon7.u90'; l: $2000; p: $2000; crc: $6284C200), (n: 'zaxxon10.u93'; l: $2000; p: $4000; crc: $A95E61FD), (n: 'zaxxon9.u92'; l: $2000; p: $6000; crc: $7E42691F));
  zaxxon_samples: array [0 .. 11] of tipo_nombre_samples = ((nombre: '03.wav'; loop: true), (nombre: '02.wav'; restart: true), (nombre: '01.wav'; restart: true; loop: true), (nombre: '00.wav'; restart: true; loop: true), (nombre: '11.wav'; restart: true), (nombre: '10.wav'),
    (nombre: '08.wav'; restart: true), (nombre: '23.wav'; restart: true), (nombre: '21.wav'; restart: true), (nombre: '20.wav'), (nombre: '05.wav'; restart: true; loop: true), (nombre: '04.wav'; restart: true; loop: true));
  // DIP
  zaxxon_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Bonus Life'; number: 4; dip: ((dip_val: $3; dip_name: '10000'), (dip_val: $1; dip_name: '20000'), (dip_val: $2; dip_name: '30000'), (dip_val: $0; dip_name: '40000'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $30; name: 'Lives'; number: 4; dip: ((dip_val: $30; dip_name: '3'), (dip_val: $10; dip_name: '4'), (dip_val: $20; dip_name: '5'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Sound'; number: 2;
    dip: ((dip_val: $40; dip_name: 'On'), (dip_val: $0; dip_name: 'Off'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  zaxxon_dip_b: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin B'; number: 16; dip: ((dip_val: $F; dip_name: '4C 1C'), (dip_val: $7; dip_name: '3C 1C'), (dip_val: $B; dip_name: '2C 1C'), (dip_val: $6; dip_name: '2C/1C 5C/3C 6C/4C'), (dip_val: $A;
    dip_name: '2C/1C 3C/2C 4C/3C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C/1C 5C/6C'), (dip_val: $C; dip_name: '1C/1C 4C/5C'), (dip_val: $4; dip_name: '1C/1C 2C/3C'), (dip_val: $D; dip_name: '1C 2C'), (dip_val: $8; dip_name: '1C/2C 5C/11C'), (dip_val: $0;
    dip_name: '1C/2C 4C/9C'), (dip_val: $5; dip_name: '1C 3C'), (dip_val: $9; dip_name: '1C 4C'), (dip_val: $1; dip_name: '1C 5C'), (dip_val: $6; dip_name: '1C 6C'))), (mask: $F0; name: 'Coin A'; number: 16;
    dip: ((dip_val: $F0; dip_name: '4C 1C'), (dip_val: $70; dip_name: '3C 1C'), (dip_val: $B0; dip_name: '2C 1C'), (dip_val: $60; dip_name: '2C/1C 5C/3C 6C/4C'), (dip_val: $A0; dip_name: '2C/1C 3C/2C 4C/3C'), (dip_val: $30; dip_name: '1C 1C'), (dip_val: $20;
    dip_name: '1C/1C 5C/6C'), (dip_val: $C0; dip_name: '1C/1C 4C/5C'), (dip_val: $40; dip_name: '1C/1C 2C/3C'), (dip_val: $D0; dip_name: '1C 2C'), (dip_val: $80; dip_name: '1C/2C 5C/11C'), (dip_val: $00; dip_name: '1C/2C 4C/9C'), (dip_val: $50; dip_name: '1C 3C'), (dip_val: $90;
    dip_name: '1C 4C'), (dip_val: $10; dip_name: '1C 5C'), (dip_val: $60; dip_name: '1C 6C'))), ());
  // Super Zaxxon
  szaxxon_rom: array [0 .. 2] of tipo_roms = ((n: '1804e.u27'; l: $2000; p: 0; crc: $AF7221DA), (n: '1803e.u28'; l: $2000; p: $2000; crc: $1B90FB2A), (n: '1802e.u29'; l: $1000; p: $4000; crc: $07258B4A));
  szaxxon_pal: array [0 .. 1] of tipo_roms = ((n: 'pr-5168.u98'; l: $100; p: 0; crc: $15727A9F), (n: 'pr-5167.u72'; l: $100; p: $100; crc: $DEAA21F7));
  szaxxon_char: array [0 .. 1] of tipo_roms = ((n: '1815b.u68'; l: $800; p: 0; crc: $BCCF560C), (n: '1816b.u69'; l: $800; p: $800; crc: $D28C628B));
  szaxxon_bg: array [0 .. 2] of tipo_roms = ((n: '1807b.u113'; l: $2000; p: 0; crc: $F51AF375), (n: '1806b.u112'; l: $2000; p: $2000; crc: $A7DE021D), (n: '1805b.u111'; l: $2000; p: $4000; crc: $5BFB3B04));
  szaxxon_sprites: array [0 .. 2] of tipo_roms = ((n: '1812e.u77'; l: $2000; p: 0; crc: $1503AE41), (n: '1813e.u78'; l: $2000; p: $2000; crc: $3B53D83F), (n: '1814e.u79'; l: $2000; p: $4000; crc: $581E8793));
  szaxxon_tilemap: array [0 .. 3] of tipo_roms = ((n: '1809b.u91'; l: $2000; p: 0; crc: $DD1B52DF), (n: '1808b.u90'; l: $2000; p: $2000; crc: $B5BC07F0), (n: '1811b.u93'; l: $2000; p: $4000; crc: $68E84174), (n: '1810b.u92'; l: $2000; p: $6000; crc: $A509994B));
  szaxxon_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Bonus Life'; number: 4; dip: ((dip_val: $3; dip_name: '10000'), (dip_val: $1; dip_name: '20000'), (dip_val: $2; dip_name: '30000'), (dip_val: $0; dip_name: '40000'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Difficulty'; number: 2; dip: ((dip_val: $4; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Lives'; number: 4;
    dip: ((dip_val: $30; dip_name: '3'), (dip_val: $10; dip_name: '4'), (dip_val: $20; dip_name: '5'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Sound'; number: 2;
    dip: ((dip_val: $40; dip_name: 'On'), (dip_val: $0; dip_name: 'Off'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Future Spy
  futspy_rom: array [0 .. 2] of tipo_roms = ((n: 'fs_snd.u27'; l: $2000; p: 0; crc: $7578FE7F), (n: 'fs_snd.u28'; l: $2000; p: $2000; crc: $8ADE203C), (n: 'fs_snd.u29'; l: $1000; p: $4000; crc: $734299C3));
  futspy_pal: array [0 .. 1] of tipo_roms = ((n: 'futrprom.u98'; l: $100; p: 0; crc: $9BA2ACAA), (n: 'futrprom.u72'; l: $100; p: $100; crc: $F9E26790));
  futspy_char: array [0 .. 1] of tipo_roms = ((n: 'fs_snd.u68'; l: $800; p: 0; crc: $305FAE2D), (n: 'fs_snd.u69'; l: $800; p: $800; crc: $3C5658C0));
  futspy_bg: array [0 .. 2] of tipo_roms = ((n: 'fs_vid.u113'; l: $2000; p: 0; crc: $36D2BDF6), (n: 'fs_vid.u112'; l: $2000; p: $2000; crc: $3740946A), (n: 'fs_vid.u111'; l: $2000; p: $4000; crc: $4CD4DF98));
  futspy_sprites: array [0 .. 2] of tipo_roms = ((n: 'fs_vid.u77'; l: $4000; p: 0; crc: $1B93C9EC), (n: 'fs_vid.u78'; l: $4000; p: $4000; crc: $50E55262), (n: 'fs_vid.u79'; l: $4000; p: $8000; crc: $BFB02E3E));
  futspy_tilemap: array [0 .. 3] of tipo_roms = ((n: 'fs_vid.u91'; l: $2000; p: 0; crc: $86DA01F4), (n: 'fs_vid.u90'; l: $2000; p: $2000; crc: $2BD41D2D), (n: 'fs_vid.u93'; l: $2000; p: $4000; crc: $B82B4997), (n: 'fs_vid.u92'; l: $2000; p: $6000; crc: $AF4015AF));
  futspy_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $08; dip_name: '4C 1C'), (dip_val: $07; dip_name: '3C 1C'), (dip_val: $06; dip_name: '2C 1C'), (dip_val: $0A; dip_name: '2C/1C 5C/3C 6C/4C'), (dip_val: $0B;
    dip_name: '2C/1C 4C/3C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $0E; dip_name: '1C/1C 2C/3C'), (dip_val: $0D; dip_name: '1C/1C 4C/5C'), (dip_val: $0C; dip_name: '1C/1C 5C/6C'), (dip_val: $09; dip_name: '2C 3C'), (dip_val: $01; dip_name: '1C 2C'), (dip_val: $0F;
    dip_name: '1C/2C 5C/11C'), (dip_val: $02; dip_name: '1C 3C'), (dip_val: $03; dip_name: '1C 4C'), (dip_val: $04; dip_name: '1C 5C'), (dip_val: $05; dip_name: '1C 6C'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $80; dip_name: '4C 1C'), (dip_val: $70; dip_name: '3C 1C'), (dip_val: $60; dip_name: '2C 1C'), (dip_val: $A0; dip_name: '2C/1C 5C/3C 6C/4C'), (dip_val: $B0; dip_name: '2C/1C 4C/3C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $E0;
    dip_name: '1C/1C 2C/3C'), (dip_val: $D0; dip_name: '1C/1C 4C/5C'), (dip_val: $C0; dip_name: '1C/1C 5C/6C'), (dip_val: $90; dip_name: '2C 3C'), (dip_val: $10; dip_name: '1C 2C'), (dip_val: $F0; dip_name: '1C/2C 5C/11C'), (dip_val: $20; dip_name: '1C 3C'), (dip_val: $30;
    dip_name: '1C 4C'), (dip_val: $40; dip_name: '1C 5C'), (dip_val: $50; dip_name: '1C 6C'))), ());
  futspy_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2; dip: ((dip_val: $1; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $2; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $4; dip_name: '4'), (dip_val: $8; dip_name: '5'), (dip_val: $C;
    dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4; dip: ((dip_val: $0; dip_name: '20K 40K 60K'), (dip_val: $10; dip_name: '30K 60K 90K'), (dip_val: $20; dip_name: '40K 70K 100K'), (dip_val: $30;
    dip_name: '40K 80K 120K'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Difficulty'; number: 4; dip: ((dip_val: $0; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Medium'), (dip_val: $80; dip_name: 'Hard'), (dip_val: $C0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  irq_vblank, bg_enable: boolean;
  congo_fg_bank, pal_offset, bg_position: word;
  bg_color, congo_color_bank, sound_latch, fg_color: byte;
  bg_mem: array [0 .. $FFF, 0 .. $FF] of byte;
  bg_mem_color: array [0 .. $1FF, 0 .. 31] of byte;
  congo_sprite, coin_status, sound_state: array [0 .. 2] of byte;
  coin_press: array [0 .. 1] of boolean;
  coin_enable: array [0 .. 2] of boolean;
  mem_dec: array [0 .. $5FFF] of byte;
  color_buffer: array [0 .. $FF] of byte;

function find_minimum_y(value: byte): byte;
var
  y: byte;
  sum: word;
begin
  // the sum of the Y position plus a constant based on the flip state
  // is added to the current flipped VF; if the top 3 bits are 1, we hit
  // first find a 16-pixel bucket where we hit
  for y := 0 to 15 do
  begin
    sum := (value + $F1 + 1) + (y * 16);
    if ((sum and $E0) = $E0) then
      break;
  end;
  y := y * 16;
  // then scan backwards until we no longer match
  while true do
  begin
    sum := (value + $F1 + 1) + (y - 1);
    if ((sum and $E0) <> $E0) then
      break;
    y := y - 1;
  end;
  // add one line since we draw sprites on the previous line
  find_minimum_y := (y + 1) and $FF;
end;

procedure draw_back;
var
  color, srcx, srcy: word;
  x, y: byte;
  pixel: array [0 .. $FF, 0 .. $FF] of word;
begin
  // Background
  if bg_enable then
  begin
    color := bg_color + (congo_color_bank shl 8);
    // loop over visible rows
    for x := 0 to $FF do
    begin
      // VF = flipped V signals
      // base of the source row comes from VF plus the scroll value
      // this is done by the 3 4-bit adders at U56, U74, U75
      srcx := x - (((bg_position - 127) shl 1) xor $FFF) - 1;
      // loop over visible columns
      for y := 0 to $FF do
      begin
        // start with HF = flipped H signals
        // position within source row is a two-stage addition
        // first stage is HF plus half the VF, done by the 2 4-bit
        // adders at U53, U54
        srcy := y - ((x shr 1) xor $FF) - 1;
        // second stage is first stage plus a constant based on the flip
        // value is 0x40 for non-flipped, or 0x38 for flipped
        srcy := srcy - $40;
        // store the pixel, offset by the color offset
        pixel[y, x] := paleta[bg_mem[srcx and $FFF, srcy and $FF] + bg_mem_color[(srcx and $FFF) shr 3, (srcy and $FF) shr 3] + color];
      end;
    end;
    putpixel(0, 0, $10000, @pixel, 3);
    update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 2);
  end
  else
    fill_full_screen(2, 0);
end;

procedure update_video_congo;
var
  f, x, y: word;
  color, nchar: byte;
  atrib, atrib2: byte;
begin
  draw_back;
  for f := $1F downto 0 do
  begin
    atrib := buffer_sprites[(f * 4) + 1];
    atrib2 := buffer_sprites[(f * 4) + 2];
    y := 224 - find_minimum_y(buffer_sprites[(f * 4) + 3]);
    nchar := atrib and $7F;
    color := (atrib2 and $1F) + (congo_color_bank shl 5);
    x := buffer_sprites[(f * 4) + 0] + $EF + 1;
    put_gfx_sprite(nchar, color shl 3, (atrib and $80) <> 0, (atrib2 and $80) <> 0, 2);
    update_gfx_sprite(x, y, 2, 2);
  end;
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f shr 5);
      y := f and $1F;
      color := (memory[$A400 + f] and $1F) shl 3;
      nchar := memory[$A000 + f] + congo_fg_bank;
      put_gfx_trans(x * 8, y * 8, nchar, color + pal_offset * 2, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_zaxxon;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // SW100
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or $4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or $8)
    else
      marcade.in2 := (marcade.in2 and $F7);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      coin_press[0] := true
    else
    begin
      if coin_press[0] then
        coin_status[0] := byte(coin_enable[0]) * $20;
      coin_press[0] := false;
    end;
    if p_contrls.map_arcade.coin[1] then
      coin_press[1] := true
    else
    begin
      if coin_press[1] then
        coin_status[1] := byte(coin_enable[1]) * $40;
      coin_press[1] := false;
    end;
  end;
end;

procedure congo_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 239 then
        begin
          if irq_vblank then
            z80_0.change_irq(ASSERT_LINE);
          update_video_congo;
        end;
      end;
      events_zaxxon;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function congo_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $8FFF:
      congo_getbyte := memory[direccion];
    $A000 .. $BFFF:
      congo_getbyte := memory[(direccion and $7FF) + $A000];
    $C000 .. $DFFF:
      case (direccion and $3F) of
        0, 4:
          congo_getbyte := marcade.in0;
        1, 5:
          congo_getbyte := marcade.in1;
        2, 6:
          congo_getbyte := marcade.dswa;
        3, 7:
          congo_getbyte := marcade.dswb;
        8 .. $F:
          congo_getbyte := marcade.in2 + coin_status[0] + coin_status[1];
      end;
  end;
end;

procedure congo_putbyte(direccion: word; valor: byte);
var
  saddr: word;
  count, daddr: byte;
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $8FFF:
      memory[direccion] := valor;
    $A000 .. $BFFF:
      if memory[(direccion and $7FF) + $A000] <> valor then
      begin
        memory[(direccion and $7FF) + $A000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $C000 .. $DFFF:
      case (direccion and $3F) of
        $18 .. $1A:
          begin
            coin_enable[direccion and $3] := (valor and 1) <> 0;
            if not(coin_enable[direccion and $3]) then
              coin_status[direccion and $3] := 0;
          end;
        $1B .. $1C:
          ; // coin_counter_w
        $1D:
          bg_enable := (valor and 1) <> 0;
        $1E:
          main_screen.flip_main_screen := (valor and 1) = 0;
        $1F:
          begin
            irq_vblank := (valor and 1) <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        $21:
          if fg_color <> ((valor and 1) shl 7) then
          begin
            fg_color := (valor and 1) shl 7;
            pal_offset := fg_color + (congo_color_bank shl 8);
            fillchar(gfx[0].buffer, $400, 1);
          end;
        $23:
          bg_color := (valor and 1) shl 7;
        $26:
          if congo_fg_bank <> ((valor and 1) shl 8) then
          begin
            congo_fg_bank := (valor and 1) shl 8;
            fillchar(gfx[0].buffer, $400, 1);
          end;
        $27:
          if congo_color_bank <> (valor and 1) then
          begin
            congo_color_bank := valor and 1;
            pal_offset := fg_color + (congo_color_bank shl 8);
            fillchar(gfx[0].buffer, $400, 1);
          end;
        $28, $2A, $2C, $2E:
          bg_position := (bg_position and $700) or valor;
        $29, $2B, $2D, $2F:
          bg_position := (bg_position and $FF) or ((valor and 7) shl 8);
        $30 .. $32:
          congo_sprite[direccion and $3] := valor;
        $33:
          if (valor = 1) then
          begin
            saddr := congo_sprite[0] or (congo_sprite[1] shl 8);
            // count cycles (just a guess)
            z80_0.contador := z80_0.contador + (count * 5);
            // this is just a guess; the chip is hardwired to the spriteram
            for count := 0 to congo_sprite[2] do
            begin
              daddr := memory[saddr + 0] * 4;
              buffer_sprites[(daddr + 0) and $FF] := memory[saddr + 1];
              buffer_sprites[(daddr + 1) and $FF] := memory[saddr + 2];
              buffer_sprites[(daddr + 2) and $FF] := memory[saddr + 3];
              buffer_sprites[(daddr + 3) and $FF] := memory[saddr + 4];
              saddr := saddr + $20;
            end;
          end;
        $38 .. $3F:
          sound_latch := valor;
      end;
  end;
end;

function snd_congo_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $1FFF:
      snd_congo_getbyte := mem_snd[direccion];
    $4000 .. $5FFF:
      snd_congo_getbyte := mem_snd[$4000 + (direccion and $7FF)];
    $8000 .. $9FFF:
      snd_congo_getbyte := pia8255_0.read(direccion and $3);
  end;
end;

procedure snd_congo_putbyte(direccion: word; valor: byte);
begin
  mem_snd[direccion] := valor;
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $4000 .. $5FFF:
      mem_snd[$4000 + (direccion and $7FF)] := valor;
    $6000 .. $7FFF:
      sn_76496_0.Write(valor);
    $8000 .. $9FFF:
      pia8255_0.Write(direccion and $3, valor);
    $A000 .. $BFFF:
      sn_76496_1.Write(valor);
  end;
end;

function ppi8255_congo_rporta: byte;
begin
  ppi8255_congo_rporta := sound_latch;
end;

procedure ppi8255_congo_wportb(valor: byte);
var
  diff: byte;
begin
  diff := valor xor sound_state[1];
  sound_state[1] := valor;
  // bit 7 = mute
  if (valor and $80) <> 0 then
    stop_all_samples;
  // GORILLA: channel 0
  if (((diff and $02) <> 0) and ((valor and $02) = 0)) then
    start_sample(0);
end;

procedure ppi8255_congo_wportc(valor: byte);
var
  diff: byte;
begin
  diff := valor xor sound_state[2];
  sound_state[2] := valor;
  // BASS DRUM: channel 1
  if (((diff and $01) <> 0) and ((valor and $01) = 0)) then
    start_sample(1);
  if (((diff and $01) <> 0) and ((valor and $01) <> 0)) then
    stop_sample(1);
  // CONGA (LOW): channel 2
  if (((diff and $02) <> 0) and ((valor and $02) = 0)) then
    start_sample(2);
  if (((diff and $02) <> 0) and ((valor and $02) <> 0)) then
    stop_sample(2);
  // CONGA (HIGH): channel 3
  if (((diff and $04) <> 0) and ((valor and $04) = 0)) then
    start_sample(3);
  if (((diff and $04) <> 0) and ((valor and $04) <> 0)) then
    stop_sample(3);
  // RIM: channel 4
  if (((diff and $08) <> 0) and ((valor and $08) = 0)) then
    start_sample(4);
  if (((diff and $08) <> 0) and ((valor and $08) <> 0)) then
    stop_sample(4);
end;

procedure congo_sound_update;
begin
  sn_76496_0.update;
  sn_76496_1.update;
  samples_update;
end;

procedure congo_sound_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

// Zaxxon
procedure update_video_zaxxon;
var
  f, x, y: word;
  nchar, color: byte;
  flipx, flipy: boolean;
begin
  draw_back;
  for f := $1F downto 0 do
  begin
    y := 224 - find_minimum_y(memory[$A003 + (f * 4)]);
    flipy := ((memory[$A002 + (f * 4)]) and $80) <> 0;
    flipx := ((memory[$A001 + (f * 4)]) and $80) <> 0;
    nchar := memory[$A001 + (f * 4)];
    color := memory[$A002 + (f * 4)] and $1F;
    x := memory[$A000 + (f * 4)] + $EF + 1;
    put_gfx_sprite(nchar, color shl 3, flipx, flipy, 2);
    update_gfx_sprite(x, y, 2, 2);
  end;
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f shr 5);
      y := f and $1F;
      color := (color_buffer[y + 32 * (f shr 7)] and $F) shl 3;
      nchar := memory[$8000 + f];
      put_gfx_trans(x * 8, y * 8, nchar, color + fg_color, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure zaxxon_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          if irq_vblank then
            z80_0.change_irq(ASSERT_LINE);
          update_video_zaxxon;
        end;
      end;
      events_zaxxon;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function zaxxon_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $6FFF:
      zaxxon_getbyte := memory[direccion];
    $8000 .. $9FFF:
      zaxxon_getbyte := memory[$8000 + (direccion and $3FF)];
    $A000 .. $BFFF:
      zaxxon_getbyte := memory[$A000 + (direccion and $FF)];
    $C000 .. $DFFF:
      case (direccion and $103) of
        $000:
          zaxxon_getbyte := marcade.in0;
        $001:
          zaxxon_getbyte := marcade.in1;
        $002:
          zaxxon_getbyte := marcade.dswa;
        $003:
          zaxxon_getbyte := marcade.dswb;
        $100:
          zaxxon_getbyte := marcade.in2 + coin_status[0] + coin_status[1]; // SW100
      end;
    $E000 .. $FFFF:
      case (direccion and $FF) of
        $3C .. $3F:
          zaxxon_getbyte := pia8255_0.read(direccion and $3);
      end;
  end;
end;

procedure zaxxon_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF:
      ; // ROM
    $6000 .. $6FFF:
      memory[direccion] := valor;
    $8000 .. $9FFF:
      if memory[(direccion and $3FF) + $8000] <> valor then
      begin
        memory[(direccion and $3FF) + $8000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $A000 .. $BFFF:
      memory[$A000 + (direccion and $FF)] := valor;
    $C000 .. $DFFF:
      case (direccion and $7) of
        0 .. 2:
          begin
            coin_enable[direccion and $3] := (valor and 1) <> 0;
            if not(coin_enable[direccion and $3]) then
              coin_status[direccion and $3] := 0;
          end;
        4:
          ; // coin_counter_w
        6:
          main_screen.flip_main_screen := (valor and 1) = 0;
      end;
    $E000 .. $FFFF:
      case (direccion and $FF) of
        $3C .. $3F:
          pia8255_0.Write(direccion and $3, valor);
        $F0:
          begin
            irq_vblank := (valor and 1) <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        $F1:
          if fg_color <> ((valor and 1) shl 7) then
          begin
            fg_color := (valor and 1) shl 7;
            fillchar(gfx[0].buffer, $400, 1);
          end;
        $F8:
          bg_position := (bg_position and $700) or valor;
        $F9:
          bg_position := (bg_position and $FF) or ((valor and 7) shl 8);
        $FA:
          bg_color := (valor and 1) shl 7;
        $FB:
          bg_enable := (valor and 1) <> 0;
      end;
  end;
end;

procedure ppi8255_zaxxon_wporta(valor: byte);
var
  diff: byte;
begin
  diff := valor xor sound_state[0];
  sound_state[0] := valor;
  // PLAYER SHIP A/B: volume
  change_vol_sample(10, 0.1 + 0.078 * (valor and $03));
  change_vol_sample(11, 0.1 + 0.078 * (valor and $03));
  // PLAYER SHIP C: channel 10
  if (((diff and $4) <> 0) and ((valor and $4) = 0)) then
    start_sample(10);
  if (((diff and $4) <> 0) and ((valor and $4) <> 0)) then
    stop_sample(10);
  // PLAYER SHIP D: channel 11
  if (((diff and $8) <> 0) and ((valor and $8) = 0)) then
    start_sample(11);
  if (((diff and $8) <> 0) and ((valor and $8) <> 0)) then
    stop_sample(11);
  // HOMING MISSILE: channel 0
  if (((diff and $10) <> 0) and ((valor and $10) = 0)) then
    start_sample(0);
  if (((diff and $10) <> 0) and ((valor and $10) <> 0)) then
    stop_sample(0);
  // BASE MISSILE: channel 1
  if (((diff and $20) <> 0) and ((valor and $20) = 0)) then
    start_sample(1);
  // LASER: channel 2
  if (((diff and $40) <> 0) and ((valor and $40) = 0)) then
    start_sample(2);
  if (((diff and $40) <> 0) and ((valor and $40) <> 0)) then
    stop_sample(2);
  // BATTLESHIP: channel 3
  if (((diff and $80) <> 0) and ((valor and $80) = 0)) then
    start_sample(3);
  if (((diff and $80) <> 0) and ((valor and $80) <> 0)) then
    stop_sample(3);
end;

procedure ppi8255_zaxxon_wportb(valor: byte);
var
  diff: byte;
begin
  diff := valor xor sound_state[1];
  sound_state[1] := valor;
  // S-EXP: channel 4
  if (((diff and $10) <> 0) and ((valor and $10) = 0)) then
    start_sample(4);
  // M-EXP: channel 5
  if (((diff and $20) <> 0) and ((valor and $20) = 0)) then
    start_sample(5);
  // CANNON: channel 6
  if (((diff and $80) <> 0) and ((valor and $80) = 0)) then
    start_sample(6);
end;

procedure ppi8255_zaxxon_wportc(valor: byte);
var
  diff: byte;
begin
  diff := valor xor sound_state[2];
  sound_state[2] := valor;
  // SHOT: channel 7
  if (((diff and $1) <> 0) and ((valor and $1) = 0)) then
    start_sample(7);
  // ALARM2: channel 8
  if (((diff and $4) <> 0) and ((valor and $4) = 0)) then
    start_sample(8);
  // ALARM3: channel 9
  if (((diff and $8) <> 0) and ((valor and $8) = 0)) then
    start_sample(9);
end;

procedure zaxxon_sound_update;
begin
  samples_update;
end;

// Encrypted Z80
function enc_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $6FFF:
      if z80_0.opcode then
        enc_getbyte := mem_dec[direccion]
      else
        enc_getbyte := memory[direccion];
  else
    enc_getbyte := zaxxon_getbyte(direccion);
  end;
end;

procedure reset_zaxxon;
begin
  z80_0.reset;
  if main_vars.machine_type = 175 then
  begin
    z80_1.reset;
    sn_76496_0.reset;
    sn_76496_1.reset;
  end;
  reset_samples;
  pia8255_0.reset;
 reset_video;
  reset_audio;
  irq_vblank := false;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  congo_fg_bank := 0;
  congo_color_bank := 0;
  pal_offset := 0;
  fg_color := 0;
  bg_enable := false;
  bg_position := 0;
  fillchar(congo_sprite, 3, 0);
  fillchar(coin_enable, 3, 0);
  fillchar(coin_status, 3, 0);
  fillchar(coin_press, 2, 0);
  sound_latch := 0;
end;

function start_zaxxon: boolean;
var
  memory_temp: array [0 .. $FFFF] of byte;
const
  ps_x: array [0 .. 31] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2,
    24 * 8 + 3, 24 * 8 + 4, 24 * 8 + 5, 24 * 8 + 6, 24 * 8 + 7);
  ps_y: array [0 .. 31] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8, 64 * 8, 65 * 8, 66 * 8, 67 * 8, 68 * 8, 69 * 8, 70 * 8, 71 * 8, 96 * 8, 97 * 8, 98 * 8, 99 * 8, 100 * 8, 101 * 8,
    102 * 8, 103 * 8);
  resistances: array [0 .. 2] of integer = (1000, 470, 220);
  procedure conv_chars;
  begin
    init_gfx(0, 8, 8, 256);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(2, 0, 8 * 8, 256 * 8 * 8, 0);
    convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;
  procedure conv_background;
  begin
    init_gfx(1, 8, 8, 1024);
    gfx_set_desc_data(3, 0, 8 * 8, 2 * 1024 * 8 * 8, 1024 * 8 * 8, 0);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;
  procedure conv_static_background(size: word);
  var
    f, sx, sy, nchar: word;
    atrib, x, y: byte;
    pos: pbyte;
  begin
    for f := 0 to (size - 1) do
    begin
      sx := ((size shr 5) - 1) - (f shr 5);
      sy := f and $1F;
      atrib := memory_temp[f + size];
      bg_mem_color[sx, sy] := (atrib and $F0) shr 1;
      nchar := memory_temp[f] + ((atrib and $3) * 256);
      pos := gfx[1].datos;
      inc(pos, nchar * 8 * 8);
      for y := 0 to 7 do
      begin
        for x := 0 to 7 do
        begin
          bg_mem[sx * 8 + x, sy * 8 + y] := pos^;
          inc(pos);
        end;
      end;
    end;
  end;
  procedure conv_sprites(size: word);
  begin
    init_gfx(2, 32, 32, size);
    gfx[2].trans[0] := true;
    gfx_set_desc_data(3, 0, 128 * 8, 2 * size * 128 * 8, 128 * size * 8, 0);
    convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;
  procedure convert_palette(size: word);
  var
    colores: tpaleta;
    f: word;
    bit0, bit1, bit2: byte;
    rweights, gweights, bweights: array [0 .. 2] of single;
  begin
    compute_resistor_weights(0, 255, -1.0, 3, @resistances, @rweights, 470, 0, 3, @resistances, @gweights, 470, 0, 2, @resistances[1], @bweights, 470, 0);
    for f := 0 to (size - 1) do
    begin
      bit0 := (memory_temp[f] shr 0) and $01;
      bit1 := (memory_temp[f] shr 1) and $01;
      bit2 := (memory_temp[f] shr 2) and $01;
      colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
      bit0 := (memory_temp[f] shr 3) and $01;
      bit1 := (memory_temp[f] shr 4) and $01;
      bit2 := (memory_temp[f] shr 5) and $01;
      colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
      bit0 := (memory_temp[f] shr 6) and $01;
      bit1 := (memory_temp[f] shr 7) and $01;
      colores[f].b := combine_2_weights(@bweights, bit0, bit1);
    end;
    set_pal(colores, size);
    copymemory(@color_buffer, @memory_temp[$100], $100);
  end;

begin
  machine_calls.reset := reset_zaxxon;
  machine_calls.fps_max := 59.999408;
  start_zaxxon := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 256, 256, false, true);
  screen_init(3, 256, 256);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3041250, 264);
  case main_vars.machine_type of
    175:
      begin // Congo Bongo
        machine_calls.general_loop := congo_loop;
        z80_0.change_ram_calls(congo_getbyte, congo_putbyte);
        // Sound
        z80_1 := cpu_z80.create(4000000, 264);
        z80_1.change_ram_calls(snd_congo_getbyte, snd_congo_putbyte);
        timers.init(z80_1.numero_cpu, 4000000 / (4000000 / 16 / 16 / 16 / 4), congo_sound_irq, nil, true);
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(ppi8255_congo_rporta, nil, nil, nil, ppi8255_congo_wportb, ppi8255_congo_wportc);
        // Samples
        load_samples(congo_samples);
        z80_1.init_sound(congo_sound_update);
        sn_76496_0 := sn76496_chip.create(4000000);
        sn_76496_1 := sn76496_chip.create(1000000);
        // cargar roms
        if not(roms_load(@memory, congo_rom)) then
          exit;
        // cargar sonido & iniciar_sonido
        if not(roms_load(@mem_snd, congo_sound)) then
          exit;
        if not(roms_load(@memory_temp, congo_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, congo_bg)) then
          exit;
        conv_background;
        // convertir sprites
        if not(roms_load(@memory_temp, congo_sprites)) then
          exit;
        conv_sprites($80);
        // poner la paleta
        if not(roms_load(@memory_temp, congo_pal)) then
          exit;
        convert_palette($200);
        // background
        if not(roms_load(@memory_temp, congo_tilemap)) then
          exit;
        conv_static_background($2000);
        // DIP
        marcade.dswa := $77;
        marcade.dswa_val := @congo_dip_a;
        marcade.dswb := $33;
        marcade.dswb_val := @zaxxon_dip_b;
      end;
    188:
      begin // Zaxxon
        machine_calls.general_loop := zaxxon_loop;
        z80_0.change_ram_calls(zaxxon_getbyte, zaxxon_putbyte);
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(nil, nil, nil, ppi8255_zaxxon_wporta, ppi8255_zaxxon_wportb, ppi8255_zaxxon_wportc);
        // Samples
        if load_samples(zaxxon_samples) then
          z80_0.init_sound(zaxxon_sound_update);
        // cargar roms
        if not(roms_load(@memory, zaxxon_rom)) then
          exit;
        if not(roms_load(@memory_temp, zaxxon_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, zaxxon_bg)) then
          exit;
        conv_background;
        // convertir sprites
        if not(roms_load(@memory_temp, zaxxon_sprites)) then
          exit;
        conv_sprites($40);
        // poner la paleta
        if not(roms_load(@memory_temp, zaxxon_pal)) then
          exit;
        convert_palette($100);
        // Background
        if not(roms_load(@memory_temp, zaxxon_tilemap)) then
          exit;
        conv_static_background($4000);
        // DIP
        marcade.dswa := $7F;
        marcade.dswa_val := @zaxxon_dip_a;
        marcade.dswb := $33;
        marcade.dswb_val := @zaxxon_dip_b;
      end;
    346:
      begin // Super Zaxxon
        machine_calls.general_loop := zaxxon_loop;
        z80_0.change_ram_calls(enc_getbyte, zaxxon_putbyte);
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(nil, nil, nil, ppi8255_zaxxon_wporta, ppi8255_zaxxon_wportb, ppi8255_zaxxon_wportc);
        // Samples
        if load_samples(zaxxon_samples, 1, 'zaxxon.zip') then
          z80_0.init_sound(zaxxon_sound_update);
        // cargar roms
        if not(roms_load(@memory, szaxxon_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 7);
        if not(roms_load(@memory_temp, szaxxon_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, szaxxon_bg)) then
          exit;
        conv_background;
        // convertir sprites
        if not(roms_load(@memory_temp, szaxxon_sprites)) then
          exit;
        conv_sprites($40);
        // poner la paleta
        if not(roms_load(@memory_temp, szaxxon_pal)) then
          exit;
        convert_palette($100);
        // Background
        if not(roms_load(@memory_temp, szaxxon_tilemap)) then
          exit;
        conv_static_background($4000);
        // DIP
        marcade.dswa := $7F;
        marcade.dswa_val := @szaxxon_dip_a;
        marcade.dswb := $33;
        marcade.dswb_val := @zaxxon_dip_b;
      end;
    347:
      begin // Future Spy
        machine_calls.general_loop := zaxxon_loop;
        z80_0.change_ram_calls(enc_getbyte, zaxxon_putbyte);
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(nil, nil, nil, ppi8255_zaxxon_wporta, ppi8255_zaxxon_wportb, ppi8255_zaxxon_wportc);
        // Samples
        if load_samples(zaxxon_samples, 1, 'zaxxon.zip') then
          z80_0.init_sound(zaxxon_sound_update);
        // cargar roms
        if not(roms_load(@memory, futspy_rom)) then
          exit;
        decrypt_sega(@memory, @mem_dec, 8);
        if not(roms_load(@memory_temp, futspy_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, futspy_bg)) then
          exit;
        conv_background;
        // convertir sprites
        if not(roms_load(@memory_temp, futspy_sprites)) then
          exit;
        conv_sprites($80);
        // poner la paleta
        if not(roms_load(@memory_temp, futspy_pal)) then
          exit;
        convert_palette($100);
        // Background
        if not(roms_load(@memory_temp, futspy_tilemap)) then
          exit;
        conv_static_background($4000);
        // DIP
        marcade.dswa := $0;
        marcade.dswa_val := @futspy_dip_a;
        marcade.dswb := $43;
        marcade.dswb_val := @futspy_dip_b;
      end;
  end;
  // final
  reset_zaxxon;
  start_zaxxon := true;
end;

end.
