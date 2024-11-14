unit freekick_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  gfx_engine,
  rom_engine,
  timer_engine,
  pal_engine,
  sound_engine,
  ppi8255,
  mc8123;

function start_freekick: boolean;

implementation

const
  // Freekick
  freekick_rom: tipo_roms = (n: 'ns6201-a_1987.10_free_kick.cpu'; l: $D000; p: 0; crc: $6D172850);
  freekick_sound_data: tipo_roms = (n: '11.1e'; l: $8000; p: 0; crc: $A6030BA9);
  freekick_pal: array [0 .. 5] of tipo_roms = ((n: '24s10n.8j'; l: $100; p: 0; crc: $53A6BC21), (n: '24s10n.7j'; l: $100; p: $100; crc: $38DD97D8), (n: '24s10n.8k'; l: $100; p: $200; crc: $18E66087),
    (n: '24s10n.7k'; l: $100; p: $300; crc: $BC21797A), (n: '24s10n.8h'; l: $100; p: $400; crc: $8AAC5FD0), (n: '24s10n.7h'; l: $100; p: $500; crc: $A507F941));
  freekick_chars: array [0 .. 2] of tipo_roms = ((n: '12.1h'; l: $4000; p: 0; crc: $FB82E486), (n: '13.1j'; l: $4000; p: $4000; crc: $3AD78EE2), (n: '14.1l'; l: $4000; p: $8000; crc: $0185695F));
  freekick_sprites: array [0 .. 2] of tipo_roms = ((n: '15.1m'; l: $4000; p: 0; crc: $0FA7C13C), (n: '16.1p'; l: $4000; p: $4000; crc: $2B996E89), (n: '17.1r'; l: $4000; p: $8000; crc: $E7894DEF));
  // Gigas
  gigas_rom: array [0 .. 1] of tipo_roms = ((n: '8.8n'; l: $4000; p: 0; crc: $34EA8262), (n: '7.8r'; l: $8000; p: $4000; crc: $43653909));
  gigas_key: tipo_roms = (n: '317-5002.key'; l: $2000; p: 0; crc: $86A7E5F6);
  gigas_pal: array [0 .. 5] of tipo_roms = ((n: 'tbp24s10n.3a'; l: $100; p: 0; crc: $A784E71F), (n: 'tbp24s10n.4d'; l: $100; p: $100; crc: $376DF30C), (n: 'tbp24s10n.4a'; l: $100; p: $200;
    crc: $4EDFF5BD), (n: 'tbp24s10n.3d'; l: $100; p: $300; crc: $FE201A4E), (n: 'tbp24s10n.3b'; l: $100; p: $400; crc: $5796CC4A), (n: 'tbp24s10n.3c'; l: $100; p: $500; crc: $28B5EE4C));
  gigas_chars: array [0 .. 2] of tipo_roms = ((n: '4.3k'; l: $4000; p: 0; crc: $8ED78981), (n: '5.3h'; l: $4000; p: $4000; crc: $0645EC2D), (n: '6.3g'; l: $4000; p: $8000; crc: $99E9CB27));
  gigas_sprites: array [0 .. 2] of tipo_roms = ((n: '1.3p'; l: $4000; p: 0; crc: $D78FAE6E), (n: '3.3l'; l: $4000; p: $4000; crc: $37DF4A4C), (n: '2.3n'; l: $4000; p: $8000; crc: $3A46E354));
  // Gigas Mark II
  gigasm2_rom: array [0 .. 1] of tipo_roms = ((n: '18.8n'; l: $4000; p: 0; crc: $32E83D80), (n: '17.8r'; l: $8000; p: $4000; crc: $460DADD2));
  gigasm2_chars: array [0 .. 2] of tipo_roms = ((n: '14.3k'; l: $4000; p: 0; crc: $20B3405F), (n: '15.3h'; l: $4000; p: $4000; crc: $D04ECFA8), (n: '16.3g'; l: $4000; p: $8000; crc: $33776801));
  gigasm2_sprites: array [0 .. 2] of tipo_roms = ((n: '11.3p'; l: $4000; p: 0; crc: $F64CBD1E), (n: '13.3l'; l: $4000; p: $4000; crc: $C228DF19), (n: '12.3n'; l: $4000; p: $8000; crc: $A6AD9CE2));
  // Omega
  omega_rom: array [0 .. 1] of tipo_roms = ((n: '17.m10'; l: $4000; p: 0; crc: $C7DE0993), (n: '8.n10'; l: $8000; p: $4000; crc: $9BB61910));
  omega_key: tipo_roms = (n: 'omega.key'; l: $2000; p: 0; crc: $0A63943F);
  omega_pal: array [0 .. 5] of tipo_roms = ((n: 'tbp24s10n.3f'; l: $100; p: 0; crc: $75EC7472), (n: 'tbp24s10n.4f'; l: $100; p: $100; crc: $5113A114), (n: 'tbp24s10n.3g'; l: $100; p: $200;
    crc: $B6B5D4A0), (n: 'tbp24s10n.4g'; l: $100; p: $300; crc: $931BC299), (n: 'tbp24s10n.3e'; l: $100; p: $400; crc: $899E089D), (n: 'tbp24s10n.4e'; l: $100; p: $500; crc: $28321DD8));
  omega_chars: array [0 .. 2] of tipo_roms = ((n: '4.f10'; l: $4000; p: 0; crc: $BF780A8E), (n: '5.h10'; l: $4000; p: $4000; crc: $B491647F), (n: '6.j10'; l: $4000; p: $8000; crc: $65BEBA5B));
  omega_sprites: array [0 .. 2] of tipo_roms = ((n: '3.d10'; l: $4000; p: 0; crc: $C678B202), (n: '1.a10'; l: $4000; p: $4000; crc: $E0AEADA9), (n: '2.c10'; l: $4000; p: $8000; crc: $DBC0A47F));
  // Perfect Billiard
  pbillrd_rom: array [0 .. 2] of tipo_roms = ((n: 'pb.18'; l: $4000; p: 0; crc: $9E6275AC), (n: 'pb.7'; l: $8000; p: $4000; crc: $DD438431), (n: 'pb.9'; l: $4000; p: $C000; crc: $089CE80A));
  pbillrd_pal: array [0 .. 5] of tipo_roms = ((n: '82s129.3a'; l: $100; p: 0; crc: $44802169), (n: '82s129.4d'; l: $100; p: $100; crc: $69CA07CC), (n: '82s129.4a'; l: $100; p: $200; crc: $145F950A),
    (n: '82s129.3d'; l: $100; p: $300; crc: $43D24E17), (n: '82s129.3b'; l: $100; p: $400; crc: $7FDC872C), (n: '82s129.3c'; l: $100; p: $500; crc: $CC1657E5));
  pbillrd_chars: array [0 .. 2] of tipo_roms = ((n: 'pb.4'; l: $4000; p: 0; crc: $2F4D4DD3), (n: 'pb.5'; l: $4000; p: $4000; crc: $9DFCCBD3), (n: 'pb.6'; l: $4000; p: $8000; crc: $B5C3F6F6));
  pbillrd_sprites: array [0 .. 2] of tipo_roms = ((n: '10619.3r'; l: $2000; p: 0; crc: $3296B9D9), (n: '10621.3m'; l: $2000; p: $2000; crc: $3DCA8E4B), (n: '10620.3n'; l: $2000; p: $4000;
    crc: $EE76B079));
  // Dip
  freekick_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Lives'; number: 2; dip: ((dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $6; name: 'Bonus Life'; number: 4; dip: ((dip_val: $6; dip_name: '20K 30K 40K 50K 60K'), (dip_val: $2; dip_name: '30K 40K 50K 60K 70K 80K'), (dip_val: $4;
    dip_name: '20K 60K'), (dip_val: $0; dip_name: '20K Only'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $18; dip_name: 'Easy'), (dip_val: $10; dip_name: 'Normal'), (dip_val: $8; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $40; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  freekick_dip_b: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $C; dip_name: '4C 1C'), (dip_val: $E;
    dip_name: '3C 1C'), (dip_val: $5; dip_name: '2C 1C'), (dip_val: $6; dip_name: '3C 2C'), (dip_val: $4; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $8;
    dip_name: '4C 5C'), (dip_val: $A; dip_name: '3C 4C'), (dip_val: $9; dip_name: '2C 3C'), (dip_val: $2; dip_name: '3C 5C'), (dip_val: $7; dip_name: '1C 2C'), (dip_val: $1;
    dip_name: '2C 5C'), (dip_val: $B; dip_name: '1C 3C'), (dip_val: $3; dip_name: '1C 4C'), (dip_val: $D; dip_name: '1C 5C'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $E0; dip_name: '3C 1C'), (dip_val: $50; dip_name: '2C 1C'), (dip_val: $60; dip_name: '3C 2C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $A0;
    dip_name: '3C 4C'), (dip_val: $90; dip_name: '2C 3C'), (dip_val: $20; dip_name: '3C 5C'), (dip_val: $70; dip_name: '1C 2C'), (dip_val: $10; dip_name: '2C 5C'), (dip_val: $B0;
    dip_name: '1C 3C'), (dip_val: $30; dip_name: '1C 4C'), (dip_val: $D0; dip_name: '1C 5C'), (dip_val: $C0; dip_name: '1C 10C'), (dip_val: $40; dip_name: '1C 25C'), (dip_val: $80;
    dip_name: '1C 50C'))), ());
  freekick_dip_c: array [0 .. 2] of def_dip = ((mask: $1; name: 'Manufacturer'; number: 2; dip: ((dip_val: $0; dip_name: 'Nihon System'), (dip_val: $1; dip_name: 'Sega/Nihon System'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Coin Slots'; number: 2; dip: ((dip_val: $0; dip_name: '1'), (dip_val: $80; dip_name: '2'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), ());
  gigas_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Lives'; number: 2; dip: ((dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $6; name: 'Bonus Life'; number: 4; dip: ((dip_val: $6; dip_name: '20K 60K Every 60K'), (dip_val: $2; dip_name: '20K 60K'), (dip_val: $4; dip_name: '30K 80K Every 80K'), (dip_val: $0;
    dip_name: '20K Only'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $18; dip_name: 'Easy'), (dip_val: $10; dip_name: 'Normal'), (dip_val: $8; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $40; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  gigas_dip_b: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $C; dip_name: '4C 1C'), (dip_val: $E;
    dip_name: '3C 1C'), (dip_val: $5; dip_name: '2C 1C'), (dip_val: $6; dip_name: '3C 2C'), (dip_val: $4; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $8;
    dip_name: '4C 5C'), (dip_val: $A; dip_name: '3C 4C'), (dip_val: $9; dip_name: '2C 3C'), (dip_val: $2; dip_name: '3C 5C'), (dip_val: $7; dip_name: '1C 2C'), (dip_val: $1;
    dip_name: '2C 5C'), (dip_val: $B; dip_name: '1C 3C'), (dip_val: $3; dip_name: '1C 4C'), (dip_val: $D; dip_name: '1C 5C'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $C0; dip_name: '4C 1C'), (dip_val: $E0; dip_name: '3C 1C'), (dip_val: $50; dip_name: '2C 1C'), (dip_val: $60; dip_name: '3C 2C'), (dip_val: $40;
    dip_name: '4C 3C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $80; dip_name: '4C 5C'), (dip_val: $A0; dip_name: '3C 4C'), (dip_val: $90; dip_name: '2C 3C'), (dip_val: $20;
    dip_name: '3C 5C'), (dip_val: $70; dip_name: '1C 2C'), (dip_val: $10; dip_name: '2C 5C'), (dip_val: $B0; dip_name: '1C 3C'), (dip_val: $30; dip_name: '1C 4C'), (dip_val: $D0;
    dip_name: '1C 5C'))), ());
  omega_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Lives'; number: 2; dip: ((dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $6; name: 'Bonus Life'; number: 4; dip: ((dip_val: $6; dip_name: '20K 60K Every 60K'), (dip_val: $2; dip_name: '30K 80K Every 80K'), (dip_val: $4; dip_name: '20K 60K'), (dip_val: $0;
    dip_name: '20K Only'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $18; dip_name: 'Easy'), (dip_val: $10; dip_name: 'Normal'), (dip_val: $8; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $20; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $40; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  omega_dip_b: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $E; dip_name: '3C 1C'), (dip_val: $5;
    dip_name: '2C 1C'), (dip_val: $6; dip_name: '3C 2C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $A; dip_name: '3C 4C'), (dip_val: $9; dip_name: '2C 3C'), (dip_val: $2;
    dip_name: '3C 5C'), (dip_val: $7; dip_name: '1C 2C'), (dip_val: $1; dip_name: '2C 5C'), (dip_val: $B; dip_name: '1C 3C'), (dip_val: $3; dip_name: '1C 4C'), (dip_val: $D;
    dip_name: '1C 5C'), (dip_val: $C; dip_name: '1C 10C'), (dip_val: $4; dip_name: '1C 25C'), (dip_val: $8; dip_name: '1C 50C'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $E0; dip_name: '3C 1C'), (dip_val: $50; dip_name: '2C 1C'), (dip_val: $60; dip_name: '3C 2C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $A0;
    dip_name: '3C 4C'), (dip_val: $90; dip_name: '2C 3C'), (dip_val: $20; dip_name: '3C 5C'), (dip_val: $70; dip_name: '1C 2C'), (dip_val: $10; dip_name: '2C 5C'), (dip_val: $B0;
    dip_name: '1C 3C'), (dip_val: $30; dip_name: '1C 4C'), (dip_val: $D0; dip_name: '1C 5C'), (dip_val: $C0; dip_name: '1C 10C'), (dip_val: $40; dip_name: '1C 25C'), (dip_val: $80;
    dip_name: '1C 50C'))), ());
  omega_dip_c: array [0 .. 3] of def_dip = ((mask: $1; name: 'Hopper Status?'; number: 2; dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $2; name: 'Invulnerability'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C0; name: 'Prize Version'; number: 4; dip: ((dip_val: $C0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On Setting 1'), (dip_val: $40; dip_name: 'On Setting 2'), (dip_val: $0;
    dip_name: 'On Setting 3'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  pbillrd_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Balls'; number: 2; dip: ((dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $6; name: 'Bonus Ball'; number: 4; dip: ((dip_val: $6; dip_name: '10K 30K 50K'), (dip_val: $2; dip_name: '20K 60K'), (dip_val: $4; dip_name: '30K 80K'), (dip_val: $0;
    dip_name: '20K Only'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $10; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Shot'; number: 2;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $20; dip_name: '3'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $40; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  spinner, nmi_enable: boolean;
  snd_rom_addr: word;
  rom_index, freekick_ff: byte;
  video_mem: array [0 .. $7FF] of byte;
  sprite_mem: array [0 .. $FF] of byte;
  rom: array [0 .. 1, 0 .. $3FFF] of byte;
  draw_sprites: procedure;

procedure sprites_freekick;
var
  f, atrib, color, x, y: byte;
  nchar: word;
begin
  for f := 0 to $3F do
  begin
    atrib := sprite_mem[$2 + (f * 4)];
    nchar := sprite_mem[$1 + (f * 4)] + ((atrib and $20) shl 3);
    color := (atrib and $1F) shl 3;
    y := 240 - sprite_mem[$3 + (f * 4)];
    x := 248 - sprite_mem[$0 + (f * 4)];
    put_gfx_sprite(nchar, color + $100, (atrib and $40) <> 0, (atrib and $80) <> 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
end;

procedure sprites_gigas;
var
  f, atrib, color, x, y: byte;
  nchar: word;
begin
  for f := 0 to $3F do
  begin
    atrib := sprite_mem[$1 + (f * 4)];
    nchar := sprite_mem[$0 + (f * 4)] + ((atrib and $20) shl 3);
    color := (atrib and $1F) shl 3;
    y := 240 - sprite_mem[$3 + (f * 4)];
    x := 240 - sprite_mem[$2 + (f * 4)];
    put_gfx_sprite(nchar, color + $100, false, false, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
end;

procedure sprites_pbillrd;
var
  f, atrib, color, x, y, nchar: byte;
begin
  for f := 0 to $3F do
  begin
    atrib := sprite_mem[$1 + (f * 4)];
    nchar := sprite_mem[$0 + (f * 4)];
    color := (atrib and $F) shl 3;
    y := 240 - sprite_mem[$3 + (f * 4)];
    x := 240 - sprite_mem[$2 + (f * 4)];
    put_gfx_sprite(nchar, color + $100, false, false, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
end;

procedure update_video_freekick;
var
  f, nchar: word;
  x, y, color, atrib: byte;
begin
  for f := $3FF downto 0 do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      atrib := video_mem[f + $400];
      color := (atrib and $1F) shl 3;
      nchar := video_mem[f] + ((atrib and $E0) shl 3);
      put_gfx(x * 8, y * 8, nchar, color, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  // sprites
  draw_sprites;
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_freekick;
begin
  if event.arcade then
  begin
    // IN0
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // IN1
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure freekick_loop;
var
  f: word;
  frame_m: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 262 do
      begin
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          update_video_freekick;
          if nmi_enable then
            z80_0.change_nmi(ASSERT_LINE);
        end;
      end;
      events_freekick;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Free Kick
function freekick_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $DFFF:
      freekick_getbyte := memory[direccion];
    $E000 .. $E7FF:
      freekick_getbyte := video_mem[direccion and $7FF];
    $E800 .. $E8FF:
      freekick_getbyte := sprite_mem[direccion and $FF];
    $EC00 .. $EC03:
      freekick_getbyte := pia8255_0.read(direccion and $3);
    $F000 .. $F003:
      freekick_getbyte := pia8255_1.read(direccion and $3);
    $F800:
      freekick_getbyte := marcade.in0;
    $F801:
      freekick_getbyte := marcade.in1;
    $F802:
      freekick_getbyte := 0;
    $F803:
      if spinner then
        freekick_getbyte := analog.c[0].x[0]
      else
        freekick_getbyte := analog.c[0].x[1];
  end;
end;

procedure freekick_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $CFFF:
      ; // ROM
    $D000 .. $DFFF:
      memory[direccion] := valor;
    $E000 .. $E7FF:
      if video_mem[direccion and $7FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        video_mem[direccion and $7FF] := valor;
      end;
    $E800 .. $E8FF:
      sprite_mem[direccion and $FF] := valor;
    $EC00 .. $EC03:
      pia8255_0.write(direccion and $3, valor);
    $F000 .. $F003:
      pia8255_1.write(direccion and $3, valor);
    $F804:
      begin
        nmi_enable := (valor and 1) <> 0;
        if not(nmi_enable) then
          z80_0.change_nmi(CLEAR_LINE);
      end;
    $F806:
      spinner := (valor and 1) = 0;
    $FC00:
      sn_76496_0.write(valor);
    $FC01:
      sn_76496_1.write(valor);
    $FC02:
      sn_76496_2.write(valor);
    $FC03:
      sn_76496_3.write(valor);
  end;
end;

function freekick_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = $FF then
    freekick_inbyte := freekick_ff;
end;

procedure freekick_outbyte(puerto: word; valor: byte);
begin
  if (puerto and $FF) = $FF then
    freekick_ff := valor;
end;

function ppi0_c_read: byte;
begin
  ppi0_c_read := mem_misc[snd_rom_addr];
end;

procedure ppi0_a_write(valor: byte);
begin
  snd_rom_addr := (snd_rom_addr and $FF00) or valor;
end;

procedure ppi0_b_write(valor: byte);
begin
  snd_rom_addr := (snd_rom_addr and $FF) or (valor shl 8);
end;

function ppi1_a_read: byte;
begin
  ppi1_a_read := marcade.dswa;
end;

function ppi1_b_read: byte;
begin
  ppi1_b_read := marcade.dswb;
end;

function ppi1_c_read: byte;
begin
  ppi1_c_read := marcade.dswc;
end;

// Gigas
function gigas_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      if z80_0.opcode then
        gigas_getbyte := mem_misc[direccion]
      else
        gigas_getbyte := memory[direccion];
    $C000 .. $CFFF, $D900 .. $DFFF:
      gigas_getbyte := memory[direccion];
    $D000 .. $D7FF:
      gigas_getbyte := video_mem[direccion and $7FF];
    $D800 .. $D8FF:
      gigas_getbyte := sprite_mem[direccion and $FF];
    $E000:
      gigas_getbyte := marcade.in0;
    $E800:
      gigas_getbyte := marcade.in1;
    $F000:
      gigas_getbyte := marcade.dswa;
    $F800:
      gigas_getbyte := marcade.dswb;
  end;
end;

procedure gigas_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF, $D900 .. $DFFF:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if video_mem[direccion and $7FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        video_mem[direccion and $7FF] := valor;
      end;
    $D800 .. $D8FF:
      sprite_mem[direccion and $FF] := valor;
    $E004:
      begin
        nmi_enable := (valor and 1) <> 0;
        if not(nmi_enable) then
          z80_0.change_nmi(CLEAR_LINE);
      end;
    $FC00:
      sn_76496_0.write(valor);
    $FC01:
      sn_76496_1.write(valor);
    $FC02:
      sn_76496_2.write(valor);
    $FC03:
      sn_76496_3.write(valor);
  end;
end;

function gigas_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      if spinner then
        gigas_inbyte := analog.c[0].x[0]
      else
        gigas_inbyte := analog.c[0].x[1];
    1:
      gigas_inbyte := marcade.dswc;
  end;
end;

procedure gigas_outbyte(puerto: word; valor: byte);
begin
  if (puerto and $FF) = 0 then
    spinner := (valor and 1) = 0;
end;

// Perfect Billard
function pbillrd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      pbillrd_getbyte := memory[direccion];
    $8000 .. $BFFF:
      pbillrd_getbyte := rom[rom_index, direccion and $3FFF];
    $C000 .. $CFFF, $D900 .. $DFFF:
      pbillrd_getbyte := memory[direccion];
    $D000 .. $D7FF:
      pbillrd_getbyte := video_mem[direccion and $7FF];
    $D800 .. $D8FF:
      pbillrd_getbyte := sprite_mem[direccion and $FF];
    $E000:
      pbillrd_getbyte := marcade.in0;
    $E800:
      pbillrd_getbyte := marcade.in1;
    $F000:
      pbillrd_getbyte := marcade.dswa;
    $F800:
      pbillrd_getbyte := marcade.dswb;
  end;
end;

procedure pbillrd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF, $D900 .. $DFFF:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if video_mem[direccion and $7FF] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        video_mem[direccion and $7FF] := valor;
      end;
    $D800 .. $D8FF:
      sprite_mem[direccion and $FF] := valor;
    $E004:
      begin
        nmi_enable := (valor and 1) <> 0;
        if not(nmi_enable) then
          z80_0.change_nmi(CLEAR_LINE);
      end;
    $F000:
      rom_index := valor and 1;
    $FC00:
      sn_76496_0.write(valor);
    $FC01:
      sn_76496_1.write(valor);
    $FC02:
      sn_76496_2.write(valor);
    $FC03:
      sn_76496_3.write(valor);
  end;
end;

// Sound
procedure freeckick_snd_irq;
begin
  z80_0.change_irq(HOLD_LINE);
end;

procedure freekick_sound_update;
begin
  sn_76496_0.update;
  sn_76496_1.update;
  sn_76496_2.update;
  sn_76496_3.update;
end;

// Main
procedure reset_freekick;
begin
  z80_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  sn_76496_2.reset;
  sn_76496_3.reset;
  if main_vars.machine_type = 211 then
  begin
    pia8255_0.reset;
    pia8255_1.reset;
  end;
 reset_video;
  reset_audio;
  snd_rom_addr := 0;
  spinner := false;
  nmi_enable := false;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
 reset_analog;
  rom_index := 0;
end;

function start_freekick: boolean;
var
  colores: tpaleta;
  f: word;
  clock: dword;
  bit0, bit1, bit2, bit3: byte;
  memory_temp: array [0 .. $FFFF] of byte;
  mem_key: array [0 .. $1FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 128 + 0, 128 + 1, 128 + 2, 128 + 3, 128 + 4, 128 + 5, 128 + 6, 128 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  procedure convert_chars(n: word);
  begin
    init_gfx(0, 8, 8, n);
    gfx_set_desc_data(3, 0, 8 * 8, n * 2 * 8 * 8, n * 1 * 8 * 8, n * 0 * 8 * 8);
    convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, true);
  end;
  procedure convert_sprites(n: word);
  begin
    init_gfx(1, 16, 16, n);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(3, 0, 16 * 16, n * 0 * 16 * 16, n * 2 * 16 * 16, n * 1 * 16 * 16);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
  end;

begin
  start_freekick := false;
  machine_calls.general_loop := freekick_loop;
  machine_calls.reset := reset_freekick;
  if main_vars.machine_type = 273 then
    machine_calls.fps_max := 60.836502
  else
    machine_calls.fps_max := 59.410646;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, false, true);
  if main_vars.machine_type = 274 then
  begin
    start_video(256, 224);
    main_screen.rot90_screen := true;
  end
  else
    start_video(224, 256);
  if main_vars.machine_type = 273 then
  begin
    clock := 3072000;
    machine_calls.fps_max := 60.836502
  end
  else
  begin
    clock := 3000000;
    machine_calls.fps_max := 59.410646;
  end;
  if main_vars.machine_type = 273 then
    clock := 3072000
  else
    clock := 3000000;
  // Main CPU
  z80_0 := cpu_z80.create(clock, 263);
  z80_0.init_sound(freekick_sound_update);
  // Sound Chips
  sn_76496_0 := sn76496_chip.create(clock);
  sn_76496_1 := sn76496_chip.create(clock);
  sn_76496_2 := sn76496_chip.create(clock);
  sn_76496_3 := sn76496_chip.create(clock);
  // IRQ Sound CPU
  timers.init(z80_0.numero_cpu, clock / 120, freeckick_snd_irq, nil, true);
  case main_vars.machine_type of
    211:
      begin // Free Kick
        z80_0.change_ram_calls(freekick_getbyte, freekick_putbyte);
        z80_0.change_io_calls(freekick_inbyte, freekick_outbyte);
        // analog
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(20, 10, $7F, $FF, 0, false);
        // PPI
        pia8255_0 := pia8255_chip.create;
        pia8255_0.change_ports(nil, nil, ppi0_c_read, ppi0_a_write, ppi0_b_write, nil);
        pia8255_1 := pia8255_chip.create;
        pia8255_1.change_ports(ppi1_a_read, ppi1_b_read, ppi1_c_read, nil, nil, nil);
        // cargar roms
        if not(roms_load(@memory, freekick_rom)) then
          exit;
        // snd rom
        if not(roms_load(@mem_misc, freekick_sound_data)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, freekick_chars)) then
          exit;
        convert_chars($800);
        // convertir sprites
        if not(roms_load(@memory_temp, freekick_sprites)) then
          exit;
        convert_sprites($200);
        draw_sprites := sprites_freekick;
        // poner la paleta
        if not(roms_load(@memory_temp, freekick_pal)) then
          exit;
        // DIP
        marcade.dswa := $BF;
        marcade.dswb := $FF;
        marcade.dswc := $80;
        marcade.dswa_val := @freekick_dip_a;
        marcade.dswb_val := @freekick_dip_b;
        marcade.dswc_val := @freekick_dip_c;
      end;
    271:
      begin // Gigas
        z80_0.change_ram_calls(gigas_getbyte, gigas_putbyte);
        z80_0.change_io_calls(gigas_inbyte, gigas_outbyte);
        // analog
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(20, 10, $7F, $FF, 0, false);
        // cargar y desencriptar ROMS
        if not(roms_load(@memory_temp, gigas_rom)) then
          exit;
        if not(roms_load(@mem_key, gigas_key)) then
          exit;
        copymemory(@memory, @memory_temp, $C000);
        mc8123_decrypt_rom(@mem_key, @memory, @mem_misc, $C000);
        // convertir chars
        if not(roms_load(@memory_temp, gigas_chars)) then
          exit;
        convert_chars($800);
        // convertir sprites
        if not(roms_load(@memory_temp, gigas_sprites)) then
          exit;
        convert_sprites($200);
        draw_sprites := sprites_gigas;
        // poner la paleta
        if not(roms_load(@memory_temp, gigas_pal)) then
          exit;
        // DIP
        marcade.dswa := $3F;
        marcade.dswb := $FF;
        marcade.dswa_val := @gigas_dip_a;
        marcade.dswb_val := @gigas_dip_b;
      end;
    272:
      begin // Gigas Mark II
        z80_0.change_ram_calls(gigas_getbyte, gigas_putbyte);
        z80_0.change_io_calls(gigas_inbyte, gigas_outbyte);
        // analog
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(20, 10, $7F, $FF, 0, false);
        // cargar y desencriptar ROMS
        if not(roms_load(@memory_temp, gigasm2_rom)) then
          exit;
        if not(roms_load(@mem_key, gigas_key)) then
          exit;
        copymemory(@memory, @memory_temp, $C000);
        mc8123_decrypt_rom(@mem_key, @memory, @mem_misc, $C000);
        // convertir chars
        if not(roms_load(@memory_temp, gigasm2_chars)) then
          exit;
        convert_chars($800);
        // convertir sprites
        if not(roms_load(@memory_temp, gigasm2_sprites)) then
          exit;
        convert_sprites($200);
        draw_sprites := sprites_gigas;
        // poner la paleta
        if not(roms_load(@memory_temp, gigas_pal)) then
          exit;
        // DIP
        marcade.dswa := $3F;
        marcade.dswb := $FF;
        marcade.dswa_val := @gigas_dip_a;
        marcade.dswb_val := @gigas_dip_b;
      end;
    273:
      begin // Omega
        z80_0.change_ram_calls(gigas_getbyte, gigas_putbyte);
        z80_0.change_io_calls(gigas_inbyte, gigas_outbyte);
        // analog
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(20, 10, $7F, $FF, 0, false);
        // cargar y desencriptar ROMS
        if not(roms_load(@memory_temp, omega_rom)) then
          exit;
        if not(roms_load(@mem_key, omega_key)) then
          exit;
        copymemory(@memory, @memory_temp, $C000);
        mc8123_decrypt_rom(@mem_key, @memory, @mem_misc, $C000);
        // convertir chars
        if not(roms_load(@memory_temp, omega_chars)) then
          exit;
        convert_chars($800);
        // convertir sprites
        if not(roms_load(@memory_temp, omega_sprites)) then
          exit;
        convert_sprites($200);
        draw_sprites := sprites_gigas;
        // poner la paleta
        if not(roms_load(@memory_temp, omega_pal)) then
          exit;
        // DIP
        marcade.dswa := $3F;
        marcade.dswb := $FF;
        marcade.dswc := $FF;
        marcade.dswa_val := @omega_dip_a;
        marcade.dswb_val := @omega_dip_b;
        marcade.dswc_val := @omega_dip_c;
      end;
    274:
      begin // Perfect Billiard
        z80_0.change_ram_calls(pbillrd_getbyte, pbillrd_putbyte);
        // cargar y desencriptar ROMS
        if not(roms_load(@memory_temp, pbillrd_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $8000);
        copymemory(@rom[0, 0], @memory_temp[$8000], $4000);
        copymemory(@rom[1, 0], @memory_temp[$C000], $4000);
        // convertir chars
        if not(roms_load(@memory_temp, pbillrd_chars)) then
          exit;
        convert_chars($800);
        // convertir sprites
        if not(roms_load(@memory_temp, pbillrd_sprites)) then
          exit;
        convert_sprites($100);
        draw_sprites := sprites_pbillrd;
        // poner la paleta
        if not(roms_load(@memory_temp, pbillrd_pal)) then
          exit;
        // DIP
        marcade.dswa := $1F;
        marcade.dswb := $FF;
        marcade.dswa_val := @pbillrd_dip_a;
        marcade.dswb_val := @gigas_dip_b;
      end;
  end;
  // Pal
  for f := 0 to $1FF do
  begin
    // red
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    bit3 := (memory_temp[f] shr 3) and 1;
    colores[f].r := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    // green
    bit0 := (memory_temp[f + $200] shr 0) and 1;
    bit1 := (memory_temp[f + $200] shr 1) and 1;
    bit2 := (memory_temp[f + $200] shr 2) and 1;
    bit3 := (memory_temp[f + $200] shr 3) and 1;
    colores[f].g := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    // blue
    bit0 := (memory_temp[f + $400] shr 0) and 1;
    bit1 := (memory_temp[f + $400] shr 1) and 1;
    bit2 := (memory_temp[f + $400] shr 2) and 1;
    bit3 := (memory_temp[f + $400] shr 3) and 1;
    colores[f].b := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  end;
  set_pal(colores, $200);
  // final
  reset_freekick;
  start_freekick := true;
end;

end.
