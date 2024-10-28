unit seta_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  m6502,
  seta_sprites,
  ym_2203,
  ym_3812,
  x1_010,
  misc_functions;

function start_seta: boolean;

implementation

const
  // Thundercade
  tndrcade_rom: array [0 .. 3] of tipo_roms = ((n: 'ua0-4.u19'; l: $20000; p: 0; crc: $73BD63EB),
    (n: 'ua0-2.u17'; l: $20000; p: $1; crc: $E96194B1), (n: 'ua0-3.u18'; l: $20000; p: $40000;
    crc: $0A7B1C41), (n: 'ua0-1.u16'; l: $20000; p: $40001; crc: $FA906626));
  tndrcade_snd: tipo_roms = (n: 'ua10-5.u24'; l: $20000; p: 0; crc: $8EFF6122);
  tndrcade_sprites: array [0 .. 7] of tipo_roms = ((n: 'ua0-10.u12'; l: $40000; p: 0;
    crc: $AA7B6757), (n: 'ua0-11.u13'; l: $40000; p: $40000; crc: $11EAF931), (n: 'ua0-12.u14';
    l: $40000; p: $80000; crc: $00B5381C), (n: 'ua0-13.u15'; l: $40000; p: $C0000; crc: $8F9A0ED3),
    (n: 'ua0-6.u8'; l: $40000; p: $100000; crc: $14ECC7BB), (n: 'ua0-7.u9'; l: $40000; p: $140000;
    crc: $FF1A4E68), (n: 'ua0-8.u10'; l: $40000; p: $180000; crc: $936E1884), (n: 'ua0-9.u11';
    l: $40000; p: $1C0000; crc: $E812371C));
  tndrcade_dip: array [0 .. 10] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $2; dip_name: 'Easy'), (dip_val: $3; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $C; dip_name: '50K'), (dip_val: $4; dip_name: '50K 150K+'), (dip_val: $0;
    dip_name: '70K 200K+'), (dip_val: $8; dip_name: '100K'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $30; name: 'Lives'; number: 4;
    dip: ((dip_val: $10; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $30;
    dip_name: '3'), (dip_val: $20; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Licensed To'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Taito America Corp.'), (dip_val: $0;
    dip_name: 'Taito Corp. Japan'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $100; name: 'Title'; number: 2; dip: ((dip_val: $100; dip_name: 'Thundercade'),
    (dip_val: $0; dip_name: 'Twin Formation'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $200; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $200; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $800; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $800; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $3000; name: 'Coin A'; number: 4;
    dip: ((dip_val: $1000; dip_name: '2C 1C'), (dip_val: $3000; dip_name: '1C 1C'), (dip_val: $0;
    dip_name: '2C 3C'), (dip_val: $2000; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C000; name: 'Coin B'; number: 4;
    dip: ((dip_val: $4000; dip_name: '2C 1C'), (dip_val: $C000; dip_name: '1C 1C'), (dip_val: $0;
    dip_name: '2C 3C'), (dip_val: $8000; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());
  // Twin Eagle
  twineagl_rom: tipo_roms = (n: 'ua2-1'; l: $80000; p: 0; crc: $5C3FE531);
  twineagl_snd: tipo_roms = (n: 'ua2-2'; l: $2000; p: 0; crc: $783CA84E);
  twineagl_sprites: array [0 .. 3] of tipo_roms = ((n: 'ua2-4'; l: $40000; p: 1; crc: $8B7532D6),
    (n: 'ua2-3'; l: $40000; p: $0; crc: $1124417A), (n: 'ua2-6'; l: $40000; p: $80001;
    crc: $99D8DBBA), (n: 'ua2-5'; l: $40000; p: $80000; crc: $6E450D28));
  twineagl_tiles: array [0 .. 3] of tipo_roms = ((n: 'ua2-7'; l: $80000; p: 0; crc: $FCE56907),
    (n: 'ua2-8'; l: $80000; p: $1; crc: $7D3A8D73), (n: 'ua2-9'; l: $80000; p: $100000;
    crc: $A451EAE9), (n: 'ua2-10'; l: $80000; p: $100001; crc: $5BBE1F56));
  twineagl_pcm: array [0 .. 1] of tipo_roms = ((n: 'ua2-11'; l: $80000; p: 0; crc: $624E6057),
    (n: 'ua2-12'; l: $80000; p: $80000; crc: $3068FF64));
  twineagl_dip: array [0 .. 10] of def_dip = ((mask: $1; name: 'Copyright / License'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Taito America / Romstar'), (dip_val: $1;
    dip_name: 'Taito Corp Japan'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $2; name: 'Flip Screen'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $8;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30;
    name: 'Coin A'; number: 4; dip: ((dip_val: $10; dip_name: '2C 1C'), (dip_val: $30;
    dip_name: '1C 1C'), (dip_val: $0; dip_name: '2C 3C'), (dip_val: $20; dip_name: '1C 2C'), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Coin B'; number: 4;
    dip: ((dip_val: $40; dip_name: '2C 1C'), (dip_val: $C0; dip_name: '1C 1C'), (dip_val: $0;
    dip_name: '2C 3C'), (dip_val: $80; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $300; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $200; dip_name: 'Easy'), (dip_val: $300; dip_name: 'Normal'), (dip_val: $100;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C00; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $C00; dip_name: 'Never'), (dip_val: $800; dip_name: '500K'), (dip_val: $400;
    dip_name: '1000K'), (dip_val: $8; dip_name: '500K 1500K'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $3000; name: 'Lives'; number: 4;
    dip: ((dip_val: $1000; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $3000;
    dip_name: '3'), (dip_val: $2000; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $4000; name: 'Licensor Option'; number: 2;
    dip: ((dip_val: $4000; dip_name: 'Option 1'), (dip_val: $0; dip_name: 'Option 2'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $8000; name: 'Coinage Type'; number: 2;
    dip: ((dip_val: $8000; dip_name: 'Coin Mode 2'), (dip_val: $0; dip_name: 'Coin Mode 2'), (), (),
    (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Thunder & Lightning
  thunderl_rom: array [0 .. 1] of tipo_roms = ((n: 'm4'; l: $8000; p: 0; crc: $1E6B9462), (n: 'm5';
    l: $8000; p: $1; crc: $7E82793E));
  thunderl_sprites: array [0 .. 3] of tipo_roms = ((n: 't17'; l: $20000; p: 1; crc: $599A632A),
    (n: 't16'; l: $20000; p: $0; crc: $3AEEF91C), (n: 't15'; l: $20000; p: $40001; crc: $B97A7B56),
    (n: 't14'; l: $20000; p: $40000; crc: $79C707BE));
  thunderl_pcm: array [0 .. 1] of tipo_roms = ((n: 'r28'; l: $80000; p: 0; crc: $A043615D),
    (n: 'r27'; l: $80000; p: $80000; crc: $CB8425A3));
  thunderl_dip_a: array [0 .. 2] of def_dip = ((mask: $10; name: 'Force 1 Life'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $E0; name: 'Copyright'; number: 8;
    dip: ((dip_val: $80; dip_name: 'Romstar'), (dip_val: $C0; dip_name: 'Seta (Romstar License)'),
    (dip_val: $E0; dip_name: 'Seta (Visco License)'), (dip_val: $A0; dip_name: 'Visco'),
    (dip_val: $60; dip_name: 'None'), (dip_val: $40; dip_name: 'None'), (dip_val: $20;
    dip_name: 'None'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), ())), ());
  thunderl_dip_b: array [0 .. 8] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
    dip: ((dip_val: $0C; dip_name: '4C 1C'), (dip_val: $0D; dip_name: '3C 1C'), (dip_val: $08;
    dip_name: '4C 2C'), (dip_val: $0E; dip_name: '2C 1C'), (dip_val: $09;
    dip_name: '3C 2C'), (dip_val: $04; dip_name: '4C 3C'), (dip_val: $0;
    dip_name: '4C 4C'), (dip_val: $05; dip_name: '3C 3C'), (dip_val: $0A;
    dip_name: '3C 3C'), (dip_val: $0F; dip_name: '1C 1C'), (dip_val: $01;
    dip_name: '3C 4C'), (dip_val: $06; dip_name: '2C 3C'), (dip_val: $02;
    dip_name: '2C 4C'), (dip_val: $0B; dip_name: '1C 2C'), (dip_val: $07;
    dip_name: '1C 3C'), (dip_val: $03; dip_name: '1C 4C'))), (mask: $F0; name: 'Coin B'; number: 16;
    dip: ((dip_val: $C0; dip_name: '4C 1C'), (dip_val: $D0; dip_name: '3C 1C'), (dip_val: $80;
    dip_name: '4C 2C'), (dip_val: $E0; dip_name: '2C 1C'), (dip_val: $90;
    dip_name: '3C 2C'), (dip_val: $40; dip_name: '4C 3C'), (dip_val: $0;
    dip_name: '4C 4C'), (dip_val: $50; dip_name: '3C 3C'), (dip_val: $A0;
    dip_name: '3C 3C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $10;
    dip_name: '3C 4C'), (dip_val: $60; dip_name: '2C 3C'), (dip_val: $20;
    dip_name: '2C 4C'), (dip_val: $B0; dip_name: '1C 2C'), (dip_val: $70;
    dip_name: '1C 3C'), (dip_val: $30; dip_name: '1C 4C'))), (mask: $200; name: 'Flip Screen';
    number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $200; dip_name: 'On'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $400; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $400; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $800; name: 'Controls'; number: 2;
    dip: ((dip_val: $800; dip_name: '2'), (dip_val: $0; dip_name: '1'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $1000; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $1000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $2000; name: 'Lives'; number: 2;
    dip: ((dip_val: $2000; dip_name: '3'), (dip_val: $0; dip_name: '2'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $C000; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $8000; dip_name: 'Easy'), (dip_val: $C000; dip_name: 'Normal'), (dip_val: $4000;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), ());

var
  rom: array [0 .. $4FFFF] of word;
  ram: array [0 .. $7FFFF] of word;
  shared_ram: array [0 .. $7FF] of byte;
  scanlines_proc: procedure(line: byte);
  eventos_proc: procedure;
  // Twin Eagle
  vram0: array [0 .. $1FFF] of word;
  tilebank: array [0 .. 3] of word;
  scroll_x, scroll_y, vram_control: word;
  xtra_mem: array [0 .. 7] of byte;
  // Sound
  rom_snd: array [0 .. $F, 0 .. $3FFF] of byte;
  sound_latch0, sound_latch1, snd_bank: byte;
  control_data: word;
  // Thunder & lighting
  thunderl_protection_reg: word;

procedure update_video_seta_sprites;
begin
  if (seta_sprite0.bg_flag and $80) = 0 then
    fill_full_screen(1, $1F0);
  seta_sprite0.draw_sprites;
  update_final_piece(0, 16, 384, 224, 1);
end;

procedure change_color(pos, data: word);
var
  color: tcolor;
begin
  color.r := pal5bit(data shr 10);
  color.g := pal5bit(data shr 5);
  color.b := pal5bit(data);
  set_pal_color(color, pos);
  buffer_color[pos shr 4] := true;
end;

procedure events_seta;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // coins
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
  end;
end;

procedure seta_loop_snd_cpu;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 255 do
    begin
      // main
      m68000_0.run(frame_m);
      frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
      // sound
      m6502_0.run(frame_s);
      frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
      scanlines_proc(f);
    end;
    events_seta;
    video_sync;
  end;
end;

procedure seta_loop;
var
  frame_m: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 255 do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        scanlines_proc(f);
      end;
      eventos_proc;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure seta_sound_update;
begin
  x1_010_0.update;
end;

function seta_sprite_cb(code, color: word): word;
var
  bank: byte;
begin
  bank := (color and $6) shr 1;
  code := (code and $3FFF) + (bank * $4000);
  seta_sprite_cb := code;
end;

// Thundercade
procedure tndrcade_scan_lines(line: byte);
begin
  case line of
    0, 16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224:
      m6502_0.change_irq(HOLD_LINE);
    239:
      begin
        m68000_0.irq[2] := ASSERT_LINE;
        m6502_0.change_nmi(PULSE_LINE);
        m6502_0.change_irq(HOLD_LINE);
        update_video_seta_sprites;
      end;
  end;
end;

function tndrcade_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      tndrcade_getword := rom[direccion shr 1];
    $380000 .. $3803FF:
      tndrcade_getword := buffer_paleta[(direccion and $3FF) shr 1];
    $600000 .. $6005FF:
      tndrcade_getword := seta_sprite0.spritey[(direccion and $FFF) shr 1];
    $600600 .. $600607:
      tndrcade_getword := seta_sprite0.control[(direccion and $7) shr 1];
    $A00000 .. $A00FFF:
      tndrcade_getword := shared_ram[(direccion and $FFF) shr 1];
    $C00000 .. $C03FFF:
      tndrcade_getword := seta_sprite0.spritelow[(direccion and $3FFF) shr 1] +
        (seta_sprite0.spritehigh[(direccion and $3FFF) shr 1] shl 8);
    $E00000 .. $E03FFF, $FFC000 .. $FFFFFF:
      tndrcade_getword := ram[(direccion and $3FFF) shr 1];
  end;
end;

procedure tndrcade_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $7FFFF:
      ;
    $200000:
      m68000_0.irq[2] := CLEAR_LINE;
    $380000 .. $3803FF:
      if buffer_paleta[(direccion and $3FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $3FF) shr 1] := valor;
        change_color((direccion and $3FF) shr 1, valor);
      end;
    $600000 .. $6005FF:
      seta_sprite0.spritey[(direccion and $7FF) shr 1] := valor and $FF;
    $600600 .. $600607:
      seta_sprite0.control[(direccion and $7) shr 1] := valor and $FF;
    $800000 .. $800007:
      case ((direccion and $7) shr 1) of
        0:
          begin
            if (((control_data and 1) = 0) and ((valor and 1) <> 0)) then
              m6502_0.change_reset(HOLD_LINE);
            control_data := valor;
          end;
        1:
          ;
        2:
          ;
        3:
          ;
      end;
    $A00000 .. $A00FFF:
      shared_ram[(direccion and $FFF) shr 1] := valor and $FF;
    $C00000 .. $C03FFF:
      begin
        seta_sprite0.spritelow[(direccion and $3FFF) shr 1] := valor and $FF;
        seta_sprite0.spritehigh[(direccion and $3FFF) shr 1] := valor shr 8;
      end;
    $E00000 .. $E03FFF, $FFC000 .. $FFFFFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function tndrcade_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FF, $6000 .. $7FFF, $C000 .. $FFFF:
      tndrcade_snd_getbyte := mem_snd[direccion];
    $800:
      tndrcade_snd_getbyte := $FF;
    $1000:
      tndrcade_snd_getbyte := marcade.in0; // p1
    $1001:
      tndrcade_snd_getbyte := marcade.in1; // p2
    $1002:
      tndrcade_snd_getbyte := marcade.in2; // coin
    $2000:
      tndrcade_snd_getbyte := ym2203_0.status;
    $2001:
      tndrcade_snd_getbyte := ym2203_0.read;
    $5000 .. $57FF:
      tndrcade_snd_getbyte := shared_ram[direccion and $7FF];
    $8000 .. $BFFF:
      tndrcade_snd_getbyte := rom_snd[snd_bank, direccion and $3FFF];
  end;
end;

procedure tndrcade_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FF:
      mem_snd[direccion] := valor;
    $1000:
      snd_bank := valor shr 4;
    $2000:
      ym2203_0.control(valor);
    $2001:
      ym2203_0.write(valor);
    $3000:
      ym3812_0.control(valor);
    $3001:
      ym3812_0.write(valor);
    $5000 .. $57FF:
      shared_ram[direccion and $7FF] := valor;
    $6000 .. $FFFF:
      ;
  end;
end;

function tndrcade_porta_read: byte;
begin
  tndrcade_porta_read := marcade.dswa shr 8;
end;

function tndrcade_portb_read: byte;
begin
  tndrcade_portb_read := marcade.dswa and $FF;
end;

procedure tndrcade_sound_update;
begin
  ym2203_0.update;
  ym3812_0.update;
end;

// Twin Eagle
procedure update_video_twineagl;
var
  x, y, f, nchar, pos: word;
  color: byte;
begin
  // Hay dos pantallas una en 0 y la otra en $1000
  pos := (vram_control and 8) shl 9;
  for f := $0 to $7FF do
  begin
    nchar := vram0[f + pos];
    color := vram0[$800 + pos + f] and $1F;
    if ((nchar and $3E00) = $3E00) then
      nchar := (nchar and $C07F) or ((tilebank[(nchar and $0180) shr 7] shr 1) shl 7);
    if (gfx[1].buffer[pos + f] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      put_gfx_flip(x * 16, y * 16, nchar and $3FFF, color shl 4, 2, 1, (nchar and $4000) <> 0,
        (nchar and $8000) <> 0);
      gfx[1].buffer[pos + f] := false;
    end;
  end;
  scroll_x_y(2, 1, scroll_x, scroll_y);
  seta_sprite0.draw_sprites;
  update_final_piece(0, 16, 384, 224, 1);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure twineagl_scan_lines(line: byte);
begin
  case line of
    112:
      m6502_0.change_irq(HOLD_LINE);
    239:
      begin
        m68000_0.irq[3] := ASSERT_LINE;
        m6502_0.change_nmi(PULSE_LINE);
        update_video_twineagl;
      end;
  end;
end;

function twineagl_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $9FFFF:
      twineagl_getword := rom[direccion shr 1];
    $100000 .. $103FFF:
      twineagl_getword := x1_010_0.read16((direccion and $3FFF) shr 1);
    $200100 .. $20010F:
      twineagl_getword := xtra_mem[(direccion and $F) shr 1];
    $600000 .. $600003:
      case ((direccion and 3) shr 1) of
        0:
          twineagl_getword := marcade.dswa shr 8;
        1:
          twineagl_getword := marcade.dswa and $FF;
      end;
    $700000 .. $7003FF:
      twineagl_getword := buffer_paleta[(direccion and $3FF) shr 1];
    $900000 .. $903FFF:
      twineagl_getword := vram0[(direccion and $3FFF) shr 1];
    $B00000 .. $B00FFF:
      twineagl_getword := shared_ram[(direccion and $FFF) shr 1];
    $D00000 .. $D005FF:
      twineagl_getword := seta_sprite0.spritey[(direccion and $FFF) shr 1];
    $D00600 .. $D00607:
      twineagl_getword := seta_sprite0.control[(direccion and $7) shr 1];
    $E00000 .. $E03FFF:
      twineagl_getword := seta_sprite0.spritelow[(direccion and $3FFF) shr 1] +
        (seta_sprite0.spritehigh[(direccion and $3FFF) shr 1] shl 8);
    $F00000 .. $FFFFFF:
      twineagl_getword := ram[(direccion and $FFFFF) shr 1];
  end;
end;

procedure twineagl_putword(direccion: dword; valor: word);
var
  pos: word;
begin
  case direccion of
    0 .. $9FFFF:
      ;
    $100000 .. $103FFF:
      x1_010_0.write16((direccion and $3FFF) shr 1, valor); // x10
    $200100 .. $20010F:
      xtra_mem[(direccion and $F) shr 1] := valor and $FF;
    $300000:
      ;
    $400000 .. $400007:
      if tilebank[(direccion and 7) shr 1] <> valor then
      begin
        tilebank[(direccion and 7) shr 1] := valor;
        fillchar(gfx[1].buffer, $2000, 1);
      end;
    $500000 .. $500001:
      if m68000_0.write_8bits_hi_dir then
      begin
        if (valor and $30) = 0 then
          m68000_0.irq[3] := CLEAR_LINE;
      end;
    $700000 .. $7003FF:
      if buffer_paleta[(direccion and $3FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $3FF) shr 1] := valor;
        change_color((direccion and $3FF) shr 1, valor);
      end;
    $800000 .. $800005:
      case ((direccion and 7) shr 1) of // VRAM Ctrl
        0:
          scroll_x := valor + 16;
        1:
          scroll_y := valor - 8;
        2:
          if vram_control <> valor then
          begin
            vram_control := valor;
            fillchar(gfx[1].buffer, $2000, 1);
          end;
      end;
    $900000 .. $903FFF:
      begin
        pos := (direccion and $3FFF) shr 1;
        if vram0[pos] <> valor then
        begin
          vram0[pos] := valor;
          gfx[1].buffer[(pos and $7FF) + (pos and $1000)] := true;
        end;
      end;
    $A00000 .. $A00007:
      case ((direccion and $7) shr 1) of
        0:
          begin
            if (((control_data and 1) = 0) and ((valor and 1) <> 0)) then
              m6502_0.change_reset(HOLD_LINE);
            control_data := valor;
          end;
        1:
          ;
        2:
          sound_latch0 := valor and $FF;
        3:
          sound_latch1 := valor and $FF;
      end;
    $B00000 .. $B00FFF:
      shared_ram[(direccion and $FFF) shr 1] := valor and $FF;
    $D00000 .. $D005FF:
      seta_sprite0.spritey[(direccion and $7FF) shr 1] := valor and $FF;
    $D00600 .. $D00607:
      seta_sprite0.control[(direccion and $7) shr 1] := valor and $FF;
    $E00000 .. $E03FFF:
      begin
        seta_sprite0.spritelow[(direccion and $3FFF) shr 1] := valor and $FF;
        seta_sprite0.spritehigh[(direccion and $3FFF) shr 1] := valor shr 8;
      end;
    $F00000 .. $FFFFFF:
      ram[(direccion and $FFFFF) shr 1] := valor;
  end;
end;

function twineagl_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FF, $7000 .. $FFFF:
      twineagl_snd_getbyte := mem_snd[direccion];
    $800:
      twineagl_snd_getbyte := sound_latch0;
    $801:
      twineagl_snd_getbyte := sound_latch1;
    $1000:
      twineagl_snd_getbyte := marcade.in0; // p1
    $1001:
      twineagl_snd_getbyte := marcade.in1; // p2
    $1002:
      twineagl_snd_getbyte := marcade.in2; // coin
    $5000 .. $57FF:
      twineagl_snd_getbyte := shared_ram[direccion and $7FF];
  end;
end;

procedure twineagl_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FF:
      mem_snd[direccion] := valor;
    $1000:
      ;
    $5000 .. $57FF:
      shared_ram[direccion and $7FF] := valor;
    $7000 .. $FFFF:
      ;
  end;
end;

// Thunder & Ligthning
procedure thunderl_events;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // coins
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
  end;
end;

procedure thunderl_scan_lines(line: byte);
begin
  case line of
    239:
      begin
        m68000_0.irq[2] := ASSERT_LINE;
        update_video_seta_sprites;
      end;
  end;
end;

function thunderl_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFF:
      thunderl_getword := rom[direccion shr 1];
    $100000 .. $103FFF:
      thunderl_getword := x1_010_0.read16((direccion and $3FFF) shr 1);
    $200000:
      m68000_0.irq[2] := CLEAR_LINE;
    $600000 .. $600003:
      case ((direccion and 3) shr 1) of
        0:
          thunderl_getword := marcade.dswb shr 8;
        1:
          thunderl_getword := marcade.dswb and $FF;
      end;
    $700000 .. $7003FF:
      thunderl_getword := buffer_paleta[(direccion and $3FF) shr 1];
    $B00000:
      thunderl_getword := marcade.in0;
    $B00002:
      thunderl_getword := marcade.in1;
    $B00004:
      thunderl_getword := (marcade.in2 and $F) or marcade.dswa;
    $B00008:
      thunderl_getword := $FFFF; // p3
    $B0000A:
      thunderl_getword := $FFFF; // p4
    $B0000C:
      thunderl_getword := thunderl_protection_reg;
    $C00000:
      thunderl_getword := ram[$4000];
    $D00000 .. $D005FF:
      thunderl_getword := seta_sprite0.spritey[(direccion and $FFF) shr 1];
    $D00600 .. $D00607:
      thunderl_getword := seta_sprite0.control[(direccion and $7) shr 1];
    $E00000 .. $E03FFF:
      thunderl_getword := seta_sprite0.spritelow[(direccion and $3FFF) shr 1] +
        (seta_sprite0.spritehigh[(direccion and $3FFF) shr 1] shl 8);
    $E04000 .. $E07FFF:
      thunderl_getword := ram[$2000 + ((direccion and $3FFF) shr 1)];
    $FFC000 .. $FFFFFF:
      thunderl_getword := ram[(direccion and $3FFF) shr 1];
  end;
end;

procedure thunderl_putword(direccion: dword; valor: word);
var
  addr: dword;
begin
  case direccion of
    0 .. $FFFF:
      ;
    $100000 .. $103FFF:
      x1_010_0.write16((direccion and $3FFF) shr 1, valor); // x10
    $200000:
      m68000_0.irq[2] := CLEAR_LINE;
    $300000:
      ;
    $400000 .. $41FFFF:
      begin // proteccion
        addr := direccion and $1FFFF;
        thunderl_protection_reg := (bit_n(addr, 2) shl 0) or
          ((bit_n(addr, 2) and bit_n(not(addr), 3)) shl 1) or
          ((bit_n(addr, 2) or bit_n(not(addr), 6)) shl 2) or
          ((bit_n(addr, 2) or bit_n(not(addr), 6) or bit_n(not(addr), 8)) shl 3) or
          ((bit_n(addr, 3) and bit_n(not(addr), 11) and bit_n(addr, 15)) shl 4) or
          ((bit_n(addr, 6) and bit_n(addr, 13)) shl 5) or
          (((bit_n(addr, 6) and bit_n(addr, 13)) or bit_n(not(addr), 16)) shl 6) or
          ((((bit_n(addr, 6) and bit_n(addr, 13)) or bit_n(not(addr), 16)) and
          (bit_n(addr, 2) or bit_n(not(addr), 6) or bit_n(not(addr), 8))) shl 7);
      end;
    $500000 .. $500001:
      ; // coin lockout
    $700000 .. $7003FF:
      if buffer_paleta[(direccion and $3FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $3FF) shr 1] := valor;
        change_color((direccion and $3FF) shr 1, valor);
      end;
    $C00000:
      ram[$4000] := valor;
    $D00000 .. $D005FF:
      seta_sprite0.spritey[(direccion and $7FF) shr 1] := valor and $FF;
    $D00600 .. $D00607:
      seta_sprite0.control[(direccion and $7) shr 1] := valor and $FF;
    $E00000 .. $E03FFF:
      begin
        seta_sprite0.spritelow[(direccion and $3FFF) shr 1] := valor and $FF;
        seta_sprite0.spritehigh[(direccion and $3FFF) shr 1] := valor shr 8;
      end;
    $E04000 .. $E07FFF:
      ram[$2000 + ((direccion and $3FFF) shr 1)] := valor;
    $FFC000 .. $FFFFFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

// Main
procedure reset_seta;
begin
  m68000_0.reset;
  case main_vars.machine_type of
    302:
      begin
        ym2203_0.reset;
        ym3812_0.reset;
        m6502_0.reset;
      end;
    303:
      begin
        x1_010_0.reset;
        m6502_0.reset;
      end;
    304:
      x1_010_0.reset;
  end;
  reset_audio;
  seta_sprite0.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_latch0 := 0;
  sound_latch1 := 0;
  scroll_x := 0;
  scroll_y := 0;
  vram_control := 0;
  thunderl_protection_reg := 0;
end;

function start_seta: boolean;
var
  memory_temp: array [0 .. $3FFFF] of byte;
  ptemp: pbyte;
  f: byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 2 * 8, 8 * 2 * 8 + 1, 8 * 2 * 8 + 2,
    8 * 2 * 8 + 3, 8 * 2 * 8 + 4, 8 * 2 * 8 + 5, 8 * 2 * 8 + 6, 8 * 2 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8 * 2, 1 * 8 * 2, 2 * 8 * 2, 3 * 8 * 2, 4 * 8 * 2,
    5 * 8 * 2, 6 * 8 * 2, 7 * 8 * 2, 8 * 2 * 8 * 2 + (8 * 2 * 0), 8 * 2 * 8 * 2 + (8 * 2 * 1),
    8 * 2 * 8 * 2 + (8 * 2 * 2), 8 * 2 * 8 * 2 + (8 * 2 * 3), 8 * 2 * 8 * 2 + (8 * 2 * 4),
    8 * 2 * 8 * 2 + (8 * 2 * 5), 8 * 2 * 8 * 2 + (8 * 2 * 6), 8 * 2 * 8 * 2 + (8 * 2 * 7));
  ps_x_te: array [0 .. 15] of dword = (4 * 4 * 8 * 3 + 0, 4 * 4 * 8 * 3 + 1, 4 * 4 * 8 * 3 + 2,
    4 * 4 * 8 * 3 + 3, 4 * 4 * 8 * 2 + 0, 4 * 4 * 8 * 2 + 1, 4 * 4 * 8 * 2 + 2, 4 * 4 * 8 * 2 + 3,
    4 * 4 * 8 + 0, 4 * 4 * 8 + 1, 4 * 4 * 8 + 2, 4 * 4 * 8 + 3, 0, 1, 2, 3);
  ps_y_te: array [0 .. 15] of dword = (0 * 4 * 4, 1 * 4 * 4, 2 * 4 * 4, 3 * 4 * 4, 4 * 4 * 4,
    5 * 4 * 4, 6 * 4 * 4, 7 * 4 * 4, 4 * 4 * 8 * 4 + (4 * 4 * 0), 4 * 4 * 8 * 4 + (4 * 4 * 1),
    4 * 4 * 8 * 4 + (4 * 4 * 2), 4 * 4 * 8 * 4 + (4 * 4 * 3), 4 * 4 * 8 * 4 + (4 * 4 * 4),
    4 * 4 * 8 * 4 + (4 * 4 * 5), 4 * 4 * 8 * 4 + (4 * 4 * 6), 4 * 4 * 8 * 4 + (4 * 4 * 7));

  procedure convert_sprites(num: word);
  begin
    init_gfx(0, 16, 16, num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 16 * 2, num * 16 * 16 * 2 + 8, num * 16 * 16 * 2, 8, 0);
    convert_gfx(0, 0, ptemp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.reset := reset_seta;
  case main_vars.machine_type of
    302:
      machine_calls.general_loop := seta_loop_snd_cpu;
    303:
      begin
        machine_calls.general_loop := seta_loop_snd_cpu;
        machine_calls.fps_max := 57.42;
      end;
    304:
      machine_calls.general_loop := seta_loop;
  end;
  start_seta := false;
  start_audio(false);
  // Pantallas
  screen_init(1, 512, 256, false, true);
  case main_vars.machine_type of
    303:
      begin
        screen_init(2, 1024, 512, false, false);
        screen_mod_scroll(2, 1024, 512, 1023, 512, 256, 511);
      end;
  end;
  main_screen.rot270_screen := true;
  start_video(384, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(16000000 div 2, 256);
  getmem(ptemp, $200000);
  case main_vars.machine_type of
    302:
      begin // Thundercade
        scanlines_proc := tndrcade_scan_lines;
        // Main CPU
        m68000_0.change_ram16_calls(tndrcade_getword, tndrcade_putword);
        if not(roms_load16w(@rom, tndrcade_rom)) then
          exit;
        // Sound CPU
        m6502_0 := cpu_m6502.create(16000000 div 8, 256, TCPU_M65C02);
        m6502_0.change_ram_calls(tndrcade_snd_getbyte, tndrcade_snd_putbyte);
        m6502_0.init_sound(tndrcade_sound_update);
        if not(roms_load(@memory_temp, tndrcade_snd)) then
          exit;
        copymemory(@mem_snd[$6000], @memory_temp[$2000], $2000);
        copymemory(@mem_snd[$C000], @memory_temp[$0], $4000);
        for f := 0 to $E do
          copymemory(@rom_snd[f, 0], @memory_temp[f * $4000], $4000);
        // Sound
        ym2203_0 := ym2203_chip.create(16000000 div 4);
        ym2203_0.change_io_calls(tndrcade_porta_read, tndrcade_portb_read, nil, nil);
        ym3812_0 := ym3812_chip.create(YM3812_FM, 16000000 div 4);
        // Video chips (sin bancos de sprites)
        seta_sprite0:=tseta_sprites.create(0,1,$1000 div $40);
        // convertir gfx
        if not(roms_load(ptemp, tndrcade_sprites)) then
          exit;
        convert_sprites($4000);
        // DIP
        marcade.dswa := $F77F;
        marcade.dswa_val := @tndrcade_dip;
      end;
    303:
      begin // Twin Eagle - Revenge Joe's Brother
        scanlines_proc := twineagl_scan_lines;
        // Main CPU
        m68000_0.change_ram16_calls(twineagl_getword, twineagl_putword);
        if not(roms_load_swap_word(@rom, twineagl_rom)) then
          exit;
        // Sound CPU
        m6502_0 := cpu_m6502.create(16000000 div 8, 256, TCPU_M65C02);
        m6502_0.change_ram_calls(twineagl_snd_getbyte, twineagl_snd_putbyte);
        m6502_0.init_sound(seta_sound_update);
        if not(roms_load(@memory_temp, twineagl_snd)) then
          exit;
        copymemory(@mem_snd[$7000], @memory_temp[$1000], $1000);
        copymemory(@mem_snd[$8000], @memory_temp[$0], $2000);
        copymemory(@mem_snd[$A000], @memory_temp[$0], $2000);
        copymemory(@mem_snd[$C000], @memory_temp[$0], $2000);
        copymemory(@mem_snd[$E000], @memory_temp[$0], $2000);
        // Sound
        x1_010_0 := tx1_010.create(16000000);
        if not(roms_load(@x1_010_0.rom, twineagl_pcm)) then
          exit;
        // Video chips (Sin bancos de sprites)
        seta_sprite0:=tseta_sprites.create(0,1,$1000 div $40);
        // convertir gfx
        if not(roms_load16w(pword(ptemp), twineagl_sprites)) then
          exit;
        convert_sprites($2000);
        if not(roms_load16w(pword(ptemp), twineagl_tiles)) then
          exit;
        init_gfx(1, 16, 16, $4000);
        gfx_set_desc_data(4, 0, 16 * 16 * 4, 0, 4, 8, 12);
        convert_gfx(1, 0, ptemp, @ps_x_te, @ps_y_te, false, false);
        // DIP
        marcade.dswa := $BFF7;
        marcade.dswa_val := @twineagl_dip;
      end;
    304:
      begin // Thunder & Lightning
        scanlines_proc := thunderl_scan_lines;
        eventos_proc := thunderl_events;
        // Main CPU
        m68000_0.change_ram16_calls(thunderl_getword, thunderl_putword);
        m68000_0.init_sound(seta_sound_update);
        if not(roms_load16w(@rom, thunderl_rom)) then
          exit;
        // Sound
        x1_010_0 := tx1_010.create(16000000);
        if not(roms_load(@x1_010_0.rom, thunderl_pcm)) then
          exit;
        // Video chips (sin bancos de sprites)
        seta_sprite0:=tseta_sprites.create(0,1,$1000 div $40);
        // convertir gfx
        if not(roms_load16w(pword(ptemp), thunderl_sprites)) then
          exit;
        convert_sprites($1000);
        // DIP
        marcade.dswa := $E0;
        marcade.dswa_val := @thunderl_dip_a;
        marcade.dswb := $E9FF;
        marcade.dswb_val := @thunderl_dip_b;
      end;
  end;
  freemem(ptemp);
  // final
  reset_seta;
  start_seta := true;
end;

end.
