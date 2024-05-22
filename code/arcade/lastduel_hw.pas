unit lastduel_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  oki6295;

function start_lastduel: boolean;

implementation

const
  lastduel_rom: array [0 .. 3] of tipo_roms = ((n: 'ldu_06b.13k'; l: $20000; p: 0; crc: $0E71ACAF),
    (n: 'ldu_05b.12k'; l: $20000; p: $1; crc: $47A85BEA), (n: 'ldu_04b.11k'; l: $10000; p: $40000;
    crc: $AA4BF001), (n: 'ldu_03b.9k'; l: $10000; p: $40001; crc: $BBAAC8AB));
  lastduel_sound: tipo_roms = (n: 'ld_02.16h'; l: $10000; p: 0; crc: $91834D0C);
  lastduel_char: tipo_roms = (n: 'ld_01.12f'; l: $8000; p: 0; crc: $AD3C6F87);
  lastduel_sprites: array [0 .. 3] of tipo_roms = ((n: 'ld-09.12a'; l: $20000; p: 0;
    crc: $6EFADB74), (n: 'ld-10.17a'; l: $20000; p: $1; crc: $B8D3B2E3), (n: 'ld-11.12b'; l: $20000;
    p: $2; crc: $49D4DBBD), (n: 'ld-12.17b'; l: $20000; p: $3; crc: $313E5338));
  lastduel_tiles: array [0 .. 1] of tipo_roms = ((n: 'ld-15.6p'; l: $20000; p: 0; crc: $D977A175),
    (n: 'ld-13.6m'; l: $20000; p: $1; crc: $BC25729F));
  lastduel_tiles2: tipo_roms = (n: 'ld-14.15n'; l: $80000; p: 0; crc: $D0653739);
  lastduel_dip: array [0 .. 10] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '4'), (dip_val: $1;
    dip_name: '5'), (dip_val: $0; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $C; dip_name: '20K 60K+'), (dip_val: $8; dip_name: '30K 70K+'), (dip_val: $4;
    dip_name: '40K 80K+'), (dip_val: $0; dip_name: '50K 90K+'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $300; name: 'Coin A'; number: 4;
    dip: ((dip_val: $100; dip_name: '2C 1C'), (dip_val: $300; dip_name: '1C 1C'), (dip_val: $200;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $C00; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $400; dip_name: '2C 3C'), (dip_val: $C00;
    dip_name: '1C 3C'), (dip_val: $800; dip_name: '1C 6C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $1000; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $1000; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $2000; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $2000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $4000; name: 'Complete Invulnerability'; number: 2;
    dip: ((dip_val: $4000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $8000; name: 'Base Ship Invulnerability'; number: 2;
    dip: ((dip_val: $8000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  madgear_rom: array [0 .. 3] of tipo_roms = ((n: 'mg_04.8b'; l: $20000; p: 0; crc: $B112257D),
    (n: 'mg_03.7b'; l: $20000; p: $1; crc: $B2672465), (n: 'mg_02.6b'; l: $20000; p: $40000;
    crc: $9F5EBE16), (n: 'mg_01.5b'; l: $20000; p: $40001; crc: $1CEA2AF0));
  madgear_sound: tipo_roms = (n: 'mg_05.14j'; l: $10000; p: 0; crc: $2FBFC945);
  madgear_char: tipo_roms = (n: 'mg_06.10k'; l: $8000; p: 0; crc: $382EE59B);
  madgear_sprites: array [0 .. 7] of tipo_roms = ((n: 'mg_m11.rom0'; l: $10000; p: $2;
    crc: $EE319A64), (n: 'mg_m07.rom2'; l: $10000; p: $40002; crc: $E5C0B211), (n: 'mg_m12.rom1';
    l: $10000; p: $0; crc: $887EF120), (n: 'mg_m08.rom3'; l: $10000; p: $40000; crc: $59709AA3),
    (n: 'mg_m13.rom0'; l: $10000; p: $3; crc: $EAE07DB4), (n: 'mg_m09.rom2'; l: $10000; p: $40003;
    crc: $40EE83EB), (n: 'mg_m14.rom1'; l: $10000; p: $1; crc: $21E5424C), (n: 'mg_m10.rom3';
    l: $10000; p: $40001; crc: $B64AFB54));
  madgear_tiles: tipo_roms = (n: 'ls-12.7l'; l: $40000; p: 0; crc: $6C1B2C6C);
  madgear_tiles2: tipo_roms = (n: 'ls-11.2l'; l: $80000; p: 0; crc: $6BF81C64);
  madgear_oki: array [0 .. 1] of tipo_roms = ((n: 'ls-06.10e'; l: $20000; p: 0; crc: $88D39A5B),
    (n: 'ls-05.12e'; l: $20000; p: $20000; crc: $B06E03B5));
  leds2011_rom: array [0 .. 3] of tipo_roms = ((n: 'lse_04.8b'; l: $20000; p: 0; crc: $166C0576),
    (n: 'lse_03.7b'; l: $20000; p: $1; crc: $0C8647B6), (n: 'ls-02.6b'; l: $20000; p: $40000;
    crc: $05C0285E), (n: 'ls-01.5b'; l: $20000; p: $40001; crc: $8BF934DD));
  leds2011_sound: tipo_roms = (n: 'ls-07.14j'; l: $10000; p: 0; crc: $98AF7838);
  leds2011_char: tipo_roms = (n: 'ls-08.10k'; l: $8000; p: 0; crc: $8803CF49);
  leds2011_sprites: array [0 .. 1] of tipo_roms = ((n: 'ls-10.13a'; l: $40000; p: $0;
    crc: $DB2C5883), (n: 'ls-09.5a'; l: $40000; p: $1; crc: $89949EFB));
  leds2011_tiles: tipo_roms = (n: 'ls-12.7l'; l: $40000; p: 0; crc: $6C1B2C6C);
  leds2011_tiles2: tipo_roms = (n: 'ls-11.2l'; l: $80000; p: 0; crc: $6BF81C64);
  leds2011_oki: array [0 .. 1] of tipo_roms = ((n: 'ls-06.10e'; l: $20000; p: 0; crc: $88D39A5B),
    (n: 'ls-05.12e'; l: $20000; p: $20000; crc: $B06E03B5));

var
  lastduel_hw_update_video, lastduel_event: procedure;
  scroll_x0, scroll_y0, scroll_x1, scroll_y1: word;
  rom: array [0 .. $3FFFF] of word;
  ram: array [0 .. $FFFF] of word;
  ram_txt: array [0 .. $FFF] of word;
  ram_video: array [0 .. $5FFF] of word;
  sprite_ram: array [0 .. $3FF] of word;
  sound_rom: array [0 .. 1, 0 .. $3FFF] of byte;
  sprite_x_mask, sound_bank, video_pri, sound_latch: byte;

procedure draw_sprites(pri: byte);
var
  atrib, color, nchar, x, y, f: word;
  flip_x, flip_y: boolean;
begin
  for f := $FE downto 0 do
  begin
    atrib := buffer_sprites_w[(f * 4) + 1];
    if pri = ((atrib and $10) shr 4) then
      continue;
    nchar := buffer_sprites_w[f * 4] and $FFF;
    y := buffer_sprites_w[(f * 4) + 3] and $1FF;
    x := buffer_sprites_w[(f * 4) + 2] and $1FF;
    flip_x := (atrib and sprite_x_mask) <> 0;
    flip_y := (atrib and $20) <> 0;
    color := (atrib and $F) shl 4;
    put_gfx_sprite(nchar, color + $200, flip_x, flip_y, 3);
    update_gfx_sprite(x, 496 - y, 5, 3);
  end;
end;

procedure update_video_lastduel;
var
  f, color, x, y, nchar, atrib: word;
begin
  for f := $0 to $FFF do
  begin
    // bg
    atrib := ram_video[$2001 + (f * 2)];
    color := atrib and $F;
    if (gfx[1].buffer[f + $1000] or buffer_color[color + $10]) then
    begin
      x := f div 64;
      y := 63 - (f mod 64);
      nchar := (ram_video[$2000 + (f * 2)]) and $7FF;
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 2, 1, (atrib and $40) <> 0,
        (atrib and $20) <> 0);
      gfx[1].buffer[f + $1000] := false;
    end;
    // fg
    atrib := ram_video[$1 + (f * 2)];
    color := atrib and $F;
    if (gfx[1].buffer[f] or buffer_color[color + $20]) then
    begin
      x := f div 64;
      y := 63 - (f mod 64);
      nchar := (ram_video[f * 2]) and $FFF;
      put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + $100, 3, 2, (atrib and $40) <> 0,
        (atrib and $20) <> 0);
      if (atrib and $80) <> 0 then
        put_gfx_trans_flip_alt(x * 16, y * 16, nchar, (color shl 4) + $100, 4, 2,
          (atrib and $40) <> 0, (atrib and $20) <> 0, 0)
      else
        put_gfx_block_trans(x * 16, y * 16, 4, 16, 16);
      gfx[1].buffer[f] := false;
    end;
  end;
  for f := $0 to $7FF do
  begin
    atrib := ram_txt[f];
    color := atrib shr 12;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f div 64;
      y := 63 - (f mod 64);
      nchar := atrib and $7FF;
      put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 2) + $300, 1, 0,
        (atrib and $800) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 5, scroll_x1, scroll_y1);
  scroll_x_y(3, 5, scroll_x0, scroll_y0);
  draw_sprites(2); // Todos los sprites
  scroll_x_y(4, 5, scroll_x0, scroll_y0);
  actualiza_trozo(0, 0, 256, 512, 1, 0, 0, 256, 512, 5);
  actualiza_trozo_final(8, 64, 240, 384, 5);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
  m68000_0.irq[2] := HOLD_LINE;
end;

procedure update_video_madgear;
var
  f, color, x, y, nchar, atrib: word;
begin
  for f := $0 to $7FF do
  begin
    // bg
    atrib := ram_video[$2800 + f];
    color := atrib and $F;
    if (gfx[1].buffer[f + $1000] or buffer_color[color + $10]) then
    begin
      x := f mod 32;
      y := 63 - (f div 32);
      nchar := (ram_video[$2000 + f]) and $7FF;
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 4, 2, 1, (atrib and $40) <> 0,
        (atrib and $20) <> 0);
      gfx[1].buffer[f + $1000] := false;
    end;
    // fg
    atrib := ram_video[f + $800];
    color := atrib and $F;
    if (gfx[1].buffer[f] or buffer_color[color + $20]) then
    begin
      x := f mod 32;
      y := 63 - (f div 32);
      nchar := (ram_video[f]) and $FFF;
      put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + $100, 3, 2, (atrib and $40) <> 0,
        (atrib and $20) <> 0);
      if (atrib and $10) <> 0 then
        put_gfx_trans_flip_alt(x * 16, y * 16, nchar, (color shl 4) + $100, 4, 2,
          (atrib and $40) <> 0, (atrib and $20) <> 0, 0)
      else
        put_gfx_block_trans(x * 16, y * 16, 4, 16, 16);
      gfx[1].buffer[f] := false;
    end;
    atrib := ram_txt[f];
    color := atrib shr 12;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f div 64;
      y := 63 - (f mod 64);
      nchar := atrib and $7FF;
      put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 2) + $300, 1, 0,
        (atrib and $800) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  fill_full_screen(5, $400);
  if video_pri <> 0 then
  begin
    scroll_x_y(3, 5, scroll_x0, scroll_y0);
    draw_sprites(0);
    scroll_x_y(4, 5, scroll_x0, scroll_y0);
    scroll_x_y(2, 5, scroll_x1, scroll_y1);
    draw_sprites(1);
  end
  else
  begin
    scroll_x_y(2, 5, scroll_x1, scroll_y1);
    scroll_x_y(3, 5, scroll_x0, scroll_y0);
    draw_sprites(0);
    scroll_x_y(4, 5, scroll_x0, scroll_y0);
    draw_sprites(1);
  end;
  actualiza_trozo(0, 0, 256, 512, 1, 0, 0, 256, 512, 5);
  actualiza_trozo_final(8, 64, 240, 384, 5);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
  m68000_0.irq[5] := HOLD_LINE;
end;

procedure events_lastduel;
begin
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
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
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure events_madgear;
begin
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FF7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $200);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FBFF)
    else
      marcade.in1 := (marcade.in1 or $400);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $F7FF)
    else
      marcade.in1 := (marcade.in1 or $800);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $7FFF)
    else
      marcade.in1 := (marcade.in1 or $8000);
    // SYSTEM
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
  end;
end;

procedure lastduel_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 248 then
        begin
          // La IRQ de VBLANK esta en la funcion de video!!
          lastduel_hw_update_video;
          copymemory(@buffer_sprites_w, @sprite_ram, $400 * 2);
        end;
      end;
      lastduel_event;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function lastduel_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $5FFFF:
      lastduel_getword := rom[direccion shr 1];
    $FC0800 .. $FC0FFF:
      lastduel_getword := sprite_ram[(direccion and $7FF) shr 1];
    $FC4000:
      lastduel_getword := marcade.in1; // P1_P2
    $FC4002:
      lastduel_getword := marcade.in0; // SYSTEM
    $FC4004:
      lastduel_getword := $FFFF; // DSW1
    $FC4006:
      lastduel_getword := $FF; // DSW2
    $FCC000 .. $FCDFFF:
      lastduel_getword := ram_txt[(direccion and $1FFF) shr 1];
    $FD8000 .. $FD87FF:
      lastduel_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $FD0000 .. $FD7FFF:
      lastduel_getword := ram_video[(direccion and $7FFF) shr 1];
    $FE0000 .. $FFFFFF:
      lastduel_getword := ram[(direccion and $1FFFF) shr 1];
  end;
end;

procedure change_color(dir: word);
var
  col_val: word;
  bright: byte;
  color: tcolor;
begin
  col_val := buffer_paleta[dir];
  bright := $10 + (col_val and $F);
  color.r := ((col_val shr 12) and $F) * bright * $11 div $1F;
  color.g := ((col_val shr 8) and $F) * bright * $11 div $1F;
  color.b := ((col_val shr 4) and $F) * bright * $11 div $1F;
  set_pal_color(color, dir);
  case dir of
    $0 .. $FF:
      buffer_color[(dir shr 4) + $10] := true;
    $100 .. $1FF:
      buffer_color[((dir shr 4) and $F) + $20] := true;
    $300 .. $33F:
      buffer_color[(dir shr 2) and $F] := true;
  end;
end;

procedure lastduel_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $5FFFF:
      ;
    $FC0800 .. $FC0FFF:
      sprite_ram[(direccion and $7FF) shr 1] := valor;
    $FC4000:
      ; // flipscreen
    $FC4002:
      sound_latch := valor and $FF;
    $FC8000:
      scroll_x0 := valor and $3FF;
    $FC8002:
      scroll_y0 := (512 - valor) and $3FF;
    $FC8004:
      scroll_x1 := valor and $3FF;
    $FC8006:
      scroll_y1 := (512 - valor) and $3FF;
    $FCC000 .. $FCDFFF:
      if ram_txt[(direccion and $1FFF) shr 1] <> valor then
      begin
        ram_txt[(direccion and $1FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $FD0000 .. $FD7FFF:
      if ram_video[(direccion and $7FFF) shr 1] <> valor then
      begin
        ram_video[(direccion and $7FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $7FFF) shr 2] := true;
      end;
    $FD8000 .. $FD87FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color((direccion and $7FF) shr 1);
      end;
    $FE0000 .. $FFFFFF:
      ram[(direccion and $1FFFF) shr 1] := valor;
  end;
end;

function lastduel_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $E7FF:
      lastduel_snd_getbyte := mem_snd[direccion];
    $E800:
      lastduel_snd_getbyte := ym2203_0.status;
    $F000:
      lastduel_snd_getbyte := ym2203_1.status;
    $F800:
      lastduel_snd_getbyte := sound_latch;
  end;
end;

procedure lastduel_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ;
    $E000 .. $E7FF:
      mem_snd[direccion] := valor;
    $E800:
      ym2203_0.Control(valor);
    $E801:
      ym2203_0.write(valor);
    $F000:
      ym2203_1.Control(valor);
    $F001:
      ym2203_1.write(valor);
  end;
end;

procedure lastduel_sound_update;
begin
  ym2203_0.update;
  ym2203_1.update;
end;

procedure lastduel_snd_timer;
begin
  m68000_0.irq[4] := HOLD_LINE;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

function madgear_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      madgear_getword := rom[direccion shr 1];
    $FC1800 .. $FC1FFF:
      madgear_getword := sprite_ram[(direccion and $7FF) shr 1];
    $FC4000:
      madgear_getword := $FFFF; // DSW1
    $FC4002:
      madgear_getword := $FF; // DSW2
    $FC4004:
      madgear_getword := marcade.in1; // P1_P2
    $FC4006:
      madgear_getword := marcade.in0; // SYSTEM
    $FC8000 .. $FC9FFF:
      madgear_getword := ram_txt[(direccion and $1FFF) shr 1];
    $FCC000 .. $FCC7FF:
      madgear_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $FD4000 .. $FDFFFF:
      madgear_getword := ram_video[(direccion - $FD4000) shr 1];
    $FF0000 .. $FFFFFF:
      madgear_getword := ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure madgear_putword(direccion: dword; valor: word);
var
  tempw: word;
begin
  case direccion of
    0 .. $7FFFF:
      ;
    $FC1800 .. $FC1FFF:
      sprite_ram[(direccion and $7FF) shr 1] := valor;
    $FC4000:
      ; // flipscreen
    $FC4002:
      sound_latch := valor and $FF;
    $FD0000:
      scroll_x0 := valor and $3FF;
    $FD0002:
      scroll_y0 := (512 - valor) and $3FF;
    $FD0004:
      scroll_x1 := valor and $3FF;
    $FD0006:
      scroll_y1 := (512 - valor) and $3FF;
    $FD000E:
      video_pri := valor;
    $FC8000 .. $FC9FFF:
      if ram_txt[(direccion and $1FFF) shr 1] <> valor then
      begin
        ram_txt[(direccion and $1FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $FCC000 .. $FCC7FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color((direccion and $7FF) shr 1);
      end;
    $FD4000 .. $FDFFFF:
      if ram_video[(direccion - $FD4000) shr 1] <> valor then
      begin
        tempw := (direccion - $FD4000) shr 1;
        ram_video[tempw] := valor;
        case tempw of
          0 .. $7FF:
            gfx[1].buffer[tempw] := true;
          $800 .. $FFF:
            gfx[1].buffer[tempw - $800] := true;
          $2000 .. $27FF:
            gfx[1].buffer[tempw - $1000] := true;
          $2800 .. $2FFF:
            gfx[1].buffer[tempw - $1800] := true;
        end;
      end;
    $FF0000 .. $FFFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

function madgear_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $D000 .. $D7FF:
      madgear_snd_getbyte := mem_snd[direccion];
    $8000 .. $CFFF:
      madgear_snd_getbyte := sound_rom[sound_bank, direccion and $3FFF];
    $F000:
      madgear_snd_getbyte := ym2203_0.status;
    $F002:
      madgear_snd_getbyte := ym2203_1.status;
    $F006:
      madgear_snd_getbyte := sound_latch;
  end;
end;

procedure madgear_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $CFFF:
      ;
    $D000 .. $D7FF:
      mem_snd[direccion] := valor;
    $F000:
      ym2203_0.Control(valor);
    $F001:
      ym2203_0.write(valor);
    $F002:
      ym2203_1.Control(valor);
    $F003:
      ym2203_1.write(valor);
    $F004:
      oki_6295_0.write(valor);
    $F00A:
      sound_bank := valor and 1;
  end;
end;

procedure madgear_sound_update;
begin
  ym2203_0.update;
  ym2203_1.update;
  oki_6295_0.update;
end;

procedure madgear_snd_timer;
begin
  m68000_0.irq[6] := HOLD_LINE;
end;

// Main
procedure reset_lastduel;
begin
  m68000_0.reset;
  z80_0.reset;
  ym2203_0.reset;
  ym2203_1.reset;
  if main_vars.machine_type <> 268 then
    oki_6295_0.reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  scroll_x0 := 0;
  scroll_y0 := 0;
  scroll_x1 := 0;
  scroll_y1 := 0;
  sound_latch := 0;
  video_pri := 0;
  sound_bank := 0;
end;

function start_lastduel: boolean;
var
  memory_temp: array [0 .. $7FFFF] of byte;
  x_size: word;
  f: byte;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4 * 2 + 0, 4 * 2 + 1, 4 * 2 + 2, 4 * 2 + 3);
  pc_y: array [0 .. 7] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4 * 4 + 0, 4 * 4 + 1, 4 * 4 + 2, 4 * 4 + 3,
    (8 * 4 * 16) + 0, (8 * 4 * 16) + 1, (8 * 4 * 16) + 2, (8 * 4 * 16) + 3, 8 * 4 * 16 + 4 * 4 + 0,
    8 * 4 * 16 + 4 * 4 + 1, 8 * 4 * 16 + 4 * 4 + 2, 8 * 4 * 16 + 4 * 4 + 3);
  pt_y: array [0 .. 15] of dword = (0 * 8 * 4, 1 * 8 * 4, 2 * 8 * 4, 3 * 8 * 4, 4 * 8 * 4,
    5 * 8 * 4, 6 * 8 * 4, 7 * 8 * 4, 8 * 8 * 4, 9 * 8 * 4, 10 * 8 * 4, 11 * 8 * 4, 12 * 8 * 4,
    13 * 8 * 4, 14 * 8 * 4, 15 * 8 * 4);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, (8 * 4 * 16) + 0, (8 * 4 * 16) + 1,
    (8 * 4 * 16) + 2, (8 * 4 * 16) + 3, (8 * 4 * 16) + 4, (8 * 4 * 16) + 5, (8 * 4 * 16) + 6,
    (8 * 4 * 16) + 7);
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $800);
    gfx[0].trans[3] := true;
    gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
    convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, true);
  end;
  procedure convert_tiles(g, num: word);
  begin
    init_gfx(g, 16, 16, num);
    gfx_set_desc_data(4, 0, 64 * 16, 3 * 4, 2 * 4, 1 * 4, 0 * 4);
    convert_gfx(g, 0, @memory_temp, @pt_x, @pt_y, false, true);
  end;
  procedure convert_sprites;
  begin
    init_gfx(3, 16, 16, $1000);
    gfx[3].trans[15] := true;
    gfx_set_desc_data(4, 0, 128 * 8, 16, 0, 24, 8);
    convert_gfx(3, 0, @memory_temp, @ps_x, @pt_y, false, true);
  end;

begin
  machine_calls.general_loop := lastduel_loop;
  machine_calls.reset := reset_lastduel;
  start_lastduel := false;
  start_audio(false);
  screen_init(1, 512, 512, true);
  if main_vars.machine_type = 268 then
    x_size := 1024
  else
    x_size := 512;
  screen_init(2, x_size, 1024, true);
  screen_mod_scroll(2, x_size, 512, x_size - 1, 1024, 512, 1023);
  screen_init(3, x_size, 1024, true);
  screen_mod_scroll(3, x_size, 512, x_size - 1, 1024, 512, 1023);
  screen_init(4, x_size, 1024, true);
  screen_mod_scroll(4, x_size, 512, x_size - 1, 1024, 512, 1023);
  screen_init(5, 512, 512, false, true);
  start_video(240, 384);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 256);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  case main_vars.machine_type of
    268:
      begin // Last Duel
        lastduel_hw_update_video := update_video_lastduel;
        lastduel_event := events_lastduel;
        sprite_x_mask := $40;
        // cargar roms
        if not(roms_load16w(@rom, lastduel_rom)) then
          exit;
        m68000_0.change_ram16_calls(lastduel_getword, lastduel_putword);
        timers.init(m68000_0.numero_cpu, 10000000 / 120, lastduel_snd_timer, nil, true);
        // cargar sonido
        if not(roms_load(@mem_snd, lastduel_sound)) then
          exit;
        z80_0.change_ram_calls(lastduel_snd_getbyte, lastduel_snd_putbyte);
        z80_0.init_sound(lastduel_sound_update);
        // convertir chars
        if not(roms_load(@memory_temp, lastduel_char)) then
          exit;
        convert_chars;
        if not(roms_load16w(@memory_temp, lastduel_tiles)) then
          exit;
        convert_tiles(1, $800);
        if not(roms_load(@memory_temp, lastduel_tiles2)) then
          exit;
        convert_tiles(2, $1000);
        gfx[2].trans[0] := true;
        for f := 0 to 6 do
          gfx[2].trans_alt[0, f] := true;
        for f := 12 to 15 do
          gfx[2].trans_alt[0, f] := true;
        if not(roms_load32b_b(@memory_temp, lastduel_sprites)) then
          exit;
        convert_sprites;
      end;
    269:
      begin // Mad Gear
        lastduel_hw_update_video := update_video_madgear;
        lastduel_event := events_madgear;
        sprite_x_mask := $80;
        // cargar roms
        if not(roms_load16w(@rom, madgear_rom)) then
          exit;
        m68000_0.change_ram16_calls(madgear_getword, madgear_putword);
        timers.init(m68000_0.numero_cpu, 10000000 / 120, madgear_snd_timer, nil, true);
        // cargar sonido
        if not(roms_load(@memory_temp, madgear_sound)) then
          exit;
        copymemory(@mem_snd[0], @memory_temp[0], $8000);
        copymemory(@sound_rom[0, 0], @memory_temp[$8000], $4000);
        copymemory(@sound_rom[1, 0], @memory_temp[$C000], $4000);
        z80_0.change_ram_calls(madgear_snd_getbyte, madgear_snd_putbyte);
        z80_0.init_sound(madgear_sound_update);
        // OKI
        oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH, 2);
        if not(roms_load(oki_6295_0.get_rom_addr, madgear_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, madgear_char)) then
          exit;
        convert_chars;
        if not(roms_load(@memory_temp, madgear_tiles)) then
          exit;
        convert_tiles(1, $800);
        gfx[1].trans[15] := true;
        if not(roms_load_swap_word(@memory_temp, madgear_tiles2)) then
          exit;
        convert_tiles(2, $1000);
        gfx[2].trans[15] := true;
        for f := 0 to 7 do
          gfx[2].trans_alt[0, f] := true;
        gfx[2].trans_alt[0, 15] := true;
        if not(roms_load32b_b(@memory_temp, madgear_sprites)) then
          exit;
        convert_sprites;
      end;
    270:
      begin // Led Storm 2011
        lastduel_hw_update_video := update_video_madgear;
        lastduel_event := events_madgear;
        sprite_x_mask := $80;
        // cargar roms
        if not(roms_load16w(@rom, leds2011_rom)) then
          exit;
        m68000_0.change_ram16_calls(madgear_getword, madgear_putword);
        timers.init(m68000_0.numero_cpu, 10000000 / 120, madgear_snd_timer, nil, true);
        // cargar sonido
        if not(roms_load(@memory_temp, leds2011_sound)) then
          exit;
        copymemory(@mem_snd[0], @memory_temp[0], $8000);
        copymemory(@sound_rom[0, 0], @memory_temp[$8000], $4000);
        copymemory(@sound_rom[1, 0], @memory_temp[$C000], $4000);
        z80_0.change_ram_calls(madgear_snd_getbyte, madgear_snd_putbyte);
        z80_0.init_sound(madgear_sound_update);
        // OKI
        oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH, 2);
        if not(roms_load(oki_6295_0.get_rom_addr, leds2011_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, leds2011_char)) then
          exit;
        convert_chars;
        if not(roms_load(@memory_temp, leds2011_tiles)) then
          exit;
        convert_tiles(1, $800);
        gfx[1].trans[15] := true;
        if not(roms_load_swap_word(@memory_temp, leds2011_tiles2)) then
          exit;
        convert_tiles(2, $1000);
        gfx[2].trans[15] := true;
        for f := 0 to 7 do
          gfx[2].trans_alt[0, f] := true;
        gfx[2].trans_alt[0, 15] := true;
        if not(roms_load16w(@memory_temp, leds2011_sprites)) then
          exit;
        convert_sprites;
      end;
  end;
  // Sound Chips
  ym2203_0 := ym2203_chip.create(3579545);
  ym2203_0.change_irq_calls(snd_irq);
  ym2203_1 := ym2203_chip.create(3579545);
  // final
  reset_lastduel;
  start_lastduel := true;
end;

end.
