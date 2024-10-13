unit galivan_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  dac,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  ym_3812;

function start_galivan: boolean;

implementation

const
  galivan_rom: array [0 .. 2] of tipo_roms = ((n: '1.1b'; l: $8000; p: 0; crc: $1E66B3F8), (n: '2.3b'; l: $4000; p: $8000; crc: $A45964F1), (n: 'gv3.4b'; l: $4000; p: $C000; crc: $82F0C5E6));
  galivan_sound: array [0 .. 1] of tipo_roms = ((n: 'gv11.14b'; l: $4000; p: 0; crc: $05F1A0E3), (n: 'gv12.15b'; l: $8000; p: $4000; crc: $5B7A0D6D));
  galivan_char: tipo_roms = (n: 'gv4.13d'; l: $4000; p: 0; crc: $162490B4);
  galivan_fondo: array [0 .. 3] of tipo_roms = ((n: 'gv7.14f'; l: $8000; p: 0; crc: $EAA1A0DB), (n: 'gv8.15f'; l: $8000; p: $8000; crc: $F174A41E), (n: 'gv9.17f'; l: $8000; p: $10000; crc: $EDC60F5D),
    (n: 'gv10.19f'; l: $8000; p: $18000; crc: $41F27FCA));
  galivan_sprites: array [0 .. 1] of tipo_roms = ((n: 'gv14.4f'; l: $8000; p: 0; crc: $03E2229F), (n: 'gv13.1f'; l: $8000; p: $8000; crc: $BCA9E66B));
  galivan_bg_tiles: array [0 .. 1] of tipo_roms = ((n: 'gv6.19d'; l: $4000; p: 0; crc: $DA38168B), (n: 'gv5.17d'; l: $4000; p: $4000; crc: $22492D2A));
  galivan_pal: array [0 .. 4] of tipo_roms = ((n: 'mb7114e.9f'; l: $100; p: 0; crc: $DE782B3E), (n: 'mb7114e.10f'; l: $100; p: $100; crc: $0AE2A857), (n: 'mb7114e.11f'; l: $100; p: $200;
    crc: $7BA8B9D1), (n: 'mb7114e.2d'; l: $100; p: $300; crc: $75466109), (n: 'mb7114e.7f'; l: $100; p: $400; crc: $06538736));
  galivan_dip_a: array [0 .. 6] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '4'), (dip_val: $1; dip_name: '5'), (dip_val: $0;
    dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $C; dip_name: '20K 60K+'), (dip_val: $8; dip_name: '50K 60K+'), (dip_val: $4; dip_name: '20K 90K+'), (dip_val: $0; dip_name: '50K 90K+'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $10; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40;
    name: 'Power Invulnerability'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Life Invulnerability'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  galivan_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coin A'; number: 4; dip: ((dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 3C'), (dip_val: $C; dip_name: '1C 3C'), (dip_val: $8; dip_name: '1C 6C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Difficulty'; number: 2; dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  dangar_rom: array [0 .. 2] of tipo_roms = ((n: '8.1b'; l: $8000; p: 0; crc: $FE4A3FD6), (n: '9.3b'; l: $4000; p: $8000; crc: $809D280F), (n: '10.4b'; l: $4000; p: $C000; crc: $99A3591B));
  dangar_sound: array [0 .. 1] of tipo_roms = ((n: '13.b14'; l: $4000; p: 0; crc: $3E041873), (n: '14.b15'; l: $8000; p: $4000; crc: $488E3463));
  dangar_char: tipo_roms = (n: '5.13d'; l: $4000; p: 0; crc: $40CB378A);
  dangar_fondo: array [0 .. 3] of tipo_roms = ((n: '1.14f'; l: $8000; p: 0; crc: $D59ED1F1), (n: '2.15f'; l: $8000; p: $8000; crc: $DFDB931C), (n: '3.17f'; l: $8000; p: $10000; crc: $6954E8C3),
    (n: '4.19f'; l: $8000; p: $18000; crc: $4AF6A8BF));
  dangar_sprites: array [0 .. 1] of tipo_roms = ((n: '12.f4'; l: $8000; p: 0; crc: $55711884), (n: '11.f1'; l: $8000; p: $8000; crc: $8CF11419));
  dangar_bg_tiles: array [0 .. 1] of tipo_roms = ((n: '7.19d'; l: $4000; p: 0; crc: $6DBA32CF), (n: '6.17d'; l: $4000; p: $4000; crc: $6C899071));
  dangar_pal: array [0 .. 4] of tipo_roms = ((n: '82s129.9f'; l: $100; p: 0; crc: $B29F6A07), (n: '82s129.10f'; l: $100; p: $100; crc: $C6DE5ECB), (n: '82s129.11f'; l: $100; p: $200; crc: $A5BBD6DC),
    (n: '82s129.2d'; l: $100; p: $300; crc: $A4AC95A5), (n: '82s129.7f'; l: $100; p: $400; crc: $29BC6216));
  dangar_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '4'), (dip_val: $1; dip_name: '5'), (dip_val: $0;
    dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $C; dip_name: '20K 60K+'), (dip_val: $8; dip_name: '50K 60K+'), (dip_val: $4; dip_name: '20K 90K+'), (dip_val: $0; dip_name: '50K 90K+'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $10; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Alternate Enemies'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  dangar_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coin A'; number: 4; dip: ((dip_val: $1; dip_name: '2C 1C'), (dip_val: $3; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Coin B'; number: 4;
    dip: ((dip_val: $4; dip_name: '2C 1C'), (dip_val: $C; dip_name: '1C 1C'), (dip_val: $0; dip_name: '2C 3C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $10; name: 'Difficulty'; number: 2; dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Allow Continue';
    number: 4; dip: ((dip_val: $C0; dip_name: 'No'), (dip_val: $80; dip_name: '3 Times'), (dip_val: $40; dip_name: '5 Times'), (dip_val: $0; dip_name: '99 Times'), (), (), (), (), (), (), (), (), (),
    (), (), ())), ());

var
  scroll_x, scroll_y: word;
  rom_mem: array [0 .. 1, 0 .. $1FFF] of byte;
  spritebank: array [0 .. $FF] of byte;
  layers, rom_bank, sound_latch: byte;

procedure update_video_galivan;
var
  f, color, x, y, nchar: word;
  atrib: byte;
  procedure draw_sprites;
  var
    atrib, color, f: byte;
    x, y, nchar: word;
  begin
    for f := 0 to $3F do
    begin
      atrib := buffer_sprites[(f * 4) + 2];
      nchar := buffer_sprites[(f * 4) + 1] + ((atrib and $6) shl 7);
      y := 240 - (buffer_sprites[(f * 4) + 3] - $80 + ((atrib and 1) shl 8));
      x := 240 - (buffer_sprites[f * 4] and $FF);
      color := ((atrib and $3C) shr 2) + 16 * (spritebank[nchar shr 2] and $F);
      put_gfx_sprite(nchar and $1FF, color shl 4, (atrib and $80) <> 0, (atrib and $40) <> 0, 2);
      update_gfx_sprite(x, y, 4, 2);
    end;
  end;

begin
  // background
  if (layers and $40) <> 0 then
    fill_full_screen(4, $100)
  else
    scroll_x_y(1, 4, scroll_x, (1792 - scroll_y) and $7FF);
  // Text
  if (layers and $80) = 0 then
  begin
    for f := $0 to $3FF do
    begin
      if gfx[0].buffer[f] then
      begin
        x := f mod 32;
        y := 31 - (f div 32);
        atrib := memory[$DC00 + f];
        nchar := memory[$D800 + f] or ((atrib and 1) shl 8);
        color := (atrib and $78) shl 1;
        put_gfx_trans(x * 8, y * 8, nchar, color, 2, 0);
        if (atrib and 8) <> 0 then
          put_gfx_block_trans(x * 8, y * 8, 3, 8, 8)
        else
          put_gfx_trans(x * 8, y * 8, nchar, color, 3, 0);
        gfx[0].buffer[f] := false;
      end;
    end;
  end;
  if (layers and $20) <> 0 then
  begin
    if (layers and $80) = 0 then
      actualiza_trozo(0, 0, 256, 256, 2, 0, 0, 256, 256, 4);
    draw_sprites;
    if (layers and $80) = 0 then
      actualiza_trozo(0, 0, 256, 256, 3, 0, 0, 256, 256, 4);
  end
  else
  begin
    if (layers and $80) = 0 then
      actualiza_trozo(0, 0, 256, 256, 3, 0, 0, 256, 256, 4);
    draw_sprites;
    if (layers and $80) = 0 then
      actualiza_trozo(0, 0, 256, 256, 2, 0, 0, 256, 256, 4);
  end;
  update_final_piece(16, 0, 224, 256, 4);
end;

procedure events_galivan;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[0] then
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
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.right[1] then
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
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
  end;
end;

procedure galivan_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
      if f = 240 then
      begin
        update_video_galivan;
        copymemory(@buffer_sprites, @memory[$E000], $100);
        z80_0.change_irq(ASSERT_LINE);
      end;
      // main
      z80_0.run(frame_m);
      frame_m := frame_m + z80_0.tframes - z80_0.contador;
      // sound
      z80_1.run(frame_s);
      frame_s := frame_s + z80_1.tframes - z80_1.contador;
    end;
    events_galivan;
    video_sync;
  end;
end;

function galivan_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF, $E000 .. $FFFF:
      galivan_getbyte := memory[direccion];
    $C000 .. $DFFF:
      galivan_getbyte := rom_mem[rom_bank, direccion and $1FFF];
  end;
end;

procedure galivan_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $D7FF:
      ;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $E000 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function galivan_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      galivan_inbyte := marcade.in1; // p1
    1:
      galivan_inbyte := marcade.in2; // p2
    2:
      galivan_inbyte := marcade.in0; // system
    3:
      galivan_inbyte := marcade.dswa; // dsw1
    4:
      galivan_inbyte := marcade.dswb; // dsw2
    $C0:
      galivan_inbyte := $58;
  end;
end;

procedure galivan_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $40:
      begin
        rom_bank := (valor and $80) shr 7;
        main_screen.flip_main_screen := (valor and $4) <> 0;
      end;
    $41:
      scroll_y := (scroll_y and $700) or valor;
    $42:
      begin
        scroll_y := (scroll_y and $FF) or ((valor and $7) shl 8);
        layers := valor and $E0;
      end;
    $43:
      scroll_x := (scroll_x and $700) or valor;
    $44:
      scroll_x := (scroll_x and $FF) or ((valor and $7) shl 8);
    $45:
      sound_latch := ((valor and $7F) shl 1) or 1;
    $47:
      z80_0.change_irq(CLEAR_LINE);
  end;
end;

function galivan_snd_getbyte(direccion: word): byte;
begin
  galivan_snd_getbyte := mem_snd[direccion];
end;

procedure galivan_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
  end;
end;

procedure galivan_snd_timer;
begin
  z80_1.change_irq(HOLD_LINE);
end;

function galivan_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    4:
      sound_latch := 0;
    6:
      galivan_snd_inbyte := sound_latch;
  end;
end;

procedure galivan_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      ym3812_0.control(valor);
    $1:
      ym3812_0.write(valor);
    $2:
      dac_0.signed_data8_w(valor);
    $3:
      dac_1.signed_data8_w(valor);
  end;
end;

procedure galivan_sound_update;
begin
  ym3812_0.update;
  dac_0.update;
  dac_1.update;
end;

// Main
procedure reset_galivan;
begin
  z80_0.reset;
  z80_1.reset;
  ym3812_0.reset;
  dac_0.reset;
  dac_1.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  sound_latch := 0;
  layers := 0;
  rom_bank := 0;
end;

function start_galivan: boolean;
var
  colores: tpaleta;
  f: word;
  tempb: byte;
  memory_temp: array [0 .. $1FFFF] of byte;
  bg_temp: array [0 .. $7FFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (1 * 4, 0 * 4, 3 * 4, 2 * 4, 5 * 4, 4 * 4, 7 * 4, 6 * 4);
  ps_x: array [0 .. 15] of dword = (4, 0, 4 + $8000 * 8, 0 + $8000 * 8, 12, 8, 12 + $8000 * 8, 8 + $8000 * 8, 20, 16, 20 + $8000 * 8, 16 + $8000 * 8, 28, 24, 28 + $8000 * 8, 24 + $8000 * 8);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  pf_x: array [0 .. 15] of dword = (4, 0, 12, 8, 20, 16, 28, 24, 32 + 4, 32 + 0, 32 + 12, 32 + 8, 32 + 20, 32 + 16, 32 + 28, 32 + 24);
  pf_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $200);
    gfx[0].trans[15] := true;
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, true);
  end;
  procedure convert_fg;
  begin
    init_gfx(1, 16, 16, $400);
    gfx_set_desc_data(4, 0, 64 * 16, 0, 1, 2, 3);
    convert_gfx(1, 0, @memory_temp, @pf_x, @pf_y, false, true);
  end;
  procedure convert_sprites;
  begin
    init_gfx(2, 16, 16, $200);
    gfx[2].trans[15] := true;
    gfx_set_desc_data(4, 0, 32 * 16, 0, 1, 2, 3);
    convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  end;
  procedure put_bg;
  var
    f, nchar: word;
    x, y, atrib, color: byte;
  begin
    for f := $0 to $3FFF do
    begin
      x := f div 128;
      y := 127 - (f mod 128);
      atrib := bg_temp[f + $4000];
      nchar := bg_temp[f] or ((atrib and $3) shl 8);
      color := (atrib and $78) shl 1;
      put_gfx(x * 16, y * 16, nchar, color, 1, 1);
    end;
  end;

begin
  machine_calls.general_loop := galivan_loop;
  machine_calls.reset := reset_galivan;
  start_galivan := false;
  start_audio(false);
  screen_init(1, 2048, 2048);
  screen_mod_scroll(1, 2048, 2048, 2047, 2048, 2048, 2047);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, true);
  screen_init(4, 256, 512, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(6000000, 256);
  z80_0.change_io_calls(galivan_inbyte, galivan_outbyte);
  z80_0.change_ram_calls(galivan_getbyte, galivan_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(4000000, 256);
  z80_1.change_ram_calls(galivan_snd_getbyte, galivan_snd_putbyte);
  z80_1.change_io_calls(galivan_snd_inbyte, galivan_snd_outbyte);
  z80_1.init_sound(galivan_sound_update);
  case main_vars.machine_type of
    266:
      begin // Galivan
        // cargar roms
        if not(roms_load(@memory_temp, galivan_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $C000);
        copymemory(@rom_mem[0, 0], @memory_temp[$C000], $2000);
        copymemory(@rom_mem[1, 0], @memory_temp[$E000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, galivan_sound)) then
          exit;
        // Sound Chips
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 0.3);
        // convertir chars
        if not(roms_load(@memory_temp, galivan_char)) then
          exit;
        convert_chars;
        // convertir fondo
        if not(roms_load(@memory_temp, galivan_fondo)) then
          exit;
        convert_fg;
        // convertir sprites
        if not(roms_load(@memory_temp, galivan_sprites)) then
          exit;
        convert_sprites;
        // tiles de bg y lo pongo en la pantalla 1
        if not(roms_load(@bg_temp, galivan_bg_tiles)) then
          exit;
        // DIP
        marcade.dswa := $DF;
        marcade.dswa_val := @galivan_dip_a;
        marcade.dswb := $FF;
        marcade.dswb_val := @galivan_dip_b;
        // poner la paleta
        if not(roms_load(@memory_temp, galivan_pal)) then
          exit;
        copymemory(@spritebank, @memory_temp[$400], $100);
      end;
    267:
      begin // Dangar
        // cargar roms
        if not(roms_load(@memory_temp, dangar_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $C000);
        copymemory(@rom_mem[0, 0], @memory_temp[$C000], $2000);
        copymemory(@rom_mem[1, 0], @memory_temp[$E000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, dangar_sound)) then
          exit;
        // Sound Chips
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 0.3);
        // convertir chars
        if not(roms_load(@memory_temp, dangar_char)) then
          exit;
        convert_chars;
        // convertir fondo
        if not(roms_load(@memory_temp, dangar_fondo)) then
          exit;
        convert_fg;
        // convertir sprites
        if not(roms_load(@memory_temp, dangar_sprites)) then
          exit;
        convert_sprites;
        // tiles de bg y lo pongo en la pantalla 1
        if not(roms_load(@bg_temp, dangar_bg_tiles)) then
          exit;
        // DIP
        marcade.dswa := $DF;
        marcade.dswa_val := @dangar_dip_a;
        marcade.dswb := $7F;
        marcade.dswb_val := @dangar_dip_b;
        // poner la paleta
        if not(roms_load(@memory_temp, dangar_pal)) then
          exit;
        copymemory(@spritebank, @memory_temp[$400], $100);
      end;
  end;
  dac_0 := dac_chip.create(0.5);
  dac_1 := dac_chip.create(0.5);
  timers.init(z80_1.numero_cpu, 4000000 / (4000000 / 512), galivan_snd_timer, nil, true);
  for f := 0 to $FF do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
    // lookup de chars
    if (f and 8) <> 0 then
      gfx[0].colores[f] := (f and $0F) or ((f shr 2) and $30)
    else
      gfx[0].colores[f] := (f and $0F) or ((f shr 0) and $30);
    // color lookup de fondo
    if (f and 8) <> 0 then
      gfx[1].colores[f] := $C0 or (f and $0F) or ((f shr 2) and $30)
    else
      gfx[1].colores[f] := $C0 or (f and $0F) or ((f shr 0) and $30);
  end;
  // color lookup de sprites
  for f := 0 to $FFF do
  begin
    if (f and $8) <> 0 then
      tempb := $80 or ((f shl 2) and $30) or (memory_temp[$300 + (f shr 4)] and $F)
    else
      tempb := $80 or ((f shl 4) and $30) or (memory_temp[$300 + (f shr 4)] and $F);
    gfx[2].colores[((f and $F) shl 8) or ((f and $FF0) shr 4)] := tempb;
  end;
  set_pal(colores, $100);
  // Despues de poner la paleta, pongo el fondo...
  put_bg;
  // final
  reset_galivan;
  start_galivan := true;
end;

end.
