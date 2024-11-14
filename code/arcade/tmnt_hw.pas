unit tmnt_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  upd7759,
  ym_2151,
  k052109,
  k051960,
  misc_functions,
  samples,
  k053244_k053245,
  k053260,
  k053251,
  eepromser,
  k007232;

function start_teenagemutantnijaturtles: boolean;

implementation

const
  // TMNT
  tmnt_rom: array [0 .. 3] of tipo_roms = ((n: '963-u23.j17'; l: $20000; p: 0; crc: $58BEC748),
    (n: '963-u24.k17'; l: $20000; p: $1; crc: $DCE87C8D), (n: '963-u21.j15'; l: $10000; p: $40000;
    crc: $ABCE5EAD), (n: '963-u22.k15'; l: $10000; p: $40001; crc: $4ECC8D6B));
  tmnt_sound: tipo_roms = (n: '963e20.g13'; l: $8000; p: 0; crc: $1692A6D6);
  tmnt_char: array [0 .. 1] of tipo_roms = ((n: '963a28.h27'; l: $80000; p: 0; crc: $DB4769A8),
    (n: '963a29.k27'; l: $80000; p: $2; crc: $8069CD2E));
  tmnt_sprites: array [0 .. 3] of tipo_roms = ((n: '963a17.h4'; l: $80000; p: 0; crc: $B5239A44),
    (n: '963a15.k4'; l: $80000; p: $2; crc: $1F324EED), (n: '963a18.h6'; l: $80000; p: $100000;
    crc: $DD51ADEF), (n: '963a16.k6'; l: $80000; p: $100002; crc: $D4BD9984));
  tmnt_prom: array [0 .. 1] of tipo_roms = ((n: '963a30.g7'; l: $100; p: 0; crc: $ABD82680), (n: '963a31.g19';
    l: $100; p: $100; crc: $F8004A1C));
  tmnt_upd: tipo_roms = (n: '963a27.d18'; l: $20000; p: 0; crc: $2DFD674B);
  tmnt_title: tipo_roms = (n: '963a25.d5'; l: $80000; p: 0; crc: $FCA078C7);
  tmnt_k007232: tipo_roms = (n: '963a26.c13'; l: $20000; p: 0; crc: $E2AC3063);
  // Sunset Riders
  ssriders_rom: array [0 .. 3] of tipo_roms = ((n: '064ebd02.8e'; l: $40000; p: 0; crc: $8DEEF9AC),
    (n: '064ebd03.8g'; l: $40000; p: $1; crc: $2370C107), (n: '064eab04.10e'; l: $20000; p: $80000;
    crc: $EF2315BD), (n: '064eab05.10g'; l: $20000; p: $80001; crc: $51D6FBC4));
  ssriders_sound: tipo_roms = (n: '064e01.2f'; l: $10000; p: 0; crc: $44B9BC52);
  ssriders_char: array [0 .. 1] of tipo_roms = ((n: '064e12.16k'; l: $80000; p: 0; crc: $E2BDC619),
    (n: '064e11.12k'; l: $80000; p: $2; crc: $2D8CA8B0));
  ssriders_sprites: array [0 .. 1] of tipo_roms = ((n: '064e09.7l'; l: $100000; p: 0; crc: $4160C372),
    (n: '064e07.3l'; l: $100000; p: $2; crc: $64DD673C));
  ssriders_eeprom: tipo_roms = (n: 'ssriders_ebd.nv'; l: $80; p: 0; crc: $CBC903F6);
  ssriders_k053260: tipo_roms = (n: '064e06.1d'; l: $100000; p: 0; crc: $59810DF9);
  // DIP
  tmnt_dip_a: array [0 .. 1] of def_dip = ((mask: $F; name: 'Coinage'; number: 16;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $2; dip_name: '4C 1C'), (dip_val: $5;
    dip_name: '3C 1C'), (dip_val: $8; dip_name: '2C 1C'), (dip_val: $4; dip_name: '2C 3C'), (dip_val: $1;
    dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D;
    dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B; dip_name: '1C 5C'), (dip_val: $A;
    dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'))), ());
  tmnt_dip_b: array [0 .. 3] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '1'), (dip_val: $2; dip_name: '2'), (dip_val: $1;
    dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $60; name: 'Difficulty'; number: 4; dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40;
    dip_name: 'Normal'), (dip_val: $20; dip_name: 'Difficult'), (dip_val: $0; dip_name: 'Very Difficult'), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  tmnt_dip_c: array [0 .. 1] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());

var
  rom: array [0 .. $5FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  ram2: array [0 .. $3F] of word;
  sprite_ram: array [0 .. $1FFF] of word;
  char_rom, sprite_rom, k007232_rom, k053260_rom: pbyte;
  sound_latch, sound_latch2, sprite_colorbase, last_snd, toggle: byte;
  layer_colorbase, layerpri: array [0 .. 2] of byte;
  irq5_mask, sprites_pri: boolean;

procedure tmnt_cb(layer, bank: word; var code: dword; var color: word; var flags: word; var priority: word);
begin
  code := code or ((color and $03) shl 8) or ((color and $10) shl 6) or ((color and $0C) shl 9) or
    (bank shl 13);
  color := layer_colorbase[layer] + ((color and $E0) shr 5);
end;

procedure tmnt_sprite_cb(var code: word; var color: word; var pri: word; var shadow: word);
begin
  code := code or ((color and $10) shl 9);
  color := sprite_colorbase + (color and $0F);
end;

procedure tmnt_k007232_cb(valor: byte);
begin
  k007232_0.set_volume(0, (valor shr 4) * $11, 0);
  k007232_0.set_volume(1, 0, (valor and $0F) * $11);
end;

procedure update_video_tmnt;
begin
  k052109_0.draw_tiles;
  k051960_0.update_sprites;
  fill_full_screen(4, 0);
  k052109_0.draw_layer(2, 4);
  if sprites_pri then
  begin
    k051960_0.draw_sprites(0, 0);
    k052109_0.draw_layer(1, 4);
  end
  else
  begin
    k052109_0.draw_layer(1, 4);
    k051960_0.draw_sprites(0, 0);
  end;
  k052109_0.draw_layer(0, 4);
  update_final_piece(96, 16, 320, 224, 4);
end;

procedure events_tmnt;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
  end;
end;

procedure tmnt_loop;
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
        if f = 239 then
        begin
          update_video_tmnt;
          if irq5_mask then
            m68000_0.irq[5] := HOLD_LINE;
        end;
      end;
      events_tmnt;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function tmnt_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $05FFFF:
      tmnt_getword := rom[direccion shr 1];
    $060000 .. $063FFF:
      tmnt_getword := ram[(direccion and $3FFF) shr 1];
    $080000 .. $080FFF:
      tmnt_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $0A0000:
      tmnt_getword := marcade.in0; // coin
    $0A0002:
      tmnt_getword := marcade.in1; // p1
    $0A0004:
      tmnt_getword := marcade.in2; // p2
    $0A0006:
      tmnt_getword := $FF; // p3
    $0A0010:
      tmnt_getword := marcade.dswa;
    $0A0012:
      tmnt_getword := marcade.dswb;
    $0A0014:
      tmnt_getword := $FF; // p4
    $0A0018:
      tmnt_getword := marcade.dswc;
    $100000 .. $107FFF:
      begin
        direccion := direccion shr 1;
        if m68000_0.write_8bits_hi_dir then
          tmnt_getword := k052109_0.read_msb(((direccion and $3000) shr 1) or (direccion and $07FF))
        else
          tmnt_getword := k052109_0.read_lsb(((direccion and $3000) shr 1) or (direccion and $07FF)) shl 8;
      end;
    $140000 .. $140007:
      tmnt_getword := k051960_0.k051937_read(direccion and 7);
    $140400 .. $1407FF:
      if m68000_0.write_8bits_hi_dir then
        tmnt_getword := k051960_0.read((direccion and $3FF) + 1)
      else
        tmnt_getword := k051960_0.read(direccion and $3FF) shl 8;
  end;
end;

procedure tmnt_putword(direccion: dword; valor: word);

  procedure change_color_tmnt(pos: word);
  var
    color: tcolor;
    data: word;
  begin
    data := (buffer_paleta[pos and $7FE] shl 8) or (buffer_paleta[(pos and $7FE) + 1] shl 0);
    color.b := pal5bit(data shr 10);
    color.g := pal5bit(data shr 5);
    color.r := pal5bit(data);
    set_pal_color_alpha(color, pos shr 1);
    k052109_0.clean_video_buffer;
  end;

begin
  case direccion of
    0 .. $5FFFF:
      ;
    $60000 .. $63FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $80000 .. $80FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> (valor and $FF) then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor and $FF;
        change_color_tmnt((direccion and $FFF) shr 1);
      end;
    $A0000:
      begin
        if ((last_snd = 8) and ((valor and 8) = 0)) then
          z80_0.change_irq(HOLD_LINE);
        last_snd := valor and 8;
        // bit 5 = irq enable
        irq5_mask := (valor and $20) <> 0;
        // bit 7 = enable char ROM reading through the video RAM */
        if (valor and $80) <> 0 then
          k052109_0.rmrd_line := ASSERT_LINE
        else
          k052109_0.rmrd_line := CLEAR_LINE;
      end;
    $A0008:
      sound_latch := valor and $FF;
    $C0000:
      sprites_pri := ((valor and $0C) shr 2) <> 0; // prioridad
    $100000 .. $107FFF:
      begin
        direccion := direccion shr 1;
        if m68000_0.write_8bits_hi_dir then
          k052109_0.write_msb(((direccion and $3000) shr 1) or (direccion and $07FF), valor)
        else
          k052109_0.write_lsb(((direccion and $3000) shr 1) or (direccion and $07FF), valor shr 8)
      end;
    $140000 .. $140007:
      k051960_0.k051937_write((direccion and $7), valor);
    $140400 .. $1407FF:
      if m68000_0.write_8bits_hi_dir then
        k051960_0.write((direccion and $3FF) + 1, valor and $FF)
      else
        k051960_0.write(direccion and $3FF, valor shr 8)
  end;
end;

function tmnt_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      tmnt_snd_getbyte := mem_snd[direccion];
    $9000:
      tmnt_snd_getbyte := sound_latch2;
    $A000:
      tmnt_snd_getbyte := sound_latch;
    $B000 .. $B00D:
      tmnt_snd_getbyte := k007232_0.read(direccion and $F);
    $C001:
      tmnt_snd_getbyte := ym2151_0.status;
    $F000:
      tmnt_snd_getbyte := upd7759_0.busy_r;
  end;
end;

procedure tmnt_snd_putbyte(direccion: word; valor: byte);
begin
  if direccion < $8000 then
    exit;
  case direccion of
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $9000:
      begin
        upd7759_0.reset_w(valor and 2);
        // bit 2 plays the title music
        if (((valor and 4) <> 0) and not(sample_status(0))) then
          start_sample(0)
        else
          stop_sample(0);
        sound_latch2 := valor;
      end;
    $B000 .. $B00D:
      k007232_0.write(direccion and $F, valor);
    $C000:
      ym2151_0.reg(valor);
    $C001:
      ym2151_0.write(valor);
    $D000:
      upd7759_0.port_w(valor);
    $E000:
      upd7759_0.start_w(valor and 1);
  end;
end;

procedure tmnt_sound_update;
begin
  ym2151_0.update;
  upd7759_0.update;
  k007232_0.update;
  samples_update;
end;

// Sunset riders
procedure ssriders_sprite_cb(var code: word; var color: word; var priority: word);
var
  pri: word;
begin
  pri := $20 or ((color and $60) shr 2);
  if (pri <= layerpri[2]) then
    priority := 0
  else if ((pri > layerpri[2]) and (pri <= layerpri[1])) then
    priority := 1
  else if ((pri > layerpri[1]) and (pri <= layerpri[0])) then
    priority := 2
  else
    priority := 3;
  color := sprite_colorbase + (color and $1F);
end;

procedure update_video_ssriders;
var
  bg_colorbase: byte;
  sorted_layer: array [0 .. 2] of byte;
begin
  // Ordenar
  sorted_layer[0] := 0;
  layerpri[0] := k053251_0.get_priority(K053251_CI2);
  sorted_layer[1] := 1;
  layerpri[1] := k053251_0.get_priority(K053251_CI4);
  sorted_layer[2] := 2;
  layerpri[2] := k053251_0.get_priority(K053251_CI3);
  konami_sortlayers3(@sorted_layer, @layerpri);
  bg_colorbase := k053251_0.get_palette_index(K053251_CI0);
  sprite_colorbase := k053251_0.get_palette_index(K053251_CI1);
  layer_colorbase[0] := k053251_0.get_palette_index(K053251_CI2);
  if k053251_0.dirty_tmap[K053251_CI2] then
  begin
    k052109_0.clean_video_buffer_layer(0);
    k053251_0.dirty_tmap[K053251_CI2] := false;
  end;
  layer_colorbase[1] := k053251_0.get_palette_index(K053251_CI4);
  if k053251_0.dirty_tmap[K053251_CI4] then
  begin
    k052109_0.clean_video_buffer_layer(1);
    k053251_0.dirty_tmap[K053251_CI4] := false;
  end;
  layer_colorbase[2] := k053251_0.get_palette_index(K053251_CI3);
  if k053251_0.dirty_tmap[K053251_CI3] then
  begin
    k052109_0.clean_video_buffer_layer(2);
    k053251_0.dirty_tmap[K053251_CI3] := false;
  end;
  k052109_0.draw_tiles;
  k05324x_update_sprites;
  fill_full_screen(4, bg_colorbase * 16);
  k05324x_sprites_draw(3);
  k052109_0.draw_layer(sorted_layer[0], 4);
  k05324x_sprites_draw(2);
  k052109_0.draw_layer(sorted_layer[1], 4);
  k05324x_sprites_draw(1);
  k052109_0.draw_layer(sorted_layer[2], 4);
  k05324x_sprites_draw(0);
  update_final_piece(112, 16, 288, 224, 4);
end;

procedure ssriders_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
      // main
      m68000_0.run(frame_m);
      frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
      // sound
      z80_0.run(frame_s);
      frame_s := frame_s + z80_0.tframes - z80_0.contador;
      case f of
        21:
          update_video_ssriders;
        239:
          if k052109_0.is_irq_enabled then
            m68000_0.irq[4] := HOLD_LINE;
      end;
    end;
    events_tmnt;
    video_sync;
  end;
end;

function ssriders_protection_r: word;
var
  data: integer;
  cmd: word;
begin
  data := ram[$1A0A shr 1];
  cmd := ram[$18FC shr 1];
  case cmd of
    $100B:
      begin
        // read twice in a row, first result discarded?
        // data is always == 0x75c
        ssriders_protection_r := $0064;
      end;
    $6003:
      ssriders_protection_r := data and $000F; // start of level
    $6004:
      ssriders_protection_r := data and $001F;
    $6000:
      ssriders_protection_r := data and $0001;
    $0000:
      ssriders_protection_r := data and $00FF;
    $6007:
      ssriders_protection_r := data and $00FF;
    $8ABC:
      begin
        // collision table
        data := -ram[$1818 shr 1];
        data := (((data div 8) - 4) and $1F) * $40;
        // 0x1040c8 is the x scroll buffer, avoids stutter on slopes + scrolling (and it's actually more logical as HW pov)
        data := data + ((((ram[$1CB0 shr 1] + ram[$00C8 shr 1]) - 6) div 8 + 12) and $3F);
        ssriders_protection_r := data;
      end;
  else
    ssriders_protection_r := $FFFF;
  end;
end;

function ssriders_getword(direccion: dword): word;
var
  res: byte;
begin
  case direccion of
    0 .. $0BFFFF:
      ssriders_getword := rom[direccion shr 1];
    $104000 .. $107FFF:
      ssriders_getword := ram[(direccion and $3FFF) shr 1];
    $140000 .. $140FFF:
      ssriders_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $180000 .. $183FFF:
      begin // k053245
        direccion := (direccion and $3FFF) shr 1;
        if (direccion and $0031) <> 0 then
          ssriders_getword := sprite_ram[direccion]
        else
        begin
          direccion := ((direccion and $000E) shr 1) or ((direccion and $1FC0) shr 3);
          ssriders_getword := k053245_word_r(direccion);
        end;
      end;
    $1C0000:
      ssriders_getword := marcade.in1; // p1
    $1C0002:
      ssriders_getword := marcade.in2; // p2
    $1C0004:
      ssriders_getword := $FF; // p3
    $1C0006:
      ssriders_getword := $FF; // p4
    $1C0100:
      ssriders_getword := marcade.in0; // coin
    $1C0102:
      begin
        res := (byte(not(main_vars.service1)) shl 7) + $78 + eepromser_0.do_read +
          (eepromser_0.ready_read shl 1);
        // falta vblank en bit 8
        toggle := toggle xor $04;
        ssriders_getword := res xor toggle;
      end;
    $1C0400:
      ssriders_getword := 0; // watchdog
    $1C0500 .. $1C057F:
      ssriders_getword := ram2[(direccion and $7F) shr 1];
    $1C0800:
      ssriders_getword := ssriders_protection_r; // proteccion
    $5A0000 .. $5A001F:
      begin // k053244
        direccion := ((direccion and $1F) shr 1) and $FE;
        /// * handle mirror address
        ssriders_getword := k053244_read(direccion + 1) + (k053244_read(direccion) shl 8);
      end;
    $5C0600 .. $5C0603:
      ssriders_getword := k053260_0.main_read((direccion and 3) shr 1); // k053260
    $600000 .. $603FFF:
      if m68000_0.read_8bits_hi_dir then
        ssriders_getword := k052109_0.read_msb((direccion and $3FFF) shr 1)
      else
        ssriders_getword := k052109_0.read_lsb((direccion and $3FFF) shr 1) shl 8;
  end;
end;

procedure ssriders_protection_w(direccion: word);
var
  logical_pri, hardware_pri: word;
  i, f: byte;
begin
  if (direccion = 1) then
  begin
    // create sprite priority attributes
    hardware_pri := 1;
    logical_pri := 1;
    for f := 1 to 8 do
    begin // ; logical_pri < 0x100; logical_pri <<= 1)
      for i := 0 to 127 do
      begin
        if (sprite_ram[(6 + 128 * i) shr 1] shr 8) = logical_pri then
        begin
          k053245_lsb_w(8 * i, hardware_pri);
          hardware_pri := hardware_pri + 1;
        end;
      end;
      logical_pri := logical_pri shl 1;
    end;
  end;
end;

procedure ssriders_putword(direccion: dword; valor: word);

  procedure change_color_ssriders(pos, valor: word);
  var
    color: tcolor;
  begin
    color.b := pal5bit(valor shr 10);
    color.g := pal5bit(valor shr 5);
    color.r := pal5bit(valor);
    set_pal_color_alpha(color, pos);
    k052109_0.clean_video_buffer;
  end;

begin
  case direccion of
    0 .. $BFFFF:
      ;
    $104000 .. $107FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $140000 .. $140FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color_ssriders((direccion and $FFF) shr 1, valor);
      end;
    $180000 .. $183FFF:
      begin // k053245
        direccion := (direccion and $3FFF) shr 1;
        sprite_ram[direccion] := valor;
        if (direccion and $0031) = 0 then
        begin
          direccion := ((direccion and $000E) shr 1) or ((direccion and $1FC0) shr 3);
          k053245_word_w(direccion, valor);
        end;
      end;
    $1C0200:
      begin // eeprom
        eepromser_0.di_write(valor and 1);
        eepromser_0.cs_write((valor shr 1) and 1);
        eepromser_0.clk_write((valor shr 2) and 1);
        k053245_bankselect(((valor and $20) shr 5) shl 2);
      end;
    $1C0300:
      begin
        // bit 3 = enable char ROM reading through the video RAM */
        if (valor and 8) <> 0 then
          k052109_0.rmrd_line := ASSERT_LINE
        else
          k052109_0.rmrd_line := CLEAR_LINE;
        // bits 4-6 control palette dimming (DIM0-DIM2) */
        // m_dim_v = (data & 0x70) >> 4;
      end;
    $1C0500 .. $1C057F:
      ram2[(direccion and $7F) shr 1] := valor;
    $1C0800 .. $1C0803:
      ssriders_protection_w((direccion and $3) shr 1); // proteccion
    $5A0000 .. $5A001F:
      begin // k053244
        direccion := ((direccion and $1F) shr 1) and $FE; // handle mirror address
        if m68000_0.write_8bits_hi_dir then
          k053244_write(direccion + 1, valor and $FF)
        else
          k053244_write(direccion, valor shr 8);
      end;
    $5C0600 .. $5C0603:
      k053260_0.main_write((direccion and 3) shr 1, valor); // k053260
    $5C0604:
      z80_0.change_irq(HOLD_LINE); // sound
    $5C0700 .. $5C071F:
      k053251_0.lsb_w((direccion and $1F) shr 1, valor); // k053251
    $600000 .. $603FFF:
      if m68000_0.write_8bits_hi_dir then
        k052109_0.write_msb((direccion and $3FFF) shr 1, valor)
      else
        k052109_0.write_lsb((direccion and $3FFF) shr 1, valor shr 8);
  end;
end;

function ssriders_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $F7FF:
      ssriders_snd_getbyte := mem_snd[direccion];
    $F801:
      ssriders_snd_getbyte := ym2151_0.status;
    $FA00 .. $FA2F:
      ssriders_snd_getbyte := k053260_0.read(direccion and $3F); // k053260
  end;
end;

procedure ssriders_snd_putbyte(direccion: word; valor: byte);
begin
  if direccion < $F000 then
    exit;
  case direccion of
    $F000 .. $F7FF:
      mem_snd[direccion] := valor;
    $F800:
      ym2151_0.reg(valor);
    $F801:
      ym2151_0.write(valor);
    $FA00 .. $FA2F:
      k053260_0.write(direccion and $3F, valor); // k053260
    $FC00:
      z80_0.change_nmi(HOLD_LINE);
  end;
end;

procedure ssriders_sound_update;
begin
  ym2151_0.update;
  k053260_0.update;
end;

// Main
procedure reset_tmnt;
begin
  m68000_0.reset;
  z80_0.reset;
  k052109_0.reset;
  ym2151_0.reset;
  case main_vars.machine_type of
    214:
      begin
        k051960_0.reset;
        upd7759_0.reset;
        upd7759_0.start_w(0);
        upd7759_0.reset_w(1);
        reset_samples;
      end;
    215:
      begin
        k053245_reset;
        k053251_0.reset;
        k053260_0.reset;
        eepromser_0.reset;
      end;
  end;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_latch := 0;
  sound_latch2 := 0;
  irq5_mask := false;
  last_snd := 0;
  sprites_pri := false;
  toggle := 0;
end;

procedure close_tmnt;
begin
  case main_vars.machine_type of
    214:
      if k007232_rom <> nil then
        freemem(k007232_rom);
    215:
      begin
        // k053245_0.free;
        if k053260_rom <> nil then
          freemem(k053260_rom);
        // eeprom free
      end;
  end;
  if char_rom <> nil then
    freemem(char_rom);
  if sprite_rom <> nil then
    freemem(sprite_rom);
  char_rom := nil;
  sprite_rom := nil;
  k053260_rom := nil;
  k007232_rom := nil;
end;

function start_teenagemutantnijaturtles: boolean;
var
  f, tempdw: dword;
  memory_temp: array [0 .. $1FF] of byte;
  ptemp: pbyte;
  ptempw: pword;
  procedure desencriptar_sprites;
  var
    len, a, b: dword;
    entry, i: byte;
    bits: array [0 .. 31] of byte;
    temp: pbyte;
  const
    CA0 = 0;
    CA1 = 1;
    CA2 = 2;
    CA3 = 3;
    CA4 = 4;
    CA5 = 5;
    CA6 = 6;
    CA7 = 7;
    CA8 = 8;
    CA9 = 9;
    // following table derived from the schematics. It indicates, for each of the */
    // 9 low bits of the sprite line address, which bit to pick it from. */
    // For example, when the PROM contains 4, which applies to 4x2 sprites, */
    // bit OA1 comes from CA5, OA2 from CA0, and so on. */
    bit_pick_table: array [0 .. 9, 0 .. 7] of byte = (
      // 0(1x1) 1(2x1) 2(1x2) 3(2x2) 4(4x2) 5(2x4) 6(4x4) 7(8x8) */
      (CA3, CA3, CA3, CA3, CA3, CA3, CA3, CA3), // CA3 */
      (CA0, CA0, CA5, CA5, CA5, CA5, CA5, CA5), // OA1 */
      (CA1, CA1, CA0, CA0, CA0, CA7, CA7, CA7), // OA2 */
      (CA2, CA2, CA1, CA1, CA1, CA0, CA0, CA9), // OA3 */
      (CA4, CA4, CA2, CA2, CA2, CA1, CA1, CA0), // OA4 */
      (CA5, CA6, CA4, CA4, CA4, CA2, CA2, CA1), // OA5 */
      (CA6, CA5, CA6, CA6, CA6, CA4, CA4, CA2), // OA6 */
      (CA7, CA7, CA7, CA7, CA8, CA6, CA6, CA4), // OA7 */
      (CA8, CA8, CA8, CA8, CA7, CA8, CA8, CA6), // OA8 */
      (CA9, CA9, CA9, CA9, CA9, CA9, CA9, CA8)); // OA9 */
  begin
    // unscramble the sprite ROM address lines
    len := $200000 div 4;
    getmem(temp, $200000);
    copymemory(temp, sprite_rom, $200000);
    for a := 0 to (len - 1) do
    begin
      // pick the correct entry in the PROM (top 8 bits of the address) */
      entry := memory_temp[(a and $7F800) shr 11] and 7;
      // the bits to scramble are the low 10 ones */
      for i := 0 to 9 do
        bits[i] := (a shr i) and $1;
      b := a and $7FC00;
      for i := 0 to 9 do
        b := b or (bits[bit_pick_table[i][entry]] shl i);
      sprite_rom[a * 4] := temp[b * 4];
      sprite_rom[(a * 4) + 1] := temp[(b * 4) + 1];
      sprite_rom[(a * 4) + 2] := temp[(b * 4) + 2];
      sprite_rom[(a * 4) + 3] := temp[(b * 4) + 3];
    end;
    freemem(temp);
  end;

  function decode_sample(orig: pbyte; dest: pword): dword;
  var
    i, pos: dword;
    val: word;
    expo, cont1, cont2: byte;
    ptemp: pword;
  begin
    // Sound sample for TMNT.D05 is stored in the following mode (ym3012 format):
    // Bit 15-13:  Exponent (2 ^ x)
    // Bit 12-3 :  Sound data (10 bit)
    // (Sound info courtesy of Dave <dave@finalburn.com>)
    // El original viene a 20Khz, lo convierto a 44Khz
    pos := 0;
    cont2 := 0;
    for i := 0 to $3FFFF do
    begin
      val := orig[2 * i] + orig[2 * i + 1] * 256;
      expo := val shr 13;
      val := (val shr 3) and $3FF; // 10 bit, Max Amplitude 0x400 */
      val := val - $200; // Centralize value */
      val := val shl (expo - 3);
      for cont1 := 0 to 1 do
      begin
        ptemp := dest;
        inc(ptemp, pos);
        ptemp^ := val;
        pos := pos + 1;
      end;
      cont2 := cont2 + 1;
      if cont2 = 5 then
      begin
        cont2 := 0;
        ptemp := dest;
        inc(ptemp, pos);
        ptemp^ := val;
        pos := pos + 1;
      end;
    end;
    decode_sample := pos;
  end;

begin
  start_teenagemutantnijaturtles := false;
  machine_calls.close := close_tmnt;
  machine_calls.reset := reset_tmnt;
  // Pantallas para el K052109
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 1024, 1024, false, true);
  case main_vars.machine_type of
    214:
      begin // TMNT
        machine_calls.general_loop := tmnt_loop;
        start_video(320, 224, true);
        start_audio(false); // Sonido mono
        // Main CPU
        m68000_0 := cpu_m68000.create(8000000, 256);
        m68000_0.change_ram16_calls(tmnt_getword, tmnt_putword);
        // Sound CPU
        z80_0 := cpu_z80.create(3579545, 256);
        z80_0.change_ram_calls(tmnt_snd_getbyte, tmnt_snd_putbyte);
        z80_0.init_sound(tmnt_sound_update);
        // cargar roms
        if not(roms_load16w(@rom, tmnt_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, tmnt_sound)) then
          exit;
        // Sound Chips
        ym2151_0 := ym2151_chip.create(3579545);
        upd7759_0 := upd7759_chip.create(0.6);
        getmem(k007232_rom, $20000);
        if not(roms_load(k007232_rom, tmnt_k007232)) then
          exit;
        k007232_0 := k007232_chip.create(3579545, k007232_rom, $20000, 0.20, tmnt_k007232_cb);
        if not(roms_load(upd7759_0.get_rom_addr, tmnt_upd)) then
          exit;
        getmem(ptemp, $80000);
        getmem(ptempw, $80000 * 3);
        if not(roms_load(ptemp, tmnt_title)) then
          exit;
        load_samples_raw(ptempw, decode_sample(ptemp, ptempw), false, false);
        freemem(ptemp);
        freemem(ptempw);
        // Iniciar video
        getmem(char_rom, $100000);
        if not(roms_load32b(char_rom, tmnt_char)) then
          exit;
        // Ordenar
        for f := 0 to $3FFFF do
        begin
          tempdw := char_rom[(f * 4) + 0];
          tempdw := tempdw or (char_rom[(f * 4) + 1] shl 8);
          tempdw := tempdw or (char_rom[(f * 4) + 2] shl 16);
          tempdw := tempdw or (char_rom[(f * 4) + 3] shl 24);
          tempdw := BITSWAP32(tempdw, 31, 27, 23, 19, 15, 11, 7, 3, 30, 26, 22, 18, 14, 10, 6, 2, 29, 25, 21,
            17, 13, 9, 5, 1, 28, 24, 20, 16, 12, 8, 4, 0);
          char_rom[(f * 4) + 0] := tempdw and $FF;
          char_rom[(f * 4) + 1] := (tempdw shr 8) and $FF;
          char_rom[(f * 4) + 2] := (tempdw shr 16) and $FF;
          char_rom[(f * 4) + 3] := (tempdw shr 24) and $FF;
        end;
        k052109_0 := k052109_chip.create(1, 2, 3, 0, tmnt_cb, char_rom, $100000);
        // Init sprites
        getmem(sprite_rom, $200000);
        if not(roms_load32b(sprite_rom, tmnt_sprites)) then
          exit;
        if not(roms_load(@memory_temp, tmnt_prom)) then
          exit;
        // Ordenar
        for f := 0 to $7FFFF do
        begin
          tempdw := sprite_rom[(f * 4) + 0];
          tempdw := tempdw or (sprite_rom[(f * 4) + 1] shl 8);
          tempdw := tempdw or (sprite_rom[(f * 4) + 2] shl 16);
          tempdw := tempdw or (sprite_rom[(f * 4) + 3] shl 24);
          tempdw := BITSWAP32(tempdw, 31, 27, 23, 19, 15, 11, 7, 3, 30, 26, 22, 18, 14, 10, 6, 2, 29, 25, 21,
            17, 13, 9, 5, 1, 28, 24, 20, 16, 12, 8, 4, 0);
          sprite_rom[(f * 4) + 0] := tempdw and $FF;
          sprite_rom[(f * 4) + 1] := (tempdw shr 8) and $FF;
          sprite_rom[(f * 4) + 2] := (tempdw shr 16) and $FF;
          sprite_rom[(f * 4) + 3] := (tempdw shr 24) and $FF;
        end;
        desencriptar_sprites;
        k051960_0 := k051960_chip.create(4, 1, sprite_rom, $200000, tmnt_sprite_cb);
        layer_colorbase[0] := 0;
        layer_colorbase[1] := 32;
        layer_colorbase[2] := 40;
        sprite_colorbase := 16;
        // DIP
        marcade.dswa := $FF;
        marcade.dswa_val := @tmnt_dip_a;
        marcade.dswb := $5E;
        marcade.dswb_val := @tmnt_dip_b;
        marcade.dswc := $FF;
        marcade.dswc_val := @tmnt_dip_c;
      end;
    215:
      begin // Sunset Riders
        machine_calls.general_loop := ssriders_loop;
        start_video(288, 224, true);
        start_audio(true); // Sonido stereo
        // Main CPU
        m68000_0 := cpu_m68000.create(16000000, 256);
        m68000_0.change_ram16_calls(ssriders_getword, ssriders_putword);
        // Sound CPU
        z80_0 := cpu_z80.create(8000000, 256);
        z80_0.change_ram_calls(ssriders_snd_getbyte, ssriders_snd_putbyte);
        z80_0.init_sound(ssriders_sound_update);
        // cargar roms
        if not(roms_load16w(@rom, ssriders_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, ssriders_sound)) then
          exit;
        // Sound Chips
        ym2151_0 := ym2151_chip.create(3579545);
        getmem(k053260_rom, $100000);
        if not(roms_load(k053260_rom, ssriders_k053260)) then
          exit;
        k053260_0 := tk053260_chip.create(3579545, k053260_rom, $100000, 0.70);
        // Iniciar video
        getmem(char_rom, $100000);
        if not(roms_load32b(char_rom, ssriders_char)) then
          exit;
        k052109_0 := k052109_chip.create(1, 2, 3, 0, tmnt_cb, char_rom, $100000);
        // Init sprites
        getmem(sprite_rom, $200000);
        if not(roms_load32b(sprite_rom, ssriders_sprites)) then
          exit;
        k053245_init(sprite_rom, $200000, ssriders_sprite_cb);
        // Prioridades
        k053251_0 := k053251_chip.create;
        // eeprom
        eepromser_0 := eepromser_chip.create(ER5911, 8);
        if not(eepromser_0.load_data('ssriders.nv')) then
        begin
          if not(roms_load(@memory_temp, ssriders_eeprom)) then
            exit;
          copymemory(eepromser_0.get_data, @memory_temp, $80);
        end;
      end;
  end;
  // final
  reset_tmnt;
  start_teenagemutantnijaturtles := true;
end;

end.
