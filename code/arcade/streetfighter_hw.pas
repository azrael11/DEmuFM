unit streetfighter_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  msm5205;

function start_streetfighter: boolean;

implementation

const
  sfighter_rom: array [0 .. 5] of tipo_roms = ((n: 'sfd-19.2a'; l: $10000; p: 0; crc: $FAAF6255), (n: 'sfd-22.2c'; l: $10000; p: $1; crc: $E1FE3519), (n: 'sfd-20.3a'; l: $10000; p: $20000;
    crc: $44B915BD), (n: 'sfd-23.3c'; l: $10000; p: $20001; crc: $79C43FF8), (n: 'sfd-21.4a'; l: $10000; p: $40000; crc: $E8DB799B), (n: 'sfd-24.4c'; l: $10000; p: $40001; crc: $466A3440));
  sfighter_snd: tipo_roms = (n: 'sf-02.7k'; l: $8000; p: 0; crc: $4A9AC534);
  sfighter_msm: array [0 .. 1] of tipo_roms = ((n: 'sfu-00.1h'; l: $20000; p: $0; crc: $A7CCE903), (n: 'sf-01.1k'; l: $20000; p: $20000; crc: $86E0F0D5));
  sfighter_char: tipo_roms = (n: 'sf-27.4d'; l: $4000; p: 0; crc: $2B09B36D);
  sfighter_bg: array [0 .. 3] of tipo_roms = ((n: 'sf-39.2k'; l: $20000; p: 0; crc: $CEE3D292), (n: 'sf-38.1k'; l: $20000; p: $20000; crc: $2EA99676), (n: 'sf-41.4k'; l: $20000; p: $40000;
    crc: $E0280495), (n: 'sf-40.3k'; l: $20000; p: $60000; crc: $C70B30DE));
  sfighter_fg: array [0 .. 7] of tipo_roms = ((n: 'sf-25.1d'; l: $20000; p: 0; crc: $7F23042E), (n: 'sf-28.1e'; l: $20000; p: $20000; crc: $92F8B91C), (n: 'sf-30.1g'; l: $20000; p: $40000;
    crc: $B1399856), (n: 'sf-34.1h'; l: $20000; p: $60000; crc: $96B6AE2E), (n: 'sf-26.2d'; l: $20000; p: $80000; crc: $54EDE9F5), (n: 'sf-29.2e'; l: $20000; p: $A0000; crc: $F0649A67),
    (n: 'sf-31.2g'; l: $20000; p: $C0000; crc: $8F4DD71A), (n: 'sf-35.2h'; l: $20000; p: $E0000; crc: $70C00FB4));
  sfighter_sprites: array [0 .. 13] of tipo_roms = ((n: 'sf-15.1m'; l: $20000; p: 0; crc: $FC0113DB), (n: 'sf-16.2m'; l: $20000; p: $20000; crc: $82E4A6D3), (n: 'sf-11.1k'; l: $20000; p: $40000;
    crc: $E112DF1B), (n: 'sf-12.2k'; l: $20000; p: $60000; crc: $42D52299), (n: 'sf-07.1h'; l: $20000; p: $80000; crc: $49F340D9), (n: 'sf-08.2h'; l: $20000; p: $A0000; crc: $95ECE9B1),
    (n: 'sf-03.1f'; l: $20000; p: $C0000; crc: $5CA05781), (n: 'sf-17.3m'; l: $20000; p: $E0000; crc: $69FAC48E), (n: 'sf-18.4m'; l: $20000; p: $100000; crc: $71CFD18D), (n: 'sf-13.3k'; l: $20000;
    p: $120000; crc: $FA2EB24B), (n: 'sf-14.4k'; l: $20000; p: $140000; crc: $AD955C95), (n: 'sf-09.3h'; l: $20000; p: $160000; crc: $41B73A31), (n: 'sf-10.4h'; l: $20000; p: $180000; crc: $91C41C50),
    (n: 'sf-05.3f'; l: $20000; p: $1A0000; crc: $538C7CBE));
  sfighter_tile_map1: array [0 .. 1] of tipo_roms = ((n: 'sf-37.4h'; l: $10000; p: 0; crc: $23D09D3D), (n: 'sf-36.3h'; l: $10000; p: $10000; crc: $EA16DF6C));
  sfighter_tile_map2: array [0 .. 1] of tipo_roms = ((n: 'sf-32.3g'; l: $10000; p: $0; crc: $72DF2BD9), (n: 'sf-33.4g'; l: $10000; p: $10000; crc: $3E99D3D5));
  // Dip
  sfighter_dip_a: array [0 .. 7] of def_dip = ((mask: $7; name: 'Coin A'; number: 8; dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $1; dip_name: '3C 1C'), (dip_val: $2;
    dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 3C'), (dip_val: $4; dip_name: '1C 4C'), (dip_val: $3;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $38; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $8; dip_name: '3C 1C'), (dip_val: $10;
    dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28; dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $18;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $100; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $100; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $200; name: 'Attract Music'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $200; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $1000; name: 'Speed'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Slow'), (dip_val: $1000; dip_name: 'Normal'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2000; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $2000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4000; name: 'Freeze'; number: 2;
    dip: ((dip_val: $4000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  sfighter_dip_b: array [0 .. 5] of def_dip = ((mask: $7; name: 'Game Continuation'; number: 6; dip: ((dip_val: $7; dip_name: '5th Stage Maximum'), (dip_val: $6;
    dip_name: '4th Stage Maximum'), (dip_val: $5; dip_name: '3th Stage Maximum'), (dip_val: $4; dip_name: '2th Stage Maximum'), (dip_val: $3; dip_name: '1th Stage Maximum'), (dip_val: $2;
    dip_name: 'None'), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Round Time Count'; number: 4; dip: ((dip_val: $18; dip_name: '100'), (dip_val: $10; dip_name: '150'), (dip_val: $8;
    dip_name: '200'), (dip_val: $0; dip_name: '250'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Normal'), (dip_val: $40; dip_name: 'Easy'), (dip_val: $20; dip_name: 'Difficult'), (dip_val: $0; dip_name: 'Very Difficult'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $380; name: 'Buy-In Feature'; number: 6; dip: ((dip_val: $380; dip_name: '5th Stage Maximum'), (dip_val: $300; dip_name: '4th Stage Maximum'), (dip_val: $280;
    dip_name: '3th Stage Maximum'), (dip_val: $200; dip_name: '2th Stage Maximum'), (dip_val: $180; dip_name: '1th Stage Maximum'), (dip_val: $80; dip_name: 'None'), (), (), (), (), (), (), (), (),
    (), ())), (mask: $400; name: 'Number of Countries Selected'; number: 2; dip: ((dip_val: $400; dip_name: '2'), (dip_val: $0; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());

var
  rom: array [0 .. $2FFFF] of word;
  ram1: array [0 .. $7FF] of word;
  ram3: array [0 .. $3FFF] of word;
  rom_misc: array [0 .. 7, 0 .. $7FFF] of byte;
  scroll_bg, scroll_fg: word;
  bg_paint, fg_paint, bg_act, fg_act, char_act, sp_act: boolean;
  ram_tile_map1, ram_tile_map2: array [0 .. $1FFFF] of byte;
  soundlatch, misc_bank: byte;

procedure update_video_sfighter;
var
  f, x, y, nchar, atrib, color, pos, nchar1, nchar2, nchar3, nchar4: word;
  flipx, flipy: boolean;
  function sf_invert(char: word): word;
  const
    delta: array [0 .. 3] of byte = ($00, $18, $18, $00);
  begin
    sf_invert := char xor delta[(char shr 3) and 3];
  end;

begin
  // bg
  if bg_act then
  begin
    if bg_paint then
    begin
      for f := $0 to $1FF do
      begin
        x := f mod 32;
        y := f div 32;
        pos := (y * 2) + (x * 32) + ((scroll_bg and $FFF0) * 2);
        atrib := ram_tile_map1[$10000 + pos];
        nchar := ((ram_tile_map1[$10001 + pos] shl 8) + ram_tile_map1[$1 + pos]) and $FFF;
        color := ram_tile_map1[pos] shl 4;
        put_gfx_flip(x * 16, y * 16, nchar, color, 3, 1, (atrib and $1) <> 0, (atrib and $2) <> 0);
      end;
      bg_paint := false;
    end;
    update_region((scroll_bg and $F), 0, 512 - (scroll_bg and $F), 256, 3, 0, 0, 512 - (scroll_bg and $F), 256, 1);
  end
  else
  begin
    fill_full_screen(1, 0);
  end;
  // foreground
  if fg_act then
  begin
    if fg_paint then
    begin
      for f := $0 to $1FF do
      begin
        x := f mod 32;
        y := f div 32;
        pos := (y * 2) + (x * 32) + ((scroll_fg and $FFF0) * 2);
        atrib := ram_tile_map2[$10000 + pos];
        nchar := ((ram_tile_map2[$10001 + pos] shl 8) + ram_tile_map2[$1 + pos]) and $1FFF;
        color := ram_tile_map2[pos] shl 4;
        put_gfx_trans_flip(x * 16, y * 16, nchar, color + 256, 4, 2, (atrib and $1) <> 0, (atrib and $2) <> 0);
      end;
      fg_paint := false;
    end;
    update_region((scroll_fg and $F), 0, 512 - (scroll_fg and $F), 256, 4, 0, 0, 512 - (scroll_fg and $F), 256, 1);
  end;
  // Sprites
  if sp_act then
  begin
    for f := $7F downto 0 do
    begin
      nchar := ram3[($6000 + (f * $20)) shr 1] and $3FFF;
      atrib := ram3[($6002 + (f * $20)) shr 1];
      color := ((atrib and $F) shl 4) + 512;
      flipx := (atrib and $100) <> 0;
      flipy := (atrib and $200) <> 0;
      y := ram3[($6004 + (f * $20)) shr 1];
      x := ram3[($6006 + (f * $20)) shr 1];
      if (atrib and $400) <> 0 then
      begin
        nchar1 := nchar;
        nchar2 := nchar + 1;
        nchar3 := nchar + 16;
        nchar4 := nchar + 17;
        if flipx then
        begin
          pos := nchar2;
          nchar2 := nchar1;
          nchar1 := pos;
          pos := nchar4;
          nchar4 := nchar3;
          nchar3 := pos;
        end;
        if flipy then
        begin
          pos := nchar3;
          nchar3 := nchar1;
          nchar1 := pos;
          pos := nchar2;
          nchar2 := nchar4;
          nchar4 := pos;
        end;
        put_gfx_sprite_diff(sf_invert(nchar1), color, flipx, flipy, 3, 0, 0);
        put_gfx_sprite_diff(sf_invert(nchar2), color, flipx, flipy, 3, 16, 0);
        put_gfx_sprite_diff(sf_invert(nchar3), color, flipx, flipy, 3, 0, 16);
        put_gfx_sprite_diff(sf_invert(nchar4), color, flipx, flipy, 3, 16, 16);
        actualiza_gfx_sprite_size(x, y, 1, 32, 32);
      end
      else
      begin
        put_gfx_sprite(sf_invert(nchar), color, flipx, flipy, 3);
        update_gfx_sprite(x, y, 1, 3);
      end;
    end;
  end;
  // chars
  if char_act then
  begin
    for f := $0 to $7FF do
    begin
      if gfx[0].buffer[f] then
      begin
        x := f mod 64;
        y := f div 64;
        atrib := ram1[f];
        nchar := atrib and $3FF;
        color := (atrib shr 12) shl 2;
        flipx := (atrib and $400) <> 0;
        flipy := (atrib and $800) <> 0;
        put_gfx_trans_flip(x * 8, y * 8, nchar, color + 768, 2, 0, flipx, flipy);
        gfx[0].buffer[f] := false;
      end;
    end;
    update_region(64, 16, 384, 224, 2, 64, 16, 384, 224, 1);
  end;
  update_final_piece(64, 16, 384, 224, 1);
end;

procedure events_sfighter;
begin
  if event.arcade then
  begin
    // P1 P2
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
    if p_contrls.map_arcade.but3[0] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but4[0] then
      marcade.in1 := (marcade.in1 and $FF7F)
    else
      marcade.in1 := (marcade.in1 or $80);
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
    if p_contrls.map_arcade.but3[1] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.but4[1] then
      marcade.in1 := (marcade.in1 and $7FFF)
    else
      marcade.in1 := (marcade.in1 or $8000);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.but5[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.but5[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FFFE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $FFFD)
    else
      marcade.in2 := (marcade.in2 or 2);
  end;
end;

procedure sfighter_loop;
var
  frame_m, frame_s, frame_a: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_1.tframes;
  frame_a := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // Sound CPU
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        // ADPCM CPU
        z80_0.run(frame_a);
        frame_a := frame_a + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          update_video_sfighter;
          m68000_0.irq[1] := HOLD_LINE;
        end;
      end;
      events_sfighter;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function sfighter_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $4FFFF:
      sfighter_getword := rom[direccion shr 1];
    $800000 .. $800FFF:
      sfighter_getword := ram1[(direccion and $FFF) shr 1];
    $B00000 .. $B007FF:
      sfighter_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $C00000:
      sfighter_getword := marcade.in0; // IN0
    $C00002:
      sfighter_getword := marcade.in1; // IN1
    $C00004, $C00006, $C0000E:
      sfighter_getword := $FFFF;
    $C00008:
      sfighter_getword := marcade.dswa; // DSW1
    $C0000A:
      sfighter_getword := marcade.dswb; // DSW2
    $C0000C:
      sfighter_getword := marcade.in2; // SYSTEM
    $C0001A:
      sfighter_getword := (byte(char_act) shl 3) + (byte(bg_act) shl 5) + (byte(fg_act) shl 6) + (byte(sp_act) shl 7);
    $FF8000 .. $FFFFFF:
      sfighter_getword := ram3[(direccion and $7FFF) shr 1];
  end;
end;

procedure sfighter_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.r := pal4bit(tmp_color shr 8);
    color.g := pal4bit(tmp_color shr 4);
    color.b := pal4bit(tmp_color);
    set_pal_color(color, numero);
    case numero of
      0 .. $FF:
        bg_paint := true;
      $100 .. $1FF:
        fg_paint := true;
    end;
  end;

begin
  case direccion of
    0 .. $4FFFF:
      ; // ROM
    $800000 .. $800FFF:
      if ram1[(direccion and $FFF) shr 1] <> valor then
      begin
        ram1[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $B00000 .. $B007FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $C00016, $C00010:
      ;
    $C00014:
      if valor <> scroll_fg then
      begin
        if abs((scroll_fg and $FFF0) - (valor and $FFF0)) > $F then
          fg_paint := true;
        scroll_fg := valor;
      end;
    $C00018:
      if valor <> scroll_bg then
      begin
        if abs((scroll_bg and $FFF0) - (valor and $FFF0)) > $F then
          bg_paint := true;
        scroll_bg := valor;
      end;
    $C0001A:
      begin
        main_screen.flip_main_screen := (valor and $4) <> 0;
        if char_act <> ((valor and $8) <> 0) then
        begin
          char_act := (valor and $8) <> 0;
          if char_act then
            fillchar(gfx[0].buffer, $800, 1);
        end;
        bg_act := (valor and $20) <> 0;
        fg_act := (valor and $40) <> 0;
        sp_act := (valor and $80) <> 0;
      end;
    $C0001C:
      begin
        soundlatch := valor and $FF;
        z80_1.change_nmi(PULSE_LINE);
      end;
    $FF8000 .. $FFFFFF:
      ram3[(direccion and $7FFF) shr 1] := valor;
  end;
end;

// sound
function sf_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $C7FF:
      sf_snd_getbyte := mem_snd[direccion];
    $C800:
      sf_snd_getbyte := soundlatch;
    $E001:
      sf_snd_getbyte := ym2151_0.status;
  end;
end;

procedure sf_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $E000:
      ym2151_0.reg(valor);
    $E001:
      ym2151_0.write(valor);
  end;
end;

function sf_misc_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      sf_misc_getbyte := rom_misc[0, direccion];
    $8000 .. $FFFF:
      sf_misc_getbyte := rom_misc[misc_bank, direccion and $7FFF];
  end;
end;

procedure sf_misc_putbyte(direccion: word; valor: byte);
begin
end;

function sf_misc_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 1 then
    sf_misc_inbyte := soundlatch;
end;

procedure sf_misc_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      begin
        msm5205_0.reset_w((valor and $80) <> 0);
        msm5205_0.data_w(valor);
        msm5205_0.vclk_w(true);
        msm5205_0.vclk_w(false);
      end;
    1:
      begin
        msm5205_1.reset_w((valor and $80) <> 0);
        msm5205_1.data_w(valor);
        msm5205_1.vclk_w(true);
        msm5205_1.vclk_w(false);
      end;
    2:
      misc_bank := valor + 1;
  end;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  z80_1.change_irq(irqstate);
end;

procedure sound_instruccion;
begin
  ym2151_0.update;
  msm5205_0.update;
  msm5205_1.update;
end;

procedure sf_adpcm_timer;
begin
  z80_0.change_irq(HOLD_LINE);
end;

// Main
procedure reset_sfighter;
begin
  m68000_0.reset;
  z80_1.reset;
  z80_0.reset;
  ym2151_0.reset;
  msm5205_0.reset;
  msm5205_1.reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  marcade.in2 := $FF7F;
  scroll_bg := 0;
  scroll_fg := 0;
  bg_paint := false;
  fg_paint := false;
  bg_act := false;
  fg_act := false;
  char_act := false;
  sp_act := false;
  misc_bank := 1;
end;

function start_streetfighter: boolean;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 16 * 16 + 0, 16 * 16 + 1, 16 * 16 + 2, 16 * 16 + 3, 16 * 16 + 8 + 0, 16 * 16 + 8 + 1, 16 * 16 + 8 + 2, 16 * 16 + 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
var
  memory_temp, ptemp: pbyte;
  f: byte;
begin
  machine_calls.general_loop := sfighter_loop;
  machine_calls.reset := reset_sfighter;
  start_streetfighter := false;
  start_audio(true);
  screen_init(1, 512, 512, false, true);
  screen_init(2, 512, 256, true);
  screen_init(3, 512, 256);
  screen_init(4, 512, 256, true);
  start_video(384, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256);
  m68000_0.change_ram16_calls(sfighter_getword, sfighter_putword);
  // Sound CPU
  z80_1 := cpu_z80.create(3579545, 256);
  z80_1.change_ram_calls(sf_snd_getbyte, sf_snd_putbyte);
  z80_1.init_sound(sound_instruccion);
  // Sub CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(sf_misc_getbyte, sf_misc_putbyte);
  z80_0.change_io_calls(sf_misc_inbyte, sf_misc_outbyte);
  timers.init(z80_0.numero_cpu, 3579545 / 8000, sf_adpcm_timer, nil, true);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  msm5205_0 := MSM5205_chip.create(384000, MSM5205_SEX_4B, 2, 0);
  msm5205_1 := MSM5205_chip.create(384000, MSM5205_SEX_4B, 2, 0);
  msm5205_0.change_advance(nil);
  msm5205_1.change_advance(nil);
  // cargar roms
  if not(roms_load16w(@rom, sfighter_rom)) then
    exit;
  // Sound CPUs
  if not(roms_load(@mem_snd, sfighter_snd)) then
    exit;
  getmem(memory_temp, $200000);
  if not(roms_load(memory_temp, sfighter_msm)) then
    exit;
  ptemp := memory_temp;
  for f := 0 to 7 do
  begin
    copymemory(@rom_misc[f, 0], ptemp, $8000);
    inc(ptemp, $8000);
  end;
  // convertir chars
  if not(roms_load(memory_temp, sfighter_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, memory_temp, @ps_x, @ps_y, false, false);
  // convertir bg y cargar tile maps
  if not(roms_load(memory_temp, sfighter_bg)) then
    exit;
  if not(roms_load(@ram_tile_map1, sfighter_tile_map1)) then
    exit;
  init_gfx(1, 16, 16, $1000);
  gfx_set_desc_data(4, 0, 64 * 8, 4, 0, $40000 * 8 + 4, $40000 * 8 + 0);
  convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
  // convertir fg y cargar tile maps
  if not(roms_load(memory_temp, sfighter_fg)) then
    exit;
  if not(roms_load(@ram_tile_map2, sfighter_tile_map2)) then
    exit;
  init_gfx(2, 16, 16, $2000);
  gfx[2].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 8, 4, 0, $80000 * 8 + 4, $80000 * 8 + 0);
  convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
  // sprites
  if not(roms_load(memory_temp, sfighter_sprites)) then
    exit;
  init_gfx(3, 16, 16, $4000);
  gfx[3].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 8, 4, 0, $E0000 * 8 + 4, $E0000 * 8 + 0);
  convert_gfx(3, 0, memory_temp, @ps_x[0], @ps_y[0], false, false);
  // DIP
  marcade.dswa := $DFFF;
  marcade.dswb := $FFFF;
  marcade.dswa_val := @sfighter_dip_a;
  marcade.dswb_val := @sfighter_dip_b;
  // final
  freemem(memory_temp);
  reset_sfighter;
  start_streetfighter := true;
end;

end.
