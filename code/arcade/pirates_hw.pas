unit pirates_hw;

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
  oki6295,
  misc_functions;

function start_pirates: boolean;

implementation

const
  // Pirates
  pirates_rom: array [0 .. 1] of tipo_roms = ((n: 'r_449b.bin'; l: $80000; p: 0; crc: $224AEEDA), (n: 'l_5c1e.bin'; l: $80000; p: $1; crc: $46740204));
  pirates_gfx: array [0 .. 3] of tipo_roms = ((n: 'p4_4d48.bin'; l: $80000; p: 0; crc: $89FDA216), (n: 'p2_5d74.bin'; l: $80000; p: $80000; crc: $40E069B4), (n: 'p1_7b30.bin'; l: $80000; p: $100000;
    crc: $26D78518), (n: 'p8_9f4f.bin'; l: $80000; p: $180000; crc: $F31696EA));
  pirates_sprites: array [0 .. 3] of tipo_roms = ((n: 's1_6e89.bin'; l: $80000; p: 0; crc: $C78A276F), (n: 's2_6df3.bin'; l: $80000; p: $80000; crc: $9F0BAD96), (n: 's4_fdcc.bin'; l: $80000;
    p: $100000; crc: $8916DDB5), (n: 's8_4b7c.bin'; l: $80000; p: $180000; crc: $1C41BD2C));
  pirates_oki: tipo_roms = (n: 's89_49d4.bin'; l: $80000; p: 0; crc: $63A739EC);
  // Genix Family
  genix_rom: array [0 .. 1] of tipo_roms = ((n: '1.15'; l: $80000; p: 0; crc: $D26ABFB0), (n: '2.16'; l: $80000; p: $1; crc: $A14A25B4));
  genix_gfx: array [0 .. 3] of tipo_roms = ((n: '7.34'; l: $40000; p: 0; crc: $58DA8AAC), (n: '9.35'; l: $40000; p: $80000; crc: $96BAD9A8), (n: '8.48'; l: $40000; p: $100000; crc: $0DDC58B6),
    (n: '10.49'; l: $40000; p: $180000; crc: $2BE308C5));
  genix_sprites: array [0 .. 3] of tipo_roms = ((n: '6.69'; l: $40000; p: 0; crc: $B8422AF7), (n: '5.70'; l: $40000; p: $80000; crc: $E46125C5), (n: '4.71'; l: $40000; p: $100000; crc: $7A8ED21B),
    (n: '3.72'; l: $40000; p: $180000; crc: $F78BD6CA));
  genix_oki: tipo_roms = (n: '0.31'; l: $80000; p: 0; crc: $80D087BC);

var
  rom: array [0 .. $7FFFF] of word;
  sound_rom: array [0 .. 1, 0 .. $3FFFF] of byte;
  ram1: array [0 .. $7FFFF] of word;
  ram2: array [0 .. $3FFF] of word;
  sprite_ram: array [0 .. $7FF] of word;
  scroll_x: word;

procedure update_video_pirates;
var
  f, x, y, nchar, color: word;
  procedure draw_sprites;
  var
    f, nchar, color, atrib, sx, sy: word;
    flip_x, flip_y: boolean;
  begin
    for f := 0 to $1FD do
    begin
      sy := sprite_ram[3 + (f * 4)]; // indeed...
      if (sy and $8000) <> 0 then
        exit; // end-of-list marker */
      sx := sprite_ram[5 + (f * 4)] - 32;
      atrib := sprite_ram[6 + (f * 4)];
      nchar := atrib shr 2;
      color := sprite_ram[4 + (f * 4)] and $FF;
      flip_x := (atrib and 2) <> 0;
      flip_y := (atrib and 1) <> 0;
      sy := $F2 - sy;
      put_gfx_sprite(nchar, (color shl 4) + $1800, flip_x, flip_y, 1);
      update_gfx_sprite(sx, sy, 4, 1);
    end;
  end;

begin
  for f := $0 to $47F do
  begin
    x := f div 32;
    y := f mod 32;
    // txt
    color := ram2[$C1 + (f * 2)] and $1FF;
    if ((gfx[0].buffer[f]) or (buffer_color[color])) then
    begin
      nchar := ram2[$C0 + (f * 2)];
      put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  for f := $0 to $7FF do
  begin
    x := f div 32;
    y := f mod 32;
    // bg
    color := ram2[$1541 + (f * 2)] and $1FF;
    if ((gfx[0].buffer[f + $480]) or (buffer_color[color + $100])) then
    begin
      nchar := ram2[$1540 + (f * 2)];
      put_gfx(x * 8, y * 8, nchar, (color + $100) shl 4, 2, 0);
      gfx[0].buffer[f + $480] := false;
    end;
    // fg
    color := ram2[$9C1 + (f * 2)] and $1FF;
    if ((gfx[0].buffer[f + $C80]) or (buffer_color[color + $80])) then
    begin
      nchar := ram2[$9C0 + (f * 2)];
      put_gfx_trans(x * 8, y * 8, nchar, (color + $80) shl 4, 3, 0);
      gfx[0].buffer[f + $C80] := false;
    end;
  end;
  scroll__x(2, 4, scroll_x);
  scroll__x(3, 4, scroll_x);
  draw_sprites;
  update_region(0, 0, 288, 256, 1, 0, 0, 288, 256, 4);
  update_final_piece(0, 16, 288, 224, 4);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_pirates;
begin
  if event.arcade then
  begin
    // input
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or $0001);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or $0002);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or $0004);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or $0008);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $0010);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $0020);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $0040);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FF7F)
    else
      marcade.in1 := (marcade.in1 or $0080);

    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FEFF)
    else
      marcade.in1 := (marcade.in1 or $0100);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $0200);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FBFF)
    else
      marcade.in1 := (marcade.in1 or $0400);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7FF)
    else
      marcade.in1 := (marcade.in1 or $0800);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    if p_contrls.map_arcade.but3[1] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7FFF)
    else
      marcade.in1 := (marcade.in1 or $8000);
    // system
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

procedure pirates_loop;
var
  frame_m: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        if (f = 239) then
        begin
          m68000_0.irq[1] := HOLD_LINE;
          update_video_pirates;
        end;
      end;
      events_pirates;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function pirates_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      pirates_getword := rom[direccion shr 1];
    $100000 .. $10FFFF:
      pirates_getword := ram1[(direccion and $FFFF) shr 1];
    $300000:
      pirates_getword := marcade.in1;
    $400000:
      pirates_getword := marcade.in0;
    $500000 .. $500FFF:
      pirates_getword := sprite_ram[(direccion and $FFF) shr 1];
    $800000 .. $803FFF:
      pirates_getword := buffer_paleta[(direccion and $3FFF) shr 1];
    $900000 .. $907FFF:
      pirates_getword := ram2[(direccion and $7FFF) shr 1];
    $A00000:
      pirates_getword := oki_6295_0.read;
  end;
end;

procedure pirates_putword(direccion: dword; valor: word);
  procedure change_color(pos, data: word);
  var
    color: tcolor;
  begin
    color.r := pal5bit(data shr 10);
    color.g := pal5bit((data shr 5) and $1F);
    color.b := pal5bit(data);
    set_pal_color(color, pos);
    buffer_color[pos shr 4] := true;
  end;

begin
  case direccion of
    $100000 .. $10FFFF:
      ram1[(direccion and $FFFF) shr 1] := valor;
    $500000 .. $500FFF:
      sprite_ram[(direccion and $FFF) shr 1] := valor;
    $600000:
      begin
        // eeprom missing
        copymemory(oki_6295_0.get_rom_addr, @sound_rom[(valor and $40) shr 6, 0], $40000);
      end;
    $700000:
      scroll_x := valor and $1FF;
    $800000 .. $803FFF:
      if buffer_paleta[(direccion and $3FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $3FFF) shr 1] := valor;
        change_color((direccion and $3FFF) shr 1, valor);
      end;
    $900000 .. $907FFF:
      begin
        ram2[(direccion and $7FFF) shr 1] := valor;
        case (direccion and $7FFF) of
          $180 .. $137F:
            gfx[0].buffer[((direccion and $7FFF) - $180) shr 2] := true;
          $1380 .. $2A7F:
            gfx[0].buffer[$C80 + (((direccion and $7FFF) - $1380) shr 2)] := true;
          $2A80 .. $4187:
            gfx[0].buffer[$480 + (((direccion and $7FFF) - $2A80) shr 2)] := true;
        end;
      end;
    $A00000:
      oki_6295_0.write(valor and $FF)
  end;
end;

procedure pirates_sound_update;
begin
  oki_6295_0.update;
end;

function genix_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      genix_getword := rom[direccion shr 1];
    $100000 .. $10FFFF:
      case (direccion and $FFFF) of
        $9E98:
          genix_getword := 4; // proteccion
        $9E99 .. $9E9B:
          genix_getword := 0; // proteccion
      else
        genix_getword := ram1[(direccion and $FFFF) shr 1];
      end;
    $300000:
      genix_getword := marcade.in1;
    $400000:
      genix_getword := marcade.in0;
    $500000 .. $500FFF:
      genix_getword := sprite_ram[(direccion and $FFF) shr 1];
    $800000 .. $803FFF:
      genix_getword := buffer_paleta[(direccion and $3FFF) shr 1];
    $900000 .. $907FFF:
      genix_getword := ram2[(direccion and $7FFF) shr 1];
    $A00000:
      genix_getword := oki_6295_0.read;
  end;
end;

// Main
procedure reset_pirates;
begin
  m68000_0.reset;
  oki_6295_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $9F;
  marcade.in1 := $FFFF;
end;

function start_pirates: boolean;
var
  ptempw: pword;
  ptempb, ptempb2: pbyte;
  procedure decr_and_load_oki;
  var
    f, adrr: dword;
    ptempb3, ptempb4: pbyte;
  begin
    ptempb3 := ptempb;
    for f := 0 to $7FFFF do
    begin
      adrr := BITSWAP24(f, 23, 22, 21, 20, 19, 10, 16, 13, 8, 4, 7, 11, 14, 17, 12, 6, 2, 0, 5, 18, 15, 3, 1, 9);
      ptempb4 := ptempb2;
      inc(ptempb4, adrr);
      ptempb4^ := BITSWAP8(ptempb3^, 2, 3, 4, 0, 7, 5, 1, 6);
      inc(ptempb3);
    end;
    copymemory(@sound_rom[0, 0], ptempb2, $40000);
    ptempb3 := ptempb2;
    inc(ptempb3, $40000);
    copymemory(@sound_rom[1, 0], ptempb3, $40000);
  end;
  procedure decr_and_load_rom;
  var
    ptempw2: pword;
    f, adrl, adrr: dword;
    vl, vr: byte;
  begin
    for f := 0 to $7FFFF do
    begin
      ptempw2 := ptempw;
      adrl := BITSWAP24(f, 23, 22, 21, 20, 19, 18, 4, 8, 3, 14, 2, 15, 17, 0, 9, 13, 10, 5, 16, 7, 12, 6, 1, 11);
      inc(ptempw2, adrl);
      vl := BITSWAP8(ptempw2^ and $FF, 4, 2, 7, 1, 6, 5, 0, 3);
      ptempw2 := ptempw;
      adrr := BITSWAP24(f, 23, 22, 21, 20, 19, 18, 4, 10, 1, 11, 12, 5, 9, 17, 14, 0, 13, 6, 15, 8, 3, 16, 7, 2);
      inc(ptempw2, adrr);
      vr := BITSWAP8(ptempw2^ shr 8, 1, 4, 7, 0, 3, 5, 6, 2);
      rom[f] := (vr shl 8) or vl;
    end;
  end;
  procedure decr_and_load_gfx;
  const
    pt_x: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
    pt_y: array [0 .. 7] of dword = (8 * 0, 8 * 1, 8 * 2, 8 * 3, 8 * 4, 8 * 5, 8 * 6, 8 * 7);
  var
    f, adrr: dword;
    ptempb3, ptempb4: pbyte;
  begin
    for f := 0 to $7FFFF do
    begin
      adrr := BITSWAP24(f, 23, 22, 21, 20, 19, 18, 10, 2, 5, 9, 7, 13, 16, 14, 11, 4, 1, 6, 12, 17, 3, 0, 15, 8);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr);
      ptempb4 := ptempb;
      inc(ptempb4, f);
      ptempb3^ := BITSWAP8(ptempb4^, 2, 3, 4, 0, 7, 5, 1, 6);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr + $80000);
      ptempb4 := ptempb;
      inc(ptempb4, f + $80000);
      ptempb3^ := BITSWAP8(ptempb4^, 4, 2, 7, 1, 6, 5, 0, 3);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr + $100000);
      ptempb4 := ptempb;
      inc(ptempb4, f + $100000);
      ptempb3^ := BITSWAP8(ptempb4^, 1, 4, 7, 0, 3, 5, 6, 2);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr + $180000);
      ptempb4 := ptempb;
      inc(ptempb4, f + $180000);
      ptempb3^ := BITSWAP8(ptempb4^, 2, 3, 4, 0, 7, 5, 1, 6);
    end;
    init_gfx(0, 8, 8, $10000);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 8 * 8, $180000 * 8, $100000 * 8, $80000 * 8, 0);
    convert_gfx(0, 0, ptempb2, @pt_x[0], @pt_y[0], false, false);
  end;
  procedure decr_and_load_sprites;
  const
    ps_x: array [0 .. 15] of dword = (7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8);
    ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  var
    f, adrr: dword;
    ptempb3, ptempb4: pbyte;
  begin
    for f := 0 to $7FFFF do
    begin
      adrr := BITSWAP24(f, 23, 22, 21, 20, 19, 18, 17, 5, 12, 14, 8, 3, 0, 7, 9, 16, 4, 2, 6, 11, 13, 1, 10, 15);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr);
      ptempb4 := ptempb;
      inc(ptempb4, f);
      ptempb3^ := BITSWAP8(ptempb4^, 4, 2, 7, 1, 6, 5, 0, 3);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr + $80000);
      ptempb4 := ptempb;
      inc(ptempb4, f + $80000);
      ptempb3^ := BITSWAP8(ptempb4^, 1, 4, 7, 0, 3, 5, 6, 2);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr + $100000);
      ptempb4 := ptempb;
      inc(ptempb4, f + $100000);
      ptempb3^ := BITSWAP8(ptempb4^, 2, 3, 4, 0, 7, 5, 1, 6);
      ptempb3 := ptempb2;
      inc(ptempb3, adrr + $180000);
      ptempb4 := ptempb;
      inc(ptempb4, f + $180000);
      ptempb3^ := BITSWAP8(ptempb4^, 4, 2, 7, 1, 6, 5, 0, 3);
    end;
    init_gfx(1, 16, 16, $4000);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 16, $180000 * 8, $100000 * 8, $80000 * 8, 0);
    convert_gfx(1, 0, ptempb2, @ps_x[0], @ps_y[0], false, false);
  end;

begin
  start_pirates := false;
  machine_calls.general_loop := pirates_loop;
  machine_calls.reset := reset_pirates;
  start_audio(false);
  // Pantallas
  screen_init(1, 288, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 512, 256, false, true);
  start_video(288, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(16000000, 256);
  m68000_0.init_sound(pirates_sound_update);
  // sound
  oki_6295_0 := snd_okim6295.create(1333333, OKIM6295_PIN7_LOW);
  getmem(ptempb, $200000);
  getmem(ptempb2, $200000);
  case main_vars.machine_type of
    206:
      begin // Pirates
        m68000_0.change_ram16_calls(pirates_getword, pirates_putword);
        // OKI snd
        if not(roms_load(ptempb, pirates_oki)) then
          exit;
        decr_and_load_oki;
        // cargar roms
        getmem(ptempw, $100000);
        if not(roms_load16w(ptempw, pirates_rom)) then
          exit;
        decr_and_load_rom;
        freemem(ptempw);
        // Protection patch
        rom[$62C0 shr 1] := $6006;
        // cargar gfx
        if not(roms_load(ptempb, pirates_gfx)) then
          exit;
        decr_and_load_gfx;
        // sprites
        if not(roms_load(ptempb, pirates_sprites)) then
          exit;
        decr_and_load_sprites;
      end;
    207:
      begin // Genix Family
        m68000_0.change_ram16_calls(genix_getword, pirates_putword);
        // OKI snd
        if not(roms_load(ptempb, genix_oki)) then
          exit;
        decr_and_load_oki;
        // cargar roms
        getmem(ptempw, $100000);
        if not(roms_load16w(ptempw, genix_rom)) then
          exit;
        decr_and_load_rom;
        freemem(ptempw);
        // cargar gfx
        if not(roms_load(ptempb, genix_gfx)) then
          exit;
        decr_and_load_gfx;
        // sprites
        if not(roms_load(ptempb, genix_sprites)) then
          exit;
        decr_and_load_sprites;
      end;
  end;
  freemem(ptempb);
  freemem(ptempb2);
  // final
  reset_pirates;
  start_pirates := true;
end;

end.
