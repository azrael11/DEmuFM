unit sidearms_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  rom_engine,
  pal_engine,
  sound_engine;

function start_sidearms: boolean;

implementation

const
  sidearms_rom: array [0 .. 2] of tipo_roms = ((n: 'sa03.bin'; l: $8000; p: 0; crc: $E10FE6A0),
    (n: 'a_14e.rom'; l: $8000; p: $8000; crc: $4925ED03), (n: 'a_12e.rom'; l: $8000; p: $10000;
    crc: $81D0ECE7));
  sidearms_snd_rom: tipo_roms = (n: 'a_04k.rom'; l: $8000; p: 0; crc: $34EFE2D2);
  sidearms_stars: tipo_roms = (n: 'b_11j.rom'; l: $8000; p: 0; crc: $134DC35B);
  sidearms_char: tipo_roms = (n: 'a_10j.rom'; l: $4000; p: 0; crc: $651FEF75);
  sidearms_tiles: array [0 .. 7] of tipo_roms = ((n: 'b_13d.rom'; l: $8000; p: 0; crc: $3C59AFE1),
    (n: 'b_13e.rom'; l: $8000; p: $8000; crc: $64BC3B77), (n: 'b_13f.rom'; l: $8000; p: $10000;
    crc: $E6BCEA6F), (n: 'b_13g.rom'; l: $8000; p: $18000; crc: $C71A3053), (n: 'b_14d.rom'; l: $8000;
    p: $20000; crc: $826E8A97), (n: 'b_14e.rom'; l: $8000; p: $28000; crc: $6CFC02A4), (n: 'b_14f.rom';
    l: $8000; p: $30000; crc: $9B9F6730), (n: 'b_14g.rom'; l: $8000; p: $38000; crc: $EF6AF630));
  sidearms_sprites: array [0 .. 7] of tipo_roms = ((n: 'b_11b.rom'; l: $8000; p: 0; crc: $EB6F278C),
    (n: 'b_13b.rom'; l: $8000; p: $8000; crc: $E91B4014), (n: 'b_11a.rom'; l: $8000; p: $10000;
    crc: $2822C522), (n: 'b_13a.rom'; l: $8000; p: $18000; crc: $3E8A9F75), (n: 'b_12b.rom'; l: $8000;
    p: $20000; crc: $86E43EDA), (n: 'b_14b.rom'; l: $8000; p: $28000; crc: $076E92D1), (n: 'b_12a.rom';
    l: $8000; p: $30000; crc: $CE107F3C), (n: 'b_14a.rom'; l: $8000; p: $38000; crc: $DBA06076));
  sidearms_back_tiles: tipo_roms = (n: 'b_03d.rom'; l: $8000; p: 0; crc: $6F348008);
  sidearms_dip_a: array [0 .. 4] of def_dip = ((mask: $7; name: 'Difficulty'; number: 8;
    dip: ((dip_val: $7; dip_name: '0 (Easiest)'), (dip_val: $6; dip_name: '1'), (dip_val: $5;
    dip_name: '2'), (dip_val: $4; dip_name: '3 (Normal)'), (dip_val: $3; dip_name: '4'), (dip_val: $2;
    dip_name: '5'), (dip_val: $1; dip_name: '6'), (dip_val: $0; dip_name: '7 (Hardest)'), (), (), (), (), (),
    (), (), ())), (mask: $8; name: 'Lives'; number: 2; dip: ((dip_val: $8; dip_name: '3'), (dip_val: $0;
    dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life';
    number: 4; dip: ((dip_val: $30; dip_name: '100K'), (dip_val: $20; dip_name: '100K 100K'), (dip_val: $10;
    dip_name: '150K 150K'), (dip_val: $0; dip_name: '200K 200K'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $40; name: 'Flip Screen'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  sidearms_dip_b: array [0 .. 4] of def_dip = ((mask: $07; name: 'Coin A'; number: 8;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $1; dip_name: '3C 1C'), (dip_val: $02;
    dip_name: '2C 1C'), (dip_val: $07; dip_name: '1C 1C'), (dip_val: $06; dip_name: '1C 2C'), (dip_val: $05;
    dip_name: '1C 3C'), (dip_val: $04; dip_name: '1C 4C'), (dip_val: $03; dip_name: '1C 6C'), (), (), (), (),
    (), (), (), ())), (mask: $38; name: 'Coin B'; number: 8;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $8; dip_name: '3C 1C'), (dip_val: $10;
    dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28;
    dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 6C'), (), (), (), (),
    (), (), (), ())), (mask: $40; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $40; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());

var
  memory_rom: array [0 .. 7, 0 .. $3FFF] of byte;
  memory_back, memory_stars: array [0 .. $7FFF] of byte;
  scroll_x, scroll_y: word;
  sound_command, rom_bank, vblank: byte;
  char_on, obj_on, bg_on, star_on, back_redraw: boolean;
  // Stars
  vcount_191, latch_374, hflop_74a_n: byte;
  hcount_191: word;

procedure update_video_sidearms;

  procedure draw_sprites;
    procedure draw_sprites_def(total: byte; pos: word);
    var
      nchar, x: word;
      f, y, atrib, color: byte;
    begin
      for f := total downto 0 do
      begin
        y := buffer_sprites[pos + $2 + (f * 32)];
        if ((y = 0) or (buffer_sprites[pos + $5 + (f * 32)] = $C3)) then
          continue;
        atrib := buffer_sprites[pos + $1 + (f * 32)];
        color := (atrib and $F) shl 4;
        nchar := buffer_sprites[pos + $0 + (f * 32)] + ((atrib shl 3) and $700);
        x := buffer_sprites[pos + $3 + (f * 32)] + ((atrib shl 4) and $100);
        put_gfx_sprite(nchar, color + $200, false, false, 1);
        update_gfx_sprite(x, y, 3, 1);
      end;
    end;

  begin
    draw_sprites_def(8, $700);
    draw_sprites_def($10, $E00);
    draw_sprites_def($38, $800);
    draw_sprites_def($38, 0);
  end;

  procedure draw_back;
  var
    pos, offset, f, color, nchar: word;
    x, y, attr: byte;
  begin
    for f := 0 to $3FFF do
    begin
      y := f div 128;
      x := f mod 128;
      offset := ((y shl 7) + x) shl 1;
      pos := ((offset and $F801) or ((offset and $0700) shr 7) or ((offset and $00FE) shl 3)) and $7FFF;
      attr := memory_back[pos + 1];
      color := (attr shr 3) and $1F;
      nchar := memory_back[pos] or (attr and $1) shl 8;
      put_gfx_trans_flip(x * 32, y * 32, nchar, color shl 4, 2, 2, (attr and 2) <> 0, (attr and 4) <> 0);
    end;
    back_redraw := false;
  end;
  procedure draw_stars;
  var
    i, hadd_283, vadd_283, x, y: word;
    punt: array [0 .. 511] of word;
  begin
    hadd_283 := 0;
    for y := 0 to 255 do
    begin // 8-bit V-clock input
      fillword(@punt, 512, 0);
      for x := 0 to 511 do
      begin // 9-bit H-clock input
        i := hadd_283; // store horizontal adder's previous state in i
        hadd_283 := hcount_191 + (x and $FF); // add lower 8 bits and preserve carry
        if ((x < 64) or (x > 447) or (y < 16) or (y > 239)) then
          continue; // clip rejection
        vadd_283 := vcount_191 + y; // add lower 8 bits and discard carry (later)
        if ((vadd_283 xor (x shr 3)) and 4) = 0 then
          continue; // logic rejection 1
        if ((vadd_283 or (hadd_283 shr 1)) and 2) <> 0 then
          continue; // logic rejection 2
        // latch data from starfield EPROM on rising edge of 74LS374's clock input
        if ((not(i) and $1F) = 0) then
        begin
          i := (vadd_283 shl 4) and $FF0; // to starfield EPROM A04-A11 (8 bits)
          i := i or ((hflop_74a_n xor (hadd_283 shr 8)) shl 3);
          // to starfield EPROM A03     (1 bit)
          i := i or (hadd_283 shr 5 and 7); // to starfield EPROM A00-A02 (3 bits)
          latch_374 := memory_stars[i + $3000]; // lines A12-A13 are always high
        end;
        if ((not((latch_374 xor hadd_283) xor 1) and $1F) <> 0) then
          continue; // logic rejection 3
        punt[x] := paleta[(latch_374 shr 5) or $378];
      end;
      putpixel(0 + ADD_SPRITE, y + ADD_SPRITE, 512, @punt, 3);
    end;
  end;

var
  f, nchar: word;
  color, attr, x, y: byte;
begin
  if star_on then
    draw_stars
  else
    fill_full_screen(3, $400);
  if bg_on then
  begin
    if back_redraw then
      draw_back;
    scroll_x_y(2, 3, scroll_x, scroll_y);
  end;
  if obj_on then
    draw_sprites;
  if char_on then
  begin
    for f := 0 to $7FF do
    begin
      // Chars
      attr := memory[f + $D800];
      color := attr and $3F;
      if (gfx[0].buffer[f] or buffer_color[color]) then
      begin
        y := f div 64;
        x := f mod 64;
        nchar := memory[f + $D000] or ((attr and $C0) shl 2);
        put_gfx_trans(x * 8, y * 8, nchar, (color shl 2) + $300, 1, 0);
        gfx[0].buffer[f] := false;
      end;
    end;
    actualiza_trozo(0, 0, 512, 256, 1, 0, 0, 512, 256, 3);
  end;
  update_final_piece(64, 16, 384, 224, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_sidearms;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[0] then
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
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.up[1] then
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
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure sidearms_loop;
var
  f: byte;
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        case f of
          0:
            vblank := 0;
          $F0:
            begin
              z80_0.change_irq(HOLD_LINE);
              update_video_sidearms;
              vblank := $80;
              copymemory(@buffer_sprites, @memory[$F000], $1000);
            end;
        end;
      end;
      events_sidearms;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function sidearms_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF, $C000 .. $C7FF, $D000 .. $FFFF:
      sidearms_getbyte := memory[direccion];
    $8000 .. $BFFF:
      sidearms_getbyte := memory_rom[rom_bank, direccion and $3FFF];
    $C800:
      sidearms_getbyte := marcade.in0;
    $C801:
      sidearms_getbyte := marcade.in1;
    $C802:
      sidearms_getbyte := marcade.in2;
    $C803:
      sidearms_getbyte := marcade.dswa;
    $C804:
      sidearms_getbyte := marcade.dswb;
    $C805:
      sidearms_getbyte := $7F or vblank;
  end;
end;

procedure change_color(numero: word); inline;
var
  color: tcolor;
  tmp_color: word;
begin
  tmp_color := memory[$C000 + numero] + (memory[$C400 + numero] shl 8);
  color.b := pal4bit(tmp_color shr 8);
  color.r := pal4bit(tmp_color shr 4);
  color.g := pal4bit(tmp_color);
  set_pal_color(color, numero);
  case numero of
    $0 .. $1FF:
      back_redraw := true;
    $300 .. $3FF:
      buffer_color[(numero shr 2) and $3F] := true;
  end;
end;

procedure sidearms_putbyte(direccion: word; valor: byte);
  procedure change_color(numero: word);
  var
    color: tcolor;
    tmp_color: word;
  begin
    tmp_color := memory[$C000 + numero] + (memory[$C400 + numero] shl 8);
    color.b := pal4bit(tmp_color shr 8);
    color.r := pal4bit(tmp_color shr 4);
    color.g := pal4bit(tmp_color);
    set_pal_color(color, numero);
    case numero of
      $0 .. $1FF:
        back_redraw := true;
      $300 .. $3FF:
        buffer_color[(numero shr 2) and $3F] := true;
    end;
  end;

var
  last_state: word;
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      begin
        memory[direccion] := valor;
        change_color(direccion and $3FF);
      end;
    $C800:
      sound_command := valor;
    $C801:
      rom_bank := valor and $7;
    $C802:
      ; // WD
    $C804:
      begin
        if (valor and $10) <> 0 then
          z80_1.change_reset(ASSERT_LINE)
        else
          z80_1.change_reset(CLEAR_LINE);
        if (star_on <> ((valor and $20) <> 0)) then
        begin
          star_on := (valor and $20) <> 0;
          hflop_74a_n := 1;
          hcount_191 := 0;
          vcount_191 := 0;
        end;
        char_on := (valor and $40) <> 0;
        main_screen.flip_main_screen := (valor and $80) <> 0;
      end;
    $C805:
      begin
        last_state := hcount_191;
        hcount_191 := (hcount_191 + 1) and $1FF;
        // invert 74LS74A(flipflop) output on 74LS191(hscan counter) carry's rising edge
        if (hcount_191 and not(last_state) and $100) <> 0 then
          hflop_74a_n := hflop_74a_n xor 1;
      end;
    $C806:
      vcount_191 := vcount_191 + 1;
    $C808:
      scroll_x := valor or (scroll_x and $F00);
    $C809:
      scroll_x := ((valor and $F) shl 8) or (scroll_x and $FF);
    $C80A:
      scroll_y := valor or (scroll_y and $F00);
    $C80B:
      scroll_y := ((valor and $F) shl 8) or (scroll_y and $FF);
    $C80C:
      begin
        obj_on := (valor and $1) <> 0;
        bg_on := (valor and $2) <> 0;
      end;
    $D000 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $E000 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function sidearms_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $C7FF:
      sidearms_snd_getbyte := mem_snd[direccion];
    $D000:
      sidearms_snd_getbyte := sound_command;
    $F000:
      sidearms_snd_getbyte := ym2203_0.status;
    $F002:
      sidearms_snd_getbyte := ym2203_1.status;
  end;
end;

procedure sidearms_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $F000:
      ym2203_0.Control(valor);
    $F001:
      ym2203_0.Write(valor);
    $F002:
      ym2203_1.Control(valor);
    $F003:
      ym2203_1.Write(valor);
  end;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_1.change_irq(irqstate);
end;

procedure sidearms_sound_update;
begin
  ym2203_0.update;
  ym2203_1.update;
end;

// Main
procedure reset_sidearms;
begin
  z80_0.reset;
  z80_1.reset;
  ym2203_0.reset;
  ym2203_1.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  rom_bank := 0;
  vblank := 0;
  sound_command := 0;
  back_redraw := false;
  star_on := false;
  bg_on := false;
  obj_on := false;
  char_on := false;
  hflop_74a_n := 1;
  latch_374 := 0;
  vcount_191 := 0;
  hcount_191 := 0;
end;

function start_sidearms: boolean;
var
  f: word;
  memory_temp: array [0 .. $3FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1,
    32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  pt_x: array [0 .. 31] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 16 + 0, 32 * 16 + 1,
    32 * 16 + 2, 32 * 16 + 3, 32 * 16 + 8 + 0, 32 * 16 + 8 + 1, 32 * 16 + 8 + 2, 32 * 16 + 8 + 3, 64 * 16 + 0,
    64 * 16 + 1, 64 * 16 + 2, 64 * 16 + 3, 64 * 16 + 8 + 0, 64 * 16 + 8 + 1, 64 * 16 + 8 + 2, 64 * 16 + 8 + 3,
    96 * 16 + 0, 96 * 16 + 1, 96 * 16 + 2, 96 * 16 + 3, 96 * 16 + 8 + 0, 96 * 16 + 8 + 1, 96 * 16 + 8 + 2,
    96 * 16 + 8 + 3);
  pt_y: array [0 .. 31] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16,
    9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16, 16 * 16, 17 * 16, 18 * 16, 19 * 16, 20 * 16,
    21 * 16, 22 * 16, 23 * 16, 24 * 16, 25 * 16, 26 * 16, 27 * 16, 28 * 16, 29 * 16, 30 * 16, 31 * 16);
begin
  machine_calls.general_loop := sidearms_loop;
  machine_calls.reset := reset_sidearms;
  start_sidearms := false;
  start_audio(false);
  screen_init(1, 512, 512, true);
  screen_init(2, 4096, 4096, true);
  screen_mod_scroll(2, 4096, 512, 4095, 4096, 512, 4095);
  screen_init(3, 512, 512, false, true);
  start_video(384, 224);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, $100);
  z80_0.change_ram_calls(sidearms_getbyte, sidearms_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(4000000, $100);
  z80_1.change_ram_calls(sidearms_snd_getbyte, sidearms_snd_putbyte);
  z80_1.init_sound(sidearms_sound_update);
  // Sound Chips
  ym2203_0 := ym2203_chip.create(4000000, 0.25, 1);
  ym2203_0.change_irq_calls(snd_irq);
  ym2203_1 := ym2203_chip.create(4000000, 0.25, 1);
  // cargar roms y ponerlas en su sitio
  if not(roms_load(@memory_temp, sidearms_rom)) then
    exit;
  copymemory(@memory, @memory_temp, $8000);
  for f := 0 to 3 do
    copymemory(@memory_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // cargar ROMS sonido
  if not(roms_load(@mem_snd, sidearms_snd_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, sidearms_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pt_y, false, false);
  // background & stars
  if not(roms_load(@memory_back, sidearms_back_tiles)) then
    exit;
  if not(roms_load(@memory_stars, sidearms_stars)) then
    exit;
  // convertir sprites
  if not(roms_load(@memory_temp, sidearms_sprites)) then
    exit;
  init_gfx(1, 16, 16, $800);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $800 * 64 * 8 + 4, $800 * 64 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @pt_y, false, false);
  // tiles
  if not(roms_load(@memory_temp, sidearms_tiles)) then
    exit;
  init_gfx(2, 32, 32, $200);
  gfx[2].trans[15] := true;
  gfx_set_desc_data(4, 0, 256 * 8, $200 * 256 * 8 + 4, $200 * 256 * 8 + 0, 4, 0);
  convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
  // DIP
  marcade.dswa := $FC;
  marcade.dswa_val := @sidearms_dip_a;
  marcade.dswb := $FF;
  marcade.dswb_val := @sidearms_dip_b;
  // final
  reset_sidearms;
  start_sidearms := true;
end;

end.
