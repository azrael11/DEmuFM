unit nmk16_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  timer_engine,
  oki6295,
  rom_engine,
  pal_engine,
  sound_engine;

function start_nmk16: boolean;

implementation

const
  // Saboten Bombers
  sbombers_rom: array [0 .. 1] of tipo_roms = ((n: 'ic76.sb1'; l: $40000; p: 0; crc: $B2B0B2CF), (n: 'ic75.sb2'; l: $40000; p: $1; crc: $367E87B7));
  sbombers_char: tipo_roms = (n: 'ic35.sb3'; l: $10000; p: 0; crc: $EB7BC99D);
  sbombers_char2: tipo_roms = (n: 'ic32.sb4'; l: $200000; p: 0; crc: $24C62205);
  sbombers_sprites: tipo_roms = (n: 'ic100.sb5'; l: $200000; p: 0; crc: $B20F166E);
  sbombers_adpcm1: tipo_roms = (n: 'ic30.sb6'; l: $100000; p: 0; crc: $288407AF);
  sbombers_adpcm2: tipo_roms = (n: 'ic27.sb7'; l: $100000; p: 0; crc: $43E33A7E);
  // Bomb Jack Twin
  bjtwin_rom: array [0 .. 1] of tipo_roms = ((n: '93087-1.bin'; l: $20000; p: 0; crc: $93C84E2D), (n: '93087-2.bin'; l: $20000; p: $1; crc: $30FF678A));
  bjtwin_char: tipo_roms = (n: '93087-3.bin'; l: $10000; p: 0; crc: $AA13DF7C);
  bjtwin_char2: tipo_roms = (n: '93087-4.bin'; l: $100000; p: 0; crc: $8A4F26D0);
  bjtwin_sprites: tipo_roms = (n: '93087-5.bin'; l: $100000; p: 0; crc: $BB06245D);
  bjtwin_adpcm1: tipo_roms = (n: '93087-6.bin'; l: $100000; p: 0; crc: $372D46DD);
  bjtwin_adpcm2: tipo_roms = (n: '93087-7.bin'; l: $100000; p: 0; crc: $8DA67808);

var
  rom: array [0 .. $3FFFF] of word;
  ram: array [0 .. $7FFF] of word;
  bg_ram: array [0 .. $7FF] of word;
  bg_bank: byte;
  adpcm_rom: array [0 .. 1] of pbyte;
  nmk112_bank: array [0 .. 7] of byte;

procedure bank_nmk112(offset, valor: byte);
var
  chip, banknum: byte;
  bankaddr: dword;
  ptemp, ptemp2: pbyte;
begin
  chip := (offset and 4) shr 2;
  banknum := offset and 3;
  bankaddr := ((valor and $FF) * $10000) mod $100000;
  // copy the samples */
  ptemp := adpcm_rom[chip];
  inc(ptemp, bankaddr + $400);
  if chip = 0 then
    ptemp2 := oki_6295_0.get_rom_addr
  else
    ptemp2 := oki_6295_1.get_rom_addr;
  inc(ptemp2, $400 + (banknum * $10000));
  copymemory(ptemp2, ptemp, $10000 - $400);
  // copio la informacion de ADPCM
  ptemp := adpcm_rom[chip];
  inc(ptemp, bankaddr + (banknum * $100));
  if chip = 0 then
    ptemp2 := oki_6295_0.get_rom_addr
  else
    ptemp2 := oki_6295_1.get_rom_addr;
  inc(ptemp2, banknum * $100);
  copymemory(ptemp2, ptemp, $100);
end;

procedure draw_sprites(priority: byte);
var
  f: word;
  sx, sy, code, color, w, h, pri, x: word;
  xx, yy: integer;
  atrib: byte;
begin
  for f := 0 to $FF do
  begin
    atrib := ram[$4000 + (f * 4)];
    if (atrib and $01) <> 0 then
    begin
      pri := (atrib and $C0) shr 6;
      if (pri <> priority) then
        continue;
      sx := ram[$4004 + (f * 4)] + 128;
      sy := ram[$4006 + (f * 4)];
      code := ram[$4003 + (f * 4)] and $3FFF;
      color := ram[$4007 + (f * 4)] shl 4;
      w := ram[$4001 + (f * 4)] and $0F;
      h := (ram[$4001 + (f * 4)] and $F0) shr 4;
      yy := h;
      while (yy >= 0) do
      begin
        x := sx;
        xx := w;
        while (xx >= 0) do
        begin
          put_gfx_sprite(code, color + $100, false, false, 2);
          update_gfx_sprite(x, sy, 2, 2);
          code := code + 1;
          x := x + 16; // delta;
          xx := xx - 1;
        end;;
        sy := sy + 16; // delta;
        yy := yy - 1;
      end;
    end;
  end;
end;

procedure update_video_sbombers;
var
  f, color, x, y, nchar, atrib: word;
  bank: byte;
begin
  // foreground
  for f := $0 to $7FF do
  begin
    atrib := bg_ram[f];
    color := (atrib and $F000) shr 12;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f div 32;
      y := f mod 32;
      bank := (atrib and $800) shr 11;
      nchar := atrib and $7FF + ((bg_bank * bank) shl 11);
      put_gfx(((x * 8) + 128) and $1FF, y * 8, nchar, color shl 4, 1, bank);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 512, 512, 1, 0, 0, 512, 256, 2);
  draw_sprites(3);
  draw_sprites(2);
  draw_sprites(1);
  draw_sprites(0);
  update_final_piece(64, 16, 384, 224, 2);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_sbombers;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
  end;
end;

procedure nmk16_loop;
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
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        if f = 239 then
        begin
          m68000_0.irq[4] := HOLD_LINE;
          update_video_sbombers;
        end;
      end;
      events_sbombers;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function sbombers_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      sbombers_getword := rom[direccion shr 1];
    $80000:
      sbombers_getword := marcade.in0;
    $80002:
      sbombers_getword := (marcade.in2 shl 8) or marcade.in1;
    $80008:
      sbombers_getword := $FFFF;
    $8000A:
      sbombers_getword := $FFFF;
    $84000:
      sbombers_getword := oki_6295_0.read;
    $84010:
      sbombers_getword := oki_6295_1.read;
    $88000 .. $887FF:
      sbombers_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $9C000 .. $9DFFF:
      sbombers_getword := bg_ram[(direccion and $FFF) shr 1];
    $F0000 .. $FFFFF:
      sbombers_getword := ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure change_color(tmp_color, numero: word);
var
  color: tcolor;
begin
  color.r := pal5bit(tmp_color shr 11) or ((tmp_color shr 3) and $01);
  color.g := pal5bit(tmp_color shr 7) or ((tmp_color shr 2) and $01);
  color.b := pal5bit(tmp_color shr 3) or ((tmp_color shr 1) and $01);
  set_pal_color(color, numero);
  if (numero < $100) then
    buffer_color[(numero shr 4) and $F] := true;
end;

procedure sbombers_putword(direccion: dword; valor: word);
var
  offset: byte;
begin
  case direccion of
    $0 .. $7FFFF, $80014, $94002:
      exit;
    $84000:
      oki_6295_0.write(valor and $FF);
    $84010:
      oki_6295_1.write(valor and $FF);
    $84020 .. $8402F:
      begin // NMK 112, controla el banco de la ROM del OKI
        offset := (direccion and $F) shr 1;
        if nmk112_bank[offset] <> (valor and $FF) then
        begin
          nmk112_bank[offset] := (valor and $FF);
          bank_nmk112(offset, valor);
        end;
      end;
    $88000 .. $887FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $94000:
      bg_bank := valor and $FF;
    $9C000 .. $9DFFF:
      if bg_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $F0000 .. $FFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

procedure sound_irq;
begin
  m68000_0.irq[1] := HOLD_LINE;
end;

procedure nmk16_update_sound;
begin
  oki_6295_0.update;
  oki_6295_1.update;
end;

// Main
procedure reset_nmk16;
var
  f: byte;
begin
  m68000_0.reset;
  oki_6295_0.reset;
  oki_6295_1.reset;
 reset_game_general;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  bg_bank := 0;
  for f := 0 to 7 do
  begin
    nmk112_bank[f] := 0;
    bank_nmk112(f, 0);
  end;
end;

procedure close_nmk16;
begin
  if adpcm_rom[0] <> nil then
    freemem(adpcm_rom[0]);
  if adpcm_rom[1] <> nil then
    freemem(adpcm_rom[1]);
  adpcm_rom[0] := nil;
  adpcm_rom[1] := nil;
end;

function start_nmk16: boolean;
var
  mem_char: pbyte;
  memory_temp: array [0 .. $FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 16 * 32 + 0 * 4, 16 * 32 + 1 * 4, 16 * 32 + 2 * 4, 16 * 32 + 3 * 4, 16 * 32 + 4 * 4, 16 * 32 + 5 * 4,
    16 * 32 + 6 * 4, 16 * 32 + 7 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  procedure decode_gfx(rom: pbyte; len: dword);
  const
    decode_data_bg: array [0 .. 7, 0 .. 7] of byte = (($3, $0, $7, $2, $5, $1, $4, $6), ($1, $2, $6, $5, $4, $0, $3, $7), ($7, $6, $5, $4, $3, $2, $1, $0), ($7, $6, $5, $0, $1, $4, $3, $2),
      ($2, $0, $1, $4, $3, $5, $7, $6), ($5, $3, $7, $0, $4, $6, $2, $1), ($2, $7, $0, $6, $5, $3, $1, $4), ($3, $4, $7, $6, $2, $0, $5, $1));
  var
    a, addr: dword;
    ptemp: pbyte;
    function decode_byte(src: byte; bitp: pbyte): byte;
    var
      ret, i: byte;
      ptemp3: pbyte;
    begin
      ret := 0;
      ptemp3 := bitp;
      for i := 0 to 7 do
      begin
        ret := ret or (((src shr ptemp3^) and 1) shl (7 - i));
        inc(ptemp3);
      end;
      decode_byte := ret;
    end;

  begin
    // GFX are scrambled.  We decode them here.  (BIG Thanks to Antiriad for descrambling info)
    // background */
    ptemp := rom;
    for a := 0 to len - 1 do
    begin
      addr := ((a and $00004) shr 2) or ((a and $00800) shr 10) or ((a and $40000) shr 16);
      ptemp^ := decode_byte(ptemp^, @decode_data_bg[addr]);
      inc(ptemp);
    end;
  end;
  procedure decode_sprites(const rom: pbyte; len: dword);
  const
    decode_data_sprite: array [0 .. 7, 0 .. 15] of byte = (($9, $3, $4, $5, $7, $1, $B, $8, $0, $D, $2, $C, $E, $6, $F, $A), ($1, $3, $C, $4, $0, $F, $B, $A, $8, $5, $E, $6, $D, $2, $7, $9),
      ($F, $E, $D, $C, $B, $A, $9, $8, $7, $6, $5, $4, $3, $2, $1, $0), ($F, $E, $C, $6, $A, $B, $7, $8, $9, $2, $3, $4, $5, $D, $1, $0),
      ($1, $6, $2, $5, $F, $7, $B, $9, $A, $3, $D, $E, $C, $4, $0, $8), // Haze 20/07/00 */
      ($7, $5, $D, $E, $B, $A, $0, $1, $9, $6, $C, $2, $3, $4, $8, $F), // Haze 20/07/00 */
      ($0, $5, $6, $3, $9, $B, $A, $7, $1, $D, $2, $E, $4, $C, $8, $F),
      // Antiriad, Corrected by Haze 20/07/00 */
      ($9, $C, $4, $2, $F, $0, $B, $8, $A, $D, $3, $6, $5, $E, $1, $7));
    // Antiriad, Corrected by Haze 20/07/00 */
  var
    a, addr: dword;
    ptemp, ptemp2: pbyte;
    tmp: word;
    function decode_word(src: word; bitp: pbyte): word;
    var
      ret: word;
      i: byte;
      ptemp3: pbyte;
    begin
      ret := 0;
      ptemp3 := bitp;
      for i := 0 to 15 do
      begin
        ret := ret or (((src shr ptemp3^) and 1) shl (15 - i));
        inc(ptemp3);
      end;
      decode_word := ret;
    end;

  begin
    // sprites
    ptemp := rom;
    ptemp2 := rom;
    inc(ptemp2);
    for a := 0 to ((len div 2) - 1) do
    begin
      addr := (((a * 2) and $00010) shr 4) or (((a * 2) and $20000) shr 16) or (((a * 2) and $100000) shr 18);
      tmp := decode_word(ptemp2^ * 256 + ptemp^, @decode_data_sprite[addr]);
      ptemp^ := tmp and $FF;
      inc(ptemp, 2);
      ptemp2^ := tmp shr 8;
      inc(ptemp2, 2);
    end;
  end;
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $800);
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;
  procedure convert_sprites(num: word);
  begin
    init_gfx(2, 16, 16, num);
    gfx[2].trans[15] := true;
    gfx_set_desc_data(4, 0, 32 * 32, 0, 1, 2, 3);
    convert_gfx(2, 0, mem_char, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := nmk16_loop;
  machine_calls.close := close_nmk16;
  machine_calls.reset := reset_nmk16;
  machine_calls.fps_max := 56;
  start_nmk16 := false;
  start_audio(false);
  if main_vars.machine_type = 71 then
    main_screen.rot270_screen := true;
  screen_init(1, 512, 512);
  screen_init(2, 512, 512, false, true);
  start_video(384, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, $100);
  m68000_0.change_ram16_calls(sbombers_getword, sbombers_putword);
  m68000_0.init_sound(nmk16_update_sound);
  // Sound Chips
  oki_6295_0 := snd_okim6295.create(16000000 div 4, OKIM6295_PIN7_LOW);
  oki_6295_1 := snd_okim6295.create(16000000 div 4, OKIM6295_PIN7_LOW);
  // Cargar ADPCM ROMS
  getmem(adpcm_rom[0], $100000);
  getmem(adpcm_rom[1], $100000);
  // Sound timer
  timers.init(0, 10000000 / 112, sound_irq, nil, true);
  // Iniciar Maquinas
  case main_vars.machine_type of
    69:
      begin // Saboten Bombers
        // cargar roms
        if not(roms_load16w(@rom, sbombers_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, sbombers_char)) then
          exit;
        // Cargar sound roms
        if not(roms_load(adpcm_rom[0], sbombers_adpcm1)) then
          exit;
        if not(roms_load(adpcm_rom[1], sbombers_adpcm2)) then
          exit;
        convert_chars;
        getmem(mem_char, $200000);
        if not(roms_load(mem_char, sbombers_char2)) then
          exit;
        decode_gfx(mem_char, $200000);
        init_gfx(1, 8, 8, $10000);
        convert_gfx(1, 0, mem_char, @ps_x, @ps_y, false, false);
        // convertir sprites
        if not(roms_load_swap_word(mem_char, sbombers_sprites)) then
          exit;
        decode_sprites(mem_char, $200000);
        convert_sprites($4000);
        freemem(mem_char);
      end;
    71:
      begin // Bombjack Twin
        // cargar roms
        if not(roms_load16w(@rom, bjtwin_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, bjtwin_char)) then
          exit;
        // Cargar sound roms
        if not(roms_load(adpcm_rom[0], bjtwin_adpcm1)) then
          exit;
        if not(roms_load(adpcm_rom[1], bjtwin_adpcm2)) then
          exit;
        convert_chars;
        getmem(mem_char, $100000);
        if not(roms_load(mem_char, bjtwin_char2)) then
          exit;
        decode_gfx(mem_char, $100000);
        init_gfx(1, 8, 8, $8000);
        convert_gfx(1, 0, mem_char, @ps_x, @ps_y, false, false);
        // convertir sprites
        if not(roms_load_swap_word(mem_char, bjtwin_sprites)) then
          exit;
        decode_sprites(mem_char, $100000);
        convert_sprites($2000);
        freemem(mem_char);
      end;
  end;
  // final
  reset_nmk16;
  start_nmk16 := true;
end;

end.
