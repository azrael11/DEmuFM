unit operationwolf_hw;

interface

uses
  WinApi.Windows,
  m68000, main_engine, controls_engine, gfx_engine, taito_sound, rom_engine,
  pal_engine, sound_engine, opwolf_cchip, msm5205;

function start_operationwolf: boolean;

var
  rom: array [0 .. $2FFFF] of word;

implementation

const
  opwolf_rom: array [0 .. 3] of tipo_roms = ((n: 'b20-05-02.40'; l: $10000; p: 0; crc: $3FFBFE3A), (n: 'b20-03-02.30'; l: $10000; p: $1; crc: $FDABD8A5), (n: 'b20-04.39'; l: $10000; p: $20000; crc: $216B4838), (n: 'b20-20.29'; l: $10000; p: $20001; crc: $D244431A));
  opwolf_sound: tipo_roms = (n: 'b20-07.10'; l: $10000; p: 0; crc: $45C7ACE3);
  opwolf_char: tipo_roms = (n: 'b20-13.13'; l: $80000; p: 0; crc: $F6ACDAB1);
  opwolf_sprites: tipo_roms = (n: 'b20-14.72'; l: $80000; p: 0; crc: $89F889E5);
  opwolf_adpcm: tipo_roms = (n: 'b20-08.21'; l: $80000; p: 0; crc: $F3E19C64);

var
  scroll_x1, scroll_y1, scroll_x2, scroll_y2: word;
  ram1: array [0 .. $3FFF] of word;
  ram3: array [0 .. $1FFF] of word;
  spritebank: byte;
  ram2: array [0 .. $7FFF] of word;

procedure update_video_opwolf;
var
  f, x, y, nchar, atrib, color: word;
  flipx, flipy: boolean;
begin
  for f := $FFF downto $0 do
  begin
    // background
    atrib := ram2[f * 2];
    color := atrib and $7F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := (ram2[$1 + (f * 2)]) and $3FFF;
      flipx := (atrib and $4000) <> 0;
      flipy := (atrib and $8000) <> 0;
      put_gfx_flip(x * 8, y * 8, nchar, color shl 4, 1, 0, flipx, flipy);
      gfx[0].buffer[f] := false;
    end;
    // foreground
    atrib := ram2[$4000 + (f * 2)];
    color := atrib and $7F;
    if (gfx[0].buffer[f + $1000] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := (ram2[$4001 + (f * 2)]) and $3FFF;
      flipx := (atrib and $4000) <> 0;
      flipy := (atrib and $8000) <> 0;
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 2, 0, flipx, flipy);
      gfx[0].buffer[f + $1000] := false;
    end;
  end;
  scroll_x_y(1, 3, scroll_x1, scroll_y1);
  // Sprites
  for f := $FF downto 0 do
  begin
    nchar := (ram3[$2 + (f * 4)]) and $FFF;
    if nchar <> 0 then
    begin
      atrib := ram3[f * 4];
      color := ((atrib and $F) or ((spritebank and $F) shl 4)) shl 4;
      put_gfx_sprite(nchar, color, (atrib and $4000) <> 0, (atrib and $8000) <> 0, 1);
      x := ram3[$3 + (f * 4)] + 16;
      y := ram3[$1 + (f * 4)];
      update_gfx_sprite(x, y, 3, 1);
    end;
  end;
  scroll_x_y(2, 3, scroll_x2, scroll_y2);
  update_final_piece(16, 8, 320, 240, 3);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_opwolf;
begin
  if event.mouse then
  begin
    if mouse_def.button1 then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if mouse_def.button2 then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
  end;
  if event.arcade then
  begin
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
  end;
end;

procedure opwolf_loop;
var
  f: byte;
begin
  init_controls(true, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        events_opwolf;
        if f = 248 then
        begin
          update_video_opwolf;
          m68000_0.irq[5] := HOLD_LINE;
        end;
        // Main CPU
        m68000_0.run(frame_main);
        frame_main := frame_main + m68000_0.tframes - m68000_0.contador;
        // Sound CPU
        tc0140syt_0.run;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function opwolf_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      opwolf_getword := rom[direccion shr 1];
    $0F0000 .. $0FFFFF:
      case (direccion and $FFF) of
        $0 .. $7FF:
          opwolf_getword := opwolf_cchip_data_r(direccion and $7FF);
        $802:
          opwolf_getword := opwolf_cchip_status_r;
      end;
    $100000 .. $107FFF:
      opwolf_getword := ram1[(direccion and $7FFF) shr 1];
    $200000 .. $200FFF:
      opwolf_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $380000:
      opwolf_getword := 1 + 0 + 4 + 8 + $30 + $C0;
    $380002:
      opwolf_getword := $7F;
    $3A0000:
      opwolf_getword := mouse_def.x + 15; // mouse x
    $3A0002:
      opwolf_getword := mouse_def.y; // mouse y
    $3E0002:
      if m68000_0.read_8bits_hi_dir then
        opwolf_getword := tc0140syt_0.comm_r;
    $C00000 .. $C0FFFF:
      opwolf_getword := ram2[(direccion and $FFFF) shr 1];
    $D00000 .. $D03FFF:
      opwolf_getword := ram3[(direccion and $3FFF) shr 1];
  end;
end;

procedure opwolf_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.r := pal4bit(tmp_color shr 8);
    color.g := pal4bit(tmp_color shr 4);
    color.b := pal4bit(tmp_color);
    set_pal_color(color, numero);
    buffer_color[(numero shr 4) and $7F] := true;
  end;

begin
  if direccion < $40000 then
    exit;
  case direccion of
    $0FF000 .. $0FF7FF:
      opwolf_cchip_data_w(direccion and $7FF, valor);
    $0FF802:
      opwolf_cchip_status_w(valor);
    $0FFC00:
      opwolf_cchip_bank_w(valor);
    $100000 .. $107FFF:
      ram1[(direccion and $7FFF) shr 1] := valor;
    $200000 .. $200FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color(valor, (direccion and $FFF) shr 1);
      end;
    $350008, $3C0000:
      ;
    $380000:
      spritebank := (valor and $E0) shr 5;
    $3E0000:
      tc0140syt_0.port_w(valor shr 8);
    $3E0002:
      tc0140syt_0.comm_w(valor shr 8);
    $C00000 .. $C03FFF:
      begin
        ram2[(direccion and $FFFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $C04000 .. $C07FFF, $C0C000 .. $C0FFFF:
      ram2[(direccion and $FFFF) shr 1] := valor;
    $C08000 .. $C0BFFF:
      begin
        ram2[(direccion and $FFFF) shr 1] := valor;
        gfx[0].buffer[((direccion and $3FFF) shr 2) + $1000] := true;
      end;
    $C20000:
      scroll_y1 := (512 - valor) and $1FF;
    $C20002:
      scroll_y2 := (512 - valor) and $1FF;
    $C40000:
      scroll_x1 := (512 - valor) and $1FF;
    $C40002:
      scroll_x2 := (512 - valor) and $1FF;
    $C50000 .. $C50003:
      ;
    $D00000 .. $D03FFF:
      ram3[(direccion and $3FFF) shr 1] := valor;
  end;
end;

// Main
procedure reset_opwolf;
begin
  m68000_0.reset;
  tc0140syt_0.reset;
  opwolf_cchip_reset;
  frame_main := m68000_0.tframes;
  marcade.in0 := $FC;
  marcade.in1 := $FF;
  scroll_x1 := 0;
  scroll_y1 := 0;
  scroll_x2 := 0;
  scroll_y2 := 0;
end;

function start_operationwolf: boolean;
const
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (2 * 4, 3 * 4, 0 * 4, 1 * 4, 6 * 4, 7 * 4, 4 * 4, 5 * 4, 10 * 4, 11 * 4, 8 * 4, 9 * 4, 14 * 4, 15 * 4, 12 * 4, 13 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
var
  memory_temp: array [0 .. $7FFFF] of byte;
begin
  start_operationwolf := false;
  machine_calls.general_loop := opwolf_loop;
  machine_calls.reset := reset_opwolf;
  start_audio(true);
  screen_init(1, 512, 512);
  screen_mod_scroll(1, 512, 512, 511, 512, 256, 511);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 256, 511);
  screen_init(3, 512, 512, false, true);
  start_video(320, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256);
  m68000_0.change_ram16_calls(opwolf_getword, opwolf_putword);
  if not(roms_load16w(@rom, opwolf_rom)) then
    exit;
  // Sound CPU
  tc0140syt_0 := tc0140syt_chip.create(4000000, 256, SOUND_OPWOLF);
  if not(roms_load(msm5205_0.rom_data, opwolf_adpcm)) then
    exit;
  if not(roms_load(msm5205_1.rom_data, opwolf_adpcm)) then
    exit;
  // cargar sonido+ponerlas en su banco+adpcm
  if not(roms_load(@memory_temp, opwolf_sound)) then
    exit;
  copymemory(@tc0140syt_0.snd_rom, @memory_temp, $4000);
  copymemory(@tc0140syt_0.snd_bank_rom[0, 0], @memory_temp[0], $4000);
  copymemory(@tc0140syt_0.snd_bank_rom[1, 0], @memory_temp[$4000], $4000);
  copymemory(@tc0140syt_0.snd_bank_rom[2, 0], @memory_temp[$8000], $4000);
  copymemory(@tc0140syt_0.snd_bank_rom[3, 0], @memory_temp[$C000], $4000);
  // MCU
  opwolf_init_cchip(m68000_0.numero_cpu);
  // convertir chars
  if not(roms_load(@memory_temp, opwolf_char)) then
    exit;
  init_gfx(0, 8, 8, $4000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, opwolf_sprites)) then
    exit;
  init_gfx(1, 16, 16, $1000);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // final
  show_mouse_cursor(true);
  start_operationwolf := true;
end;

end.
