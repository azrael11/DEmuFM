unit badlands_hw;

interface

uses
  WinApi.Windows,
  m6502,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ym_2151,
  atari_mo,
  file_engine;

function start_badlands: boolean;

implementation
uses
  uDataModule;

const
  badlands_rom: array [0 .. 3] of tipo_roms = ((n: '136074-1008.20f'; l: $10000; p: 0; crc: $A3DA5774), (n: '136074-1006.27f'; l: $10000; p: $1; crc: $AA03B4F3), (n: '136074-1009.17f'; l: $10000;
    p: $20000; crc: $0E2E807F), (n: '136074-1007.24f'; l: $10000; p: $20001; crc: $99A20C2C));
  badlands_sound: tipo_roms = (n: '136074-1018.9c'; l: $10000; p: $0; crc: $A05FD146);
  badlands_back: array [0 .. 5] of tipo_roms = ((n: '136074-1012.4n'; l: $10000; p: 0; crc: $5D124C6C), (n: '136074-1013.2n'; l: $10000; p: $10000; crc: $B1EC90D6), (n: '136074-1014.4s'; l: $10000;
    p: $20000; crc: $248A6845), (n: '136074-1015.2s'; l: $10000; p: $30000; crc: $792296D8), (n: '136074-1016.4u'; l: $10000; p: $40000; crc: $878F7C66), (n: '136074-1017.2u'; l: $10000; p: $50000;
    crc: $AD0071A3));
  badlands_mo: array [0 .. 2] of tipo_roms = ((n: '136074-1010.14r'; l: $10000; p: 0; crc: $C15F629E), (n: '136074-1011.10r'; l: $10000; p: $10000; crc: $FB0B6717), (n: '136074-1019.14t'; l: $10000;
    p: $20000; crc: $0E26BFF6));
  badlands_proms: array [0 .. 2] of tipo_roms = ((n: '74s472-136037-101.7u'; l: $200; p: 0; crc: $2964F76F), (n: '74s472-136037-102.5l'; l: $200; p: $200; crc: $4D4FEC6C), (n: '74s287-136037-103.4r';
    l: $100; p: $400; crc: $6C5CCF08));
  badlands_mo_config: atari_motion_objects_config = (gfxindex: 1; // index to which gfx system */
    bankcount: 1; // number of motion object banks */
    linked: false; // are the entries linked? */
    split: true; // are the entries split? */
    reverse: false; // render in reverse order? */
    swapxy: false; // render in swapped X/Y order? */
    nextneighbor: false; // does the neighbor bit affect the next object? */
    slipheight: 0; // pixels per SLIP entry (0 for no-slip) */
    slipoffset: 0; // pixel offset for SLIPs */
    maxperline: 0; // maximum number of links to visit/scanline (0=all) */
    palettebase: $80; // base palette entry */
    maxcolors: $80; // maximum number of colors */
    transpen: 0; // transparent pen index */
    link_entry: (0, 0, 0, $03F); // mask for the link */
    code_entry: (data_lower: ($0FFF, 0, 0, 0); data_upper: (0, 0, 0, 0));
    // mask for the code index */
    color_entry: (data_lower: (0, 0, 0, $0007); data_upper: (0, 0, 0, 0)); // mask for the color */
    xpos_entry: (0, 0, 0, $FF80); // mask for the X position */
    ypos_entry: (0, $FF80, 0, 0); // mask for the Y position */
    width_entry: (0, 0, 0, 0); // mask for the width, in tiles*/
    height_entry: (0, $000F, 0, 0); // mask for the height, in tiles */
    hflip_entry: (0, 0, 0, 0); // mask for the horizontal flip */
    vflip_entry: (0, 0, 0, 0); // mask for the vertical flip */
    priority_entry: (0, 0, 0, $0008); // mask for the priority */
    neighbor_entry: (0, 0, 0, 0); // mask for the neighbor */
    absolute_entry: (0, 0, 0, 0); // mask for absolute coordinates */
    special_entry: (0, 0, 0, 0); // mask for the special value */
    specialvalue: 0; // resulting value to indicate "special" */
  );

var
  rom: array [0 .. $1FFFF] of word;
  ram: array [0 .. $FFF] of word;
  eeprom_ram: array [0 .. $FFF] of byte;
  sound_rom: array [0 .. 3, 0 .. $FFF] of byte;
  pedal1, pedal2, sound_bank, playfield_tile_bank, soundlatch, mainlatch: byte;
  write_eeprom, main_pending, sound_pending: boolean;
  pant_bl: array [0 .. ((512 * 256) - 1)] of word;

procedure update_video_badlands;
  procedure put_gfx_bl(pos_x, pos_y, nchar, color: word; ngfx: byte; pant_dest: pword);
  var
    x, y: byte;
    temp, temp2: pword;
    pos: pbyte;
  begin
    pos := gfx[ngfx].datos;
    inc(pos, nchar * 8 * 8);
    for y := 0 to 7 do
    begin
      temp := punbuf;
      for x := 0 to 7 do
      begin
        temp^ := gfx[ngfx].colores[pos^ + color];
        inc(pos);
        inc(temp);
      end;
      temp2 := pant_dest;
      inc(temp2, ((pos_y + y) * 512) + pos_x);
      copymemory(temp2, punbuf, 8 * 2);
    end;
  end;

var
  f, color, x, y, nchar, atrib: word;
  pant1, pant2: array [0 .. ((512 * 256) - 1)] of word;
  cont: dword;
  repaint: boolean;
begin
  repaint := false;
  for f := 0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib := ram[f];
    if (atrib and $1000) <> 0 then
      nchar := (atrib and $1FFF) + (playfield_tile_bank shl 12)
    else
      nchar := (atrib and $1FFF);
    color := (atrib shr 13) and 7;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      put_gfx_bl(x * 8, y * 8, nchar, color shl 4, 0, @pant_bl);
      gfx[0].buffer[f] := false;
      repaint := true;
    end;
  end;
  if repaint then
  begin
    for cont := 0 to ((512 * 256) - 1) do
    begin
      atrib := pant_bl[cont];
      if (atrib and 8) <> 0 then
      begin
        pant1[cont] := paleta[0];
        pant2[cont] := paleta[atrib];
      end
      else
      begin
        pant1[cont] := paleta[atrib];
        pant2[cont] := paleta[MAX_COLORS];
      end;
    end;
    putpixel(0, 0, 512 * 256, @pant1, 1);
    putpixel(0, 0, 512 * 256, @pant2, 2);
  end;
  update_region(0, 0, 512, 256, 1, 0, 0, 512, 256, 3);
  atari_mo_0.draw(0, 0, 0);
  update_region(0, 0, 512, 256, 2, 0, 0, 512, 256, 3);
  atari_mo_0.draw(0, 0, 1);
  update_final_piece(0, 0, 336, 240, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_badlands;
begin
  if event.arcade then
  begin
    // Audio CPU
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    // Buttons
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // Pedals
    if p_contrls.map_arcade.but1[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
  end;
end;

procedure badlands_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 261 do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sound
        m6502_0.run(frame_s);
        frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        case f of
          0, 64, 128, 192:
            m6502_0.change_irq(ASSERT_LINE);
          239:
            begin // VBLANK
              update_video_badlands;
              m68000_0.irq[1] := ASSERT_LINE;
              marcade.in1 := marcade.in1 or $40;
            end;
          261:
            marcade.in1 := marcade.in1 and $BF;
        end;
      end;
      if (marcade.in2 and 1) = 0 then
        pedal1 := pedal1 - 1;
      if (marcade.in2 and 2) = 0 then
        pedal2 := pedal2 - 1;
      events_badlands;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function badlands_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      badlands_getword := rom[direccion shr 1];
    $FC0000 .. $FC1FFF:
      badlands_getword := $FEFF or ($100 * byte(sound_pending));
    $FD0000 .. $FD1FFF:
      badlands_getword := $FF00 or eeprom_ram[(direccion and $1FFF) shr 1];
    $FE4000 .. $FE5FFF:
      badlands_getword := marcade.in1; // in1
    $FE6000:
      badlands_getword := $FF00 or analog.c[0].x[0]; // in2
    $FE6002:
      badlands_getword := $FF00 or analog.c[0].x[1]; // in3
    $FE6004:
      badlands_getword := pedal1; // pedal1
    $FE6006:
      badlands_getword := pedal2; // pedal2
    $FEA000 .. $FEBFFF:
      begin
        badlands_getword := mainlatch shl 8;
        main_pending := false;
        m68000_0.irq[2] := CLEAR_LINE;
      end;
    $FFC000 .. $FFC3FF:
      badlands_getword := buffer_paleta[(direccion and $3FF) shr 1];
    $FFE000 .. $FFFFFF:
      badlands_getword := ram[(direccion and $1FFF) shr 1];
  end;
end;

procedure badlands_putword(direccion: dword; valor: word);

  procedure change_color(numero: word);
  var
    color: tcolor;
    i: byte;
    tmp_color: word;
  begin
    numero := numero shr 1;
    tmp_color := (buffer_paleta[numero * 2] and $FF00) or (buffer_paleta[(numero * 2) + 1] shr 8);
    i := (tmp_color shr 15) and 1;
    color.r := pal6bit(((tmp_color shr 9) and $3E) or i);
    color.g := pal6bit(((tmp_color shr 4) and $3E) or i);
    color.b := pal6bit(((tmp_color shl 1) and $3E) or i);
    set_pal_color(color, numero);
    if numero < $80 then
      buffer_color[(numero shr 4) and 7] := true;
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $FC0000 .. $FC1FFF:
      begin
        m6502_0.change_reset(PULSE_LINE);
        sound_bank := 0;
        ym2151_0.reset;
      end;
    $FD0000 .. $FD1FFF:
      if write_eeprom then
      begin
        eeprom_ram[(direccion and $1FFF) shr 1] := valor;
        write_eeprom := false;
      end;
    $FE0000 .. $FE1FFF:
      ; // WD
    $FE2000 .. $FE3FFF:
      m68000_0.irq[1] := CLEAR_LINE;
    $FE8000 .. $FE9FFF:
      begin
        soundlatch := valor shr 8;
        m6502_0.change_nmi(ASSERT_LINE);
        sound_pending := true;
      end;
    $FEC000 .. $FEDFFF:
      begin
        playfield_tile_bank := valor and 1;
        fillchar(gfx[0].buffer, $800, 1);
      end;
    $FEE000 .. $FEFFFF:
      write_eeprom := true;
    $FFC000 .. $FFC3FF:
      if buffer_paleta[(direccion and $3FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $3FF) shr 1] := valor;
        change_color((direccion and $3FF) shr 1);
      end;
    $FFE000 .. $FFEFFF:
      if ram[(direccion and $FFF) shr 1] <> valor then
      begin
        ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $FFF000 .. $FFFFFF:
      ram[(direccion and $1FFF) shr 1] := valor;
  end;
end;

function badlands_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $FFFF:
      badlands_snd_getbyte := mem_snd[direccion];
    $2000 .. $27FF:
      if (direccion and $1) <> 0 then
        badlands_snd_getbyte := ym2151_0.status;
    $2800 .. $29FF:
      case (direccion and 6) of
        2:
          begin
            badlands_snd_getbyte := soundlatch;
            sound_pending := false;
            m6502_0.change_nmi(CLEAR_LINE);
          end;
        4:
          badlands_snd_getbyte := marcade.in0 or $10 or ($20 * byte(main_pending)) or ($40 * (byte(not(sound_pending))));
        6:
          m6502_0.change_irq(CLEAR_LINE);
      end;
    $3000 .. $3FFF:
      badlands_snd_getbyte := sound_rom[sound_bank, direccion and $FFF];
  end;
end;

procedure badlands_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      mem_snd[direccion] := valor;
    $2000 .. $27FF:
      case (direccion and 1) of
        0:
          ym2151_0.reg(valor);
        1:
          ym2151_0.write(valor);
      end;
    $2800 .. $29FF:
      if (direccion and 6) = 6 then
        m6502_0.change_irq(CLEAR_LINE);
    $2A00 .. $2BFF:
      case (direccion and 6) of
        2:
          begin
            mainlatch := valor;
            m68000_0.irq[2] := ASSERT_LINE;
            main_pending := true;
          end;
        4:
          begin
            sound_bank := (valor shr 6) and 3;
            if (valor and 1) = 0 then
              ym2151_0.reset;
          end;
      end;
    $3000 .. $FFFF:
      ; // ROM
  end;
end;

procedure badlands_sound_update;
begin
  ym2151_0.update;
end;

// Main
procedure reset_badlands;
begin
  m68000_0.reset;
  m6502_0.reset;
  ym2151_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := $FFBF;
  marcade.in2 := 0;
 reset_analog;
  write_eeprom := false;
  sound_pending := false;
  main_pending := false;
  soundlatch := 0;
  mainlatch := 0;
  playfield_tile_bank := 0;
  sound_bank := 0;
  pedal1 := $80;
  pedal2 := $80;
end;

procedure close_badlands;
var
  nombre: string;
begin
  nombre := 'badlands.nv';
  write_file(dm.tConfignvram.AsString + nombre, @eeprom_ram, $1000);
end;

function start_badlands: boolean;
var
  memory_temp: array [0 .. $5FFFF] of byte;
  f: dword;
  longitud: integer;
const
  pc_x: array [0 .. 15] of dword = (0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60);
  pc_y: array [0 .. 7] of dword = (0 * 8, 4 * 8, 8 * 8, 12 * 8, 16 * 8, 20 * 8, 24 * 8, 28 * 8);
  ps_y: array [0 .. 7] of dword = (0 * 8, 8 * 8, 16 * 8, 24 * 8, 32 * 8, 40 * 8, 48 * 8, 56 * 8);
begin
  machine_calls.general_loop := badlands_loop;
  machine_calls.reset := reset_badlands;
  machine_calls.close := close_badlands;
  machine_calls.fps_max := 59.922743;
  start_badlands := false;
  start_audio(true);
  // Chars
  screen_init(1, 512, 256, false);
  screen_init(2, 512, 256, true);
  // Final
  screen_init(3, 512, 256, false, true);
  start_video(336, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(14318180 div 2, 262, TCPU_68000);
  m68000_0.change_ram16_calls(badlands_getword, badlands_putword);
  // Sound CPU
  m6502_0 := cpu_m6502.create(14318180 div 8, 262, TCPU_M6502);
  m6502_0.change_ram_calls(badlands_snd_getbyte, badlands_snd_putbyte);
  m6502_0.init_sound(badlands_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(14318180 div 4);
  // cargar roms
  if not(roms_load16w(@rom, badlands_rom)) then
    exit;
  // cargar sonido
  if not(roms_load(@memory_temp, badlands_sound)) then
    exit;
  copymemory(@mem_snd[$4000], @memory_temp[$4000], $C000);
  for f := 0 to 3 do
    copymemory(@sound_rom[f, 0], @memory_temp[f * $1000], $1000);
  // convertir gfx
  if not(roms_load(@memory_temp, badlands_back)) then
    exit;
  for f := 0 to $5FFFF do
    memory_temp[f] := not(memory_temp[f]);
  init_gfx(0, 8, 8, $3000);
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // eeprom
  if read_file_size(dm.tConfignvram.AsString + 'badlands.nv', longitud) then
    read_file(dm.tConfignvram.AsString + 'badlands.nv', @eeprom_ram, longitud)
  else
    fillchar(eeprom_ram[0], $1000, $FF);
  // atari mo
  if not(roms_load(@memory_temp, badlands_mo)) then
    exit;
  for f := 0 to $2FFFF do
    memory_temp[f] := not(memory_temp[f]);
  init_gfx(1, 16, 8, $C00);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @pc_x, @ps_y, false, false);
  atari_mo_0 := tatari_mo.create(nil, @ram[$1000 shr 1], badlands_mo_config, 3, 336 + 8, 240 + 8);
  // Init Analog
  init_analog(m68000_0.numero_cpu, m68000_0.clock);
  analog_0(50, 10, $0, $FF, $0, false, true, true, true);
  // final
  reset_badlands;
  start_badlands := true;
end;

end.
