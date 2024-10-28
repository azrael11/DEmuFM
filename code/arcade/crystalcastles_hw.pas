unit crystalcastles_hw;

interface

uses
  WinApi.Windows,
  m6502,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  pokey,
  file_engine;

function start_crystalcastles: boolean;

implementation

const
  ccastles_rom: array [0 .. 4] of tipo_roms = ((n: '136022-403.1k'; l: $2000; p: $2000; crc: $81471AE5), (n: '136022-404.1l'; l: $2000; p: $0; crc: $820DAF29), (n: '136022-405.1n'; l: $2000; p: $4000; crc: $4BEFC296), (n: '136022-102.1h'; l: $2000; p: $8000; crc: $F6CCFBD4),
    (n: '136022-101.1f'; l: $2000; p: $6000; crc: $E2E17236));
  ccastles_sprites: array [0 .. 1] of tipo_roms = ((n: '136022-106.8d'; l: $2000; p: $0; crc: $9D1D89FC), (n: '136022-107.8b'; l: $2000; p: $2000; crc: $39960B7D));
  ccastles_pal: array [0 .. 3] of tipo_roms = ((n: '82s129-136022-108.7k'; l: $100; p: $0; crc: $6ED31E3B), (n: '82s129-136022-109.6l'; l: $100; p: $100; crc: $B3515F1A), (n: '82s129-136022-110.11l'; l: $100; p: $200; crc: $068BDC7E), (n: '82s129-136022-111.10k'; l: $100;
    p: $300; crc: $C29C18D9));

var
  rom_bank: array [0 .. 1, 0 .. $3FFF] of byte;
  hscroll, vscroll, num_bank: byte;
  outlatch, bitmode_addr: array [0 .. 1] of byte;
  video_ram: array [0 .. $7FFF] of byte;
  syncprom, wpprom, priprom: array [0 .. $FF] of byte;
  weights_r, weights_g, weights_b: array [0 .. 2] of single;

procedure update_video_ccastles;
var
  effy: integer;
  y, f, effx, mopix, pix, prindex, prvalue, flip, nchar, color: byte;
  x, pos_videoram: word;
  screen_data: array [0 .. 255, 0 .. 319] of word;
  screen_sprites: array [0 .. 255, 0 .. 319] of byte;

  procedure put_sprite_cc(nchar: dword; color: word; flip: boolean; sx, sy: byte);
  var
    x, y, pos_y, pos_x, pos_x_temp: byte;
    pos: pbyte;
    dir: integer;
  begin
    pos := gfx[0].datos;
    inc(pos, nchar * 8 * 16);
    if flip then
    begin
      pos_y := 15;
      dir := -1;
      pos_x := 7;
    end
    else
    begin
      pos_y := 0;
      dir := 1;
      pos_x := 0;
    end;
    for y := 0 to 15 do
    begin
      pos_x_temp := pos_x;
      for x := 0 to 7 do
      begin
        if not(gfx[0].trans[pos^]) then
          screen_sprites[sx + pos_x_temp, sy + pos_y] := pos^ + color;
        pos_x_temp := pos_x_temp + dir;
        inc(pos);
      end;
      pos_y := pos_y + dir;
    end;
  end;

begin
  if (outlatch[1] and $10) <> 0 then
    flip := $FF
  else
    flip := 0;
  pos_videoram := ((outlatch[1] and $80) shl 1) or $8E00;
  fillchar(screen_sprites, $14000, $FF);
  for f := 0 to $27 do
  begin
    x := memory[pos_videoram + 3 + (f * 4)];
    y := (240 - memory[pos_videoram + 1 + (f * 4)]) and $FF;
    nchar := memory[pos_videoram + (f * 4)];
    color := memory[pos_videoram + 2 + (f * 4)] shr 7;
    put_sprite_cc(nchar, color shl 3, flip <> 0, x, y);
  end;
  for y := 0 to 255 do
  begin
    // if we're in the VBLANK region, just fill with black
    if (syncprom[y] and 1) <> 0 then
    begin
      for x := 0 to 319 do
        screen_data[y, x] := paleta[$400];
      // non-VBLANK region: merge the sprites and the bitmap
    end
    else
    begin
      // the "POTATO" chip does some magic here; this is just a guess
      if (flip <> 0) then
        effy := (((y - 24) + 0) xor flip) and $FF
      else
        effy := (((y - 24) + vscroll) xor flip) and $FF;
      if (effy < 24) then
        effy := 24;
      pos_videoram := effy * 128;
      // loop over X
      for x := 0 to 319 do
      begin
        // if we're in the HBLANK region, just store black
        if (x >= 256) then
        begin
          screen_data[y, x] := paleta[$400]
          // otherwise, process normally
        end
        else
        begin
          effx := (hscroll + (x xor flip)) and $FF;
          // low 4 bits = left pixel, high 4 bits = right pixel
          pix := (video_ram[pos_videoram + (effx div 2)] shr ((effx and 1) * 4)) and $0F;
          mopix := screen_sprites[x, y];
          { Inputs to the priority PROM:
            Bit 7 = GND
            Bit 6 = /CRAM
            Bit 5 = BA4
            Bit 4 = MV2
            Bit 3 = MV1
            Bit 2 = MV0
            Bit 1 = MPI
            Bit 0 = BIT3 }
          prindex := $40 or ((mopix and 7) shl 2) or ((mopix and 8) shr 2) or ((pix and 8) shr 3);
          prvalue := priprom[prindex];
          // Bit 1 of prvalue selects the low 4 bits of the final pixel
          if (prvalue and 2) <> 0 then
            pix := mopix;
          // Bit 0 of prvalue selects bit 4 of the final color
          pix := pix or ((prvalue and 1) shl 4);
          // store the pixel value and also a priority value based on the topmost bit
          screen_data[y, x] := paleta[pix];
        end;
      end;
    end;
  end;
  putpixel(0, 0, $14000, @screen_data, 1);
  update_region(0, 24, 256, 232, 1, 0, 0, 256, 232, PANT_TEMP);
end;

procedure events_ccastles;
begin
  if main_vars.service1 then
    marcade.in0 := marcade.in0 and $EF
  else
    marcade.in0 := marcade.in0 or $10;
  if event.arcade then
  begin
    // in0
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $BF
    else
      marcade.in0 := marcade.in0 or $40;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // in1
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
  end;
end;

procedure ccastles_loop;
var
  f: byte;
  frame: single;
begin
  init_controls(false, false, false, true);
  frame := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      if EmulationPaused = false then
      begin
        for f := 0 to 255 do
          case f of
            0:
              begin
                marcade.in0 := marcade.in0 or $20;
                m6502_0.change_irq(ASSERT_LINE);
              end;
            24:
              marcade.in0 := marcade.in0 and $DF;
            64, 128, 192:
              m6502_0.change_irq(ASSERT_LINE);
          end;
        m6502_0.run(frame);
        frame := frame + m6502_0.tframes - m6502_0.contador;
      end;
      update_video_ccastles;
      events_ccastles;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure bitmode_autoinc;
begin
  // auto increment in the x-direction if it's enabled
  if (outlatch[1] and 1) = 0 then
  begin // /AX
    if (outlatch[1] and 4) = 0 then
      bitmode_addr[0] := bitmode_addr[0] + 1 // /XINC
    else
      bitmode_addr[0] := bitmode_addr[0] - 1;
  end;
  // auto increment in the y-direction if it's enabled
  if (outlatch[1] and 2) = 0 then
  begin // /AY
    if (outlatch[1] and 8) = 0 then
      bitmode_addr[1] := bitmode_addr[1] + 1 // /YINC
    else
      bitmode_addr[1] := bitmode_addr[1] - 1;
  end;
end;

function getbyte_ccastles(direccion: word): byte;
var
  res: byte;
  tempw: word;
begin
  case direccion of
    0, 1, 3 .. $7FFF:
      getbyte_ccastles := video_ram[direccion];
    2:
      begin
        // in bitmode, the address comes from the autoincrement latches
        tempw := (bitmode_addr[1] shl 7) or (bitmode_addr[0] shr 1);
        // the appropriate pixel is selected into the upper 4 bits
        res := video_ram[tempw] shl ((not(bitmode_addr[0]) and 1) * 4);
        // autoincrement because /BITMD was selected
        bitmode_autoinc;
        // the low 4 bits of the data lines are not driven so make them all 1's
        getbyte_ccastles := res or $0F;
      end;
    $8000 .. $8FFF, $E000 .. $FFFF:
      getbyte_ccastles := memory[direccion];
    $9000 .. $93FF:
      getbyte_ccastles := memory[(direccion and $FF) + $9000]; // nvram_r
    $9400 .. $95FF:
      case (direccion and 3) of
        0:
          getbyte_ccastles := analog.c[0].y[0];
        1:
          getbyte_ccastles := analog.c[0].x[0];
        2, 3:
          getbyte_ccastles := $FF;
      end;
    $9600 .. $97FF:
      getbyte_ccastles := marcade.in0; // in_0
    $9800 .. $99FF:
      getbyte_ccastles := pokey_0.read(direccion and $F);
    $9A00 .. $9BFF:
      getbyte_ccastles := pokey_1.read(direccion and $F);
    $A000 .. $DFFF:
      getbyte_ccastles := rom_bank[num_bank, direccion and $3FFF];
  end;
end;

procedure ccastles_write_vram(direccion: word; valor: byte; bitmd: boolean; pixba: byte);
var
  promaddr, wpbits: byte;
  dest: word;
begin
  dest := direccion and $7FFE;
  { Inputs to the write-protect PROM:
    Bit 7 = BA1520 = 0 if (BA15-BA12 != 0), or 1 otherwise
    Bit 6 = DRBA11
    Bit 5 = DRBA10
    Bit 4 = /BITMD
    Bit 3 = GND
    Bit 2 = BA0
    Bit 1 = PIXB
    Bit 0 = PIXA }
  promaddr := (byte((direccion and $F000) = 0) shl 7) or ((direccion and $0C00) shr 5) or (byte(not(bitmd)) shl 4) or ((direccion and $0001) shl 2) or (pixba shl 0);
  // look up the PROM result
  wpbits := wpprom[promaddr];
  // write to the appropriate parts of VRAM depending on the result
  if ((wpbits and 1) = 0) then
    video_ram[dest] := (video_ram[dest] and $F0) or (valor and $0F);
  if ((wpbits and 2) = 0) then
    video_ram[dest] := (video_ram[dest] and $0F) or (valor and $F0);
  if ((wpbits and 4) = 0) then
    video_ram[dest + 1] := (video_ram[dest + 1] and $F0) or (valor and $0F);
  if ((wpbits and 8) = 0) then
    video_ram[dest + 1] := (video_ram[dest + 1] and $0F) or (valor and $F0);
end;

procedure putbyte_ccastles(direccion: word; valor: byte);
var
  tempw: word;
  r, g, b, bit0, bit1, bit2: byte;
  color: tcolor;
begin
  case direccion of
    0 .. 1:
      begin
        ccastles_write_vram(direccion, valor, false, 0);
        bitmode_addr[direccion] := valor;
      end;
    2:
      begin
        tempw := (bitmode_addr[1] shl 7) or (bitmode_addr[0] shr 1);
        // the upper 4 bits of data are replicated to the lower 4 bits
        valor := (valor and $F0) or (valor shr 4);
        // write through the generic VRAM routine, passing the low 2 X bits as PIXB/PIXA
        ccastles_write_vram(tempw, valor, true, bitmode_addr[0] and 3);
        // autoincrement because /BITMD was selected
        bitmode_autoinc;
      end;
    3 .. $7FFF:
      ccastles_write_vram(direccion, valor, false, 0);
    $8000 .. $8FFF:
      memory[direccion] := valor;
    $9000 .. $93FF:
      memory[(direccion and $FF) + $9000] := valor; // nvram_w
    $9800 .. $99FF:
      pokey_0.write(direccion and $F, valor);
    $9A00 .. $9BFF:
      pokey_1.write(direccion and $F, valor);
    $9C00 .. $9C7F:
      ; // nvram_recall_w
    $9C80 .. $9CFF:
      hscroll := valor;
    $9D00 .. $9D7F:
      vscroll := valor;
    $9D80 .. $9DFF:
      m6502_0.change_irq(CLEAR_LINE);
    $9E00 .. $9E7F:
      ; // watchdog
    $9E80 .. $9EFF:
      case (direccion and 7) of
        0 .. 6:
          ;
        7:
          num_bank := valor and 1;
      end;
    $9F00 .. $9F7F:
      outlatch[1] := (outlatch[1] and not(1 shl (direccion and 7))) or (((valor shr 3) and 1) shl (direccion and 7));
    $9F80 .. $9FFF:
      begin
        // extract the raw RGB bits
        r := not(((valor and $C0) shr 6) or ((direccion and $20) shr 3));
        b := not((valor and $38) shr 3);
        g := not(valor and $07);
        // red component (inverted)
        bit0 := (r shr 0) and $01;
        bit1 := (r shr 1) and $01;
        bit2 := (r shr 2) and $01;
        color.r := combine_3_weights(@weights_r, bit0, bit1, bit2);
        // green component (inverted)
        bit0 := (g shr 0) and $01;
        bit1 := (g shr 1) and $01;
        bit2 := (g shr 2) and $01;
        color.g := combine_3_weights(@weights_g, bit0, bit1, bit2);
        // blue component (inverted)
        bit0 := (b shr 0) and $01;
        bit1 := (b shr 1) and $01;
        bit2 := (b shr 2) and $01;
        color.b := combine_3_weights(@weights_b, bit0, bit1, bit2);
        set_pal_color(color, direccion and $1F);
      end;
    $A000 .. $FFFF:
      ; // ROM
  end;
end;

function input_in1(pot: byte): byte;
begin
  input_in1 := marcade.in1;
end;

procedure ccastles_sound_update;
begin
  pokey_0.update;
  pokey_1.update;
end;

// Main
procedure reset_ccastles;
begin
  m6502_0.reset;
  pokey_0.reset;
  pokey_1.reset;
  reset_audio;
  num_bank := 0;
  bitmode_addr[0] := 0;
  bitmode_addr[1] := 0;
  outlatch[0] := 0;
  outlatch[1] := 0;
  hscroll := 0;
  vscroll := 0;
  marcade.in0 := $FF;
  marcade.in1 := $DF;
end;

procedure close_ccastles;
begin
  write_file(Directory.Arcade_nvram + 'ccastles.nv', @memory[$9000], $100);
end;

function start_crystalcastles: boolean;
var
  memory_temp: array [0 .. $FFFF] of byte;
  longitud: integer;
const
  ps_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  resistances: array [0 .. 2] of integer = (22000, 10000, 4700);
begin
  start_crystalcastles := false;
  machine_calls.general_loop := ccastles_loop;
  machine_calls.reset := reset_ccastles;
  machine_calls.fps_max := 61.035156;
  machine_calls.close := close_ccastles;
  start_audio(false);
  screen_init(1, 320, 256);
  start_video(256, 232);
  // Main CPU
  m6502_0 := cpu_m6502.create(10000000 div 8, 256, TCPU_M6502);
  m6502_0.change_ram_calls(getbyte_ccastles, putbyte_ccastles);
  m6502_0.init_sound(ccastles_sound_update);
  // analog
  init_analog(m6502_0.numero_cpu, m6502_0.clock);
  analog_0(10, -30, $7F, $FF, 0, false, true, false, true);
  // Sound Chip
  pokey_0 := pokey_chip.create(10000000 div 8);
  pokey_1 := pokey_chip.create(10000000 div 8);
  pokey_1.change_all_pot(input_in1);
  // cargar roms
  if not(roms_load(@memory_temp, ccastles_rom)) then
    exit;
  copymemory(@rom_bank[0, 0], @memory_temp[0], $4000);
  copymemory(@memory[$E000], @memory_temp[$4000], $2000);
  copymemory(@rom_bank[1, 0], @memory_temp[$6000], $4000);
  // Cargar sprites
  if not(roms_load(@memory_temp, ccastles_sprites)) then
    exit;
  init_gfx(0, 8, 16, $100);
  gfx[0].trans[7] := true;
  gfx_set_desc_data(3, 0, 32 * 8, 4, $2000 * 8, $2000 * 8 + 4);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, ccastles_pal)) then
    exit;
  copymemory(@syncprom, @memory_temp[0], $100);
  copymemory(@wpprom, @memory_temp[$200], $100);
  copymemory(@priprom, @memory_temp[$300], $100);
  compute_resistor_weights(0, 255, -1.0, 3, @resistances, @weights_r, 1000, 0, 3, @resistances, @weights_g, 1000, 0, 3, @resistances, @weights_b, 1000, 0);
  // cargar NVram
  if read_file_size(Directory.Arcade_nvram + 'ccastles.nv', longitud) then
    read_file(Directory.Arcade_nvram + 'ccastles.nv', @memory[$9000], longitud);
  // final
  reset_ccastles;
  start_crystalcastles := true;
end;

end.
