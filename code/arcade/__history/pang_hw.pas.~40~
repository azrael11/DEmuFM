unit pang_hw;

interface

uses
  WinApi.Windows,
  nz80,
  kabuki_decript,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  oki6295,
  sound_engine,
  eeprom;

function start_pang: boolean;

implementation

const
  // Pang
  pang_rom: array [0 .. 1] of tipo_roms = ((n: 'pang6.bin'; l: $8000; p: 0; crc: $68BE52CD),
    (n: 'pang7.bin'; l: $20000; p: $10000; crc: $4A2E70F6));
  pang_oki: tipo_roms = (n: 'bb1.bin'; l: $20000; p: 0; crc: $C52E5B8E);
  pang_sprites: array [0 .. 1] of tipo_roms = ((n: 'bb10.bin'; l: $20000; p: 0; crc: $FDBA4F6E),
    (n: 'bb9.bin'; l: $20000; p: $20000; crc: $39F47A63));
  pang_char: array [0 .. 3] of tipo_roms = ((n: 'pang_09.bin'; l: $20000; p: 0; crc: $3A5883F5),
    (n: 'bb3.bin'; l: $20000; p: $20000; crc: $79A8ED08), (n: 'pang_11.bin'; l: $20000; p: $80000;
    crc: $166A16AE), (n: 'bb5.bin'; l: $20000; p: $A0000; crc: $2FB3DB6C));
  // Super Pang
  spang_rom: array [0 .. 2] of tipo_roms = ((n: 'spe_06.rom'; l: $8000; p: 0; crc: $1AF106FB),
    (n: 'spe_07.rom'; l: $20000; p: $10000; crc: $208B5F54), (n: 'spe_08.rom'; l: $20000; p: $30000;
    crc: $2BC03ADE));
  spang_oki: tipo_roms = (n: 'spe_01.rom'; l: $20000; p: 0; crc: $2D19C133);
  spang_sprites: array [0 .. 1] of tipo_roms = ((n: 'spj10_2k.bin'; l: $20000; p: 0;
    crc: $EEDD0ADE), (n: 'spj09_1k.bin'; l: $20000; p: $20000; crc: $04B41B75));
  spang_char: array [0 .. 3] of tipo_roms = ((n: 'spe_02.rom'; l: $20000; p: 0; crc: $63C9DFD2),
    (n: '03.f2'; l: $20000; p: $20000; crc: $3AE28BC1), (n: 'spe_04.rom'; l: $20000; p: $80000;
    crc: $9D7B225B), (n: '05.g2'; l: $20000; p: $A0000; crc: $4A060884));
  spang_eeprom: tipo_roms = (n: 'eeprom-spang.bin'; l: $80; p: 0; crc: $DEAE1291);

var
  mem_rom_op, mem_rom_dat: array [0 .. $F, 0 .. $3FFF] of byte;
  mem_dat: array [0 .. $7FFF] of byte;
  rom_nbank, video_bank: byte;
  obj_ram: array [0 .. $FFF] of byte;
  vblank, irq_source: byte;
  pal_bank: word;

procedure update_video_pang;
var
  x, y, f, color, nchar: word;
  atrib: byte;
begin
  fill_full_screen(2, 0);
  for f := $0 to $7FF do
  begin
    atrib := memory[$C800 + f];
    color := atrib and $7F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f and $3F;
      y := f shr 6;
      nchar := (memory[$D000 + (f * 2)] + (memory[$D001 + (f * 2)] shl 8)) and $7FFF;
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 1, 0, (atrib and $80) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  actualiza_trozo(0, 0, 512, 256, 1, 0, 0, 512, 256, 2);
  for f := $7D downto 0 do
  begin
    atrib := obj_ram[(f * $20) + 1];
    nchar := obj_ram[f * $20] + ((atrib and $E0) shl 3);
    color := (atrib and $F) shl 4;
    x := obj_ram[(f * $20) + 3] + ((atrib and $10) shl 4);
    y := ((obj_ram[(f * $20) + 2] + 8) and $FF) - 8;
    put_gfx_sprite(nchar, color, false, false, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  actualiza_trozo_final(64, 8, 384, 240, 2);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_pang;
begin
  if event.arcade then
  begin
    // IN1
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // IN2
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
    // IN0
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure pang_loop;
var
  frame_m: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        case f of
          $EF:
            begin
              z80_0.change_irq(HOLD_LINE);
              irq_source := 1;
            end;
          $F7:
            vblank := 8;
          $FF:
            begin
              z80_0.change_irq(HOLD_LINE);
              vblank := 0;
              irq_source := 0;
            end;
        end;
      end;
      update_video_pang;
      events_pang;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function pang_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF:
      if z80_0.opcode then
        pang_getbyte := memory[direccion]
      else
        pang_getbyte := mem_dat[direccion];
    $8000 .. $BFFF:
      if z80_0.opcode then
        pang_getbyte := mem_rom_op[rom_nbank, direccion and $3FFF]
      else
        pang_getbyte := mem_rom_dat[rom_nbank, direccion and $3FFF];
    $C000 .. $C7FF:
      pang_getbyte := buffer_paleta[(direccion and $7FF) + pal_bank];
    $D000 .. $DFFF:
      if (video_bank <> 0) then
        pang_getbyte := obj_ram[direccion and $FFF]
      else
        pang_getbyte := memory[direccion];
    $C800 .. $CFFF, $E000 .. $FFFF:
      pang_getbyte := memory[direccion];
  end;
end;

procedure pang_putbyte(direccion: word; valor: byte);
  procedure change_color(pos: word); inline;
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[$1 + pos];
    color.r := pal4bit(tmp_color);
    tmp_color := buffer_paleta[pos];
    color.g := pal4bit(tmp_color shr 4);
    color.b := pal4bit(tmp_color);
    set_pal_color(color, pos shr 1);
    buffer_color[(pos shr 5) and $7F] := true;
  end;

begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $C7FF:
      if buffer_paleta[(direccion and $7FF) + pal_bank] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) + pal_bank] := valor;
        change_color((direccion and $7FE) + pal_bank);
      end;
    $C800 .. $CFFF:
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $D000 .. $DFFF:
      if (video_bank <> 0) then
        obj_ram[direccion and $FFF] := valor
      else
      begin
        memory[direccion] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $E000 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function pang_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      pang_inbyte := marcade.in0;
    1:
      pang_inbyte := marcade.in1;
    2:
      pang_inbyte := marcade.in2;
    5:
      pang_inbyte := (eeprom_0.readbit shl 7) or vblank or 2 or irq_source;
  end;
end;

procedure pang_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      begin
        main_screen.flip_main_screen := (valor and $4) <> 0;
        pal_bank := (valor and $20) shl 6;
      end;
    $2:
      rom_nbank := valor and $F;
    $5:
      oki_6295_0.write(valor);
    $7:
      video_bank := valor;
    $8:
      if valor <> 0 then
        eeprom_0.set_cs_line(CLEAR_LINE)
      else
        eeprom_0.set_cs_line(ASSERT_LINE); // eeprom_cs_w
    $10:
      if (valor <> 0) then
        eeprom_0.set_clock_line(CLEAR_LINE)
      else
        eeprom_0.set_clock_line(ASSERT_LINE); // eeprom_clock_w
    $18:
      eeprom_0.write_bit(valor); // eeprom_serial_w
  end;
end;

procedure pang_sound_update;
begin
  oki_6295_0.update;
end;

// Main
procedure reset_pang;
begin
  z80_0.reset;
  reset_audio;
  oki_6295_0.reset;
  eeprom_0.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  rom_nbank := 0;
  video_bank := 0;
  pal_bank := 0;
  vblank := 0;
  irq_source := 0;
end;

function start_pang: boolean;
var
  f: byte;
  memory_temp: array [0 .. $4FFFF] of byte;
  ptemp, mem_temp2, mem_temp3, mem_temp4: pbyte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1,
    32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16,
    8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);

  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $8000);
    gfx[0].trans[15] := true;
    gfx_set_desc_data(4, 0, 16 * 8, $8000 * 16 * 8 + 4, $8000 * 16 * 8 + 0, 4, 0);
    convert_gfx(0, 0, ptemp, @ps_x[0], @ps_y[0], false, false);
  end;

  procedure convert_sprites;
  begin
    init_gfx(1, 16, 16, $800);
    gfx[1].trans[15] := true;
    gfx_set_desc_data(4, 0, 64 * 8, $800 * 64 * 8 + 4, $800 * 64 * 8 + 0, 4, 0);
    convert_gfx(1, 0, ptemp, @ps_x[0], @ps_y[0], false, false);
  end;

begin
  start_pang := false;
  machine_calls.general_loop := pang_loop;
  machine_calls.reset := reset_pang;
  machine_calls.fps_max := 57.42;
  start_audio(false);
  // Pantallas
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, false, true);
  start_video(384, 240);
  // Main CPU
  z80_0 := cpu_z80.create(8000000, 256);
  z80_0.change_ram_calls(pang_getbyte, pang_putbyte);
  z80_0.change_io_calls(pang_inbyte, pang_outbyte);
  z80_0.init_sound(pang_sound_update);
  // eeprom
  eeprom_0 := eeprom_class.create(6, 16, '0110', '0101', '0111');
  // Sound Chips
  // YM2413  --> Falta!
  oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH, 2);
  getmem(ptemp, $100000);
  getmem(mem_temp2, $50000);
  getmem(mem_temp3, $50000);
  case main_vars.machine_type of
    119:
      begin // Pang
        if not(roms_load(oki_6295_0.get_rom_addr, pang_oki)) then
          exit;
        // Cargar roms, desencriptar y poner en su sitio las ROMS
        if not(roms_load(@memory_temp, pang_rom)) then
          exit;
        kabuki_mitchell_decode(@memory_temp[0], mem_temp2, mem_temp3, 8, $01234567, $76543210,
          $6548, $24);
        copymemory(@memory[0], mem_temp2, $8000);
        copymemory(@mem_dat[0], mem_temp3, $8000);
        for f := 0 to 7 do
          copymemory(@mem_rom_op[f, 0], @mem_temp2[$10000 + (f * $4000)], $4000);
        for f := 0 to 7 do
          copymemory(@mem_rom_dat[f, 0], @mem_temp3[$10000 + (f * $4000)], $4000);
        // convertir chars
        fillchar(ptemp^, $100000, $FF);
        if not(roms_load(ptemp, pang_char)) then
          exit;
        convert_chars;
        // convertir sprites
        if not(roms_load(ptemp, pang_sprites)) then
          exit;
        convert_sprites;
      end;
    183:
      begin // Super Pang
        if not(roms_load(oki_6295_0.get_rom_addr, spang_oki)) then
          exit;
        // Cargar roms, desencriptar y poner en su sitio las ROMS
        if not(roms_load(@memory_temp, spang_rom)) then
          exit;
        kabuki_mitchell_decode(@memory_temp[0], mem_temp2, mem_temp3, $10, $45670123, $45670123,
          $5852, $43);
        copymemory(@memory[0], mem_temp2, $8000);
        copymemory(@mem_dat[0], mem_temp3, $8000);
        for f := 0 to $F do
          copymemory(@mem_rom_op[f, 0], @mem_temp2[$10000 + (f * $4000)], $4000);
        for f := 0 to $F do
          copymemory(@mem_rom_dat[f, 0], @mem_temp3[$10000 + (f * $4000)], $4000);
        // convertir chars
        fillchar(ptemp^, $100000, $FF);
        if not(roms_load(ptemp, spang_char)) then
          exit;
        convert_chars;
        // convertir sprites
        if not(roms_load(ptemp, spang_sprites)) then
          exit;
        convert_sprites;
        // load eeprom si no lo esta ya...
        mem_temp4 := eeprom_0.get_rom_addr;
        inc(mem_temp4);
        if mem_temp4^ <> 0 then
          if not(roms_load(eeprom_0.get_rom_addr, spang_eeprom)) then
            exit;
      end;
  end;
  freemem(mem_temp3);
  freemem(mem_temp2);
  freemem(ptemp);
  // final
  reset_pang;
  start_pang := true;
end;

end.
