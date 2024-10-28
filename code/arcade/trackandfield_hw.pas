unit trackandfield_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  vlm_5030,
  gfx_engine,
  dac,
  rom_engine,
  pal_engine,
  konami_decrypt,
  sound_engine,
  qsnapshot,
  file_engine;

function start_tracknfield: boolean;

implementation

const
  trackfield_rom: array [0 .. 4] of tipo_roms = ((n: 'a01_e01.bin'; l: $2000; p: $6000; crc: $2882F6D4), (n: 'a02_e02.bin'; l: $2000; p: $8000; crc: $1743B5EE), (n: 'a03_k03.bin'; l: $2000; p: $A000; crc: $6C0D1EE9), (n: 'a04_e04.bin'; l: $2000; p: $C000; crc: $21D6C448),
    (n: 'a05_e05.bin'; l: $2000; p: $E000; crc: $F08C7B7E));
  trackfield_char: array [0 .. 2] of tipo_roms = ((n: 'h16_e12.bin'; l: $2000; p: 0; crc: $50075768), (n: 'h15_e11.bin'; l: $2000; p: $2000; crc: $DDA9E29F), (n: 'h14_e10.bin'; l: $2000; p: $4000; crc: $C2166A5C));
  trackfield_sprites: array [0 .. 3] of tipo_roms = ((n: 'c11_d06.bin'; l: $2000; p: 0; crc: $82E2185A), (n: 'c12_d07.bin'; l: $2000; p: $2000; crc: $800FF1F1), (n: 'c13_d08.bin'; l: $2000; p: $4000; crc: $D9FAF183), (n: 'c14_d09.bin'; l: $2000; p: $6000; crc: $5886C802));
  trackfield_pal: array [0 .. 2] of tipo_roms = ((n: '361b16.f1'; l: $20; p: 0; crc: $D55F30B5), (n: '361b17.b16'; l: $100; p: $20; crc: $D2BA4D32), (n: '361b18.e15'; l: $100; p: $120; crc: $053E5861));
  trackfield_vlm: tipo_roms = (n: 'c9_d15.bin'; l: $2000; p: 0; crc: $F546A56B);
  trackfield_snd: tipo_roms = (n: 'c2_d13.bin'; l: $2000; p: 0; crc: $95BF79B6);
  trackfield_dip_a: array [0 .. 1] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), ());
  trackfield_dip_b: array [0 .. 7] of def_dip2 = ((mask: $1; name: 'Lives'; number: 2; val2: (1, 0); name2: ('1', '2')), (mask: $2; name: 'After Last Event'; number: 2; val2: (2, 0); name2: ('Game Over', 'Game Continues')), (mask: $4; name: 'Cabinet'; number: 2; val2: (0, 4);
    name2: ('Upright', 'Cocktail')), (mask: $8; name: 'Bonus Life'; number: 2; val2: (8, 0); name2: ('None', '100K')), (mask: $10; name: 'World Records'; number: 2; val2: ($10, 0); name2: ('Don''t Erase', 'Erase on Reset')), (mask: $60; name: 'Difficulty'; number: 4;
    val4: ($60, $40, $20, 0); name4: ('Easy', 'Normal', 'Hard', 'Difficult')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  irq_ena: boolean;
  lst_snd_irq, sound_latch, chip_latch: byte;
  mem_opcodes: array [0 .. $9FFF] of byte;
  last_addr: word;
  scroll_x: array [0 .. $1F] of word;

procedure update_video_trackfield;
var
  x, y, atrib: byte;
  f, nchar, color: word;
begin
  for f := 0 to $7FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$3800 + f];
      nchar := memory[$3000 + f] + ((atrib and $C0) shl 2);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $10) <> 0, (atrib and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x_part2(1, 2, 8, @scroll_x);
  for f := $1F downto 0 do
  begin
    atrib := memory[$1800 + (f * 2)];
    nchar := memory[$1C01 + (f * 2)] + ((atrib and $20) shl 3);
    y := 241 - memory[$1801 + (f * 2)];
    x := memory[$1C00 + (f * 2)] - 1;
    color := (atrib and $F) shl 4;
    put_gfx_sprite(nchar, color, (atrib and $40) = 0, (atrib and $80) <> 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(0, 16, 256, 224, 2);
end;

procedure events_trackfield;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure trackfield_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 240 then
        begin
          if irq_ena then
            m6809_0.change_irq(HOLD_LINE);
          update_video_trackfield;
        end;
        // main
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
      end;
      // General
      events_trackfield;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function trackfield_getbyte(direccion: word): byte;
begin
  case direccion of
    $1200 .. $127F:
      trackfield_getbyte := marcade.dswb;
    $1280 .. $128F:
      case (direccion and 3) of
        0:
          trackfield_getbyte := marcade.in2;
        1:
          trackfield_getbyte := marcade.in0;
        2:
          trackfield_getbyte := marcade.in1;
        3:
          trackfield_getbyte := marcade.dswa;
      end;
    $1800 .. $1FFF, $2800 .. $3FFF:
      trackfield_getbyte := memory[direccion];
    $6000 .. $FFFF:
      if m6809_0.opcode then
        trackfield_getbyte := mem_opcodes[direccion - $6000]
      else
        trackfield_getbyte := memory[direccion];
  end;
end;

procedure trackfield_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $1080 .. $10FF:
      case (direccion and 7) of
        0:
          main_screen.flip_main_screen := (valor and $1) <> 0;
        1:
          begin
            if ((lst_snd_irq = 0) and (valor <> 0)) then
              z80_0.change_irq(HOLD_LINE);
            lst_snd_irq := valor;
          end;
        7:
          irq_ena := (valor <> 0);
      end;
    $1100 .. $117F:
      sound_latch := valor;
    $1800 .. $183F, $1860 .. $1C3F, $1C60 .. $1FFF, $2800 .. $2FFF:
      memory[direccion] := valor;
    $1840 .. $185F, $1C40 .. $1C5F:
      begin
        memory[direccion] := valor;
        direccion := direccion and $1F;
        scroll_x[direccion] := memory[$1840 + direccion] + ((memory[$1C40 + direccion] and 1) shl 8);
      end;
    $3000 .. $3FFF:
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $6000 .. $FFFF:
      ; // ROM
  end;
end;

function trackfield_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      trackfield_snd_getbyte := mem_snd[direccion];
    $4000 .. $5FFF:
      trackfield_snd_getbyte := mem_snd[(direccion and $3FF) + $4000];
    $6000 .. $7FFF:
      trackfield_snd_getbyte := sound_latch;
    $8000 .. $9FFF:
      trackfield_snd_getbyte := (z80_0.totalt shr 10) and $F;
    $E000 .. $FFFF:
      if ((direccion and 7) = 2) then
        trackfield_snd_getbyte := vlm5030_0.get_bsy shl 4;
  end;
end;

procedure trackfield_snd_putbyte(direccion: word; valor: byte);
var
  changes, offset: word;
begin
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $4000 .. $4FFF:
      mem_snd[$4000 + (direccion and $3FF)] := valor;
    $A000 .. $BFFF:
      chip_latch := valor;
    $C000 .. $DFFF:
      sn_76496_0.Write(chip_latch);
    $E000 .. $FFFF:
      case (direccion and 7) of
        0:
          dac_0.data8_w(valor);
        1, 2:
          ;
        3:
          begin
            offset := direccion and $3FF;
            changes := offset xor last_addr;
            // A4 VLM5030 ST pin */
            if (changes and $100) <> 0 then
              vlm5030_0.set_st((offset and $100) shr 8);
            // A5 VLM5030 RST pin */
            if (changes and $200) <> 0 then
              vlm5030_0.set_rst((offset and $200) shr 9);
            last_addr := offset;
          end;
        4:
          vlm5030_0.data_w(valor);
      end;
  end;
end;

procedure trackfield_sound_update;
begin
  sn_76496_0.Update;
  dac_0.Update;
  vlm5030_0.Update;
end;

procedure trackfield_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
  size: word;
begin
  open_qsnapshot_save('trackfield' + nombre);
  getmem(data, 250);
  // CPU
  size := m6809_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := sn_76496_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := vlm5030_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := dac_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory, $6000);
  savedata_qsnapshot(@mem_snd[$2000], $E000);
  // MISC
  buffer[0] := byte(irq_ena);
  buffer[1] := sound_latch;
  buffer[2] := chip_latch;
  buffer[3] := last_addr and $FF;
  buffer[4] := last_addr shr 8;
  savedata_qsnapshot(@buffer, 5);
  freemem(data);
  close_qsnapshot;
end;

procedure trackfield_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
begin
  if not(open_qsnapshot_load('trackfield' + nombre)) then
    exit;
  getmem(data, 250);
  // CPU
  loaddata_qsnapshot(data);
  m6809_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  sn_76496_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  vlm5030_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  dac_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory);
  loaddata_qsnapshot(@mem_snd[$2000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  irq_ena := buffer[0] <> 0;
  sound_latch := buffer[1];
  chip_latch := buffer[2];
  last_addr := buffer[3] or (buffer[4] shl 8);
  freemem(data);
  close_qsnapshot;
  // end
  fillchar(gfx[0].buffer, $800, 1);
end;

// Main
procedure close_trackfield;
begin
  write_file(Directory.Arcade_nvram + 'trackfield.nv', @memory[$2800], $800);
end;

procedure reset_trackfield;
begin
  m6809_0.reset;
  z80_0.reset;
  vlm5030_0.reset;
  dac_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_ena := false;
  sound_latch := 0;
  chip_latch := 0;
  last_addr := 0;
  lst_snd_irq := 0;
end;

function start_tracknfield: boolean;
var
  colores: tpaleta;
  f: word;
  longitud: integer;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $BFFF] of byte;
  rweights, gweights: array [0 .. 3] of single;
  bweights: array [0 .. 2] of single;
const
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
begin
  start_tracknfield := false;
  machine_calls.general_loop := trackfield_loop;
  machine_calls.reset := reset_trackfield;
  machine_calls.close := close_trackfield;
  machine_calls.save_qsnap := trackfield_qsave;
  machine_calls.load_qsnap := trackfield_qload;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, $100, TCPU_M6809);
  m6809_0.change_ram_calls(trackfield_getbyte, trackfield_putbyte);
  if not(roms_load(@memory, trackfield_rom)) then
    exit;
  konami1_decode(@memory[$6000], @mem_opcodes, $A000);
  // Sound CPU
  z80_0 := cpu_z80.Create(14318180 div 4, $100);
  z80_0.change_ram_calls(trackfield_snd_getbyte, trackfield_snd_putbyte);
  z80_0.init_sound(trackfield_sound_update);
  if not(roms_load(@mem_snd, trackfield_snd)) then
    exit;
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(14318180 div 8);
  vlm5030_0 := vlm5030_chip.Create(3579545, $2000, 4);
  if not(roms_load(vlm5030_0.get_rom_addr, trackfield_vlm)) then
    exit;
  dac_0 := dac_chip.Create(0.80);
  // NV ram
  if read_file_size(Directory.Arcade_nvram + 'trackfield.nv', longitud) then
    read_file(Directory.Arcade_nvram + 'trackfield.nv', @memory[$2800], longitud);
  // convertir chars
  if not(roms_load(@memory_temp, trackfield_char)) then
    exit;
  init_gfx(0, 8, 8, $300, $3FF);
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, trackfield_sprites)) then
    exit;
  init_gfx(1, 16, 16, $100);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, ($100 * 64 * 8) + 4, $100 * 64 * 8, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // paleta
  if not(roms_load(@memory_temp, trackfield_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rweights, 1000, 0, 3, @resistances_rg, @gweights, 1000, 0, 2, @resistances_b, @bweights, 1000, 0);
  for f := 0 to $1F do
  begin
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$120 + f] and $F) or $10;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
  end;
  // DIP
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $59;
  marcade.dswa_val2 := @trackfield_dip_a;
  marcade.dswb_val2 := @trackfield_dip_b;
  // final
  reset_trackfield;
  start_tracknfield := true;
end;

end.
