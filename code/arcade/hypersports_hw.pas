unit hypersports_hw;

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

function start_hypersports: boolean;

implementation

const
  hypersports_rom: array [0 .. 5] of tipo_roms = ((n: 'c01'; l: $2000; p: $4000; crc: $0C720EEB),
    (n: 'c02'; l: $2000; p: $6000; crc: $560258E0), (n: 'c03'; l: $2000; p: $8000; crc: $9B01C7E6),
    (n: 'c04'; l: $2000; p: $A000; crc: $10D7E9A2), (n: 'c05'; l: $2000; p: $C000; crc: $B105A8CD),
    (n: 'c06'; l: $2000; p: $E000; crc: $1A34A849));
  hypersports_char: array [0 .. 3] of tipo_roms = ((n: 'c26'; l: $2000; p: 0; crc: $A6897EAC),
    (n: 'c24'; l: $2000; p: $2000; crc: $5FB230C0), (n: 'c22'; l: $2000; p: $4000; crc: $ED9271A0),
    (n: 'c20'; l: $2000; p: $6000; crc: $183F4324));
  hypersports_sprites: array [0 .. 7] of tipo_roms = ((n: 'c14'; l: $2000; p: 0; crc: $C72D63BE),
    (n: 'c13'; l: $2000; p: $2000; crc: $76565608), (n: 'c12'; l: $2000; p: $4000; crc: $74D2CC69),
    (n: 'c11'; l: $2000; p: $6000; crc: $66CBCB4D), (n: 'c18'; l: $2000; p: $8000; crc: $ED25E669),
    (n: 'c17'; l: $2000; p: $A000; crc: $B145B39F), (n: 'c16'; l: $2000; p: $C000; crc: $D7FF9F2B),
    (n: 'c15'; l: $2000; p: $E000; crc: $F3D454E6));
  hypersports_pal: array [0 .. 2] of tipo_roms = ((n: 'c03_c27.bin'; l: $20; p: $0; crc: $BC8A5956),
    (n: 'j12_c28.bin'; l: $100; p: $20; crc: $2C891D59), (n: 'a09_c29.bin'; l: $100; p: $120;
    crc: $811A3F3F));
  hypersports_vlm: tipo_roms = (n: 'c08'; l: $2000; p: $0; crc: $E8F8EA78);
  hypersports_snd: array [0 .. 1] of tipo_roms = ((n: 'c10'; l: $2000; p: $0; crc: $3DC1A6FF),
    (n: 'c09'; l: $2000; p: $2000; crc: $9B525C3E));
  hypersports_dip_a: array [0 .. 1] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
    dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'),
    (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'),
    (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'),
    (dip_val: $0; dip_name: 'Free Play'))), ());
  hypersports_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: 'After Last Event'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Game Over'), (dip_val: $0; dip_name: 'Game Continues'), (), (),
    (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $2; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $8; name: 'World Records'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Don''t Erase'), (dip_val: $0; dip_name: 'Erase on Reset'), (),
    (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $F0; name: 'Difficulty';
    number: 16; dip: ((dip_val: $F0; dip_name: 'Easy 1'), (dip_val: $E0; dip_name: 'Easy 2'),
    (dip_val: $D0; dip_name: 'Easy 3'), (dip_val: $C0; dip_name: 'Easy 4'), (dip_val: $B0;
    dip_name: 'Normal 1'), (dip_val: $A0; dip_name: 'Normal 2'), (dip_val: $90;
    dip_name: 'Normal 3'), (dip_val: $80; dip_name: 'Normal 4'), (dip_val: $70;
    dip_name: 'Normal 5'), (dip_val: $60; dip_name: 'Normal 6'), (dip_val: $50;
    dip_name: 'Normal 7'), (dip_val: $40; dip_name: 'Normal 8'), (dip_val: $30;
    dip_name: 'Difficult 1'), (dip_val: $20; dip_name: 'Difficult 2'), (dip_val: $10;
    dip_name: 'Difficult 3'), (dip_val: $0; dip_name: 'Difficult 4'))), ());

var
  irq_ena: boolean;
  sound_latch, chip_latch: byte;
  mem_opcodes: array [0 .. $BFFF] of byte;
  last_addr: word;

procedure update_video_hypersports;
var
  x, y, atrib: byte;
  f, nchar, color: word;
  scroll_x: array [0 .. $1F] of word;
begin
  for f := 0 to $7FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$2800 + f];
      nchar := memory[$2000 + f] + ((atrib and $80) shl 1) + ((atrib and $40) shl 3);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $10) <> 0, (atrib and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;

  for f := 0 to $1F do
    scroll_x[f] := memory[$10C0 + (f * 2)] + ((memory[$10C1 + (f * 2)] and 1) shl 8);
  scroll__x_part2(1, 2, 8, @scroll_x);
  for f := $1F downto 0 do
  begin
    atrib := memory[$1000 + (f * 4)];
    nchar := memory[$1002 + (f * 4)] + ((atrib and $20) shl 3);
    y := 241 - memory[$1001 + (f * 4)];
    x := memory[$1003 + (f * 4)];
    color := (atrib and $F) shl 4;
    put_gfx_sprite(nchar, color, (atrib and $40) = 0, (atrib and $80) <> 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(0, 16, 256, 224, 2);
end;

procedure events_hypersports;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // System
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
  end;
end;

procedure hypersports_loop;
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
        // main
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          if irq_ena then
            m6809_0.change_irq(HOLD_LINE);
          update_video_hypersports;
        end;
      end;
      // General
      events_hypersports;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function hypersports_getbyte(direccion: word): byte;
begin
  case direccion of
    $1000 .. $10FF, $2000 .. $3FFF:
      hypersports_getbyte := memory[direccion];
    $1600:
      hypersports_getbyte := marcade.dswb; // DSW2
    $1680:
      hypersports_getbyte := marcade.in2; // SYSTEM
    $1681:
      hypersports_getbyte := marcade.in0;
    $1682:
      hypersports_getbyte := marcade.in1; // P3 y P4
    $1683:
      hypersports_getbyte := marcade.dswa; // DSW1
    $4000 .. $FFFF:
      if m6809_0.opcode then
        hypersports_getbyte := mem_opcodes[direccion - $4000]
      else
        hypersports_getbyte := memory[direccion];
  end;
end;

procedure hypersports_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $1000 .. $10FF, $3000 .. $3FFF:
      memory[direccion] := valor;
    $1480:
      main_screen.flip_main_screen := (valor and $1) <> 0;
    $1481:
      z80_0.change_irq(HOLD_LINE);
    $1487:
      irq_ena := (valor <> 0);
    $1500:
      sound_latch := valor;
    $2000 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function hypersports_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $4FFF:
      hypersports_snd_getbyte := mem_snd[direccion];
    $6000:
      hypersports_snd_getbyte := sound_latch;
    $8000:
      hypersports_snd_getbyte := (z80_0.totalt shr 10) and $F;
  end;
end;

procedure hypersports_snd_putbyte(direccion: word; valor: byte);
var
  changes, offset: integer;
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $4FFF:
      mem_snd[direccion] := valor;
    $A000:
      vlm5030_0.data_w(valor);
    $C000 .. $DFFF:
      begin
        offset := direccion and $1FFF;
        changes := offset xor last_addr;
        // A4 VLM5030 ST pin */
        if (changes and $10) <> 0 then
          vlm5030_0.set_st((offset and $10) shr 4);
        // A5 VLM5030 RST pin */
        if (changes and $20) <> 0 then
          vlm5030_0.set_rst((offset and $20) shr 5);
        last_addr := offset;
      end;
    $E000:
      dac_0.data8_w(valor);
    $E001:
      chip_latch := valor;
    $E002:
      sn_76496_0.Write(chip_latch);
  end;
end;

procedure hypersports_sound_update;
begin
  sn_76496_0.Update;
  dac_0.Update;
  vlm5030_0.Update;
end;

procedure hypersports_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
  size: word;
begin
  open_qsnapshot_save('hypersports' + nombre);
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
  savedata_qsnapshot(@memory, $4000);
  savedata_qsnapshot(@mem_snd[$4000], $C000);
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

procedure hypersports_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
begin
  if not(open_qsnapshot_load('hypersports' + nombre)) then
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
  loaddata_qsnapshot(@mem_snd[$4000]);
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
procedure close_hypersports;
begin
  write_file(Directory.Arcade_nvram + 'hypersports.nv', @memory[$3800], $800);
end;

procedure reset_hypersports;
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
end;

function start_hypersports: boolean;
var
  colores: tpaleta;
  f: word;
  longitud: integer;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $FFFF] of byte;
  rweights, gweights: array [0 .. 3] of single;
  bweights: array [0 .. 2] of single;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3,
    16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8,
    33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
begin
  start_hypersports := false;
  machine_calls.general_loop := hypersports_loop;
  machine_calls.reset := reset_hypersports;
  machine_calls.close := close_hypersports;
  machine_calls.save_qsnap := hypersports_qsave;
  machine_calls.load_qsnap := hypersports_qload;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, $100, TCPU_M6809);
  m6809_0.change_ram_calls(hypersports_getbyte, hypersports_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(14318180 div 4, $100);
  z80_0.change_ram_calls(hypersports_snd_getbyte, hypersports_snd_putbyte);
  z80_0.init_sound(hypersports_sound_update);
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(14318180 div 8);
  vlm5030_0 := vlm5030_chip.Create(3579545, $2000, 4);
  if not(roms_load(vlm5030_0.get_rom_addr, hypersports_vlm)) then
    exit;
  dac_0 := dac_chip.Create(0.80);
  if not(roms_load(@memory, hypersports_rom)) then
    exit;
  konami1_decode(@memory[$4000], @mem_opcodes[0], $C000);
  // NV ram
  if read_file_size(Directory.Arcade_nvram + 'hypersports.nv', longitud) then
    read_file(Directory.Arcade_nvram + 'hypersports.nv', @memory[$3800], longitud);
  if not(roms_load(@mem_snd, hypersports_snd)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, hypersports_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx_set_desc_data(4, 0, 16 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, hypersports_sprites)) then
    exit;
  init_gfx(1, 16, 16, $200);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $8000 * 8 + 4, $8000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // paleta
  if not(roms_load(@memory_temp, hypersports_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rweights, 1000, 0, 3, @resistances_rg,
    @gweights, 1000, 0, 2, @resistances_b, @bweights, 1000, 0);
  for f := 0 to $1F do
  begin
    // red component */
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    colores[f].r := combine_3_weights(@rweights[0], bit0, bit1, bit2);
    // green component */
    bit0 := (memory_temp[f] shr 3) and $01;
    bit1 := (memory_temp[f] shr 4) and $01;
    bit2 := (memory_temp[f] shr 5) and $01;
    colores[f].g := combine_3_weights(@gweights[0], bit0, bit1, bit2);
    // blue component */
    bit0 := (memory_temp[f] shr 6) and $01;
    bit1 := (memory_temp[f] shr 7) and $01;
    colores[f].b := combine_2_weights(@bweights[0], bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$120 + f] and $F) or $10;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $49;
  marcade.dswa_val := @hypersports_dip_a;
  marcade.dswb_val := @hypersports_dip_b;
  // final
  reset_hypersports;
  start_hypersports := true;
end;

end.
