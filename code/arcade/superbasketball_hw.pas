unit superbasketball_hw;

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
  qsnapshot;

function start_superbasketball: boolean;

implementation

const
  sbasketb_rom: array [0 .. 2] of tipo_roms = ((n: '405g05.14j'; l: $2000; p: $6000; crc: $336DC0AB), (n: '405i03.11j'; l: $4000; p: $8000; crc: $D33B82DD), (n: '405i01.9j'; l: $4000; p: $C000;
    crc: $1C09CC3F));
  sbasketb_char: tipo_roms = (n: '405e12.22f'; l: $4000; p: 0; crc: $E02C54DA);
  sbasketb_sprites: array [0 .. 2] of tipo_roms = ((n: '405h06.14g'; l: $4000; p: 0; crc: $CFBBFF07), (n: '405h08.17g'; l: $4000; p: $4000; crc: $C75901B6), (n: '405h10.20g'; l: $4000; p: $8000;
    crc: $95BC5942));
  sbasketb_pal: array [0 .. 4] of tipo_roms = ((n: '405e17.5a'; l: $100; p: $0; crc: $B4C36D57), (n: '405e16.4a'; l: $100; p: $100; crc: $0B7B03B8), (n: '405e18.6a'; l: $100; p: $200; crc: $9E533BAD),
    (n: '405e20.19d'; l: $100; p: $300; crc: $8CA6DE2F), (n: '405e19.16d'; l: $100; p: $400; crc: $E0BC782F));
  sbasketb_vlm: tipo_roms = (n: '405e15.11f'; l: $2000; p: $0; crc: $01BB5CE9);
  sbasketb_snd: tipo_roms = (n: '405e13.7a'; l: $2000; p: $0; crc: $1EC7458B);
  sbasketb_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 15;
    dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $40; dip_name: '3C 2C'), (dip_val: $10; dip_name: '4C 3C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $30; dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90; dip_name: '1C 7C'), (dip_val: $0;
    dip_name: 'Free Play'))), ());
  sbasketb_dip_b: array [0 .. 6] of def_dip = ((mask: $3; name: 'Game Time'; number: 4; dip: ((dip_val: $3; dip_name: '30'), (dip_val: $1; dip_name: '40'), (dip_val: $2; dip_name: '50'), (dip_val: $0;
    dip_name: '60'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Starting Score'; number: 2;
    dip: ((dip_val: $8; dip_name: '70-78'), (dip_val: $0; dip_name: '100-115'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Ranking'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Data Remaining'), (dip_val: $10; dip_name: 'Data Initialized'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Medium'), (dip_val: $20; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  pedir_snd_irq, irq_ena: boolean;
  sound_latch, chip_latch, scroll_x, sbasketb_palettebank, sprite_select: byte;
  mem_opcodes: array [0 .. $9FFF] of byte;
  last_addr: word;

procedure update_video_sbasketb;
var
  f, nchar, color, offset: word;
  x, y, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory[$3000 + f];
      nchar := memory[$3400 + f] + ((atrib and $20) shl 3);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // La parte de arriba es fija...
  update_region(0, 0, 256, 48, 1, 0, 0, 256, 48, 2);
  scroll__x_part2(1, 2, 208, @scroll_x, 0, 0, 48);
  offset := sprite_select * $100;
  for f := 0 to $3F do
  begin
    atrib := memory[$3801 + offset + (f * 4)];
    nchar := memory[$3800 + offset + (f * 4)] + ((atrib and $20) shl 3);
    y := memory[$3802 + offset + (f * 4)];
    x := 240 - (memory[$3803 + offset + (f * 4)]);
    color := ((atrib and $0F) + 16 * sbasketb_palettebank) shl 4;
    put_gfx_sprite(nchar, color, (atrib and $80) <> 0, (atrib and $40) <> 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_sbasketb;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // P2
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
  end;
end;

procedure sbasketb_loop;
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
          update_video_sbasketb;
        end;
      end;
      // General
      events_sbasketb;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function sbasketb_getbyte(direccion: word): byte;
begin
  case direccion of
    $2000 .. $3BFF:
      sbasketb_getbyte := memory[direccion];
    $3E00:
      sbasketb_getbyte := marcade.in0;
    $3E01:
      sbasketb_getbyte := marcade.in1;
    $3E02:
      sbasketb_getbyte := marcade.in2;
    $3E80:
      sbasketb_getbyte := marcade.dswb;
    $3F00:
      sbasketb_getbyte := marcade.dswa;
    $6000 .. $FFFF:
      if m6809_0.opcode then
        sbasketb_getbyte := mem_opcodes[direccion - $6000]
      else
        sbasketb_getbyte := memory[direccion];
  end;
end;

procedure sbasketb_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $2000 .. $2FFF, $3800 .. $3BFF:
      memory[direccion] := valor;
    $3000 .. $37FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $3C20:
      sbasketb_palettebank := valor;
    $3C80:
      main_screen.flip_main_screen := (valor and $1) <> 0;
    $3C81:
      irq_ena := (valor <> 0);
    $3C85:
      sprite_select := valor and 1;
    $3D00:
      sound_latch := valor;
    $3D80:
      z80_0.change_irq(HOLD_LINE);
    $3F80:
      scroll_x := not(valor);
    $6000 .. $FFFF:
      ; // ROM
  end;
end;

function sbasketb_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $43FF:
      sbasketb_snd_getbyte := mem_snd[direccion];
    $6000:
      sbasketb_snd_getbyte := sound_latch;
    $8000:
      sbasketb_snd_getbyte := ((z80_0.totalt shr 10) and $3) or ((vlm5030_0.get_bsy and 1) shl 2);
  end;
end;

procedure sbasketb_snd_putbyte(direccion: word; valor: byte);
var
  changes, offset: word;
begin
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
    $A000:
      vlm5030_0.data_w(valor);
    $C000 .. $DFFF:
      begin
        offset := direccion and $1FFF;
        changes := offset xor last_addr;
        // A4 VLM5030 ST pin
        if (changes and $10) <> 0 then
          vlm5030_0.set_st((offset and $10) shr 4);
        // A5 VLM5030 RST pin
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

procedure sbasketb_sound_update;
begin
  sn_76496_0.Update;
  dac_0.Update;
  vlm5030_0.Update;
end;

procedure sbasketb_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 8] of byte;
  size: word;
begin
  open_qsnapshot_save('sbasketb' + nombre);
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
  buffer[0] := byte(pedir_snd_irq);
  buffer[1] := byte(irq_ena);
  buffer[2] := sound_latch;
  buffer[3] := chip_latch;
  buffer[4] := scroll_x;
  buffer[5] := sbasketb_palettebank;
  buffer[6] := sprite_select;
  buffer[7] := last_addr and $FF;
  buffer[8] := last_addr shr 8;
  savedata_qsnapshot(@buffer, 9);
  freemem(data);
  close_qsnapshot;
end;

procedure sbasketb_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 8] of byte;
begin
  if not(open_qsnapshot_load('sbasketb' + nombre)) then
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
  pedir_snd_irq := buffer[0] <> 0;
  irq_ena := buffer[1] <> 0;
  sound_latch := buffer[2];
  chip_latch := buffer[3];
  scroll_x := buffer[4];
  sbasketb_palettebank := buffer[5];
  sprite_select := buffer[6];
  last_addr := buffer[7] or (buffer[8] shl 8);
  freemem(data);
  close_qsnapshot;
  // END
  fillchar(gfx[0].buffer, $400, 1);
end;

// Main
procedure reset_sbasketb;
begin
  m6809_0.reset;
  z80_0.reset;
  vlm5030_0.reset;
  dac_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  pedir_snd_irq := false;
  irq_ena := false;
  sound_latch := 0;
  chip_latch := 0;
  scroll_x := 0;
  sbasketb_palettebank := 0;
  sprite_select := 0;
  last_addr := 0;
end;

function start_superbasketball: boolean;
var
  colores: tpaleta;
  f, j: byte;
  memory_temp: array [0 .. $BFFF] of byte;
const
  pc_y: array [0 .. 7] of dword = (0 * 4 * 8, 1 * 4 * 8, 2 * 4 * 8, 3 * 4 * 8, 4 * 4 * 8, 5 * 4 * 8, 6 * 4 * 8, 7 * 4 * 8);
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 8 * 4, 9 * 4, 10 * 4, 11 * 4, 12 * 4, 13 * 4, 14 * 4, 15 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 4 * 16, 1 * 4 * 16, 2 * 4 * 16, 3 * 4 * 16, 4 * 4 * 16, 5 * 4 * 16, 6 * 4 * 16, 7 * 4 * 16, 8 * 4 * 16, 9 * 4 * 16, 10 * 4 * 16, 11 * 4 * 16, 12 * 4 * 16,
    13 * 4 * 16, 14 * 4 * 16, 15 * 4 * 16);
begin
  machine_calls.general_loop := sbasketb_loop;
  machine_calls.reset := reset_sbasketb;
  machine_calls.save_qsnap := sbasketb_qsave;
  machine_calls.load_qsnap := sbasketb_qload;
  start_superbasketball := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(1400000, $100, TCPU_MC6809E);
  m6809_0.change_ram_calls(sbasketb_getbyte, sbasketb_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(3579545, $100);
  z80_0.change_ram_calls(sbasketb_snd_getbyte, sbasketb_snd_putbyte);
  z80_0.init_sound(sbasketb_sound_update);
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(1789772);
  vlm5030_0 := vlm5030_chip.Create(3579545, $2000, 4);
  if not(roms_load(vlm5030_0.get_rom_addr, sbasketb_vlm)) then
    exit;
  dac_0 := dac_chip.Create(0.80);
  // cargar roms
  if not(roms_load(@memory, sbasketb_rom)) then
    exit;
  konami1_decode(@memory[$6000], @mem_opcodes, $A000);
  // cargar snd roms
  if not(roms_load(@mem_snd, sbasketb_snd)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, sbasketb_char)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx_set_desc_data(4, 0, 8 * 4 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, true, false);
  // sprites
  if not(roms_load(@memory_temp, sbasketb_sprites)) then
    exit;
  init_gfx(1, 16, 16, 384);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 4 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // paleta
  if not(roms_load(@memory_temp, sbasketb_pal)) then
    exit;
  for f := 0 to $FF do
  begin
    colores[f].r := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
    colores[f].g := ((memory_temp[f + $100] and $F) shl 4) or (memory_temp[f + $100] shr 4);
    colores[f].b := ((memory_temp[f + $200] and $F) shl 4) or (memory_temp[f + $200] and $F);
  end;
  set_pal(colores, 256);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$300 + f] and $F) or $F0;
    for j := 0 to $F do
      gfx[1].colores[(j shl 8) or f] := ((j shl 4) or (memory_temp[f + $400] and $0F));
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $68;
  marcade.dswa_val := @sbasketb_dip_a;
  marcade.dswb_val := @sbasketb_dip_b;
  // final
  reset_sbasketb;
  start_superbasketball := true;
end;

end.
