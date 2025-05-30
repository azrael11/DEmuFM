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
uses
  uDataModule;

const
  hypersports_rom: array [0 .. 5] of tipo_roms = ((n: 'c01'; l: $2000; p: $4000; crc: $0C720EEB), (n: 'c02'; l: $2000; p: $6000; crc: $560258E0), (n: 'c03'; l: $2000; p: $8000; crc: $9B01C7E6), (n: 'c04'; l: $2000; p: $A000; crc: $10D7E9A2), (n: 'c05'; l: $2000; p: $C000;
    crc: $B105A8CD), (n: 'c06'; l: $2000; p: $E000; crc: $1A34A849));
  hypersports_char: array [0 .. 3] of tipo_roms = ((n: 'c26'; l: $2000; p: 0; crc: $A6897EAC), (n: 'c24'; l: $2000; p: $2000; crc: $5FB230C0), (n: 'c22'; l: $2000; p: $4000; crc: $ED9271A0), (n: 'c20'; l: $2000; p: $6000; crc: $183F4324));
  hypersports_sprites: array [0 .. 7] of tipo_roms = ((n: 'c14'; l: $2000; p: 0; crc: $C72D63BE), (n: 'c13'; l: $2000; p: $2000; crc: $76565608), (n: 'c12'; l: $2000; p: $4000; crc: $74D2CC69), (n: 'c11'; l: $2000; p: $6000; crc: $66CBCB4D), (n: 'c18'; l: $2000; p: $8000;
    crc: $ED25E669), (n: 'c17'; l: $2000; p: $A000; crc: $B145B39F), (n: 'c16'; l: $2000; p: $C000; crc: $D7FF9F2B), (n: 'c15'; l: $2000; p: $E000; crc: $F3D454E6));
  hypersports_pal: array [0 .. 2] of tipo_roms = ((n: 'c03_c27.bin'; l: $20; p: 0; crc: $BC8A5956), (n: 'j12_c28.bin'; l: $100; p: $20; crc: $2C891D59), (n: 'a09_c29.bin'; l: $100; p: $120; crc: $811A3F3F));
  hypersports_vlm: tipo_roms = (n: 'c08'; l: $2000; p: 0; crc: $E8F8EA78);
  hypersports_snd: array [0 .. 1] of tipo_roms = ((n: 'c10'; l: $2000; p: 0; crc: $3DC1A6FF), (n: 'c09'; l: $2000; p: $2000; crc: $9B525C3E));
  hypersports_dip_a: array [0 .. 1] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), ());
  hypersports_dip_b: array [0 .. 5] of def_dip2 = ((mask: 1; name: 'After Last Event'; number: 2; val2: (1, 0); name2: ('Game Over', 'Game Continues')), (mask: 2; name: 'Cabinet'; number: 2; val2: (0, 2); name2: ('Upright', 'Cocktail')), (mask: 4; name: 'Demo Sounds'; number: 2;
    val2: (4, 0); name2: ('Off', 'On')), (mask: 8; name: 'World Records'; number: 2; val2: (8, 0); name2: ('Don''t Erase', 'Erase on Reset')), (mask: $F0; name: 'Difficulty'; number: 16; val16: ($F0, $E0, $D0, $C0, $B0, $A0, $90, $80, $70, $60, $50, $40, $30, $20, $10, 0);
    name16: ('Easy 1', 'Easy 2', 'Easy 3', 'Easy 4', 'Normal 1', 'Normal 2', 'Normal 3', 'Normal 4', 'Normal 5', 'Normal 6', 'Normal 7', 'Normal 8', 'Difficult 1', 'Difficult 2', 'Difficult 3', 'Difficult 4')), ());
  roadf_rom: array [0 .. 5] of tipo_roms = ((n: 'g05_g01.bin'; l: $2000; p: $4000; crc: $E2492A06), (n: 'g07_f02.bin'; l: $2000; p: $6000; crc: $0BF75165), (n: 'g09_g03.bin'; l: $2000; p: $8000; crc: $DDE401F8), (n: 'g11_f04.bin'; l: $2000; p: $A000; crc: $B1283C77),
    (n: 'g13_f05.bin'; l: $2000; p: $C000; crc: $0AD4D796), (n: 'g15_f06.bin'; l: $2000; p: $E000; crc: $FA42E0ED));
  roadf_char: array [0 .. 3] of tipo_roms = ((n: 'a14_e26.bin'; l: $4000; p: 0; crc: $F5C738E2), (n: 'a12_d24.bin'; l: $2000; p: $4000; crc: $2D82C930), (n: 'c14_e22.bin'; l: $4000; p: $6000; crc: $FBCFBEB9), (n: 'c12_d20.bin'; l: $2000; p: $A000; crc: $5E0CF994));
  roadf_sprites: array [0 .. 1] of tipo_roms = ((n: 'j19_e14.bin'; l: $4000; p: 0; crc: $16D2BCFF), (n: 'g19_e18.bin'; l: $4000; p: $4000; crc: $490685FF));
  roadf_pal: array [0 .. 2] of tipo_roms = ((n: 'c03_c27.bin'; l: $20; p: 0; crc: $45D5E352), (n: 'j12_c28.bin'; l: $100; p: $20; crc: $2955E01F), (n: 'a09_c29.bin'; l: $100; p: $120; crc: $5B3B5F2A));
  roadf_snd: tipo_roms = (n: 'a17_d10.bin'; l: $2000; p: 0; crc: $C33C927E);
  roadf_dip_b: array [0 .. 6] of def_dip2 = ((mask: 1; name: 'Allow Continue'; number: 2; val2: (1, 0); name2: ('No', 'Yes')), (mask: 6; name: 'Number of Opponents'; number: 4; val4: (6, 4, 2, 0); name4: ('Few', 'Normal', 'Many', 'Great Many')), (mask: 8;
    name: 'Speed of Opponents'; number: 2; val2: (8, 0); name2: ('Fast', 'Slow')), (mask: $30; name: 'Fuel Consumption'; number: 4; val4: ($30, $20, $10, 0); name4: ('Slow', 'Normal', 'Fast', 'Very Fast')), (mask: $40; name: 'Cabinet'; number: 2; val2: (0, $40);
    name2: ('Upright', 'Cocktail')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  flip_screen, irq_ena: boolean;
  sound_latch, chip_latch, lst_snd_irq: byte;
  mem_opcodes: array [0 .. $BFFF] of byte;
  update_video_call, eventos_call: procedure;
  scroll_x: array [0 .. $1F] of word;
  // Hypersports
  last_addr: word;

procedure draw_sprites;
var
  x, y, atrib: byte;
  f, nchar, color: word;
  flip_x: boolean;
begin
  for f := $1F downto 0 do
  begin
    atrib := memory[$1000 + (f * 4)];
    nchar := memory[$1002 + (f * 4)] + ((atrib and $20) shl 3);
    y := 240 - memory[$1001 + (f * 4)];
    if flip_screen then
    begin
      x := 240 - memory[$1003 + (f * 4)];
      flip_x := (atrib and $40) <> 0;
    end
    else
    begin
      x := memory[$1003 + (f * 4)];
      flip_x := (atrib and $40) = 0;
    end;
    color := (atrib and $F) shl 4;
    put_gfx_sprite(nchar, color, flip_x, (atrib and $80) <> 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
end;

procedure update_video_hypersports;
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
      atrib := memory[$2800 + f];
      nchar := memory[$2000 + f] + ((atrib and $80) shl 1) + ((atrib and $40) shl 3);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $10) <> 0, (atrib and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x_part2(1, 2, 8, @scroll_x);
  draw_sprites;
  update_final_piece(0, 16, 256, 224, 2);
end;

procedure update_video_roadf;
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
      atrib := memory[$2800 + f];
      nchar := memory[$2000 + f] + ((atrib and $80) shl 1) + ((atrib and $60) shl 4);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $10) <> 0, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x_part2(1, 2, 8, @scroll_x);
  draw_sprites;
  update_final_piece(0, 16, 256, 224, 2);
end;

procedure events_hypersports;
begin
  if event.arcade then
  begin
    // P1+P2
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

procedure eventos_roadf;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
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

procedure hypersports_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
	  events_hypersports;
        if f = 240 then
        begin
          if irq_ena then
            m6809_0.change_irq(ASSERT_LINE);
          update_video_call;
        end;
        // main
        m6809_0.run(frame_main);
        frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
        // sound
        z80_0.run(frame_snd);
        frame_snd := frame_snd + z80_0.tframes - z80_0.contador;
      end;
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
      hypersports_getbyte := marcade.dswb;
    $1680:
      hypersports_getbyte := marcade.in2;
    $1681:
      hypersports_getbyte := marcade.in0;
    $1682:
      hypersports_getbyte := marcade.in1;
    $1683:
      hypersports_getbyte := marcade.dswa;
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
    $1000 .. $10BF, $3000 .. $3FFF:
      memory[direccion] := valor;
    $10C0 .. $10FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        direccion := (direccion and $3F) shr 1;
        if flip_screen then
          scroll_x[direccion] := 512 - (memory[$10C0 + (direccion * 2)] + ((memory[$10C1 + (direccion * 2)] and 1) shl 8))
        else
          scroll_x[direccion] := memory[$10C0 + (direccion * 2)] + ((memory[$10C1 + (direccion * 2)] and 1) shl 8);
      end;
    $1480:
      begin
        flip_screen := (valor and 1) <> 0;
        main_screen.flip_main_screen := flip_screen;
      end;
    $1481:
      begin
        if ((lst_snd_irq = 0) and (valor <> 0)) then
          z80_0.change_irq(HOLD_LINE);
        lst_snd_irq := valor;
      end;
    $1487:
      begin
        irq_ena := (valor <> 0);
        if not(irq_ena) then
          m6809_0.change_irq(CLEAR_LINE);
      end;
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
      hypersports_snd_getbyte := (vlm5030_0.get_bsy shl 2) or ((z80_0.totalt shr 10) and 3);
  end;
end;

procedure hypersports_snd_putbyte(direccion: word; valor: byte);
var
  changes, offset: word;
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

procedure hypersports_sound_update;
begin
  sn_76496_0.Update;
  dac_0.Update;
  vlm5030_0.Update;
end;

// Road Fighter
function roadf_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $4FFF:
      roadf_snd_getbyte := mem_snd[direccion];
    $6000:
      roadf_snd_getbyte := sound_latch;
    $8000:
      roadf_snd_getbyte := (z80_0.totalt shr 10) and 3;
  end;
end;

procedure roadf_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $4FFF:
      mem_snd[direccion] := valor;
    $E000:
      dac_0.data8_w(valor);
    $E001:
      chip_latch := valor;
    $E002:
      sn_76496_0.Write(chip_latch);
  end;
end;

procedure roadf_sound_update;
begin
  sn_76496_0.Update;
  dac_0.Update;
end;

// Snapshot
procedure hypersports_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
  size: word;
begin
  case main_vars.machine_type of
    227:
      open_qsnapshot_save('hypersports' + nombre);
    400:
      open_qsnapshot_save('roadf' + nombre);
  end;
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
  case main_vars.machine_type of
    227:
      if not(open_qsnapshot_load('hypersports' + nombre)) then
        exit;
    400:
      if not(open_qsnapshot_load('roadf' + nombre)) then
        exit;
  end;
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
var
  save_name: string;
begin
  case main_vars.machine_type of
    227:
      save_name := 'hypersports.nv';
    400:
      save_name := 'roadf.nv';
  end;
  write_file(dm.tConfignvram.AsString + save_name, @memory[$3800], $800);
end;

procedure reset_hypersports;
begin
  m6809_0.reset;
  z80_0.reset;
  frame_main := m6809_0.tframes;
  frame_snd := z80_0.tframes;
  dac_0.reset;
  marcade.in0 := $FF;
  if (main_vars.machine_type = 400) then
    marcade.in1 := $BF
  else
  begin
    vlm5030_0.reset;
    marcade.in1 := $FF;
  end;
  marcade.in2 := $FF;
  irq_ena := false;
  sound_latch := 0;
  chip_latch := 0;
  last_addr := 0;
  flip_screen := false;
  lst_snd_irq := 0;
  fillchar(scroll_x, $40, 0);
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
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
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
  if (main_vars.machine_type = 400) then
    main_screen.rot90_screen := true;
  screen_init(2, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, $100, TCPU_M6809);
  m6809_0.change_ram_calls(hypersports_getbyte, hypersports_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(14318180 div 4, $100);
  if (main_vars.machine_type = 400) then
    z80_0.init_sound(roadf_sound_update)
  else
    z80_0.init_sound(hypersports_sound_update);
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(14318180 div 8);
  dac_0 := dac_chip.Create(0.80);
  case main_vars.machine_type of
    227:
      begin
        update_video_call := update_video_hypersports;
        eventos_call := events_hypersports;
        if not(roms_load(@memory, hypersports_rom)) then
          exit;
        konami1_decode(@memory[$4000], @mem_opcodes[0], $C000);
        // Sound CPU
        z80_0.change_ram_calls(hypersports_snd_getbyte, hypersports_snd_putbyte);
        if not(roms_load(@mem_snd, hypersports_snd)) then
          exit;
        // Extra Sound Chip
        vlm5030_0 := vlm5030_chip.Create(3579545, $2000, 1);
        if not(roms_load(vlm5030_0.get_rom_addr, hypersports_vlm)) then
          exit;
        // NV ram
        if read_file_size(dm.tConfignvram.AsString + 'hypersports.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'hypersports.nv', @memory[$3800], longitud);
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
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $49;
        marcade.dswa_val2 := @hypersports_dip_a;
        marcade.dswb_val2 := @hypersports_dip_b;
        // paleta
        if not(roms_load(@memory_temp, hypersports_pal)) then
          exit;
      end;
    400:
      begin
        update_video_call := update_video_roadf;
        eventos_call := eventos_roadf;
        if not(roms_load(@memory, roadf_rom)) then
          exit;
        konami1_decode(@memory[$4000], @mem_opcodes[0], $C000);
        // Sound CPU
        z80_0.change_ram_calls(roadf_snd_getbyte, roadf_snd_putbyte);
        if not(roms_load(@mem_snd, roadf_snd)) then
          exit;
        // NV ram
        if read_file_size(dm.tConfignvram.AsString + 'roadf.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'roadf.nv', @memory[$3800], longitud);
        // convertir chars
        if not(roms_load(@memory_temp, roadf_char)) then
          exit;
        init_gfx(0, 8, 8, $600);
        gfx_set_desc_data(4, 0, 16 * 8, $6000 * 8 + 4, $6000 * 8 + 0, 4, 0);
        convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // sprites
        if not(roms_load(@memory_temp, roadf_sprites)) then
          exit;
        init_gfx(1, 16, 16, $100);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(4, 0, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // DIP
        marcade.dswa := $FF;
        marcade.dswb := $2D;
        marcade.dswa_val2 := @hypersports_dip_a;
        marcade.dswb_val2 := @roadf_dip_b;
        // paleta
        if not(roms_load(@memory_temp, roadf_pal)) then
          exit;
      end;
  end;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rweights, 1000, 0, 3, @resistances_rg, @gweights, 1000, 0, 2, @resistances_b, @bweights, 1000, 0);
  for f := 0 to $1F do
  begin
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rweights[0], bit0, bit1, bit2);
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@gweights[0], bit0, bit1, bit2);
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights[0], bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$120 + f] and $F) or $10;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
  end;
  // final
  start_hypersports := true;
end;

end.
