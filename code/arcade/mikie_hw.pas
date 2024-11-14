unit mikie_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_mikie: boolean;

implementation

const
  mikie_rom: array [0 .. 2] of tipo_roms = ((n: 'n14.11c'; l: $2000; p: $6000; crc: $F698E6DD), (n: 'o13.12a'; l: $4000; p: $8000; crc: $826E7035), (n: 'o17.12d'; l: $4000; p: $C000; crc: $161C25C8));
  mikie_sound: tipo_roms = (n: 'n10.6e'; l: $2000; p: 0; crc: $2CF9D670);
  mikie_char: tipo_roms = (n: 'o11.8i'; l: $4000; p: 0; crc: $3C82AAF3);
  mikie_sprites: array [0 .. 3] of tipo_roms = ((n: '001.f1'; l: $4000; p: 0; crc: $A2BA0DF5), (n: '003.f3'; l: $4000; p: $4000; crc: $9775AB32), (n: '005.h1'; l: $4000; p: $8000; crc: $BA44AEEF), (n: '007.h3'; l: $4000; p: $C000; crc: $31AFC153));
  mikie_pal: array [0 .. 4] of tipo_roms = ((n: 'd19.1i'; l: $100; p: 0; crc: $8B83E7CF), (n: 'd21.3i'; l: $100; p: $100; crc: $3556304A), (n: 'd20.2i'; l: $100; p: $200; crc: $676A0669), (n: 'd22.12h'; l: $100; p: $300; crc: $872BE05C), (n: 'd18.f9'; l: $100; p: $400;
    crc: $7396B374));
  // Dip
  mikie_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), ());
  mikie_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '7')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: $18; name: 'Bonus Life'; number: 4;
    val4: ($18, $10, 8, 0); name4: ('20K 70K 50K+', '30K 90K 60K+', '30K Only', '40K Only')), (mask: $60; name: 'Difficulty'; number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());
  mikie_dip_c: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Flip Screen'; number: 2; val2: (0, 1); name2: ('Off', 'On')), (mask: 2; name: 'Upright Controls'; number: 2; val2: (2, 0); name2: ('Single', 'Dual')), ());

var
  banco_pal, sound_latch, sound_trq: byte;
  irq_ena: boolean;

procedure update_video_mikie;
var
  x, y, atrib: byte;
  f, color, nchar: word;
  flip_x, flip_y: boolean;
begin
  for f := $3FF downto 0 do
  begin
    if gfx[0].buffer[f] then
    begin
      y := 31 - (f mod 32);
      x := f div 32;
      atrib := memory[$3800 + f];
      color := ((atrib and $F) + ($10 * banco_pal)) shl 4;
      nchar := memory[$3C00 + f] + ((atrib and $20) shl 3);
      flip_x := (atrib and $80) = 0;
      flip_y := (atrib and $40) = 0;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, flip_x, flip_y);
      if (atrib and $10) <> 0 then
        put_gfx_mask_flip(x * 8, y * 8, nchar, color, 2, 0, 0, $FF, flip_x, flip_y)
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  for f := 0 to $23 do
  begin
    atrib := memory[$2800 + (f * 4)];
    nchar := ((atrib and $40) shl 1) + (memory[$2802 + (f * 4)] and $3F) + ((memory[$2802 + (f * 4)] and $80) shr 1) + (memory[$2802 + (f * 4)] and $40) shl 2;
    color := ((atrib and $F) + ($10 * banco_pal)) shl 4;
    x := 244 - memory[$2801 + (f * 4)];
    flip_x := (atrib and $20) = 0;
    if not(main_screen.flip_main_screen) then
    begin
      y := 240 - memory[$2803 + (f * 4)];
      flip_y := (atrib and $10) <> 0;
    end
    else
    begin
      flip_y := (atrib and $10) = 0;
      y := memory[$2803 + (f * 4)];
      x := x - 2;
    end;
    put_gfx_sprite(nchar, color, flip_x, flip_y, 1);
    update_gfx_sprite(x, y, 3, 1);
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(16, 0, 224, 256, 3);
end;

procedure events_mikie;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // misc
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

procedure mikie_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 240 then
        begin
          if irq_ena then
            m6809_0.change_irq(ASSERT_LINE);
          update_video_mikie;
        end;
        // Main CPU
        m6809_0.run(frame_main);
        frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
        // Sound CPU
        z80_0.run(frame_snd);
        frame_snd := frame_snd + z80_0.tframes - z80_0.contador;
      end;
      events_mikie;
      video_sync;
    end
    else
      pause_action;
  end
end;

function mikie_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $FF, $2800 .. $FFFF:
      mikie_getbyte := memory[direccion];
    $2400:
      mikie_getbyte := marcade.in2;
    $2401:
      mikie_getbyte := marcade.in0;
    $2402:
      mikie_getbyte := marcade.in1;
    $2403:
      mikie_getbyte := marcade.dswc;
    $2500:
      mikie_getbyte := marcade.dswa;
    $2501:
      mikie_getbyte := marcade.dswb;
  end;
end;

procedure mikie_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FF, $2800 .. $37FF:
      memory[direccion] := valor;
    $2002:
      begin
        if ((sound_trq = 0) and (valor = 1)) then
          z80_0.change_irq(HOLD_LINE);
        sound_trq := valor;
      end;
    $2006:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $2007:
      begin
        irq_ena := (valor <> 0);
        if not(irq_ena) then
          m6809_0.change_irq(CLEAR_LINE);
      end;
    $2100:
      ; // wd
    $2200:
      banco_pal := valor and 7;
    $2400:
      sound_latch := valor;
    $3800 .. $3FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function sound_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $43FF:
      sound_getbyte := mem_snd[direccion];
    $8003:
      sound_getbyte := sound_latch;
    $8005:
      sound_getbyte := z80_0.totalt shr 9;
  end;
end;

procedure sound_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
    $8002:
      sn_76496_0.Write(valor);
    $8004:
      sn_76496_1.Write(valor);
  end;
end;

procedure sound_update;
begin
  sn_76496_0.update;
  sn_76496_1.update;
end;

procedure mikie_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
  size: word;
begin
  open_qsnapshot_save('mikie' + nombre);
  getmem(data, 180);
  // CPU
  size := m6809_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := sn_76496_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := sn_76496_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[0], $4000);
  savedata_qsnapshot(@mem_snd[$2000], $E000);
  // MISC
  buffer[0] := banco_pal;
  buffer[2] := byte(irq_ena);
  buffer[3] := sound_latch;
  buffer[4] := sound_trq;
  savedata_qsnapshot(@buffer, 5);
  freemem(data);
  close_qsnapshot;
end;

procedure mikie_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
begin
  if not(open_qsnapshot_load('mikie' + nombre)) then
    exit;
  getmem(data, 180);
  // CPU
  loaddata_qsnapshot(data);
  m6809_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  sn_76496_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  sn_76496_1.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory);
  loaddata_qsnapshot(@mem_snd[$2000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  banco_pal := buffer[0];
  irq_ena := buffer[2] <> 0;
  sound_latch := buffer[3];
  sound_trq := buffer[4];
  freemem(data);
  close_qsnapshot;
  // END
  fillchar(gfx[0].buffer, $400, 1);
end;

// Main
procedure reset_mikie;
begin
  m6809_0.reset;
  z80_0.reset;
  frame_main := m6809_0.tframes;
  frame_snd := z80_0.tframes;
  sn_76496_0.reset;
  sn_76496_1.reset;
 reset_video;
  reset_audio;
  banco_pal := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_ena := false;
  sound_trq := 0;
end;

function start_mikie: boolean;
var
  colores: tpaleta;
  f, bit0, bit1, bit2, bit3: byte;
  memory_temp: array [0 .. $FFFF] of byte;
  rweights, gweights, bweights: array [0 .. 3] of single;
const
  ps_x: array [0 .. 15] of dword = (32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 0, 1, 2, 3, 48 * 8 + 0, 48 * 8 + 1, 48 * 8 + 2, 48 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 32 * 16, 33 * 16, 34 * 16, 35 * 16, 36 * 16, 37 * 16, 38 * 16, 39 * 16);
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 4 * 8, 1 * 4 * 8, 2 * 4 * 8, 3 * 4 * 8, 4 * 4 * 8, 5 * 4 * 8, 6 * 4 * 8, 7 * 4 * 8);
  resistances: array [0 .. 3] of integer = (2200, 1000, 470, 220);
begin
  machine_calls.general_loop := mikie_loop;
  machine_calls.reset := reset_mikie;
  machine_calls.save_qsnap := mikie_qsave;
  machine_calls.load_qsnap := mikie_qload;
  machine_calls.fps_max := 60.59;
  start_mikie := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  screen_init(3, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, 256, TCPU_M6809);
  m6809_0.change_ram_calls(mikie_getbyte, mikie_putbyte);
  if not(roms_load(@memory, mikie_rom)) then
    exit;
  // Sound CPU
  z80_0 := cpu_z80.Create(14318180 div 4, 256);
  z80_0.change_ram_calls(sound_getbyte, sound_putbyte);
  z80_0.init_sound(sound_update);
  if not(roms_load(@mem_snd, mikie_sound)) then
    exit;
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(14318180 div 8);
  sn_76496_1 := sn76496_chip.Create(14318180 div 4);
  // convertir chars
  if not(roms_load(@memory_temp, mikie_char)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, true, false);
  // sprites
  if not(roms_load(@memory_temp, mikie_sprites)) then
    exit;
  init_gfx(1, 16, 16, 512);
  gfx[1].trans[0] := true;
  for f := 0 to 1 do
  begin
    gfx_set_desc_data(4, 2, 128 * 8, 0 + f * 8, 4 + f * 8, f * 8 + $8000 * 8 + 0, f * 8 + $8000 * 8 + 4);
    convert_gfx(1, $100 * f * 16 * 16, @memory_temp, @ps_x, @ps_y, true, false);
  end;
  // paleta
  if not(roms_load(@memory_temp, mikie_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 4, @resistances, @rweights, 470, 0, 4, @resistances, @gweights, 470, 0, 4, @resistances, @bweights, 470, 0);
  for f := 0 to $FF do
  begin
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    bit3 := (memory_temp[f] shr 3) and 1;
    colores[f].r := combine_4_weights(@rweights, bit0, bit1, bit2, bit3);
    bit0 := (memory_temp[f + $100] shr 0) and 1;
    bit1 := (memory_temp[f + $100] shr 1) and 1;
    bit2 := (memory_temp[f + $100] shr 2) and 1;
    bit3 := (memory_temp[f + $100] shr 3) and 1;
    colores[f].g := combine_4_weights(@gweights, bit0, bit1, bit2, bit3);
    bit0 := (memory_temp[f + $200] shr 0) and 1;
    bit1 := (memory_temp[f + $200] shr 1) and 1;
    bit2 := (memory_temp[f + $200] shr 2) and 1;
    bit3 := (memory_temp[f + $200] shr 3) and 1;
    colores[f].b := combine_4_weights(@bweights, bit0, bit1, bit2, bit3);
  end;
  set_pal(colores, 256);
  // tabla_colores char & sprites
  for bit1 := 0 to $FF do
  begin
    for bit2 := 0 to 7 do
    begin
      gfx[0].colores[bit1 + (bit2 shl 8)] := (memory_temp[bit1 + $300] and $F) + (bit2 shl 5) + 16;
      gfx[1].colores[bit1 + (bit2 shl 8)] := (memory_temp[bit1 + $400] and $F) + (bit2 shl 5);
    end;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $7B;
  marcade.dswc := $FE;
  marcade.dswa_val2 := @mikie_dip_a;
  marcade.dswb_val2 := @mikie_dip_b;
  marcade.dswc_val2 := @mikie_dip_c;
  // final
  reset_mikie;
  start_mikie := true;
end;

end.
