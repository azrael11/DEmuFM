unit greenberet_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  gfx_engine,
  timer_engine,
  rom_engine,
  file_engine,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_greenberet: boolean;

implementation

const
  // Green Beret
  gberet_rom: array [0 .. 2] of tipo_roms = ((n: '577l03.10c'; l: $4000; p: 0; crc: $AE29E4FF), (n: '577l02.8c'; l: $4000; p: $4000; crc: $240836A5), (n: '577l01.7c'; l: $4000; p: $8000; crc: $41FA3E1F));
  gberet_pal: array [0 .. 2] of tipo_roms = ((n: '577h09.2f'; l: $20; p: 0; crc: $C15E7C80), (n: '577h11.6f'; l: $100; p: $20; crc: $2A1A992B), (n: '577h10.5f'; l: $100; p: $120; crc: $E9DE1E53));
  gberet_char: tipo_roms = (n: '577l07.3f'; l: $4000; p: 0; crc: $4DA7BD1B);
  gberet_sprites: array [0 .. 3] of tipo_roms = ((n: '577l06.5e'; l: $4000; p: 0; crc: $0F1CB0CA), (n: '577l05.4e'; l: $4000; p: $4000; crc: $523A8B66), (n: '577l08.4f'; l: $4000; p: $8000; crc: $883933A4), (n: '577l04.3e'; l: $4000; p: $C000; crc: $CCECDA4C));
  // Mr Goemon
  mrgoemon_rom: array [0 .. 1] of tipo_roms = ((n: '621d01.10c'; l: $8000; p: 0; crc: $B2219C56), (n: '621d02.12c'; l: $8000; p: $8000; crc: $C3337A97));
  mrgoemon_pal: array [0 .. 2] of tipo_roms = ((n: '621a06.5f'; l: $20; p: 0; crc: $7C90DE5F), (n: '621a08.7f'; l: $100; p: $20; crc: $2FB244DD), (n: '621a07.6f'; l: $100; p: $120; crc: $3980ACDC));
  mrgoemon_char: tipo_roms = (n: '621a05.6d'; l: $4000; p: 0; crc: $F0A6DFC5);
  mrgoemon_sprites: array [0 .. 1] of tipo_roms = ((n: '621d03.4d'; l: $8000; p: 0; crc: $66F2B973), (n: '621d04.5d'; l: $8000; p: $8000; crc: $47DF6301));
  // Dip
  gberet_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $99, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Invalid')), ());
  gberet_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('2', '3', '5', '7')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: $18; name: 'Bonus Life'; number: 4;
    val4: ($18, $10, 8, 0); name4: ('30K 70K+', '40K 80K+', '50K 100K+', '50K 200K+')), (mask: $60; name: 'Difficulty'; number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Normal', 'Difficult', 'Very Difficult')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());
  gberet_dip_c: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Flip Screen'; number: 2; val2: (1, 0); name2: ('Off', 'On')), (mask: 2; name: 'Upright Controls'; number: 2; val2: (2, 0); name2: ('Single', 'Dual')), ());
  mrgoemon_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('2', '3', '5', '7')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: $18; name: 'Bonus Life'; number: 4;
    val4: ($18, $10, 8, 0); name4: ('20K 60K+', '30K 70K+', '40K 80K+', '50K 90K+')), (mask: $60; name: 'Difficulty'; number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Normal', 'Difficult', 'Very Difficult')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());

var
  scroll_lineas: array [0 .. $1F] of word;
  memoria_rom: array [0 .. 7, 0 .. $7FF] of byte;
  interrupt_mask, interrupt_ticks, sound_latch, rom_bank, timer_hs: byte;
  banco_sprites: word;

procedure update_video_gberet;
var
  f, x, y, color, nchar, atrib2: word;
  atrib: byte;
begin
  for f := $7FF downto 0 do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      // Color RAM
      // c000-c7ff --> Bits
      // 0-3 --> Color
      // 4 --> flip X
      // 5 --> Flip Y
      // 6 --> Numero Char
      // 7 --> prioridad
      atrib := memory[f + $C000];
      color := (atrib and $F) shl 4;
      nchar := memory[f + $C800] + ((atrib and $40) shl 2);
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $10) <> 0, (atrib and $20) <> 0);
      if (atrib and $80) <> 0 then
        put_gfx_block_trans(x * 8, y * 8, 3, 8, 8)
      else
        put_gfx_mask_flip(x * 8, y * 8, nchar, color, 3, 0, 0, $F, (atrib and $10) <> 0, (atrib and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // hacer el scroll independiente linea a linea
  scroll__x_part2(1, 2, 8, @scroll_lineas);
  // sprites
  for f := 0 to $2F do
  begin
    atrib2 := $D000 + banco_sprites + (f * 4);
    atrib := memory[1 + atrib2];
    nchar := memory[atrib2] + (atrib and $40) shl 2;
    color := (atrib and $F) shl 4;
    x := memory[2 + atrib2] + (atrib and $80) shl 1;
    y := memory[3 + atrib2];
    put_gfx_sprite_mask(nchar, color, (atrib and $10) <> 0, (atrib and $20) <> 0, 1, 0, $F);
    update_gfx_sprite(x, y, 2, 1);
  end;
  scroll__x_part2(3, 2, 8, @scroll_lineas);
  update_final_piece(8, 16, 240, 224, 2);
end;

procedure eventos_gberet;
begin
  if event.arcade then
  begin
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
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
  end;
end;

procedure gberet_loop;
var
  f, ticks_mask: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 255 do
    begin
      if f = 240 then
        update_video_gberet;
      if (f and $F) = 0 then
      begin // every 16 scanlines
        ticks_mask := not(interrupt_ticks) and (interrupt_ticks + 1); // 0->1
        interrupt_ticks := interrupt_ticks + 1;
        // NMI on d0
        if (ticks_mask and interrupt_mask and 1) <> 0 then
          z80_0.change_nmi(ASSERT_LINE);
        // IRQ on d4
        if (ticks_mask and (interrupt_mask shl 2) and 8) <> 0 then
          z80_0.change_irq(ASSERT_LINE);
        if (ticks_mask and (interrupt_mask shl 2) and 16) <> 0 then
          z80_0.change_irq(ASSERT_LINE);
      end;
      z80_0.run(frame_main);
      frame_main := frame_main + z80_0.tframes - z80_0.contador;
    end;
    eventos_gberet;
    video_sync;
  end;
end;

function gberet_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $E03F:
      gberet_getbyte := memory[direccion];
    $F200:
      gberet_getbyte := marcade.dswb;
    $F400:
      gberet_getbyte := marcade.dswc;
    $F600:
      gberet_getbyte := marcade.dswa;
    $F601:
      gberet_getbyte := $FF;
    $F602:
      gberet_getbyte := marcade.in0;
    $F603:
      gberet_getbyte := marcade.in1;
    $F800 .. $FFFF:
      gberet_getbyte := memoria_rom[rom_bank, direccion and $7FF];
  end;
end;

procedure gberet_putbyte(direccion: word; valor: byte);
var
  ack_mask: byte;
begin
  case direccion of
    0 .. $BFFF, $F800 .. $FFFF:
      ; // ROM
    $C000 .. $CFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $D000 .. $DFFF:
      memory[direccion] := valor;
    $E000 .. $E01F:
      begin
        scroll_lineas[direccion and $1F] := (scroll_lineas[direccion and $1F] and $100) or valor;
        memory[direccion] := valor;
      end;
    $E020 .. $E03F:
      begin
        scroll_lineas[direccion and $1F] := (scroll_lineas[direccion and $1F] and $FF) or ((valor and 1) shl 8);
        memory[direccion] := valor;
      end;
    $E043:
      banco_sprites := (valor and 8) shl 5;
    $E044:
      begin
        // bits 0/1/2 = interrupt enable
        ack_mask := not(valor) and interrupt_mask; // 1->0
        if (ack_mask and 1) <> 0 then
          z80_0.change_nmi(CLEAR_LINE);
        if (ack_mask and 6) <> 0 then
          z80_0.change_irq(CLEAR_LINE);
        interrupt_mask := valor and 7;
        // bit 3 = flip screen
        main_screen.flip_main_screen := (valor and 8) <> 0;
      end;
    $F000:
      rom_bank := (valor and $E0) shr 5;
    $F200:
      sound_latch := valor;
    $F400:
      sn_76496_0.Write(sound_latch);
  end;
end;

procedure gberet_sound_update;
begin
  sn_76496_0.update;
end;

procedure gberet_hi_score;
begin
  if ((memory[$DB06] = 3) and (memory[$DB07] = $30) and (memory[$DB08] = 0) and (memory[$DB0B] = $1C)) then
  begin
    load_hi('gberet.hi', @memory[$D900], 60);
    copymemory(@memory[$DB06], @memory[$D900], 3);
    timers.enabled(timer_hs, false);
  end;
end;

procedure gberet_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    17:
      open_qsnapshot_save('gberet' + nombre);
    203:
      open_qsnapshot_save('mrgoemon' + nombre);
  end;
  getmem(data, 200);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := sn_76496_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[$C000], $4000);
  // MISC
  savedata_qsnapshot(@scroll_lineas, $20 * 2);
  buffer[0] := interrupt_mask;
  buffer[1] := interrupt_ticks;
  buffer[2] := sound_latch;
  buffer[3] := banco_sprites and $FF;
  buffer[4] := banco_sprites shr 8;
  buffer[5] := rom_bank;
  savedata_qsnapshot(@buffer, 6);
  freemem(data);
  close_qsnapshot;
end;

procedure gberet_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    17:
      if not(open_qsnapshot_load('gberet' + nombre)) then
        exit;
    203:
      if not(open_qsnapshot_load('mrgoemon' + nombre)) then
        exit;
  end;
  getmem(data, 200);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  sn_76496_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory[$C000]);
  loaddata_qsnapshot(@scroll_lineas);
  loaddata_qsnapshot(@buffer);
  // MISC
  interrupt_mask := buffer[0];
  interrupt_ticks := buffer[1];
  sound_latch := buffer[2];
  banco_sprites := buffer[3];
  banco_sprites := banco_sprites or (buffer[4] shl 8);
  rom_bank := buffer[5];
  freemem(data);
  close_qsnapshot;
  fillchar(gfx[0].buffer, $800, 1);
end;

// Main
procedure reset_gberet;
begin
  z80_0.reset;
  frame_main := z80_0.tframes;
  sn_76496_0.reset;
 reset_game_general;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  banco_sprites := 0;
  interrupt_mask := 0;
  interrupt_ticks := 0;
  sound_latch := 0;
  fillchar(scroll_lineas[0], $20, 0);
  rom_bank := 0;
end;

procedure close_gberet;
begin
  if main_vars.machine_type = 17 then
    save_hi('gberet.hi', @memory[$D900], 60);
end;

function start_greenberet: boolean;
var
  colores: tpaleta;
  f: word;
  ctemp1: byte;
  memoria_temp: array [0 .. $FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 32 * 8 + 0 * 4, 32 * 8 + 1 * 4, 32 * 8 + 2 * 4, 32 * 8 + 3 * 4, 32 * 8 + 4 * 4, 32 * 8 + 5 * 4, 32 * 8 + 6 * 4, 32 * 8 + 7 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 64 * 8 + 0 * 32, 64 * 8 + 1 * 32, 64 * 8 + 2 * 32, 64 * 8 + 3 * 32, 64 * 8 + 4 * 32, 64 * 8 + 5 * 32, 64 * 8 + 6 * 32, 64 * 8 + 7 * 32);
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, 512);
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memoria_temp, @ps_x, @ps_y, false, false);
  end;
  procedure convert_sprites;
  begin
    init_gfx(1, 16, 16, 512);
    gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
    convert_gfx(1, 0, @memoria_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := gberet_loop;
  machine_calls.close := close_gberet;
  machine_calls.reset := reset_gberet;
  machine_calls.fps_max := 60.60606060;
  machine_calls.save_qsnap := gberet_qsave;
  machine_calls.load_qsnap := gberet_qload;
  start_greenberet := false;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 512, 256, false, true);
  screen_init(3, 512, 256, true, false);
  screen_mod_scroll(3, 512, 256, 511, 256, 256, 255);
  start_video(240, 224);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 256);
  z80_0.change_ram_calls(gberet_getbyte, gberet_putbyte);
  z80_0.init_sound(gberet_sound_update);
  // Sound Chips
  sn_76496_0 := sn76496_chip.create(1536000);
  case main_vars.machine_type of
    17:
      begin // Green Beret
        // Timers
        timer_hs := timers.init(z80_0.numero_cpu, 10000, gberet_hi_score, nil, true);
        // cargar roms
        if not(roms_load(@memory, gberet_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memoria_temp, gberet_char)) then
          exit;
        convert_chars;
        // convertir sprites
        if not(roms_load(@memoria_temp, gberet_sprites)) then
          exit;
        convert_sprites;
        // poner la paleta
        if not(roms_load(@memoria_temp, gberet_pal)) then
          exit;
        marcade.dswb_val2 := @gberet_dip_b;
      end;
    203:
      begin // Mr. Goemon
        if not(roms_load(@memoria_temp, mrgoemon_rom)) then
          exit;
        copymemory(@memory, @memoria_temp, $C000);
        for f := 0 to 7 do
          copymemory(@memoria_rom[f, 0], @memoria_temp[$C000 + (f * $800)], $800);
        // convertir chars
        if not(roms_load(@memoria_temp, mrgoemon_char)) then
          exit;
        convert_chars;
        // convertir sprites
        if not(roms_load(@memoria_temp, mrgoemon_sprites)) then
          exit;
        convert_sprites;
        // poner la paleta
        if not(roms_load(@memoria_temp, mrgoemon_pal)) then
          exit;
        marcade.dswb_val2 := @mrgoemon_dip_b;
      end;
  end;
  for f := 0 to 31 do
  begin
    ctemp1 := memoria_temp[f];
    colores[f].r := $21 * (ctemp1 and 1) + $47 * ((ctemp1 shr 1) and 1) + $97 * ((ctemp1 shr 2) and 1);
    colores[f].g := $21 * ((ctemp1 shr 3) and 1) + $47 * ((ctemp1 shr 4) and 1) + $97 * ((ctemp1 shr 5) and 1);
    colores[f].b := 0 + $47 * ((ctemp1 shr 6) and 1) + $97 * ((ctemp1 shr 7) and 1);
  end;
  set_pal(colores, 32);
  // Poner el CLUT
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := memoria_temp[$20 + f] + $10;
    gfx[1].colores[f] := memoria_temp[$120 + f] and $F;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $4A;
  marcade.dswc := $FF;
  marcade.dswa_val2 := @gberet_dip_a;
  marcade.dswc_val2 := @gberet_dip_c;
  // final
  reset_gberet;
  start_greenberet := true;
end;

end.
