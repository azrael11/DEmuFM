unit returnofinvaders_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  timer_engine,
  sound_engine,
  sn_76496,
  misc_functions,
  taito_68705;

function start_returnoftheinvaders: boolean;

implementation

const
  retofinv_rom: array [0 .. 2] of tipo_roms = ((n: 'a37__03.ic70'; l: $2000; p: 0; crc: $EAE7459D), (n: 'a37__02.ic71'; l: $2000; p: $2000; crc: $72895E37), (n: 'a37__01.ic72'; l: $2000; p: $4000; crc: $505DD20B));
  retofinv_sub: tipo_roms = (n: 'a37__04.ic62'; l: $2000; p: 0; crc: $D2899CC1);
  retofinv_snd: tipo_roms = (n: 'a37__05.ic17'; l: $2000; p: 0; crc: $9025ABEA);
  retofinv_mcu: tipo_roms = (n: 'a37__09.ic37'; l: $800; p: 0; crc: $6A6D008D);
  retofinv_char: tipo_roms = (n: 'a37__16.gfxboard.ic61'; l: $2000; p: 0; crc: $4E3F501C);
  retofinv_tiles: array [0 .. 1] of tipo_roms = ((n: 'a37__14.gfxboard.ic55'; l: $2000; p: 0; crc: $EF7F8651), (n: 'a37__15.gfxboard.ic56'; l: $2000; p: $2000; crc: $03B40905));
  retofinv_sprites: array [0 .. 3] of tipo_roms = ((n: 'a37__10.gfxboard.ic8'; l: $2000; p: 0; crc: $6AFDEEC8), (n: 'a37__11.gfxboard.ic9'; l: $2000; p: $2000; crc: $D3DC9DA3), (n: 'a37__12.gfxboard.ic10'; l: $2000; p: $4000; crc: $D10B2EED), (n: 'a37__13.gfxboard.ic11';
    l: $2000; p: $6000; crc: $00CA6B3D));
  retofinv_proms: array [0 .. 2] of tipo_roms = ((n: 'a37-06.ic13'; l: $100; p: 0; crc: $E9643B8B), (n: 'a37-07.ic4'; l: $100; p: $100; crc: $E8F34E11), (n: 'a37-08.ic3'; l: $100; p: $200; crc: $50030AF0));
  retofinv_clut: array [0 .. 3] of tipo_roms = ((n: 'a37-17.gfxboard.ic36'; l: $400; p: 0; crc: $C63CF10E), (n: 'a37-18.gfxboard.ic37'; l: $400; p: $800; crc: $6DB07BD1), (n: 'a37-19.gfxboard.ic83'; l: $400; p: $400; crc: $A92AEA27), (n: 'a37-20.gfxboard.ic84'; l: $400; p: $C00;
    crc: $77A7AAF6));
  // Dip
  retofinv_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Bonus Life'; number: 4; val4: (3, 2, 1, 0); name4: ('30K 80K 80K+', '30K 80K', '30K', 'None')), (mask: 4; name: 'Free Play'; number: 2; val2: (4, 0); name2: ('No', 'Yes')), (mask: $18; name: 'Lives'; number: 4;
    val4: ($18, $10, 8, 0); name4: ('1', '2', '3', '5')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2; val2: (0, $80); name2: ('Upright', 'Cocktail')), ());
  retofinv_dip_b: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: ($F, $E, $D, $C, $B, $A, 9, 8, 0, 1, 2, 3, 4, 5, 6, 7);
    name16: ('9C 1C', '8C 1C', '7C 1C', '6C 1C', '5C 1C', '4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', '1C 8C')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($F0, $E0, $D0, $C0, $B0, $A0, $90, $80, 0, $10, $20, $30, $40, $50, $60, $70); name16: ('9C 1C', '8C 1C', '7C 1C', '6C 1C', '5C 1C', '4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', '1C 8C')), ());
  retofinv_dip_c: array [0 .. 5] of def_dip2 = ((mask: 1; name: 'Push Start to Skip Stage'; number: 2; val2: (1, 0); name2: ('Off', 'On')), (mask: $10; name: 'Coin Per Play Display'; number: 2; val2: (0, $10); name2: ('No', 'Yes')), (mask: $20; name: 'Year Display'; number: 2;
    val2: (0, $20); name2: ('No', 'Yes')), (mask: $40; name: 'Invulnerability'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Coinage'; number: 2; val2: ($80, 0); name2: ('A and B', 'A only')), ());

var
  sound_latch, sound_return, bg_bank, fg_bank: byte;
  main_vblank, sub_vblank: boolean;

procedure update_video_retofinv;
var
  f, nchar, x, y, offs, color: word;
  size_sprite, atrib, sx, sy: byte;
  flip_x, flip_y: boolean;
begin
  for x := 0 to 27 do
  begin
    for y := 0 to 35 do
    begin
      sx := 29 - x;
      sy := y - 2;
      if (sy and $20) <> 0 then
        offs := sx + ((sy and $1F) shl 5)
      else
        offs := sy + (sx shl 5);
      if gfx[0].buffer[offs] then
      begin
        color := (memory[$8400 + offs]) shl 1;
        put_gfx_mask(x * 8, y * 8, memory[$8000 + offs] + (fg_bank shl 8), color, 1, 0, 0, $FF);
        gfx[0].buffer[offs] := false;
      end;
      if gfx[1].buffer[offs] then
      begin
        color := ((memory[$A400 + offs]) and $3F) shl 4;
        put_gfx(x * 8, y * 8, memory[$A000 + offs] + (bg_bank shl 8), $400 + color, 2, 1);
        gfx[1].buffer[offs] := false;
      end;
    end;
  end;
  update_region(0, 0, 224, 288, 2, 0, 0, 224, 288, 3);
  for f := 0 to $3F do
  begin
    nchar := memory[$8F80 + (f * 2)];
    color := (memory[$8F81 + (f * 2)] and $3F) shl 4;
    atrib := memory[$9F80 + (f * 2)];
    size_sprite := (atrib and $C) shr 2;
    if not(main_screen.flip_main_screen) then
    begin
      y := ((memory[$9781 + (f * 2)] shl 1) + ((memory[$9F81 + (f * 2)] and $80) shr 7)) - 39;
      x := ((memory[$9780 + (f * 2)] shl 1) + ((atrib and $80) shr 7)) - 17;
    end
    else
    begin
      y := 255 - ((memory[$9781 + (f * 2)] shl 1) + ((memory[$9F81 + (f * 2)] and $80) shr 7)) + 56;
      x := 255 - ((memory[$9780 + (f * 2)] shl 1) + ((atrib and $80) shr 7)) - 31;
      if (size_sprite <> 0) then
      begin
        x := x - 16;
        if (size_sprite <> 1) then
          y := y - 16;
      end;
    end;
    flip_x := (atrib and 2) <> 0;
    flip_y := (atrib and 1) <> 0;
    nchar := nchar and not(size_sprite);
    case size_sprite of
      0:
        begin // 16x16
          put_gfx_sprite_mask(nchar, color, flip_x, flip_y, 2, $FF, $FF);
          update_gfx_sprite(x, y, 3, 2);
        end;
      1:
        begin // 32x16
          put_gfx_sprite_mask_diff(nchar + 2, color, flip_x, flip_y, 2, $FF, $FF, 0, 0);
          put_gfx_sprite_mask_diff(nchar, color, flip_x, flip_y, 2, $FF, $FF, 16, 0);
          actualiza_gfx_sprite_size(x, y, 3, 32, 16);
        end;
      2:
        begin // 16x32
          put_gfx_sprite_mask_diff(nchar, color, flip_x, flip_y, 2, $FF, $FF, 0, 0);
          put_gfx_sprite_mask_diff(nchar + 1, color, flip_x, flip_y, 2, $FF, $FF, 0, 16);
          actualiza_gfx_sprite_size(x, y, 3, 16, 32);
        end;
      3:
        begin // 32x32;
          put_gfx_sprite_mask_diff(nchar + 2, color, flip_x, flip_y, 2, $FF, $FF, 0, 0);
          put_gfx_sprite_mask_diff(nchar, color, flip_x, flip_y, 2, $FF, $FF, 16, 0);
          put_gfx_sprite_mask_diff(nchar + 3, color, flip_x, flip_y, 2, $FF, $FF, 0, 16);
          put_gfx_sprite_mask_diff(nchar + 1, color, flip_x, flip_y, 2, $FF, $FF, 16, 16);
          actualiza_gfx_sprite_size(x, y, 3, 32, 32);
        end;
    end;
  end;
  update_region(0, 0, 224, 288, 1, 0, 0, 224, 288, 3);
  update_final_piece(0, 0, 224, 288, 3);
end;

procedure events_retofinv;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $7F
    else
      marcade.in0 := marcade.in0 or $80;
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
    // botones 3
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := marcade.in2 and $FE
    else
      marcade.in2 := marcade.in2 or 1;
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := marcade.in2 and $FD
    else
      marcade.in2 := marcade.in2 or 2;
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := marcade.in2 or $10
    else
      marcade.in2 := marcade.in2 and $EF;
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := marcade.in2 or $20
    else
      marcade.in2 := marcade.in2 and $DF;

  end;
end;

procedure retofinv_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 223 do
      begin
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // Sub
        z80_1.run(frame_sub);
        frame_sub := frame_sub + z80_1.tframes - z80_1.contador;
        // Sound
        z80_2.run(frame_snd);
        frame_snd := frame_snd + z80_2.tframes - z80_2.contador;
        // mcu
        taito_68705_0.run;
      end;
      update_video_retofinv;
      events_retofinv;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function getbyte_retofinv(direccion: word): byte;
begin
  case direccion of
    0 .. $A7FF:
      getbyte_retofinv := memory[direccion];
    $C000:
      getbyte_retofinv := marcade.in0;
    $C001:
      getbyte_retofinv := marcade.in1;
    $C002:
      getbyte_retofinv := 0; // Debe devolve 0 o se resetea
    $C003:
      getbyte_retofinv := (byte(not(taito_68705_0.main_sent)) shl 4) or (byte(taito_68705_0.mcu_sent) shl 5);
    $C004:
      getbyte_retofinv := marcade.in2;
    $C005:
      getbyte_retofinv := marcade.dswa;
    $C006:
      getbyte_retofinv := marcade.dswb;
    $C007:
      getbyte_retofinv := marcade.dswc;
    $E000:
      getbyte_retofinv := taito_68705_0.read;
    $F800:
      getbyte_retofinv := sound_return;
  end;
end;

procedure putbyte_retofinv(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $8800 .. $9FFF:
      memory[direccion] := valor;
    $A000 .. $A7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[1].buffer[direccion and $3FF] := true;
      end;
    $B800:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $B801:
      begin
        fg_bank := (valor and 1);
        fillchar(gfx[0].buffer, $400, 1);
      end;
    $B802:
      begin
        bg_bank := (valor and 1);
        fillchar(gfx[1].buffer, $400, 1);
      end;
    $C800:
      begin
        main_vblank := (valor and 1) <> 0;
        if not(main_vblank) then
          z80_0.change_irq(CLEAR_LINE);
      end;
    $C801, $D000:
      ; // coinlockout + watch dog
    $C802:
      if valor = 0 then
        z80_2.change_reset(ASSERT_LINE)
      else
        z80_2.change_reset(CLEAR_LINE);
    $C803:
      if valor = 0 then
        taito_68705_0.change_reset(ASSERT_LINE)
      else
        taito_68705_0.change_reset(CLEAR_LINE);
    $C805:
      if valor = 0 then
        z80_1.change_reset(ASSERT_LINE)
      else
        z80_1.change_reset(CLEAR_LINE);
    $D800:
      begin
        sound_latch := valor;
        z80_2.change_irq(HOLD_LINE);
      end;
    $E800:
      taito_68705_0.write(valor);
  end;
end;

function getbyte_sub_retofinv(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      getbyte_sub_retofinv := mem_misc[direccion];
    $8000 .. $A7FF:
      getbyte_sub_retofinv := memory[direccion];
  end;
end;

procedure putbyte_sub_retofinv(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $8000 .. $A7FF:
      putbyte_retofinv(direccion, valor);
    $C804:
      begin
        sub_vblank := (valor and 1) <> 0;
        if not(sub_vblank) then
          z80_1.change_irq(CLEAR_LINE);
      end;
  end;
end;

function getbyte_snd_retofinv(direccion: word): byte;
begin
  case direccion of
    0 .. $27FF, $E000 .. $FFFF:
      getbyte_snd_retofinv := mem_snd[direccion];
    $4000:
      getbyte_snd_retofinv := sound_latch;
  end;
end;

procedure putbyte_snd_retofinv(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF, $E000 .. $FFFF:
      ; // ROM
    $2000 .. $27FF:
      mem_snd[direccion] := valor;
    $6000:
      sound_return := valor;
    $8000:
      sn_76496_0.write(valor);
    $A000:
      sn_76496_1.write(valor);
  end;
end;

procedure retofinv_sound_update;
begin
  sn_76496_0.update;
  sn_76496_1.update;
end;

procedure retofinv_snd_nmi;
begin
  z80_2.change_nmi(PULSE_LINE);
end;

// Main
procedure reset_retofinv;
begin
  z80_0.reset;
  z80_1.reset;
  z80_2.reset;
  frame_main := z80_0.tframes;
  frame_sub := z80_1.tframes;
  frame_snd := z80_2.tframes;
  taito_68705_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $CF;
  sound_latch := 0;
  sound_return := 0;
  bg_bank := 0;
  fg_bank := 0;
  main_vblank := false;
  sub_vblank := false;
end;

function start_returnoftheinvaders: boolean;
const
  pc_x: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $7FFF] of byte;
begin
  start_returnoftheinvaders := false;
  machine_calls.general_loop := retofinv_loop;
  machine_calls.reset := reset_retofinv;
  machine_calls.fps_max := 60.58;
  start_returnoftheinvaders := false;
  start_audio(false);
  screen_init(1, 224, 288, true);
  screen_init(2, 224, 288);
  screen_init(3, 512, 512, false, true);
  start_video(224, 288);
  // Main CPU
  z80_0 := cpu_z80.create(18432000 div 6, 224);
  z80_0.change_ram_calls(getbyte_retofinv, putbyte_retofinv);
  if not(roms_load(@memory, retofinv_rom)) then
    exit;
  // Sub
  z80_1 := cpu_z80.create(18432000 div 6, 224);
  z80_1.change_ram_calls(getbyte_sub_retofinv, putbyte_sub_retofinv);
  if not(roms_load(@mem_misc, retofinv_sub)) then
    exit;
  // Sound CPU
  z80_2 := cpu_z80.create(18432000 div 6, 224);
  z80_2.change_ram_calls(getbyte_snd_retofinv, putbyte_snd_retofinv);
  z80_2.init_sound(retofinv_sound_update);
  timers.init(z80_2.numero_cpu, (18432000 div 6) / (2 * 60), retofinv_snd_nmi, nil, true);
  if not(roms_load(@mem_snd, retofinv_snd)) then
    exit;
  // MCU CPU
  taito_68705_0 := taito_68705p.create(18432000 div 6, 224);
  if not(roms_load(taito_68705_0.get_rom_addr, retofinv_mcu)) then
    exit;
  // Sound Chips
  sn_76496_0 := sn76496_chip.create(18432000 div 6);
  sn_76496_1 := sn76496_chip.create(18432000 div 6);
  // Cargar chars
  if not(roms_load(@memory_temp, retofinv_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx_set_desc_data(1, 0, 8 * 8, 0);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, true, false);
  // Cargar tiles
  if not(roms_load(@memory_temp, retofinv_tiles)) then
    exit;
  init_gfx(1, 8, 8, $200);
  gfx_set_desc_data(4, 0, 16 * 8, 0, ($200 * 16 * 8) + 4, $200 * 16 * 8, 4);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // sprites
  if not(roms_load(@memory_temp, retofinv_sprites)) then
    exit;
  init_gfx(2, 16, 16, $100);
  gfx_set_desc_data(4, 0, 64 * 8, 0, ($100 * 64 * 8) + 4, $100 * 64 * 8, 4);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // pal
  if not(roms_load(@memory_temp, retofinv_proms)) then
    exit;
  for f := 0 to $FF do
  begin
    colores[f].r := pal4bit(memory_temp[f + 0]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
  end;
  set_pal(colores, $100);
  // CLUT
  if not(roms_load(@memory_temp, retofinv_clut)) then
    exit;
  for f := 0 to $1FF do
  begin
    if (f and 1) <> 0 then
      gfx[0].colores[f] := f shr 1
    else
      gfx[0].colores[f] := 0;
  end;
  for f := 0 to $7FF do
    memory_temp[$1000 + f] := ((memory_temp[f] and $F) shl 4) or (memory_temp[$800 + f] and $F);
  for f := 0 to $7FF do
  begin
    gfx[1].colores[f] := memory_temp[$1000 + bitswap16(f, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 0, 1, 2)];
    gfx[2].colores[f] := memory_temp[$1000 + bitswap16(f, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 0, 1, 2)];
  end;
  // Dip
  marcade.dswa := $6F;
  marcade.dswb := 0;
  marcade.dswc := $FF;
  marcade.dswa_val2 := @retofinv_dip_a;
  marcade.dswb_val2 := @retofinv_dip_b;
  marcade.dswc_val2 := @retofinv_dip_c;
  // final
  reset_retofinv;
  start_returnoftheinvaders := true;
end;

end.
