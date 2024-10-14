unit skykid_hw;

interface

uses
  WinApi.Windows,
  m6809,
  m680x,
  namco_snd,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  misc_functions,
  sound_engine;

function start_skykid: boolean;

implementation

const
  // Sky Kid
  skykid_rom: array [0 .. 2] of tipo_roms = ((n: 'sk2_2.6c'; l: $4000; p: $0; crc: $EA8A5822), (n: 'sk1-1c.6b'; l: $4000; p: $4000; crc: $7ABE6C6C), (n: 'sk1_3.6d'; l: $4000; p: $8000;
    crc: $314B8765));
  skykid_char: tipo_roms = (n: 'sk1_6.6l'; l: $2000; p: 0; crc: $58B731B9);
  skykid_tiles: tipo_roms = (n: 'sk1_5.7e'; l: $2000; p: 0; crc: $C33A498E);
  skykid_sprites: array [0 .. 1] of tipo_roms = ((n: 'sk1_8.10n'; l: $4000; p: 0; crc: $44BB7375), (n: 'sk1_7.10m'; l: $4000; p: $4000; crc: $3454671D));
  skykid_mcu: array [0 .. 1] of tipo_roms = ((n: 'sk2_4.3c'; l: $2000; p: $1000; crc: $A460D0E0), (n: 'cus63-63a1.mcu'; l: $1000; p: 0; crc: $6EF08FB3));
  skykid_prom: array [0 .. 4] of tipo_roms = ((n: 'sk1-1.2n'; l: $100; p: $0; crc: $0218E726), (n: 'sk1-2.2p'; l: $100; p: $100; crc: $FC0D5B85), (n: 'sk1-3.2r'; l: $100; p: $200; crc: $D06B620B),
    (n: 'sk1-4.5n'; l: $200; p: $300; crc: $C697AC72), (n: 'sk1-5.6n'; l: $200; p: $500; crc: $161514A4));
  // Dragon Buster
  drgnbstr_rom: array [0 .. 2] of tipo_roms = ((n: 'db1_2b.6c'; l: $4000; p: $0; crc: $0F11CD17), (n: 'db1_1.6b'; l: $4000; p: $4000; crc: $1C7C1821), (n: 'db1_3.6d'; l: $4000; p: $8000;
    crc: $6DA169AE));
  drgnbstr_char: tipo_roms = (n: 'db1_6.6l'; l: $2000; p: 0; crc: $C080B66C);
  drgnbstr_tiles: tipo_roms = (n: 'db1_5.7e'; l: $2000; p: 0; crc: $28129AED);
  drgnbstr_sprites: array [0 .. 1] of tipo_roms = ((n: 'db1_8.10n'; l: $4000; p: 0; crc: $11942C61), (n: 'db1_7.10m'; l: $4000; p: $4000; crc: $CC130FE2));
  drgnbstr_mcu: array [0 .. 1] of tipo_roms = ((n: 'db1_4.3c'; l: $2000; p: $1000; crc: $8A0B1FC1), (n: 'cus60-60a1.mcu'; l: $1000; p: 0; crc: $076EA82A));
  drgnbstr_prom: array [0 .. 4] of tipo_roms = ((n: 'db1-1.2n'; l: $100; p: $0; crc: $3F8CCE97), (n: 'db1-2.2p'; l: $100; p: $100; crc: $AFE32436), (n: 'db1-3.2r'; l: $100; p: $200; crc: $C95FF576),
    (n: 'db1-4.5n'; l: $200; p: $300; crc: $B2180C21), (n: 'db1-5.6n'; l: $200; p: $500; crc: $5E2B3F74));

var
  rom_bank: array [0 .. 1, 0 .. $1FFF] of byte;
  rom_nbank, scroll_y, inputport_selected, priority: byte;
  scroll_x: word;
  irq_enable, irq_enable_mcu, screen_flip: boolean;

procedure update_video_skykid;
  procedure draw_sprites;
  var
    nchar, color, x: word;
    f, flipx_v, flipy_v, atrib, y, size, a, b, c, d, mix: byte;
    flipx, flipy: boolean;
  begin
    for f := 0 to $3F do
    begin
      atrib := memory[$5F80 + (f * 2)];
      nchar := memory[$4F80 + (f * 2)] + ((atrib and $80) shl 1);
      color := ((memory[$4F81 + (f * 2)] and $3F) shl 3) + $200;
      flipx_v := not(atrib) and $01;
      flipy_v := not(atrib) and $02;
      flipx := (flipx_v = 0);
      flipy := (flipy_v = 0);
      if screen_flip then
      begin
        x := ((memory[$5781 + (f * 2)] + (memory[$5F81 + (f * 2)] and 1) shl 8) - 71) and $1FF;
        y := ((256 - memory[$5780 + (f * 2)] - 7) and $FF) - 32;
        if (atrib and $8) <> 0 then
          y := y - 16;
      end
      else
      begin
        x := (327 - (memory[$5781 + (f * 2)] + (memory[$5F81 + (f * 2)] and 1) shl 8)) and $1FF;
        y := (memory[$5780 + (f * 2)]) - 9;
        if (atrib and $4) = 0 then
          x := x + 16;
      end;
      size := (atrib and $C) shr 2;
      case size of
        0:
          begin // 16x16
            put_gfx_sprite_mask(nchar, color, flipx, flipy, 1, $F, $F);
            update_gfx_sprite(x, y, 3, 1);
          end;
        1:
          begin // 32x16
            a := 1 xor flipx_v;
            b := 0 xor flipx_v;
            put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, 15, $F, 0, 0);
            put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, 15, $F, 16, 0);
            actualiza_gfx_sprite_size(x, y, 3, 32, 16);
          end;
        2:
          begin // 16x32
            a := 2 xor flipy_v;
            b := 0 xor flipy_v;
            put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, 15, $F, 0, 0);
            put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, 15, $F, 0, 16);
            actualiza_gfx_sprite_size(x, y, 3, 16, 32);
          end;
        3:
          begin // 32x32
            if flipx then
            begin
              a := 1;
              b := 0;
              c := 3;
              d := 2
            end
            else
            begin
              a := 0;
              b := 1;
              c := 2;
              d := 3;
            end;
            if flipy then
            begin
              mix := a;
              a := c;
              c := mix;
              mix := b;
              b := d;
              d := mix;
            end;
            put_gfx_sprite_mask_diff(nchar + a, color, flipx, flipy, 1, 15, $F, 0, 0);
            put_gfx_sprite_mask_diff(nchar + b, color, flipx, flipy, 1, 15, $F, 16, 0);
            put_gfx_sprite_mask_diff(nchar + c, color, flipx, flipy, 1, 15, $F, 0, 16);
            put_gfx_sprite_mask_diff(nchar + d, color, flipx, flipy, 1, 15, $F, 16, 16);
            actualiza_gfx_sprite_size(x, y, 3, 32, 32);
          end;
      end;
    end;
  end;

var
  f, color, nchar, offs: word;
  x, y, sx, sy, atrib: byte;
begin
  for x := 0 to 27 do
  begin
    for y := 0 to 35 do
    begin
      sx := x + 2;
      sy := y - 2;
      if (sy and $20) <> 0 then
        offs := sx + ((sy and $1F) shl 5)
      else
        offs := sy + (sx shl 5);
      if gfx[0].buffer[offs] then
      begin
        color := ((memory[$4400 + offs]) and $3F) shl 2;
        put_gfx_trans(y * 8, x * 8, memory[$4000 + offs], color, 1, 0);
        gfx[0].buffer[offs] := false;
      end;
    end;
  end;
  for f := 0 to $7FF do
  begin
    if gfx[2].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$2800 + f];
      color := (((atrib and $7E) shr 1) + ((atrib and $01) shl 6)) shl 2;
      nchar := memory[$2000 + f] + (atrib and $1) shl 8;
      put_gfx(x * 8, y * 8, nchar, color, 2, 2);
      gfx[2].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x, scroll_y);
  if (priority and $F0) <> $50 then
  begin
    draw_sprites;
    update_region(0, 0, 288, 224, 1, 0, 0, 288, 224, 3);
  end
  else
  begin
    update_region(0, 0, 288, 224, 1, 0, 0, 288, 224, 3);
    draw_sprites;
  end;
  update_final_piece(0, 0, 288, 224, 3);
end;

procedure events_skykid;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in3 := (marcade.in3 and $F7)
    else
      marcade.in3 := (marcade.in3 or $8);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in3 := (marcade.in3 and $FB)
    else
      marcade.in3 := (marcade.in3 or $4);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure skykid_loop;
var
  f: byte;
  frame_m, frame_mcu: single;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_mcu := m6800_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 223 do
      begin
        // Main CPU
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Sound CPU
        m6800_0.run(frame_mcu);
        frame_mcu := frame_mcu + m6800_0.tframes - m6800_0.contador;
      end;
      if irq_enable then
        m6809_0.change_irq(ASSERT_LINE);
      if irq_enable_mcu then
        m6800_0.change_irq(ASSERT_LINE);
      update_video_skykid;
      events_skykid;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function skykid_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $1FFF:
      skykid_getbyte := rom_bank[rom_nbank, direccion];
    $2000 .. $2FFF, $4000 .. $5FFF, $8000 .. $FFFF:
      skykid_getbyte := memory[direccion];
    $6800 .. $6BFF:
      skykid_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
  end;
end;

procedure skykid_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $1FFF, $A002 .. $FFFF:
      ; // ROM
    $2000 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $47FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $4800 .. $5FFF:
      memory[direccion] := valor;
    $6000 .. $60FF:
      begin
        scroll_y := direccion and $FF;
        if screen_flip then
          scroll_y := (scroll_y + 25) and $FF
        else
          scroll_y := (7 - scroll_y) and $FF;
      end;
    $6200 .. $63FF:
      begin
        scroll_x := direccion and $1FF;
        if screen_flip then
          scroll_x := (scroll_x + 35) and $1FF
        else
          scroll_x := (189 - (scroll_x xor 1)) and $1FF;
      end;
    $6800 .. $6BFF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $7000 .. $7FFF:
      begin
        irq_enable := not(BIT((direccion and $FFF), 11));
        if not(irq_enable) then
          m6809_0.change_irq(CLEAR_LINE);
      end;
    $8000 .. $8FFF:
      if not(BIT((direccion and $FFF), 11)) then
        m6800_0.reset;
    $9000 .. $9FFF:
      rom_nbank := (not(BIT_n((direccion and $FFF), 11))) and 1;
    $A000 .. $A001:
      priority := valor;
  end;
end;

function mcu_getbyte(direccion: word): byte;
begin
  case direccion of
    $1000 .. $13FF:
      mcu_getbyte := namco_snd_0.namcos1_cus30_r(direccion and $3FF);
    $8000 .. $C7FF:
      mcu_getbyte := mem_snd[direccion];
  end;
end;

procedure mcu_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $1000 .. $13FF:
      namco_snd_0.namcos1_cus30_w(direccion and $3FF, valor);
    $4000 .. $7FFF:
      begin
        irq_enable_mcu := not(BIT(direccion and $3FFF, 13));
        if not(irq_enable_mcu) then
          m6800_0.change_irq(CLEAR_LINE);
      end;
    $8000 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF:
      mem_snd[direccion] := valor;
  end;
end;

procedure skykid_sound_update;
begin
  namco_snd_0.update;
end;

procedure out_port1(valor: byte);
begin
  if ((valor and $E0) = $60) then
    inputport_selected := valor and $07;
end;

function in_port1: byte;
begin
  case inputport_selected of
    $00:
      in_port1 := (($FF) and $F8) shr 3; // DSW B (bits 0-4) */
    $01:
      in_port1 := ((($FF) and $7) shl 2) or ((($FF) and $C0) shr 6);
    // DSW B (bits 5-7), DSW A (bits 0-1) */
    $02:
      in_port1 := (($FF) and $3E) shr 1; // DSW A (bits 2-6) */
    $03:
      in_port1 := ((($FF) and $1) shl 4) or (marcade.in3 and $F);
    // DSW A (bit 7), DSW C (bits 0-3) */
    $04:
      in_port1 := marcade.in2; // coins, start */
    $05:
      in_port1 := marcade.in1; // 2P controls */
    $06:
      in_port1 := marcade.in0; // 1P controls */
  else
    in_port1 := $FF;
  end;
end;

function in_port2: byte;
begin
  in_port2 := $FF;
end;

// Main
procedure reset_skykid;
begin
  m6809_0.reset;
  m6800_0.reset;
  namco_snd_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  marcade.in3 := $FF;
  rom_nbank := 0;
  irq_enable := false;
  irq_enable_mcu := false;
  scroll_y := 0;
  inputport_selected := 0;
  scroll_x := 0;
end;

function start_skykid: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $FFFF] of byte;
  ptemp: pbyte;
const
  pc_x: array [0 .. 7] of dword = (8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  pt_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3);
  pt_y: array [0 .. 7] of dword = (0 * 8, 2 * 8, 4 * 8, 6 * 8, 8 * 8, 10 * 8, 12 * 8, 14 * 8);
begin
  start_skykid := false;
  machine_calls.general_loop := skykid_loop;
  machine_calls.reset := reset_skykid;
  machine_calls.fps_max := 60.60606060606060;
  start_audio(false);
  screen_init(1, 288, 224, true);
  screen_init(2, 512, 256);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, false, true);
  start_video(288, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(1536000, 224, TCPU_M6809);
  m6809_0.change_ram_calls(skykid_getbyte, skykid_putbyte);
  // MCU CPU
  m6800_0 := cpu_m6800.Create(6144000, 224, TCPU_HD63701V);
  m6800_0.change_ram_calls(mcu_getbyte, mcu_putbyte);
  m6800_0.change_io_calls(in_port1, in_port2, nil, nil, out_port1, nil, nil, nil);
  m6800_0.init_sound(skykid_sound_update);
  namco_snd_0 := namco_snd_chip.Create(8, true);
  case main_vars.machine_type of
    123:
      begin // Skykid
        // cargar roms
        if not(roms_load(@memory_temp, skykid_rom)) then
          exit;
        // Pongo las ROMs en su banco
        copymemory(@memory[$8000], @memory_temp[$0], $8000);
        for f := 0 to 1 do
          copymemory(@rom_bank[f, 0], @memory_temp[$8000 + (f * $2000)], $2000);
        // Cargar MCU
        if not(roms_load(@memory_temp, skykid_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$8000], @memory_temp[$1000], $2000);
        // convertir chars
        if not(roms_load(@memory_temp, skykid_char)) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
        // sprites
        fillchar(memory_temp[0], $10000, 0);
        if not(roms_load(@memory_temp, skykid_sprites)) then
          exit;
        // unpack the third sprite ROM
        for f := 0 to $1FFF do
        begin
          memory_temp[f + $4000 + $4000] := memory_temp[f + $4000]; // sprite set #1, plane 3
          memory_temp[f + $6000 + $4000] := memory_temp[f + $4000] shr 4;
          // sprite set #2, plane 3
          memory_temp[f + $4000] := memory_temp[f + $2000 + $4000];
          // sprite set #3, planes 1&2 (plane 3 is empty)
        end;
        init_gfx(1, 16, 16, $200);
        gfx_set_desc_data(3, 0, 64 * 8, $200 * 64 * 8 + 4, 0, 4);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // tiles
        if not(roms_load(@memory_temp, skykid_tiles)) then
          exit;
        init_gfx(2, 8, 8, $200);
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
        // Paleta
        if not(roms_load(@memory_temp, skykid_prom)) then
          exit;
        screen_flip := false;
      end;
    194:
      begin // Dragon Blaster
        // cargar roms
        if not(roms_load(@memory_temp, drgnbstr_rom)) then
          exit;
        // Pongo las ROMs en su banco
        copymemory(@memory[$8000], @memory_temp[$0], $8000);
        for f := 0 to 1 do
          copymemory(@rom_bank[f, 0], @memory_temp[$8000 + (f * $2000)], $2000);
        // Cargar MCU
        if not(roms_load(@memory_temp, drgnbstr_mcu)) then
          exit;
        ptemp := m6800_0.get_rom_addr;
        copymemory(@ptemp[$1000], @memory_temp[0], $1000);
        copymemory(@mem_snd[$8000], @memory_temp[$1000], $2000);
        // convertir chars
        if not(roms_load(@memory_temp, drgnbstr_char)) then
          exit;
        init_gfx(0, 8, 8, $200);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
        // sprites
        fillchar(memory_temp[0], $10000, 0);
        if not(roms_load(@memory_temp, drgnbstr_sprites)) then
          exit;
        // unpack the third sprite ROM
        for f := 0 to $1FFF do
        begin
          memory_temp[f + $4000 + $4000] := memory_temp[f + $4000]; // sprite set #1, plane 3
          memory_temp[f + $6000 + $4000] := memory_temp[f + $4000] shr 4;
          // sprite set #2, plane 3
          memory_temp[f + $4000] := memory_temp[f + $2000 + $4000];
          // sprite set #3, planes 1&2 (plane 3 is empty)
        end;
        init_gfx(1, 16, 16, $200);
        gfx_set_desc_data(3, 0, 64 * 8, $200 * 64 * 8 + 4, 0, 4);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // tiles
        if not(roms_load(@memory_temp, drgnbstr_tiles)) then
          exit;
        init_gfx(2, 8, 8, $200);
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
        // Paleta
        if not(roms_load(@memory_temp, drgnbstr_prom)) then
          exit;
        screen_flip := true;
      end;
  end;
  for f := 0 to $FF do
  begin
    colores[f].r := (memory_temp[f] and $F) + ((memory_temp[f] and $F) shl 4);
    colores[f].g := (memory_temp[f + $100] and $F) + ((memory_temp[f + $100] and $F) shl 4);
    colores[f].b := (memory_temp[f + $200] and $F) + ((memory_temp[f + $200] and $F) shl 4);
  end;
  set_pal(colores, $100);
  // tiles/sprites color table
  for f := $0 to $3FF do
  begin
    gfx[1].colores[f] := memory_temp[$300 + f];
    gfx[2].colores[f] := memory_temp[$300 + f];
  end;
  // final
  reset_skykid;
  start_skykid := true;
end;

end.
