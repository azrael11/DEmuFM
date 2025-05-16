unit vulgus_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_vulgus: boolean;

implementation

const
  vulgus_rom: array [0 .. 4] of tipo_roms = ((n: 'vulgus.002'; l: $2000; p: 0; crc: $E49D6C5D), (n: 'vulgus.003'; l: $2000; p: $2000; crc: $51ACEF76), (n: 'vulgus.004'; l: $2000; p: $4000; crc: $489E7F60), (n: 'vulgus.005'; l: $2000; p: $6000; crc: $DE3A24A8), (n: '1-8n.bin';
    l: $2000; p: $8000; crc: $6CA5CA41));
  vulgus_snd_rom: tipo_roms = (n: '1-11c.bin'; l: $2000; p: 0; crc: $3BD2ACF4);
  vulgus_pal: array [0 .. 5] of tipo_roms = ((n: 'e8.bin'; l: $100; p: 0; crc: $06A83606), (n: 'e9.bin'; l: $100; p: $100; crc: $BEACF13C), (n: 'e10.bin'; l: $100; p: $200; crc: $DE1FB621), (n: 'd1.bin'; l: $100; p: $300; crc: $7179080D), (n: 'j2.bin'; l: $100; p: $400;
    crc: $D0842029), (n: 'c9.bin'; l: $100; p: $500; crc: $7A1F0BD6));
  vulgus_char: tipo_roms = (n: '1-3d.bin'; l: $2000; p: 0; crc: $8BC5D7A5);
  vulgus_sprites: array [0 .. 3] of tipo_roms = ((n: '2-2n.bin'; l: $2000; p: 0; crc: $6DB1B10D), (n: '2-3n.bin'; l: $2000; p: $2000; crc: $5D8C34EC), (n: '2-4n.bin'; l: $2000; p: $4000; crc: $0071A2E3), (n: '2-5n.bin'; l: $2000; p: $6000; crc: $4023A1EC));
  vulgus_tiles: array [0 .. 5] of tipo_roms = ((n: '2-2a.bin'; l: $2000; p: 0; crc: $E10AACA1), (n: '2-3a.bin'; l: $2000; p: $2000; crc: $8DA520DA), (n: '2-4a.bin'; l: $2000; p: $4000; crc: $206A13F1), (n: '2-5a.bin'; l: $2000; p: $6000; crc: $B6D81984), (n: '2-6a.bin'; l: $2000;
    p: $8000; crc: $5A26B38F), (n: '2-7a.bin'; l: $2000; p: $A000; crc: $1E1CA773));
  // Dip
  vulgus_dip_a: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 2, 3, 0); name4: ('1', '2', '3', '5')), (mask: $1C; name: 'Coin B'; number: 8; val8: ($10, 8, $18, 4, $1C, $C, $14, 0);
    name8: ('5C 1C', '4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', 'Invalid')), (mask: $E0; name: 'Coin A'; number: 8; val8: ($80, $40, $C0, $20, $E0, $60, $A0, 0); name8: ('5C 1C', '4C 1C', '3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', 'Free Play')), ());
  vulgus_dip_b: array [0 .. 4] of def_dip2 = ((mask: 4; name: 'Demo Music'; number: 2; val2: (0, 4); name2: ('Off', 'On')), (mask: 8; name: 'Demo Sounds'; number: 2; val2: (0, 8); name2: ('Off', 'On')), (mask: $70; name: 'Bonus Life'; number: 8;
    val8: ($30, $50, $10, $70, $60, $20, $40, 0); name8: ('10K 50K', '10K 60K', '10K 70K', '20K 60K', '20K 70K', '20K 80K', '30K 70K', 'None')), (mask: $80; name: 'Cabinet'; number: 2; val2: (0, $80); name2: ('Upright', 'Cocktail')), ());

var
  scroll_x, scroll_y: word;
  sound_command, palette_bank: byte;

procedure update_video_vulgus;
var
  f, color, nchar, x, y: word;
  attr, row: byte;
begin
  for f := $3FF downto 0 do
  begin
    // tiles
    if gfx[2].buffer[f] then
    begin
      x := f mod 32;
      y := 31 - (f div 32);
      attr := memory[$DC00 + f];
      nchar := memory[$D800 + f] + ((attr and $80) shl 1);
      color := ((attr and $1F) + ($20 * palette_bank)) shl 3;
      put_gfx_flip(x * 16, y * 16, nchar, color, 2, 2, (attr and $40) <> 0, (attr and $20) <> 0);
      gfx[2].buffer[f] := false;
    end;
    // Chars
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      attr := memory[f + $D400];
      color := (attr and $3F) shl 2;
      nchar := memory[f + $D000] + ((attr and $80) shl 1);
      put_gfx_mask(x * 8, y * 8, nchar, color, 3, 0, $2F, $3F);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 1, scroll_x and $1FF, 256 - (scroll_y and $1FF));
  // sprites
  for f := $1F downto 0 do
  begin
    attr := memory[$CC01 + (f * 4)];
    nchar := memory[$CC00 + (f * 4)];
    color := (attr and $F) shl 4;
    x := memory[$CC02 + (f * 4)];
    y := 240 - memory[$CC03 + (f * 4)];
    if y = 240 then
      continue;
    row := (attr and $C0) shr 6;
    case row of
      0:
        begin // 16x16
          put_gfx_sprite(nchar, color, false, false, 1);
          update_gfx_sprite(x, y, 1, 1);
        end;
      1:
        begin // 32x16
          put_gfx_sprite_diff(nchar + 1, color, false, false, 1, 16, 0);
          put_gfx_sprite_diff(nchar, color, false, false, 1, 0, 0);
          actualiza_gfx_sprite_size(x, y, 1, 32, 16);
        end;
      2:
        begin // 64x16
          put_gfx_sprite_diff(nchar + 3, color, false, false, 1, 48, 0);
          put_gfx_sprite_diff(nchar + 2, color, false, false, 1, 32, 0);
          put_gfx_sprite_diff(nchar + 1, color, false, false, 1, 16, 0);
          put_gfx_sprite_diff(nchar, color, false, false, 1, 0, 0);
          actualiza_gfx_sprite_size(x, y, 1, 64, 16);
        end;
    end;
  end;
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 1);
  update_final_piece(16, 0, 224, 256, 1);
end;

procedure events_vulgus;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // system
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure vulgus_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 255 do
      begin
        events_vulgus;
        if f = 240 then
        begin
          z80_0.change_irq_vector(HOLD_LINE, $D7);
          update_video_vulgus;
        end;
        // main
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // snd
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function vulgus_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $9FFF, $CC00 .. $CC7F, $D000 .. $EFFF:
      vulgus_getbyte := memory[direccion];
    $C000:
      vulgus_getbyte := marcade.in0;
    $C001:
      vulgus_getbyte := marcade.in1;
    $C002:
      vulgus_getbyte := marcade.in2;
    $C003:
      vulgus_getbyte := marcade.dswa;
    $C004:
      vulgus_getbyte := marcade.dswb;
    $C802:
      vulgus_getbyte := scroll_x and $FF;
    $C803:
      vulgus_getbyte := scroll_y and $FF;
    $C902:
      vulgus_getbyte := scroll_x shr 8;
    $C903:
      vulgus_getbyte := scroll_y shr 8;
  end;
end;

procedure vulgus_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $9FFF:
      ;
    $C800:
      sound_command := valor;
    $C802:
      scroll_x := (scroll_x and $FF00) or valor;
    $C803:
      scroll_y := (scroll_y and $FF00) or valor;
    $C804:
      main_screen.flip_main_screen := (valor and $80) <> 0;
    $C805:
      if palette_bank <> (valor and 3) then
      begin
        palette_bank := valor and 3;
        fillchar(gfx[2].buffer[0], $400, 1);
      end;
    $C902:
      scroll_x := (scroll_x and $FF) or (valor shl 8);
    $C903:
      scroll_y := (scroll_y and $FF) or (valor shl 8);
    $CC00 .. $CC7F, $E000 .. $EFFF:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function vulgus_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $47FF:
      vulgus_snd_getbyte := mem_snd[direccion];
    $6000:
      vulgus_snd_getbyte := sound_command;
  end;
end;

procedure vulgus_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ;
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
    $8000:
      ay8910_0.Control(valor);
    $8001:
      ay8910_0.Write(valor);
    $C000:
      ay8910_1.Control(valor);
    $C001:
      ay8910_1.Write(valor);
  end;
end;

procedure vulgus_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

procedure vulgus_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

// Main
procedure reset_vulgus;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  sound_command := 0;
  palette_bank := $FF;
end;

function start_vulgus: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $BFFF] of byte;
  bit0, bit1, bit2, bit3: byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  machine_calls.general_loop := vulgus_loop;
  machine_calls.reset := reset_vulgus;
  machine_calls.fps_max := 59.59;
  start_vulgus := false;
  start_audio(false);
  screen_init(1, 512, 512, false, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 256, 256, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3000000, 256);
  z80_0.change_ram_calls(vulgus_getbyte, vulgus_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(3000000, 256);
  z80_1.change_ram_calls(vulgus_snd_getbyte, vulgus_snd_putbyte);
  z80_1.init_sound(vulgus_sound_update);
  // IRQ Sound CPU
  timers.init(z80_1.numero_cpu, 3000000 / (8 * 60), vulgus_snd_irq, nil, true);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(1500000, AY8910);
  ay8910_1 := ay8910_chip.create(1500000, AY8910);
  // cargar y desencriptar las ROMS
  if not(roms_load(@memory, vulgus_rom)) then
    exit;
  // cargar ROMS sonido
  if not(roms_load(@mem_snd, vulgus_snd_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, vulgus_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // convertir sprites
  if not(roms_load(@memory_temp, vulgus_sprites)) then
    exit;
  init_gfx(1, 16, 16, $100);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // tiles
  if not(roms_load(@memory_temp, vulgus_tiles)) then
    exit;
  init_gfx(2, 16, 16, $200);
  gfx_set_desc_data(3, 0, 32 * 8, 0, $4000 * 8, $4000 * 8 * 2);
  convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // poner la paleta
  if not(roms_load(@memory_temp, vulgus_pal)) then
    exit;
  for f := 0 to 255 do
  begin
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    bit3 := (memory_temp[f] shr 3) and 1;
    colores[f].r := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    bit0 := (memory_temp[f + $100] shr 0) and 1;
    bit1 := (memory_temp[f + $100] shr 1) and 1;
    bit2 := (memory_temp[f + $100] shr 2) and 1;
    bit3 := (memory_temp[f + $100] shr 3) and 1;
    colores[f].g := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    bit0 := (memory_temp[f + $200] shr 0) and 1;
    bit1 := (memory_temp[f + $200] shr 1) and 1;
    bit2 := (memory_temp[f + $200] shr 2) and 1;
    bit3 := (memory_temp[f + $200] shr 3) and 1;
    colores[f].b := $E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  end;
  set_pal(colores, 256);
  // crear la tabla de colores
  for f := 0 to $FF do
    gfx[0].colores[f] := memory_temp[$300 + f] + 32;
  for f := 0 to $FF do
    gfx[1].colores[f] := memory_temp[$400 + f] + 16;
  for f := 0 to $FF do
  begin
    gfx[2].colores[0 * 32 * 8 + f] := memory_temp[$500 + f];
    gfx[2].colores[1 * 32 * 8 + f] := memory_temp[$500 + f] + 64;
    gfx[2].colores[2 * 32 * 8 + f] := memory_temp[$500 + f] + 128;
    gfx[2].colores[3 * 32 * 8 + f] := memory_temp[$500 + f] + 192;
  end;
  // Dip
  marcade.dswa := $FF;
  marcade.dswb := $7F;
  marcade.dswa_val2 := @vulgus_dip_a;
  marcade.dswb_val2 := @vulgus_dip_b;
  // final
  start_vulgus := true;
end;

end.
