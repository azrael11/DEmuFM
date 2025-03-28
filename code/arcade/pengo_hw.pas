unit pengo_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  namco_snd,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  sega_decrypt;

function start_pengo: boolean;

implementation

const
  pengo_rom: array [0 .. 7] of tipo_roms = ((n: 'ep1689c.8'; l: $1000; p: 0; crc: $F37066A8), (n: 'ep1690b.7'; l: $1000; p: $1000; crc: $BAF48143), (n: 'ep1691b.15'; l: $1000; p: $2000;
    crc: $ADF0EBA0), (n: 'ep1692b.14'; l: $1000; p: $3000; crc: $A086D60F), (n: 'ep1693b.21'; l: $1000; p: $4000; crc: $B72084EC), (n: 'ep1694b.20'; l: $1000; p: $5000; crc: $94194A89),
    (n: 'ep5118b.32'; l: $1000; p: $6000; crc: $AF7B12C4), (n: 'ep5119c.31'; l: $1000; p: $7000; crc: $933950FE));
  pengo_pal: array [0 .. 1] of tipo_roms = ((n: 'pr1633.78'; l: $20; p: 0; crc: $3A5844EC), (n: 'pr1634.88'; l: $400; p: $20; crc: $766B139B));
  pengo_sound: tipo_roms = (n: 'pr1635.51'; l: $100; p: 0; crc: $C29DEA27);
  pengo_sprites: array [0 .. 1] of tipo_roms = ((n: 'ep1640.92'; l: $2000; p: $0; crc: $D7EEC6CD), (n: 'ep1695.105'; l: $2000; p: $4000; crc: $5BFD26E9));
  pengo_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Bonus Life'; number: 2; dip: ((dip_val: $0; dip_name: '30K'), (dip_val: $1; dip_name: '50K'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Lives';
    number: 4; dip: ((dip_val: $18; dip_name: '2'), (dip_val: $10; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Rack Test'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0;
    name: 'Difficulty'; number: 4; dip: ((dip_val: $C0; dip_name: 'Easy'), (dip_val: $80; dip_name: 'Medium'), (dip_val: $40; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  pengo_dip_b: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $08; dip_name: '3C 1C'), (dip_val: $04;
    dip_name: '2C 1C'), (dip_val: $09; dip_name: '2C 1C/5C 3C'), (dip_val: $05; dip_name: '2C 1C/4C 3C'), (dip_val: $0C; dip_name: '1C 1C'), (dip_val: $0D; dip_name: '1C 1C/5C 6C'), (dip_val: $03;
    dip_name: '1C 1C/4C 5C'), (dip_val: $0B; dip_name: '1C 2C/2C 3C'), (dip_val: $02; dip_name: '1C 2C'), (dip_val: $07; dip_name: '1C 2C/5C 11C'), (dip_val: $0F;
    dip_name: '1C 3C/4C 9C'), (dip_val: $0A; dip_name: '1C 3C'), (dip_val: $06; dip_name: '1C 4C'), (dip_val: $0E; dip_name: '1C 5C'), (dip_val: $01; dip_name: '1C 6C'))), (mask: $F0; name: 'Coin B';
    number: 16; dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $80; dip_name: '3C 1C'), (dip_val: $40; dip_name: '2C 1C'), (dip_val: $90; dip_name: '2C 1C/5C 3C'), (dip_val: $50;
    dip_name: '2C 1C/4C 3C'), (dip_val: $C0; dip_name: '1C 1C'), (dip_val: $D0; dip_name: '1C 1C/5C 6C'), (dip_val: $30; dip_name: '1C 1C/4C 5C'), (dip_val: $B0;
    dip_name: '1C 2C/2C 3C'), (dip_val: $20; dip_name: '1C 2C'), (dip_val: $70; dip_name: '1C 2C/5C 11C'), (dip_val: $F0; dip_name: '1C 3C/4C 9C'), (dip_val: $A0; dip_name: '1C 3C'), (dip_val: $60;
    dip_name: '1C 4C'), (dip_val: $E0; dip_name: '1C 5C'), (dip_val: $10; dip_name: '1C 6C'))), ());

var
  irq_enable: boolean;
  rom_opcode: array [0 .. $7FFF] of byte;
  colortable_bank, gfx_bank, pal_bank: byte;

procedure update_video_pengo;
var
  x, y, f, color, nchar, offs: word;
  sx, sy, atrib: byte;
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
        color := (((memory[$8400 + offs]) and $1F) or (colortable_bank shl 5) or (pal_bank shl 6)) shl 2;
        nchar := memory[$8000 + offs] + (gfx_bank shl 8);
        put_gfx(x * 8, y * 8, nchar, color, 1, 0);
        gfx[0].buffer[offs] := false;
      end;
    end;
  end;
  update_region(0, 0, 224, 288, 1, 0, 0, 224, 288, 2);
  for f := 7 downto 0 do
  begin
    atrib := memory[$8FF0 + (f * 2)];
    nchar := (atrib shr 2) or (gfx_bank shl 6);
    color := (((memory[$8FF1 + (f * 2)]) and $1F) or (colortable_bank shl 5) or (pal_bank shl 6)) shl 2;
    x := (240 - memory[$9020 + (f * 2)] - 1) and $FF;
    y := 272 - memory[$9021 + (f * 2)];
    put_gfx_sprite_mask(nchar, color, (atrib and 2) <> 0, (atrib and 1) <> 0, 1, 0, $F);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(0, 0, 224, 288, 2);
end;

procedure events_pengo;
begin
  if event.arcade then
  begin
    // marcade.in0
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // marcade.in1
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
  end;
end;

procedure pengo_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 223 then
        begin
          update_video_pengo;
          if irq_enable then
            z80_0.change_irq(HOLD_LINE);
        end;
      end;
      events_pengo;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function pengo_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      if z80_0.opcode then
        pengo_getbyte := rom_opcode[direccion]
      else
        pengo_getbyte := memory[direccion];
    $8000 .. $8FFF:
      pengo_getbyte := memory[direccion];
    $9000 .. $903F:
      pengo_getbyte := marcade.dswb;
    $9040 .. $907F:
      pengo_getbyte := marcade.dswa;
    $9080 .. $90BF:
      pengo_getbyte := marcade.in1;
    $90C0 .. $90FF:
      pengo_getbyte := marcade.in0;
  end;
end;

procedure pengo_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $3FF)] := true;
        memory[direccion] := valor;
      end;
    $8800 .. $8FFF, $9020 .. $902F:
      memory[direccion] := valor;
    $9000 .. $901F:
      namco_snd_0.regs[direccion and $1F] := valor;
    $9040:
      irq_enable := (valor <> 0);
    $9041:
      namco_snd_0.enabled := valor <> 0;
    $9042:
      if pal_bank <> valor then
      begin
        pal_bank := valor;
        fillchar(gfx[0].buffer, $400, 1);
      end;
    $9043:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $9046:
      if colortable_bank <> valor then
      begin
        colortable_bank := valor;
        fillchar(gfx[0].buffer, $400, 1);
      end;
    $9047:
      if gfx_bank <> (valor and $1) then
      begin
        gfx_bank := valor and $1;
        fillchar(gfx[0].buffer, $400, 1);
      end;
    $9070:
      ; // watchdog
  end;
end;

procedure pengo_sound_update;
begin
  namco_snd_0.update;
end;

// Main
procedure reset_pengo;
begin
  z80_0.reset;
  namco_snd_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  irq_enable := false;
  gfx_bank := 0;
  pal_bank := 0;
  colortable_bank := 0;
end;

function start_pengo: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $FFFF] of byte;
  rweights, gweights, bweights: array [0 .. 3] of single;
const
  ps_x: array [0 .. 15] of dword = (8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3, 0, 1, 2, 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  pc_x: array [0 .. 7] of dword = (8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  resistances: array [0 .. 2] of integer = (1000, 470, 220);
begin
  machine_calls.general_loop := pengo_loop;
  machine_calls.reset := reset_pengo;
  machine_calls.fps_max := 18432000 / 3 / 384 / 264;
  start_pengo := false;
  start_audio(false);
  screen_init(1, 224, 288);
  screen_init(2, 224, 288, false, true);
  screen_mod_sprites(2, 256, 512, $FF, $1FF);
  start_video(224, 288);
  // Main CPU
  z80_0 := cpu_z80.create(18432000 div 6, 264);
  z80_0.change_ram_calls(pengo_getbyte, pengo_putbyte);
  z80_0.init_sound(pengo_sound_update);
  // cargar roms
  if not(roms_load(@memory, pengo_rom)) then
    exit;
  decrypt_sega(@memory, @rom_opcode, 2);
  // cargar sonido & iniciar_sonido
  namco_snd_0 := namco_snd_chip.create(3);
  if not(roms_load(namco_snd_0.get_wave_dir, pengo_sound)) then
    exit;
  // organizar y convertir gfx
  if not(roms_load(@memory_temp, pengo_sprites)) then
    exit;
  copymemory(@memory_temp[$2000], @memory_temp[$1000], $1000);
  copymemory(@memory_temp[$1000], @memory_temp[$4000], $1000);
  copymemory(@memory_temp[$3000], @memory_temp[$5000], $1000);
  // chars
  init_gfx(0, 8, 8, $200);
  gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, true, false);
  // sprites
  init_gfx(1, 16, 16, $80);
  gfx_set_desc_data(2, 0, 64 * 8, 0, 4);
  convert_gfx(1, 0, @memory_temp[$2000], @ps_x, @ps_y, true, false);
  // poner la paleta
  if not(roms_load(@memory_temp, pengo_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances, @rweights, 0, 0, 3, @resistances, @gweights, 0, 0, 2, @resistances[1], @bweights, 0, 0);
  for f := 0 to $1F do
  begin
    // red component */
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    // green component */
    bit0 := (memory_temp[f] shr 3) and $01;
    bit1 := (memory_temp[f] shr 4) and $01;
    bit2 := (memory_temp[f] shr 5) and $01;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // blue component */
    bit0 := (memory_temp[f] shr 6) and $01;
    bit1 := (memory_temp[f] shr 7) and $01;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to 255 do
  begin
    gfx[0].colores[f] := memory_temp[$20 + f] and $F;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
    gfx[0].colores[f + $100] := (memory_temp[$20 + f] and $F) + $10;
    gfx[1].colores[f + $100] := (memory_temp[$20 + f] and $F) + $10;
  end;
  // DIP
  marcade.dswa := $B0;
  marcade.dswb := $CC;
  marcade.dswa_val := @pengo_dip_a;
  marcade.dswb_val := @pengo_dip_b;
  // final
  reset_pengo;
  start_pengo := true;
end;

end.
