unit crazyclimber_hw;

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
  crazyclimber_hw_dac;

function start_crazyclimber: boolean;

implementation

const
  cclimber_rom: array [0 .. 4] of tipo_roms = ((n: 'cc11'; l: $1000; p: 0; crc: $217EC4FF),
    (n: 'cc10'; l: $1000; p: $1000; crc: $B3C26CEF), (n: 'cc09'; l: $1000; p: $2000;
    crc: $6DB0879C), (n: 'cc08'; l: $1000; p: $3000; crc: $F48C5FE3), (n: 'cc07'; l: $1000;
    p: $4000; crc: $3E873BAF));
  cclimber_pal: array [0 .. 2] of tipo_roms = ((n: 'cclimber.pr1'; l: $20; p: 0; crc: $751C3325),
    (n: 'cclimber.pr2'; l: $20; p: $20; crc: $AB1940FA), (n: 'cclimber.pr3'; l: $20; p: $40;
    crc: $71317756));
  cclimber_char: array [0 .. 3] of tipo_roms = ((n: 'cc06'; l: $800; p: 0; crc: $481B64CC),
    (n: 'cc05'; l: $800; p: $1000; crc: $2C33B760), (n: 'cc04'; l: $800; p: $2000; crc: $332347CB),
    (n: 'cc03'; l: $800; p: $3000; crc: $4E4B3658));
  cclimber_bigsprites: array [0 .. 1] of tipo_roms = ((n: 'cc02'; l: $800; p: $0; crc: $14F3ECC9),
    (n: 'cc01'; l: $800; p: $800; crc: $21C0F9FB));
  cclimber_samples: array [0 .. 1] of tipo_roms = ((n: 'cc13'; l: $1000; p: $0; crc: $E0042F75),
    (n: 'cc12'; l: $1000; p: $1000; crc: $5DA13AAA));
  // DIP
  cclimber_dip_a: array [0 .. 3] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '4'), (dip_val: $2;
    dip_name: '5'), (dip_val: $3; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $30; name: 'Coin A'; number: 4; dip: ((dip_val: $30; dip_name: '4C 1C'), (dip_val: $20;
    dip_name: '3C 1C'), (dip_val: $10; dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '1C 1C'), (dip_val: $40; dip_name: '1C 2C'), (dip_val: $80;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (),
    (), (), ())), ());
  cclimber_dip_b: array [0 .. 1] of def_dip = ((mask: $10; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), ());

var
  nmi_mask: boolean;
  mem_decode: array [0 .. $5FFF] of byte;
  scroll_y: array [0 .. $1F] of word;

procedure update_video_cclimber;
  procedure draw_sprites;
  var
    f, x, y, nchar, attr, attr2, color: byte;
    flipx, flipy: boolean;
  begin
    for f := $7 downto 0 do
    begin
      x := memory[$9883 + (f * 4)] + 1;
      y := 240 - memory[$9882 + (f * 4)];
      attr := memory[(f * 4) + $9881];
      attr2 := memory[(f * 4) + $9880];
      nchar := ((attr and $10) shl 3) or ((attr and $20) shl 1) or (attr2 and $3F);
      color := (attr and $0F) shl 2;
      flipx := (attr2 and $40) <> 0;
      flipy := (attr2 and $80) <> 0;
      put_gfx_sprite_mask(nchar, color, flipx, flipy, 1, 0, 3);
      update_gfx_sprite(x, y, 3, 1);
    end;
  end;

var
  f, tile_index, nchar: word;
  color, attr, x, y: byte;
  flipx, flipy, bs_changed: boolean;
begin
  bs_changed := false;
  for f := 0 to $3FF do
  begin
    // Fondo
    attr := memory[f + $9C00];
    flipy := (attr and $20) <> 0;
    tile_index := f;
    if flipy then
      tile_index := tile_index xor $20;
    if gfx[0].buffer[tile_index] then
    begin
      flipx := (attr and $40) <> 0;
      attr := memory[tile_index + $9C00];
      x := f mod 32;
      y := f div 32;
      color := (attr and $F) shl 2;
      nchar := ((attr and $10) shl 5) or ((attr and $20) shl 3) or memory[$9000 + tile_index];
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, flipx, flipy);
      gfx[0].buffer[tile_index] := false;
    end;
    // Sprites grandes
    tile_index := ((f and $1E0) shr 1) or (f and $F);
    if gfx[2].buffer[tile_index] then
    begin
      x := f mod 32;
      y := f div 32;
      if (f and $210) = $210 then
      begin
        attr := memory[$98DD];
        color := (attr and $7) shl 2;
        nchar := ((attr and $08) shl 5) or memory[$8800 + tile_index];
        put_gfx_mask(x * 8, y * 8, nchar, color + 64, 2, 2, 0, $3);
        gfx[2].buffer[tile_index] := false;
        bs_changed := true;
      end
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
    end;
  end;
  // La pantalla de los sprites grandes, puede invertir el eje x y/o el y
  flipx := (memory[$98DD] and $10) <> 0;
  flipy := (memory[$98DD] and $20) <> 0;
  if bs_changed then
    flip_surface(2, flipx, flipy);
  x := memory[$98DF] - $8 - (byte(flipx) * $80) - (byte(main_screen.flip_main_screen) * $27);
  y := memory[$98DE] - (byte(flipy) * $80);
  // Poner todo en su sitio
  scroll__y_part2(1, 3, 8, @scroll_y);
  // for f:=0 to 31 do scroll__y_part(1,3,memoria[$9800+f],0,f*8,8);
  if (memory[$98DC] and 1) <> 0 then
  begin
    scroll_x_y(2, 3, x, y);
    draw_sprites;
  end
  else
  begin
    draw_sprites;
    scroll_x_y(2, 3, x, y);
  end;
  actualiza_trozo_final(0, 16, 256, 224, 3);
end;

procedure events_cclimber;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or $4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or $8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or $4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or $8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure cclimber_loop;
var
  f: byte;
  frame: single;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          if nmi_mask then
            z80_0.change_nmi(PULSE_LINE);
          update_video_cclimber;
        end;
      end;
      events_cclimber;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function cclimber_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $5FFF:
      if z80_0.opcode then
        cclimber_getbyte := mem_decode[direccion]
      else
        cclimber_getbyte := memory[direccion];
    $6000 .. $6BFF, $8000 .. $83FF, $8800 .. $8BFF, $9820 .. $9FFF:
      cclimber_getbyte := memory[direccion];
    $9000 .. $97FF:
      cclimber_getbyte := memory[$9000 + (direccion and $3FF)];
    $9800 .. $981F:
      cclimber_getbyte := scroll_y[direccion and $1F];
    $A000:
      cclimber_getbyte := marcade.in0;
    $A800:
      cclimber_getbyte := marcade.in1;
    $B000:
      cclimber_getbyte := marcade.dswa;
    $B800:
      cclimber_getbyte := marcade.dswb or marcade.in2;
  end;
end;

procedure cclimber_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF:
      ; // ROM
    $6000 .. $6BFF, $8000 .. $83FF, $8900 .. $8BFF, $9820 .. $98DC, $98DE .. $9BFF:
      memory[direccion] := valor;
    $8800 .. $88FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[2].buffer[direccion and $FF] := true;
      end;
    $9000 .. $97FF:
      if memory[$9000 + (direccion and $3FF)] <> valor then
      begin
        memory[$9000 + (direccion and $3FF)] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $9800 .. $981F:
      scroll_y[direccion and $1F] := valor;
    $98DD:
      if memory[$98DD] <> valor then
      begin
        fillchar(gfx[2].buffer, $400, 1);
        memory[$98DD] := valor;
      end;
    $9C00 .. $9FFF:
      begin
        memory[$9C00 + ((direccion and $3FF) and not($20))] := valor;
        gfx[0].buffer[(direccion and $3FF) and not($20)] := true;
        memory[$9C00 + ((direccion and $3FF) or $20)] := valor;
        gfx[0].buffer[(direccion and $3FF) or $20] := true;
      end;
    $A000, $A003:
      nmi_mask := (valor and $1) <> 0;
    $A001 .. $A002:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $A004:
      if valor <> 0 then
        cclimber_audio.trigger_w;
    $A800:
      cclimber_audio.change_freq(valor);
    $B000:
      cclimber_audio.change_volume(valor);
  end;
end;

function cclimber_inbyte(port: word): byte;
begin
  if (port and $FF) = $C then
    cclimber_inbyte := ay8910_0.Read;
end;

procedure cclimber_outbyte(port: word; valor: byte);
begin
  case (port and $FF) of
    $8:
      ay8910_0.control(valor);
    $9:
      ay8910_0.write(valor);
  end;
end;

procedure cclimber_porta_write(valor: byte);
begin
  cclimber_audio.change_sample(valor);
end;

procedure cclimber_sound_update;
begin
  ay8910_0.update;
  cclimber_audio.update;
end;

// Main
procedure reset_cclimber;
begin
  z80_0.reset;
  ay8910_0.reset;
  cclimber_audio.reset;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  nmi_mask := false;
end;

procedure cclimber_decode;
var
  f: word;
  i, j, src: byte;
const
  convtable: array [0 .. 7, 0 .. 15] of byte = (
    // 0xff marks spots which are unused and therefore unknown */
    ($44, $14, $54, $10, $11, $41, $05, $50, $51, $00, $40, $55, $45, $04, $01, $15),
    ($44, $10, $15, $55, $00, $41, $40, $51, $14, $45, $11, $50, $01, $54, $04, $05),
    ($45, $10, $11, $44, $05, $50, $51, $04, $41, $14, $15, $40, $01, $54, $55, $00),
    ($04, $51, $45, $00, $44, $10, $FF, $55, $11, $54, $50, $40, $05, $FF, $14, $01),
    ($54, $51, $15, $45, $44, $01, $11, $41, $04, $55, $50, $FF, $00, $10, $40, $FF),
    ($FF, $54, $14, $50, $51, $01, $FF, $40, $41, $10, $00, $55, $05, $44, $11, $45),
    ($51, $04, $10, $FF, $50, $40, $00, $FF, $41, $01, $05, $15, $11, $14, $44, $54),
    ($FF, $FF, $54, $01, $15, $40, $45, $41, $51, $04, $50, $05, $11, $44, $10, $14));
begin
  for f := 0 to $5FFF do
  begin
    src := memory[f];
    // pick the translation table from bit 0 of the address */
    // and from bits 1 7 of the source data */
    i := (f and 1) or (src and $2) or ((src and $80) shr 5);
    // pick the offset in the table from bits 0 2 4 6 of the source data */
    j := (src and $01) or ((src and $04) shr 1) or ((src and $10) shr 2) or ((src and $40) shr 3);
    // decode the opcodes */
    mem_decode[f] := (src and $AA) or convtable[i, j];
  end;
end;

function start_crazyclimber: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $3FFF] of byte;
  bit0, bit1, bit2: byte;
  rg_weights: array [0 .. 2] of single;
  b_weights: array [0 .. 1] of single;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2,
    8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8,
    17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
begin
  start_crazyclimber := false;
  machine_calls.general_loop := cclimber_loop;
  machine_calls.reset := reset_cclimber;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  screen_init(3, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(18432000 div 3 div 2, 256);
  z80_0.change_ram_calls(cclimber_getbyte, cclimber_putbyte);
  z80_0.change_io_calls(cclimber_inbyte, cclimber_outbyte);
  z80_0.init_sound(cclimber_sound_update);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(3072000 div 2, AY8910, 1);
  ay8910_0.change_io_calls(nil, nil, cclimber_porta_write, nil);
  cclimber_audio := tcclimber_audio.create;
  // cargar y desencriptar las ROMS
  if not(roms_load(@memory, cclimber_rom)) then
    exit;
  cclimber_decode;
  // samples
  if not(roms_load(cclimber_audio.get_rom_addr, cclimber_samples)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, cclimber_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx_set_desc_data(2, 0, 8 * 8, 0, $400 * 8 * 8);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // convertir sprites
  init_gfx(1, 16, 16, $100);
  gfx_set_desc_data(2, 0, 32 * 8, 0, $100 * 8 * 32);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // big sprites
  if not(roms_load(@memory_temp, cclimber_bigsprites)) then
    exit;
  init_gfx(2, 8, 8, $100);
  gfx_set_desc_data(2, 0, 8 * 8, 0, $100 * 8 * 8);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, cclimber_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rg_weights, 0, 0, 3, @resistances_b,
    @b_weights, 0, 0, 0, nil, nil, 0, 0);
  for f := 0 to $5F do
  begin
    // red component */
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    colores[f].r := combine_3_weights(@rg_weights, bit0, bit1, bit2);
    // green component */
    bit0 := (memory_temp[f] shr 3) and $01;
    bit1 := (memory_temp[f] shr 4) and $01;
    bit2 := (memory_temp[f] shr 5) and $01;
    colores[f].g := combine_3_weights(@rg_weights, bit0, bit1, bit2);
    // blue component */
    bit0 := (memory_temp[f] shr 6) and $01;
    bit1 := (memory_temp[f] shr 7) and $01;
    colores[f].b := combine_2_weights(@b_weights, bit0, bit1);
  end;
  set_pal(colores, $60);
  // DIP
  marcade.dswa := 0;
  marcade.dswb := $10;
  marcade.dswa_val := @cclimber_dip_a;
  marcade.dswb_val := @cclimber_dip_b;
  // final
  reset_cclimber;
  start_crazyclimber := true;
end;

end.
