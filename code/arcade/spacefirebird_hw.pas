unit spacefirebird_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  mcs48,
  pal_engine,
  sound_engine,
  dac,
  samples;

function start_spacefirebird: boolean;

implementation

const
        //spacefb
        spacefb_rom:array[0..7] of tipo_roms=(
        (n:'tst-c-u.5e';l:$800;p:0;crc:$79c3527e),(n:'tst-c-u.5f';l:$800;p:$800;crc:$c0973965),
        (n:'tst-c-u.5h';l:$800;p:$1000;crc:$02c60ec5),(n:'tst-c-u.5i';l:$800;p:$1800;crc:$76fd18c7),
        (n:'tst-c-u.5j';l:$800;p:$2000;crc:$df52c97c),(n:'tst-c-u.5k';l:$800;p:$2800;crc:$1713300c),
        (n:'tst-c-u.5m';l:$800;p:$3000;crc:$6286f534),(n:'tst-c-u.5n';l:$800;p:$3800;crc:$1c9f91ee));
        spacefb_gfx:array[0..1] of tipo_roms=(
        (n:'tst-v-a.5k';l:$800;p:0;crc:$236e1ff7),(n:'tst-v-a.6k';l:$800;p:$800;crc:$bf901a4e));
        spacefb_bullet:tipo_roms=(n:'4i.vid';l:$100;p:0;crc:$528e8533);
        spacefb_mcu:tipo_roms=(n:'ic20.snd';l:$400;p:0;crc:$1c8670b3);
        spacefb_prom:tipo_roms=(n:'mb7051.3n';l:$20;p:0;crc:$465d07af);
        spacefb_samples:array[0..3] of tipo_nombre_samples=(
        (nombre:'ekilled.wav';restart:true),(nombre:'explode1.wav'),(nombre:'explode2.wav'),(nombre:'shipfire.wav';restart:true));
        //Dip
        spacefb_dip:array [0..4] of def_dip2=(
        (mask:3;name:'Lives';number:4;val4:(0,1,2,3);name4:('3','4','5','6')),
        (mask:$c;name:'Coinage';number:4;val4:(8,4,0,$c);name4:('3C 1C','2C 1C','1C 1C','1C 2C')),
        (mask:$10;name:'Bonus Life';number:2;val2:(0,$10);name2:('5K','8K')),
        (mask:$20;name:'Cabinet';number:2;val2:($20,0);name2:('Upright','Cocktail')),());

var
  mem_snd_mcu: array [0 .. $3FF] of byte;
  gfx1: array [0 .. $FFF] of byte;
  gfx2: array [0 .. $FF] of byte;
  port_0, port_2, sound_latch: byte;
  prom: array [0 .. $1F] of byte;
  rgweights: array [0 .. 2] of single;
  bweights: array [0 .. 1] of single;
  punt: array [0 .. $FFFF] of word;
  star_shift_reg: dword;

procedure update_video_spacefb;
var
  offs: byte;
  flip: boolean;
  procedure draw_sprite(offs: byte; flip: boolean);
  var
    sy, code, color_base, y, sx, dy, x, data, data1, data2, dx: byte;
  begin
    code := not(memory[offs + $8200]);
    color_base := (not(memory[offs + $8300]) and $F) shl 2;
    y := not(memory[offs + $8100]) - 2;
    for sy := 0 to 7 do
    begin
      data1 := gfx1[0 or (code shl 3) or (sy xor 7)];
      data2 := gfx1[$800 or (code shl 3) or (sy xor 7)];
      x := memory[offs + $8000] - 3;
      if flip then
        dy := not(y)
      else
        dy := y;
      for sx := 0 to 7 do
      begin
        if not(flip) then
          dx := (255 - x)
        else
          dx := x;
        data := ((data1 shl 1) and 2) or (data2 and 1);
        if data <> 0 then
          punt[dy + (dx * 256)] := paleta[color_base or data];
        x := x + 1;
        data1 := data1 shr 1;
        data2 := data2 shr 1;
      end;
      y := y + 1;
    end;
  end;
  procedure draw_bullet(offs: byte; flip: boolean);
  var
    sy, code, y, sx, dy, data, x, dx: byte;
  begin
    code := memory[offs + $8200] and $3F;
    y := not(memory[offs + $8100]) - 2;
    for sy := 0 to 3 do
    begin
      data := gfx2[(code shl 2) or sy];
      x := memory[offs + $8000];
      if flip then
        dy := not(y)
      else
        dy := y;
      for sx := 0 to 3 do
      begin
        if (data and 1) <> 0 then
        begin
          if not(flip) then
            dx := (255 - x)
          else
            dx := x;
          punt[dy + (dx * 256)] := paleta[$40];
        end;
        x := x + 1;
        data := data shr 1;
      end;
      y := y + 1;
    end;
  end;

begin
  offs := (port_0 and $20) shl 2;
  flip := (port_0 and 1) <> 0;
  while true do
  begin
    if (memory[$8300 + offs] and $20) <> 0 then
      draw_bullet(offs, flip)
    else if (memory[$8300 + offs] and $40) <> 0 then
      draw_sprite(offs, flip);
    // next object
    offs := offs + 1;
    // end of bank?
    if ((offs and $7F) = 0) then
      break;
  end;
  putpixel(0, 0, $10000, @punt[0], 1);
  update_region(16, 0, 224, 256, 1, 0, 0, 224, 256, PANT_TEMP);
end;

procedure shift_star_generator;
begin
  star_shift_reg := ((star_shift_reg shl 1) or (((not(star_shift_reg) shr 16) and 1) xor ((star_shift_reg shr 4) and 1))) and $1FFFF;
end;

procedure draw_stars(y: byte);
var
  x: word;
begin
  for x := 0 to 255 do
  begin
    shift_star_generator;
    // draw the star - the 4 possible values come from the effect of the two XOR gates
    if (((star_shift_reg and $1C0FF) = $0C0B7) or ((star_shift_reg and $1C0FF) = $0C0D7) or ((star_shift_reg and $1C0FF) = $0C0BB) or ((star_shift_reg and $1C0FF) = $0C0DB)) then
      punt[y + 256 * x] := paleta[((star_shift_reg shr 8) and $3F) + $50]
    else
      punt[y + 256 * x] := paleta[$50];
  end;
end;

procedure events_spacefb;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
    // p2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 or $80)
    else
      marcade.in2 := (marcade.in2 and $7F);
    // system
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
  end;
end;

procedure spacefb_loop;
var
  f:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to 255 do begin
    events_spacefb;
    case f of
      128:z80_0.change_irq_vector(HOLD_LINE,$cf);
      240:begin
            z80_0.change_irq_vector(HOLD_LINE,$d7);
            update_video_spacefb;
          end;
    end;
    //Main
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
    //MCU
    mcs48_0.run(frame_snd);
    frame_snd:=frame_snd+mcs48_0.tframes-mcs48_0.contador;
    draw_stars(f);
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

procedure get_sprite_pens;
const
  fade_weights: array [0 .. 3] of double = (1.0, 1.5, 2.5, 4.0);
var
  i, data, r0, r1, r2, g0, g1, g2, b1, b2: byte;
  fade_weight: double;
  color: tcolor;
begin
  for i := 0 to $3F do
  begin
    data := prom[((port_0 and $40) shr 2) or (i and $0F)];
    r0 := (data shr 0) and 1;
    r1 := (data shr 1) and 1;
    r2 := (data shr 2) and 1;
    g0 := (data shr 3) and 1;
    g1 := (data shr 4) and 1;
    g2 := (data shr 5) and 1;
    b1 := (data shr 6) and 1;
    b2 := (data shr 7) and 1;
    color.r := combine_3_weights(@rgweights[0], r0, r1, r2);
    color.g := combine_3_weights(@rgweights[0], g0, g1, g2);
    color.b := combine_2_weights(@bweights[0], b1, b2);
    if (i shr 4) <> 0 then
    begin
      fade_weight := fade_weights[i shr 4];
      // faded pens
      color.r := trunc((color.r / fade_weight) + 0.5);
      color.g := trunc((color.g / fade_weight) + 0.5);
      color.b := trunc((color.b / fade_weight) + 0.5);
    end;
    set_pal_color(color, i);
  end;
end;

procedure get_stars_pens;
var
  i, gb, ga, bb, ba, ra, rb, color_contrast_r, color_contrast_g, color_contrast_b, background_red, background_blue, disable_star_field: byte;
  color: tcolor;
begin
  // generate the pens based on the various enable bits */
  color_contrast_r := port_2 and 1;
  color_contrast_g := (port_2 and 2) shr 1;
  color_contrast_b := (port_2 and 4) shr 2;
  background_red := (port_2 and 8) shr 3;
  background_blue := (port_2 and $10) shr 4;
  disable_star_field := (port_2 and $80) shr 7;
  for i := 0 to $3F do
  begin
    gb := ((i shr 0) and 1) and color_contrast_g and not(disable_star_field);
    ga := ((i shr 1) and 1) and not(disable_star_field);
    bb := ((i shr 2) and 1) and color_contrast_b and not(disable_star_field);
    ba := (((i shr 3) and 1) or background_blue) and not(disable_star_field);
    ra := (((i shr 4) and 1) or background_red) and not(disable_star_field);
    rb := ((i shr 5) and 1) and color_contrast_r and not(disable_star_field);
    color.r := combine_3_weights(@rgweights[0], 0, rb, ra);
    color.g := combine_3_weights(@rgweights[0], 0, gb, ga);
    color.b := combine_2_weights(@bweights[0], bb, ba);
    set_pal_color(color, i + $50);
  end;
end;

function spacefb_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      spacefb_getbyte := memory[direccion];
    $4000 .. $7FFF:
      ;
    $8000 .. $BFFF:
      spacefb_getbyte := memory[$8000 + (direccion and $3FF)];
    $C000 .. $FFFF:
      case (direccion and $FFF) of
        0 .. $7FF:
          spacefb_getbyte := memory[$C000 + (direccion and $7FF)];
        $800 .. $FFF:
          ;
      end;
  end;
end;

procedure spacefb_putbyte(direccion: word; valor: byte);
begin
  if direccion < $8000 then
    exit;
  case direccion of
    $8000 .. $BFFF:
      memory[$8000 + (direccion and $3FF)] := valor;
    $C000 .. $FFFF:
      case (direccion and $FFF) of
        0 .. $7FF:
          memory[$C000 + (direccion and $7FF)] := valor;
        $800 .. $FFF:
          ;
      end;
  end;
end;

function spacefb_inbyte(puerto: word): byte;
begin
  case (puerto and 7) of
    0:
      spacefb_inbyte := marcade.in1; // P1
    1:
      spacefb_inbyte := marcade.in2; // P2
    2:
      spacefb_inbyte := marcade.in0; // SYSTEM
    3:
      spacefb_inbyte := marcade.dswa; // DSW
    4 .. 7:
      ;
  end;
end;

procedure spacefb_outbyte(puerto: word; valor: byte);
begin
  case (puerto and 7) of
    0, 4:
      begin
        port_0 := valor;
        get_sprite_pens;
      end;
    1, 5:
      begin
        if (valor and 2) <> 0 then
          mcs48_0.change_irq(CLEAR_LINE)
        else
          mcs48_0.change_irq(ASSERT_LINE);
        // enemy killed
        if (((valor and 1) = 0) and ((sound_latch and 1) <> 0)) then
          start_sample(0);
        // ship fire
        if (((valor and $40) = 0) and ((sound_latch and $40) <> 0)) then
          start_sample(3);
        if ((valor and $80) <> (sound_latch and $80)) then
        begin
          if (valor and $80) <> 0 then
            start_sample(1) // play decaying noise
          else
            start_sample(2); // start looping noise */
        end;
        sound_latch := valor;
      end;
    2, 6:
      begin
        port_2 := valor;
        get_stars_pens;
      end;
    3, 7:
      ;
  end;
end;

// SND
function spacefb_snd_getbyte(direccion: word): byte;
begin
  spacefb_snd_getbyte := mem_snd_mcu[direccion and $3FF];
end;

function spacefb_snd_inport(puerto: word): byte;
begin
  case puerto of
    MCS48_PORT_P2:
      spacefb_snd_inport := (sound_latch and $18) shl 1;
    MCS48_PORT_T0:
      spacefb_snd_inport := sound_latch and $20;
    MCS48_PORT_T1:
      spacefb_snd_inport := sound_latch and 4;
  end;
end;

procedure spacefb_snd_outport(puerto: word; valor: byte);
begin
  if puerto = MCS48_PORT_P1 then
    dac_0.data8_w(valor);
end;

procedure spacefb_sound_update;
begin
  dac_0.update;
  samples_update;
end;

// Main
procedure reset_spacefb;
begin
  z80_0.reset;
  mcs48_0.reset;
  dac_0.reset;
 frame_main:=z80_0.tframes;
 frame_snd:=mcs48_0.tframes;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  sound_latch := 0;
  port_0 := 0;
  port_2 := 0;
  star_shift_reg := $18F89;
end;

function start_spacefirebird: boolean;
const
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
var
  color: tcolor;
begin
  start_spacefirebird := false;
  machine_calls.general_loop := spacefb_loop;
  machine_calls.reset := reset_spacefb;
  machine_calls.fps_max := 61.523438;
  start_audio(false);
  screen_init(1, 256, 256);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(6000000 div 2, 256);
  z80_0.change_ram_calls(spacefb_getbyte, spacefb_putbyte);
  z80_0.change_io_calls(spacefb_inbyte, spacefb_outbyte);
  // MCU
  mcs48_0 := cpu_mcs48.create(6000000, 256, I8035);
  mcs48_0.change_ram_calls(spacefb_snd_getbyte, nil);
  mcs48_0.change_io_calls(spacefb_snd_inport, spacefb_snd_outport, nil, nil);
  mcs48_0.init_sound(spacefb_sound_update);
  // cargar roms
  if not(roms_load(@memory, spacefb_rom)) then
    exit;
  // Cargar MCU
  if not(roms_load(@mem_snd_mcu, spacefb_mcu)) then
    exit;
  // Sound
  load_samples(spacefb_samples);
  dac_0 := dac_chip.create(0.5);
  // Cargar GFX and proms
  if not(roms_load(@gfx1, spacefb_gfx)) then
    exit;
  if not(roms_load(@gfx2, spacefb_bullet)) then
    exit;
  if not(roms_load(@prom, spacefb_prom)) then
    exit;
  // DIP
  marcade.dswa := $20;
marcade.dswa_val2:=@spacefb_dip;
  // Calcular paleta
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg[0], @rgweights[0], 470, 0, 2, @resistances_b[0], @bweights[0], 470, 0, 0, nil, nil, 0, 0);
  // Poner el color rojo del disparo...
  color.r := $FF;
  color.g := 0;
  color.b := 0;
  set_pal_color(color, $40);
  // final
  start_spacefirebird := true;
end;

end.
