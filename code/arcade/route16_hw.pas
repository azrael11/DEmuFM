unit route16_hw;

interface

uses
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  rom_engine,
  pal_engine,
  sound_engine,
  dac;

function start_route16: boolean;

implementation

const
  route16_cpu1: array [0 .. 5] of tipo_roms = ((n: 'stvg54.a0'; l: $800; p: 0; crc: $B8471CDC), (n: 'stvg55.a1'; l: $800; p: $800; crc: $3EC52FE5), (n: 'stvg56.a2'; l: $800; p: $1000; crc: $A8E92871),
    (n: 'stvg57.a3'; l: $800; p: $1800; crc: $A0FC9FC5), (n: 'stvg58.a4'; l: $800; p: $2000; crc: $CC95C02C), (n: 'stvg59.a5'; l: $800; p: $2800; crc: $A39EF648));
  route16_cpu2: array [0 .. 3] of tipo_roms = ((n: 'stvg60.b0'; l: $800; p: 0; crc: $FEF605F3), (n: 'stvg61.b1'; l: $800; p: $800; crc: $D0D6C189), (n: 'stvg62.b2'; l: $800; p: $1000; crc: $DEFC5797),
    (n: 'stvg63.b3'; l: $800; p: $1800; crc: $88D94A66));
  route16_proms: array [0 .. 1] of tipo_roms = ((n: 'mb7052.59'; l: $100; p: 0; crc: $08793EF7), (n: 'mb7052.61'; l: $100; p: $100; crc: $08793EF7));
  // Dip
  route16_dip_a: array [0 .. 5] of def_dip = ((mask: $1; name: 'Lives'; number: 2; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $18; name: 'Coinage'; number: 4; dip: ((dip_val: $8; dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $10; dip_name: '1C 2C'), (dip_val: $18;
    dip_name: '2C 1C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Speak and rescue
  speakres_cpu1: array [0 .. 5] of tipo_roms = ((n: 'speakres.1'; l: $800; p: 0; crc: $6026E4EA), (n: 'speakres.2'; l: $800; p: $800; crc: $93F0D4DA), (n: 'speakres.3'; l: $800; p: $1000;
    crc: $A3874304), (n: 'speakres.4'; l: $800; p: $1800; crc: $F484BE3A), (n: 'speakres.5'; l: $800; p: $2000; crc: $61B12A67), (n: 'speakres.6  '; l: $800; p: $2800; crc: $220E0AB2));
  speakres_cpu2: array [0 .. 1] of tipo_roms = ((n: 'speakres.7'; l: $800; p: 0; crc: $D417BE13), (n: 'speakres.8'; l: $800; p: $800; crc: $D417BE13));
  speakres_proms: array [0 .. 1] of tipo_roms = ((n: 'im5623.f10'; l: $100; p: 0; crc: $08793EF7), (n: 'im5623.f12'; l: $100; p: $100; crc: $08793EF7));
  // Dip
  speakres_dip_a: array [0 .. 6] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '4'), (dip_val: $2; dip_name: '5'), (dip_val: $3;
    dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: '2 Attackers at Wave'; number: 4; dip: ((dip_val: $0; dip_name: '2'), (dip_val: $4; dip_name: '3'), (dip_val: $8;
    dip_name: '4'), (dip_val: $C; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $0; dip_name: '5000'), (dip_val: $10; dip_name: '8000'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $20; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Voices'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  pal1, pal2: byte;
  protection_data: word;
  proms: array [0 .. $1FF] of byte;
  update_video: procedure;

procedure update_video_route16;
var
  f, temp: word;
  color1, color2, h, x, y, data1, data2: byte;
begin
  for f := 0 to $3FFF do
  begin
    x := (f shl 2) and $FF;
    y := (f shr 6) and $FF;
    data1 := memory[f + $8000];
    data2 := mem_snd[f + $8000];
    for h := 0 to 3 do
    begin
      color1 := proms[((pal1 shl 6) and $80) or (pal1 shl 2) or ((data1 shr 3) and $02) or ((data1 shr 0) and $01)];
      // bit 7 of the 2nd color is the OR of the 1st color bits 0 and 1 - this is a guess
      color2 := proms[$100 + (((pal2 shl 6) and $80) or (((color1 shl 6) and $80) or ((color1 shl 7) and $80)) or (pal2 shl 2) or ((data2 shr 3) and $02) or ((data2 shr 0) and $01))];
      // the final color is the OR of the two colors (verified)
      temp := paleta[(color1 or color2) and $7];
      putpixel(y, 255 - x, 1, @temp, 1);
      x := x + 1;
      data1 := data1 shr 1;
      data2 := data2 shr 1;
    end;
  end;
  update_region(0, 0, 255, 255, 1, 0, 0, 255, 255, PANT_TEMP);
end;

procedure update_video_speakres;
var
  f, temp: word;
  color1, color2, h, x, y, data1, data2: byte;
begin
  for f := 0 to $3FFF do
  begin
    x := (f shl 2) and $FF;
    y := (f shr 6) and $FF;
    data1 := memory[f + $8000];
    data2 := mem_snd[f + $8000];
    for h := 0 to 3 do
    begin
      color1 := proms[(pal1 shl 2) or ((data1 shr 3) and $02) or ((data1 shr 0) and $01)];
      // bit 7 of the 2nd color is the OR of the 1st color bits 0 and 1 (verified)
      color2 := proms[$100 + ((((data1 shl 3) and $80) or ((data1 shl 7) and $80)) or (pal2 shl 2) or ((data2 shr 3) and $02) or ((data2 shr 0) and $01))];
      temp := paleta[(color1 or color2) and $7];
      putpixel(y, 255 - x, 1, @temp, 1);
      x := x + 1;
      data1 := data1 shr 1;
      data2 := data2 shr 1;
    end;
  end;
  update_region(0, 0, 255, 255, 1, 0, 0, 255, 255, PANT_TEMP);
end;

procedure events_route16;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or $4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or $8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
  end;
end;

procedure route16_hw_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 255 do
      begin
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 0 then
        begin
          z80_0.change_irq(HOLD_LINE);
          update_video;
        end;
      end;
      events_route16;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function route16_cpu1_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $2FFF, $4000 .. $43FF, $8000 .. $BFFF:
      route16_cpu1_getbyte := memory[direccion];
    $3000 .. $3001:
      begin // proteccion
        protection_data := protection_data + 1;
        route16_cpu1_getbyte := (1 shl ((protection_data shr 1) and 7));
      end;
    $4800:
      route16_cpu1_getbyte := marcade.dswa;
    $5000:
      route16_cpu1_getbyte := marcade.in0;
    $5800:
      route16_cpu1_getbyte := marcade.in1;
  end;
end;

procedure route16_cpu1_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $2FFF:
      ; // ROM
    $4000 .. $43FF, $8000 .. $BFFF:
      memory[direccion] := valor;
    $4800:
      pal1 := valor and $1F;
    $5000:
      begin
        pal2 := valor and $1F;
        main_screen.flip_main_screen := (valor and $20) <> 0;
      end;
  end;
end;

procedure route16_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $1FF) of
    $0 .. $FF:
      ay8910_0.Write(valor);
    $100 .. $1FF:
      ay8910_0.Control(valor);
  end;
end;

function route16_cpu2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $8000 .. $BFFF:
      route16_cpu2_getbyte := mem_snd[direccion];
    $4000 .. $43FF:
      route16_cpu2_getbyte := memory[direccion];
  end;
end;

procedure route16_cpu2_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $4000 .. $43FF:
      memory[direccion] := valor;
    $8000 .. $BFFF:
      mem_snd[direccion] := valor;
  end;
end;

procedure route16_sound_update;
begin
  ay8910_0.update;
end;

// Speak & Rescue
function speakres_cpu1_getbyte(direccion: word): byte;
var
  bit0, bit1: byte;
begin
  case direccion of
    0 .. $2FFF, $4000 .. $43FF, $8000 .. $BFFF:
      speakres_cpu1_getbyte := memory[direccion];
    $4800:
      speakres_cpu1_getbyte := marcade.dswa;
    $5000:
      speakres_cpu1_getbyte := marcade.in0;
    $5800:
      speakres_cpu1_getbyte := marcade.in1;
    $6000:
      begin
        bit0 := 1 + 4;
        bit1 := 2;
        protection_data := protection_data + 1;
        if (protection_data > $300) then
          bit0 := 0;
        if (protection_data > $200) then
          bit1 := 0;
        speakres_cpu1_getbyte := $F8 or bit0 or bit1;
      end;
  end;
end;

procedure speakres_cpu1_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $2FFF:
      ; // ROM
    $4000 .. $43FF, $8000 .. $BFFF:
      memory[direccion] := valor;
    $4800:
      pal1 := valor and $1F;
    $5000:
      begin
        pal2 := valor and $1F;
        main_screen.flip_main_screen := (valor and $20) <> 0;
      end;
    $5800:
      protection_data := 0;
  end;
end;

procedure speakres_cpu2_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $2800:
      dac_0.data8_w(valor);
    $4000 .. $43FF:
      memory[direccion] := valor;
    $8000 .. $BFFF:
      mem_snd[direccion] := valor;
  end;
end;

procedure speakres_sound_update;
begin
  ay8910_0.update;
  dac_0.update;
end;

// Main
procedure reset_route16_hw;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  if main_vars.machine_type = 259 then
    dac_0.reset;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  pal1 := 0;
  pal2 := 0;
  protection_data := 0;
end;

function start_route16: boolean;
var
  colores: tpaleta;
  f: byte;
begin
  machine_calls.general_loop := route16_hw_loop;
  machine_calls.reset := reset_route16_hw;
  machine_calls.fps_max := 57;
  start_route16 := false;
  start_audio(false);
  screen_init(1, 256, 256);
  start_video(256, 256);
  // Main CPU
  z80_0 := cpu_z80.create(10000000 div 4, 256);
  z80_0.change_io_calls(nil, route16_outbyte);
  z80_1 := cpu_z80.create(10000000 div 4, 256);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(10000000 div 8, AY8910, 1);
  case main_vars.machine_type of
    258:
      begin
        z80_0.change_ram_calls(route16_cpu1_getbyte, route16_cpu1_putbyte);
        z80_0.init_sound(route16_sound_update);
        z80_1.change_ram_calls(route16_cpu2_getbyte, route16_cpu2_putbyte);
        // cargar roms
        if not(roms_load(@memory, route16_cpu1)) then
          exit;
        // Quitar proteccion
        memory[$105] := 0;
        memory[$106] := 0;
        memory[$107] := 0;
        memory[$731] := 0;
        memory[$732] := 0;
        memory[$733] := 0;
        memory[$0E9] := $3A;
        memory[$747] := $C3;
        memory[$748] := $56;
        memory[$749] := $07;
        if not(roms_load(@mem_snd, route16_cpu2)) then
          exit;
        if not(roms_load(@proms, route16_proms)) then
          exit;
        update_video := update_video_route16;
        marcade.dswa := $A0;
        marcade.dswa_val := @route16_dip_a;
      end;
    259:
      begin
        z80_0.change_ram_calls(speakres_cpu1_getbyte, speakres_cpu1_putbyte);
        z80_0.init_sound(speakres_sound_update);
        z80_1.change_ram_calls(route16_cpu2_getbyte, speakres_cpu2_putbyte);
        dac_0 := dac_chip.create;
        // cargar roms
        if not(roms_load(@memory, speakres_cpu1)) then
          exit;
        if not(roms_load(@mem_snd, speakres_cpu2)) then
          exit;
        if not(roms_load(@proms, speakres_proms)) then
          exit;
        update_video := update_video_speakres;
        marcade.dswa := $20;
        marcade.dswa_val := @speakres_dip_a;
      end;
  end;
  // Paleta
  for f := 0 to 7 do
  begin
    colores[f].r := (f and 1) * $FF;
    colores[f].g := ((f shr 1) and 1) * $FF;
    colores[f].b := ((f shr 2) and 1) * $FF;
  end;
  set_pal(colores, 8);
  // final
  reset_route16_hw;
  start_route16 := true;
end;

end.
