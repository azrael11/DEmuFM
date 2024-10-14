unit arabian_hw;

interface

uses
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  ay_8910,
  pal_engine,
  sound_engine,
  mb88xx;

function start_arabian: boolean;

implementation

const
  // arabian
  arabian_rom: array [0 .. 3] of tipo_roms = ((n: 'ic1rev2.87'; l: $2000; p: 0; crc: $5E1C98B8), (n: 'ic2rev2.88'; l: $2000; p: $2000; crc: $092F587E), (n: 'ic3rev2.89'; l: $2000; p: $4000;
    crc: $15145F23), (n: 'ic4rev2.90'; l: $2000; p: $6000; crc: $32B77B44));
  arabian_gfx: array [0 .. 3] of tipo_roms = ((n: 'tvg-91.ic84'; l: $2000; p: 0; crc: $C4637822), (n: 'tvg-92.ic85'; l: $2000; p: $2000; crc: $F7C6866D), (n: 'tvg-93.ic86'; l: $2000; p: $4000;
    crc: $71ACD48D), (n: 'tvg-94.ic87'; l: $2000; p: $6000; crc: $82160B9A));
  arabian_mcu: tipo_roms = (n: 'sun-8212.ic3'; l: $800; p: 0; crc: $8869611E);
  // Dip
  arabian_dip_a: array [0 .. 5] of def_dip = ((mask: $1; name: 'Lives'; number: 2; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $2; name: 'Cabinet'; number: 2; dip: ((dip_val: $2; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4;
    name: 'Flip Screen'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Difficulty';
    number: 2; dip: ((dip_val: $8; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Easy'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $F0; name: 'Coinage'; number: 16;
    dip: ((dip_val: $10; dip_name: 'A 2/1 B 2/1'), (dip_val: $20; dip_name: 'A 2/1 B 1/3'), (dip_val: $0; dip_name: 'A 1/1 B 1/1'), (dip_val: $30; dip_name: 'A 1/1 B 1/2'), (dip_val: $40;
    dip_name: 'A 1/1 B 1/3'), (dip_val: $50; dip_name: 'A 1/1 B 1/4'), (dip_val: $60; dip_name: 'A 1/1 B 1/5'), (dip_val: $70; dip_name: 'A 1/1 B 1/6'), (dip_val: $80;
    dip_name: 'A 1/2 B 1/2'), (dip_val: $90; dip_name: 'A 1/2 B 1/4'), (dip_val: $A0; dip_name: 'A 1/2 B 1/5'), (dip_val: $E0; dip_name: 'A 1/2 B 1/6'), (dip_val: $B0;
    dip_name: 'A 1/2 B 1/10'), (dip_val: $C0; dip_name: 'A 1/2 B 1/11'), (dip_val: $D0; dip_name: 'A 1/2 B 1/12'), (dip_val: $F0; dip_name: 'Free Play'))), ());
  arabian_dip_b: array [0 .. 3] of def_dip = ((mask: $1; name: 'Coin Counters'; number: 2; dip: ((dip_val: $1; dip_name: '1'), (dip_val: $0; dip_name: '2'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $C; dip_name: '30k 70k 40+'), (dip_val: $4; dip_name: '20k Only'), (dip_val: $8; dip_name: '40k Only'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  blitter: array [0 .. 7] of byte;
  video_ram, converted_gfx: array [0 .. $FFFF] of byte;
  video_control, mcu_port_p, mcu_port_o: byte;
  mcu_port_r: array [0 .. 3] of byte;

procedure update_video_arabian;
var
  x, y: byte;
  punt: array [0 .. $FFFF] of word;
begin
  for x := 0 to 255 do
    for y := 0 to 255 do
      punt[y + ((255 - x) * 256)] := paleta[(video_ram[y * 256 + x] + (video_control shl 8))];
  putpixel(0, 0, $10000, @punt, 1);
  update_region(11, 0, 234, 256, 1, 0, 0, 234, 256, pant_temp);
end;

procedure events_arabian;
begin
  if event.arcade then
  begin
    // in1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or $4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 or $8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    // in2
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 or $1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    // in3
    if p_contrls.map_arcade.coin[0] then
      marcade.in3 := (marcade.in3 or $1)
    else
      marcade.in3 := (marcade.in3 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in3 := (marcade.in3 or $2)
    else
      marcade.in3 := (marcade.in3 and $FD);
    // in0
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
  end;
end;

procedure arabian_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := mb88xx_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 255 do
      begin
        // Main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // MCU
        mb88xx_0.run(frame_s);
        frame_s := frame_s + mb88xx_0.tframes - mb88xx_0.contador;
        if f = 244 then
        begin
          update_video_arabian;
          z80_0.change_irq(HOLD_LINE);
        end;
      end;
      events_arabian;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function arabian_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      arabian_getbyte := memory[direccion];
    $C000 .. $C1FF:
      arabian_getbyte := marcade.in3;
    $C200 .. $C3FF:
      arabian_getbyte := marcade.dswa;
    $D000 .. $DFFF:
      arabian_getbyte := memory[$D000 + (direccion and $7FF)];
  end;
end;

procedure arabian_putbyte(direccion: word; valor: byte);
  procedure video_ram_w(pos: word; valor: byte);
  var
    x, y: byte;
    base: word;
  begin
    x := (pos shr 8) shl 2;
    y := pos;
    // get a pointer to the pixels
    base := y * 256 + x;
    // enable writes to AZ/AR
    if (blitter[0] and $08) <> 0 then
    begin
      video_ram[base + 0] := (video_ram[base + 0] and $FC) or ((valor and $10) shr 3) or ((valor and $01) shr 0);
      video_ram[base + 1] := (video_ram[base + 1] and $FC) or ((valor and $20) shr 4) or ((valor and $02) shr 1);
      video_ram[base + 2] := (video_ram[base + 2] and $FC) or ((valor and $40) shr 5) or ((valor and $04) shr 2);
      video_ram[base + 3] := (video_ram[base + 3] and $FC) or ((valor and $80) shr 6) or ((valor and $08) shr 3);
    end;
    // enable writes to AG/AB
    if (blitter[0] and $04) <> 0 then
    begin
      video_ram[base + 0] := (video_ram[base + 0] and $F3) or ((valor and $10) shr 1) or ((valor and $01) shl 2);
      video_ram[base + 1] := (video_ram[base + 1] and $F3) or ((valor and $20) shr 2) or ((valor and $02) shl 1);
      video_ram[base + 2] := (video_ram[base + 2] and $F3) or ((valor and $40) shr 3) or ((valor and $04) shl 0);
      video_ram[base + 3] := (video_ram[base + 3] and $F3) or ((valor and $80) shr 4) or ((valor and $08) shr 1);
    end;
    // enable writes to BZ/BR
    if (blitter[0] and $02) <> 0 then
    begin
      video_ram[base + 0] := (video_ram[base + 0] and $CF) or ((valor and $10) shl 1) or ((valor and $01) shl 4);
      video_ram[base + 1] := (video_ram[base + 1] and $CF) or ((valor and $20) shl 0) or ((valor and $02) shl 3);
      video_ram[base + 2] := (video_ram[base + 2] and $CF) or ((valor and $40) shr 1) or ((valor and $04) shl 2);
      video_ram[base + 3] := (video_ram[base + 3] and $CF) or ((valor and $80) shr 2) or ((valor and $08) shl 1);
    end;
    // enable writes to BG/BB
    if (blitter[0] and $01) <> 0 then
    begin
      video_ram[base + 0] := (video_ram[base + 0] and $3F) or ((valor and $10) shl 3) or ((valor and $01) shl 6);
      video_ram[base + 1] := (video_ram[base + 1] and $3F) or ((valor and $20) shl 2) or ((valor and $02) shl 5);
      video_ram[base + 2] := (video_ram[base + 2] and $3F) or ((valor and $40) shl 1) or ((valor and $04) shl 4);
      video_ram[base + 3] := (video_ram[base + 3] and $3F) or ((valor and $80) shl 0) or ((valor and $08) shl 3);
    end;
  end;

  procedure blit_area(plane: byte; src: word; x, y, sx, sy: byte);
  var
    srcdata, base: word;
    i, j, p1, p2, p3, p4: byte;
  begin
    srcdata := src * 4;
    // loop over X, then Y
    for i := 0 to sx do
    begin
      for j := 0 to sy do
      begin
        p1 := converted_gfx[srcdata];
        srcdata := srcdata + 1;
        p2 := converted_gfx[srcdata];
        srcdata := srcdata + 1;
        p3 := converted_gfx[srcdata];
        srcdata := srcdata + 1;
        p4 := converted_gfx[srcdata];
        srcdata := srcdata + 1;
        // get a pointer to the bitmap
        base := ((y + j) and $FF) * 256 + (x and $FF);
        // bit 0 means write to upper plane (upper 4 bits of our bitmap)
        if (plane and $01) <> 0 then
        begin
          if (p4 <> 8) then
            video_ram[base + 0] := (video_ram[base + 0] and $0F) or (p4 shl 4);
          if (p3 <> 8) then
            video_ram[base + 1] := (video_ram[base + 1] and $0F) or (p3 shl 4);
          if (p2 <> 8) then
            video_ram[base + 2] := (video_ram[base + 2] and $0F) or (p2 shl 4);
          if (p1 <> 8) then
            video_ram[base + 3] := (video_ram[base + 3] and $0F) or (p1 shl 4);
        end;
        // bit 2 means write to lower plane (lower 4 bits of our bitmap)
        if (plane and $04) <> 0 then
        begin
          if (p4 <> 8) then
            video_ram[base + 0] := (video_ram[base + 0] and $F0) or p4;
          if (p3 <> 8) then
            video_ram[base + 1] := (video_ram[base + 1] and $F0) or p3;
          if (p2 <> 8) then
            video_ram[base + 2] := (video_ram[base + 2] and $F0) or p2;
          if (p1 <> 8) then
            video_ram[base + 3] := (video_ram[base + 3] and $F0) or p1;
        end;
      end; // for j
      x := x + 4;
    end; // for i
  end;

begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $BFFF:
      video_ram_w(direccion, valor); // pant
    $D000 .. $DFFF:
      memory[$D000 + (direccion and $7FF)] := valor;
    $E000 .. $EFFF:
      begin // blitter
        blitter[direccion and $7] := valor;
        if (direccion and $7) = 6 then
          blit_area(blitter[0], blitter[1] or (blitter[2] shl 8), blitter[4] shl 2, blitter[3], blitter[6], blitter[5]);
      end;
  end;
end;

procedure arabian_outbyte(puerto: word; valor: byte);
begin
  case puerto of
    $C800 .. $C9FF:
      ay8910_0.control(valor);
    $CA00 .. $CBFF:
      ay8910_0.write(valor);
  end;
end;

procedure arabian_portaw(valor: byte);
begin
  video_control := valor shr 3;
end;

procedure arabian_portbw(valor: byte);
begin
  // reset mcu + irq mcu
  if (valor and $20) = 0 then
    mb88xx_0.set_irq_line(ASSERT_LINE)
  else
    mb88xx_0.set_irq_line(CLEAR_LINE);
  if (valor and $10) <> 0 then
    mb88xx_0.change_reset(CLEAR_LINE)
  else
    mb88xx_0.change_reset(ASSERT_LINE);
end;

procedure arabian_sound_update;
begin
  ay8910_0.update;
end;

// MCU
function mcu_port_r_r(port: byte): byte;
var
  val: byte;
begin
  val := mcu_port_r[port];
  // RAM mode is enabled */
  if (port = 0) then
    val := val or 4;
  mcu_port_r_r := val;
end;

procedure mcu_port_r_w(port, valor: byte);
var
  ram_addr: word;
begin
  if (port = 0) then
  begin
    ram_addr := ((mcu_port_p and 7) shl 8) or mcu_port_o;
    if (not(valor) and 2) <> 0 then
      memory[$D000 + ram_addr] := $F0 or mcu_port_r[3];
    main_screen.flip_main_screen := (valor and 8) <> 0;
  end;
  mcu_port_r[port] := valor and $0F;
end;

function mcu_port_k_r: byte;
var
  val, sel, i: byte;
  ram_addr: word;
begin
  val := $F;
  if (not(mcu_port_r[0]) and 1) <> 0 then
  begin
    ram_addr := ((mcu_port_p and 7) shl 8) or mcu_port_o;
    val := memory[$D000 + ram_addr];
  end
  else
  begin
    sel := ((mcu_port_r[2] shl 4) or mcu_port_r[1]) and $3F;
    for i := 0 to 5 do
    begin
      if (not(sel) and (1 shl i)) <> 0 then
      begin
        case i of
          0:
            val := marcade.in0;
          1:
            val := marcade.in1;
          2:
            val := marcade.in2;
          3:
            val := 0; // Cocktail
          4:
            val := 0; // Cocktail
          5:
            val := marcade.dswb;
        end;
        break;
      end;
    end;
  end;
  mcu_port_k_r := val and $F;
end;

procedure mcu_port_o_w(valor: byte);
var
  res: byte;
begin
  res := valor and $0F;
  if (valor and $10) <> 0 then
    mcu_port_o := (mcu_port_o and $0F) or (res shl 4)
  else
    mcu_port_o := (mcu_port_o and $F0) or res;
end;

procedure mcu_port_p_w(valor: byte);
begin
  mcu_port_p := valor and $0F;
end;

// Main
procedure reset_arabian;
begin
  z80_0.reset;
  mb88xx_0.reset;
  ay8910_0.reset;
  reset_audio;
  video_control := 0;
  mcu_port_p := 0;
  mcu_port_o := 0;
  mcu_port_r[0] := 0;
  mcu_port_r[1] := 0;
  mcu_port_r[2] := 0;
  mcu_port_r[3] := 0;
  marcade.in0 := 1;
  marcade.in1 := 0;
  marcade.in2 := 0;
  marcade.in3 := 0;
end;

procedure create_palette;
var
  colores: tpaleta;
  i: word;
  planea, enb: boolean;
  ena, abhf, aghf, arhf, az, ar, ag, ab, bz, br, bg, bb: byte;
  rhi, rlo, ghi, glo, bhi, bbase: byte;
begin
  for i := 0 to $1FFF do
  begin
    ena := (i shr 12) and 1;
    enb := (i and $200) <> 0;
    abhf := ((i shr 10) and 1) xor 1;
    aghf := ((i shr 9) and 1) xor 1;
    arhf := ((i shr 8) and 1) xor 1;
    az := (i shr 7) and 1;
    ar := (i shr 6) and 1;
    ag := (i shr 5) and 1;
    ab := (i shr 4) and 1;
    bz := (i shr 3) and 1;
    br := (i shr 2) and 1;
    bg := (i shr 1) and 1;
    bb := (i shr 0) and 1;
    planea := ((az or ar or ag or ab) and ena) <> 0;
    if planea then
    begin
      // Red derivation
      rhi := ar;
      if ((arhf xor 1) and az) <> 0 then
        rlo := 0
      else
        rlo := ar;
      // Green Derivation
      ghi := ag;
      if ((aghf xor 1) and az) <> 0 then
        glo := 0
      else
        glo := ag;
    end
    else
    begin
      // Red derivation
      rhi := bz * byte(enb);
      rlo := br * byte(enb);
      // Green Derivation
      ghi := bb * byte(enb);
      glo := bg * byte(enb);
    end;
    // Blue derivation
    bhi := ab;
    if ((abhf xor 1) and az) <> 0 then
      bbase := 0
    else
      bbase := ab;
    // Paleta
    if (rhi or rlo) <> 0 then
      colores[i].r := round((rhi * 115.7) + (rlo * 77.3) + 62)
    else
      colores[i].r := round((rhi * 115.7) + (rlo * 77.3));
    if (ghi or glo) <> 0 then
      colores[i].g := round((ghi * 117.9588) + (glo * 75.0411) + 62)
    else
      colores[i].g := round((ghi * 117.9588) + (glo * 75.0411));
    colores[i].b := (bhi * 192) + (bbase * 63);
  end;
  set_pal(colores, $2000);
end;

function start_arabian: boolean;
var
  memory_temp: array [0 .. $FFFF] of byte;
  procedure convert_gfx_arabian;
  var
    f: word;
    v1, v2: byte;
  begin
    for f := 0 to $3FFF do
    begin
      v1 := memory_temp[f];
      v2 := memory_temp[f + $4000];
      converted_gfx[f * 4 + 3] := (v1 and $01) or ((v1 and $10) shr 3) or ((v2 and $01) shl 2) or ((v2 and $10) shr 1);
      v1 := v1 shr 1;
      v2 := v2 shr 1;
      converted_gfx[f * 4 + 2] := (v1 and $01) or ((v1 and $10) shr 3) or ((v2 and $01) shl 2) or ((v2 and $10) shr 1);
      v1 := v1 shr 1;
      v2 := v2 shr 1;
      converted_gfx[f * 4 + 1] := (v1 and $01) or ((v1 and $10) shr 3) or ((v2 and $01) shl 2) or ((v2 and $10) shr 1);
      v1 := v1 shr 1;
      v2 := v2 shr 1;
      converted_gfx[f * 4 + 0] := (v1 and $01) or ((v1 and $10) shr 3) or ((v2 and $01) shl 2) or ((v2 and $10) shr 1);
    end;
  end;

begin
  start_arabian := false;
  machine_calls.general_loop := arabian_loop;
  machine_calls.reset := reset_arabian;
  start_audio(false);
  screen_init(1, 256, 256);
  start_video(234, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3000000, 256);
  z80_0.change_ram_calls(arabian_getbyte, arabian_putbyte);
  z80_0.change_io_calls(nil, arabian_outbyte);
  z80_0.init_sound(arabian_sound_update);
  // MCU
  mb88xx_0 := cpu_mb88xx.create(2000000, 256);
  mb88xx_0.change_io_calls(mcu_port_k_r, mcu_port_o_w, nil, mcu_port_p_w, mcu_port_r_r, mcu_port_r_w);
  // Audio chips
  ay8910_0 := ay8910_chip.create(1500000, AY8910, 0.5);
  ay8910_0.change_io_calls(nil, nil, arabian_portaw, arabian_portbw);
  // cargar roms
  if not(roms_load(@memory, arabian_rom)) then
    exit;
  // Cargar MCU
  if not(roms_load(mb88xx_0.get_rom_addr, arabian_mcu)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, arabian_gfx)) then
    exit;
  convert_gfx_arabian;
  create_palette;
  marcade.dswa := $06;
  marcade.dswb := $0F;
  marcade.dswa_val := @arabian_dip_a;
  marcade.dswb_val := @arabian_dip_b;
  // final
  reset_arabian;
  start_arabian := true;
end;

end.
