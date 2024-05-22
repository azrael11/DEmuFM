unit kangaroo_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  ay_8910,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_kangaroo: boolean;

implementation

const
  kangaroo_rom: array [0 .. 5] of tipo_roms = ((n: 'tvg_75.0'; l: $1000; p: 0; crc: $0D18C581),
    (n: 'tvg_76.1'; l: $1000; p: $1000; crc: $5978D37A), (n: 'tvg_77.2'; l: $1000; p: $2000;
    crc: $522D1097), (n: 'tvg_78.3'; l: $1000; p: $3000; crc: $063DA970), (n: 'tvg_79.4'; l: $1000;
    p: $4000; crc: $9E5CF8CA), (n: 'tvg_80.5'; l: $1000; p: $5000; crc: $2FC18049));
  kangaroo_gfx: array [0 .. 3] of tipo_roms = ((n: 'tvg_83.v0'; l: $1000; p: 0; crc: $C0446CA6),
    (n: 'tvg_85.v2'; l: $1000; p: $1000; crc: $72C52695), (n: 'tvg_84.v1'; l: $1000; p: $2000;
    crc: $E4CB26C2), (n: 'tvg_86.v3'; l: $1000; p: $3000; crc: $9E6A599F));
  kangaroo_sound: tipo_roms = (n: 'tvg_81.8'; l: $1000; p: 0; crc: $FB449BFD);
  // DIP
  kangaroo_dipa: array [0 .. 3] of def_dip = ((mask: $20; name: 'Music'; number: 2;
    dip: ((dip_val: $0; dip_name: 'On'), (dip_val: $20; dip_name: 'Off'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $40; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $40; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  kangaroo_dipb: array [0 .. 4] of def_dip = ((mask: $1; name: 'Lives'; number: 2;
    dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '5'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Hard'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $8; dip_name: '10000 30000'), (dip_val: $C; dip_name: '20000 40000'),
    (dip_val: $4; dip_name: '10000'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $F0; name: 'Coinage'; number: 16;
    dip: ((dip_val: $10; dip_name: '2C/1C'), (dip_val: $20; dip_name: 'A 2C/1C B 1C/3C'),
    (dip_val: $0; dip_name: '1C/1C'), (dip_val: $30; dip_name: 'A 1C/1C B 1C/2C'), (dip_val: $40;
    dip_name: 'A 1C/1C B 1C/3C'), (dip_val: $50; dip_name: 'A 1C/1C B 1C/4C'), (dip_val: $60;
    dip_name: 'A 1C/1C B 1C/5C'), (dip_val: $70; dip_name: 'A 1C/1C B 1C/6C'), (dip_val: $80;
    dip_name: '1C/2C'), (dip_val: $90; dip_name: 'A 1C/2C B 1C/4C'), (dip_val: $A0;
    dip_name: 'A 1C/2C B 1C/5C'), (dip_val: $E0; dip_name: 'A 1C/2C B 1C/6C'), (dip_val: $B0;
    dip_name: 'A 1C/2C B 1C/10C'), (dip_val: $C0; dip_name: 'A 1C/2C B 1C/11C'), (dip_val: $D0;
    dip_name: 'A 1C/2C B 1C/12C'), (dip_val: $F0; dip_name: 'Free Play'))), ());

var
  video_control: array [0 .. $F] of byte;
  sound_latch, mcu_clock, rom_bank: byte;
  video_ram: array [0 .. (256 * 64) - 1] of dword;
  gfx_data: array [0 .. 1, 0 .. $1FFF] of byte;

procedure update_video_kangaroo;
var
  x, y, scrolly, scrollx, maska, maskb, xora, xorb: byte;
  effxb, effyb, pixa, pixb, finalpens: byte;
  effxa, effya, sy, tempa, tempb: word;
  enaa, enab, pria, prib: boolean;
  punt: array [0 .. $1FFFF] of word;
begin
  scrolly := video_control[6];
  scrollx := video_control[7];
  maska := (video_control[10] and $28) shr 3;
  maskb := (video_control[10] and $07);
  xora := $FF * ((video_control[9] and $20) shr 5);
  xorb := $FF * ((video_control[9] and $10) shr 4);
  enaa := (video_control[9] and $08) <> 0;
  enab := (video_control[9] and $04) <> 0;
  pria := (not(video_control[9]) and $02) <> 0;
  prib := (not(video_control[9]) and $01) <> 0;
  // iterate over pixels */
  for y := 0 to 255 do
  begin
    sy := 0;
    for x := 0 to 255 do
    begin
      effxa := scrollx + (x xor xora);
      effya := scrolly + (y xor xora);
      effxb := x xor xorb;
      effyb := y xor xorb;
      tempa := effya + 256 * (effxa shr 2);
      tempb := effyb + 256 * (effxb shr 2);
      pixa := (video_ram[tempa] shr (8 * (effxa mod 4) + 0)) and $F;
      pixb := (video_ram[tempb] shr (8 * (effxb mod 4) + 4)) and $F;
      // for each layer, contribute bits if (a) enabled, and (b) either has priority or the opposite plane is 0 */
      finalpens := 0;
      if (enaa and (pria or (pixb = 0))) then
        finalpens := finalpens or pixa;
      if (enab and (prib or (pixa = 0))) then
        finalpens := finalpens or pixb;
      // store the first of two pixels, which is always full brightness */
      punt[sy * 256 + (255 - y)] := paleta[finalpens and 7];
      // KOS1 alternates at 5MHz, offset from the pixel clock by 1/2 clock */
      // when 0, it enables the color mask for pixels with Z = 0 */
      finalpens := 0;
      if (enaa and (pria or (pixb = 0))) then
      begin
        if ((pixa and $08) = 0) then
          pixa := pixa and maska;
        finalpens := finalpens or pixa;
      end;
      if (enab and (prib or (pixa = 0))) then
      begin
        if ((pixb and $08) = 0) then
          pixb := pixb and maskb;
        finalpens := finalpens or pixb;
      end;
      // store the second of two pixels, which is affected by KOS1 and the A/B masks */
      punt[(sy + 1) * 256 + (255 - y)] := paleta[finalpens and 7];
      sy := sy + 2;
    end;
  end;
  putpixel(0, 0, $20000, @punt, 1);
  actualiza_trozo(8, 0, 240, 512, 1, 0, 0, 240, 512, PANT_TEMP);
end;

procedure events_kangaroo;
begin
  if event.arcade then
  begin
    // P1
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
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 or $1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 or $2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 or $4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 or $8)
    else
      marcade.in2 := (marcade.in2 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
  end;
end;

procedure kangaroo_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 259 do
      begin
        // Main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 247 then
        begin
          z80_0.change_irq(HOLD_LINE);
          z80_1.change_irq(HOLD_LINE);
          update_video_kangaroo;
        end;
      end;
      events_kangaroo;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure videoram_write(offset: word; data, mask: byte);
var
  expdata, layermask: dword;
begin
  // data contains 4 2-bit values packed as DCBADCBA; expand these into 4 8-bit values */
  expdata := 0;
  if (data and $01) <> 0 then
    expdata := expdata or $00000055;
  if (data and $10) <> 0 then
    expdata := expdata or $000000AA;
  if (data and $02) <> 0 then
    expdata := expdata or $00005500;
  if (data and $20) <> 0 then
    expdata := expdata or $0000AA00;
  if (data and $04) <> 0 then
    expdata := expdata or $00550000;
  if (data and $40) <> 0 then
    expdata := expdata or $00AA0000;
  if (data and $08) <> 0 then
    expdata := expdata or $55000000;
  if (data and $80) <> 0 then
    expdata := expdata or $AA000000;
  // determine which layers are enabled */
  layermask := 0;
  if (mask and $08) <> 0 then
    layermask := layermask or $30303030;
  if (mask and $04) <> 0 then
    layermask := layermask or $C0C0C0C0;
  if (mask and $02) <> 0 then
    layermask := layermask or $03030303;
  if (mask and $01) <> 0 then
    layermask := layermask or $0C0C0C0C;
  // update layers */
  video_ram[offset] := (video_ram[offset] and not(layermask)) or (expdata and layermask);
end;

procedure blitter_execute;
var
  src, dst, effdst, effsrc: word;
  height, width, mask, x, y: byte;
begin
  src := video_control[0] + (video_control[1] shl 8);
  dst := video_control[2] + (video_control[3] shl 8);
  height := video_control[5];
  width := video_control[4];
  mask := video_control[8];
  // during DMA operations, the top 2 bits are ORed together, as well as the bottom 2 bits */
  // adjust the mask to account for this */
  if (mask and $0C) <> 0 then
    mask := mask or $0C;
  if (mask and $03) <> 0 then
    mask := mask or $03;
  // loop over height, then width */
  for y := 0 to height do
  begin
    for x := 0 to width do
    begin
      effdst := (dst + x) and $3FFF;
      effsrc := src and $1FFF;
      src := src + 1;
      videoram_write(effdst, gfx_data[0, effsrc], mask and $05);
      videoram_write(effdst, gfx_data[1, effsrc], mask and $0A);
    end;
    dst := dst + 256;
  end;
end;

function kangaroo_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $5FFF, $E000 .. $E3FF:
      kangaroo_getbyte := memory[direccion];
    $C000 .. $DFFF:
      kangaroo_getbyte := gfx_data[rom_bank, direccion and $1FFF];
    $E400 .. $E7FF:
      kangaroo_getbyte := marcade.dswb;
    $EC00 .. $ECFF:
      kangaroo_getbyte := marcade.in0 + marcade.dswa;
    $ED00 .. $EDFF:
      kangaroo_getbyte := marcade.in1;
    $EE00 .. $EEFF:
      kangaroo_getbyte := marcade.in2;
    $EF00 .. $EFFF:
      begin
        mcu_clock := mcu_clock + 1;
        kangaroo_getbyte := mcu_clock and $F;
      end;
  end;
end;

procedure kangaroo_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF:
      ;
    $8000 .. $BFFF:
      videoram_write((direccion and $3FFF), valor, video_control[8]);
    $E000 .. $E3FF:
      memory[direccion] := valor;
    $E800 .. $EBFF:
      begin
        video_control[direccion and $F] := valor;
        case (direccion and $F) of
          5:
            blitter_execute;
          8:
            rom_bank := byte((valor and $5) = 0);
        end;
      end;
    $EC00 .. $ECFF:
      sound_latch := valor;
  end;
end;

function kangaroo_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $FFF:
      kangaroo_snd_getbyte := mem_snd[direccion];
    $4000 .. $4FFF:
      kangaroo_snd_getbyte := mem_snd[$4000 + (direccion and $3FF)];
    $6000 .. $6FFF:
      kangaroo_snd_getbyte := sound_latch;
  end;
end;

procedure kangaroo_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FFF:
      ;
    $4000 .. $4FFF:
      mem_snd[$4000 + (direccion and $3FF)] := valor;
    $7000 .. $7FFF:
      ay8910_0.write(valor);
    $8000 .. $8FFF:
      ay8910_0.control(valor);
  end;
end;

procedure kangaroo_sound_update;
begin
  ay8910_0.update;
end;

procedure kangaroo_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 2] of byte;
begin
  open_qsnapshot_save('kangaroo' + nombre);
  getmem(data, 200);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ay8910_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[$6000], $A000);
  savedata_qsnapshot(@mem_snd[$1000], $F000);
  // MISC
  buffer[0] := sound_latch;
  buffer[1] := mcu_clock;
  buffer[2] := rom_bank;
  savedata_qsnapshot(@buffer, 3);
  savedata_qsnapshot(@video_control, $10);
  savedata_qsnapshot(@video_ram, $4000 * 4);
  freemem(data);
  close_qsnapshot;
end;

procedure kangaroo_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 2] of byte;
begin
  if not(open_qsnapshot_load('kangaroo' + nombre)) then
    exit;
  getmem(data, 200);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  z80_1.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ay8910_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory[$6000]);
  loaddata_qsnapshot(@mem_snd[$1000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  sound_latch := buffer[0];
  mcu_clock := buffer[1];
  rom_bank := buffer[2];
  loaddata_qsnapshot(@video_control);
  loaddata_qsnapshot(@video_ram);
  freemem(data);
  close_qsnapshot;
end;

// Main
procedure reset_kangaroo;
begin
  z80_0.reset;
  z80_0.change_nmi(PULSE_LINE);
  z80_1.reset;
  ay8910_0.reset;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  sound_latch := 0;
  fillchar(video_control, $10, 0);
  fillchar(video_ram, 256 * 64 * 4, 0);
  mcu_clock := 0;
  rom_bank := 0;
end;

function start_kangaroo: boolean;
var
  colores: tpaleta;
  f: word;
  mem_temp: array [0 .. $3FFF] of byte;
begin
  machine_calls.general_loop := kangaroo_loop;
  machine_calls.reset := reset_kangaroo;
  machine_calls.fps_max := 60.096154;
  machine_calls.save_qsnap := kangaroo_qsave;
  machine_calls.load_qsnap := kangaroo_qload;
  start_kangaroo := false;
  start_audio(false);
  screen_init(1, 256, 512);
  start_video(240, 512);
  // Main CPU
  z80_0 := cpu_z80.create(10000000 div 4, 260);
  z80_0.change_ram_calls(kangaroo_getbyte, kangaroo_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(10000000 div 8, 260);
  z80_1.change_ram_calls(kangaroo_snd_getbyte, kangaroo_snd_putbyte);
  z80_1.change_io_calls(kangaroo_snd_getbyte, kangaroo_snd_putbyte);
  z80_1.init_sound(kangaroo_sound_update);
  // Sound chip
  ay8910_0 := ay8910_chip.create(10000000 div 8, AY8910, 0.5);
  // cargar roms
  if not(roms_load(@memory, kangaroo_rom)) then
    exit;
  // cargar roms snd
  if not(roms_load(@mem_snd, kangaroo_sound)) then
    exit;
  // cargar gfx
  if not(roms_load(@mem_temp, kangaroo_gfx)) then
    exit;
  copymemory(@gfx_data[0, 0], @mem_temp[0], $2000);
  copymemory(@gfx_data[1, 0], @mem_temp[$2000], $2000);
  for f := 0 to 7 do
  begin
    colores[f].r := pal1bit(f shr 2);
    colores[f].g := pal1bit(f shr 1);
    colores[f].b := pal1bit(f shr 0);
  end;
  set_pal(colores, 8);
  marcade.dswa := $0;
  marcade.dswa_val := @kangaroo_dipa;
  marcade.dswb := $0;
  marcade.dswb_val := @kangaroo_dipb;
  // final
  reset_kangaroo;
  start_kangaroo := true;
end;

end.
