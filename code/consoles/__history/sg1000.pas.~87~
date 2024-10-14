unit sg1000;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  tms99xx,
  sn_76496,
  System.SysUtils,
  misc_functions,
  sound_engine;

function start_sg1000: boolean;

type
  tsg1000 = record
    ram_8k, mid_8k_ram, push_pause: boolean;
    keys: array [0 .. 1] of byte;
  end;

var
  sg1000_0: tsg1000;

implementation

uses
  main,
  snapshot;

procedure events_sg;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $FE)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $1);
    if p_contrls.map_arcade.down[0] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $FD)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $2);
    if p_contrls.map_arcade.left[0] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $FB)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $4);
    if p_contrls.map_arcade.right[0] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $F7)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $8);
    if p_contrls.map_arcade.but0[0] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $EF)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $10);
    if p_contrls.map_arcade.but1[0] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $DF)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $20);
    // P2
    if p_contrls.map_arcade.up[1] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $BF)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $40);
    if p_contrls.map_arcade.down[1] then
      sg1000_0.keys[0] := (sg1000_0.keys[0] and $7F)
    else
      sg1000_0.keys[0] := (sg1000_0.keys[0] or $80);
    if p_contrls.map_arcade.left[1] then
      sg1000_0.keys[1] := (sg1000_0.keys[1] and $FE)
    else
      sg1000_0.keys[1] := (sg1000_0.keys[1] or $1);
    if p_contrls.map_arcade.right[1] then
      sg1000_0.keys[1] := (sg1000_0.keys[1] and $FD)
    else
      sg1000_0.keys[1] := (sg1000_0.keys[1] or $2);
    if p_contrls.map_arcade.but0[1] then
      sg1000_0.keys[1] := (sg1000_0.keys[1] and $FB)
    else
      sg1000_0.keys[1] := (sg1000_0.keys[1] or $4);
    if p_contrls.map_arcade.but1[1] then
      sg1000_0.keys[1] := (sg1000_0.keys[1] and $F7)
    else
      sg1000_0.keys[1] := (sg1000_0.keys[1] or $8);
    if p_contrls.map_arcade.coin[0] then
      sg1000_0.push_pause := true
    else
    begin
      if sg1000_0.push_pause then
        z80_0.change_nmi(PULSE_LINE);
      sg1000_0.push_pause := false;
    end;
  end;
end;

procedure sg_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 261 do
    begin
      z80_0.run(frame);
      frame := frame + z80_0.tframes - z80_0.contador;
      tms_0.refresh(f);
    end;
  actualiza_trozo(0,0,284,243,1,0,0,284,243,PANT_TEMP);
    events_sg;
    video_sync;
  end;
end;

function sg_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      sg_getbyte := memory[direccion];
    $C000 .. $FFFF:
      sg_getbyte := memory[$C000 + direccion and $1FFF];
  end;
end;

procedure sg_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF, $4000 .. $7FFF, $A000 .. $BFFF:
      ; // ROM
    $2000 .. $3FFF:
      if sg1000_0.ram_8k then
        memory[direccion] := valor;
    $8000 .. $9FFF:
      if sg1000_0.mid_8k_ram then
        memory[direccion] := valor;
    $C000 .. $FFFF:
      memory[$C000 + (direccion and $1FFF)] := valor;
  end;
end;

function sg_inbyte(puerto: word): byte;
begin
  sg_inbyte := $FF;
  case (puerto and $FF) of
    $80 .. $BF:
      if (puerto and $01) <> 0 then
        sg_inbyte := tms_0.register_r
      else
        sg_inbyte := tms_0.vram_r;
    $C0 .. $FF:
      if (puerto and 1) <> 0 then
        sg_inbyte := sg1000_0.keys[1]
      else
        sg_inbyte := sg1000_0.keys[0];
  end;
end;

procedure sg_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $40 .. $7F:
      sn_76496_0.Write(valor);
    $80 .. $BF:
      if (puerto and $1) <> 0 then
        tms_0.register_w(valor)
      else
        tms_0.vram_w(valor);
    $C0 .. $FF:
      ; // mandos
  end;
end;

procedure sg_interrupt(int: boolean);
begin
  if int then
    z80_0.change_irq(ASSERT_LINE)
  else
    z80_0.change_irq(CLEAR_LINE);
end;

procedure sg_sound_update;
begin
  sn_76496_0.update;
end;

// Main
procedure reset_sg;
begin
  z80_0.reset;
  sn_76496_0.reset;
  tms_0.reset;
  reset_audio;
  sg1000_0.keys[0] := $FF;
  sg1000_0.keys[1] := $FF;
  sg1000_0.push_pause := false;
end;

procedure sg1000_grabar_snapshot;
var
  nombre: string;
begin
  nombre := snapshot_main_write;
  Directory.sg1000 := ExtractFilePath(nombre);
end;

procedure abrir_sg;
var
  extension, nombre_file, romfile: string;
  datos: pbyte;
  longitud: integer;
  crc_val: dword;
begin
  if not(openrom(romfile)) then
    exit;
  getmem(datos, $10000);
  if not(extract_data(romfile, datos, longitud, nombre_file)) then
  begin
    freemem(datos);
    exit;
  end;
  extension := extension_fichero(nombre_file);
  // Resetear variables siempre antes de cargar el snapshot!!!
  sg1000_0.ram_8k := false;
  sg1000_0.mid_8k_ram := false;
  if longitud > 49152 then
    longitud := 49152;
  if (extension = 'DSP') then
    snapshot_r(datos, longitud)
  else
  begin
    copymemory(@memory[0], datos, longitud);
    reset_sg;
  end;
  crc_val := calc_crc(datos, longitud);
  freemem(datos);
  case dword(crc_val) of
    // BomberMan Super (2), King's Valley, Knightmare, Legend of Kage, Rally X, Road Fighter, Tank Battalion, Twinbee, YieAr KungFu II
    $69FC1494, $CE5648C3, $223397A1, $281D2888, $2E7166D5, $306D5F78, $29E047CC, $5CBD1163,
      $C550B4F0, $FC87463C:
      sg1000_0.ram_8k := true;
    // Castle, Othello (2)
    $92F29D6, $AF4F14BC, $1D1A0CA3:
      sg1000_0.mid_8k_ram := true;
  end;
  Directory.sg1000 := ExtractFilePath(romfile);
end;

function start_sg1000: boolean;
begin
  start_sg1000 := false;
  machine_calls.general_loop := sg_loop;
  machine_calls.reset := reset_sg;
  machine_calls.cartridges := abrir_sg;
  machine_calls.take_snapshot := sg1000_grabar_snapshot;
  machine_calls.fps_max := 59.922743;
  start_audio(false);
  screen_init(1, 284, 243);
  start_video(284, 243);
  // Main CPU
  z80_0 := cpu_z80.create(3579545, 262);
  z80_0.change_ram_calls(sg_getbyte, sg_putbyte);
  z80_0.change_io_calls(sg_inbyte, sg_outbyte);
  z80_0.init_sound(sg_sound_update);
  // TMS
  tms_0 := tms99xx_chip.create(1, sg_interrupt);
  // Chip Sonido
  sn_76496_0 := sn76496_chip.create(3579545);
  // final
  reset_sg;
  if main_vars.console_init then
    abrir_sg;
  start_sg1000 := true;
end;

end.
