unit sega_gg;

interface

uses
  nz80,
  WinApi.Windows,
  main_engine,
  controls_engine,
  sega_vdp,
  sn_76496,
  System.SysUtils,
  misc_functions,
  sound_engine,
  sms;

function start_gg: boolean;

implementation

uses
  main,
  snapshot;

var
  gg_0: tmastersystem;

procedure events_gg;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      gg_0.keys[0] := (gg_0.keys[0] and $FE)
    else
      gg_0.keys[0] := (gg_0.keys[0] or 1);
    if p_contrls.map_arcade.down[0] then
      gg_0.keys[0] := (gg_0.keys[0] and $FD)
    else
      gg_0.keys[0] := (gg_0.keys[0] or 2);
    if p_contrls.map_arcade.left[0] then
      gg_0.keys[0] := (gg_0.keys[0] and $FB)
    else
      gg_0.keys[0] := (gg_0.keys[0] or 4);
    if p_contrls.map_arcade.right[0] then
      gg_0.keys[0] := (gg_0.keys[0] and $F7)
    else
      gg_0.keys[0] := (gg_0.keys[0] or 8);
    if p_contrls.map_arcade.but0[0] then
      gg_0.keys[0] := (gg_0.keys[0] and $EF)
    else
      gg_0.keys[0] := (gg_0.keys[0] or $10);
    if p_contrls.map_arcade.but1[0] then
      gg_0.keys[0] := (gg_0.keys[0] and $DF)
    else
      gg_0.keys[0] := (gg_0.keys[0] or $20);
    if p_contrls.map_arcade.start[0] then
      gg_0.keys[1] := (gg_0.keys[1] and $7F)
    else
      gg_0.keys[1] := (gg_0.keys[1] or $80);
  end;
end;

procedure gg_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, true, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to (vdp_0.VIDEO_Y_TOTAL - 1) do
    begin
      z80_0.run(frame);
      frame := frame + z80_0.tframes - z80_0.contador;
      vdp_0.refresh(f);
    end;
    if vdp_0.gg_set then
      update_region(61, 51, 160, 144, 1, 0, 0, 160, 144, PANT_TEMP)
    else
      update_region(0, 0, 284, 243, 1, 0, 0, 284, 243, PANT_TEMP);
    events_gg;
    video_sync;
  end;
end;

function gg_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      gg_getbyte := gg_0.mapper.rom[gg_0.mapper.rom_bank[0], direccion];
    $4000 .. $7FFF:
      gg_getbyte := gg_0.mapper.rom[gg_0.mapper.rom_bank[1], direccion and $3FFF];
    $8000 .. $BFFF:
      if gg_0.mapper.slot2_ram then
        gg_getbyte := gg_0.mapper.ram_slot2[gg_0.mapper.slot2_bank, direccion and $3FFF]
      else
        gg_getbyte := gg_0.mapper.rom[gg_0.mapper.rom_bank[2], direccion and $3FFF];
    $C000 .. $FFFF:
      gg_getbyte := gg_0.mapper.ram[direccion and $1FFF];
  end;
end;

procedure gg_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $BFFF:
      if gg_0.mapper.slot2_ram then
        gg_0.mapper.ram_slot2[gg_0.mapper.slot2_bank, direccion and $3FFF] := valor;
    $C000 .. $FFFB:
      gg_0.mapper.ram[direccion and $1FFF] := valor;
    $FFFC .. $FFFF:
      begin
        gg_0.mapper.ram[direccion and $1FFF] := valor;
        case (direccion and $3) of
          0:
            begin
              gg_0.mapper.slot2_ram := (valor and 8) <> 0;
              gg_0.mapper.slot2_bank := (valor and 4) shr 2;
              // if (valor and $3<>0) then MessageDlg('Escribe $fffc '+inttohex(valor,2), mtInformation,[mbOk], 0);
            end;
          1:
            gg_0.mapper.rom_bank[0] := valor mod gg_0.mapper.max;
          2:
            gg_0.mapper.rom_bank[1] := valor mod gg_0.mapper.max;
          3:
            gg_0.mapper.rom_bank[2] := valor mod gg_0.mapper.max;
        end;
      end;
  end;
end;

function gg_inbyte(puerto: word): byte;
begin
  puerto := puerto and $FF;
  case puerto of
    0:
      gg_inbyte := gg_0.keys[1] or $40;
    1 .. 6:
      gg_inbyte := gg_0.io[puerto];
    7 .. $3F, $C2 .. $DB, $DE .. $FF:
      gg_inbyte := $FF;
    $40 .. $7F:
      if (puerto and 1) <> 0 then
        gg_inbyte := vdp_0.hpos
      else
        gg_inbyte := vdp_0.linea_back;
    $80 .. $BF:
      if (puerto and $01) <> 0 then
        gg_inbyte := vdp_0.register_r
      else
        gg_inbyte := vdp_0.vram_r;
    $C0, $DC:
      gg_inbyte := gg_0.keys[0];
    $C1, $DD:
      gg_inbyte := $FF;
  end;
end;

function gg_getbyte_codemasters(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      gg_getbyte_codemasters := gg_0.mapper.rom[gg_0.mapper.rom_bank[0], direccion];
    $4000 .. $7FFF:
      gg_getbyte_codemasters := gg_0.mapper.rom[gg_0.mapper.rom_bank[1], direccion and $3FFF];
    $8000 .. $9FFF:
      gg_getbyte_codemasters := gg_0.mapper.rom[gg_0.mapper.rom_bank[2], direccion and $3FFF];
    $A000 .. $BFFF:
      if gg_0.mapper.slot2_ram then
        gg_getbyte_codemasters := gg_0.mapper.ram_slot2[0, direccion and $1FFF]
      else
        gg_getbyte_codemasters := gg_0.mapper.rom[gg_0.mapper.rom_bank[2], direccion and $3FFF];
    $C000 .. $FFFF:
      gg_getbyte_codemasters := gg_0.mapper.ram[direccion and $1FFF];
  end;
end;

procedure gg_putbyte_codemasters(direccion: word; valor: byte);
begin
  case direccion of
    0:
      gg_0.mapper.rom_bank[0] := valor mod gg_0.mapper.max; // mapper slot 0
    $4000:
      begin // mapper slot 1
        gg_0.mapper.rom_bank[1] := (valor and $7F) mod gg_0.mapper.max;
        gg_0.mapper.slot2_ram := (valor and $80) <> 0;
      end;
    $8000:
      gg_0.mapper.rom_bank[2] := valor mod gg_0.mapper.max; // mapper slot 2
    $A000 .. $BFFF:
      if gg_0.mapper.slot2_ram then
        gg_0.mapper.ram_slot2[0, direccion and $1FFF] := valor;
    $C000 .. $FFFF:
      gg_0.mapper.ram[direccion and $1FFF] := valor;
  end;
end;

procedure memory_control(valor: byte);
begin
  // gg_0.bios_enabled:=(valor and 8)=0;
end;

procedure gg_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0 .. 6:
      gg_0.io[puerto and $FF] := valor;
    7 .. $3F:
      if (puerto and $1) = 0 then
        memory_control(valor);
    $40 .. $7F:
      sn_76496_0.Write(valor);
    $80 .. $BF:
      if (puerto and $1) <> 0 then
        vdp_0.register_w(valor)
      else
        vdp_0.vram_w(valor);
    $C0 .. $FF:
      ;
  end;
end;

procedure gg_interrupt(int: boolean);
begin
  if int then
    z80_0.change_irq(ASSERT_LINE)
  else
    z80_0.change_irq(CLEAR_LINE);
end;

procedure gg_sound_update;
begin
  sn_76496_0.update;
end;

procedure gg_set_hpos(estados: word);
begin
  vdp_0.set_hpos(z80_0.contador);
end;

procedure reset_gg;
var
  z80_r: npreg_z80;
begin
  z80_0.reset;
  z80_r := z80_0.get_internal_r;
  z80_r.sp := $DFEB;
  z80_r.bc.w := $FF3C;
  z80_r.bc2.w := $300;
  z80_r.de2.w := $C73C;
  z80_r.hl.w := $2;
  z80_r.hl2.w := $C739;
  z80_r.a := $14;
  sn_76496_0.reset;
  vdp_0.reset;
  reset_audio;
  gg_0.mapper.slot2_ram := false;
  gg_0.keys[0] := $FF;
  gg_0.keys[1] := $80;
  gg_0.mapper.rom_bank[0] := 0;
  gg_0.mapper.rom_bank[1] := 1 mod gg_0.mapper.max;
  gg_0.mapper.rom_bank[2] := 2 mod gg_0.mapper.max;
  gg_0.mapper.rom_bank[3] := 0;
  gg_0.mapper.slot2_bank := 0;
  gg_0.io[0] := $C0;
  gg_0.io[1] := $7F;
  gg_0.io[2] := $FF;
  gg_0.io[3] := 0;
  gg_0.io[4] := $FF;
  gg_0.io[5] := 0;
  gg_0.io[6] := $FF;
end;

procedure abrir_gg;
  function abrir_cartucho_gg(data: pbyte; long: dword): boolean;
  var
    ptemp: pbyte;
    long_temp: dword;
  begin
    fillchar(sms_0.mapper.rom[0], sizeof(sms_0.mapper.rom), 0);
    ptemp := data;
    gg_0.mapper.max := 0;
    if (long mod $4000) = 512 then
    begin
      inc(ptemp, 512);
      long_temp := long - 512;
    end
    else
      long_temp := long;
    while long_temp > 0 do
    begin
      if long_temp < $4000 then
      begin
        copymemory(@gg_0.mapper.rom[gg_0.mapper.max, 0], ptemp, long_temp);
        long_temp := 0;
      end
      else
      begin
        copymemory(@gg_0.mapper.rom[gg_0.mapper.max, 0], ptemp, $4000);
        inc(ptemp, $4000);
        long_temp := long_temp - $4000;
      end;
      gg_0.mapper.max := gg_0.mapper.max + 1;
    end;
    abrir_cartucho_gg := true;
    reset_gg;
  end;

var
  extension, nombre_file, romfile: string;
  datos: pbyte;
  longitud: integer;
  crc_val: dword;
begin
  if not(openrom(romfile)) then
    exit;
  getmem(datos, $400000);
  if not(extract_data(romfile, datos, longitud, nombre_file)) then
  begin
    freemem(datos);
    exit;
  end;
  extension := extension_fichero(nombre_file);
  if not(vdp_0.gg_set) then
  begin
    vdp_0.set_gg(true);
    change_video_size(160, 144);
  end;
  z80_0.change_ram_calls(gg_getbyte, gg_putbyte);
  crc_val := calc_crc(datos, longitud);
  case crc_val of
    $5E53C7F7, $DBE8895C, $F7C524F6, $C888222B, $AA140C9C, $8813514B, $9FA727A0, $FB481971, $D9A7F170, $76C5BDFB, $C1756BEE, $6CAA625B, $152F0DCC, $72981057:
      begin // Codemasters
        z80_0.change_ram_calls(gg_getbyte_codemasters, gg_putbyte_codemasters);
      end;
  end;
  case crc_val of // Video especial... Tamaño SMS
    $E5F789B9, $9942B69B, $5877B10D, $59840FD6, $AA140C9C, $C8381DEF, $C888222B, $76C5BDFB, $1D93246E, $CE97EFE8, $A2F9C7AF, $3382D73F, $1EAB89D, $F037EC00, $2AA12D7E, $189931E, $86E5B455, $45F058D6,
      $311D2863, $BA6344FC, $1C6C149C, $9C76FB3A, $56201996, $4902B7A2, $FB481971, $9FA727A0, $10DBBEF4, $BD1CC7DF, $8230384E, $DA8E95A9, $6F8E46CF, $7BB81E3D, $44FBE8F6, $3B627808, $18086B70,
      $8813514B:
      if vdp_0.gg_set then
      begin
        vdp_0.set_gg(false);
        change_video_size(284, 243);
      end;
  end;
  if extension = 'DSP' then
    snapshot_r(datos, longitud)
  else
    abrir_cartucho_gg(datos, longitud);
  Directory.gg := ExtractFilePath(romfile);
  freemem(datos);
end;

function read_memory(direccion: word): byte;
begin
  read_memory := vdp_0.tms.mem[direccion];
end;

procedure write_memory(direccion: word; valor: byte);
begin
  vdp_0.tms.mem[direccion] := valor;
end;

procedure gg_grabar_snapshot;
var
  nombre: string;
begin
  nombre := snapshot_main_write;
  Directory.gg := ExtractFilePath(nombre);
end;

function start_gg: boolean;
begin
  start_gg := false;
  machine_calls.general_loop := gg_loop;
  machine_calls.reset := reset_gg;
  machine_calls.cartridges := abrir_gg;
  machine_calls.take_snapshot := gg_grabar_snapshot;
  machine_calls.fps_max := FPS_NTSC;
  start_audio(false);
  screen_init(1, 284, 243);
  start_video(160, 144);
  // Main CPU
  z80_0 := cpu_z80.create(CLOCK_NTSC, LINES_NTSC);
  z80_0.change_ram_calls(gg_getbyte, gg_putbyte);
  z80_0.change_io_calls(gg_inbyte, gg_outbyte);
  z80_0.init_sound(gg_sound_update);
  z80_0.change_misc_calls(gg_set_hpos);
  // VDP
  vdp_0 := vdp_chip.create(1, gg_interrupt, z80_0.numero_cpu, read_memory, write_memory);
  vdp_0.video_ntsc(0);
  vdp_0.set_gg(true); // Lo pongo temporalmente, hasta que compruebe que no está en modo SMS
  // Sound
  sn_76496_0 := sn76496_chip.create(CLOCK_NTSC);
  // Importante!!!
  fillchar(sms_0.mapper.bios[0], sizeof(sms_0.mapper.bios), 0);
  fillchar(sms_0.mapper.rom[0], sizeof(sms_0.mapper.rom), 0);
  gg_0.mapper.max := 1;
  reset_gg;
  if main_vars.console_init then
    abrir_gg;
  start_gg := true;
end;

end.
