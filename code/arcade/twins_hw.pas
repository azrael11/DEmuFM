unit twins_hw;

interface

uses
  WinApi.Windows,
  nec_v20_v30,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ay_8910,
  misc_functions,
  i2cmem,
  file_engine;

function start_twins: boolean;

implementation
uses
  uDataModule;

const
        twins_rom:array[0..1] of tipo_roms=(
        (n:'2.u8';l:$80000;p:0;crc:$1ec942b0),(n:'1.u9';l:$80000;p:1;crc:$4417ff34));
        twins_nv:tipo_roms=(n:'24c02.u15';l:$100;p:0;crc:$2ff05b0e);
        twinsed1_rom:array[0..1] of tipo_roms=(
        (n:'1.bin';l:$80000;p:0;crc:$d5ef7b0d),(n:'2.bin';l:$80000;p:1;crc:$8a5392f4));
        hotblock_rom:array[0..1] of tipo_roms=(
        (n:'hotblk5.ic4';l:$80000;p:0;crc:$5f90f776),(n:'hotblk6.ic5';l:$80000;p:$80000;crc:$3176d231));

var
  main_rom: array [0 .. $FFFFF] of byte;
  ram, vram: array [0 .. $FFFF] of byte;
  int_index, pal_index, pal_mask, vbank: byte;
  video_render: procedure;

procedure update_video_twins;
var
  x, y: word;
  punt: array [0 .. $FFEF] of word;
begin
  for x := 0 to 319 do
    for y := 0 to 203 do
      punt[y + (x * 204)] := paleta[vram[y + (x * 204)]];
  putpixel(0, 0, $FF00, @punt, 1);
  update_region(0, 0, 320, 204, 1, 0, 0, 320, 204, PANT_TEMP);
end;

procedure update_video_hotblock;
var
  x, y: word;
  punt: array [0 .. $F9FF] of word;
begin
  if (vbank and $40) = 0 then
    fill_full_screen(1, $100)
  else
  begin
    for x := 0 to 319 do
      for y := 0 to 199 do
        punt[y + (x * 200)] := paleta[vram[y + (x * 200)]];
    putpixel(0, 0, $FA00, @punt, 1);
  end;
  update_region(0, 0, 320, 200, 1, 0, 0, 320, 200, PANT_TEMP);
end;

procedure events_twins;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;


// needs pause
procedure twins_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
 for f:=0 to 311 do begin
    events_twins;
    if f=204 then begin
      nec_0.set_input(NMI_IRQ,HOLD_LINE);
      video_render;
    end;
    //Main CPU
    nec_0.run(frame_main);
    frame_main:=frame_main+nec_0.tframes-nec_0.contador;
 end;
 video_sync;
  end;
end;

function twins_getbyte(direccion: dword): byte;
begin
  case direccion of
    0 .. $FFFF:
      twins_getbyte := ram[direccion];
    $10000 .. $1FFFF:
      twins_getbyte := vram[direccion and $FFFF];
    $20000 .. $FFFFF:
      twins_getbyte := main_rom[direccion];
  end;
end;

procedure twins_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    0 .. $FFFF:
      ram[direccion] := valor;
    $10000 .. $1FFFF:
      vram[direccion and $FFFF] := valor;
    $20000 .. $FFFFF:
      ; // ROM
  end;
end;

function twins_inbyte(puerto: word): byte;
begin
  case puerto of
    4:
      begin
        twins_inbyte := buffer_paleta[pal_index or (int_index shl 8)];
        int_index := int_index + 1;
        if int_index = 3 then
        begin
          int_index := 0;
          pal_index := pal_index + 1;
        end;
      end;
    $10:
      twins_inbyte := ay8910_0.read;
    $18:
      twins_inbyte := i2cmem_0.read_sda;
  end;
end;

procedure twins_outbyte(puerto: word; valor: byte);
var
  color: tcolor;
begin
  case puerto of
    0:
      pal_index := valor;
    2:
      pal_mask := valor;
    4:
      begin
        buffer_paleta[pal_index or (int_index shl 8)] := valor;
        color.r := pal6bit(buffer_paleta[pal_index]);
        color.g := pal6bit(buffer_paleta[pal_index or $100]);
        color.b := pal6bit(buffer_paleta[pal_index or $200]);
        set_pal_color(color, pal_index);
        int_index := int_index + 1;
        if int_index = 3 then
        begin
          int_index := 0;
          pal_index := pal_index + 1;
        end;
      end;
    8:
      ay8910_0.Control(valor);
    $10:
      ay8910_0.Write(valor);
    $18:
      begin
        i2cmem_0.write_scl((valor shr 1) and 1);
        i2cmem_0.write_sda(valor and 1);
      end;
  end;
end;

function twins_read_porta: byte;
begin
  twins_read_porta := marcade.in0;
end;

function twins_read_portb: byte;
begin
  twins_read_portb := marcade.in1;
end;

procedure twins_update_sound;
begin
  ay8910_0.update;
end;

// Twins Electronic Devices
function twinsed1_inbyte(puerto: word): byte;
begin
  case puerto of
    2:
      twinsed1_inbyte := ay8910_0.read;
    4:
      twinsed1_inbyte := i2cmem_0.read_sda;
  end;
end;

procedure twinsed1_outbyte(puerto: word; valor: byte);
begin
  case puerto of
    0:
      ay8910_0.Control(valor);
    2:
      ay8910_0.Write(valor);
    4:
      begin
        i2cmem_0.write_scl((valor shr 1) and 1);
        i2cmem_0.write_sda(valor and 1);
      end;
  end;
end;

procedure twinsed1_outword(puerto: dword; valor: word);
var
  color: tcolor;
begin
  case puerto of
    6:
      begin
        color.r := pal5bit(BITSWAP8(valor and $1F, 7, 6, 5, 0, 1, 2, 3, 4));
        color.g := pal5bit(BITSWAP8((valor shr 5) and $1F, 7, 6, 5, 0, 1, 2, 3, 4));
        color.b := pal5bit(BITSWAP8((valor shr 10) and $1F, 7, 6, 5, 0, 1, 2, 3, 4));
        set_pal_color(color, pal_index);
        pal_index := pal_index + 1;
      end;
    $E:
      pal_index := 0;
  end;
end;

// Hot Block
function hotblock_getbyte(direccion: dword): byte;
begin
  case direccion of
    0 .. $FFFF:
      hotblock_getbyte := ram[direccion];
    $10000 .. $1FFFF:
      case vbank of
        $88, $C8:
          hotblock_getbyte := vram[direccion and $FFFF];
        $A8, $E8:
          hotblock_getbyte := buffer_paleta[direccion and $1FF];
      end;
    $20000 .. $FFFFF:
      hotblock_getbyte := main_rom[direccion];
  end;
end;

procedure hotblock_putbyte(direccion: dword; valor: byte);
var
  color: tcolor;
  tmpw: word;
begin
  case direccion of
    0 .. $FFFF:
      ram[direccion] := valor;
    $10000 .. $1FFFF:
      case vbank of
        $88, $C8:
          vram[direccion and $FFFF] := valor;
        $A8, $E8:
          begin
            buffer_paleta[direccion and $1FF] := valor;
            tmpw := buffer_paleta[direccion and $1FE] or (buffer_paleta[(direccion and $1FF) or 1] shl 8);
            color.b := pal5bit(tmpw shr 10);
            color.g := pal5bit(tmpw shr 5);
            color.r := pal5bit(tmpw shr 0);
            set_pal_color(color, (direccion and $1FF) shr 1);
          end;
      end;
    $20000 .. $FFFFF:
      ; // ROM
  end;
end;

function hotblock_inbyte(puerto: word): byte;
begin
  case puerto of
    4:
      hotblock_inbyte := i2cmem_0.read_sda;
    $8001:
      hotblock_inbyte := ay8910_0.read;
  end;
end;

procedure hotblock_outbyte(puerto: word; valor: byte);
begin
  case puerto of
    0:
      vbank := valor;
    4:
      begin
        i2cmem_0.write_scl((valor shr 1) and 1);
        i2cmem_0.write_sda(valor and 1);
      end;
    $8000:
      ay8910_0.Control(valor);
    $8001:
      ay8910_0.Write(valor);
  end;
end;

// Main
procedure reset_twins;
begin
  nec_0.reset;
 frame_main:=nec_0.tframes;
  ay8910_0.reset;
  i2cmem_0.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  pal_index := 0;
  pal_mask := $FF;
  int_index := 0;
  vbank := 0;
end;

procedure close_twins;
begin
  case main_vars.machine_type of
    341:
      i2cmem_0.write_data(dm.tConfignvram.AsString + 'twins.nv');
    342:
      i2cmem_0.write_data(dm.tConfignvram.AsString + 'twinsed.nv');
    343:
      i2cmem_0.write_data(dm.tConfignvram.AsString + 'hotblock.nv');
  end;
end;

function start_twins: boolean;
var
  ay_clock: byte;
  longitud: integer;
  memory_temp: array [0 .. $200] of byte;
begin
  start_twins := false;
  machine_calls.reset := reset_twins;
  machine_calls.fps_max := 50.080128;
  machine_calls.general_loop := twins_loop;
  machine_calls.close := close_twins;
  start_audio(false);
  if main_vars.machine_type <> 343 then
  begin
    screen_init(1, 320, 204);
    start_video(320, 204);
    // Main CPU
    nec_0 := cpu_nec.create(8000000, 312, NEC_V30);
    nec_0.change_ram_calls(twins_getbyte, twins_putbyte);
    ay_clock := 2;
  end
  else
  begin
    screen_init(1, 320, 200);
    start_video(320, 200);
    // Main CPU
    nec_0 := cpu_nec.create(8000000, 312, NEC_V20);
    nec_0.change_ram_calls(hotblock_getbyte, hotblock_putbyte);
    ay_clock := 1;
  end;
  nec_0.init_sound(twins_update_sound);
  // Eeprom
  i2cmem_0 := i2cmem_chip.create(I2C_24C02);
  // Sound
  ay8910_0 := ay8910_chip.create(1000000 * ay_clock, AY8910);
  ay8910_0.change_io_calls(twins_read_porta, twins_read_portb, nil, nil);
  case main_vars.machine_type of
    341:
      begin // twins
        if not(roms_load16b(@main_rom, twins_rom)) then
          exit;
        main_rom[$3497D] := $90;
        main_rom[$3497E] := $90;
        main_rom[$34986] := $90;
        main_rom[$34987] := $90;
        nec_0.change_io_calls(twins_inbyte, twins_outbyte);
        video_render := update_video_twins;
        if read_file_size(dm.tConfignvram.AsString + 'twins.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'twins.nv', @memory_temp, longitud)
        else if not(roms_load(@memory_temp, twins_nv)) then
          exit;
        i2cmem_0.load_data(@memory_temp);
      end;
    342:
      begin // twins ED
        if not(roms_load16b(@main_rom, twinsed1_rom)) then
          exit;
        nec_0.change_io_calls(twinsed1_inbyte, twinsed1_outbyte);
        nec_0.change_io_calls16(nil, twinsed1_outword);
        video_render := update_video_twins;
        if read_file_size(dm.tConfignvram.AsString+ 'twinsed.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'twinsed.nv', @memory_temp, longitud)
        else if not(roms_load(@memory_temp, twins_nv)) then
          exit;
        i2cmem_0.load_data(@memory_temp);
      end;
    343:
      begin // hot block
        if not(roms_load(@main_rom, hotblock_rom)) then
          exit;
        nec_0.change_io_calls(hotblock_inbyte, hotblock_outbyte);
        video_render := update_video_hotblock;
        if read_file_size(dm.tConfignvram.AsString+ 'hotblock.nv', longitud) then
        begin
          read_file(dm.tConfignvram.AsString + 'hotblock.nv', @memory_temp[0], longitud);
          i2cmem_0.load_data(@memory_temp);
        end;
      end;
  end;
  start_twins := true;
end;

end.
