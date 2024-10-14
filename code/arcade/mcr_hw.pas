unit mcr_hw;

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
  z80daisy,
  z80ctc,
  timer_engine,
  file_engine;

function start_mcr: boolean;

implementation

const
  tapper_rom: array [0 .. 3] of tipo_roms = ((n: 'tapper_c.p.u._pg_0_1c_1-27-84.1c'; l: $4000; p: 0; crc: $BB060BB0), (n: 'tapper_c.p.u._pg_1_2c_1-27-84.2c'; l: $4000; p: $4000; crc: $FD9ACC22),
    (n: 'tapper_c.p.u._pg_2_3c_1-27-84.3c'; l: $4000; p: $8000; crc: $B3755D41), (n: 'tapper_c.p.u._pg_3_4c_1-27-84.4c'; l: $2000; p: $C000; crc: $77273096));
  tapper_snd: array [0 .. 3] of tipo_roms = ((n: 'tapper_sound_snd_0_a7_12-7-83.a7'; l: $1000; p: 0; crc: $0E8BB9D5), (n: 'tapper_sound_snd_1_a8_12-7-83.a8'; l: $1000; p: $1000; crc: $0CF0E29B),
    (n: 'tapper_sound_snd_2_a9_12-7-83.a9'; l: $1000; p: $2000; crc: $31EB6DC6), (n: 'tapper_sound_snd_3_a10_12-7-83.a10'; l: $1000; p: $3000; crc: $01A9BE6A));
  tapper_char: array [0 .. 1] of tipo_roms = ((n: 'tapper_c.p.u._bg_1_6f_12-7-83.6f'; l: $4000; p: 0; crc: $2A30238C), (n: 'tapper_c.p.u._bg_0_5f_12-7-83.5f'; l: $4000; p: $4000; crc: $394AB576));
  tapper_sprites: array [0 .. 7] of tipo_roms = ((n: 'tapper_video_fg_1_a7_12-7-83.a7'; l: $4000; p: 0; crc: $32509011), (n: 'tapper_video_fg_0_a8_12-7-83.a8'; l: $4000; p: $4000; crc: $8412C808),
    (n: 'tapper_video_fg_3_a5_12-7-83.a5'; l: $4000; p: $8000; crc: $818FFFD4), (n: 'tapper_video_fg_2_a6_12-7-83.a6'; l: $4000; p: $C000; crc: $67E37690), (n: 'tapper_video_fg_5_a3_12-7-83.a3';
    l: $4000; p: $10000; crc: $800F7C8A), (n: 'tapper_video_fg_4_a4_12-7-83.a4'; l: $4000; p: $14000; crc: $32674EE6), (n: 'tapper_video_fg_7_a1_12-7-83.a1'; l: $4000; p: $18000; crc: $070B4C81),
    (n: 'tapper_video_fg_6_a2_12-7-83.a2'; l: $4000; p: $1C000; crc: $A37AEF36));
  // DIP
  tapper_dipa: array [0 .. 3] of def_dip = ((mask: $4; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $40; name: 'Cabinet'; number: 2; dip: ((dip_val: $40; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Coin Meters'; number: 2; dip: ((dip_val: $80; dip_name: '1'), (dip_val: $0; dip_name: '2'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 8;

var
  nvram: array [0 .. $7FF] of byte;
  // video
  // prio:array[0..$1f,0..$1f] of byte;
  // Sonido
  ssio_status: byte;
  ssio_data: array [0 .. 3] of byte;
  ssio_14024_count: byte;

procedure update_video_tapper;
  procedure put_sprite_mcr(pri: byte);
  var
    prioridad, f, color, atrib, x, y, pos_y: byte;
    temp, temp2: pword;
    pos: pbyte;
    dir_x, dir_y: integer;
    nchar, sx, sy: word;
  begin
    for f := 0 to $7F do
    begin
      sx := ((memory[$E803 + (f * 4)] - 3) * 2) and $1FF;
      sy := ((241 - memory[$E800 + (f * 4)]) * 2) and $1FF;
      nchar := memory[$E802 + (f * 4)] + (((memory[$E801 + (f * 4)] shr 3) and $1) shl 8);
      atrib := memory[$E801 + (f * 4)];
      color := (not(memory[$E801 + (f * 4)]) and 3) shl 4;
      pos := gfx[1].datos;
      inc(pos, nchar * 32 * 32);
      if (atrib and $20) <> 0 then
      begin
        pos_y := 31;
        dir_y := -1;
      end
      else
      begin
        pos_y := 0;
        dir_y := 1;
      end;
      if (atrib and $10) <> 0 then
      begin
        temp2 := punbuf;
        inc(temp2, 31);
        dir_x := -1;
      end
      else
      begin
        temp2 := punbuf;
        dir_x := 1;
      end;
      for y := 0 to 31 do
      begin
        temp := temp2;
        for x := 0 to 31 do
        begin
          // prioridad:=prio[((sx+x) and $1ff) div 16,((sy+y) and $1ff) div 16];
          // if prio[((sx+x) and $1ff) div 16,((sy+y) and $1ff) div 16]<>pri then begin
          // temp^:=paleta[MAX_COLORES];
          // end else begin
          if (gfx[1].colores[pos^ + color] and $7) <> 0 then
            temp^ := paleta[gfx[1].colores[pos^ + color]]
          else
            temp^ := paleta[MAX_COLORS];
          // end;
          inc(temp, dir_x);
          inc(pos);
        end;
        putpixel_gfx_int(0, pos_y, 32, PANT_SPRITES);
        pos_y := pos_y + dir_y;
      end;
      update_gfx_sprite(sx, sy, 2, 1);
    end;
  end;

var
  x, y, h, atrib2: byte;
  atrib, f, nchar, color: word;
begin
  // fill_full_screen(5,$100);
  for f := 0 to $3BF do
  begin
    atrib := memory[$F000 + (f * 2)] or (memory[$F001 + (f * 2)] shl 8);
    color := (atrib shr 12) and 3;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f and $1F;
      y := f shr 5;
      nchar := atrib and $3FF;
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 1, 0, (atrib and $400) <> 0, (atrib and $800) <> 0);
      atrib2 := ((atrib shr 14) and 3) + 1;
      // prio[x, y] := atrib2;
      for h := 1 to 4 do
      begin
        if h = atrib2 then
          put_gfx_flip(x * 16, y * 16, nchar, color shl 4, h, 0, (atrib and $400) <> 0, (atrib and $800) <> 0)
        else
          put_gfx_block_trans(x * 16, y * 16, h, 16, 16);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 512, 480, 1, 0, 0, 512, 480, 2);
  put_sprite_mcr(1);
  update_region(0, 0, 512, 480, 2, 0, 0, 512, 480, 5);
  put_sprite_mcr(2);
  update_region(0, 0, 512, 480, 3, 0, 0, 512, 480, 5);
  put_sprite_mcr(3);
  update_region(0, 0, 512, 480, 4, 0, 0, 512, 480, 5);
  put_sprite_mcr(4);
  update_final_piece(0, 0, 512, 480, 2);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_tapper;
begin
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    // ip1
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
    // ip2
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
  end;
end;

procedure tapper_loop;
var
  frame_m, frame_s: single;
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 479 do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // Main CPU
          z80_0.run(frame_m);
          frame_m := frame_m + z80_0.tframes - z80_0.contador;
          // Sound
          z80_1.run(frame_s);
          frame_s := frame_s + z80_1.tframes - z80_1.contador;
        end;
        if ((f = 0) or (f = 239)) then
        begin
          ctc_0.trigger(2, true);
          ctc_0.trigger(2, false);
        end;
        if (f = 0) then
        begin
          update_video_tapper;
          ctc_0.trigger(3, true);
          ctc_0.trigger(3, false);
        end;
      end;
      events_tapper;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function tapper_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $DFFF, $F000 .. $F7FF:
      tapper_getbyte := memory[direccion];
    $E000 .. $E7FF:
      tapper_getbyte := nvram[direccion and $7FF];
    $E800 .. $EBFF:
      tapper_getbyte := memory[$E800 + (direccion and $1FF)];
  else
    tapper_getbyte := $FF;
  end;
end;

procedure change_color(pos: byte; tmp_color: word);
var
  color: tcolor;
begin
  color.r := pal3bit(tmp_color shr 6);
  color.g := pal3bit(tmp_color shr 0);
  color.b := pal3bit(tmp_color shr 3);
  set_pal_color(color, pos);
  buffer_color[(pos shr 4) and 3] := true;
end;

procedure tapper_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ; // ROM
    $E000 .. $E7FF:
      nvram[direccion and $7FF] := valor;
    $E800 .. $EBFF:
      memory[$E800 + (direccion and $1FF)] := valor;
    $F000 .. $F7FF:
      begin
        memory[direccion] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $F800 .. $FFFF:
      change_color((direccion and $7F) shr 1, valor or ((direccion and 1) shl 8));
  end;
end;

function tapper_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $0 .. $1F:
      case (puerto and 7) of
        $0:
          tapper_inbyte := marcade.in0;
        $1:
          tapper_inbyte := marcade.in1;
        $2:
          tapper_inbyte := marcade.in2;
        $3:
          tapper_inbyte := marcade.dswa;
        $4:
          tapper_inbyte := $FF;
        $7:
          tapper_inbyte := ssio_status; // ssio_device_read;
      end;
    $F0 .. $F3:
      tapper_inbyte := ctc_0.read(puerto and $3);
  end;
end;

procedure tapper_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0 .. $7:
      ; // ssio_ioport_write;
    $1C .. $1F:
      ssio_data[puerto and 3] := valor; // ssio_write;
    $E0:
      ; // wathcdog
    $E8:
      ;
    $F0 .. $F3:
      ctc_0.write(puerto and $3, valor);
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      snd_getbyte := mem_snd[direccion];
    $8000 .. $8FFF:
      snd_getbyte := mem_snd[$8000 or (direccion and $3FF)];
    $9000 .. $9FFF:
      snd_getbyte := ssio_data[direccion and $3];
    $A000 .. $AFFF:
      if (direccion and 3) = 1 then
        snd_getbyte := ay8910_0.read;
    $B000 .. $BFFF:
      if (direccion and 3) = 1 then
        snd_getbyte := ay8910_1.read;
    $E000 .. $EFFF:
      begin
        ssio_14024_count := 0;
        z80_1.change_irq(CLEAR_LINE);
      end;
    $F000 .. $FFFF:
      snd_getbyte := $FF; // DIP
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $8000 .. $8FFF:
      mem_snd[$8000 or (direccion and $3FF)] := valor;
    $A000 .. $AFFF:
      case (direccion and 3) of
        $0:
          ay8910_0.Control(valor);
        $2:
          ay8910_0.write(valor);
      end;
    $B000 .. $BFFF:
      case (direccion and 3) of
        $0:
          ay8910_1.Control(valor);
        $2:
          ay8910_1.write(valor);
      end;
    $C000 .. $CFFF:
      ssio_status := valor;
  end;
end;

procedure z80ctc_int(state: byte);
begin
  z80_0.change_irq(state);
end;

procedure tapper_snd_irq;
begin
  //
  // /SINT is generated as follows:
  //
  // Starts with a 16MHz oscillator
  // /2 via 7474 flip-flop @ F11
  // /16 via 74161 binary counter @ E11
  // /10 via 74190 decade counter @ D11
  //
  // Bit 3 of the decade counter clocks a 14024 7-bit async counter @ C12.
  // This routine is called to clock this 7-bit counter.
  // Bit 6 of the output is inverted and connected to /SINT.
  //
  ssio_14024_count := (ssio_14024_count + 1) and $7F;
  // if the low 5 bits clocked to 0, bit 6 has changed state
  if ((ssio_14024_count and $3F) = 0) then
    if (ssio_14024_count and $40) <> 0 then
      z80_1.change_irq(ASSERT_LINE)
    else
      z80_1.change_irq(CLEAR_LINE);
end;

procedure tapper_update_sound;
begin
  tsample[ay8910_0.get_sample_num, sound_status.sound_position] := ay8910_0.update_internal^;
  tsample[ay8910_0.get_sample_num, sound_status.sound_position + 1] := ay8910_1.update_internal^;
end;

// Main
procedure mcr_reset;
begin
  z80_0.reset;
  z80_1.reset;
  ctc_0.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  // Sonido
  ssio_status := 0;
  fillchar(ssio_data[0], 4, 0);
  fillchar(nvram[0], $800, 1);
  ssio_14024_count := 0;
end;

procedure close_tapper;
begin
  write_file(Directory.Arcade_nvram + 'tapper.nv', @nvram, $800);
end;

function start_mcr: boolean;
const
  pc_x: array [0 .. 15] of dword = (0 * 2, 0 * 2, 1 * 2, 1 * 2, 2 * 2, 2 * 2, 3 * 2, 3 * 2, 4 * 2, 4 * 2, 5 * 2, 5 * 2, 6 * 2, 6 * 2, 7 * 2, 7 * 2);
  pc_y: array [0 .. 15] of dword = (0 * 16, 0 * 16, 1 * 16, 1 * 16, 2 * 16, 2 * 16, 3 * 16, 3 * 16, 4 * 16, 4 * 16, 5 * 16, 5 * 16, 6 * 16, 6 * 16, 7 * 16, 7 * 16);
  ps_x: array [0 .. 31] of dword = (0, 4, $100 * 32 * 32, $100 * 32 * 32 + 4, $200 * 32 * 32, $200 * 32 * 32 + 4, $300 * 32 * 32, $300 * 32 * 32 + 4, 8, 12, $100 * 32 * 32 + 8, $100 * 32 * 32 + 12,
    $200 * 32 * 32 + 8, $200 * 32 * 32 + 12, $300 * 32 * 32 + 8, $300 * 32 * 32 + 12, 16, 20, $100 * 32 * 32 + 16, $100 * 32 * 32 + 20, $200 * 32 * 32 + 16, $200 * 32 * 32 + 20, $300 * 32 * 32 + 16,
    $300 * 32 * 32 + 20, 24, 28, $100 * 32 * 32 + 24, $100 * 32 * 32 + 28, $200 * 32 * 32 + 24, $200 * 32 * 32 + 28, $300 * 32 * 32 + 24, $300 * 32 * 32 + 28);
  ps_y: array [0 .. 31] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32, 16 * 32, 17 * 32, 18 * 32,
    19 * 32, 20 * 32, 21 * 32, 22 * 32, 23 * 32, 24 * 32, 25 * 32, 26 * 32, 27 * 32, 28 * 32, 29 * 32, 30 * 32, 31 * 32);
var
  memory_temp: array [0 .. $1FFFF] of byte;
  longitud: integer;
begin
  machine_calls.general_loop := tapper_loop;
  machine_calls.reset := mcr_reset;
  machine_calls.fps_max := 30;
  machine_calls.close := close_tapper;
  start_mcr := false;
  start_audio(true);
  screen_init(1, 512, 480, true);
  // screen_init(2,512,480,true);
  // screen_init(3,512,480,true);
  // screen_init(4,512,480,true);
  screen_init(2, 512, 512, false, true);
  start_video(512, 480);
  // Main CPU
  z80_0 := cpu_z80.create(5000000, 480 * CPU_SYNC);
  z80_0.change_ram_calls(tapper_getbyte, tapper_putbyte);
  z80_0.change_io_calls(tapper_inbyte, tapper_outbyte);
  z80_0.daisy := true;
  ctc_0 := tz80ctc.create(z80_0.numero_cpu, 5000000, z80_0.clock, 0, CTC0_TRG01);
  ctc_0.change_calls(z80ctc_int);
  z80daisy_init(Z80_CTC0_TYPE);
  // Sound CPU
  z80_1 := cpu_z80.create(2000000, 480 * CPU_SYNC);
  z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_1.init_sound(tapper_update_sound);
  timers.init(z80_1.numero_cpu, 2000000 / (160 * 2 * 16 * 10), tapper_snd_irq, nil, true);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(2000000, AY8910, 1);
  ay8910_1 := ay8910_chip.create(2000000, AY8910, 1, true);
  // cargar roms
  if not(roms_load(@memory, tapper_rom)) then
    exit;
  // cargar roms sonido
  if not(roms_load(@mem_snd, tapper_snd)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, tapper_char)) then
    exit;
  init_gfx(0, 16, 16, $400);
  gfx_set_desc_data(4, 0, 16 * 8, $400 * 16 * 8, ($400 * 16 * 8) + 1, 0, 1);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, tapper_sprites)) then
    exit;
  init_gfx(1, 32, 32, $100);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 32, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // Cargar NVRam
  if read_file_size(Directory.Arcade_nvram + 'tapper.nv', longitud) then
    read_file(Directory.Arcade_nvram + 'tapper.nv', @nvram, longitud);
  // DIP
  marcade.dswa := $C0;
  marcade.dswa_val := @tapper_dipa;
  // final
  mcr_reset;
  start_mcr := true;
end;

end.
