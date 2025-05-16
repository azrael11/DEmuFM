unit kyugo_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_kyugo: boolean;

implementation

const
  repulse_rom: array [0 .. 2] of tipo_roms = ((n: 'repulse.b5'; l: $2000; p: 0; crc: $FB2B7C9D), (n: 'repulse.b6'; l: $2000; p: $2000; crc: $99129918), (n: '7.j4'; l: $2000; p: $4000; crc: $57A8E900));
  repulse_snd: array [0 .. 3] of tipo_roms = ((n: '1.f2'; l: $2000; p: 0; crc: $C485C621), (n: '2.h2'; l: $2000; p: $2000; crc: $B3C6A886), (n: '3.j2'; l: $2000; p: $4000; crc: $197E314C), (n: 'repulse.b4'; l: $2000; p: $6000; crc: $86B267F3));
  repulse_char: tipo_roms = (n: 'repulse.a11'; l: $1000; p: 0; crc: $8E1DE90A);
  repulse_tiles: array [0 .. 2] of tipo_roms = ((n: '15.9h'; l: $2000; p: 0; crc: $C9213469), (n: '16.10h'; l: $2000; p: $2000; crc: $7DE5D39E), (n: '17.11h'; l: $2000; p: $4000; crc: $0BA5F72C));
  repulse_sprites: array [0 .. 5] of tipo_roms = ((n: '8.6a'; l: $4000; p: 0; crc: $0E9F757E), (n: '9.7a'; l: $4000; p: $4000; crc: $F7D2E650), (n: '10.8a'; l: $4000; p: $8000; crc: $E717BAF4), (n: '11.9a'; l: $4000; p: $C000; crc: $04B2250B), (n: '12.10a'; l: $4000; p: $10000;
    crc: $D110E140), (n: '13.11a'; l: $4000; p: $14000; crc: $8FDC713C));
  repulse_prom: array [0 .. 2] of tipo_roms = ((n: 'b.1j'; l: $100; p: 0; crc: $3EA35431), (n: 'g.1h'; l: $100; p: $100; crc: $ACD7A69E), (n: 'r.1f'; l: $100; p: $200; crc: $B7F48B41));
  srdmission_rom: array [0 .. 1] of tipo_roms = ((n: '5.t2'; l: $4000; p: 0; crc: $A682B48C), (n: '7.t3'; l: $4000; p: $4000; crc: $1719C58C));
  srdmission_snd: array [0 .. 1] of tipo_roms = ((n: '1.t7'; l: $4000; p: 0; crc: $DC48595E), (n: '3.t8'; l: $4000; p: $4000; crc: $216BE1E8));
  srdmission_char: tipo_roms = (n: '15.4a'; l: $1000; p: 0; crc: $4961F7FD);
  srdmission_tiles: array [0 .. 2] of tipo_roms = ((n: '17.9h'; l: $2000; p: 0; crc: $41211458), (n: '18.10h'; l: $2000; p: $2000; crc: $740ECCD4), (n: '16.11h'; l: $2000; p: $4000; crc: $C1F4A5DB));
  srdmission_sprites: array [0 .. 5] of tipo_roms = ((n: '14.6a'; l: $4000; p: 0; crc: $3D4C0447), (n: '13.7a'; l: $4000; p: $4000; crc: $22414A67), (n: '12.8a'; l: $4000; p: $8000; crc: $61E34283), (n: '11.9a'; l: $4000; p: $C000; crc: $BBBAFFEF), (n: '10.10a'; l: $4000;
    p: $10000; crc: $DE564F97), (n: '9.11a'; l: $4000; p: $14000; crc: $890DC815));
  srdmission_prom: array [0 .. 3] of tipo_roms = ((n: 'mr.1j'; l: $100; p: 0; crc: $110A436E), (n: 'mg.1h'; l: $100; p: $100; crc: $0FBFD9F0), (n: 'mb.1f'; l: $100; p: $200; crc: $A342890C), (n: 'm2.5j'; l: $20; p: $300; crc: $190A55AD));
  airwolf_rom: tipo_roms = (n: 'b.2s'; l: $8000; p: 0; crc: $8C993CCE);
  airwolf_snd: tipo_roms = (n: 'a.7s'; l: $8000; p: 0; crc: $A3C7AF5C);
  airwolf_char: tipo_roms = (n: 'f.4a'; l: $1000; p: 0; crc: $4DF44CE9);
  airwolf_tiles: array [0 .. 2] of tipo_roms = ((n: '09h_14.bin'; l: $2000; p: 0; crc: $25E57E1F), (n: '10h_13.bin'; l: $2000; p: $2000; crc: $CF0DE5E9), (n: '11h_12.bin'; l: $2000; p: $4000; crc: $4050C048));
  airwolf_sprites: array [0 .. 2] of tipo_roms = ((n: 'e.6a'; l: $8000; p: 0; crc: $E8FBC7D2), (n: 'd.8a'; l: $8000; p: $8000; crc: $C5D4156B), (n: 'c.10a'; l: $8000; p: $10000; crc: $DE91DFB1));
  airwolf_prom: array [0 .. 3] of tipo_roms = ((n: '01j.bin'; l: $100; p: 0; crc: $6A94B2A3), (n: '01h.bin'; l: $100; p: $100; crc: $EC0923D3), (n: '01f.bin'; l: $100; p: $200; crc: $ADE97052), (n: '74s288-2.bin'; l: $20; p: $300; crc: $190A55AD));

var
  scroll_x: word;
  scroll_y, fg_color, bg_pal_bank: byte;
  nmi_enable: boolean;
  color_codes: array [0 .. $1F] of byte;

procedure update_video_kyugo_hw;
var
  f, nchar: word;
  atrib, x, y, color: byte;

  procedure draw_sprites;
  var
    n, y: byte;
    offs, color, sx, sy, nchar, atrib: word;
  begin
    for n := 0 to (12 * 2) - 1 do
    begin
      offs := (n mod 12) shl 1 + 64 * (n div 12);
      sx := memory[$9029 + offs] + 256 * (memory[$9829 + offs] and 1);
      sy := 255 - memory[$A028 + offs] + 2;
      color := (memory[$A029 + offs] and $1F) shl 3;
      for y := 0 to 15 do
      begin
        nchar := memory[$9028 + offs + 128 * y];
        atrib := memory[$9828 + offs + 128 * y];
        nchar := nchar or ((atrib and $01) shl 9) or ((atrib and $02) shl 7);
        put_gfx_sprite(nchar, color, (atrib and 8) <> 0, (atrib and 4) <> 0, 2);
        update_gfx_sprite(sx, sy + 16 * y, 3, 2);
      end;
    end;
  end;

begin
  for f := 0 to $7FF do
  begin
    // background
    if gfx[1].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$8800 + f];
      nchar := memory[$8000 + f] + ((atrib and $03) shl 8);
      color := ((atrib shr 4) or bg_pal_bank) shl 3;
      put_gfx_flip(x * 8, y * 8, nchar, color, 2, 1, (atrib and $4) <> 0, (atrib and $8) <> 0);
      gfx[1].buffer[f] := false;
    end;
    // foreground
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := memory[$9000 + f];
      put_gfx_trans(x * 8, y * 8, nchar, 2 * color_codes[nchar shr 3] + fg_color, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x + 32, scroll_y);
  draw_sprites;
  update_region(0, 0, 256, 512, 1, 0, 0, 256, 512, 3);
  update_final_piece(0, 16, 288, 224, 3);
end;

procedure events_kyugo_hw;
begin
  if event.arcade then
  begin
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
  end;
end;

procedure kyugo_hw_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        events_kyugo_hw;
        if f = 240 then
        begin
          if nmi_enable then
            z80_0.change_nmi(PULSE_LINE);
          update_video_kyugo_hw;
        end;
        // Main CPU
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // Sound CPU
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function kyugo_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $A7FF, $F000 .. $F7FF:
      kyugo_getbyte := memory[direccion];
  end;
end;

procedure kyugo_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $8FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $9000 .. $97FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $9800 .. $9FFF:
      memory[direccion] := valor or $F0;
    $A000 .. $A7FF, $F000 .. $F7FF:
      memory[direccion] := valor;
    $A800:
      scroll_x := (scroll_x and $100) or valor;
    $B000:
      begin
        scroll_x := (scroll_x and $FF) or ((valor and $1) shl 8);
        if fg_color <> ((valor and $20) shr 3) then
        begin
          fg_color := (valor and $20) shr 3;
          fillchar(gfx[0].buffer[0], $800, 1);
        end;
        if bg_pal_bank <> ((valor and $40) shr 2) then
        begin
          bg_pal_bank := (valor and $40) shr 2;
          fillchar(gfx[1].buffer[0], $800, 1);
        end;
      end;
    $B800:
      scroll_y := valor;
  end;
end;

procedure kyugo_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $7) of
    0:
      nmi_enable := (valor and 1) <> 0;
    1:
      main_screen.flip_main_screen := (valor <> 0);
    2:
      if (valor <> 0) then
        z80_1.change_halt(CLEAR_LINE)
      else
        z80_1.change_halt(ASSERT_LINE);
  end;
end;

// Sound
function snd_kyugo_hw_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      snd_kyugo_hw_getbyte := mem_snd[direccion];
    $A000 .. $A7FF:
      snd_kyugo_hw_getbyte := memory[direccion + $5000];
    $C000:
      snd_kyugo_hw_getbyte := marcade.in2;
    $C040:
      snd_kyugo_hw_getbyte := marcade.in1;
    $C080:
      snd_kyugo_hw_getbyte := marcade.in0;
  end;
end;

procedure snd_kyugo_hw_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $A000 .. $A7FF:
      memory[direccion + $5000] := valor;
  end;
end;

function snd_kyugo_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 2 then
    snd_kyugo_inbyte := ay8910_0.read;
end;

procedure snd_kyugo_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $00:
      ay8910_0.Control(valor);
    $01:
      ay8910_0.write(valor);
    $40:
      ay8910_1.Control(valor);
    $41:
      ay8910_1.write(valor);
  end;
end;

function kyugo_porta_r: byte;
begin
  kyugo_porta_r := marcade.dswa;
end;

function kyugo_portb_r: byte;
begin
  kyugo_portb_r := marcade.dswb;
end;

procedure kyugo_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

procedure kyugo_snd_update;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

// SRD Mission
function srdmission_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $A7FF, $E000 .. $E7FF:
      srdmission_getbyte := memory[direccion];
  end;
end;

procedure srdmission_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $8FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $9000 .. $97FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $9800 .. $9FFF:
      memory[direccion] := valor or $F0;
    $A000 .. $A7FF, $E000 .. $E7FF:
      memory[direccion] := valor;
    $A800:
      scroll_x := (scroll_x and $100) or valor;
    $B000:
      begin
        scroll_x := (scroll_x and $FF) or ((valor and $1) shl 8);
        if fg_color <> ((valor and $20) shr 5) then
        begin
          fg_color := (valor and $20) shr 5;
          fillchar(gfx[0].buffer[0], $800, 1);
        end;
        if bg_pal_bank <> ((valor and $40) shr 6) then
        begin
          bg_pal_bank := (valor and $40) shr 6;
          fillchar(gfx[1].buffer[0], $800, 1);
        end;
      end;
    $B800:
      scroll_y := valor;
  end;
end;

function snd_srdmission_hw_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $8800 .. $8FFF:
      snd_srdmission_hw_getbyte := mem_snd[direccion];
    $8000 .. $87FF:
      snd_srdmission_hw_getbyte := memory[direccion + $6000];
    $F400:
      snd_srdmission_hw_getbyte := marcade.in0;
    $F401:
      snd_srdmission_hw_getbyte := marcade.in1;
    $F402:
      snd_srdmission_hw_getbyte := marcade.in2;
  end;
end;

procedure snd_srdmission_hw_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      memory[direccion + $6000] := valor;
    $8800 .. $8FFF:
      mem_snd[direccion] := valor;
  end;
end;

function snd_srdmission_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = $82 then
    snd_srdmission_inbyte := ay8910_0.read;
end;

procedure snd_srdmission_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $80:
      ay8910_0.Control(valor);
    $81:
      ay8910_0.write(valor);
    $84:
      ay8910_1.Control(valor);
    $85:
      ay8910_1.write(valor);
  end;
end;

// Main
procedure reset_kyugo_hw;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  scroll_x := 0;
  scroll_y := 0;
  fg_color := 0;
  bg_pal_bank := 0;
  nmi_enable := false;
  z80_1.change_halt(ASSERT_LINE);
end;

function start_kyugo: boolean;
var
  memory_temp, memory_temp2: array [0 .. $17FFF] of byte;
  colores: tpaleta;
  f, bit0, bit1, bit2, bit3: byte;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $100);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(2, 0, 8 * 8 * 2, 0, 4);
    convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  end;
  procedure convert_tiles;
  begin
    init_gfx(1, 8, 8, $400);
    gfx_set_desc_data(3, 0, 8 * 8, 0, $400 * 8 * 8, $400 * 8 * 8 * 2);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;
  procedure convert_sprites;
  begin
    init_gfx(2, 16, 16, $400);
    gfx[2].trans[0] := true;
    gfx_set_desc_data(3, 0, 16 * 16, 0, $400 * 16 * 16, $400 * 16 * 16 * 2);
    convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := kyugo_hw_loop;
  machine_calls.reset := reset_kyugo_hw;
  start_kyugo := false;
  start_audio(false);
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, false, true);
  if ((main_vars.machine_type = 128) or (main_vars.machine_type = 330)) then
    main_screen.rot90_screen := true;
  start_video(288, 224);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 256);
  z80_0.change_io_calls(nil, kyugo_outbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(3072000, 256);
  z80_1.init_sound(kyugo_snd_update);
  timers.init(z80_1.numero_cpu, 3072000 / (60 * 4), kyugo_snd_irq, nil, true);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(1536000, AY8910);
  ay8910_0.change_io_calls(kyugo_porta_r, kyugo_portb_r, nil, nil);
  ay8910_1 := ay8910_chip.create(1536000, AY8910);
  case main_vars.machine_type of
    128:
      begin // repulse
        // cargar roms
        z80_0.change_ram_calls(kyugo_getbyte, kyugo_putbyte);
        if not(roms_load(@memory, repulse_rom)) then
          exit;
        // cargar roms snd
        z80_1.change_ram_calls(snd_kyugo_hw_getbyte, snd_kyugo_hw_putbyte);
        z80_1.change_io_calls(snd_kyugo_inbyte, snd_kyugo_outbyte);
        if not(roms_load(@mem_snd, repulse_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, repulse_char)) then
          exit;
        convert_chars;
        // convertir tiles
        if not(roms_load(@memory_temp, repulse_tiles)) then
          exit;
        convert_tiles;
        // convertir sprites
        if not(roms_load(@memory_temp, repulse_sprites)) then
          exit;
        convert_sprites;
        // paleta
        if not(roms_load(@memory_temp, repulse_prom)) then
          exit;
        fillchar(color_codes, $20, 0);
        marcade.dswa := $BF;
        marcade.dswb := $BF;
      end;
    330:
      begin // SRD Mission
        // cargar roms
        z80_0.change_ram_calls(srdmission_getbyte, srdmission_putbyte);
        if not(roms_load(@memory, srdmission_rom)) then
          exit;
        // cargar roms snd
        z80_1.change_ram_calls(snd_srdmission_hw_getbyte, snd_srdmission_hw_putbyte);
        z80_1.change_io_calls(snd_srdmission_inbyte, snd_srdmission_outbyte);
        if not(roms_load(@mem_snd, srdmission_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, srdmission_char)) then
          exit;
        convert_chars;
        // convertir tiles
        if not(roms_load(@memory_temp, srdmission_tiles)) then
          exit;
        convert_tiles;
        // convertir sprites
        if not(roms_load(@memory_temp, srdmission_sprites)) then
          exit;
        convert_sprites;
        // paleta
        if not(roms_load(@memory_temp, srdmission_prom)) then
          exit;
        fillchar(color_codes, $20, 0);
        copymemory(@color_codes, @memory_temp[$300], $20);
        marcade.dswa := $BF;
        marcade.dswb := $FF;
      end;
    331:
      begin // Airwolf
        // cargar roms
        z80_0.change_ram_calls(srdmission_getbyte, srdmission_putbyte);
        if not(roms_load(@memory, airwolf_rom)) then
          exit;
        // cargar roms snd
        z80_1.change_ram_calls(snd_srdmission_hw_getbyte, snd_srdmission_hw_putbyte);
        z80_1.change_io_calls(snd_srdmission_inbyte, snd_srdmission_outbyte);
        if not(roms_load(@mem_snd, airwolf_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, airwolf_char)) then
          exit;
        convert_chars;
        // convertir tiles
        if not(roms_load(@memory_temp, airwolf_tiles)) then
          exit;
        convert_tiles;
        // convertir sprites
        if not(roms_load(@memory_temp2, airwolf_sprites)) then
          exit;
        copymemory(@memory_temp[0], @memory_temp2[0], $2000);
        copymemory(@memory_temp[$4000], @memory_temp2[$2000], $2000);
        copymemory(@memory_temp[$2000], @memory_temp2[$4000], $2000);
        copymemory(@memory_temp[$6000], @memory_temp2[$6000], $2000);
        copymemory(@memory_temp[$8000], @memory_temp2[$8000], $2000);
        copymemory(@memory_temp[$C000], @memory_temp2[$A000], $2000);
        copymemory(@memory_temp[$A000], @memory_temp2[$C000], $2000);
        copymemory(@memory_temp[$E000], @memory_temp2[$E000], $2000);
        copymemory(@memory_temp[$10000], @memory_temp2[$10000], $2000);
        copymemory(@memory_temp[$14000], @memory_temp2[$12000], $2000);
        copymemory(@memory_temp[$12000], @memory_temp2[$14000], $2000);
        copymemory(@memory_temp[$16000], @memory_temp2[$16000], $2000);
        convert_sprites;
        // paleta
        if not(roms_load(@memory_temp, airwolf_prom)) then
          exit;
        fillchar(color_codes, $20, 0);
        copymemory(@color_codes, @memory_temp[$300], $20);
        marcade.dswa := $BF;
        marcade.dswb := $FF;
      end;
  end;
  for f := 0 to $FF do
  begin
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    bit3 := (memory_temp[f] shr 3) and 1;
    colores[f].r := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    bit0 := (memory_temp[f + $100] shr 0) and 1;
    bit1 := (memory_temp[f + $100] shr 1) and 1;
    bit2 := (memory_temp[f + $100] shr 2) and 1;
    bit3 := (memory_temp[f + $100] shr 3) and 1;
    colores[f].g := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    bit0 := (memory_temp[f + $200] shr 0) and 1;
    bit1 := (memory_temp[f + $200] shr 1) and 1;
    bit2 := (memory_temp[f + $200] shr 2) and 1;
    bit3 := (memory_temp[f + $200] shr 3) and 1;
    colores[f].b := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  end;
  set_pal(colores, $100);
  start_kyugo := true;
end;

end.
