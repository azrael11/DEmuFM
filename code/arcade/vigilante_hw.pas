unit vigilante_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  dac,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_vigilante: boolean;

implementation

const
  vigilante_rom: array [0 .. 1] of tipo_roms = ((n: 'vg_a-8h-e.ic55'; l: $8000; p: 0; crc: $0D4E6866), (n: 'vg_a-8l-a.ic57'; l: $10000; p: $8000; crc: $690D812F));
  vigilante_chars: array [0 .. 1] of tipo_roms = ((n: 'vg_b-4f-.ic34'; l: $10000; p: 0; crc: $01579D20), (n: 'vg_b-4j-.ic35'; l: $10000; p: $10000; crc: $4F5872F0));
  vigilante_sprites: array [0 .. 3] of tipo_roms = ((n: 'vg_b-6l-.ic62'; l: $20000; p: 0; crc: $FBE9552D), (n: 'vg_b-6k-.ic61'; l: $20000; p: $20000; crc: $AE09D5C0), (n: 'vg_b-6p-.ic64'; l: $20000;
    p: $40000; crc: $AFB77461), (n: 'vg_b-6n-.ic63'; l: $20000; p: $60000; crc: $5065CD35));
  vigilante_dac: tipo_roms = (n: 'vg_a-4d-.ic26'; l: $10000; p: 0; crc: $9B85101D);
  vigilante_sound: tipo_roms = (n: 'vg_a-5j-.ic37'; l: $10000; p: 0; crc: $10582B2D);
  vigilante_tiles: array [0 .. 2] of tipo_roms = ((n: 'vg_b-1d-.ic2'; l: $10000; p: $00000; crc: $81B1EE5C), (n: 'vg_b-1f-.ic3'; l: $10000; p: $10000; crc: $D0D33673), (n: 'vg_b-1h-.ic4'; l: $10000;
    p: $20000; crc: $AAE81695));

var
  rom_bank: array [0 .. 3, 0 .. $3FFF] of byte;
  banco_rom, sound_latch, rear_color: byte;
  rear_scroll, scroll_x, sample_addr: word;
  rear_disable, rear_ch_color: boolean;
  mem_dac: array [0 .. $FFFF] of byte;

procedure update_video_vigilante;
var
  f, color, nchar, x, y, h, c, i: word;
  atrib: byte;
  flip_y: boolean;
  ccolor: tcolor;
begin
  // fondo
  if not(rear_disable) then
  begin
    if rear_ch_color then
    begin
      for i := 0 to 15 do
      begin
        ccolor.r := (buffer_paleta[$400 + 16 * rear_color + i] shl 3) and $FF;
        ccolor.g := (buffer_paleta[$500 + 16 * rear_color + i] shl 3) and $FF;
        ccolor.b := (buffer_paleta[$600 + 16 * rear_color + i] shl 3) and $FF;
        set_pal_color(ccolor, 512 + i);
        ccolor.r := (buffer_paleta[$400 + 16 * rear_color + 32 + i] shl 3) and $FF;
        ccolor.g := (buffer_paleta[$500 + 16 * rear_color + 32 + i] shl 3) and $FF;
        ccolor.b := (buffer_paleta[$600 + 16 * rear_color + 32 + i] shl 3) and $FF;
        set_pal_color(ccolor, 512 + i + 16);
      end;
      nchar := 0;
      for c := 0 to 2 do
      begin
        for y := 0 to 255 do
        begin
          for x := 0 to 15 do
          begin
            if y < 128 then
              put_gfx(512 * c + (x * 32), y, nchar, 512 + 0, 4, 2)
            else
              put_gfx(512 * c + (x * 32), y, nchar, 512 + 16, 4, 2);
            nchar := nchar + 1;
          end;
        end;
      end;
      rear_ch_color := false;
    end;
    scroll__x(4, 3, rear_scroll - (378 + 16 * 8));
  end
  else
    fill_full_screen(3, $400);
  // chars
  for f := 0 to $7FF do
  begin
    color := memory[$D001 + (f * 2)] and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := memory[$D000 + (f * 2)] + ((memory[$D001 + (f * 2)] and $F0) shl 4);
      if (color >= 4) then
        put_gfx(x * 8, y * 8, nchar, (color shl 4) + 256, 1, 0)
      else
        put_gfx_trans(x * 8, y * 8, nchar, (color shl 4) + 256, 1, 0);
      if (color and $C) = $C then
        put_gfx_trans_alt(x * 8, y * 8, nchar, (color shl 4) + 256, 2, 0, 1)
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__x(1, 3, scroll_x);
  // sprites
  for f := 0 to $17 do
  begin
    atrib := memory[$C025 + (f * 8)];
    nchar := memory[$C024 + (f * 8)] + ((atrib and $0F) shl 8);
    color := (memory[$C020 + (f * 8)] and $0F) shl 4;
    h := 1 shl ((atrib and $30) shr 4);
    nchar := nchar and not(h - 1);
    x := (memory[$C026 + (f * 8)] + ((memory[$C027 + (f * 8)] and $01) shl 8));
    y := (256 + 128 - (memory[$C022 + (f * 8)] + ((memory[$C023 + (f * 8)] and $01) shl 8))) - (16 * h);
    flip_y := (atrib and $80) <> 0;
    for i := 0 to (h - 1) do
    begin
      if flip_y then
        c := nchar + (h - 1 - i)
      else
        c := nchar + i;
      put_gfx_sprite(c, color, (atrib and $40) <> 0, flip_y, 1);
      update_gfx_sprite(x, y + (16 * i), 3, 1);
    end;
  end;
  scroll__x(2, 3, scroll_x);
  update_region(128, 0, 256, 48, 1, 128, 0, 256, 48, 3);
  update_final_piece(128, 0, 256, 256, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_vigilante;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or 10);
  end;
end;

procedure vigilante_loop;
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
      for f := 0 to $FF do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound CPU
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
      end;
      z80_0.change_irq(HOLD_LINE);
      update_video_vigilante;
      events_vigilante;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function vigilante_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C020 .. $C0DF, $D000 .. $EFFF:
      vigilante_getbyte := memory[direccion];
    $8000 .. $BFFF:
      vigilante_getbyte := rom_bank[banco_rom, (direccion and $3FFF)];
    $C800 .. $CFFF:
      vigilante_getbyte := buffer_paleta[direccion and $7FF];
  end;
end;

procedure vigilante_putbyte(direccion: word; valor: byte);
  procedure change_color(dir: word);
  var
    color: tcolor;
    pos2: byte;
    bank, pos: word;
  begin
    bank := dir and $400;
    pos2 := (dir and $FF);
    pos := bank + pos2;
    color.r := (buffer_paleta[pos + $000]) shl 3;
    color.g := (buffer_paleta[pos + $100]) shl 3;
    color.b := (buffer_paleta[pos + $200]) shl 3;
    set_pal_color(color, (bank shr 2) + pos2);
    case (bank shr 2) + pos2 of
      $100 .. $1FF:
        buffer_color[(((bank shr 2) + pos2) shr 4) and $F] := true;
    end;
    rear_ch_color := (bank = $400);
  end;

begin
  case direccion of
    0 .. $BFFF:
      ;
    $C020 .. $C0DF, $E000 .. $EFFF:
      memory[direccion] := valor;
    $C800 .. $CFFF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color(direccion and $7FF);
      end;
    $D000 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
        memory[direccion] := valor;
      end;
  end;
end;

procedure snd_irq_set(tipo: byte);
begin
  case tipo of
    0:
      z80_1.im0 := z80_1.im0 or $20; // Clear Z80
    1:
      z80_1.im0 := z80_1.im0 and $DF; // Set Z80
    2:
      z80_1.im0 := z80_1.im0 or $10; // Clear YM2151
    3:
      z80_1.im0 := z80_1.im0 and $EF; // Set YM2151
  end;
  if (z80_1.im0 <> $FF) then
    z80_1.change_irq(ASSERT_LINE)
  else
    z80_1.change_irq(CLEAR_LINE);
end;

function vigilante_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      vigilante_inbyte := marcade.in0;
    1:
      vigilante_inbyte := marcade.in1;
    2:
      vigilante_inbyte := marcade.in2;
    3:
      vigilante_inbyte := $FF;
    4:
      vigilante_inbyte := $FD;
  end;
end;

procedure vigilante_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      begin
        sound_latch := valor;
        snd_irq_set(1);
      end;
    4:
      banco_rom := valor and $7;
    $80:
      scroll_x := (scroll_x and $100) or valor;
    $81:
      scroll_x := (scroll_x and $FF) or ((valor and 1) shl 8);
    $82:
      rear_scroll := (rear_scroll and $700) or valor;
    $83:
      rear_scroll := (rear_scroll and $FF) or ((valor and 7) shl 8);
    $84:
      begin
        rear_disable := (valor and $40) <> 0;
        rear_color := valor and $D;
      end;
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  snd_getbyte := mem_snd[direccion];
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  if direccion < $C000 then
    exit;
  mem_snd[direccion] := valor;
end;

function snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $1:
      snd_inbyte := ym2151_0.status;
    $80:
      snd_inbyte := sound_latch;
    $84:
      snd_inbyte := mem_dac[sample_addr];
  end;
end;

procedure snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ym2151_0.reg(valor);
    1:
      ym2151_0.write(valor);
    $80:
      sample_addr := (sample_addr and $FF00) or valor;
    $81:
      sample_addr := (sample_addr and $FF) or (valor shl 8);
    $82:
      begin
        dac_0.signed_data8_w(valor);
        sample_addr := sample_addr + 1;
      end;
    $83:
      snd_irq_set(0);
  end;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  snd_irq_set(2 + irqstate);
end;

procedure vigilante_snd_nmi;
begin
  z80_1.change_nmi(PULSE_LINE);
end;

procedure snd_despues_instruccion;
begin
  ym2151_0.update;
  dac_0.update;
end;

// Main
procedure reset_vigilante;
begin
  z80_0.reset;
  z80_1.reset;
  ym2151_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  banco_rom := 0;
  rear_ch_color := true;
  sample_addr := 0;
  sound_latch := 0;
  rear_color := 0;
  rear_scroll := 0;
  scroll_x := 0;
  rear_disable := true;
end;

function start_vigilante: boolean;
const
  ps_x: array [0 .. 15] of dword = ($00 * 8 + 0, $00 * 8 + 1, $00 * 8 + 2, $00 * 8 + 3, $10 * 8 + 0, $10 * 8 + 1, $10 * 8 + 2, $10 * 8 + 3, $20 * 8 + 0, $20 * 8 + 1, $20 * 8 + 2, $20 * 8 + 3,
    $30 * 8 + 0, $30 * 8 + 1, $30 * 8 + 2, $30 * 8 + 3);
  ps_y: array [0 .. 15] of dword = ($00 * 8, $01 * 8, $02 * 8, $03 * 8, $04 * 8, $05 * 8, $06 * 8, $07 * 8, $08 * 8, $09 * 8, $0A * 8, $0B * 8, $0C * 8, $0D * 8, $0E * 8, $0F * 8);
  pt_x: array [0 .. 31] of dword = (0 * 8 + 1, 0 * 8, 1 * 8 + 1, 1 * 8, 2 * 8 + 1, 2 * 8, 3 * 8 + 1, 3 * 8, 4 * 8 + 1, 4 * 8, 5 * 8 + 1, 5 * 8, 6 * 8 + 1, 6 * 8, 7 * 8 + 1, 7 * 8, 8 * 8 + 1, 8 * 8,
    9 * 8 + 1, 9 * 8, 10 * 8 + 1, 10 * 8, 11 * 8 + 1, 11 * 8, 12 * 8 + 1, 12 * 8, 13 * 8 + 1, 13 * 8, 14 * 8 + 1, 14 * 8, 15 * 8 + 1, 15 * 8);
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 64 + 0, 64 + 1, 64 + 2, 64 + 3);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
var
  f: word;
  memory_temp: array [0 .. $7FFFF] of byte;
  mem_load: pbyte;
begin
  machine_calls.general_loop := vigilante_loop;
  machine_calls.reset := reset_vigilante;
  machine_calls.fps_max := 55;
  start_vigilante := false;
  start_audio(true);
  screen_init(1, 512, 256, true);
  screen_mod_scroll(1, 512, 256 + 128, 511, 256, 256, 255);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 256 + 128, 511, 256, 256, 255);
  screen_init(3, 512, 512, false, true);
  screen_init(4, 512 * 4, 256);
  screen_mod_scroll(4, 2048, 512, 2047, 256, 256, 255);
  start_video(256, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3579645, 256);
  z80_0.change_ram_calls(vigilante_getbyte, vigilante_putbyte);
  z80_0.change_io_calls(vigilante_inbyte, vigilante_outbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(3579645, 256);
  z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_1.change_io_calls(snd_inbyte, snd_outbyte);
  z80_1.init_sound(snd_despues_instruccion);
  timers.init(z80_1.numero_cpu, 3579645 / (128 * 55), vigilante_snd_nmi, nil, true);
  // sound chips
  dac_0 := dac_chip.create;
  ym2151_0 := ym2151_chip.create(3579645);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  // cargar roms y rom en bancos
  if not(roms_load(@memory_temp, vigilante_rom)) then
    exit;
  copymemory(@memory[0], @memory_temp[0], $8000);
  for f := 0 to 3 do
    copymemory(@rom_bank[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // cargar sonido
  if not(roms_load(@mem_snd, vigilante_sound)) then
    exit;
  if not(roms_load(@mem_dac, vigilante_dac)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, vigilante_chars)) then
    exit;
  init_gfx(0, 8, 8, 4096);
  gfx[0].trans[0] := true;
  for f := 0 to 7 do
    gfx[0].trans_alt[1, f] := true;
  gfx_set_desc_data(4, 0, 128, 64 * 1024 * 8, (64 * 1024 * 8) + 4, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // sprites
  getmem(mem_load, $80000);
  if not(roms_load(mem_load, vigilante_sprites)) then
    exit;
  copymemory(@memory_temp[0], @mem_load[0], $10000);
  copymemory(@memory_temp[$20000], @mem_load[$10000], $10000);
  copymemory(@memory_temp[$10000], @mem_load[$20000], $10000);
  copymemory(@memory_temp[$30000], @mem_load[$30000], $10000);
  copymemory(@memory_temp[$40000], @mem_load[$40000], $10000);
  copymemory(@memory_temp[$60000], @mem_load[$50000], $10000);
  copymemory(@memory_temp[$50000], @mem_load[$60000], $10000);
  copymemory(@memory_temp[$70000], @mem_load[$70000], $10000);
  freemem(mem_load);
  init_gfx(1, 16, 16, 4096);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, $40 * 8, $40000 * 8, $40000 * 8 + 4, 0, 4);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // tiles
  if not(roms_load(@memory_temp, vigilante_tiles)) then
    exit;
  init_gfx(2, 32, 1, 3 * 512 * 8);
  gfx_set_desc_data(4, 0, 16 * 8, 0, 2, 4, 6);
  convert_gfx(2, 0, @memory_temp, @pt_x, @pc_y, false, false);
  // final
  reset_vigilante;
  start_vigilante := true;
end;

end.
