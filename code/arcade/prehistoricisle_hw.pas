unit prehistoricisle_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  ym_3812,
  nz80,
  upd7759,
  sound_engine;

function start_prehistoricisle: boolean;

implementation

const
  prehisle_rom: array [0 .. 1] of tipo_roms = ((n: 'gt-e2.2h'; l: $20000; p: 0; crc: $7083245A),
    (n: 'gt-e3.3h'; l: $20000; p: $1; crc: $6D8CDF58));
  prehisle_char: tipo_roms = (n: 'gt15.b15'; l: $8000; p: 0; crc: $AC652412);
  prehisle_fondo_rom: tipo_roms = (n: 'gt.11'; l: $10000; p: 0; crc: $B4F0FCF0);
  prehisle_fondo1: tipo_roms = (n: 'pi8914.b14'; l: $40000; p: 0; crc: $207D6187);
  prehisle_fondo2: tipo_roms = (n: 'pi8916.h16'; l: $40000; p: 0; crc: $7CFFE0F6);
  prehisle_sound: tipo_roms = (n: 'gt1.1'; l: $10000; p: 0; crc: $80A4C093);
  prehisle_upd: tipo_roms = (n: 'gt4.4'; l: $20000; p: 0; crc: $85DFB9EC);
  prehisle_sprites: array [0 .. 1] of tipo_roms = ((n: 'pi8910.k14'; l: $80000; p: 0;
    crc: $5A101B0B), (n: 'gt.5'; l: $20000; p: $80000; crc: $3D3AB273));
  // Dip
  prehisle_dip_a: array [0 .. 5] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Level Select'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $4; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Only Twice'), (dip_val: $0; dip_name: 'Allways'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Coinage'; number: 4;
    dip: ((dip_val: $0; dip_name: 'A 4C/1C B 1C/4C'), (dip_val: $10; dip_name: 'A 3C/1C B 1C/3C'),
    (dip_val: $20; dip_name: 'A 2C/1C B 1C/2C'), (dip_val: $30; dip_name: '1C 1C'), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $80; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $40;
    dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  prehisle_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $2; dip_name: 'Easy'), (dip_val: $3; dip_name: 'Standard'), (dip_val: $1;
    dip_name: 'Middle'), (dip_val: $0; dip_name: 'Difficult'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $C; name: 'Game Mode'; number: 4;
    dip: ((dip_val: $8; dip_name: 'Demo Sounds Off'), (dip_val: $C; dip_name: 'Demo Sounds On'),
    (dip_val: $0; dip_name: 'Freeze'), (dip_val: $4; dip_name: 'Infinite Lives'), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $30; dip_name: '100k 200k'), (dip_val: $20; dip_name: '150k 300k'),
    (dip_val: $10; dip_name: '300k 500k'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $40; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $40; dip_name: 'Yes'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());

var
  rom: array [0 .. $1FFFF] of word;
  ram, back_ram: array [0 .. $1FFF] of word;
  fondo_rom: array [0 .. $7FFF] of word;
  video_ram: array [0 .. $3FF] of word;
  invert_controls, sound_latch: byte;
  scroll_x1, scroll_y1, scroll_x2, scroll_y2: word;

procedure update_video_prehisle;
  procedure poner_sprites(prioridad: boolean);
  var
    atrib, nchar, color, x, y: word;
    f: byte;
  begin
    for f := 0 to $FF do
    begin
      color := buffer_sprites_w[(f * 4) + 3] shr 12;
      if (color < $4) <> prioridad then
        continue;
      atrib := buffer_sprites_w[(f * 4) + 2];
      nchar := atrib and $1FFF;
      if nchar > $1400 then
        nchar := nchar - $1400;
      x := buffer_sprites_w[(f * 4) + 1];
      y := buffer_sprites_w[(f * 4)];
      put_gfx_sprite(nchar, 256 + (color shl 4), (atrib and $4000) <> 0, (atrib and $8000) <> 0, 1);
      update_gfx_sprite(x, y, 1, 1);
    end;
  end;

var
  f, color, pos, x, y, sx, sy, nchar, atrib: word;
begin
  for f := $0 to $120 do
  begin
    x := f div 17;
    y := f mod 17;
    // background
    sx := x + ((scroll_x1 and $3FF0) shr 4);
    sy := y + ((scroll_y1 and $1F0) shr 4);
    pos := (sy and $1F) + ((sx and $3FF) shl 5);
    atrib := fondo_rom[pos];
    color := atrib shr 12;
    if (gfx[2].buffer[pos] or buffer_color[color + $10]) then
    begin
      nchar := atrib and $7FF;
      put_gfx_flip(x * 16, y * 16, nchar, (color shl 4) + 768, 4, 2, (atrib and $800) <> 0, false);
      gfx[2].buffer[pos] := false;
    end;
    // background 2
    sx := x + ((scroll_x2 and $FF0) shr 4);
    sy := y + ((scroll_y2 and $1F0) shr 4);
    pos := (sy and $1F) + ((sx and $FF) shl 5);
    atrib := back_ram[pos];
    color := atrib shr 12;
    if (gfx[3].buffer[pos] or buffer_color[color + $20]) then
    begin
      nchar := atrib and $7FF;
      put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + 512, 5, 3, false,
        (atrib and $800) <> 0);
      gfx[3].buffer[pos] := false;
    end;
  end;
  // foreground
  for f := $0 to $3FF do
  begin
    atrib := video_ram[f];
    color := atrib shr 12;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := atrib and $3FF;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // Mix
  scroll_x_y(4, 1, scroll_x1 and $F, scroll_y1 and $F);
  poner_sprites(false);
  scroll_x_y(5, 1, scroll_x2 and $F, scroll_y2 and $F);
  poner_sprites(true);
  actualiza_trozo(0, 0, 256, 256, 2, 0, 0, 256, 256, 1);
  update_final_piece(0, 16, 256, 224, 1);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_prehisle;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
  end;
end;

procedure prehisle_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        case f of
          21:
            marcade.dswb := marcade.dswb and $7F;
          239:
            begin
              marcade.dswb := marcade.dswb or $80;
              m68000_0.irq[4] := HOLD_LINE;
              update_video_prehisle;
            end;
        end;
      end;
      events_prehisle;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function prehisle_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $3FFFF:
      prehisle_getword := rom[direccion shr 1];
    $70000 .. $73FFF:
      prehisle_getword := ram[(direccion and $3FFF) shr 1];
    $90000 .. $907FF:
      prehisle_getword := video_ram[(direccion and $7FF) shr 1];
    $A0000 .. $A07FF:
      prehisle_getword := buffer_sprites_w[(direccion and $7FF) shr 1];
    $B0000 .. $B3FFF:
      prehisle_getword := back_ram[(direccion and $3FFF) shr 1];
    $D0000 .. $D07FF:
      prehisle_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $E0010:
      prehisle_getword := marcade.in1; // P2
    $E0020:
      prehisle_getword := marcade.in2; // COIN
    $E0040:
      prehisle_getword := marcade.in0 xor invert_controls; // P1
    $E0042:
      prehisle_getword := marcade.dswa;
    $E0044:
      prehisle_getword := marcade.dswb;
  end;
end;

procedure prehisle_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.r := pal4bit(tmp_color shr 12);
    color.g := pal4bit(tmp_color shr 8);
    color.b := pal4bit(tmp_color shr 4);
    set_pal_color(color, numero);
    case numero of
      0 .. 255:
        buffer_color[numero shr 4] := true;
      512 .. 767:
        buffer_color[((numero shr 4) and $F) + $20] := true;
      768 .. 1023:
        buffer_color[((numero shr 4) and $F) + $10] := true;
    end;
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $70000 .. $73FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $90000 .. $907FF:
      if video_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        video_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $A0000 .. $A07FF:
      buffer_sprites_w[(direccion and $7FF) shr 1] := valor;
    $B0000 .. $B3FFF:
      if back_ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        back_ram[(direccion and $3FFF) shr 1] := valor;
        gfx[3].buffer[(direccion and $3FFF) shr 1] := true;
      end;
    $D0000 .. $D07FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $F0000:
      if scroll_y2 <> (valor and $1FF) then
      begin
        if abs((scroll_y2 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[3].buffer[0], $2000, 1);
        scroll_y2 := (valor and $1FF);
      end;
    $F0010:
      if scroll_x2 <> (valor and $FFF) then
      begin
        if abs((scroll_x2 and $FF0) - (valor and $FF0)) > 15 then
          fillchar(gfx[3].buffer[0], $2000, 1);
        scroll_x2 := (valor and $FFF);
      end;
    $F0020:
      if scroll_y1 <> (valor and $1FF) then
      begin
        if abs((scroll_y1 and $1F0) - (valor and $1F0)) > 15 then
          fillchar(gfx[2].buffer[0], $8000, 1);
        scroll_y1 := (valor and $1FF);
      end;
    $F0030:
      if scroll_x1 <> (valor and $3FFF) then
      begin
        if abs((scroll_x1 and $3FF0) - (valor and $3FF0)) > 15 then
          fillchar(gfx[2].buffer[0], $8000, 1);
        scroll_x1 := (valor and $3FFF);
      end;
    $F0046:
      if valor <> 0 then
        invert_controls := $FF
      else
        invert_controls := 0;
    $F0060:
      main_screen.flip_main_screen := (valor and $1) <> 0;
    $F0031 .. $F0045, $F0047 .. $F005F, $F0061 .. $F0069:
      ;
    $F0070:
      begin
        sound_latch := valor and $FF;
        z80_0.change_nmi(PULSE_LINE);
      end;
  end;
end;

function prehisle_snd_getbyte(direccion: word): byte;
begin
  if direccion = $F800 then
    prehisle_snd_getbyte := sound_latch
  else
    prehisle_snd_getbyte := mem_snd[direccion];
end;

procedure prehisle_snd_putbyte(direccion: word; valor: byte);
begin
  if direccion > $EFFF then
    mem_snd[direccion] := valor;
end;

function prehisle_snd_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 0 then
    prehisle_snd_inbyte := ym3812_0.status;
end;

procedure prehisle_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $00:
      ym3812_0.control(valor);
    $20:
      ym3812_0.write(valor);
    $40:
      begin
        upd7759_0.port_w(valor);
        upd7759_0.start_w(0);
        upd7759_0.start_w(1);
      end;
    $80:
      upd7759_0.reset_w(valor and $80);
  end;
end;

procedure prehisle_sound_update;
begin
  ym3812_0.update;
  upd7759_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

// Main
procedure reset_prehisle;
begin
  m68000_0.reset;
  z80_0.reset;
  ym3812_0.reset;
  upd7759_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  invert_controls := 0;
  scroll_x1 := 0;
  scroll_y1 := 0;
  scroll_x2 := 0;
  scroll_y2 := 0;
  sound_latch := 0;
end;

function start_prehistoricisle: boolean;
const
  ps_x: array [0 .. 15] of dword = (0, 4, 8, 12, 16, 20, 24, 28, 0 + 64 * 8, 4 + 64 * 8, 8 + 64 * 8,
    12 + 64 * 8, 16 + 64 * 8, 20 + 64 * 8, 24 + 64 * 8, 28 + 64 * 8);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32,
    8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
var
  ptempb, memory_temp: pbyte;
  tempw, f: word;
begin
  machine_calls.general_loop := prehisle_loop;
  machine_calls.reset := reset_prehisle;
  start_prehistoricisle := false;
  start_audio(false);
  screen_init(1, 512, 512, false, true);
  screen_init(2, 256, 256, true);
  // BG2
  screen_init(4, 272, 272);
  screen_mod_scroll(4, 272, 256, 255, 272, 256, 255);
  // BG0
  screen_init(5, 272, 272, true);
  screen_mod_scroll(5, 272, 256, 255, 272, 256, 255);
  start_video(256, 224);
  // Main CPU
  getmem(memory_temp, $100000);
  m68000_0 := cpu_m68000.create(9000000, $100);
  m68000_0.change_ram16_calls(prehisle_getword, prehisle_putword);
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, $100);
  z80_0.change_ram_calls(prehisle_snd_getbyte, prehisle_snd_putbyte);
  z80_0.change_io_calls(prehisle_snd_inbyte, prehisle_snd_outbyte);
  z80_0.init_sound(prehisle_sound_update);
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3812_FM, 4000000);
  ym3812_0.change_irq_calls(snd_irq);
  upd7759_0 := upd7759_chip.create(0.9);
  // cargar roms
  if not(roms_load16w(@rom, prehisle_rom)) then
    exit;
  // cargar sonido
  if not(roms_load(@mem_snd, prehisle_sound)) then
    exit;
  if not(roms_load(upd7759_0.get_rom_addr, prehisle_upd)) then
    exit;
  // convertir chars
  if not(roms_load(memory_temp, prehisle_char)) then
    exit;
  init_gfx(0, 8, 8, 1024);
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, memory_temp, @ps_x, @ps_y, false, false);
  gfx[0].trans[15] := true;
  // sprites
  if not(roms_load(memory_temp, prehisle_sprites)) then
    exit;
  init_gfx(1, 16, 16, $1400);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
  // fondo 1
  if not(roms_load(memory_temp, prehisle_fondo_rom)) then
    exit;
  // Lo transformo en word...
  ptempb := memory_temp;
  for f := 0 to $7FFF do
  begin
    tempw := ptempb^ shl 8;
    inc(ptempb);
    tempw := tempw or ptempb^;
    inc(ptempb);
    fondo_rom[f] := tempw;
  end;
  if not(roms_load(memory_temp, prehisle_fondo1)) then
    exit;
  init_gfx(2, 16, 16, $800);
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
  // fondo2
  if not(roms_load(memory_temp, prehisle_fondo2)) then
    exit;
  init_gfx(3, 16, 16, $800);
  gfx[3].trans[15] := true;
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(3, 0, memory_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $7F;
  marcade.dswa_val := @prehisle_dip_a;
  marcade.dswb_val := @prehisle_dip_b;
  // final
  freemem(memory_temp);
  reset_prehisle;
  start_prehistoricisle := true;
end;

end.
