unit solomonkey_hw;

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
  timer_engine;

function start_solomonskey: boolean;

implementation

const
  solomon_rom: array [0 .. 2] of tipo_roms = ((n: '6.3f'; l: $4000; p: 0; crc: $645EB0F3), (n: '7.3h'; l: $8000; p: $4000; crc: $1BF5C482), (n: '8.3jk'; l: $8000; p: $C000; crc: $0A6CDEFC));
  solomon_snd_rom: tipo_roms = (n: '1.3jk'; l: $4000; p: 0; crc: $FA6E562E);
  solomon_chars: array [0 .. 1] of tipo_roms = ((n: '12.3t'; l: $8000; p: 0; crc: $B371291C), (n: '11.3r'; l: $8000; p: $8000; crc: $6F94D2AF));
  solomon_sprites: array [0 .. 3] of tipo_roms = ((n: '2.5lm'; l: $4000; p: 0; crc: $80FA2BE3), (n: '3.6lm'; l: $4000; p: $4000; crc: $236106B4), (n: '4.7lm'; l: $4000; p: $8000; crc: $088FE5D9), (n: '5.8lm'; l: $4000; p: $C000; crc: $8366232A));
  solomon_tiles: array [0 .. 1] of tipo_roms = ((n: '10.3p'; l: $8000; p: 0; crc: $8310C2A1), (n: '9.3m'; l: $8000; p: $8000; crc: $AB7E6C42));
  // Dip
  solomon_dip_a: array [0 .. 5] of def_dip2 = ((mask: 1; name: 'Demo Sound'; number: 2; val2: (1, 0); name2: ('Off', 'On')), (mask: 2; name: 'Cabinet'; number: 2; val2: (2, 0); name2: ('Upright', 'Cocktail')), (mask: $C; name: 'Lives'; number: 4; val4: ($C, 0, 8, 4);
    name4: ('2', '3', '4', '5')), (mask: $30; name: 'Coin B'; number: 4; val4: ($20, 0, $10, $30); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $C0; name: 'Coin A'; number: 4; val4: ($80, 0, $40, $C0); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), ());
  solomon_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Difficulty'; number: 4; val4: (2, 0, 1, 3); name4: ('Easy', 'Normal', 'Harder', 'Difficult')), (mask: $C; name: 'Time Speed'; number: 4; val4: (8, 0, 4, $C); name4: ('Slow', 'Normal', 'Faster', 'Fastest')),
    (mask: $10; name: 'Extra'; number: 2; val2: (0, $10); name2: ('Normal', 'Difficult')), (mask: $E0; name: 'Bonus Life'; number: 8; val8: (0, $80, $40, $C0, $20, $A0, $60, $E0);
    name8: ('30K 200K 500K', '100K 300K 800K', '30K 200K', '100K 300K', '30K', '100K', '200K', 'None')), ());

var
  sound_latch: byte;
  nmi_enable: boolean;

procedure update_video_solomon;
var
  f, color, nchar, x, y: word;
  atrib: byte;
begin
  for f := $3FF downto 0 do
  begin
    // tiles
    atrib := memory[$D800 + f];
    color := (atrib and $70) shr 4;
    if (gfx[1].buffer[f] or buffer_color[color + 8]) then
    begin
      x := (f and $1F) shl 3;
      y := (f shr 5) shl 3;
      nchar := memory[$DC00 + f] + ((atrib and 7) shl 8);
      put_gfx_flip(x, y, nchar, (color shl 4) + 128, 1, 1, (atrib and $80) <> 0, (atrib and 8) <> 0);
      gfx[1].buffer[f] := false;
    end;
    // Chars
    atrib := memory[$D000 + f];
    color := (atrib and $70) shr 4;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := (f and $1F) shl 3;
      y := (f shr 5) shl 3;
      nchar := memory[$D400 + f] + ((atrib and 7) shl 8);
      put_gfx_trans(x, y, nchar, color shl 4, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  // sprites
  for f := $1F downto 0 do
  begin
    atrib := memory[$E001 + (f * 4)];
    nchar := memory[$E000 + (f * 4)] + 16 * (atrib and $10);
    color := (atrib and $E) shl 3;
    x := memory[$E003 + (f * 4)];
    y := 241 - memory[$E002 + (f * 4)];
    put_gfx_sprite(nchar, color, (atrib and $40) <> 0, (atrib and $80) <> 0, 2);
    update_gfx_sprite(x, y, 3, 2);
  end;
  update_final_piece(0, 16, 256, 224, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_solomon;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    // p2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    // system
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure solomon_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to $ff do begin
    events_solomon;
    if f=240 then begin
        if nmi_enable then z80_0.change_nmi(PULSE_LINE);
        update_video_solomon;
    end;
    //main
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
    //snd
    z80_1.run(frame_snd);
    frame_snd:=frame_snd+z80_1.tframes-z80_1.contador;
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

// Main
function solomon_getbyte(direccion: word): byte;
var
  z80_0_reg: npreg_z80;
begin
  case direccion of
    0 .. $E07F, $F000 .. $FFFF:
      solomon_getbyte := memory[direccion];
    $E400 .. $E5FF:
      solomon_getbyte := buffer_paleta[direccion and $1FF];
    $E600:
      solomon_getbyte := marcade.in0;
    $E601:
      solomon_getbyte := marcade.in1;
    $E602:
      solomon_getbyte := marcade.in2;
    $E603:
      begin
        z80_0_reg := z80_0.get_internal_r;
        if (z80_0_reg.pc = $4CF0) then
          solomon_getbyte := z80_0_reg.bc.w and 8 // proteccion ???
        else
          solomon_getbyte := 0;
      end;
    $E604:
      solomon_getbyte := marcade.dswa;
    $E605:
      solomon_getbyte := marcade.dswb;
    $E606:
      ;
  end;
end;

procedure solomon_putbyte(direccion: word; valor: byte);
  procedure change_color(dir: word);
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[dir];
    color.r := pal4bit(tmp_color);
    color.g := pal4bit(tmp_color shr 4);
    tmp_color := buffer_paleta[dir + 1];
    color.b := pal4bit(tmp_color);
    dir := dir shr 1;
    set_pal_color(color, dir);
    case dir of
      0 .. $7F:
        buffer_color[dir shr 4] := true;
      $80 .. $FF:
        buffer_color[((dir shr 4) and 7) + 8] := true;
    end;
  end;

begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF, $E000 .. $E07F:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if (valor <> memory[direccion]) then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $D800 .. $DFFF:
      if (valor <> memory[direccion]) then
      begin
        gfx[1].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $E400 .. $E5FF:
      if buffer_paleta[direccion and $1FF] <> valor then
      begin
        buffer_paleta[direccion and $1FF] := valor;
        change_color(direccion and $1FE);
      end;
    $E600:
      nmi_enable := valor <> 0;
    $E604:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $E800:
      begin
        sound_latch := valor;
        z80_1.change_nmi(PULSE_LINE);
      end;
    $F000 .. $FFFF:
      ;
  end;
end;

// Sound
function solomon_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF:
      solomon_snd_getbyte := mem_snd[direccion];
    $8000:
      solomon_snd_getbyte := sound_latch;
  end;
end;

procedure solomon_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
  end;
end;

procedure solomon_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $10:
      ay8910_0.control(valor);
    $11:
      ay8910_0.write(valor);
    $20:
      ay8910_1.control(valor);
    $21:
      ay8910_1.write(valor);
    $30:
      ay8910_2.control(valor);
    $31:
      ay8910_2.write(valor);
  end;
end;

procedure solomon_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

procedure solomon_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
  ay8910_2.update;
end;

// Main
procedure reset_solomon;
begin
  z80_0.reset;
  z80_1.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  ay8910_0.reset;
  ay8910_1.reset;
  ay8910_2.reset;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  sound_latch := 0;
  nmi_enable := true;
end;

function start_solomonskey: boolean;
var
  memory_temp: array [0 .. $13FFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
begin
  start_solomonskey := false;
  machine_calls.general_loop := solomon_loop;
  machine_calls.reset := reset_solomon;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 256);
  z80_0.change_ram_calls(solomon_getbyte, solomon_putbyte);
  if not(roms_load(@memory_temp, solomon_rom)) then
    exit;
  copymemory(@memory, @memory_temp, $4000);
  copymemory(@memory[$4000], @memory_temp[$8000], $4000);
  copymemory(@memory[$8000], @memory_temp[$4000], $4000);
  copymemory(@memory[$F000], @memory_temp[$C000], $1000);
  // Sound CPU
  z80_1 := cpu_z80.create(3072000, 256);
  z80_1.change_ram_calls(solomon_snd_getbyte, solomon_snd_putbyte);
  z80_1.change_io_calls(nil, solomon_snd_outbyte);
  z80_1.init_sound(solomon_sound_update);
  timers.init(z80_1.numero_cpu, 3072000 / (60 * 2), solomon_snd_irq, nil, true);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(1500000, AY8910);
  ay8910_1 := ay8910_chip.create(1500000, AY8910);
  ay8910_2 := ay8910_chip.create(1500000, AY8910);
  // convertir chars
  if not(roms_load(@memory_temp, solomon_chars)) then
    exit;
  init_gfx(0, 8, 8, $800);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // tiles
  if not(roms_load(@memory_temp, solomon_tiles)) then
    exit;
  init_gfx(1, 8, 8, $800);
  convert_gfx(1, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, solomon_sprites)) then
    exit;
  init_gfx(2, 16, 16, $400);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 512 * 32 * 8, 2 * 512 * 32 * 8, 3 * 512 * 32 * 8);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := 2;
  marcade.dswa_val2 := @solomon_dip_a;
  marcade.dswb := 0;
  marcade.dswb_val2 := @solomon_dip_b;
  // final
  start_solomonskey := true;
end;

end.
