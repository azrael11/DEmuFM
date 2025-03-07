unit hw_1942_ogl;

interface

uses
  WinApi.Windows,
  FMX.Platform.Win,
  nz80,
  main_engine,
  // main_engine_ogl,
  pal_engine_ogl,
  controls_engine,
  gfx_engine_ogl,
  ay_8910,
  rom_engine,
  sound_engine,
  qsnapshot,
  SDL2;

function start_1942_ogl: boolean;

implementation

uses
  umain_config,
  main;

const
  hw1942_rom: array [0 .. 4] of tipo_roms = ((n: 'srb-03.m3'; l: $4000; p: 0; crc: $D9DAFCC3), (n: 'srb-04.m4'; l: $4000; p: $4000; crc: $DA0CF924), (n: 'srb-05.m5'; l: $4000; p: $8000; crc: $D102911C), (n: 'srb-06.m6'; l: $2000; p: $C000; crc: $466F8248), (n: 'srb-07.m7';
    l: $4000; p: $10000; crc: $0D31038C));
  hw1942_snd_rom: tipo_roms = (n: 'sr-01.c11'; l: $4000; p: 0; crc: $BD87F06B);
  hw1942_pal: array [0 .. 5] of tipo_roms = ((n: 'sb-5.e8'; l: $100; p: 0; crc: $93AB8153), (n: 'sb-6.e9'; l: $100; p: $100; crc: $8AB44F7D), (n: 'sb-7.e10'; l: $100; p: $200; crc: $F4ADE9A4), (n: 'sb-0.f1'; l: $100; p: $300; crc: $6047D91B), (n: 'sb-4.d6'; l: $100; p: $400;
    crc: $4858968D), (n: 'sb-8.k3'; l: $100; p: $500; crc: $F6FAD943));
  hw1942_char: tipo_roms = (n: 'sr-02.f2'; l: $2000; p: 0; crc: $6EBCA191);
  hw1942_tiles: array [0 .. 5] of tipo_roms = ((n: 'sr-08.a1'; l: $2000; p: 0; crc: $3884D9EB), (n: 'sr-09.a2'; l: $2000; p: $2000; crc: $999CF6E0), (n: 'sr-10.a3'; l: $2000; p: $4000; crc: $8EDB273A), (n: 'sr-11.a4'; l: $2000; p: $6000; crc: $3A2726C3), (n: 'sr-12.a5'; l: $2000;
    p: $8000; crc: $1BD3D8BB), (n: 'sr-13.a6'; l: $2000; p: $A000; crc: $658F02C4));
  hw1942_sprites: array [0 .. 3] of tipo_roms = ((n: 'sr-14.l1'; l: $4000; p: 0; crc: $2528BEC6), (n: 'sr-15.l2'; l: $4000; p: $4000; crc: $F89287AA), (n: 'sr-16.n1'; l: $4000; p: $8000; crc: $024418F8), (n: 'sr-17.n2'; l: $4000; p: $C000; crc: $E2C7E489));
  hw1942_dip_a: array [0 .. 4] of def_dip = ((mask: $7; name: 'Coin A'; number: 8; dip: ((dip_val: $1; dip_name: '4C 1C'), (dip_val: $2; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $3; dip_name: '2C 3C'), (dip_val: $6;
    dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 4C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $8; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $30; dip_name: '20K 80K Every 80K'), (dip_val: $20; dip_name: '20K 100K Every 100K'), (dip_val: $10; dip_name: '30K 80K Every 80K'), (dip_val: $0; dip_name: '30K 100K Every 100K'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives';
    number: 4; dip: ((dip_val: $80; dip_name: '1'), (dip_val: $40; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  hw1942_dip_b: array [0 .. 4] of def_dip = ((mask: $7; name: 'Coin B'; number: 8; dip: ((dip_val: $1; dip_name: '4C 1C'), (dip_val: $2; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $3; dip_name: '2C 3C'), (dip_val: $6;
    dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 4C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $60; dip_name: 'Normal'), (dip_val: $20; dip_name: 'Difficult'), (dip_val: $0; dip_name: 'Very Difficult'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Screen Stop'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  memory_rom: array [0 .. 2, 0 .. $3FFF] of byte;
  scroll: word;
  sound_command, rom_bank, palette_bank: byte;

procedure update_video_hw1942;
var
  f, color, nchar, pos, x, y: word;
  attr: byte;
  procedure draw_sprites;
  var
    f, color, nchar, x, y: word;
    i, h, atrib: byte;
  begin
    for f := $1F downto 0 do
    begin
      atrib := memory[$CC01 + (f * 4)];
      nchar := (memory[$CC00 + (f * 4)] and $7F) + (atrib and $20) shl 2 + (memory[$CC00 + (f * 4)] and $80) shl 1;
      color := (atrib and $0F) shl 4;
      x := 240 - (memory[$CC03 + (f * 4)] - $10 * (atrib and $10));
      y := memory[$CC02 + (f * 4)];
      // handle double or quadruple height
      i := (atrib and $C0) shr 6;
      if (i = 2) then
        i := 3;
      for h := i downto 0 do
      begin
        engine_gfx_opengl.PutGfxSpriteOpenGL(nchar + h, color, false, false, 1);
        engine_gfx_opengl.UpdateGfxSpriteOpenGL(y + 16 * h, x, 1, 1);
      end;
    end;
  end;

begin
  for f := 0 to $1FF do
  begin
    if engine_gfx_opengl.gfx[2].buffer[f] then
    begin
      x := f and $F;
      y := 31 - (f shr 4);
      pos := x + ((f and $1F0) shl 1);
      attr := memory[$D810 + pos];
      nchar := memory[$D800 + pos] + ((attr and $80) shl 1);
      color := ((attr and $1F) + (palette_bank * $20)) shl 3;
      engine_gfx_opengl.PutGfxFlipOpenGL(x * 16, y * 16, nchar, color, 2, 2, (attr and $40) <> 0, (attr and $20) <> 0);
      engine_gfx_opengl.gfx[2].buffer[f] := false;
    end;
  end;
  for f := 0 to $3FF do
  begin
    if engine_gfx_opengl.gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      attr := memory[f + $D400];
      color := (attr and $3F) shl 2;
      nchar := memory[f + $D000] + ((attr and $80) shl 1);
      engine_gfx_opengl.PutGfxTransOpenGL(x * 8, y * 8, nchar, color, 3, 0);
      engine_gfx_opengl.gfx[0].buffer[f] := false;
    end;
  end;

  engine_gfx_opengl.ScrollYOpenGL(2, 1, 256 - scroll);
  draw_sprites;
  engine_opengl.update_region_OpenGL(0, 0, 256, 256, 3, 0, 0, 256, 256, 1);
  engine_opengl.update_final_piece_OpenGL(16, 0, 224, 256, 1);
end;

procedure events_hw1942;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure hw1942_loop;
var
  f: byte;
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;

  while EmuStatus = EsRunning do
  begin
    if engine_opengl.EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        case f of
          $2C:
            z80_1.change_irq(HOLD_LINE);
          $6D:
            begin
              z80_0.change_irq_vector(HOLD_LINE, $CF);
              z80_1.change_irq(HOLD_LINE);
            end;
          $AF:
            z80_1.change_irq(HOLD_LINE);
          $F0:
            begin
              z80_0.change_irq_vector(HOLD_LINE, $D7);
              z80_1.change_irq(HOLD_LINE);
              update_video_hw1942;
            end;
        end;
      end;
      events_hw1942;
      engine_opengl.video_sync_OpenGL;
    end
    else
      // pause_action;
  end;
end;

function hw1942_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF, $CC00 .. $CC7F, $D000 .. $DBFF, $E000 .. $EFFF:
      hw1942_getbyte := memory[direccion];
    $8000 .. $BFFF:
      hw1942_getbyte := memory_rom[rom_bank, direccion and $3FFF];
    $C000:
      hw1942_getbyte := marcade.in0;
    $C001:
      hw1942_getbyte := marcade.in1;
    $C002:
      hw1942_getbyte := marcade.in2;
    $C003:
      hw1942_getbyte := marcade.dswa;
    $C004:
      hw1942_getbyte := marcade.dswb;
  end;
end;

procedure hw1942_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C800:
      sound_command := valor;
    $C802:
      scroll := valor or (scroll and $100);
    $C803:
      scroll := ((valor and $1) shl 8) or (scroll and $FF);
    $C804:
      begin
        if (valor and $10) <> 0 then
          z80_1.change_reset(ASSERT_LINE)
        else
          z80_1.change_reset(CLEAR_LINE);
        engine_opengl.OGL_MainScreen.flip_main_screen := (valor and $80) <> 0;
      end;
    $C805:
      palette_bank := valor;
    $C806:
      rom_bank := valor and $3;
    $CC00 .. $CC7F, $E000 .. $EFFF:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        engine_gfx_opengl.gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $D800 .. $DBFF:
      if memory[direccion] <> valor then
      begin
        engine_gfx_opengl.gfx[2].buffer[(direccion and $F) + ((direccion and $3E0) shr 1)] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function hw1942_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF:
      hw1942_snd_getbyte := mem_snd[direccion];
    $6000:
      hw1942_snd_getbyte := sound_command
  end;
end;

procedure hw1942_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
    $8000:
      ay8910_0.Control(valor);
    $8001:
      ay8910_0.Write(valor);
    $C000:
      ay8910_1.Control(valor);
    $C001:
      ay8910_1.Write(valor);
  end;
end;

procedure hw1942_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

procedure hw1942_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 4] of byte;
begin
  open_qsnapshot_save('1942' + nombre);
  getmem(data, 200);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ay8910_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := ay8910_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[$C000], $4000);
  savedata_qsnapshot(@mem_snd[$4000], $800);
  // MISC
  buffer[0] := scroll and $FF;
  buffer[1] := scroll shr 8;
  buffer[2] := sound_command;
  buffer[3] := rom_bank;
  buffer[4] := palette_bank;
  savedata_qsnapshot(@buffer, 5);
  freemem(data);
  close_qsnapshot;
end;

procedure hw1942_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 4] of byte;
begin
  if not(open_qsnapshot_load('1942' + nombre)) then
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
  loaddata_qsnapshot(data);
  ay8910_1.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory[$C000]);
  loaddata_qsnapshot(@mem_snd[$4000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  scroll := buffer[0] or (buffer[1] shl 8);
  sound_command := buffer[2];
  rom_bank := buffer[3];
  palette_bank := buffer[4];
  freemem(data);
  close_qsnapshot;
end;

// Main
procedure reset_hw1942_ogl;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  engine_gfx_opengl.ResetVideoOpenGL;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll := 0;
  rom_bank := 0;
  palette_bank := 0;
  sound_command := 0;
end;

function start_1942_ogl: boolean;
var
  colors: TPalette;
  f: word;
  // memory_temp: array [0 .. $17FFF] of byte;
  memory_temp: array [0 .. 300000] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 16 * 16 + 0, 16 * 16 + 1, 16 * 16 + 2, 16 * 16 + 3, 16 * 16 + 8 + 0, 16 * 16 + 8 + 1, 16 * 16 + 8 + 2, 16 * 16 + 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  engine_opengl := TOPENGL_ENGINE.Create;
  start_1942_ogl := false;
  machine_calls.general_loop := hw1942_loop;
  machine_calls.reset := reset_hw1942_ogl;
  machine_calls.save_qsnap := hw1942_qsave;
  machine_calls.load_qsnap := hw1942_qload;
  start_audio(false);
  engine_opengl.screen_init_OpenGL(1, 256, 512, false, true);
  engine_opengl.screen_init_OpenGL(2, 256, 512);
  engine_opengl.screen_mod_scroll_OpenGL(2, 256, 256, 255, 512, 256, 511);
  engine_opengl.screen_init_OpenGL(3, 256, 256, true);
  engine_opengl.start_video_OpenGL(224, 256);

  // Main CPU
  z80_0 := cpu_z80.Create(4000000, $100);
  z80_0.change_ram_calls(hw1942_getbyte, hw1942_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.Create(3000000, $100);
  z80_1.change_ram_calls(hw1942_snd_getbyte, hw1942_snd_putbyte);
  z80_1.init_sound(hw1942_sound_update);
  // Sound Chips
  ay8910_0 := ay8910_chip.Create(1500000, AY8910, 1);
  ay8910_1 := ay8910_chip.Create(1500000, AY8910, 1);
  // cargar roms y ponerlas en su sitio
  if not(roms_load(@memory_temp, hw1942_rom)) then
    exit;
  copymemory(@memory, @memory_temp, $8000);
  for f := 0 to 2 do
    copymemory(@memory_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // cargar ROMS sonido
  if not(roms_load(@mem_snd, hw1942_snd_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, hw1942_char)) then
    exit;
  engine_gfx_opengl.InitGfxOpenGL(0, 8, 8, $200);
  engine_gfx_opengl.gfx[0].transparency[0] := true;
  engine_gfx_opengl.GfxSetDescData(2, 0, 16 * 8, 4, 0);
  engine_gfx_opengl.ConvertGfxOpenGL(0, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // convertir sprites
  if not(roms_load(@memory_temp, hw1942_sprites)) then
    exit;
  engine_gfx_opengl.InitGfxOpenGL(1, 16, 16, $200);
  engine_gfx_opengl.gfx[1].transparency[15] := true;
  engine_gfx_opengl.GfxSetDescData(4, 0, 64 * 8, 512 * 64 * 8 + 4, 512 * 64 * 8 + 0, 4, 0);
  engine_gfx_opengl.ConvertGfxOpenGL(1, 0, @memory_temp, @ps_x, @ps_y, false, true);

  // tiles
  if not(roms_load(@memory_temp, hw1942_tiles)) then
    exit;
  engine_gfx_opengl.InitGfxOpenGL(2, 16, 16, $200);
  engine_gfx_opengl.GfxSetDescData(3, 0, 32 * 8, 0, $4000 * 8, $4000 * 8 * 2);
  engine_gfx_opengl.ConvertGfxOpenGL(2, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // poner la paleta
  if not(roms_load(@memory_temp, hw1942_pal)) then
    exit;
  for f := 0 to $FF do
  begin
    colors[f].r := pal4bit(memory_temp[f]);
    colors[f].g := pal4bit(memory_temp[f + $100]);
    colors[f].b := pal4bit(memory_temp[f + $200]);
    engine_gfx_opengl.gfx[0].colors[f] := memory_temp[$300 + f] + $80; // chars
    engine_gfx_opengl.gfx[2].colors[f] := memory_temp[$400 + f]; // tiles
    engine_gfx_opengl.gfx[2].colors[f + $100] := memory_temp[$400 + f] + $10;
    engine_gfx_opengl.gfx[2].colors[f + $200] := memory_temp[$400 + f] + $20;
    engine_gfx_opengl.gfx[2].colors[f + $300] := memory_temp[$400 + f] + $30;
    engine_gfx_opengl.gfx[1].colors[f] := memory_temp[$500 + f] + $40; // sprites
  end;
  SetPalette(colors, 256);
  // DIP
  marcade.dswa := $77;
  marcade.dswa_val := @hw1942_dip_a;
  marcade.dswb := $FF;
  marcade.dswb_val := @hw1942_dip_b;
  // final
  reset_hw1942_ogl;
  start_1942_ogl := true;
end;

end.
