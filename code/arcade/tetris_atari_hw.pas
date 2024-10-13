unit tetris_atari_hw;

interface

uses
  WinApi.Windows,
  m6502,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  pokey,
  slapstic,
  file_engine;

function start_tetris: boolean;

implementation

const
  tetris_rom: tipo_roms = (n: '136066-1100.45f'; l: $10000; p: $0; crc: $2ACBDB09);
  tetris_gfx: tipo_roms = (n: '136066-1101.35a'; l: $10000; p: $0; crc: $84A1939F);
  // Dip
  tetris_dip_a: array [0 .. 3] of def_dip = ((mask: $4; name: 'Freeze'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $4; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $8; name: 'Freeze Step'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $8; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $80; name: 'Service'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $80; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());

var
  rom_mem: array [0 .. 1, 0 .. $3FFF] of byte;
  nv_ram: array [0 .. $1FF] of byte;
  rom_bank: byte;
  nvram_write_enable: boolean;

procedure update_video_tetris;
var
  f, nchar, x, y: word;
  color, atrib: byte;
begin
  for f := 0 to $7FF do
  begin
    atrib := memory[$1001 + (f * 2)];
    color := atrib shr 4;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := memory[$1000 + (f * 2)] + ((atrib and $7) shl 8);
      put_gfx(x * 8, y * 8, nchar, color shl 4, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  actualiza_trozo(0, 0, 336, 240, 1, 0, 0, 336, 240, 2);
  update_final_piece(0, 0, 336, 240, 2);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_tetris;
begin
  if event.arcade then
  begin
    // Coin
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := marcade.in0 or $2
    else
      marcade.in0 := marcade.in0 and $FD;
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := marcade.in0 or $1
    else
      marcade.in0 := marcade.in0 and $FE;
    // Players
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := marcade.in1 or 1
    else
      marcade.in1 := marcade.in1 and $FE;
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := marcade.in1 or 2
    else
      marcade.in1 := marcade.in1 and $FD;
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 or 4
    else
      marcade.in1 := marcade.in1 and $FB;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 or 8
    else
      marcade.in1 := marcade.in1 and $F7;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 or $10
    else
      marcade.in1 := marcade.in1 and $EF;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 or $20
    else
      marcade.in1 := marcade.in1 and $DF;
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 or $40
    else
      marcade.in1 := marcade.in1 and $BF;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 or $80
    else
      marcade.in1 := marcade.in1 and $7F;
  end;
end;

procedure tetris_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 261 do
      begin
        m6502_0.run(frame);
        frame := frame + m6502_0.tframes - m6502_0.contador;
        case f of
          0:
            marcade.in0 := marcade.in0 or $40;
          47, 111, 175:
            m6502_0.change_irq(ASSERT_LINE);
          239:
            begin
              update_video_tetris;
              marcade.in0 := marcade.in0 and $BF;
              m6502_0.change_irq(ASSERT_LINE);
            end;
        end;
      end;
      events_tetris;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function getbyte_tetris(direccion: word): byte;
var
  res, new_bank: byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $5FFF, $8000 .. $FFFF:
      getbyte_tetris := memory[direccion];
    $2000 .. $23FF:
      getbyte_tetris := buffer_paleta[direccion and $FF];
    $2400 .. $27FF:
      getbyte_tetris := nv_ram[direccion and $1FF];
    $2800 .. $2BFF:
      case (direccion and $1F) of
        0 .. $F:
          getbyte_tetris := pokey_0.read(direccion and $F);
        $10 .. $1F:
          getbyte_tetris := pokey_1.read(direccion and $F);
      end;
    $6000 .. $7FFF:
      begin // SLAPSTIC
        res := memory[direccion];
        new_bank := slapstic_0.slapstic_tweak(direccion and $1FFF) and 1;
        // update for the new bank
        if (new_bank <> rom_bank) then
        begin
          rom_bank := new_bank;
          copymemory(@memory[$4000], @rom_mem[rom_bank, 0], $4000);
        end;
        getbyte_tetris := res;
      end;
  end;
end;

procedure putbyte_tetris(direccion: word; valor: byte);
  procedure change_color(dir: byte);
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[dir];
    color.r := pal3bit(tmp_color shr 5);
    color.g := pal3bit((tmp_color shr 2) and $7);
    color.b := pal2bit(tmp_color and $3);
    set_pal_color(color, dir);
    buffer_color[dir shr 4] := true;
  end;

begin
  case direccion of
    0 .. $FFF:
      memory[direccion] := valor;
    $1000 .. $1FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $2000 .. $23FF:
      if buffer_paleta[direccion and $FF] <> valor then
      begin
        buffer_paleta[direccion and $FF] := valor;
        change_color(direccion and $FF);
      end;
    $2400 .. $27FF:
      begin
        if nvram_write_enable then
          nv_ram[direccion and $1FF] := valor;
        nvram_write_enable := false;
      end;
    $2800 .. $2BFF:
      case (direccion and $1F) of
        0 .. $F:
          pokey_0.write(direccion and $F, valor);
        $10 .. $1F:
          pokey_1.write(direccion and $F, valor);
      end;
    $3000 .. $33FF:
      ; // Watchdog
    $3400 .. $37FF:
      nvram_write_enable := true;
    $3800 .. $3BFF:
      m6502_0.change_irq(CLEAR_LINE);
    $3C00 .. $3FFF:
      ; // coincount
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function tetris_pokey_0(pot: byte): byte;
begin
  tetris_pokey_0 := marcade.in0 or marcade.dswa;
end;

function tetris_pokey_1(pot: byte): byte;
begin
  tetris_pokey_1 := marcade.in1;
end;

procedure tetris_sound_update;
begin
  pokey_0.update;
  pokey_1.update;
end;

// Main
procedure reset_tetris;
begin
  m6502_0.reset;
  slapstic_0.reset;
  pokey_0.reset;
  pokey_1.reset;
  marcade.in0 := $40;
  marcade.in1 := 0;
  rom_bank := slapstic_0.current_bank and 1;
  copymemory(@memory[$4000], @rom_mem[1, 0], $4000);
  nvram_write_enable := false;
end;

procedure close_tetris;
begin
  write_file(Directory.Arcade_nvram + 'tetris.nv', @nv_ram[0], $200);
end;

function start_tetris: boolean;
const
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 4 * 8, 1 * 4 * 8, 2 * 4 * 8, 3 * 4 * 8, 4 * 4 * 8, 5 * 4 * 8,
    6 * 4 * 8, 7 * 4 * 8);
var
  memory_temp: array [0 .. $FFFF] of byte;
  longitud: integer;
begin
  machine_calls.general_loop := tetris_loop;
  machine_calls.reset := reset_tetris;
  machine_calls.close := close_tetris;
  start_tetris := false;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_init(2, 336, 240, false, true);
  start_video(336, 240);
  // Main CPU
  m6502_0 := cpu_m6502.create(1789772, 262, TCPU_M6502);
  m6502_0.change_ram_calls(getbyte_tetris, putbyte_tetris);
  m6502_0.init_sound(tetris_sound_update);
  // Slapstic
  slapstic_0 := slapstic_type.create(101, false);
  // Sound Chip
  pokey_0 := pokey_chip.create(1789772);
  pokey_0.change_all_pot(tetris_pokey_0);
  pokey_1 := pokey_chip.create(1789772);
  pokey_1.change_all_pot(tetris_pokey_1);
  // nv_ram
  if read_file_size(Directory.Arcade_nvram + 'tetrisa.nv', longitud) then
    read_file(Directory.Arcade_nvram + 'tetrisa.nv', @nv_ram[0], longitud)
  else
    for longitud := 0 to $1FF do
      nv_ram[longitud] := $FF;
  // cargar roms
  if not(roms_load(@memory_temp, tetris_rom)) then
    exit;
  copymemory(@rom_mem[0, 0], @memory_temp[$0], $4000);
  copymemory(@rom_mem[1, 0], @memory_temp[$4000], $4000);
  copymemory(@memory[$8000], @memory_temp[$8000], $8000);
  // Cargar chars
  if not(roms_load(@memory_temp, tetris_gfx)) then
    exit;
  init_gfx(0, 8, 8, $800);
  gfx_set_desc_data(4, 0, 8 * 8 * 4, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // Dip
  marcade.dswa := $0;
  marcade.dswa_val := @tetris_dip_a;
  // final
  reset_tetris;
  start_tetris := true;
end;

end.
