unit dietgogo_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  oki6295,
  sound_engine,
  hu6280,
  deco_16ic,
  deco_decr,
  deco_common,
  deco_104,
  misc_functions;

function start_dietgogo: boolean;

implementation

const
  dietgo_rom: array [0 .. 1] of tipo_roms = ((n: 'jy00-2.h4'; l: $40000; p: 1; crc: $014DCF62), (n: 'jy01-2.h5'; l: $40000; p: $0; crc: $793EBD83));
  dietgo_sound: tipo_roms = (n: 'jy02.m14'; l: $10000; p: $0; crc: $4E3492A5);
  dietgo_char: tipo_roms = (n: 'may00'; l: $100000; p: 0; crc: $234D1F8D);
  dietgo_oki: tipo_roms = (n: 'may03'; l: $80000; p: 0; crc: $B6E42BAE);
  dietgo_sprites: array [0 .. 1] of tipo_roms = ((n: 'may01'; l: $100000; p: 0; crc: $2DA57D04), (n: 'may02'; l: $100000; p: $1; crc: $3A66A713));
  dietgo_dip_a: array [0 .. 7] of def_dip = ((mask: $0007; name: 'Coin A'; number: 8; dip: ((dip_val: $0; dip_name: '3 Coin - 1 Credit'), (dip_val: $1; dip_name: '2 Coin - 1 Credit'), (dip_val: $7;
    dip_name: '1 Coin - 1 Credit'), (dip_val: $6; dip_name: '1 Coin - 2 Credit'), (dip_val: $5; dip_name: '1 Coin - 3 Credit'), (dip_val: $4; dip_name: '1 Coin - 4 Credit'), (dip_val: $3;
    dip_name: '1 Coin - 5 Credit'), (dip_val: $2; dip_name: '1 Coin - 6 Credit'), (), (), (), (), (), (), (), ())), (mask: $0038; name: 'Coin B'; number: 8;
    dip: ((dip_val: $0; dip_name: '3 Coin - 1 Credit'), (dip_val: $8; dip_name: '2 Coin - 1 Credit'), (dip_val: $38; dip_name: '1 Coin - 1 Credit'), (dip_val: $30;
    dip_name: '1 Coin - 2 Credit'), (dip_val: $28; dip_name: '1 Coin - 3 Credit'), (dip_val: $20; dip_name: '1 Coin - 4 Credit'), (dip_val: $18; dip_name: '1 Coin - 5 Credit'), (dip_val: $10;
    dip_name: '1 Coin - 6 Credit'), (), (), (), (), (), (), (), ())), (mask: $0040; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0080; name: 'Continue Coin'; number: 2;
    dip: ((dip_val: $80; dip_name: '1 Start/1 Continue'), (dip_val: $0; dip_name: '2 Start/1 Continue'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0300; name: 'Lives';
    number: 4; dip: ((dip_val: $100; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $300; dip_name: '3'), (dip_val: $200; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $0C00; name: 'Difficulty'; number: 4; dip: ((dip_val: $800; dip_name: 'Easy'), (dip_val: $C00; dip_name: 'Normal'), (dip_val: $400; dip_name: 'Hard'), (dip_val: $000;
    dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $1000; name: 'Free Play'; number: 2;
    dip: ((dip_val: $1000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  rom_opcode, rom_data: array [0 .. $3FFFF] of word;
  ram: array [0 .. $7FFF] of word;

procedure update_video_dietgo;
begin
  deco16ic_0.update_pf_2(3, false);
  deco16ic_0.update_pf_1(3, true);
  deco_sprites_0.draw_sprites;
  update_final_piece(0, 8, 320, 240, 3);
end;

procedure events_dietgo;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $0001);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $0002);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $0004);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $0008);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFEF)
    else
      marcade.in0 := (marcade.in0 or $0010);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $0020);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $0040);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $0080);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $0100);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $0200);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $0400);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $0800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
  end;
end;

procedure dietgo_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := h6280_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // Sound
        h6280_0.run(trunc(frame_s));
        frame_s := frame_s + h6280_0.tframes - h6280_0.contador;
        case f of
          247:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              update_video_dietgo;
              marcade.in1 := marcade.in1 or $8;
            end;
          255:
            marcade.in1 := marcade.in1 and $7;
        end;
      end;
      events_dietgo;
      video_sync;
    end;
  end;
end;

function dietgo_getword(direccion: dword): word;
  function dietgo_protection_region_0_104_r(real_address: word): word;
  var
    deco146_addr: word;
    cs: byte;
  begin
    // int real_address = 0 + (offset *2);
    deco146_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
    cs := 0;
    dietgo_protection_region_0_104_r := main_deco104.read_data(deco146_addr, cs);
  end;

begin
  case direccion of
    $0 .. $7FFFF:
      if m68000_0.opcode then
        dietgo_getword := rom_opcode[direccion shr 1]
      else
        dietgo_getword := rom_data[direccion shr 1];
    $210000 .. $210FFF:
      dietgo_getword := deco16ic_0.pf1.data[(direccion and $FFF) shr 1];
    $212000 .. $212FFF:
      dietgo_getword := deco16ic_0.pf2.data[(direccion and $FFF) shr 1];
    $280000 .. $2807FF:
      dietgo_getword := deco_sprites_0.ram[(direccion and $7FF) shr 1];
    $300000 .. $300BFF:
      dietgo_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $340000 .. $343FFF:
      dietgo_getword := dietgo_protection_region_0_104_r(direccion and $3FFF);
    $380000 .. $38FFFF:
      dietgo_getword := ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure dietgo_putword(direccion: dword; valor: word);

  procedure change_color(numero: word);
  var
    color: tcolor;
  begin
    color.b := buffer_paleta[(numero shl 1)] and $FF;
    color.g := buffer_paleta[(numero shl 1) + 1] shr 8;
    color.r := buffer_paleta[(numero shl 1) + 1] and $FF;
    set_pal_color(color, numero);
    case numero of
      $000 .. $0FF:
        deco16ic_0.pf1.buffer_color[(numero shr 4) and $F] := true;
      $100 .. $1FF:
        deco16ic_0.pf2.buffer_color[(numero shr 4) and $F] := true;
    end;
  end;

  procedure dietgo_protection_region_0_104_w(real_address, data: word);
  var
    deco146_addr: word;
    cs: byte;
  begin
    // int real_address = 0 + (offset *2);
    deco146_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
    cs := 0;
    main_deco104.write_data(deco146_addr, data, cs);
  end;

begin
  case direccion of
    0 .. $7FFFF:
      ; // ROM
    $200000 .. $20000F:
      deco16ic_0.control_w((direccion and $F) shr 1, valor);
    $210000 .. $210FFF:
      begin
        deco16ic_0.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $212000 .. $212FFF:
      begin
        deco16ic_0.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $220000 .. $2207FF:
      deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $222000 .. $2227FF:
      deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
    $280000 .. $2807FF:
      deco_sprites_0.ram[(direccion and $7FF) shr 1] := valor;
    $211000 .. $211FFF, $213000 .. $213FFF, $2C0002, $2C000A, $230000 .. $2300FF, $240000:
      ;
    $300000 .. $300BFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color((direccion and $FFF) shr 2);
      end;
    $340000 .. $343FFF:
      dietgo_protection_region_0_104_w(direccion and $3FFF, valor);
    $380000 .. $38FFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

function dietgo_bank_callback(bank: word): word;
begin
  dietgo_bank_callback := ((bank shr 4) and $7) * $1000;
end;

procedure sound_bank_rom(valor: byte);
begin
  copymemory(oki_6295_0.get_rom_addr, @oki_rom[valor and 1], $40000);
end;

// Main
procedure reset_dietgo;
begin
  m68000_0.reset;
  deco16ic_0.reset;
  deco_sprites_0.reset;
  main_deco104.reset;
  copymemory(oki_6295_0.get_rom_addr, @oki_rom[0], $40000);
  deco16_snd_simple_reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $7;
end;

function start_dietgogo: boolean;
const
  pt_x: array [0 .. 15] of dword = (256, 257, 258, 259, 260, 261, 262, 263, 0, 1, 2, 3, 4, 5, 6, 7);
  pt_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  ps_x: array [0 .. 15] of dword = (512, 513, 514, 515, 516, 517, 518, 519, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
var
  memory_temp, ptemp: pbyte;
  memory_temp_rom: pword;
begin
  start_dietgogo := false;
  machine_calls.general_loop := dietgo_loop;
  machine_calls.reset := reset_dietgo;
  machine_calls.fps_max := 58;
  start_audio(false);
  deco16ic_0 := chip_16ic.create(1, 2, $0, $0, $F, $F, 0, 1, 0, 16, dietgo_bank_callback, dietgo_bank_callback);
  deco_sprites_0 := tdeco16_sprite.create(2, 3, 304, $200, $3FFF);
  screen_init(3, 512, 512, false, true);
  start_video(320, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(14000000, $100);
  m68000_0.change_ram16_calls(dietgo_getword, dietgo_putword);
  // Sound CPU
  deco16_snd_simple_init(32220000 div 12, 32220000, sound_bank_rom);
  getmem(memory_temp, $200000);
  getmem(memory_temp_rom, $80000);
  // cargar roms
  if not(roms_load16w(memory_temp_rom, dietgo_rom)) then
    exit;
  deco102_decrypt_cpu(memory_temp_rom, @rom_opcode, @rom_data, $E9BA, $01, $19, $80000);
  // cargar sonido
  if not(roms_load(@mem_snd, dietgo_sound)) then
    exit;
  // OKI rom
  if not(roms_load(memory_temp, dietgo_oki)) then
    exit;
  ptemp := memory_temp;
  copymemory(@oki_rom[0], ptemp, $40000);
  inc(ptemp, $40000);
  copymemory(@oki_rom[1], ptemp, $40000);
  // convertir chars
  if not(roms_load(memory_temp, dietgo_char)) then
    exit;
  deco56_decrypt_gfx(memory_temp, $100000);
  init_gfx(0, 8, 8, $8000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 16 * 8, $8000 * 16 * 8 + 8, $8000 * 16 * 8 + 0, 8, 0);
  convert_gfx(0, 0, memory_temp, @pt_x[8], @pt_y, false, false);
  // Tiles
  init_gfx(1, 16, 16, $2000);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 16, $2000 * 32 * 16 + 8, $2000 * 32 * 16 + 0, 8, 0);
  convert_gfx(1, 0, memory_temp, @pt_x, @pt_y, false, false);
  // Sprites
  if not(roms_load16b(memory_temp, dietgo_sprites)) then
    exit;
  init_gfx(2, 16, 16, $4000);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 32, 24, 8, 16, 0);
  convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
  // Proteccion deco104
  main_deco104 := cpu_deco_104.create;
  main_deco104.SET_INTERFACE_SCRAMBLE_INTERLEAVE;
  main_deco104.SET_USE_MAGIC_ADDRESS_XOR;
  // Dip
  marcade.dswa := $FFFF;
  marcade.dswa_val := @dietgo_dip_a;
  // final
  freemem(memory_temp_rom);
  freemem(memory_temp);
  reset_dietgo;
  start_dietgogo := true;
end;

end.
