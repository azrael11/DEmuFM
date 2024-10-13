unit superburgertime_hw;

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
  deco_common;

function start_superbergertime: boolean;

implementation

const
  supbtime_rom: array [0 .. 1] of tipo_roms = ((n: 'gk03'; l: $20000; p: 0; crc: $AEAEED61), (n: 'gk04'; l: $20000; p: $1; crc: $2BC5A4EB));
  supbtime_sound: tipo_roms = (n: 'gc06.bin'; l: $10000; p: $0; crc: $E0E6C0F4);
  supbtime_char: tipo_roms = (n: 'mae02.bin'; l: $80000; p: 0; crc: $A715CCA0);
  supbtime_oki: tipo_roms = (n: 'gc05.bin'; l: $20000; p: 0; crc: $2F2246FF);
  supbtime_sprites: array [0 .. 1] of tipo_roms = ((n: 'mae00.bin'; l: $80000; p: 1; crc: $30043094), (n: 'mae01.bin'; l: $80000; p: $0; crc: $434AF3FB));
  supbtime_dip_a: array [0 .. 8] of def_dip = ((mask: $0001; name: 'Cabinet'; number: 2; dip: ((dip_val: $1; dip_name: 'Cocktail'), (dip_val: $0; dip_name: 'Upright'), (), (), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $0002; name: 'Flip Screen'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $001C; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $10; dip_name: '2C 1C'), (dip_val: $1C; dip_name: '1C 1C'), (dip_val: $0C;
    dip_name: '1C 2C'), (dip_val: $14; dip_name: '1C 3C'), (dip_val: $04; dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 5C'), (dip_val: $08; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())),
    (mask: $00E0; name: 'Coin A'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $E0; dip_name: '1C 1C'), (dip_val: $60;
    dip_name: '1C 2C'), (dip_val: $A0; dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $C0; dip_name: '1C 5C'), (dip_val: $40; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())),
    (mask: $0100; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $100; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0200;
    name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $200; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $3000;
    name: 'Difficulty'; number: 4; dip: ((dip_val: $1000; dip_name: 'Easy'), (dip_val: $3000; dip_name: 'Normal'), (dip_val: $2000; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C000; name: 'Lives'; number: 4;
    dip: ((dip_val: $8000; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $C000; dip_name: '3'), (dip_val: $4000; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  tumblep_rom: array [0 .. 1] of tipo_roms = ((n: 'hl00-1.f12'; l: $40000; p: 0; crc: $FD697C1B), (n: 'hl01-1.f13'; l: $40000; p: $1; crc: $D5A62A3F));
  tumblep_sound: tipo_roms = (n: 'hl02-.f16'; l: $10000; p: $0; crc: $A5CAB888);
  tumblep_char: tipo_roms = (n: 'map-02.rom'; l: $80000; p: 0; crc: $DFCEAA26);
  tumblep_oki: tipo_roms = (n: 'hl03-.j15'; l: $20000; p: 0; crc: $01B81DA0);
  tumblep_sprites: array [0 .. 1] of tipo_roms = ((n: 'map-01.rom'; l: $80000; p: 0; crc: $E81FFA09), (n: 'map-00.rom'; l: $80000; p: $1; crc: $8C879CFE));
  tumblep_dip_a: array [0 .. 8] of def_dip = ((mask: $0001; name: 'Start Price'; number: 2; dip: ((dip_val: $1; dip_name: '1 Coin'), (dip_val: $0; dip_name: '2 Coin'), (), (), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $0002; name: 'Flip Screen'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $001C; name: 'Coin B'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $10; dip_name: '2C 1C'), (dip_val: $1C; dip_name: '1C 1C'), (dip_val: $0C;
    dip_name: '1C 2C'), (dip_val: $14; dip_name: '1C 3C'), (dip_val: $04; dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 5C'), (dip_val: $08; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())),
    (mask: $00E0; name: 'Coin A'; number: 8; dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $E0; dip_name: '1C 1C'), (dip_val: $60;
    dip_name: '1C 2C'), (dip_val: $A0; dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $C0; dip_name: '1C 5C'), (dip_val: $40; dip_name: '1C 6C'), (), (), (), (), (), (), (), ())),
    (mask: $0100; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $100; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $0200;
    name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $200; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $3000;
    name: 'Difficulty'; number: 4; dip: ((dip_val: $1000; dip_name: 'Easy'), (dip_val: $3000; dip_name: 'Normal'), (dip_val: $2000; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C000; name: 'Lives'; number: 4;
    dip: ((dip_val: $8000; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $C000; dip_name: '3'), (dip_val: $4000; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  rom: array [0 .. $3FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  video_update: procedure;

procedure update_video_supbtime;
begin
  fill_full_screen(3, 768);
  deco16ic_0.update_pf_2(3, true);
  deco_sprites_0.draw_sprites;
  deco16ic_0.update_pf_1(3, true);
  update_final_piece(0, 8, 320, 240, 3);
end;

procedure update_video_tumblep;
begin
  deco16ic_0.update_pf_2(3, false);
  deco16ic_0.update_pf_1(3, true);
  deco_sprites_0.draw_sprites;
  update_final_piece(0, 8, 319, 240, 3);
end;

procedure events_supbtime;
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
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
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

procedure supbtime_loop;
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
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        h6280_0.run(frame_s);
        frame_s := frame_s + h6280_0.tframes - h6280_0.contador;
        case f of
          247:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              video_update;
              marcade.in1 := marcade.in1 or $8;
            end;
          255:
            marcade.in1 := marcade.in1 and $F7;
        end;
      end;
      events_supbtime;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function supbtime_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $3FFFF:
      supbtime_getword := rom[direccion shr 1];
    $100000 .. $103FFF:
      supbtime_getword := ram[(direccion and $3FFF) shr 1];
    $120000 .. $1207FF:
      supbtime_getword := deco_sprites_0.ram[(direccion and $7FF) shr 1];
    $140000 .. $1407FF:
      supbtime_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $180000:
      supbtime_getword := marcade.in0;
    $180004, $180006, $18000E:
      supbtime_getword := $FFFF;
    $180002:
      supbtime_getword := marcade.dswa;
    $180008:
      supbtime_getword := marcade.in1;
    $18000A, $18000C:
      supbtime_getword := 0;
    $300000 .. $30000F:
      supbtime_getword := deco16ic_0.control_r((direccion and $F) shr 1);
    $320000 .. $320FFF:
      supbtime_getword := deco16ic_0.pf1.data[(direccion and $FFF) shr 1];
    $322000 .. $322FFF:
      supbtime_getword := deco16ic_0.pf2.data[(direccion and $FFF) shr 1];
  end;
end;

procedure change_color(tmp_color, numero: word);
var
  color: tcolor;
begin
  color.b := pal4bit(tmp_color shr 8);
  color.g := pal4bit(tmp_color shr 4);
  color.r := pal4bit(tmp_color);
  set_pal_color(color, numero);
  case numero of
    $100 .. $1FF:
      deco16ic_0.pf1.buffer_color[(numero shr 4) and $F] := true;
    $200 .. $2FF:
      deco16ic_0.pf2.buffer_color[(numero shr 4) and $F] := true;
  end;
end;

procedure supbtime_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $100000 .. $103FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $104000 .. $11FFFF, $120800 .. $13FFFF, $18000A .. $18000D:
      ;
    $120000 .. $1207FF:
      deco_sprites_0.ram[(direccion and $7FF) shr 1] := valor;
    $140000 .. $1407FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $1A0000:
      begin
        deco16_sound_latch := valor and $FF;
        h6280_0.set_irq_line(0, HOLD_LINE);
      end;
    $300000 .. $30000F:
      deco16ic_0.control_w((direccion and $F) shr 1, valor);
    $320000 .. $320FFF:
      begin
        deco16ic_0.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $321000 .. $321FFF, $323000 .. $323FFF:
      ;
    $322000 .. $322FFF:
      begin
        deco16ic_0.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $340000 .. $3407FF:
      deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $342000 .. $3427FF:
      deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
  end;
end;

// Tumblepop
function tumblep_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $7FFFF:
      tumblep_getword := rom[direccion shr 1];
    $120000 .. $123FFF:
      tumblep_getword := ram[(direccion and $3FFF) shr 1];
    $180000 .. $18000F:
      case (direccion and $F) of
        $0:
          tumblep_getword := marcade.in0;
        $2:
          tumblep_getword := marcade.dswa;
        $8:
          tumblep_getword := marcade.in1;
        $A, $C:
          tumblep_getword := 0;
      else
        tumblep_getword := $FFFF;
      end;
    $1A0000 .. $1A07FF:
      tumblep_getword := deco_sprites_0.ram[(direccion and $7FF) shr 1];
    $320000 .. $320FFF:
      tumblep_getword := deco16ic_0.pf1.data[(direccion and $FFF) shr 1];
    $322000 .. $322FFF:
      tumblep_getword := deco16ic_0.pf2.data[(direccion and $FFF) shr 1];
  end;
end;

procedure tumblep_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $7FFFF:
      ;
    $100000:
      begin
        deco16_sound_latch := valor and $FF;
        h6280_0.set_irq_line(0, HOLD_LINE);
      end;
    $120000 .. $123FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $140000 .. $1407FF:
      if (buffer_paleta[(direccion and $7FF) shr 1] <> valor) then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $18000C:
      ;
    $1A0000 .. $1A07FF:
      deco_sprites_0.ram[(direccion and $7FF) shr 1] := valor;
    $300000 .. $30000F:
      deco16ic_0.control_w((direccion and $F) shr 1, valor);
    $320000 .. $320FFF:
      begin
        deco16ic_0.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $322000 .. $322FFF:
      begin
        deco16ic_0.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $340000 .. $3407FF:
      deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $342000 .. $3427FF:
      deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
  end;
end;

// Main
procedure reset_supbtime;
begin
  m68000_0.reset;
  deco16ic_0.reset;
  deco_sprites_0.reset;
  deco16_snd_simple_reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $F7;
end;

function start_superbergertime: boolean;
const
  pt_x: array [0 .. 15] of dword = (256, 257, 258, 259, 260, 261, 262, 263, 0, 1, 2, 3, 4, 5, 6, 7);
  pt_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  ps_x: array [0 .. 15] of dword = (512, 513, 514, 515, 516, 517, 518, 519, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
var
  memory_temp: pbyte;
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $4000);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 8, $4000 * 16 * 8 + 8, $4000 * 16 * 8 + 0, 8, 0);
    convert_gfx(0, 0, memory_temp, @pt_x[8], @pt_y, false, false);
  end;
  procedure convert_tiles;
  begin
    init_gfx(1, 16, 16, $1000);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(4, 0, 32 * 16, $1000 * 32 * 16 + 8, $1000 * 32 * 16 + 0, 8, 0);
    convert_gfx(1, 0, memory_temp, @pt_x, @pt_y, false, false);
  end;
  procedure convert_sprites;
  begin
    init_gfx(2, 16, 16, $2000);
    gfx[2].trans[0] := true;
    gfx_set_desc_data(4, 0, 32 * 32, 24, 8, 16, 0);
    convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  start_superbergertime := false;
  machine_calls.general_loop := supbtime_loop;
  machine_calls.reset := reset_supbtime;
  machine_calls.fps_max := 58;
  start_audio(false);
  deco16ic_0 := chip_16ic.create(1, 2, $100, $100, $F, $F, 0, 1, 0, 16, nil, nil);
  deco_sprites_0 := tdeco16_sprite.create(2, 3, 304, 0, $1FFF);
  screen_init(3, 512, 512, false, true);
  start_video(320, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(14000000, $100);
  // Sound CPU
  deco16_snd_simple_init(32220000 div 8, 32220000, nil);
  getmem(memory_temp, $100000);
  case main_vars.machine_type of
    159:
      begin // Superburger Time
        video_update := update_video_supbtime;
        m68000_0.change_ram16_calls(supbtime_getword, supbtime_putword);
        // cargar roms
        if not(roms_load16w(@rom, supbtime_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, supbtime_sound)) then
          exit;
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, supbtime_oki)) then
          exit;
        // convertir chars}
        if not(roms_load(memory_temp, supbtime_char)) then
          exit;
        convert_chars;
        // Tiles
        convert_tiles;
        // Sprites
        if not(roms_load16b(memory_temp, supbtime_sprites)) then
          exit;
        convert_sprites;
        // final
        freemem(memory_temp);
        // Dip
        marcade.dswa := $FEFE;
        marcade.dswa_val := @supbtime_dip_a;
      end;
    161:
      begin // Tumblepop
        video_update := update_video_tumblep;
        m68000_0.change_ram16_calls(tumblep_getword, tumblep_putword);
        // cargar roms
        if not(roms_load16w(@rom, tumblep_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, tumblep_sound)) then
          exit;
        // OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, tumblep_oki)) then
          exit;
        // convertir chars}
        if not(roms_load(memory_temp, tumblep_char)) then
          exit;
        deco56_decrypt_gfx(memory_temp, $80000);
        convert_chars;
        // Tiles
        convert_tiles;
        // Sprites
        if not(roms_load16b(memory_temp, tumblep_sprites)) then
          exit;
        convert_sprites;
        // final
        freemem(memory_temp);
        // Dip
        marcade.dswa := $FEFF;
        marcade.dswa_val := @tumblep_dip_a;
      end;
  end;
  reset_supbtime;
  start_superbergertime := true;
end;

end.
