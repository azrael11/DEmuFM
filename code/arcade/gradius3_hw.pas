unit gradius3_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ym_2151,
  k052109,
  k051960,
  k007232;

function start_gradius3: boolean;

implementation

const
  // gradius3
  gradius3_rom: array [0 .. 1] of tipo_roms = ((n: '945_r13.f15'; l: $20000; p: 0; crc: $CFFD103F),
    (n: '945_r12.e15'; l: $20000; p: $1; crc: $0B968EF6));
  gradius3_rom_sub: array [0 .. 7] of tipo_roms = ((n: '945_m09.r17'; l: $20000; p: 0;
    crc: $B4A6DF25), (n: '945_m08.n17'; l: $20000; p: $1; crc: $74E981D2), (n: '945_l06b.r11';
    l: $20000; p: $40000; crc: $83772304), (n: '945_l06a.n11'; l: $20000; p: $40001;
    crc: $E1FD75B6), (n: '945_l07c.r15'; l: $20000; p: $80000; crc: $C1E399B6), (n: '945_l07a.n15';
    l: $20000; p: $80001; crc: $96222D04), (n: '945_l07d.r13'; l: $20000; p: $C0000;
    crc: $4C16D4BD), (n: '945_l07b.n13'; l: $20000; p: $C0001; crc: $5E209D01));
  gradius3_sound: tipo_roms = (n: '945_r05.d9'; l: $10000; p: 0; crc: $C8C45365);
  gradius3_sprites_1: array [0 .. 1] of tipo_roms = ((n: '945_a02.l3'; l: $80000; p: 0;
    crc: $4DFFFD74), (n: '945_a01.h3'; l: $80000; p: 2; crc: $339D6DD2));
  gradius3_sprites_2: array [0 .. 7] of tipo_roms = ((n: '945_l04a.k6'; l: $20000; p: $100000;
    crc: $884E21EE), (n: '945_l04c.m6'; l: $20000; p: $100001; crc: $45BCD921), (n: '945_l03a.e6';
    l: $20000; p: $100002; crc: $A67EF087), (n: '945_l03c.h6'; l: $20000; p: $100003;
    crc: $A56BE17A), (n: '945_l04b.k8'; l: $20000; p: $180000; crc: $843BC67D), (n: '945_l04d.m8';
    l: $20000; p: $180001; crc: $0A98D08E), (n: '945_l03b.e8'; l: $20000; p: $180002;
    crc: $933E68B9), (n: '945_l03d.h8'; l: $20000; p: $180003; crc: $F375E87B));
  gradius3_k007232: array [0 .. 2] of tipo_roms = ((n: '945_a10.b15'; l: $40000; p: 0;
    crc: $1D083E10), (n: '945_l11a.c18'; l: $20000; p: $40000; crc: $6043F4EB), (n: '945_l11b.c20';
    l: $20000; p: $60000; crc: $89EA3BAF));
  // DIP
  gradius3_dip_a: array [0 .. 1] of def_dip = ((mask: $F; name: 'Coinage'; number: 16;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $2; dip_name: '4C 1C'), (dip_val: $5;
    dip_name: '3C 1C'), (dip_val: $8; dip_name: '2C 1C'), (dip_val: $4; dip_name: '2C 3C'),
    (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3;
    dip_name: '3C 4C'), (dip_val: $7; dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'),
    (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C;
    dip_name: '1C 4C'), (dip_val: $B; dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'),
    (dip_val: $9; dip_name: '1C 7C'))), ());
  gradius3_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '2'), (dip_val: $2; dip_name: '3'), (dip_val: $1;
    dip_name: '5'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $18; dip_name: '20k 70k+'), (dip_val: $10;
    dip_name: '100k 100k+'), (dip_val: $8; dip_name: '50k'), (dip_val: $0; dip_name: '100k'), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20;
    dip_name: 'Difficult'), (dip_val: $0; dip_name: 'Very Difficult'), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  gradius3_dip_c: array [0 .. 2] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Upright Controls'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Single'), (dip_val: $0; dip_name: 'Dual'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), ());

var
  rom: array [0 .. $1FFFF] of word;
  rom_sub: array [0 .. $7FFFF] of word;
  ram, ram_sub, ram_share: array [0 .. $1FFF] of word;
  ram_gfx: array [0 .. $FFFF] of word;
  sprite_rom, k007232_rom: pbyte;
  sound_latch, sprite_colorbase, irqB_mask: byte;
  layer_colorbase: array [0 .. 2] of byte;
  irqA_mask, priority: boolean;

procedure gradius3_cb(layer, bank: word; var code: dword; var color: word; var flags: word;
  var priority: word);
begin
  code := code or (((color and $01) shl 8) or ((color and $1C) shl 7));
  color := layer_colorbase[layer] + ((color and $E0) shr 5);
end;

procedure gradius3_sprite_cb(var code: word; var color: word; var pri: word; var shadow: word);
const
  L0 = 1;
  L1 = 2;
  L2 = 3;
  primask: array [0 .. 1, 0 .. 3] of byte = ((L0 or L2, L0, L0 or L2, L0 or L1 or L2),
    (L1 or L2, L2, 0, L0 or L1 or L2));
var
  prio: byte;
begin
  prio := ((color and $60) shr 5);
  if not(priority) then
    pri := primask[0][prio]
  else
    pri := primask[1][prio];
  code := code or ((color and $01) shl 13);
  color := sprite_colorbase + ((color and $1E) shr 1);
end;

procedure gradius3_k007232_cb(valor: byte);
begin
  k007232_0.set_volume(0, (valor shr 4) * $11, 0);
  k007232_0.set_volume(1, 0, (valor and $0F) * $11);
end;

procedure update_video_gradius3;
begin
  k052109_0.write($1D80, $10);
  k052109_0.write($1F00, $32);
  k052109_0.draw_tiles;
  k051960_0.update_sprites;
  fill_full_screen(4, 0);
  if priority then
  begin
    k051960_0.draw_sprites(6, -1);
    k051960_0.draw_sprites(5, -1);
    k052109_0.draw_layer(0, 4);
    k051960_0.draw_sprites(4, -1);
    k051960_0.draw_sprites(3, -1);
    k052109_0.draw_layer(1, 4);
    k051960_0.draw_sprites(2, -1);
    k051960_0.draw_sprites(1, -1);
    k052109_0.draw_layer(2, 4);
    k051960_0.draw_sprites(0, -1);
  end
  else
  begin
    k051960_0.draw_sprites(6, -1);
    k051960_0.draw_sprites(5, -1);
    k052109_0.draw_layer(1, 4);
    k051960_0.draw_sprites(4, -1);
    k051960_0.draw_sprites(3, -1);
    k052109_0.draw_layer(2, 4);
    k051960_0.draw_sprites(2, -1);
    k051960_0.draw_sprites(1, -1);
    k052109_0.draw_layer(0, 4);
    k051960_0.draw_sprites(0, -1);
  end;
  update_final_piece(96, 16, 320, 224, 4);
end;

procedure events_gradius3;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
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
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
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
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
  end;
end;

procedure gradius3_loop;
var
  frame_m, frame_sub, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_sub := m68000_1.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused then
    begin
      for f := 0 to $FF do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sub
        m68000_1.run(frame_sub);
        frame_sub := frame_sub + m68000_1.tframes - m68000_1.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        case f of
          15:
            if (irqB_mask and 2) <> 0 then
              m68000_1.irq[2] := HOLD_LINE;
          239:
            begin
              update_video_gradius3;
              if irqA_mask then
                m68000_0.irq[2] := HOLD_LINE;
              if (irqB_mask and 1) <> 0 then
                m68000_1.irq[1] := HOLD_LINE;
            end;
        end;
      end;
      events_gradius3;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Main CPU
function gradius3_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      gradius3_getword := rom[direccion shr 1];
    $040000 .. $043FFF:
      gradius3_getword := ram[(direccion and $3FFF) shr 1];
    $080000 .. $080FFF:
      gradius3_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $0C8000:
      gradius3_getword := marcade.in0; // system
    $0C8002:
      gradius3_getword := marcade.in1; // p1
    $0C8004:
      gradius3_getword := marcade.in2; // p2
    $0C8006:
      gradius3_getword := marcade.dswc; // dsw3
    $0D0000:
      gradius3_getword := marcade.dswa; // dsw1
    $0D0002:
      gradius3_getword := marcade.dswb; // dsw2
    $100000 .. $103FFF:
      gradius3_getword := ram_share[(direccion and $3FFF) shr 1];
    $14C000 .. $153FFF:
      gradius3_getword := k052109_0.read((direccion - $14C000) shr 1);
    $180000 .. $19FFFF:
      gradius3_getword := (ram_gfx[(direccion and $1FFFF) shr 1] shr 8) +
        ((ram_gfx[(direccion and $1FFFF) shr 1] and $FF) shl 8);
  end;
end;

procedure gradius3_putword(direccion: dword; valor: word);
  procedure change_color_gradius3(pos, valor: word); inline;
  var
    color: tcolor;
  begin
    color.r := pal5bit(valor shr 10);
    color.g := pal5bit(valor shr 5);
    color.b := pal5bit(valor);
    set_pal_color_alpha(color, pos);
    k052109_0.clean_video_buffer;
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $40000 .. $43FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $80000 .. $80FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color_gradius3((direccion and $FFF) shr 1, valor);
      end;
    $C0000:
      begin
        valor := valor shr 8;
        priority := (valor and $4) <> 0;
        if (valor and $8) <> 0 then
          m68000_1.change_halt(CLEAR_LINE)
        else
          m68000_1.change_halt(ASSERT_LINE);
        irqA_mask := (valor and $20) <> 0;
      end;
    $D8000:
      if (irqB_mask and 4) <> 0 then
        m68000_1.irq[4] := HOLD_LINE;
    $E0000:
      ; // wd
    $E8000:
      sound_latch := valor shr 8;
    $F0000:
      z80_0.change_irq(HOLD_LINE);
    $100000 .. $103FFF:
      ram_share[(direccion and $3FFF) shr 1] := valor;
    $14C000 .. $153FFF:
      begin
        direccion := (direccion - $14C000) shr 1;
        if not(m68000_0.write_8bits_lo_dir) then
          k052109_0.write(direccion, valor);
        if m68000_0.write_8bits_lo_dir then
          k052109_0.write(direccion, valor shr 8);
      end;
    $180000 .. $19FFFF:
      if ram_gfx[(direccion and $1FFFF) shr 1] <> (((valor and $FF) shl 8) + (valor shr 8)) then
      begin
        ram_gfx[(direccion and $1FFFF) shr 1] := ((valor and $FF) shl 8) + (valor shr 8);
        k052109_0.recalc_chars(((direccion and $1FFFF) shr 1) div 16);
      end;
  end;
end;

// Sub CPU
function gradius3_getword_sub(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      gradius3_getword_sub := rom_sub[direccion shr 1];
    $100000 .. $103FFF:
      gradius3_getword_sub := ram_sub[(direccion and $3FFF) shr 1];
    $200000 .. $203FFF:
      gradius3_getword_sub := ram_share[(direccion and $3FFF) shr 1];
    $24C000 .. $253FFF:
      gradius3_getword_sub := k052109_0.read((direccion - $24C000) shr 1);
    $280000 .. $29FFFF:
      gradius3_getword_sub := (ram_gfx[(direccion and $1FFFF) shr 1] shr 8) +
        ((ram_gfx[(direccion and $1FFFF) shr 1] and $FF) shl 8);
    $2C0000 .. $2C000F:
      gradius3_getword_sub := k051960_0.k051937_read((direccion and $F) shr 1);
    $2C0800 .. $2C0FFF:
      gradius3_getword_sub := k051960_0.read((direccion and $7FF) shr 1);
    $400000 .. $5FFFFF:
      gradius3_getword_sub := (sprite_rom[(direccion and $1FFFFF) + 1] shl 8) +
        sprite_rom[direccion and $1FFFFF];
  end;
end;

procedure gradius3_putword_sub(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $103FFF:
      ram_sub[(direccion and $3FFF) shr 1] := valor;
    $140000:
      irqB_mask := (valor shr 8) and $7;
    $200000 .. $203FFF:
      ram_share[(direccion and $3FFF) shr 1] := valor;
    $24C000 .. $253FFF:
      begin
        direccion := (direccion - $24C000) shr 1;
        if not(m68000_1.write_8bits_lo_dir) then
          k052109_0.write(direccion, valor);
        if m68000_1.write_8bits_lo_dir then
          k052109_0.write(direccion, valor shr 8);
      end;
    $280000 .. $29FFFF:
      if ram_gfx[(direccion and $1FFFF) shr 1] <> (((valor and $FF) shl 8) + (valor shr 8)) then
      begin
        ram_gfx[(direccion and $1FFFF) shr 1] := ((valor and $FF) shl 8) + (valor shr 8);
        k052109_0.recalc_chars(((direccion and $1FFFF) shr 1) div 16);
      end;
    $2C0000 .. $2C000F:
      k051960_0.k051937_write((direccion and $F) shr 1, valor and $FF);
    $2C0800 .. $2C0FFF:
      k051960_0.write((direccion and $7FF) shr 1, valor and $FF);
  end;
end;

// Audio CPU
function gradius3_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $EFFF, $F800 .. $FFFF:
      gradius3_snd_getbyte := mem_snd[direccion];
    $F010:
      gradius3_snd_getbyte := sound_latch;
    $F020 .. $F02D:
      gradius3_snd_getbyte := k007232_0.read(direccion and $F);
    $F031:
      gradius3_snd_getbyte := ym2151_0.status;
  end;
end;

procedure gradius3_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $EFFF:
      ; // ROM
    $F000:
      k007232_0.set_bank(valor and 3, (valor shr 2) and 3);
    $F020 .. $F02D:
      k007232_0.write(direccion and $F, valor);
    $F030:
      ym2151_0.reg(valor);
    $F031:
      ym2151_0.write(valor);
    $F800 .. $FFFF:
      mem_snd[direccion] := valor;
  end;
end;

procedure gradius3_sound_update;
begin
  ym2151_0.update;
  k007232_0.update;
end;

// Main
procedure reset_gradius3;
begin
  m68000_0.reset;
  m68000_1.reset;
  m68000_1.change_halt(ASSERT_LINE);
  z80_0.reset;
  k052109_0.reset;
  ym2151_0.reset;
  k051960_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_latch := 0;
  irqA_mask := false;
  irqB_mask := 0;
end;

procedure close_gradius3;
begin
  if k007232_rom <> nil then
    freemem(k007232_rom);
  if sprite_rom <> nil then
    freemem(sprite_rom);
  k007232_rom := nil;
  sprite_rom := nil;
end;

function start_gradius3: boolean;
begin
  start_gradius3 := false;
  machine_calls.close := close_gradius3;
  machine_calls.reset := reset_gradius3;
  machine_calls.general_loop := gradius3_loop;
  // Pantallas para el K052109
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, true);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 1024, 1024, false, true);
  start_video(320, 224, true);
  start_audio(true);
  // cargar roms
  if not(roms_load16w(@rom, gradius3_rom)) then
    exit;
  if not(roms_load16w(@rom_sub, gradius3_rom_sub)) then
    exit;
  // cargar sonido
  if not(roms_load(@mem_snd, gradius3_sound)) then
    exit;
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 256);
  m68000_0.change_ram16_calls(gradius3_getword, gradius3_putword);
  m68000_1 := cpu_m68000.create(10000000, 256);
  m68000_1.change_ram16_calls(gradius3_getword_sub, gradius3_putword_sub);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(gradius3_snd_getbyte, gradius3_snd_putbyte);
  z80_0.init_sound(gradius3_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  getmem(k007232_rom, $80000);
  if not(roms_load(k007232_rom, gradius3_k007232)) then
    exit;
  k007232_0 := k007232_chip.create(3579545, k007232_rom, $80000, 0.20, gradius3_k007232_cb, true);
  // Iniciar video
  k052109_0 := k052109_chip.create(1, 2, 3, 0, gradius3_cb, pbyte(@ram_gfx[0]), $20000);
  getmem(sprite_rom, $200000);
  if not(roms_load32b(sprite_rom, gradius3_sprites_1)) then
    exit;
  if not(roms_load32b_b(sprite_rom, gradius3_sprites_2)) then
    exit;
  k051960_0 := k051960_chip.create(4, 1, sprite_rom, $200000, gradius3_sprite_cb, 1);
  layer_colorbase[0] := 0;
  layer_colorbase[1] := 32;
  layer_colorbase[2] := 48;
  sprite_colorbase := 16;
  // DIP
  marcade.dswa := $FF;
  marcade.dswa_val := @gradius3_dip_a;
  marcade.dswb := $5A;
  marcade.dswb_val := @gradius3_dip_b;
  marcade.dswc := $FF;
  marcade.dswc_val := @gradius3_dip_c;
  // final
  reset_gradius3;
  start_gradius3 := true;
end;

end.
