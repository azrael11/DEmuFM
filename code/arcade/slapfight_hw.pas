unit slapfight_hw;

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
  m6805,
  sound_engine,
  timer_engine,
  taito_68705;

function start_slapfight: boolean;

implementation

const
  // Tiger Heli
  tigerh_rom: array [0 .. 2] of tipo_roms = ((n: 'a47_00-1.8p'; l: $4000; p: 0; crc: $4BE73246), (n: 'a47_01-1.8n'; l: $4000; p: $4000; crc: $AAD04867), (n: 'a47_02-1.8k'; l: $4000; p: $8000; crc: $4843F15C));
  tigerh_snd: tipo_roms = (n: 'a47_03.12d'; l: $2000; p: 0; crc: $D105260F);
  tigerh_mcu: tipo_roms = (n: 'a47_14.6a'; l: $800; p: 0; crc: $4042489F);
  tigerh_pal: array [0 .. 2] of tipo_roms = ((n: '82s129.12q'; l: $100; p: 0; crc: $2C69350D), (n: '82s129.12m'; l: $100; p: $100; crc: $7142E972), (n: '82s129.12n'; l: $100; p: $200; crc: $25F273F2));
  tigerh_char: array [0 .. 1] of tipo_roms = ((n: 'a47_05.6f'; l: $2000; p: 0; crc: $C5325B49), (n: 'a47_04.6g'; l: $2000; p: $2000; crc: $CD59628E));
  tigerh_sprites: array [0 .. 3] of tipo_roms = ((n: 'a47_13.8j'; l: $4000; p: 0; crc: $739A7E7E), (n: 'a47_12.6j'; l: $4000; p: $4000; crc: $C064ECDB), (n: 'a47_11.8h'; l: $4000; p: $8000; crc: $744FAE9B), (n: 'a47_10.6h'; l: $4000; p: $C000; crc: $E1CF844E));
  tigerh_tiles: array [0 .. 3] of tipo_roms = ((n: 'a47_09.4m'; l: $4000; p: 0; crc: $31FAE8A8), (n: 'a47_08.6m'; l: $4000; p: $4000; crc: $E539AF2B), (n: 'a47_07.6n'; l: $4000; p: $8000; crc: $02FDD429), (n: 'a47_06.6p'; l: $4000; p: $C000; crc: $11FBCC8C));
  tigerh_dip_a: array [0 .. 6] of def_dip2 = ((mask: 7; name: 'Coinage'; number: 8; val8: (2, 4, 7, 3, 6, 5, 0, 1); name8: ('3C 1C', '2C 1C', '1C 1C', '2C 3C', '1C 2C', '1C 3C', 'Free Play', 'Invalid')), (mask: 8; name: 'Demo Sounds'; number: 2; val2: (0, 8);
    name2: ('Off', 'On')), (mask: $10; name: 'Cabinet'; number: 2; val2: (0, $10); name2: ('Upright', 'Cocktail')), (mask: $20; name: 'Flip Screen'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $40; name: 'Service'; number: 2; val2: ($40, 0); name2: ('Off', 'On')),
    (mask: $80; name: 'Player Speed'; number: 2; val2: ($80, 0); name2: ('Normal', 'Fast')), ());
  tigerh_dip_b: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (1, 0, 3, 2); name4: ('1', '2', '3', '5')), (mask: $C; name: 'Difficulty'; number: 4; val4: ($C, 8, 4, 0); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: $10; name: 'Bonus Life';
    number: 2; val2: ($10, 0); name2: ('20K 80K+', '50K 120K+')), ());
  // Slap Fight
  sf_rom: array [0 .. 1] of tipo_roms = ((n: 'a77_00.8p'; l: $8000; p: 0; crc: $674C0E0F), (n: 'a77_01.8n'; l: $8000; p: $8000; crc: $3C42E4A7));
  sf_snd: tipo_roms = (n: 'a77_02.12d'; l: $2000; p: 0; crc: $87F4705A);
  sf_mcu: tipo_roms = (n: 'a77_13.6a'; l: $800; p: 0; crc: $A70C81D9);
  sf_pal: array [0 .. 2] of tipo_roms = ((n: '21_82s129.12q'; l: $100; p: 0; crc: $A0EFAF99), (n: '20_82s129.12m'; l: $100; p: $100; crc: $A56D57E5), (n: '19_82s129.12n'; l: $100; p: $200; crc: $5CBF9FBF));
  sf_char: array [0 .. 1] of tipo_roms = ((n: 'a77_04.6f'; l: $2000; p: 0; crc: $2AC7B943), (n: 'a77_03.6g'; l: $2000; p: $2000; crc: $33CADC93));
  sf_sprites: array [0 .. 3] of tipo_roms = ((n: 'a77_12.8j'; l: $8000; p: 0; crc: $8545D397), (n: 'a77_11.7j'; l: $8000; p: $8000; crc: $B1B7B925), (n: 'a77_10.8h'; l: $8000; p: $10000; crc: $422D946B), (n: 'a77_09.7h'; l: $8000; p: $18000; crc: $587113AE));
  sf_tiles: array [0 .. 3] of tipo_roms = ((n: 'a77_08.6k'; l: $8000; p: 0; crc: $B6358305), (n: 'a77_07.6m'; l: $8000; p: $8000; crc: $E92D9D60), (n: 'a77_06.6n'; l: $8000; p: $10000; crc: $5FAEEEA3), (n: 'a77_05.6p'; l: $8000; p: $18000; crc: $974E2EA9));
  sf_dip_a: array [0 .. 6] of def_dip2 = ((mask: 3; name: 'Coin B'; number: 4; val4: (2, 3, 0, 1); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $C; name: 'Coin A'; number: 4; val4: (8, $C, 0, 4); name4: ('2C 1C', '1C 1C', '2C 3C', '1C 2C')), (mask: $10;
    name: 'Demo Sounds'; number: 2; val2: (0, $10); name2: ('Off', 'On')), (mask: $20; name: 'Screen Test'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $80; name: 'Cabinet'; number: 2;
    val2: (0, $80); name2: ('Upright', 'Cocktail')), ());
  sf_dip_b: array [0 .. 4] of def_dip2 = ((mask: 2; name: 'Service'; number: 2; val2: (0, 1); name2: ('On', 'Off')), (mask: $C; name: 'Lives'; number: 4; val4: (8, 0, $C, 4); name4: ('1', '2', '3', '5')), (mask: $30; name: 'Bonus Life'; number: 4; val4: ($30, $10, $20, 0);
    name4: ('30K 100K', '50K 200K', '50K', '100K')), (mask: $C0; name: 'Difficulty'; number: 4; val4: ($40, $C0, $80, 0); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), ());

var
  scroll_y: word;
  scroll_x, rom_bank, slapfight_status_state: byte;
  ena_irq, sound_nmi: boolean;
  rom: array [0 .. 1, 0 .. $3FFF] of byte;

procedure update_video_sf_hw;
var
  f, x, y, color, nchar: word;
  atrib: byte;
begin
  for f := 0 to $7FF do
  begin
    // Foreground
    if gfx[0].buffer[f] then
    begin
      x := f div 64;
      y := 63 - (f mod 64);
      atrib := memory[$F800 + f];
      color := atrib and $FC;
      nchar := (memory[$F000 + f] + (atrib and 3) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color, 1, 0);
      gfx[0].buffer[f] := false;
    end;
    // Background
    if gfx[1].buffer[f] then
    begin
      x := f div 64;
      y := 63 - (f mod 64);
      atrib := memory[$D800 + f];
      color := atrib and $F0;
      nchar := (memory[$D000 + f] + (atrib and $F) shl 8);
      put_gfx(x * 8, y * 8, nchar, color, 2, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x + 17, 736 - scroll_y);
  for f := 0 to $1FF do
  begin
    atrib := memory[$E002 + (f * 4)];
    nchar := memory[$E000 + (f * 4)] + ((atrib and $C0) shl 2);
    color := (atrib and $1E) shl 3;
    x := memory[$E003 + (f * 4)] - 16;
    y := 797 - (memory[$E001 + (f * 4)] + ((atrib and 1) shl 8));
    put_gfx_sprite(nchar, color, false, false, 2);
    update_gfx_sprite(x, y, 3, 2);
  end;
  update_region(16, 224, 239, 280, 1, 0, 0, 240, 239, 3);
  update_final_piece(0, 0, 239, 280, 3);
end;

procedure events_sf_hw;
begin
  if event.arcade then
  begin
    // P1 & P2
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // System
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure sf_hw_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to 269 do begin
    events_sf_hw;
    if f=240 then begin
      if ena_irq then z80_0.change_irq(ASSERT_LINE);
      update_video_sf_hw;
    end;
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
    //Sound CPU
    z80_1.run(frame_snd);
    frame_snd:=frame_snd+z80_1.tframes-z80_1.contador;
    //MCU CPU
    taito_68705_0.run;
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

// Slap Fight
function sf_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $E7FF, $F000 .. $FFFF:
      sf_getbyte := memory[direccion];
    $8000 .. $BFFF:
      sf_getbyte := rom[rom_bank, direccion and $3FFF];
    $E803:
      sf_getbyte := taito_68705_0.read;
  end;
end;

procedure sf_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $CFFF, $E000 .. $E7FF:
      memory[direccion] := valor;
    $D000 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $E800:
      scroll_y := (scroll_y and $FF00) or valor;
    $E801:
      scroll_y := (scroll_y and $FF) or (valor shl 8);
    $E802:
      scroll_x := valor;
    $E803:
      taito_68705_0.write(valor);
    $F000 .. $FFFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function sf_inbyte(puerto: word): byte;
const
  states: array [0 .. 2] of byte = ($C7, $55, 0);
begin
  if (puerto and $FF) = 0 then
  begin
    sf_inbyte := (states[slapfight_status_state] and $F9) or (byte(not(taito_68705_0.main_sent)) shl 1) or (byte(not(taito_68705_0.mcu_sent)) shl 2);
    slapfight_status_state := (slapfight_status_state + 1) and 3;
  end;
end;

procedure sf_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      z80_1.change_reset(ASSERT_LINE);
    1:
      begin
        z80_1.change_reset(CLEAR_LINE);
        sound_nmi := false;
      end;
    2, 3:
      ; // flip screen
    6:
      begin
        ena_irq := false;
        z80_0.change_irq(CLEAR_LINE);
      end;
    7:
      ena_irq := true;
    8:
      rom_bank := 0;
    9:
      rom_bank := 1;
  end;
end;

procedure sf_scroll_y(pos, valor: byte);
begin
  case pos of
    0:
      scroll_y := (scroll_y and $FF00) or valor;
    1:
      scroll_y := (scroll_y and $FF) or (valor shl 8);
  end;
end;

// Sound
function snd_sf_hw_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $D000 .. $FFFF:
      snd_sf_hw_getbyte := mem_snd[direccion];
    $A081:
      snd_sf_hw_getbyte := ay8910_0.read;
    $A091:
      snd_sf_hw_getbyte := ay8910_1.read;
    $C800 .. $CFFF:
      snd_sf_hw_getbyte := memory[direccion];
  end;
end;

procedure snd_sf_hw_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ;
    $A080:
      ay8910_0.Control(valor);
    $A082:
      ay8910_0.write(valor);
    $A090:
      ay8910_1.Control(valor);
    $A092:
      ay8910_1.write(valor);
    $A0E0 .. $A0EF:
      sound_nmi := (direccion and 1) = 0;
    $C800 .. $CFFF:
      memory[direccion] := valor;
    $D000 .. $FFFF:
      mem_snd[direccion] := valor;
  end;
end;

function ay8910_porta_0: byte;
begin
  ay8910_porta_0 := marcade.in0;
end;

function ay8910_portb_0: byte;
begin
  ay8910_portb_0 := marcade.in1;
end;

function ay8910_porta_1: byte;
begin
  ay8910_porta_1 := marcade.dswa;
end;

function ay8910_portb_1: byte;
begin
  ay8910_portb_1 := marcade.dswb;
end;

procedure sf_hw_sound_update;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

procedure sf_sound_nmi;
begin
  if sound_nmi then
    z80_1.change_nmi(PULSE_LINE);
end;

// Main
procedure reset_sf_hw;
begin
  z80_0.reset;
  z80_1.reset;
  z80_1.change_reset(ASSERT_LINE);
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  taito_68705_0.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  ena_irq := false;
  sound_nmi := false;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  rom_bank := 0;
  slapfight_status_state := 0;
  scroll_y := 0;
  scroll_x := 0;
end;

function start_slapfight: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2, bit3: byte;
  memory_temp: array [0 .. $1FFFF] of byte;
const
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  procedure make_chars(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(2, 0, 8 * 8, 0 * 8 * 8, num * 8 * 8);
    convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, false, true);
  end;
  procedure make_tiles(num: word);
  begin
    init_gfx(1, 8, 8, num);
    gfx_set_desc_data(4, 0, 8 * 8, num * 8 * 8 * 0, num * 8 * 8 * 1, num * 8 * 8 * 2, num * 8 * 8 * 3);
    convert_gfx(1, 0, @memory_temp, @ps_x, @pc_y, false, true);
  end;
  procedure make_sprites(num: word);
  begin
    init_gfx(2, 16, 16, num);
    gfx[2].trans[0] := true;
    gfx_set_desc_data(4, 0, 32 * 8, num * 32 * 8 * 0, num * 32 * 8 * 1, num * 32 * 8 * 2, num * 32 * 8 * 3);
    convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  end;

begin
  start_slapfight := false;
  machine_calls.reset := reset_sf_hw;
  machine_calls.fps_max := 36000000 / 6 / 388 / 270;
  machine_calls.general_loop := sf_hw_loop;
  start_audio(false);
  screen_init(1, 256, 512, true);
  screen_init(2, 256, 512);
  screen_mod_scroll(2, 256, 256, 255, 512, 512, 511);
  screen_init(3, 256, 512, false, true);
  start_video(239, 280);
  // Main CPU
  z80_0 := cpu_z80.create(6000000, 270);
  z80_0.change_ram_calls(sf_getbyte, sf_putbyte);
  z80_0.change_io_calls(sf_inbyte, sf_outbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(3000000, 270);
  z80_1.change_ram_calls(snd_sf_hw_getbyte, snd_sf_hw_putbyte);
  z80_1.init_sound(sf_hw_sound_update);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(1500000, AY8910);
  ay8910_0.change_io_calls(ay8910_porta_0, ay8910_portb_0, nil, nil);
  ay8910_1 := ay8910_chip.create(1500000, AY8910);
  ay8910_1.change_io_calls(ay8910_porta_1, ay8910_portb_1, nil, nil);
  case main_vars.machine_type of
    98:
      begin // Tiger Heli
        // SND CPU
        timers.init(z80_1.numero_cpu, 3000000 / 360, sf_sound_nmi, nil, true);
        if not(roms_load(@mem_snd, tigerh_snd)) then
          exit;
        // MCU CPU
        taito_68705_0 := taito_68705p.create(3000000, 270, TIGER_HELI);
        if not(roms_load(taito_68705_0.get_rom_addr, tigerh_mcu)) then
          exit;
        // cargar roms
        if not(roms_load(@memory, tigerh_rom)) then
          exit;
        copymemory(@rom[0, 0], @memory[$8000], $4000);
        copymemory(@rom[1, 0], @memory[$8000], $4000);
        // convertir chars
        if not(roms_load(@memory_temp, tigerh_char)) then
          exit;
        make_chars($400);
        // convertir tiles
        if not(roms_load(@memory_temp, tigerh_tiles)) then
          exit;
        make_tiles($800);
        // convertir sprites
        if not(roms_load(@memory_temp, tigerh_sprites)) then
          exit;
        make_sprites($200);
        // Dip
        marcade.dswa := $6F;
        marcade.dswa_val2 := @tigerh_dip_a;
        marcade.dswb := $EB;
        marcade.dswb_val2 := @tigerh_dip_b;
        // Poner colores
        if not(roms_load(@memory_temp, tigerh_pal)) then
          exit;
      end;
    99:
      begin // Slap Fight
        // SND CPU
        timers.init(z80_1.numero_cpu, 3000000 / 180, sf_sound_nmi, nil, true);
        if not(roms_load(@mem_snd, sf_snd)) then
          exit;
        // MCU CPU
        taito_68705_0 := taito_68705p.create(3000000, 270);
        taito_68705_0.misc_call := sf_scroll_y;
        if not(roms_load(taito_68705_0.get_rom_addr, sf_mcu)) then
          exit;
        // cargar roms
        if not(roms_load(@memory_temp, sf_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $8000);
        copymemory(@rom[0, 0], @memory_temp[$8000], $4000);
        copymemory(@rom[1, 0], @memory_temp[$C000], $4000);
        // convertir chars
        if not(roms_load(@memory_temp, sf_char)) then
          exit;
        make_chars($400);
        // convertir tiles
        if not(roms_load(@memory_temp, sf_tiles)) then
          exit;
        make_tiles($1000);
        // convertir sprites
        if not(roms_load(@memory_temp, sf_sprites)) then
          exit;
        make_sprites($400);
        // Dip
        marcade.dswa := $7F;
        marcade.dswa_val2 := @sf_dip_a;
        marcade.dswb := $FF;
        marcade.dswb_val2 := @sf_dip_b;
        // Poner colores
        if not(roms_load(@memory_temp, sf_pal)) then
          exit;
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
  start_slapfight := true;
end;

end.
