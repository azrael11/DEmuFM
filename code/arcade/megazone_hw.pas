unit megazone_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  konami_decrypt,
  ay_8910,
  mcs48,
  dac;

function start_megazone: boolean;

implementation

const
  megazone_rom: array [0 .. 4] of tipo_roms = ((n: '319_l07.11h'; l: $2000; p: $6000; crc: $73B616CA), (n: '319_l06.9h'; l: $2000; p: $8000; crc: $0CED03F9), (n: '319_l05.8h'; l: $2000; p: $A000; crc: $9DC3B5A1), (n: '319_l04.7h'; l: $2000; p: $C000; crc: $785B983D),
    (n: '319_l03.6h'; l: $2000; p: $E000; crc: $A5318686));
  megazone_snd: tipo_roms = (n: '319e02.6d'; l: $2000; p: 0; crc: $D5D45EDB);
  megazone_snd_sub: tipo_roms = (n: '319e01.3a'; l: $1000; p: 0; crc: $ED5725A0);
  megazone_char: array [0 .. 1] of tipo_roms = ((n: '319_g12.8c'; l: $2000; p: 0; crc: $07B8B24B), (n: '319_g13.10c'; l: $2000; p: $2000; crc: $3D8F3743));
  megazone_sprites: array [0 .. 3] of tipo_roms = ((n: '319e11.3e'; l: $2000; p: 0; crc: $965A7FF6), (n: '319e09.2e'; l: $2000; p: $2000; crc: $5EAA7F3E), (n: '319e10.3d'; l: $2000; p: $4000; crc: $7BB1AEEE), (n: '319e08.2d'; l: $2000; p: $6000; crc: $6ADD71B1));
  megazone_pal: array [0 .. 2] of tipo_roms = ((n: '319b18.a16'; l: $20; p: 0; crc: $23CB02AF), (n: '319b16.c6'; l: $100; p: $20; crc: $5748E933), (n: '319b17.a11'; l: $100; p: $120; crc: $1FBFCE73));
  megazone_dip_a: array [0 .. 1] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), ());
  megazone_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '7')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: $18; name: 'Bonus Life'; number: 4;
    val4: ($18, $10, 8, 0); name4: ('20K 70K 70K+', '20K 80K 80K+', '30K 90K 90K+', '30K 100K 100K+')), (mask: $60; name: 'Difficulty'; number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());

var
  mem_opcodes: array [0 .. $BFFF] of byte;
  irq_enable: boolean;
  i8039_status, sound_latch, scroll_x, scroll_y: byte;
  mem_snd_sub: array [0 .. $FFF] of byte;

procedure update_video_megazone;
var
  x, y, atrib, color: byte;
  f, nchar: word;
begin
  for f := $3FF downto 0 do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory[$2800 + f];
      color := (atrib and $F) shl 4;
      nchar := memory[$2000 + f] + ((atrib and $80) shl 1);
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $20) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(1, 3, scroll_x, scroll_y, 0, 32);
  // Sprites
  for f := $FF downto 0 do
  begin
    atrib := memory[$3000 + (f * 4)];
    nchar := memory[$3002 + (f * 4)];
    color := (atrib and $F) shl 4;
    x := memory[$3001 + (f * 4)];
    y := memory[$3003 + (f * 4)];
    put_gfx_sprite_mask(nchar, color, (atrib and $80) <> 0, (atrib and $40) = 0, 1, 0, $F);
    update_gfx_sprite(x, y + 32, 3, 1);
  end;
  // Parte de arriba
  for x := 31 downto 0 do
  begin
    f := (31 - x) * 32;
    for y := 0 to 5 do
    begin
      if gfx[1].buffer[f] then
      begin
        atrib := memory[$2C00 + f];
        color := (atrib and $F) shl 4;
        nchar := memory[$2400 + f] + ((atrib and $80) shl 1);
        put_gfx_flip(x * 8, y * 8, nchar, color, 2, 0, (atrib and $20) <> 0, (atrib and $40) <> 0);
        gfx[1].buffer[f] := false;
      end;
      f := f + 1;
    end;
  end;
  update_region(0, 0, 256, 48, 2, 0, 0, 256, 48, 3);
  update_final_piece(16, 0, 224, 288, 3);
end;

procedure events_megazone;
begin
  if event.arcade then
  begin
    // marcade.in1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // marcade.in2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // service
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
  end;
end;

procedure megazone_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        m6809_0.run(frame_main);
        frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
        // Sound CPU
        z80_0.run(frame_snd);
        frame_snd := frame_snd + z80_0.tframes - z80_0.contador;
        // snd sub
        mcs48_0.run(frame_snd2);
        frame_snd2 := frame_snd2 + mcs48_0.tframes - mcs48_0.contador;
        if f = 239 then
        begin
          if irq_enable then
            m6809_0.change_irq(HOLD_LINE);
          z80_0.change_irq(HOLD_LINE);
          update_video_megazone;
        end;
      end;
      events_megazone;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function megazone_getbyte(direccion: word): byte;
begin
  case direccion of
    $2000 .. $33FF, $3800 .. $3FFF:
      megazone_getbyte := memory[direccion];
    $4000 .. $5FFF:
      ;
    $6000 .. $FFFF:
      if m6809_0.opcode then
        megazone_getbyte := mem_opcodes[direccion - $6000]
      else
        megazone_getbyte := memory[direccion];
  end;
end;

procedure megazone_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. 1, $800:
      ; // Coin counter + Watchdog
    5:
      main_screen.flip_main_screen := (valor and $1) <> 0;
    7:
      irq_enable := (valor and 1) <> 0;
    $1000:
      scroll_y := valor;
    $1800:
      scroll_x := 255 - valor;
    $2000 .. $23FF, $2800 .. $2BFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $2400 .. $27FF, $2C00 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $3000 .. $33FF, $3800 .. $3FFF:
      memory[direccion] := valor;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function megazone_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      megazone_snd_getbyte := mem_snd[direccion];
    $6000:
      megazone_snd_getbyte := marcade.in0;
    $6001:
      megazone_snd_getbyte := marcade.in1;
    $6002:
      megazone_snd_getbyte := marcade.in2;
    $8000:
      megazone_snd_getbyte := marcade.dswb;
    $8001:
      megazone_snd_getbyte := marcade.dswa;
    $E000 .. $E7FF:
      megazone_snd_getbyte := memory[(direccion and $7FF) + $3800];
  end;
end;

procedure megazone_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      ; // ROM
    $2000:
      mcs48_0.change_irq(ASSERT_LINE);
    $4000:
      sound_latch := valor;
    $A000, $C000, $C001:
      ; // NMI+Watch Dog
    $E000 .. $E7FF:
      memory[(direccion and $7FF) + $3800] := valor;
  end;
end;

function megazone_sound_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) < 3 then
    megazone_sound_inbyte := ay8910_0.read;
end;

procedure megazone_sound_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ay8910_0.Control(valor);
    2:
      ay8910_0.Write(valor);
  end;
end;

// I8039
function megazone_sound2_getbyte(direccion: word): byte;
begin
  if direccion < $1000 then
    megazone_sound2_getbyte := mem_snd_sub[direccion];
end;

function megazone_sound2_inport(puerto: word): byte;
begin
  if puerto < $100 then
    megazone_sound2_inport := sound_latch;
end;

procedure megazone_sound2_outport(puerto: word; valor: byte);
begin
  case puerto of
    MCS48_PORT_P1:
      dac_0.data8_w(valor);
    MCS48_PORT_P2:
      begin
        if (valor and $80) = 0 then
          mcs48_0.change_irq(CLEAR_LINE);
        i8039_status := (valor and $70) shr 4;
      end;
  end;
end;

function megazone_portar: byte;
var
  timer: byte;
begin
  timer := trunc((z80_0.totalt * (7159 / 12288)) / (1024 / 2)) and $F;
  megazone_portar := (timer shl 4) or i8039_status;
end;

procedure megazone_portbw(valor: byte); // filter RC
begin
end;

procedure megazone_sound_update;
begin
  ay8910_0.update;
  dac_0.update;
end;

// Main
procedure reset_megazone;
begin
  m6809_0.reset;
  z80_0.reset;
  mcs48_0.reset;
  frame_main := m6809_0.tframes;
  frame_snd := z80_0.tframes;
  frame_snd2 := mcs48_0.tframes;
  ay8910_0.reset;
  dac_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_enable := false;
  sound_latch := 0;
  scroll_x := 0;
  scroll_y := 0;
  i8039_status := 0;
end;

function start_megazone: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $FFFF] of byte;
  rweights, gweights: array [0 .. 3] of single;
  bweights: array [0 .. 2] of single;
const
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
begin
  start_megazone := false;
  machine_calls.general_loop := megazone_loop;
  machine_calls.reset := reset_megazone;
  machine_calls.fps_max := 60.60606060606;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 48);
  screen_init(3, 256, 288, false, true);
  screen_mod_sprites(3, 256, 512, 255, 511);
  start_video(224, 288);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, $100, TCPU_M6809);
  m6809_0.change_ram_calls(megazone_getbyte, megazone_putbyte);
  if not(roms_load(@memory, megazone_rom)) then
    exit;
  konami1_decode(@memory[$6000], @mem_opcodes[0], $C000);
  // Sound CPU
  z80_0 := cpu_z80.Create(18432000 div 6, $100);
  z80_0.change_ram_calls(megazone_snd_getbyte, megazone_snd_putbyte);
  z80_0.change_io_calls(megazone_sound_inbyte, megazone_sound_outbyte);
  z80_0.init_sound(megazone_sound_update);
  if not(roms_load(@mem_snd, megazone_snd)) then
    exit;
  // Sound CPU 2
  mcs48_0 := cpu_mcs48.Create(14318000 div 2, $100, I8039);
  mcs48_0.change_ram_calls(megazone_sound2_getbyte, nil);
  mcs48_0.change_io_calls(nil, megazone_sound2_outport, megazone_sound2_inport, nil);
  if not(roms_load(@mem_snd_sub, megazone_snd_sub)) then
    exit;
  // Sound Chip
  ay8910_0 := ay8910_chip.Create(14318000 div 8, AY8910, 0.5);
  ay8910_0.change_io_calls(megazone_portar, nil, nil, megazone_portbw);
  dac_0 := dac_chip.Create(1);
  // convertir chars
  if not(roms_load(@memory_temp, megazone_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, true, false);
  // sprites
  if not(roms_load(@memory_temp, megazone_sprites)) then
    exit;
  init_gfx(1, 16, 16, $100);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // paleta
  if not(roms_load(@memory_temp, megazone_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rweights, 1000, 0, 3, @resistances_rg, @gweights, 1000, 0, 2, @resistances_b, @bweights, 1000, 0);
  for f := 0 to $1F do
  begin
    // red component */
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rweights[0], bit0, bit1, bit2);
    // green component */
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@gweights[0], bit0, bit1, bit2);
    // blue component */
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights[0], bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$120 + f] and $F) or $10;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $5B;
  marcade.dswa_val2 := @megazone_dip_a;
  marcade.dswb_val2 := @megazone_dip_b;
  // final
  reset_megazone;
  start_megazone := true;
end;

end.
