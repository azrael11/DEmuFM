unit jailbreak_hw;

interface

uses
  WinApi.Windows,
  m6809,
  main_engine,
  controls_engine,
  sn_76496,
  vlm_5030,
  gfx_engine,
  timer_engine,
  rom_engine,
  pal_engine,
  konami_decrypt,
  sound_engine;

function start_jailbreak: boolean;

implementation

const
  jailbreak_rom: array [0 .. 1] of tipo_roms = ((n: '507p03.11d'; l: $4000; p: $8000; crc: $A0B88DFD), (n: '507p02.9d'; l: $4000; p: $C000; crc: $444B7D8E));
  jailbreak_char: array [0 .. 1] of tipo_roms = ((n: '507l08.4f'; l: $4000; p: 0; crc: $E3B7A226), (n: '507j09.5f'; l: $4000; p: $4000; crc: $504F0912));
  jailbreak_sprites: array [0 .. 3] of tipo_roms = ((n: '507j04.3e'; l: $4000; p: 0; crc: $0D269524), (n: '507j05.4e'; l: $4000; p: $4000; crc: $27D4F6F4), (n: '507j06.5e'; l: $4000; p: $8000; crc: $717485CB), (n: '507j07.3f'; l: $4000; p: $C000; crc: $E933086F));
  jailbreak_pal: array [0 .. 3] of tipo_roms = ((n: '507j10.1f'; l: $20; p: 0; crc: $F1909605), (n: '507j11.2f'; l: $20; p: $20; crc: $F70BB122), (n: '507j13.7f'; l: $100; p: $40; crc: $D4FE5C97), (n: '507j12.6f'; l: $100; p: $140; crc: $0266C7DB));
  jailbreak_vlm: tipo_roms = (n: '507l01.8c'; l: $4000; p: $0; crc: $0C8A3605);
  // Dip
  jailbreak_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8; dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F;
    dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7; dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B; dip_name: '1C 5C'), (dip_val: $A;
    dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 15; dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $40;
    dip_name: '3C 2C'), (dip_val: $10; dip_name: '4C 3C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $30; dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90; dip_name: '1C 7C'), ())), ());
  jailbreak_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '1'), (dip_val: $2; dip_name: '2'), (dip_val: $1; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $8; dip_name: '30K 70K+'), (dip_val: $0; dip_name: '40K 80K+'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $30; dip_name: 'Easy'), (dip_val: $20; dip_name: 'Normal'), (dip_val: $10; dip_name: 'Difficult'), (dip_val: $0; dip_name: 'Very Difficult'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  jailbreak_dip_c: array [0 .. 2] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2; dip: ((dip_val: $1; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $2; name: 'Upright Controls'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Single'), (dip_val: $0; dip_name: 'Dual'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  irq_ena, nmi_ena, scroll_dir: boolean;
  mem_opcodes: array [0 .. $7FFF] of byte;
  scroll_lineas: array [0 .. $1F] of word;

procedure update_video_jailbreak;
var
  y, atrib: byte;
  f, x, nchar, color: word;
begin
  for f := 0 to $7FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 64;
      y := f div 64;
      atrib := memory[$0 + f];
      nchar := memory[$800 + f] + ((atrib and $C0) shl 2);
      color := (atrib and $F) shl 4;
      put_gfx(x * 8, y * 8, nchar, color, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  if scroll_dir then
    scroll__y_part2(1, 2, 8, @scroll_lineas)
  else
    scroll__x_part2(1, 2, 8, @scroll_lineas);
  for f := 0 to $2F do
  begin
    atrib := memory[$1001 + (f * 4)];
    nchar := memory[$1000 + (f * 4)] + ((atrib and $40) shl 2);
    color := (atrib and $F) shl 4;
    x := memory[$1002 + (f * 4)] + ((atrib and $80) shl 1);
    y := memory[$1003 + (f * 4)];
    put_gfx_sprite_mask(nchar, color, (atrib and $10) <> 0, (atrib and $20) <> 0, 1, 0, $F);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(8, 16, 240, 224, 2);
end;

procedure events_jailbreak;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure jailbreak_loop;
var
  f:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to $ff do begin
    events_jailbreak;
    if f=240 then begin
      if irq_ena then m6809_0.change_irq(HOLD_LINE);
      update_video_jailbreak;
    end;
    m6809_0.run(frame_main);
    frame_main:=frame_main+m6809_0.tframes-m6809_0.contador;
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

function jailbreak_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $203F, $3000 .. $307F:
      jailbreak_getbyte := memory[direccion];
    $3100:
      jailbreak_getbyte := marcade.dswb;
    $3200:
      jailbreak_getbyte := marcade.dswc;
    $3300:
      jailbreak_getbyte := marcade.in2;
    $3301:
      jailbreak_getbyte := marcade.in0;
    $3302:
      jailbreak_getbyte := marcade.in1;
    $3303:
      jailbreak_getbyte := marcade.dswa;
    $6000:
      jailbreak_getbyte := vlm5030_0.get_bsy;
    $8000 .. $FFFF:
      if m6809_0.opcode then
        jailbreak_getbyte := mem_opcodes[direccion and $7FFF]
      else
        jailbreak_getbyte := memory[direccion];
  end;
end;

procedure jailbreak_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $7FF] := true;
        memory[direccion] := valor;
      end;
    $1000 .. $1FFF:
      memory[direccion] := valor;
    $2000 .. $201F:
      begin
        scroll_lineas[direccion and $1F] := (scroll_lineas[direccion and $1F] and $FF00) or valor;
        memory[direccion] := valor;
      end;
    $2020 .. $203F:
      begin
        scroll_lineas[direccion and $1F] := (scroll_lineas[direccion and $1F] and $00FF) or ((valor and 1) shl 8);
        memory[direccion] := valor;
      end;
    $2042:
      scroll_dir := (valor and 4) <> 0;
    $2044:
      begin
        nmi_ena := ((valor and 1) <> 0);
        irq_ena := ((valor and 2) <> 0);
        main_screen.flip_main_screen := (valor and 8) <> 0;
      end;
    $3100:
      sn_76496_0.Write(valor);
    $4000:
      begin
        vlm5030_0.set_st((valor shr 1) and 1);
        vlm5030_0.set_rst((valor shr 2) and 1);
      end;
    $5000:
      vlm5030_0.data_w(valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

procedure jailbreak_snd_nmi;
begin
  if nmi_ena then
    m6809_0.change_nmi(PULSE_LINE);
end;

procedure jailbreak_sound;
begin
  sn_76496_0.update;
  vlm5030_0.update;
end;

// Main
procedure reset_jailbreak;
begin
  m6809_0.reset;
  sn_76496_0.reset;
  vlm5030_0.reset;
 frame_main:=m6809_0.tframes;
 reset_game_general;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_ena := false;
  nmi_ena := false;
  scroll_dir := false;
end;

function start_jailbreak: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 32 * 8 + 0 * 4, 32 * 8 + 1 * 4, 32 * 8 + 2 * 4, 32 * 8 + 3 * 4, 32 * 8 + 4 * 4, 32 * 8 + 5 * 4, 32 * 8 + 6 * 4, 32 * 8 + 7 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 16 * 32, 17 * 32, 18 * 32, 19 * 32, 20 * 32, 21 * 32, 22 * 32, 23 * 32);
begin
  machine_calls.general_loop := jailbreak_loop;
  machine_calls.reset := reset_jailbreak;
  machine_calls.fps_max := 60.606060606060;
  start_jailbreak := false;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 512, 256, false, true);
  start_video(240, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, $100, TCPU_M6809);
  m6809_0.change_ram_calls(jailbreak_getbyte, jailbreak_putbyte);
  m6809_0.init_sound(jailbreak_sound);
  if not(roms_load(@memory, jailbreak_rom)) then
    exit;
  konami1_decode(@memory[$8000], @mem_opcodes[0], $8000);
  // mem_opcodes[$9a7c and $7fff]:=$20;  //inmune
  // mem_opcodes[$9aee and $7fff]:=$39;
  // mem_opcodes[$9b4b and $7fff]:=$20;
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(18432000 div 12);
  // cargar rom sonido
  vlm5030_0 := vlm5030_chip.Create(3579545, $2000, 2);
  if not(roms_load(@memory_temp, jailbreak_vlm)) then
    exit;
  copymemory(vlm5030_0.get_rom_addr, @memory_temp, $2000);
  // NMI sonido
  timers.init(m6809_0.numero_cpu, 1536000 / 480, jailbreak_snd_nmi, nil, true);
  // convertir chars
  if not(roms_load(@memory_temp, jailbreak_char)) then
    exit;
  init_gfx(0, 8, 8, 1024);
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, jailbreak_sprites)) then
    exit;
  init_gfx(1, 16, 16, 512);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // paleta
  if not(roms_load(@memory_temp, jailbreak_pal)) then
    exit;
  for f := 0 to $1F do
  begin
    colores[f].r := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
    colores[f].g := ((memory_temp[f] shr 4) shl 4) or (memory_temp[f] shr 4);
    colores[f].b := ((memory_temp[f + $20] and $F) shl 4) or (memory_temp[f + $20] and $F);
  end;
  set_pal(colores, 32);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$40 + f] and $F) + $10; // chars
    gfx[1].colores[f] := memory_temp[$140 + f] and $F; // sprites
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $19;
  marcade.dswc := $3;
  marcade.dswa_val := @jailbreak_dip_a;
  marcade.dswb_val := @jailbreak_dip_b;
  marcade.dswc_val := @jailbreak_dip_c;
  // final
  start_jailbreak := true;
end;

end.
