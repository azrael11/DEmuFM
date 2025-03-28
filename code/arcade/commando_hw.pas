unit commando_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ym_2203,
  gfx_engine,
  misc_functions,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_commando: boolean;

implementation

const
  commando_rom: array [0 .. 1] of tipo_roms = ((n: 'cm04.9m'; l: $8000; p: 0; crc: $8438B694), (n: 'cm03.8m'; l: $4000; p: $8000; crc: $35486542));
  commando_snd_rom: tipo_roms = (n: 'cm02.9f'; l: $4000; p: 0; crc: $F9CC4A74);
  commando_pal: array [0 .. 2] of tipo_roms = ((n: 'vtb1.1d'; l: $100; p: 0; crc: $3ABA15A1), (n: 'vtb2.2d'; l: $100; p: $100; crc: $88865754), (n: 'vtb3.3d'; l: $100; p: $200; crc: $4C14C3F6));
  commando_char: tipo_roms = (n: 'vt01.5d'; l: $4000; p: 0; crc: $505726E0);
  commando_sprites: array [0 .. 5] of tipo_roms = ((n: 'vt05.7e'; l: $4000; p: 0; crc: $79F16E3D), (n: 'vt06.8e'; l: $4000; p: $4000; crc: $26FEE521), (n: 'vt07.9e'; l: $4000; p: $8000;
    crc: $CA88BDFD), (n: 'vt08.7h'; l: $4000; p: $C000; crc: $2019C883), (n: 'vt09.8h'; l: $4000; p: $10000; crc: $98703982), (n: 'vt10.9h'; l: $4000; p: $14000; crc: $F069D2F8));
  commando_tiles: array [0 .. 5] of tipo_roms = ((n: 'vt11.5a'; l: $4000; p: 0; crc: $7B2E1B48), (n: 'vt12.6a'; l: $4000; p: $4000; crc: $81B417D3), (n: 'vt13.7a'; l: $4000; p: $8000; crc: $5612DBD2),
    (n: 'vt14.8a'; l: $4000; p: $C000; crc: $2B2DEE36), (n: 'vt15.9a'; l: $4000; p: $10000; crc: $DE70BABF), (n: 'vt16.10a'; l: $4000; p: $14000; crc: $14178237));
  // DIP
        commando_dip_a:array [0..4] of def_dip2=(
        (mask:$3;name:'Starting Area';number:4;val4:(3,1,2,0);name4:('0 (Forest 1)','2 (Desert 1)','4 (Forest 2)','6 (Desert 2)')),
        (mask:$c;name:'Lives';number:4;val4:(4,$c,8,0);name4:('2','3','4','5')),
        (mask:$30;name:'Coin B';number:4;val4:(0,$20,$10,$30);name4:('4C 1C','3C 1C','2C 1C','1C 1C')),
        (mask:$c0;name:'Coin A';number:4;val4:(0,$c0,$40,$80);name4:('2C 1C','1C 1C','1C 2C','1C 3C')),());
        commando_dip_b:array [0..5] of def_dip2=(
        (mask:$7;name:'Bonus Life';number:8;val8:(7,3,5,1,6,2,4,0);name8:('10K 50K+','10K 60K+','20K 60K+','20K 70K+','30K 70K+','30K 80K+','40K 100K+','None')),
        (mask:$8;name:'Demo Sounds';number:2;val2:(0,8);name2:('Off','On')),
        (mask:$10;name:'Difficulty';number:2;val2:($10,0);name2:('Normal','Difficult')),
        (mask:$20;name:'Flip Screen';number:2;val2:($20,0);name2:('On','Off')),
        (mask:$c0;name:'Cabinet';number:4;val4:(0,$40,$c0,$80);name4:('Upright','Upright Two Players','Cocktail','Invalid')),());

var
  memory_dec: array [0 .. $BFFF] of byte;
  scroll_x, scroll_y: word;
  sound_command: byte;

procedure update_video_commando;
var
  f, color, nchar, x, y: word;
  attr, bank: byte;
begin
  for f := $3FF downto 0 do
  begin
    // tiles
    if gfx[2].buffer[f] then
    begin
      x := f mod 32;
      y := 31 - (f div 32);
      attr := memory[$DC00 + f];
      nchar := memory[$D800 + f] + ((attr and $C0) shl 2);
      color := (attr and $F) shl 3;
      put_gfx_flip(x * 16, y * 16, nchar, color, 2, 2, (attr and $20) <> 0, (attr and $10) <> 0);
      gfx[2].buffer[f] := false;
    end;
    // Chars
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      attr := memory[f + $D400];
      color := (attr and $F) shl 2;
      nchar := memory[f + $D000] + ((attr and $C0) shl 2);
      put_gfx_trans_flip(x * 8, y * 8, nchar, color, 3, 0, (attr and $20) <> 0, (attr and $10) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 1, scroll_x, 256 - scroll_y);
  // sprites
  for f := $7F downto 0 do
  begin
    attr := buffer_sprites[1 + (f * 4)];
    bank := (attr and $C0) shr 6;
    if bank < 3 then
    begin
      nchar := buffer_sprites[f * 4] + (bank shl 8);
      color := attr and $30;
      x := buffer_sprites[2 + (f * 4)];
      y := 240 - (buffer_sprites[3 + (f * 4)] + ((attr and $1) shl 8));
      put_gfx_sprite(nchar, color, (attr and $8) <> 0, (attr and $4) <> 0, 1);
      update_gfx_sprite(x, y, 1, 1);
    end;
  end;
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 1);
  update_final_piece(16, 0, 224, 256, 1);
  copymemory(@buffer_sprites[0], @memory[$FE00], $200);
end;

procedure events_commando;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
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
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    // BUT
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
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
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
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
  end;
end;

procedure commando_loop;
var
  f: word;
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 261 do
      begin
        // main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // snd
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 245 then
        begin
      z80_0.change_irq_vector(HOLD_LINE,$d7);
          update_video_commando;
        end;
      end;
      events_commando;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function commando_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $BFFF:
      if z80_0.opcode then
        commando_getbyte := memory_dec[direccion]
      else
        commando_getbyte := memory[direccion];
    $C000:
      commando_getbyte := marcade.in0;
    $C001:
      commando_getbyte := marcade.in1;
    $C002:
      commando_getbyte := marcade.in2;
    $C003:
      commando_getbyte := marcade.dswa;
    $C004:
      commando_getbyte := marcade.dswb;
    $D000 .. $FFFF:
      commando_getbyte := memory[direccion];
  end;
end;

procedure commando_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C800:
      sound_command := valor;
    $C804:
      begin
        if (valor and $10) <> 0 then
          z80_1.change_reset(ASSERT_LINE)
        else
          z80_1.change_reset(CLEAR_LINE);
        main_screen.flip_main_screen := (valor and $80) <> 0;
      end;
    $C808:
      scroll_y := (scroll_y and $100) or valor;
    $C809:
      scroll_y := (scroll_y and $FF) or ((valor and $1) shl 8);
    $C80A:
      scroll_x := (scroll_x and $100) or valor;
    $C80B:
      scroll_x := (scroll_x and $FF) or ((valor and $1) shl 8);
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $E000 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function commando_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF:
      commando_snd_getbyte := mem_snd[direccion];
    $6000:
      commando_snd_getbyte := sound_command;
  end;
end;

procedure commando_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
    $8000:
      ym2203_0.Control(valor);
    $8001:
      ym2203_0.Write(valor);
    $8002:
      ym2203_1.Control(valor);
    $8003:
      ym2203_1.Write(valor);
  end;
end;

procedure commando_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
end;

procedure commando_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

// Main
procedure reset_commando;
begin
  z80_0.reset;
  z80_1.reset;
  ym2203_0.reset;
  ym2203_1.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  sound_command := 0;
end;

function start_commando: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $17FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  machine_calls.general_loop := commando_loop;
  machine_calls.reset := reset_commando;
  machine_calls.fps_max := 12000000 / 2 / 384 / 262;
  start_commando := false;
  start_audio(false);
  screen_init(1, 512, 512, false, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 256, 256, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3000000, 262);
  z80_0.change_ram_calls(commando_getbyte, commando_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(3000000, 262);
  z80_1.change_ram_calls(commando_snd_getbyte, commando_snd_putbyte);
  z80_1.init_sound(commando_sound_update);
  // IRQ Sound CPU
  timers.init(z80_1.numero_cpu, 3000000 / (4 * 60), commando_snd_irq, nil, true);
  // Sound Chips
ym2203_0:=ym2203_chip.create(1500000);
ym2203_1:=ym2203_chip.create(1500000);
  // cargar y desencriptar las ROMS
  if not(roms_load(@memory, commando_rom)) then
    exit;
  memory_dec[0] := memory[0];
  for f := 1 to $BFFF do
    memory_dec[f] := bitswap8(memory[f], 3, 2, 1, 4, 7, 6, 5, 0);
  // cargar ROMS sonido
  if not(roms_load(@mem_snd, commando_snd_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, commando_char)) then
    exit;
  init_gfx(0, 8, 8, 1024);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // convertir sprites
  if not(roms_load(@memory_temp, commando_sprites)) then
    exit;
  init_gfx(1, 16, 16, 768);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $C000 * 8 + 4, $C000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // tiles
  if not(roms_load(@memory_temp, commando_tiles)) then
    exit;
  init_gfx(2, 16, 16, 1024);
  gfx_set_desc_data(3, 0, 32 * 8, 0, $8000 * 8, $8000 * 8 * 2);
  convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // poner la paleta
  if not(roms_load(@memory_temp, commando_pal)) then
    exit;
  for f := 0 to 255 do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
  end;
  set_pal(colores, 256);
  // crear la tabla de colores
  for f := 0 to 63 do
  begin
    gfx[1].colores[f] := f + 128;
    gfx[0].colores[f] := f + 192;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $1F;
marcade.dswa_val2:=@commando_dip_a;
marcade.dswb_val2:=@commando_dip_b;
  // final
  reset_commando;
  start_commando := true;
end;

end.
