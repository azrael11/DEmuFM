unit exedexes_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  sn_76496,
  ay_8910,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine;

function start_exedexes: boolean;

implementation

const
  exedexes_rom: array [0 .. 2] of tipo_roms = ((n: '11m_ee04.bin'; l: $4000; p: 0; crc: $44140DBD), (n: '10m_ee03.bin'; l: $4000; p: $4000; crc: $BF72CFBA), (n: '09m_ee02.bin'; l: $4000; p: $8000;
    crc: $7AD95E2F));
  exedexes_snd_rom: tipo_roms = (n: '11e_ee01.bin'; l: $4000; p: 0; crc: $73CDF3B2);
  exedexes_pal: array [0 .. 7] of tipo_roms = ((n: '02d_e-02.bin'; l: $100; p: 0; crc: $8D0D5935), (n: '03d_e-03.bin'; l: $100; p: $100; crc: $D3C17EFC), (n: '04d_e-04.bin'; l: $100; p: $200;
    crc: $58BA964C), (n: '06f_e-05.bin'; l: $100; p: $300; crc: $35A03579), (n: 'l04_e-10.bin'; l: $100; p: $400; crc: $1DFAD87A), (n: 'c04_e-07.bin'; l: $100; p: $500; crc: $850064E0),
    (n: 'l09_e-11.bin'; l: $100; p: $600; crc: $2BB68710), (n: 'l10_e-12.bin'; l: $100; p: $700; crc: $173184EF));
  exedexes_char: tipo_roms = (n: '05c_ee00.bin'; l: $2000; p: 0; crc: $CADB75BD);
  exedexes_sprites: array [0 .. 1] of tipo_roms = ((n: 'j11_ee10.bin'; l: $4000; p: 0; crc: $BC83E265), (n: 'j12_ee11.bin'; l: $4000; p: $4000; crc: $0E0F300D));
  exedexes_tiles1: tipo_roms = (n: 'h01_ee08.bin'; l: $4000; p: 0; crc: $96A65C1D);
  exedexes_tiles2: array [0 .. 1] of tipo_roms = ((n: 'a03_ee06.bin'; l: $4000; p: 0; crc: $6039BDD1), (n: 'a02_ee05.bin'; l: $4000; p: $4000; crc: $B32D8252));
  exedexes_tilesbg_pos: array [0 .. 1] of tipo_roms = ((n: 'c01_ee07.bin'; l: $4000; p: 0; crc: $3625A68D), (n: 'h04_ee09.bin'; l: $2000; p: $4000; crc: $6057C907));
        exedexes_dip_a:array [0..5] of def_dip2=(
        (mask:$3;name:'Difficulty';number:4;val4:(2,3,1,0);name4:('Easy','Normal','Hard','Hardest')),
        (mask:$c;name:'Lives';number:4;val4:(8,4,$c,0);name4:('1','2','3','5')),
        (mask:$10;name:'2 Players Game';number:2;val2:(0,$10);name2:('1 Credit','2 Credit')),
        (mask:$20;name:'Languaje';number:2;val2:(0,$20);name2:('English','Japanese')),
        (mask:$40;name:'Freeze';number:2;val2:($40,0);name2:('Off','On')),());
        exedexes_dip_b:array [0..4] of def_dip2=(
        (mask:$7;name:'Coin A';number:8;val8:(0,1,2,7,6,5,4,3);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$38;name:'Coin B';number:8;val8:(0,8,$10,$38,$30,$28,$20,$18);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$40;name:'Allow Continue';number:2;val2:(0,$40);name2:('No','Yes')),
        (mask:$80;name:'Demo Sounds';number:2;val2:(0,$80);name2:('Off','On')),());
var
  scroll_x, scroll_y, scroll_bg: word;
  sound_command: byte;
  sc2on, sc1on, objon, chon: boolean;

procedure update_video_exedexes;
var
  f, color, nchar, x, y: word;
  attr: byte;
  procedure draw_sprites(pri: byte);
  var
    f, color, nchar, x, y: word;
    atrib: byte;
  begin
    for f := 127 downto 0 do
    begin
      atrib := buffer_sprites[(f * 32) + 1];
      if ((atrib and $40) = pri) then
      begin
        nchar := buffer_sprites[f * 32];
        atrib := buffer_sprites[(f * 32) + 1];
        color := (atrib and $0F) shl 4;
        y := 240 - (buffer_sprites[(f * 32) + 3] - ((atrib and $80) shl 1));
        x := buffer_sprites[(f * 32) + 2];
        put_gfx_sprite(nchar, color, (atrib and $20) <> 0, (atrib and $10) <> 0, 3);
        update_gfx_sprite(x, y, 4, 3);
      end;
    end;
  end;

begin
  if sc2on then
    scroll__y(1, 4, 1792 - scroll_bg)
  else
    fill_full_screen(4, 0);
  if objon then
    draw_sprites($40);
  if sc1on then
    scroll_x_y(2, 4, scroll_x, 1792 - scroll_y);
  if objon then
    draw_sprites(0);
  if chon then
  begin // chars activos
    for f := $3FF downto 0 do
    begin
      // Chars
      if gfx[0].buffer[f] then
      begin
        x := f div 32;
        y := 31 - (f mod 32);
        attr := memory[f + $D400];
        color := (attr and $3F) shl 2;
        nchar := memory[f + $D000] + ((attr and $80) shl 1);
        put_gfx_mask(x * 8, y * 8, nchar, color, 3, 0, $CF, $FF);
        gfx[0].buffer[f] := false;
      end;
    end;
    update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 4);
  end;
  update_final_piece(16, 0, 224, 256, 4);
  copymemory(@buffer_sprites, @memory[$F000], $1000);
end;

procedure events_exedexes;
begin
  if event.arcade then
  begin
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
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
  end;
end;

procedure exedexes_hw_loop;
var
  f: byte;
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // sonido
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        case f of
          239:
            begin
              z80_0.im0 := $D7; // rst 10
              z80_0.change_irq(HOLD_LINE);
              update_video_exedexes;
            end;
          255:
            begin
              z80_0.im0 := $CF; // rst 8
              z80_0.change_irq(HOLD_LINE);
            end;
        end;
      end;
      events_exedexes;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function exedexes_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $BFFF, $D000 .. $D7FF, $E000 .. $FFFF:
      exedexes_getbyte := memory[direccion];
    $C000:
      exedexes_getbyte := marcade.in0;
    $C001:
      exedexes_getbyte := marcade.in1;
    $C002:
      exedexes_getbyte := marcade.in2;
    $C003:
      exedexes_getbyte := marcade.dswa;
    $C004:
      exedexes_getbyte := marcade.dswb;
  end;
end;

procedure exedexes_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C800:
      sound_command := valor;
    $C804:
      if chon <> ((valor and $80) <> 0) then
      begin
        chon := (valor and $80) <> 0;
        if chon then
          fillchar(gfx[0].buffer, $400, 1);
      end;
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $D800:
      scroll_y := (scroll_y and $700) or valor;
    $D801:
      scroll_y := (scroll_y and $FF) or ((valor and $7) shl 8);
    $D802:
      scroll_x := (scroll_x and $700) or valor;
    $D803:
      scroll_x := (scroll_x and $FF) or ((valor and $7) shl 8);
    $D804:
      scroll_bg := (scroll_bg and $700) or valor;
    $D805:
      scroll_bg := (scroll_bg and $FF) or ((valor and $7) shl 8);
    $D807:
      begin
        sc2on := (valor and $10) <> 0;
        sc1on := (valor and $20) <> 0;
        objon := (valor and $40) <> 0;
      end;
    $E000 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function exedexes_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $47FF:
      exedexes_snd_getbyte := mem_snd[direccion];
    $6000:
      exedexes_snd_getbyte := sound_command;
  end;
end;

procedure exedexes_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $47FF:
      mem_snd[direccion] := valor;
    $8000:
      ay8910_0.Control(valor);
    $8001:
      ay8910_0.Write(valor);
    $8002:
      sn_76496_0.Write(valor);
    $8003:
      sn_76496_1.Write(valor);
  end;
end;

procedure exedexes_snd_irq;
begin
  z80_1.change_irq(HOLD_LINE);
end;

procedure exedexes_sound;
begin
  ay8910_0.update;
  sn_76496_0.update;
  sn_76496_1.update;
end;

// Main
procedure reset_exedexes_hw;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  sc2on := true;
  sc1on := true;
  objon := true;
end;

function start_exedexes: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $7FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  pt_x: array [0 .. 31] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 64 * 8 + 0, 64 * 8 + 1, 64 * 8 + 2, 64 * 8 + 3, 65 * 8 + 0, 65 * 8 + 1, 65 * 8 + 2, 65 * 8 + 3, 128 * 8 + 0, 128 * 8 + 1,
    128 * 8 + 2, 128 * 8 + 3, 129 * 8 + 0, 129 * 8 + 1, 129 * 8 + 2, 129 * 8 + 3, 192 * 8 + 0, 192 * 8 + 1, 192 * 8 + 2, 192 * 8 + 3, 193 * 8 + 0, 193 * 8 + 1, 193 * 8 + 2, 193 * 8 + 3);
  pt_y: array [0 .. 31] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16, 16 * 16, 17 * 16, 18 * 16,
    19 * 16, 20 * 16, 21 * 16, 22 * 16, 23 * 16, 24 * 16, 25 * 16, 26 * 16, 27 * 16, 28 * 16, 29 * 16, 30 * 16, 31 * 16);
  procedure poner_bg;
  var
    f, pos, color: word;
    x, y, atrib, nchar: byte;
    flipx, flipy: boolean;
  begin
    for f := 0 to $FFF do
    begin
      x := f div 64;
      y := f mod 64;
      pos := ((y * 32 and $E0) shr 5) + ((x * 32 and $E0) shr 2) + ((y * 32 and $3F00) shr 1);
      atrib := memory_temp[pos + $4000];
      nchar := atrib and $3F;
      color := (memory_temp[pos + (8 * 8) + $4000]) shl 2;
      flipx := (atrib and $40) <> 0;
      flipy := (atrib and $80) <> 0;
      put_gfx_flip(x * 32, (63 - y) * 32, nchar, color, 1, 1, flipx, flipy);
    end;
  end;
  procedure poner_fg;
  var
    f, pos: word;
    x, y, nchar: byte;
  begin
    for f := 0 to $1FFF do
    begin
      x := f div 128;
      y := f mod 128;
      pos := ((y * 16 and $F0) shr 4) + (x * 16 and $F0) + (y * 16 and $700) + ((x * 16 and $700) shl 3);
      nchar := memory_temp[pos];
      put_gfx_trans(x * 16, (127 - y) * 16, nchar, 0, 2, 2);
    end;
  end;

begin
  start_exedexes := false;
  machine_calls.general_loop := exedexes_hw_loop;
  machine_calls.reset := reset_exedexes_hw;
  start_audio(false);
  screen_init(1, 2048, 2048);
  screen_mod_scroll(1, 2048, 256, 2047, 2048, 256, 2047);
  screen_init(2, 2048, 2048, true);
  screen_mod_scroll(2, 2048, 256, 2047, 2048, 256, 2047);
  screen_init(3, 256, 256, true);
  screen_init(4, 256, 512, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 256);
  z80_0.change_ram_calls(exedexes_getbyte, exedexes_putbyte);
  // Sound CPU
  z80_1 := cpu_z80.create(3000000, 256);
  z80_1.change_ram_calls(exedexes_snd_getbyte, exedexes_snd_putbyte);
  z80_1.init_sound(exedexes_sound);
//Sound Chips
AY8910_0:=ay8910_chip.create(1500000,AY8910,0.4);
sn_76496_0:=sn76496_chip.Create(3000000,1);
sn_76496_1:=sn76496_chip.Create(3000000,1);
  timers.init(z80_1.numero_cpu, 3000000 / (4 * 60), exedexes_snd_irq, nil, true);
  // cargar roms
  if not(roms_load(@memory, exedexes_rom)) then
    exit;
  // cargar ROMS sonido
  if not(roms_load(@mem_snd, exedexes_snd_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, exedexes_char)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // tiles 32x32
  if not(roms_load(@memory_temp, exedexes_tiles1)) then
    exit;
  init_gfx(1, 32, 32, 64);
  gfx_set_desc_data(2, 0, 256 * 8, 0, 4);
  convert_gfx(1, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // tiles 16x16
  if not(roms_load(@memory_temp, exedexes_tiles2)) then
    exit;
  init_gfx(2, 16, 16, 256);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(2, 0, @memory_temp, @ps_x, @pt_y, false, true);
  // convertir sprites
  if not(roms_load(@memory_temp, exedexes_sprites)) then
    exit;
  init_gfx(3, 16, 16, 256);
  gfx[3].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(3, 0, @memory_temp, @ps_x, @pt_y, false, true);
  // poner la paleta y clut
  if not(roms_load(@memory_temp, exedexes_pal)) then
    exit;
  for f := 0 to $FF do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
  end;
  set_pal(colores, 256);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := memory_temp[$300 + f] + $C0;
    gfx[1].colores[f] := memory_temp[$400 + f];
    gfx[2].colores[f] := memory_temp[$500 + f] + $40;
    gfx[3].colores[f] := memory_temp[$600 + f] + (memory_temp[$700 + f] shl 4) + $80;
  end;
  // El fondo es fijo, no cambia lo pongo despues de la paleta
  if not(roms_load(@memory_temp, exedexes_tilesbg_pos)) then
    exit;
  poner_bg;
  poner_fg;
  // DIP
  marcade.dswa := $DF;
marcade.dswa_val2:=@exedexes_dip_a;
  marcade.dswb := $FF;
marcade.dswb_val2:=@exedexes_dip_b;
  // final
  reset_exedexes_hw;
  start_exedexes := true;
end;

end.
