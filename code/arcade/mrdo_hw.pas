unit mrdo_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine;

function start_mrdo: boolean;

implementation

const
        mrdo_rom:array[0..3] of tipo_roms=(
        (n:'a4-01.bin';l:$2000;p:0;crc:$03dcfba2),(n:'c4-02.bin';l:$2000;p:$2000;crc:$0ecdd39c),
        (n:'e4-03.bin';l:$2000;p:$4000;crc:$358f5dc2),(n:'f4-04.bin';l:$2000;p:$6000;crc:$f4190cfc));
        mrdo_pal:array[0..2] of tipo_roms=(
        (n:'u02--2.bin';l:$20;p:0;crc:$238a65d7),(n:'t02--3.bin';l:$20;p:$20;crc:$ae263dc0),
        (n:'f10--1.bin';l:$20;p:$40;crc:$16ee4ca2));
        mrdo_char1:array[0..1] of tipo_roms=(
        (n:'s8-09.bin';l:$1000;p:0;crc:$aa80c5b6),(n:'u8-10.bin';l:$1000;p:$1000;crc:$d20ec85b));
        mrdo_char2:array[0..1] of tipo_roms=(
        (n:'r8-08.bin';l:$1000;p:0;crc:$dbdc9ffa),(n:'n8-07.bin';l:$1000;p:$1000;crc:$4b9973db));
        mrdo_sprites:array[0..1] of tipo_roms=(
        (n:'h5-05.bin';l:$1000;p:0;crc:$e1218cc5),(n:'k5-06.bin';l:$1000;p:$1000;crc:$b1f68b04));
        //Dip
        mrdo_dip_a:array [0..6] of def_dip2=(
        (mask:3;name:'Difficulty';number:4;val4:(3,2,1,0);name4:('Easy','Medium','Hard','Hardest')),
        (mask:4;name:'Rack Test';number:2;val2:(4,0);name2:('Off','On')),
        (mask:8;name:'Special';number:2;val2:(8,0);name2:('Easy','Hard')),
        (mask:$10;name:'Extra';number:2;val2:($10,0);name2:('Easy','Hard')),
        (mask:$20;name:'Cabinet';number:2;val2:(0,$20);name2:('Upright','Cocktail')),
        (mask:$c0;name:'Lives';number:4;val4:(0,$c0,$80,$40);name4:('2','3','4','5')),());
        mrdo_dip_b:array [0..2] of def_dip2=(
        (mask:$f0;name:'Coin A';number:16;val16:($60,$80,$a0,$70,$f0,$90,$e0,$d0,$c0,$b0,0,$10,$20,$30,$40,$50);name16:('4C 1C','3C 1C','2C 1C','3C 2C','1C 1C','2C 3C','1C 2C','1C 3C','1C 4C','1C 5C','Free Play','Invalid','Invalid','Invalid','Invalid','Invalid')),
        (mask:$f;name:'Coin B';number:16;val16:(6,8,$a,7,$f,9,$e,$d,$c,$b,0,1,2,3,4,5);name16:('4C 1C','3C 1C','2C 1C','3C 2C','1C 1C','2C 3C','1C 2C','1C 3C','1C 4C','1C 5C','Free Play','Invalid','Invalid','Invalid','Invalid','Invalid')),());

var
  scroll_x, scroll_y, prot: byte;

procedure update_video_mrdo;
var
  f, color, nchar: word;
  x, y, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    x := f div 32;
    y := 31 - (f mod 32);
    if gfx[1].buffer[f] then
    begin
      atrib := memory[$8000 + f];
      nchar := memory[$8400 + f] + (atrib and $80) shl 1;
      color := (atrib and $3F) shl 2;
      if (atrib and $40) <> 0 then
      begin
        put_gfx(x * 8, y * 8, nchar, color, 1, 1);
        put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
      end
      else
      begin
        put_gfx_block(x * 8, y * 8, 1, 8, 8, 0);
        put_gfx_trans(x * 8, y * 8, nchar, color, 4, 1);
      end;
      gfx[1].buffer[f] := false;
    end;
    atrib := memory[$8800 + f];
    if ((gfx[0].buffer[f]) or ((atrib and $40) <> 0)) then
    begin
      nchar := memory[$8C00 + f] + (atrib and $80) shl 1;
      color := (atrib and $3F) shl 2;
      if (atrib and $40) <> 0 then
      begin
        put_gfx(x * 8, y * 8, nchar, color, 2, 0);
        put_gfx_block_trans(x * 8, y * 8, 5, 8, 8);
      end
      else
      begin
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
        put_gfx_trans(x * 8, y * 8, nchar, color, 5, 0);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  // Es MUY IMPORTANTE este orden para poder pintar correctamente la pantalla!!!
  scroll_x_y(1, 3, scroll_x, scroll_y);
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  scroll_x_y(4, 3, scroll_x, scroll_y);
  update_region(0, 0, 256, 256, 5, 0, 0, 256, 256, 3);
  for f := $3F downto 0 do
  begin
    x := memory[(f * 4) + $9001];
    if (x <> 0) then
    begin
      nchar := memory[(f * 4) + $9000] and $7F;
      atrib := memory[(f * 4) + $9002];
      color := (atrib and $F) shl 2;
      y := 240 - memory[(f * 4) + $9003];
      put_gfx_sprite(nchar, color, (atrib and $20) <> 0, (atrib and $10) <> 0, 2);
      update_gfx_sprite(256 - x, y, 3, 2);
    end;
  end;
  update_final_piece(32, 8, 192, 240, 3);
end;

procedure events_mrdo;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
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
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
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

procedure mrdo_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
  for f:=0 to 261 do begin
      if f=224 then begin
          z80_0.change_irq(HOLD_LINE);
          update_video_mrdo;
      end;
      z80_0.run(frame_main);
      frame_main:=frame_main+z80_0.tframes-z80_0.contador;
  end;
      events_mrdo;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function mrdo_getbyte(direccion: word): byte;
var
  z80_0_reg: npreg_z80;
begin
  case direccion of
    0 .. $8FFF, $E000 .. $EFFF:
      mrdo_getbyte := memory[direccion];
    $9803:
      begin
        z80_0_reg := z80_0.get_internal_r;
        mrdo_getbyte := memory[z80_0_reg.hl.w];
      end;
    $A000:
      mrdo_getbyte := marcade.in0;
    $A001:
      mrdo_getbyte := marcade.in1;
    $A002:
      mrdo_getbyte := marcade.dswa;
    $A003:
      mrdo_getbyte := marcade.dswb;
  end;
end;

procedure mrdo_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $8800 .. $8FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $9000 .. $90FF, $E000 .. $EFFF:
      memory[direccion] := valor;
    $9800:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $9801:
      sn_76496_0.Write(valor);
    $9802:
      sn_76496_1.Write(valor);
    $F000 .. $F7FF:
      scroll_y := valor;
    $F800 .. $FFFF:
      scroll_x := valor;
  end;
end;

procedure mrdo_update_sound;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
end;

// Main
procedure reset_mrdo;
begin
  z80_0.reset;
 frame_main:=z80_0.tframes;
  sn_76496_0.reset;
  sn_76496_1.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  prot := $FF;
end;

function start_mrdo: boolean;
var
  memory_temp: array [0 .. $1FFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  ps_x: array [0 .. 15] of dword = (3, 2, 1, 0, 8 + 3, 8 + 2, 8 + 1, 8 + 0, 16 + 3, 16 + 2, 16 + 1, 16 + 0, 24 + 3, 24 + 2, 24 + 1, 24 + 0);
  ps_y: array [0 .. 15] of dword = (0 * 16, 2 * 16, 4 * 16, 6 * 16, 8 * 16, 10 * 16, 12 * 16, 14 * 16, 16 * 16, 18 * 16, 20 * 16, 22 * 16, 24 * 16, 26 * 16, 28 * 16, 30 * 16);
  procedure calc_paleta;
  var
    weight, pot: array [0 .. 15] of single;
    par: single;
    f, a1, a2, bits0, bits1: byte;
    colores: tpaleta;
  const
    R1 = 150;
    R2 = 120;
    R3 = 100;
    R4 = 75;
    PULL = 220;
    POTADJUST = 0.7; // diode voltage drop
  begin
    for f := $F downto 0 do
    begin
      par := 0;
      if (f and 1) <> 0 then
        par := par + (1.0 / R1);
      if (f and 2) <> 0 then
        par := par + (1.0 / R2);
      if (f and 4) <> 0 then
        par := par + (1.0 / R3);
      if (f and 8) <> 0 then
        par := par + (1.0 / R4);
      if (par <> 0) then
      begin
        par := 1 / par;
        pot[f] := PULL / (PULL + par) - POTADJUST;
      end
      else
        pot[f] := 0;
      weight[f] := $FF * pot[f] / pot[$F];
      if weight[f] < 0 then
        weight[f] := 0;
    end;
    for f := 0 to $FF do
    begin
      a1 := ((f shr 3) and $1C) + (f and 3) + $20;
      a2 := ((f shr 0) and $1C) + (f and 3);
      bits0 := (memory_temp[a1] shr 0) and 3;
      bits1 := (memory_temp[a2] shr 0) and 3;
      colores[f].r := trunc(weight[bits0 + (bits1 shl 2)]);
      bits0 := (memory_temp[a1] shr 2) and 3;
      bits1 := (memory_temp[a2] shr 2) and 3;
      colores[f].g := trunc(weight[bits0 + (bits1 shl 2)]);
      bits0 := (memory_temp[a1] shr 4) and 3;
      bits1 := (memory_temp[a2] shr 4) and 3;
      colores[f].b := trunc(weight[bits0 + (bits1 shl 2)]);
    end;
    set_pal(colores, $100);
    // CLUT sprites
    for f := 0 to $3F do
    begin
      bits0 := memory_temp[($40 + (f and $1F))];
      if (f and $20) <> 0 then
        bits0 := bits0 shr 4 // high 4 bits are for sprite color n + 8
      else
        bits0 := bits0 and $F; // low 4 bits are for sprite color n
      gfx[2].colores[f] := bits0 + ((bits0 and $C) shl 3);
    end;
  end;

begin
  machine_calls.general_loop := mrdo_loop;
  machine_calls.reset := reset_mrdo;
  machine_calls.fps_max := 59.94323742;
  start_mrdo := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  screen_init(4, 256, 256, true);
  screen_mod_scroll(4, 256, 256, 255, 256, 256, 255);
  screen_init(5, 256, 256, true);
  start_video(192, 240);
  // Main CPU
  z80_0 := cpu_z80.create(4100000, 262);
  z80_0.change_ram_calls(mrdo_getbyte, mrdo_putbyte);
  z80_0.init_sound(mrdo_update_sound);
  // Sound Chips
  sn_76496_0 := sn76496_chip.create(4100000);
  sn_76496_1 := sn76496_chip.create(4100000);
  // cargar roms
  if not(roms_load(@memory, mrdo_rom)) then
    exit;
  // convertir chars fg
  if not(roms_load(@memory_temp, mrdo_char1)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(2, 0, 8 * 8, 0, 512 * 8 * 8);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, true);
  // convertir chars bg
  if not(roms_load(@memory_temp, mrdo_char2)) then
    exit;
  init_gfx(1, 8, 8, 512);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(2, 0, 8 * 8, 0, 512 * 8 * 8);
  convert_gfx(1, 0, @memory_temp, @pc_x, @pc_y, false, true);
  // convertir sprites
  if not(roms_load(@memory_temp, mrdo_sprites)) then
    exit;
  init_gfx(2, 16, 16, 128);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(2, 0, 64 * 8, 4, 0);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // poner la paleta
  if not(roms_load(@memory_temp, mrdo_pal)) then
    exit;
  calc_paleta;
  // dip
  marcade.dswa := $DF;
  marcade.dswb := $FF;
marcade.dswa_val2:=@mrdo_dip_a;
marcade.dswb_val2:=@mrdo_dip_b;
  // final
  reset_mrdo;
  start_mrdo := true;
end;

end.
