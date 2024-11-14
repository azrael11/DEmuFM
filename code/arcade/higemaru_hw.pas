unit higemaru_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine;

function start_pirateshiphigemaru: boolean;

implementation

const
  higemaru_rom: array [0 .. 3] of tipo_roms = ((n: 'hg4.p12'; l: $2000; p: 0; crc: $DC67A7F9), (n: 'hg5.m12'; l: $2000; p: $2000; crc: $F65A4B68), (n: 'hg6.p11'; l: $2000; p: $4000; crc: $5F5296AA),
    (n: 'hg7.m11'; l: $2000; p: $6000; crc: $DC5D455D));
  higemaru_pal: array [0 .. 2] of tipo_roms = ((n: 'hgb3.l6'; l: $20; p: 0; crc: $629CEBD8), (n: 'hgb5.m4'; l: $100; p: $20; crc: $DBAA4443), (n: 'hgb1.h7'; l: $100; p: $120; crc: $07C607CE));
  higemaru_char: tipo_roms = (n: 'hg3.m1'; l: $2000; p: 0; crc: $B37B88C8);
  higemaru_sprites: array [0 .. 1] of tipo_roms = ((n: 'hg1.c14'; l: $2000; p: 0; crc: $EF4C2F5D), (n: 'hg2.e14'; l: $2000; p: $2000; crc: $9133F804));
  // Dip
  higemaru_dip_a: array [0 .. 3] of def_dip = ((mask: $7; name: 'Coin A'; number: 8; dip: ((dip_val: $1; dip_name: '5C 1C'), (dip_val: $2; dip_name: '4C 1C'), (dip_val: $3;
    dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 5C'), (dip_val: $0;
    dip_name: 'Free Play'), (), (), (), (), (), (), (), ())), (mask: $38; name: 'Coin B'; number: 8; dip: ((dip_val: $8; dip_name: '5C 1C'), (dip_val: $10; dip_name: '4C 1C'), (dip_val: $18;
    dip_name: '3C 1C'), (dip_val: $20; dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28; dip_name: '1C 5C'), (dip_val: $0;
    dip_name: 'Free Play'), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4; dip: ((dip_val: $80; dip_name: '1'), (dip_val: $40; dip_name: '2'), (dip_val: $C0;
    dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  higemaru_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $1; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $E; name: 'Bonus Life'; number: 8; dip: ((dip_val: $E; dip_name: '10K 50K 50K+'), (dip_val: $C; dip_name: '10K 60K 60K+'), (dip_val: $A;
    dip_name: '20K 60K 60K+'), (dip_val: $8; dip_name: '20K 70K 70K+'), (dip_val: $6; dip_name: '30K 70K 70K+'), (dip_val: $4; dip_name: '30K 80K 80K+'), (dip_val: $2;
    dip_name: '40K 100K 100K+'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), ())), (mask: $10; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $10; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20; name: 'Demo Music'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

procedure update_video_higemaru;
var
  f, nchar: word;
  color, x, y, attr: byte;
begin
  for f := $3FF downto 0 do
  begin
    // Chars
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      attr := memory[f + $D400];
      color := (attr and $1F) shl 2;
      nchar := memory[f + $D000] + ((attr and $80) shl 1);
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (attr and $40) <> 0, (attr and $20) <> 0);
      if (attr and $80) = 0 then
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8)
      else
        put_gfx_trans_flip(x * 8, y * 8, nchar, color, 2, 0, (attr and $40) <> 0, (attr and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  // sprites
  for f := $17 downto 0 do
  begin
    attr := memory[$D884 + (f * 16)];
    nchar := memory[$D880 + (f * 16)] and $7F;
    color := (attr and $F) shl 4;
    x := memory[$D88C + (f * 16)];
    y := memory[$D888 + (f * 16)];
    put_gfx_sprite(nchar, color, (attr and $10) <> 0, (attr and $20) <> 0, 1);
    update_gfx_sprite(x, y, 3, 1);
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(0, 16, 256, 224, 3);
end;

procedure events_higemaru;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    // System
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
  end;
end;

procedure higemaru_loop;
var
  f: byte;
  frame_m: single;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // snd
        case f of
      239:begin
            z80_0.change_irq_vector(HOLD_LINE,$cf);
            update_video_higemaru;
          end;
      255:z80_0.change_irq_vector(HOLD_LINE,$d7);
    end;
  end;
      events_higemaru;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function higemaru_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF, $D000 .. $D7FF, $D880 .. $D9FF, $E000 .. $EFFF:
      higemaru_getbyte := memory[direccion];
    $C000:
      higemaru_getbyte := marcade.in0;
    $C001:
      higemaru_getbyte := marcade.in1;
    $C002:
      higemaru_getbyte := marcade.in2;
    $C003:
      higemaru_getbyte := marcade.dswa;
    $C004:
      higemaru_getbyte := marcade.dswb;
  end;
end;

procedure higemaru_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $C800:
      main_screen.flip_main_screen := (valor and $80) <> 0;
    $C801:
      ay8910_0.control(valor);
    $C802:
      ay8910_0.write(valor);
    $C803:
      ay8910_1.control(valor);
    $C804:
      ay8910_1.write(valor);
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $D880 .. $D9FF, $E000 .. $EFFF:
      memory[direccion] := valor;
  end;
end;

procedure higemaru_sound;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

// Main
procedure reset_higemaru;
begin
  z80_0.reset;
  ay8910_0.reset;
  ay8910_1.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
end;

function start_pirateshiphigemaru: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp: array [0 .. $3FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
begin
  start_pirateshiphigemaru := false;
  machine_calls.general_loop := higemaru_loop;
  machine_calls.reset := reset_higemaru;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(3000000, 256);
  z80_0.change_ram_calls(higemaru_getbyte, higemaru_putbyte);
  z80_0.init_sound(higemaru_sound);
  // Sound Chips
  ay8910_0 := ay8910_chip.create(1500000, AY8910, 0.5);
  ay8910_1 := ay8910_chip.create(1500000, AY8910, 0.5);
  // cargar ROMS
  if not(roms_load(@memory, higemaru_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, higemaru_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx[0].trans[15] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, higemaru_sprites)) then
    exit;
  init_gfx(1, 16, 16, $80);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $80 * 8 * 64 + 4, $80 * 8 * 64 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, higemaru_pal)) then
    exit;
  for f := 0 to $1F do
  begin
    colores[f].r := ($21 * ((memory_temp[f] shr 0) and 1)) + ($47 * ((memory_temp[f] shr 1) and 1)) + ($97 * ((memory_temp[f] shr 2) and 1));
    colores[f].g := ($21 * ((memory_temp[f] shr 3) and 1)) + ($47 * ((memory_temp[f] shr 4) and 1)) + ($97 * ((memory_temp[f] shr 5) and 1));
    colores[f].b := 0 + ($47 * ((memory_temp[f] shr 6) and 1)) + ($97 * ((memory_temp[f] shr 7) and 1));
  end;
  set_pal(colores, 32);
  // crear la tabla de colores
  for f := 0 to $7F do
    gfx[0].colores[f] := memory_temp[f + $20] and $F;
  for f := 0 to $FF do
    gfx[1].colores[f] := (memory_temp[f + $120] and $F) or $10;
  // Dip
  marcade.dswa := $FF;
  marcade.dswb := $FE;
  marcade.dswa_val := @higemaru_dip_a;
  marcade.dswb_val := @higemaru_dip_b;
  // final
  reset_higemaru;
  start_pirateshiphigemaru := true;
end;

end.
