unit mariobros_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  samples,
  rom_engine,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_mario: boolean;

implementation

const
  mario_rom: array [0 .. 3] of tipo_roms = ((n: 'tma1-c-7f_f.7f'; l: $2000; p: 0; crc: $C0C6E014), (n: 'tma1-c-7e_f.7e'; l: $2000; p: $2000; crc: $94FB60D6), (n: 'tma1-c-7d_f.7d'; l: $2000; p: $4000;
    crc: $DCCEB6C1), (n: 'tma1-c-7c_f.7c'; l: $1000; p: $F000; crc: $4A63D96B));
  mario_pal: tipo_roms = (n: 'tma1-c-4p_1.4p'; l: $200; p: 0; crc: $8187D286);
  mario_char: array [0 .. 1] of tipo_roms = ((n: 'tma1-v-3f.3f'; l: $1000; p: 0; crc: $28B0C42C), (n: 'tma1-v-3j.3j'; l: $1000; p: $1000; crc: $0C8CC04D));
  mario_sprites: array [0 .. 5] of tipo_roms = ((n: 'tma1-v-7m.7m'; l: $1000; p: 0; crc: $22B7372E), (n: 'tma1-v-7n.7n'; l: $1000; p: $1000; crc: $4F3A1F47), (n: 'tma1-v-7p.7p'; l: $1000; p: $2000;
    crc: $56BE6CCD), (n: 'tma1-v-7s.7s'; l: $1000; p: $3000; crc: $56F1D613), (n: 'tma1-v-7t.7t'; l: $1000; p: $4000; crc: $641F0008), (n: 'tma1-v-7u.7u'; l: $1000; p: $5000; crc: $7BAF5309));
  mario_samples: array [0 .. 28] of tipo_nombre_samples = ((nombre: 'mario_run.wav'; restart: true), (nombre: 'luigi_run.wav'; restart: true), (nombre: 'skid.wav'; restart: true),
    (nombre: 'bite_death.wav'), (nombre: 'death.wav'), (nombre: 'tune1.wav'; restart: true), (nombre: 'tune2.wav'; restart: true), (nombre: 'tune3.wav'; restart: true), (nombre: 'tune4.wav';
    restart: true), (nombre: 'tune5.wav'; restart: true), (nombre: 'tune6.wav'; restart: true), (nombre: 'tune7.wav'), (nombre: 'tune8.wav'; restart: true), (nombre: 'tune9.wav'; restart: true),
    (nombre: 'tune10.wav'; restart: true), (nombre: 'tune11.wav'; restart: true), (nombre: 'tune12.wav'; restart: true), (nombre: 'tune13.wav'; restart: true), (nombre: 'tune14.wav'; restart: true),
    (nombre: 'tune15.wav'; restart: true), (nombre: 'tune16.wav'; restart: true), (nombre: 'tune17.wav'), (nombre: 'tune18.wav'), (nombre: 'tune19.wav'), (nombre: 'coin.wav'),
    (nombre: 'insert_coin.wav'), (nombre: 'turtle.wav'), (nombre: 'crab.wav'), (nombre: 'fly.wav'));
  // Dip
        mario_dip_a:array [0..4] of def_dip2=(
        (mask:3;name:'Lives';number:4;val4:(0,1,2,3);name4:('3','4','5','6')),
        (mask:$c;name:'Coinage';number:4;val4:(4,0,8,$c);name4:('2C 1C','1C 1C','1C 2C','1C 3C')),
        (mask:$30;name:'Bonus Life';number:4;val4:(0,$10,$20,$30);name4:('20K','30K','40K','None')),
        (mask:$c0;name:'Difficulty';number:4;val4:(0,$80,$40,$c0);name4:('Easy','Medium','Hard','Hardest')),());
var
  haz_nmi: boolean;
  gfx_bank, palette_bank, scroll_y, death_val, skid_val: byte;

procedure update_video_mario;
var
  atrib: byte;
  f, x, y, color, nchar: word;
begin
  // Poner chars
  for f := $3FF downto 0 do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f mod 32);
      y := 31 - (f div 32);
      atrib := memory[$7400 + f];
      nchar := atrib + (gfx_bank shl 8);
      color := ((atrib shr 2) and $38) or $40 or (palette_bank shl 7);
      put_gfx(x * 8, y * 8, nchar, color, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll__y(1, 2, scroll_y);
  for f := 0 to $7F do
  begin
    if memory[$7000 + (f * 4)] = 0 then
      continue;
    nchar := memory[$7002 + (f * 4)];
    atrib := memory[$7001 + (f * 4)];
    color := ((atrib and $F) + 16 * palette_bank) shl 3;
    x := 240 - (memory[$7003 + (f * 4)] - 8);
    y := memory[$7000 + (f * 4)] + $F9;
    put_gfx_sprite(nchar, color, (atrib and $80) = 0, (atrib and $40) = 0, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(0, 16, 256, 224, 2);
end;

procedure events_mario;
begin
  if main_vars.service1 then
    marcade.in0 := (marcade.in0 or $80)
  else
    marcade.in0 := (marcade.in0 and $7F);
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 or $10
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 or $10
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
  end;
end;

procedure mario_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
  for f:=0 to 263 do begin
    if f=240 then begin
      if haz_nmi then z80_0.change_nmi(PULSE_LINE);
      update_video_mario;
    end;
    z80_0.run(frame_main);
    frame_main:=frame_main+z80_0.tframes-z80_0.contador;
  end;
      events_mario;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function mario_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $77FF, $F000 .. $FFFF:
      mario_getbyte := memory[direccion];
    $7C00:
      mario_getbyte := marcade.in0;
    $7C80:
      mario_getbyte := marcade.in1;
    $7F80:
      mario_getbyte := marcade.dswa;
  end;
end;

procedure mario_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF, $F000 .. $FFFF:
      ; // ROM
    $6000 .. $73FF:
      memory[direccion] := valor;
    $7400 .. $77FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $7C00:
      start_sample(0);
    $7C80:
      start_sample(1);
    $7D00:
      scroll_y := valor + 17;
    $7D80, $7E87:
      ; // ??
    $7E00:
      begin
        case (valor and $F) of
          1:
            start_sample(5); // pow
          2:
            start_sample(6); // tune sale vida
          3:
            start_sample(7); // salto mario
          4:
            start_sample(8); // tirar al agua un bicho
          5:
            start_sample(9); // tortuga boca abajo
          6:
            start_sample(10); // sonido agua del bicho
          7:
            start_sample(11); // vida extra
          8:
            start_sample(12); // tune presentacion cangrejos
          9:
            start_sample(13); // tune comenzar la partida
          10:
            start_sample(14); // tune presentacion tortugas
          11:
            start_sample(15); // tune game over
          12:
            start_sample(16); // tune bonus perfecto
          13:
            start_sample(17); // lanzar el ultimo bicho
          14:
            start_sample(18); // tune en bonus
          15:
            start_sample(19); // tune coin cojido en bonus
        end;
        case (valor shr 4) of
          1:
            start_sample(20);
          2, 3:
            start_sample(21);
          4 .. 7:
            start_sample(22);
          8 .. $F:
            start_sample(23);
        end;
      end;
    $7E80:
      if gfx_bank <> (valor and 1) then
      begin
        gfx_bank := valor and 1;
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
    $7E82:
      main_screen.flip_main_screen := (valor and 1) = 0;
    $7E83:
      if palette_bank <> (valor and 1) then
      begin
        palette_bank := valor and 1;
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
    $7E84:
      haz_nmi := (valor and 1) <> 0;
    $7E85:
      if (valor and 1) <> 0 then
        copymemory(@memory[$7000], @memory[$6900], $400);
    $7F00 .. $7F07:
      case (direccion and 7) of
        0:
          begin // cuando pasa de 0 a 1 mordisco, cuando pasa de 1 a 0 muerte
            if ((death_val = 0) and ((valor and 1) = 1)) then
              start_sample(3);
            if ((death_val = 1) and ((valor and 1) = 0)) then
              start_sample(4);
            death_val := valor and 1;
          end;
        1:
          if valor <> 0 then
            start_sample(25); // get coin
        2:
          ; // NADA
        3:
          if valor <> 0 then
            start_sample(27); // crab sale
        4:
          if valor <> 0 then
            start_sample(26); // turtle sale
        5:
          if valor <> 0 then
            start_sample(28); // fly sale
        6:
          if valor <> 0 then
            start_sample(24); // coin sale
        7:
          begin // skid cuando pasa de 1 a 0
            if ((skid_val = 1) and ((valor and 1) = 0)) then
              start_sample(2);
            skid_val := valor and 1;
          end;
      end;
  end;
end;

procedure mario_sound_update;
begin
  samples_update;
end;

// Main
procedure reset_mario;
begin
  z80_0.reset;
 frame_main:=z80_0.tframes;
  reset_samples;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  haz_nmi := false;
  gfx_bank := 0;
  palette_bank := 0;
  scroll_y := 0;
  death_val := 0;
  skid_val := 0;
end;

procedure mario_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
  size: word;
begin
  open_qsnapshot_save('mario' + nombre);
  getmem(data, 20000);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[$6000], $1800);
  // MISC
  buffer[0] := byte(haz_nmi);
  buffer[1] := gfx_bank;
  buffer[2] := palette_bank;
  buffer[3] := scroll_y;
  buffer[4] := death_val;
  buffer[5] := skid_val;
  savedata_qsnapshot(@buffer[0], 6);
  freemem(data);
  close_qsnapshot;
end;

procedure mario_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
begin
  if not(open_qsnapshot_load('mario' + nombre)) then
    exit;
  getmem(data, 20000);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory[$6000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  haz_nmi := buffer[0] <> 0;
  gfx_bank := buffer[1];
  palette_bank := buffer[2];
  scroll_y := buffer[3];
  death_val := buffer[4];
  skid_val := buffer[5];
  freemem(data);
  close_qsnapshot;
  fillchar(gfx[0].buffer[0], $400, 1);
end;

function start_mario: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $5FFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
  pc_y: array [0 .. 7] of dword = (7 * 8, 6 * 8, 5 * 8, 4 * 8, 3 * 8, 2 * 8, 1 * 8, 0 * 8);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 256 * 16 * 8 + 0, 256 * 16 * 8 + 1, 256 * 16 * 8 + 2, 256 * 16 * 8 + 3, 256 * 16 * 8 + 4, 256 * 16 * 8 + 5, 256 * 16 * 8 + 6,
    256 * 16 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  machine_calls.general_loop := mario_loop;
  machine_calls.reset := reset_mario;
  machine_calls.save_qsnap := mario_qsave;
  machine_calls.load_qsnap := mario_qload;
  machine_calls.fps_max := 59.185606;
  start_mario := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 264);
  z80_0.change_ram_calls(mario_getbyte, mario_putbyte);
  // cargar roms
  if not(roms_load(@memory, mario_rom)) then
    exit;
  // samples
  if load_samples(mario_samples) then
    z80_0.init_sound(mario_sound_update);
  // convertir chars
  if not(roms_load(@memory_temp, mario_char)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx_set_desc_data(2, 0, 8 * 8, 512 * 8 * 8, 0);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, mario_sprites)) then
    exit;
  init_gfx(1, 16, 16, 256);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(3, 0, 16 * 8, 2 * 256 * 16 * 16, 256 * 16 * 16, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, mario_pal)) then
    exit;
  for f := 0 to $1FF do
  begin
    bit0 := (memory_temp[f] shr 5) and 1;
    bit1 := (memory_temp[f] shr 6) and 1;
    bit2 := (memory_temp[f] shr 7) and 1;
    colores[f].r := not($21 * bit0 + $47 * bit1 + $97 * bit2);
    bit0 := (memory_temp[f] shr 2) and 1;
    bit1 := (memory_temp[f] shr 3) and 1;
    bit2 := (memory_temp[f] shr 4) and 1;
    colores[f].g := not($21 * bit0 + $47 * bit1 + $97 * bit2);
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    colores[f].b := not($55 * bit0 + $AA * bit1);
  end;
  set_pal(colores, $200);
  // DIP
  marcade.dswa := 0;
marcade.dswa_val2:=@mario_dip_a;
  // final
  reset_mario;
  start_mario := true;
end;

end.
