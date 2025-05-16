unit speedrumbler_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  ym_2203,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine;

function start_speedrumbler: boolean;

implementation

const
        speedr_rom:array[0..7] of tipo_roms=(
        (n:'rc04.14e';l:$8000;p:$0;crc:$a68ce89c),(n:'rc03.13e';l:$8000;p:$8000;crc:$87bda812),
        (n:'rc02.12e';l:$8000;p:$10000;crc:$d8609cca),(n:'rc01.11e';l:$8000;p:$18000;crc:$27ec4776),
        (n:'rc09.14f';l:$8000;p:$20000;crc:$2146101d),(n:'rc08.13f';l:$8000;p:$28000;crc:$838369a6),
        (n:'rc07.12f';l:$8000;p:$30000;crc:$de785076),(n:'rc06.11f';l:$8000;p:$38000;crc:$a70f4fd4));
        speedr_sound:tipo_roms=(n:'rc05.2f';l:$8000;p:0;crc:$0177cebe);
        speedr_char:tipo_roms=(n:'rc10.6g';l:$4000;p:0;crc:$adabe271);
        speedr_tiles:array[0..7] of tipo_roms=(
        (n:'rc11.11a';l:$8000;p:$0;crc:$5fa042ba),(n:'rc12.13a';l:$8000;p:$8000;crc:$a2db64af),
        (n:'rc13.14a';l:$8000;p:$10000;crc:$f1df5499),(n:'rc14.15a';l:$8000;p:$18000;crc:$b22b31b3),
        (n:'rc15.11c';l:$8000;p:$20000;crc:$ca3a3af3),(n:'rc16.13c';l:$8000;p:$28000;crc:$c49a4a11),
        (n:'rc17.14c';l:$8000;p:$30000;crc:$aa80aaab),(n:'rc18.15c';l:$8000;p:$38000;crc:$ce67868e));
        speedr_sprites:array[0..7] of tipo_roms=(
        (n:'rc20.15e';l:$8000;p:$0;crc:$3924c861),(n:'rc19.14e';l:$8000;p:$8000;crc:$ff8f9129),
        (n:'rc22.15f';l:$8000;p:$10000;crc:$ab64161c),(n:'rc21.14f';l:$8000;p:$18000;crc:$fd64bcd1),
        (n:'rc24.15h';l:$8000;p:$20000;crc:$c972af3e),(n:'rc23.14h';l:$8000;p:$28000;crc:$8c9abf57),
        (n:'rc26.15j';l:$8000;p:$30000;crc:$d4f1732f),(n:'rc25.14j';l:$8000;p:$38000;crc:$d2a4ea4f));
        speedr_prom:array[0..1] of tipo_roms=(
        (n:'63s141.12a';l:$100;p:$0;crc:$8421786f),(n:'63s141.13a';l:$100;p:$100;crc:$6048583f));
        //Dip
        speedr_dip_a:array [0..3] of def_dip2=(
        (mask:7;name:'Coin B';number:8;val8:(0,1,2,7,6,5,4,3);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 6C')),
        (mask:$38;name:'Coin A';number:8;val8:(0,8,$10,$38,$30,$28,$20,$18);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 6C')),
        (mask:$80;name:'Flip Screen';number:2;val2:($80,0);name2:('Off','On')),());
        speedr_dip_b:array [0..5] of def_dip2=(
        (mask:3;name:'Lives';number:4;val4:(3,2,1,0);name4:('3','4','5','7')),
        (mask:4;name:'Cabinet';number:2;val2:(0,4);name2:('Upright','Cocktail')),
        (mask:$18;name:'Bonus Life';number:4;val4:($18,$10,8,0);name4:('20K 70K+','30K 80K+','20K 80K','30K 80K')),
        (mask:$60;name:'Difficulty';number:4;val4:($40,$60,$20,0);name4:('Easy','Normal','Difficult','Very Difficult')),
        (mask:$80;name:'Allow Continue';number:2;val2:(0,$80);name2:('No','Yes')),());

var
  memory_rom: array [0 .. $3F, 0 .. $FFF] of byte;
  prom_bank: array [0 .. $1FF] of byte;
  memory_fg: array [0 .. $FFF] of byte;
  soundlatch: byte;
  scroll_x, scroll_y: word;

procedure update_video_speedr;
var
  x, y, f, color, nchar: word;
  atrib: byte;
begin
  // background
  for f := $0 to $FFF do
  begin
    atrib := memory[$2000 + (f * 2)];
    color := (atrib and $E0) shr 5;
    if (gfx[1].buffer[f] or buffer_color[color + $10]) then
    begin
      x := f mod 64;
      y := 63 - (f div 64);
      nchar := memory[$2001 + (f * 2)] + ((atrib and $7) shl 8);
      put_gfx_flip(x * 16, y * 16, nchar, (color shl 4) + $80, 2, 1, (atrib and 8) <> 0, false);
      if (atrib and $10) <> 0 then
        put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + $80, 3, 1, (atrib and 8) <> 0, false)
      else
        put_gfx_block_trans(x * 16, y * 16, 3, 16, 16);
      gfx[1].buffer[f] := false;
    end;
  end;
  // foreground
  for f := $0 to $7FF do
  begin
    atrib := memory_fg[f * 2];
    color := (atrib and $3C) shr 2;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := 63 - (f div 32);
      nchar := memory_fg[$1 + (f * 2)] + ((atrib and $3) shl 8);
      if (atrib and $40) <> 0 then
        put_gfx(x * 8, y * 8, nchar, (color shl 2) + $1C0, 1, 0)
      else
        put_gfx_trans(x * 8, y * 8, nchar, (color shl 2) + $1C0, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 4, scroll_x, 512 - scroll_y);
  // sprites
  for f := $7F downto 0 do
  begin
    atrib := buffer_sprites[(f shl 2) + 1];
    nchar := buffer_sprites[f shl 2] + ((atrib and $E0) shl 3);
    color := ((atrib and $1C) shl 2) + $100;
    x := buffer_sprites[$2 + (f shl 2)];
    y := 496 - (buffer_sprites[$3 + (f shl 2)] + ((atrib and $1) shl 8));
    put_gfx_sprite(nchar, color, (atrib and 2) <> 0, false, 2);
    update_gfx_sprite(x, y, 4, 2);
  end;
  scroll_x_y(3, 4, scroll_x, 512 - scroll_y);
  update_region(0, 0, 256, 512, 1, 0, 0, 256, 512, 4);
  // Actualiza buffer sprites
  copymemory(@buffer_sprites, @memory[$1E00], $200);
  // chars
  update_final_piece(8, 80, 240, 352, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_speedr;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
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
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
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
    // SYS
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
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

procedure speedr_loop;
var
  f:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
  for f:=0 to $ff do begin
    events_speedr;
    case f of
      0:m6809_0.change_firq(HOLD_LINE);
      248:begin
            update_video_speedr;
            m6809_0.change_irq(HOLD_LINE);
          end;
    end;
    //Main CPU
    m6809_0.run(frame_main);
    frame_main:=frame_main+m6809_0.tframes-m6809_0.contador;
    //Sound CPU
    z80_0.run(frame_snd);
    frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
  end;
  video_sync;
    end
    else
      pause_action;
  end;
end;

procedure change_color(pos: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[pos];
  color.r := pal4bit(tmp_color shr 4);
  color.g := pal4bit(tmp_color);
  tmp_color := buffer_paleta[$1 + pos];
  color.b := pal4bit(tmp_color shr 4);
  pos := pos shr 1;
  set_pal_color(color, pos);
  case pos of
    $80 .. $FF:
      buffer_color[((pos shr 4) and $7) + $10] := true;
    $1C0 .. $1FF:
      buffer_color[(pos shr 2) and $F] := true;
  end;
end;

procedure change_bank(valor: byte);
var
  f, bank: byte;
  pos1, pos2: word;
begin
  pos1 := valor and $F0;
  pos2 := $100 + ((valor and $F) shl 4);
  for f := 5 to $F do
  begin
    bank := ((prom_bank[f + pos1] and $03) shl 4) or (prom_bank[f + pos2] and $0F);
    copymemory(@memory[f * $1000], @memory_rom[bank, 0], $1000);
  end;
end;

function speedr_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $3FFF, $5000 .. $FFFF:
      speedr_getbyte := memory[direccion];
    $4008:
      speedr_getbyte := marcade.in0;
    $4009:
      speedr_getbyte := marcade.in1;
    $400A:
      speedr_getbyte := marcade.in2;
    $400B:
      speedr_getbyte := marcade.dswa;
    $400C:
      speedr_getbyte := marcade.dswb;
  end;
end;

procedure speedr_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      memory[direccion] := valor;
    $2000 .. $3FFF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $4009:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $4008:
      change_bank(valor);
    $400A:
      scroll_y := (scroll_y and $300) or valor;
    $400B:
      scroll_y := (scroll_y and $FF) or ((valor and 3) shl 8);
    $400C:
      scroll_x := (scroll_x and $300) or valor;
    $400D:
      scroll_x := (scroll_x and $FF) or ((valor and 3) shl 8);
    $400E:
      soundlatch := valor;
    $5000 .. $5FFF:
      if memory_fg[direccion and $FFF] <> valor then
      begin
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
        memory_fg[direccion and $FFF] := valor;
      end;
    $7000 .. $73FF:
      if buffer_paleta[direccion and $3FF] <> valor then
      begin
        buffer_paleta[direccion and $3FF] := valor;
        change_color(direccion and $3FE);
      end;
    $6000 .. $6FFF, $7400 .. $FFFF:
      ; // ROM
  end;
end;

function sound_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $C7FF:
      sound_getbyte := mem_snd[direccion];
    $E000:
      sound_getbyte := soundlatch;
  end;
end;

procedure sound_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $8000:
      ym2203_0.Control(valor);
    $8001:
      ym2203_0.Write(valor);
    $A000:
      ym2203_1.Control(valor);
    $A001:
      ym2203_1.Write(valor);
  end;
end;

procedure speedr_sound_update;
begin
  ym2203_0.update;
  ym2203_1.update;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

// Main
procedure reset_speedr;
begin
 //Poner el banco antes que el reset!!!
  change_bank(0);
  m6809_0.reset;
  z80_0.reset;
  ym2203_0.reset;
  ym2203_1.reset;
 frame_main:=m6809_0.tframes;
 frame_snd:=z80_0.tframes;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  soundlatch := 0;
  scroll_x := 0;
  scroll_y := 0;
end;

function start_speedrumbler: boolean;
var
  f: byte;
  memory_temp: array [0 .. $3FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 2 * 64 + 0, 2 * 64 + 1, 2 * 64 + 2, 2 * 64 + 3, 2 * 64 + 4, 2 * 64 + 5, 2 * 64 + 6, 2 * 64 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 33 * 8 + 0, 33 * 8 + 1, 33 * 8 + 2, 33 * 8 + 3);
  pt_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
begin
  start_speedrumbler := false;
  machine_calls.general_loop := speedr_loop;
  machine_calls.reset := reset_speedr;
  start_audio(false);
  // Background
  screen_init(1, 256, 512, true);
  // Foreground
  screen_init(2, 1024, 1024);
  screen_mod_scroll(2, 1024, 256, 1023, 1024, 512, 1023);
  screen_init(3, 1024, 1024, true);
  screen_mod_scroll(3, 1024, 256, 1023, 1024, 512, 1023);
  screen_init(4, 256, 512, false, true); // Final
  start_video(240, 352);
  // Main CPU
  m6809_0 := cpu_m6809.Create(8000000, 256, TCPU_MC6809);
  m6809_0.change_ram_calls(speedr_getbyte, speedr_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(4000000, 256);
  z80_0.change_ram_calls(sound_getbyte, sound_putbyte);
  z80_0.init_sound(speedr_sound_update);
  // Sound Chip
  ym2203_0 := ym2203_chip.Create(4000000);
  ym2203_0.change_irq_calls(snd_irq);
  ym2203_1 := ym2203_chip.Create(4000000);
  // cargar roms
  if not(roms_load(@memory_temp, speedr_rom)) then
    exit;
  if not(roms_load(@prom_bank, speedr_prom)) then
    exit;
  // Pongo las ROMs en su banco
  for f := 0 to $3F do
    copymemory(@memory_rom[f, 0], @memory_temp[(f * $1000)], $1000);
  // Cargar Sound
  if not(roms_load(@mem_snd, speedr_sound)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, speedr_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // tiles
  if not(roms_load(@memory_temp, speedr_tiles)) then
    exit;
  init_gfx(1, 16, 16, $800);
  for f := 0 to 10 do
    gfx[1].trans[f] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $800 * 64 * 8 + 4, $800 * 64 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @pt_x, @pt_y, false, true);
  // sprites
  if not(roms_load(@memory_temp, speedr_sprites)) then
    exit;
  init_gfx(2, 16, 16, $800);
  gfx[2].trans[15] := true;
  gfx_set_desc_data(4, 0, 32 * 8, $1800 * 32 * 8, $1000 * 32 * 8, $800 * 32 * 8, $0 * 32 * 8);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // Dip
  marcade.dswa := $FF;
  marcade.dswb := $73;
marcade.dswa_val2:=@speedr_dip_a;
marcade.dswb_val2:=@speedr_dip_b;
  // final
  start_speedrumbler := true;
end;

end.
