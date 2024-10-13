unit rastan_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  msm5205,
  taitosnd,
  rom_engine,
  pal_engine,
  sound_engine;

function start_rastan: boolean;

implementation

const
  rastan_rom: array [0 .. 5] of tipo_roms = ((n: 'b04-38.19'; l: $10000; p: 0; crc: $1C91DBB1),
    (n: 'b04-37.7'; l: $10000; p: $1; crc: $ECF20BDD), (n: 'b04-40.20'; l: $10000; p: $20000;
    crc: $0930D4B3), (n: 'b04-39.8'; l: $10000; p: $20001; crc: $D95ADE5E), (n: 'b04-42.21';
    l: $10000; p: $40000; crc: $1857A7CB), (n: 'b04-43-1.9'; l: $10000; p: $40001; crc: $CA4702FF));
  rastan_char: array [0 .. 3] of tipo_roms = ((n: 'b04-01.40'; l: $20000; p: 0; crc: $CD30DE19),
    (n: 'b04-03.39'; l: $20000; p: $20000; crc: $AB67E064), (n: 'b04-02.67'; l: $20000; p: $40000;
    crc: $54040FEC), (n: 'b04-04.66'; l: $20000; p: $60000; crc: $94737E93));
  rastan_sound: tipo_roms = (n: 'b04-19.49'; l: $10000; p: 0; crc: $EE81FDD8);
  rastan_sprites: array [0 .. 3] of tipo_roms = ((n: 'b04-05.15'; l: $20000; p: 0; crc: $C22D94AC),
    (n: 'b04-07.14'; l: $20000; p: $20000; crc: $B5632A51), (n: 'b04-06.28'; l: $20000; p: $40000;
    crc: $002CCF39), (n: 'b04-08.27'; l: $20000; p: $60000; crc: $FEAFCA05));
  rastan_adpcm: tipo_roms = (n: 'b04-20.76'; l: $10000; p: 0; crc: $FD1A34CC);

var
 scroll_x1,scroll_y1,scroll_x2,scroll_y2:word;
  bank_sound: array [0 .. 3, $0 .. $3FFF] of byte;
  rom: array [0 .. $2FFFF] of word;
  ram1, ram3: array [0 .. $1FFF] of word;
  spritebank, sound_bank: byte;
  ram2: array [0 .. $7FFF] of word;

procedure update_video_rastan;
var
  f, x, y, nchar, atrib, color: word;
  flipx, flipy: boolean;
begin
  for f := $FFF downto $0 do
  begin
    // background
    atrib := ram2[f * 2];
    color := atrib and $7F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := ram2[$1 + (f * 2)] and $3FFF;
      flipx := (atrib and $4000) <> 0;
      flipy := (atrib and $8000) <> 0;
      put_gfx_flip(x * 8, y * 8, nchar, color shl 4, 1, 0, flipx, flipy);
      gfx[0].buffer[f] := false;
    end;
    // foreground
    atrib := ram2[$4000 + (f * 2)];
    color := atrib and $7F;
    if (gfx[0].buffer[f + $1000] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := ram2[$4001 + (f * 2)] and $3FFF;
      flipx := (atrib and $4000) <> 0;
      flipy := (atrib and $8000) <> 0;
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 2, 0, flipx, flipy);
      gfx[0].buffer[f + $1000] := false;
    end;
  end;
  scroll_x_y(1, 3, scroll_x1, scroll_y1);
  scroll_x_y(2, 3, scroll_x2, scroll_y2);
  // Sprites
  for f := $FF downto 0 do
  begin
    nchar := (ram3[$2 + (f * 4)]) and $FFF;
    if nchar <> 0 then
    begin
      atrib := ram3[f * 4];
      color := ((atrib and $F) or ((spritebank and $F) shl 4)) shl 4;
      put_gfx_sprite(nchar, color, (atrib and $4000) <> 0, (atrib and $8000) <> 0, 1);
      x := (ram3[$3 + (f * 4)] + 16) and $1FF;
      y := (ram3[$1 + (f * 4)]) and $1FF;
      update_gfx_sprite(x, y, 3, 1);
    end;
  end;
  actualiza_trozo_final(16, 8, 320, 240, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_rastan;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
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

procedure rastan_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := tc0140syt_0.z80.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // Sound CPU
        tc0140syt_0.z80.run(frame_s);
        frame_s := frame_s + tc0140syt_0.z80.tframes - tc0140syt_0.z80.contador;
        if f = 247 then
        begin
          update_video_rastan;
          m68000_0.irq[5] := HOLD_LINE;
        end;
      end;
      events_rastan;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function rastan_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $5FFFF:
      rastan_getword := rom[direccion shr 1];
    $10C000 .. $10FFFF:
      rastan_getword := ram1[(direccion and $3FFF) shr 1];
    $200000 .. $200FFF:
      rastan_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $390000:
      rastan_getword := marcade.in1;
    $390002, $39000A:
      rastan_getword := $FF;
    $390004:
      rastan_getword := $8F;
    $390006:
      rastan_getword := marcade.in0;
    $390008:
      rastan_getword := $FE;
    $39000C .. $39000F:
      rastan_getword := $00;
    $3E0002:
      if m68000_0.read_8bits_hi_dir then
        rastan_getword := tc0140syt_0.comm_r;
    $C00000 .. $C0FFFF:
      rastan_getword := ram2[(direccion and $FFFF) shr 1];
    $D00000 .. $D03FFF:
      rastan_getword := ram3[(direccion and $3FFF) shr 1];
  end;
end;

procedure rastan_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal5bit(tmp_color shr 10);
    color.g := pal5bit(tmp_color shr 5);
    color.r := pal5bit(tmp_color);
    set_pal_color(color, numero);
    buffer_color[(numero shr 4) and $7F] := true;
  end;

begin
  case direccion of
    0 .. $5FFFF:
      ; // ROM
    $10C000 .. $10FFFF:
      ram1[(direccion and $3FFF) shr 1] := valor;
    $200000 .. $200FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color(valor, (direccion and $FFF) shr 1);
      end;
    $350008, $3C0000:
      ;
    $380000:
      spritebank := (valor and $E0) shr 5;
    $3E0000:
      tc0140syt_0.port_w(valor and $FF);
    $3E0002:
      tc0140syt_0.comm_w(valor and $FF);
    $C00000 .. $C03FFF:
      if ram2[(direccion and $FFFF) shr 1] <> valor then
      begin
        ram2[(direccion and $FFFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $C04000 .. $C07FFF, $C0C000 .. $C0FFFF:
      ram2[(direccion and $FFFF) shr 1] := valor;
    $C08000 .. $C0BFFF:
      if ram2[(direccion and $FFFF) shr 1] <> valor then
      begin
        ram2[(direccion and $FFFF) shr 1] := valor;
        gfx[0].buffer[((direccion and $3FFF) shr 2) + $1000] := true;
      end;
    $C20000:
      scroll_y1 := (512 - valor) and $1FF;
    $C20002:
      scroll_y2 := (512 - valor) and $1FF;
    $C40000:
      scroll_x1 := (512 - valor) and $1FF;
    $C40002:
      scroll_x2 := (512 - valor) and $1FF;
    $C50000 .. $C50003:
      ;
    $D00000 .. $D03FFF:
      ram3[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function rastan_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF, $8000 .. $8FFF:
      rastan_snd_getbyte := mem_snd[direccion];
    $4000 .. $7FFF:
      rastan_snd_getbyte := bank_sound[sound_bank, direccion and $3FFF];
    $9001:
      rastan_snd_getbyte := ym2151_0.status;
    $A001:
      rastan_snd_getbyte := tc0140syt_0.slave_comm_r;
  end;
end;

procedure rastan_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $8FFF:
      mem_snd[direccion] := valor;
    $9000:
      ym2151_0.reg(valor);
    $9001:
      ym2151_0.write(valor);
    $A000:
      tc0140syt_0.slave_port_w(valor);
    $A001:
      tc0140syt_0.slave_comm_w(valor);
  $b000:msm5205_0.pos:=(msm5205_0.pos and $ff) or (valor shl 8);
  $c000:msm5205_0.reset_w(false);
    $D000:
      begin
           msm5205_0.reset_w(true);
           msm5205_0.pos:=msm5205_0.pos and $ff00;
      end;
end;
end;

procedure snd_adpcm;
begin
if msm5205_0.data_val<>-1 then begin
		msm5205_0.data_w(msm5205_0.data_val and $f);
		msm5205_0.data_val:=-1;
    msm5205_0.pos:=(msm5205_0.pos+1) and $ffff;
end else begin
		msm5205_0.data_val:=msm5205_0.rom_data[msm5205_0.pos];
		msm5205_0.data_w(msm5205_0.data_val shr 4);
end;
end;

procedure sound_bank_rom(valor: byte);
begin
  sound_bank := valor and 3;
end;

procedure sound_instruccion;
begin
  ym2151_0.update;
  msm5205_0.update;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  tc0140syt_0.z80.change_irq(irqstate);
end;

//Main
procedure reset_rastan;
begin
  m68000_0.reset;
  tc0140syt_0.reset;
  ym2151_0.reset;
  msm5205_0.reset;
  reset_audio;
  marcade.in0 := $1F;
  marcade.in1 := $FF;
  sound_bank := 0;
  scroll_x1 := 0;
  scroll_y1 := 0;
  scroll_x2 := 0;
  scroll_y2 := 0;
end;

function start_rastan: boolean;
const
  pc_y: array [0 .. 7] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16);
  ps_x: array [0 .. 15] of dword = (0, 4, $40000 * 8 + 0, $40000 * 8 + 4, 8 + 0, 8 + 4,
    $40000 * 8 + 8 + 0, $40000 * 8 + 8 + 4, 16 + 0, 16 + 4, $40000 * 8 + 16 + 0,
    $40000 * 8 + 16 + 4, 24 + 0, 24 + 4, $40000 * 8 + 24 + 0, $40000 * 8 + 24 + 4);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32,
    8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
var
  memory_temp: array [0 .. $7FFFF] of byte;
begin
  machine_calls.general_loop := rastan_loop;
  machine_calls.reset := reset_rastan;
  start_rastan := false;
  start_audio(false);
  screen_init(1, 512, 512);
  screen_mod_scroll(1, 512, 512, 511, 512, 256, 511);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 256, 511);
  screen_init(3, 512, 512, false, true);
  start_video(320, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256);
  m68000_0.change_ram16_calls(rastan_getword, rastan_putword);
  // Sound CPU
  tc0140syt_0 := tc0140syt_chip.create(4000000, 256);
  tc0140syt_0.z80.change_ram_calls(rastan_snd_getbyte, rastan_snd_putbyte);
  tc0140syt_0.z80.init_sound(sound_instruccion);
  // Sound Chips
msm5205_0:=MSM5205_chip.create(384000,MSM5205_S48_4B,1,$10000);
msm5205_0.change_advance(snd_adpcm);
if not(roms_load(msm5205_0.rom_data,rastan_adpcm)) then exit;
  ym2151_0 := ym2151_chip.create(4000000);
  ym2151_0.change_port_func(sound_bank_rom);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  // cargar roms
  if not(roms_load16w(@rom, rastan_rom)) then
    exit;
//rom[$05FF9F]:=$fa;  //Cheeeeeeeeat
//cargar sonido+ponerlas en su banco
  if not(roms_load(@memory_temp, rastan_sound)) then
    exit;
  copymemory(@mem_snd[0], @memory_temp[0], $4000);
  copymemory(@bank_sound[0, 0], @memory_temp[$0], $4000);
  copymemory(@bank_sound[1, 0], @memory_temp[$4000], $4000);
  copymemory(@bank_sound[2, 0], @memory_temp[$8000], $4000);
  copymemory(@bank_sound[3, 0], @memory_temp[$C000], $4000);
  // convertir chars
  if not(roms_load(@memory_temp, rastan_char)) then
    exit;
  init_gfx(0, 8, 8, $4000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 16 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, rastan_sprites)) then
    exit;
  init_gfx(1, 16, 16, $1000);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // final
  reset_rastan;
  start_rastan := true;
end;

end.
