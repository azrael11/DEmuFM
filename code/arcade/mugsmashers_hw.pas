unit mugsmashers_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  nz80,
  oki6295,
  sound_engine,
  ym_2151;

function start_mugsmash: boolean;

implementation

const
        mugsmash_rom:array[0..1] of tipo_roms=(
        (n:'mugs_04.bin';l:$40000;p:0;crc:$2498fd27),(n:'mugs_05.bin';l:$40000;p:1;crc:$95efb40b));
        mugsmash_sound:tipo_roms=(n:'mugs_03.bin';l:$10000;p:0;crc:$0101df2d);
        mugsmash_tiles:array[0..3] of tipo_roms=(
        (n:'mugs_12.bin';l:$80000;p:0;crc:$c0a6ed98),(n:'mugs_13.bin';l:$80000;p:$80000;crc:$e2be8595),
        (n:'mugs_14.bin';l:$80000;p:$100000;crc:$24e81068),(n:'mugs_15.bin';l:$80000;p:$180000;crc:$82e8187c));
        mugsmash_sprites:array[0..5] of tipo_roms=(
        (n:'mugs_11.bin';l:$80000;p:0;crc:$1c9f5acf),(n:'mugs_10.bin';l:$80000;p:1;crc:$6b3c22d9),
        (n:'mugs_09.bin';l:$80000;p:$100000;crc:$4e9490f3),(n:'mugs_08.bin';l:$80000;p:$100001;crc:$716328d5),
        (n:'mugs_07.bin';l:$80000;p:$200000;crc:$9e3167fd),(n:'mugs_06.bin';l:$80000;p:$200001;crc:$8df75d29));
        mugsmash_oki:array[0..1] of tipo_roms=(
        (n:'mugs_02.bin';l:$20000;p:0;crc:$f92a7f4a),(n:'mugs_01.bin';l:$20000;p:$20000;crc:$1a3a0b39));
        //Dip
        mugsmash_dip_a:array [0..4] of def_dip2=(
        (mask:$100;name:'Draw Objects';number:2;val2:($100,0);name2:('Off','On')),
        (mask:$200;name:'Freeze';number:2;val2:($200,0);name2:('Off','On')),
        (mask:$1000;name:'Color Test';number:2;val2:($1000,0);name2:('Off','On')),
        (mask:$2000;name:'Draw SF.';number:2;val2:($2000,0);name2:('Off','On')),());
        mugsmash_dip_b:array [0..3] of def_dip2=(
        (mask:$e00;name:'Coinage';number:8;val8:($c00,$a00,$800,0,$200,$400,$600,$e00);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','Free Play')),
        (mask:$1000;name:'Allow Continue';number:2;val2:($1000,0);name2:('No','Yes')),
        (mask:$2000;name:'Sound Test';number:2;val2:($2000,0);name2:('Off','On')),());
        mugsmash_dip_c:array [0..3] of def_dip2=(
        (mask:$100;name:'Demo Sounds';number:2;val2:($100,0);name2:('Off','On')),
        (mask:$600;name:'Lives';number:4;val4:(0,$200,$400,$600);name4:('1','2','3','4')),
        (mask:$3000;name:'Difficulty';number:4;val4:(0,$1000,$2000,$3000);name4:('Very Easy','Easy','Hard','Very Hard')),());

var
  rom: array [0 .. $3FFFF] of word;
  video_ram1, video_ram2: array [0 .. $7FF] of word;
  ram: array [0 .. $7FFF] of word;
  sprite_ram: array [0 .. $1FFF] of word;
  sound_latch: byte;
  scroll_x1, scroll_x2, scroll_y1, scroll_y2: word;

procedure update_video_mugsmash;
var
  f, atrib, nchar, x, y: word;
  color: byte;
begin
  for f := 0 to $3FF do
  begin
    x := f mod 32;
    y := f div 32;
    // FG
    atrib := video_ram2[f * 2];
    color := atrib and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := video_ram2[(f * 2) + 1];
      put_gfx_flip(x * 16, y * 16, nchar, $100 + ((color + $10) shl 4), 1, 0, (atrib and $40) <> 0,
        (atrib and $80) <> 0);
      gfx[0].buffer[f] := false;
    end;
    // BG
    atrib := video_ram1[f * 2];
    color := atrib and $F;
    if (gfx[0].buffer[f + $400] or buffer_color[color]) then
    begin
      nchar := video_ram1[(f * 2) + 1];
      put_gfx_trans_flip(x * 16, y * 16, nchar, $100 + (color shl 4), 2, 0, (atrib and $40) <> 0,
        (atrib and $80) <> 0);
      gfx[0].buffer[f + $400] := false;
    end;
  end;
  scroll_x_y(1, 3, scroll_x2 + 7, scroll_y2 + 12);
  scroll_x_y(2, 3, scroll_x1 + 3, scroll_y1 + 12);
  for f := 0 to $3FF do
  begin
    atrib := sprite_ram[(f * 8) + 1];
    color := (atrib and $F) shl 4;
    nchar := ((sprite_ram[(f * 8) + 3] and $FF) or ((sprite_ram[(f * 8) + 2] and $FF) shl 8));
    x := (sprite_ram[f * 8] and $FF) + ((atrib and $20) shl 3) - 28;
    y := (sprite_ram[(f * 8) + 4] and $FF) + ((atrib and $10) shl 4) - 24;
    put_gfx_sprite(nchar, color, (atrib and $80) <> 0, false, 1);
    update_gfx_sprite(x and $1FF, y and $1FF, 3, 1);
  end;
  update_final_piece(0, 0, 320, 240, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_mugsmash;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFEF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FF7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure mugsmash_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
 for f:=0 to $ff do begin
   if f=248 then begin
      m68000_0.irq[6]:=ASSERT_LINE;
      update_video_mugsmash;
   end;
   m68000_0.run(frame_main);
   frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
   z80_0.run(frame_snd);
   frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
 end;
    events_mugsmash;
    video_sync;
  end;
end;

function mugsmash_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      mugsmash_getword := rom[direccion shr 1];
    $80000 .. $80FFF:
      mugsmash_getword := video_ram1[(direccion and $FFF) shr 1];
    $82000 .. $82FFF:
      mugsmash_getword := video_ram2[(direccion and $FFF) shr 1];
    $100000 .. $1005FF:
      mugsmash_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $1C0000 .. $1CFFFF:
      mugsmash_getword := ram[(direccion and $FFFF) shr 1];
    $200000 .. $203FFF:
      mugsmash_getword := sprite_ram[(direccion and $3FFF) shr 1];
    $180000:
      mugsmash_getword := marcade.in0 or (marcade.dswa and $3000);
    $180002:
      mugsmash_getword := marcade.in1 or marcade.dswb;
    $180004:
      mugsmash_getword := marcade.dswc; 
    $180006:
      mugsmash_getword := $FCFF or (marcade.dswa and $300); 
  end;
end;

procedure mugsmash_putword(direccion: dword; valor: word);
  procedure cambiar_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.r := pal5bit(tmp_color shr 10);
    color.g := pal5bit(tmp_color shr 5);
    color.b := pal5bit(tmp_color shr 0);
    set_pal_color(color, numero);
    if numero > $100 then
      buffer_color[(numero shr 4) and $F] := true;
  end;

begin
  case direccion of
    0 .. $7FFFF:
      ; // ROM
    $80000 .. $80FFF:
      if video_ram1[(direccion and $FFF) shr 1] <> valor then
      begin
        video_ram1[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[((direccion and $FFF) shr 2) + $400] := true;
      end;
    $82000 .. $82FFF:
      if video_ram2[(direccion and $FFF) shr 1] <> valor then
      begin
        video_ram2[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 2] := true;
      end;
    $C0000:
      scroll_x1 := valor;
    $C0002:
      scroll_y1 := valor;
    $C0004:
      scroll_x2 := valor;
    $C0006:
      scroll_y2 := valor;
    $100000 .. $1005FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        cambiar_color(valor, (direccion and $7FF) shr 1);
      end;
    $140002:
      begin
        sound_latch := valor;
        z80_0.change_nmi(PULSE_LINE);
      end;
    $140004:
      m68000_0.irq[6] := CLEAR_LINE;
    $140006:
      ;
    $1C0000 .. $1CFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
    $200000 .. $203FFF:
      sprite_ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function mugsmash_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      mugsmash_snd_getbyte := mem_snd[direccion];
    $8801:
      mugsmash_snd_getbyte := ym2151_0.status;
    $9800:
      mugsmash_snd_getbyte := oki_6295_0.read;
    $A000:
      mugsmash_snd_getbyte := sound_latch;
  end;
end;

procedure mugsmash_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $8800:
      ym2151_0.reg(valor);
    $8801:
      ym2151_0.write(valor);
    $9800:
      oki_6295_0.write(valor);
  end;
end;

procedure mugsmash_sound_update;
begin
  ym2151_0.update;
  oki_6295_0.update;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

// Main
procedure reset_mugsmash;
begin
  m68000_0.reset;
  z80_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=z80_0.tframes;
  ym2151_0.reset;
  oki_6295_0.reset;
  reset_audio;
  marcade.in0 := $CFFF;
  marcade.in1 := $C1FF;
  sound_latch := 0;
  scroll_x1 := 0;
  scroll_x2 := 0;
  scroll_y1 := 0;
  scroll_y2 := 0;
end;

function start_mugsmash: boolean;
const
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3,
    16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8,
    10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  ps_x: array [0 .. 15] of dword = (16, 20, 24, 28, 0, 4, 8, 12, 48, 52, 56, 60, 32, 36, 40, 44);
  ps_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64,
    9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
var
  memoria_temp: pbyte;
begin
  machine_calls.general_loop := mugsmash_loop;
  machine_calls.reset := reset_mugsmash;
  start_mugsmash := false;
  start_audio(true);
  screen_init(1, 512, 512);
  screen_mod_scroll(1, 512, 512, 511, 512, 512, 511);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 512, 511);
  screen_init(3, 512, 512, false, true);
  start_video(320, 240);
  getmem(memoria_temp, $300000);
  // Main CPU
  m68000_0 := cpu_m68000.create(12000000, $100);
  m68000_0.change_ram16_calls(mugsmash_getword, mugsmash_putword);
  if not(roms_load16w(@rom, mugsmash_rom)) then
    exit;
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, $100);
  z80_0.change_ram_calls(mugsmash_snd_getbyte, mugsmash_snd_putbyte);
  z80_0.init_sound(mugsmash_sound_update);
  if not(roms_load(@mem_snd, mugsmash_sound)) then
    exit;
  // Sound chips
  ym2151_0 := ym2151_chip.create(3579545);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  oki_6295_0 := snd_okim6295.create(1122000, OKIM6295_PIN7_HIGH, 1);
  if not(roms_load(oki_6295_0.get_rom_addr, mugsmash_oki)) then
    exit;
  // tiles
  if not(roms_load(memoria_temp, mugsmash_tiles)) then
    exit;
  init_gfx(0, 16, 16, $4000);
  gfx_set_desc_data(4, 0, 32 * 8, $80000 * 3 * 8, $80000 * 2 * 8, $80000 * 1 * 8, $80000 * 0 * 8);
  convert_gfx(0, 0, memoria_temp, @pt_x, @pt_y, false, false);
  gfx[0].trans[0] := true;
  // sprites
  if not(roms_load16b(memoria_temp, mugsmash_sprites)) then
    exit;
  init_gfx(1, 16, 16, $6000);
  gfx_set_desc_data(4, 0, 16 * 64, 0, 1, 2, 3);
  convert_gfx(1, 0, memoria_temp, @ps_x, @ps_y, false, false);
  gfx[1].trans[0] := true;
  // DIP
  marcade.dswa := $3300;
marcade.dswa_val2:=@mugsmash_dip_a;
  marcade.dswb := $2000;
marcade.dswb_val2:=@mugsmash_dip_b;
  marcade.dswc := $DAFF;
marcade.dswc_val2:=@mugsmash_dip_c;
  // final
  freemem(memoria_temp);
  reset_mugsmash;
  start_mugsmash := true;
end;

end.
