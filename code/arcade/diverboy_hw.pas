unit diverboy_hw;

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
  sound_engine;

function start_diverboy: boolean;

implementation

const
        diverboy_rom:array[0..1] of tipo_roms=(
        (n:'db_01.bin';l:$20000;p:0;crc:$6aa11366),(n:'db_02.bin';l:$20000;p:1;crc:$45f8a673));
        diverboy_sound:tipo_roms=(n:'db_05.bin';l:$10000;p:0;crc:$ffeb49ec);
        diverboy_obj1:array[0..1] of tipo_roms=(
        (n:'db_08.bin';l:$80000;p:0;crc:$7bb96220),(n:'db_09.bin';l:$80000;p:1;crc:$12b15476));
        diverboy_obj2:array[0..3] of tipo_roms=(
        (n:'db_07.bin';l:$20000;p:0;crc:$18485741),(n:'db_10.bin';l:$20000;p:1;crc:$c381d1cc),
        (n:'db_06.bin';l:$20000;p:$40000;crc:$21b4e352),(n:'db_11.bin';l:$20000;p:$40001;crc:$41d29c81));
        diverboy_oki:array[0..1] of tipo_roms=(
        (n:'db_03.bin';l:$80000;p:0;crc:$50457505),(n:'db_04.bin';l:$20000;p:$80000;crc:$01b81da0));
        //Dip
        diverboy_dip:array [0..5] of def_dip2=(
        (mask:7;name:'Coinage';number:8;val8:(7,6,5,0,1,2,3,4);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 6C')),
        (mask:8;name:'Lives';number:2;val2:(0,8);name2:('2','3')),
        (mask:$10;name:'Display Copyright';number:2;val2:(0,$10);name2:('No','Yes')),
        (mask:$60;name:'Difficulty';number:4;val4:(0,$20,$40,$60);name4:('Easy','Normal','Hard','Hardest')),
        (mask:$80;name:'Free Play';number:2;val2:($80,0);name2:('No','Yes')),());
var
  rom: array [0 .. $1FFFF] of word;
  ram: array [0 .. $7FFF] of word;
  obj_ram: array [0 .. $1FFF] of word;
  sound_latch: byte;
  frame: boolean;
  oki_rom: array [0 .. 3, 0 .. $3FFFF] of byte;

procedure update_video_diverboy;
var
  f, atrib, nchar, color, x, y: word;
  flash: boolean;
  bank: byte;
begin
  for f := 0 to $3FF do
  begin
    atrib := obj_ram[(f * 8) + 1];
    color := (atrib and $F0) or ((atrib and $C) shl 6);
    flash := (atrib and $1000) <> 0;
    bank := (atrib and 2) shr 1;
    if (atrib and 8) <> 0 then
    begin
      gfx[0].trans[0] := false;
      gfx[1].trans[0] := false;
    end
    else
    begin
      gfx[0].trans[0] := true;
      gfx[1].trans[0] := true;
    end;
    nchar := obj_ram[(f * 8) + 3];
    x := obj_ram[f * 8];
    y := $100 - obj_ram[(f * 8) + 4];
    if (not(flash) or frame) then
    begin
      put_gfx_sprite(nchar, color, false, false, bank);
      update_gfx_sprite(x, y, 1, bank);
    end;
  end;
  update_final_piece(4, 16, 318, 240, 1);
end;

procedure events_diverboy;
begin
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.right[0] then
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
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
  end;
end;

procedure diverboy_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
 for f:=0 to $ff do begin
   m68000_0.run(frame_main);
   frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
   z80_0.run(frame_snd);
   frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
   if f=255 then begin
      m68000_0.irq[6]:=HOLD_LINE;
      update_video_diverboy;
      frame:=not(frame);
   end;
 end;
    events_diverboy;
    video_sync;
  end;
end;

function diverboy_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      diverboy_getword := rom[direccion shr 1];
    $40000 .. $4FFFF:
      diverboy_getword := ram[(direccion and $FFFF) shr 1];
    $80000 .. $83FFF:
      diverboy_getword := obj_ram[(direccion and $3FFF) shr 1];
    $140000 .. $1407FF:
      diverboy_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $180000:
      diverboy_getword := marcade.in0; // P1+P2
    $180002:
      diverboy_getword := marcade.dswa;
    $180008:
      diverboy_getword := marcade.in1;
  end;
end;

procedure diverboy_putword(direccion: dword; valor: word);
  procedure cambiar_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal4bit(tmp_color shr 8);
    color.g := pal4bit(tmp_color shr 4);
    color.r := pal4bit(tmp_color shr 0);
    set_pal_color(color, numero);
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $40000 .. $4FFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
    $80000 .. $83FFF:
      obj_ram[(direccion and $3FFF) shr 1] := valor;
    $100000:
      begin
        sound_latch := valor;
        z80_0.change_irq(HOLD_LINE);
      end;
    $140000 .. $1407FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        cambiar_color(valor, (direccion and $7FF) shr 1);
      end;
  end;
end;

function diverboy_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      diverboy_snd_getbyte := mem_snd[direccion];
    $9800:
      diverboy_snd_getbyte := oki_6295_0.read;
    $A000:
      diverboy_snd_getbyte := sound_latch;
  end;
end;

procedure diverboy_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $9000:
      copymemory(oki_6295_0.get_rom_addr, @oki_rom[valor and 3, 0], $40000);
    $9800:
      oki_6295_0.write(valor);
  end;
end;

procedure diverboy_sound_update;
begin
  oki_6295_0.update;
end;

// Main
procedure reset_diverboy;
begin
  m68000_0.reset;
  z80_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=z80_0.tframes;
  oki_6295_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $F7;
  sound_latch := 0;
  frame := false;
end;

function start_diverboy: boolean;
const
  ps_x: array [0 .. 15] of dword = (1 * 4, 0 * 4, 3 * 4, 2 * 4, 5 * 4, 4 * 4, 7 * 4, 6 * 4, 9 * 4, 8 * 4,
    11 * 4, 10 * 4, 13 * 4, 12 * 4, 15 * 4, 14 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 16 * 4, 1 * 16 * 4, 2 * 16 * 4, 3 * 16 * 4, 4 * 16 * 4, 5 * 16 * 4,
    6 * 16 * 4, 7 * 16 * 4, 8 * 16 * 4, 9 * 16 * 4, 10 * 16 * 4, 11 * 16 * 4, 12 * 16 * 4, 13 * 16 * 4,
    14 * 16 * 4, 15 * 16 * 4);
var
  memoria_temp: pbyte;
begin
  machine_calls.general_loop := diverboy_loop;
  machine_calls.reset := reset_diverboy;
  start_diverboy := false;
  start_audio(false);
  screen_init(1, 512, 512, false, true);
  start_video(318, 240);
  main_screen.flip_main_x := true;
  getmem(memoria_temp, $100000);
  // Main CPU
  m68000_0 := cpu_m68000.create(12000000, $100);
  m68000_0.change_ram16_calls(diverboy_getword, diverboy_putword);
  if not(roms_load16w(@rom, diverboy_rom)) then
    exit;
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, $100);
  z80_0.change_ram_calls(diverboy_snd_getbyte, diverboy_snd_putbyte);
  z80_0.init_sound(diverboy_sound_update);
  if not(roms_load(@mem_snd, diverboy_sound)) then
    exit;
  copymemory(@mem_snd[0], @mem_snd[$8000], $8000);
  // Sound chip
  oki_6295_0 := snd_okim6295.create(1320000, OKIM6295_PIN7_HIGH, 1);
  if not(roms_load(memoria_temp, diverboy_oki)) then
    exit;
  copymemory(@oki_rom[0, 0], @memoria_temp[0], $20000);
  copymemory(@oki_rom[1, 0], @memoria_temp[$20000], $20000);
  copymemory(@oki_rom[2, 0], @memoria_temp[$40000], $20000);
  copymemory(@oki_rom[3, 0], @memoria_temp[$60000], $20000);
  copymemory(@oki_rom[0, $20000], @memoria_temp[$80000], $20000);
  copymemory(@oki_rom[1, $20000], @memoria_temp[$80000], $20000);
  copymemory(@oki_rom[2, $20000], @memoria_temp[$80000], $20000);
  copymemory(@oki_rom[3, $20000], @memoria_temp[$80000], $20000);
  // obj1
  if not(roms_load16b(memoria_temp, diverboy_obj1)) then
    exit;
  init_gfx(0, 16, 16, $2000);
  gfx_set_desc_data(4, 0, 16 * 16 * 4, 0, 1, 2, 3);
  convert_gfx(0, 0, memoria_temp, @ps_x, @ps_y, false, false);
  // obj2
  if not(roms_load16b(memoria_temp, diverboy_obj2)) then
    exit;
  init_gfx(1, 16, 16, $3000);
  gfx_set_desc_data(4, 0, 16 * 16 * 4, 0, 1, 2, 3);
  convert_gfx(1, 0, memoria_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := $B8;
marcade.dswa_val2:=@diverboy_dip;
  // final
  freemem(memoria_temp);
  reset_diverboy;
  start_diverboy := true;
end;

end.
