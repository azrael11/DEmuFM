unit cabal_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  seibu_sound,
  rom_engine,
  pal_engine,
  sound_engine;

function start_cabal: boolean;

implementation

const
        cabal_rom:array[0..3] of tipo_roms=(
        (n:'13.7h';l:$10000;p:0;crc:$00abbe0c),(n:'11.6h';l:$10000;p:1;crc:$44736281),
        (n:'12.7j';l:$10000;p:$20000;crc:$d763a47c),(n:'10.6j';l:$10000;p:$20001;crc:$96d5e8af));
  cabal_char: tipo_roms = (n: '5-6s'; l: $4000; p: 0; crc: $6A76955A);
        cabal_sprites:array[0..7] of tipo_roms=(
        (n:'sp_rom1.bin';l:$10000;p:0;crc:$34d3cac8),(n:'sp_rom2.bin';l:$10000;p:1;crc:$4e49c28e),
        (n:'sp_rom3.bin';l:$10000;p:$20000;crc:$7065e840),(n:'sp_rom4.bin';l:$10000;p:$20001;crc:$6a0e739d),
        (n:'sp_rom5.bin';l:$10000;p:$40000;crc:$0e1ec30e),(n:'sp_rom6.bin';l:$10000;p:$40001;crc:$581a50c1),
        (n:'sp_rom7.bin';l:$10000;p:$60000;crc:$55c44764),(n:'sp_rom8.bin';l:$10000;p:$60001;crc:$702735c9));
  cabal_tiles: array [0 .. 7] of tipo_roms = ((n: 'bg_rom1.bin'; l: $10000; p: 0; crc: $1023319B), (n: 'bg_rom2.bin'; l: $10000; p: $1; crc: $3B6D2B09), (n: 'bg_rom3.bin'; l: $10000; p: $20000;
    crc: $420B0801), (n: 'bg_rom4.bin'; l: $10000; p: $20001; crc: $77BC7A60), (n: 'bg_rom5.bin'; l: $10000; p: $40000; crc: $543FCB37), (n: 'bg_rom6.bin'; l: $10000; p: $40001; crc: $0BC50075),
    (n: 'bg_rom7.bin'; l: $10000; p: $60000; crc: $D28D921E), (n: 'bg_rom8.bin'; l: $10000; p: $60001; crc: $67E4FE47));
  cabal_sound: array [0 .. 1] of tipo_roms = ((n: '4-3n'; l: $2000; p: 0; crc: $4038EFF2), (n: '3-3p'; l: $8000; p: $8000; crc: $D9DEFCBF));
  cabal_adpcm: array [0 .. 1] of tipo_roms = ((n: '2-1s'; l: $10000; p: 0; crc: $850406B4), (n: '1-1u'; l: $10000; p: $10000; crc: $8B3E0789));
        cabal_dip_a:array [0..9] of def_dip2=(
        (mask:$f;name:'Coinage';number:16;val16:($a,$b,$c,$d,1,$e,2,3,$f,4,9,8,7,6,5,0);name16:('6C 1C','5C 1C','4C 1C','3C 1C','8C 3C','2C 1C','5C 3C','3C 2C','1C 1C','2C 3C','1C 2C','1C 3C','1C 4C','1C 5C','1C 6C','Free Play')),
        //(mask:3;name:'Coin A';number:4;dip:((dip_val:0;dip_name:'5C 1C'),(dip_val:1;dip_name:'3C 1C'),(dip_val:2;dip_name:'2C 1C'),(dip_val:3;dip_name:'1C 1C'),(),(),(),(),(),(),(),(),(),(),(),())),
        //(mask:$c;name:'Coin B';number:4;dip:((dip_val:$c;dip_name:'1C 2C'),(dip_val:8;dip_name:'1C 3C'),(dip_val:4;dip_name:'1C 5C'),(dip_val:0;dip_name:'1C 6C'),(),(),(),(),(),(),(),(),(),(),(),())),
        (mask:$10;name:'Coin Mode';number:2;val2:($10,0);name2:('Mode 1','Mode 2')),
        (mask:$20;name:'Invert Buttons';number:2;val2:($20,0);name2:('Off','On')),
        (mask:$40;name:'Flip Screen';number:2;val2:($40,0);name2:('Off','On')),
        (mask:$80;name:'Track Ball';number:2;val2:($80,0);name2:('Small','Large')),
        (mask:$300;name:'Lives';number:4;val4:($200,$300,$100,0);name4:('2','3','5','121')),
        (mask:$c00;name:'Bonus Life';number:4;val4:($c00,$800,$400,0);name4:('150K 650K 500K+','200K 800K 600K+','300K 1000K 700K+','300K')),
        (mask:$3000;name:'Difficulty';number:4;val4:($3000,$2000,$1000,0);name4:('Easy','Normal','Hard','Very Hard')),
        (mask:$8000;name:'Demo Sounds';number:2;val2:(0,$8000);name2:('Off','On')),());

var
  rom: array [0 .. $1FFFF] of word;
  main_ram: array [0 .. $7FFF] of word;
  bg_ram: array [0 .. $1FF] of word;
  fg_ram: array [0 .. $3FF] of word;

procedure update_video_cabal;
var
  f, color, x, y, nchar, atrib: word;
begin
  // Background
  for f := 0 to $FF do
  begin
    atrib := bg_ram[f];
    color := (atrib shr 12) and $F;
    if (gfx[2].buffer[f] or buffer_color[color + $40]) then
    begin
      x := f mod 16;
      y := f div 16;
      nchar := atrib and $FFF;
      put_gfx(x * 16, y * 16, nchar, (color shl 4) + 512, 2, 2);
      gfx[2].buffer[f] := false;
    end;
  end;
  // Foreground
  for f := 0 to $3FF do
  begin
    atrib := fg_ram[f];
    color := (atrib shr 10) and $3F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := atrib and $3FF;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 2, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  // Sprites
  for f := $1FF downto 0 do
  begin
    y := main_ram[(f * 4) + $1C00];
    if (y and $100) <> 0 then
    begin
      atrib := main_ram[(f * 4) + $1C01];
      x := main_ram[(f * 4) + $1C02];
      nchar := atrib and $FFF;
      color := (x and $7800) shr 7;
      put_gfx_sprite(nchar, color + 256, (x and $400) <> 0, false, 1);
      update_gfx_sprite(x, y, 3, 1);
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  update_final_piece(0, 16, 256, 224, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_cabal;
begin
  if event.arcade then
  begin
  //P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
  //P2
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $7FFF)
    else
      marcade.in1 := (marcade.in1 or $8000);
    // Coins
    if p_contrls.map_arcade.coin[1] then
      seibu_snd_0.input := (seibu_snd_0.input or 1)
    else
      seibu_snd_0.input := (seibu_snd_0.input and $FE);
    if p_contrls.map_arcade.coin[0] then
      seibu_snd_0.input := (seibu_snd_0.input or 2)
    else
      seibu_snd_0.input := (seibu_snd_0.input and $FD);
  end;
end;

procedure cabal_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
   for f:=0 to $ff do begin
      events_cabal;
      if f=240 then begin
          update_video_cabal;
          m68000_0.irq[1]:=HOLD_LINE;
      end;
      //Main CPU
      m68000_0.run(frame_main);
      frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
      //Sound CPU
      seibu_snd_0.run;
   end;
    video_sync;
  end;
end;

function cabal_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      cabal_getword := rom[direccion shr 1];
    $40000 .. $4FFFF:
      cabal_getword := main_ram[(direccion and $FFFF) shr 1];
    $60000 .. $607FF:
      cabal_getword := fg_ram[(direccion and $7FF) shr 1];
    $80000 .. $803FF:
      cabal_getword := bg_ram[(direccion and $3FF) shr 1];
    $A0000:
      cabal_getword := marcade.dswa; // DSW
    $A0008:
      cabal_getword := marcade.in0;
    $A000C:
      cabal_getword := $FFFF; // track 0
    $A000A, $A000E:
      cabal_getword := 0; // track 1
    $A0010:
      cabal_getword := marcade.in1; // input
    $E0000 .. $E07FF:
      cabal_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $E8000 .. $E800D:
      cabal_getword := seibu_snd_0.get((direccion and $E) shr 1);
  end;
end;

procedure cabal_putword(direccion: dword; valor: word);

  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal4bit(tmp_color shr 8);
    color.g := pal4bit(tmp_color shr 4);
    color.r := pal4bit(tmp_color);
    set_pal_color(color, numero);
    case numero of
      0 .. $FF:
        buffer_color[numero shr 2] := true;
      512 .. 767:
        buffer_color[((numero shl 4) and $F) + $40] := true;
    end;
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $40000 .. $4FFFF:
      main_ram[(direccion and $FFFF) shr 1] := valor;
    $60000 .. $607FF:
      if fg_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        fg_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $80000 .. $803FF:
      if bg_ram[(direccion and $3FF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $3FF) shr 1] := valor;
        gfx[2].buffer[(direccion and $3FF) shr 1] := true;
      end;
    $C0040:
      ; // NOP
    $C0080:
      main_screen.flip_main_screen := (valor <> 0); // Flip screen
    $E0000 .. $E07FF:
      if (buffer_paleta[(direccion and $7FF) shr 1] <> valor) then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $E8000 .. $E800D:
      seibu_snd_0.put((direccion and $E) shr 1, valor);
  end;
end;

// Main
procedure reset_cabal;
begin
  m68000_0.reset;
 frame_main:=m68000_0.tframes;
  seibu_snd_0.reset;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  seibu_snd_0.input := $FC;
end;

function start_cabal: boolean;
const
  pc_x: array [0 .. 7] of dword = (3, 2, 1, 0, 8 + 3, 8 + 2, 8 + 1, 8 + 0);
  pc_y: array [0 .. 7] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16);
  pt_x: array [0 .. 15] of dword = (3, 2, 1, 0, 16 + 3, 16 + 2, 16 + 1, 16 + 0, 32 * 16 + 3, 32 * 16 + 2, 32 * 16 + 1, 32 * 16 + 0, 33 * 16 + 3, 33 * 16 + 2, 33 * 16 + 1, 33 * 16 + 0);
  pt_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  ps_x: array [0 .. 15] of dword = (3, 2, 1, 0, 16 + 3, 16 + 2, 16 + 1, 16 + 0, 32 + 3, 32 + 2, 32 + 1, 32 + 0, 48 + 3, 48 + 2, 48 + 1, 48 + 0);
  ps_y: array [0 .. 15] of dword = (30 * 32, 28 * 32, 26 * 32, 24 * 32, 22 * 32, 20 * 32, 18 * 32, 16 * 32, 14 * 32, 12 * 32, 10 * 32, 8 * 32, 6 * 32, 4 * 32, 2 * 32, 0 * 32);
var
  memory_temp: array [0 .. $7FFFF] of byte;
begin
  machine_calls.general_loop := cabal_loop;
  machine_calls.reset := reset_cabal;
  machine_calls.fps_max := 59.60;
  start_cabal := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 256, 256, true);
  screen_init(3, 512, 256, false, true);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 256);
  m68000_0.change_ram16_calls(cabal_getword, cabal_putword);
  // Load Rom
  if not(roms_load16w(@rom, cabal_rom)) then
    exit;
  // Load Audio
  if not(roms_load(@memory_temp, cabal_sound)) then
    exit;
  seibu_snd_0 := seibu_snd_type.create(SEIBU_ADPCM, 3579545, 256, @memory_temp, true);
  copymemory(@mem_snd[$8000], @memory_temp[$8000], $8000);
  // adpcm
  if not(roms_load(@memory_temp, cabal_adpcm)) then
    exit;
  seibu_snd_0.adpcm_load_roms(@memory_temp, $10000);
  // convert chars
  if not(roms_load(@memory_temp, cabal_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // sprites
  if not(roms_load16b(@memory_temp, cabal_sprites)) then
    exit;
  init_gfx(1, 16, 16, $1000);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 64 * 16, 2 * 4, 3 * 4, 0 * 4, 1 * 4);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // tiles
  if not(roms_load16b(@memory_temp, cabal_tiles)) then
    exit;
  init_gfx(2, 16, 16, $1000);
  gfx_set_desc_data(4, 0, 64 * 16, 2 * 4, 3 * 4, 0 * 4, 1 * 4);
  convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
  // Dip
  marcade.dswa := $EFFF;
marcade.dswa_val2:=@cabal_dip_a;
  // final
  start_cabal := true;
end;

end.
