unit superduck_hw;

interface

uses
  Winapi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  oki6295;

function start_superduck: boolean;

implementation

const
  superduck_rom: array [0 .. 1] of tipo_roms = ((n: '5.u16n'; l: $20000; p: 0; crc: $837A559A), (n: '6.u16l'; l: $20000; p: 1; crc: $508E9905));
  superduck_sound: tipo_roms = (n: '4.su6'; l: $8000; p: 0; crc: $D75863EA);
  superduck_char: tipo_roms = (n: '3.cu15'; l: $8000; p: 0; crc: $B1CACCA4);
  superduck_bg: array [0 .. 3] of tipo_roms = ((n: '11.ul29'; l: $20000; p: 0; crc: $1B6958A4), (n: '12.ul30'; l: $20000; p: $20000; crc: $3E6BD24B), (n: '13.ul31'; l: $20000; p: $40000;
    crc: $BFF7B7CD), (n: '14.ul32'; l: $20000; p: $60000; crc: $97A7310B));
  superduck_fg: array [0 .. 3] of tipo_roms = ((n: '7.uu29'; l: $20000; p: 0; crc: $F3251B20), (n: '8.uu30'; l: $20000; p: $20000; crc: $03C60CBD), (n: '9.uu31'; l: $20000; p: $40000; crc: $9B6D3430),
    (n: '10.uu32'; l: $20000; p: $60000; crc: $BEED2616));
  superduck_sprites: array [0 .. 3] of tipo_roms = ((n: '15.u1d'; l: $20000; p: 0; crc: $81BF1F27), (n: '16.u2d'; l: $20000; p: 1; crc: $9573D6EC), (n: '17.u1c'; l: $20000; p: 2; crc: $21EF14D4),
    (n: '18.u2c'; l: $20000; p: 3; crc: $33DD0674));
  superduck_oki: array [0 .. 1] of tipo_roms = ((n: '2.su12'; l: $20000; p: 0; crc: $745D42FB), (n: '1.su13'; l: $80000; p: $20000; crc: $7FB1ED42));
  // DIP
        superduck_dip:array [0..5] of def_dip2=(
        (mask:7;name:'Coin A';number:8;val8:(0,1,2,3,7,6,5,4);name8:('5C 1C','4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C')),
        (mask:$10;name:'Demo Sounds';number:2;val2:(0,$10);name2:('Off','On')),
        (mask:$20;name:'Game Sounds';number:2;val2:(0,$20);name2:('Off','On')),
        (mask:$c0;name:'Lives';number:4;val4:($c0,$80,$40,0);name4:('2','3','4','5')),
        (mask:$4000;name:'Character Test';number:2;val2:($4000,0);name2:('Off','On')),());

var
  scroll_fg_x, scroll_fg_y, scroll_bg_x, scroll_bg_y: word;
  rom: array [0 .. $1FFFF] of word;
  sprite_ram: array [0 .. $FFF] of word;
  txt_ram: array [0 .. $7FF] of word;
  ram, bg_ram, fg_ram: array [0 .. $1FFF] of word;
  sound_latch: byte;
  oki_rom: array [0 .. 3, 0 .. $1FFFF] of byte;

procedure update_video_superduck;
var
  f, nchar, atrib, sx, sy, x, y, pos: word;
  color, atrib2: byte;
begin
  for f := 0 to $50 do
  begin
    x := f div 9;
    y := f mod 9;
    // bg
    sx := x + ((scroll_bg_x and $FE0) shr 5);
    sy := y + ((scroll_bg_y and $7E0) shr 5);
  pos:=((((sx and $fff8) shr 3) shl 6)+(((sy xor $3f) and 7) shl 3)+(sx and 7)) and $3ff;
	pos:=(pos+((((sy xor $3f) and $fff8) shr 3)*$400)) and $1fff;
    atrib := bg_ram[pos];
    atrib2 := atrib shr 8;
    color := atrib2 and $F;
    if (gfx[1].buffer[pos] or buffer_color[color + $20]) then
    begin
      nchar := (atrib and $FF) + ((atrib2 and $C0) shl 2);
      put_gfx_flip(x shl 5, y shl 5, nchar, (color shl 4) + 256, 2, 1, (atrib2 and $20) <> 0, (atrib2 and $10) <> 0);
      gfx[1].buffer[pos] := false;
    end;
    // fg
    sx := x + ((scroll_fg_x and $FE0) shr 5);
    sy := y + ((scroll_fg_y and $7E0) shr 5);
  pos:=((((sx and $fff8) shr 3) shl 6)+(((sy xor $3f) and 7) shl 3)+(sx and 7)) and $3ff;
	pos:=(pos+((((sy xor $3f) and $fff8) shr 3)*$400)) and $1fff;
    atrib := fg_ram[pos];
    atrib2 := atrib shr 8;
    color := atrib2 and $F;
    if (gfx[2].buffer[pos] or buffer_color[color + $10]) then
    begin
      nchar := (atrib and $FF) + ((atrib2 and $C0) shl 2);
      put_gfx_trans_flip(x shl 5, y shl 5, nchar, color shl 4, 3, 2, (atrib2 and $20) <> 0, (atrib2 and $10) <> 0);
      gfx[2].buffer[pos] := false;
    end;
  end;
  scroll_x_y(2, 4, scroll_bg_x and $1F, scroll_bg_y and $1F);
  scroll_x_y(3, 4, scroll_fg_x and $1F, scroll_fg_y and $1F);
  for f := $3FF downto 0 do
  begin
    nchar := buffer_sprites_w[f * 4] and $FFF;
    atrib := buffer_sprites_w[(f * 4) + 1];
    y := 240 - buffer_sprites_w[(f * 4) + 2];
    x := buffer_sprites_w[(f * 4) + 3];
    color := (atrib and $3C) shl 2;
    put_gfx_sprite(nchar, color + $200, (atrib and 2) <> 0, (atrib and 1) <> 0, 3);
    update_gfx_sprite(x, y, 4, 3);
  end;
  // text
  for f := 0 to $3FF do
  begin
    atrib := txt_ram[f];
    atrib2 := atrib shr 8;
    color := atrib2 and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := (atrib and $FF) + ((atrib2 and $C0) shl 2) + ((atrib2 and $20) shl 5);
    put_gfx_trans_flip(x*8,y*8,nchar,(color shl 2)+768,1,0,false,(atrib2 and $10)<>0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // front
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(0, 16, 256, 224, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_superduck;
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
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.up[0] then
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
    if p_contrls.map_arcade.but3[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.up[1] then
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
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.but3[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // system
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FEFF)
    else
      marcade.in1 := (marcade.in1 or $100);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $200);
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $7FFF)
    else
      marcade.in1 := (marcade.in1 or $8000);
  end;
end;

procedure superduck_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
 for f:=0 to 261 do begin
    events_superduck;
    case f of
      0:marcade.in1:=marcade.in1 or $400;
      246:begin
            marcade.in1:=marcade.in1 and $fbff;
            m68000_0.irq[2]:=HOLD_LINE;
            update_video_superduck;
            copymemory(@buffer_sprites_w,@sprite_ram,$1000*2);
          end;
    end;
    //main
    m68000_0.run(frame_main);
    frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
    //sound
    z80_0.run(frame_snd);
    frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
 end;
 video_sync;
    end
    else
      pause_action;
  end;
end;

function superduck_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      superduck_getword := rom[direccion shr 1];
    $FE0000 .. $FE1FFF:
      superduck_getword := sprite_ram[(direccion and $1FFF) shr 1];
    $FE4000:
      superduck_getword := marcade.in0;
    $FE4002:
      superduck_getword := marcade.in1;
    $FE4004:
      superduck_getword := marcade.dswa;
    $FEC000 .. $FECFFF:
      superduck_getword := txt_ram[(direccion and $FFF) shr 1];
    $FF0000 .. $FF3FFF:
      superduck_getword := bg_ram[(direccion and $3FFF) shr 1];
    $FF4000 .. $FF7FFF:
      superduck_getword := fg_ram[(direccion and $3FFF) shr 1];
    $FF8000 .. $FF87FF:
      superduck_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $FFC000 .. $FFFFFF:
      superduck_getword := ram[(direccion and $3FFF) shr 1];
  end;
end;

procedure superduck_putword(direccion: dword; valor: word);
var
  tempw: word;
  procedure change_color(pos, data: word);
  var
    color: tcolor;
  begin
    color.r := pal5bit(((data shr 8) and $F) or ((data shr 10) and $10));
    color.g := pal5bit(((data shr 4) and $F) or ((data shr 9) and $10));
    color.b := pal5bit(((data shr 0) and $F) or ((data shr 8) and $10));
    set_pal_color(color, pos);
    case pos of
      0 .. 255:
        buffer_color[(pos shr 4) + $10] := true;
      256 .. 511:
        buffer_color[((pos shr 4) and $F) + $20] := true;
      768 .. 831:
        buffer_color[(pos shr 2) and $F] := true;
    end;
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ;
    $FE0000 .. $FE1FFF:
      sprite_ram[(direccion and $1FFF) shr 1] := valor;
    $FE4000:
      ;
    $FE4002:
      begin
        sound_latch := valor shr 8;
        z80_0.change_irq(ASSERT_LINE);
      end;
    $FE4004:
      ;
    $FE8000 .. $FE8007:
      case ((direccion and 7) shr 1) of
        0:
          begin
            tempw := valor and $FFF;
            if scroll_bg_x <> tempw then
            begin
              if abs((scroll_bg_x and $FE0) - (tempw and $FE0)) > 31 then
                fillchar(gfx[1].buffer, $2000, 1);
              scroll_bg_x := tempw;
            end;
          end;
        1:
          begin
            tempw := 1792 - (valor and $7FF);
            if scroll_bg_y <> tempw then
            begin
              if abs((scroll_bg_y and $7E0) - (tempw and $7E0)) > 31 then
                fillchar(gfx[1].buffer, $2000, 1);
              scroll_bg_y := tempw;
            end;
          end;
        2:
          begin
            tempw := valor and $FFF;
            if scroll_fg_x <> tempw then
            begin
              if abs((scroll_fg_x and $FE0) - (tempw and $FE0)) > 31 then
                fillchar(gfx[2].buffer, $2000, 1);
              scroll_fg_x := tempw;
            end;
          end;
        3:
          begin
            tempw := 1792 - (valor and $7FF);
            if scroll_fg_y <> tempw then
            begin
              if abs((scroll_fg_y and $7E0) - (tempw and $7E0)) > 31 then
                fillchar(gfx[2].buffer, $2000, 1);
              scroll_fg_y := tempw;
            end;
          end;
      end;
    $FEC000 .. $FECFFF:
      if txt_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        txt_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $FF0000 .. $FF3FFF:
      if bg_ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $3FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $3FFF) shr 1] := true;
      end;
    $FF4000 .. $FF7FFF:
      if fg_ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        fg_ram[(direccion and $3FFF) shr 1] := valor;
        gfx[2].buffer[(direccion and $3FFF) shr 1] := true;
      end;
    $FF8000 .. $FF87FF:
      if (buffer_paleta[(direccion and $7FF) shr 1] <> valor) then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color((direccion and $7FF) shr 1, valor);
      end;
    $FFC000 .. $FFFFFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function superduck_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      superduck_snd_getbyte := mem_snd[direccion];
    $9800:
      superduck_snd_getbyte := oki_6295_0.read;
    $A000:
      begin
        superduck_snd_getbyte := sound_latch;
        z80_0.change_irq(CLEAR_LINE);
      end;
  end;
end;

procedure superduck_snd_putbyte(direccion: word; valor: byte);
var
  ptemp: pbyte;
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $9000:
      begin
        ptemp := oki_6295_0.get_rom_addr;
        copymemory(@ptemp[$20000], @oki_rom[valor and 3, 0], $20000);
      end;
    $9800:
      oki_6295_0.write(valor);
  end;
end;

procedure superduck_sound_update;
begin
  oki_6295_0.update;
end;

// Main
procedure reset_superduck;
begin
  m68000_0.reset;
  z80_0.reset;
  oki_6295_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=z80_0.tframes;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  scroll_fg_x := 0;
  scroll_fg_y := 0;
  scroll_bg_x := 0;
  scroll_bg_y := 0;
  sound_latch := 0;
end;

function start_superduck: boolean;
var
  memoria_temp: pbyte;
  f: byte;
const
  pf_x: array [0 .. 31] of dword = (0, 1, 2, 3, 8, 9, 10, 11, (4 * 2 * 2 * 32) + 0, (4 * 2 * 2 * 32) + 1, (4 * 2 * 2 * 32) + 2, (4 * 2 * 2 * 32) + 3, (4 * 2 * 2 * 32) + 8, (4 * 2 * 2 * 32) + 9,
    (4 * 2 * 2 * 32) + 10, (4 * 2 * 2 * 32) + 11, (4 * 2 * 2 * 64) + 0, (4 * 2 * 2 * 64) + 1, (4 * 2 * 2 * 64) + 2, (4 * 2 * 2 * 64) + 3, (4 * 2 * 2 * 64) + 8, (4 * 2 * 2 * 64) + 9,
    (4 * 2 * 2 * 64) + 10, (4 * 2 * 2 * 64) + 11, (4 * 2 * 2 * 96) + 0, (4 * 2 * 2 * 96) + 1, (4 * 2 * 2 * 96) + 2, (4 * 2 * 2 * 96) + 3, (4 * 2 * 2 * 96) + 8, (4 * 2 * 2 * 96) + 9,
    (4 * 2 * 2 * 96) + 10, (4 * 2 * 2 * 96) + 11);
  pf_y: array [0 .. 31] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16, 16 * 16, 17 * 16, 18 * 16,
    19 * 16, 20 * 16, 21 * 16, 22 * 16, 23 * 16, 24 * 16, 25 * 16, 26 * 16, 27 * 16, 28 * 16, 29 * 16, 30 * 16, 31 * 16);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 4 * 16 + 0, 8 * 4 * 16 + 1, 8 * 4 * 16 + 2, 8 * 4 * 16 + 3, 8 * 4 * 16 + 4, 8 * 4 * 16 + 5, 8 * 4 * 16 + 6, 8 * 4 * 16 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8 * 4, 1 * 8 * 4, 2 * 8 * 4, 3 * 8 * 4, 4 * 8 * 4, 5 * 8 * 4, 6 * 8 * 4, 7 * 8 * 4, 8 * 8 * 4, 9 * 8 * 4, 10 * 8 * 4, 11 * 8 * 4, 12 * 8 * 4, 13 * 8 * 4,
    14 * 8 * 4, 15 * 8 * 4);
begin
  machine_calls.general_loop := superduck_loop;
  machine_calls.reset := reset_superduck;
  machine_calls.fps_max := 6000000 / 384 / 262;
  start_superduck := false;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256, true);
  screen_init(2, 288, 288);
  screen_mod_scroll(2, 288, 256, 255, 288, 256, 255);
  screen_init(3, 288, 288, true);
  screen_mod_scroll(3, 288, 256, 255, 288, 256, 255);
  screen_init(4, 512, 512, false, true);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 262);
  m68000_0.change_ram16_calls(superduck_getword, superduck_putword);
  // Sound CPU
  z80_0 := cpu_z80.create(2000000, 262);
  z80_0.change_ram_calls(superduck_snd_getbyte, superduck_snd_putbyte);
  z80_0.init_sound(superduck_sound_update);
  getmem(memoria_temp, $1000000);
  // Sound Chips
  oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH);
  if not(roms_load(memoria_temp, superduck_oki)) then
    exit;
  copymemory(oki_6295_0.get_rom_addr, memoria_temp, $40000);
  for f := 0 to 3 do
    copymemory(@oki_rom[f, 0], @memoria_temp[$20000 + (f * $20000)], $20000);
  // cargar roms
  if not(roms_load16w(@rom, superduck_rom)) then
    exit;
  // cargar sonido
  if not(roms_load(@mem_snd, superduck_sound)) then
    exit;
  // convertir chars
  if not(roms_load(memoria_temp, superduck_char)) then
    exit;
  init_gfx(0, 8, 8, $800);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 128, 4, 0);
  convert_gfx(0, 0, memoria_temp, @pf_x, @pf_y, false, false);
  // convertir bg
  if not(roms_load(memoria_temp, superduck_bg)) then
    exit;
  init_gfx(1, 32, 32, $400);
  gfx_set_desc_data(4, 0, 256 * 8, ($400 * 8 * 256) + 4, $400 * 8 * 256, 4, 0);
  convert_gfx(1, 0, memoria_temp, @pf_x, @pf_y, false, false);
  // convertir fg
  if not(roms_load(memoria_temp, superduck_fg)) then
    exit;
  init_gfx(2, 32, 32, $400);
  gfx[2].trans[$F] := true;
  gfx_set_desc_data(4, 0, 256 * 8, ($400 * 8 * 256) + 4, $400 * 8 * 256, 4, 0);
  convert_gfx(2, 0, memoria_temp, @pf_x, @pf_y, false, false);
  // convertir sprites
  if not(roms_load32b_b(memoria_temp, superduck_sprites)) then
    exit;
  init_gfx(3, 16, 16, $1000);
  gfx[3].trans[15] := true;
  gfx_set_desc_data(4, 0, 16 * 16 * 4, 0, 8, 16, 24);
  convert_gfx(3, 0, memoria_temp, @ps_x, @ps_y, false, false);
  freemem(memoria_temp);
  // DIP
  marcade.dswa := $FFBF;
marcade.dswa_val2:=@superduck_dip;
  // final
  start_superduck := true;
end;

end.
