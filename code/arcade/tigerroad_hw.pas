unit tigerroad_hw;

interface

uses
  WinApi.Windows,
  m68000,
  mcs51,
  main_engine,
  controls_engine,
  gfx_engine,
  nz80,
  ym_2203,
  rom_engine,
  pal_engine,
  sound_engine;

function start_tigerroad: boolean;

implementation

const
  // Tiger Road
  tigeroad_rom: array [0 .. 1] of tipo_roms = ((n: 'tre_02.6j'; l: $20000; p: 0; crc: $C394ADD0), (n: 'tre_04.6k'; l: $20000; p: 1; crc: $73BFBF4A));
  tigeroad_sound: tipo_roms = (n: 'tru_05.12k'; l: $8000; p: 0; crc: $F9A7C9BF);
  tigeroad_char: tipo_roms = (n: 'tr_01.10d'; l: $8000; p: 0; crc: $74A9F08C);
  tigeroad_fondo: array [0 .. 7] of tipo_roms = ((n: 'tr-01a.3f'; l: $20000; p: 0; crc: $A8AA2E59), (n: 'tr-04a.3h'; l: $20000; p: $20000; crc: $8863A63C), (n: 'tr-02a.3j'; l: $20000; p: $40000;
    crc: $1A2C5F89), (n: 'tr-05.3l'; l: $20000; p: $60000; crc: $5BF453B3), (n: 'tr-03a.2f'; l: $20000; p: $80000; crc: $1E0537EA), (n: 'tr-06a.2h'; l: $20000; p: $A0000; crc: $B636C23A),
    (n: 'tr-07a.2j'; l: $20000; p: $C0000; crc: $5F907D4D), (n: 'tr_08.2l'; l: $20000; p: $E0000; crc: $ADEE35E2));
  tigeroad_fondo_rom: tipo_roms = (n: 'tr_13.7l'; l: $8000; p: 0; crc: $A79BE1EB);
  tigeroad_sprites: array [0 .. 3] of tipo_roms = ((n: 'tr-09a.3b'; l: $20000; p: 0; crc: $3D98AD1E), (n: 'tr-10a.2b'; l: $20000; p: $20000; crc: $8F6F03D7), (n: 'tr-11a.3d'; l: $20000; p: $40000;
    crc: $CD9152E5), (n: 'tr-12a.2d'; l: $20000; p: $60000; crc: $7D8A99D0));
  // F1 Dream
  f1dream_rom: array [0 .. 1] of tipo_roms = ((n: '06j_02.bin'; l: $20000; p: 0; crc: $3C2EC697), (n: '06k_03.bin'; l: $20000; p: 1; crc: $85EBAD91));
  f1dream_sound: tipo_roms = (n: '12k_04.bin'; l: $8000; p: 0; crc: $4B9A7524);
  f1dream_mcu: tipo_roms = (n: '8751.mcu'; l: $1000; p: 0; crc: $C8E6075C);
  f1dream_char: tipo_roms = (n: '10d_01.bin'; l: $8000; p: 0; crc: $361CAF00);
  f1dream_fondo: array [0 .. 5] of tipo_roms = ((n: '03f_12.bin'; l: $10000; p: 0; crc: $BC13E43C), (n: '01f_10.bin'; l: $10000; p: $10000; crc: $F7617AD9), (n: '03h_14.bin'; l: $10000; p: $20000;
    crc: $E33CD438), (n: '02f_11.bin'; l: $10000; p: $30000; crc: $4AA49CD7), (n: '17f_09.bin'; l: $10000; p: $40000; crc: $CA622155), (n: '02h_13.bin'; l: $10000; p: $50000; crc: $2A63961E));
  f1dream_fondo_rom: tipo_roms = (n: '07l_15.bin'; l: $8000; p: 0; crc: $978758B7);
  f1dream_sprites: array [0 .. 3] of tipo_roms = ((n: '03b_06.bin'; l: $10000; p: 0; crc: $5E54E391), (n: '02b_05.bin'; l: $10000; p: $10000; crc: $CDD119FD), (n: '03d_08.bin'; l: $10000; p: $20000;
    crc: $811F2E22), (n: '02d_07.bin'; l: $10000; p: $30000; crc: $AA9A1233));
        tigeroad_dip_a:array [0..8] of def_dip2=(
        (mask:7;name:'Coin A';number:8;val8:(0,1,2,7,6,5,4,3);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$38;name:'Coin B';number:8;val8:(0,8,$10,$38,$30,$28,$20,$18);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$80;name:'Flip Screen';number:2;val2:($80,0);name2:('Off','On')),
        (mask:$300;name:'Lives';number:4;val4:($300,$200,$100,0);name4:('3','4','5','7')),
        (mask:$400;name:'Cabinet';number:2;val2:(0,$400);name2:('Upright','Cocktail')),
        (mask:$1800;name:'Bonus Life';number:4;val4:($1800,$1000,$800,0);name4:('20K 70K 70K','20K 80K 80K','30K 80K 80K','30K 90K 90K')),
        (mask:$6000;name:'Difficulty';number:4;val4:($2000,$4000,$6000,0);name4:('Very Easy (Level 0)','Easy (Level 10)','Normal (Level 20)','Difficult (Level 30)')),
        (mask:$8000;name:'Allow Continue';number:2;val2:(0,$8000);name2:('No','Yes')),());
        f1dream_dip_a:array [0..9] of def_dip2=(
        (mask:7;name:'Coin A';number:8;val8:(0,1,2,7,6,5,4,3);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$38;name:'Coin B';number:8;val8:(0,8,$10,$38,$30,$28,$20,$18);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$80;name:'Flip Screen';number:2;val2:($80,0);name2:('Off','On')),
        (mask:$300;name:'Lives';number:4;val4:($300,$200,$100,0);name4:('3','4','5','7')),
        (mask:$400;name:'Cabinet';number:2;val2:(0,$400);name2:('Upright','Cocktail')),
        (mask:$1800;name:'F1 Up Point';number:4;val4:($1800,$1000,$800,0);name4:('12','16','18','20')),
        (mask:$2000;name:'Difficulty';number:2;val2:($2000,0);name2:('Normal','Difficult')),
        (mask:$4000;name:'Version';number:2;val2:(0,$4000);name2:('World','Japan')),
        (mask:$8000;name:'Allow Continue';number:2;val2:(0,$8000);name2:('No','Yes')),());

var
 scroll_x,scroll_y:word;
  rom: array [0 .. $1FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  ram2: array [0 .. $803] of word;
  video_ram: array [0 .. $3FF] of word;
  fondo_rom: array [0 .. $7FFF] of byte;
  pintar_fondo: boolean;
  old_p3, fondo_bank, sound_latch: byte;

procedure update_video_tigeroad;
var
  f, color, x, y, nchar, atrib: word;
  atrib2: byte;
  procedure draw_font;
  var
    nchar, color, pos: word;
  x,y,f,atrib,sx,sy:byte;
  begin
    for f := 0 to $50 do
    begin
      x := f div 9;
      y := f mod 9;
      sx := (x + ((scroll_x and $FE0) shr 5)) and $7F;
      sy := (y + ((scroll_y and $FE0) shr 5)) and $7F;
      pos := (((sx and 7) shl 1) + (((127 - sy) and 7) shl 4) + ((sx shr 3) shl 7) + (((127 - sy) shr 3) shl 11)) and $7FFF;
      atrib := fondo_rom[pos + 1];
  nchar:=fondo_rom[pos]+((atrib and $c0) shl 2)+(fondo_bank shl 10);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x shl 5, y shl 5, nchar, color, 2, 1, (atrib and $20) <> 0, false);
      if (atrib and $10) <> 0 then
        put_gfx_trans_flip(x shl 5, y shl 5, nchar, color, 4, 1, (atrib and $20) <> 0, false)
      else
        put_gfx_block_trans(x shl 5, y shl 5, 4, 32, 32);
    end;
    pintar_fondo := false;
  end;

begin
  // background
  if pintar_fondo then
    draw_font;
  scroll_x_y(2, 3, scroll_x and $1F, scroll_y and $1F);
  // sprites
  for f := $9F downto 0 do
  begin
    nchar := buffer_sprites_w[f * 4];
    atrib := buffer_sprites_w[(f * 4) + 1];
    y := 240 - buffer_sprites_w[(f * 4) + 2];
    x := buffer_sprites_w[(f * 4) + 3];
    color := (atrib and $3C) shl 2;
    put_gfx_sprite(nchar, color + $100, (atrib and 2) <> 0, (atrib and 1) <> 0, 2);
    update_gfx_sprite(x, y, 3, 2);
  end;
  scroll_x_y(4, 3, scroll_x and $1F, scroll_y and $1F);
  // foreground
  for f := 0 to $3FF do
  begin
    atrib := video_ram[f];
    atrib2 := atrib shr 8;
    color := atrib2 and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := (atrib and $FF) + ((atrib2 and $C0) shl 2) + ((atrib2 and $20) shl 5);
    put_gfx_trans_flip(x*8,y*8,nchar,(color shl 2)+512,1,0,false,(atrib2 and $10)<>0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  update_final_piece(0, 16, 256, 224, 3);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_tigeroad;
begin
  if event.arcade then
  begin
    // P1 P2
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
    // System
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

procedure tigeroad_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
  for f:=0 to $ff do begin
    if f=240 then begin
      update_video_tigeroad;
      m68000_0.irq[2]:=HOLD_LINE;
      copymemory(@buffer_sprites_w,@ram2,$280*2);
    end;
    //Main CPU
    m68000_0.run(frame_main);
    frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
    //Sound CPU
    z80_0.run(frame_snd);
    frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
  end;
      events_tigeroad;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function tigeroad_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      tigeroad_getword := rom[direccion shr 1];
    $FE4000:
      tigeroad_getword := marcade.in0;
    $FE4002:
      tigeroad_getword := marcade.in1;
    $FE4004:
      tigeroad_getword := marcade.dswa;
    $FE0800 .. $FE1807:
      tigeroad_getword := ram2[(direccion - $FE0800) shr 1];
    $FEC000 .. $FEC7FF:
      tigeroad_getword := video_ram[(direccion and $7FF) shr 1];
    $FF8200 .. $FF867F:
      tigeroad_getword := buffer_paleta[(direccion - $FF8200) shr 1];
    $FFC000 .. $FFFFFF:
      tigeroad_getword := ram[(direccion and $3FFF) shr 1];
  end;
end;

procedure change_color(tmp_color, numero: word);
var
  color: tcolor;
begin
  color.r := pal4bit(tmp_color shr 8);
  color.g := pal4bit(tmp_color shr 4);
  color.b := pal4bit(tmp_color);
  set_pal_color(color, numero);
  case numero of
    0 .. $FF:
      pintar_fondo := true;
    512 .. 575:
      buffer_color[(numero shr 2) and $F] := true;
  end;
end;

procedure tigeroad_putword(direccion: dword; valor: word);
var
  tempw: word;
  bank: byte;
begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $FE0800 .. $FE1807:
      ram2[(direccion - $FE0800) shr 1] := valor;
    $FE4000:
      begin // video control
        bank := (valor shr 10) and 1;
        if (fondo_bank <> bank) then
        begin
          pintar_fondo := true;
          fondo_bank := bank;
        end;
        main_screen.flip_main_screen := (valor and $200) <> 0;
      end;
    $FE4002:
      sound_latch := valor shr 8;
    $FE8000:
      if scroll_x <> (valor and $FFF) then
      begin
        if abs((scroll_x and $FE0) - (valor and $FE0)) > 31 then
          pintar_fondo := true;
        scroll_x := valor and $FFF;
      end;
    $FE8002:
      begin
        tempw := (-valor - 256) and $FFF;
        if scroll_y <> tempw then
        begin
          if abs((scroll_y and $FE0) - (tempw and $FE0)) > 31 then
            pintar_fondo := true;
          scroll_y := tempw;
        end;
      end;
    $FEC000 .. $FEC7FF:
      if video_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        video_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $FF8200 .. $FF867F:
      begin
        tempw := (direccion - $FF8200) shr 1;
        if buffer_paleta[tempw] <> valor then
        begin
          buffer_paleta[tempw] := valor;
          change_color(valor, tempw);
        end;
      end;
    $FFC000 .. $FFFFFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function tigeroad_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $C7FF:
      tigeroad_snd_getbyte := mem_snd[direccion];
    $8000:
      tigeroad_snd_getbyte := ym2203_0.status;
    $8001:
      tigeroad_snd_getbyte := ym2203_0.read;
    $A000:
      tigeroad_snd_getbyte := ym2203_1.status;
    $A001:
      tigeroad_snd_getbyte := ym2203_1.read;
    $E000:
      tigeroad_snd_getbyte := sound_latch;
  end;
end;

procedure tigeroad_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000:
      ym2203_0.Control(valor);
    $8001:
      ym2203_0.Write(valor);
    $A000:
      ym2203_1.Control(valor);
    $A001:
      ym2203_1.Write(valor);
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
  end;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

procedure tigeroad_sound_update;
begin
  ym2203_0.Update;
  ym2203_1.Update;
end;

// F1dream
procedure f1dream_putword(direccion: dword; valor: word);
var
  tempw: word;
  bank: byte;
begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $FE0800 .. $FE1807:
      ram2[(direccion - $FE0800) shr 1] := valor;
    $FE4000:
      begin // video control
        bank := (valor shr 10) and 1;
        if (fondo_bank <> bank) then
        begin
          fondo_bank := bank;
          pintar_fondo := true;
        end;
        main_screen.flip_main_screen := (valor and $200) <> 0;
      end;
    $FE4002:
      begin
        mcs51_0.change_irq0(HOLD_LINE);
        m68000_0.change_halt(ASSERT_LINE);
      end;
    $FE8000:
      if scroll_x <> (valor and $FFF) then
      begin
        if abs((scroll_x and $FE0) - (valor and $FE0)) > 31 then
          pintar_fondo := true;
        scroll_x := valor and $FFF;
      end;
    $FE8002:
      begin
        tempw := (-valor - 256) and $FFF;
        if scroll_y <> tempw then
        begin
          if abs((scroll_y and $FE0) - (tempw and $FE0)) > 31 then
            pintar_fondo := true;
          scroll_y := tempw;
        end;
      end;
    $FEC000 .. $FEC7FF:
      if video_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        video_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $FF8200 .. $FF867F:
      begin
        tempw := (direccion - $FF8200) shr 1;
        if buffer_paleta[tempw] <> valor then
        begin
          buffer_paleta[tempw] := valor;
          change_color(valor, tempw);
        end;
      end;
    $FFC000 .. $FFFFFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

procedure out_port1(valor: byte);
begin
  sound_latch := valor;
end;

procedure out_port3(valor: byte);
begin
  if ((old_p3 and $20) <> (valor and $20)) then
  begin
    // toggles at the start and end of interrupt
  end;
  if ((old_p3 and 1) <> (valor and 1)) then
  begin
    // toggles at the end of interrupt
    if ((valor and 1) = 0) then
      m68000_0.change_halt(CLEAR_LINE);
  end;
  old_p3 := valor;
end;

function mcu_ext_ram_read(direccion: word): byte;
begin
  mcu_ext_ram_read := ram[$1800 + direccion];
end;

procedure mcu_ext_ram_write(direccion: word; valor: byte);
begin
  ram[$1800 + direccion] := (ram[$1800 + direccion] and $FF00) or valor;
end;

procedure f1dream_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
  for f:=0 to $ff do begin
    if f=240 then begin
      update_video_tigeroad;
      m68000_0.irq[2]:=HOLD_LINE;
      copymemory(@buffer_sprites_w,@ram2,$280*2);
    end;
    //Main CPU
    m68000_0.run(frame_main);
    frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
    //Sound CPU
    z80_0.run(frame_snd);
    frame_snd:=frame_snd+z80_0.tframes-z80_0.contador;
    //mcu
    mcs51_0.run(frame_mcu);
    frame_mcu:=frame_mcu+mcs51_0.tframes-mcs51_0.contador;
  end;
      events_tigeroad;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Main
procedure reset_tigeroad;
begin
  m68000_0.reset;
  z80_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=z80_0.tframes;
  ym2203_0.reset;
  ym2203_1.reset;
  if main_vars.machine_type <> 52 then
  begin
    mcs51_0.reset;
  frame_mcu:=mcs51_0.tframes;
 end;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  scroll_x := 0;
  scroll_y := 0;
  pintar_fondo := true;
  fondo_bank := 0;
  sound_latch := 0;
  old_p3 := 0;
end;

function start_tigerroad: boolean;
var
  memory_temp: pbyte;
const
  pb_x: array [0 .. 31] of dword = (0, 1, 2, 3, 8 + 0, 8 + 1, 8 + 2, 8 + 3, 64 * 8 + 0, 64 * 8 + 1, 64 * 8 + 2, 64 * 8 + 3, 64 * 8 + 8 + 0, 64 * 8 + 8 + 1, 64 * 8 + 8 + 2, 64 * 8 + 8 + 3,
    2 * 64 * 8 + 0, 2 * 64 * 8 + 1, 2 * 64 * 8 + 2, 2 * 64 * 8 + 3, 2 * 64 * 8 + 8 + 0, 2 * 64 * 8 + 8 + 1, 2 * 64 * 8 + 8 + 2, 2 * 64 * 8 + 8 + 3, 3 * 64 * 8 + 0, 3 * 64 * 8 + 1, 3 * 64 * 8 + 2,
    3 * 64 * 8 + 3, 3 * 64 * 8 + 8 + 0, 3 * 64 * 8 + 8 + 1, 3 * 64 * 8 + 8 + 2, 3 * 64 * 8 + 8 + 3);
  pb_y: array [0 .. 31] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16, 16 * 16, 17 * 16, 18 * 16,
    19 * 16, 20 * 16, 21 * 16, 22 * 16, 23 * 16, 24 * 16, 25 * 16, 26 * 16, 27 * 16, 28 * 16, 29 * 16, 30 * 16, 31 * 16);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);

  procedure tiger_road_chars;
  begin
    init_gfx(0, 8, 8, $800);
    gfx[0].trans[3] := true;
    gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
    convert_gfx(0, 0, memory_temp, @pb_x, @pb_y, false, false);
  end;

  procedure tiger_road_tiles(nchars: word);
  var
    f: byte;
  begin
    init_gfx(1, 32, 32, nchars);
    for f := 0 to 8 do
      gfx[1].trans[f] := true;
    gfx_set_desc_data(4, 0, 256 * 8, nchars * 256 * 8 + 4, nchars * 256 * 8 + 0, 4, 0);
    convert_gfx(1, 0, memory_temp, @pb_x, @pb_y, false, false);
  end;

  procedure tiger_road_sprites(nchars: word);
  begin
    init_gfx(2, 16, 16, nchars);
    gfx[2].trans[15] := true;
    gfx_set_desc_data(4, 0, 32 * 8, nchars * 32 * 8 * 3, nchars * 32 * 8 * 2, nchars * 32 * 8 * 1, nchars * 32 * 8 * 0);
    convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.reset := reset_tigeroad;
  machine_calls.fps_max := 60.08;
  start_tigerroad := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 288, 288);
  screen_mod_scroll(2, 288, 256, 255, 288, 256, 255);
  screen_init(3, 512, 512, false, true);
  screen_init(4, 288, 288, true);
  screen_mod_scroll(4, 288, 256, 255, 288, 256, 255);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, $100);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, $100);
  z80_0.change_ram_calls(tigeroad_snd_getbyte, tigeroad_snd_putbyte);
  z80_0.init_sound(tigeroad_sound_update);
  // sound chips
  ym2203_0 := ym2203_chip.create(3579545, 0.5, 1);
  ym2203_0.change_irq_calls(snd_irq);
  ym2203_1 := ym2203_chip.create(3579545, 0.5, 1);
  getmem(memory_temp, $100000);
  case main_vars.machine_type of
    52:
      begin // Tiger Road
        machine_calls.general_loop := tigeroad_loop;
        m68000_0.change_ram16_calls(tigeroad_getword, tigeroad_putword);
        if not(roms_load16w(@rom, tigeroad_rom)) then
          exit;
        if not(roms_load(@mem_snd, tigeroad_sound)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, tigeroad_char)) then
          exit;
        tiger_road_chars;
        // background
        if not(roms_load(@fondo_rom, tigeroad_fondo_rom)) then
          exit;
        if not(roms_load(memory_temp, tigeroad_fondo)) then
          exit;
        tiger_road_tiles($800);
        // sprites
        if not(roms_load(memory_temp, tigeroad_sprites)) then
          exit;
        tiger_road_sprites($1000);
        // DIP
        marcade.dswa := $FBFF;
        marcade.dswa_val2:=@tigeroad_dip_a;
      end;
    53:
      begin // F1 Dream
        machine_calls.general_loop := f1dream_loop;
        m68000_0.change_ram16_calls(tigeroad_getword, f1dream_putword);
        if not(roms_load16w(@rom, f1dream_rom)) then
          exit;
        if not(roms_load(@mem_snd, f1dream_sound)) then
          exit;
        // MCU
        mcs51_0 := cpu_mcs51.create(I8X51, 10000000, 256);
        mcs51_0.change_io_calls(nil, nil, nil, nil, nil, out_port1, nil, out_port3);
        mcs51_0.change_ram_calls(mcu_ext_ram_read, mcu_ext_ram_write);
        if not(roms_load(mcs51_0.get_rom_addr, f1dream_mcu)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, f1dream_char)) then
          exit;
        tiger_road_chars;
        // background
        if not(roms_load(@fondo_rom, f1dream_fondo_rom)) then
          exit;
        if not(roms_load(memory_temp, f1dream_fondo)) then
          exit;
        tiger_road_tiles($300);
        // sprites
        if not(roms_load(memory_temp, f1dream_sprites)) then
          exit;
        tiger_road_sprites($800);
        // DIP
        marcade.dswa := $BBFF;
        marcade.dswa_val2:=@f1dream_dip_a;
      end;
  end;
  // final
  freemem(memory_temp);
  reset_tigeroad;
  start_tigerroad := true;
end;

end.
