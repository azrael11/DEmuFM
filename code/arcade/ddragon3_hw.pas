unit ddragon3_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine,
  oki6295,
  qsnapshot;

function start_doubledragon3: boolean;

implementation

const
        //Double Dragon 3
        ddragon3_rom:array[0..1] of tipo_roms=(
        (n:'30a14-0.ic78';l:$40000;p:1;crc:$f42fe016),(n:'30a15-0.ic79';l:$20000;p:$0;crc:$ad50e92c));
        ddragon3_sound:tipo_roms=(n:'30a13-0.ic43';l:$10000;p:0;crc:$1e974d9b);
        ddragon3_oki:tipo_roms=(n:'30j-8.ic73';l:$80000;p:0;crc:$c3ad40f3);
        ddragon3_sprites:array[0..7] of tipo_roms=(
        (n:'30j-3.ic9';l:$80000;p:0;crc:$b3151871),(n:'30a12-0.ic8';l:$10000;p:$80000;crc:$20d64bea),
        (n:'30j-2.ic11';l:$80000;p:$100000;crc:$41c6fb08),(n:'30a11-0.ic10';l:$10000;p:$180000;crc:$785d71b0),
        (n:'30j-1.ic13';l:$80000;p:$200000;crc:$67a6f114),(n:'30a10-0.ic12';l:$10000;p:$280000;crc:$15e43d12),
        (n:'30j-0.ic15';l:$80000;p:$300000;crc:$f15dafbe),(n:'30a9-0.ic14';l:$10000;p:$380000;crc:$5a47e7a4));
        ddragon3_bg:array[0..3] of tipo_roms=(
        (n:'30j-7.ic4';l:$40000;p:0;crc:$89d58d32),(n:'30j-6.ic5';l:$40000;p:$1;crc:$9bf1538e),
        (n:'30j-5.ic6';l:$40000;p:$80000;crc:$8f671a62),(n:'30j-4.ic7';l:$40000;p:$80001;crc:$0f74ea1c));
        //Combat tribe
        ctribe_rom:array[0..2] of tipo_roms=(
        (n:'28a16-2.ic26';l:$20000;p:1;crc:$c46b2e63),(n:'28a15-2.ic25';l:$20000;p:$0;crc:$3221c755),
        (n:'28j17-0.104';l:$10000;p:$40001;crc:$8c2c6dbd));
        ctribe_sound:tipo_roms=(n:'28a10-0.ic89';l:$8000;p:0;crc:$4346de13);
        ctribe_oki:array[0..1] of tipo_roms=(
        (n:'28j9-0.ic83';l:$20000;p:0;crc:$f92a7f4a),(n:'28j8-0.ic82';l:$20000;p:$20000;crc:$1a3a0b39));
        ctribe_sprites:array[0..7] of tipo_roms=(
        (n:'28j3-0.ic77';l:$80000;p:0;crc:$1ac2a461),(n:'28a14-0.ic60';l:$10000;p:$80000;crc:$972faddb),
        (n:'28j2-0.ic78';l:$80000;p:$100000;crc:$8c796707),(n:'28a13-0.ic61';l:$10000;p:$180000;crc:$eb3ab374),
        (n:'28j1-0.ic97';l:$80000;p:$200000;crc:$1c9badbd),(n:'28a12-0.ic85';l:$10000;p:$280000;crc:$c602ac97),
        (n:'28j0-0.ic98';l:$80000;p:$300000;crc:$ba73c49e),(n:'28a11-0.ic86';l:$10000;p:$380000;crc:$4da1d8e5));
        ctribe_bg:array[0..3] of tipo_roms=(
        (n:'28j7-0.ic11';l:$40000;p:0;crc:$a8b773f1),(n:'28j6-0.ic13';l:$40000;p:$1;crc:$617530fc),
        (n:'28j5-0.ic12';l:$40000;p:$80000;crc:$cef0a821),(n:'28j4-0.ic14';l:$40000;p:$80001;crc:$b84fda09));
        //DIP
        ddragon3_dip_a:array [0..9] of def_dip2=(
        (mask:3;name:'Coinage';number:4;val4:(0,1,3,2);name4:('3C 1C','2C 1C','1C 1C','1C 2C')),
        (mask:$10;name:'Continue Discount';number:2;val2:($10,0);name2:('Off','On')),
        (mask:$20;name:'Demo Sounds';number:2;val2:(0,$20);name2:('Off','On')),
        (mask:$40;name:'Flip Screen';number:2;val2:($40,0);name2:('Off','On')),
        (mask:$300;name:'Difficulty';number:4;val4:($200,$300,$100,0);name4:('Easy','Normal','Hard','Very Hard')),
        (mask:$400;name:'Player Vs. Player Damage';number:2;val2:($400,0);name2:('Off','On')),
        (mask:$2000;name:'Stage Clear Energy';number:2;val2:(0,$2000);name2:('0','50')),
        (mask:$4000;name:'Starting Energy';number:2;val2:(0,$4000);name2:('200','230')),
        (mask:$8000;name:'Players';number:2;val2:($8000,0);name2:('2','3')),());
        ctribe_dip_a:array [0..3] of def_dip2=(
        (mask:$300;name:'Coinage';number:4;val4:(0,$100,$300,$200);name4:('3C 1C','2C 1C','1C 1C','1C 2C')),
        (mask:$1000;name:'Continue Discount';number:2;val2:($1000,0);name2:('Off','On')),
        (mask:$2000;name:'Demo Sounds';number:2;val2:(0,$2000);name2:('Off','On')),());
        ctribe_dip_b:array [0..4] of def_dip2=(
        (mask:$300;name:'Difficulty';number:4;val4:($200,$300,$100,0);name4:('Easy','Normal','Hard','Very Hard')),
        (mask:$400;name:'Timer Speed';number:2;val2:($400,0);name2:('Normal','Fast')),
        (mask:$800;name:'FBI Logo';number:2;val2:(0,$800);name2:('Off','On')),
        (mask:$2000;name:'Stage Clear Energy';number:2;val2:($2000,0);name2:('0','50')),());
        ctribe_dip_c:array [0..3] of def_dip2=(
        (mask:$100;name:'More Stage Clear Energy';number:2;val2:($100,0);name2:('Off','On')),
        (mask:$200;name:'Players';number:2;val2:($200,0);name2:('2','3')),
        (mask:$1000;name:'Flip Screen';number:2;val2:($1000,0);name2:('Off','On')),());

var
  video_update_dd3: procedure;
  events_update_dd3: procedure;
  vreg, bg_tilebase, fg_scrollx, fg_scrolly, bg_scrollx, bg_scrolly: word;
  mem_oki: array [0 .. $7FFFF] of byte;
  rom: array [0 .. $3FFFF] of word;
  bg_ram: array [0 .. $3FF] of word;
  ram: array [0 .. $1FFF] of word;
  fg_ram, ram2: array [0 .. $7FF] of word;
  sound_latch, vblank: byte;

procedure draw_sprites;
var
  atrib, nchar, color, x, y, count: word;
  f, h: byte;
  { - SPR RAM Format -**
    16 bytes per sprite  (8-bit RAM? only every other byte is used)

    ---- ----  yyyy yyyy  ---- ----  lllF fXYE  ---- ----  nnnn nnnn  ---- ----  NNNN NNNN
    ---- ----  ---- CCCC  ---- ----  xxxx xxxx  ---- ----  ---- ----  ---- ----  ---- ----
    Yy = sprite Y Position
    Xx = sprite X Position
    C  = colour bank
    f  = flip Y
    F  = flip X
    l  = chain sprite
    E  = sprite enable
    Nn = Sprite Number

    other bits unused }
begin
  for f := 0 to $FF do
  begin
    atrib := buffer_sprites_w[(f * 8) + 1];
    if (atrib and 1) <> 0 then
    begin
      x := (buffer_sprites_w[(f * 8) + 5] and $00FF) or ((atrib and $0004) shl 6);
      y := (buffer_sprites_w[f * 8] and $00FF) or ((atrib and $0002) shl 7);
      y := ((256 - y) and $1FF) - 16;
      count := (atrib and $00E0) shr 5;
      nchar := ((buffer_sprites_w[(f * 8) + 2] and $00FF) or
        ((buffer_sprites_w[(f * 8) + 3] and $00FF) shl 8)) and $7FFF;
      color := buffer_sprites_w[(f * 8) + 4] and $000F;
      for h := 0 to count do
      begin
        put_gfx_sprite(nchar + h, color shl 4, atrib and $10 <> 0, atrib and $8 <> 0, 1);
        update_gfx_sprite(x, y - (16 * h), 3, 1);
      end;
    end; // del enable
  end; // del for
end;

procedure draw_video;
var
  f, x, y, nchar, atrib: word;
  color: byte;
begin
  for f := $0 to $3FF do
  begin
    x := f and $1F;
    y := f shr 5;
    // background
    atrib := bg_ram[f];
    color := (atrib and $F000) shr 12;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := (atrib and $0FFF) or ((bg_tilebase and $01) shl 12);
      put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + 512, 1, 0);
      gfx[0].buffer[f] := false;
    end;
    // fg
    atrib := fg_ram[f * 2];
    color := (atrib and $F);
    if (gfx[0].buffer[$400 + f] or buffer_color[$10 + color]) then
    begin
      nchar := fg_ram[(f * 2) + 1] and $1FFF;
      put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + 256, 2, 0, (atrib and $40) <> 0,
        (atrib and $80) <> 0);
      gfx[0].buffer[$400 + f] := false;
    end;
  end;
  fill_full_screen(3, $600);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure update_video_ddragon3;
begin
  draw_video;
  case (vreg and $60) of
    $40:
      begin
        scroll_x_y(1, 3, bg_scrollx, bg_scrolly);
        scroll_x_y(2, 3, fg_scrollx, fg_scrolly);
        draw_sprites;
      end;
    $60:
      begin
        scroll_x_y(2, 3, fg_scrollx, fg_scrolly);
        scroll_x_y(1, 3, bg_scrollx, bg_scrolly);
        draw_sprites;
      end;
  else
    begin
      scroll_x_y(1, 3, bg_scrollx, bg_scrolly);
      draw_sprites;
      scroll_x_y(2, 3, fg_scrollx, fg_scrolly);
    end;
  end;
  update_final_piece(0, 8, 320, 240, 3);
end;

procedure update_video_ctribe;
begin
  draw_video;
  if (vreg and $8) <> 0 then
  begin
    scroll_x_y(2, 3, fg_scrollx, fg_scrolly);
    draw_sprites;
    scroll_x_y(1, 3, bg_scrollx, bg_scrolly);
  end
  else
  begin
    scroll_x_y(1, 3, bg_scrollx, bg_scrolly);
    scroll_x_y(2, 3, fg_scrollx, fg_scrolly);
    draw_sprites;
  end;
  update_final_piece(0, 8, 320, 240, 3);
end;

procedure events_ddragon3;
begin
  if event.arcade then
  begin
    // p1
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
    // p2
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.down[1] then
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
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
  end;
end;

procedure events_ctribe;
begin
  if event.arcade then
  begin
    // p1
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
    // p2
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

procedure ddragon3_loop;
var
  f:word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
 for f:=0 to 271 do begin
    events_update_dd3;
    case f of
        0,16,32,48,64,80,96,112,128,144,160,176,192,108,224,240,256:m68000_0.irq[5]:=ASSERT_LINE;
        8:vblank:=0;
        248:begin
              m68000_0.irq[6]:=ASSERT_LINE;
              vblank:=1;
              video_update_dd3;
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

procedure ddragon3_scroll_io(dir: byte; valor: word);
begin
  case dir of
    0:
      fg_scrollx := valor;
    1:
      fg_scrolly := valor;
    2:
      bg_scrollx := valor;
    3:
      bg_scrolly := valor;
    5:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    6:
      if bg_tilebase <> (valor and $1FF) then
      begin
        bg_tilebase := valor and $1FF;
        fillchar(gfx[0].buffer, $400, 1);
      end;
  end;
end;

procedure ddragon3_io_w(dir: byte; valor: word);
begin
  case dir of
    0:
      vreg := valor and $FF;
    1:
      begin
        sound_latch := valor and $FF;
        z80_0.change_nmi(PULSE_LINE);
      end;
    2, 4:
      m68000_0.irq[6] := CLEAR_LINE;
    3:
      m68000_0.irq[5] := CLEAR_LINE;
  end;
end;

function ddragon3_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      ddragon3_getword := rom[direccion shr 1];
    $80000 .. $80FFF:
      ddragon3_getword := fg_ram[(direccion and $FFF) shr 1];
    $82000 .. $827FF:
      ddragon3_getword := bg_ram[(direccion and $7FF) shr 1];
    $100000:
      ddragon3_getword := marcade.in0;
    $100002:
      ddragon3_getword := marcade.in1;
    $100004:
      ddragon3_getword := marcade.dswa;
    $100006:
      ddragon3_getword := $FFFF; // P3!!
    $140000 .. $1405FF:
      ddragon3_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $180000 .. $180FFF:
      ddragon3_getword := buffer_sprites_w[(direccion and $FFF) shr 1];
    $1C0000 .. $1C3FFF:
      ddragon3_getword := ram[(direccion and $3FFF) shr 1];
  end;
end;

procedure change_color(pos, data: word);
var
  color: tcolor;
begin
  color.b := pal5bit(data shr 10);
  color.g := pal5bit(data shr 5);
  color.r := pal5bit(data);
  set_pal_color(color, pos);
  case pos of
    $100 .. $1FF:
      buffer_color[$10 + ((pos shr 4) and $F)] := true;
    $200 .. $2FF:
      buffer_color[(pos shr 4) and $F] := true;
  end;
end;

procedure ddragon3_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $7FFFF:
      ; // ROM
    $80000 .. $80FFF:
      if fg_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        fg_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[$400 + ((direccion and $FFF) shr 2)] := true;
      end;
    $82000 .. $827FF:
      if bg_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $C0000 .. $C000F:
      ddragon3_scroll_io((direccion and $F) shr 1, valor);
    $100000 .. $10000F:
      ddragon3_io_w((direccion and $F) shr 1, valor);
    $140000 .. $1405FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color((direccion and $7FF) shr 1, valor);
      end;
    $180000 .. $180FFF:
      buffer_sprites_w[(direccion and $FFF) shr 1] := valor;
    $1C0000 .. $1C3FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function ddragon3_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $C7FF:
      ddragon3_snd_getbyte := mem_snd[direccion];
    $C801:
      ddragon3_snd_getbyte := ym2151_0.status;
    $D800:
      ddragon3_snd_getbyte := oki_6295_0.read;
    $E000:
      ddragon3_snd_getbyte := sound_latch;
  end;
end;

procedure ddragon3_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
    $C800:
      ym2151_0.reg(valor);
    $C801:
      ym2151_0.write(valor);
    $D800:
      oki_6295_0.write(valor);
    $E800:
      copymemory(oki_6295_0.get_rom_addr, @mem_oki[(valor and 1) * $40000], $40000);
  end;
end;

// Ctribe
function ctribe_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      ctribe_getword := rom[direccion shr 1];
    $80000 .. $80FFF:
      ctribe_getword := fg_ram[(direccion and $FFF) shr 1];
    $81000 .. $81FFF:
      ctribe_getword := buffer_sprites_w[(direccion and $FFF) shr 1];
    $82000 .. $827FF:
      ctribe_getword := bg_ram[(direccion and $7FF) shr 1];
    $82800 .. $82FFF:
      ctribe_getword := ram2[(direccion and $7FF) shr 1];
    $C0000 .. $C000F:
      case ((direccion and $F) shr 1) of
        0:
          ctribe_getword := fg_scrollx;
        1:
          ctribe_getword := fg_scrolly;
        2:
          ctribe_getword := bg_scrollx;
        3:
          ctribe_getword := bg_scrolly;
        5:
          ctribe_getword := byte(main_screen.flip_main_screen);
        6:
          ctribe_getword := bg_tilebase;
      else
        ctribe_getword := 0;
      end;
    $100000 .. $1005FF:
      ctribe_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $180000:
      ctribe_getword := (marcade.in0 and $E7FF) or (vblank * $800) or (marcade.dswc and $1000);
    $180002:
      ctribe_getword := (marcade.in1 and $FF) or (marcade.dswb and $FF00);
    $180004:
      ctribe_getword := $FF or (marcade.dswb and $FF00); // P3
    $180006:
      ctribe_getword := marcade.dswc or $1000;
    $1C0000 .. $1C3FFF:
      ctribe_getword := ram[(direccion and $3FFF) shr 1];
  end;
end;

procedure change_color_ctribe(pos, data: word);
var
  color: tcolor;
begin
  color.b := pal4bit(data shr 8);
  color.g := pal4bit(data shr 4);
  color.r := pal4bit(data);
  set_pal_color(color, pos);
  case pos of
    $100 .. $1FF:
      buffer_color[$10 + ((pos shr 4) and $F)] := true;
    $200 .. $2FF:
      buffer_color[(pos shr 4) and $F] := true;
  end;
end;

procedure ctribe_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $7FFFF:
      ; // ROM
    $80000 .. $80FFF:
      if fg_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        fg_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[$400 + ((direccion and $FFF) shr 2)] := true;
      end;
    $81000 .. $81FFF:
      buffer_sprites_w[(direccion and $FFF) shr 1] := valor;
    $82000 .. $827FF:
      if bg_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $82800 .. $82FFF:
      ram2[(direccion and $7FF) shr 1] := valor;
    $C0000 .. $C000F:
      ddragon3_scroll_io((direccion and $F) shr 1, valor);
    $100000 .. $1005FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color_ctribe((direccion and $7FF) shr 1, valor);
      end;
    $140000 .. $14000F:
      ddragon3_io_w((direccion and $F) shr 1, valor);
    $1C0000 .. $1C3FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function ctribe_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      ctribe_snd_getbyte := mem_snd[direccion];
    $8801:
      ctribe_snd_getbyte := ym2151_0.status;
    $9800:
      ctribe_snd_getbyte := oki_6295_0.read;
    $A000:
      ctribe_snd_getbyte := sound_latch;
  end;
end;

procedure ctribe_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
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

procedure ym2151_snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

procedure ddragon3_sound_update;
begin
  ym2151_0.update;
  oki_6295_0.update;
end;

// Snapshot
procedure ddragon3_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 13] of byte;
  size: word;
  name: string;
begin
  case main_vars.machine_type of
    196:
      name := 'ddragon3';
    232:
      name := 'ctribe';
  end;
  open_qsnapshot_save(name + nombre);
  getmem(data, 20000);
  // CPU
  size := m68000_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ym2151_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := oki_6295_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@fg_ram, $800 * 2);
  savedata_qsnapshot(@bg_ram, $400 * 2);
  savedata_qsnapshot(@ram, $2000 * 2);
  savedata_qsnapshot(@buffer_sprites_w, $800 * 2);
  buffer[0] := vreg and $FF;
  buffer[1] := vreg shr 8;
  buffer[2] := bg_tilebase and $FF;
  buffer[3] := bg_tilebase shr 8;
  buffer[4] := fg_scrollx and $FF;
  buffer[5] := fg_scrollx shr 8;
  buffer[6] := fg_scrolly and $FF;
  buffer[7] := fg_scrolly shr 8;
  buffer[8] := bg_scrollx and $FF;
  buffer[9] := bg_scrollx shr 8;
  buffer[10] := bg_scrolly and $FF;
  buffer[11] := bg_scrolly shr 8;
  buffer[12] := sound_latch;
  buffer[13] := vblank;
  savedata_qsnapshot(@buffer, 14);
  savedata_qsnapshot(@buffer_paleta, $400 * 2);
  freemem(data);
  close_qsnapshot;
end;

procedure ddragon3_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 13] of byte;
  f: word;
  name: string;
begin
  case main_vars.machine_type of
    196:
      name := 'ddragon3';
    232:
      name := 'ctribe';
  end;
  if not(open_qsnapshot_load(name + nombre)) then
    exit;
  getmem(data, 20000);
  // CPU
  loaddata_qsnapshot(data);
  m68000_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ym2151_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  oki_6295_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@fg_ram);
  loaddata_qsnapshot(@bg_ram);
  loaddata_qsnapshot(@ram);
  loaddata_qsnapshot(@buffer_sprites_w);
  loaddata_qsnapshot(@buffer);
  vreg := buffer[0] or (buffer[1] shl 8);
  bg_tilebase := buffer[2] or (buffer[3] shl 8);
  fg_scrollx := buffer[4] or (buffer[5] shl 8);
  fg_scrolly := buffer[6] or (buffer[7] shl 8);
  bg_scrollx := buffer[8] or (buffer[9] shl 8);
  bg_scrolly := buffer[10] or (buffer[11] shl 8);
  sound_latch := buffer[12];
  vblank := buffer[13];
  loaddata_qsnapshot(@buffer_paleta);
  freemem(data);
  close_qsnapshot;
  // END
  for f := 0 to $3FF do
  begin
    if main_vars.machine_type = 196 then
      change_color(f, buffer_paleta[f])
    else
      change_color_ctribe(f, buffer_paleta[f]);
  end;
  fillchar(buffer_color, $400, 1);
  fillchar(gfx[0].buffer, $800, 1);
end;

// Main
procedure reset_ddragon3;
begin
  m68000_0.reset;
  z80_0.reset;
  ym2151_0.reset;
  oki_6295_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=z80_0.tframes;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  bg_tilebase := 0;
  fg_scrollx := 0;
  fg_scrolly := 0;
  bg_scrollx := 0;
  bg_scrolly := 0;
  vreg := 0;
  sound_latch := 0;
  vblank := 0;
end;

function start_doubledragon3: boolean;
var
  memory_temp: pbyte;
const
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2,
    32 * 8 + 3, 32 * 8 + 4, 32 * 8 + 5, 32 * 8 + 6, 32 * 8 + 7);
  pt_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16,
    16 * 8, 16 * 9, 16 * 10, 16 * 11, 16 * 12, 16 * 13, 16 * 14, 16 * 15);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2,
    16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8,
    9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  start_doubledragon3 := false;
  machine_calls.general_loop := ddragon3_loop;
  machine_calls.reset := reset_ddragon3;
  machine_calls.fps_max := 57.444853;
  machine_calls.load_qsnap := ddragon3_qload;
  machine_calls.save_qsnap := ddragon3_qsave;
  start_audio(false);
  // Pantallas
  screen_init(1, 512, 512, true);
  screen_mod_scroll(1, 512, 320, 511, 512, 256, 511);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 320, 511, 512, 256, 511);
  screen_init(3, 512, 512, false, true);
  start_video(320, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 272);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 272);
  z80_0.init_sound(ddragon3_sound_update);
  // Sound Chips
ym2151_0:=ym2151_chip.create(3579545,1);
  ym2151_0.change_irq_func(ym2151_snd_irq);
oki_6295_0:=snd_okim6295.Create(1056000,OKIM6295_PIN7_HIGH,1);
  getmem(memory_temp, $400000);
  case main_vars.machine_type of
    196:
      begin // DDW 3
        // Cargar ADPCM ROMS
        if not(roms_load(@mem_oki, ddragon3_oki)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, @mem_oki, $40000);
        // cargar roms
        m68000_0.change_ram16_calls(ddragon3_getword, ddragon3_putword);
        if not(roms_load16w(@rom, ddragon3_rom)) then
          exit;
        // cargar sonido
        z80_0.change_ram_calls(ddragon3_snd_getbyte, ddragon3_snd_putbyte);
        if not(roms_load(@mem_snd, ddragon3_sound)) then
          exit;
        // convertir background
        if not(roms_load16w(pword(memory_temp), ddragon3_bg)) then
          exit;
        init_gfx(0, 16, 16, $2000);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(4, 0, 64 * 8, 8, 0, $80000 * 8 + 8, $80000 * 8 + 0);
        convert_gfx(0, 0, memory_temp, @pt_x, @pt_y, false, false);
        // convertir sprites
        if not(roms_load(memory_temp, ddragon3_sprites)) then
          exit;
        init_gfx(1, 16, 16, $8000);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, $100000 * 8, $100000 * 8 * 2, $100000 * 8 * 3);
        convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
        // DIP
        marcade.dswa := $FFFF;
        marcade.dswa_val2:=@ddragon3_dip_a;
        video_update_dd3 := update_video_ddragon3;
        events_update_dd3 := events_ddragon3;
      end;
    232:
      begin // The Combatribes
        // Cargar ADPCM ROMS
        if not(roms_load(oki_6295_0.get_rom_addr, ctribe_oki)) then
          exit;
        // cargar roms
        m68000_0.change_ram16_calls(ctribe_getword, ctribe_putword);
        if not(roms_load16w(@rom, ctribe_rom)) then
          exit;
        // cargar sonido
        z80_0.change_ram_calls(ctribe_snd_getbyte, ctribe_snd_putbyte);
        if not(roms_load(@mem_snd, ctribe_sound)) then
          exit;
        // convertir background
        if not(roms_load16w(pword(memory_temp), ctribe_bg)) then
          exit;
        init_gfx(0, 16, 16, $2000);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(4, 0, 64 * 8, 8, 0, $80000 * 8 + 8, $80000 * 8 + 0);
        convert_gfx(0, 0, memory_temp, @pt_x, @pt_y, false, false);
        // convertir sprites
        if not(roms_load(memory_temp, ctribe_sprites)) then
          exit;
        init_gfx(1, 16, 16, $8000);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, $100000 * 8, $100000 * 8 * 2, $100000 * 8 * 3);
        convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
        // DIP
        marcade.dswa := $FFFF;
        marcade.dswa_val2:=@ctribe_dip_a;
        marcade.dswb := $FFFF;
        marcade.dswb_val2:=@ctribe_dip_b;
        marcade.dswc := $FFFF;
        marcade.dswc_val2:=@ctribe_dip_c;
        video_update_dd3 := update_video_ctribe;
        events_update_dd3 := events_ctribe;
      end;
  end;
  // final
  freemem(memory_temp);
  start_doubledragon3 := true;
end;

end.
