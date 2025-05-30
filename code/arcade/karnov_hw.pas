unit karnov_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  ym_2203,
  ym_3812,
  m6502,
  sound_engine,
  mcs51;

function start_karnov: boolean;

implementation

const
        //Karnov
        karnov_rom:array[0..5] of tipo_roms=(
        (n:'dn08-6.j15';l:$10000;p:0;crc:$4c60837f),(n:'dn11-6.j20';l:$10000;p:1;crc:$cd4abb99),
        (n:'dn07-.j14';l:$10000;p:$20000;crc:$fc14291b),(n:'dn10-.j18';l:$10000;p:$20001;crc:$a4a34e37),
        (n:'dn06-5.j13';l:$10000;p:$40000;crc:$29d64e42),(n:'dn09-5.j17';l:$10000;p:$40001;crc:$072d7c49));
        karnov_mcu:tipo_roms=(n:'dn-5.k14';l:$1000;p:0;crc:$d056de4e);
        karnov_sound:tipo_roms=(n:'dn05-5.f3';l:$8000;p:$8000;crc:$fa1a31a8);
        karnov_char:tipo_roms=(n:'dn00-.c5';l:$8000;p:0;crc:$0ed77c6d);
        karnov_tiles:array[0..3] of tipo_roms=(
        (n:'dn04-.d18';l:$10000;p:0;crc:$a9121653),(n:'dn01-.c15';l:$10000;p:$10000;crc:$18697c9e),
        (n:'dn03-.d15';l:$10000;p:$20000;crc:$90d9dd9c),(n:'dn02-.c18';l:$10000;p:$30000;crc:$1e04d7b9));
        karnov_sprites:array[0..7] of tipo_roms=(
        (n:'dn12-.f8';l:$10000;p:0;crc:$9806772c),(n:'dn14-5.f11';l:$8000;p:$10000;crc:$ac9e6732),
        (n:'dn13-.f9';l:$10000;p:$20000;crc:$a03308f9),(n:'dn15-5.f12';l:$8000;p:$30000;crc:$8933fcb8),
        (n:'dn16-.f13';l:$10000;p:$40000;crc:$55e63a11),(n:'dn17-5.f15';l:$8000;p:$50000;crc:$b70ae950),
        (n:'dn18-.f16';l:$10000;p:$60000;crc:$2ad53213),(n:'dn19-5.f18';l:$8000;p:$70000;crc:$8fd4fa40));
        karnov_proms:array[0..1] of tipo_roms=(
        (n:'dn-21.k8';l:$400;p:0;crc:$aab0bb93),(n:'dn-20.l6';l:$400;p:$400;crc:$02f78ffb));
        //Chelnov
        chelnov_rom:array[0..5] of tipo_roms=(
        (n:'ee08-e.j16';l:$10000;p:0;crc:$8275cc3a),(n:'ee11-e.j19';l:$10000;p:1;crc:$889e40a0),
        (n:'ee07.j14';l:$10000;p:$20000;crc:$51465486),(n:'ee10.j18';l:$10000;p:$20001;crc:$d09dda33),
        (n:'ee06-e.j13';l:$10000;p:$40000;crc:$55acafdb),(n:'ee09-e.j17';l:$10000;p:$40001;crc:$303e252c));
        chelnov_sound:tipo_roms=(n:'ee05-.f3';l:$8000;p:$8000;crc:$6a8936b4);
        chelnov_mcu:tipo_roms=(n:'ee-e.k14';l:$1000;p:0;crc:$b7045395);
        chelnov_char:tipo_roms=(n:'ee00-e.c5';l:$8000;p:0;crc:$e06e5c6b);
        chelnov_tiles:array[0..3] of tipo_roms=(
        (n:'ee04-.d18';l:$10000;p:0;crc:$96884f95),(n:'ee01-.c15';l:$10000;p:$10000;crc:$f4b54057),
        (n:'ee03-.d15';l:$10000;p:$20000;crc:$7178e182),(n:'ee02-.c18';l:$10000;p:$30000;crc:$9d7c45ae));
        chelnov_sprites:array[0..3] of tipo_roms=(
        (n:'ee12-.f8';l:$10000;p:0;crc:$9b1c53a5),(n:'ee13-.f9';l:$10000;p:$20000;crc:$72b8ae3e),
        (n:'ee14-.f13';l:$10000;p:$40000;crc:$d8f4bbde),(n:'ee15-.f15';l:$10000;p:$60000;crc:$81e3e68b));
        chelnov_proms:array[0..1] of tipo_roms=(
        (n:'ee21.k8';l:$400;p:0;crc:$b1db6586),(n:'ee20.l6';l:$400;p:$400;crc:$41816132));
        //DIP
        karnov_dip:array [0..9] of def_dip2=(
        (mask:3;name:'Coin A';number:4;val4:(0,3,2,1);name4:('2C 1C','1C 1C','1C 2C','1C 3C')),
        (mask:$c;name:'Coin B';number:4;val4:(0,$c,8,4);name4:('2C 1C','1C 1C','1C 2C','1C 3C')),
        (mask:$20;name:'Flip Screen';number:2;val2:($20,0);name2:('Off','On')),
        (mask:$40;name:'Cabinet';number:2;val2:(0,$40);name2:('Upright','Cocktail')),
        (mask:$300;name:'Lives';number:4;val4:($100,$300,$200,0);name4:('1','3','5','Infinite')),
        (mask:$c00;name:'Bonus Life';number:4;val4:($c00,$800,$400,0);name4:('50 "K"','70 "K"','90 "K"','100 "K"')),
        (mask:$3000;name:'Difficulty';number:4;val4:($2000,$3000,$1000,0);name4:('Easy','Normal','Hard','Hardest')),
        (mask:$4000;name:'Demo Sounds';number:2;val2:(0,$4000);name2:('Off','On')),
        (mask:$8000;name:'Time Speed';number:2;val2:($8000,0);name2:('Normal','Fast')),());
        chelnov_dip:array [0..8] of def_dip2=(
        (mask:3;name:'Coin A';number:4;val4:(0,3,2,1);name4:('1C 6C','1C 2C','1C 3C','1C 4C')),
        (mask:$c;name:'Coin B';number:4;val4:(0,$c,8,4);name4:('1C 4C','1C 1C','2C 1C','3C 1C')),
        (mask:$20;name:'Demo Sounds';number:2;val2:($20,0);name2:('On','Off')),
        (mask:$40;name:'Flip Screen';number:2;val2:(0,$40);name2:('On','Off')),
        (mask:$80;name:'Cainet';number:2;val2:(0,$80);name2:('Upright','Cocktail')),
        (mask:$300;name:'Lives';number:4;val4:($100,$300,$200,0);name4:('1','3','5','Infinite')),
        (mask:$c00;name:'Difficulty';number:4;val4:($800,$c00,$400,0);name4:('Easy','Normal','Hard','Hardest')),
        (mask:$1000;name:'Allow Continue';number:2;val2:(0,$1000);name2:('No','Yes')),());

var
  rom: array [0 .. $2FFFF] of word;
  ram: array [0 .. $1FFF] of word;
 sprite_ram:array[0..$7ff] of word;
  background_ram, video_ram: array [0 .. $3FF] of word;
  sound_latch, mcu_p0, mcu_p1, mcu_p2: byte;
  scroll_x, scroll_y, maincpu_to_mcu, mcu_to_maincpu: word;
  irq_ena: boolean;

procedure events_karnov;
begin
  if event.arcade then
  begin
    // P1 + P2
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
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
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
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    // Coin
    if p_contrls.map_arcade.coin[1] then
    begin
      marcade.in2 := (marcade.in2 and $DF);
      mcs51_0.change_irq0(ASSERT_LINE);
    end
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in2 := (marcade.in2 and $BF);
      mcs51_0.change_irq0(ASSERT_LINE);
    end
    else
      marcade.in2 := (marcade.in2 or $40);
  end;
end;

procedure update_video_karnov;
var
  f, atrib, nchar, nchar2, x, y: word;
  color: byte;
  extra, fx, fy: boolean;
begin
  for f := 0 to $3FF do
  begin
    y := f shr 5;
    x := f and $1F;
    // Chars
    if gfx[0].buffer[f] then
    begin
      atrib := video_ram[f];
      nchar := atrib and $3FF;
      color := atrib shr 14;
      put_gfx_trans(x * 8, y * 8, nchar, color shl 3, 1, 0);
      gfx[0].buffer[f] := false;
    end;
    // Tiles
    if gfx[1].buffer[f] then
    begin
      atrib := background_ram[f];
      nchar := atrib and $7FF;
      color := atrib shr 12;
      put_gfx(x * 16, y * 16, nchar, (color shl 4) + $200, 2, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x, scroll_y);
  // Sprites
  for f := 0 to $1FF do
  begin
    y:=buffer_sprites_w[f*4];
    if ((y and $8000) = 0) then
      continue;
    atrib:=buffer_sprites_w[(f*4)+1];
    if ((atrib and 1) = 0) then
      continue;
    y := y and $1FF;
    nchar:=buffer_sprites_w[(f*4)+3];
    color := nchar shr 12;
    nchar := nchar and $FFF;
    x:=buffer_sprites_w[(f*4)+2] and $1ff;
    extra := (atrib and $10) <> 0;
    fy := (atrib and 2) <> 0;
    fx := (atrib and 4) <> 0;
    if extra then
    begin
      y := y + 16;
      nchar := nchar and $FFE;
    end;
    // Convert the co-ords..
    x := (x + 16) and $1FF;
    y := (y + 16) and $1FF;
    x := (256 - x) and $1FF;
    y := (256 - y) and $1FF;
    // Y Flip determines order of multi-sprite
    if (extra and fy) then
    begin
      nchar2 := nchar;
      nchar := nchar + 1;
    end
    else
      nchar2 := nchar + 1;
    put_gfx_sprite(nchar, (color shl 4) + 256, fx, fy, 2);
    update_gfx_sprite(x, y, 3, 2);
    // 1 more sprite drawn underneath
    if extra then
    begin
      put_gfx_sprite(nchar2, (color shl 4) + 256, fx, fy, 2);
      update_gfx_sprite(x, y + 16, 3, 2);
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  update_final_piece(0, 8, 256, 240, 3);
end;

procedure karnov_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
 for f:=0 to $ff do begin
   case f of
      31:marcade.in1:=marcade.in1 and $7f;
      248:begin
            marcade.in1:=marcade.in1 or $80;
            if irq_ena then m68000_0.irq[7]:=ASSERT_LINE;
            update_video_karnov;
          end;
   end;
   m68000_0.run(frame_main);
   frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
   m6502_0.run(frame_snd);
   frame_snd:=frame_snd+m6502_0.tframes-m6502_0.contador;
   mcs51_0.run(frame_mcu);
   frame_mcu:=frame_mcu+mcs51_0.tframes-mcs51_0.contador;
 end;
 events_karnov;
 video_sync;
    end
    else
      pause_action;
  end;
end;

function karnov_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $5FFFF:
      karnov_getword := rom[direccion shr 1];
    $60000 .. $63FFF:
      karnov_getword := ram[(direccion and $3FFF) shr 1];
    $80000 .. $80FFF:
      karnov_getword := sprite_ram[(direccion and $FFF) shr 1];
    $A0000 .. $A07FF:
      karnov_getword := video_ram[(direccion and $7FF) shr 1];
    $C0000:
      karnov_getword := marcade.in0;
    $C0002:
      karnov_getword := marcade.in1;
    $C0004:
      karnov_getword := marcade.dswa;
    $C0006:
      karnov_getword := mcu_to_maincpu;
  end;
end;

procedure karnov_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $5FFFF:
      ; // ROM
    $60000 .. $63FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $80000 .. $80FFF:
      sprite_ram[(direccion and $FFF) shr 1] := valor;
    $A0000 .. $A0FFF:
      if video_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        video_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $A1000 .. $A17FF:
      if background_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        background_ram[(direccion and $7FF) shr 1] := valor;
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $A1800 .. $A1FFF:
      begin
        direccion := (direccion and $7FF) shr 1;
        direccion := ((direccion and $1F) shl 5) or ((direccion and $3E0) shr 5);
        if background_ram[direccion and $3FF] <> valor then
        begin
          background_ram[direccion and $3FF] := valor;
          gfx[1].buffer[direccion and $3FF] := true;
        end;
      end;
    $C0000:
      m68000_0.irq[6] := CLEAR_LINE;
    $C0002:
      begin
        sound_latch := valor and $FF;
        m6502_0.change_nmi(PULSE_LINE);
      end;
    $C0004:
      copymemory(@buffer_sprites_w,@sprite_ram,$800*2);
    $C0006:
      begin
        maincpu_to_mcu := valor;
        mcs51_0.change_irq1(ASSERT_LINE);
      end;
    $C0008:
      begin
        scroll_x := valor and $1FF;
        main_screen.flip_main_screen := (valor and $8000) <> 0;
      end;
    $C000A:
      scroll_y := valor and $1FF;
    $C000C, $C000E:
      begin
        m68000_0.irq[7] := CLEAR_LINE;
        irq_ena := (direccion and 2) <> 0;
      end;
  end;
end;

function karnov_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $5FF, $8000 .. $FFFF:
      karnov_snd_getbyte := mem_snd[direccion];
    $800:
      karnov_snd_getbyte := sound_latch;
  end;
end;

procedure karnov_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $5FF:
      mem_snd[direccion] := valor;
    $1000:
      ym2203_0.Control(valor);
    $1001:
      ym2203_0.Write(valor);
    $1800:
      ym3812_0.Control(valor);
    $1801:
      ym3812_0.Write(valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

procedure karnov_sound_update;
begin
  ym3812_0.update;
  ym2203_0.update;
end;

function in_port0: byte;
begin
  in_port0 := mcu_p0;
end;

function in_port1: byte;
begin
  in_port1 := mcu_p1;
end;

function in_port3: byte;
begin
  // COIN
  in_port3 := marcade.in2;
end;

procedure out_port0(valor: byte);
begin
  mcu_p0 := valor;
end;

procedure out_port1(valor: byte);
begin
  mcu_p1 := valor;
end;

procedure out_port2(valor: byte);
begin
  if (((mcu_p2 and 1) <> 0) and ((valor and 1) = 0)) then
    mcs51_0.change_irq0(CLEAR_LINE);
  if (((mcu_p2 and 2) <> 0) and ((valor and 2) = 0)) then
    mcs51_0.change_irq1(CLEAR_LINE);
  if (((mcu_p2 and 4) <> 0) and ((valor and 4) = 0)) then
    m68000_0.irq[6] := ASSERT_LINE;
  if (((mcu_p2 and $10) <> 0) and ((valor and $10) = 0)) then
    mcu_p0 := maincpu_to_mcu shr 0;
  if (((mcu_p2 and $20) <> 0) and ((valor and $20) = 0)) then
    mcu_p1 := maincpu_to_mcu shr 8;
  if (((mcu_p2 and $40) <> 0) and ((valor and $40) = 0)) then
    mcu_to_maincpu := (mcu_to_maincpu and $FF00) or (mcu_p0 shl 0);
  if (((mcu_p2 and $80) <> 0) and ((valor and $80) = 0)) then
    mcu_to_maincpu := (mcu_to_maincpu and $FF) or (mcu_p1 shl 8);
  mcu_p2 := valor;
end;

procedure snd_irq(irqstate: byte);
begin
  m6502_0.change_irq(irqstate);
end;

// Main
procedure reset_karnov;
begin
  m68000_0.reset;
  mcs51_0.reset;
  m6502_0.reset;
  ym3812_0.reset;
  ym2203_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=m6502_0.tframes;
 frame_mcu:=mcs51_0.tframes;
  marcade.in0 := $FFFF;
  marcade.in1 := $7F;
  marcade.in2 := $FF;
  sound_latch := 0;
  mcu_p0 := 0;
  mcu_p1 := 0;
  mcu_p2 := 0;
  mcu_to_maincpu := 0;
  maincpu_to_mcu := 0;
  irq_ena := false;
end;

function start_karnov: boolean;
const
  ps_x: array [0 .. 15] of dword = (16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  memory_temp: array [0 .. $7FFFF] of byte;
  f: word;
  ctemp1, ctemp2, ctemp3, ctemp4: byte;
  colores: tpaleta;
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $400);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(3, 0, 8 * 8, $6000 * 8, $4000 * 8, $2000 * 8);
    convert_gfx(0, 0, @memory_temp, @ps_x[8], @ps_y, false, false);
  end;
  procedure convert_tiles(num_gfx, mul: byte);
  begin
    init_gfx(num_gfx, 16, 16, $800 * mul);
    gfx[num_gfx].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 16, ($30000 * mul) * 8, 0, ($10000 * mul) * 8, ($20000 * mul) * 8);
    convert_gfx(num_gfx, 0, @memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := karnov_loop;
  machine_calls.reset := reset_karnov;
  start_karnov := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 512, 512, false, true);
  start_video(256, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 256);
  m68000_0.change_ram16_calls(karnov_getword, karnov_putword);
  // Sound CPU
  m6502_0 := cpu_m6502.create(1500000, 256, TCPU_M6502);
  m6502_0.change_ram_calls(karnov_snd_getbyte, karnov_snd_putbyte);
  m6502_0.init_sound(karnov_sound_update);
  // MCU
  mcs51_0 := cpu_mcs51.create(I8X51, 8000000, 256);
  mcs51_0.change_io_calls(in_port0, in_port1, nil, in_port3, out_port0, out_port1, out_port2, nil);
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3526_FM, 3000000);
  ym3812_0.change_irq_calls(snd_irq);
  ym2203_0 := ym2203_chip.create(1500000, 0.25, 0.25);
  case main_vars.machine_type of
    219:
      begin // Karnov
        // MCU ROM
        if not(roms_load(mcs51_0.get_rom_addr, karnov_mcu)) then
          exit;
        // cargar roms
        if not(roms_load16w(@rom, karnov_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, karnov_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, karnov_char)) then
          exit;
        convert_chars;
        // tiles
        if not(roms_load(@memory_temp, karnov_tiles)) then
          exit;
        convert_tiles(1, 1);
        // sprites
        if not(roms_load(@memory_temp, karnov_sprites)) then
          exit;
        convert_tiles(2, 2);
        // Paleta
        if not(roms_load(@memory_temp, karnov_proms)) then
          exit;
        // DIP
        marcade.dswa := $FFBF;
        marcade.dswa_val2:=@karnov_dip;
      end;
    220:
      begin // Chelnov
        // MCU ROM
        if not(roms_load(mcs51_0.get_rom_addr, chelnov_mcu)) then
          exit;
        // cargar roms
        if not(roms_load16w(@rom, chelnov_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, chelnov_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, chelnov_char)) then
          exit;
        convert_chars;
        // tiles
        if not(roms_load(@memory_temp, chelnov_tiles)) then
          exit;
        convert_tiles(1, 1);
        // sprites
        if not(roms_load(@memory_temp, chelnov_sprites)) then
          exit;
        convert_tiles(2, 2);
        // Paleta
        if not(roms_load(@memory_temp, chelnov_proms)) then
          exit;
        // DIP
        marcade.dswa := $FF7F;
        marcade.dswa_val2:=@chelnov_dip;
      end;
  end;
  // poner la paleta
  for f := 0 to $3FF do
  begin
    // red
    ctemp1 := (memory_temp[f] shr 0) and 1;
    ctemp2 := (memory_temp[f] shr 1) and 1;
    ctemp3 := (memory_temp[f] shr 2) and 1;
    ctemp4 := (memory_temp[f] shr 3) and 1;
    colores[f].r := $E * ctemp1 + $1F * ctemp2 + $43 * ctemp3 + $8F * ctemp4;
    // green
    ctemp1 := (memory_temp[f] shr 4) and 1;
    ctemp2 := (memory_temp[f] shr 5) and 1;
    ctemp3 := (memory_temp[f] shr 6) and 1;
    ctemp4 := (memory_temp[f] shr 7) and 1;
    colores[f].g := $E * ctemp1 + $1F * ctemp2 + $43 * ctemp3 + $8F * ctemp4;
    // blue
    ctemp1 := (memory_temp[f + $400] shr 0) and 1;
    ctemp2 := (memory_temp[f + $400] shr 1) and 1;
    ctemp3 := (memory_temp[f + $400] shr 2) and 1;
    ctemp4 := (memory_temp[f + $400] shr 3) and 1;
    colores[f].b := $E * ctemp1 + $1F * ctemp2 + $43 * ctemp3 + $8F * ctemp4;
  end;
  set_pal(colores, $400);
  // final
  start_karnov := true;
end;

end.
