unit volfied_hw;

// {$DEFINE MCU=1}

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  taitosnd,
  rom_engine,
  pal_engine,
  sound_engine,
{$IFDEF MCU}
  taito_cchip,
{$ELSE IF}
  volfied_cchip;
{$ENDIF}
function start_volfied: boolean;

implementation

const
        volfied_rom:array[0..3] of tipo_roms=(
        (n:'c04-12-1.30';l:$10000;p:0;crc:$afb6a058),(n:'c04-08-1.10';l:$10000;p:1;crc:$19f7e66b),
        (n:'c04-11-1.29';l:$10000;p:$20000;crc:$1aaf6e9b),(n:'c04-25-1.9';l:$10000;p:$20001;crc:$b39e04f9));
        volfied_rom2:array[0..3] of tipo_roms=(
        (n:'c04-20.7';l:$20000;p:0;crc:$0aea651f),(n:'c04-22.9';l:$20000;p:1;crc:$f405d465),
        (n:'c04-19.6';l:$20000;p:$40000;crc:$231493ae),(n:'c04-21.8';l:$20000;p:$40001;crc:$8598d38e));
        volfied_sound:tipo_roms=(n:'c04-06.71';l:$8000;p:0;crc:$b70106b2);
        volfied_sprites:array[0..7] of tipo_roms=(
        (n:'c04-16.2';l:$20000;p:0;crc:$8c2476ef),(n:'c04-18.4';l:$20000;p:1;crc:$7665212c),
        (n:'c04-15.1';l:$20000;p:$40000;crc:$7c50b978),(n:'c04-17.3';l:$20000;p:$40001;crc:$c62fdeb8),
        (n:'c04-10.15';l:$10000;p:$80000;crc:$429b6b49),(n:'c04-09.14';l:$10000;p:$80001;crc:$c78cf057),
        (n:'c04-10.15';l:$10000;p:$a0000;crc:$429b6b49),(n:'c04-09.14';l:$10000;p:$a0001;crc:$c78cf057));
        {$IFDEF MCU}cchip_eeprom:tipo_roms=(n:'cchip_c04-23';l:$2000;p:0;crc:$46b0b479);{$ENDIF}
        //DIP
        volfied_dip1:array [0..5] of def_dip2=(
        (mask:1;name:'Cabinet';number:2;val2:(0,1);name2:('Upright','Cocktail')),
        (mask:2;name:'Flip_Screen';number:2;val2:(2,0);name2:('Off','On')),
        (mask:8;name:'Demo_Sounds';number:2;val2:(0,8);name2:('Off','On')),
        (mask:$30;name:'Coin A';number:4;val4:(0,$10,$20,$30);name4:('4C-1C','3C-1C','2C-1C','1C-1C')),
        (mask:$c0;name:'Coin B';number:4;val4:($c0,$80,$40,0);name4:('1C-2C','1C-3C','1C-4C','1C-6C')),());
        volfied_dip2:array [0..4] of def_dip2=(
        (mask:3;name:'Bonus Life';number:4;val4:(2,3,1,0);name4:('20K 40K 120K 480K 2400K','50K 150K 600K 3000K','70K 280K 1400K','100K 500K')),
        (mask:$c;name:'Difficulty';number:4;val4:(8,$c,4,0);name4:('Easy','Medium','Hard','Hardest')),
        (mask:$70;name:'Lives';number:4;val4:($70,$60,$50,$40);name4:('3','4','5','6')),
        (mask:$80;name:'Languaje';number:2;val2:(0,$80);name2:('English','Japanese')),());
        CPU_SYNC=4;

var
  video_mask, video_ctrl: word;
  rom: array [0 .. $1FFFF] of word;
  rom2, ram2: array [0 .. $3FFFF] of word;
  ram1, ram3: array [0 .. $1FFF] of word;
  spritebank: byte;

procedure update_video_volfied;
var
  x, y, nchar, atrib, color: word;
  p: dword;
  f: byte;
begin
  p := 0;
  if (video_ctrl and 1) <> 0 then
    p := p + $20000;
  for y := 0 to 247 do
  begin
    for x := 1 to 336 do
    begin // Hmm, 1 pixel offset is needed to align properly with sprites
      atrib := ram2[p + x];
      color := (atrib shl 2) and $700;
      if (atrib and $8000) <> 0 then
      begin
        color := color or $800 or ((atrib shr 9) and $F);
        if (atrib and $2000) <> 0 then
          color := color and not($F); // hack
      end
      else
      begin
        color := color or (atrib and $F);
      end;
      putpixel(y, (336 - x - 1) and $1FF, 1, @paleta[color], 1);
    end;
    p := p + 512;
  end;
  update_region(0, 0, 248, 336, 1, 0, 0, 248, 336, 2);
  // Sprites
  for f := $FF downto 0 do
  begin
    nchar := (ram3[2 + (f * 4)]) mod $1800;
    atrib := ram3[f * 4];
    color := ((atrib and $F) or spritebank) shl 4;
    put_gfx_sprite(nchar, color + $1000, (atrib and $8000) <> 0, (atrib and $4000) <> 0, 0);
    y := 320 - ram3[3 + (f * 4)];
    x := ram3[1 + (f * 4)];
    update_gfx_sprite(x and $1FF, y and $1FF, 2, 0);
  end;
  update_final_piece(8, 16, 240, 320, 2);
end;

procedure events_volfied;
begin
  if event.arcade then
  begin
    // F00007
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // F00009
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    // F0000B
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.left[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.right[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // F0000D
    if p_contrls.map_arcade.up[1] then
      marcade.in3 := (marcade.in3 and $FD)
    else
      marcade.in3 := (marcade.in3 or 2);
    if p_contrls.map_arcade.down[1] then
      marcade.in3 := (marcade.in3 and $FB)
    else
      marcade.in3 := (marcade.in3 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in3 := (marcade.in3 and $EF)
    else
      marcade.in3 := (marcade.in3 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in3 := (marcade.in3 and $DF)
    else
      marcade.in3 := (marcade.in3 or $20);
    if p_contrls.map_arcade.left[1] then
      marcade.in3 := (marcade.in3 and $7F)
    else
      marcade.in3 := (marcade.in3 or $80);
  end;
end;

procedure volfied_loop;
var
  f, h: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
    for f:=0 to $ff do begin
        if f=248 then begin
          update_video_volfied;
          m68000_0.irq[4]:=HOLD_LINE;
          {$IFDEF MCU}cchip_0.set_int;{$ENDIF}
        end;
        for h:=1 to CPU_SYNC do begin
          //Main CPU
          m68000_0.run(frame_main);
          frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
          //Sound CPU
          tc0140syt_0.z80.run(frame_snd);
          frame_snd:=frame_snd+tc0140syt_0.z80.tframes-tc0140syt_0.z80.contador;
          //MCU
          {$IFDEF MCU}
          cchip_0.upd7810.run(frame_mcu);
          frame_mcu:=frame_mcu+cchip_0.upd7810.tframes-cchip_0.upd7810.contador;
          {$ENDIF}
        end;
    end;
      events_volfied;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function volfied_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      volfied_getword := rom[direccion shr 1];
    $80000 .. $FFFFF:
      volfied_getword := rom2[(direccion and $7FFFF) shr 1];
    $100000 .. $103FFF:
      volfied_getword := ram1[(direccion and $3FFF) shr 1];
    $200000 .. $203FFF:
      volfied_getword := ram3[(direccion and $3FFF) shr 1];
    $400000 .. $47FFFF:
      volfied_getword := ram2[(direccion and $7FFFF) shr 1];
    $500000 .. $503FFF:
      volfied_getword := buffer_paleta[(direccion and $3FFF) shr 1];
    $D00000:
      volfied_getword := $60;
    $E00002:
      if m68000_0.read_8bits_hi_dir then
        volfied_getword := tc0140syt_0.comm_r;
{$IFDEF MCU}
    $F00000 .. $F007FF:
      volfied_getword := cchip_0.mem_r((direccion and $7FF) shr 1);
    $F00800 .. $F00FFF:
      volfied_getword := cchip_0.asic_r((direccion and $7FF) shr 1);
{$ELSE IF}
    $F00000 .. $F007FF:
      volfied_getword := volfied_cchip_ram_r(direccion and $7FF);
    $F00802:
      volfied_getword := volfied_cchip_ctrl_r;
{$ENDIF}
  end;
end;

procedure volfied_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal5bit(tmp_color shr 10);
    color.g := pal5bit(tmp_color shr 5);
    color.r := pal5bit(tmp_color);
    set_pal_color(color, numero);
  end;

begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $103FFF:
      ram1[(direccion and $3FFF) shr 1] := valor;
    $200000 .. $203FFF:
      ram3[(direccion and $3FFF) shr 1] := valor;
    $400000 .. $47FFFF:
      ram2[(direccion and $7FFFF) shr 1] := (ram2[(direccion and $7FFFF) shr 1] and not(video_mask)) or (valor and video_mask);
    $500000 .. $503FFF:
      if buffer_paleta[(direccion and $3FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $3FFF) shr 1] := valor;
        change_color(valor, (direccion and $3FFF) shr 1);
      end;
    $600000:
      video_mask := valor;
    $700000:
      spritebank := (valor and $3C) shl 2;
    $D00000:
      video_ctrl := valor;
    $E00000:
      tc0140syt_0.port_w(valor and $FF);
    $E00002:
      tc0140syt_0.comm_w(valor and $FF);
{$IFDEF MCU}
    $F00000 .. $F007FF:
      cchip_0.mem_w((direccion and $7FF) shr 1, valor and $FF);
    $F00800 .. $F00FFF:
      cchip_0.asic_w((direccion and $7FF) shr 1, valor and $FF);
{$ELSE IF}
    $F00000 .. $F007FF:
      volfied_cchip_ram_w(direccion and $7FF, valor);
    $F00802:
      volfied_cchip_ctrl_w(valor);
    $F00C00:
      volfied_cchip_bank_w(valor);
{$ENDIF}
  end;
end;

function volfied_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      volfied_snd_getbyte := mem_snd[direccion];
    $8801:
      volfied_snd_getbyte := tc0140syt_0.slave_comm_r;
    $9000:
      volfied_snd_getbyte := ym2203_0.status;
    $9001:
      volfied_snd_getbyte := ym2203_0.read;
  end;
end;

procedure volfied_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $8800:
      tc0140syt_0.slave_port_w(valor);
    $8801:
      tc0140syt_0.slave_comm_w(valor);
    $9000:
      ym2203_0.Control(valor);
    $9001:
      ym2203_0.Write(valor);
  end;
end;

procedure volfied_update_sound;
begin
  ym2203_0.Update;
end;

function volfied_dipa: byte;
begin
  volfied_dipa := marcade.dswa;
end;

function volfied_dipb: byte;
begin
  volfied_dipb := marcade.dswb;
end;

function volfied_f00007: byte;
begin
  volfied_f00007 := marcade.in0;
end;

function volfied_f00009: byte;
begin
  volfied_f00009 := marcade.in1;
end;

function volfied_f0000c: byte;
begin
  volfied_f0000c := marcade.in2;
end;

function volfied_f0000d: byte;
begin
  volfied_f0000d := marcade.in3;
end;

procedure snd_irq(irqstate: byte);
begin
  tc0140syt_0.z80.change_irq(irqstate);
end;

// Main
procedure reset_volfied;
begin
  m68000_0.reset;
  tc0140syt_0.reset;
  ym2203_0.reset;
{$IFDEF MCU}
  cchip_0.reset;
{$ELSE IF}
  volfied_cchip_reset;
{$ENDIF}
 frame_main:=m68000_0.tframes;
 frame_snd:=tc0140syt_0.z80.tframes;
 {$IFDEF MCU}frame_mcu:=cchip_0.upd7810.tframes;{$ENDIF}
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FC;
  marcade.in2 := $FF;
  marcade.in3 := $FF;
  video_mask := 0;
  video_ctrl := 0;
  spritebank := 0;
end;

function start_volfied: boolean;
const
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 8 * 4, 9 * 4, 10 * 4, 11 * 4, 12 * 4, 13 * 4, 14 * 4, 15 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
var
  memory_temp: pbyte;
begin
  start_volfied := false;
  machine_calls.general_loop := volfied_loop;
  machine_calls.reset := reset_volfied;
  start_audio(false);
  screen_init(1, 248, 512);
  screen_init(2, 512, 512, false, true);
  start_video(240, 320);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256 * CPU_SYNC);
  m68000_0.change_ram16_calls(volfied_getword, volfied_putword);
  // Sound CPU
  tc0140syt_0 := tc0140syt_chip.create(4000000, 256 * CPU_SYNC);
  tc0140syt_0.z80.change_ram_calls(volfied_snd_getbyte, volfied_snd_putbyte);
  tc0140syt_0.z80.init_sound(volfied_update_sound);
  // Sound Chips
ym2203_0:=ym2203_chip.create(4000000);
  ym2203_0.change_io_calls(volfied_dipa, volfied_dipb, nil, nil);
  ym2203_0.change_irq_calls(snd_irq);
  // MCU
{$IFDEF MCU}
  // ?????????? Tengo que poner 4Mhz mas... En teoria son 10Mhz, pero si lo pongo hae cosas raras...
  cchip_0 := cchip_chip.create(10000000, 256);
  cchip_0.change_ad(volfied_f0000d);
  cchip_0.change_in(volfied_f00007, volfied_f00009, volfied_f0000c, nil, nil);
  if not(roms_load(cchip_0.get_eeprom_dir, cchip_eeprom)) then
    exit;
{$ELSE IF}
  volfied_init_cchip(m68000_0.numero_cpu);
{$ENDIF}
  // ROMS
  if not(roms_load16w(@rom, volfied_rom)) then
    exit;
  if not(roms_load16w(@rom2, volfied_rom2)) then
    exit;
  // cargar sonido+ponerlas en su banco
  if not(roms_load(@mem_snd, volfied_sound)) then
    exit;
  // convertir sprites
  getmem(memory_temp, $100000);
  if not(roms_load16b(memory_temp, volfied_sprites)) then
    exit;
  init_gfx(0, 16, 16, $1800);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, memory_temp, @ps_x, @ps_y, false, true);
  freemem(memory_temp);
  // DIP
  marcade.dswa := $FE;
marcade.dswa_val2:=@volfied_dip1;
  marcade.dswb := $7F;
marcade.dswb_val2:=@volfied_dip2;
  // final
  reset_volfied;
  start_volfied := true;
end;

end.
