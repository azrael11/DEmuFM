unit m63_hw;

interface

uses
  WinApi.Windows,
  nz80,
  mcs48,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine, dac;

function start_irem_m63: boolean;

implementation

const
        //Wily Tower
        wilytower_rom:array[0..5] of tipo_roms=(
        (n:'wt4e.bin';l:$2000;p:0;crc:$a38e4b8a),(n:'wt4h.bin';l:$2000;p:$2000;crc:$c1405ceb),
        (n:'wt4j.bin';l:$2000;p:$4000;crc:$379fb1c3),(n:'wt4k.bin';l:$2000;p:$6000;crc:$2dd6f9c7),
        (n:'wt_a-4m.bin';l:$2000;p:$8000;crc:$c1f8a7d5),(n:'wt_a-4n.bin';l:$2000;p:$a000;crc:$b212f7d2));
        wilytower_pal:array[0..3] of tipo_roms=(
        (n:'wt_a-5s-.bpr';l:$100;p:0;crc:$041950e7),(n:'wt_a-5r-.bpr';l:$100;p:$100;crc:$bc04bf25),
        (n:'wt_a-5p-.bpr';l:$100;p:$200;crc:$ed819a19),(n:'wt_b-9l-.bpr';l:$20;p:$300;crc:$d2728744));
        wilytower_char:array[0..1] of tipo_roms=(
        (n:'wt_b-5e.bin';l:$1000;p:0;crc:$fe45df43),(n:'wt_b-5f.bin';l:$1000;p:$1000;crc:$87a17eff));
        wilytower_tiles:array[0..2] of tipo_roms=(
        (n:'wtb5a.bin';l:$2000;p:0;crc:$efc1cbfa),(n:'wtb5b.bin';l:$2000;p:$2000;crc:$ab4bfd07),
        (n:'wtb5d.bin';l:$2000;p:$4000;crc:$40f23e1d));
        wilytower_sound:tipo_roms=(n:'wt4d.bin';l:$1000;p:0;crc:$25a171bf);
        wilytower_sprites:array[0..5] of tipo_roms=(
        (n:'wt2j.bin';l:$1000;p:0;crc:$d1bf0670),(n:'wt3k.bin';l:$1000;p:$1000;crc:$83c39a0e),
        (n:'wt_a-3m.bin';l:$1000;p:$2000;crc:$e7e468ae),(n:'wt_a-3n.bin';l:$1000;p:$3000;crc:$0741d1a9),
        (n:'wt_a-3p.bin';l:$1000;p:$4000;crc:$7299f362),(n:'wt_a-3s.bin';l:$1000;p:$5000;crc:$9b37d50d));
        wilytower_misc:tipo_roms=(n:'wt_a-6d.bin';l:$1000;p:0;crc:$a5dde29b);
        wilytower_dip_a:array [0..4] of def_dip2=(
        (mask:3;name:'Lives';number:4;val4:(0,1,2,3);name4:('2','3','4','5')),
        (mask:$c;name:'Bonus Points Rate';number:4;val4:(0,4,8,$c);name4:('Normal','x1.2','x1.4','x1.6')),
        (mask:$30;name:'Coin A';number:4;val4:($20,$10,0,$30);name4:('3C 1C','2C 1C','1C 1C','Free Play')),
        (mask:$c0;name:'Coin B';number:4;val4:(0,$40,$80,$c0);name4:('1C 2C','1C 3C','1C 5C','1C 6C')),());
        wilytower_dip_b:array [0..5] of def_dip2=(
        (mask:1;name:'Flip Screen';number:2;val2:(0,1);name2:('Off','On')),
        (mask:2;name:'Cabinet';number:2;val2:(2,0);name2:('Upright','Cocktail')),
        (mask:4;name:'Coin Mode';number:2;val2:(0,4);name2:('Mode 1','Mode 2')),
        (mask:$10;name:'Stop Mode';number:2;val2:(0,$10);name2:('Off','On')),
        (mask:$40;name:'Invulnerability';number:2;val2:(0,$40);name2:('Off','On')),());
        //Fighting Basketball
        fightbasket_rom:array[0..4] of tipo_roms=(
        (n:'fb14.0f';l:$2000;p:0;crc:$82032853),(n:'fb13.2f';l:$2000;p:$2000;crc:$5306df0f),
        (n:'fb12.3f';l:$2000;p:$4000;crc:$ee9210d4),(n:'fb10.6f';l:$2000;p:$8000;crc:$6b47efba),
        (n:'fb09.7f';l:$2000;p:$a000;crc:$be69e087));
        fightbasket_pal:array[0..2] of tipo_roms=(
        (n:'fb_r.9e';l:$100;p:0;crc:$c5cdc8ba),(n:'fb_g.10e';l:$100;p:$100;crc:$1460c936),
        (n:'fb_b.11e';l:$100;p:$200;crc:$fca5bf0e));
        fightbasket_char:tipo_roms=(n:'fb08.12f';l:$1000;p:0;crc:$271cd7b8);
        fightbasket_tiles:array[0..2] of tipo_roms=(
        (n:'fb21.25e';l:$2000;p:0;crc:$02843591),(n:'fb22.23e';l:$2000;p:$2000;crc:$cd51d8e7),
        (n:'fb23.22e';l:$2000;p:$4000;crc:$62bcac87));
        fightbasket_sound:tipo_roms=(n:'fb07.0b';l:$1000;p:0;crc:$50432dbd);
        fightbasket_sprites:array[0..5] of tipo_roms=(
        (n:'fb16.35a';l:$2000;p:0;crc:$a5df1652),(n:'fb15.37a';l:$2000;p:$2000;crc:$59c4de06),
        (n:'fb18.32a';l:$2000;p:$4000;crc:$c23ddcd7),(n:'fb17.34a';l:$2000;p:$6000;crc:$7db28013),
        (n:'fb20.29a';l:$2000;p:$8000;crc:$1a1b48f8),(n:'fb19.31a';l:$2000;p:$a000;crc:$7ff7e321));
        fightbasket_misc:tipo_roms=(n:'fb06.12a';l:$2000;p:0;crc:$bea3df99);
        fightbasket_samples:array[0..4] of tipo_roms=(
        (n:'fb01.42a';l:$2000;p:0;crc:$1200b220),(n:'fb02.41a';l:$2000;p:$2000;crc:$0b67aa82),
        (n:'fb03.40a';l:$2000;p:$4000;crc:$c71269ed),(n:'fb04.39a';l:$2000;p:$6000;crc:$02ddc42d),
        (n:'fb05.38a';l:$2000;p:$8000;crc:$72ea6b49));
        fightbasket_dip_a:array [0..5] of def_dip2=(
        (mask:3;name:'Coin A';number:4;val4:(3,1,0,2);name4:('3C 1C','2C 1C','1C 1C','1C 2C')),
        (mask:$c;name:'Coin B';number:4;val4:(4,0,8,$c);name4:('1C 1C','1C 2C','1C 4C','99 Credits/Sound Test')),
        (mask:$20;name:'Time Count Down';number:2;val2:(0,$20);name2:('Slow','Too Fast')),
        (mask:$40;name:'Cabinet';number:2;val2:($40,0);name2:('Upright','Cocktail')),
        (mask:$80;name:'Demo Sounds';number:2;val2:(0,$80);name2:('Off','On')),());

var
  sound_latch, snd_status, pal_bank, p1_data, p2_data, sprite_y: byte;
  sound_irq, nmi_enabled, sample_play: boolean;
  mem_user: array [0 .. $1FFF] of byte;
  fg_color, mem_desp, sample_pos, sample_count: word;
  eventos_func: procedure;
  sample_data: array [0 .. $FFFF] of byte;

procedure update_video_m63;
var
  f, nchar: word;
  x, y, atrib, color: byte;
  scroll_y: array [0 .. $1F] of word;
  flip_x: boolean;
begin
  for f := 0 to $3FF do
  begin
    x := f mod 32;
    y := f div 32;
    // FG
    if gfx[0].buffer[f] then
    begin
      nchar := memory[$D400 + f + mem_desp];
      put_gfx_trans(x * 8, y * 8, nchar, fg_color, 2, 0);
      gfx[0].buffer[f] := false;
    end;
    // BG
    if gfx[1].buffer[f] then
    begin
      atrib := memory[$DC00 + f + mem_desp];
      color := (atrib and $F) + pal_bank;
      nchar := memory[$D800 + f + mem_desp] + ((atrib and $30) shl 4);
      put_gfx(x * 8, y * 8, nchar, color shl 3, 1, 1);
      gfx[1].buffer[f] := false;
    end;
  end;
  for f := 0 to $1F do
    scroll_y[f] := memory[$D300 + (f * 8) + mem_desp];
  scroll__y_part2(1, 3, 8, @scroll_y);
  for f := 0 to $3F do
  begin
    atrib := memory[$D202 + (f * 4) + mem_desp];
    nchar := memory[$D201 + (f * 4) + mem_desp] or ((atrib and $10) shl 4);
    color := (atrib and $F) + pal_bank;
    x := memory[$D203 + (f * 4) + mem_desp];
    y := sprite_y - memory[$D200 + (f * 4) + mem_desp];
    flip_x := (atrib and $20) <> 0;
    put_gfx_sprite(nchar, color shl 3, flip_x, false, 2);
    update_gfx_sprite(x, y, 3, 2)
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(0, 16, 256, 224, 3);
end;

procedure events_irem_m63;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // P2
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
  end;
end;

procedure events_irem_fb;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // P2
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
  end;
end;

procedure irem_m63_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := mcs48_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
	    if f=240 then begin
        if nmi_enabled then z80_0.change_nmi(PULSE_LINE);
        update_video_m63;
    end;
      // main
      z80_0.run(frame_m);
      frame_m := frame_m + z80_0.tframes - z80_0.contador;
      // snd
      mcs48_0.run(frame_s);
      frame_s := frame_s + mcs48_0.tframes - mcs48_0.contador;
 
    end;
    eventos_func;
    video_sync;
  end;
end;

// Wily Tower
function wilytower_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF, $D000 .. $EFFF:
      wilytower_getbyte := memory[direccion];
    $F800:
      wilytower_getbyte := marcade.in0; // p1
    $F801:
      wilytower_getbyte := marcade.in1; // p2
    $F802:
      wilytower_getbyte := marcade.dswa; // dsw1
    $F806:
      wilytower_getbyte := marcade.dswb; // dsw2
  end;
end;

procedure wilytower_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $D000 .. $E3FF:
      memory[direccion] := valor;
    $E400 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $E800 .. $EFFF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[1].buffer[direccion and $3FF] := true;
      end;
    $F000:
      nmi_enabled := (valor and 1) <> 0;
    $F002:
      ; // flip screen
    $F003:
      if pal_bank <> ((valor and 1) shl 4) then
      begin
        pal_bank := (valor and 1) shl 4;
        fillchar(gfx[1].buffer, $400, 1);
      end;
    $F800:
      sound_latch := valor;
    $F803:
      mcs48_0.change_irq(ASSERT_LINE);
  end;
end;

// Fighting Basketball
function fightbasket_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $5FFF, $8000 .. $DFFF:
      fightbasket_getbyte := memory[direccion];
    $F000:
      fightbasket_getbyte := snd_status;
    $F001:
      fightbasket_getbyte := marcade.in0; // p1
    $F002:
      fightbasket_getbyte := marcade.in1; // p2
    $F003:
      fightbasket_getbyte := marcade.dswa; // dsw1
  end;
end;

procedure fightbasket_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF, $8000 .. $BFFF:
      ;
    $C000 .. $D3FF:
      memory[direccion] := valor;
    $D400 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[1].buffer[direccion and $3FF] := true;
      end;
    $F000:
      mcs48_0.change_irq(ASSERT_LINE);
    $F002:
      sound_latch := valor;
    $F801:
      nmi_enabled := (valor and 1) <> 0;
    $F802:
      ; // flip screen
    $F807:
      if (valor and 1) <> 0 then
      begin
        sample_count := 0;
        sample_pos := (valor and $F0) shl 8;
        sample_play := true;
      end;
  end;
end;

// sonido
function m63_snd_getbyte(direccion: word): byte;
begin
  if direccion < $1000 then
    m63_snd_getbyte := mem_snd[direccion];
end;

function m63_snd_inport(puerto: word): byte;
begin
  case puerto of
    MCS48_PORT_T1:
      begin
        m63_snd_inport := byte(sound_irq);
        if sound_irq then
          sound_irq := false;
      end;
  end;
end;

procedure m63_snd_outport(puerto: word; valor: byte);
begin
  case puerto of
    MCS48_PORT_P1:
      p1_data := valor;
    MCS48_PORT_P2:
      begin
        p2_data := valor;
        if (valor and $F0) = $50 then
          mcs48_0.change_irq(CLEAR_LINE);
      end;
  end;
end;

function m63_snd_ext_inport(puerto: word): byte;
var
  res: byte;
begin
  res := $FF;
  case (p2_data and $F0) of
    $60:
      res := sound_latch;
    $70:
      res := mem_user[((p1_data and $1F) shl 8) or (puerto and $FF)];
  end;
  m63_snd_ext_inport := res;
end;

procedure m63_snd_ext_outport(puerto: word; valor: byte);
begin
  puerto := puerto and $FF;
  if ((p2_data and $F0) = $E0) then
    ay8910_0.control(puerto)
  else if ((p2_data and $F0) = $A0) then
    ay8910_0.Write(puerto)
  else if ((p1_data and $E0) = $60) then
    ay8910_1.control(puerto)
  else if ((p1_data and $E0) = $40) then
    ay8910_1.Write(puerto);
end;

procedure fb_snd_ext_outport(puerto: word; valor: byte);
begin
  puerto := puerto and $FF;
  if ((p2_data and $F0) = $E0) then
    ay8910_0.control(puerto)
  else if ((p2_data and $F0) = $A0) then
    ay8910_0.Write(puerto)
  else if ((p2_data and $F0) = $70) then
    snd_status := puerto;
end;

procedure m63_snd_irq;
begin
  sound_irq := true;
end;

procedure irem_m63_play_sound;
begin
  ay8910_0.update;
  ay8910_1.update;
end;

procedure irem_fb_play_sound;
begin
  ay8910_0.update;
  if sample_play then
    dac_0.update;
end;

procedure fb_update_sample;
begin
  if sample_play then
  begin
    dac_0.signed_data8_w(sample_data[sample_pos]);
    sample_count := sample_count + 1;
    sample_pos := sample_pos + 1;
    if sample_count = $2000 then
      sample_play := false;
  end;
end;

// Main
procedure reset_irem_m63;
begin
  z80_0.reset;
  mcs48_0.reset;
 reset_video;
  reset_audio;
  ay8910_0.reset;
  if main_vars.machine_type = 354 then
    ay8910_1.reset
  else
    dac_0.reset;
  marcade.in0 := 0;
  marcade.in1 := 0;
  pal_bank := 0;
  sound_latch := 0;
  snd_status := 0;
  nmi_enabled := false;
  sound_irq := false;
  p1_data := 0;
  p2_data := 0;
  sample_play := false;
  sample_pos := 0;
  sample_count := 0;
end;

function start_irem_m63: boolean;
var
  mamory_temp: array [0 .. $FFFF] of byte;
  colores: tpaleta;
  bit0, bit1, bit2, bit3: byte;
  f: word;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, $100 * 16 * 8 + 0, $100 * 16 * 8 + 1,
    $100 * 16 * 8 + 2, $100 * 16 * 8 + 3, $100 * 16 * 8 + 4, $100 * 16 * 8 + 5, $100 * 16 * 8 + 6,
    $100 * 16 * 8 + 7);
  ps_fb_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, $200 * 16 * 8 + 0, $200 * 16 * 8 + 1,
    $200 * 16 * 8 + 2, $200 * 16 * 8 + 3, $200 * 16 * 8 + 4, $200 * 16 * 8 + 5, $200 * 16 * 8 + 6,
    $200 * 16 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8,
    10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  procedure make_chars;
  begin
    init_gfx(0, 8, 8, $200);
    gfx_set_desc_data(2, 0, 8 * 8, $200 * 8 * 8, 0);
    convert_gfx(0, 0, @mamory_temp, @ps_x, @ps_y, false, false);
    gfx[0].trans[0] := true;
  end;
  procedure make_tiles;
  begin
    init_gfx(1, 8, 8, $400);
    gfx_set_desc_data(3, 0, 8 * 8, $400 * 8 * 8 * 2, $400 * 8 * 8, 0);
    convert_gfx(1, 0, @mamory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := irem_m63_loop;
  machine_calls.reset := reset_irem_m63;
  start_irem_m63 := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(12000000 div 4, $100);
  // Sound CPU
  mcs48_0 := cpu_mcs48.create(12000000 div 4, $100, I8039);
  mcs48_0.change_ram_calls(m63_snd_getbyte, nil);
  // IRQ Sound CPU
  timers.init(mcs48_0.numero_cpu, 12000000 / 4 / 15 / 60, m63_snd_irq, nil, true);
  case main_vars.machine_type of
    354:
      begin // Wily Tower
        mem_desp := $1000;
        fg_color := $100;
        sprite_y := 238;
        eventos_func := events_irem_m63;
        main_screen.rot180_screen := true;
        // cargar roms
        z80_0.change_ram_calls(wilytower_getbyte, wilytower_putbyte);
        if not(roms_load(@memory, wilytower_rom)) then
          exit;
        if not(roms_load(@mem_user, wilytower_misc)) then
          exit;
        // cargar sonido
        mcs48_0.change_io_calls(m63_snd_inport, m63_snd_outport, m63_snd_ext_inport, m63_snd_ext_outport);
        mcs48_0.init_sound(irem_m63_play_sound);
        // sound chips
        ay8910_0 := ay8910_chip.create(12000000 div 8, AY8910, 1);
        ay8910_1 := ay8910_chip.create(12000000 div 8, AY8910, 1);
        if not(roms_load(@mem_snd, wilytower_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@mamory_temp, wilytower_char)) then
          exit;
        make_chars;
        // convertir tiles
        if not(roms_load(@mamory_temp, wilytower_tiles)) then
          exit;
        make_tiles;
        // convertir sprites
        if not(roms_load(@mamory_temp, wilytower_sprites)) then
          exit;
        init_gfx(2, 16, 16, $100);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(3, 0, 16 * 8, $200 * 16 * 8 * 2, $200 * 16 * 8, 0);
        convert_gfx(2, 0, @mamory_temp, @ps_x, @ps_y, false, false);
        // dip
        marcade.dswa_val2:=@wilytower_dip_a;
        marcade.dswb_val2:=@wilytower_dip_b;
        marcade.dswa := 1;
        marcade.dswb := 2;
        // poner la paleta
        if not(roms_load(@mamory_temp, wilytower_pal)) then
          exit;
        for f := 0 to $FF do
        begin
          bit0 := (mamory_temp[f] shr 0) and 1;
          bit1 := (mamory_temp[f] shr 1) and 1;
          bit2 := (mamory_temp[f] shr 2) and 1;
          bit3 := (mamory_temp[f] shr 3) and 1;
          colores[f].r:=$e*bit0+$1f*bit1+$43*bit2+$8f*bit3;
          bit0 := (mamory_temp[f + $100] shr 0) and 1;
          bit1 := (mamory_temp[f + $100] shr 1) and 1;
          bit2 := (mamory_temp[f + $100] shr 2) and 1;
          bit3 := (mamory_temp[f + $100] shr 3) and 1;
          colores[f].g:=$e*bit0+$1f*bit1+$43*bit2+$8f*bit3;
          bit0 := (mamory_temp[f + $200] shr 0) and 1;
          bit1 := (mamory_temp[f + $200] shr 1) and 1;
          bit2 := (mamory_temp[f + $200] shr 2) and 1;
          bit3 := (mamory_temp[f + $200] shr 3) and 1;
          colores[f].b:=$e*bit0+$1f*bit1+$43*bit2+$8f*bit3;
        end;
        for f := 0 to 3 do
        begin
          bit0 := (mamory_temp[f + $300] shr 0) and 1;
          bit1 := (mamory_temp[f + $300] shr 1) and 1;
          bit2 := (mamory_temp[f + $300] shr 2) and 1;
          colores[f + $100].r := $21 * bit0 + $47 * bit1 + $97 * bit2;
          bit0 := (mamory_temp[f + $300] shr 3) and 1;
          bit1 := (mamory_temp[f + $300] shr 4) and 1;
          bit2 := (mamory_temp[f + $300] shr 5) and 1;
          colores[f + $100].g := $21 * bit0 + $47 * bit1 + $97 * bit2;
          bit0 := (mamory_temp[f + $300] shr 6) and 1;
          bit1 := (mamory_temp[f + $300] shr 7) and 1;
          colores[f + $100].b := $4F * bit0 + $8A * bit1;
        end;
        set_pal(colores, $104);
      end;
    355:
      begin // Fighting Basketball
        mem_desp := 0;
        fg_color := $10;
        sprite_y := 240;
        eventos_func := events_irem_fb;
        // cargar roms
        z80_0.change_ram_calls(fightbasket_getbyte, fightbasket_putbyte);
        if not(roms_load(@memory, fightbasket_rom)) then
          exit;
        if not(roms_load(@mem_user, fightbasket_misc)) then
          exit;
        // cargar sonido
        mcs48_0.change_io_calls(m63_snd_inport, m63_snd_outport, m63_snd_ext_inport, fb_snd_ext_outport);
        mcs48_0.init_sound(irem_fb_play_sound);
        // sound chips
        ay8910_0 := ay8910_chip.create(12000000 div 8, AY8910, 1);
        if not(roms_load(@mem_snd, fightbasket_sound)) then
          exit;
        // samples
        if not(roms_load(@sample_data, fightbasket_samples)) then
          exit;
        dac_0 := dac_chip.create;
        timers.init(mcs48_0.numero_cpu, 12000000 / 4 / 15 / 8000, fb_update_sample, nil, true);
        // convertir chars
        if not(roms_load(@mamory_temp, fightbasket_char)) then
          exit;
        fillchar(mamory_temp[$1000], $1000, 0);
        make_chars;
        // convertir tiles
        if not(roms_load(@mamory_temp, fightbasket_tiles)) then
          exit;
        make_tiles;
        // convertir sprites
        if not(roms_load(@mamory_temp, fightbasket_sprites)) then
          exit;
        init_gfx(2, 16, 16, $200);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(3, 0, 16 * 8, $400 * 16 * 8 * 2, $400 * 16 * 8, 0);
        convert_gfx(2, 0, @mamory_temp, @ps_fb_x, @ps_y, false, false);
        // dip
        marcade.dswa_val2:=@fightbasket_dip_a;
        marcade.dswa := $C4;
        // poner la paleta
        if not(roms_load(@mamory_temp, fightbasket_pal)) then
          exit;
        for f := 0 to $FF do
        begin
          colores[f].r := pal4bit(mamory_temp[f]);
          colores[f].g := pal4bit(mamory_temp[f + $100]);
          colores[f].b := pal4bit(mamory_temp[f + $200]);
        end;
        set_pal(colores, $100);
      end;
  end;
  // final
  reset_irem_m63;
  start_irem_m63 := true;
end;

end.
