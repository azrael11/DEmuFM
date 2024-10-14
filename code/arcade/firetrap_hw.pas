unit firetrap_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m6502,
  mcs51,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ym_3812,
  msm5205;

function start_firetrap: boolean;

implementation

const
  firetrap_rom: array [0 .. 2] of tipo_roms = ((n: 'di-02.4a'; l: $8000; p: 0; crc: $3D1E4BF7),
    (n: 'di-01.3a'; l: $8000; p: $8000; crc: $9BBAE38B), (n: 'di-00-a.2a'; l: $8000; p: $10000;
    crc: $F39E2CF4));
  firetrap_snd: array [0 .. 1] of tipo_roms = ((n: 'di-17.10j'; l: $8000; p: 0; crc: $8605F6B9),
    (n: 'di-18.12j'; l: $8000; p: $8000; crc: $49508C93));
  firetrap_mcu: tipo_roms = (n: 'di-12.16h'; l: $1000; p: 0; crc: $6340A4D7);
  firetrap_char: tipo_roms = (n: 'di-03.17c'; l: $2000; p: 0; crc: $46721930);
  firetrap_tiles: array [0 .. 3] of tipo_roms = ((n: 'di-06.3e'; l: $8000; p: $0; crc: $441D9154),
    (n: 'di-04.2e'; l: $8000; p: $8000; crc: $8E6E7EEC), (n: 'di-07.6e'; l: $8000; p: $10000;
    crc: $EF0A7E23), (n: 'di-05.4e'; l: $8000; p: $18000; crc: $EC080082));
  firetrap_tiles2: array [0 .. 3] of tipo_roms = ((n: 'di-09.3j'; l: $8000; p: $0; crc: $D11E28E8),
    (n: 'di-08.2j'; l: $8000; p: $8000; crc: $C32A21D8), (n: 'di-11.6j'; l: $8000; p: $10000;
    crc: $6424D5C3), (n: 'di-10.4j'; l: $8000; p: $18000; crc: $9B89300A));
  firetrap_sprites: array [0 .. 3] of tipo_roms = ((n: 'di-16.17h'; l: $8000; p: $0;
    crc: $0DE055D7), (n: 'di-13.13h'; l: $8000; p: $8000; crc: $869219DA), (n: 'di-14.14h';
    l: $8000; p: $10000; crc: $6B65812E), (n: 'di-15.15h'; l: $8000; p: $18000; crc: $3E27F77D));
  firetrap_pal: array [0 .. 2] of tipo_roms = ((n: 'firetrap.3b'; l: $100; p: $0; crc: $8BB45337),
    (n: 'firetrap.4b'; l: $100; p: $100; crc: $D5ABFC64), (n: 'firetrap.1a'; l: $100; p: $200;
    crc: $D67F3514));
  // DIP
  firetrap_dip_a: array [0 .. 5] of def_dip = ((mask: $7; name: 'Coin A'; number: 5;
    dip: ((dip_val: $7; dip_name: '1C 1C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5;
    dip_name: '1C 3C'), (dip_val: $3; dip_name: '1C 4C'), (dip_val: $4; dip_name: '1C 6C'), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Coin B'; number: 4;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $8; dip_name: '3C 1C'), (dip_val: $10;
    dip_name: '2C 1C'), (dip_val: $18; dip_name: '1C 1C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Demo Sound'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $40; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  firetrap_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $2; dip_name: 'Easy'), (dip_val: $3; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C;
    dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4; dip_name: '5'), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $10; dip_name: '30K 70K'), (dip_val: $0; dip_name: '50K 100K'), (dip_val: $30;
    dip_name: '30K'), (dip_val: $20; dip_name: '50K'), (), (), (), (), (), (), (), (), (), (), (),
    ())), (mask: $40; name: 'Allow Continue'; number: 2;
    dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $40; dip_name: 'Yes'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 8;

var
  main_bank, snd_bank, sound_latch, mcu_to_maincpu, maincpu_to_mcu, mcu_p3, vblank, coins,
    msm5205_toggle: byte;
  main_rom: array [0 .. 3, 0 .. $3FFF] of byte;
  snd_rom: array [0 .. 1, 0 .. $3FFF] of byte;
  sound_irq_enable, nmi_enable: boolean;
  bg1_scrollx, bg1_scrolly, bg2_scrollx, bg2_scrolly: word;

procedure update_video_firetrap;
var
  f, nchar, pos: word;
  color, attr, x, y: byte;
  flipx, flipy: boolean;

  procedure draw_sprites;
  var
    f, x, y, nchar, attr, attr2, color: byte;
    flipx, flipy: boolean;
  begin
    for f := $7 downto 0 do
    begin
      x := memory[$9883 + (f * 4)] + 1;
      y := 240 - memory[$9882 + (f * 4)];
      attr := memory[(f * 4) + $9881];
      attr2 := memory[(f * 4) + $9880];
      nchar := ((attr and $10) shl 3) or ((attr and $20) shl 1) or (attr2 and $3F);
      color := (attr and $0F) shl 2;
      flipx := (attr2 and $40) <> 0;
      flipy := (attr2 and $80) <> 0;
      put_gfx_sprite_mask(nchar, color, flipx, flipy, 1, 0, 3);
      update_gfx_sprite(x, y, 3, 1);
    end;
  end;

begin
  for f := 0 to $3FF do
  begin
    // Fondo
    x := f div 32;
    y := f mod 32;
    pos := (y xor $1F) + (x shl 5);
    if gfx[0].buffer[pos] then
    begin
      attr := memory[pos + $E400];
      color := (attr and $F0) shr 2;
      nchar := memory[pos + $E000] or ((attr and 1) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color, 1, 0);
      gfx[0].buffer[pos] := false;
    end;
    pos := ((y and $0F) xor $0F) or ((x and $0F) shl 4) or ((y and $10) shl 5) or
      ((x and $10) shl 6);
    if gfx[1].buffer[pos] then
    begin
      attr := memory[pos + $D100];
      color := attr and $30;
      nchar := memory[$D000 + pos] or ((attr and 3) shl 8);
      put_gfx_trans_flip(x * 16, y * 16, nchar, color + $80, 2, 1, (attr and 8) <> 0,
        (attr and 4) <> 0);
      gfx[1].buffer[pos] := false;
    end;
    if gfx[2].buffer[pos] then
    begin
      attr := memory[pos + $D900];
      color := attr and $30;
      nchar := memory[$D800 + pos] or ((attr and 3) shl 8);
      put_gfx_flip(x * 16, y * 16, nchar, color + $C0, 3, 2, (attr and 8) <> 0, (attr and 4) <> 0);
      gfx[2].buffer[pos] := false;
    end;
  end;
  scroll_x_y(3, 4, bg2_scrollx, 512 - bg2_scrolly);
  scroll_x_y(2, 4, bg1_scrollx, 512 - bg1_scrolly);
  for f := 0 to $5F do
  begin
    x := memory[$E802 + (f * 4)];
    y := memory[$E800 + (f * 4)];
    attr := memory[(f * 4) + $E801];
    nchar := memory[(f * 4) + $E803] + ((attr and $C0) shl 2);
    color := (((attr and $08) shr 2) or (attr and $01)) shl 4;
    flipx := (attr and $04) <> 0;
    flipy := (attr and $02) <> 0;
    if (attr and $10) <> 0 then
    begin // doble
      if flipy then
      begin
        put_gfx_sprite_diff(nchar and $FFE, $40 + color, flipx, flipy, 3, 0, 0);
        put_gfx_sprite_diff(nchar or 1, $40 + color, flipx, flipy, 3, 0, 16);
      end
      else
      begin
        put_gfx_sprite_diff(nchar and $FFE, $40 + color, flipx, flipy, 3, 0, 16);
        put_gfx_sprite_diff(nchar or 1, $40 + color, flipx, flipy, 3, 0, 0);
      end;
      actualiza_gfx_sprite_size(x, y, 4, 16, 32);
    end
    else
    begin
      put_gfx_sprite(nchar, color + $40, flipx, flipy, 3);
      update_gfx_sprite(x, y, 4, 3);
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(0, 8, 256, 240, 4);
end;

procedure events_firetrap;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
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
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // SYSTEM
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      coins := (coins and $FB)
    else
      coins := (coins or $4);
    if p_contrls.map_arcade.coin[1] then
      coins := (coins and $F7)
    else
      coins := (coins or $8);
  end;
end;

procedure firetrap_loop;
var
  f: word;
  frame_m, frame_s, frame_mcu: single;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := m6502_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // main
          z80_0.run(frame_m);
          frame_m := frame_m + z80_0.tframes - z80_0.contador;
          // Sound
          m6502_0.run(frame_s);
          frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
          // MCU
          mcs51_0.run(frame_mcu);
          frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        end;
        case f of
          8:
            begin
              mcs51_0.change_irq1(CLEAR_LINE);
              vblank := 0;
            end;
          247:
            begin
              update_video_firetrap;
              vblank := $80;
              if nmi_enable then
                z80_0.change_nmi(ASSERT_LINE);
              mcs51_0.change_irq1(ASSERT_LINE);
            end;
        end;
      end;
      events_firetrap;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function firetrap_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FFF, $C000 .. $E97F:
      firetrap_getbyte := memory[direccion];
    $8000 .. $BFFF:
      firetrap_getbyte := main_rom[main_bank, direccion and $3FFF];
    $F010:
      firetrap_getbyte := marcade.in0; // in0
    $F011:
      firetrap_getbyte := marcade.in1; // in1
    $F012:
      firetrap_getbyte := marcade.in2 or vblank; // in2
    $F013:
      firetrap_getbyte := marcade.dswa; // dsw0
    $F014:
      firetrap_getbyte := marcade.dswb; // dsw1
    $F016:
      firetrap_getbyte := mcu_to_maincpu; // mcu_r
  end;
end;

procedure firetrap_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $CFFF, $E800 .. $E97F:
      memory[direccion] := valor;
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[1].buffer[direccion and $6FF] := true;
      end;
    $D800 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[2].buffer[direccion and $6FF] := true;
      end;
    $E000 .. $E7FF:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $F000:
      z80_0.change_irq(CLEAR_LINE); // firetrap_state::irqack_w
    $F001:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(PULSE_LINE);
      end;
    $F002:
      main_bank := valor and 3; // firetrap_bankselect_w
    $F003:
      ; // flip_screen_w
    $F004:
      begin // nmi_disable_w
        nmi_enable := (valor and 1) = 0;
        if not(nmi_enable) then
          z80_0.change_nmi(CLEAR_LINE);
      end;
    $F005:
      begin // mcu_w
        maincpu_to_mcu := valor;
        mcs51_0.change_irq0(ASSERT_LINE);
      end;
    $F008:
      bg1_scrollx := (bg1_scrollx and $FF00) or valor;
    $F009:
      bg1_scrollx := (bg1_scrollx and $FF) or (valor shl 8);
    $F00A:
      bg1_scrolly := (bg1_scrolly and $FF00) or valor;
    $F00B:
      bg1_scrolly := (bg1_scrolly and $FF) or (valor shl 8);
    $F00C:
      bg2_scrollx := (bg2_scrollx and $FF00) or valor;
    $F00D:
      bg2_scrollx := (bg2_scrollx and $FF) or (valor shl 8);
    $F00E:
      bg2_scrolly := (bg2_scrolly and $FF00) or valor;
    $F00F:
      bg2_scrolly := (bg2_scrolly and $FF) or (valor shl 8);
  end;
end;

function firetrap_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF, $8000 .. $FFFF:
      firetrap_snd_getbyte := mem_snd[direccion];
    $3400:
      firetrap_snd_getbyte := sound_latch;
    $4000 .. $7FFF:
      firetrap_snd_getbyte := snd_rom[snd_bank, direccion and $3FFF];
  end;
end;

procedure firetrap_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $7FF:
      mem_snd[direccion] := valor;
    $1000:
      ym3812_0.control(valor);
    $1001:
      ym3812_0.write(valor);
    $2000:
      begin // adpcm_data_w
        m6502_0.change_irq(CLEAR_LINE);
        msm5205_0.data_val:=valor;
      end;
    $2400:
      begin // sound_flip_flop_w
        msm5205_0.reset_w((valor and 1)=0);
        sound_irq_enable := (valor and 2) <> 0;
        if not(sound_irq_enable) then
          m6502_0.change_irq(CLEAR_LINE);
      end;
    $2800:
      snd_bank := valor and 1; // sound_bankselect_w
    $4000 .. $FFFF:
      ;
  end;
end;

procedure snd_adpcm;
begin
  msm5205_0.data_w(msm5205_0.data_val shr 4);
  msm5205_0.data_val:=msm5205_0.data_val shl 4;
  msm5205_toggle := msm5205_toggle xor 1;
  if (sound_irq_enable and (msm5205_toggle = 1)) then
    m6502_0.change_irq(ASSERT_LINE);
end;

function in_port0: byte;
var
  inserted: byte;
begin
  inserted := byte((coins and $E) = $E);
  in_port0 := (coins and $E) or inserted;
end;

procedure out_port1(valor: byte);
begin
  mcu_to_maincpu := valor;
end;

function in_port2: byte;
begin
  in_port2 := maincpu_to_mcu;
end;

procedure out_port3(valor: byte);
begin
  if (((mcu_p3 and 1) <> 0) and ((valor and 1) = 0)) then
    z80_0.change_irq(ASSERT_LINE);
  if (((mcu_p3 and 2) <> 0) and ((valor and 2) = 0)) then
    mcs51_0.change_irq0(CLEAR_LINE);
  mcu_p3 := valor;
end;

procedure firetrap_sound_update;
begin
  ym3812_0.update;
  msm5205_0.update;
end;

// Main
procedure reset_firetrap;
begin
  z80_0.reset;
  m6502_0.reset;
  mcs51_0.reset;
 msm5205_0.reset;
  ym3812_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $7F;
  main_bank := 0;
  snd_bank := 0;
  sound_latch := 0;
  mcu_to_maincpu := 0;
  maincpu_to_mcu := 0;
  msm5205_toggle := 0;
  mcu_p3 := 0;
  vblank := $80;
  coins := $FF;
  nmi_enable := false;
  sound_irq_enable := false;
  bg1_scrollx := 0;
  bg1_scrolly := 0;
  bg2_scrollx := 0;
  bg2_scrolly := 0;
end;

function start_firetrap: boolean;
var
  colores: tpaleta;
  f: word;
  memory_temp, ptemp: array [0 .. $1FFFF] of byte;
  bit0, bit1, bit2, bit3: byte;
const
  pc_x: array [0 .. 7] of dword = (3, 2, 1, 0, $200 * 8 * 8 + 3, $200 * 8 * 8 + 2, $200 * 8 * 8 + 1,
    $200 * 8 * 8 + 0);
  pt_x: array [0 .. 15] of dword = (3, 2, 1, 0, $400 * 32 * 8 + 3, $400 * 32 * 8 + 2,
    $400 * 32 * 8 + 1, $400 * 32 * 8 + 0, 16 * 8 + 3, 16 * 8 + 2, 16 * 8 + 1, 16 * 8 + 0,
    $400 * 32 * 8 + 16 * 8 + 3, $400 * 32 * 8 + 16 * 8 + 2, $400 * 32 * 8 + 16 * 8 + 1,
    $400 * 32 * 8 + 16 * 8 + 0);
  ps_x: array [0 .. 15] of dword = (7, 6, 5, 4, 3, 2, 1, 0, 16 * 8 + 7, 16 * 8 + 6, 16 * 8 + 5,
    16 * 8 + 4, 16 * 8 + 3, 16 * 8 + 2, 16 * 8 + 1, 16 * 8 + 0);
  ps_y: array [0 .. 15] of dword = (15 * 8, 14 * 8, 13 * 8, 12 * 8, 11 * 8, 10 * 8, 9 * 8, 8 * 8,
    7 * 8, 6 * 8, 5 * 8, 4 * 8, 3 * 8, 2 * 8, 1 * 8, 0 * 8);
  procedure convert_tiles(num: byte);
  begin
    copymemory(@memory_temp[0], @ptemp[0], $2000);
    copymemory(@memory_temp[$8000], @ptemp[$2000], $2000);
    copymemory(@memory_temp[$2000], @ptemp[$4000], $2000);
    copymemory(@memory_temp[$A000], @ptemp[$6000], $2000);
    copymemory(@memory_temp[$4000], @ptemp[$8000], $2000);
    copymemory(@memory_temp[$C000], @ptemp[$A000], $2000);
    copymemory(@memory_temp[$6000], @ptemp[$C000], $2000);
    copymemory(@memory_temp[$E000], @ptemp[$E000], $2000);
    copymemory(@memory_temp[$10000], @ptemp[$10000], $2000);
    copymemory(@memory_temp[$18000], @ptemp[$12000], $2000);
    copymemory(@memory_temp[$12000], @ptemp[$14000], $2000);
    copymemory(@memory_temp[$1A000], @ptemp[$16000], $2000);
    copymemory(@memory_temp[$14000], @ptemp[$18000], $2000);
    copymemory(@memory_temp[$1C000], @ptemp[$1A000], $2000);
    copymemory(@memory_temp[$16000], @ptemp[$1C000], $2000);
    copymemory(@memory_temp[$1E000], @ptemp[$1E000], $2000);
    init_gfx(num, 16, 16, $400);
    gfx_set_desc_data(4, 0, 32 * 8, 0, 4, $800 * 32 * 8 + 0, $800 * 32 * 8 + 4);
    convert_gfx(num, 0, @memory_temp, @pt_x, @ps_y, false, false);
  end;

begin
  start_firetrap := false;
  machine_calls.general_loop := firetrap_loop;
  machine_calls.reset := reset_firetrap;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 512, 511);
  screen_init(3, 512, 512);
  screen_mod_scroll(3, 512, 512, 511, 512, 512, 511);
  screen_init(4, 512, 512, false, true);
  main_screen.rot90_screen := true;
  start_video(256, 240);
  // Main CPU
  z80_0 := cpu_z80.create(12000000 div 2, 272 * CPU_SYNC);
  z80_0.change_ram_calls(firetrap_getbyte, firetrap_putbyte);
  if not(roms_load(@memory_temp, firetrap_rom)) then
    exit;
  copymemory(@memory[0], @memory_temp[0], $8000);
  for f := 0 to 3 do
    copymemory(@main_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // Sound CPU
  m6502_0 := cpu_m6502.create(12000000 div 8, 272 * CPU_SYNC, TCPU_M6502);
  m6502_0.change_ram_calls(firetrap_snd_getbyte, firetrap_snd_putbyte);
  m6502_0.init_sound(firetrap_sound_update);
  if not(roms_load(@memory_temp, firetrap_snd)) then
    exit;
  copymemory(@mem_snd[$8000], @memory_temp[0], $8000);
  for f := 0 to 1 do
    copymemory(@snd_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // MCU
  mcs51_0:=cpu_mcs51.create(I8X51,8000000,272*CPU_SYNC);
  mcs51_0.change_io_calls(in_port0, nil, in_port2, nil, nil, out_port1, nil, out_port3);
  if not(roms_load(mcs51_0.get_rom_addr, firetrap_mcu)) then
    exit;
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3526_FM, 3000000);
  msm5205_0:=MSM5205_chip.create(12000000 div 32,MSM5205_S48_4B,0.3,0);
  msm5205_0.change_advance(snd_adpcm);
  // convertir chars
  if not(roms_load(@memory_temp, firetrap_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(2, 0, 8 * 8, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y[8], false, false);
  // convertir bg
  if not(roms_load(@ptemp, firetrap_tiles)) then
    exit;
  convert_tiles(1);
  if not(roms_load(@ptemp, firetrap_tiles2)) then
    exit;
  convert_tiles(2);
  // convertir sprites
  if not(roms_load(@memory_temp, firetrap_sprites)) then
    exit;
  init_gfx(3, 16, 16, $400);
  gfx[3].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, $400 * 32 * 8, $800 * 32 * 8, $C00 * 32 * 8);
  convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, firetrap_pal)) then
    exit;
  for f := 0 to $FF do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    bit3 := (memory_temp[f] shr 3) and $01;
    colores[f].r := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    // green component
    bit0 := (memory_temp[f] shr 4) and $01;
    bit1 := (memory_temp[f] shr 5) and $01;
    bit2 := (memory_temp[f] shr 6) and $01;
    bit3 := (memory_temp[f] shr 7) and $01;
    colores[f].g := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    // blue component 
    bit0 := (memory_temp[f + $100] shr 0) and $01;
    bit1 := (memory_temp[f + $100] shr 1) and $01;
    bit2 := (memory_temp[f + $100] shr 2) and $01;
    bit3 := (memory_temp[f + $100] shr 3) and $01;
    colores[f].b := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
  end;
  set_pal(colores, $100);
  // DIP
  marcade.dswa := $DF;
  marcade.dswb := $FF;
  marcade.dswa_val := @firetrap_dip_a;
  marcade.dswb_val := @firetrap_dip_b;
  // final
  reset_firetrap;
  start_firetrap := true;
end;

end.
