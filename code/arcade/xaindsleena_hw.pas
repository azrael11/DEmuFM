unit xaindsleena_hw;

interface

uses
  WinApi.Windows,
  m6809,
  m6805,
  ym_2203,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine;

function start_xainnsleena: boolean;

implementation

const
  xain_rom: array [0 .. 1] of tipo_roms = ((n: 'p9-08.ic66'; l: $8000; p: 0; crc: $5179AE3F), (n: 'pa-09.ic65'; l: $8000; p: $8000; crc: $10A7C800));
  xain_sub: array [0 .. 1] of tipo_roms = ((n: 'p1-0.ic29'; l: $8000; p: 0; crc: $A1A860E2), (n: 'p0-0.ic15'; l: $8000; p: $8000; crc: $948B9757));
  xain_snd: tipo_roms = (n: 'p2-0.ic49'; l: $8000; p: $8000; crc: $A5318CB8);
  xain_mcu: tipo_roms = (n: 'pz-0.113'; l: $800; p: 0; crc: $A432A907);
  xain_char: tipo_roms = (n: 'pb-01.ic24'; l: $8000; p: 0; crc: $83C00DD8);
  xain_tiles1: array [0 .. 5] of tipo_roms = ((n: 'p5-0.ic44'; l: $8000; p: 0; crc: $5C6C453C), (n: 'p4-0.ic45'; l: $8000; p: $8000; crc: $59D87A9A), (n: 'p3-0.ic46'; l: $8000; p: $10000; crc: $84884A2E), (n: 'p6-0.ic43'; l: $8000; p: $20000; crc: $8D637639), (n: 'p7-0.ic42';
    l: $8000; p: $28000; crc: $71EEC4E6), (n: 'p8-0.ic41'; l: $8000; p: $30000; crc: $7FC9704F));
  xain_tiles2: array [0 .. 7] of tipo_roms = ((n: 'pk-0.ic136'; l: $8000; p: 0; crc: $11EB4247), (n: 'pl-0.ic135'; l: $8000; p: $8000; crc: $422B536E), (n: 'pm-0.ic134'; l: $8000; p: $10000; crc: $828C1B0C), (n: 'pn-0.ic133'; l: $8000; p: $18000; crc: $D37939E0),
    (n: 'pc-0.ic114'; l: $8000; p: $20000; crc: $8F0AA1A7), (n: 'pd-0.ic113'; l: $8000; p: $28000; crc: $45681910), (n: 'pe-0.ic112'; l: $8000; p: $30000; crc: $A8EEABC8), (n: 'pf-0.ic111'; l: $8000; p: $38000; crc: $E59A2F27));
  xain_sprites: array [0 .. 7] of tipo_roms = ((n: 'po-0.ic131'; l: $8000; p: 0; crc: $252976AE), (n: 'pp-0.ic130'; l: $8000; p: $8000; crc: $E6F1E8D5), (n: 'pq-0.ic129'; l: $8000; p: $10000; crc: $785381ED), (n: 'pr-0.ic128'; l: $8000; p: $18000; crc: $59754E3D),
    (n: 'pg-0.ic109'; l: $8000; p: $20000; crc: $4D977F33), (n: 'ph-0.ic108'; l: $8000; p: $28000; crc: $3F3B62A0), (n: 'pi-0.ic107'; l: $8000; p: $30000; crc: $76641EE3), (n: 'pj-0.ic106'; l: $8000; p: $38000; crc: $37671F36));
  // Dip
  xain_dip_a: array [0 .. 6] of def_dip2 = ((mask: 3; name: 'Coin B'; number: 4; val4: (0, 3, 2, 1); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $C; name: 'Coin A'; number: 4; val4: (0, $C, 8, 4); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $10;
    name: 'Demo Sounds'; number: 2; val2: (0, $10); name2: ('Off', 'On')), (mask: $20; name: 'Allow Continue'; number: 2; val2: (0, $20); name2: ('No', 'Yes')), (mask: $40; name: 'Cabinet'; number: 2; val2: (0, $40); name2: ('Upright', 'Cocktail')), (mask: $80;
    name: 'Flip Screen'; number: 2; val2: (0, $80); name2: ('Off', 'On')), ());
  xain_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Difficulty'; number: 4; val4: (3, 2, 1, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $C; name: 'Game Time'; number: 4; val4: ($C, 8, 4, 0); name4: ('Slow', 'Normal', 'Fast', 'Very Fast')), (mask: $30;
    name: 'Bonus Life'; number: 4; val4: ($30, $20, $10, 0); name4: ('20k 70k+', '30k 80k+', '20k 80k', '30k 80k')), (mask: $C0; name: 'Lives'; number: 4; val4: ($C0, $80, $40, 0); name4: ('3', '4', '6', 'Infinite')), ());
  CPU_SYNC = 4;

var
  main_rom, sub_rom: array [0 .. 1, 0 .. $3FFF] of byte;
  banco_main, banco_sub, soundlatch, xain_pri, vblank: byte;
  scroll_x_p1, scroll_y_p1, scroll_x_p0, scroll_y_p0: word;
  // mcu
  mcu_mem: array [0 .. $7FF] of byte;
  port_c_in, port_c_out, port_b_out, port_b_in, port_a_in, port_a_out: byte;
  ddr_a, ddr_b, ddr_c, from_main, from_mcu: byte;
  mcu_accept, mcu_ready: boolean;

procedure update_video_xain;
  procedure chars(trans: boolean);
  var
    x, y, color, f, nchar: word;
    atrib: byte;
  begin
    for f := 0 to $3FF do
    begin
      atrib := memory[$2400 + f];
      color := (atrib and $E0) shr 5;
      if (gfx[0].buffer[f] or buffer_color[color]) then
      begin
        y := f div 32;
        x := f mod 32;
        nchar := memory[$2000 + f] + ((atrib and 3) shl 8);
        if trans then
          put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 1, 0)
        else
          put_gfx(x * 8, y * 8, nchar, color shl 4, 1, 0);
        gfx[0].buffer[f] := false;
      end;
    end;
    update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  end;
  procedure sprites;
  var
    x, y, color, f, nchar: word;
    atrib: byte;
  begin
    for f := 0 to $5F do
    begin
      atrib := memory[$3801 + (f * 4)];
      nchar := memory[$3802 + (f * 4)] or ((atrib and 7) shl 8);
      if nchar <> 0 then
      begin
        color := ((atrib and $38) shl 1) + 128;
        x := 238 - memory[$3803 + (f * 4)];
        y := 240 - memory[$3800 + (f * 4)];
        if (atrib and $80) <> 0 then
        begin
          put_gfx_sprite_diff(nchar, color, (atrib and $40) <> 0, false, 1, 0, 0);
          put_gfx_sprite_diff(nchar + 1, color, (atrib and $40) <> 0, false, 1, 0, 16);
          actualiza_gfx_sprite_size(x, y - 16, 4, 16, 32);
        end
        else
        begin
          put_gfx_sprite(nchar, color, (atrib and $40) <> 0, false, 1);
          update_gfx_sprite(x, y, 4, 1);
        end;
      end;
    end;
  end;
  procedure tiles_0(trans: boolean);
  var
    x, y, color, f, nchar, pos: word;
    atrib: byte;
  begin
    for f := 0 to $3FF do
    begin
      y := f div 32;
      x := f mod 32;
      pos := (x and $F) + ((y and $F) shl 4) + ((x and $10) shl 4) + ((y and $10) shl 5);
      atrib := memory[$3400 + pos];
      color := (atrib and $70) shr 4;
      if (gfx[2].buffer[pos] or buffer_color[color + 8]) then
      begin
        nchar := memory[$3000 + pos] + ((atrib and 7) shl 8);
        if trans then
          put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + 384, 2, 2, (atrib and $80) <> 0, false)
        else
          put_gfx_flip(x * 16, y * 16, nchar, (color shl 4) + 384, 2, 2, (atrib and $80) <> 0, false);
        gfx[2].buffer[pos] := false;
      end;
    end;
    scroll_x_y(2, 4, scroll_x_p0, scroll_y_p0);
  end;
  procedure tiles_1(trans: boolean);
  var
    x, y, color, f, nchar, pos: word;
    atrib: byte;
  begin
    for f := 0 to $3FF do
    begin
      y := f div 32;
      x := f mod 32;
      pos := (x and $F) + ((y and $F) shl 4) + ((x and $10) shl 4) + ((y and $10) shl 5);
      atrib := memory[$2C00 + pos];
      color := (atrib and $70) shr 4;
      if (gfx[3].buffer[pos] or buffer_color[color + $10]) then
      begin
        nchar := memory[$2800 + pos] + ((atrib and 7) shl 8);
        if trans then
          put_gfx_trans_flip(x * 16, y * 16, nchar, (color shl 4) + 256, 3, 3, (atrib and $80) <> 0, false)
        else
          put_gfx_flip(x * 16, y * 16, nchar, (color shl 4) + 256, 3, 3, (atrib and $80) <> 0, false);
        gfx[3].buffer[pos] := false;
      end;
    end;
    scroll_x_y(3, 4, scroll_x_p1, scroll_y_p1);
  end;

begin
  case xain_pri of
    0:
      begin // bg2 bg1 sprt char
        tiles_0(false);
        tiles_1(true);
        sprites;
        chars(true);
      end;
    1:
      begin // bg1 bg2 sprt char
        tiles_1(false);
        tiles_0(true);
        sprites;
        chars(true);
      end;
    2:
      begin // char bg2 sprt bg1
        chars(false);
        tiles_0(true);
        sprites;
        tiles_1(true);
      end;
    3:
      begin // char bg1 sprt bg2
        chars(false);
        tiles_1(true);
        sprites;
        tiles_0(true);
      end;
    4:
      begin // bg2 char sprt bg1
        tiles_0(false);
        chars(true);
        sprites;
        tiles_1(true);
      end;
    5:
      begin // bg1 char sprt bg2
        tiles_1(false);
        chars(true);
        sprites;
        tiles_0(true);
      end;
    6:
      begin // bg2 sprt bg1 char
        tiles_0(false);
        sprites;
        tiles_1(true);
        chars(true);
      end;
    7:
      begin // bg1 sprt bg2 char
        tiles_1(false);
        sprites;
        tiles_0(true);
        chars(true);
      end;
  end;
  update_final_piece(0, 8, 256, 240, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_xain;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure xain_loop;
var
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 271 do
      begin
        events_xain;
        case f of
          0:
            vblank := 0;
          16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 264:
            m6809_0.change_firq(ASSERT_LINE);
          239:
            vblank := $20;
          240:
            begin
              m6809_0.change_nmi(ASSERT_LINE);
              update_video_xain;
              m6809_0.change_firq(ASSERT_LINE);
            end;
        end;
        for h := 1 to CPU_SYNC do
        begin
          // main
          m6809_0.run(frame_main);
          frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
          // sub
          m6809_1.run(frame_sub);
          frame_sub := frame_sub + m6809_1.tframes - m6809_1.contador;
          // snd
          m6809_2.run(frame_snd);
          frame_snd := frame_snd + m6809_2.tframes - m6809_2.contador;
          // mcu
          m6805_0.run(frame_mcu);
          frame_mcu := frame_mcu + m6805_0.tframes - m6805_0.contador;
        end;
      end;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function mcu_xain_hw_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FF;
  case direccion of
    0:
      mcu_xain_hw_getbyte := (port_a_out and ddr_a) or (port_a_in and not(ddr_a));
    1:
      mcu_xain_hw_getbyte := (port_b_out and ddr_b) or (port_b_in and not(ddr_b));
    2:
      begin
        port_c_in := 0;
        if not(mcu_accept) then
          port_c_in := port_c_in or 1;
        if mcu_ready then
          port_c_in := port_c_in or 2;
        mcu_xain_hw_getbyte := (port_c_out and ddr_c) or (port_c_in and not(ddr_c));
      end;
    $10 .. $7FF:
      mcu_xain_hw_getbyte := mcu_mem[direccion];
  end;
end;

procedure mcu_xain_hw_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FF;
  case direccion of
    0:
      port_a_out := valor;
    1:
      begin
        if (((ddr_b and 2) <> 0) and ((not(valor) and 2) <> 0)) then
        begin
          port_a_in := from_main;
        end
        else
        begin
          if (((ddr_b and 2) <> 0) and ((not(port_b_out) and 2) <> 0) and ((valor and 2) <> 0)) then
          begin
            mcu_accept := true;
            m6805_0.irq_request(0, CLEAR_LINE);
          end;
        end;
        if (((ddr_b and 4) <> 0) and ((valor and 4) <> 0) and ((not(port_b_out) and 4) <> 0)) then
        begin
          from_mcu := port_a_out;
          mcu_ready := false;
        end;
        port_b_out := valor;
      end;
    2:
      port_c_out := valor;
    4:
      ddr_a := valor;
    5:
      ddr_b := valor;
    6:
      ddr_c := valor;
    $10 .. $7F:
      mcu_mem[direccion] := valor;
    $80 .. $7FF:
      ; // ROM
  end;
end;

function xain_getbyte(direccion: word): byte;
  function mcu_status_r: byte;
  begin
    mcu_status_r := (byte(mcu_ready) shl 3) or (byte(mcu_accept) shl 4);
  end;

begin
  case direccion of
    0 .. $397F, $8000 .. $FFFF:
      xain_getbyte := memory[direccion];
    $3A00:
      xain_getbyte := marcade.in0;
    $3A01:
      xain_getbyte := marcade.in1;
    $3A02:
      xain_getbyte := marcade.dswa;
    $3A03:
      xain_getbyte := marcade.dswb;
    $3A04:
      begin
        mcu_ready := true;
        xain_getbyte := from_mcu;
      end;
    $3A05:
      xain_getbyte := $C7 or mcu_status_r or vblank; // VBlank
    $3A06:
      begin
        mcu_ready := true;
        mcu_accept := true;
        m6805_0.irq_request(0, CLEAR_LINE);
        xain_getbyte := $FF;
      end;
    $4000 .. $7FFF:
      xain_getbyte := main_rom[banco_main, direccion and $3FFF]
  end;
end;

procedure xain_putbyte(direccion: word; valor: byte);
  procedure change_color(pos: word);
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[pos];
    color.r := pal4bit(tmp_color);
    color.g := pal4bit(tmp_color shr 4);
    tmp_color := buffer_paleta[pos + $200];
    color.b := pal4bit(tmp_color);
    set_pal_color(color, pos);
    case pos of
      0 .. 127:
        buffer_color[pos shr 4] := true;
      256 .. 383:
        buffer_color[((pos shr 4) and 7) + $10] := true;
      384 .. 511:
        buffer_color[((pos shr 4) and 7) + 8] := true;
    end;
  end;

begin
  case direccion of
    0 .. $1FFF, $3800 .. $397F:
      memory[direccion] := valor;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $2800 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[3].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $3000 .. $37FF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $3A00:
      scroll_x_p1 := (scroll_x_p1 and $100) or valor;
    $3A01:
      scroll_x_p1 := (scroll_x_p1 and $FF) or ((valor and 1) shl 8);
    $3A02:
      scroll_y_p1 := (scroll_y_p1 and $100) or valor;
    $3A03:
      scroll_y_p1 := (scroll_y_p1 and $FF) or ((valor and 1) shl 8);
    $3A04:
      scroll_x_p0 := (scroll_x_p0 and $100) or valor;
    $3A05:
      scroll_x_p0 := (scroll_x_p0 and $FF) or ((valor and 1) shl 8);
    $3A06:
      scroll_y_p0 := (scroll_y_p0 and $100) or valor;
    $3A07:
      scroll_y_p0 := (scroll_y_p0 and $FF) or ((valor and 1) shl 8);
    $3A08:
      begin
        soundlatch := valor;
        m6809_2.change_irq(HOLD_LINE);
      end;
    $3A09:
      m6809_0.change_nmi(CLEAR_LINE);
    $3A0A:
      m6809_0.change_firq(CLEAR_LINE);
    $3A0B:
      m6809_0.change_irq(CLEAR_LINE);
    $3A0C:
      m6809_1.change_irq(ASSERT_LINE);
    $3A0D:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $3A0E:
      begin
        from_main := valor;
        mcu_accept := false;
        m6805_0.irq_request(0, ASSERT_LINE);
      end;
    $3A0F:
      begin
        if (xain_pri <> valor and 7) then
        begin
          xain_pri := valor and 7;
          fillchar(gfx[0].buffer[0], $400, 1);
          fillchar(gfx[2].buffer[0], $400, 1);
          fillchar(gfx[3].buffer[0], $400, 1);
        end;
        banco_main := (valor shr 3) and 1;
      end;
    $3C00 .. $3FFF:
      if buffer_paleta[direccion and $3FF] <> valor then
      begin
        buffer_paleta[direccion and $3FF] := valor;
        change_color(direccion and $1FF);
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function xain_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      xain_sub_getbyte := memory[direccion];
    $4000 .. $7FFF:
      xain_sub_getbyte := sub_rom[banco_sub, direccion and $3FFF];
    $8000 .. $FFFF:
      xain_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure xain_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      memory[direccion] := valor;
    $2000:
      m6809_0.change_irq(ASSERT_LINE);
    $2800:
      m6809_1.change_irq(CLEAR_LINE);
    $3000:
      banco_sub := valor and 1;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function xain_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF, $4000 .. $FFFF:
      xain_snd_getbyte := mem_snd[direccion];
    $1000:
      xain_snd_getbyte := soundlatch;
  end;
end;

procedure xain_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $2800:
      ym2203_0.Control(valor);
    $2801:
      ym2203_0.Write(valor);
    $3000:
      ym2203_1.Control(valor);
    $3001:
      ym2203_1.Write(valor);
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

procedure snd_irq(irqstate: byte);
begin
  m6809_2.change_firq(irqstate);
end;

procedure xain_sound_update;
begin
  ym2203_0.update;
  ym2203_1.update;
end;

// Main
procedure reset_xain;
begin
  m6809_0.reset;
  m6809_1.reset;
  m6809_2.reset;
  m6805_0.reset;
  ym2203_0.reset;
  ym2203_1.reset;
  frame_main := m6809_0.tframes;
  frame_sub := m6809_1.tframes;
  frame_snd := m6809_2.tframes;
  frame_mcu := m6805_0.tframes;
  banco_main := 0;
  banco_sub := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  soundlatch := 0;
  vblank := 0;
  xain_pri := 0;
  scroll_x_p1 := 0;
  scroll_y_p1 := 0;
  scroll_x_p0 := 0;
  scroll_y_p0 := 0;
  // mcu
  port_a_in := 0;
  port_a_out := 0;
  ddr_a := 0;
  port_b_in := 0;
  port_b_out := 0;
  ddr_b := 0;
  port_c_in := 0;
  port_c_out := 0;
  ddr_c := 0;
  mcu_accept := true;
  mcu_ready := true;
  from_main := 0;
  from_mcu := 0;
end;

function start_xainnsleena: boolean;
var
  f: word;
  memory_temp: array [0 .. $3FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (1, 0, 8 * 8 + 1, 8 * 8 + 0, 16 * 8 + 1, 16 * 8 + 0, 24 * 8 + 1, 24 * 8 + 0);
  ps_x: array [0 .. 15] of dword = (3, 2, 1, 0, 16 * 8 + 3, 16 * 8 + 2, 16 * 8 + 1, 16 * 8 + 0, 32 * 8 + 3, 32 * 8 + 2, 32 * 8 + 1, 32 * 8 + 0, 48 * 8 + 3, 48 * 8 + 2, 48 * 8 + 1, 48 * 8 + 0);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  start_xainnsleena := false;
  machine_calls.general_loop := xain_loop;
  machine_calls.reset := reset_xain;
  machine_calls.fps_max := 6000000 / 384 / 272;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 512, 512, true);
  screen_mod_scroll(3, 512, 256, 511, 512, 256, 511);
  screen_init(4, 512, 512, false, true);
  screen_mod_sprites(4, 256, 256, $FF, $FF);
  start_video(256, 240);
  // Main CPU
  m6809_0 := cpu_m6809.Create(1500000, 272 * CPU_SYNC, TCPU_M6809);
  m6809_0.change_ram_calls(xain_getbyte, xain_putbyte);
  if not(roms_load(@memory_temp, xain_rom)) then
    exit;
  copymemory(@memory[$8000], @memory_temp, $8000);
  for f := 0 to 1 do
    copymemory(@main_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // Sub CPU
  m6809_1 := cpu_m6809.Create(1500000, 272 * CPU_SYNC, TCPU_M6809);
  m6809_1.change_ram_calls(xain_sub_getbyte, xain_sub_putbyte);
  if not(roms_load(@memory_temp, xain_sub)) then
    exit;
  copymemory(@mem_misc[$8000], @memory_temp, $8000);
  for f := 0 to 1 do
    copymemory(@sub_rom[f, 0], @memory_temp[$8000 + (f * $4000)], $4000);
  // Sound CPU
  m6809_2 := cpu_m6809.Create(1500000, 272 * CPU_SYNC, TCPU_M6809);
  m6809_2.change_ram_calls(xain_snd_getbyte, xain_snd_putbyte);
  m6809_2.init_sound(xain_sound_update);
  if not(roms_load(@mem_snd, xain_snd)) then
    exit;
  // MCU CPU
  m6805_0 := cpu_m6805.Create(3000000, 272 * CPU_SYNC, tipo_m68705);
  m6805_0.change_ram_calls(mcu_xain_hw_getbyte, mcu_xain_hw_putbyte);
  if not(roms_load(@mcu_mem, xain_mcu)) then
    exit;
  // Sound Chip
  ym2203_0 := ym2203_chip.Create(3000000);
  ym2203_0.change_irq_calls(snd_irq);
  ym2203_1 := ym2203_chip.Create(3000000);
  // chars
  if not(roms_load(@memory_temp, xain_char)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 2, 4, 6);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  // sprites
  if not(roms_load(@memory_temp, xain_sprites)) then
    exit;
  init_gfx(1, 16, 16, $800);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $8000 * 4 * 8 + 0, $8000 * 4 * 8 + 4, 0, 4);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // tiles1
  if not(roms_load(@memory_temp, xain_tiles1)) then
    exit;
  init_gfx(2, 16, 16, $800);
  gfx[2].trans[0] := true;
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // tiles2
  if not(roms_load(@memory_temp, xain_tiles2)) then
    exit;
  init_gfx(3, 16, 16, $800);
  gfx[3].trans[0] := true;
  convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := $3F;
  marcade.dswb := $FF;
  marcade.dswa_val2 := @xain_dip_a;
  marcade.dswb_val2 := @xain_dip_b;
  // final
  start_xainnsleena := true;
end;

end.
