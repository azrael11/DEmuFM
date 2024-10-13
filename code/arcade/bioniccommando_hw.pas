unit bioniccommando_hw;

interface

uses
  WinApi.Windows,
  nz80,
  mcs51,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine;

function start_bioniccommando: boolean;

implementation

const
  bionicc_rom: array [0 .. 3] of tipo_roms = ((n: 'tse_02.1a'; l: $10000; p: 0; crc: $E4AEEFAA),
    (n: 'tse_04.1b'; l: $10000; p: $1; crc: $D0C8EC75), (n: 'tse_03.2a'; l: $10000; p: $20000;
    crc: $B2AC0A45), (n: 'tse_05.2b'; l: $10000; p: $20001; crc: $A79CB406));
  bionicc_sound: tipo_roms = (n: 'ts_01b.4e'; l: $8000; p: 0; crc: $A9A6CAFA);
  bionicc_mcu: tipo_roms = (n: 'ts.2f'; l: $1000; p: 0; crc: $3ED7F0BE);
  bionicc_char: tipo_roms = (n: 'tsu_08.8l'; l: $8000; p: 0; crc: $9BF0B7A2);
  bionicc_bg: array [0 .. 1] of tipo_roms = ((n: 'tsu_07.5l'; l: $8000; p: 0; crc: $9469EFA4),
    (n: 'tsu_06.4l'; l: $8000; p: $8000; crc: $40BF0EB4));
  bionicc_fg: array [0 .. 7] of tipo_roms = ((n: 'ts_12.17f'; l: $8000; p: 0; crc: $E4B4619E),
    (n: 'ts_11.15f'; l: $8000; p: $8000; crc: $AB30237A), (n: 'ts_17.17g'; l: $8000; p: $10000;
    crc: $DEB657E4), (n: 'ts_16.15g'; l: $8000; p: $18000; crc: $D363B5F9), (n: 'ts_13.18f'; l: $8000;
    p: $20000; crc: $A8F5A004), (n: 'ts_18.18g'; l: $8000; p: $28000; crc: $3B36948C), (n: 'ts_23.18j';
    l: $8000; p: $30000; crc: $BBFBE58A), (n: 'ts_24.18k'; l: $8000; p: $38000; crc: $F156E564));
  bionicc_sprites: array [0 .. 7] of tipo_roms = ((n: 'tse_10.13f'; l: $8000; p: 0; crc: $D28EEACC),
    (n: 'tsu_09.11f'; l: $8000; p: $8000; crc: $6A049292), (n: 'tse_15.13g'; l: $8000; p: $10000;
    crc: $9B5593C0), (n: 'tsu_14.11g'; l: $8000; p: $18000; crc: $46B2AD83), (n: 'tse_20.13j'; l: $8000;
    p: $20000; crc: $B03DB778), (n: 'tsu_19.11j'; l: $8000; p: $28000; crc: $B5C82722), (n: 'tse_22.17j';
    l: $8000; p: $30000; crc: $D4DEDEB3), (n: 'tsu_21.15j'; l: $8000; p: $38000; crc: $98777006));
  // DIP
  bionicc_dip: array [0 .. 8] of def_dip = ((mask: $7; name: 'Coin A'; number: 8;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $1; dip_name: '3C 1C'), (dip_val: $2;
    dip_name: '2C 1C'), (dip_val: $7; dip_name: '1C 1C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $5;
    dip_name: '1C 3C'), (dip_val: $4; dip_name: '1C 4C'), (dip_val: $3; dip_name: '1C 6C'), (), (), (), (),
    (), (), (), ())), (mask: $38; name: 'Coin B'; number: 8;
    dip: ((dip_val: $0; dip_name: '4C 1C'), (dip_val: $8; dip_name: '3C 1C'), (dip_val: $10;
    dip_name: '2C 1C'), (dip_val: $38; dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C 2C'), (dip_val: $28;
    dip_name: '1C 3C'), (dip_val: $20; dip_name: '1C 4C'), (dip_val: $18; dip_name: '1C 6C'), (), (), (), (),
    (), (), (), ())), (mask: $80; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $300; name: 'Lives'; number: 4;
    dip: ((dip_val: $300; dip_name: '3'), (dip_val: $200; dip_name: '4'), (dip_val: $100;
    dip_name: '5'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $400; name: 'Cabinet'; number: 2; dip: ((dip_val: $400; dip_name: 'Upright'), (dip_val: $0;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $1800;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $1800; dip_name: '20k 40k 100k 60k+'), (dip_val: $1000;
    dip_name: '30k 50k 120k 70k+'), (dip_val: $800; dip_name: '20k 60k'), (dip_val: $0;
    dip_name: '30k 70k'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $6000; name: 'Difficulty';
    number: 4; dip: ((dip_val: $4000; dip_name: 'Easy'), (dip_val: $6000; dip_name: 'Medium'),
    (dip_val: $2000; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $8000; name: 'Freeze'; number: 2;
    dip: ((dip_val: $8000; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (),
    (), (), (), (), (), ())), ());

var
  scroll_fg_x, scroll_fg_y, scroll_bg_x, scroll_bg_y: word;
  rom: array [0 .. $1FFFF] of word;
  ram, ram2, fg_ram, bg_ram: array [0 .. $1FFF] of word;
  txt_ram: array [0 .. $7FF] of word;
  // MCU
  audiocpu_to_mcu, mcu_to_audiocpu, mcu_p1, mcu_p3: byte;

procedure update_video_bionicc;
var
  f, color, x, y, nchar, atrib, sx, sy, pos: word;
begin
  fill_full_screen(5, $400);
  for f := $0 to $440 do
  begin
    // BG
    x := f mod 33;
    y := f div 33;
    sx := x + ((scroll_bg_x and $1F8) shr 3);
    sy := y + ((scroll_bg_y and $1F8) shr 3);
    pos := (sx and $3F) + ((sy and $3F) * 64);
    atrib := bg_ram[(pos shl 1) + 1] and $FF;
    color := (atrib and $18) shr 3;
    if (gfx[1].buffer[pos] or buffer_color[color + $40]) then
    begin
      nchar := (bg_ram[pos shl 1] and $FF) or ((atrib and $7) shl 8);
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 2, 1, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[1].buffer[f] := false;
    end;
  end;
  // FG
  for f := $0 to $120 do
  begin // $121=17*17
    x := f mod 17; // 17 --> numero de filas (numero de x) que queremos
    y := f div 17;
    // scroll and [numero_maximo_scroll-long_gfx_x] shr [numero bits long_gfx_x] (por ejemplo 16 bits --> shr 4)
    sx := x + ((scroll_fg_x and $3F0) shr 4);
    sy := y + ((scroll_fg_y and $3F0) shr 4);
    // sx and [numero_maximo_scroll-long_gfx_x] shr [numero bits long_gfx_x] por ejemplo antes $3f0 shr 4=$3f
    // (sy and [igual que antes])*[numero de filas de la pantalla total] (este caso 1024/16=64)
    pos := (sx and $3F) + ((sy and $3F) * 64);
    atrib := fg_ram[(pos shl 1) + 1] and $FF;
    if (atrib and $C0) <> $C0 then
    begin
      color := (atrib and $18) shr 3;
      if (gfx[2].buffer[pos] or buffer_color[color + $44]) then
      begin
        nchar := (fg_ram[pos shl 1] and $FF) or ((atrib and $7) shl 8);
        put_gfx_trans_flip_alt(x * 16, y * 16, nchar, (color shl 4) + 256, 3, 2, (atrib and $80) <> 0,
          (atrib and $40) <> 0, 0);
        if (atrib and $20) <> 0 then
          put_gfx_trans_flip_alt(x * 16, y * 16, nchar, (color shl 4) + 256, 4, 2, (atrib and $80) <> 0,
            (atrib and $40) <> 0, 1)
        else
          put_gfx_block_trans(x * 16, y * 16, 4, 16, 16);
        gfx[2].buffer[pos] := false;
      end;
    end;
  end;
  // text
  for f := $0 to $3FF do
  begin
    atrib := txt_ram[$400 + f] and $FF;
    color := atrib and $3F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f and $1F;
      y := f shr 5;
      nchar := (txt_ram[f] and $FF) or ((atrib and $C0) shl 2);
      put_gfx_trans(x * 8, y * 8, nchar, (color shl 2) + 768, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // back
  scroll_x_y(2, 5, scroll_bg_x and $7, scroll_bg_y and $7);
  scroll_x_y(3, 5, scroll_fg_x and $F, scroll_fg_y and $F);
  // sprites
  for f := $9F downto 0 do
  begin
    nchar := buffer_sprites_w[f * 4] and $7FF;
    if nchar <> $7FF then
    begin
      atrib := buffer_sprites_w[(f * 4) + 1];
      color := ((atrib and $3C) shl 2) + 512;
      y := buffer_sprites_w[(f * 4) + 2];
      x := buffer_sprites_w[(f * 4) + 3];
      put_gfx_sprite(nchar, color, (atrib and 2) <> 0, false, 3);
      update_gfx_sprite(x, y, 5, 3);
    end;
  end;
  scroll_x_y(4, 5, scroll_fg_x and $F, scroll_fg_y and $F);
  // front
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 5);
  update_final_piece(0, 16, 256, 224, 5);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_bionicc;
begin
  if event.arcade then
  begin
    // P2
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FFEF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    // P1
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    // system
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
  end;
end;

procedure bionicc_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        // mcu
        mcs51_0.run(frame_mcu);
        frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
        case f of
          127:
            m68000_0.irq[4] := HOLD_LINE;
          239:
            begin
              m68000_0.irq[2] := HOLD_LINE;
              update_video_bionicc;
              copymemory(@buffer_sprites_w, @ram[$400], $280 * 2);
            end;
        end;
      end;
      events_bionicc;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function bionicc_getword(direccion: dword): word;
begin
  direccion := direccion and $FFFFF;
  case direccion of
    0 .. $3FFFF:
      bionicc_getword := rom[direccion shr 1];
    $E0000 .. $E3FFF:
      bionicc_getword := ram[(direccion and $3FFF) shr 1];
    $E4000 .. $E7FFF:
      case (direccion and 3) of
        0:
          bionicc_getword := marcade.in0;
        2:
          bionicc_getword := marcade.dswa;
      end;
    $EC000 .. $EFFFF:
      bionicc_getword := txt_ram[(direccion and $FFF) shr 1];
    $F0000 .. $F3FFF:
      bionicc_getword := fg_ram[(direccion and $3FFF) shr 1];
    $F4000 .. $F7FFF:
      bionicc_getword := bg_ram[(direccion and $3FFF) shr 1];
    $F8000 .. $F87FF:
      bionicc_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $FC000 .. $FFFFF:
      bionicc_getword := ram2[(direccion and $3FFF) shr 1];
  end;
end;

procedure bionicc_putword(direccion: dword; valor: word);

  procedure cambiar_color(pos, data: word);
  var
    bright: byte;
    color: tcolor;
  begin
    bright := data and $0F;
    color.r := ((data shr 12) and $0F) * $11;
    color.g := ((data shr 8) and $0F) * $11;
    color.b := ((data shr 4) and $0F) * $11;
    if ((bright and $08) = 0) then
    begin
      color.r := color.r * ($07 + bright) div $0E;
      color.g := color.g * ($07 + bright) div $0E;
      color.b := color.b * ($07 + bright) div $0E;
    end;
    set_pal_color(color, pos);
    case pos of
      0 .. 63:
        buffer_color[(pos shr 4) + $40] := true;
      256 .. 319:
        buffer_color[((pos shr 4) and $3) + $44] := true;
      768 .. 1023:
        buffer_color[(pos shr 2) and $3F] := true;
    end;
  end;

begin
  direccion := direccion and $FFFFF;
  case direccion of
    0 .. $3FFFF:
      ;
    $E0000 .. $E3FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $E4000 .. $E7FFF:
      case (direccion and 3) of
        0:
          ; // flip
        2:
          z80_0.change_nmi(PULSE_LINE);
      end;
    $E8010:
      if scroll_fg_x <> valor then
      begin
        if abs((scroll_fg_x and $3F0) - (valor and $3F0)) > 15 then
          fillchar(gfx[2].buffer, $1000, 1);
        scroll_fg_x := valor and $3FF;
      end;
    $E8012:
      if scroll_fg_y <> valor then
      begin
        if abs((scroll_fg_y and $3F0) - (valor and $3F0)) > 15 then
          fillchar(gfx[2].buffer, $1000, 1);
        scroll_fg_y := valor and $3FF;
      end;
    $E8014:
      if scroll_bg_x <> valor then
      begin
        if abs((scroll_bg_x and $1F8) - (valor and $1F8)) > 7 then
          fillchar(gfx[1].buffer, $1000, 1);
        scroll_bg_x := valor and $1FF;
      end;
    $E8016:
      if scroll_bg_y <> valor then
      begin
        if abs((scroll_bg_y and $1F8) - (valor and $1F8)) > 7 then
          fillchar(gfx[1].buffer, $1000, 1);
        scroll_bg_y := valor and $1FF;
      end;
    $E801A:
      begin // dmaon_w
        mcs51_0.change_irq0(ASSERT_LINE);
        m68000_0.change_halt(ASSERT_LINE);
      end;
    $EC000 .. $EFFFF:
      if txt_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        txt_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $F0000 .. $F3FFF:
      if fg_ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        fg_ram[(direccion and $3FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $F4000 .. $F7FFF:
      if bg_ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $3FFF) shr 1] := valor;
        gfx[2].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $F8000 .. $F87FF:
      if (buffer_paleta[(direccion and $7FF) shr 1] <> valor) then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        cambiar_color((direccion and $7FF) shr 1, valor);
      end;
    $FC000 .. $FFFFF:
      ram2[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function bionicc_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $C7FF:
      bionicc_snd_getbyte := mem_snd[direccion];
    $8001:
      bionicc_snd_getbyte := ym2151_0.status;
    $A000:
      bionicc_snd_getbyte := mcu_to_audiocpu;
  end;
end;

procedure bionicc_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000:
      ym2151_0.reg(valor);
    $8001:
      ym2151_0.write(valor);
    $A000:
      audiocpu_to_mcu := valor;
    $C000 .. $C7FF:
      mem_snd[direccion] := valor;
  end;
end;

procedure bionicc_sound_update;
begin
  ym2151_0.update;
end;

function in_port1: byte;
begin
  in_port1 := audiocpu_to_mcu;
end;

procedure out_port1(valor: byte);
begin
  mcu_p1 := valor;
end;

procedure out_port3(valor: byte);
begin
  // 7-------  read strobe
  // -6------  write strobe
  // --5-----  dma
  // ---4----  int1 ack
  // ----3---  int1
  // -----2--  int0
  // ------1-  int0 flip-flop preset
  // -------0  int0 ack
  // mcs51_0.change_irq0(CLEAR_LINE);
  if (((mcu_p3 and 1) <> 0) and ((valor and 1) = 0)) then
  begin
    mcs51_0.change_irq0(CLEAR_LINE);
    m68000_0.change_halt(CLEAR_LINE);
  end;
  if (((mcu_p3 and $10) <> 0) and ((valor and $10) = 0)) then
    mcs51_0.change_irq1(CLEAR_LINE);
  if (((mcu_p3 and $40) <> 0) and ((valor and $40) = 0)) then
    mcu_to_audiocpu := mcu_p1;
  mcu_p3 := valor;
end;

function mcu_ext_ram_read(direccion: word): byte;
var
  address: dword;
  res: byte;
begin
  res := $FF;
  if ((mcu_p3 and $20) = 0) then
  begin
    direccion := direccion and $7FF;
    // various address bits are pulled high because the mcu doesn't drive them
    // the 3 upper address bits (p2.0, p2.1, p2.2) are connected to a14 to a16
    address := $E3E01 or ((direccion and $700) shl 6) or ((direccion and $FF) shl 1);
    res := bionicc_getword(address);
  end;
  mcu_ext_ram_read := res;
end;

procedure mcu_ext_ram_write(direccion: word; valor: byte);
var
  address: dword;
begin
  if ((mcu_p3 and $20) = 0) then
  begin
    direccion := direccion and $7FF;
    address := $E3E01 or ((direccion and $700) shl 6) or ((direccion and $FF) shl 1);
    bionicc_putword(address, valor);
  end;
end;

// Main
procedure reset_bionicc;
begin
  m68000_0.reset;
  z80_0.reset;
  ym2151_0.reset;
  mcs51_0.reset;
  reset_audio;
  marcade.in0 := $FFFF;
  scroll_fg_x := 0;
  scroll_fg_y := 0;
  scroll_bg_x := 0;
  scroll_bg_y := 0;
  audiocpu_to_mcu := 0;
  mcu_to_audiocpu := 0;
  mcu_p1 := 0;
  mcu_p3 := 0;
end;

function start_bioniccommando: boolean;
var
  memory_temp: array [0 .. $3FFFF] of byte;
const
  pf_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8, 9, 10, 11, (8 * 4 * 8) + 0, (8 * 4 * 8) + 1,
    (8 * 4 * 8) + 2, (8 * 4 * 8) + 3, (8 * 4 * 8) + 8, (8 * 4 * 8) + 9, (8 * 4 * 8) + 10, (8 * 4 * 8) + 11);
  pf_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16,
    9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, (16 * 8) + 0, (16 * 8) + 1, (16 * 8) + 2,
    (16 * 8) + 3, (16 * 8) + 4, (16 * 8) + 5, (16 * 8) + 6, (16 * 8) + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8,
    10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  start_bioniccommando := false;
  machine_calls.general_loop := bionicc_loop;
  machine_calls.reset := reset_bionicc;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256, true);
  screen_init(2, 256 + 8, 256 + 8, true);
  screen_mod_scroll(2, 264, 256, 255, 264, 256, 255);
  screen_init(3, 256 + 16, 256 + 16, true);
  screen_mod_scroll(3, 272, 256, 255, 272, 256, 255);
  screen_init(4, 256 + 16, 256 + 16, true);
  screen_mod_scroll(4, 272, 256, 255, 272, 256, 255);
  screen_init(5, 512, 512, false, true);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(12000000, 256);
  m68000_0.change_ram16_calls(bionicc_getword, bionicc_putword);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(bionicc_snd_getbyte, bionicc_snd_putbyte);
  z80_0.init_sound(bionicc_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  // cargar roms
  if not(roms_load16w(@rom, bionicc_rom)) then
    exit;
  // cargar sonido
  if not(roms_load(@mem_snd, bionicc_sound)) then
    exit;
  // MCU
  mcs51_0 := cpu_mcs51.create(I8X51, 6000000, 256);
  mcs51_0.change_io_calls(nil, in_port1, nil, nil, nil, out_port1, nil, out_port3);
  mcs51_0.change_ram_calls(mcu_ext_ram_read, mcu_ext_ram_write);
  if not(roms_load(mcs51_0.get_rom_addr, bionicc_mcu)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, bionicc_char)) then
    exit;
  init_gfx(0, 8, 8, 1024);
  gfx[0].trans[3] := true;
  gfx_set_desc_data(2, 0, 128, 4, 0);
  convert_gfx(0, 0, @memory_temp, @pf_x, @pf_y, false, false);
  // convertir bg
  if not(roms_load(@memory_temp, bionicc_bg)) then
    exit;
  init_gfx(1, 8, 8, 2048);
  gfx[1].trans[15] := true;
  gfx_set_desc_data(4, 0, 128, ($8000 * 8) + 4, $8000 * 8, 4, 0);
  convert_gfx(1, 0, @memory_temp, @pf_x, @pf_y, false, false);
  // convertir fg
  if not(roms_load(@memory_temp, bionicc_fg)) then
    exit;
  init_gfx(2, 16, 16, 2048);
  gfx[2].trans_alt[0, 15] := true;
  gfx[2].trans_alt[1, 1] := true;
  gfx[2].trans_alt[1, 2] := true;
  gfx[2].trans_alt[1, 3] := true;
  gfx[2].trans_alt[1, 4] := true;
  gfx[2].trans_alt[1, 5] := true;
  gfx[2].trans_alt[1, 15] := true;
  gfx_set_desc_data(4, 0, 512, ($20000 * 8) + 4, $20000 * 8, 4, 0);
  convert_gfx(2, 0, @memory_temp, @pf_x, @pf_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, bionicc_sprites)) then
    exit;
  init_gfx(3, 16, 16, 2048);
  gfx[3].trans[15] := true;
  gfx_set_desc_data(4, 0, 256, $30000 * 8, $20000 * 8, $10000 * 8, 0);
  convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := $DFFF;
  marcade.dswa_val := @bionicc_dip;
  // final
  reset_bionicc;
  start_bioniccommando := true;
end;

end.
