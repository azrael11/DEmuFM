unit m72_hw;

interface

uses
  WinApi.Windows,
  nz80,
  nec_v20_v30,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine,
  timer_engine,
  dac;

function start_m72: boolean;

implementation

const
  // Rtype
  rtype_rom: array [0 .. 3] of tipo_roms = ((n: 'rt_r-h0-b.1b'; l: $10000; p: 1; crc: $591C7754), (n: 'rt_r-l0-b.3b'; l: $10000; p: $0; crc: $A1928DF0), (n: 'rt_r-h1-b.1c'; l: $10000; p: $20001;
    crc: $A9D71ECA), (n: 'rt_r-l1-b.3c'; l: $10000; p: $20000; crc: $0DF3573D));
  rtype_char: array [0 .. 3] of tipo_roms = ((n: 'rt_b-a0.3c'; l: $8000; p: 0; crc: $4E212FB0), (n: 'rt_b-a1.3d'; l: $8000; p: $8000; crc: $8A65BDFF), (n: 'rt_b-a2.3a'; l: $8000; p: $10000;
    crc: $5A4AE5B9), (n: 'rt_b-a3.3e'; l: $8000; p: $18000; crc: $73327606));
  rtype_char2: array [0 .. 3] of tipo_roms = ((n: 'rt_b-b0.3j'; l: $8000; p: 0; crc: $A7B17491), (n: 'rt_b-b1.3k'; l: $8000; p: $8000; crc: $B9709686), (n: 'rt_b-b2.3h'; l: $8000; p: $10000;
    crc: $433B229A), (n: 'rt_b-b3.3f'; l: $8000; p: $18000; crc: $AD89B072));
  irem_m72_sprites: array [0 .. 11] of tipo_roms = ((n: 'rt_r-00.1h'; l: $10000; p: 0; crc: $DAD53BC0), (n: 'rt_r-01.1j'; l: $8000; p: $10000; crc: $5E441E7F), (n: 'rt_r-01.1j'; l: $8000; p: $18000;
    crc: $5E441E7F), (n: 'rt_r-10.1k'; l: $10000; p: $20000; crc: $D6A66298), (n: 'rt_r-11.1l'; l: $8000; p: $30000; crc: $791DF4F8), (n: 'rt_r-11.1l'; l: $8000; p: $38000; crc: $791DF4F8),
    (n: 'rt_r-20.3h'; l: $10000; p: $40000; crc: $FC247C8A), (n: 'rt_r-21.3j'; l: $8000; p: $50000; crc: $ED793841), (n: 'rt_r-21.3j'; l: $8000; p: $58000; crc: $ED793841), (n: 'rt_r-30.3k';
    l: $10000; p: $60000; crc: $EB02A1CB), (n: 'rt_r-31.3l'; l: $8000; p: $70000; crc: $8558355D), (n: 'rt_r-31.3l'; l: $8000; p: $78000; crc: $8558355D));
  // Hammering Harry
  hharry_rom: array [0 .. 3] of tipo_roms = ((n: 'a-h0-v.rom'; l: $20000; p: 1; crc: $C52802A5), (n: 'a-l0-v.rom'; l: $20000; p: $0; crc: $F463074C), (n: 'a-h1-0.rom'; l: $10000; p: $60001;
    crc: $3AE21335), (n: 'a-l1-0.rom'; l: $10000; p: $60000; crc: $BC6AC5F9));
  hharry_char: array [0 .. 3] of tipo_roms = ((n: 'hh_a0.rom'; l: $20000; p: 0; crc: $C577BA5F), (n: 'hh_a1.rom'; l: $20000; p: $20000; crc: $429D12AB), (n: 'hh_a2.rom'; l: $20000; p: $40000;
    crc: $B5B163B0), (n: 'hh_a3.rom'; l: $20000; p: $60000; crc: $8EF566A1));
  hharry_sprites: array [0 .. 3] of tipo_roms = ((n: 'hh_00.rom'; l: $20000; p: 0; crc: $EC5127EF), (n: 'hh_10.rom'; l: $20000; p: $20000; crc: $DEF65294), (n: 'hh_20.rom'; l: $20000; p: $40000;
    crc: $BB0D6AD4), (n: 'hh_30.rom'; l: $20000; p: $60000; crc: $4351044E));
  hharry_snd: tipo_roms = (n: 'a-sp-0.rom'; l: $10000; p: 0; crc: $80E210E7);
  hharry_dac: tipo_roms = (n: 'a-v0-0.rom'; l: $20000; p: 0; crc: $FAAACAFF);
  // R-Type 2
  rtype2_rom: array [0 .. 3] of tipo_roms = ((n: 'rt2-a-h0-d.54'; l: $20000; p: 1; crc: $D8ECE6F4), (n: 'rt2-a-l0-d.60'; l: $20000; p: $0; crc: $32CFB2E4), (n: 'rt2-a-h1-d.53'; l: $20000; p: $40001;
    crc: $4F6E9B15), (n: 'rt2-a-l1-d.59'; l: $20000; p: $40000; crc: $0FD123BF));
  rtype2_char: array [0 .. 7] of tipo_roms = ((n: 'ic50.7s'; l: $20000; p: 0; crc: $F3F8736E), (n: 'ic51.7u'; l: $20000; p: $20000; crc: $B4C543AF), (n: 'ic56.8s'; l: $20000; p: $40000;
    crc: $4CB80D66), (n: 'ic57.8u'; l: $20000; p: $60000; crc: $BEE128E0), (n: 'ic65.9r'; l: $20000; p: $80000; crc: $2DC9C71A), (n: 'ic66.9u'; l: $20000; p: $A0000; crc: $7533C428), (n: 'ic63.9m';
    l: $20000; p: $C0000; crc: $A6AD67F2), (n: 'ic64.9p'; l: $20000; p: $E0000; crc: $3686D555));
  rtype2_sprites: array [0 .. 3] of tipo_roms = ((n: 'ic31.6l'; l: $20000; p: 0; crc: $2CD8F913), (n: 'ic21.4l'; l: $20000; p: $20000; crc: $5033066D), (n: 'ic32.6m'; l: $20000; p: $40000;
    crc: $EC3A0450), (n: 'ic22.4m'; l: $20000; p: $60000; crc: $DB6176FC));
  rtype2_snd: tipo_roms = (n: 'ic17.4f'; l: $10000; p: 0; crc: $73FFECB4);
  rtype2_dac: tipo_roms = (n: 'ic14.4c'; l: $20000; p: 0; crc: $637172D5);

type
  tipo_update_video_m72 = procedure;
  tipo_paint_video_irem_m72 = procedure(princ, ultimo: word);

var
  rom: array [0 .. $7FFFF] of byte;
  spriteram: array [0 .. $3FF] of byte;
  palette1, palette2: array [0 .. $BFF] of byte;
  ram, videoram1, videoram2: array [0 .. $3FFF] of byte;
  sound_latch, snd_irq_vector, timer_sound, irq_pos: byte;
  m72_raster_irq_position, scroll_x1, scroll_y1, scroll_x2, scroll_y2: word;
  video_off: boolean;
  sample_addr: dword;
  mem_dac: array [0 .. $1FFFF] of byte;
  irq_base: array [0 .. 5] of byte;
  // video
  update_video_irem_m72: tipo_update_video_m72;
  paint_video_irem_m72: tipo_paint_video_irem_m72;

procedure draw_sprites;
var
  f, nchar, atrib, color, c: word;
  flipx, flipy: boolean;
  w, h, wx, wy: byte;
  x, y: integer;
begin
  for f := $0 to $7F do
  begin
    nchar := (buffer_sprites[(f * 8) + 2] + (buffer_sprites[(f * 8) + 3] shl 8)) and $FFF;
    atrib := buffer_sprites[(f * 8) + 4] + (buffer_sprites[(f * 8) + 5] shl 8);
    color := (atrib and $F) shl 4;
    x := (buffer_sprites[(f * 8) + 6] + (buffer_sprites[(f * 8) + 7] shl 8)) - 256;
    y := 384 - (buffer_sprites[(f * 8) + 0] + (buffer_sprites[(f * 8) + 1] shl 8));
    flipx := (atrib and $800) <> 0;
    flipy := (atrib and $400) <> 0;
    w := 1 shl ((atrib and $C000) shr 14);
    h := 1 shl ((atrib and $3000) shr 12);
    y := y - (16 * h);
    for wx := 0 to (w - 1) do
    begin
      for wy := 0 to (h - 1) do
      begin
        c := nchar;
        if flipx then
          c := c + 8 * (w - 1 - wx)
        else
          c := c + 8 * wx;
        if flipy then
          c := c + h - 1 - wy
        else
          c := c + wy;
        put_gfx_sprite(c and $FFF, color, flipx, flipy, 2);
        update_gfx_sprite((x + 16 * wx) and $3FF, (y + 16 * wy) and $1FF, 5, 2);
      end;
    end;
  end;
end;

procedure update_video_rtype;
var
  f, x, y, nchar, atrib, atrib2, color: word;
begin
  for f := 0 to $FFF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib2 := videoram2[(f * 4) + 2];
    color := atrib2 and $F;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      // Background
      atrib := videoram2[(f * 4) + 1];
      nchar := (videoram2[(f * 4) + 0] + ((atrib and $3F) shl 8)) and $FFF;
      if (atrib2 and $80) = 0 then
      begin
        put_gfx_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 1, 1, (atrib and $40) <> 0, (atrib and $80) <> 0);
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      end
      else
      begin
        put_gfx_block(x * 8, y * 8, 1, 8, 8, 0);
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 2, 1, (atrib and $40) <> 0, (atrib and $80) <> 0);
      end;
      gfx[1].buffer[f] := false;
    end;
    // Foreground
    atrib2 := videoram1[(f * 4) + 2];
    color := atrib2 and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      atrib := videoram1[(f * 4) + 1];
      nchar := (videoram1[(f * 4) + 0] + ((atrib and $3F) shl 8)) and $FFF;
      if (atrib2 and $80) = 0 then
      begin
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 3, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
        put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
      end
      else
      begin
        put_gfx_block_trans(x * 8, y * 8, 3, 8, 8);
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 4, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure paint_video_rtype(linea_o, linea_d: word);
begin
  scroll_x_y(1, 5, scroll_x2, scroll_y2 + 128);
  scroll_x_y(3, 5, scroll_x1, scroll_y1 + 129);
  draw_sprites;
  scroll_x_y(2, 5, scroll_x2, scroll_y2 + 128);
  scroll_x_y(4, 5, scroll_x1, scroll_y1 + 129);
  // Actualizar el video desde la linea actual a la ultima pintada
  update_region(64 + ADD_SPRITE, linea_o + ADD_SPRITE, 384 + ADD_SPRITE, (linea_d - linea_o) + ADD_SPRITE, 5, 0, linea_o, 384, linea_d - linea_o, 6);
end;

procedure change_color1(num: word);
var
  color: tcolor;
begin
  color.r := pal5bit(palette1[num]);
  color.g := pal5bit(palette1[num + $400]);
  color.b := pal5bit(palette1[num + $800]);
  set_pal_color(color, num shr 1);
end;

procedure change_color2(num: word);
var
  color: tcolor;
begin
  color.r := pal5bit(palette2[num]);
  color.g := pal5bit(palette2[num + $400]);
  color.b := pal5bit(palette2[num + $800]);
  num := num shr 1;
  set_pal_color(color, num + $100);
  buffer_color[(num shr 4) and $F] := true;
end;

procedure events_irem_m72;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // marcade.in1
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or $8);
  end;
end;

procedure irem_m72_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := nec_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 283 do
      begin
        // Main CPU
        nec_0.run(frame_m);
        frame_m := frame_m + nec_0.tframes - nec_0.contador;
        // Sound CPU
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if ((f < 255) and (f = (m72_raster_irq_position - 1))) then
        begin
          nec_0.vect_req := irq_base[1] + 2;
          nec_0.change_irq(HOLD_LINE);
          if not(video_off) then
            paint_video_irem_m72(0, f);
        end;
        if f = 255 then
        begin
          nec_0.vect_req := irq_base[1];
          nec_0.change_irq(HOLD_LINE);
          if not(video_off) then
          begin
            paint_video_irem_m72(m72_raster_irq_position and $FF, f);
            update_video_irem_m72;
          end
          else
            fill_full_screen(0, 0);
        end;
      end;
      update_region(0, 0, 384, 256, 6, 0, 0, 384, 256, PANT_TEMP);
      events_irem_m72;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// R-Type
function irem_m72_getbyte(direccion: dword): byte;
begin
  case direccion of
    0 .. $3FFFF:
      irem_m72_getbyte := rom[direccion];
    $40000 .. $43FFF:
      irem_m72_getbyte := ram[direccion and $3FFF];
    $C0000 .. $C03FF:
      irem_m72_getbyte := spriteram[direccion and $3FF];
    $C8000 .. $C8BFF:
      if (direccion and 1) <> 0 then
        irem_m72_getbyte := $FF
      else
        irem_m72_getbyte := palette1[(direccion and $1FF) + (direccion and $C00)] + $E0;
    $CC000 .. $CCBFF:
      if (direccion and 1) <> 0 then
        irem_m72_getbyte := $FF
      else
        irem_m72_getbyte := palette2[(direccion and $1FF) + (direccion and $C00)] + $E0;
    $D0000 .. $D3FFF:
      irem_m72_getbyte := videoram1[direccion and $3FFF];
    $D8000 .. $DBFFF:
      irem_m72_getbyte := videoram2[direccion and $3FFF];
    $E0000 .. $EFFFF:
      irem_m72_getbyte := mem_snd[direccion and $FFFF];
    $FFFF0 .. $FFFFF:
      irem_m72_getbyte := rom[direccion and $3FFFF];
  end;
end;

procedure irem_m72_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    0 .. $3FFFF, $FFFF0 .. $FFFFF:
      ;
    $40000 .. $43FFF:
      ram[direccion and $3FFF] := valor; // ram 1
    $C0000 .. $C03FF:
      spriteram[direccion and $3FF] := valor; // ram 7
    $C8000 .. $C8BFF:
      begin // ram 0
        palette1[(direccion and $1FF) + (direccion and $C00)] := valor;
        change_color1(direccion and $1FE);
      end;
    $CC000 .. $CCBFF:
      begin
        palette2[(direccion and $1FF) + (direccion and $C00)] := valor; // ram 9
        change_color2(direccion and $1FE);
      end;
    $D0000 .. $D3FFF:
      begin // ram 3
        videoram1[direccion and $3FFF] := valor;
        gfx[0].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $D8000 .. $DBFFF:
      begin
        videoram2[direccion and $3FFF] := valor; // ram 4
        gfx[1].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $E0000 .. $EFFFF:
      mem_snd[direccion and $FFFF] := valor;
  end;
end;

procedure irem_m72_outword(puerto: dword; valor: word);
begin
  case puerto of
    0:
      begin
        sound_latch := valor and $FF;
        snd_irq_vector := snd_irq_vector and $DF;
        timers.enabled(timer_sound, true);
      end;
    2:
      begin
        if (valor and $10) = 0 then
          z80_0.change_reset(ASSERT_LINE)
        else
          z80_0.change_reset(CLEAR_LINE);
        video_off := (valor and $08) <> 0;
      end;
    4:
      begin // DMA
        copymemory(@buffer_sprites[0], @spriteram[0], $400);
        fillchar(spriteram[0], $400, 0);
      end;
    6:
      m72_raster_irq_position := valor - 128;
    $40:
      begin
        irq_base[0] := valor;
        irq_pos := 1;
      end;
    $42:
      begin
        irq_base[irq_pos] := valor;
        irq_pos := irq_pos + 1;
      end;
    $80:
      if scroll_y1 <> valor then
        scroll_y1 := valor; // FG
    $82:
      if scroll_x1 <> valor then
        scroll_x1 := valor; // FG
    $84:
      if scroll_y2 <> valor then
        scroll_y2 := valor; // BG
    $86:
      if scroll_x2 <> valor then
        scroll_x2 := valor; // FG
  end;
end;

function irem_m72_inword(puerto: dword): word;
begin
  case puerto of
    0:
      irem_m72_inword := $FF00 or marcade.in0;
    2:
      irem_m72_inword := $FF00 or marcade.in1;
    4:
      irem_m72_inword := $FDFB;
  end;
end;

// Hammerin' Harry
procedure update_video_hharry;
var
  f, x, y, nchar, atrib, atrib2, color: word;
begin
  for f := 0 to $FFF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib2 := videoram2[(f * 4) + 2];
    color := atrib2 and $F;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      // Background
      atrib := videoram2[(f * 4) + 1];
      nchar := (videoram2[(f * 4) + 0] + ((atrib and $3F) shl 8)) and $3FFF;
      if (atrib2 and $80) = 0 then
      begin
        put_gfx_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 1, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      end
      else
      begin
        put_gfx_block(x * 8, y * 8, 1, 8, 8, 0);
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 2, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
      end;
      gfx[1].buffer[f] := false;
    end;
    // Foreground
    atrib2 := videoram1[(f * 4) + 2];
    color := atrib2 and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      atrib := videoram1[(f * 4) + 1];
      nchar := (videoram1[(f * 4) + 0] + ((atrib and $3F) shl 8)) and $3FFF;
      if (atrib2 and $80) = 0 then
      begin
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 3, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
        put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
      end
      else
      begin
        put_gfx_block_trans(x * 8, y * 8, 3, 8, 8);
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 4, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure paint_video_hharry(linea_o, linea_d: word);
begin
  scroll_x_y(1, 5, scroll_x2 - 6, scroll_y2 + 128);
  scroll_x_y(3, 5, scroll_x1 - 4, scroll_y1 + 128);
  draw_sprites;
  scroll_x_y(2, 5, scroll_x2 - 6, scroll_y2 + 128);
  scroll_x_y(4, 5, scroll_x1 - 4, scroll_y1 + 128);
  // Actualizar el video desde la linea actual a la ultima pintada
  update_region(64 + ADD_SPRITE, linea_o + ADD_SPRITE, 384 + ADD_SPRITE, (linea_d - linea_o) + ADD_SPRITE, 5, 0, linea_o, 384, linea_d - linea_o, 6);
end;

function hharry_getbyte(direccion: dword): byte;
begin
  case direccion of
    0 .. $7FFFF:
      hharry_getbyte := rom[direccion];
    $A0000 .. $A3FFF:
      hharry_getbyte := ram[direccion and $3FFF];
    $C0000 .. $C03FF:
      hharry_getbyte := spriteram[direccion and $3FF];
    $C8000 .. $C8BFF:
      if (direccion and 1) <> 0 then
        hharry_getbyte := $FF
      else
        hharry_getbyte := palette1[(direccion and $1FF) + (direccion and $C00)] + $E0;
    $CC000 .. $CCBFF:
      if (direccion and 1) <> 0 then
        hharry_getbyte := $FF
      else
        hharry_getbyte := palette2[(direccion and $1FF) + (direccion and $C00)] + $E0;
    $D0000 .. $D3FFF:
      hharry_getbyte := videoram1[direccion and $3FFF];
    $D8000 .. $DBFFF:
      hharry_getbyte := videoram2[direccion and $3FFF];
    $FFFF0 .. $FFFFF:
      hharry_getbyte := rom[direccion and $7FFFF];
  end;
end;

procedure hharry_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    0 .. $7FFFF, $FFFF0 .. $FFFFF:
      ;
    $A0000 .. $A3FFF:
      ram[direccion and $3FFF] := valor; // ram 1
    $C0000 .. $C03FF:
      spriteram[direccion and $3FF] := valor; // ram 7
    $C8000 .. $C8BFF:
      begin // ram 0
        palette1[(direccion and $1FF) + (direccion and $C00)] := valor;
        change_color1(direccion and $1FE);
      end;
    $CC000 .. $CCBFF:
      begin
        palette2[(direccion and $1FF) + (direccion and $C00)] := valor; // ram 9
        change_color2(direccion and $1FE);
      end;
    $D0000 .. $D3FFF:
      begin // ram 3
        videoram1[direccion and $3FFF] := valor;
        gfx[0].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $D8000 .. $DBFFF:
      begin
        videoram2[direccion and $3FFF] := valor; // ram 4
        gfx[1].buffer[(direccion and $3FFF) shr 2] := true;
      end;
  end;
end;

procedure out_io(puerto, valor: word);
begin
  case puerto of
    0:
      begin
        sound_latch := valor and $FF;
        snd_irq_vector := snd_irq_vector and $DF;
        timers.enabled(timer_sound, true);
      end;
    2:
      video_off := (valor and $08) <> 0;
    4:
      begin // DMA
        copymemory(@buffer_sprites[0], @spriteram[0], $400);
        fillchar(spriteram[0], $400, 0);
      end;
    6:
      m72_raster_irq_position := valor - 128;
    $40:
      begin
        irq_base[0] := valor;
        irq_pos := 1;
      end;
    $42:
      begin
        irq_base[irq_pos] := valor;
        irq_pos := irq_pos + 1;
      end;
    $80:
      if scroll_y1 <> valor then
        scroll_y1 := valor; // FG
    $82:
      if scroll_x1 <> valor then
        scroll_x1 := valor; // FG
    $84:
      if scroll_y2 <> valor then
        scroll_y2 := valor; // BG
    $86:
      if scroll_x2 <> valor then
        scroll_x2 := valor; // FG
  end;
end;

procedure hharry_outword(puerto: dword; valor: word);
begin
  out_io(puerto, valor);
end;

function in_io(puerto: word): word;
begin
  case puerto of
    0:
      in_io := $FF00 or marcade.in0;
    2:
      in_io := $FF00 or marcade.in1;
    4:
      in_io := $FDBF;
  end;
end;

function hharry_inword(puerto: dword): word;
begin
  hharry_inword := in_io(puerto);
end;

procedure hharry_outbyte(puerto: word; valor: byte);
begin
  out_io(puerto, valor);
end;

function hharry_inbyte(puerto: word): byte;
begin
  if (puerto and 1) <> 0 then
    hharry_inbyte := in_io(puerto) shr 8
  else
    hharry_inbyte := in_io(puerto) and $FF;
end;

// Rtype2
procedure update_video_rtype2;
var
  f, x, y, nchar, atrib, atrib2, color: word;
begin
  for f := 0 to $FFF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib2 := videoram2[(f * 4) + 2];
    color := atrib2 and $F;
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      // Background
      atrib := videoram2[(f * 4) + 3];
      nchar := (videoram2[(f * 4) + 0] + (videoram2[(f * 4) + 1] shl 8)) and $7FFF;
      if (atrib and $1) = 0 then
      begin
        put_gfx_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 1, 0, (atrib2 and $20) <> 0, (atrib2 and $40) <> 0);
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      end
      else
      begin
        put_gfx_block(x * 8, y * 8, 1, 8, 8, 0);
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 2, 0, (atrib2 and $20) <> 0, (atrib2 and $40) <> 0);
      end;
      gfx[1].buffer[f] := false;
    end;
    // Foreground
    atrib2 := videoram1[(f * 4) + 2];
    color := atrib2 and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      atrib := videoram1[(f * 4) + 3];
      nchar := (videoram1[(f * 4) + 0] + (videoram1[(f * 4) + 1] shl 8)) and $7FFF;
      if (atrib and $1) = 0 then
      begin
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 3, 0, (atrib2 and $20) <> 0, (atrib2 and $40) <> 0);
        put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
      end
      else
      begin
        put_gfx_block_trans(x * 8, y * 8, 3, 8, 8);
        put_gfx_trans_flip(x * 8, y * 8, nchar, (color shl 4) + 256, 4, 0, (atrib2 and $20) <> 0, (atrib2 and $40) <> 0);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure paint_video_rtype2(linea_o, linea_d: word);
begin
  scroll_x_y(1, 5, scroll_x2 - 6, scroll_y2 + 128);
  scroll_x_y(3, 5, scroll_x1 - 4, scroll_y1 + 128);
  draw_sprites;
  scroll_x_y(2, 5, scroll_x2 - 6, scroll_y2 + 128);
  scroll_x_y(4, 5, scroll_x1 - 4, scroll_y1 + 128);
  // Actualizar el video desde la linea actual a la ultima pintada
  update_region(64 + ADD_SPRITE, linea_o + ADD_SPRITE, 384 + ADD_SPRITE, (linea_d - linea_o) + ADD_SPRITE, 5, 0, linea_o, 384, linea_d - linea_o, 6);
end;

function rtype2_getbyte(direccion: dword): byte;
begin
  case direccion of
    0 .. $7FFFF:
      rtype2_getbyte := rom[direccion];
    $C0000 .. $C03FF:
      rtype2_getbyte := spriteram[direccion and $3FF];
    $C8000 .. $C8BFF:
      if (direccion and 1) <> 0 then
        rtype2_getbyte := $FF
      else
        rtype2_getbyte := palette1[(direccion and $1FF) + (direccion and $C00)] + $E0;
    $D0000 .. $D3FFF:
      rtype2_getbyte := videoram1[direccion and $3FFF];
    $D4000 .. $D7FFF:
      rtype2_getbyte := videoram2[direccion and $3FFF];
    $D8000 .. $D8BFF:
      if (direccion and 1) <> 0 then
        rtype2_getbyte := $FF
      else
        rtype2_getbyte := palette2[(direccion and $1FF) + (direccion and $C00)] + $E0;
    $E0000 .. $E3FFF:
      rtype2_getbyte := ram[direccion and $3FFF];
    $FFFF0 .. $FFFFF:
      rtype2_getbyte := rom[direccion and $7FFFF];
  end;
end;

procedure rtype2_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    0 .. $7FFFF, $FFFF0 .. $FFFFF:
      ;
    $B0000 .. $B0001:
      copymemory(@buffer_sprites[0], @spriteram[0], $400); // DMA
    $BC000 .. $BC001:
      m72_raster_irq_position := valor + 64;
    $C0000 .. $C03FF:
      spriteram[direccion and $3FF] := valor; // ram 7
    $C8000 .. $C8BFF:
      begin // ram 0
        palette1[(direccion and $1FF) + (direccion and $C00)] := valor;
        change_color1(direccion and $1FE);
      end;
    $D0000 .. $D3FFF:
      begin // ram 3
        videoram1[direccion and $3FFF] := valor;
        gfx[0].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $D4000 .. $D7FFF:
      begin
        videoram2[direccion and $3FFF] := valor; // ram 4
        gfx[1].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $D8000 .. $D8BFF:
      begin
        palette2[(direccion and $1FF) + (direccion and $C00)] := valor; // ram 9
        change_color2(direccion and $1FE);
      end;
    $E0000 .. $E3FFF:
      ram[direccion and $3FFF] := valor; // ram 1
  end;
end;

procedure rtype2_out_io(puerto, valor: word);
begin
  case puerto of
    0:
      begin
        sound_latch := valor and $FF;
        snd_irq_vector := snd_irq_vector and $DF;
        timers.enabled(timer_sound, true);
      end;
    2:
      video_off := (valor and $08) <> 0;
    $40:
      begin
        irq_base[0] := valor;
        irq_pos := 1;
      end;
    $42:
      begin
        irq_base[irq_pos] := valor;
        irq_pos := irq_pos + 1;
      end;
    $80:
      if scroll_y1 <> valor then
        scroll_y1 := valor; // FG
    $82:
      if scroll_x1 <> valor then
        scroll_x1 := valor; // FG
    $84:
      if scroll_y2 <> valor then
        scroll_y2 := valor; // BG
    $86:
      if scroll_x2 <> valor then
        scroll_x2 := valor; // FG
    $8C:
      ;
  end;
end;

procedure rtype2_outword(puerto: dword; valor: word);
begin
  rtype2_out_io(puerto, valor);
end;

function rtype2_in_io(puerto: word): word;
begin
  case puerto of
    0:
      rtype2_in_io := marcade.in0;
    2:
      rtype2_in_io := marcade.in1;
    4:
      rtype2_in_io := $F7FF;
  end;
end;

function rtype2_inword(puerto: dword): word;
begin
  rtype2_inword := rtype2_in_io(puerto);
end;

procedure rtype2_outbyte(puerto: word; valor: byte);
begin
  rtype2_out_io(puerto, valor);
end;

function rtype2_inbyte(puerto: word): byte;
begin
  if (puerto and 1) <> 0 then
    rtype2_inbyte := rtype2_in_io(puerto) shr 8
  else
    rtype2_inbyte := rtype2_in_io(puerto) and $FF;
end;

// Sound
procedure sound_irq_ack;
begin
  z80_0.im0 := snd_irq_vector;
  if snd_irq_vector = $FF then
    z80_0.change_irq(CLEAR_LINE)
  else
    z80_0.change_irq(ASSERT_LINE);
  timers.enabled(timer_sound, false);
end;

function irem_m72_snd_getbyte(direccion: word): byte;
begin
  irem_m72_snd_getbyte := mem_snd[direccion];
end;

procedure irem_m72_snd_putbyte(direccion: word; valor: byte);
begin
  mem_snd[direccion] := valor;
end;

function irem_m72_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $1:
      irem_m72_snd_inbyte := ym2151_0.status;
    $2:
      irem_m72_snd_inbyte := sound_latch;
  end;
end;

procedure irem_m72_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      ym2151_0.reg(valor);
    $1:
      ym2151_0.write(valor);
    $6:
      begin
        snd_irq_vector := snd_irq_vector or $20;
        timers.enabled(timer_sound, true);
      end;
  end;
end;

function rtype2_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $1:
      rtype2_snd_inbyte := ym2151_0.status;
    $80:
      rtype2_snd_inbyte := sound_latch;
    $84:
      rtype2_snd_inbyte := mem_dac[sample_addr];
  end;
end;

procedure rtype2_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      ym2151_0.reg(valor);
    $1:
      ym2151_0.write(valor);
    $80:
      begin
        sample_addr := sample_addr shr 5;
        sample_addr := (sample_addr and $FF00) or valor;
        sample_addr := sample_addr shl 5;
      end;
    $81:
      begin
        sample_addr := sample_addr shr 5;
        sample_addr := (sample_addr and $FF) or (valor shl 8);
        sample_addr := sample_addr shl 5;
      end;
    $82:
      begin
        dac_0.signed_data8_w(valor);
        sample_addr := (sample_addr + 1) and $1FFFF;
      end;
    $83:
      begin
        snd_irq_vector := snd_irq_vector or $20;
        timers.enabled(timer_sound, true);
      end;
  end;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  if irqstate = 1 then
    snd_irq_vector := snd_irq_vector and $EF
  else
    snd_irq_vector := snd_irq_vector or $10;
  timers.enabled(timer_sound, true);
end;

procedure rtype2_perodic_int;
begin
  z80_0.change_nmi(PULSE_LINE);
end;

procedure irem_m72_sound_update;
begin
  ym2151_0.update;
end;

procedure rtype2_sound_update;
begin
  ym2151_0.update;
  dac_0.update;
end;

// Main
procedure reset_irem_m72;
begin
  nec_0.reset;
  z80_0.reset;
  ym2151_0.reset;
  case main_vars.machine_type of
    190, 191:
      dac_0.reset;
  end;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  scroll_x1 := 0;
  scroll_x2 := 0;
  scroll_y1 := 0;
  scroll_y2 := 0;
  snd_irq_vector := $FF;
  sound_latch := 0;
  m72_raster_irq_position := 0;
  video_off := true;
  sample_addr := 0;
  fillchar(irq_base[0], 5, 0);
  irq_pos := 0;
  timers.enabled(timer_sound, false);
end;

function start_m72: boolean;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  memory_temp: pbyte;
begin
  start_m72 := false;
  machine_calls.reset := reset_irem_m72;
  machine_calls.fps_max := 55.017606;
  machine_calls.general_loop := irem_m72_loop;
  start_audio(false);
  screen_init(1, 512, 512);
  screen_mod_scroll(1, 512, 512, 511, 512, 256, 511);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 256, 511);
  screen_init(3, 512, 512, true);
  screen_mod_scroll(3, 512, 512, 511, 512, 256, 511);
  screen_init(4, 512, 512, true);
  screen_mod_scroll(4, 512, 512, 511, 512, 256, 511);
  screen_init(5, 1024, 512, false, true);
  screen_init(6, 384, 256);
  start_video(384, 256);
  // iniciar_video(1024,512);
  // Main CPU
  nec_0 := cpu_nec.create(8000000, 284, NEC_V30);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 284);
  z80_0.change_ram_calls(irem_m72_snd_getbyte, irem_m72_snd_putbyte);
  timer_sound := timers.init(z80_0.numero_cpu, 1, sound_irq_ack, nil, true);
  getmem(memory_temp, $100000);
  case main_vars.machine_type of
    87:
      begin // R-Type
        // Main CPU
        nec_0.change_ram_calls(irem_m72_getbyte, irem_m72_putbyte);
        nec_0.change_io_calls16(irem_m72_inword, irem_m72_outword);
        if not(roms_load16b(@rom, rtype_rom)) then
          exit;
        // Sound
        z80_0.change_io_calls(irem_m72_snd_inbyte, irem_m72_snd_outbyte);
        z80_0.init_sound(irem_m72_sound_update);
        // video
        update_video_irem_m72 := update_video_rtype;
        paint_video_irem_m72 := paint_video_rtype;
        // convertir chars
        if not(roms_load(memory_temp, rtype_char)) then
          exit;
        init_gfx(0, 8, 8, $1000);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(4, 0, 8 * 8, $18000 * 8, $10000 * 8, $8000 * 8, 0 * 8);
        convert_gfx(0, 0, memory_temp, @ps_x, @ps_y, false, false);
        // chars 2
        if not(roms_load(memory_temp, rtype_char2)) then
          exit;
        init_gfx(1, 8, 8, $1000);
        gfx[1].trans[0] := true;
        convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
        // convertir sprites
        if not(roms_load(memory_temp, irem_m72_sprites)) then
          exit;
        init_gfx(2, 16, 16, $1000);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, $60000 * 8, $40000 * 8, $20000 * 8, 0);
        convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
      end;
    190:
      begin // Hammerin' Harry
        // Main CPU
        nec_0.change_ram_calls(hharry_getbyte, hharry_putbyte);
        nec_0.change_io_calls16(hharry_inword, hharry_outword);
        nec_0.change_io_calls(hharry_inbyte, hharry_outbyte);
        if not(roms_load16b(@rom, hharry_rom)) then
          exit;
        // Sound
        if not(roms_load(@mem_snd, hharry_snd)) then
          exit;
        z80_0.change_io_calls(rtype2_snd_inbyte, rtype2_snd_outbyte);
        timers.init(z80_0.numero_cpu, 3579645 / (128 * 55), rtype2_perodic_int, nil, true);
        z80_0.init_sound(rtype2_sound_update);
        dac_0 := dac_chip.create;
        if not(roms_load(@mem_dac, hharry_dac)) then
          exit;
        // video
        update_video_irem_m72 := update_video_hharry;
        paint_video_irem_m72 := paint_video_hharry;
        // convertir chars
        if not(roms_load(memory_temp, hharry_char)) then
          exit;
        init_gfx(0, 8, 8, $4000);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(4, 0, 8 * 8, $18000 * 8 * 4, $10000 * 8 * 4, $8000 * 8 * 4, 0 * 8);
        convert_gfx(0, 0, memory_temp, @ps_x, @ps_y, false, false);
        // convertir sprites
        if not(roms_load(memory_temp, hharry_sprites)) then
          exit;
        init_gfx(2, 16, 16, $1000);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, $60000 * 8, $40000 * 8, $20000 * 8, 0);
        convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
      end;
    191:
      begin // R-Type 2
        // Main CPU
        nec_0.change_ram_calls(rtype2_getbyte, rtype2_putbyte);
        nec_0.change_io_calls16(rtype2_inword, rtype2_outword);
        nec_0.change_io_calls(rtype2_inbyte, rtype2_outbyte);
        if not(roms_load16b(@rom, rtype2_rom)) then
          exit;
        // Sound
        if not(roms_load(@mem_snd, rtype2_snd)) then
          exit;
        z80_0.change_io_calls(rtype2_snd_inbyte, rtype2_snd_outbyte);
        timers.init(z80_0.numero_cpu, 3579645 / (128 * 55), rtype2_perodic_int, nil, true);
        z80_0.init_sound(rtype2_sound_update);
        dac_0 := dac_chip.create;
        if not(roms_load(@mem_dac, rtype2_dac)) then
          exit;
        // video
        update_video_irem_m72 := update_video_rtype2;
        paint_video_irem_m72 := paint_video_rtype2;
        // convertir chars
        if not(roms_load(memory_temp, rtype2_char)) then
          exit;
        init_gfx(0, 8, 8, $8000);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(4, 0, 8 * 8, $18000 * 8 * 4 * 2, $10000 * 8 * 4 * 2, $8000 * 8 * 4 * 2, 0 * 8);
        convert_gfx(0, 0, memory_temp, @ps_x, @ps_y, false, false);
        // convertir sprites
        if not(roms_load(memory_temp, rtype2_sprites)) then
          exit;
        init_gfx(2, 16, 16, $1000);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, $60000 * 8, $40000 * 8, $20000 * 8, 0);
        convert_gfx(2, 0, memory_temp, @ps_x, @ps_y, false, false);
      end;
  end;
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  freemem(memory_temp);
  reset_irem_m72;
  start_m72 := true;
end;

end.
