﻿unit gb;

interface

uses
  WinApi.Windows,
  file_engine,
  lr35902,
  main_engine,
  controls_engine,
  gfx_engine,
  timer_engine,
  FMX.Dialogs,
  System.SysUtils,
  gb_sound,
  rom_engine,
  misc_functions,
  pal_engine,
  gb_mappers,
  sound_engine,
  config_gb,
  FMX.Forms;

type
  tgameboy = record
    read_io: function(direccion: byte): byte;
    write_io: procedure(direccion: byte; valor: byte);
    video_render: procedure;
    is_gbc: boolean;
  end;

  tgb_head = packed record
    none1: array [0 .. $103] of byte;
    logo: array [0 .. $2F] of byte;
    title: array [0 .. 10] of ansichar;
    manu: array [0 .. 3] of ansichar;
    cgb_flag: byte;
    new_license: array [0 .. 1] of byte;
    sbg_flag: byte;
    cart_type: byte;
    rom_size: byte;
    ram_size: byte;
    region: byte;
    license: byte;
    rom_ver: byte;
    head_sum: byte;
    total_sum: word;
    cart_size: word;
  end;

function start_gb: boolean;

var
  ram_enable: boolean;
  gb_head: tgb_head;
  gb_palette: byte;

implementation

uses
  main,
  snapshot;

const
  color_pal: array [0 .. 1, 0 .. 3] of tcolor = (((r: $9B; g: $BC; b: $0F), (r: $8B; g: $AC; b: $0F), (r: $30; g: $62; b: $30), (r: $0F; g: $38; b: $0F)), ((r: $FF; g: $FF; b: $FF), (r: $AA; g: $AA;
    b: $AA), (r: $55; g: $55; b: $55), (r: 0; g: 0; b: 0)));
  gb_rom: tipo_roms = (n: 'dmg_boot.bin'; l: $100; p: 0; crc: $59C8598E);
  gbc_rom: array [0 .. 1] of tipo_roms = ((n: 'gbc_boot.1'; l: $100; p: 0; crc: $779EA374), (n: 'gbc_boot.2'; l: $700; p: $200; crc: $F741807D));
  GB_CLOCK = 4194304;

var
  scroll_x, stat, linea_actual, lcd_control, bg_pal, sprt0_pal, sprt1_pal: byte;
  tcontrol, tmodulo, mtimer, prog_timer, ly_compare, window_x, window_y, window_y_draw: byte;
  wram_bank: array [0 .. 7, 0 .. $FFF] of byte;
  vram_bank: array [0 .. 1, 0 .. $1FFF] of byte;
  io_ram, sprt_ram, bg_prio: array [0 .. $FF] of byte;
  bios_rom: array [0 .. $8FF] of byte;
  bgc_pal, spc_pal: array [0 .. $3F] of word;
  sprites_ord: array [0 .. 9] of byte;
  scroll_y: array [0 .. $FF] of byte;
  enable_bios, rom_exist, bgcolor_inc, spcolor_inc, lcd_ena, hdma_ena: boolean;
  scroll_y_last, irq_ena, joystick, vram_nbank, wram_nbank, bgcolor_index, spcolor_index: byte;
  scroll_y_pos, dma_src, dma_dst: word;
  nv_ram_name: string;
  oam_dma, haz_dma, hay_nvram: boolean;
  oam_dma_pos, hdma_size, gb_timer, sprites_time: byte;
  gameboy: tgameboy;

procedure get_active_sprites;
var
  f, h, g, pos_x, size: byte;
  sprites_x: array [0 .. 9] of byte;
  pos_y: integer;
  pos_linea: word;
begin
  fillchar(sprites_x[0], 10, $FF);
  fillchar(sprites_ord[0], 10, $FF);
  sprites_time := 0;
  for f := 0 to $27 do
  begin
    pos_y := sprt_ram[$00 + (f * 4)];
    // Los sprites con y=0 o mayor de 159 no cuentan
    if ((pos_y = 0) or (pos_y >= 160)) then
      continue;
    // El parentesis es importante!!
    pos_linea := linea_actual - (pos_y - 16);
    size := 8 shl ((lcd_control and 4) shr 2);
    // Si el sprite esta en la linea... Lo cuento
    if (pos_linea < size) then
    begin
      // Los de la X siempre cuentan! Da igual si se salen fuera de la pantalla
      pos_x := sprt_ram[$01 + (f * 4)];
      // Solo se puenden pintar 10
      for h := 0 to 9 do
      begin
        // Si la X del sprite es menor los cambio
        if sprites_x[h] > pos_x then
        begin
          for g := 8 downto h do
          begin
            sprites_x[g + 1] := sprites_x[g];
            sprites_ord[g + 1] := sprites_ord[g];
          end;
          sprites_x[h] := pos_x;
          sprites_ord[h] := f;
          sprites_time := sprites_time + 8;
          break;
        end;
      end;
    end;
  end;
end;

procedure draw_sprites(pri: byte);
var
  flipx, flipy: boolean;
  f, x, pal, atrib, pval, sprite_num: byte;
  num_char, def_y, tile_val1, tile_val2, long_x, main_x: byte;
  pos_linea: word;
  ptemp: pword;
  pos_y, pos_x: integer;
begin
  for f := 9 downto 0 do
  begin
    sprite_num := sprites_ord[f];
    if sprite_num = $FF then
      continue;
    atrib := sprt_ram[$03 + (sprite_num * 4)];
    pos_y := sprt_ram[$00 + (sprite_num * 4)];
    if (atrib and $80) <> pri then
      continue;
    pos_y := pos_y - 16;
    pos_linea := linea_actual - pos_y;
    pos_x := sprt_ram[$01 + (sprite_num * 4)];
    if ((pos_x = 0) or (pos_x >= 168)) then
      continue;
    pos_x := pos_x - 8;
    pal := ((atrib and $10) shr 2) + 4;
    num_char := sprt_ram[$02 + (sprite_num * 4)];
    flipx := (atrib and $20) <> 0;
    flipy := (atrib and $40) <> 0;
    if (lcd_control and 4) = 0 then
    begin // 8x8
      if flipy then
        def_y := 7 - (pos_linea and 7)
      else
        def_y := pos_linea and 7;
    end
    else
    begin // 8x16
      if flipy then
      begin
        def_y := 7 - (pos_linea and 7);
        num_char := (num_char and $FE) + (not(pos_linea shr 3) and 1);
      end
      else
      begin
        def_y := pos_linea and 7;
        num_char := (num_char and $FE) + (pos_linea shr 3);
      end;
    end;
    ptemp := punbuf;
    tile_val1 := vram_bank[0, num_char * 16 + (def_y * 2)];
    tile_val2 := vram_bank[0, num_char * 16 + 1 + (def_y * 2)];
    if flipx then
    begin
      for x := 0 to 7 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        if pval = 0 then
          ptemp^ := paleta[MAX_COLORS]
        else
        begin
          ptemp^ := paleta[pval + pal];
          bg_prio[(pos_x + x) and $FF] := bg_prio[(pos_x + x) and $FF] or 1;
        end;
        inc(ptemp);
      end;
      putpixel(0, 0, 8, punbuf, PANT_SPRITES);
    end
    else
    begin
      for x := 7 downto 0 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        if pval = 0 then
          ptemp^ := paleta[MAX_COLORS]
        else
        begin
          bg_prio[(pos_x + (7 - x)) and $FF] := bg_prio[(pos_x + (7 - x)) and $FF] or 1;
          ptemp^ := paleta[pval + pal];
        end;
        inc(ptemp);
      end;
      putpixel(0, 0, 8, punbuf, PANT_SPRITES);
    end;
    long_x := 8;
    main_x := 0;
    if pos_x < 0 then
    begin
      long_x := 8 + pos_x;
      main_x := abs(pos_x);
      pos_x := 0;
    end;
    if (pos_x + 8) > 160 then
      long_x := 160 - pos_x;
    update_region(main_x, 0, long_x, 1, PANT_SPRITES, pos_x + 7, pos_y + pos_linea, long_x, 1, 2);
  end;
end;

procedure update_bg(prio: byte);
var
  tile_addr, bg_addr: word;
  f, x, tile_val1, tile_val2, y, pval, linea_pant: byte;
  n2: integer;
  tile_mid: boolean;
  ptemp: pword;
begin
  bg_addr := $1800 + ((lcd_control and $8) shl 7);
  tile_mid := (lcd_control and $10) = 0;
  tile_addr := $1000 * byte(tile_mid); // Cuidado! Tiene signo despues
  for f := 0 to 31 do
  begin
    linea_pant := linea_actual + scroll_y[round(f * 3.5625)];
    y := (linea_pant and $7) * 2;
    if tile_mid then
      n2 := shortint(vram_bank[0, (bg_addr + f + ((linea_pant div 8) * 32)) and $1FFF])
    else
      n2 := byte(vram_bank[0, (bg_addr + f + ((linea_pant div 8) * 32)) and $1FFF]);
    tile_val1 := vram_bank[0, (n2 * 16 + tile_addr + y) and $1FFF];
    tile_val2 := vram_bank[0, (n2 * 16 + tile_addr + 1 + y) and $1FFF];
    ptemp := punbuf;
    for x := 7 downto 0 do
    begin
      pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
      // Prioridad con los sprites
      case prio of
        0:
          ptemp^ := paleta[pval];
        1:
          if (pval <> 0) then
          begin
            if ((bg_prio[f * 8 + (7 - x)] and 2) = 0) then
              ptemp^ := paleta[pval]
            else
              ptemp^ := paleta[MAX_COLORS];
          end
          else
            ptemp^ := paleta[MAX_COLORS];
      end;
      inc(ptemp);
    end; // del for x
    putpixel(f * 8, 0, 8, punbuf, 1);
  end; // del for f
  // Scroll X
  if scroll_x <> 0 then
  begin
    update_region(0, 0, scroll_x, 1, 1, (256 - scroll_x) + 7, linea_actual, scroll_x, 1, 2);
    update_region(scroll_x, 0, 256 - scroll_x, 1, 1, 7, linea_actual, 256 - scroll_x, 1, 2);
  end
  else
    update_region(0, 0, 256, 1, 1, 7, linea_actual, 256, 1, 2);
end;

procedure update_window(prio: byte);
var
  tile_addr, bg_addr: word;
  f, x, tile_val1, tile_val2, y, pval: byte;
  n2: integer;
  tile_mid: boolean;
  ptemp: pword;
begin
  if ((linea_actual < window_y) or (window_x > 166) or (window_x = 0)) then
    exit;
  bg_addr := $1800 + ((lcd_control and $40) shl 4);
  tile_mid := (lcd_control and $10) = 0;
  tile_addr := $1000 * byte(tile_mid); // Cuidado! Tiene signo despues
  y := (window_y_draw and $7) * 2;
  for f := 0 to 31 do
  begin
    if tile_mid then
      n2 := shortint(vram_bank[0, (bg_addr + f + ((window_y_draw div 8) * 32)) and $1FFF])
    else
      n2 := vram_bank[0, (bg_addr + f + ((window_y_draw div 8) * 32)) and $1FFF];
    tile_val1 := vram_bank[0, (n2 * 16 + tile_addr + y) and $1FFF];
    tile_val2 := vram_bank[0, (n2 * 16 + tile_addr + 1 + y) and $1FFF];
    ptemp := punbuf;
    for x := 7 downto 0 do
    begin
      pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
      case prio of
        0:
          begin
            ptemp^ := paleta[pval];
            bg_prio[(f * 8 + (7 - x) - (256 - scroll_x) + window_x - 7) and $FF] := bg_prio[(f * 8 + (7 - x) - (256 - scroll_x) + window_x - 7) and $FF] or 2;
          end;
        1:
          if (pval = 0) then
          begin
            if bg_prio[(f * 8 + (7 - x) - (256 - scroll_x) + window_x - 7) and $FF] = 0 then
              ptemp^ := paleta[0]
            else
              ptemp^ := paleta[MAX_COLORS];
          end
          else
            ptemp^ := paleta[pval];
      end;
      inc(ptemp);
    end; // del for x
    putpixel(f * 8, 0, 8, punbuf, 1);
  end; // del for f
  update_region(0, 0, 256, 1, 1, window_x, linea_actual, 256, 1, 2);
end;

// Por fin lo entiendo! Como renderiza la GB
// Primero el fondo (0 transparente en la segunda pasada), encima window (nunca transparente) y encima sprites (0 transparente)
// Ahora las prioridades --> Si hay window encima del fondo, el fondo pierde la prioridad!
// Por lo que si pinto un sprite encima de window, el sprite tiene prioridad sobre fondo SIEMPRE, da igual las prioridades de despues
// La segunda pasada, para prioridades del fondo y window. Si no hay window debajo del fondo en esta pasada, prioridades normales
// (el 0 es transparente) y machaca sprites. Si hay window no toca nada.
// Para window en la segunda pasada es distinto, no es transparente EXCEPTO si lo que hay debajo es un sprite! Si es un sprite
// lo que hay debajo, el 0 es transparente
// Para acabar, otra pasada de sprites con prioridad maxima, 0 transparente y el resto lo machaca
procedure update_video_gb;
var
  f: byte;
begin
  for f := scroll_y_pos to 113 do
    scroll_y[f] := scroll_y_last;
  scroll_y_pos := 0;
  single_line(7, linea_actual, paleta[0], 160, 2);
  if lcd_ena then
  begin
    fillchar(bg_prio[0], $100, 0);
    if (lcd_control and 1) <> 0 then
    begin
      update_bg(0);
      if (lcd_control and $20) <> 0 then
        update_window(0);
    end;
    if (((lcd_control and 2) <> 0) and not(oam_dma)) then
    begin
      get_active_sprites;
      draw_sprites($80);
    end;
    if (lcd_control and 1) <> 0 then
    begin
      update_bg(1);
      if (lcd_control and $20) <> 0 then
        update_window(1);
    end;
    if (((lcd_control and 2) <> 0) and not(oam_dma)) then
      draw_sprites(0);
    if (not((linea_actual < window_y) or (window_x > 166) or (window_x = 0)) and ((lcd_control and $20) <> 0)) then
      window_y_draw := window_y_draw + 1;
  end;
end;

// GBC
procedure get_active_sprites_gbc;
var
  f, size, num_sprites: byte;
  pos_y: integer;
  pos_linea: word;
begin
  fillchar(sprites_ord[0], 10, $FF);
  sprites_time := 0;
  num_sprites := 0;
  for f := 0 to $27 do
  begin
    pos_y := sprt_ram[$00 + (f * 4)];
    // Los sprites con y=0 o mayor de 159 no cuentan
    if ((pos_y = 0) or (pos_y >= 160)) then
      continue;
    // El parentesis es importante!!
    pos_linea := linea_actual - (pos_y - 16);
    size := 8 shl ((lcd_control and 4) shr 2);
    // Si el sprite esta en la linea... Lo cuento
    if (pos_linea < size) then
    begin
      // El orden es la prioridad en la posicion de la memoria
      sprites_ord[num_sprites] := f;
      sprites_time := sprites_time + (8 shr lr35902_0.speed);
      num_sprites := num_sprites + 1;
      if num_sprites = 10 then
        exit;
    end;
  end;
end;

procedure draw_sprites_gbc(pri: byte);
var
  flipx, flipy: boolean;
  sprite_num, f, x, pal, atrib, pval, spr_bank: byte;
  num_char, def_y, tile_val1, tile_val2, long_x, main_x: byte;
  pos_linea: word;
  ptemp: pword;
  pos_y, pos_x: integer;
begin
  for f := 9 downto 0 do
  begin
    sprite_num := sprites_ord[f];
    if sprite_num = $FF then
      continue;
    pos_y := sprt_ram[$00 + (sprite_num * 4)];
    atrib := sprt_ram[$03 + (sprite_num * 4)];
    if (atrib and $80) <> pri then
      continue;
    pos_y := pos_y - 16;
    pos_linea := linea_actual - pos_y;
    pos_x := sprt_ram[$01 + (sprite_num * 4)];
    if ((pos_x = 0) or (pos_x >= 168)) then
      continue;
    pos_x := pos_x - 8;
    pal := (atrib and $7) * 4;
    spr_bank := (atrib shr 3) and 1;
    num_char := sprt_ram[$02 + (sprite_num * 4)];
    flipx := (atrib and $20) <> 0;
    flipy := (atrib and $40) <> 0;
    if (lcd_control and 4) = 0 then
    begin // 8x8
      if flipy then
        def_y := 7 - (pos_linea and 7)
      else
        def_y := pos_linea and 7;
    end
    else
    begin // 8x16
      if flipy then
      begin
        def_y := 7 - (pos_linea and 7);
        num_char := (num_char and $FE) + (not(pos_linea shr 3) and 1);
      end
      else
      begin
        def_y := pos_linea and 7;
        num_char := (num_char and $FE) + (pos_linea shr 3);
      end;
    end;
    ptemp := punbuf;
    // Sprites 8x8 o 8x16
    tile_val1 := vram_bank[spr_bank, num_char * 16 + (def_y * 2)];
    tile_val2 := vram_bank[spr_bank, num_char * 16 + 1 + (def_y * 2)];
    if flipx then
    begin
      for x := 0 to 7 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        if pval = 0 then
          ptemp^ := paleta[MAX_COLORS]
        else
        begin
          if ((bg_prio[(pos_x + x) and $FF]) and 4) = 0 then
          begin
            ptemp^ := paleta[(spc_pal[pval + pal]) and $7FFF];
            bg_prio[(pos_x + x) and $FF] := bg_prio[(pos_x + x) and $FF] or 1;
          end
          else
            ptemp^ := paleta[MAX_COLORS];
        end;
        inc(ptemp);
      end;
      putpixel(0, 0, 8, punbuf, PANT_SPRITES);
    end
    else
    begin
      for x := 7 downto 0 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        if pval = 0 then
          ptemp^ := paleta[MAX_COLORS]
        else
        begin
          if ((bg_prio[(pos_x + (7 - x)) and $FF]) and 4) = 0 then
          begin
            ptemp^ := paleta[(spc_pal[pval + pal]) and $7FFF];
            bg_prio[(pos_x + (7 - x)) and $FF] := bg_prio[(pos_x + (7 - x)) and $FF] or 1;
          end
          else
            ptemp^ := paleta[MAX_COLORS];
        end;
        inc(ptemp);
      end;
      putpixel(0, 0, 8, punbuf, PANT_SPRITES);
    end;
    long_x := 8;
    main_x := 0;
    if pos_x < 0 then
    begin
      long_x := 8 + pos_x;
      main_x := abs(pos_x);
      pos_x := 0;
    end;
    if (pos_x + 8) > 160 then
      long_x := 160 - pos_x;
    update_region(main_x, 0, long_x, 1, PANT_SPRITES, pos_x + 7, pos_y + pos_linea, long_x, 1, 2);
  end;
end;

procedure update_bg_gbc(prio: byte);
var
  tile_addr, bg_addr: word;
  f, atrib, tile_bank, tile_pal: byte;
  x, tile_val1, tile_val2, y, pval, linea_pant: byte;
  n2: integer;
  tile_mid: boolean;
  ptemp: pword;
begin
  bg_addr := $1800 + ((lcd_control and $8) shl 7);
  tile_mid := (lcd_control and $10) = 0;
  tile_addr := $1000 * byte(tile_mid); // Cuidado! Tiene signo despues
  for f := 0 to 31 do
  begin
    linea_pant := linea_actual + scroll_y[round(f * 3.5625)];
    if tile_mid then
      n2 := shortint(vram_bank[0, bg_addr + (f + ((linea_pant div 8) * 32) and $3FF)])
    else
      n2 := byte(vram_bank[0, bg_addr + (f + ((linea_pant div 8) * 32) and $3FF)]);
    atrib := vram_bank[1, bg_addr + (f + ((linea_pant div 8) * 32) and $3FF)];
    if (atrib and $40) <> 0 then
      y := (7 - (linea_pant and $7)) * 2
    else
      y := (linea_pant and $7) * 2;
    tile_bank := (atrib shr 3) and 1;
    tile_pal := (atrib and 7) shl 2;
    tile_val1 := vram_bank[tile_bank, (n2 * 16 + tile_addr + y) and $1FFF];
    tile_val2 := vram_bank[tile_bank, (n2 * 16 + tile_addr + 1 + y) and $1FFF];
    ptemp := punbuf;
    if (atrib and $20) <> 0 then
    begin
      for x := 0 to 7 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        // Ignorar la prioridad de los sprites!
        if (((atrib and $80) <> 0) and (pval <> 0)) then
        begin
          bg_prio[(f * 8 + x + (256 - scroll_x)) and $FF] := bg_prio[f * 8 + x + (256 - scroll_x)] or 4;
          ptemp^ := paleta[(bgc_pal[pval + tile_pal]) and $7FFF];
        end
        else
        begin
          case prio of
            0:
              ptemp^ := paleta[(bgc_pal[pval + tile_pal]) and $7FFF];
            1:
              if (pval <> 0) then
              begin
                if ((bg_prio[(f * 8 + x + (256 - scroll_x) + 7) and $FF] and 2) = 0) then
                  ptemp^ := paleta[(bgc_pal[pval + tile_pal]) and $7FFF]
                else
                  ptemp^ := paleta[MAX_COLORS];
              end
              else
                ptemp^ := paleta[MAX_COLORS];
          end;
        end;
        inc(ptemp);
      end;
    end
    else
    begin // Flipx
      for x := 7 downto 0 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        if (((atrib and $80) <> 0) and (pval <> 0)) then
        begin
          bg_prio[(f * 8 + (7 - x) + (256 - scroll_x)) and $FF] := bg_prio[f * 8 + (7 - x) + (256 - scroll_x)] or 4;
          ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF];
        end
        else
        begin
          case prio of
            0:
              ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF];
            1:
              if (pval <> 0) then
              begin
                if ((bg_prio[(f * 8 + (7 - x) + (256 - scroll_x) + 7) and $FF] and 2) = 0) then
                  ptemp^ := paleta[(bgc_pal[pval + tile_pal]) and $7FFF]
                else
                  ptemp^ := paleta[MAX_COLORS];
              end
              else
                ptemp^ := paleta[MAX_COLORS];
          end;
        end;
        inc(ptemp);
      end;
    end;
    putpixel(f * 8, 0, 8, punbuf, 1);
  end;
  // Scroll X
  if scroll_x <> 0 then
  begin
    update_region(0, 0, scroll_x, 1, 1, (256 - scroll_x) + 7, linea_actual, scroll_x, 1, 2);
    update_region(scroll_x, 0, 256 - scroll_x, 1, 1, 7, linea_actual, 256 - scroll_x, 1, 2);
  end
  else
    update_region(0, 0, 256, 1, 1, 7, linea_actual, 256, 1, 2);
end;

procedure update_window_gbc(prio: byte);
var
  tile_addr, bg_addr: word;
  f, atrib, tile_bank, tile_pal: byte;
  x, tile_val1, tile_val2, y, pval: byte;
  n2: integer;
  tile_mid: boolean;
  ptemp: pword;
begin
  if ((linea_actual < window_y) or (window_x > 166) or (window_x = 0)) then
    exit;
  bg_addr := $1800 + ((lcd_control and $40) shl 4);
  tile_mid := (lcd_control and $10) = 0;
  tile_addr := $1000 * byte(tile_mid); // Cuidado! Tiene signo despues
  for f := 0 to 31 do
  begin
    if tile_mid then
      n2 := shortint(vram_bank[0, (bg_addr + f + ((window_y_draw div 8) * 32)) and $1FFF])
    else
      n2 := byte(vram_bank[0, (bg_addr + f + ((window_y_draw div 8) * 32)) and $1FFF]);
    atrib := vram_bank[1, (bg_addr + f + ((window_y_draw div 8) * 32)) and $1FFF];
    if (atrib and $40) <> 0 then
      y := (7 - (window_y_draw and $7)) * 2
    else
      y := (window_y_draw and $7) * 2;
    tile_bank := (atrib shr 3) and 1;
    tile_pal := (atrib and 7) shl 2;
    tile_val1 := vram_bank[tile_bank, (n2 * 16 + tile_addr + y) and $1FFF];
    tile_val2 := vram_bank[tile_bank, (n2 * 16 + tile_addr + 1 + y) and $1FFF];
    ptemp := punbuf;
    if (atrib and $20) <> 0 then
    begin
      for x := 0 to 7 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        case prio of
          0:
            begin
              ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF];
              bg_prio[(f * 8 + x - (256 - scroll_x) + window_x - 7) and $FF] := bg_prio[(f * 8 + x - (256 - scroll_x) + window_x - 7) and $FF] or 2;
            end;
          1:
            if (pval = 0) then
            begin
              if bg_prio[(f * 8 + x - (256 - scroll_x) + window_x - 7) and $FF] = 0 then
                ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF]
              else
                ptemp^ := paleta[MAX_COLORS];
            end
            else
              ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF];
        end;
        inc(ptemp);
      end;
    end
    else
    begin
      for x := 7 downto 0 do
      begin
        pval := ((tile_val1 shr x) and $1) + (((tile_val2 shr x) and $1) shl 1);
        case prio of
          0:
            begin
              ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF];
              bg_prio[(f * 8 + (7 - x) - (256 - scroll_x) + window_x - 7) and $FF] := bg_prio[(f * 8 + (7 - x) - (256 - scroll_x) + window_x - 7) and $FF] or 2;
            end;
          1:
            if (pval = 0) then
            begin
              if bg_prio[(f * 8 + (7 - x) - (256 - scroll_x) + window_x - 7) and $FF] = 0 then
                ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF]
              else
                ptemp^ := paleta[MAX_COLORS];
            end
            else
              ptemp^ := paleta[bgc_pal[pval + tile_pal] and $7FFF];
        end;
        inc(ptemp);
      end;
    end;
    putpixel(f * 8, 0, 8, punbuf, 1);
  end;
  // Pos X
  update_region(0, 0, 256, 1, 1, window_x, linea_actual, 256, 1, 2);
end;

// El renderizado de la GBC es igual que la GB normal, excepto por dos cosas
// Se puede hacer que el fondo tenga prioridad sobre los sprites, independientemente de la prioridad de los sprites (Intro en 007 TWNI)
// En lugar de encender/apagar el fondo, el bit se usa para priorizar los sprites sobre fondo y window, no importa la prioridad
procedure update_video_gbc;
var
  f: byte;
begin
  for f := scroll_y_pos to 113 do
    scroll_y[f] := scroll_y_last;
  scroll_y_pos := 0;
  single_line(7, linea_actual, paleta[0], 160, 2);
  if lcd_ena then
  begin
    fillchar(bg_prio[0], $100, 0);
    update_bg_gbc(0);
    if (lcd_control and $20) <> 0 then
      update_window_gbc(0);
    if (((lcd_control and 2) <> 0) and not(oam_dma)) then
    begin
      get_active_sprites_gbc;
      draw_sprites_gbc($80);
    end;
    if (lcd_control and 1) <> 0 then
    begin // Mirar si fondo y window pierden sus prioridades!
      update_bg_gbc(1);
      if (lcd_control and $20) <> 0 then
        update_window_gbc(1);
    end;
    if (((lcd_control and 2) <> 0) and not(oam_dma)) then
      draw_sprites_gbc(0);
    if (not((linea_actual < window_y) or (window_x > 166) or (window_x = 0)) and ((lcd_control and $20) <> 0)) then
      window_y_draw := window_y_draw + 1;
  end;
end;

procedure eventos_gb;
var
  tmp_in0: byte;
begin
  if event.arcade then
  begin
    tmp_in0 := marcade.in0;
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if tmp_in0 <> marcade.in0 then
      lr35902_0.joystick_req := true;
  end;
end;

procedure cerrar_gb;
begin
  if hay_nvram then
    write_file(nv_ram_name, @ram_bank[0, 0], $2000);
  gameboy_sound_close;
end;

function leer_io(direccion: byte): byte;
var
  tempb: byte;
begin
  case direccion of
    $00:
      begin
        // Es muy importante este orden!!! Por ejemplo Robocop Rev1 no funciona si no es asi
        if (joystick and $10) = 0 then
          joystick := (joystick or $F) and (marcade.in0 or $F0);
        if (joystick and $20) = 0 then
          joystick := (joystick or $F) and ((marcade.in0 shr 4) or $F0);
        leer_io := joystick;
      end;
    $01:
      leer_io := 0;
    $02:
      leer_io := $7E; // Serial
    $04:
      leer_io := mtimer;
    $05:
      leer_io := prog_timer;
    $06:
      leer_io := tmodulo;
    $07:
      leer_io := $F8 or tcontrol;
    $0F:
      begin
        tempb := $E0;
        if lr35902_0.vblank_req then
          tempb := tempb or $1;
        if lr35902_0.lcdstat_req then
          tempb := tempb or $2;
        if lr35902_0.timer_req then
          tempb := tempb or $4;
        if lr35902_0.serial_req then
          tempb := tempb or $8;
        if lr35902_0.joystick_req then
          tempb := tempb or $10;
        leer_io := tempb;
      end;
    $10 .. $26:
      leer_io := gb_sound_r(direccion - $10); // Sound
    $30 .. $3F:
      leer_io := gb_wave_r(direccion - $30); // Sound Wav
    $40:
      leer_io := lcd_control;
    $41:
      leer_io := $80 or stat;
    $42:
      leer_io := scroll_y_last;
    $43:
      leer_io := scroll_x;
    $44:
      leer_io := linea_actual;
    $45:
      leer_io := ly_compare;
    $47:
      leer_io := bg_pal;
    $48:
      leer_io := sprt0_pal;
    $49:
      leer_io := sprt1_pal;
    $4A:
      leer_io := window_y;
    $4B:
      leer_io := window_x;
    // $80..$fe:leer_io:=io_ram[direccion];  //high memory
    $FF:
      leer_io := irq_ena;
  else
    leer_io := io_ram[direccion];
  end;
end;

procedure escribe_io(direccion, valor: byte);
var
  f: byte;
  addrs: word;
begin
  case direccion of
    $00:
      joystick := $CF or (valor and $30);
    $01:
      ; // Serial
    $02:
      if (valor and $81) = $81 then
        lr35902_0.serial_req := true;
    $04:
      mtimer := 0;
    $05:
      prog_timer := valor;
    $06:
      tmodulo := valor;
    $07:
      begin // timer control
        tcontrol := valor and $7;
        case (valor and $3) of
          0:
            timers.timer[gb_timer].time_final := GB_CLOCK / 4096;
          1:
            timers.timer[gb_timer].time_final := GB_CLOCK / 262144;
          2:
            timers.timer[gb_timer].time_final := GB_CLOCK / 65536;
          3:
            timers.timer[gb_timer].time_final := GB_CLOCK / 16384;
        end;
        timers.enabled(gb_timer, (valor and $4) <> 0);
      end;
    $0F:
      begin // irq request
        lr35902_0.vblank_req := (valor and $1) <> 0;
        lr35902_0.lcdstat_req := (valor and $2) <> 0;
        lr35902_0.timer_req := (valor and $4) <> 0;
        lr35902_0.serial_req := (valor and $8) <> 0;
        lr35902_0.joystick_req := (valor and $10) <> 0;
      end;
    $10 .. $26:
      gb_sound_w(direccion - $10, valor); // Sound
    $30 .. $3F:
      gb_wave_w(direccion and $F, valor); // Sound Wav
    $40:
      begin
        lcd_control := valor;
        lcd_ena := (valor and $80) <> 0;
        if not(lcd_ena) then
          stat := stat and $FC;
      end;
    $41:
      stat := (stat and $7) or (valor and $F8);
    $42:
      begin
        addrs := lr35902_0.contador div 4;
        for f := scroll_y_pos to (addrs - 1) do
          scroll_y[f] := valor;
        scroll_y_pos := addrs;
        scroll_y_last := valor;
      end;
    $43:
      scroll_x := valor;
    $44:
      ;
    $45:
      ly_compare := valor;
    $46:
      begin // DMA trans OAM
        addrs := valor shl 8;
        for f := 0 to $9F do
        begin
          case addrs of
            $0000 .. $7FFF:
              sprt_ram[f] := memory[addrs];
            $8000 .. $9FFF:
              sprt_ram[f] := $FF; // Lee datos incorrectos
            $A000 .. $BFFF:
              if @gb_mapper.ext_ram_getbyte <> nil then
                sprt_ram[f] := gb_mapper.ext_ram_getbyte(addrs and $1FFF)
              else
                sprt_ram[f] := $FF;
            $C000 .. $CFFF:
              sprt_ram[f] := wram_bank[0, addrs and $FFF];
            $D000 .. $DFFF:
              sprt_ram[f] := wram_bank[1, addrs and $FFF];
            $E000 .. $FFFF:
              if @gb_mapper.ext_ram_getbyte <> nil then
                sprt_ram[f] := gb_mapper.ext_ram_getbyte(addrs and $1FFF) // a000
              else
                sprt_ram[f] := $FF;
          end;
          addrs := addrs + 1;
        end;
        // CUIDADO!!! La CPU no se para!! Sigue funcionando, pero la memoria NO es accesible!
        oam_dma_pos := 0;
        oam_dma := true;
      end;
    $47:
      begin
        bg_pal := valor;
        set_pal_color(color_pal[gb_palette, (valor shr 0) and $3], 0);
        set_pal_color(color_pal[gb_palette, (valor shr 2) and $3], 1);
        set_pal_color(color_pal[gb_palette, (valor shr 4) and $3], 2);
        set_pal_color(color_pal[gb_palette, (valor shr 6) and $3], 3);
      end;
    $48:
      begin // sprt0
        sprt0_pal := valor;
        set_pal_color(color_pal[gb_palette, (valor shr 0) and $3], 4);
        set_pal_color(color_pal[gb_palette, (valor shr 2) and $3], 5);
        set_pal_color(color_pal[gb_palette, (valor shr 4) and $3], 6);
        set_pal_color(color_pal[gb_palette, (valor shr 6) and $3], 7);
      end;
    $49:
      begin
        sprt1_pal := valor;
        set_pal_color(color_pal[gb_palette, (valor shr 0) and $3], 8);
        set_pal_color(color_pal[gb_palette, (valor shr 2) and $3], 9);
        set_pal_color(color_pal[gb_palette, (valor shr 4) and $3], 10);
        set_pal_color(color_pal[gb_palette, (valor shr 6) and $3], 11);
      end;
    $4A:
      window_y := valor;
    $4B:
      window_x := valor;
    $50:
      enable_bios := false; // disable ROM
    // $80..$fe:io_ram[direccion]:=valor;  //high memory
    $FF:
      begin // irq enable
        irq_ena := valor;
        lr35902_0.vblank_ena := (valor and $1) <> 0;
        lr35902_0.lcdstat_ena := (valor and $2) <> 0;
        lr35902_0.timer_ena := (valor and $4) <> 0;
        lr35902_0.serial_ena := (valor and $8) <> 0;
        lr35902_0.joystick_ena := (valor and $10) <> 0;
      end;
  else
    io_ram[direccion] := valor;
    // MessageDlg('IO desconocida escribe pos= '+inttohex(direccion and $ff,2)+' - '+inttohex(valor,2), mtInformation,[mbOk], 0);
  end;
end;

// Color GB
function leer_io_gbc(direccion: byte): byte;
var
  tempb: byte;
begin
  case direccion of
    $00:
      begin
        if (joystick and $10) = 0 then
          joystick := (joystick or $F) and (marcade.in0 or $F0);
        if (joystick and $20) = 0 then
          joystick := (joystick or $F) and ((marcade.in0 shr 4) or $F0);
        leer_io_gbc := joystick;
      end;
    $01:
      leer_io_gbc := 0;
    $02:
      leer_io_gbc := $7C; // Serial
    $04:
      leer_io_gbc := mtimer;
    $05:
      leer_io_gbc := prog_timer;
    $06:
      leer_io_gbc := tmodulo;
    $07:
      leer_io_gbc := $F8 or tcontrol;
    $0F:
      begin
        tempb := $E0;
        if lr35902_0.vblank_req then
          tempb := tempb or $1;
        if lr35902_0.lcdstat_req then
          tempb := tempb or $2;
        if lr35902_0.timer_req then
          tempb := tempb or $4;
        if lr35902_0.serial_req then
          tempb := tempb or $8;
        if lr35902_0.joystick_req then
          tempb := tempb or $10;
        leer_io_gbc := tempb;
      end;
    $10 .. $26:
      leer_io_gbc := gb_sound_r(direccion - $10); // Sound
    // $27..$2f:leer_io_gbc:=io_ram[direccion];
    $30 .. $3F:
      leer_io_gbc := gb_wave_r(direccion and $F); // Sound Wav
    $40:
      leer_io_gbc := lcd_control;
    $41:
      leer_io_gbc := $80 or stat;
    $42:
      leer_io_gbc := scroll_y_last;
    $43:
      leer_io_gbc := scroll_x;
    $44:
      leer_io_gbc := linea_actual;
    $45:
      leer_io_gbc := ly_compare;
    $47:
      leer_io_gbc := bg_pal;
    $48:
      leer_io_gbc := sprt0_pal;
    $49:
      leer_io_gbc := sprt1_pal;
    $4A:
      leer_io_gbc := window_y;
    $4B:
      leer_io_gbc := window_x;
    $4D:
      leer_io_gbc := (lr35902_0.speed shl 7) + $7E + byte(lr35902_0.change_speed);
    $4F:
      leer_io_gbc := $FE or vram_nbank;
    $51 .. $54:
      leer_io_gbc := $FF;
    $55:
      leer_io_gbc := hdma_size;
    $56:
      leer_io_gbc := 1;
    $68:
      leer_io_gbc := bgcolor_index;
    $69:
      if (bgcolor_index and 1) <> 0 then
        leer_io_gbc := bgc_pal[bgcolor_index shr 1] shr 8
      else
        leer_io_gbc := bgc_pal[bgcolor_index shr 1] and $FF;
    $6A:
      leer_io_gbc := spcolor_index;
    $6B:
      if (spcolor_index and 1) <> 0 then
        leer_io_gbc := spc_pal[spcolor_index shr 1] shr 8
      else
        leer_io_gbc := spc_pal[spcolor_index shr 1] and $FF;
    $70:
      leer_io_gbc := $F8 or wram_nbank;
    $80 .. $FE:
      leer_io_gbc := io_ram[direccion]; // high memory
    $FF:
      leer_io_gbc := irq_ena;
  else
    begin
      // MessageDlg('IO desconocida leer pos= '+inttohex(direccion and $ff,2), mtInformation,[mbOk], 0);
      leer_io_gbc := io_ram[direccion];
    end;
  end;
end;

procedure dma_trans(size: word);
var
  f: word;
  temp: byte;
begin
  for f := 0 to (size - 1) do
  begin
    temp := $FF;
    case dma_src of
      $0000 .. $7FFF:
        temp := memory[dma_src];
      // $8000..$9fff:temp:=$ff;
      $A000 .. $BFFF:
        if @gb_mapper.ext_ram_getbyte <> nil then
          temp := gb_mapper.ext_ram_getbyte(dma_src and $1FFF);
      $C000 .. $CFFF:
        temp := wram_bank[0, dma_src and $FFF];
      $D000 .. $DFFF:
        temp := wram_bank[wram_nbank, dma_src and $FFF];
      $E000 .. $FFFF:
        if @gb_mapper.ext_ram_getbyte <> nil then
          temp := gb_mapper.ext_ram_getbyte(dma_src and $1FFF);
    end;
    vram_bank[vram_nbank, dma_dst and $1FFF] := temp;
    dma_dst := dma_dst + 1;
    dma_src := dma_src + 1;
  end;
end;

procedure escribe_io_gbc(direccion, valor: byte);
var
  addrs: word;
  f: byte;
begin
  io_ram[direccion] := valor;
  case direccion of
    $00:
      joystick := $CF or (valor and $30);
    $01:
      ; // Serial
    $02:
      if (valor and $81) = $81 then
        lr35902_0.serial_req := true;
    $04:
      mtimer := 0;
    $05:
      prog_timer := valor;
    $06:
      tmodulo := valor;
    $07:
      begin // timer control
        tcontrol := valor and $7;
        case (valor and $3) of
          0:
            timers.timer[gb_timer].time_final := GB_CLOCK / 4096;
          1:
            timers.timer[gb_timer].time_final := GB_CLOCK / 262144;
          2:
            timers.timer[gb_timer].time_final := GB_CLOCK / 65536;
          3:
            timers.timer[gb_timer].time_final := GB_CLOCK / 16384;
        end;
        timers.enabled(gb_timer, (valor and $4) <> 0);
      end;
    $0F:
      begin // irq request
        lr35902_0.vblank_req := (valor and $1) <> 0;
        lr35902_0.lcdstat_req := (valor and $2) <> 0;
        lr35902_0.timer_req := (valor and $4) <> 0;
        lr35902_0.serial_req := (valor and $8) <> 0;
        lr35902_0.joystick_req := (valor and $10) <> 0;
      end;
    $10 .. $26:
      gb_sound_w(direccion - $10, valor); // Sound
    // $27..$2f:io_ram[direccion]:=valor;
    $30 .. $3F:
      gb_wave_w(direccion and $F, valor); // Sound Wav
    $40:
      begin
        lcd_control := valor;
        lcd_ena := (valor and $80) <> 0;
        if not(lcd_ena) then
          stat := stat and $FC;
      end;
    $41:
      stat := (stat and $7) or (valor and $F8);
    $42:
      begin
        addrs := (lr35902_0.contador shr lr35902_0.speed) div 4;
        for f := scroll_y_pos to (addrs - 1) do
          scroll_y[f] := valor;
        scroll_y_pos := addrs;
        scroll_y_last := valor;
      end;
    $43:
      scroll_x := valor;
    $44:
      ;
    $45:
      ly_compare := valor;
    $46:
      begin // DMA trans OAM
        addrs := valor shl 8;
        for f := 0 to $9F do
        begin
          case addrs of
            $0000 .. $7FFF:
              sprt_ram[f] := memory[addrs];
            $8000 .. $9FFF:
              sprt_ram[f] := $FF; // Lee datos incorrectos
            $A000 .. $BFFF:
              if @gb_mapper.ext_ram_getbyte <> nil then
                sprt_ram[f] := gb_mapper.ext_ram_getbyte(addrs and $1FFF)
              else
                sprt_ram[f] := $FF;
            $C000 .. $CFFF:
              sprt_ram[f] := wram_bank[0, addrs and $FFF];
            $D000 .. $DFFF:
              sprt_ram[f] := wram_bank[1, addrs and $FFF];
            $E000 .. $FFFF:
              if @gb_mapper.ext_ram_getbyte <> nil then
                sprt_ram[f] := gb_mapper.ext_ram_getbyte(addrs and $1FFF) // a000
              else
                sprt_ram[f] := $FF;
          end;
          addrs := addrs + 1;
        end;
        oam_dma_pos := 0;
        oam_dma := true;
      end;
    $47:
      bg_pal := valor;
    $48:
      sprt0_pal := valor;
    $49:
      sprt1_pal := valor;
    $4A:
      window_y := valor;
    $4B:
      window_x := valor;
    // $4c:io_ram[direccion]:=valor;  //????
    $4D:
      lr35902_0.change_speed := (valor and 1) <> 0; // Cambiar velocidad
    $4F:
      vram_nbank := valor and 1; // VRAM Bank
    $50:
      enable_bios := false; // disable ROM
    $51:
      dma_src := (dma_src and $FF) or (valor shl 8);
    $52:
      dma_src := (dma_src and $FF00) or (valor and $F0);
    $53:
      dma_dst := (dma_dst and $FF) or ((valor and $1F) shl 8);
    $54:
      dma_dst := (dma_dst and $FF00) or (valor and $F0);
    $55:
      if (hdma_ena and ((valor and $80) <> 0)) then
      begin // Cancelar la transferencia!
        hdma_ena := false;
        hdma_size := hdma_size or $80;
      end
      else
      begin
        if (valor and $80) <> 0 then
        begin
          hdma_size := valor and $7F;
          hdma_ena := true;
        end
        else
        begin
          valor := valor + 1;
          dma_trans(valor * $10);
          lr35902_0.estados_demas := lr35902_0.estados_demas + (220 shr lr35902_0.speed) + (8 * valor);
        end;
      end;
    $56:
      ;
    $68:
      begin
        bgcolor_inc := (valor and $80) <> 0;
        bgcolor_index := valor and $3F;
      end;
    $69:
      begin
        if (stat and 3) <> 3 then
        begin
          if (bgcolor_index and 1) <> 0 then
            bgc_pal[bgcolor_index shr 1] := (bgc_pal[bgcolor_index shr 1] and $FF) or (valor shl 8)
          else
            bgc_pal[bgcolor_index shr 1] := (bgc_pal[bgcolor_index shr 1] and $FF00) or valor;
        end;
        if bgcolor_inc then
          bgcolor_index := (bgcolor_index + 1) and $3F;
      end;
    $6A:
      begin
        spcolor_inc := (valor and $80) <> 0;
        spcolor_index := valor and $3F;
      end;
    $6B:
      begin
        if (stat and 3) <> 3 then
        begin
          if (spcolor_index and 1) <> 0 then
            spc_pal[spcolor_index shr 1] := (spc_pal[spcolor_index shr 1] and $FF) or (valor shl 8)
          else
            spc_pal[spcolor_index shr 1] := (spc_pal[spcolor_index shr 1] and $FF00) or valor;
        end;
        if spcolor_inc then
          spcolor_index := (spcolor_index + 1) and $3F;
      end;
    $70:
      begin
        wram_nbank := valor and 7;
        if wram_nbank = 0 then
          wram_nbank := 1;
      end;
    $7E, $7F:
      ;
    // $80..$fe:io_ram[direccion]:=valor;  //high memory
    $FF:
      begin // irq enable
        irq_ena := valor;
        lr35902_0.vblank_ena := (valor and $1) <> 0;
        lr35902_0.lcdstat_ena := (valor and $2) <> 0;
        lr35902_0.timer_ena := (valor and $4) <> 0;
        lr35902_0.serial_ena := (valor and $8) <> 0;
        lr35902_0.joystick_ena := (valor and $10) <> 0;
      end;
    // else io_ram[direccion]:=valor;//MessageDlg('IO desconocida escribe pos= '+inttohex(direccion and $ff,2)+' - '+inttohex(valor,2), mtInformation,[mbOk], 0);
  end;
end;

procedure gb_loop;
var
  frame_m: single;
begin
  init_controls(false, false, false, true);
  frame_m := lr35902_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for linea_actual := 0 to 153 do
    begin
      lr35902_0.run(frame_m);
      frame_m := frame_m + lr35902_0.tframes - lr35902_0.contador;
      if linea_actual < 144 then
        gameboy.video_render; // Modos 2-3-0
    end;
    window_y_draw := 0;
    eventos_gb;
    update_region(7, 0, 160, 144, 2, 0, 0, 160, 144, pant_temp);
    video_sync;
  end;
end;

function gb_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $FF, $200 .. $8FF:
      if enable_bios then
        gb_getbyte := bios_rom[direccion]
      else
        gb_getbyte := memory[direccion];
    $0100 .. $1FF, $900 .. $3FFF:
      gb_getbyte := memory[direccion];
    $4000 .. $7FFF:
      gb_getbyte := memory[direccion];
    $8000 .. $9FFF:
      gb_getbyte := vram_bank[vram_nbank, direccion and $1FFF];
    $A000 .. $BFFF:
      if @gb_mapper.ext_ram_getbyte <> nil then
        gb_getbyte := gb_mapper.ext_ram_getbyte(direccion)
      else
        gb_getbyte := $FF;
    $C000 .. $CFFF, $E000 .. $EFFF:
      gb_getbyte := wram_bank[0, direccion and $FFF];
    $D000 .. $DFFF, $F000 .. $FDFF:
      gb_getbyte := wram_bank[wram_nbank, direccion and $FFF];
    $FE00 .. $FE9F:
      gb_getbyte := sprt_ram[direccion and $FF];
    $FEA0 .. $FEFF:
      if not(gameboy.is_gbc) then
        gb_getbyte := 0
      else
      begin
        case (direccion and $FF) of
          $A0 .. $CF:
            gb_getbyte := memory[direccion];
          $D0 .. $FF:
            gb_getbyte := memory[$FEC0 + (direccion and $F)];
        end;
      end;
    $FF00 .. $FFFF:
      gb_getbyte := gameboy.read_io(direccion and $FF);
  end;
end;

procedure gb_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0000 .. $7FFF:
      if @gb_mapper.rom_putbyte <> nil then
        gb_mapper.rom_putbyte(direccion, valor);
    $8000 .. $9FFF:
      vram_bank[vram_nbank, direccion and $1FFF] := valor;
    $A000 .. $BFFF:
      if @gb_mapper.ext_ram_putbyte <> nil then
        gb_mapper.ext_ram_putbyte(direccion, valor);
    $C000 .. $CFFF, $E000 .. $EFFF:
      wram_bank[0, direccion and $FFF] := valor;
    $D000 .. $DFFF, $F000 .. $FDFF:
      wram_bank[wram_nbank, direccion and $FFF] := valor;
    $FE00 .. $FE9F:
      sprt_ram[direccion and $FF] := valor;
    $FEA0 .. $FEFF:
      if gameboy.is_gbc then
      begin
        case (direccion and $FF) of
          $A0 .. $CF:
            memory[direccion] := valor;
          $D0 .. $FF:
            memory[$FEC0 + (direccion and $F)] := valor;
        end;
      end;
    $FF00 .. $FFFF:
      gameboy.write_io(direccion and $FF, valor);
  end;
end;

procedure gb_despues_instruccion(estados_t: word);
var
  lcd_compare, lcd_mode: boolean;
begin
  lcd_compare := false;
  lcd_mode := false;
  // Ver si estoy en OAM DMA
  if oam_dma then
  begin
    oam_dma_pos := oam_dma_pos + estados_t;
    if oam_dma_pos >= 160 then
      oam_dma := false;
  end;
  if not(lcd_ena) then
    exit;
  // Si la linea es 144 y el LCD estα en ON --> VBLANK
  // Haaaaaack, si no lo hace en la 146 SML2 no funciona...
  if ((linea_actual = 146) and (lr35902_0.contador = 8)) then
    lr35902_0.vblank_req := true;
  // CUIDADO! Cuando se activa la IRQ en la linea del LCD ya no se aceptan mαs IRQ en la misma linea!!
  // Esto se llama STAT IRQ glitch
  case lr35902_0.contador of
    8:
      begin
        // LY compare
        if linea_actual = ly_compare then
        begin
          lcd_compare := (stat and $40) <> 0;
          stat := stat or $4;
        end
        else
          stat := stat and $FB;
        case linea_actual of
          0 .. 143:
            if ((stat and 3) <> 2) then
            begin // Modo 2
              lcd_mode := (stat and $20) <> 0;
              stat := (stat and $FC) or $2;
            end;
          144:
            if ((stat and 3) <> 1) then
            begin
              // Modo 1
              lcd_mode := (stat and $10) <> 0;
              stat := (stat and $FC) or $1;
            end;
        end;
      end;
    88:
      if ((linea_actual < 144) and ((stat and 3) <> 3)) then
      begin // Modo 3
        lcd_mode := ((stat and $20) <> 0) and ((stat and $10) = 0);
        stat := (stat and $FC) or $3;
      end;
    252 .. 600:
      if ((linea_actual < 144) and ((sprites_time + 252) >= lr35902_0.contador) and ((stat and 3) <> 0)) then
      begin // Modo 0
        lcd_mode := ((stat and $8) <> 0) and ((stat and $20) = 0);
        stat := stat and $FC;
      end;
  end;
  lr35902_0.lcdstat_req := lr35902_0.lcdstat_req or lcd_compare or lcd_mode;
end;

procedure gbc_despues_instruccion(estados_t: word);
var
  lcd_compare, lcd_mode: boolean;
  contador: word;
begin
  lcd_compare := false;
  lcd_mode := false;
  // Ver si estoy en OAM DMA
  if oam_dma then
  begin
    oam_dma_pos := oam_dma_pos + (estados_t shr lr35902_0.speed);;
    if oam_dma_pos >= 160 then
      oam_dma := false;
  end;
  if lr35902_0.changed_speed then
  begin
    lr35902_0.tframes := round(((GB_CLOCK shl lr35902_0.speed) / 154) / machine_calls.fps_max);
    sound_engine_change_clock(GB_CLOCK shl lr35902_0.speed);
    lr35902_0.changed_speed := false;
  end;
  if not(lcd_ena) then
    exit;
  contador := lr35902_0.contador shr lr35902_0.speed;
  if ((linea_actual = 144) and (contador = 8)) then
    lr35902_0.vblank_req := true; // int 40!!
  case contador of
    8:
      begin
        haz_dma := false;
        // LY compare
        if linea_actual = ly_compare then
        begin
          lcd_compare := (stat and $40) <> 0;
          stat := stat or $4;
        end
        else
          stat := stat and $FB;
        case linea_actual of
          0 .. 143:
            if ((stat and 3) <> 2) then
            begin // Mode 2
              lcd_mode := (stat and $20) <> 0;
              stat := (stat and $FC) or $2;
            end;
          144:
            if ((stat and 3) <> 1) then
            begin
              lcd_mode := ((stat and $10) <> 0);
              stat := (stat and $FC) or $1;
            end;
        end;
      end;
    88:
      if ((linea_actual < 144) and ((stat and 3) <> 3)) then
      begin // Modo 3
        lcd_mode := ((stat and $20) <> 0) and ((stat and $10) = 0);
        stat := (stat and $FC) or $3;
      end;
    252 .. 600:
      if (linea_actual < 144) then
      begin
        if ((contador >= 308) and hdma_ena and not(haz_dma)) then
        begin // DMA H-Blank
          dma_trans($10);
          hdma_size := hdma_size - 1;
          if hdma_size = $FF then
            hdma_ena := false;
          lr35902_0.contador := lr35902_0.contador + 8;
          haz_dma := true;
        end;
        if (((sprites_time + 252) >= contador) and ((stat and 3) <> 0)) then
        begin // Modo 0
          lcd_mode := ((stat and $8) <> 0) and ((stat and $20) = 0);
          stat := stat and $FC;
        end;
      end;
  end;
  lr35902_0.lcdstat_req := lr35902_0.lcdstat_req or lcd_compare or lcd_mode;
end;

// Sonido and timers
procedure gb_main_timer;
begin
  mtimer := mtimer + 1;
end;

// Main
procedure reset_gb;
var
  lr_reg: reg_lr;
begin
  lr35902_0.tframes := (GB_CLOCK / 154) / machine_calls.fps_max;
  sound_engine_change_clock(GB_CLOCK);
  lr35902_0.reset;
  reset_audio;
  gameboy_sound_reset;
  scroll_x := 0;
  fillchar(scroll_y[0], $FF, 0);
  fillchar(io_ram[0], $FF, 0);
  // io_ram[128]:=$e0; //3e
  // io_ram[129]:=$46; //df
  // io_ram[130]:=$3e; //e0
  // io_ram[131]:=$28; //46
  // io_ram[132]:=$3d; //3e
  // io_ram[133]:=$20; //28
  // io_ram[134]:=$fd; //3d
  // io_ram[135]:=$c9; //20
  // io_ram[136]:=$fd;
  // io_ram[137]:=$c9;
  fillchar(sprt_ram[0], $FF, 0);
  fillchar(bgc_pal[0], $FF, 0);
  fillchar(bgc_pal[0], $FF, 0);
  scroll_y_pos := 0;
  scroll_y_last := 0;
  stat := 0;
  tmodulo := 0;
  mtimer := 0;
  prog_timer := 0;
  rom_nbank := 0;
  ram_nbank := 0;
  vram_nbank := 0;
  wram_nbank := 1;
  ly_compare := $FF;
  irq_ena := 0;
  marcade.in0 := $FF;
  joystick := $FF;
  hdma_ena := false;
  hdma_size := $FF;
  lcd_control := $80;
  lcd_ena := true;
  oam_dma_pos := 0;
  oam_dma := false;
  window_y_draw := 0;
  bg_pal := 0;
  sprt0_pal := 0;
  sprt1_pal := 0;
  window_x := 0;
  window_y := 0;
  if not(rom_exist) then
  begin
    enable_bios := false;
    lr_reg.pc := $100;
    lr_reg.sp := $FFFE;
    lr_reg.f.z := true;
    lr_reg.f.n := false;
    if not(gameboy.is_gbc) then
    begin
      lr_reg.a := $11;
      lr_reg.f.h := false;
      lr_reg.f.c := false;
      lr_reg.BC.w := $0;
      lr_reg.DE.w := $FF56;
      lr_reg.HL.w := $000D;
    end
    else
    begin
      lr_reg.a := $01;
      lr_reg.f.h := true;
      lr_reg.f.c := true;
      lr_reg.BC.w := $0013;
      lr_reg.DE.w := $00D8;
      lr_reg.HL.w := $014D;
      escribe_io(05, 00);
      escribe_io(06, 00);
      escribe_io(07, 00);
      escribe_io($10, $80);
      escribe_io($11, $BF);
      escribe_io($12, $F3);
      escribe_io($14, $BF);
      escribe_io($16, $3F);
      escribe_io($17, $00);
      escribe_io($19, $BF);
      escribe_io($1A, $7F);
      escribe_io($1B, $F);
      escribe_io($1C, $9F);
      escribe_io($1E, $BF);
      escribe_io($20, $FF);
      escribe_io($21, $00);
      escribe_io($22, $00);
      escribe_io($23, $BF);
      escribe_io($24, $77);
      escribe_io($25, $F3);
      escribe_io($26, $F1);
      escribe_io($40, $91);
      escribe_io($42, $00);
      escribe_io($43, $00);
      escribe_io($45, $00);
      escribe_io($47, $FC);
      escribe_io($48, $FF);
      escribe_io($49, $FF);
      escribe_io($4A, $00);
      escribe_io($4B, $00);
      escribe_io($00, $00);
    end;
    lr35902_0.set_internal_r(@lr_reg);
  end
  else
    enable_bios := true;
  gb_mapper_reset(gb_head.cart_type);
end;

procedure gb_prog_timer;
begin
  prog_timer := prog_timer + 1;
  if prog_timer = 0 then
  begin
    prog_timer := tmodulo;
    lr35902_0.timer_req := true; // timer request irq
  end;
end;

procedure abrir_gb;
const
  main_logo: array [0 .. $2F] of byte = ($CE, $ED, $66, $66, $CC, $0D, $00, $0B, $03, $73, $00, $83, $00, $0C, $00, $0D, $00, $08, $11, $1F, $88, $89, $00, $0E, $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99,
    $BB, $BB, $67, $63, $6E, $0E, $EC, $CC, $DD, $DC, $99, $9F, $BB, $B9, $33, $3E);
var
  unlicensed: boolean;
  extension, nombre_file, romfile, cadena: string;
  datos, ptemp: pbyte;
  longitud: integer;
  f, h: word;
  colores: tpaleta;
  crc32: dword;
begin
  if not(openrom(romfile)) then
    exit;
  getmem(datos, $800000); // 8Gb??
  if not(extract_data(romfile, datos, longitud, nombre_file)) then
  begin
    freemem(datos);
    exit;
  end;
  extension := extension_fichero(nombre_file);
  // Guardar NVRAM si la hay...
  if hay_nvram then
    write_file(nv_ram_name, @ram_bank[0, 0], $2000);
  hay_nvram := false;
  unlicensed := false;
  if extension = 'DSP' then
    snapshot_r(datos, longitud)
  else
  begin // Cartucho
    ptemp := datos;
    // Copiar datos del cartucho
    copymemory(@gb_head, ptemp, sizeof(tgb_head));
    // Comprobar si está el logo de nintendo... Si no está, unlicensed
    for f := 0 to $2F do
    begin
      unlicensed := (main_logo[f] <> gb_head.logo[f]);
      if unlicensed then
        break;
    end;
    rom_exist := false;
    if gb_head.rom_size = 0 then
      gb_head.rom_size := longitud div $20000;
    if longitud < 32768 then
    begin
      if longitud > 16384 then
      begin
        gb_head.rom_size := 2;
        gb_head.cart_size := 2;
      end
      else
      begin
        gb_head.rom_size := 1;
        gb_head.cart_size := 1;
      end;
    end
    else
    begin
      gb_head.cart_size := (32 shl gb_head.rom_size) div 16;
    end;
    for f := 0 to (gb_head.cart_size - 1) do
    begin
      copymemory(@rom_bank[f, 0], ptemp, $4000);
      inc(ptemp, $4000);
    end;
  end;
  if (gb_head.cgb_flag and $80) <> 0 then
  begin // GameBoy Color
    gameboy.read_io := leer_io_gbc;
    gameboy.write_io := escribe_io_gbc;
    gameboy.video_render := update_video_gbc;
    gameboy.is_gbc := true;
    lr35902_0.change_despues_instruccion(gbc_despues_instruccion);
    cadena := gb_head.title;
    if not(unlicensed) then
      rom_exist := roms_load(@bios_rom[0], gbc_rom, false, 'gbcolor.zip');
    // Iniciar Paletas
    for h := 0 to $7FFF do
    begin
      colores[h].r := (h and $1F) shl 3;
      colores[h].g := ((h shr 5) and $1F) shl 3;
      colores[h].b := ((h shr 10) and $1F) shl 3;
    end;
    set_pal(colores, $8000);
    for f := 0 to $1F do
      bgc_pal[f] := $7FFF;
    for f := 0 to $1F do
      spc_pal[f] := 0;
  end
  else
  begin
    gameboy.read_io := leer_io;
    gameboy.write_io := escribe_io;
    gameboy.video_render := update_video_gb;
    gameboy.is_gbc := false;
    lr35902_0.change_despues_instruccion(gb_despues_instruccion);
    if not(unlicensed) then
      rom_exist := roms_load(@bios_rom[0], gb_rom, false);
    cadena := gb_head.title + gb_head.manu + ansichar(gb_head.cgb_flag);
  end;
  crc32 := calc_crc(datos, longitud);
  freemem(datos);
  gb_mapper.ext_ram_getbyte := nil;
  gb_mapper.ext_ram_putbyte := nil;
  gb_mapper.rom_putbyte := nil;
  case gb_head.cart_type of
    0:
      ; // No mapper
    $01 .. $03:
      begin // mbc1
        gb_mapper.rom_putbyte := gb_putbyte_mbc1;
        case crc32 of
          $B91D6C8D, $509A6B73, $F724B5CE, $B1A8DFD0, $339F1694, $AD376905, $7D1D8FDC, $18B4A02:
            begin
              mbc1_mask := $F;
              mbc1_shift := 4;
            end;
        else
          begin
            mbc1_mask := $1F;
            mbc1_shift := 5;
          end;
        end;
        case gb_head.cart_type of
          1:
            ;
          2:
            begin // RAM
              gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc1;
              gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc1;
            end;
          3:
            begin // RAM + Battery
              gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc1;
              gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc1;
              if read_file_size(nv_ram_name, longitud) then
                read_file(nv_ram_name, @ram_bank[0, 0], longitud);
              hay_nvram := true;
            end;
        end;
      end;
    $5, $6:
      begin // mbc2
        gb_mapper.rom_putbyte := gb_putbyte_mbc2;
        gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc2;
        gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc2;
        gb_head.ram_size := 0;
        if gb_head.cart_type = 6 then
        begin // Battery (No extra RAM!)
          if read_file_size(nv_ram_name, longitud) then
            read_file(nv_ram_name, @ram_bank[0, 0], longitud);
          hay_nvram := true;
        end;
      end;
    $B .. $D:
      begin // mmm01
        gb_mapper.rom_putbyte := gb_putbyte_mmm01;
        gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mmm01;
        gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mmm01;
      end;
    $F .. $13:
      begin // mbc3
        gb_mapper.rom_putbyte := gb_putbyte_mbc3;
        case gb_head.cart_type of
          $F:
            begin // Timer + Battery
              if read_file_size(nv_ram_name, longitud) then
                read_file(nv_ram_name, @ram_bank[0, 0], longitud);
              hay_nvram := true;
            end;
          $10, $13:
            begin // [Timer] + RAM + Battery
              gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc3;
              gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc3;
              if read_file_size(nv_ram_name, longitud) then
                read_file(nv_ram_name, @ram_bank[0, 0], longitud);
              hay_nvram := true;
            end;
          $11:
            ;
          $12:
            begin // RAM
              gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc3;
              gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc3;
            end;
        end;
      end;
    $19 .. $1E:
      begin // mbc5
        gb_mapper.rom_putbyte := gb_putbyte_mbc5;
        case gb_head.cart_type of
          $19, $1C:
            ; // [Rumble]
          $1A, $1D:
            begin // RAM + [Rumble]
              gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc5;
              gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc5;
            end;
          $1B, $1E:
            begin // RAM + Battery + [Rumble]
              gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc5;
              gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc5;
              if read_file_size(nv_ram_name, longitud) then
                read_file(nv_ram_name, @ram_bank[0, 0], longitud);
              hay_nvram := true;
            end;
        end;
      end;
    $22:
      begin // RAM + Acelerometro
        gb_mapper.rom_putbyte := gb_putbyte_mbc7;
        gb_mapper.ext_ram_getbyte := gb_get_ext_ram_mbc7;
        gb_mapper.ext_ram_putbyte := gb_put_ext_ram_mbc7;
      end;
    $FF:
      begin // HuC-1 (RAM+Battery)
        gb_mapper.rom_putbyte := gb_putbyte_huc1;
        gb_mapper.ext_ram_getbyte := gb_get_ext_ram_huc1;
        gb_mapper.ext_ram_putbyte := gb_put_ext_ram_huc1;
        if read_file_size(nv_ram_name, longitud) then
          read_file(nv_ram_name, @ram_bank[0, 0], longitud);
        hay_nvram := true;
      end;
  else
    begin
      // MessageDlg('Mapper ' + inttohex(gb_head.cart_type, 2) + ' no implementado', mtInformation,
      // [mbOk], 0);
    end;
  end;
  if extension <> 'DSP' then
    reset_gb;
  if hay_nvram then
    nv_ram_name := Directory.Arcade_nvram + ChangeFileExt(nombre_file, '.nv');
  Directory.gameboy := ExtractFilePath(romfile);
end;

procedure gb_config_call;
begin
  // configgb.show;
  // while configgb.Showing do
  // application.ProcessMessages;
  if not(gameboy.is_gbc) then
  begin
    set_pal_color(color_pal[gb_palette, (bg_pal shr 0) and $3], 0);
    set_pal_color(color_pal[gb_palette, (bg_pal shr 2) and $3], 1);
    set_pal_color(color_pal[gb_palette, (bg_pal shr 4) and $3], 2);
    set_pal_color(color_pal[gb_palette, (bg_pal shr 6) and $3], 3);
    set_pal_color(color_pal[gb_palette, (sprt0_pal shr 0) and $3], 4);
    set_pal_color(color_pal[gb_palette, (sprt0_pal shr 2) and $3], 5);
    set_pal_color(color_pal[gb_palette, (sprt0_pal shr 4) and $3], 6);
    set_pal_color(color_pal[gb_palette, (sprt0_pal shr 6) and $3], 7);
    set_pal_color(color_pal[gb_palette, (sprt1_pal shr 0) and $3], 8);
    set_pal_color(color_pal[gb_palette, (sprt1_pal shr 2) and $3], 9);
    set_pal_color(color_pal[gb_palette, (sprt1_pal shr 4) and $3], 10);
    set_pal_color(color_pal[gb_palette, (sprt1_pal shr 6) and $3], 11);
  end;
end;

function start_gb: boolean;
begin
  start_gb := false;
  start_audio(true);
  machine_calls.general_loop := gb_loop;
  machine_calls.close := cerrar_gb;
  machine_calls.reset := reset_gb;
  machine_calls.fps_max := 59.727500569605832763727500569606;
  machine_calls.cartridges := abrir_gb;
  machine_calls.setup := gb_config_call;
  screen_init(1, 256, 1, true);
  screen_init(2, 256 + 166 + 7, 154); // 256 pantalla normal + 166 window + 7 de desplazamiento
  start_video(160, 144);
  // Main CPU
  lr35902_0 := cpu_lr.Create(GB_CLOCK, 154); // 154 lineas, 456 estados t por linea
  lr35902_0.change_ram_calls(gb_getbyte, gb_putbyte);
  lr35902_0.init_sound(gameboy_sound_update);
  lr35902_0.change_despues_instruccion(gb_despues_instruccion);
  gameboy.read_io := leer_io;
  gameboy.write_io := escribe_io;
  gameboy.video_render := update_video_gb;
  gameboy.is_gbc := false;
  // Timers internos de la GB
  timers.init(0, GB_CLOCK / 16384, gb_main_timer, nil, true);
  gb_timer := timers.init(0, GB_CLOCK / 4096, gb_prog_timer, nil, false);
  // Sound Chips
  gameboy_sound_ini;
  reset_gb;
  if main_vars.console_init then
    abrir_gb;
  start_gb := true;
end;

end.
