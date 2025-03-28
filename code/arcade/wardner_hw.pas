unit wardner_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  tms32010,
  ym_3812,
  rom_engine,
  pal_engine,
  sound_engine;

function start_wardner: boolean;

implementation

const
  wardner_rom: array [0 .. 3] of tipo_roms = ((n: 'wardner.17'; l: $8000; p: 0; crc: $C5DD56FD), (n: 'b25-18.rom'; l: $10000; p: $8000; crc: $9AAB8EE2), (n: 'b25-19.rom'; l: $10000; p: $18000; crc: $95B68813), (n: 'wardner.20'; l: $8000; p: $28000; crc: $347F411B));
  wardner_snd_rom: tipo_roms = (n: 'b25-16.rom'; l: $8000; p: 0; crc: $E5202FF8);
  wardner_char: array [0 .. 2] of tipo_roms = ((n: 'wardner.07'; l: $4000; p: 0; crc: $1392B60D), (n: 'wardner.06'; l: $4000; p: $4000; crc: $0ED848DA), (n: 'wardner.05'; l: $4000; p: $8000; crc: $79792C86));
  wardner_sprites: array [0 .. 3] of tipo_roms = ((n: 'b25-01.rom'; l: $10000; p: 0; crc: $42EC01FB), (n: 'b25-02.rom'; l: $10000; p: $10000; crc: $6C0130B7), (n: 'b25-03.rom'; l: $10000; p: $20000; crc: $B923DB99), (n: 'b25-04.rom'; l: $10000; p: $30000; crc: $8059573C));
  wardner_fg_tiles: array [0 .. 3] of tipo_roms = ((n: 'b25-12.rom'; l: $8000; p: 0; crc: $15D08848), (n: 'b25-15.rom'; l: $8000; p: $8000; crc: $CDD2D408), (n: 'b25-14.rom'; l: $8000; p: $10000; crc: $5A2AEF4F), (n: 'b25-13.rom'; l: $8000; p: $18000; crc: $BE21DB2B));
  wardner_bg_tiles: array [0 .. 3] of tipo_roms = ((n: 'b25-08.rom'; l: $8000; p: 0; crc: $883CCAA3), (n: 'b25-11.rom'; l: $8000; p: $8000; crc: $D6EBD510), (n: 'b25-10.rom'; l: $8000; p: $10000; crc: $B9A61E81), (n: 'b25-09.rom'; l: $8000; p: $18000; crc: $585411B7));
  wardner_mcu_rom: array [0 .. 7] of tipo_roms = ((n: '82s137.1d'; l: $400; p: 0; crc: $CC5B3F53), (n: '82s137.1e'; l: $400; p: $400; crc: $47351D55), (n: '82s137.3d'; l: $400; p: $800; crc: $70B537B9), (n: '82s137.3e'; l: $400; p: $C00; crc: $6EDB2DE8), (n: '82s131.3b'; l: $200;
    p: $1000; crc: $9DFFFAFF), (n: '82s131.3a'; l: $200; p: $1200; crc: $712BAD47), (n: '82s131.2a'; l: $200; p: $1400; crc: $AC843CA6), (n: '82s131.1a'; l: $200; p: $1600; crc: $50452FF8));
  wardner_dip_a: array [0 .. 5] of def_dip2 = ((mask: $1; name: 'Cabinet'; number: 2; val2: (1, 0); name2: ('Upright', 'Cocktail')), (mask: $2; name: 'Flip Screen'; number: 2; val2: (0, 2); name2: ('Off', 'On')), (mask: $8; name: 'Demo Sounds'; number: 2; val2: (8, 0);
    name2: ('Off', 'On')), (mask: $30; name: 'Coin A'; number: 4; val4: ($30, $20, $10, 0); name4: ('4C 1C', '3C 1C', '2C 1C', '1C 1C')), (mask: $C0; name: 'Coin B'; number: 4; val4: (0, $40, $80, $C0); name4: ('1C 2C', '1C 3C', '1C 4C', '1C 6C')), ());
  wardner_dip_b: array [0 .. 3] of def_dip2 = ((mask: $3; name: 'Difficulty'; number: 4; val4: (1, 0, 2, 3); name4: ('Easy', 'Normal', 'Hard', 'Very Hard')), (mask: $C; name: 'Bonus Life'; number: 4; val4: (0, 4, 8, $C); name4: ('30K 80K 50K+', '50K 100K 50K+', '30K', '50K')),
    (mask: $30; name: 'Lives'; number: 4; val4: ($30, 0, $10, $20); name4: ('1', '3', '4', '5')), ());

var
  mem_rom: array [0 .. 7, 0 .. $7FFF] of byte;
  rom_bank: byte;
  int_enable, wardner_dsp_bio, dsp_execute, video_ena: boolean;
  txt_ram: array [0 .. $7FF] of word;
  bg_ram: array [0 .. $1FFF] of word;
  fg_ram: array [0 .. $FFF] of word;
  txt_offs, bg_offs, fg_offs, bg_bank, fg_bank, main_ram_seg, dsp_addr_w: word;
  txt_scroll_x, txt_scroll_y, bg_scroll_x, bg_scroll_y, fg_scroll_x, fg_scroll_y: word;

procedure update_video_wardner;
  procedure draw_sprites(priority: word);
  var
    f, nchar, atrib, x, y: word;
    flipx, flipy: boolean;
    color: byte;
  begin
    for f := 0 to $1FF do
    begin
      atrib := memory[$8002 + (f shl 3)] + (memory[$8003 + (f shl 3)] shl 8);
      if ((atrib and $C00) = priority) then
      begin
        y := (memory[$8006 + (f shl 3)] + (memory[$8007 + (f shl 3)] shl 8)) shr 7;
        if (y and $1FF) > $100 then
          continue;
        nchar := (memory[$8000 + (f shl 3)] + (memory[$8001 + (f shl 3)] shl 8)) and $7FF;
        color := atrib and $3F;
        x := (memory[$8004 + (f shl 3)] + (memory[$8005 + (f shl 3)] shl 8)) shr 7;
        flipx := (atrib and $100) <> 0;
        if flipx then
          x := x - 14; // should really be 15
        flipy := (atrib and $200) <> 0;
        put_gfx_sprite(nchar, color shl 4, flipx, flipy, 3);
        update_gfx_sprite(x - 32, y - 16, 4, 3);
      end;
    end;
  end;

var
  f, nchar, x, y, atrib: word;
  color: byte;
begin
  if video_ena then
  begin
    for f := $7FF downto 0 do
    begin
      // Chars
      atrib := txt_ram[f];
      color := (atrib and $F800) shr 11;
      if (gfx[0].buffer[f] or buffer_color[color]) then
      begin
        x := (f and $3F) shl 3;
        y := (f shr 6) shl 3;
        nchar := atrib and $7FF;
        put_gfx_trans(x, y, nchar, (color shl 3) + $600, 1, 0);
        gfx[0].buffer[f] := false;
      end;
    end;
    for f := 0 to $FFF do
    begin
      atrib := bg_ram[f + bg_bank];
      color := (atrib and $F000) shr 12;
      if (gfx[2].buffer[f + bg_bank] or buffer_color[color + $30]) then
      begin
        // background
        x := (f and $3F) shl 3;
        y := (f shr 6) shl 3;
        nchar := atrib and $FFF;
        put_gfx(x, y, nchar, (color shl 4) + $400, 3, 2);
        gfx[2].buffer[f + bg_bank] := false;
      end;
      atrib := fg_ram[f];
      color := (atrib and $F000) shr 12;
      if (gfx[1].buffer[f] or buffer_color[color + $20]) then
      begin
        // foreground
        x := (f and $3F) shl 3;
        y := (f shr 6) shl 3;
        nchar := (atrib and $FFF) + fg_bank;
        put_gfx_trans(x, y, nchar and $FFF, (color shl 4) + $500, 2, 1);
        gfx[1].buffer[f] := false;
      end;
    end;
    scroll_x_y(3, 4, bg_scroll_x + 55, bg_scroll_y + 30);
    draw_sprites($400);
    scroll_x_y(2, 4, fg_scroll_x + 55, fg_scroll_y + 30);
    draw_sprites($800);
    scroll_x_y(1, 4, 512 - txt_scroll_x - 55, 256 - txt_scroll_y - 30);
    draw_sprites($C00);
  end
  else
    fill_full_screen(4, $800);
  update_final_piece(0, 0, 320, 240, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_wardner;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
  end;
end;

procedure wardnerhw_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 285 do
      begin
        case f of
          0:
            marcade.in0 := marcade.in0 and $7F;
          240:
            begin
              marcade.in0 := marcade.in0 or $80;
              if int_enable then
              begin
                z80_0.change_irq(HOLD_LINE);
                int_enable := false;
              end;
              update_video_wardner;
            end;
        end;
        // MAIN CPU
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // SND CPU
        z80_1.run(frame_snd);
        frame_snd := frame_snd + z80_1.tframes - z80_1.contador;
        // MCU
        tms32010_0.run(frame_mcu);
        frame_mcu := frame_mcu + tms32010_0.tframes - tms32010_0.contador;
      end;
      events_wardner;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function wardner_dsp_r: word;
begin
  // DSP can read data from main CPU RAM via DSP IO port 1
  case main_ram_seg of
    $7000, $8000, $A000:
      wardner_dsp_r := memory[main_ram_seg + (dsp_addr_w + 0)] or (memory[main_ram_seg + (dsp_addr_w + 1)] shl 8);
  else
    wardner_dsp_r := 0;
  end;
end;

procedure wardner_dsp_w(valor: word);
begin
  // Data written to main CPU RAM via DSP IO port 1
  dsp_execute := false;
  case main_ram_seg of
    $7000:
      begin
        if ((dsp_addr_w < 3) and (valor = 0)) then
          dsp_execute := true;
        memory[main_ram_seg + dsp_addr_w] := valor and $FF;
        memory[main_ram_seg + (dsp_addr_w + 1)] := (valor shr 8) and $FF;
      end;
    $8000, $A000:
      begin
        memory[main_ram_seg + dsp_addr_w] := valor and $FF;
        memory[main_ram_seg + (dsp_addr_w + 1)] := (valor shr 8) and $FF;
      end;
  end;
end;

procedure wardner_dsp_addrsel_w(valor: word);
begin
  main_ram_seg := valor and $E000;
  dsp_addr_w := (valor and $7FF) shl 1;
  if (main_ram_seg = $6000) then
    main_ram_seg := $7000;
end;

procedure wardner_dsp_bio_w(valor: word);
begin
  if (valor and $8000) <> 0 then
    wardner_dsp_bio := false;
  if (valor = 0) then
  begin
    if dsp_execute then
    begin
      z80_0.change_halt(CLEAR_LINE);
      dsp_execute := false;
    end;
    wardner_dsp_bio := true;
  end;
end;

function wardner_bio_r: boolean;
begin
  wardner_bio_r := wardner_dsp_bio;
end;

function wardner_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $807F, $C800 .. $CFFF:
      wardner_snd_getbyte := mem_snd[direccion];
    $C000 .. $C7FF:
      wardner_snd_getbyte := memory[direccion];
  end;
end;

procedure wardner_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $807F, $C800 .. $CFFF:
      mem_snd[direccion] := valor;
    $C000 .. $C7FF:
      memory[direccion] := valor;
  end;
end;

function wardner_snd_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 0 then
    wardner_snd_inbyte := ym3812_0.status;
end;

procedure wardner_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ym3812_0.control(valor);
    1:
      ym3812_0.write(valor);
  end;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_1.change_irq(irqstate);
end;

function wardner_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      wardner_getbyte := memory[direccion];
    $8000 .. $FFFF:
      if rom_bank = 0 then
      begin
        case (direccion and $7FFF) of
          0 .. $1FFF, $3000 .. $7FFF:
            wardner_getbyte := memory[direccion];
          $2000 .. $2FFF:
            wardner_getbyte := buffer_paleta[direccion and $FFF];
        end;
      end
      else
        wardner_getbyte := mem_rom[rom_bank, direccion and $7FFF];
  end;
end;

procedure wardner_putbyte(direccion: word; valor: byte);
  procedure change_color(numero: word);
  var
    tmp_color: word;
    color: tcolor;
  begin
    tmp_color := (buffer_paleta[numero + 1] shl 8) + buffer_paleta[numero];
    color.b := pal5bit(tmp_color shr 10);
    color.g := pal5bit(tmp_color shr 5);
    color.r := pal5bit(tmp_color);
    numero := numero shr 1;
    set_pal_color(color, numero);
    case numero of
      $400 .. $4FF:
        buffer_color[((numero shr 4) and $F) + $30] := true;
      $500 .. $5FF:
        buffer_color[((numero shr 4) and $F) + $20] := true;
      $600 .. $6FF:
        buffer_color[(numero shr 3) and $1F] := true;
    end;
  end;

begin
  case direccion of
    0 .. $6FFF:
      ;
    $7000 .. $7FFF:
      memory[direccion] := valor;
    $8000 .. $FFFF:
      if rom_bank = 0 then
      begin
        case (direccion and $7FFF) of
          0 .. $FFF, $4000 .. $47FF:
            memory[direccion] := valor;
          $1000 .. $1FFF, $3000 .. $3FFF, $4800 .. $7FFF:
            ;
          $2000 .. $2FFF:
            if buffer_paleta[direccion and $FFF] <> valor then
            begin
              buffer_paleta[direccion and $FFF] := valor;
              change_color(direccion and $FFE);
            end;
        end;
      end;
  end;
end;

function wardner_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $50:
      wardner_inbyte := marcade.dswa;
    $52:
      wardner_inbyte := marcade.dswb;
    $54:
      wardner_inbyte := marcade.in1;
    $56:
      wardner_inbyte := marcade.in2;
    $58:
      wardner_inbyte := marcade.in0;
    $60:
      wardner_inbyte := txt_ram[txt_offs] and $FF;
    $61:
      wardner_inbyte := txt_ram[txt_offs] shr 8;
    $62:
      wardner_inbyte := bg_ram[bg_offs + bg_bank] and $FF;
    $63:
      wardner_inbyte := bg_ram[bg_offs + bg_bank] shr 8;
    $64:
      wardner_inbyte := fg_ram[fg_offs] and $FF;
    $65:
      wardner_inbyte := fg_ram[fg_offs] shr 8;
  end;
end;

procedure wardner_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $10:
      txt_scroll_x := (txt_scroll_x and $FF00) or valor;
    $11:
      txt_scroll_x := (txt_scroll_x and $FF) or ((valor and 1) shl 8);
    $12:
      txt_scroll_y := (txt_scroll_y and $FF00) or valor;
    $13:
      txt_scroll_y := (txt_scroll_y and $FF) or ((valor and 1) shl 8);
    $14:
      txt_offs := (txt_offs and $FF00) or valor;
    $15:
      txt_offs := (txt_offs and $FF) or ((valor and 7) shl 8);
    $20:
      bg_scroll_x := (bg_scroll_x and $FF00) or valor;
    $21:
      bg_scroll_x := (bg_scroll_x and $FF) or ((valor and 1) shl 8);
    $22:
      bg_scroll_y := (bg_scroll_y and $FF00) or valor;
    $23:
      bg_scroll_y := (bg_scroll_y and $FF) or ((valor and 1) shl 8);
    $24:
      bg_offs := (bg_offs and $FF00) or valor;
    $25:
      bg_offs := (bg_offs and $FF) or ((valor and $F) shl 8);
    $30:
      fg_scroll_x := (fg_scroll_x and $FF00) or valor;
    $31:
      fg_scroll_x := (fg_scroll_x and $FF) or ((valor and 1) shl 8);
    $32:
      fg_scroll_y := (fg_scroll_y and $FF00) or valor;
    $33:
      fg_scroll_y := (fg_scroll_y and $FF) or ((valor and 1) shl 8);
    $34:
      fg_offs := (fg_offs and $FF00) or valor;
    $35:
      fg_offs := (fg_offs and $FF) or ((valor and $F) shl 8);
    $5A:
      case (valor and $F) of
        0:
          begin
            tms32010_0.change_halt(CLEAR_LINE);
            z80_0.change_halt(ASSERT_LINE);
            tms32010_0.change_irq(ASSERT_LINE);
          end;
        1:
          begin
            tms32010_0.change_irq(CLEAR_LINE);
            tms32010_0.change_halt(ASSERT_LINE);
          end;
      end;
    $5C:
      case (valor and $F) of
        4:
          int_enable := false;
        5:
          int_enable := true;
        6:
          main_screen.flip_main_screen := false;
        7:
          main_screen.flip_main_screen := true;
        8:
          bg_bank := 0;
        9:
          bg_bank := $1000;
        $A:
          fg_bank := 0;
        $B:
          fg_bank := $1000;
        $C:
          video_ena := false;
        $D:
          begin
            if not(video_ena) then
            begin
              fillchar(gfx[0].buffer, $800, 1);
              fillchar(gfx[1].buffer, $1000, 1);
              fillchar(gfx[2].buffer, $2000, 1);
            end;
            video_ena := true;
          end;
      end;
    $60:
      if (txt_ram[txt_offs] and $FF) <> valor then
      begin
        txt_ram[txt_offs] := (txt_ram[txt_offs] and $FF00) or valor;
        gfx[0].buffer[txt_offs] := true;
      end;
    $61:
      if (txt_ram[txt_offs] and $FF00) <> (valor shl 8) then
      begin
        txt_ram[txt_offs] := (txt_ram[txt_offs] and $FF) or (valor shl 8);
        gfx[0].buffer[txt_offs] := true;
      end;
    $62:
      if (bg_ram[bg_offs + bg_bank] and $FF) <> valor then
      begin
        bg_ram[bg_offs + bg_bank] := (bg_ram[bg_offs + bg_bank] and $FF00) or valor;
        gfx[2].buffer[bg_offs + bg_bank] := true;
      end;
    $63:
      if (bg_ram[bg_offs + bg_bank] and $FF00) <> (valor shl 8) then
      begin
        bg_ram[bg_offs + bg_bank] := (bg_ram[bg_offs + bg_bank] and $FF) or (valor shl 8);
        gfx[2].buffer[bg_offs + bg_bank] := true;
      end;
    $64:
      if (fg_ram[fg_offs] and $FF) <> valor then
      begin
        fg_ram[fg_offs] := (fg_ram[fg_offs] and $FF00) or valor;
        gfx[1].buffer[fg_offs] := true;
      end;
    $65:
      if (fg_ram[fg_offs] and $FF00) <> (valor shl 8) then
      begin
        fg_ram[fg_offs] := (fg_ram[fg_offs] and $FF) or (valor shl 8);
        gfx[1].buffer[fg_offs] := true;
      end;
    $70:
      rom_bank := valor;
  end;
end;

procedure wardner_sound_update;
begin
  ym3812_0.update;
end;

// Main
procedure reset_wardnerhw;
begin
  z80_0.reset;
  z80_1.reset;
  tms32010_0.reset;
  frame_main := z80_0.tframes;
  frame_snd := z80_1.tframes;
  frame_mcu := tms32010_0.tframes;
  ym3812_0.reset;
 reset_video;
  reset_audio;
  txt_scroll_x := 0;
  txt_scroll_y := 0;
  bg_scroll_x := 0;
  bg_scroll_y := 0;
  fg_scroll_x := 0;
  fg_scroll_y := 0;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  rom_bank := 0;
  txt_offs := 0;
  bg_offs := 0;
  fg_offs := 0;
  bg_bank := 0;
  fg_bank := 0;
  int_enable := false;
  video_ena := true;
  wardner_dsp_bio := false;
  dsp_execute := false;
  main_ram_seg := 0;
  dsp_addr_w := 0;
end;

function start_wardner: boolean;
var
  f: word;
  memory_temp: array [0 .. $3FFFF] of byte;
  rom: array [0 .. $FFF] of word;
const
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
begin
  machine_calls.general_loop := wardnerhw_loop;
  machine_calls.reset := reset_wardnerhw;
  machine_calls.fps_max := (14000000 / 2) / (446 * 286);
  start_wardner := false;
  start_audio(false);
  screen_init(1, 512, 256, true);
  screen_mod_scroll(1, 512, 512, 511, 256, 256, 255);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 256, 511);
  screen_init(3, 512, 512);
  screen_mod_scroll(3, 512, 512, 511, 512, 256, 511);
  screen_init(4, 512, 512, false, true);
  start_video(320, 240);
  // Main CPU
  z80_0 := cpu_z80.create(24000000 div 4, 286);
  z80_0.change_ram_calls(wardner_getbyte, wardner_putbyte);
  z80_0.change_io_calls(wardner_inbyte, wardner_outbyte);
  if not(roms_load(@memory_temp, wardner_rom)) then
    exit;
  copymemory(@memory, @memory_temp, $8000);
  for f := 0 to 3 do
    copymemory(@mem_rom[f + 2, 0], @memory_temp[$8000 + (f * $8000)], $8000);
  copymemory(@mem_rom[7, 0], @memory_temp[$28000], $8000);
  // Sound CPU
  z80_1 := cpu_z80.create(14000000 div 4, 286);
  z80_1.change_ram_calls(wardner_snd_getbyte, wardner_snd_putbyte);
  z80_1.change_io_calls(wardner_snd_inbyte, wardner_snd_outbyte);
  z80_1.init_sound(wardner_sound_update);
  if not(roms_load(@mem_snd, wardner_snd_rom)) then
    exit;
  // MCU CPU
  tms32010_0 := cpu_tms32010.create(14000000, 286);
  tms32010_0.change_io_calls(wardner_bio_r, nil, wardner_dsp_r, nil, nil, nil, nil, nil, nil, wardner_dsp_addrsel_w, wardner_dsp_w, nil, wardner_dsp_bio_w, nil, nil, nil, nil);
  if not(roms_load(@memory_temp, wardner_mcu_rom)) then
    exit;
  for f := 0 to $3FF do
    rom[f] := (((memory_temp[f] and $F) shl 4 + (memory_temp[f + $400] and $F)) shl 8) or (memory_temp[f + $800] and $F) shl 4 + (memory_temp[f + $C00] and $F);
  for f := 0 to $1FF do
    // 1024-2047
    rom[f + $400] := (((memory_temp[f + $1000] and $F) shl 4 + (memory_temp[f + $1200] and $F)) shl 8) or (memory_temp[f + $1400] and $F) shl 4 + (memory_temp[f + $1600] and $F);
  copymemory(tms32010_0.get_rom_addr, @rom, $1000);
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3812_FM, 14000000 div 4);
  ym3812_0.change_irq_calls(snd_irq);
  // convertir chars
  if not(roms_load(@memory_temp, wardner_char)) then
    exit;
  init_gfx(0, 8, 8, 2048);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(3, 0, 8 * 8, 0 * 2048 * 8 * 8, 1 * 2048 * 8 * 8, 2 * 2048 * 8 * 8);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, false, false);
  // convertir tiles fg
  if not(roms_load(@memory_temp, wardner_fg_tiles)) then
    exit;
  init_gfx(1, 8, 8, 4096);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 8 * 8, 0 * 4096 * 8 * 8, 1 * 4096 * 8 * 8, 2 * 4096 * 8 * 8, 3 * 4096 * 8 * 8);
  convert_gfx(1, 0, @memory_temp, @ps_x, @pc_y, false, false);
  // convertir tiles bg
  if not(roms_load(@memory_temp, wardner_bg_tiles)) then
    exit;
  init_gfx(2, 8, 8, 4096);
  convert_gfx(2, 0, @memory_temp, @ps_x, @pc_y, false, false);
  // convertir tiles sprites
  if not(roms_load(@memory_temp, wardner_sprites)) then
    exit;
  init_gfx(3, 16, 16, 2048);
  gfx[3].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0 * 2048 * 32 * 8, 1 * 2048 * 32 * 8, 2 * 2048 * 32 * 8, 3 * 2048 * 32 * 8);
  convert_gfx(3, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // DIP
  marcade.dswa := 1;
  marcade.dswb := 0;
  marcade.dswa_val2 := @wardner_dip_a;
  marcade.dswb_val2 := @wardner_dip_b;
  // final
  reset_wardnerhw;
  start_wardner := true;
end;

end.
