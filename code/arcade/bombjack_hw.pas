unit bombjack_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  qsnapshot,
  sega_decrypt_2;

function start_bombjack: boolean;

implementation

const
  bombjack_rom: array [0 .. 4] of tipo_roms = ((n: '09_j01b.bin'; l: $2000; p: 0; crc: $C668DC30), (n: '10_l01b.bin'; l: $2000; p: $2000; crc: $52A1E5FB), (n: '11_m01b.bin'; l: $2000; p: $4000; crc: $B68A062A), (n: '12_n01b.bin'; l: $2000; p: $6000; crc: $1D3ECEE5), (n: '13.1r';
    l: $2000; p: $C000; crc: $70E0244D));
  bombjack_char: array [0 .. 2] of tipo_roms = ((n: '03_e08t.bin'; l: $1000; p: 0; crc: $9F0470D5), (n: '04_h08t.bin'; l: $1000; p: $1000; crc: $81EC12E6), (n: '05_k08t.bin'; l: $1000; p: $2000; crc: $E87EC8B1));
  bombjack_tiles: array [0 .. 2] of tipo_roms = ((n: '06_l08t.bin'; l: $2000; p: 0; crc: $51EEBD89), (n: '07_n08t.bin'; l: $2000; p: $2000; crc: $9DD98E9D), (n: '08_r08t.bin'; l: $2000; p: $4000; crc: $3155EE7D));
  bombjack_sprites: array [0 .. 2] of tipo_roms = ((n: '16_m07b.bin'; l: $2000; p: 0; crc: $94694097), (n: '15_l07b.bin'; l: $2000; p: $2000; crc: $013F58F2), (n: '14_j07b.bin'; l: $2000; p: $4000; crc: $101C858D));
  bombjack_tiles_map: tipo_roms = (n: '02_p04t.bin'; l: $1000; p: 0; crc: $398D4A02);
  bombjack_sonido: tipo_roms = (n: '01_h03t.bin'; l: $2000; p: 0; crc: $8407917D);
  bombjack_dipa: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Coin A'; number: 4; val4: (0, 1, 2, 3); name4: ('1C 1C', '1C 2C', '1C 3C', '1C 6C')), (mask: $C; name: 'Coin B'; number: 4; val4: (4, 0, 8, $C); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $30;
    name: 'Lives'; number: 4; val4: ($30, 0, $10, $20); name4: ('2', '3', '4', '5')), (mask: $40; name: 'Cabinet'; number: 2; val2: ($40, 0); name2: ('Upright', 'Cocktail')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: (0, $80); name2: ('Off', 'On')), ());
  bombjack_dipb: array [0 .. 4] of def_dip2 = ((mask: $7; name: 'Bonus Life'; number: 8; val8: (2, 1, 7, 5, 3, 6, 4, 0); name8: ('30K+', '100K+', '50K 100K 300K', '50K 100K', '50K', '100K 300K', '100K', 'None')), (mask: $18; name: 'Bird Speed'; number: 4; val4: (0, 8, $10, $18);
    name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: $60; name: 'Enemies Number && Speed'; number: 4; val4: ($20, 0, $40, $60); name4: ('Easy', 'Medium', 'Hard', 'Hardest')), (mask: $80; name: 'Special Coin'; number: 2; val2: (0, $80); name2: ('Easy', 'Hard')), ());
  // Calorie Kun
  caloriekun_rom: array [0 .. 2] of tipo_roms = ((n: 'epr10072.1j'; l: $4000; p: 0; crc: $ADE792C1), (n: 'epr10073.1k'; l: $4000; p: $4000; crc: $B53E109F), (n: 'epr10074.1m'; l: $4000; p: $8000; crc: $A08DA685));
  caloriekun_sonido: tipo_roms = (n: 'epr10075.4d'; l: $4000; p: 0; crc: $CA547036);
  caloriekun_char: array [0 .. 2] of tipo_roms = ((n: 'epr10082.5r'; l: $2000; p: 0; crc: $5984EA44), (n: 'epr10081.4r'; l: $2000; p: $2000; crc: $E2D45DD8), (n: 'epr10080.3r'; l: $2000; p: $4000; crc: $42EDFCFE));
  caloriekun_tiles: array [0 .. 2] of tipo_roms = ((n: 'epr10078.7d'; l: $4000; p: 0; crc: $5B8EECCE), (n: 'epr10077.6d'; l: $4000; p: $4000; crc: $01BCB609), (n: 'epr10076.5d'; l: $4000; p: $8000; crc: $B1529782));
  caloriekun_sprites: array [0 .. 2] of tipo_roms = ((n: 'epr10071.7m'; l: $4000; p: 0; crc: $5F55527A), (n: 'epr10070.7k'; l: $4000; p: $4000; crc: $97F35A23), (n: 'epr10069.7j'; l: $4000; p: $8000; crc: $C0C3DEAF));
  caloriekun_tiles_map: tipo_roms = (n: 'epr10079.8d'; l: $2000; p: 0; crc: $3C61A42C);
  caloriekun_dipa: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Coin A'; number: 4; val4: (0, 1, 2, 3); name4: ('1C 1C', '1C 2C', '1C 3C', '1C 6C')), (mask: $C; name: 'Coin B'; number: 4; val4: ($C, 0, 4, 8); name4: ('2C 1C', '1C 1C', '1C 2C', '1C 3C')), (mask: $10;
    name: 'Cabinet'; number: 2; val2: ($10, 0); name2: ('Upright', 'Cocktail')), (mask: $20; name: 'Demo Sounds'; number: 2; val2: (0, $20); name2: ('Off', 'On')), (mask: $C0; name: 'Lives'; number: 4; val4: ($C0, 0, $40, $80); name4: ('2', '3', '4', '5')), ());
  caloriekun_dipb: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Bonus Life'; number: 4; val4: (0, 1, 3, 2); name4: ('None', '20K', '20K 60K', 'Invalid')), (mask: $4; name: 'Number of Bombs'; number: 2; val2: (0, 4); name2: ('3', '5')), (mask: $8;
    name: 'Difficulty - Mogura Nian'; number: 2; val2: (0, 8); name2: ('Normal', 'Hard')), (mask: $30; name: 'Difficulty - Select of Mogura'; number: 4; val4: (0, $20, $10, $30); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $80; name: 'Infinite Lives'; number: 2;
    val2: (0, $80); name2: ('Off', 'On')), ());

var
  memory_fondo: array [0 .. $1FFF] of byte;
  numero_fondo, sound_latch, mask_sprites: byte;
  fondo_activo, nmi_vblank, bombjack_video: boolean;
  memoria_sprites: array [0 .. $7F] of byte;
  memoria_screen: array [0 .. $7FF] of byte;
  memoria_ram: array [0 .. $FFF] of byte;
  actualiza_fondo: boolean;
  // Bomb Jack
  sprite_control: array [0 .. 1] of byte;
  // Calorie
  mem_dec: array [0 .. $7FFF] of byte;

procedure update_video_bombjack;
  procedure change_font(base: word);
  var
    x, y, color, f, atrib: byte;
    nchar: word;
  begin
    for f := 0 to $FF do
    begin
      atrib := memory_fondo[$100 + base + f];
      color := atrib and $F;
      if (gfx[1].buffer[f] or buffer_color[color]) then
      begin
        x := f mod 16;
        y := f div 16;
        nchar := memory_fondo[base + f] + ((atrib and $30) shl 4);
        put_gfx_flip(x * 16, y * 16, nchar, color shl 3, 1, 1, (atrib and $40) <> 0, false);
        gfx[1].buffer[f] := false;
      end;
    end;
  end;

var
  x, y, atrib: byte;
  f, nchar, color: word;
  large, rev: boolean;
begin
  if fondo_activo then
  begin
    if actualiza_fondo then
    begin
      change_font(numero_fondo * $200);
      actualiza_fondo := false;
    end;
    update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  end
  else
    fill_full_screen(3, 0);
  for f := 0 to $3FF do
  begin
    atrib := memoria_screen[$400 + f];
    color := atrib and $F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memoria_screen[f] + (atrib and $30) shl 4;
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 3, 2, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  for f := 31 downto 7 do
  begin
    x := memoria_sprites[3 + (f * 4)];
    y := 240 - memoria_sprites[2 + (f * 4)];
    nchar := memoria_sprites[0 + (f * 4)];
    atrib := memoria_sprites[1 + (f * 4)];
    color := (atrib and $F) shl 3;
    if bombjack_video then
    begin
      rev := sprite_control[0] > sprite_control[1];
      if rev then
        large := ((f shr 1) > sprite_control[1]) and ((f shr 1) <= sprite_control[0])
      else
        large := ((f shr 1) > sprite_control[0]) and ((f shr 1) <= sprite_control[1]);
      y := y + 1;
    end
    else
    begin
      large := (atrib and $10) <> 0;
    end;
    if not(large) then
    begin
      put_gfx_sprite(nchar, color, atrib and $40 <> 0, atrib and $80 <> 0, 2);
      update_gfx_sprite(x, y, 3, 2);
    end
    else
    begin
      if ((f and 1) <> 0) then
        continue;
      put_gfx_sprite(nchar and mask_sprites, color, atrib and $40 <> 0, atrib and $80 <> 0, 3);
      update_gfx_sprite(x, y - 16, 3, 3);
    end;
  end;
  update_final_piece(0, 16, 256, 224, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_bombjack;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // p2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure events_caloriekun;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    // p2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    // System
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or 4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or 8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure bombjack_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 239 then
        begin
          if nmi_vblank then
            z80_0.change_nmi(ASSERT_LINE);
          update_video_bombjack;
          z80_1.change_nmi(PULSE_LINE);
        end;
      end;
      events_bombjack;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure caloriekun_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s := z80_1.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_s);
        frame_s := frame_s + z80_1.tframes - z80_1.contador;
        if f = 239 then
        begin
          update_video_bombjack;
          z80_0.change_irq(HOLD_LINE);
          z80_1.change_irq(HOLD_LINE);
        end;
      end;
      events_caloriekun;
      video_sync;
    end
    else
      pause_action;
  end;
end;

procedure change_color(dir: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[dir];
  color.r := pal4bit(tmp_color);
  color.g := pal4bit(tmp_color shr 4);
  tmp_color := buffer_paleta[dir + 1];
  color.b := pal4bit(tmp_color);
  dir := dir shr 1;
  set_pal_color(color, dir);
  buffer_color[(dir shr 3) and $F] := true;
  actualiza_fondo := true;
end;

function gen_map_read(direccion: word): byte;
begin
  case direccion of
    $0 .. $FFF:
      gen_map_read := memoria_ram[direccion];
    $1000 .. $17FF:
      gen_map_read := memoria_screen[direccion and $7FF];
    $1C00 .. $1DFF:
      gen_map_read := buffer_paleta[direccion and $FF];
    $3000 .. $37FF:
      case (direccion and 7) of
        0:
          gen_map_read := marcade.in0;
        1:
          gen_map_read := marcade.in1;
        2:
          gen_map_read := marcade.in2;
        4:
          gen_map_read := marcade.dswa;
        5:
          gen_map_read := marcade.dswb;
      end;
  end;
end;

procedure gen_map_write(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $FFF:
      memoria_ram[direccion] := valor;
    $1000 .. $17FF:
      if memoria_screen[(direccion and $7FF)] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memoria_screen[(direccion and $7FF)] := valor;
      end;
    $1800 .. $19FF:
      memoria_sprites[direccion and $7F] := valor;
    $1C00 .. $1DFF:
      if buffer_paleta[direccion and $FF] <> valor then
      begin
        buffer_paleta[direccion and $FF] := valor;
        change_color(direccion and $FE);
      end;
    $1E00 .. $1FFF:
      begin
        fondo_activo := (valor and $10) <> 0;
        if (numero_fondo <> (valor and $F)) then
        begin
          numero_fondo := valor and $F;
          actualiza_fondo := true;
          fillchar(gfx[1].buffer, $100, 1);
        end;
      end;
    $3800 .. $3FFF:
      sound_latch := valor;
  end;
end;

function bombjack_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $DFFF:
      bombjack_getbyte := memory[direccion]; // ROM
    $8000 .. $97FF, $9C00 .. $9DFF, $B000 .. $B7FF:
      bombjack_getbyte := gen_map_read(direccion - $8000);
  end;
end;

procedure bombjack_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF, $C000 .. $DFFF:
      ; // ROM
    $8000 .. $99FF, $9C00 .. $9FFF, $B800 .. $BFFF:
      gen_map_write(direccion - $8000, valor);
    $9A00 .. $9BFF:
      sprite_control[direccion and 1] := valor and $F;
    $B000 .. $B7FF:
      case (direccion and 7) of
        0:
          begin
            nmi_vblank := valor <> 0;
            if not(nmi_vblank) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        3:
          ; // WD
        4:
          main_screen.flip_main_screen := (valor and 1) <> 0;
      end;
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case (direccion and $7FFF) of
    0 .. $1FFF:
      snd_getbyte := mem_snd[direccion];
    $4000 .. $5FFF:
      snd_getbyte := mem_snd[$4000 + (direccion and $7FF)];
    $6000 .. $7FFF:
      begin
        snd_getbyte := sound_latch;
        sound_latch := 0;
      end;
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case (direccion and $7FFF) of
    0 .. $1FFF:
      ; // ROM
    $4000 .. $5FFF:
      mem_snd[$4000 + (direccion and $7FF)] := valor;
  end;
end;

function snd_inbyte(puerto: word): byte;
begin
  case (puerto and $91) of
    $1:
      snd_inbyte := ay8910_0.read;
    $11:
      snd_inbyte := ay8910_1.read;
    $81:
      snd_inbyte := ay8910_2.read;
  end;
end;

procedure snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $91) of
    $0:
      ay8910_0.control(valor);
    $1:
      ay8910_0.write(valor);
    $10:
      ay8910_1.control(valor);
    $11:
      ay8910_1.write(valor);
    $80:
      ay8910_2.control(valor);
    $81:
      ay8910_2.write(valor);
  end
end;

procedure bombjack_update_sound;
begin
  ay8910_0.update;
  ay8910_1.update;
  ay8910_2.update;
end;

// Calorie kun
function caloriekun_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      if z80_0.opcode then
        caloriekun_getbyte := mem_dec[direccion]
      else
        caloriekun_getbyte := memory[direccion];
    $8000 .. $BFFF:
      caloriekun_getbyte := memory[direccion]; // ROM
    $C000 .. $FFFF:
      caloriekun_getbyte := gen_map_read(direccion - $C000);
  end;
end;

procedure caloriekun_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $FFFF:
      gen_map_write(direccion - $C000, valor);
  end;
end;

function calorie_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      calorie_snd_getbyte := mem_snd[direccion and $3FFF];
    $8000 .. $BFFF:
      calorie_snd_getbyte := mem_snd[$8000 + (direccion and $7FF)];
    $C000 .. $FFFF:
      begin
        calorie_snd_getbyte := sound_latch;
        sound_latch := 0;
      end;
  end;
end;

procedure calorie_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $BFFF:
      mem_snd[$8000 + (direccion and $7FF)] := valor;
  end;
end;

procedure bombjack_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    13:
      open_qsnapshot_save('bombjack' + nombre);
    383:
      open_qsnapshot_save('caloriekun' + nombre);
  end;
  getmem(data, 200);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ay8910_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := ay8910_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := ay8910_2.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[$8000], $8000);
  savedata_qsnapshot(@mem_snd[$2000], $E000);
  savedata_qsnapshot(@memoria_sprites[0], $80);
  savedata_qsnapshot(@memoria_screen[0], $800);
  savedata_qsnapshot(@memoria_ram[0], $1000);
  // MISC
  buffer[0] := numero_fondo;
  buffer[1] := sound_latch;
  buffer[2] := byte(fondo_activo);
  buffer[3] := byte(nmi_vblank);
  buffer[4] := sprite_control[0];
  buffer[5] := sprite_control[1];
  savedata_qsnapshot(@buffer, 6);
  savedata_qsnapshot(@buffer_paleta, $100 * 2);
  freemem(data);
  close_qsnapshot;
end;

procedure bombjack_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
  f: byte;
begin
  case main_vars.machine_type of
    13:
      if not(open_qsnapshot_load('bombjack' + nombre)) then
        exit;
    383:
      if not(open_qsnapshot_load('caloriekun' + nombre)) then
        exit;
  end;
  getmem(data, 200);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  z80_1.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ay8910_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  ay8910_1.load_snapshot(data);
  loaddata_qsnapshot(data);
  ay8910_2.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory[$8000]);
  loaddata_qsnapshot(@mem_snd[$2000]);
  loaddata_qsnapshot(@memoria_sprites[0]);
  loaddata_qsnapshot(@memoria_screen[0]);
  loaddata_qsnapshot(@memoria_ram[0]);
  // MISC
  loaddata_qsnapshot(@buffer[0]);
  numero_fondo := buffer[0];
  sound_latch := buffer[1];
  fondo_activo := buffer[2] <> 0;
  nmi_vblank := buffer[3] <> 0;
  sprite_control[0] := buffer[4];
  sprite_control[1] := buffer[5];
  loaddata_qsnapshot(@buffer_paleta);
  freemem(data);
  close_qsnapshot;
  for f := 0 to $7F do
    change_color(f * 2);
  actualiza_fondo := true;
end;

// Main
procedure bombjack_reset;
begin
  z80_0.reset;
  z80_1.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  ay8910_2.reset;
  reset_video;
  reset_audio;
  nmi_vblank := false;
  fondo_activo := false;
  sound_latch := 0;
  numero_fondo := $FF;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := $F0;
  sprite_control[0] := 0;
  sprite_control[1] := 0;
end;

function start_bombjack: boolean;
const
  pt_x: array [0 .. 31] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7, 32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 32 * 8 + 4, 32 * 8 + 5, 32 * 8 + 6, 32 * 8 + 7, 40 * 8 + 0, 40 * 8 + 1, 40 * 8 + 2,
    40 * 8 + 3, 40 * 8 + 4, 40 * 8 + 5, 40 * 8 + 6, 40 * 8 + 7);
  pt_y: array [0 .. 31] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8, 64 * 8, 65 * 8, 66 * 8, 67 * 8, 68 * 8, 69 * 8, 70 * 8, 71 * 8, 80 * 8, 81 * 8, 82 * 8, 83 * 8, 84 * 8, 85 * 8,
    86 * 8, 87 * 8);
var
  memoria_temp: array [0 .. $FFFF] of byte;
  procedure convert_chars(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(3, 0, 8 * 8, 0 * 8, num * 8 * 8, num * 8 * 2 * 8);
    convert_gfx(0, 0, @memoria_temp, @pt_x, @pt_y, false, false);
  end;
  procedure convert_tiles(num: word);
  begin
    init_gfx(1, 16, 16, num);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(3, 0, 32 * 8, 0, $20 * num * 8, 2 * $20 * num * 8);
    convert_gfx(1, 0, @memoria_temp, @pt_x, @pt_y, false, false);
  end;
  procedure convert_sprites(num: word);
  begin
    init_gfx(2, 16, 16, num);
    gfx[2].trans[0] := true;
    gfx_set_desc_data(3, 0, 32 * 8, 0, $20 * num * 8, 2 * $20 * num * 8);
    convert_gfx(2, 0, @memoria_temp, @pt_x, @pt_y, false, false);
    num := num shr 2;
    init_gfx(3, 32, 32, num);
    gfx[3].trans[0] := true;
    gfx_set_desc_data(3, 0, 128 * 8, 0 * 8, $80 * num * 8, 2 * $80 * num * 8);
    convert_gfx(3, 0, @memoria_temp[$40 * num], @pt_x, @pt_y, false, false);
  end;

begin
  machine_calls.reset := bombjack_reset;
  machine_calls.save_qsnap := bombjack_qsave;
  machine_calls.load_qsnap := bombjack_qload;
  start_bombjack := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  if main_vars.machine_type = 13 then
    main_screen.rot90_screen := true;
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 264);
  // Sound CPU
  z80_1 := cpu_z80.create(3000000, 264);
  z80_1.change_io_calls(snd_inbyte, snd_outbyte);
  z80_1.init_sound(bombjack_update_sound);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(1500000, AY8910, 1);
  ay8910_1 := ay8910_chip.create(1500000, AY8910, 1);
  ay8910_2 := ay8910_chip.create(1500000, AY8910, 1);
  case main_vars.machine_type of
    13:
      begin // Bomb Jack
        machine_calls.general_loop := bombjack_loop;
        bombjack_video := true;
        // Main
        if not(roms_load(@memory, bombjack_rom)) then
          exit;
        z80_0.change_ram_calls(bombjack_getbyte, bombjack_putbyte);
        // Snd
        if not(roms_load(@mem_snd, bombjack_sonido)) then
          exit;
        z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
        // chars
        if not(roms_load(@memoria_temp, bombjack_char)) then
          exit;
        convert_chars($200);
        // tiles
        if not(roms_load(@memory_fondo, bombjack_tiles_map)) then
          exit;
        if not(roms_load(@memoria_temp, bombjack_tiles)) then
          exit;
        convert_tiles($100);
        // sprites
        if not(roms_load(@memoria_temp, bombjack_sprites)) then
          exit;
        convert_sprites($100);
        mask_sprites := $1F;
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val2 := @bombjack_dipa;
        marcade.dswb := $50;
        marcade.dswb_val2 := @bombjack_dipb;
      end;
    383:
      begin
        machine_calls.general_loop := caloriekun_loop;
        bombjack_video := false;
        if not(roms_load(@memory, caloriekun_rom)) then
          exit;
        z80_0.change_ram_calls(caloriekun_getbyte, caloriekun_putbyte);
        decode_sega_type2(@memory, @mem_dec, S317_000X, 0);
        // Snd
        if not(roms_load(@mem_snd, caloriekun_sonido)) then
          exit;
        z80_1.change_ram_calls(calorie_snd_getbyte, calorie_snd_putbyte);
        // convertir chars
        if not(roms_load(@memoria_temp, caloriekun_char)) then
          exit;
        convert_chars($400);
        // convertir tiles
        if not(roms_load(@memory_fondo, caloriekun_tiles_map)) then
          exit;
        if not(roms_load(@memoria_temp, caloriekun_tiles)) then
          exit;
        convert_tiles($200);
        // sprites
        if not(roms_load(@memoria_temp, caloriekun_sprites)) then
          exit;
        convert_sprites($200);
        mask_sprites := $3F;
        // DIP
        marcade.dswa := $30;
        marcade.dswa_val2 := @caloriekun_dipa;
        marcade.dswb := 0;
        marcade.dswb_val2 := @caloriekun_dipb;
      end;
  end;
  // final
  bombjack_reset;
  start_bombjack := true;
end;

end.
