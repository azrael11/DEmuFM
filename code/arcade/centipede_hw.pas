unit centipede_hw;

interface

uses
  WinApi.Windows,
  m6502,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  file_engine,
  pokey;

function start_centipede: boolean;

implementation

uses
  uDataModule;

const
  centipede_rom: array [0 .. 3] of tipo_roms = ((n: '136001-407.d1'; l: $800; p: $2000; crc: $C4D995EB), (n: '136001-408.e1'; l: $800; p: $2800; crc: $BCDEBE1B), (n: '136001-409.fh1'; l: $800; p: $3000; crc: $66D7B04A), (n: '136001-410.j1'; l: $800; p: $3800; crc: $33CE4640));
  centipede_chars: array [0 .. 1] of tipo_roms = ((n: '136001-211.f7'; l: $800; p: 0; crc: $880ACFB9), (n: '136001-212.hj7'; l: $800; p: $800; crc: $B1397029));
  centipede_dip_a: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Language'; number: 4; val4: (0, 1, 2, 3); name4: ('English', 'German', 'French', 'Spanish')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C); name4: ('2', '3', '4', '5')), (mask: $30; name: 'Bonus Life';
    number: 4; val4: (0, $10, $20, $30); name4: ('10K', '12K', '15K', '20K')), (mask: $40; name: 'Difficulty'; number: 2; val2: ($40, 0); name2: ('Easy', 'Hard')), (mask: $80; name: 'Credit Minimum'; number: 2; val2: (0, $80); name2: ('1', '2')), ());
  centipede_dip_b: array [0 .. 3] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 2, 1, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $1C; name: 'Game Time'; number: 8; val8: (0, 4, 8, $C, $10, $14, $18, $1C);
    name8: ('Untimed', '1 Minute', '2 Minutes', '3 Minutes', '4 Minutes', '5 Minutes', '6 Minutes', '7 Minutes')), (mask: $E0; name: 'Bonus Coin'; number: 8; val8: (0, $20, $40, $60, $80, $A0, $C0, $E0);
    name8: ('None', '3C 2C', '5C 4C', '6C 4C', '6C 5C', '4C 3C', 'Invalid', 'Invalid')), ());
  centipede_dip_c: array [0 .. 1] of def_dip2 = ((mask: $10; name: 'Cabinet'; number: 2; val2: (0, $10); name2: ('Upright', 'Cocktail')), ());
  milliped_rom: array [0 .. 3] of tipo_roms = ((n: '136013-104.mn1'; l: $1000; p: $4000; crc: $40711675), (n: '136013-103.l1'; l: $1000; p: $5000; crc: $FB01BAF2), (n: '136013-102.jk1'; l: $1000; p: $6000; crc: $62E137E0), (n: '136013-101.h1'; l: $1000; p: $7000;
    crc: $46752C7D));
  milliped_chars: array [0 .. 1] of tipo_roms = ((n: '136013-107.r5'; l: $800; p: 0; crc: $68C3437A), (n: '136013-106.p5'; l: $800; p: $800; crc: $F4468045));
  milliped_dip_a: array [0 .. 6] of def_dip2 = ((mask: 1; name: 'Millipede Head'; number: 2; val2: (0, 1); name2: ('Easy', 'Hard')), (mask: 2; name: 'Beetle'; number: 2; val2: (0, 2); name2: ('Easy', 'Hard')), (mask: $C; name: 'Lives'; number: 4; val4: (0, 4, 8, $C);
    name4: ('2', '3', '4', '5')), (mask: $30; name: 'Bonus Life'; number: 4; val4: (0, $10, $20, $30); name4: ('12K', '15K', '20K', 'None')), (mask: $40; name: 'Spider'; number: 2; val2: (0, $40); name2: ('Easy', 'Hard')), (mask: $80; name: 'Starting Score Select'; number: 2;
    val2: ($80, 0); name2: ('Off', 'On')), ());
  milliped_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Coinage'; number: 4; val4: (3, 2, 1, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C; name: 'Right Coin'; number: 4; val4: (0, 4, 8, $C); name4: ('*1', '*4', '*5', '*6')), (mask: $10;
    name: 'Left Coin'; number: 2; val2: (0, $10); name2: ('*1', '*2')), (mask: $E0; name: 'Bonus Coin'; number: 8; val8: (0, $20, $40, $60, $80, $A0, $C0, $E0); name8: ('None', '3C 2C', '5C 4C', '6C 4C', '6C 5C', '4C 3C', 'Demo Mode', 'Invalid')), ());
  milliped_dip_c: array [0 .. 2] of def_dip2 = ((mask: 3; name: 'Language'; number: 4; val4: (0, 1, 2, 3); name4: ('English', 'German', 'French', 'Spanish')), (mask: $C; name: 'Bonus'; number: 4; val4: (0, 4, 8, $C); name4: ('0', '0 1X', '0 1X 2X', '0 1X 2X 3X')), ());

var
  nvram: array [0 .. $3F] of byte;
  update_video_centipede_hw: procedure;
  eventos_centipede_hw: procedure;

procedure update_video_centipede;
var
  f: word;
  color, x, y, atrib, nchar: byte;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      atrib := memory[$400 + f];
      nchar := (atrib and $3F) or $40;
      put_gfx_flip(x * 8, y * 8, nchar, 0, 1, 0, (atrib and $40) <> 0, (atrib and $80) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  for f := 0 to $F do
  begin
    y := 255 - memory[$7E0 + f];
    if y > 7 then
    begin
      atrib := memory[$7C0 + f];
      nchar := ((atrib and $3E) shr 1) or ((atrib and $1) shl 6);
      color := memory[$7F0 + f];
      x := 240 - memory[$7D0 + f];
      put_gfx_sprite(nchar, color shl 2, (atrib and $80) <> 0, (atrib and $40) <> 0, 1);
      update_gfx_sprite(x, y - 7, 2, 1);
    end;
  end;
  update_final_piece(0, 0, 240, 256, 2);
end;

procedure update_video_millipede;
var
  f: word;
  color, x, y, atrib, nchar: byte;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      atrib := memory[$1000 + f];
      nchar := (atrib and $3F) or $40 or ((atrib and $40) shl 1);
      color := (atrib shr 6) and 3;
      put_gfx(x * 8, y * 8, nchar, color shl 2, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  for f := 0 to $F do
  begin
    y := 255 - memory[$13E0 + f];
    if y > 7 then
    begin
      atrib := memory[$13C0 + f];
      nchar := ((atrib and $3E) shr 1) or ((atrib and $1) shl 6);
      color := memory[$13F0 + f];
      x := 240 - memory[$13D0 + f];
      put_gfx_sprite(nchar, color shl 2, (atrib and $80) <> 0, false, 1);
      update_gfx_sprite(x, y - 7, 2, 1);
    end;
  end;
  update_final_piece(0, 0, 240, 256, 2);
end;

procedure events_centipede;
begin
  if event.arcade then
  begin
    // system
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // P1+P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure events_millipede;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.right[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.right[1] then
      marcade.in3 := (marcade.in3 and $FE)
    else
      marcade.in3 := (marcade.in3 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in3 := (marcade.in3 and $FD)
    else
      marcade.in3 := (marcade.in3 or 2);
    if p_contrls.map_arcade.down[1] then
      marcade.in3 := (marcade.in3 and $FB)
    else
      marcade.in3 := (marcade.in3 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in3 := (marcade.in3 and $F7)
    else
      marcade.in3 := (marcade.in3 or 8);
  end;
end;

// needs pause
procedure centipede_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
      events_centipede;
      case f of
        0:
          marcade.dswc := marcade.dswc and $BF;
        16, 80, 144, 208:
          m6502_0.change_irq(CLEAR_LINE);
        48, 112, 176:
          m6502_0.change_irq(ASSERT_LINE);
        240:
          begin
            update_video_centipede_hw;
            m6502_0.change_irq(ASSERT_LINE);
            marcade.dswc := marcade.dswc or $40;
          end;
      end;
      m6502_0.run(frame_main);
      frame_main := frame_main + m6502_0.tframes - m6502_0.contador;
    end;
    video_sync;
  end;
end;

function centipede_getbyte(direccion: word): byte;
begin
  direccion := direccion and $3FFF;
  case direccion of
    0 .. $7FF, $2000 .. $3FFF:
      centipede_getbyte := memory[direccion];
    $800:
      centipede_getbyte := marcade.dswa;
    $801:
      centipede_getbyte := marcade.dswb;
    $C00:
      centipede_getbyte := marcade.dswc; // vsync+trackball 1
    $C01:
      centipede_getbyte := marcade.in0;
    $C02:
      centipede_getbyte := 0; // trackball 2
    $C03:
      centipede_getbyte := marcade.in1;
    $1000 .. $100F:
      centipede_getbyte := pokey_0.read(direccion and $F);
    $1700 .. $173F:
      centipede_getbyte := nvram[direccion and $3F];
  end;
end;

procedure centipede_putbyte(direccion: word; valor: byte);
  procedure change_color(pos, valor: byte);
  var
    color: tcolor;
    f: byte;
  begin
    // bit 2 of the output palette RAM is always pulled high, so we ignore
    // any palette changes unless the write is to a palette RAM address
    // that is actually used
    if (pos and 4) <> 0 then
    begin
      color.r := $FF * ((not(valor) shr 0) and 1);
      color.g := $FF * ((not(valor) shr 1) and 1);
      color.b := $FF * ((not(valor) shr 2) and 1);
      if (not(valor) and $08) <> 0 then
      begin // alternate = 1
        // when blue component is not 0, decrease it. When blue component is 0,
        // decrease green component.
        if (color.b) <> 0 then
          color.b := $C0
        else if (color.g) <> 0 then
          color.g := $C0;
      end;
      // sprite colors
      if ((pos and $8) <> 0) then
      begin
        set_pal_color(color, pos);
        for f := 0 to $3F do
        begin
          if ((pos and $3) = (((f * 4) shr 2) and $3)) then
            gfx[1].colores[(f * 4) + 1] := pos;
          if ((pos and $3) = (((f * 4) shr 4) and $3)) then
            gfx[1].colores[(f * 4) + 2] := pos;
          if ((pos and $3) = (((f * 4) shr 6) and $3)) then
            gfx[1].colores[(f * 4) + 3] := pos;
        end;
      end
      else
      begin
        set_pal_color(color, pos and $3);
        fillchar(gfx[0].buffer, $400, 1);
      end;
    end;
  end;

begin
  direccion := direccion and $3FFF;
  case direccion of
    0 .. $3FF:
      memory[direccion] := valor;
    $400 .. $7FF:
      if memory[direccion] <> valor then
      begin // video+sprites
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $1000 .. $100F:
      pokey_0.write(direccion and $F, valor);
    $1400 .. $140F:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        change_color(direccion and $F, valor);
      end;
    $1600 .. $163F:
      nvram[direccion and $3F] := valor;
    $1680:
      ; // nvram clock
    $1800:
      m6502_0.change_irq(CLEAR_LINE);
    $1C07:
      main_screen.flip_main_screen := (valor and $80) <> 0;
    $2000:
      ; // WD
    $2001 .. $3FFF:
      ;
  end;
end;

procedure centipede_sound_update;
begin
  pokey_0.update;
end;

// Millipede
function millipede_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FF, $1000 .. $13FF, $4000 .. $7FFF:
      millipede_getbyte := memory[direccion];
    $400 .. $40F:
      millipede_getbyte := pokey_0.read(direccion and $F);
    $800 .. $80F:
      millipede_getbyte := pokey_1.read(direccion and $F);
    $2000:
      millipede_getbyte := marcade.dswc or marcade.in0;
    $2001:
      millipede_getbyte := marcade.in1; // DIP0
    $2010:
      millipede_getbyte := marcade.in2;
    $2011:
      millipede_getbyte := marcade.in3; // DIP1
    $2030 .. $206F:
      millipede_getbyte := nvram[direccion and $3F];
  end;
end;

procedure millipede_putbyte(direccion: word; valor: byte);
  procedure change_color(pos, valor: byte);
  var
    color: tcolor;
    f, bit0, bit1, bit2: byte;
    base, res: word;
  begin
    // red component
    bit0 := (not(valor) shr 5) and 1;
    bit1 := (not(valor) shr 6) and 1;
    bit2 := (not(valor) shr 7) and 1;
    color.r := $21 * bit0 + $47 * bit1 + $97 * bit2;
    // green component
    bit0 := 0;
    bit1 := (not(valor) shr 3) and 1;
    bit2 := (not(valor) shr 4) and 1;
    color.g := $21 * bit0 + $47 * bit1 + $97 * bit2;
    // blue component
    bit0 := (not(valor) shr 0) and 1;
    bit1 := (not(valor) shr 1) and 1;
    bit2 := (not(valor) shr 2) and 1;
    color.b := $21 * bit0 + $47 * bit1 + $97 * bit2;
    set_pal_color(color, pos);
    // sprite colors
    if (pos >= $10) then
    begin
      base := (pos and $C) shl 6;
      for f := 0 to $3F do
      begin
        res := base + (f * 4);
        if ((pos and $3) = ((res shr 2) and $3)) then
          gfx[1].colores[res + 1] := pos;
        if ((pos and $3) = ((res shr 4) and $3)) then
          gfx[1].colores[res + 2] := pos;
        if ((pos and $3) = ((res shr 6) and $3)) then
          gfx[1].colores[res + 3] := pos;
      end;
    end
    else
      fillchar(gfx[0].buffer, $400, 1);
  end;

begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FF:
      memory[direccion] := valor;
    $400 .. $40F:
      pokey_0.write(direccion and $F, valor);
    $800 .. $80F:
      pokey_1.write(direccion and $F, valor);
    $1000 .. $13FF:
      if memory[direccion] <> valor then
      begin // video+sprites
        memory[direccion] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $2480 .. $249F:
      if memory[direccion] <> valor then
      begin
        memory[direccion] := valor;
        change_color(direccion and $1F, valor);
      end;
    $2500 .. $2505, $2507:
      ;
    $2506:
      main_screen.flip_main_screen := (valor and $80) <> 0;
    $2600:
      m6502_0.change_irq(CLEAR_LINE);
    $2680:
      ; // WD
    $2700:
      ; // nvram control
    $2780 .. $27BF:
      nvram[direccion and $3F] := valor;
    $4000 .. $7FFF:
      ;
  end;
end;

procedure millipede_sound_update;
begin
  pokey_0.update;
  pokey_1.update;
end;

function millipede_dsw1(pot: byte): byte;
begin
  millipede_dsw1 := marcade.dswa;
end;

function millipede_dsw2(pot: byte): byte;
begin
  millipede_dsw2 := marcade.dswb;
end;

// Main
procedure reset_centipede;
begin
  m6502_0.reset;
  pokey_0.reset;
  frame_main := m6502_0.tframes;
  case main_vars.machine_type of
    218:
      begin
        fillchar(memory[$1400], $10, $FF); // Resetear la paleta...
        marcade.in0 := $FF;
        marcade.in1 := $FF;
      end;
    348:
      begin
        pokey_1.reset;
        fillchar(memory[$2480], $20, $FF); // Resetear la paleta...
        marcade.in0 := $30;
        marcade.in1 := $30;
        marcade.in2 := $FF;
        marcade.in3 := $FF;
      end;
  end;
end;

procedure close_centipede;
begin
  case main_vars.machine_type of
    218:
      write_file(dm.tConfignvram.AsString + 'centiped.nv', @nvram, $40);
    348:
      write_file(dm.tConfignvram.AsString + 'milliped.nv', @nvram, $40);
  end;
end;

function start_centipede: boolean;
var
  memoria_temp: array [0 .. $FFF] of byte;
  longitud: integer;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  start_centipede := false;
  machine_calls.general_loop := centipede_loop;
  machine_calls.close := close_centipede;
  machine_calls.reset := reset_centipede;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true, true);
  start_video(240, 256);
  // Main CPU
  m6502_0 := cpu_m6502.create(12096000 div 8, 256, TCPU_M6502);
  case main_vars.machine_type of
    218:
      begin // Centipede
        m6502_0.change_ram_calls(centipede_getbyte, centipede_putbyte);
        m6502_0.init_sound(centipede_sound_update);
        // Sound Chips
        pokey_0 := pokey_chip.create(12096000 div 8);
        // cargar roms
        if not(roms_load(@memory, centipede_rom)) then
          exit;
        // convertir chars y sprites
        if not(roms_load(@memoria_temp, centipede_chars)) then
          exit;
        init_gfx(0, 8, 8, $100);
        gfx_set_desc_data(2, 0, 8 * 8, $100 * 8 * 8, 0);
        convert_gfx(0, 0, @memoria_temp, @pc_x, @ps_y, false, true);
        init_gfx(1, 8, 16, $80);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, $80 * 16 * 8, 0);
        convert_gfx(1, 0, @memoria_temp, @pc_x, @ps_y, false, true);
        // DIP
        marcade.dswa := $54;
        marcade.dswa_val2 := @centipede_dip_a;
        marcade.dswb := $2;
        marcade.dswb_val2 := @centipede_dip_b;
        marcade.dswc := $20;
        marcade.dswc_val2 := @centipede_dip_c;
        // NVRAM
        if read_file_size(dm.tConfignvram.AsString + 'centiped.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'centiped.nv', @nvram[0], longitud);
        update_video_centipede_hw := update_video_centipede;
        eventos_centipede_hw := events_centipede;
      end;
    348:
      begin // Millipede
        m6502_0.change_ram_calls(millipede_getbyte, millipede_putbyte);
        m6502_0.init_sound(millipede_sound_update);
        // Sound Chips
        pokey_0 := pokey_chip.create(12096000 div 8);
        pokey_1 := pokey_chip.create(12096000 div 8);
        pokey_0.change_all_pot(millipede_dsw1);
        pokey_1.change_all_pot(millipede_dsw2);
        // cargar roms
        if not(roms_load(@memory, milliped_rom)) then
          exit;
        // convertir chars y sprites
        if not(roms_load(@memoria_temp, milliped_chars)) then
          exit;
        init_gfx(0, 8, 8, $100);
        gfx_set_desc_data(2, 0, 8 * 8, $100 * 8 * 8, 0);
        convert_gfx(0, 0, @memoria_temp, @pc_x, @ps_y, false, true);
        init_gfx(1, 8, 16, $80);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, $80 * 16 * 8, 0);
        convert_gfx(1, 0, @memoria_temp, @pc_x, @ps_y, false, true);
        // DIP
        marcade.dswa := $14;
        marcade.dswa_val2 := @milliped_dip_a;
        marcade.dswb := $2;
        marcade.dswb_val2 := @milliped_dip_b;
        marcade.dswc := $4;
        marcade.dswc_val2 := @milliped_dip_c;
        // NVRAM
        if read_file_size(dm.tConfignvram.AsString + 'milliped.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'milliped.nv', @nvram[0], longitud);
        update_video_centipede_hw := update_video_millipede;
        eventos_centipede_hw := events_millipede;
      end;
  end;
  // final
  start_centipede := true;
end;

end.
