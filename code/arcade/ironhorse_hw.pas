unit ironhorse_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  rom_engine,
  pal_engine,
  sound_engine;

function start_ironhorse: boolean;

implementation

const
  ironhorse_rom: array [0 .. 1] of tipo_roms = ((n: '560_k03.13c'; l: $8000; p: $4000;
    crc: $395351B4), (n: '560_k02.12c'; l: $4000; p: $C000; crc: $1CFF3D59));
  ironhorse_snd: tipo_roms = (n: '560_h01.10c'; l: $4000; p: 0; crc: $2B17930F);
  ironhorse_gfx: array [0 .. 3] of tipo_roms = ((n: '560_h06.08f'; l: $8000; p: 0; crc: $F21D8C93),
    (n: '560_h05.07f'; l: $8000; p: $1; crc: $60107859), (n: '560_h07.09f'; l: $8000; p: $10000;
    crc: $C761EC73), (n: '560_h04.06f'; l: $8000; p: $10001; crc: $C1486F61));
  ironhorse_pal: array [0 .. 4] of tipo_roms = ((n: '03f_h08.bin'; l: $100; p: 0; crc: $9F6DDF83),
    (n: '04f_h09.bin'; l: $100; p: $100; crc: $E6773825), (n: '05f_h10.bin'; l: $100; p: $200;
    crc: $30A57860), (n: '10f_h12.bin'; l: $100; p: $300; crc: $5EB33E73), (n: '10f_h11.bin';
    l: $100; p: $400; crc: $A63E37D8));
  // Dip
  ironhorse_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16;
    dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'),
    (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'),
    (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'),
    (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 15;
    dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80;
    dip_name: '2C 1C'), (dip_val: $40; dip_name: '3C 2C'), (dip_val: $10;
    dip_name: '4C 3C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $30;
    dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0;
    dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0;
    dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90;
    dip_name: '1C 7C'), (dip_val: $0; dip_name: 'No Coin B'))), ());
  ironhorse_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4;
    dip: ((dip_val: $3; dip_name: '2'), (dip_val: $2; dip_name: '3'), (dip_val: $1;
    dip_name: '5'), (dip_val: $0; dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $4; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4;
    dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $18; dip_name: '30K 70K+'), (dip_val: $10;
    dip_name: '40K 80K+'), (dip_val: $8; dip_name: '40K'), (dip_val: $0; dip_name: '50K'), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20;
    dip_name: 'Difficult'), (dip_val: $0; dip_name: 'Very Difficult'), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  ironhorse_dip_c: array [0 .. 3] of def_dip = ((mask: $1; name: 'Flip Screen'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $1; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Upright Controls'; number: 2;
    dip: ((dip_val: $2; dip_name: 'Single'), (dip_val: $0; dip_name: 'Dual'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Button Layout'; number: 2;
    dip: ((dip_val: $4; dip_name: 'Power Attack Squat'), (dip_val: $0;
    dip_name: 'Squat Attack Power'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  pedir_nmi, pedir_firq: boolean;
  sound_latch, charbank, palettebank: byte;
  spritebank: word;
  scroll_x: array [0 .. $1F] of word;

procedure update_video_ironhorse;
var
  x, y, atrib, a, b, c, d: byte;
  f, nchar, color: word;
  flipx, flipy: boolean;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      atrib := memory[$2000 + f];
      nchar := memory[$2400 + f] + ((atrib and $40) shl 2) + ((atrib and $20) shl 4) +
        (charbank shl 10);
      color := ((atrib and $F) + 16 * palettebank) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $10) <> 0, (atrib and $20) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // Scroll linea a linea
  scroll__x_part2(1, 2, 8, @scroll_x);
  for f := 0 to $32 do
  begin
    x := memory[spritebank + 3 + (f * 5)];
    y := memory[spritebank + 2 + (f * 5)];
    atrib := memory[spritebank + 1 + (f * 5)];
    nchar := (memory[spritebank + (f * 5)] shl 2) + ((atrib and $03) shl 10) +
      ((atrib and $0C) shr 2);
    color := ((((atrib and $F0) shr 4) + 16 * palettebank) shl 4) + $800;
    atrib := memory[spritebank + 4 + (f * 5)];
    flipx := ((atrib and $20) shr 5) <> 0;
    flipy := ((atrib and $40) shr 5) <> 0;
    case (atrib and $C) of
      $0:
        begin // 16x16
          a := (0 xor byte(flipx)) xor byte(flipy);
          b := (1 xor byte(flipx)) xor byte(flipy);
          c := (2 xor byte(flipx)) xor byte(flipy);
          d := (3 xor byte(flipx)) xor byte(flipy);
          put_gfx_sprite_diff(nchar + a, color, flipx, flipy, 0, 0, 0);
          put_gfx_sprite_diff(nchar + b, color, flipx, flipy, 0, 8, 0);
          put_gfx_sprite_diff(nchar + c, color, flipx, flipy, 0, 0, 8);
          put_gfx_sprite_diff(nchar + d, color, flipx, flipy, 0, 8, 8);
          actualiza_gfx_sprite_size(x, y, 2, 16, 16);
        end;
      $4:
        begin // 16x8
          a := 0 xor byte(flipx);
          b := 1 xor byte(flipx);
          put_gfx_sprite_diff(nchar + a, color, flipx, flipy, 0, 0, 0);
          put_gfx_sprite_diff(nchar + b, color, flipx, flipy, 0, 8, 0);
          actualiza_gfx_sprite_size(x, y, 2, 16, 8);
        end;
      $8:
        begin // 8x16
          a := 0 xor byte(flipy);
          b := 2 xor byte(flipy);
          put_gfx_sprite_diff(nchar + a, color, flipx, flipy, 0, 0, 0);
          put_gfx_sprite_diff(nchar + b, color, flipx, flipy, 0, 0, 8);
          actualiza_gfx_sprite_size(x, y, 2, 8, 16);
        end;
      $C:
        begin // 8x8
          put_gfx_sprite_mask(nchar, color, flipx, flipy, 0, 0, $F);
          update_gfx_sprite(x, y, 2, 0);
        end;
    end;
  end;
  actualiza_trozo_final(8, 16, 240, 224, 2);
end;

procedure events_ironhorse;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
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
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure ironhorse_loop;
var
  frame_m, frame_s: single;
  f: byte;
  frame: boolean;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := z80_0.tframes;
  frame := false;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // snd
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        case f of
          47, 111, 175:
            if pedir_nmi then
              m6809_0.change_nmi(PULSE_LINE);
          239:
            begin
              if (pedir_firq and frame) then
                m6809_0.change_firq(HOLD_LINE);
              if pedir_nmi then
                m6809_0.change_nmi(PULSE_LINE);
            end;
        end;
      end;
      update_video_ironhorse;
      events_ironhorse;
      video_sync;
      frame := not(frame);
    end
    else
      pause_action;
  end;
end;

function ironhorse_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1F, $40 .. $DF, $2000 .. $FFFF:
      ironhorse_getbyte := memory[direccion];
    $20 .. $3F:
      ironhorse_getbyte := scroll_x[direccion and $1F];
    $900:
      ironhorse_getbyte := marcade.dswc; // dsw3
    $A00:
      ironhorse_getbyte := marcade.dswb; // dsw2
    $B00:
      ironhorse_getbyte := marcade.dswa; // dsw1
    $B01:
      ironhorse_getbyte := marcade.in1; // p2
    $B02:
      ironhorse_getbyte := marcade.in0; // p1
    $B03:
      ironhorse_getbyte := marcade.in2; // system
  end;
end;

procedure ironhorse_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. 2, 5 .. $1F, $40 .. $DF, $2800 .. $3FFF:
      memory[direccion] := valor;
    $3:
      begin
        if charbank <> (valor and $3) then
        begin
          charbank := valor and $3;
          fillchar(gfx[0].buffer[0], $400, 1);
        end;
        spritebank := $3000 + ((valor and $8) shl 8);
      end;
    $4:
      begin
        pedir_nmi := (valor and $1) <> 0;
        pedir_firq := (valor and $4) <> 0;
      end;
    $20 .. $3F:
      scroll_x[direccion and $1F] := valor;
    $800:
      sound_latch := valor;
    $900:
      z80_0.change_irq(HOLD_LINE);
    $A00:
      if palettebank <> (valor and $7) then
      begin
        palettebank := valor and $7;
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
    $B00:
      main_screen.flip_main_screen := (valor and $8) = 0;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function ironhorse_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $43FF:
      ironhorse_snd_getbyte := mem_snd[direccion];
    $8000:
      ironhorse_snd_getbyte := sound_latch
  end;
end;

procedure ironhorse_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
  end;
end;

function ironhorse_snd_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 0 then
    ironhorse_snd_inbyte := ym2203_0.status;
end;

procedure ironhorse_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ym2203_0.Control(valor);
    1:
      ym2203_0.Write(valor);
  end;
end;

procedure ironhorse_sound_update;
begin
  ym2203_0.Update;
end;

// Main
procedure reset_ironhorse;
begin
  m6809_0.reset;
  z80_0.reset;
  ym2203_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  pedir_nmi := false;
  pedir_firq := false;
  charbank := 0;
  sound_latch := 0;
  spritebank := $3800;
  palettebank := 0;
end;

function start_ironhorse: boolean;
var
  colores: tpaleta;
  valor, j: byte;
  f, valor2: word;
  memory_temp: array [0 .. $1FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4);
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
begin
  machine_calls.general_loop := ironhorse_loop;
  machine_calls.reset := reset_ironhorse;
  machine_calls.fps_max := 61;
  start_ironhorse := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, false, true);
  start_video(240, 224);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, $100, TCPU_M6809);
  m6809_0.change_ram_calls(ironhorse_getbyte, ironhorse_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(3072000, $100);
  z80_0.change_ram_calls(ironhorse_snd_getbyte, ironhorse_snd_putbyte);
  z80_0.change_io_calls(ironhorse_snd_inbyte, ironhorse_snd_outbyte);
  z80_0.init_sound(ironhorse_sound_update);
  // Sound Chip
  ym2203_0 := ym2203_chip.Create(3072000);
  // cargar roms
  if not(roms_load(@memory, ironhorse_rom)) then
    exit;
  // roms sonido
  if not(roms_load(@mem_snd, ironhorse_snd)) then
    exit;
  // convertir chars
  if not(roms_load16b(@memory_temp, ironhorse_gfx)) then
    exit;
  init_gfx(0, 8, 8, $1000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // paleta
  if not(roms_load(@memory_temp, ironhorse_pal)) then
    exit;
  for f := 0 to $FF do
  begin
    colores[f].r := ((memory_temp[f] and $F) shl 4) or (memory_temp[f] and $F);
    colores[f].g := ((memory_temp[f + $100] and $F) shl 4) or (memory_temp[f + $100] shr 4);
    colores[f].b := ((memory_temp[f + $200] and $F) shl 4) or (memory_temp[f + $200] and $F);
  end;
  set_pal(colores, 256);
  for f := 0 to $1FF do
  begin
    for j := 0 to 7 do
    begin
      valor := (j shl 5) or ((not(f) and $100) shr 4) or (memory_temp[f + $300] and $0F);
      valor2 := ((f and $100) shl 3) or (j shl 8) or (f and $FF);
      gfx[0].colores[valor2] := valor; // chars
      gfx[1].colores[valor2] := valor; // sprites
    end;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $5A;
  marcade.dswc := $FE;
  marcade.dswa_val := @ironhorse_dip_a;
  marcade.dswb_val := @ironhorse_dip_b;
  marcade.dswc_val := @ironhorse_dip_c;
  // final
  reset_ironhorse;
  start_ironhorse := true;
end;

end.
