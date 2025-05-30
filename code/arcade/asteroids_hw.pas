unit asteroids_hw;

interface

uses
  asteroids_hw_audio,
  m6502,
  main_engine,
  controls_engine,
  gfx_engine,
  timer_engine,
  samples,
  rom_engine,
  pal_engine,
  sound_engine,
  avg_dvg;

function start_asteroids: boolean;

implementation

const
  as_rom: array [0 .. 3] of tipo_roms = ((n: '035145-04e.ef2'; l: $800; p: $6800; crc: $B503EAF7), (n: '035144-04e.h2'; l: $800; p: $7000; crc: $25233192), (n: '035143-02.j2'; l: $800; p: $7800; crc: $312CAA02), (n: '035127-02.np3'; l: $800; p: $5000; crc: $8B71FD9E));
  as_prom: tipo_roms = (n: '034602-01.c8'; l: $100; p: $0; crc: $97953DB8);
  as_samples: array [0 .. 2] of tipo_nombre_samples = ((nombre: 'explode1.wav'), (nombre: 'explode2.wav'), (nombre: 'explode3.wav'));
  asteroids_dip_a: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Lenguaje'; number: 4; val4: (0, 1, 2, 3); name4: ('English', 'German', 'French', 'Spanish')), (mask: $4; name: 'Lives'; number: 2; val2: (4, 0); name2: ('3', '4')), (mask: $8; name: 'Center Mech'; number: 2;
    val2: (0, 8); name2: ('X 1', 'X 2')), (mask: $30; name: 'Right Mech'; number: 4; val4: (0, $10, $20, $30); name4: ('X 1', 'X 4', 'X 5', 'X 6')), (mask: $C0; name: 'Coinage'; number: 4; val4: ($C0, $80, $40, 0); name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), ());
  llander_rom: array [0 .. 6] of tipo_roms = ((n: '034572-02.f1'; l: $800; p: $6000; crc: $B8763EEA), (n: '034571-02.de1'; l: $800; p: $6800; crc: $77DA4B2F), (n: '034570-01.c1'; l: $800; p: $7000; crc: $2724E591), (n: '034569-02.b1'; l: $800; p: $7800; crc: $72837A4E),
    (n: '034599-01.r3'; l: $800; p: $4800; crc: $355A9371), (n: '034598-01.np3'; l: $800; p: $5000; crc: $9C4FFA68), (n: '034597-01.m3'; l: $800; p: $5800; crc: $EBB744F2));
  llander_prom: tipo_roms = (n: '034602-01.c8'; l: $100; p: $0; crc: $97953DB8);

var
  hay_samples: boolean;
  ram: array [0 .. 1, 0 .. $FF] of byte;
  x_actual, y_actual: integer;
  ram_bank: byte;

procedure update_video_as;
var
  color, posicion, opcode: word;
  istack: array [0 .. 3] of word;
  i, iscale, x, y, dx, dy, xa_d, ya_d: integer;
  pila: byte;
  salir, draw: boolean;
begin
  fill_full_screen(1, 0);
  pila := 0;
  posicion := $4000;
  salir := false;
  iscale := 0;
  while not(salir) do
  begin
    opcode := (memory[posicion + 1] shl 8) or memory[posicion];
    posicion := posicion + 2;
    draw := false;
    case (opcode and $F000) of
      0, $B000:
        salir := true; // halt
      $1000, $2000, $3000, $4000, $5000, $6000, $7000, $8000, $9000:
        begin // vectores largos
          i := ((opcode shr 12) + iscale) and $F; // Scale factor */
          if i > 9 then
            i := -1;
          y := opcode and $3FF;
          dy := y shr (9 - i);
          if (opcode and $400) <> 0 then
            dy := -dy; // Y sign bit */
          opcode := (memory[posicion + 1] shl 8) or memory[posicion];
          posicion := posicion + 2; // get second half of instruction */
          x := opcode and $3FF;
          dx := x shr (9 - i); // Adjust for both scale factors *
          if (opcode and $400) <> 0 then
            dx := -dx; // X sign bit */
          color := (opcode shr 12) and $FF;
          x := x_actual;
          y := y_actual;
          x_actual := x_actual + dx;
          y_actual := y_actual + dy;
          draw := color <> 0;
        end;
      $A000:
        begin // Posicion del haz y factor de escalado
          y_actual := opcode and $FFF; // Lower 12 bits are Y position */
          opcode := (memory[posicion + 1] shl 8) or memory[posicion];
          posicion := posicion + 2; // get second half of instruction */
          x_actual := opcode and $FFF; // Lower 12 bits are X position */
          iscale := (opcode and $F000) shr 12;
          if (opcode and $8000) <> 0 then
            iscale := iscale - 16; // divisor = negative shift
        end;
      $C000:
        begin // Llamada subrutina
          if ((opcode and $1FFF) = 0) then
            salir := true // Address of 0 same as HALT */
          else
          begin
            pila := (pila + 1) and $F;
            istack[pila and 3] := posicion; // push current position */
            posicion := $4000 + (opcode and $1FFF) * 2;
          end;
        end;
      $D000:
        begin // Vuelta de subrutina
          posicion := istack[pila and 3];
          pila := (pila - 1) and $F;
        end;
      $E000:
        begin // Saltar a posicion
          if ((opcode and $1FFF) = 0) then
            salir := true // Address of 0 same as HALT */
          else
            posicion := $4000 + (opcode and $1FFF) * 2;
        end;
      $F000:
        begin // vector corto
          i := ((opcode shr 2) and $2) + ((opcode shr 11) and $1);
          i := ((iscale + i) and $F);
          if i > 7 then
            i := -1;
          color := ((opcode shr 4) and $F);
          x := (opcode and 3) shl 8;
          dx := x shr (7 - i);
          if (opcode and 4) <> 0 then
            dx := -dx;
          y := opcode and $300;
          dy := y shr (7 - i);
          if (opcode and $400) <> 0 then
            dy := -dy;
          x := x_actual;
          y := y_actual;
          x_actual := x_actual + dx;
          y_actual := y_actual + dy;
          draw := color <> 0;
        end;
    end;
    // Resolucion pantalla 400x320, resolucion real max 1024x1024
    if draw then
    begin
      if x > 1024 then
        x := 400
      else if x < 0 then
        x := 0
      else
        x := trunc(x / 2.56);
      if x_actual > 1024 then
        xa_d := 400
      else if x_actual < 0 then
        xa_d := 0
      else
        xa_d := trunc(x_actual / 2.56);
      if y > 1024 then
        y := 400
      else if y < 0 then
        y := 0
      else
        y := trunc((1024 - y) / 2.56);
      if y_actual > 1024 then
        ya_d := 0
      else if y_actual < 0 then
        ya_d := 400
      else
        ya_d := trunc((1024 - y_actual) / 2.56);
      draw_line(x + ADD_SPRITE, y + ADD_SPRITE, xa_d + ADD_SPRITE, ya_d + ADD_SPRITE, color, 1);
    end;
  end;
  update_final_piece(0, 40, 400, 320, 1);
end;

procedure events_as;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := marcade.in0 or $8
    else
      marcade.in0 := marcade.in0 and $F7;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 or $10
    else
      marcade.in0 := marcade.in0 and $EF;
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := marcade.in1 or 1
    else
      marcade.in1 := marcade.in1 and $FE;
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := marcade.in1 or 2
    else
      marcade.in1 := marcade.in1 and $FD;
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := marcade.in1 or 8
    else
      marcade.in1 := marcade.in1 and $F7;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 or $10
    else
      marcade.in1 := marcade.in1 and $EF;
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := marcade.in1 or $20
    else
      marcade.in1 := marcade.in1 and $DF;
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := marcade.in1 or $40
    else
      marcade.in1 := marcade.in1 and $BF;
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := marcade.in1 or $80
    else
      marcade.in1 := marcade.in1 and $7F;
  end;
end;

procedure asteroids_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 299 do
      begin
        m6502_0.run(frame);
        frame := frame + m6502_0.tframes - m6502_0.contador;
      end;
      events_as;
      video_sync;
    end
    else
      pause_action;
  end;
end;

// Asteroids
function getbyte_as(direccion: word): byte;
var
  mascara: byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $1FF, $4000 .. $47FF, $5000 .. $57FF, $6800 .. $7FFF:
      getbyte_as := memory[direccion];
    $200 .. $2FF:
      getbyte_as := ram[ram_bank, direccion and $FF];
    $300 .. $3FF:
      getbyte_as := ram[1 - ram_bank, direccion and $FF];
    $2001:
      getbyte_as := (m6502_0.totalt and $100) shr 8;
    $2000, $2003 .. $2007:
      begin
        mascara := 1 shl (direccion and $7);
        if (marcade.in0 and mascara) <> 0 then
          getbyte_as := $80
        else
          getbyte_as := $7F;
      end;
    $2002:
      getbyte_as := avgdvg_0.done_r * $80;
    $2400 .. $2407:
      begin
        mascara := 1 shl (direccion and $7);
        if (marcade.in1 and mascara) <> 0 then
          getbyte_as := $80
        else
          getbyte_as := $7F;
      end;
    $2800:
      getbyte_as := $FC or ((marcade.dswa shr 6) and $3); // Coinage
    $2801:
      getbyte_as := $FC or ((marcade.dswa shr 4) and $3); // Right Mech
    $2802:
      getbyte_as := $FC or ((marcade.dswa shr 2) and $3); // Lives+Center Mech
    $2803:
      getbyte_as := $FC or ((marcade.dswa shr 0) and $3); // Lenguaje
  end;
end;

procedure putbyte_as(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $1FF, $4000 .. $47FF:
      memory[direccion] := valor;
    $200 .. $2FF:
      ram[ram_bank, direccion and $FF] := valor;
    $300 .. $3FF:
      ram[1 - ram_bank, direccion and $FF] := valor;
    $3000:
      avgdvg_0.go_w;
    $3200:
      ram_bank := (valor and 4) shr 2;
    $3600:
      asteroid_explode_w(valor, hay_samples);
    $3A00:
      asteroid_thump_w(valor);
    $3C00 .. $3C05:
      asteroid_sounds_w(direccion and $7, valor);
    $5000 .. $57FF, $6800 .. $7FFF:
      ;
  end;
end;

// Lunar Lander
function getbyte_llander(direccion: word): byte;
var
  mascara: byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $1FFF:
      getbyte_llander := memory[direccion and $FF];
    $2000:
      getbyte_llander := not(avgdvg_0.done_r) or ((m6502_0.totalt and $100) shr 2) or $BE;
    $2400 .. $2407:
      begin
        mascara := 1 shl (direccion and $7);
        if ($50 and mascara) <> 0 then
          getbyte_llander := $80
        else
          getbyte_llander := $7F;
      end;
    $2800:
      getbyte_llander := $FE; // c or ((marcade.dswa shr 6) and $3); //Coinage
    $2801:
      getbyte_llander := $FC; // or ((marcade.dswa shr 4) and $3); //Right Mech
    $2802:
      getbyte_llander := $FC; // or ((marcade.dswa shr 2) and $3); //Lives+Center Mech
    $2803:
      getbyte_llander := $FC; // or ((marcade.dswa shr 0) and $3); //Lenguaje
    $2C00:
      getbyte_llander := 0;
    $4000 .. $7FFF:
      getbyte_llander := memory[direccion];
  end;
end;

procedure putbyte_llander(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $1FFF:
      memory[direccion and $FF] := valor;
    $3000:
      avgdvg_0.go_w;
    $3C00:
      ;
    $3E00:
      ;
    $4000 .. $47FF:
      memory[direccion] := valor;
    $4800 .. $7FFF:
      ;
  end;
end;

procedure as_snd_nmi;
begin
  m6502_0.change_nmi(PULSE_LINE);
end;

procedure as_sound;
begin
  asteroid_sound_update(hay_samples);
end;

// Main
procedure reset_as;
begin
  m6502_0.reset;
reset_game_general;
  marcade.in0 := 0;
  marcade.in1 := 0;
  x_actual := 0;
  y_actual := 0;
  ram_bank := 0;
end;

function start_asteroids: boolean;
var
  colores: tpaleta;
  f: byte;
begin
  machine_calls.general_loop := asteroids_loop;
  machine_calls.reset := reset_as;
  machine_calls.fps_max := 12096000 / 4096 / 12 / 4;
  start_asteroids := false;
  start_audio(false);
  screen_init(1, 400, 400, false, true);
  start_video(400, 320);
  // Main CPU
  m6502_0 := cpu_m6502.create(1512000, 300, TCPU_M6502);
  case main_vars.machine_type of
    23:
      begin // Asteroids
        m6502_0.change_ram_calls(getbyte_as, putbyte_as);
        m6502_0.init_sound(as_sound);
        asteroid_sound_init;
        // Timers
        timers.init(0, 1512000 / (12096000 / 4096 / 12), as_snd_nmi, nil, true);
        // cargar roms
        if not(roms_load(@memory, as_rom)) then
          exit;
        // Vectors
        avgdvg_0 := avgdvg_chip.create(m6502_0.numero_cpu, 1, $4000, 40);
        if not(roms_load(avgdvg_0.get_prom_data, as_prom)) then
          exit;
        // samples
        hay_samples := load_samples(as_samples);
        // dip
        marcade.dswa := $84;
        marcade.dswa_val2 := @asteroids_dip_a;
      end;
    233:
      begin // Lunar Lander
        m6502_0.change_ram_calls(getbyte_llander, putbyte_llander);
        // m6502_0.init_sound(as_sound);
        // Timers
        timers.init(0, 1512000 / (12096000 / 4096 / 12), as_snd_nmi, nil, true);
        // cargar roms
        if not(roms_load(@memory, llander_rom)) then
          exit;
        // Vectors
        avgdvg_0 := avgdvg_chip.create(m6502_0.numero_cpu, 1, $4000, 80);
        if not(roms_load(avgdvg_0.get_prom_data, llander_prom)) then
          exit;
        // samples
        // hay_samples:=load_samples('asteroid.zip',@as_samples,3);
        hay_samples := false;
        // dip
        marcade.dswa := $84;
        marcade.dswa_val2 := @asteroids_dip_a;
      end;
  end;
  // poner la paleta
  for f := 0 to 15 do
  begin
    colores[f].r := 17 * f;
    colores[f].g := 17 * f;
    colores[f].b := 17 * f;
  end;
  set_pal(colores, 256);
  // final
  reset_as;
  start_asteroids := true;
end;

end.
