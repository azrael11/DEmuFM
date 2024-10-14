unit tutankham_hw;

interface

uses
  WinApi.Windows,
  m6809,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  konami_snd,
  sound_engine;

function start_tutankham: boolean;

implementation

const
  tutan_rom: array [0 .. 14] of tipo_roms = ((n: 'm1.1h'; l: $1000; p: $0; crc: $DA18679F), (n: 'm2.2h'; l: $1000; p: $1000; crc: $A0F02C85), (n: '3j.3h'; l: $1000; p: $2000; crc: $EA03A1AB),
    (n: 'm4.4h'; l: $1000; p: $3000; crc: $BD06FAD0), (n: 'm5.5h'; l: $1000; p: $4000; crc: $BF9FD9B0), (n: 'j6.6h'; l: $1000; p: $5000; crc: $FE079C5B), (n: 'c1.1i'; l: $1000; p: $6000;
    crc: $7EB59B21), (n: 'c2.2i'; l: $1000; p: $7000; crc: $6615EFF3), (n: 'c3.3i'; l: $1000; p: $8000; crc: $A10D4444), (n: 'c4.4i'; l: $1000; p: $9000; crc: $58CD143C), (n: 'c5.5i'; l: $1000;
    p: $A000; crc: $D7E7AE95), (n: 'c6.6i'; l: $1000; p: $B000; crc: $91F62B82), (n: 'c7.7i'; l: $1000; p: $C000; crc: $AFD0A81F), (n: 'c8.8i'; l: $1000; p: $D000; crc: $DABB609B), (n: 'c9.9i';
    l: $1000; p: $E000; crc: $8EA9C6A6));
  tutan_sound: array [0 .. 1] of tipo_roms = ((n: 's1.7a'; l: $1000; p: 0; crc: $B52D01FA), (n: 's2.8a'; l: $1000; p: $1000; crc: $9DB5C0CE));
  // Dip
  tutan_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 15;
    dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $40; dip_name: '3C 2C'), (dip_val: $10; dip_name: '4C 3C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $30; dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90; dip_name: '1C 7C'), ())), ());
  tutan_dip_b: array [0 .. 6] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '3'), (dip_val: $1; dip_name: '4'), (dip_val: $2; dip_name: '5'), (dip_val: $0;
    dip_name: '255'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Bonus Life'; number: 2;
    dip: ((dip_val: $8; dip_name: '30K'), (dip_val: $0; dip_name: '40K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $30; dip_name: 'Easy'), (dip_val: $20; dip_name: 'Normal'), (dip_val: $10; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Flash Bomb'; number: 2; dip: ((dip_val: $40; dip_name: '1 per Life'), (dip_val: $0; dip_name: '1 per Game'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $80; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  irq_enable: boolean;
  xorx, xory, rom_nbank, scroll_y: byte;
  rom_bank: array [0 .. $F, 0 .. $FFF] of byte;

procedure update_video_tutankham;
var
  x, y, effx, effy, vrambyte, shifted: byte;
  punt: array [0 .. $FFFF] of word;
begin
  for y := 0 to 255 do
  begin
    for x := 0 to 255 do
    begin
      effy := y xor xory;
      // La parte de arriba es fija!
      if effy < 192 then
        effx := (x xor xorx) + scroll_y
      else
        effx := (x xor xorx);
      vrambyte := memory[effx * 128 + effy shr 1];
      shifted := vrambyte shr (4 * (effy and 1));
      punt[y * 256 + x] := paleta[shifted and $0F];
    end;
  end;
  putpixel(0, 0, $10000, @punt, 1);
  update_region(16, 0, 224, 256, 1, 0, 0, 224, 256, PANT_TEMP);
end;

procedure events_tutankham;
begin
  if event.arcade then
  begin
    // marcade.in1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // marcade.in2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // service
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
  end;
end;

procedure tutankham_loop;
var
  frame_m: single;
  f: byte;
  irq_req: boolean;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  irq_req := false;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Sound CPU
        konamisnd_0.run;
        if f = 239 then
        begin
          if (irq_req and irq_enable) then
            m6809_0.change_irq(ASSERT_LINE);
          update_video_tutankham;
        end;
      end;
      irq_req := not(irq_req);
      events_tutankham;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function tutankham_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $8100, $8800 .. $8FFF, $A000 .. $FFFF:
      tutankham_getbyte := memory[direccion];
    $8000 .. $80FF:
      tutankham_getbyte := buffer_paleta[direccion and $F];
    $8160 .. $816F:
      tutankham_getbyte := marcade.dswb;
    $8180 .. $818F:
      tutankham_getbyte := marcade.in0;
    $81A0 .. $81AF:
      tutankham_getbyte := marcade.in1;
    $81C0 .. $81CF:
      tutankham_getbyte := marcade.in2;
    $81E0 .. $81EF:
      tutankham_getbyte := marcade.dswa;
    $9000 .. $9FFF:
      tutankham_getbyte := rom_bank[rom_nbank, direccion and $FFF];
  end;
end;

procedure tutankham_putbyte(direccion: word; valor: byte);
var
  color: tcolor;
begin
  case direccion of
    0 .. $7FFF, $8800 .. $8FFF:
      memory[direccion] := valor;
    $8000 .. $80FF:
      begin
        color.r := pal3bit(valor shr 0);
        color.g := pal3bit(valor shr 3);
        color.b := pal2bit(valor shr 6);
        set_pal_color(color, direccion and $F);
        buffer_paleta[direccion and $F] := valor;
      end;
    $8100 .. $810F:
      scroll_y := valor;
    $8200 .. $82FF:
      case (direccion and $7) of
        0:
          begin
            irq_enable := (valor and 1) <> 0;
            if not(irq_enable) then
              m6809_0.change_irq(CLEAR_LINE);
          end;
        6:
          xory := 255 * (valor and 1);
        7:
          xorx := 255 * (not(valor) and 1); // La x esta invertida...
      end;
    $8300 .. $83FF:
      rom_nbank := valor and $F;
    $8600 .. $86FF:
      konamisnd_0.pedir_irq := HOLD_LINE;
    $8700 .. $87FF:
      konamisnd_0.sound_latch := valor;
    $9000 .. $FFFF:
      ; // ROM
  end;
end;

// Main
procedure reset_tutankham;
begin
  m6809_0.reset;
  reset_audio;
  konamisnd_0.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_enable := false;
end;

function start_tutankham: boolean;
var
  f: byte;
  memory_temp: array [0 .. $EFFF] of byte;
begin
  machine_calls.general_loop := tutankham_loop;
  machine_calls.reset := reset_tutankham;
  start_tutankham := false;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(1536000, $100, TCPU_M6809);
  m6809_0.change_ram_calls(tutankham_getbyte, tutankham_putbyte);
  // Sound Chip
  konamisnd_0 := konamisnd_chip.Create(4, TIPO_TIMEPLT, 1789772, $100);
  if not(roms_load(@konamisnd_0.memory, tutan_sound)) then
    exit;
  // cargar roms
  if not(roms_load(@memory_temp, tutan_rom)) then
    exit;
  copymemory(@memory[$A000], @memory_temp[0], $6000);
  for f := 0 to 8 do
    copymemory(@rom_bank[f, 0], @memory_temp[$6000 + (f * $1000)], $1000);
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $7B;
  marcade.dswa_val := @tutan_dip_a;
  marcade.dswb_val := @tutan_dip_b;
  // final
  reset_tutankham;
  start_tutankham := true;
end;

end.
