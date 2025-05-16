unit pooyan_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  konami_snd,
  sound_engine;

function start_pooyan: boolean;

implementation

const
  pooyan_rom: array [0 .. 3] of tipo_roms = ((n: '1.4a'; l: $2000; p: 0; crc: $BB319C63), (n: '2.5a'; l: $2000; p: $2000; crc: $A1463D98), (n: '3.6a'; l: $2000; p: $4000; crc: $FE1A9E08), (n: '4.7a'; l: $2000; p: $6000; crc: $9E0F9BCC));
  pooyan_pal: array [0 .. 2] of tipo_roms = ((n: 'pooyan.pr1'; l: $20; p: 0; crc: $A06A6D0E), (n: 'pooyan.pr2'; l: $100; p: $20; crc: $82748C0B), (n: 'pooyan.pr3'; l: $100; p: $120; crc: $8CD4CD60));
  pooyan_char: array [0 .. 1] of tipo_roms = ((n: '8.10g'; l: $1000; p: 0; crc: $931B29EB), (n: '7.9g'; l: $1000; p: $1000; crc: $BBE6D6E4));
  pooyan_sound: array [0 .. 1] of tipo_roms = ((n: 'xx.7a'; l: $1000; p: 0; crc: $FBE2B368), (n: 'xx.8a'; l: $1000; p: $1000; crc: $E1795B3D));
  pooyan_sprites: array [0 .. 1] of tipo_roms = ((n: '6.9a'; l: $1000; p: 0; crc: $B2D8C121), (n: '5.8a'; l: $1000; p: $1000; crc: $1097C2B6));
  // Dip
  pooyan_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Invalid')), ());
  pooyan_dip_b: array [0 .. 5] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '255')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: 8; name: 'Bonus Life'; number: 2; val2: (8, 0);
    name2: ('50K 80K+', '30K 70K+')), (mask: $70; name: 'Difficulty'; number: 8; val8: ($70, $60, $50, $40, $30, $20, $10, 0); name8: ('1 (Easy)', '2', '3', '4', '5', '6', '7', '8 (Hard)')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  nmi_vblank: boolean;
  last: byte;

procedure update_video_pooyan;
var
  f: word;
  x, y, color, nchar, atrib: byte;
  flipx, flipy: boolean;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory[$8000 + f];
      color := (atrib and $F) shl 4;
      nchar := memory[$8400 + f] + 8 * (atrib and $20);
      put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  for f := 0 to $17 do
  begin
    atrib := memory[$9410 + (f * 2)];
    nchar := memory[$9011 + (f * 2)] and $3F;
    color := (atrib and $F) shl 4;
    x := memory[$9411 + (f * 2)];
    y := memory[$9010 + (f * 2)];
    flipx := (atrib and $80) <> 0;
    flipy := (atrib and $40) = 0;
    if main_screen.flip_main_screen then
    begin
      x := 240 - x;
      y := 240 - y;
      flipx := not(flipx);
      flipy := not(flipy);
    end;
    put_gfx_sprite(nchar, color, flipx, flipy, 1);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_pooyan;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
  end;
end;

procedure pooyan_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 240 then
        begin
          if nmi_vblank then
            z80_0.change_nmi(ASSERT_LINE);
          update_video_pooyan;
        end;
        // Main CPU
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // SND CPU
        konamisnd_0.run;
      end;
      events_pooyan;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function pooyan_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $8FFF:
      pooyan_getbyte := memory[direccion];
    $9000 .. $9FFF:
      case (direccion and $7FF) of
        0 .. $3FF:
          pooyan_getbyte := memory[$9000 + (direccion and $FF)];
        $400 .. $7FF:
          pooyan_getbyte := memory[$9400 + (direccion and $FF)];
      end;
    $A000 .. $BFFF:
      case (direccion and $3FF) of
        0 .. $7F, $200 .. $27F:
          pooyan_getbyte := marcade.dswb;
        $80 .. $9F, $280 .. $29F:
          pooyan_getbyte := marcade.in0;
        $A0 .. $BF, $2A0 .. $2BF:
          pooyan_getbyte := marcade.in1;
        $C0 .. $DF, $2C0 .. $2DF:
          pooyan_getbyte := marcade.in2;
        $E0 .. $FF, $2E0 .. $2FF:
          pooyan_getbyte := marcade.dswa;
      end;
  end;
end;

procedure pooyan_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF:
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $8800 .. $8FFF:
      memory[direccion] := valor;
    $9000 .. $9FFF:
      case (direccion and $7FF) of
        0 .. $3FF:
          memory[$9000 + (direccion and $FF)] := valor;
        $400 .. $7FF:
          memory[$9400 + (direccion and $FF)] := valor;
      end;
    $A000 .. $BFFF:
      case (direccion and $3FF) of
        0 .. $7F, $200 .. $27F:
          ; // WatchDog
        $100 .. $17F, $300 .. $37F:
          konamisnd_0.sound_latch := valor;
        $180, $380:
          begin
            nmi_vblank := valor <> 0;
            if not(nmi_vblank) then
              z80_0.change_nmi(CLEAR_LINE);
          end;
        $181, $381:
          begin
            if ((last = 0) and (valor <> 0)) then
              konamisnd_0.pedir_irq := HOLD_LINE;
            last := valor;
          end;
        $182, $382:
          konamisnd_0.enabled := (valor = 0);
        $187, $387:
          main_screen.flip_main_screen := (valor and 1) = 0;
      end;
  end;
end;

// Main
procedure reset_pooyan;
begin
  z80_0.reset;
  frame_main := z80_0.tframes;
 reset_game_general;
  konamisnd_0.reset;
  nmi_vblank := false;
  last := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
end;

function start_pooyan: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $1FFF] of byte;
  rweights, gweights, bweights: array [0 .. 2] of single;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  resistances: array [0 .. 2] of integer = (1000, 470, 220);
begin
  machine_calls.general_loop := pooyan_loop;
  machine_calls.reset := reset_pooyan;
  start_pooyan := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 256);
  z80_0.change_ram_calls(pooyan_getbyte, pooyan_putbyte);
  // Sound Chip
konamisnd_0:=konamisnd_chip.create(2,TIPO_TIMEPLT,1789772,256);
  if not(roms_load(@konamisnd_0.memory, pooyan_sound)) then
    exit;
  // cargar roms
  if not(roms_load(@memory, pooyan_rom)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, pooyan_char)) then
    exit;
  init_gfx(0, 8, 8, 256);
  gfx_set_desc_data(4, 0, 16 * 8, $1000 * 8 + 4, $1000 * 8 + 0, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // convertir sprites
  if not(roms_load(@memory_temp, pooyan_sprites)) then
    exit;
  init_gfx(1, 16, 16, 64);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $1000 * 8 + 4, $1000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // poner la paleta
  if not(roms_load(@memory_temp, pooyan_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances, @rweights, 0, 0, 3, @resistances, @gweights, 0, 0, 2, @resistances[1], @bweights, 0, 0);
  for f := 0 to $1F do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    // green component
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // blue component
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $FF do
  begin
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
    gfx[0].colores[f] := (memory_temp[$120 + f] and $F) + $10;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $7B;
  marcade.dswa_val2 := @pooyan_dip_a;
  marcade.dswb_val2 := @pooyan_dip_b;
  // final
  reset_pooyan;
  start_pooyan := true;
end;

end.
