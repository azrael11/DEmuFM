unit foodfight_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  file_engine,
  pokey;

function start_foodfight: boolean;

implementation

uses
  uDataModule;

const
  foodf_rom: array [0 .. 7] of tipo_roms = ((n: '136020-301.8c'; l: $2000; p: 1; crc: $DFC3D5A8), (n: '136020-302.9c'; l: $2000; p: 0; crc: $EF92DC5C), (n: '136020-303.8d'; l: $2000; p: $4001; crc: $64B93076), (n: '136020-204.9d'; l: $2000; p: $4000; crc: $EA596480),
    (n: '136020-305.8e'; l: $2000; p: $8001; crc: $E6CFF1B1), (n: '136020-306.9e'; l: $2000; p: $8000; crc: $95159A3E), (n: '136020-307.8f'; l: $2000; p: $C001; crc: $17828DBB), (n: '136020-208.9f'; l: $2000; p: $C000; crc: $608690C9));
  foodf_char: tipo_roms = (n: '136020-109.6lm'; l: $2000; p: 0; crc: $C13C90EB);
  foodf_sprites: array [0 .. 1] of tipo_roms = ((n: '136020-110.4e'; l: $2000; p: 0; crc: $8870E3D6), (n: '136020-111.4d'; l: $2000; p: $2000; crc: $84372EDF));
  foodf_nvram: tipo_roms = (n: 'foodf.nv'; l: $100; p: 0; crc: $A4186B13);
  // DIP
  foodf_dip: array [0 .. 4] of def_dip2 = ((mask: 7; name: 'Bonus Coins'; number: 8; val8: (0, 5, 2, 1, 6, 3, 4, 7); name8: ('None', '1 for every 2', '1 for every 4', '1 for every 5', '2 for every 4', 'Invalid', 'Invalid', 'Invalid')), (mask: 8; name: 'Coin A'; number: 2;
    val2: (0, 8); name2: ('1C 1C', '1C 2C')), (mask: $30; name: 'Coin B'; number: 4; val4: (0, $20, $10, $30); name4: ('1C 1C', '1C 4C', '1C 5C', '1C 6C')), (mask: $C0; name: 'Coinage'; number: 4; val4: ($80, 0, $C0, $40); name4: ('2C 1C', '1C 1C', '1C 2C', 'FreePlay')), ());

var
  rom: array [0 .. $FFFF] of word;
  ram, ram2: array [0 .. $7FF] of word;
  sprite_ram: array [0 .. $7F] of word;
  bg_ram: array [0 .. $3FF] of word;
  nvram: array [0 .. $FF] of byte;
  rweights, gweights, bweights: array [0 .. 2] of single;
  analog_data: array [0 .. 7] of byte;
  analog_select: byte;

procedure update_video_foodf;
  procedure draw_sprites(prio: byte);
  var
    color, atrib, atrib2: word;
    nchar, x, y, f, pri: byte;
  begin
    for f := $10 to $3F do
    begin
      atrib := sprite_ram[f * 2];
      pri := (atrib shr 13) and 1;
      if pri <> prio then
        continue;
      atrib2 := sprite_ram[(f * 2) + 1];
      nchar := atrib and $FF;
      color := ((atrib shr 8) and $1F) shl 2;
      x := (atrib2 shr 8) and $FF;
      y := ($FF - atrib2 - 16) and $FF;
      put_gfx_sprite(nchar, color, ((atrib shr 15) and 1) <> 0, ((atrib shr 14) and 1) <> 0, 1);
      update_gfx_sprite(x, y, 2, 1);
    end;
  end;

var
  f, nchar, atrib: word;
  x, y, color: byte;
begin
  for f := 0 to $3FF do
  begin
    atrib := bg_ram[f];
    color := (atrib shr 8) and $3F;
    if ((gfx[0].buffer[f]) or (buffer_color[color])) then
    begin
      x := (f shr 5) + 1;
      y := f and $1F;
      nchar := (atrib and $FF) or ((atrib shr 7) and $100);
      put_gfx((x * 8) and $FF, y * 8, nchar, color shl 2, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  draw_sprites(0);
  draw_sprites(1);
  update_final_piece(0, 0, 256, 224, 2);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_foodf;
begin
  if main_vars.service1 then
    marcade.in0 := (marcade.in0 and $FF7F)
  else
    marcade.in0 := (marcade.in0 or $80);
  if event.arcade then
  begin
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
  end;
end;

procedure foodf_loop;
var
  f: word;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 258 do
      begin
        case f of
          0, 64, 128, 192:
            m68000_0.irq[1] := ASSERT_LINE;
          224:
            begin
              m68000_0.irq[2] := ASSERT_LINE;
              update_video_foodf;
            end;
        end;
        // main
        m68000_0.run(frame_main);
        frame_main := frame_main + m68000_0.tframes - m68000_0.contador;
      end;
      analog_data[1] := analog.c[0].y[0];
      analog_data[5] := analog.c[0].x[0];
    end
    else
      pause_action;
  end;
end;

function foodf_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFFF:
      case (direccion and $1FFFF) of
        0 .. $FFFF:
          foodf_getword := rom[direccion shr 1];
        $14000 .. $17FFF:
          foodf_getword := ram[(direccion and $FFF) shr 1];
        $18000 .. $1BFFF:
          foodf_getword := ram2[(direccion and $FFF) shr 1];
        $1C000 .. $1FFFF:
          foodf_getword := sprite_ram[(direccion and $FF) shr 1];
      end;
    $800000 .. $83FFFF:
      foodf_getword := bg_ram[(direccion and $7FF) shr 1];
    $900000 .. $93FFFF:
      foodf_getword := nvram[(direccion and $1FF) shr 1] and $FF;
    $940000 .. $97FFFF:
      case (direccion and $1FFFF) of
        0 .. $3FFF:
          foodf_getword := $FF00 + analog_data[analog_select and $7];
        $8000 .. $BFFF:
          foodf_getword := marcade.in0;
        $18000 .. $1BFFF:
          foodf_getword := $FFFF;
      end;
    $A40000 .. $A7FFFF:
      foodf_getword := pokey_1.read((direccion and $1F) shr 1);
    $A80000 .. $ABFFFF:
      foodf_getword := pokey_0.read((direccion and $1F) shr 1);
    $AC0000 .. $AFFFFF:
      foodf_getword := pokey_2.read((direccion and $1F) shr 1);
  end;
end;

procedure foodf_putword(direccion: dword; valor: word);

  procedure change_color(pos, data: word);
  var
    color: tcolor;
    bit0, bit1, bit2: byte;
  begin
    bit0 := (data shr 0) and 1;
    bit1 := (data shr 1) and 1;
    bit2 := (data shr 2) and 1;
    color.r := combine_3_weights(@rweights[0], bit0, bit1, bit2);
    bit0 := (data shr 3) and 1;
    bit1 := (data shr 4) and 1;
    bit2 := (data shr 5) and 1;
    color.g := combine_3_weights(@gweights[0], bit0, bit1, bit2);
    bit0 := (data shr 6) and 1;
    bit1 := (data shr 7) and 1;
    color.b := combine_2_weights(@bweights[0], bit0, bit1);
    set_pal_color(color, pos);
    if pos < 64 then
      buffer_color[pos] := true;
  end;

begin
  case direccion of
    0 .. $3FFFFF:
      case (direccion and $1FFFF) of
        0 .. $FFFF:
          ; // ROM
        $14000 .. $17FFF:
          ram[(direccion and $FFF) shr 1] := valor;
        $18000 .. $1BFFF:
          ram2[(direccion and $FFF) shr 1] := valor;
        $1C000 .. $1FFFF:
          sprite_ram[(direccion and $FF) shr 1] := valor;
      end;
    $800000 .. $83FFFF:
      if bg_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $7FF) shr 1] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $900000 .. $93FFFF:
      nvram[(direccion and $1FF) shr 1] := valor and $FF;
    $940000 .. $97FFFF:
      case (direccion and $1FFFF) of
        $4000 .. $7FFF:
          analog_select := (direccion and 7) xor 3;
        $8000 .. $BFFF:
          begin
            if (valor and 4) = 0 then
              m68000_0.irq[1] := CLEAR_LINE;
            if (valor and 8) = 0 then
              m68000_0.irq[2] := CLEAR_LINE;
          end;
        $10000 .. $13FFF:
          change_color((direccion and $1FF) shr 1, valor);
        $14000:
          ; // read nvram recall
      end;
    $A40000 .. $A7FFFF:
      pokey_1.write((direccion and $1F) shr 1, valor and $FF);
    $A80000 .. $ABFFFF:
      pokey_0.write((direccion and $1F) shr 1, valor and $FF);
    $AC0000 .. $AFFFFF:
      pokey_2.write((direccion and $1F) shr 1, valor and $FF);
  end;
end;

function foodf_pot_r(pot: byte): byte;
begin
  foodf_pot_r := (marcade.dswa shr pot) shl 7;
end;

procedure foodf_sound_update;
begin
  pokey_0.update;
  pokey_1.update;
  pokey_2.update;
end;

// Main
procedure reset_foodf;
begin
  m68000_0.reset;
  frame_main := m68000_0.tframes;
  reset_analog;
  pokey_0.reset;
  pokey_1.reset;
  pokey_2.reset;
  reset_video;
  reset_audio;
  marcade.in0 := $FFFF;
  analog_select := 0;
  fillchar(analog_data[0], 8, $FF);
end;

procedure close_foodf;
begin
  write_file(dm.tConfignvram.AsString + 'foodf.nv', @nvram, $100);
end;

function start_foodfight: boolean;
var
  memory_temp: array [0 .. $3FFFF] of byte;
  longitud: integer;
const
  pc_x: array [0 .. 7] of dword = (8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  ps_x: array [0 .. 15] of dword = (8 * 16 + 0, 8 * 16 + 1, 8 * 16 + 2, 8 * 16 + 3, 8 * 16 + 4, 8 * 16 + 5, 8 * 16 + 6, 8 * 16 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  resistances: array [0 .. 2] of integer = (1000, 470, 220);
begin
  start_foodfight := false;
  machine_calls.general_loop := foodf_loop;
  machine_calls.close := close_foodf;
  machine_calls.reset := reset_foodf;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256, true);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(12096000 div 2, 259);
  m68000_0.change_ram16_calls(foodf_getword, foodf_putword);
  m68000_0.init_sound(foodf_sound_update);
  if not(roms_load16w(@rom, foodf_rom)) then
    exit;
  // Init Analog
  init_analog(m68000_0.numero_cpu, m68000_0.clock);
  analog_0(100, 10, $7F, $FF, 0, true);
  // Sound Chips
  pokey_0 := pokey_chip.create(trunc(12096000 / 2 / 10));
  pokey_0.change_all_pot(foodf_pot_r);
  pokey_1 := pokey_chip.create(trunc(12096000 / 2 / 10));
  pokey_2 := pokey_chip.create(trunc(12096000 / 2 / 10));
  // convertir chars
  if not(roms_load(@memory_temp, foodf_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx_set_desc_data(2, 0, 8 * 16, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, foodf_sprites)) then
    exit;
  init_gfx(1, 16, 16, $100);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(2, 0, 8 * 32, $100 * 8 * 32, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // paleta
  compute_resistor_weights(0, 255, -1.0, 3, @resistances[0], @rweights, 0, 0, 3, @resistances[0], @gweights, 0, 0, 2, @resistances[1], @bweights, 0, 0);
  // DIP
  marcade.dswa := 0;
  marcade.dswa_val2 := @foodf_dip;
  // NVRAM
  if read_file_size(dm.tConfignvram.AsString + 'foodf.nv', longitud) then
    read_file(dm.tConfignvram.AsString + 'foodf.nv', @nvram, longitud)
  else if not(roms_load(@nvram, foodf_nvram)) then
    exit;
  // final
  reset_foodf;
  start_foodfight := true;
end;

end.
