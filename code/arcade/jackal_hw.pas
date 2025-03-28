unit jackal_hw;

interface

uses
  WinApi.Windows,
  m6809,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine;

function start_jackal: boolean;

implementation

const
  jackal_rom: array [0 .. 1] of tipo_roms = ((n: 'j-v02.rom'; l: $10000; p: 0; crc: $0B7E0584), (n: 'j-v03.rom'; l: $4000; p: $10000; crc: $3E0DFB83));
  jackal_chars: array [0 .. 3] of tipo_roms = ((n: '631t04.bin'; l: $20000; p: 0; crc: $457F42F0), (n: '631t05.bin'; l: $20000; p: 1; crc: $732B3FC1), (n: '631t06.bin'; l: $20000; p: $40000; crc: $2D10E56E), (n: '631t07.bin'; l: $20000; p: $40001; crc: $4961C397));
  jackal_sound: tipo_roms = (n: '631t01.bin'; l: $8000; p: $8000; crc: $B189AF6A);
  jackal_proms: array [0 .. 1] of tipo_roms = ((n: '631r08.bpr'; l: $100; p: 0; crc: $7553A172), (n: '631r09.bpr'; l: $100; p: $100; crc: $A74DD86C));
  // Dip
  jackal_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'No Coin B')), ());
  jackal_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('2', '3', '4', '7')), (mask: $18; name: 'Bonus Life'; number: 4; val4: ($18, $10, 8, 0); name4: ('30K 150K', '50K 200K', '30K', '50K')), (mask: $60; name: 'Difficulty';
    number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Normal', 'Difficult', 'Very Difficult')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());
  jackal_dip_c: array [0 .. 3] of def_dip2 = ((mask: $20; name: 'Flip Screen'; number: 2; val2: ($20, 0); name2: ('Off', 'On')), (mask: $40; name: 'Sound Adjustment'; number: 2; val2: (0, $40); name2: ('Upright', 'Cocktail')), (mask: $80; name: 'Sound Mode'; number: 2;
    val2: ($80, 0); name2: ('Mono', 'Stereo')), ());

var
  memory_rom: array [0 .. 1, 0 .. $7FFF] of byte;
  memory_zram: array [0 .. 1, 0 .. $3F] of word;
  memory_sprite, memory_voram: array [0 .. 1, 0 .. $FFF] of byte;
  banco, scroll_x, scroll_y, scroll_crt, sprite_crt, ram_bank, sprite_bank: byte;
  irq_enable: boolean;

procedure update_video_jackal;
  procedure draw_sprites(bank: byte; pos: word);
  var
    sn1, sn2, attr, a, b, c, d, flipx_v, flipy_v: byte;
    flipx, flipy: boolean;
    nchar, color, x, y: word;
  begin
    sn1 := memory_sprite[bank, pos];
    sn2 := memory_sprite[bank, pos + 1];
    attr := memory_sprite[bank, pos + 4];
    x := 240 - (memory_sprite[bank, pos + 2]);
    y := memory_sprite[bank, pos + 3] + ((attr and 1) shl 8);
    flipy := (attr and $20) <> 0;
    flipx := (attr and $40) <> 0;
    flipy_v := (attr and $20) shr 1;
    flipx_v := (attr and $40) shr 2;
    color := (sn2 and $F0) + (bank * $100);
    if (attr and $C) <> 0 then
    begin // half-size sprite
      nchar := (sn1 * 4 + ((sn2 and (8 + 4)) shr 2) + ((sn2 and (2 + 1)) shl 10)) + (bank * 4096);
      case (attr and $C) of
        4:
          begin
            put_gfx_sprite_diff(nchar, color, flipx, flipy, 1, 8, 0);
            put_gfx_sprite_diff(nchar + 1, color, flipx, flipy, 1, 8, 8);
            actualiza_gfx_sprite_size(x, y, 2, 16, 16);
          end;
        8:
          begin
            put_gfx_sprite_diff(nchar, color, flipx, flipy, 1, 0, 0);
            put_gfx_sprite_diff(nchar - 2, color, flipx, flipy, 1, 8, 0);
            actualiza_gfx_sprite_size(x, y, 2, 16, 8);
          end;
        $C:
          begin
            put_gfx_sprite(nchar, color, flipx, flipy, 1);
            update_gfx_sprite(x + 8, y, 2, 1);
          end;
      end;
    end
    else
    begin
      nchar := (sn1 + ((sn2 and 3) shl 8)) + (bank * 1024);
      if (attr and $10) <> 0 then
      begin
        a := 16 xor flipx_v;
        b := 0 xor flipx_v;
        c := 0 xor flipy_v;
        d := 16 xor flipy_v;
        put_gfx_sprite_diff(nchar, color, flipx, flipy, 2, a, c);
        put_gfx_sprite_diff(nchar + 1, color, flipx, flipy, 2, a, d);
        put_gfx_sprite_diff(nchar + 2, color, flipx, flipy, 2, b, c);
        put_gfx_sprite_diff(nchar + 3, color, flipx, flipy, 2, b, d);
        actualiza_gfx_sprite_size(x - 16, y, 2, 32, 32);
      end
      else
      begin
        put_gfx_sprite(nchar, color, flipx, flipy, 2);
        update_gfx_sprite(x, y, 2, 2);
      end;
    end;
  end;

var
  x, y, f, nchar: word;
  atrib: byte;
begin
  // background
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory_voram[ram_bank, f];
      nchar := memory_voram[ram_bank, $400 + f] + ((atrib and $C0) shl 2) + ((atrib and $30) shl 6);
      put_gfx_flip(x * 8, y * 8, nchar, 0, 1, 0, (atrib and $20) <> 0, (atrib and $10) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  // scroll de varios tipos...
  if (scroll_crt and 2) <> 0 then
  begin
    // horizontal 8 lineas independientes
    // eje X
    if (scroll_crt and 4) <> 0 then
      scroll__x_part2(1, 2, 8, @memory_zram[ram_bank]);
    // Eje Y
    if (scroll_crt and 8) <> 0 then
      scroll__y_part2(1, 2, 8, @memory_zram[ram_bank]);
  end
  else
  begin
    // Scroll total
    scroll_x_y(1, 2, scroll_x, scroll_y);
  end;
  // sprites
  for f := 0 to 48 do
    draw_sprites(1, (sprite_crt and 8) * $100 + f * 5);
  for f := 0 to $FF do
    draw_sprites(0, (sprite_crt and 8) * $100 + f * 5);
  update_final_piece(16, 8, 224, 240, 2);
end;

procedure events_jackal;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // SYSTEM
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

procedure jackal_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 255 do
      begin
        if f = 240 then
        begin
          if irq_enable then
          begin
            m6809_0.change_irq(HOLD_LINE);
            m6809_1.change_nmi(PULSE_LINE);
          end;
          update_video_jackal;
        end;
        m6809_0.run(frame_main);
        frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
        m6809_1.run(frame_snd);
        frame_snd := frame_snd + m6809_1.tframes - m6809_1.contador;
      end;
      events_jackal;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function jackal_getbyte(direccion: word): byte;
begin
  case direccion of
    $10:
      jackal_getbyte := marcade.dswa; // dsw1
    $11:
      jackal_getbyte := marcade.in1;
    $12:
      jackal_getbyte := marcade.in2;
    $13:
      jackal_getbyte := marcade.in0 + marcade.dswc;
    $14, $15:
      ; // Torreta
    $18:
      jackal_getbyte := marcade.dswb; // dsw2
    $20 .. $5F:
      jackal_getbyte := 255 - memory_zram[ram_bank, direccion - $20];
    $60 .. $1FFF, $C000 .. $FFFF:
      jackal_getbyte := memory[direccion];
    $2000 .. $2FFF:
      jackal_getbyte := memory_voram[ram_bank, direccion and $FFF];
    $3000 .. $3FFF:
      jackal_getbyte := memory_sprite[sprite_bank, direccion and $FFF];
    $4000 .. $BFFF:
      jackal_getbyte := memory_rom[banco, direccion - $4000];
  end;
end;

procedure jackal_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0:
      scroll_x := not(valor);
    1:
      scroll_y := valor;
    2:
      scroll_crt := valor;
    $3:
      sprite_crt := valor;
    4:
      begin
        irq_enable := (valor and 2) <> 0;
        main_screen.flip_main_screen := (valor and 8) <> 0;
      end;
    $1C:
      begin
        banco := (valor and $20) shr 5;
        ram_bank := (valor and $10) shr 4;
        sprite_bank := (valor and 8) shr 3;
      end;
    $20 .. $5F:
      memory_zram[ram_bank, direccion - $20] := 255 - valor;
    $60 .. $1FFF:
      memory[direccion] := valor;
    $2000 .. $2FFF:
      if memory_voram[ram_bank, direccion and $FFF] <> valor then
      begin
        memory_voram[ram_bank, direccion and $FFF] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $3000 .. $3FFF:
      memory_sprite[sprite_bank, direccion and $FFF] := valor;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function sound_getbyte(direccion: word): byte;
begin
  case direccion of
    $2001:
      sound_getbyte := ym2151_0.status;
    $4000 .. $43FF:
      sound_getbyte := buffer_paleta[direccion and $3FF];
    $6000 .. $7FFF:
      sound_getbyte := memory[direccion and $1FFF];
    $8000 .. $FFFF:
      sound_getbyte := mem_snd[direccion];
  end;
end;

procedure sound_putbyte(direccion: word; valor: byte);
  procedure change_color(dir: word);
  var
    data: word;
    color: tcolor;
  begin
    data := buffer_paleta[dir] + (buffer_paleta[dir + 1] shl 8);
    color.r := pal5bit(data);
    color.g := pal5bit(data shr 5);
    color.b := pal5bit(data shr 10);
    dir := dir shr 1;
    set_pal_color(color, dir);
    // Si el color es de los chars, reseteo todo el buffer, al tener 8 bits de profundidad de
    // color no cambia en cuando lo pinta
    if ((dir > $FF) and (dir < $200)) then
      fillchar(gfx[0].buffer, $400, 1);
  end;

begin
  case direccion of
    $2000:
      ym2151_0.reg(valor);
    $2001:
      ym2151_0.write(valor);
    $4000 .. $43FF:
      if buffer_paleta[direccion and $3FF] <> valor then
      begin
        buffer_paleta[direccion and $3FF] := valor;
        change_color(direccion and $3FE);
      end;
    $6000 .. $7FFF:
      memory[direccion and $1FFF] := valor;
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

procedure sound_instruccion;
begin
  ym2151_0.update;
end;

// Main
procedure reset_jackal;
begin
  m6809_0.reset;
  m6809_1.reset;
  frame_main := m6809_0.tframes;
  frame_snd := m6809_1.tframes;
  ym2151_0.reset;
  reset_video;
  reset_audio;
  marcade.in0 := $1F;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_enable := false;
  ram_bank := 0;
  sprite_bank := 0;
  scroll_x := 0;
  scroll_y := 0;
  scroll_crt := 0;
  sprite_crt := 0;
end;

function start_jackal: boolean;
var
  f: word;
  memory_temp: array [0 .. $7FFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 32 * 8 + 0 * 4, 32 * 8 + 1 * 4, 32 * 8 + 2 * 4, 32 * 8 + 3 * 4, 32 * 8 + 4 * 4, 32 * 8 + 5 * 4, 32 * 8 + 6 * 4, 32 * 8 + 7 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 16 * 32, 17 * 32, 18 * 32, 19 * 32, 20 * 32, 21 * 32, 22 * 32, 23 * 32);
begin
  machine_calls.general_loop := jackal_loop;
  machine_calls.reset := reset_jackal;
  start_jackal := false;
  start_audio(true);
  // Pantallas
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 512, false, true);
  start_video(224, 240);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 12, 256, TCPU_MC6809E);
  m6809_0.change_ram_calls(jackal_getbyte, jackal_putbyte);
  // Sound CPU
  m6809_1 := cpu_m6809.Create(18432000 div 12, 256, TCPU_MC6809E);
  m6809_1.change_ram_calls(sound_getbyte, sound_putbyte);
  m6809_1.init_sound(sound_instruccion);
  // Audio chips
  ym2151_0 := ym2151_Chip.Create(3579545, 0.5);
  // cargar roms
  if not(roms_load(@memory_temp, jackal_rom)) then
    exit;
  // Pongo las ROMs en su banco
  copymemory(@memory[$C000], @memory_temp[$10000], $4000);
  copymemory(@memory_rom[0, 0], @memory_temp[0], $8000);
  copymemory(@memory_rom[1, 0], @memory_temp[$8000], $8000);
  // Cargar Sound
  if not(roms_load(@mem_snd, jackal_sound)) then
    exit;
  // convertir chars
  if not(roms_load16b(@memory_temp, jackal_chars)) then
    exit;
  init_gfx(0, 8, 8, 4096);
  gfx_set_desc_data(8, 0, 32 * 8, 0, 1, 2, 3, $40000 * 8 + 0, $40000 * 8 + 1, $40000 * 8 + 2, $40000 * 8 + 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // sprites
  init_gfx(1, 8, 8, 4096 * 2);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 2, 32 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp[$20000], @ps_x, @ps_y, true, false);
  convert_gfx(1, 4096 * 8 * 8, @memory_temp[$60000], @ps_x, @ps_y, true, false);
  // sprites x2
  init_gfx(2, 16, 16, 1024 * 2);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 2, 32 * 32, 0, 1, 2, 3);
  convert_gfx(2, 0, @memory_temp[$20000], @ps_x, @ps_y, true, false);
  convert_gfx(2, 1024 * 16 * 16, @memory_temp[$60000], @ps_x, @ps_y, true, false);
  // Color lookup chars
  for f := 0 to $FF do
    gfx[0].colores[f] := f or $100;
  // Color lookup sprites
  if not(roms_load(@memory_temp, jackal_proms)) then
    exit;
  // 0..$ff --> banco 0 --> valor
  // $100..$1ff --> banco 1 --> valor+$10
  for f := 0 to $1FF do
    gfx[1].colores[f] := memory_temp[f] and $F + ((f shr 8) * $10);
  // sprites x2
  copymemory(@gfx[2].colores, @gfx[1].colores, $200 * 2);
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $5F;
  marcade.dswc := $20;
  marcade.dswa_val2 := @jackal_dip_a;
  marcade.dswb_val2 := @jackal_dip_b;
  marcade.dswc_val2 := @jackal_dip_c;
  // final
  reset_jackal;
  start_jackal := true;
end;

end.
