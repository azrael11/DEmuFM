unit magmax_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  rom_engine,
  pal_engine,
  sound_engine;

function start_magmax: boolean;

implementation

const
  magmax_rom: array [0 .. 5] of tipo_roms = ((n: '1.3b'; l: $4000; p: 1; crc: $33793CBB), (n: '6.3d'; l: $4000; p: 0; crc: $677EF450), (n: '2.5b'; l: $4000; p: $8001; crc: $1A0C84DF), (n: '7.5d'; l: $4000; p: $8000; crc: $01C35E95), (n: '3.6b'; l: $2000; p: $10001;
    crc: $D06E6CAE), (n: '8.6d'; l: $2000; p: $10000; crc: $790A82BE));
  magmax_sound: array [0 .. 1] of tipo_roms = ((n: '15.17b'; l: $2000; p: 0; crc: $19E7B983), (n: '16.18b'; l: $2000; p: $2000; crc: $055E3126));
  magmax_char: tipo_roms = (n: '23.15g'; l: $2000; p: 0; crc: $A7471DA2);
  magmax_sprites: array [0 .. 5] of tipo_roms = ((n: '17.3e'; l: $2000; p: 0; crc: $8E305B2E), (n: '18.5e'; l: $2000; p: $2000; crc: $14C55A60), (n: '19.6e'; l: $2000; p: $4000; crc: $FA4141D8), (n: '20.3g'; l: $2000; p: $8000; crc: $6FA3918B), (n: '21.5g'; l: $2000; p: $A000;
    crc: $DD52EDA4), (n: '22.6g'; l: $2000; p: $C000; crc: $4AFC98FF));
  magmax_pal: array [0 .. 5] of tipo_roms = ((n: 'mag_e.10f'; l: $100; p: 0; crc: $75E4F06A), (n: 'mag_d.10e'; l: $100; p: $100; crc: $34B6A6E3), (n: 'mag_a.10d'; l: $100; p: $200; crc: $A7EA7718), (n: 'mag_g.2e'; l: $100; p: $300; crc: $830BE358), (n: 'mag_b.14d'; l: $100;
    p: $400; crc: $A0FB7297), (n: 'mag_c.15d'; l: $100; p: $500; crc: $D84A6F78));
  magmax_fondo1: array [0 .. 1] of tipo_roms = ((n: '4.18b'; l: $2000; p: 0; crc: $1550942E), (n: '5.20b'; l: $2000; p: 1; crc: $3B93017F));
  magmax_fondo2: array [0 .. 5] of tipo_roms = ((n: '9.18d'; l: $2000; p: $4000; crc: $9ECC9AB8), (n: '10.20d'; l: $2000; p: $6000; crc: $E2FF7293), (n: '11.15f'; l: $2000; p: $8000; crc: $91F3EDB6), (n: '12.17f'; l: $2000; p: $A000; crc: $99771EFF), (n: '13.18f'; l: $2000;
    p: $C000; crc: $75F30159), (n: '14.20f'; l: $2000; p: $E000; crc: $96BABCBA));
  magmax_dip: array [0 .. 9] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '6')), (mask: $C; name: 'Bonus Life'; number: 4; val4: ($C, 8, 4, 0); name4: ('30K 80K 50K+', '50K 120K 70K+', '70K 160K 90K+', '90K 200K 110K+')),
    (mask: $10; name: 'Demo Sounds'; number: 2; val2: (0, $10); name2: ('Off', 'On')), (mask: $20; name: 'Cabinet'; number: 2; val2: (0, $20); name2: ('Upright', 'Cocktail')), (mask: $300; name: 'Coin A'; number: 4; val4: ($100, $300, $200, 0);
    name4: ('2C 1C', '1C 1C', '1C 2C', 'Free Play')), (mask: $C00; name: 'Coin B'; number: 4; val4: (0, $400, $C00, $800); name4: ('3C 1C', '2C 3C', '1C 3C', '1C 6C')), (mask: $1000; name: 'Difficulty'; number: 2; val2: ($1000, 0); name2: ('Easy', 'Hard')), (mask: $2000;
    name: 'Flip Screen'; number: 2; val2: ($2000, 0); name2: ('Off', 'On')), (mask: $8000; name: 'Debug Mode'; number: 2; val2: ($8000, 0); name2: ('No', 'Yes')), ());

var
  gain_control, vreg, scroll_x, scroll_y: word;
  rom: array [0 .. $9FFF] of word;
  ram: array [0 .. $7FF] of word;
  ram_video: array [0 .. $3FF] of word;
  prom_mem, ram_sprites: array [0 .. $FF] of word;
  LS74_q, LS74_clr, sound_latch: byte;
  rom18B: array [0 .. $FFFF] of byte;
  redraw_bg: boolean;

procedure update_video_magmax;
  procedure draw_background;
  var
    prom_data, pen_base, scroll_h, map_v_scr_100, rom18D_addr, rom15F_addr, map_v_scr_1fe_6, LS283: word;
    scroll_v, f, h, graph_color, graph_data: byte;
    pixel1, pixel2: array [0 .. $FF] of word;
  begin
    scroll_h := scroll_x and $3FFF;
    scroll_v := scroll_y;
    for f := 16 to 239 do
    begin // only for visible area
      fillword(@pixel1, $100, paleta[$100]);
      fillword(@pixel2, $100, paleta[MAX_COLORS]);
      map_v_scr_100 := (scroll_v + f) and $100;
      rom18D_addr := ((scroll_v + f) and $F8) + (map_v_scr_100 shl 5);
      rom15F_addr := (((scroll_v + f) and 7) shl 2) + (map_v_scr_100 shl 5);
      map_v_scr_1fe_6 := ((scroll_v + f) and $1FE) shl 6;
      pen_base := $20 + (map_v_scr_100 shr 1);
      for h := 0 to $FF do
      begin
        LS283 := scroll_h + h;
        if (map_v_scr_100 = 0) then
        begin
          if (h and $80) <> 0 then
            LS283 := LS283 + (rom18B[map_v_scr_1fe_6 + (h xor $FF)] xor $FF)
          else
            LS283 := LS283 + rom18B[map_v_scr_1fe_6 + h] + $FF01;
        end;
        prom_data := prom_mem[(LS283 shr 6) and $FF];
        rom18D_addr := rom18D_addr and $20F8;
        rom18D_addr := rom18D_addr + ((prom_data and $1F00) + ((LS283 and $38) shr 3));
        rom15F_addr := rom15F_addr and $201C;
        rom15F_addr := rom15F_addr + ((rom18B[$4000 + rom18D_addr] shl 5) + ((LS283 and 6) shr 1));
        rom15F_addr := rom15F_addr + (prom_data and $4000);
        graph_color := prom_data and $70;
        graph_data := rom18B[$8000 + rom15F_addr];
        if ((LS283 and 1) <> 0) then
          graph_data := graph_data shr 4;
        graph_data := graph_data and $F;
        pixel1[h] := paleta[pen_base + graph_color + graph_data];
        // priority: background over sprites
        if ((map_v_scr_100 <> 0) and ((graph_data and $C) = $C)) then
          pixel2[h] := pixel1[h];
      end;
      putpixel(0, f, 256, @pixel1, 1);
      putpixel(0, f, 256, @pixel2, 2);
    end;
    redraw_bg := false;
  end;

var
  f, x, nchar: word;
  color, y, atrib: byte;
begin
  for f := 0 to $3FF do
  begin
    if gfx[1].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := ram_video[f] and $FF;
      put_gfx_trans(x * 8, y * 8, nchar, 0, 3, 0);
      gfx[1].buffer[f] := false;
    end;
  end;
  if (vreg and $40) <> 0 then
    fill_full_screen(4, $100)
  else
  begin
    if redraw_bg then
      draw_background;
    update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  end;
  // sprites
  for f := 0 to $3F do
  begin
    y := ram_sprites[f * 4];
    if y <> 0 then
    begin
      y := 239 - y;
      atrib := ram_sprites[(f * 4) + 2];
      nchar := ram_sprites[(f * 4) + 1] and $FF;
      if (nchar and $80) <> 0 then
        nchar := nchar + ((vreg and $30) * 8);
      x := (ram_sprites[(f * 4) + 3] and $FF) - $80 + $100 * (atrib and 1);
      color := atrib and $F0;
      put_gfx_sprite_mask(nchar, color, (atrib and 4) <> 0, (atrib and 8) <> 0, 1, $1F, $1F);
      update_gfx_sprite(x, y, 4, 1);
    end;
  end;
  if (vreg and $40) = 0 then
    update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 4);
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 4);
  update_final_piece(0, 16, 256, 224, 4);
end;

procedure events_magmax;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FFFE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FFFD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FFFB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FFF7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $FFEF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $FFDF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // SYSTEM
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or 8);
  end;
end;

procedure magmax_loop;
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
        case f of
          64, 192:
            if (LS74_clr <> 0) then
              LS74_q := 1;
          240:
            begin
              update_video_magmax;
              m68000_0.irq[1] := ASSERT_LINE;
            end;
        end;
        // main
        m68000_0.run(frame_main);
        frame_main := frame_main + m68000_0.tframes - m68000_0.contador;
        // sound
        z80_0.run(frame_snd);
        frame_snd := frame_snd + z80_0.tframes - z80_0.contador;
      end;
      events_magmax;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function magmax_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $13FFF:
      magmax_getword := rom[direccion shr 1];
    $18000 .. $18FFF:
      magmax_getword := ram[(direccion and $FFF) shr 1];
    $20000 .. $207FF:
      magmax_getword := ram_video[(direccion and $7FF) shr 1];
    $28000 .. $281FF:
      magmax_getword := ram_sprites[(direccion and $1FF) shr 1];
    $30000:
      magmax_getword := marcade.in1;
    $30002:
      magmax_getword := marcade.in2;
    $30004:
      magmax_getword := marcade.in0;
    $30006:
      magmax_getword := marcade.dswa;
  end;
end;

procedure magmax_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $13FFF:
      ;
    $18000 .. $18FFF:
      ram[(direccion and $FFF) shr 1] := valor;
    $20000 .. $207FF:
      if ram_video[(direccion and $7FF) shr 1] <> valor then
      begin
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
        ram_video[(direccion and $7FF) shr 1] := valor;
      end;
    $28000 .. $281FF:
      ram_sprites[(direccion and $1FF) shr 1] := valor;
    $30010:
      vreg := valor;
    $30012:
      if scroll_x <> valor then
      begin
        redraw_bg := true;
        scroll_x := valor;
      end;
    $30014:
      if scroll_y <> valor then
      begin
        redraw_bg := true;
        scroll_y := valor
      end;
    $3001C:
      begin
        sound_latch := valor;
        z80_0.change_irq(ASSERT_LINE);
      end;
    $3001E:
      m68000_0.irq[1] := CLEAR_LINE;
  end;
end;

function magmax_snd_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FFF:
      magmax_snd_getbyte := mem_snd[direccion];
    $4000 .. $5FFF:
      z80_0.change_irq(CLEAR_LINE);
    $6000 .. $7FFF:
      magmax_snd_getbyte := mem_snd[$6000 + (direccion and $7FF)];
  end;
end;

procedure magmax_snd_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FFF:
      ;
    $4000 .. $5FFF:
      z80_0.change_irq(CLEAR_LINE);
    $6000 .. $7FFF:
      mem_snd[$6000 + (direccion and $7FF)] := valor;
  end;
end;

function magmax_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      magmax_snd_inbyte := ay8910_0.Read;
    2:
      magmax_snd_inbyte := ay8910_1.Read;
    4:
      magmax_snd_inbyte := ay8910_2.Read;
    6:
      magmax_snd_inbyte := (sound_latch shl 1) or LS74_q;
  end;
end;

procedure magmax_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ay8910_0.Control(valor);
    1:
      ay8910_0.Write(valor);
    2:
      ay8910_1.Control(valor);
    3:
      ay8910_1.Write(valor);
    4:
      ay8910_2.Control(valor);
    5:
      ay8910_2.Write(valor);
  end;
end;

procedure magmax_sound_update;
begin
  ay8910_0.Update;
  ay8910_1.Update;
  ay8910_2.Update;
end;

procedure magmax_porta_w(valor: byte);
var
  gain_t1, gain_t2: single;
begin
  if gain_control = (valor and $F) then
    exit;
  gain_control := valor and $F;
  gain_t1 := 0.5 + 0.5 * (gain_control and 1);
  gain_t2 := 0.23 + 0.23 * (gain_control and 1);
  ay8910_0.change_gain(gain_t1, gain_t2, gain_t2);
  gain_t1 := 0.23 + 0.23 * ((gain_control and 4) shr 2);
  ay8910_1.change_gain(gain_t2, gain_t2, gain_t1);
  gain_t2 := 0.23 + 0.23 * ((gain_control and 8) shr 3);
  ay8910_2.change_gain(gain_t1, gain_t2, gain_t2);
end;

procedure magmax_portb_w(valor: byte);
begin
  LS74_clr := valor and 1;
  if (LS74_clr = 0) then
    LS74_q := 0;
end;

// Main
procedure reset_magmax;
begin
  m68000_0.reset;
  z80_0.reset;
  frame_main := m68000_0.tframes;
  frame_snd := z80_0.tframes;
  ay8910_0.reset;
  ay8910_1.reset;
  ay8910_2.reset;
  reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  scroll_x := 0;
  scroll_y := 0;
  sound_latch := 0;
  LS74_clr := 0;
  LS74_q := 0;
  gain_control := 0;
  redraw_bg := true;
end;

function start_magmax: boolean;
var
  colores: tpaleta;
  f, v: byte;
  memory_temp: array [0 .. $FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (4, 0, 12, 8, 20, 16, 28, 24);
  ps_x: array [0 .. 15] of dword = (4, 0, 4 + 512 * 64 * 8, 0 + 512 * 64 * 8, 12, 8, 12 + 512 * 64 * 8, 8 + 512 * 64 * 8, 20, 16, 20 + 512 * 64 * 8, 16 + 512 * 64 * 8, 28, 24, 28 + 512 * 64 * 8, 24 + 512 * 64 * 8);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
begin
  machine_calls.general_loop := magmax_loop;
  machine_calls.reset := reset_magmax;
  start_magmax := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, true);
  screen_init(4, 512, 256, false, true);
  start_video(256, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256);
  m68000_0.change_ram16_calls(magmax_getword, magmax_putword);
  if not(roms_load16w(@rom, magmax_rom)) then
    exit;
  // Sound CPU
  z80_0 := cpu_z80.create(2500000, 256);
  z80_0.change_ram_calls(magmax_snd_getbyte, magmax_snd_putbyte);
  z80_0.change_io_calls(magmax_snd_inbyte, magmax_snd_outbyte);
  z80_0.init_sound(magmax_sound_update);
  if not(roms_load(@mem_snd, magmax_sound)) then
    exit;
  // Sound Chips
  ay8910_0 := ay8910_chip.create(1250000, AY8910, 1);
  ay8910_0.change_io_calls(nil, nil, magmax_porta_w, magmax_portb_w);
  ay8910_1 := ay8910_chip.create(1250000, AY8910, 1);
  ay8910_2 := ay8910_chip.create(1250000, AY8910, 1);
  // poner los datos de bg
  if not(roms_load16b(@rom18B, magmax_fondo1)) then
    exit;
  if not(roms_load(@rom18B, magmax_fondo2)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, magmax_char)) then
    exit;
  init_gfx(0, 8, 8, $100);
  gfx[0].trans[15] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, magmax_sprites)) then
    exit;
  init_gfx(1, 16, 16, $200);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
  // poner la paleta
  if not(roms_load(@memory_temp, magmax_pal)) then
    exit;
  for f := 0 to $FF do
  begin
    v := (memory_temp[$400 + f] shl 4) + memory_temp[$500 + f];
    prom_mem[f] := ((v and $1F) shl 8) or ((v and $10) shl 10) or ((v and $E0) shr 1);
  end;
  for f := 0 to $FF do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
  end;
  set_pal(colores, $100);
  // color lookup de sprites
  for f := 0 to $FF do
    gfx[1].colores[f] := (memory_temp[$300 + f] and $F) or $10;
  // DIP
  marcade.dswa := $FFDF;
  marcade.dswa_val2 := @magmax_dip;
  // final
  reset_magmax;
  start_magmax := true;
end;

end.
