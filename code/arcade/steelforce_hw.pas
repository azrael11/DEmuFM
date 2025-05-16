unit steelforce_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  oki6295,
  sound_engine,
  eepromser;

function start_steelforce: boolean;

implementation

const
  steelforce_rom: array [0 .. 1] of tipo_roms = ((n: 'stlforce.105'; l: $20000; p: 0; crc: $3EC804CA),
    (n: 'stlforce.104'; l: $20000; p: $1; crc: $69B5F429));
  steelforce_tiles: array [0 .. 3] of tipo_roms = ((n: 'stlforce.u27'; l: $80000; p: 1; crc: $C42EF365),
    (n: 'stlforce.u28'; l: $80000; p: 0; crc: $6A4B7C98), (n: 'stlforce.u29'; l: $80000; p: $100001;
    crc: $30488F44), (n: 'stlforce.u30'; l: $80000; p: $100000; crc: $CF19D43A));
  steelforce_sprites: array [0 .. 3] of tipo_roms = ((n: 'stlforce.u36'; l: $40000; p: 0; crc: $037DFA9F),
    (n: 'stlforce.u31'; l: $40000; p: $40000; crc: $305A8EB5), (n: 'stlforce.u32'; l: $40000; p: $80000;
    crc: $760E8601), (n: 'stlforce.u33'; l: $40000; p: $C0000; crc: $19415CF3));
  steelforce_oki: tipo_roms = (n: 'stlforce.u1'; l: $80000; p: 0; crc: $0A55EDF1);
  steelforce_eeprom: tipo_roms = (n: 'eeprom-stlforce.bin'; l: $80; p: 0; crc: $3FB83951);
  twinbrats_rom: array [0 .. 1] of tipo_roms = ((n: '12.u105'; l: $20000; p: 0; crc: $552529B1),
    (n: '13.u104'; l: $20000; p: $1; crc: $9805BA90));
  twinbrats_tiles: array [0 .. 3] of tipo_roms = ((n: '6.bin'; l: $80000; p: 0; crc: $AF10DDFD), (n: '7.bin';
    l: $80000; p: 1; crc: $3696345A), (n: '4.bin'; l: $80000; p: $100000; crc: $1AE8A751), (n: '5.bin';
    l: $80000; p: $100001; crc: $CF235EEB));
  twinbrats_sprites: array [0 .. 3] of tipo_roms = ((n: '11.bin'; l: $40000; p: 0; crc: $00EECB03),
    (n: '10.bin'; l: $40000; p: $40000; crc: $7556BEE9), (n: '9.bin'; l: $40000; p: $80000; crc: $13194D89),
    (n: '8.bin'; l: $40000; p: $C0000; crc: $79F14528));
  twinbrats_oki: tipo_roms = (n: '1.bin'; l: $80000; p: 0; crc: $76296578);
  twinbrats_eeprom: tipo_roms = (n: 'eeprom-twinbrat.bin'; l: $80; p: 0; crc: $9366263D);
  mortalrace_rom: array [0 .. 1] of tipo_roms = ((n: '2.u105'; l: $80000; p: 0; crc: $550C48E3), (n: '3.u104';
    l: $80000; p: $1; crc: $92FAD747));
  mortalrace_tiles: array [0 .. 5] of tipo_roms = ((n: '8_bot.u27'; l: $80000; p: 1; crc: $042297F3),
    (n: '9_bot.u28'; l: $80000; p: 0; crc: $AB330185), (n: '12_top.u27'; l: $80000; p: $100001;
    crc: $FA95773C), (n: '13_top.u28'; l: $80000; p: $100000; crc: $F2342348), (n: '10.u29'; l: $80000;
    p: $200001; crc: $FB39B032), (n: '11.u30'; l: $80000; p: $200000; crc: $A82F2421));
  mortalrace_sprites: array [0 .. 3] of tipo_roms = ((n: '4.u36'; l: $80000; p: 0; crc: $6D1E6367),
    (n: '5.u31'; l: $80000; p: $80000; crc: $54B223BF), (n: '6.u32'; l: $80000; p: $100000; crc: $DAB08A04),
    (n: '7.u33'; l: $80000; p: $180000; crc: $9A856797));
  mortalrace_oki: tipo_roms = (n: '1.u1'; l: $80000; p: 0; crc: $E5C730C2);

var
  rom: array [0 .. $7FFFF] of word;
  ram: array [0 .. $FFFF] of word;
  ram2: array [0 .. $7FFF] of word;
  x_pos, sprite_x_add: byte;
  txt_ram, bg_ram, fglow_ram, fghigh_ram: array [0 .. $7FF] of word;
  x_size: word;
  which: boolean;
  oki_rom: array [0 .. 3, 0 .. $1FFFF] of byte;

procedure update_video_steelforce;
  procedure draw_sprites(pri: byte);
  var
    i, f, size: byte;
    x, y, nchar, color, atrib, atrib2: word;
  begin
    for f := 0 to $FF do
    begin
      atrib := buffer_sprites_w[f * 4];
      atrib2 := buffer_sprites_w[(f * 4) + 1];
      if (atrib2 and $30) <> pri then
        continue;
      if (atrib and $800) <> 0 then
      begin
        y := $1FF - (atrib and $1FF);
        x := buffer_sprites_w[(f * 4) + 3] + 8;
        color := (atrib2 and $F) shl 4;
        size := (atrib and $F000) shr 12;
        nchar := buffer_sprites_w[(f * 4) + 2];
        for i := 0 to size do
        begin
          put_gfx_sprite(nchar + i, color + $400, (atrib2 and $200) <> 0, false, 4);
          update_gfx_sprite(x + sprite_x_add, y + i * 16, 5, 4);
        end;
      end;
    end;
  end;

var
  f, atrib, atrib2, nchar, color, x, y, scroll_y: word;
begin
  for f := 0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    // FG
    atrib := txt_ram[f];
    nchar := atrib and $1FFF;
    color := (atrib and $E000) shr 13;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      put_gfx_trans(x * 8, y * 8, nchar, $180 + (color shl 4), 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  for f := 0 to $3FF do
  begin
    x := f div 16;
    y := f mod 16;
    atrib := bg_ram[f];
    nchar := atrib and $1FFF;
    color := (atrib and $E000) shr 13;
    if (gfx[1].buffer[f] or buffer_color[color + $10]) then
    begin
      put_gfx(x * 16, y * 16, nchar, color shl 4, 2, 1);
      gfx[1].buffer[f] := false;
    end;
    atrib := fglow_ram[f];
    nchar := atrib and $1FFF;
    color := (atrib and $E000) shr 13;
    if (gfx[2].buffer[f] or buffer_color[color + $20]) then
    begin
      put_gfx_trans(x * 16, y * 16, nchar, $80 + (color shl 4), 3, 2);
      gfx[2].buffer[f] := false;
    end;
    atrib := fghigh_ram[f];
    nchar := atrib and $1FFF;
    color := (atrib and $E000) shr 13;
    if (gfx[3].buffer[f] or buffer_color[color + $30]) then
    begin
      put_gfx_trans(x * 16, y * 16, nchar, $100 + (color shl 4), 4, 3);
      gfx[3].buffer[f] := false;
    end;
  end;
  atrib := ram[$3C0C shr 1];
  atrib2 := ram[$3C0A shr 1];
  if (atrib2 and 1) <> 0 then
  begin
    scroll_y := ram[$3C02 shr 1] + 1;
    if (atrib and 1) <> 0 then
      scroll__x_part2(2, 5, 16, @ram[$3000 shr 1], 0, scroll_y)
    else
      scroll_x_y(2, 5, ram[$3000 shr 1], scroll_y);
  end
  else
    fill_full_screen(5, 0);
  if (atrib2 and $10) <> 0 then
    draw_sprites(0);
  if (atrib2 and 2) <> 0 then
  begin
    scroll_y := ram[$3C04 shr 1] + 1;
    if (atrib and 4) <> 0 then
      scroll__x_part2(3, 5, 16, @ram[$3400 shr 1], 0, scroll_y)
    else
      scroll_x_y(3, 5, ram[$3400 shr 1] - 1, scroll_y);
  end;
  if (atrib2 and $10) <> 0 then
    draw_sprites($10);
  if (atrib2 and 4) <> 0 then
  begin
    scroll_y := ram[$3C06 shr 1] + 1;
    if (atrib and $10) <> 0 then
      scroll__x_part2(4, 5, 16, @ram[$3800 shr 1], 0, scroll_y)
    else
      scroll_x_y(4, 5, ram[$3800 shr 1], scroll_y);
  end;
  if (atrib2 and $10) <> 0 then
  begin
    draw_sprites($20);
    draw_sprites($30);
  end;
  if (atrib2 and 8) <> 0 then
    scroll_x_y(1, 5, ram[$3C00 shr 1] - 3, ram[$3C08 shr 1] + 1);
  update_final_piece(x_pos, 0, x_size, 240, 5);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_steelforce;
begin
  if main_vars.service1 then
    marcade.in1 := (marcade.in1 and $FFF7)
  else
    marcade.in1 := (marcade.in1 or $8);
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFEF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.left[1] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.right[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.but0[1] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
  end;
end;

// needs pause
procedure steelforce_loop;
var
  f:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
 for f:=0 to 255 do begin
   events_steelforce;
   case f of
      0:marcade.in1:=marcade.in1 and $ffef;
      240:begin
            m68000_0.irq[4]:=HOLD_LINE;
            update_video_steelforce;
            marcade.in1:=marcade.in1 or $10;
          end;
   end;
   m68000_0.run(frame_main);
   frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
 end;
 video_sync;
  end;
end;

function steelforce_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $FFFFF:
      steelforce_getword := rom[direccion shr 1];
    $100000 .. $1007FF:
      steelforce_getword := bg_ram[(direccion and $7FF) shr 1];
    $100800 .. $100FFF:
      steelforce_getword := fglow_ram[(direccion and $7FF) shr 1];
    $101000 .. $1017FF:
      steelforce_getword := fghigh_ram[(direccion and $7FF) shr 1];
    $101800 .. $1027FF:
      steelforce_getword := txt_ram[(direccion - $101800) shr 1];
    $102800 .. $103FFF, $105000 .. $11FFFF:
      steelforce_getword := ram[(direccion and $1FFFF) shr 1];
    $104000 .. $104FFF:
      steelforce_getword := buffer_paleta[(direccion and $FFF) shr 1];
    $120000 .. $12FFFF:
      steelforce_getword := ram2[(direccion and $FFFF) shr 1];
    $400000:
      steelforce_getword := marcade.in0;
    $400002:
      steelforce_getword := marcade.in1 or (eepromser_0.do_read shl 6);
    $410000:
      steelforce_getword := oki_6295_0.read;
  end;
end;

procedure steelforce_putword(direccion: dword; valor: word);
var
  ptemp: pbyte;
  procedure cambiar_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal5bit(tmp_color shr 10);
    color.g := pal5bit(tmp_color shr 5);
    color.r := pal5bit(tmp_color shr 0);
    set_pal_color(color, numero);
    case numero of
      0 .. $7F:
        buffer_color[((numero shr 4) and 7) + $10] := true;
      $80 .. $FF:
        buffer_color[((numero shr 4) and 7) + $20] := true;
      $100 .. $17F:
        buffer_color[((numero shr 4) and 7) + $30] := true;
      $180 .. $1FF:
        buffer_color[(numero shr 4) and 7] := true;
    end;
  end;

begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $1007FF:
      if bg_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        bg_ram[(direccion and $7FF) shr 1] := valor;
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $100800 .. $100FFF:
      if fglow_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        fglow_ram[(direccion and $7FF) shr 1] := valor;
        gfx[2].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $101000 .. $1017FF:
      if fghigh_ram[(direccion and $7FF) shr 1] <> valor then
      begin
        fghigh_ram[(direccion and $7FF) shr 1] := valor;
        gfx[3].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $101800 .. $1027FF:
      if txt_ram[(direccion - $101800) shr 1] <> valor then
      begin
        txt_ram[(direccion - $101800) shr 1] := valor;
        gfx[0].buffer[(direccion - $101800) shr 1] := true;
      end;
    $102800 .. $103FFF, $105000 .. $11FFFF:
      ram[(direccion and $1FFFF) shr 1] := valor;
    $104000 .. $104FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        cambiar_color(valor, (direccion and $FFF) shr 1);
      end;
    $120000 .. $12FFFF:
      ram2[(direccion and $FFFF) shr 1] := valor;
    $400010:
      begin
        eepromser_0.di_write(valor and 1);
        eepromser_0.cs_write((valor shr 1) and 1);
        eepromser_0.clk_write((valor shr 2) and 1);
      end;
    $400012:
      begin
        ptemp := oki_6295_0.get_rom_addr;
        copymemory(@ptemp[$20000], @oki_rom[valor and 3, 0], $20000);
      end;
    $40001E:
      begin
        if which then
        begin
          case (valor and $F) of
            0:
              begin
                fillchar(buffer_sprites_w, $800, 0);
                which := false;
              end;
            $D:
              ;
          else
            copymemory(@buffer_sprites_w, @ram[$8000 shr 1], $800);
          end;
        end;
        which := not(which)
      end;
    $410000:
      oki_6295_0.write(valor);
  end;
end;

procedure steelforce_sound_update;
begin
  oki_6295_0.update;
end;

// Main
procedure reset_steelforce;
begin
  m68000_0.reset;
  oki_6295_0.reset;
  eepromser_0.reset;
 frame_main:=m68000_0.tframes;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFAF;
  which := false;
end;

procedure close_steelforce;
begin
  case main_vars.machine_type of
    358:
      eepromser_0.write_data('steelforce.nv');
    359:
      eepromser_0.write_data('twinbrats.nv');
    360:
      eepromser_0.write_data('mortalrace.nv');
  end;
end;

function start_steelforce: boolean;
const
  pc_x: array [0 .. 15] of dword = (12, 8, 4, 0, 28, 24, 20, 16, 16 * 32 + 12, 16 * 32 + 8, 16 * 32 + 4,
    16 * 32 + 0, 16 * 32 + 28, 16 * 32 + 24, 16 * 32 + 20, 16 * 32 + 16);
  pc_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32,
    9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  ps_x: array [0 .. 15] of dword = (16 * 8 + 7, 16 * 8 + 6, 16 * 8 + 5, 16 * 8 + 4, 16 * 8 + 3, 16 * 8 + 2,
    16 * 8 + 1, 16 * 8 + 0, 7, 6, 5, 4, 3, 2, 1, 0);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8,
    10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  memoria_temp: pbyte;
  cpu_clock, snd_clock: dword;
  procedure ext_chars_tiles(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx_set_desc_data(4, 0, 8 * 32, 0, 1, 2, 3);
    gfx[0].trans[0] := true;
    convert_gfx(0, 0, @memoria_temp[$20 * num * 3], @pc_x, @pc_y, false, false);
    // tiles 1,2 & 3
    init_gfx(1, 16, 16, num shr 2);
    gfx_set_desc_data(4, 0, 32 * 32, 0, 1, 2, 3);
    convert_gfx(1, 0, memoria_temp, @pc_x, @pc_y, false, false);
    init_gfx(2, 16, 16, num shr 2);
    convert_gfx(2, 0, @memoria_temp[$20 * num * 1], @pc_x, @pc_y, false, false);
    gfx[2].trans[0] := true;
    init_gfx(3, 16, 16, num shr 2);
    convert_gfx(3, 0, @memoria_temp[$20 * num * 2], @pc_x, @pc_y, false, false);
    gfx[3].trans[0] := true;
  end;
  procedure ext_sprites(num: word);
  begin
    init_gfx(4, 16, 16, num);
    gfx_set_desc_data(4, 0, 32 * 8, $20 * num * 3 * 8, $20 * num * 2 * 8, $20 * num * 1 * 8,
      $20 * num * 0 * 8);
    convert_gfx(4, 0, memoria_temp, @ps_x, @ps_y, false, false);
    gfx[4].trans[0] := true;
  end;

begin
  start_steelforce := false;
  machine_calls.general_loop := steelforce_loop;
  machine_calls.reset := reset_steelforce;
  machine_calls.close := close_steelforce;
  machine_calls.fps_max := 58;
  start_audio(false);
  screen_init(1, 512, 256, true);
  screen_mod_scroll(1, 512, 512, 511, 256, 256, 255);
  screen_init(2, 1024, 256);
  screen_mod_scroll(2, 1024, 512, 1023, 256, 256, 255);
  screen_init(3, 1024, 256, true);
  screen_mod_scroll(3, 1024, 512, 1023, 256, 256, 255);
  screen_init(4, 1024, 256, true);
  screen_mod_scroll(4, 1024, 512, 1023, 256, 256, 255);
  screen_init(5, 512, 512, false, true);
  if main_vars.machine_type <> 359 then
  begin
    cpu_clock := 15000000;
    snd_clock := 32000000 div 32;
    x_size := 372;
    sprite_x_add := 0;
    x_pos := 24;
  end
  else
  begin
    cpu_clock := 14745600;
    snd_clock := 30000000 div 32;
    x_size := 328;
    sprite_x_add := 9;
    x_pos := 24 + 16;
  end;
  start_video(x_size, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(cpu_clock, $100);
  m68000_0.init_sound(steelforce_sound_update);
  m68000_0.change_ram16_calls(steelforce_getword, steelforce_putword);
  // Sound
  oki_6295_0 := snd_okim6295.create(snd_clock, OKIM6295_PIN7_HIGH, 1);
  // eeprom
  eepromser_0 := eepromser_chip.create(E93C46, 16);
  getmem(memoria_temp, $300000);
  case main_vars.machine_type of
    358:
      begin // Steel Force
        // cpu data
        if not(roms_load16w(@rom, steelforce_rom)) then
          exit;
        // Sound data
        if not(roms_load(memoria_temp, steelforce_oki)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, @memoria_temp[0], $40000);
        copymemory(@oki_rom[0, 0], @memoria_temp[$20000], $20000); // El banco está fijo
        copymemory(@oki_rom[1, 0], @memoria_temp[$20000], $20000);
        copymemory(@oki_rom[2, 0], @memoria_temp[$20000], $20000);
        copymemory(@oki_rom[3, 0], @memoria_temp[$20000], $20000);
        // eeprom data
        if not(eepromser_0.load_data('steelforce.nv')) then
        begin
          if not(roms_load(memoria_temp, steelforce_eeprom)) then
            exit;
          copymemory(eepromser_0.get_data, memoria_temp, $80);
        end;
        // char
        if not(roms_load16b(memoria_temp, steelforce_tiles)) then
          exit;
        ext_chars_tiles($4000);
        // sprites
        if not(roms_load(memoria_temp, steelforce_sprites)) then
          exit;
        ext_sprites($2000);
      end;
    359:
      begin // Twin Brats
        // cpu data
        if not(roms_load16w(@rom, twinbrats_rom)) then
          exit;
        // Sound data
        if not(roms_load(memoria_temp, twinbrats_oki)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, @memoria_temp[0], $40000);
        copymemory(@oki_rom[0, 0], @memoria_temp[0], $20000);
        copymemory(@oki_rom[1, 0], @memoria_temp[$20000], $20000);
        copymemory(@oki_rom[2, 0], @memoria_temp[$40000], $20000);
        copymemory(@oki_rom[3, 0], @memoria_temp[$60000], $20000);
        // eeprom data
        if not(eepromser_0.load_data('twinbrats.nv')) then
        begin
          if not(roms_load(memoria_temp, twinbrats_eeprom)) then
            exit;
          copymemory(eepromser_0.get_data, memoria_temp, $80);
        end;
        // char
        if not(roms_load16b(memoria_temp, twinbrats_tiles)) then
          exit;
        ext_chars_tiles($4000);
        // sprites
        if not(roms_load(memoria_temp, twinbrats_sprites)) then
          exit;
        ext_sprites($2000);
      end;
    360:
      begin // Mortal Race
        // cpu data
        if not(roms_load16w(@rom, mortalrace_rom)) then
          exit;
        // Sound data
        if not(roms_load(memoria_temp, mortalrace_oki)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, @memoria_temp[0], $40000);
        copymemory(@oki_rom[0, 0], @memoria_temp[$20000], $20000); // El banco está fijo
        copymemory(@oki_rom[1, 0], @memoria_temp[$20000], $20000);
        copymemory(@oki_rom[2, 0], @memoria_temp[$20000], $20000);
        copymemory(@oki_rom[3, 0], @memoria_temp[$20000], $20000);
        // eeprom
        eepromser_0.load_data('mortalrace.nv');
        // char
        if not(roms_load16b(memoria_temp, mortalrace_tiles)) then
          exit;
        ext_chars_tiles($8000);
        // sprites
        if not(roms_load(memoria_temp, mortalrace_sprites)) then
          exit;
        ext_sprites($4000);
      end;
  end;
  // final
  freemem(memoria_temp);
  start_steelforce := true;
end;

end.
