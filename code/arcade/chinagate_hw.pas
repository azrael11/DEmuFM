unit chinagate_hw;

interface

uses
  WinApi.Windows,
  hd6309,
  nz80,
  ym_2151,
  main_engine,
  controls_engine,
  gfx_engine,
  oki6295,
  rom_engine,
  pal_engine,
  sound_engine;

function start_chinagate: boolean;

implementation

const
  chinagate_rom: tipo_roms = (n: 'cgate51.bin'; l: $20000; p: $0; crc: $439A3B19);
  chinagate_sub: tipo_roms = (n: '23j4-0.48'; l: $20000; p: $0; crc: $2914AF38);
  chinagate_snd: tipo_roms = (n: '23j0-0.40'; l: $8000; p: $0; crc: $9FFCADB6);
  chinagate_char: tipo_roms = (n: 'cgate18.bin'; l: $20000; p: 0; crc: $8D88D64D);
  chinagate_tiles: array [0 .. 3] of tipo_roms = ((n: 'chinagat_a-13'; l: $10000; p: 0; crc: $B745CAC4), (n: 'chinagat_a-12'; l: $10000; p: $10000; crc: $3C864299), (n: 'chinagat_a-15'; l: $10000;
    p: $20000; crc: $2F268F37), (n: 'chinagat_a-14'; l: $10000; p: $30000; crc: $AEF814C8));
  chinagate_sprites: array [0 .. 3] of tipo_roms = ((n: '23j7-0.103'; l: $20000; p: 0; crc: $2F445030), (n: '23j8-0.102'; l: $20000; p: $20000; crc: $237F725A), (n: '23j9-0.101'; l: $20000; p: $40000;
    crc: $8CAF6097), (n: '23ja-0.100'; l: $20000; p: $60000; crc: $F678594F));
  chinagate_adpcm: array [0 .. 1] of tipo_roms = ((n: '23j1-0.53'; l: $20000; p: 0; crc: $F91F1001), (n: '23j2-0.52'; l: $20000; p: $20000; crc: $8B6F26E9));
        chinagate_dip_a:array [0..4] of def_dip2=(
        (mask:$7;name:'Coin A';number:8;val8:(0,1,2,7,6,5,4,3);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$38;name:'Coin B';number:8;val8:(0,8,$10,$38,$30,$28,$20,$18);name8:('4C 1C','3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C')),
        (mask:$40;name:'Cabinet';number:2;val2:(0,$40);name2:('Upright','Cocktail')),
        (mask:$80;name:'Flip Screen';number:2;val2:($80,0);name2:('Off','On')),());
        chinagate_dip_b:array [0..4] of def_dip2=(
        (mask:$3;name:'Difficulty';number:4;val4:(1,3,2,0);name4:('Easy','Normal','Hard','Hardest')),
        (mask:$4;name:'Demo Sounds';number:2;val2:(0,4);name2:('Off','On')),
        (mask:$30;name:'Timer';number:4;val4:(0,$20,$30,$10);name4:('50','55','60','70')),
        (mask:$c0;name:'Lives';number:4;val4:(0,$c0,$80,$40);name4:('1','2','3','4')),());
  CPU_SYNC = 4;

var
  rom, rom_sub: array [0 .. 7, 0 .. $3FFF] of byte;
  banco_rom, banco_rom_sub, soundlatch: byte;
  scroll_x, scroll_y: word;
  chinagate_scanline: array [0 .. 271] of word;

procedure update_video_chinagate;
  procedure draw_sprites;
  var
    size, x, y, nchar: word;
    color, f, atrib: byte;
    flipx, flipy: boolean;
  begin
    for f := 0 to $3F do
    begin
      atrib := memory[$3801 + (f * 5)];
      if (atrib and $80) <> 0 then
      begin // visible
        x := 240 - memory[$3804 + (f * 5)] + ((atrib and 2) shl 7);
        y := 240 - memory[$3800 + (f * 5)] + ((atrib and 1) shl 8);
        size := (atrib and $30) shr 4;
        flipx := (atrib and 8) <> 0;
        flipy := (atrib and 4) <> 0;
        color := (memory[$3802 + (f * 5)] and $70) + $80;
        nchar := memory[$3803 + (f * 5)] + ((memory[$3802 + (f * 5)] and $F) shl 8);
        nchar := nchar and not(size);
        case size of
          0:
            begin // normal
              put_gfx_sprite(nchar, color, flipx, flipy, 2);
              update_gfx_sprite(x, y, 3, 2);
            end;
          1:
            begin // double y
              put_gfx_sprite_diff(nchar, color, flipx, flipy, 2, 0, 0);
              put_gfx_sprite_diff(nchar + 1, color, flipx, flipy, 2, 0, 16);
              actualiza_gfx_sprite_size(x, y - 16, 3, 16, 32);
            end;
          2:
            begin // double x
              put_gfx_sprite_diff(nchar, color, flipx, flipy, 2, 0, 0);
              put_gfx_sprite_diff(nchar + 1, color, flipx, flipy, 2, 16, 0);
              actualiza_gfx_sprite_size(x - 16, y, 3, 32, 16);
            end;
          3:
            begin
              put_gfx_sprite_diff(nchar, color, flipx, flipy, 2, 0, 0);
              put_gfx_sprite_diff(nchar + 1, color, flipx, flipy, 2, 16, 0);
              put_gfx_sprite_diff(nchar + 2, color, flipx, flipy, 2, 0, 16);
              put_gfx_sprite_diff(nchar + 3, color, flipx, flipy, 2, 16, 16);
              actualiza_gfx_sprite_size(x - 16, y - 16, 3, 32, 32);
            end;
        end;
      end; // visible
    end; // for
  end;

var
  x, y, color, f, nchar, pos: word;
  atrib: byte;
begin
  for f := $0 to $3FF do
  begin
    x := f mod 32;
    y := f div 32;
    // background
    pos := (x and $0F) + ((y and $0F) shl 4) + ((x and $10) shl 4) + ((y and $10) shl 5);
    atrib := memory[(pos * 2) + $2800];
    color := (atrib and $38) shr 3;
    if (gfx[1].buffer[pos] or buffer_color[color + 8]) then
    begin
      nchar := memory[(pos * 2) + $2801] + ((atrib and $7) shl 8);
      put_gfx_flip(x * 16, y * 16, nchar, $100 + (color shl 4), 2, 1, (atrib and $40) <> 0, (atrib and $80) <> 0);
      gfx[1].buffer[pos] := false;
    end;
    // foreground
    atrib := memory[$2000 + (f * 2)];
    color := (atrib and $F0) shr 4;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := memory[$2001 + (f * 2)] + ((atrib and $F) shl 8);
      put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x, scroll_y);
  draw_sprites;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  update_final_piece(0, 8, 256, 240, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_chinagate;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // p2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
  end;
end;

procedure chinagate_loop;
var
  f, l: word;
  frame_m, frame_s, frame_snd: single;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := hd6309_0.tframes;
  frame_s := hd6309_1.tframes;
  frame_snd := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // main
          hd6309_0.run(frame_m);
          frame_m := frame_m + hd6309_0.tframes - hd6309_0.contador;
          // sub
          hd6309_1.run(frame_s);
          frame_s := frame_s + hd6309_1.tframes - hd6309_1.contador;
          // snd
          z80_0.run(frame_snd);
          frame_snd := frame_snd + z80_0.tframes - z80_0.contador;
        end;
        // video
        case chinagate_scanline[f] of
          $8:
            marcade.in0 := marcade.in0 and $FE;
          $F8:
            begin
              hd6309_0.change_nmi(ASSERT_LINE);
              update_video_chinagate;
              marcade.in0 := marcade.in0 or 1;
            end;
        end;
        if f <> 0 then
          l := f - 1
        else
          l := 271;
        if (((chinagate_scanline[l] and $8) = 0) and ((chinagate_scanline[f] and $8) <> 0)) then
          hd6309_0.change_firq(ASSERT_LINE);
      end;
      events_chinagate;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function chinagate_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $1FFF, $2000 .. $2FFF, $3800 .. $397F, $8000 .. $FFFF:
      chinagate_getbyte := memory[direccion];
    $3F00:
      chinagate_getbyte := marcade.in0;
    $3F01:
      chinagate_getbyte := marcade.dswa;
    $3F02:
      chinagate_getbyte := marcade.dswb;
    $3F03:
      chinagate_getbyte := marcade.in1;
    $3F04:
      chinagate_getbyte := marcade.in2;
    $4000 .. $7FFF:
      chinagate_getbyte := rom[banco_rom, direccion and $3FFF];
  end;
end;

procedure change_color(pos: word);
var
  tmp_color: byte;
  color: tcolor;
begin
  tmp_color := buffer_paleta[pos];
  color.r := pal4bit(tmp_color);
  color.g := pal4bit(tmp_color shr 4);
  tmp_color := buffer_paleta[pos + $400];
  color.b := pal4bit(tmp_color);
  set_pal_color(color, pos);
  case pos of
    0 .. 127:
      buffer_color[pos shr 4] := true;
    256 .. 383:
      buffer_color[((pos shr 4) and $7) + 8] := true;
  end;
end;

procedure chinagate_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF, $3800 .. $397F:
      memory[direccion] := valor;
    $2000 .. $27FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $2800 .. $2FFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
        memory[direccion] := valor;
      end;
    $3000 .. $37FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color(direccion and $3FF);
      end;
    $3E00:
      begin
        soundlatch := valor;
        z80_0.change_nmi(ASSERT_LINE);
      end;
    $3E01:
      hd6309_0.change_nmi(CLEAR_LINE);
    $3E02:
      hd6309_0.change_firq(CLEAR_LINE);
    $3E03:
      hd6309_0.change_irq(CLEAR_LINE);
    $3E04:
      hd6309_1.change_irq(ASSERT_LINE);
    $3E06:
      scroll_y := (scroll_y and $100) or valor;
    $3E07:
      scroll_x := (scroll_x and $100) or valor;
    $3F00:
      begin
        scroll_x := (scroll_x and $FF) or ((valor and $1) shl 8);
        scroll_y := (scroll_y and $FF) or ((valor and $2) shl 7);
        main_screen.flip_main_screen := (valor and 4) = 0;
      end;
    $3F01:
      banco_rom := valor and $7;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

function chinagate_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF:
      chinagate_sub_getbyte := memory[direccion];
    $4000 .. $7FFF:
      chinagate_sub_getbyte := rom_sub[banco_rom_sub, direccion and $3FFF];
    $8000 .. $FFFF:
      chinagate_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure chinagate_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $1FFF:
      memory[direccion] := valor;
    $2000:
      banco_rom_sub := valor and $7;
    $2800:
      hd6309_1.change_irq(CLEAR_LINE);
    $4000 .. $FFFF:
      ;
  end;
end;

function chinagate_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      chinagate_snd_getbyte := mem_snd[direccion];
    $8801:
      chinagate_snd_getbyte := ym2151_0.status;
    $9800:
      chinagate_snd_getbyte := oki_6295_0.read;
    $A000:
      begin
        chinagate_snd_getbyte := soundlatch;
        z80_0.change_nmi(CLEAR_LINE);
      end;
  end;
end;

procedure chinagate_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $8800:
      ym2151_0.reg(valor);
    $8801:
      ym2151_0.write(valor);
    $9800:
      oki_6295_0.write(valor);
  end;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

procedure chinagate_sound_update;
begin
  ym2151_0.update;
  oki_6295_0.update;
end;

// Main
procedure reset_chinagate;
begin
  hd6309_0.reset;
  hd6309_1.reset;
  z80_0.reset;
  ym2151_0.reset;
  oki_6295_0.reset;
  reset_audio;
  marcade.in0 := $E;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  soundlatch := 0;
  banco_rom := 0;
  banco_rom_sub := 0;
  scroll_x := 0;
  scroll_y := 0;
end;

function start_chinagate: boolean;
var
  f: word;
  memory_temp: array [0 .. $7FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (1, 0, 65, 64, 129, 128, 193, 192);
  pt_x: array [0 .. 15] of dword = (3, 2, 1, 0, 16 * 8 + 3, 16 * 8 + 2, 16 * 8 + 1, 16 * 8 + 0, 32 * 8 + 3, 32 * 8 + 2, 32 * 8 + 1, 32 * 8 + 0, 48 * 8 + 3, 48 * 8 + 2, 48 * 8 + 1, 48 * 8 + 0);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
begin
  machine_calls.general_loop := chinagate_loop;
  machine_calls.reset := reset_chinagate;
  machine_calls.fps_max := 6000000 / 384 / 272;
  start_chinagate := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 512, 512, false, true);
  start_video(256, 240);
  // Main CPU
  hd6309_0 := cpu_hd6309.create(12000000 div 2, 272 * CPU_SYNC, TCPU_HD6309);
  hd6309_0.change_ram_calls(chinagate_getbyte, chinagate_putbyte);
  // Sub CPU
  hd6309_1 := cpu_hd6309.create(12000000 div 2, 272 * CPU_SYNC, TCPU_HD6309);
  hd6309_1.change_ram_calls(chinagate_sub_getbyte, chinagate_sub_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 272 * CPU_SYNC);
  z80_0.change_ram_calls(chinagate_snd_getbyte, chinagate_snd_putbyte);
  z80_0.init_sound(chinagate_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  oki_6295_0 := snd_okim6295.create(1056000, OKIM6295_PIN7_HIGH, 0.5);
  if not(roms_load(oki_6295_0.get_rom_addr, chinagate_adpcm)) then
    exit;
  // Main roms
  if not(roms_load(@memory_temp, chinagate_rom)) then
    exit;
  copymemory(@memory[$8000], @memory_temp[$18000], $8000);
  for f := 0 to 5 do
    copymemory(@rom[f, 0], @memory_temp[(f * $4000)], $4000);
  // Sub roms
  if not(roms_load(@memory_temp, chinagate_sub)) then
    exit;
  copymemory(@mem_misc[$8000], @memory_temp[$18000], $8000);
  for f := 0 to 5 do
    copymemory(@rom_sub[f, 0], @memory_temp[(f * $4000)], $4000);
  // Sound roms
  if not(roms_load(@mem_snd, chinagate_snd)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, chinagate_char)) then
    exit;
  init_gfx(0, 8, 8, $1000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 2, 4, 6);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pt_y, false, false);
  // convertir tiles
  if not(roms_load(@memory_temp, chinagate_tiles)) then
    exit;
  init_gfx(1, 16, 16, $800);
  gfx_set_desc_data(4, 0, 64 * 8, $800 * 64 * 8 + 0, $800 * 64 * 8 + 4, 0, 4);
  convert_gfx(1, 0, @memory_temp, @pt_x, @pt_y, false, false);
  // convertir sprites
  if not(roms_load(@memory_temp, chinagate_sprites)) then
    exit;
  init_gfx(2, 16, 16, $1000);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 0, 64 * 8, $1000 * 64 * 8 + 0, $1000 * 64 * 8 + 4, 0, 4);
  convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
  // DIP
  marcade.dswa := $BF;
  marcade.dswb := $E7;
marcade.dswa_val2:=@chinagate_dip_a;
marcade.dswb_val2:=@chinagate_dip_b;
  // init scanlines
  for f := 8 to $FF do
    chinagate_scanline[f - 8] := f; // 08,09,0A,0B,...,FC,FD,FE,FF
  for f := $E8 to $FF do
    chinagate_scanline[f + $10] := f + $100; // E8,E9,EA,EB,...,FC,FD,FE,FF
  // final
  reset_chinagate;
  start_chinagate := true;
end;

end.
