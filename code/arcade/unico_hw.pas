unit unico_hw;

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
  ym_3812,
  oki6295;

function start_unico: boolean;

implementation

const
  burglarx_rom: array [0 .. 1] of tipo_roms = ((n: 'bx-rom2.pgm'; l: $80000; p: 0; crc: $F81120C8),
    (n: 'bx-rom3.pgm'; l: $80000; p: $1; crc: $080B4E82));
  burglarx_sprites: array [0 .. 7] of tipo_roms = ((n: 'bx-rom4'; l: $80000; p: 0; crc: $F74CE31F),
    (n: 'bx-rom10'; l: $80000; p: $1; crc: $6F56CA23), (n: 'bx-rom9'; l: $80000; p: $100000; crc: $33F29D79),
    (n: 'bx-rom8'; l: $80000; p: $100001; crc: $24367092), (n: 'bx-rom7'; l: $80000; p: $200000;
    crc: $AFF6BDEA), (n: 'bx-rom6'; l: $80000; p: $200001; crc: $246AFED2), (n: 'bx-rom11'; l: $80000;
    p: $300000; crc: $898D176A), (n: 'bx-rom5'; l: $80000; p: $300001; crc: $FDEE1423));
  burglarx_tiles: array [0 .. 7] of tipo_roms = ((n: 'bx-rom14'; l: $80000; p: 0; crc: $30413373),
    (n: 'bx-rom18'; l: $80000; p: $1; crc: $8E7FC99F), (n: 'bx-rom19'; l: $80000; p: $100000; crc: $D40EABCD),
    (n: 'bx-rom15'; l: $80000; p: $100001; crc: $78833C75), (n: 'bx-rom17'; l: $80000; p: $200000;
    crc: $F169633F), (n: 'bx-rom12'; l: $80000; p: $200001; crc: $71EB160F), (n: 'bx-rom13'; l: $80000;
    p: $300000; crc: $DA34BBB5), (n: 'bx-rom16'; l: $80000; p: $300001; crc: $55B28EF9));
  burglarx_oki: tipo_roms = (n: 'bx-rom1.snd'; l: $80000; p: 0; crc: $8AE67138);
  burglarx_dip_a: array [0 .. 3] of def_dip = ((mask: $200; name: 'Free Play'; number: 2;
    dip: ((dip_val: $200; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $800; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $800; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $E000; name: 'Coinage'; number: 8;
    dip: ((dip_val: $0; dip_name: '5C 1C'), (dip_val: $2000; dip_name: '4C 1C'), (dip_val: $4000;
    dip_name: '3C 1C'), (dip_val: $6000; dip_name: '2C 1C'), (dip_val: $E000;
    dip_name: '1C 1C'), (dip_val: $C000; dip_name: '1C 2C'), (dip_val: $A000;
    dip_name: '1C 3C'), (dip_val: $8000; dip_name: '1C 4C'), (), (), (), (), (), (), (), ())), ());
  burglarx_dip_b: array [0 .. 4] of def_dip = ((mask: $300; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $200; dip_name: 'None'), (dip_val: $300; dip_name: 'A'), (dip_val: $100;
    dip_name: 'B'), (dip_val: $0; dip_name: 'C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $800; name: 'Energy'; number: 2; dip: ((dip_val: $0; dip_name: '2'), (dip_val: $800;
    dip_name: '3'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $3000;
    name: 'Difficulty'; number: 4; dip: ((dip_val: $2000; dip_name: 'Easy'), (dip_val: $3000;
    dip_name: 'Normal'), (dip_val: $1000; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $C000; name: 'Lives'; number: 4;
    dip: ((dip_val: $8000; dip_name: '2'), (dip_val: $C000; dip_name: '3'), (dip_val: $4000;
    dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  zeropnt_rom: array [0 .. 1] of tipo_roms = ((n: 'unico_2.rom2'; l: $80000; p: 0; crc: $1E599509),
    (n: 'unico_3.rom3'; l: $80000; p: $1; crc: $588AEEF7));
  zeropnt_sprites: array [0 .. 3] of tipo_roms = ((n: 'unico_zpobj_z01.bin'; l: $200000; p: 0;
    crc: $1F2768A3), (n: 'unico_zpobj_z02.bin'; l: $200000; p: $200000; crc: $DE34F33A),
    (n: 'unico_zpobj_z03.bin'; l: $200000; p: $400000; crc: $D7A657F7), (n: 'unico_zpobj_z04.bin'; l: $200000;
    p: $600000; crc: $3AEC2F8D));
  zeropnt_tiles: array [0 .. 3] of tipo_roms = ((n: 'unico_zpscr_z06.bin'; l: $200000; p: 0; crc: $E1E53CF0),
    (n: 'unico_zpscr_z05.bin'; l: $200000; p: $200000; crc: $0D7D4850), (n: 'unico_zpscr_z07.bin'; l: $200000;
    p: $400000; crc: $BB178F32), (n: 'unico_zpscr_z08.bin'; l: $200000; p: $600000; crc: $672F02E5));
  zeropnt_oki: tipo_roms = (n: 'unico_1.rom1'; l: $80000; p: 0; crc: $FD2384FA);
  zeropnt_dip_b: array [0 .. 2] of def_dip = ((mask: $3000; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $2000; dip_name: 'Easy'), (dip_val: $3000; dip_name: 'Normal'), (dip_val: $1000;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C000; name: 'Lives'; number: 4; dip: ((dip_val: $8000; dip_name: '2'), (dip_val: $C000;
    dip_name: '3'), (dip_val: $4000; dip_name: '4'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());

var
  rom: array [0 .. $7FFFF] of word;
  ram: array [0 .. $7FFF] of word;
  ram2: array [0 .. $1FFF] of word;
  video_ram: array [0 .. $5FFF] of word;
  sprite_ram: array [0 .. $3FF] of word;
  oki_rom: array [0 .. 1, 0 .. $3FFFF] of byte;
  scroll_ram: array [0 .. $15] of word;
  gr_mask: word;
  events_unico: procedure;
  scr_frame: byte;

procedure update_video_unico;
var
  f, x, y, nchar, atrib: word;
  color: byte;
  procedure draw_sprites(pri: byte);
  var
    f, size: byte;
    x, y, nchar, atrib: word;
    flipx, flipy: boolean;
    startx, endx: word;
    incr: integer;
  begin
    for f := 0 to $FF do
    begin
      atrib := sprite_ram[(f * 4) + 3];
      if (pri <> ((atrib shr 12) and 3)) then
        continue;
      x := sprite_ram[(f * 4) + 0] - 15;
      y := sprite_ram[(f * 4) + 1] + 2;
      nchar := sprite_ram[(f * 4) + 2] and gr_mask;
      flipx := (atrib and $20) <> 0;
      flipy := (atrib and $40) <> 0;
      size := ((atrib shr 8) and $F) + 1;
      if flipx then
      begin
        startx := x + (size - 1) * 16;
        endx := x - 16;
        incr := -16;
      end
      else
      begin
        startx := x;
        endx := x + size * 16;
        incr := 16;
      end;
      x := startx;
      while x <> endx do
      begin
        put_gfx_sprite(nchar, (atrib and $1F) shl 8, flipx, flipy, 1);
        update_gfx_sprite(x, y, 4, 1);
        nchar := nchar + 1;
        x := x + incr;
      end;
    end;
  end;

begin
  fill_full_screen(4, $1F00);
  for f := $0 to $FFF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib := video_ram[(f * 2) + $4001];
    color := atrib and $1F;
    if (gfx[1].buffer[f + $2000] or (buffer_color[color])) then
    begin
      nchar := video_ram[$4000 + (f * 2)] and gr_mask;
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 8, 1, 0, (atrib and $20) <> 0,
        (atrib and $40) <> 0);
      gfx[1].buffer[f + $2000] := false;
    end;
    atrib := video_ram[(f * 2) + 1];
    color := atrib and $1F;
    if (gfx[1].buffer[f] or (buffer_color[color])) then
    begin
      nchar := video_ram[f * 2] and gr_mask;
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 8, 2, 0, (atrib and $20) <> 0,
        (atrib and $40) <> 0);
      gfx[1].buffer[f] := false;
    end;
    atrib := video_ram[(f * 2) + $2001];
    color := atrib and $1F;
    if (gfx[1].buffer[f + $1000] or (buffer_color[color])) then
    begin
      nchar := video_ram[$2000 + (f * 2)] and gr_mask;
      put_gfx_trans_flip(x * 16, y * 16, nchar, color shl 8, 3, 0, (atrib and $20) <> 0,
        (atrib and $40) <> 0);
      gfx[1].buffer[f + $1000] := false;
    end;
  end;
  draw_sprites(0);
  scroll_x_y(1, 4, scroll_ram[0] + 2, scroll_ram[1]);
  draw_sprites(2);
  scroll_x_y(2, 4, scroll_ram[5] + 2, scroll_ram[$A]);
  draw_sprites(1);
  scroll_x_y(3, 4, scroll_ram[4] + 2, scroll_ram[2]);
  draw_sprites(3);
  update_final_piece(48, 16, 384, 224, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_burglarx;
begin
  if event.arcade then
  begin
    // P1+P2
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or 8);
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
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EFFF)
    else
      marcade.in0 := (marcade.in0 or $1000);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DFFF)
    else
      marcade.in0 := (marcade.in0 or $2000);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
  end;
end;

procedure events_zeropoint;
begin
  if event.arcade then
  begin
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FFFE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FFFD);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $FFEF);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $FFDF);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FEFF)
    else
      marcade.in1 := (marcade.in1 or $100);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $200);
  end;
end;


// needs pause
procedure unico_loop;
var
  f:byte;
begin
  init_controls(true, false, false, true);
  while EmuStatus = EsRunning do
  begin
 for f:=0 to 223 do begin
  events_unico;
  if f=0 then begin
    update_video_unico;
    m68000_0.irq[2]:=HOLD_LINE;
  end;
  m68000_0.run(frame_main);
  frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
 end;
 scr_frame:=scr_frame xor 1;
 video_sync;
  end;
end;

function burglarx_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      burglarx_getword := rom[direccion shr 1];
    $800000:
      burglarx_getword := marcade.in0;
    $800018:
      burglarx_getword := marcade.in1;
    $80001A:
      burglarx_getword := marcade.dswa;
    $80001C:
      burglarx_getword := marcade.dswb;
    $80010C .. $800121:
      burglarx_getword := scroll_ram[(direccion - $80010C) shr 1];
    $800188:
      burglarx_getword := oki_6295_0.read;
    $80018C:
      burglarx_getword := ym3812_0.status shl 8;
    $904000 .. $90FFFF:
      burglarx_getword := video_ram[(direccion - $904000) shr 1];
    $920000 .. $923FFF:
      burglarx_getword := ram2[(direccion and $3FFF) shr 1];
    $930000 .. $9307FF:
      burglarx_getword := sprite_ram[(direccion and $7FF) shr 1];
    $940000 .. $947FFF:
      burglarx_getword := buffer_paleta[(direccion and $7FFF) shr 1];
    $FF0000 .. $FFFFFF:
      burglarx_getword := ram[(direccion and $FFFF) shr 1];
  end;
end;

procedure burglarx_putword(direccion: dword; valor: word);
  procedure cambiar_color(dir: word);
  var
    tmp_color: dword;
    color: tcolor;
  begin
    tmp_color := (buffer_paleta[dir or 1] shl 16) + buffer_paleta[dir or 0];
    color.r := (tmp_color shr 8) and $FC;
    color.r := color.r or (color.r shr 6);
    color.g := (tmp_color shr 0) and $FC;
    color.g := color.g or (color.g shr 6);
    color.b := (tmp_color shr 24) and $FC;
    color.b := color.b or (color.b shr 6);
    dir := dir shr 1;
    set_pal_color(color, dir);
    buffer_color[dir shr 8] := true;
  end;

begin
  case direccion of
    0 .. $FFFFF:
      ;
    $800030, $8001E0:
      ;
    $80010C .. $800121:
      scroll_ram[(direccion - $80010C) shr 1] := valor;
    $800188:
      oki_6295_0.write(valor);
    $80018A:
      ym3812_0.write(valor shr 8);
    $80018C:
      ym3812_0.control(valor shr 8);
    $80018E:
      copymemory(oki_6295_0.get_rom_addr, @oki_rom[valor and 1, 0], $40000);
    $904000 .. $90FFFF:
      if video_ram[(direccion - $904000) shr 1] <> valor then
      begin
        video_ram[(direccion - $904000) shr 1] := valor;
        gfx[1].buffer[(direccion - $904000) shr 2] := true;
      end;
    $920000 .. $923FFF:
      ram2[(direccion and $3FFF) shr 1] := valor;
    $930000 .. $9307FF:
      sprite_ram[(direccion and $7FF) shr 1] := valor;
    $940000 .. $947FFF:
      if buffer_paleta[(direccion and $7FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FFF) shr 1] := valor;
        cambiar_color((direccion and $7FFC) shr 1);
      end;
    $FF0000 .. $FFFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  end;
end;

// Zero Point
function zeropnt_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      zeropnt_getword := rom[direccion shr 1];
    // Para que funcione bien, tengo que meterle la pistola por las dos entradas de los players...
    // Ademas tengo que hacer que tiemble un poco para que funcione el disparo �?
    $800170:
      zeropnt_getword := ($80 xor scr_frame) shl 8; // P2 Y
    $800174:
      zeropnt_getword := ($80 xor scr_frame) shl 8; // P2 X
    $800178:
      zeropnt_getword := (((mouse_def.y + 24) and $FF) xor scr_frame) shl 8; // P1 Y
    $80017C:
      zeropnt_getword := (((trunc((mouse_def.x + 52) * 0.666667)) and $FF) xor scr_frame) shl 8; // P1 X
    $EF0000 .. $EFFFFF:
      zeropnt_getword := ram[(direccion and $FFFF) shr 1];
  else
    zeropnt_getword := burglarx_getword(direccion);
  end;
end;

procedure zeropnt_putword(direccion: dword; valor: word);
var
  ptemp: pbyte;
begin
  case direccion of
    0 .. $FFFFF:
      ;
    $80018E:
      begin
        ptemp := oki_6295_0.get_rom_addr;
        copymemory(@ptemp[$20000], @oki_rom[valor and 1, 0], $20000);
      end;
    $EF0000 .. $EFFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  else
    burglarx_putword(direccion, valor);
  end;
end;

procedure unico_sound_update;
begin
  ym3812_0.update;
  oki_6295_0.update;
end;

// Main
procedure reset_unico;
begin
  m68000_0.reset;
  ym3812_0.reset;
  oki_6295_0.reset;
 frame_main:=m68000_0.tframes;
  scr_frame := 0;
  marcade.in0 := $FFFF;
  case main_vars.machine_type of
    380:
      marcade.in1 := $FFFF;
    381:
      marcade.in1 := $FF00;
  end;
end;

function start_unico: boolean;
var
  ptemp: pbyte;
const
  pt_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23);
  pt_y: array [0 .. 15] of dword = (0 * 16 * 2, 1 * 16 * 2, 2 * 16 * 2, 3 * 16 * 2, 4 * 16 * 2, 5 * 16 * 2,
    6 * 16 * 2, 7 * 16 * 2, 8 * 16 * 2, 9 * 16 * 2, 10 * 16 * 2, 11 * 16 * 2, 12 * 16 * 2, 13 * 16 * 2,
    14 * 16 * 2, 15 * 16 * 2);
  procedure convert_graph(gfxn: byte; num: word);
  begin
    init_gfx(gfxn, 16, 16, num);
    gfx[gfxn].trans[0] := true;
    gfx_set_desc_data(8, 0, 16 * 16 * 2, $C0 * num * 8 + 8, $C0 * num * 8 + 0, $80 * num * 8 + 8,
      $80 * num * 8 + 0, $40 * num * 8 + 8, $40 * num * 8 + 0, 8, 0);
    convert_gfx(gfxn, 0, ptemp, @pt_x, @pt_y, false, false, true);
  end;

begin
  machine_calls.general_loop := unico_loop;
  machine_calls.reset := reset_unico;
  start_unico := false;
  start_audio(true);
  screen_init(1, 1024, 1024, true);
  screen_mod_scroll(1, 1024, 512, 1023, 1024, 256, 1023);
  screen_init(2, 1024, 1024, true);
  screen_mod_scroll(2, 1024, 512, 1023, 1024, 256, 1023);
  screen_init(3, 1024, 1024, true);
  screen_mod_scroll(3, 1024, 512, 1023, 1024, 256, 1023);
  screen_init(4, 1024, 1024, false, true);
  start_video(384, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(16000000, 224);
  m68000_0.init_sound(unico_sound_update);
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3812_FM, 14318181 div 4, 1);
  oki_6295_0 := snd_okim6295.create(1000000, OKIM6295_PIN7_HIGH);
  case main_vars.machine_type of
    380:
      begin // BurglarX
        // cargar roms
        m68000_0.change_ram16_calls(burglarx_getword, burglarx_putword);
        if not(roms_load16w(@rom, burglarx_rom)) then
          exit;
        if not(roms_load(@oki_rom[0, 0], burglarx_oki)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, @oki_rom[0, 0], $40000);
        // convertir tiles
        getmem(ptemp, $400000);
        if not(roms_load16b(ptemp, burglarx_tiles)) then
          exit;
        convert_graph(0, $4000);
        // convertir sprites
        if not(roms_load16b(ptemp, burglarx_sprites)) then
          exit;
        convert_graph(1, $4000);
        freemem(ptemp);
        gr_mask := $3FFF;
        events_unico := events_burglarx;
        // DIP
        marcade.dswa := $F7FF;
        marcade.dswa_val := @burglarx_dip_a;
        marcade.dswb := $FFFF;
        marcade.dswb_val := @burglarx_dip_b;
      end;
    381:
      begin // Zero Point
        // cargar roms
        m68000_0.change_ram16_calls(zeropnt_getword, zeropnt_putword);
        if not(roms_load16w(@rom, zeropnt_rom)) then
          exit;
        if not(roms_load(@oki_rom[0, 0], zeropnt_oki)) then
          exit;
        copymemory(oki_6295_0.get_rom_addr, @oki_rom[0, 0], $40000);
        // convertir tiles
        getmem(ptemp, $800000);
        if not(roms_load(ptemp, zeropnt_tiles)) then
          exit;
        convert_graph(0, $8000);
        // convertir sprites
        if not(roms_load(ptemp, zeropnt_sprites)) then
          exit;
        convert_graph(1, $8000);
        freemem(ptemp);
        gr_mask := $7FFF;
        events_unico := events_zeropoint;
        show_mouse_cursor(true);
        // DIP
        marcade.dswa := $800;
        marcade.dswa_val := @burglarx_dip_a;
        marcade.dswb := 0;
        marcade.dswb_val := @zeropnt_dip_b;
      end;
  end;
  // final
  start_unico := true;
end;

end.
