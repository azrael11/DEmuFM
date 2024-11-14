unit raiden_hw;

interface

uses
  WinApi.Windows,
  nec_v20_v30,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  seibu_sound,
  misc_functions;

function start_raiden: boolean;

implementation

const
  raiden_rom_main: array [0 .. 3] of tipo_roms = ((n: '1.u0253'; l: $10000; p: 0; crc: $A4B12785), (n: '2.u0252'; l: $10000; p: 1; crc: $17640BD5), (n: '3.u022'; l: $20000; p: $20000; crc: $F6AF09D0), (n: '4j.u023'; l: $20000; p: $20001; crc: $505C4C5D));
  raiden_rom_sub: array [0 .. 1] of tipo_roms = ((n: '5.u042'; l: $20000; p: 0; crc: $ED03562E), (n: '6.u043'; l: $20000; p: 1; crc: $A19D5B5D));
  raiden_sound: tipo_roms = (n: '8.u212'; l: $10000; p: 0; crc: $CBE055C7);
  raiden_chars: array [0 .. 1] of tipo_roms = ((n: '9'; l: $8000; p: 1; crc: $1922B25E), (n: '10'; l: $8000; p: 0; crc: $5F90786A));
  raiden_bgtiles: tipo_roms = (n: 'sei420'; l: $80000; p: 0; crc: $DA151F0B);
  raiden_fgtiles: tipo_roms = (n: 'sei430'; l: $80000; p: 0; crc: $AC1F57AC);
  raiden_sprites: tipo_roms = (n: 'sei440'; l: $80000; p: 0; crc: $946D7BDE);
  raiden_oki: tipo_roms = (n: '7.u203'; l: $10000; p: 0; crc: $8F927822);
  CPU_SYNC = 4;

var
  main_rom: array [0 .. $5FFFF] of byte;
  sub_rom: array [0 .. $3FFFF] of byte;
  scroll_ram: array [0 .. $3F] of byte;
  ram: array [0 .. $8FFF] of byte;
  sub_ram: array [0 .. $2FFF] of byte;
  text: array [0 .. $7FF] of byte;
  bg_enabled, fg_enabled, tx_enabled, sp_enabled: boolean;

procedure update_video_raiden;
var
  f, x, y, nchar, scroll_x, scroll_y: word;
  color, atrib: byte;
  procedure draw_sprites(prio: byte);
  var
    atrib, atrib2, f, color: byte;
    sx, sy, nchar: word;
  begin
    for f := $FF downto 0 do
    begin
      atrib := ram[$7001 + (f * 8)];
      if (atrib and $80) = 0 then
        continue;
      atrib2 := ram[$7005 + (f * 8)];
      if prio <> (atrib2 shr 6) then
        continue;
      sx := ram[$7000 + (f * 8)];
      nchar := ram[$7002 + (f * 8)] + ((ram[$7003 + (f * 8)] and $F) shl 8);
      color := (atrib and $F) shl 4;
      sy := ram[$7004 + (f * 8)] + ((atrib2 and 1) shl 8);
      if (atrib2 and 1) <> 0 then
        sy := 240 - (sy or $FE00)
      else
        sy := 240 - sy;
      put_gfx_sprite(nchar, color + $200, (atrib and $40) <> 0, (atrib and $20) <> 0, 3);
      update_gfx_sprite(sx, sy, 4, 3);
    end;
  end;

begin
  for f := 0 to $3FF do
  begin
    // Text
    if tx_enabled then
    begin
      atrib := text[(f * 2) + 1];
      color := atrib and $F;
      if (gfx[0].buffer[f] or buffer_color[color + $30]) then
      begin
        x := f div 32;
        y := 31 - (f mod 32);
        nchar := text[(f * 2) + 0] or ((atrib and $C0) shl 2);
        put_gfx_trans(x * 8, y * 8, nchar, (color shl 4) + $300, 1, 0);
        gfx[0].buffer[f] := false;
      end;
    end;
    // Back
    if bg_enabled then
    begin
      atrib := sub_ram[(f * 2) + $2001];
      color := atrib shr 4;
      if (gfx[1].buffer[f] or buffer_color[color]) then
      begin
        x := f mod 32;
        y := 31 - (f div 32);
        nchar := sub_ram[(f * 2) + $2000] or ((atrib and $F) shl 8);
        put_gfx(x * 16, y * 16, nchar, color shl 4, 2, 1);
        gfx[1].buffer[f] := false;
      end;
    end;
    // Front
    if fg_enabled then
    begin
      atrib := sub_ram[(f * 2) + $2801];
      color := atrib shr 4;
      if (gfx[2].buffer[f] or buffer_color[color + $10]) then
      begin
        x := f mod 32;
        y := 31 - (f div 32);
        nchar := sub_ram[(f * 2) + $2800] or ((atrib and $F) shl 8);
        put_gfx_trans(x * 16, y * 16, nchar, (color shl 4) + $100, 3, 2);
        gfx[2].buffer[f] := false;
      end;
    end;
  end;
  // if sp_enabled then draw_sprites(0); No sirve de nada!
  if bg_enabled then
  begin
    scroll_y := ((scroll_ram[$12] and $F0) shl 4) or ((scroll_ram[$14] and $7F) shl 1) or ((scroll_ram[$14] and $80) shr 7);
    scroll_x := ((scroll_ram[$02] and $F0) shl 4) or ((scroll_ram[$04] and $7F) shl 1) or ((scroll_ram[$04] and $80) shr 7);
    scroll_x_y(2, 4, scroll_x, 256 - scroll_y);
  end
  else
    fill_full_screen(4, $500);
  if sp_enabled then
    draw_sprites(1);
  if fg_enabled then
  begin
    scroll_y := ((scroll_ram[$32] and $F0) shl 4) or ((scroll_ram[$34] and $7F) shl 1) or ((scroll_ram[$34] and $80) shr 7);
    scroll_x := ((scroll_ram[$22] and $F0) shl 4) or ((scroll_ram[$24] and $7F) shl 1) or ((scroll_ram[$24] and $80) shr 7);
    scroll_x_y(3, 4, scroll_x, 256 - scroll_y);
  end;
  if sp_enabled then
    draw_sprites(2);
  if sp_enabled then
    draw_sprites(3);
  if tx_enabled then
    update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 4);
  update_final_piece(16, 0, 224, 256, 4);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_raiden;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.right[0] then
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
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    if p_contrls.map_arcade.coin[0] then
      seibu_snd_0.input := (seibu_snd_0.input or 1)
    else
      seibu_snd_0.input := (seibu_snd_0.input and $FE);
    if p_contrls.map_arcade.coin[1] then
      seibu_snd_0.input := (seibu_snd_0.input or 2)
    else
      seibu_snd_0.input := (seibu_snd_0.input and $FD);
  end;
end;

procedure raiden_loop;
var
  f, h: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
      if f = 240 then
      begin
        nec_0.set_input(INT_IRQ, HOLD_LINE, $32); // $c8 div 4
        nec_1.set_input(INT_IRQ, HOLD_LINE, $32); // $c8 div 4
        update_video_raiden;
      end;
      for h := 1 to CPU_SYNC do
      begin
        // Main CPU
        nec_0.run(frame_main);
        frame_main := frame_main + nec_0.tframes - nec_0.contador;
        // Sub CPU
        nec_1.run(frame_sub);
        frame_sub := frame_sub + nec_1.tframes - nec_1.contador;
        // Sound CPU
        seibu_snd_0.run;
      end;
    end;
    events_raiden;
    video_sync;
  end;
end;

function raiden_getbyte(direccion: dword): byte;
begin
  case direccion of
    $0 .. $8FFF:
      raiden_getbyte := ram[direccion];
    $A000 .. $A00D:
      raiden_getbyte := seibu_snd_0.get((direccion and $F) shr 1);
    $E000:
      raiden_getbyte := marcade.in0 and $FF;
    $E001:
      raiden_getbyte := marcade.in0 shr 8;
    $E002, $E003:
      raiden_getbyte := $FF;
    $A0000 .. $FFFFF:
      raiden_getbyte := main_rom[direccion - $A0000];
  end;
end;

procedure raiden_putbyte(direccion: dword; valor: byte);
begin
  case direccion of
    $0 .. $8FFF:
      ram[direccion] := valor;
    $A000 .. $A00D:
      seibu_snd_0.put((direccion and $F) shr 1, valor);
    $C000 .. $C7FF:
      if text[direccion and $7FF] <> valor then
      begin
        text[direccion and $7FF] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $E004, $E005:
      ;
    $E006:
      begin
        bg_enabled := (valor and 1) = 0;
        fg_enabled := (valor and 2) = 0;
        tx_enabled := (valor and 4) = 0;
        sp_enabled := (valor and 8) = 0;
        main_screen.flip_main_screen := (valor and $40) <> 0;
      end;
    $F000 .. $F03F:
      scroll_ram[direccion and $3F] := valor;
    $A0000 .. $FFFFF:
      ; // ROM
  end;
end;

function raiden_sub_getbyte(direccion: dword): byte;
begin
  case direccion of
    $0 .. $2FFF:
      raiden_sub_getbyte := sub_ram[direccion];
    $3000 .. $3FFF:
      raiden_sub_getbyte := buffer_paleta[direccion and $FFF];
    $4000 .. $4FFF:
      raiden_sub_getbyte := ram[$8000 + (direccion and $FFF)];
    $C0000 .. $FFFFF:
      raiden_sub_getbyte := sub_rom[direccion - $C0000];
  end;
end;

procedure raiden_sub_putbyte(direccion: dword; valor: byte);
  procedure cambiar_color(dir: word);
  var
    color: tcolor;
    tmp_color: byte;
  begin
    tmp_color := buffer_paleta[dir or 1];
    color.b := pal4bit(tmp_color shr 0);
    tmp_color := buffer_paleta[dir];
    color.g := pal4bit(tmp_color shr 4);
    color.r := pal4bit(tmp_color);
    dir := dir shr 1;
    set_pal_color(color, dir);
    case dir of
      0 .. $FF:
        buffer_color[(dir shr 4) and $F] := true; // back
      $100 .. $1FF:
        buffer_color[((dir shr 4) and $F) + $10] := true; // front
      $300 .. $3FF:
        buffer_color[((dir shr 4) and $F) + $30] := true; // text
    end;
  end;

begin
  case direccion of
    $0 .. $1FFF:
      sub_ram[direccion] := valor;
    $2000 .. $27FF:
      if sub_ram[direccion] <> valor then
      begin
        sub_ram[direccion] := valor;
        gfx[1].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $2800 .. $2FFF:
      if sub_ram[direccion] <> valor then
      begin
        sub_ram[direccion] := valor;
        gfx[2].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $3000 .. $3FFF:
      if buffer_paleta[direccion and $FFF] <> valor then
      begin
        buffer_paleta[direccion and $FFF] := valor;
        cambiar_color(direccion and $FFE);
      end;
    $4000 .. $4FFF:
      ram[$8000 + (direccion and $FFF)] := valor;
    $C0000 .. $FFFFF:
      ; // ROM
  end;
end;

// Main
procedure reset_raiden;
begin
  nec_0.reset;
  nec_1.reset;
  frame_main := nec_0.tframes;
  frame_sub := nec_1.tframes;
  seibu_snd_0.reset;
  reset_video;
  reset_audio;
  marcade.in0 := $FFFF;
  seibu_snd_0.input := 0;
  bg_enabled := true;
  fg_enabled := true;
  tx_enabled := true;
  sp_enabled := true;
end;

function start_raiden: boolean;
const
  pc_x: array [0 .. 15] of dword = (0, 1, 2, 3, 16 + 0, 16 + 1, 16 + 2, 16 + 3, 16 * 16 * 2 + 0, 16 * 16 * 2 + 1, 16 * 16 * 2 + 2, 16 * 16 * 2 + 3, 16 * 16 * 2 + 16 + 0, 16 * 16 * 2 + 16 + 1, 16 * 16 * 2 + 16 + 2, 16 * 16 * 2 + 16 + 3);
  pc_y: array [0 .. 15] of dword = (0 * 16 * 2, 1 * 16 * 2, 2 * 16 * 2, 3 * 16 * 2, 4 * 16 * 2, 5 * 16 * 2, 6 * 16 * 2, 7 * 16 * 2, 8 * 16 * 2, 9 * 16 * 2, 10 * 16 * 2, 11 * 16 * 2, 12 * 16 * 2, 13 * 16 * 2, 14 * 16 * 2, 15 * 16 * 2);
  main_xor_table: array [0 .. $F] of word = ($200E, $0006, $000A, $0002, $240E, $000E, $04C2, $00C2, $008C, $0004, $0088, $0000, $048C, $000C, $04C0, $00C0);
  sub_xor_table: array [0 .. 7] of word = ($0080, $0080, $0244, $0288, $0288, $0288, $1041, $1009);
var
  memory_temp: pbyte;
  ptemp: pword;
  f: dword;
  tempw: word;
begin
  start_raiden := false;
  machine_calls.reset := reset_raiden;
  machine_calls.fps_max := 59.599998;
  machine_calls.general_loop := raiden_loop;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 256, 511, 512, 256, 511);
  screen_init(3, 512, 512, true);
  screen_mod_scroll(3, 512, 256, 511, 512, 256, 511);
  screen_init(4, 256, 512, false, true);
  start_video(224, 256);
  // Main CPU
  nec_0 := cpu_nec.create(10000000, 256 * CPU_SYNC, NEC_V30);
  nec_0.change_ram_calls(raiden_getbyte, raiden_putbyte);
  if not(roms_load16b(@main_rom, raiden_rom_main)) then
    exit;
  ptemp := @main_rom[$20000];
  for f := 0 to $1FFFF do
  begin
    tempw := ptemp^;
    tempw := tempw xor main_xor_table[f and $F];
    tempw := BITSWAP16(tempw, 15, 14, 10, 12, 11, 13, 9, 8, 3, 2, 5, 4, 7, 1, 6, 0);
    ptemp^ := tempw;
    inc(ptemp);
  end;
  // Sub CPU
  nec_1 := cpu_nec.create(10000000, 256 * CPU_SYNC, NEC_V30);
  nec_1.change_ram_calls(raiden_sub_getbyte, raiden_sub_putbyte);
  if not(roms_load16b(@sub_rom, raiden_rom_sub)) then
    exit;
  ptemp := @sub_rom[0];
  for f := 0 to $1FFFF do
  begin
    tempw := ptemp^;
    tempw := tempw xor sub_xor_table[f and 7];
    tempw := BITSWAP16(tempw, 15, 14, 13, 9, 11, 10, 12, 8, 2, 0, 5, 4, 7, 3, 1, 6);
    ptemp^ := tempw;
    inc(ptemp);
  end;
  getmem(memory_temp, $100000);
  // sound
  if not(roms_load(memory_temp, raiden_sound)) then
    exit;
  copymemory(@mem_snd, memory_temp, $8000);
  seibu_snd_0 := seibu_snd_type.create(SEIBU_OKI, 3579545, 256 * CPU_SYNC, memory_temp, true);
  // La memoria superior tambien esta encriptada, en teoria tanto opcodes como datos,
  // pero no ejecuta codigo, solo usa los datos. Los desencripto aqui.
  copymemory(@seibu_snd_0.sound_rom[0, 0], @memory_temp[$8000], $8000);
  seibu_snd_0.decript_extra(@seibu_snd_0.sound_rom[0, 0], $8000);
  copymemory(@seibu_snd_0.sound_rom[1, 0], @memory_temp[0], $8000);
  seibu_snd_0.decript_extra(@seibu_snd_0.sound_rom[1, 0], $8000);
  if not(roms_load(seibu_snd_0.oki_6295_get_rom_addr, raiden_oki)) then
    exit;
  // convertir chars
  if not(roms_load16b(memory_temp, raiden_chars)) then
    exit;
  init_gfx(0, 8, 8, $800);
  gfx[0].trans[15] := true;
  gfx_set_desc_data(4, 0, 8 * 8 * 4, 12, 8, 4, 0);
  convert_gfx(0, 0, memory_temp, @pc_x, @pc_y, false, true);
  // bgtiles
  if not(roms_load(memory_temp, raiden_bgtiles)) then
    exit;
  init_gfx(1, 16, 16, $1000);
  gfx_set_desc_data(4, 0, 16 * 16 * 4, 12, 8, 4, 0);
  convert_gfx(1, 0, memory_temp, @pc_x, @pc_y, false, true);
  // fgtiles
  if not(roms_load(memory_temp, raiden_fgtiles)) then
    exit;
  init_gfx(2, 16, 16, $1000);
  gfx[2].trans[15] := true;
  convert_gfx(2, 0, memory_temp, @pc_x, @pc_y, false, true);
  // convertir sprites
  if not(roms_load(memory_temp, raiden_sprites)) then
    exit;
  init_gfx(3, 16, 16, $1000);
  gfx[3].trans[15] := true;
  convert_gfx(3, 0, memory_temp, @pc_x, @pc_y, false, true);
  freemem(memory_temp);
  reset_raiden;
  start_raiden := true;
end;

end.
