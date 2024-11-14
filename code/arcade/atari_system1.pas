unit atari_system1;

interface

uses
  WinApi.Windows,
  m6502,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pokey,
  pal_engine,
  sound_engine,
  slapstic,
  ym_2151,
  atari_mo;

function start_atarisystem1: boolean;

implementation

const
  // System ROMS
  atari_sys1_bios: array [0 .. 1] of tipo_roms = ((n: '136032.205.l13'; l: $4000; p: $0; crc: $88D0BE26), (n: '136032.206.l12'; l: $4000; p: $1; crc: $3C79EF05));
  atari_sys1_char: tipo_roms = (n: '136032.104.f5'; l: $2000; p: 0; crc: $7A29DC07);
  peterpak_rom: array [0 .. 7] of tipo_roms = ((n: '136028.142'; l: $4000; p: $0; crc: $4F9FC020), (n: '136028.143'; l: $4000; p: $1; crc: $9FB257CC), (n: '136028.144'; l: $4000; p: $8000;
    crc: $50267619), (n: '136028.145'; l: $4000; p: $8001; crc: $7B6A5004), (n: '136028.146'; l: $4000; p: $10000; crc: $4183A67A), (n: '136028.147'; l: $4000; p: $10001; crc: $14E2D97B),
    (n: '136028.148'; l: $4000; p: $20000; crc: $230E8BA9), (n: '136028.149'; l: $4000; p: $20001; crc: $0FF0C13A));
  peterpak_sound: array [0 .. 1] of tipo_roms = ((n: '136028.101'; l: $4000; p: $8000; crc: $FF712AA2), (n: '136028.102'; l: $4000; p: $C000; crc: $89EA21A1));
  peterpak_back: array [0 .. 11] of tipo_roms = ((n: '136028.138'; l: $8000; p: 0; crc: $53EAA018), (n: '136028.139'; l: $8000; p: $10000; crc: $354A19CB), (n: '136028.140'; l: $8000; p: $20000;
    crc: $8D2C4717), (n: '136028.141'; l: $8000; p: $30000; crc: $BF59EA19), (n: '136028.150'; l: $8000; p: $80000; crc: $83362483), (n: '136028.151'; l: $8000; p: $90000; crc: $6E95094E),
    (n: '136028.152'; l: $8000; p: $A0000; crc: $9553F084), (n: '136028.153'; l: $8000; p: $B0000; crc: $C2A9B028), (n: '136028.105'; l: $4000; p: $104000; crc: $AC9A5A44), (n: '136028.108'; l: $4000;
    p: $114000; crc: $51941E64), (n: '136028.111'; l: $4000; p: $124000; crc: $246599F3), (n: '136028.114'; l: $4000; p: $134000; crc: $918A5082));
  peterpak_proms: array [0 .. 1] of tipo_roms = ((n: '136028.136'; l: $200; p: 0; crc: $861CFA36), (n: '136028.137'; l: $200; p: $200; crc: $8507E5EA));
  // Indiana
  indy_rom: array [0 .. 7] of tipo_roms = ((n: '136036.432'; l: $8000; p: $0; crc: $D888CDF1), (n: '136036.431'; l: $8000; p: $1; crc: $B7AC7431), (n: '136036.434'; l: $8000; p: $10000;
    crc: $802495FD), (n: '136036.433'; l: $8000; p: $10001; crc: $3A914E5C), (n: '136036.456'; l: $4000; p: $20000; crc: $EC146B09), (n: '136036.457'; l: $4000; p: $20001; crc: $6628DE01),
    (n: '136036.358'; l: $4000; p: $28000; crc: $D9351106), (n: '136036.359'; l: $4000; p: $28001; crc: $E731CAEA));
  indy_sound: array [0 .. 2] of tipo_roms = ((n: '136036.153'; l: $4000; p: $4000; crc: $95294641), (n: '136036.154'; l: $4000; p: $8000; crc: $CBFC6ADB), (n: '136036.155'; l: $4000; p: $C000;
    crc: $4C8233AC));
  indy_back: array [0 .. 15] of tipo_roms = ((n: '136036.135'; l: $8000; p: 0; crc: $FFA8749C), (n: '136036.139'; l: $8000; p: $10000; crc: $B682BFCA), (n: '136036.143'; l: $8000; p: $20000;
    crc: $7697DA26), (n: '136036.147'; l: $8000; p: $30000; crc: $4E9D664C), (n: '136036.136'; l: $8000; p: $80000; crc: $B2B403AA), (n: '136036.140'; l: $8000; p: $90000; crc: $EC0C19CA),
    (n: '136036.144'; l: $8000; p: $A0000; crc: $4407DF98), (n: '136036.148'; l: $8000; p: $B0000; crc: $70DCE06D), (n: '136036.137'; l: $8000; p: $100000; crc: $3F352547), (n: '136036.141'; l: $8000;
    p: $110000; crc: $9CBDFFD0), (n: '136036.145'; l: $8000; p: $120000; crc: $E828E64B), (n: '136036.149'; l: $8000; p: $130000; crc: $81503A23), (n: '136036.138'; l: $8000; p: $180000;
    crc: $48C4D79D), (n: '136036.142'; l: $8000; p: $190000; crc: $7FAAE75F), (n: '136036.146'; l: $8000; p: $1A0000; crc: $8AE5A7B5), (n: '136036.150'; l: $8000; p: $1B0000; crc: $A10C4BD9));
  indy_proms: array [0 .. 1] of tipo_roms = ((n: '136036.152'; l: $200; p: 0; crc: $4F96E57C), (n: '136036.151'; l: $200; p: $200; crc: $7DAF351F));
  // Marble
  marble_rom: array [0 .. 9] of tipo_roms = ((n: '136033.623'; l: $4000; p: $0; crc: $284ED2E9), (n: '136033.624'; l: $4000; p: $1; crc: $D541B021), (n: '136033.625'; l: $4000; p: $8000;
    crc: $563755C7), (n: '136033.626'; l: $4000; p: $8001; crc: $860FEEB3), (n: '136033.627'; l: $4000; p: $10000; crc: $D1DBD439), (n: '136033.628'; l: $4000; p: $10001; crc: $957D6801),
    (n: '136033.229'; l: $4000; p: $18000; crc: $C81D5C14), (n: '136033.630'; l: $4000; p: $18001; crc: $687A09F7), (n: '136033.107'; l: $4000; p: $20000; crc: $F3B8745B), (n: '136033.108'; l: $4000;
    p: $20001; crc: $E51EECAA));
  marble_sound: array [0 .. 1] of tipo_roms = ((n: '136033.421'; l: $4000; p: $8000; crc: $78153DC3), (n: '136033.422'; l: $4000; p: $C000; crc: $2E66300E));
  marble_back: array [0 .. 12] of tipo_roms = ((n: '136033.137'; l: $4000; p: 0; crc: $7A45F5C1), (n: '136033.138'; l: $4000; p: $4000; crc: $7E954A88), (n: '136033.139'; l: $4000; p: $10000;
    crc: $1EB1BB5F), (n: '136033.140'; l: $4000; p: $14000; crc: $8A82467B), (n: '136033.141'; l: $4000; p: $20000; crc: $52448965), (n: '136033.142'; l: $4000; p: $24000; crc: $B4A70E4F),
    (n: '136033.143'; l: $4000; p: $30000; crc: $7156E449), (n: '136033.144'; l: $4000; p: $34000; crc: $4C3E4C79), (n: '136033.145'; l: $4000; p: $40000; crc: $9062BE7F), (n: '136033.146'; l: $4000;
    p: $44000; crc: $14566DCA), (n: '136033.149'; l: $4000; p: $84000; crc: $B6658F06), (n: '136033.151'; l: $4000; p: $94000; crc: $84EE1C80), (n: '136033.153'; l: $4000; p: $A4000; crc: $DAA02926));
  marble_proms: array [0 .. 1] of tipo_roms = ((n: '136033.118'; l: $200; p: 0; crc: $2101B0ED), (n: '136033.119'; l: $200; p: $200; crc: $19F6E767));
  atari_sys1_mo_config: atari_motion_objects_config = (gfxindex: 1; // index to which gfx system */
    bankcount: 8; // number of motion object banks */
    linked: true; // are the entries linked? */
    split: true; // are the entries split? */
    reverse: false; // render in reverse order? */
    swapxy: false; // render in swapped X/Y order? */
    nextneighbor: false; // does the neighbor bit affect the next object? */
    slipheight: 0; // pixels per SLIP entry (0 for no-slip) */
    slipoffset: 0; // pixel offset for SLIPs */
    maxperline: $38; // maximum number of links to visit/scanline (0=all) */
    palettebase: $100; // base palette entry */
    maxcolors: $100; // maximum number of colors */
    transpen: 0; // transparent pen index */
    link_entry: (0, 0, 0, $003F); // mask for the link */
    code_entry: (data_lower: (0, $FFFF, 0, 0); data_upper: (0, 0, 0, 0));
    // mask for the code index */
    color_entry: (data_lower: (0, $FF00, 0, 0); data_upper: (0, 0, 0, 0)); // mask for the color */
    xpos_entry: (0, 0, $3FE0, 0); // mask for the X position */
    ypos_entry: ($3FE0, 0, 0, 0); // mask for the Y position */
    width_entry: (0, 0, 0, 0); // mask for the width, in tiles*/
    height_entry: ($000F, 0, 0, 0); // mask for the height, in tiles */
    hflip_entry: ($8000, 0, 0, 0); // mask for the horizontal flip */
    vflip_entry: (0, 0, 0, 0); // mask for the vertical flip */
    priority_entry: (0, 0, $8000, 0); // mask for the priority */
    neighbor_entry: (0, 0, 0, 0); // mask for the neighbor */
    absolute_entry: (0, 0, 0, 0); // mask for absolute coordinates */
    special_entry: (0, $FFFF, 0, 0); // mask for the special value */
    specialvalue: $FFFF; // resulting value to indicate "special" */
  );
  CPU_SYNC = 1;

var
  rom: array [0 .. $3FFFF] of word;
  slapstic_rom: array [0 .. 3, 0 .. $FFF] of word;
  ram: array [0 .. $FFF] of word;
  ram2: array [0 .. $7FFFF] of word;
  ram3: array [0 .. $1FFF] of word;
  sound_latch, main_latch: byte;
  write_eeprom, main_pending, sound_pending: boolean;
  // Video
  playfield_lookup: array [0 .. $FF] of word;
  bank_color_shift: array [0 .. 7] of byte;
  linea, scroll_x, scroll_y, scroll_y_latch, bankselect: word;
  rom_bank, vblank, playfield_tile_bank: byte;
  eeprom_ram: array [0 .. $7FF] of byte;

procedure update_video_atari_sys1;
var
  f, color, x, y, nchar, atrib, atrib2: word;
  gfx_index: byte;
begin
  fill_full_screen(3, $2000);
  for f := 0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib := ram3[($3000 + (f * 2)) shr 1];
    color := (atrib shr 10) and 7;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := atrib and $3FF;
      if (atrib and $2000) = 0 then
        put_gfx_trans(x * 8, y * 8, nchar, color shl 2, 1, 0)
      else
        put_gfx(x * 8, y * 8, nchar, color shl 2, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  for f := 0 to $FFF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib := ram3[f];
    atrib2 := playfield_lookup[((atrib shr 8) and $7F) or (playfield_tile_bank shl 7)];
    gfx_index := (atrib2 shr 8) and $F;
    color := $20 + (((atrib2 shr 12) and $F) shl bank_color_shift[gfx_index]);
    if (gfx[1].buffer[f] or buffer_color[color]) then
    begin
      nchar := ((atrib2 and $FF) shl 8) or (atrib and $FF);
      put_gfx_flip(x * 8, y * 8, nchar, color shl 4, 2, gfx_index, ((atrib shr 15) and 1) <> 0, false);
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(2, 3, scroll_x, scroll_y);
  atari_mo_0.draw(0, 256, -1);
  update_region(0, 0, 512, 256, 1, 0, 0, 512, 256, 3);
  update_final_piece(0, 0, 336, 240, 3);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_atari_sys1;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or 4);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
  end;
end;

procedure atari_sys1_loop;
var
  frame_m, frame_s: single;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for linea := 0 to 261 do
      begin
        // main
        for h := 1 to CPU_SYNC do
        begin
          m68000_0.run(frame_m);
          frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
          // sound
          m6502_0.run(frame_s);
          frame_s := frame_s + m6502_0.tframes - m6502_0.contador;
        end;
        case linea of
          239:
            begin
              update_video_atari_sys1;
              vblank := $0;
              m68000_0.irq[4] := ASSERT_LINE;
            end;
          261:
            vblank := $10;
        end;
      end;
      scroll_y := scroll_y_latch;
      events_atari_sys1;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function atari_sys1_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      atari_sys1_getword := rom[direccion shr 1];
    $80000 .. $87FFF:
      begin
        atari_sys1_getword := slapstic_rom[rom_bank, (direccion and $1FFF) shr 1];
        rom_bank := slapstic_0.slapstic_tweak((direccion and $7FFF) shr 1);
      end;
    $2E0000:
      if m68000_0.irq[3] <> CLEAR_LINE then
        atari_sys1_getword := $80
      else
        atari_sys1_getword := 0;
    $400000 .. $401FFF:
      atari_sys1_getword := ram[(direccion and $1FFF) shr 1];
    $900000 .. $9FFFFF:
      atari_sys1_getword := ram2[(direccion and $FFFFF) shr 1];
    $A00000 .. $A03FFF:
      atari_sys1_getword := ram3[(direccion and $3FFF) shr 1];
    $B00000 .. $B007FF:
      atari_sys1_getword := buffer_paleta[(direccion and $7FF) shr 1];
    $F00000 .. $F00FFF:
      atari_sys1_getword := eeprom_ram[(direccion and $FFF) shr 1];
    $F20000 .. $F20007:
      atari_sys1_getword := $FF; // trakball_r
    $F40000 .. $F4001F:
      atari_sys1_getword := 0; // Controles
    $F60000 .. $F60003:
      atari_sys1_getword := marcade.in0 or vblank or ($80 * (byte(sound_pending)));
    $FC0000:
      begin
        main_pending := false;
        m68000_0.irq[6] := CLEAR_LINE;
        atari_sys1_getword := main_latch;
      end;
  end;
end;

procedure change_color(tmp_color, numero: word);
var
  color: tcolor;
begin
  color.r := pal4bit_i(tmp_color shr 8, tmp_color shr 12);
  color.g := pal4bit_i(tmp_color shr 4, tmp_color shr 12);
  color.b := pal4bit_i(tmp_color, tmp_color shr 12);
  set_pal_color(color, numero);
  if numero < $20 then
    buffer_color[(numero shr 2) and 7] := true
  else
    buffer_color[$20 + ((numero shr 4) and $F)] := true
end;

procedure atari_sys1_putword(direccion: dword; valor: word);
var
  diff: word;
begin
  case direccion of
    0 .. $7FFFF:
      ;
    $80000 .. $87FFF:
      rom_bank := slapstic_0.slapstic_tweak((direccion and $7FFF) shr 1);
    $400000 .. $401FFF:
      ram[(direccion and $1FFF) shr 1] := valor;
    $800000:
      scroll_x := valor;
    $820000:
      begin
        scroll_y_latch := valor;
        if linea < 240 then
          scroll_y := valor - (linea + 1)
        else
          scroll_y := valor;
      end;
    $840000:
      ; // atarisy1_priority_w
    $860000:
      begin // atarisy1_bankselect_w
        diff := bankselect xor valor;
        // playfield bank select
        if (diff and $4) <> 0 then
        begin
          playfield_tile_bank := (valor shr 2) and 1;
          fillchar(gfx[1].buffer[0], $1000, 1);
        end;
        if (diff and $80) <> 0 then
        begin
          if (valor and $80) <> 0 then
            m6502_0.change_reset(CLEAR_LINE)
          else
            m6502_0.change_reset(ASSERT_LINE);
          // if (valor and $80)<>0 then tcm5220.reset;
        end;
        atari_mo_0.set_bank((valor shr 3) and 7);
        // Revisar para Road runners!!!!!
        // update_timers(scanline);
        bankselect := valor;
      end;
    $880000:
      ; // watchdog
    $8A0000:
      m68000_0.irq[4] := CLEAR_LINE;
    $8C0000:
      write_eeprom := true;
    $900000 .. $9FFFFF:
      ram2[(direccion and $FFFFF) shr 1] := valor;
    $A00000 .. $A01FFF:
      if ram3[(direccion and $3FFF) shr 1] <> valor then
      begin
        ram3[(direccion and $3FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $A02000 .. $A02FFF:
      ram3[(direccion and $3FFF) shr 1] := valor;
    $A03000 .. $A03FFF:
      if ram3[(direccion and $3FFF) shr 1] <> valor then
      begin
        ram3[(direccion and $3FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $B00000 .. $B007FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $F00000 .. $F00FFF:
      if write_eeprom then
      begin
        eeprom_ram[(direccion and $FFF) shr 1] := valor and $FF;
        write_eeprom := false;
      end;
    $F40000 .. $F4001F:
      ; // joystick_w
    $F80000, $FE0000:
      begin
        sound_latch := valor;
        sound_pending := true;
        m6502_0.change_nmi(ASSERT_LINE)
      end;
  end;
end;

function atari_sys1_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $FFF, $4000 .. $FFFF:
      atari_sys1_snd_getbyte := mem_snd[direccion];
    $1000 .. $100F:
      ; // via6522_device, read
    $1801:
      atari_sys1_snd_getbyte := ym2151_0.status;
    $1810:
      begin
        sound_pending := false;
        m6502_0.change_nmi(CLEAR_LINE);
        atari_sys1_snd_getbyte := sound_latch;
      end;
    $1820:
      atari_sys1_snd_getbyte := marcade.in2 or ($8 * byte(sound_pending)) or ($10 * byte(main_pending));
    $1870 .. $187F:
      pokey_0.read(direccion and $F);
  end;
end;

procedure atari_sys1_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FFF:
      mem_snd[direccion] := valor;
    $1000 .. $100F:
      ; // via6522_device, write
    $1800:
      ym2151_0.reg(valor);
    $1801:
      ym2151_0.write(valor);
    $1810:
      begin
        main_latch := valor;
        main_pending := true;
        m68000_0.irq[6] := ASSERT_LINE;
      end;
    $1824 .. $1825:
      ; // led_w
    $1870 .. $187F:
      pokey_0.write(direccion and $F, valor);
    $4000 .. $FFFF:
      ;
  end;
end;

procedure atari_sys1_sound_update;
begin
  ym2151_0.update;
  pokey_0.update;
  // tms5220_update
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  m6502_0.change_irq(irqstate);
end;

// Main
procedure reset_atari_sys1;
begin
  m68000_0.reset;
  m6502_0.reset;
  ym2151_0.reset;
  pokey_0.reset;
  slapstic_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF6F;
  marcade.in1 := $FF;
  marcade.in2 := $87;
  scroll_x := 0;
  scroll_y := 0;
  rom_bank := slapstic_0.current_bank;
  vblank := $10;
  bankselect := 0;
  playfield_tile_bank := 0;
  write_eeprom := false;
  sound_pending := false;
  main_pending := false;
  main_latch := 0;
  sound_latch := 0;
end;

function start_atarisystem1: boolean;
var
  memory_temp: array [0 .. $3FFFF] of byte;
  proms_temp: array [0 .. $3FF] of byte;
  f: dword;
  mem_temp, ptemp: pbyte;
  ptempw: pword;
  motable: array [0 .. $FF] of word;
  bank_gfx: array [0 .. 2, 0 .. 7] of byte;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8, 9, 10, 11);
  pc_y: array [0 .. 7] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16);
  // Convertir los GFX del fondo y MO
  procedure convert_back(size_back: dword);
  var
    gfx_index, bank, color, offset, obj, i, bpp: byte;
    f: dword;
  const
    ps_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
    ps_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
    PROM2_PLANE_4_ENABLE = $10;
    PROM2_PLANE_5_ENABLE = $20;
    PROM1_OFFSET_MASK = $0F;
    PROM2_PF_COLOR_MASK = $0F;
    PROM2_MO_COLOR_MASK = $07;
    PROM1_BANK_1 = $10;
    PROM1_BANK_2 = $20;
    PROM1_BANK_3 = $40;
    PROM1_BANK_4 = $80;
    PROM2_BANK_5 = $40;
    PROM2_BANK_6_OR_7 = $80;
    PROM2_BANK_7 = $08;
    function get_bank(prom1, prom2, bpp: byte; size_back: dword): byte;
    var
      bank_index: byte;
      srcdata: pbyte;
    begin
      // determine the bank index
      if ((prom1 and PROM1_BANK_1) = 0) then
        bank_index := 1
      else if ((prom1 and PROM1_BANK_2) = 0) then
        bank_index := 2
      else if ((prom1 and PROM1_BANK_3) = 0) then
        bank_index := 3
      else if ((prom1 and PROM1_BANK_4) = 0) then
        bank_index := 4
      else if ((prom2 and PROM2_BANK_5) = 0) then
        bank_index := 5
      else if ((prom2 and PROM2_BANK_6_OR_7) = 0) then
      begin
        if ((prom2 and PROM2_BANK_7) = 0) then
          bank_index := 7
        else
          bank_index := 6;
      end
      else
      begin
        get_bank := 0;
        exit;
      end;
      // find the bank, si ya lo tengo, no hago nada me salgo
      if (bank_gfx[bpp - 4][bank_index] <> 0) then
      begin
        get_bank := bank_gfx[bpp - 4][bank_index];
        exit;
      end;
      // if the bank is out of range, call it 0
      if ($80000 * (bank_index - 1) >= size_back) then
      begin
        get_bank := 0;
        exit;
      end;
      // decode the graphics */
      srcdata := mem_temp;
      inc(srcdata, $80000 * (bank_index - 1));
      init_gfx(gfx_index, 8, 8, $1000);
      gfx[gfx_index].trans[0] := true;
      case bpp of
        4:
          begin
            gfx_set_desc_data(4, 0, 8 * 8, 3 * 8 * $10000, 2 * 8 * $10000, 1 * 8 * $10000, 0 * 8 * $10000);
            convert_gfx(gfx_index, 0, srcdata, @ps_x, @ps_y, false, false);
          end;
        5:
          begin
            gfx_set_desc_data(5, 0, 8 * 8, 4 * 8 * $10000, 3 * 8 * $10000, 2 * 8 * $10000, 1 * 8 * $10000, 0 * 8 * $10000);
            convert_gfx(gfx_index, 0, srcdata, @ps_x, @ps_y, false, false);
          end;
        6:
          begin
            gfx_set_desc_data(6, 0, 8 * 8, 5 * 8 * $10000, 4 * 8 * $10000, 3 * 8 * $10000, 2 * 8 * $10000, 1 * 8 * $10000, 0 * 8 * $10000);
            convert_gfx(gfx_index, 0, srcdata, @ps_x, @ps_y, false, false);
          end;
      end;
      // set the entry and return it
      bank_gfx[bpp - 4][bank_index] := gfx_index;
      bank_color_shift[gfx_index] := bpp - 3;
      get_bank := gfx_index;
      gfx_index := gfx_index + 1;
    end;

  begin
    ptemp := mem_temp;
    for f := 0 to (size_back - 1) do
    begin
      ptemp^ := not(ptemp^);
      inc(ptemp);
    end;
    fillchar(bank_gfx, 3 * 8, 0);
    gfx_index := 1;
    for obj := 0 to 1 do
    begin
      for i := 0 to 255 do
      begin
        bpp := 4;
        if (proms_temp[$200 + i + ($100 * obj)] and PROM2_PLANE_4_ENABLE) <> 0 then
          bpp := 5
        else if (proms_temp[$200 + i + ($100 * obj)] and PROM2_PLANE_5_ENABLE) <> 0 then
          bpp := 6;
        // determine the offset
        offset := proms_temp[i + $100 * obj] and PROM1_OFFSET_MASK;
        // determine the bank
        bank := get_bank(proms_temp[i + ($100 * obj)], proms_temp[$200 + i + ($100 * obj)], bpp, size_back);
        // set the value */
        if (obj = 0) then
        begin
          // playfield case
          color := (not(proms_temp[$200 + i + ($100 * obj)]) and PROM2_PF_COLOR_MASK) shr (bpp - 4);
          if (bank = 0) then
          begin
            bank := 1;
            offset := 0;
            color := 0;
          end;
          playfield_lookup[i] := offset or (bank shl 8) or (color shl 12);
        end
        else
        begin
          // motion objects (high bit ignored)
          color := (not(proms_temp[$200 + i + ($100 * obj)]) and PROM2_MO_COLOR_MASK) shr (bpp - 4);
          motable[i] := offset or (bank shl 8) or (color shl 12);
        end;
      end;
    end;
  end;

begin
  start_atarisystem1 := false;
  machine_calls.general_loop := atari_sys1_loop;
  machine_calls.reset := reset_atari_sys1;
  machine_calls.fps_max := 59.922743;
  start_audio(true);
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 512);
  screen_mod_scroll(2, 512, 512, 511, 512, 512, 511);
  screen_init(3, 512, 512, false, true);
  start_video(336, 240);
  // cargar BIOS
  if not(roms_load16w(@rom, atari_sys1_bios)) then
    exit;
  // Main CPU
  m68000_0 := cpu_m68000.create(14318180 div 2, 262 * CPU_SYNC);
  m68000_0.change_ram16_calls(atari_sys1_getword, atari_sys1_putword);
  // Sound CPU
  m6502_0 := cpu_m6502.create(14318180 div 8, 262 * CPU_SYNC, TCPU_M6502);
  m6502_0.change_ram_calls(atari_sys1_snd_getbyte, atari_sys1_snd_putbyte);
  m6502_0.init_sound(atari_sys1_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(14318180 div 4);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  pokey_0 := pokey_chip.create(14318180 div 8);
  // convertir chars
  if not(roms_load(@memory_temp, atari_sys1_char)) then
    exit;
  init_gfx(0, 8, 8, $200);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
  convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  // TMS5520
  case main_vars.machine_type of
    244:
      begin // Peter Pack Rat
        if not(roms_load16w(@memory_temp, peterpak_rom)) then
          exit;
        copymemory(@rom[$10000 shr 1], @memory_temp[$0], $8000 * 3);
        // Slapstic
        slapstic_0 := slapstic_type.create(107, true);
        copymemory(@slapstic_rom[0, 0], @memory_temp[$20000], $2000);
        copymemory(@slapstic_rom[1, 0], @memory_temp[$22000], $2000);
        copymemory(@slapstic_rom[2, 0], @memory_temp[$24000], $2000);
        copymemory(@slapstic_rom[3, 0], @memory_temp[$26000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, peterpak_sound)) then
          exit;
        // convertir fondo y mo
        getmem(mem_temp, $180000);
        fillchar(mem_temp^, $180000, $FF);
        if not(roms_load(@proms_temp, peterpak_proms)) then
          exit;
        if not(roms_load(mem_temp, peterpak_back)) then
          exit;
        convert_back($180000);
        freemem(mem_temp);
      end;
    263:
      begin // Indiana Jones
        if not(roms_load16w(@memory_temp, indy_rom)) then
          exit;
        copymemory(@rom[$10000 shr 1], @memory_temp[$0], $8000 * 5);
        // Slapstic
        slapstic_0 := slapstic_type.create(105, true);
        copymemory(@slapstic_rom[0, 0], @memory_temp[$28000], $2000);
        copymemory(@slapstic_rom[1, 0], @memory_temp[$2A000], $2000);
        copymemory(@slapstic_rom[2, 0], @memory_temp[$2C000], $2000);
        copymemory(@slapstic_rom[3, 0], @memory_temp[$2E000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, indy_sound)) then
          exit;
        // convertir fondo y mo
        getmem(mem_temp, $200000);
        fillchar(mem_temp^, $200000, $FF);
        if not(roms_load(@proms_temp, indy_proms)) then
          exit;
        if not(roms_load(mem_temp, indy_back)) then
          exit;
        convert_back($200000);
        freemem(mem_temp);
      end;
    264:
      begin // Marble Madness
        if not(roms_load16w(@memory_temp, marble_rom)) then
          exit;
        copymemory(@rom[$10000 shr 1], @memory_temp[$0], $8000 * 4);
        // Slapstic
        slapstic_0 := slapstic_type.create(103, true);
        copymemory(@slapstic_rom[0, 0], @memory_temp[$20000], $2000);
        copymemory(@slapstic_rom[1, 0], @memory_temp[$22000], $2000);
        copymemory(@slapstic_rom[2, 0], @memory_temp[$24000], $2000);
        copymemory(@slapstic_rom[3, 0], @memory_temp[$26000], $2000);
        // cargar sonido
        if not(roms_load(@mem_snd, marble_sound)) then
          exit;
        // convertir fondo y mo
        getmem(mem_temp, $100000);
        fillchar(mem_temp^, $100000, $FF);
        if not(roms_load(@proms_temp, marble_proms)) then
          exit;
        if not(roms_load(mem_temp, marble_back)) then
          exit;
        convert_back($100000);
        freemem(mem_temp);
      end;
  end;
  // atari mo
  atari_mo_0 := tatari_mo.create(nil, @ram3[$2000 shr 1], atari_sys1_mo_config, 3, 336 + 8, 240 + 8);
  ptempw := atari_mo_0.get_codelookup;
  for f := 0 to $FFFF do
  begin
    ptempw^ := (f and $FF) or ((motable[f shr 8] and $FF) shl 8);
    inc(ptempw);
  end;
  ptempw := atari_mo_0.get_colorlookup;
  ptemp := atari_mo_0.get_gfxlookup;
  for f := 0 to $FF do
  begin
    ptempw^ := ((motable[f] shr 12) and $F) shl 1;
    inc(ptempw);
    ptemp^ := (motable[f] shr 8) and $F;
    inc(ptemp);
  end;
  // final
  reset_atari_sys1;
  start_atarisystem1 := true;
end;

end.
