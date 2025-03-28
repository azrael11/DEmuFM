unit rainbowislands_hw;

// {$DEFINE MCU=1}

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  taitosnd,
  rom_engine,
  pal_engine,
  sound_engine,
{$IFDEF MCU}
  taito_cchip,
{$ELSE IF}
  rainbow_cchip;
{$ENDIF}
function start_rainbowislands: boolean;

implementation

const
  rainbow_rom: array [0 .. 5] of tipo_roms = ((n: 'b22-10-1.19'; l: $10000; p: 0; crc: $E34A50CA), (n: 'b22-11-1.20'; l: $10000; p: $1; crc: $6A31A093), (n: 'b22-08-1.21'; l: $10000; p: $20000; crc: $15D6E17A), (n: 'b22-09-1.22'; l: $10000; p: $20001; crc: $454E66BC),
    (n: 'b22-03.23'; l: $20000; p: $40000; crc: $3EBB0FB8), (n: 'b22-04.24'; l: $20000; p: $40001; crc: $91625E7F));
  rainbow_char: tipo_roms = (n: 'b22-01.2'; l: $80000; p: 0; crc: $B76C9168);
  rainbow_sound: tipo_roms = (n: 'b22-14.43'; l: $10000; p: 0; crc: $113C1A5B);
  rainbow_sprites1: tipo_roms = (n: 'b22-02.5'; l: $80000; p: 0; crc: $1B87ECF0);
  rainbow_sprites2: array [0 .. 1] of tipo_roms = ((n: 'b22-12.7'; l: $10000; p: $80000; crc: $67A76DC6), (n: 'b22-13.6'; l: $10000; p: $80001; crc: $2FDA099F));
  rainbowe_rom: array [0 .. 5] of tipo_roms = ((n: 'b39-01.19'; l: $10000; p: 0; crc: $50690880), (n: 'b39-02.20'; l: $10000; p: $1; crc: $4DEAD71F), (n: 'b39-03.21'; l: $10000; p: $20000; crc: $4A4CB785), (n: 'b39-04.22'; l: $10000; p: $20001; crc: $4CAA53BD), (n: 'b22-03.23';
    l: $20000; p: $40000; crc: $3EBB0FB8), (n: 'b22-04.24'; l: $20000; p: $40001; crc: $91625E7F));
{$IFDEF MCU}rainbow_cchip_eeprom: tipo_roms = (n: 'cchip_b22-15.53'; l: $2000; p: 0; crc: $08C588A6);
  rainbowe_cchip_eeprom: tipo_roms = (n: 'cchip_b39-05.53'; l: $2000; p: 0; crc: $397735E3);
{$ENDIF}
  // DIP
  rainbow_dip1: array [0 .. 2] of def_dip = ((mask: $30; name: 'Coin A'; number: 4; dip: ((dip_val: $10; dip_name: 'ModeA 2C-1C/ModeB 3C-1C'), (dip_val: $30; dip_name: 'ModeAB 1C-1C'), (dip_val: $0; dip_name: 'ModeA 2C-3C/ModeB 4C-1C'), (dip_val: $20;
    dip_name: 'ModeA 1C-2C/ModeB 2C-1C'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Coin B'; number: 4; dip: ((dip_val: $40; dip_name: 'ModeA 2C-1C/ModeB 1C-4C'), (dip_val: $C0; dip_name: 'ModeA 1C-1C/ModeB 1C-2C'), (dip_val: $0;
    dip_name: 'ModeA 2C-3C/ModeB 1C-6C'), (dip_val: $80; dip_name: 'ModeA 1C-2C/ModeB 1C-3C'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // DIP
  rainbow_dip2: array [0 .. 5] of def_dip = ((mask: $4; name: 'Bonus Life'; number: 2; dip: ((dip_val: $4; dip_name: '100k 1000k'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Complete Bonus'; number: 2;
    dip: ((dip_val: $8; dip_name: '1up'), (dip_val: $0; dip_name: '100k'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Lives'; number: 4;
    dip: ((dip_val: $10; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $30; dip_name: '3'), (dip_val: $20; dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Languaje'; number: 2;
    dip: ((dip_val: $0; dip_name: 'English'), (dip_val: $40; dip_name: 'Japanese'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Coin Mode'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Mode A (Japan)'), (dip_val: $0; dip_name: 'Mode B (World)'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  CPU_SYNC = 1;

var
  scroll_x1, scroll_y1, scroll_x2, scroll_y2: word;
  bank_sound: array [0 .. 3, $0 .. $3FFF] of byte;
  rom: array [0 .. $3FFFF] of word;
  ram1, ram3: array [0 .. $1FFF] of word;
  ram2: array [0 .. $7FFF] of word;
  spritebank, sound_bank: byte;

procedure update_video_rainbow;
var
  f, x, y, nchar, atrib, color: word;
  flipx, flipy: boolean;
begin
  for f := $FFF downto $0 do
  begin
    // background
    atrib := ram2[f * 2];
    color := atrib and $7F;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := (ram2[$1 + (f * 2)]) and $3FFF;
      flipx := (atrib and $4000) <> 0;
      flipy := (atrib and $8000) <> 0;
      put_gfx_flip(x * 8, y * 8, nchar, color shl 4, 1, 0, flipx, flipy);
      gfx[0].buffer[f] := false;
    end;
    // foreground
    atrib := ram2[$4000 + (f * 2)];
    color := atrib and $7F;
    if (gfx[0].buffer[f + $1000] or buffer_color[color]) then
    begin
      x := f mod 64;
      y := f div 64;
      nchar := (ram2[$4001 + (f * 2)]) and $3FFF;
      flipx := (atrib and $4000) <> 0;
      flipy := (atrib and $8000) <> 0;
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 4, 2, 0, flipx, flipy);
      gfx[0].buffer[f + $1000] := false;
    end;
  end;
  scroll_x_y(1, 3, scroll_x1, scroll_y1);
  // Sprites
  for f := $FF downto 0 do
  begin
    nchar := (ram3[$2 + (f * 4)]) mod $1400;
    atrib := ram3[f * 4];
    color := ((atrib and $F) or (spritebank shl 4)) shl 4;
    put_gfx_sprite(nchar, color, (atrib and $4000) <> 0, (atrib and $8000) <> 0, 1);
    x := ram3[$3 + (f * 4)] + 16;
    y := ram3[$1 + (f * 4)];
    update_gfx_sprite(x and $1FF, y and $1FF, 3, 1);
  end;
  scroll_x_y(2, 3, scroll_x2, scroll_y2);
  update_final_piece(16, 16, 320, 224, 3);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_rainbow;
begin
  if event.arcade then
  begin
    // 800007
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // 800009
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    // 80000B
    if p_contrls.map_arcade.left[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.right[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but0[0] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.but1[0] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
  end;
end;

procedure rainbow_loop;
var
  frame_m, frame_s{$IFDEF MCU}, frame_mcu{$ENDIF}: single;
  h, f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := tc0140syt_0.z80.tframes;
{$IFDEF MCU}frame_mcu := cchip_0.upd7810.tframes; {$ENDIF}
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // Main CPU
          m68000_0.run(frame_m);
          frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
          // Sound CPU
          tc0140syt_0.z80.run(frame_s);
          frame_s := frame_s + tc0140syt_0.z80.tframes - tc0140syt_0.z80.contador;
          // MCU
{$IFDEF MCU}
          cchip_0.upd7810.run(frame_mcu);
          frame_mcu := frame_mcu + cchip_0.upd7810.tframes - cchip_0.upd7810.contador;
{$ENDIF}
        end;
        if f = 239 then
        begin
          update_video_rainbow;
          m68000_0.irq[4] := HOLD_LINE;
{$IFDEF MCU}cchip_0.set_int; {$ENDIF}
        end;
      end;
      events_rainbow;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function rainbow_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $7FFFF:
      rainbow_getword := rom[direccion shr 1];
    $10C000 .. $10FFFF:
      rainbow_getword := ram1[(direccion and $3FFF) shr 1];
    $200000 .. $203FFF:
      rainbow_getword := buffer_paleta[(direccion and $3FFF) shr 1];
    $390000 .. $390002:
      rainbow_getword := marcade.dswa;
    $3B0000 .. $3B0002:
      rainbow_getword := marcade.dswb;
    $3E0002:
      if m68000_0.read_8bits_hi_dir then
        rainbow_getword := tc0140syt_0.comm_r;
{$IFDEF MCU}
    $800000 .. $8007FF:
      rainbow_getword := cchip_0.mem_r((direccion and $7FF) shr 1);
    $800800 .. $800FFF:
      rainbow_getword := cchip_0.asic_r((direccion and $7FF) shr 1);
{$ELSE IF}
    $800000 .. $8007FF:
      rainbow_getword := rbisland_cchip_ram_r(direccion and $7FF);
    $800802:
      rainbow_getword := rbisland_cchip_ctrl_r;
{$ENDIF}
    $C00000 .. $C0FFFF:
      rainbow_getword := ram2[(direccion and $FFFF) shr 1];
    $D00000 .. $D03FFF:
      rainbow_getword := ram3[(direccion and $3FFF) shr 1];
  end;
end;

procedure rainbow_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word);
  var
    color: tcolor;
  begin
    color.b := pal5bit(tmp_color shr 10);
    color.g := pal5bit(tmp_color shr 5);
    color.r := pal5bit(tmp_color);
    set_pal_color(color, numero);
    buffer_color[(numero shr 4) and $7F] := true;
  end;

begin
  case direccion of
    0 .. $7FFFF:
      ;
    $10C000 .. $10FFFF:
      ram1[(direccion and $3FFF) shr 1] := valor;
    $200000 .. $200FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color(valor, (direccion and $FFF) shr 1);
      end;
    $201000 .. $203FFF:
      buffer_paleta[(direccion and $3FFF) shr 1] := valor;
    $350008, $3C0000:
      ;
    $3A0000:
      spritebank := (valor and $E0) shr 5;
    $3E0000:
      tc0140syt_0.port_w(valor and $FF);
    $3E0002:
      tc0140syt_0.comm_w(valor and $FF);
{$IFDEF MCU}
    $800000 .. $8007FF:
      cchip_0.mem_w((direccion and $7FF) shr 1, valor);
    $800800 .. $800FFF:
      cchip_0.asic_w((direccion and $7FF) shr 1, valor);
{$ELSE IF}
    $800000 .. $8007FF:
      rbisland_cchip_ram_w(direccion and $7FF, valor);
    $800802:
      rbisland_cchip_ctrl_w;
    $800C00:
      rbisland_cchip_bank_w(valor);
{$ENDIF}
    $C00000 .. $C03FFF:
      begin
        ram2[(direccion and $FFFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $3FFF) shr 2] := true;
      end;
    $C04000 .. $C07FFF, $C0C000 .. $C0FFFF:
      ram2[(direccion and $FFFF) shr 1] := valor;
    $C08000 .. $C0BFFF:
      begin
        ram2[(direccion and $FFFF) shr 1] := valor;
        gfx[0].buffer[((direccion and $3FFF) shr 2) + $1000] := true;
      end;
    $C20000:
      scroll_y1 := (512 - valor) and $1FF;
    $C20002:
      scroll_y2 := (512 - valor) and $1FF;
    $C40000:
      scroll_x1 := (512 - valor) and $1FF;
    $C40002:
      scroll_x2 := (512 - valor) and $1FF;
    $C50000 .. $C50003:
      ;
    $D00000 .. $D03FFF:
      ram3[(direccion and $3FFF) shr 1] := valor;
  end;
end;

function rainbow_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $4000 .. $7FFF:
      rainbow_snd_getbyte := bank_sound[sound_bank, direccion and $3FFF];
    $9001:
      rainbow_snd_getbyte := ym2151_0.status;
    $A001:
      rainbow_snd_getbyte := tc0140syt_0.slave_comm_r;
  else
    rainbow_snd_getbyte := mem_snd[direccion];
  end;
end;

procedure rainbow_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $9000:
      ym2151_0.reg(valor);
    $9001:
      ym2151_0.write(valor);
    $A000:
      tc0140syt_0.slave_port_w(valor);
    $A001:
      tc0140syt_0.slave_comm_w(valor);
  else
    mem_snd[direccion] := valor;
  end;
end;

procedure sound_bank_rom(valor: byte);
begin
  sound_bank := valor and 3;
end;

procedure sound_instruccion;
begin
  ym2151_0.update;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  tc0140syt_0.z80.change_irq(irqstate);
end;

function rainbow_800007: byte;
begin
  rainbow_800007 := marcade.in0;
end;

function rainbow_800009: byte;
begin
  rainbow_800009 := marcade.in1;
end;

function rainbow_80000c: byte;
begin
  rainbow_80000c := marcade.in2;
end;

function rainbow_80000d: byte;
begin
  rainbow_80000d := marcade.in3;
end;

// Main
procedure reset_rainbow;
begin
  m68000_0.reset;
  tc0140syt_0.reset;
  ym2151_0.reset;
{$IFDEF MCU}cchip_0.reset; {$ENDIF}
  reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := 0; // $fc;
  marcade.in2 := $FF;
  marcade.in3 := $FF;
  sound_bank := 0;
  scroll_x1 := 0;
  scroll_y1 := 0;
  scroll_x2 := 0;
  scroll_y2 := 0;
end;

function start_rainbowislands: boolean;
const
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (8, 12, 0, 4, 24, 28, 16, 20, 40, 44, 32, 36, 56, 60, 48, 52);
  ps_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
var
  memory_temp, ptemp: pbyte;
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $4000);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, memory_temp, @ps_x, @pc_y, false, false);
  end;

  procedure convert_sprites;
  begin
    init_gfx(1, 16, 16, $1400);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
    convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  start_rainbowislands := false;
  machine_calls.general_loop := rainbow_loop;
  machine_calls.reset := reset_rainbow;
  start_audio(false);
  screen_init(1, 512, 512);
  screen_mod_scroll(1, 512, 512, 511, 512, 256, 511);
  screen_init(2, 512, 512, true);
  screen_mod_scroll(2, 512, 512, 511, 512, 256, 511);
  screen_init(3, 512, 512, false, true);
  start_video(320, 224);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256 * CPU_SYNC);
  m68000_0.change_ram16_calls(rainbow_getword, rainbow_putword);
  // Sound CPU
  tc0140syt_0 := tc0140syt_chip.create(4000000, 256 * CPU_SYNC);
  tc0140syt_0.z80.change_ram_calls(rainbow_snd_getbyte, rainbow_snd_putbyte);
  tc0140syt_0.z80.init_sound(sound_instruccion);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(4000000);
  ym2151_0.change_port_func(sound_bank_rom);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  // cargar roms
  getmem(memory_temp, $100000);
  case main_vars.machine_type of
    179:
      begin
        // MCU
{$IFDEF MCU}
        cchip_0 := cchip_chip.create(12000000, 256 * CPU_SYNC);
        cchip_0.change_ad(rainbow_80000d);
        cchip_0.change_in(rainbow_800007, rainbow_800009, rainbow_80000c, nil, nil);
        if not(roms_load(cchip_0.get_eeprom_dir, rainbow_cchip_eeprom)) then
          exit;
{$ELSE IF}
        rbisland_init_cchip(m68000_0.numero_cpu, 0);
{$ENDIF}
        if not(roms_load16w(@rom, rainbow_rom)) then
          exit;
        // cargar sonido+ponerlas en su banco
        ptemp := memory_temp;
        if not(roms_load(memory_temp, rainbow_sound)) then
          exit;
        copymemory(@mem_snd[0], memory_temp, $4000);
        copymemory(@bank_sound[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@bank_sound[1, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@bank_sound[2, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@bank_sound[3, 0], ptemp, $4000);
        // convertir chars
        if not(roms_load(memory_temp, rainbow_char)) then
          exit;
        convert_chars;
        // convertir sprites
        if not(roms_load(memory_temp, rainbow_sprites1)) then
          exit;
        if not(roms_load16b(memory_temp, rainbow_sprites2)) then
          exit;
        convert_sprites;
      end;
    180:
      begin
        // MCU
{$IFDEF MCU}
        cchip_0 := cchip_chip.create(12000000, 256 * CPU_SYNC);
        cchip_0.change_ad(rainbow_80000d);
        cchip_0.change_in(rainbow_800007, rainbow_800009, rainbow_80000c, nil, nil);
        if not(roms_load(cchip_0.get_eeprom_dir, rainbow_cchip_eeprom)) then
          exit;
{$ELSE IF}
        rbisland_init_cchip(m68000_0.numero_cpu, 1);
{$ENDIF}
        if not(roms_load16w(@rom, rainbowe_rom)) then
          exit;
        // cargar sonido+ponerlas en su banco
        ptemp := memory_temp;
        if not(roms_load(memory_temp, rainbow_sound)) then
          exit;
        copymemory(@mem_snd[0], memory_temp, $4000);
        copymemory(@bank_sound[0, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@bank_sound[1, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@bank_sound[2, 0], ptemp, $4000);
        inc(ptemp, $4000);
        copymemory(@bank_sound[3, 0], ptemp, $4000);
        // convertir chars
        if not(roms_load(memory_temp, rainbow_char)) then
          exit;
        convert_chars;
        // convertir sprites
        if not(roms_load(memory_temp, rainbow_sprites1)) then
          exit;
        if not(roms_load16b(memory_temp, rainbow_sprites2)) then
          exit;
        convert_sprites;
      end;
  end;
  // DIP
  marcade.dswa := $FE;
  marcade.dswa_val := @rainbow_dip1;
  marcade.dswb := $BF;
  marcade.dswb_val := @rainbow_dip2;
  // final
  freemem(memory_temp);
  reset_rainbow;
  start_rainbowislands := true;
end;

end.
