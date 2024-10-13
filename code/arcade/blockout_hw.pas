unit blockout_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2151,
  rom_engine,
  pal_engine,
  sound_engine,
  oki6295;

function start_blockout: boolean;

implementation

const
  blockout_rom: array [0 .. 1] of tipo_roms = ((n: 'bo29a0-2.bin'; l: $20000; p: 0; crc: $B0103427),
    (n: 'bo29a1-2.bin'; l: $20000; p: $1; crc: $5984D5A2));
  blockout_sound: tipo_roms = (n: 'bo29e3-0.bin'; l: $8000; p: 0; crc: $3EA01F78);
  blockout_oki: tipo_roms = (n: 'bo29e2-0.bin'; l: $20000; p: 0; crc: $15C5A99D);
  // DIP
  blockout_dipa: array [0 .. 3] of def_dip = ((mask: $3; name: 'Coinage'; number: 4;
    dip: ((dip_val: $0; dip_name: '3C 1C'), (dip_val: $1; dip_name: '2C 1C'), (dip_val: $3;
    dip_name: '1C 1C'), (dip_val: $2; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $10; name: '1 Coint to Continue'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $20; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), ());
  blockout_dipb: array [0 .. 2] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $2; dip_name: 'Easy'), (dip_val: $3; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Very Hard'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'Rotate Buttons'; number: 2;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $4; dip_name: '3'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), ());

var
  rom: array [0 .. $1FFFF] of word;
  ram, ram2: array [0 .. $5FFF] of word;
  ram3: array [0 .. $BFFF] of word;
  video_ram: array [0 .. $1FFFF] of word;
  video_ram_buff: array [0 .. $1FFFF] of boolean;
  fvideo_ram: array [0 .. $7FFF] of byte;
  sound_latch: byte;

procedure update_video_blockout;
var
  x, y, atrib: word;
  punt: array [0 .. 319] of word;
  front, back: word;
begin
  fill_full_screen(2, MAX_COLORS);
  for y := 0 to $FF do
  begin
    fillword(@punt[0], 320, paleta[MAX_COLORS]);
    for x := 0 to $27 do
    begin
      atrib := fvideo_ram[(y * 64 + x) * 2];
      if (atrib <> 0) then
      begin
        if (atrib and $80) <> 0 then
          punt[0 + (x * 8)] := paleta[512];
        if (atrib and $40) <> 0 then
          punt[1 + (x * 8)] := paleta[512];
        if (atrib and $20) <> 0 then
          punt[2 + (x * 8)] := paleta[512];
        if (atrib and $10) <> 0 then
          punt[3 + (x * 8)] := paleta[512];
        if (atrib and $08) <> 0 then
          punt[4 + (x * 8)] := paleta[512];
        if (atrib and $04) <> 0 then
          punt[5 + (x * 8)] := paleta[512];
        if (atrib and $02) <> 0 then
          punt[6 + (x * 8)] := paleta[512];
        if (atrib and $01) <> 0 then
          punt[7 + (x * 8)] := paleta[512];
      end;
    end;
    putpixel(0, y, 320, @punt[0], 2);
  end;
  for y := 0 to $FF do
  begin
    for x := 0 to 159 do
    begin
      if video_ram_buff[(y * 256) + x] then
      begin
        front := video_ram[(y * 256) + x];
        back := video_ram[$10000 + (y * 256) + x];
        if (front shr 8) <> 0 then
          punt[0] := paleta[front shr 8]
        else
          punt[0] := paleta[(back shr 8) + 256];
        if (front and $FF) <> 0 then
          punt[1] := paleta[front and $FF]
        else
          punt[1] := paleta[(back and $FF) + 256];
        putpixel(x * 2, y, 2, @punt[0], 1);
        video_ram_buff[(y * 256) + x] := false;
      end;
    end;
  end;
  actualiza_trozo(0, 0, 320, 256, 1, 0, 0, 320, 256, 3);
  actualiza_trozo(0, 0, 320, 256, 2, 0, 0, 320, 256, 3);
  update_final_piece(0, 8, 320, 240, 3);
end;

procedure events_blockout;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but3[0] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $FF7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    if p_contrls.map_arcade.but0[0] then
      marcade.dswa := (marcade.dswa and $FFBF)
    else
      marcade.dswa := (marcade.dswa or $40);
    // p2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FFFE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FFFD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FFFB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FFF7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $FFEF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $FFDF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but3[1] then
      marcade.in2 := (marcade.in2 and $FFBF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $FF7F)
    else
      marcade.in2 := (marcade.in2 or $80);
    if p_contrls.map_arcade.but0[1] then
      marcade.dswa := (marcade.dswa and $FF7F)
    else
      marcade.dswa := (marcade.dswa or $80);
    // system
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
  end;
end;

procedure blockout_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        case f of
          247:
            begin
              m68000_0.irq[6] := ASSERT_LINE;
              update_video_blockout;
            end;
          255:
            m68000_0.irq[5] := ASSERT_LINE;
        end;
      end;
      events_blockout;
      video_sync;
    end;
  end;
end;

function blockout_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFF:
      blockout_getword := rom[direccion shr 1];
    $100000:
      blockout_getword := marcade.in1; // p1
    $100002:
      blockout_getword := marcade.in2; // p2
    $100004:
      blockout_getword := marcade.in0; // sys
    $100006:
      blockout_getword := marcade.dswa; // dsw1
    $100008:
      blockout_getword := marcade.dswb; // dsw2
    $180000 .. $1BFFFF:
      blockout_getword := video_ram[(direccion and $3FFFF) shr 1];
    $1D4000 .. $1DFFFF:
      blockout_getword := ram[(direccion - $1D4000) shr 1];
    $1F4000 .. $1FFFFF:
      blockout_getword := ram2[(direccion - $1F4000) shr 1];
    $200000 .. $207FFF:
      blockout_getword := fvideo_ram[direccion and $7FFF];
    $208000 .. $21FFFF:
      blockout_getword := ram3[(direccion - $208000) shr 1];
    $280002:
      blockout_getword := buffer_paleta[512];
    $280200 .. $2805FF:
      blockout_getword := buffer_paleta[(direccion - $280200) shr 1];
  end;
end;

procedure blockout_putword(direccion: dword; valor: word);

  procedure change_color(pos, data: word);
  var
    bit0, bit1, bit2, bit3: byte;
    color: tcolor;
  begin
    // red component */
    bit0 := (data shr 0) and $01;
    bit1 := (data shr 1) and $01;
    bit2 := (data shr 2) and $01;
    bit3 := (data shr 3) and $01;
    color.r := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    // green component */
    bit0 := (data shr 4) and $01;
    bit1 := (data shr 5) and $01;
    bit2 := (data shr 6) and $01;
    bit3 := (data shr 7) and $01;
    color.g := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    // blue component */
    bit0 := (data shr 8) and $01;
    bit1 := (data shr 9) and $01;
    bit2 := (data shr 10) and $01;
    bit3 := (data shr 11) and $01;
    color.b := $0E * bit0 + $1F * bit1 + $43 * bit2 + $8F * bit3;
    set_pal_color(color, pos);
    fillchar(video_ram_buff, $20000, 1);
  end;

begin
  case direccion of
    0 .. $3FFFF:
      ; // ROM
    $100010:
      m68000_0.irq[6] := CLEAR_LINE;
    $100012:
      m68000_0.irq[5] := CLEAR_LINE;
    $100014:
      begin
        sound_latch := valor and $FF;
        z80_0.change_nmi(PULSE_LINE);
      end;
    $180000 .. $1BFFFF: 
	  if video_ram[(direccion and $3ffff) shr 1]<>valor then 	
      begin
        video_ram[(direccion and $3FFFF) shr 1] := valor;
        video_ram_buff[(direccion and $3FFFF) shr 1] := true;
      end;
    $1D4000 .. $1DFFFF:
      ram[(direccion - $1D4000) shr 1] := valor;
    $1F4000 .. $1FFFFF:
      ram2[(direccion - $1F4000) shr 1] := valor;
    $200000 .. $207FFF:
      fvideo_ram[direccion and $7FFF] := valor;
    $208000 .. $21FFFF:
      ram3[(direccion - $208000) shr 1] := valor;
    $280002:
      if buffer_paleta[512] <> valor then
      begin
        buffer_paleta[512] := valor;
        change_color(512, valor);
      end;
    $280200 .. $2805FF:
      if buffer_paleta[(direccion - $280200) shr 1] <> valor then
      begin
        buffer_paleta[(direccion - $280200) shr 1] := valor;
        change_color((direccion - $280200) shr 1, valor);
      end;
  end;
end;

function blockout_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      blockout_snd_getbyte := mem_snd[direccion];
    $8801:
      blockout_snd_getbyte := ym2151_0.status;
    $9800:
      blockout_snd_getbyte := oki_6295_0.read;
    $A000:
      blockout_snd_getbyte := sound_latch;
  end;
end;

procedure blockout_snd_putbyte(direccion: word; valor: byte);
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

procedure blockout_sound_update;
begin
  ym2151_0.update;
  oki_6295_0.update;
end;

// Main
procedure reset_blockout;
begin
  m68000_0.reset;
  z80_0.reset;
  ym2151_0.reset;
  oki_6295_0.reset;
  reset_audio;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  marcade.in1 := $FFFF;
  sound_latch := 0;
  fillchar(video_ram_buff, $20000, 1);
end;

function start_blockout: boolean;
begin
  start_blockout := false;
  machine_calls.general_loop := blockout_loop;
  machine_calls.reset := reset_blockout;
  start_audio(false);
  // Pantallas
  screen_init(1, 320, 256);
  screen_init(2, 320, 256, true);
  screen_init(3, 320, 256, false, true);
  start_video(320, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(10000000, 256);
  m68000_0.change_ram16_calls(blockout_getword, blockout_putword);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(blockout_snd_getbyte, blockout_snd_putbyte);
  z80_0.init_sound(blockout_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  ym2151_0.change_irq_func(ym2151_snd_irq);
  oki_6295_0 := snd_okim6295.create(1056000, OKIM6295_PIN7_HIGH);
  if not(roms_load(oki_6295_0.get_rom_addr, blockout_oki)) then
    exit;
  // cargar roms
  if not(roms_load16w(@rom, blockout_rom)) then
    exit;
  // cargar sonido
  if not(roms_load(@mem_snd, blockout_sound)) then
    exit;
  // DIP
  marcade.dswa := $FFFF;
  marcade.dswa_val := @blockout_dipa;
  marcade.dswb := $FFFF;
  marcade.dswb_val := @blockout_dipb;
  // final
  reset_blockout;
  start_blockout := true;
end;

end.
