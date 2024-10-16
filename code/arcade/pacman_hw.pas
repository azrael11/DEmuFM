unit pacman_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  namco_snd,
  controls_engine,
  gfx_engine,
  rom_engine,
  misc_functions,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_pacman: boolean;

implementation

const
  // Pacman
  pacman_rom: array [0 .. 3] of tipo_roms = ((n: 'pacman.6e'; l: $1000; p: 0; crc: $C1E6AB10), (n: 'pacman.6f'; l: $1000; p: $1000; crc: $1A6FB2D4), (n: 'pacman.6h'; l: $1000; p: $2000;
    crc: $BCDD1BEB), (n: 'pacman.6j'; l: $1000; p: $3000; crc: $817D94E3));
  pacman_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: '82s126.4a'; l: $100; p: $20; crc: $3EB3A8E4));
  pacman_char: tipo_roms = (n: 'pacman.5e'; l: $1000; p: 0; crc: $0C944964);
  pacman_sound: tipo_roms = (n: '82s126.1m'; l: $100; p: 0; crc: $A9CC86BF);
  pacman_sprites: tipo_roms = (n: 'pacman.5f'; l: $1000; p: 0; crc: $958FEDF9);
  // MS-Pacman
  mspacman_rom: array [0 .. 6] of tipo_roms = ((n: 'pacman.6e'; l: $1000; p: 0; crc: $C1E6AB10), (n: 'pacman.6f'; l: $1000; p: $1000; crc: $1A6FB2D4), (n: 'pacman.6h'; l: $1000; p: $2000;
    crc: $BCDD1BEB), (n: 'pacman.6j'; l: $1000; p: $3000; crc: $817D94E3), (n: 'u5'; l: $800; p: $8000; crc: $F45FBBCD), (n: 'u6'; l: $1000; p: $9000; crc: $A90E7000), (n: 'u7'; l: $1000; p: $B000;
    crc: $C82CD714));
  mspacman_char: tipo_roms = (n: '5e'; l: $1000; p: 0; crc: $5C281D01);
  mspacman_sprites: tipo_roms = (n: '5f'; l: $1000; p: 0; crc: $615AF909);
  // Crush Roller
  crush_rom: array [0 .. 3] of tipo_roms = ((n: 'crushkrl.6e'; l: $1000; p: 0; crc: $A8DD8F54), (n: 'crushkrl.6f'; l: $1000; p: $1000; crc: $91387299), (n: 'crushkrl.6h'; l: $1000; p: $2000;
    crc: $D4455F27), (n: 'crushkrl.6j'; l: $1000; p: $3000; crc: $D59FC251));
  crush_char: tipo_roms = (n: 'maketrax.5e'; l: $1000; p: 0; crc: $91BAD2DA);
  crush_sprites: tipo_roms = (n: 'maketrax.5f'; l: $1000; p: 0; crc: $AEA79F55);
  crush_pal: array [0 .. 1] of tipo_roms = ((n: '82s123.7f'; l: $20; p: 0; crc: $2FC650BD), (n: '2s140.4a'; l: $100; p: $20; crc: $63EFB927));
  // Ms Pac Man Twin
  mspactwin_rom: tipo_roms = (n: 'm27256.bin'; l: $8000; p: 0; crc: $77A99184);
  mspactwin_char: array [0 .. 1] of tipo_roms = ((n: '4__2716.5d'; l: $800; p: 0; crc: $483C1D1C), (n: '2__2716.5g'; l: $800; p: $800; crc: $C08D73A2));
  mspactwin_sprites: array [0 .. 1] of tipo_roms = ((n: '3__2516.5f'; l: $800; p: $0; crc: $22B0188A), (n: '1__2516.5j'; l: $800; p: $800; crc: $0A8C46A0));
  mspactwin_pal: array [0 .. 1] of tipo_roms = ((n: 'mb7051.8h'; l: $20; p: 0; crc: $FF344446), (n: '82s129.4a'; l: $100; p: $20; crc: $A8202D0D));
  // Birdiy
  birdiy_rom: array [0 .. 3] of tipo_roms = ((n: 'a6.6a'; l: $1000; p: 0; crc: $3A58F8AD), (n: 'c6.6c'; l: $1000; p: $1000; crc: $FEC61EA2), (n: 'a4.4a'; l: $1000; p: $2000; crc: $3392783B),
    (n: 'c4.4c'; l: $1000; p: $3000; crc: $2391D83D));
  birdiy_pal: array [0 .. 1] of tipo_roms = ((n: 'n82s123n.10n'; l: $20; p: 0; crc: $FF344446), (n: 'n82s129n.9m'; l: $100; p: $20; crc: $63EFB927));
  birdiy_char: tipo_roms = (n: 'c1.1c'; l: $1000; p: 0; crc: $8F6BF54F);
  birdiy_sound: tipo_roms = (n: 'n82s129n.4k'; l: $100; p: 0; crc: $A9CC86BF);
  birdiy_sprites: tipo_roms = (n: 'c3.3c'; l: $1000; p: 0; crc: $10B55440);
  // DIP
  pacman_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Coinage'; number: 4; dip: ((dip_val: $3; dip_name: '2C 1C'), (dip_val: $1; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '1'), (dip_val: $4; dip_name: '2'), (dip_val: $8; dip_name: '3'), (dip_val: $C; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30;
    name: 'Bonus Life'; number: 3; dip: ((dip_val: $0; dip_name: '10000'), (dip_val: $10; dip_name: '15000'), (dip_val: $20; dip_name: '20000'), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Difficulty'; number: 2; dip: ((dip_val: $40; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Ghost Names'; number: 2; dip: ((dip_val: $80; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Alternate'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  pacman_dip_b: array [0 .. 1] of def_dip = ((mask: $10; name: 'Rack Test'; number: 2; dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), ());
  pacman_dip_c: array [0 .. 1] of def_dip = ((mask: $80; name: 'Cabinet'; number: 2; dip: ((dip_val: $80; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  mspacman_dip: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coinage'; number: 4; dip: ((dip_val: $3; dip_name: '2C 1C'), (dip_val: $1; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '1'), (dip_val: $4; dip_name: '2'), (dip_val: $8; dip_name: '3'), (dip_val: $C; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $0; dip_name: '10000'), (dip_val: $10; dip_name: '15000'), (dip_val: $20; dip_name: '20000'), (dip_val: $30;
    dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $40; dip_name: 'Normal'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  crush_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coinage'; number: 4; dip: ((dip_val: $3; dip_name: '2C 1C'), (dip_val: $1; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '3'), (dip_val: $4; dip_name: '4'), (dip_val: $8; dip_name: '5'), (dip_val: $C; dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10;
    name: 'First Pattern'; number: 2; dip: ((dip_val: $10; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Hard'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Teleport Holes'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  crush_dip_b: array [0 .. 1] of def_dip = ((mask: $10; name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $10; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  mspactwin_dip_a: array [0 .. 3] of def_dip = ((mask: $3; name: 'Coinage'; number: 4; dip: ((dip_val: $3; dip_name: '2C 1C'), (dip_val: $1; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '1'), (dip_val: $4; dip_name: '2'), (dip_val: $8; dip_name: '3'), (dip_val: $C; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30;
    name: 'Bonus Life'; number: 4; dip: ((dip_val: $0; dip_name: '10000'), (dip_val: $10; dip_name: '15000'), (dip_val: $20; dip_name: '20000'), (dip_val: $30;
    dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  mspactwin_dip_b: array [0 .. 1] of def_dip = ((mask: $10; name: 'Jama'; number: 2; dip: ((dip_val: $10; dip_name: 'Slow'), (dip_val: $0; dip_name: 'Fast'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), ());
  mspactwin_dip_c: array [0 .. 1] of def_dip = ((mask: $80; name: 'Skip Screen'; number: 2; dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (),
    (), (), (), (), ())), ());
  birdiy_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Coinage'; number: 4; dip: ((dip_val: $3; dip_name: '2C 1C'), (dip_val: $1; dip_name: '1C 1C'), (dip_val: $2;
    dip_name: '1C 2C'), (dip_val: $0; dip_name: 'Free Play'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '1'), (dip_val: $4; dip_name: '2'), (dip_val: $8; dip_name: '3'), (dip_val: $C; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $10;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $10; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Skip Screen'; number: 2; dip: ((dip_val: $20; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  irq_vblank, dec_enable, croller_disable_protection: boolean;
  rom_decode: array [0 .. $BFFF] of byte;
  read_events: procedure;
  croller_counter, croller_offset, unk_latch: byte;
  sprite_ram: array [0 .. $F] of byte;

procedure update_video_pacman;
var
  color, offs: word;
  nchar, f, sx, sy, atrib, x, y: byte;
  flip_x, flip_y: boolean;
begin
  for x := 0 to 27 do
  begin
    for y := 0 to 35 do
    begin
      sx := 29 - x;
      sy := y - 2;
      if (sy and $20) <> 0 then
        offs := sx + ((sy and $1F) shl 5)
      else
        offs := sy + (sx shl 5);
      if gfx[0].buffer[offs] then
      begin
        color := ((memory[$4400 + offs]) and $1F) shl 2;
        put_gfx(x * 8, y * 8, memory[$4000 + offs], color, 1, 0);
        gfx[0].buffer[offs] := false;
      end;
    end;
  end;
  update_region(0, 0, 224, 288, 1, 0, 0, 224, 288, 2);
  // sprites pacman posicion $5060
  // byte 0 --> x
  // byte 1 --> y
  // sprites pacman atributos $4FF0
  // byte 0
  // bit 0 --> flipy
  // bit 1 --> flipx
  // bits 2..7 --> numero char
  for f := 7 downto 0 do
  begin
    atrib := memory[$4FF0 + (f * 2)];
    nchar := atrib shr 2;
    color := (memory[$4FF1 + (f * 2)] and $1F) shl 2;
    if main_screen.flip_main_screen then
    begin
      x := sprite_ram[$0 + (f * 2)] - 32;
      y := sprite_ram[$1 + (f * 2)];
      flip_y := (atrib and 1) = 0;
      flip_x := (atrib and 2) = 0;
    end
    else
    begin
      x := 240 - sprite_ram[$0 + (f * 2)] - 1;
      y := 272 - sprite_ram[$1 + (f * 2)];
      flip_y := (atrib and 1) <> 0;
      flip_x := (atrib and 2) <> 0;
    end;
    put_gfx_sprite_mask(nchar, color, flip_x, flip_y, 1, 0, $F);
    if (f < 2) then
      update_gfx_sprite((x - 1) and $FF, y, 2, 1)
    else
      update_gfx_sprite(x and $FF, y, 2, 1)
  end;
  update_final_piece(0, 0, 224, 288, 2);
end;

procedure events_pacman;
begin
  if event.arcade then
  begin
    // in 0
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // in 1
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but0[0] then
    begin
      if (memory[$180B] <> $01) then
      begin
        memory[$180B] := $01;
        memory[$1FFD] := $BD;
      end
    end
    else
    begin
      if (memory[$180B] <> $BE) then
      begin
        memory[$180B] := $BE;
        memory[$1FFD] := $00;
      end
    end;
  end;
end;

procedure pacman_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        if f = 96 then
          update_video_pacman;
        if ((f = 224) and irq_vblank) then
          z80_0.change_irq(ASSERT_LINE);
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
      end;
      read_events;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function pacman_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FFF:
      pacman_getbyte := memory[direccion];
    $4000 .. $47FF, $6000 .. $67FF:
      pacman_getbyte := memory[(direccion and $7FF) + $4000];
    $4800 .. $4BFF, $6800 .. $6BFF:
      pacman_getbyte := $BF;
    $4C00 .. $4FFF, $6C00 .. $6FFF:
      pacman_getbyte := memory[(direccion and $3FF) + $4C00];
    $5000 .. $5FFF, $7000 .. $7FFF:
      case (direccion and $FF) of
        $00 .. $3F:
          pacman_getbyte := marcade.in0 or marcade.dswb;
        $40 .. $7F:
          pacman_getbyte := marcade.in1 or marcade.dswc;
        $80 .. $BF:
          pacman_getbyte := marcade.dswa;
        $C0 .. $FF:
          pacman_getbyte := $0;
      end;
  end;
end;

procedure pacman_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $47FF, $6000 .. $67FF:
      if memory[(direccion and $7FF) + $4000] <> valor then
      begin
        memory[(direccion and $7FF) + $4000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $4C00 .. $4FFF, $6C00 .. $6FFF:
      memory[(direccion and $3FF) + $4C00] := valor;
    $5000 .. $5FFF, $7000 .. $7FFF:
      case (direccion and $FF) of
        0:
          begin
            irq_vblank := valor <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        1:
          namco_snd_0.enabled := valor <> 0;
        3:
          main_screen.flip_main_screen := (valor and 1) <> 0;
        $40 .. $5F:
          namco_snd_0.regs[direccion and $1F] := valor;
        $60 .. $6F:
          sprite_ram[direccion and $F] := valor;
      end;
  end;
end;

procedure pacman_outbyte(puerto: word; valor: byte);
begin
  if (puerto and $FF) = 0 then
    z80_0.im2_lo := valor;
end;

procedure pacman_sound_update;
begin
  namco_snd_0.update;
end;

// MS Pacman
procedure events_mspacman;
begin
  if event.arcade then
  begin
    // in 0
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // in 1
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
  end;
end;

function mspacman_getbyte(direccion: word): byte;
begin
  case direccion of
    $38 .. $3F, $3B0 .. $3B7, $1600 .. $1607, $2120 .. $2127, $3FF0 .. $3FF7, $8000 .. $8007, $97F0 .. $97F7:
      dec_enable := false;
    $3FF8 .. $3FFF:
      dec_enable := true;
  end;
  case direccion of
    $0 .. $3FFF, $8000 .. $BFFF:
      if dec_enable then
        mspacman_getbyte := rom_decode[direccion]
      else
        mspacman_getbyte := memory[direccion and $3FFF];
  else
    mspacman_getbyte := pacman_getbyte(direccion);
  end;
end;

procedure mspacman_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $38 .. $3F, $3B0 .. $3B7, $1600 .. $1607, $2120 .. $2127, $3FF0 .. $3FF7, $8000 .. $8007, $97F0 .. $97F7:
      dec_enable := false;
    $3FF8 .. $3FFF:
      dec_enable := true;
  else
    pacman_putbyte(direccion, valor);
  end;
end;

// Crush Roller
function crush_getbyte(direccion: word): byte;
const
  protdata_odd: array [0 .. $1D] of byte = ( // table at $ebd (odd entries)
    $00, $C0, $00, $40, $C0, $40, $00, $C0, $00, $40, $00, $C0, $00, $40, $C0, $40, $00, $C0, $00, $40, $00, $C0, $00, $40, $C0, $40, $00, $C0, $00, $40);
  protdata_even: array [0 .. $1D] of byte = ( // table at $ebd (even entries)
    $1F, $3F, $2F, $2F, $0F, $0F, $0F, $3F, $0F, $0F, $1C, $3C, $2C, $2C, $0C, $0C, $0C, $3C, $0C, $0C, $11, $31, $21, $21, $01, $01, $01, $31, $01, $01);
var
  tempb: byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    $5000 .. $5FFF, $7000 .. $7FFF:
      case (direccion and $FF) of
        $00 .. $3F:
          crush_getbyte := marcade.in0 + marcade.dswb;
        $40 .. $7F:
          crush_getbyte := marcade.in1;
        $80 .. $BF:
          begin // proteccion 1
            tempb := marcade.dswa and $3F;
            if not(croller_disable_protection) then
            begin
              crush_getbyte := protdata_odd[croller_offset] or tempb;
              exit;
            end;
            case (direccion and $3F) of
              $01, $04:
                crush_getbyte := tempb or $40;
              $05, $0E, $10:
                crush_getbyte := tempb or $C0;
            else
              crush_getbyte := tempb;
            end;
          end;
        $C0 .. $CF:
          begin // proteccion 2
            if not(croller_disable_protection) then
            begin
              crush_getbyte := protdata_even[croller_offset];
              exit;
            end;
            case (direccion and $F) of
              $0:
                crush_getbyte := $1F;
              $9:
                crush_getbyte := $30;
              $C:
                crush_getbyte := 0;
            else
              crush_getbyte := $20;
            end;
          end;
        $D0 .. $FF:
          crush_getbyte := $0;
      end;
  else
    crush_getbyte := pacman_getbyte(direccion);
  end;
end;

procedure crush_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    $5000 .. $5FFF, $7000 .. $7FFF:
      case (direccion and $FF) of
        0:
          begin
            irq_vblank := valor <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        1:
          namco_snd_0.enabled := valor <> 0;
        3:
          main_screen.flip_main_screen := (valor and 1) <> 0;
        4:
          case valor of // proteccion
            0:
              begin // disable protection / reset?
                croller_counter := 0;
                croller_offset := 0;
                croller_disable_protection := true;
              end;
            1:
              begin
                croller_disable_protection := false;
                croller_counter := croller_counter + 1;
                if (croller_counter = $3C) then
                begin
                  croller_counter := 0;
                  croller_offset := croller_offset + 1;
                  if (croller_offset = $1E) then
                    croller_offset := 0;
                end;
              end;
          end;
        $40 .. $5F:
          namco_snd_0.regs[direccion and $1F] := valor;
        $60 .. $6F:
          sprite_ram[direccion and $F] := valor;
      end;
  else
    pacman_putbyte(direccion, valor);
  end;
end;

// Ms Pac Man Twin
function mspactwin_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $3FFF, $8000 .. $BFFF:
      if z80_0.opcode then
        mspactwin_getbyte := rom_decode[direccion]
      else
        mspactwin_getbyte := memory[direccion];
    $6000 .. $67FF:
      if z80_0.opcode then
        mspactwin_getbyte := rom_decode[(direccion and $1FFF) + $2000]
      else
        mspactwin_getbyte := memory[(direccion and $1FFF) + $2000];
    $4000 .. $47FF, $C000 .. $C7FF:
      mspactwin_getbyte := memory[(direccion and $7FF) + $4000];
    $4800 .. $4BFF, $6800 .. $6BFF, $C800 .. $CBFF:
      mspactwin_getbyte := 0;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      mspactwin_getbyte := memory[(direccion and $3FF) + $4C00];
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      case (direccion and $FF) of
        $00 .. $3F:
          mspactwin_getbyte := marcade.in0 or marcade.dswb;
        $40 .. $7F:
          mspactwin_getbyte := marcade.in1 or marcade.dswc;
        $80 .. $BF:
          mspactwin_getbyte := marcade.dswa;
        $C0 .. $FF:
          mspactwin_getbyte := unk_latch;
      end;
  end;
end;

procedure mspactwin_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF, $6000 .. $67FF, $8000 .. $BFFF:
      ;
    $4000 .. $47FF, $C000 .. $C7FF:
      if memory[(direccion and $7FF) + $4000] <> valor then
      begin
        memory[(direccion and $7FF) + $4000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $4C00 .. $4FFF, $6C00 .. $6FFF, $CC00 .. $CFFF, $EC00 .. $EFFF:
      memory[(direccion and $3FF) + $4C00] := valor;
    $5000 .. $5FFF, $7000 .. $7FFF, $D000 .. $DFFF, $F000 .. $FFFF:
      case (direccion and $FF) of
        0:
          begin
            irq_vblank := valor <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        1:
          namco_snd_0.enabled := valor <> 0;
        3:
          main_screen.flip_main_screen := (valor and 1) <> 0;
        $40 .. $5F:
          namco_snd_0.regs[direccion and $1F] := valor;
        $60 .. $6F:
          sprite_ram[direccion and $F] := valor;
        $80 .. $BF:
          unk_latch := valor;
        $C0 .. $FF:
          ; // WD
      end;
  end;
end;

// Birdiy
function birdiy_getbyte(direccion: word): byte;
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FFF:
      birdiy_getbyte := memory[direccion];
    $4000 .. $47FF:
      birdiy_getbyte := memory[(direccion and $7FF) + $4000];
    $4C00 .. $4FFF:
      birdiy_getbyte := memory[(direccion and $3FF) + $4C00];
    $5000 .. $5FFF:
      case (direccion and $FF) of
        $00 .. $3F:
          birdiy_getbyte := marcade.in0 or $10;
        $40 .. $7F:
          birdiy_getbyte := marcade.in1;
        $80 .. $BF:
          birdiy_getbyte := marcade.dswa;
        $C0 .. $FF:
          birdiy_getbyte := $FF;
      end;
  end;
end;

procedure birdiy_putbyte(direccion: word; valor: byte);
begin
  direccion := direccion and $7FFF;
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $47FF:
      if memory[(direccion and $7FF) + $4000] <> valor then
      begin
        memory[(direccion and $7FF) + $4000] := valor;
        gfx[0].buffer[direccion and $3FF] := true;
      end;
    $4C00 .. $4FFF:
      memory[(direccion and $3FF) + $4C00] := valor;
    $5000 .. $5FFF:
      case (direccion and $FF) of
        1:
          begin
            irq_vblank := valor <> 0;
            if not(irq_vblank) then
              z80_0.change_irq(CLEAR_LINE);
          end;
        $80 .. $9F:
          namco_snd_0.regs[direccion and $1F] := valor;
        $A0 .. $AF:
          sprite_ram[direccion and $F] := valor;
      end;
  end;
end;

procedure pacman_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    10:
      open_qsnapshot_save('pacman' + nombre);
    88:
      open_qsnapshot_save('mspacman' + nombre);
    234:
      open_qsnapshot_save('crushroller' + nombre);
    305:
      open_qsnapshot_save('mspactwin' + nombre);
    353:
      open_qsnapshot_save('birdiy' + nombre);
  end;
  getmem(data, 2000);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := namco_snd_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@sprite_ram[0], $10);
  savedata_qsnapshot(@memory[$4000], $4000);
  if main_vars.machine_type = 88 then
    savedata_qsnapshot(@memory[$C000], $4000);
  // MISC
  buffer[0] := byte(irq_vblank);
  buffer[1] := byte(dec_enable);
  buffer[2] := croller_counter;
  buffer[3] := croller_offset;
  buffer[4] := byte(croller_disable_protection);
  buffer[5] := unk_latch;
  savedata_qsnapshot(@buffer, 6);
  freemem(data);
  close_qsnapshot;
end;

procedure pacman_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
begin
  case main_vars.machine_type of
    10:
      if not(open_qsnapshot_load('pacman' + nombre)) then
        exit;
    88:
      if not(open_qsnapshot_load('mspacman' + nombre)) then
        exit;
    234:
      if not(open_qsnapshot_load('crushroller' + nombre)) then
        exit;
    305:
      if not(open_qsnapshot_load('mspactwin' + nombre)) then
        exit;
    353:
      if not(open_qsnapshot_load('birdiy' + nombre)) then
        exit;
  end;
  getmem(data, 2000);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  namco_snd_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@sprite_ram[0]);
  loaddata_qsnapshot(@memory[$4000]);
  if main_vars.machine_type = 88 then
    loaddata_qsnapshot(@memory[$C000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  irq_vblank := buffer[0] <> 0;
  dec_enable := buffer[1] <> 0;
  croller_counter := buffer[2];
  croller_offset := buffer[3];
  croller_disable_protection := buffer[4] <> 0;
  unk_latch := buffer[5];
  freemem(data);
  close_qsnapshot;
  fillchar(gfx[0].buffer, $400, 1);
end;

// Main
procedure reset_pacman;
begin
  z80_0.reset;
  namco_snd_0.reset;
  reset_audio;
  irq_vblank := false;
  dec_enable := false;
  marcade.in0 := $EF;
  marcade.in1 := $7F;
  croller_counter := 0;
  croller_offset := 0;
  croller_disable_protection := false;
  unk_latch := 0;
end;

procedure mspacman_install_patches;
var
  i: byte;
begin
  // copy forty 8-byte patches into Pac-Man code
  for i := 0 to 7 do
  begin
    rom_decode[$0410 + i] := rom_decode[$8008 + i];
    rom_decode[$08E0 + i] := rom_decode[$81D8 + i];
    rom_decode[$0A30 + i] := rom_decode[$8118 + i];
    rom_decode[$0BD0 + i] := rom_decode[$80D8 + i];
    rom_decode[$0C20 + i] := rom_decode[$8120 + i];
    rom_decode[$0E58 + i] := rom_decode[$8168 + i];
    rom_decode[$0EA8 + i] := rom_decode[$8198 + i];
    rom_decode[$1000 + i] := rom_decode[$8020 + i];
    rom_decode[$1008 + i] := rom_decode[$8010 + i];
    rom_decode[$1288 + i] := rom_decode[$8098 + i];
    rom_decode[$1348 + i] := rom_decode[$8048 + i];
    rom_decode[$1688 + i] := rom_decode[$8088 + i];
    rom_decode[$16B0 + i] := rom_decode[$8188 + i];
    rom_decode[$16D8 + i] := rom_decode[$80C8 + i];
    rom_decode[$16F8 + i] := rom_decode[$81C8 + i];
    rom_decode[$19A8 + i] := rom_decode[$80A8 + i];
    rom_decode[$19B8 + i] := rom_decode[$81A8 + i];
    rom_decode[$2060 + i] := rom_decode[$8148 + i];
    rom_decode[$2108 + i] := rom_decode[$8018 + i];
    rom_decode[$21A0 + i] := rom_decode[$81A0 + i];
    rom_decode[$2298 + i] := rom_decode[$80A0 + i];
    rom_decode[$23E0 + i] := rom_decode[$80E8 + i];
    rom_decode[$2418 + i] := rom_decode[$8000 + i];
    rom_decode[$2448 + i] := rom_decode[$8058 + i];
    rom_decode[$2470 + i] := rom_decode[$8140 + i];
    rom_decode[$2488 + i] := rom_decode[$8080 + i];
    rom_decode[$24B0 + i] := rom_decode[$8180 + i];
    rom_decode[$24D8 + i] := rom_decode[$80C0 + i];
    rom_decode[$24F8 + i] := rom_decode[$81C0 + i];
    rom_decode[$2748 + i] := rom_decode[$8050 + i];
    rom_decode[$2780 + i] := rom_decode[$8090 + i];
    rom_decode[$27B8 + i] := rom_decode[$8190 + i];
    rom_decode[$2800 + i] := rom_decode[$8028 + i];
    rom_decode[$2B20 + i] := rom_decode[$8100 + i];
    rom_decode[$2B30 + i] := rom_decode[$8110 + i];
    rom_decode[$2BF0 + i] := rom_decode[$81D0 + i];
    rom_decode[$2CC0 + i] := rom_decode[$80D0 + i];
    rom_decode[$2CD8 + i] := rom_decode[$80E0 + i];
    rom_decode[$2CF0 + i] := rom_decode[$81E0 + i];
    rom_decode[$2D60 + i] := rom_decode[$8160 + i];
  end;
end;

function start_pacman: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $7FFF] of byte;
  rweights, gweights, bweights: array [0 .. 2] of single;
const
  ps_x: array [0 .. 15] of dword = (8 * 8, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3, 0, 1, 2, 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  pc_x: array [0 .. 7] of dword = (8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 0, 1, 2, 3);
  resistances: array [0 .. 2] of integer = (1000, 470, 220);
  procedure conv_chars;
  begin
    init_gfx(0, 8, 8, 256);
    gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
    convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, true, false);
  end;
  procedure conv_sprites;
  begin
    init_gfx(1, 16, 16, 64);
    gfx_set_desc_data(2, 0, 64 * 8, 0, 4);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;

begin
  machine_calls.general_loop := pacman_loop;
  machine_calls.reset := reset_pacman;
  machine_calls.fps_max := 60.6060606060;
  machine_calls.save_qsnap := pacman_qsave;
  machine_calls.load_qsnap := pacman_qload;
  start_pacman := false;
  start_audio(false);
  screen_init(1, 224, 288);
  screen_init(2, 256, 512, false, true);
  start_video(224, 288);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 264);
  z80_0.change_io_calls(nil, pacman_outbyte);
  z80_0.init_sound(pacman_sound_update);
  namco_snd_0 := namco_snd_chip.create(3);
  case main_vars.machine_type of
    10:
      begin // Pacman
        z80_0.change_ram_calls(pacman_getbyte, pacman_putbyte);
        // cargar roms
        if not(roms_load(@memory, pacman_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, pacman_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, pacman_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, pacman_pal)) then
          exit;
        // DIP
        read_events := events_pacman;
        marcade.dswa := $C9;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val := @pacman_dip_a;
        marcade.dswb_val := @pacman_dip_b;
        marcade.dswc_val := @pacman_dip_c;
      end;
    88:
      begin // MS Pacman
        z80_0.change_ram_calls(mspacman_getbyte, mspacman_putbyte);
        // cargar y desencriptar roms
        if not(roms_load(@memory, mspacman_rom)) then
          exit;
        copymemory(@rom_decode, @memory, $1000); // pacman.6e
        copymemory(@rom_decode[$1000], @memory[$1000], $1000); // pacman.6f
        copymemory(@rom_decode[$2000], @memory[$2000], $1000); // pacman.6h
        for f := 0 to $FFF do
          rom_decode[$3000 + f] := BITSWAP8(memory[$B000 + BITSWAP16(f, 15, 14, 13, 12, 11, 3, 7, 9, 10, 8, 6, 5, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt u7 */
        for f := 0 to $7FF do
        begin
          rom_decode[$8000 + f] := BITSWAP8(memory[$8000 + BITSWAP16(f, 15, 14, 13, 12, 11, 8, 7, 5, 9, 10, 6, 3, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt u5 */
          rom_decode[$8800 + f] := BITSWAP8(memory[$9800 + BITSWAP16(f, 15, 14, 13, 12, 11, 3, 7, 9, 10, 8, 6, 5, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt half of u6 */
          rom_decode[$9000 + f] := BITSWAP8(memory[$9000 + BITSWAP16(f, 15, 14, 13, 12, 11, 3, 7, 9, 10, 8, 6, 5, 4, 2, 1, 0)], 0, 4, 5, 7, 6, 3, 2, 1); // decrypt half of u6 */
        end;
        copymemory(@rom_decode[$9800], @memory[$1800], $800); // mirror of pacman.6f high
        copymemory(@rom_decode[$A000], @memory[$2000], $1000); // mirror of pacman.6h
        copymemory(@rom_decode[$B000], @memory[$3000], $1000); // mirror of pacman.6j
        mspacman_install_patches;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, mspacman_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, mspacman_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, pacman_pal)) then
          exit;
        // DIP
        read_events := events_mspacman;
        marcade.dswa := $C9;
        marcade.dswb := $10;
        marcade.dswc := $80;
        marcade.dswa_val := @mspacman_dip;
        marcade.dswb_val := @pacman_dip_b;
        marcade.dswc_val := @pacman_dip_c;
      end;
    234:
      begin // Crush Roller
        z80_0.change_ram_calls(crush_getbyte, crush_putbyte);
        // cargar roms
        if not(roms_load(@memory, crush_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, crush_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, crush_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, crush_pal)) then
          exit;
        // DIP
        read_events := events_mspacman;
        marcade.dswa := $31;
        marcade.dswb := $0;
        marcade.dswa_val := @crush_dip_a;
        marcade.dswb_val := @crush_dip_b;
      end;
    305:
      begin // MS Pacman Twin
        z80_0.change_ram_calls(mspactwin_getbyte, mspactwin_putbyte);
        // cargar y desencriptar roms
        if not(roms_load(@memory_temp, mspactwin_rom)) then
          exit;
        copymemory(@memory, @memory_temp, $4000);
        copymemory(@memory[$8000], @memory_temp[$4000], $4000);
        for f := 0 to $1FFF do
        begin
          // decode opcode
          rom_decode[f * 2] := BITSWAP8(memory[f * 2], 4, 5, 6, 7, 0, 1, 2, 3);
          rom_decode[(f * 2) + 1] := BITSWAP8(memory[(f * 2) + 1] xor $9A, 6, 4, 5, 7, 2, 0, 3, 1);
          rom_decode[$8000 + (f * 2)] := BITSWAP8(memory[$8000 + (f * 2)], 4, 5, 6, 7, 0, 1, 2, 3);
          rom_decode[$8001 + (f * 2)] := BITSWAP8(memory[$8001 + (f * 2)] xor $9A, 6, 4, 5, 7, 2, 0, 3, 1);
          // decode operand
          memory[f * 2] := BITSWAP8(memory[f * 2], 0, 1, 2, 3, 4, 5, 6, 7);
          memory[(f * 2) + 1] := BITSWAP8(memory[(f * 2) + 1] xor $A3, 2, 4, 6, 3, 7, 0, 5, 1);
          memory[$8000 + (f * 2)] := BITSWAP8(memory[$8000 + (f * 2)], 0, 1, 2, 3, 4, 5, 6, 7);
          memory[$8001 + (f * 2)] := BITSWAP8(memory[$8001 + (f * 2)] xor $A3, 2, 4, 6, 3, 7, 0, 5, 1);
        end;
        // cargar sonido
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, mspactwin_char)) then
          exit;
        conv_chars;
        // convertir sprites
        if not(roms_load(@memory_temp, mspactwin_sprites)) then
          exit;
        conv_sprites;
        // poner la paleta
        if not(roms_load(@memory_temp, mspactwin_pal)) then
          exit;
        // DIP
        read_events := events_mspacman;
        marcade.dswa := $C9;
        marcade.dswa_val := @mspactwin_dip_a;
        marcade.dswb := $10;
        marcade.dswb_val := @mspactwin_dip_b;
        marcade.dswc := $80;
        marcade.dswc_val := @mspactwin_dip_c;
      end;
    353:
      begin // Birdiy
        z80_0.change_ram_calls(birdiy_getbyte, birdiy_putbyte);
        if not(roms_load(@memory, birdiy_rom)) then
          exit;
        if not(roms_load(namco_snd_0.get_wave_dir, pacman_sound)) then
          exit;
        if not(roms_load(@memory_temp, birdiy_char)) then
          exit;
        conv_chars;
        if not(roms_load(@memory_temp, birdiy_sprites)) then
          exit;
        conv_sprites;
        if not(roms_load(@memory_temp, birdiy_pal)) then
          exit;
        read_events := events_mspacman;
        marcade.dswa := $E9;
        marcade.dswa_val := @birdiy_dip_a;
      end;
  end;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances, @rweights, 0, 0, 3, @resistances, @gweights, 0, 0, 2, @resistances[1], @bweights, 0, 0);
  for f := 0 to $1F do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and $1;
    bit1 := (memory_temp[f] shr 1) and $1;
    bit2 := (memory_temp[f] shr 2) and $1;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    // green component
    bit0 := (memory_temp[f] shr 3) and $1;
    bit1 := (memory_temp[f] shr 4) and $1;
    bit2 := (memory_temp[f] shr 5) and $1;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // blue component
    bit0 := (memory_temp[f] shr 6) and $1;
    bit1 := (memory_temp[f] shr 7) and $1;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to 255 do
  begin
    gfx[0].colores[f] := memory_temp[$20 + f] and $F;
    gfx[1].colores[f] := memory_temp[$20 + f] and $F;
  end;
  // final
  reset_pacman;
  start_pacman := true;
end;

end.
