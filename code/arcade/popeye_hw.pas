unit popeye_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  ay_8910,
  rom_engine,
  misc_functions,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_popeye: boolean;

implementation

const
  popeye_rom: array [0 .. 3] of tipo_roms = ((n: 'tpp2-c_f.7a'; l: $2000; p: 0; crc: $9AF7C821), (n: 'tpp2-c_f.7b'; l: $2000; p: $2000; crc: $C3704958), (n: 'tpp2-c_f.7c'; l: $2000; p: $4000;
    crc: $5882EBF9), (n: 'tpp2-c_f.7e'; l: $2000; p: $6000; crc: $EF8649CA));
  popeye_pal: array [0 .. 3] of tipo_roms = ((n: 'tpp2-c.4a'; l: $20; p: 0; crc: $375E1602), (n: 'tpp2-c.3a'; l: $20; p: $20; crc: $E950BEA1), (n: 'tpp2-c.5b'; l: $100; p: $40; crc: $C5826883),
    (n: 'tpp2-c.5a'; l: $100; p: $140; crc: $C576AFBA));
  popeye_char: tipo_roms = (n: 'tpp2-v.5n'; l: $1000; p: 0; crc: $CCA61DDD);
  popeye_sprites: array [0 .. 3] of tipo_roms = ((n: 'tpp2-v.1e'; l: $2000; p: 0; crc: $0F2CD853), (n: 'tpp2-v.1f'; l: $2000; p: $2000; crc: $888F3474), (n: 'tpp2-v.1j'; l: $2000; p: $4000;
    crc: $7E864668), (n: 'tpp2-v.1k'; l: $2000; p: $6000; crc: $49E1D170));
  skyskipper_rom: array [0 .. 6] of tipo_roms = ((n: 'tnx1-c.2a'; l: $1000; p: 0; crc: $BDC7F218), (n: 'tnx1-c.2b'; l: $1000; p: $1000; crc: $CBE601A8), (n: 'tnx1-c.2c'; l: $1000; p: $2000;
    crc: $5CA79ABF), (n: 'tnx1-c.2d'; l: $1000; p: $3000; crc: $6B7A7071), (n: 'tnx1-c.2e'; l: $1000; p: $4000; crc: $6B0C0525), (n: 'tnx1-c.2f'; l: $1000; p: $5000; crc: $D1712424), (n: 'tnx1-c.2g';
    l: $1000; p: $6000; crc: $8B33C4CF));
  skyskipper_pal: array [0 .. 3] of tipo_roms = ((n: 'tnx1-t.4a'; l: $20; p: 0; crc: $98846924), (n: 'tnx1-t.1a'; l: $20; p: $20; crc: $C2BCA435), (n: 'tnx1-t.3a'; l: $100; p: $40; crc: $8ABF9DE4),
    (n: 'tnx1-t.2a'; l: $100; p: $140; crc: $AA7FF322));
  skyskipper_char: tipo_roms = (n: 'tnx1-v.3h'; l: $800; p: 0; crc: $ECB6A046);
  skyskipper_sprites: array [0 .. 3] of tipo_roms = ((n: 'tnx1-t.1e'; l: $1000; p: 0; crc: $01C1120E), (n: 'tnx1-t.2e'; l: $1000; p: $1000; crc: $70292A71), (n: 'tnx1-t.3e'; l: $1000; p: $2000;
    crc: $92B6A0E8), (n: 'tnx1-t.5e'; l: $1000; p: $3000; crc: $CC5F0AC3));
  // Dip
  popeye_dip_a: array [0 .. 2] of def_dip = ((mask: $F; name: 'Coinage'; number: 9; dip: ((dip_val: $8; dip_name: '6C 1C'), (dip_val: $5; dip_name: '5C 1C'), (dip_val: $9;
    dip_name: '4C 1C'), (dip_val: $A; dip_name: '3C 1C'), (dip_val: $D; dip_name: '2C 1C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $3;
    dip_name: '1C 3C'), (dip_val: $0; dip_name: 'Freeplay'), (), (), (), (), (), (), ())), (mask: $60; name: 'Copyright'; number: 3;
    dip: ((dip_val: $40; dip_name: 'Nintendo'), (dip_val: $20; dip_name: 'Nintendo Co.,Ltd'), (dip_val: $60; dip_name: 'Nintendo of America'), (), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  popeye_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '1'), (dip_val: $2; dip_name: '2'), (dip_val: $1; dip_name: '3'), (dip_val: $0;
    dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Difficulty'; number: 4; dip: ((dip_val: $C; dip_name: 'Easy'), (dip_val: $8; dip_name: 'Medium'), (dip_val: $4;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $30; dip_name: '40K'), (dip_val: $20; dip_name: '60K'), (dip_val: $10; dip_name: '80K'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $40; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80;
    name: 'Cabinet'; number: 2; dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  skyskipper_dip_a: array [0 .. 1] of def_dip = ((mask: $F; name: 'Coinage'; number: 9; dip: ((dip_val: $3; dip_name: 'A 3/1 B 1/2'), (dip_val: $E; dip_name: '2C 1C'), (dip_val: $1;
    dip_name: 'A 2/1 B 2/5'), (dip_val: $4; dip_name: 'A 2/1 B 1/3'), (dip_val: $7; dip_name: 'A 1/1 B 2/1'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $C; dip_name: 'A 1/1 B 1/2'), (dip_val: $D;
    dip_name: '1C 2C'), (dip_val: $6; dip_name: 'A 1/2 B 1/4'), (dip_val: $B; dip_name: 'A 1/2 B 1/5'), (dip_val: $2; dip_name: 'A 2/5 B 1/1'), (dip_val: $A; dip_name: 'A 1/3 B 1/1'), (dip_val: $9;
    dip_name: 'A 1/4 B 1/1'), (dip_val: $5; dip_name: 'A 1/5 B 1/1'), (dip_val: $8; dip_name: 'A 1/6 B 1/1'), (dip_val: $0; dip_name: 'Freeplay'))), ());
  skyskipper_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '1'), (dip_val: $2; dip_name: '2'), (dip_val: $1; dip_name: '3'), (dip_val: $0;
    dip_name: '4'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $1C; name: 'Difficulty'; number: 8;
    dip: ((dip_val: $1C; dip_name: 'Easiest'), (dip_val: $18; dip_name: 'Very Easy'), (dip_val: $14; dip_name: 'Easy'), (dip_val: $10; dip_name: 'Medium Easy'), (dip_val: $C;
    dip_name: 'Medium Hard'), (dip_val: $8; dip_name: 'Hard'), (dip_val: $4; dip_name: 'Very Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), ())), (mask: $20;
    name: 'Bonus Life'; number: 2; dip: ((dip_val: $20; dip_name: '15K'), (dip_val: $0; dip_name: '30K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Service';
    number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  prot0, prot1, prot_shift, palette_bank, scroll_y, dswbit, field: byte;
  fondo_write, nmi_enabled: boolean;
  scroll_x: word;
  update_video_popeye_hw: procedure;
  back_ram: array [0 .. $FFF] of byte;

procedure update_video_popeye;
var
  f, color, nchar, x, y: word;
  atrib, atrib2: byte;
  punto: array [0 .. 511] of word;
  sx, sy, shift: integer;
begin
  if fondo_write then
  begin
    for y := 0 to 511 do
    begin
      sy := y - ($200 - (2 * scroll_y));
      for x := 0 to 511 do
      begin
        if (sy < 0) then
          punto[x] := paleta[back_ram[0] and $F]
        else
        begin
          sx := x + (2 * scroll_x) + $70;
          shift := sy and 4;
          punto[x] := paleta[(back_ram[(((sx div 8) and $3F) + ((sy div 8) * $40))] shr shift) and $F];
        end;
      end;
      putpixel(0, y, 512, @punto[0], 1);
    end;
    fondo_write := false;
  end;
  update_region(0, 0, 512, 512, 1, 0, 0, 512, 512, 3);
  // Sprites
  for f := 0 to $9E do
  begin
    atrib := buffer_sprites[3 + (f * 4)];
    atrib2 := buffer_sprites[2 + (f * 4)];
    nchar := ((atrib2 and $7F) + ((atrib and $10) shl 3) + ((atrib and $04) shl 6)) xor $1FF;
    color := ((atrib and $7) + (palette_bank and $07) shl 3) shl 2;
    put_gfx_sprite(nchar, color + 48, (atrib2 and $80) <> 0, (atrib and $08) <> 0, 1);
    x := (buffer_sprites[0 + (f * 4)] shl 1) - 6;
    y := (256 - buffer_sprites[1 + (f * 4)]) shl 1;
    update_gfx_sprite(x, y, 3, 1);
  end;
  // Chars
  for f := $0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$A000 + f];
      color := ((memory[$A400 + f]) and $F) shl 1;
      put_gfx_trans(x * 16, y * 16, nchar, color + 16, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 512, 512, 2, 0, 0, 512, 512, 3);
  update_final_piece(0, 32, 512, 448, 3);
end;

procedure events_popeye;
begin
  if event.arcade then
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
  if p_contrls.map_arcade.left[0] then
    marcade.in0 := (marcade.in0 or $2)
  else
    marcade.in0 := (marcade.in0 and $FD);
  if p_contrls.map_arcade.up[0] then
    marcade.in0 := (marcade.in0 or $4)
  else
    marcade.in0 := (marcade.in0 and $FB);
  if p_contrls.map_arcade.down[0] then
    marcade.in0 := (marcade.in0 or $8)
  else
    marcade.in0 := (marcade.in0 and $F7);
  if p_contrls.map_arcade.but0[0] then
    marcade.in0 := (marcade.in0 or $10)
  else
    marcade.in0 := (marcade.in0 and $EF);
  if p_contrls.map_arcade.but1[0] then
    marcade.in0 := (marcade.in0 or $80)
  else
    marcade.in0 := (marcade.in0 and $7F);
  // P2
  if p_contrls.map_arcade.right[1] then
    marcade.in1 := (marcade.in1 or $1)
  else
    marcade.in1 := (marcade.in1 and $FE);
  if p_contrls.map_arcade.left[1] then
    marcade.in1 := (marcade.in1 or $2)
  else
    marcade.in1 := (marcade.in1 and $FD);
  if p_contrls.map_arcade.up[1] then
    marcade.in1 := (marcade.in1 or $4)
  else
    marcade.in1 := (marcade.in1 and $FB);
  if p_contrls.map_arcade.down[1] then
    marcade.in1 := (marcade.in1 or $8)
  else
    marcade.in1 := (marcade.in1 and $F7);
  if p_contrls.map_arcade.but0[1] then
    marcade.in1 := (marcade.in1 or $10)
  else
    marcade.in1 := (marcade.in1 and $EF);
  if p_contrls.map_arcade.but1[1] then
    marcade.in1 := (marcade.in1 or $80)
  else
    marcade.in1 := (marcade.in1 and $7F);
  // SYSTEM
  if p_contrls.map_arcade.start[0] then
    marcade.in2 := (marcade.in2 or $4)
  else
    marcade.in2 := (marcade.in2 and $FB);
  if p_contrls.map_arcade.start[1] then
    marcade.in2 := (marcade.in2 or $8)
  else
    marcade.in2 := (marcade.in2 and $F7);
  if p_contrls.map_arcade.coin[1] then
    marcade.in2 := (marcade.in2 or $20)
  else
    marcade.in2 := (marcade.in2 and $DF);
  if p_contrls.map_arcade.coin[0] then
    marcade.in2 := (marcade.in2 or $80)
  else
    marcade.in2 := (marcade.in2 and $7F);
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or $4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or $8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $80)
    else
      marcade.in2 := (marcade.in2 and $7F);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or $4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or $8)
    else
      marcade.in2 := (marcade.in2 and $F7);
  end;
end;

procedure popeye_loop;
  procedure cambiar_paleta(valor: byte);
  var
    f, ctemp1, ctemp2, ctemp3, ctemp4: byte;
    colores: tcolor;
  begin
    for f := 0 to 15 do
    begin
      ctemp4 := buffer_paleta[f + $10 * valor];
      // red component
      ctemp1 := (ctemp4 shr 0) and $1;
      ctemp2 := (ctemp4 shr 1) and $1;
      ctemp3 := (ctemp4 shr 2) and $1;
      colores.r := $1C * ctemp1 + $31 * ctemp2 + $47 * ctemp3;
      // green component
      ctemp1 := (ctemp4 shr 3) and $1;
      ctemp2 := (ctemp4 shr 4) and $1;
      ctemp3 := (ctemp4 shr 5) and $1;
      colores.g := $1C * ctemp1 + $31 * ctemp2 + $47 * ctemp3;
      // blue component
      ctemp1 := (ctemp4 shr 6) and $1;
      ctemp2 := (ctemp4 shr 7) and $1;
      colores.b := $31 * ctemp1 + $47 * ctemp2;
      set_pal_color(colores, f);
    end;
    fondo_write := true;
  end;

var
  frame: single;
  f, tempw: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
    if EmulationPaused = false then
    begin
      for f := 0 to 511 do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 479 then
        begin
          update_video_popeye_hw;
          if nmi_enabled then
            z80_0.change_nmi(ASSERT_LINE);
          field := field xor $10;
          // dma
          tempw := ((memory[$8C02] and 1) shl 8) or memory[$8C00];
          if tempw <> scroll_x then
          begin
            scroll_x := tempw;
            fondo_write := true;
          end;
          tempw := memory[$8C01];
          if tempw <> scroll_y then
          begin
            scroll_y := tempw;
            fondo_write := true;
          end;
          copymemory(@buffer_sprites, @memory[$8C04], $2C7);
          if palette_bank <> memory[$8C03] then
          begin
            palette_bank := memory[$8C03];
            cambiar_paleta((palette_bank shr 3) and 1);
          end;
        end;
        events_popeye;
        video_sync
      end;
    end
    else
      pause_action;
end;

function popeye_getbyte(direccion: word): byte;
begin
  case direccion of
    $0000 .. $7FFF, $8800 .. $8FFF, $A000 .. $A7FF, $C000 .. $DFFF:
      popeye_getbyte := memory[direccion];
    $E000:
      popeye_getbyte := ((prot1 shl prot_shift) or (prot0 shr (8 - prot_shift))) and $FF;
    $E001:
      popeye_getbyte := 0;
  end;
end;

procedure popeye_putbyte(direccion: word; valor: byte);
var
  offset: word;
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8800 .. $8FFF:
      memory[direccion] := valor;
    $A000 .. $A7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $C000 .. $DFFF:
      if memory[direccion] <> valor then
      begin
        fondo_write := true;
        memory[direccion] := valor;
        offset := ((direccion and $1FFF) and $3F) or (((direccion and $1FFF) and not($7F)) shr 1);
        if (direccion and $40) = 0 then
          back_ram[offset] := (back_ram[offset] and $F0) or (valor and $F)
        else
          back_ram[offset] := (back_ram[offset] and $0F) or (valor shl 4);
      end;
    $E000:
      prot_shift := valor and $7;
    $E001:
      begin
        prot0 := prot1;
        prot1 := valor;
      end;
  end;
end;

function popeye_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      popeye_inbyte := marcade.in0;
    1:
      popeye_inbyte := marcade.in1;
    2:
      popeye_inbyte := marcade.in2 or field;
    3:
      popeye_inbyte := ay8910_0.read;
  end;
end;

procedure popeye_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      ay8910_0.control(valor);
    1:
      ay8910_0.write(valor);
  end;
end;

procedure popeye_nmi_clear(instruccion: byte);
var
  main_z80_reg: npreg_z80;
  temp_nmi: boolean;
begin
  main_z80_reg := z80_0.get_internal_r;
  temp_nmi := (main_z80_reg.i and 1) <> 0;
  if nmi_enabled <> temp_nmi then
  begin
    nmi_enabled := temp_nmi;
    if not(temp_nmi) then
      z80_0.change_nmi(CLEAR_LINE);
  end;
end;

function popeye_portar: byte;
begin
  popeye_portar := marcade.dswa or (marcade.dswb shl (7 - dswbit)) and $80;
end;

procedure popeye_portbw(valor: byte);
begin
  main_screen.flip_main_screen := (valor and 1) <> 0;
  dswbit := (valor and $E) shr 1; // El bit que quiere leer
end;

procedure popeye_sound_update;
begin
  ay8910_0.update;
end;

// Sky skipper
procedure update_video_skyskipper;
var
  f, color, nchar, x, y: word;
  atrib, atrib2: byte;
  punto: array [0 .. 511] of word;
  sx, sy, shift: integer;
begin
  if fondo_write then
  begin
    for y := 0 to 511 do
    begin
      sy := y - ($200 - (2 * scroll_y));
      for x := 0 to 511 do
      begin
        if (sy < 0) then
          punto[x] := paleta[back_ram[0] and $F]
        else
        begin
          sx := x + (2 * scroll_x) + $70;
          shift := (sx and $200) div $80;
          punto[x] := paleta[(back_ram[(((sx div 8) and $3F) + ((sy div 8) * $40))] shr shift) and $F];
        end;
      end;
      putpixel(0, y, 512, @punto[0], 1);
    end;
    fondo_write := false;
  end;
  update_region(0, 0, 512, 512, 1, 0, 0, 512, 512, 3);
  // Sprites
  for f := 0 to $9E do
  begin
    atrib := buffer_sprites[3 + (f * 4)];
    atrib2 := buffer_sprites[2 + (f * 4)];
    nchar := ((atrib2 and $7F) + ((atrib and $10) shl 3) + ((atrib and $04) shl 6)) xor $1FF;
    color := ((atrib and $7) + (palette_bank and $07) shl 3) shl 2;
    put_gfx_sprite(nchar and $FF, color + 48, (atrib2 and $80) <> 0, (atrib and $08) <> 0, 1);
    x := (buffer_sprites[0 + (f * 4)] shl 1) - 6;
    y := (256 - buffer_sprites[1 + (f * 4)]) shl 1;
    update_gfx_sprite(x, y, 3, 1);
  end;
  // Chars
  for f := $0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := memory[$A000 + f];
      color := ((memory[$A400 + f]) and $F) shl 1;
      put_gfx_trans(x * 16, y * 16, nchar, color + 16, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 512, 512, 2, 0, 0, 512, 512, 3);
  update_final_piece(0, 32, 512, 448, 3);
end;

function skyskipper_getbyte(direccion: word): byte;
begin
  case direccion of
    $0000 .. $7FFF, $8000 .. $87FF, $8C00 .. $8FFF, $A000 .. $A7FF, $C000 .. $CFFF:
      skyskipper_getbyte := memory[direccion];
    $E000:
      skyskipper_getbyte := ((prot1 shl prot_shift) or (prot0 shr (8 - prot_shift))) and $FF;
    $E001:
      skyskipper_getbyte := 0;
  end;
end;

procedure skyskipper_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ;
    $8000 .. $87FF, $8C00 .. $8FFF:
      memory[direccion] := valor;
    $A000 .. $A7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $C000 .. $CFFF:
      if memory[direccion] <> valor then
      begin
        fondo_write := true;
        memory[direccion] := valor;
        if (valor and $80) = 0 then
          back_ram[direccion and $FFF] := (back_ram[direccion and $FFF] and $F0) or (valor and $F)
        else
          back_ram[direccion and $FFF] := (back_ram[direccion and $FFF] and $0F) or (valor shl 4);
      end;
    $E000:
      prot_shift := valor and $7;
    $E001:
      begin
        prot0 := prot1;
        prot1 := valor;
      end;
  end;
end;

procedure popeye_qsave(nombre: string);
var
  data: pbyte;
  size: word;
  buffer: array [0 .. 7] of byte;
begin
  case main_vars.machine_type of
    39:
      open_qsnapshot_save('popeye' + nombre);
    376:
      open_qsnapshot_save('skyskipper' + nombre);
  end;
  getmem(data, 200);
  // CPU
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ay8910_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory[$8000], $8000);
  // MISC
  buffer[0] := prot0;
  buffer[1] := prot1;
  buffer[2] := prot_shift;
  buffer[3] := palette_bank;
  buffer[4] := scroll_y;
  buffer[5] := dswbit;
  buffer[6] := scroll_x and $FF;
  buffer[7] := scroll_x shr 8;
  savedata_qsnapshot(@buffer, 8);
  freemem(data);
  close_qsnapshot;
end;

procedure popeye_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 7] of byte;
begin
  case main_vars.machine_type of
    39:
      if not(open_qsnapshot_load('popeye' + nombre)) then
        exit;
    376:
      if not(open_qsnapshot_load('skyskipper' + nombre)) then
        exit;
  end;
  getmem(data, 200);
  // CPU
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ay8910_0.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory[$8000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  prot0 := buffer[0];
  prot1 := buffer[1];
  prot_shift := buffer[2];
  palette_bank := buffer[3];
  scroll_y := buffer[4];
  dswbit := buffer[5];
  scroll_x := buffer[6] or (buffer[7] shl 8);
  freemem(data);
  close_qsnapshot;
  fillchar(fondo_write, $2000, 1);
  fillchar(gfx[0].buffer, $400, 1);
end;

// Main
procedure reset_popeye;
begin
  z80_0.reset;
  ay8910_0.reset;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  // Importante: forzar un cambio de paleta!!
  fondo_write := true;
  palette_bank := $FF;
  prot0 := 0;
  prot1 := 0;
  prot_shift := 0;
  scroll_y := 0;
  dswbit := 0;
  scroll_x := 0;
  field := 0;
  nmi_enabled := false;
  fillchar(back_ram, $1000, 0);
end;

function start_popeye: boolean;
var
  colores: tcolor;
  f, pos: word;
  ctemp1, ctemp2, ctemp3, ctemp4: byte;
  memory_temp: array [0 .. $7FFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (7 + ($2000 * 8), 6 + ($2000 * 8), 5 + ($2000 * 8), 4 + ($2000 * 8), 3 + ($2000 * 8), 2 + ($2000 * 8), 1 + ($2000 * 8), 0 + ($2000 * 8), 7, 6, 5, 4, 3, 2, 1, 0);
  pss_x: array [0 .. 15] of dword = (7 + ($1000 * 8), 6 + ($1000 * 8), 5 + ($1000 * 8), 4 + ($1000 * 8), 3 + ($1000 * 8), 2 + ($1000 * 8), 1 + ($1000 * 8), 0 + ($1000 * 8), 7, 6, 5, 4, 3, 2, 1, 0);
  ps_y: array [0 .. 15] of dword = (15 * 8, 14 * 8, 13 * 8, 12 * 8, 11 * 8, 10 * 8, 9 * 8, 8 * 8, 7 * 8, 6 * 8, 5 * 8, 4 * 8, 3 * 8, 2 * 8, 1 * 8, 0 * 8);
  pc_x: array [0 .. 15] of dword = (7, 7, 6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0);
  pc_y: array [0 .. 15] of dword = (0 * 8, 0 * 8, 1 * 8, 1 * 8, 2 * 8, 2 * 8, 3 * 8, 3 * 8, 4 * 8, 4 * 8, 5 * 8, 5 * 8, 6 * 8, 6 * 8, 7 * 8, 7 * 8);
begin
  machine_calls.general_loop := popeye_loop;
  machine_calls.reset := reset_popeye;
  machine_calls.save_qsnap := popeye_qsave;
  machine_calls.load_qsnap := popeye_qload;
  machine_calls.fps_max := 59.94;
  start_popeye := false;
  start_audio(false);
  screen_init(1, 512, 512);
  screen_init(2, 512, 512, true);
  screen_init(3, 512, 512, false, true);
  start_video(512, 448);
  // Main CPU
  z80_0 := cpu_z80.create(8000000 div 2, 512);
  z80_0.change_misc_calls(nil, nil, popeye_nmi_clear);
  z80_0.change_io_calls(popeye_inbyte, popeye_outbyte);
  z80_0.init_sound(popeye_sound_update);
  // Audio chips
  ay8910_0 := ay8910_chip.create(8000000 div 4, AY8910, 1);
  ay8910_0.change_io_calls(popeye_portar, nil, nil, popeye_portbw);
  case main_vars.machine_type of
    39:
      begin // Popeye
        update_video_popeye_hw := update_video_popeye;
        z80_0.change_ram_calls(popeye_getbyte, popeye_putbyte);
        // cargar roms y decodificarlas
        if not(roms_load(@memory_temp, popeye_rom)) then
          exit;
        for f := 0 to $7FFF do
        begin
          pos := bitswap16(f, 15, 14, 13, 12, 11, 10, 8, 7, 6, 3, 9, 5, 4, 2, 1, 0);
          memory[f] := bitswap8(memory_temp[pos xor $3F], 3, 4, 2, 5, 1, 6, 0, 7);
        end;
        // convertir chars
        if not(roms_load(@memory_temp, popeye_char)) then
          exit;
        init_gfx(0, 16, 16, $100);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(1, 0, 8 * 8, 0);
        convert_gfx(0, 0, @memory_temp[$800], @pc_x, @pc_y, false, false);
        // convertir sprites
        if not(roms_load(@memory_temp, popeye_sprites)) then
          exit;
        init_gfx(1, 16, 16, $200);
        gfx[1].trans[0] := true;
        for f := 0 to 1 do
        begin
          gfx_set_desc_data(2, 2, 16 * 8, (0 + f * $1000) * 8, ($4000 + f * $1000) * 8);
          convert_gfx(1, 256 * f * 16 * 16, @memory_temp, @ps_x, @ps_y, false, false);
        end;
        // DIP
        marcade.dswa := $5F;
        marcade.dswb := $3D;
        marcade.dswa_val := @popeye_dip_a;
        marcade.dswb_val := @popeye_dip_b;
        if not(roms_load(@memory_temp, popeye_pal)) then
          exit;
      end;
    376:
      begin // Sky Skipper
        update_video_popeye_hw := update_video_skyskipper;
        z80_0.change_ram_calls(skyskipper_getbyte, skyskipper_putbyte);
        // cargar roms y decodificarlas
        if not(roms_load(@memory_temp, skyskipper_rom)) then
          exit;
        for f := 0 to $6FFF do
        begin
          pos := bitswap16(f, 15, 14, 13, 12, 11, 10, 8, 7, 0, 1, 2, 4, 5, 9, 3, 6);
          memory[f] := bitswap8(memory_temp[pos xor $FC], 3, 4, 2, 5, 1, 6, 0, 7);
        end;
        fillchar(memory[$7000], $1000, $FF);
        // convertir chars
        if not(roms_load(@memory_temp, skyskipper_char)) then
          exit;
        init_gfx(0, 16, 16, $100);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(1, 0, 8 * 8, 0);
        convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
        // convertir sprites
        if not(roms_load(@memory_temp, skyskipper_sprites)) then
          exit;
        init_gfx(1, 16, 16, $100);
        gfx[1].trans[0] := true;
        for f := 0 to 1 do
        begin
          gfx_set_desc_data(2, 2, 16 * 8, (0 + f * $800) * 8, ($2000 + f * $800) * 8);
          convert_gfx(1, $80 * f * 16 * 16, @memory_temp, @pss_x, @ps_y, false, false);
        end;
        // DIP
        marcade.dswa := $7F;
        marcade.dswb := $6D;
        marcade.dswa_val := @skyskipper_dip_a;
        marcade.dswb_val := @skyskipper_dip_b;
        if not(roms_load(@memory_temp, skyskipper_pal)) then
          exit;
      end;
  end;
  for f := 0 to $23F do
    memory_temp[f] := memory_temp[f] xor $FF;
  for f := 0 to $1F do
    buffer_paleta[f] := memory_temp[f];
  for f := 0 to 15 do
    ctemp4 := f or ((f and 8) shl 1);
  // red component
  ctemp1 := (memory_temp[ctemp4 + $20] shr 0) and $1;
  ctemp2 := (memory_temp[ctemp4 + $20] shr 1) and $1;
  ctemp3 := (memory_temp[ctemp4 + $20] shr 2) and $1;
  colores.r := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
  // green component
  ctemp1 := (memory_temp[ctemp4 + $20] shr 3) and $1;
  ctemp2 := (memory_temp[ctemp4 + $20] shr 4) and $1;
  ctemp3 := (memory_temp[ctemp4 + $20] shr 5) and $1;
  colores.g := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
  // blue component
  ctemp1 := 0;
  ctemp2 := (memory_temp[ctemp4 + $20] shr 6) and $1;
  ctemp3 := (memory_temp[ctemp4 + $20] shr 7) and $1;
  colores.b := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
  set_pal_color(colores, 16 + (2 * f) + 1);

  // Poner la paleta sprites
  for f := 0 to $FF do
  begin
    // red component
    ctemp1 := (memory_temp[$40 + f] shr 0) and $1;
    ctemp2 := (memory_temp[$40 + f] shr 1) and $1;
    ctemp3 := (memory_temp[$40 + f] shr 2) and $1;
    colores.r := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
    // green component
    ctemp1 := (memory_temp[$40 + f] shr 3) and $1;
    ctemp2 := (memory_temp[$140 + f] shr 0) and $1;
    ctemp3 := (memory_temp[$140 + f] shr 1) and $1;
    colores.g := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
    // blue component
    ctemp1 := 0;
    ctemp2 := (memory_temp[$140 + f] shr 2) and $1;
    ctemp3 := (memory_temp[$140 + f] shr 3) and $1;
    colores.b := $21 * ctemp1 + $47 * ctemp2 + $97 * ctemp3;
    set_pal_color(colores, 48 + f);
  end;
  // final
  reset_popeye;
  start_popeye := true;
end;

end.
