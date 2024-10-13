unit snk68_hw;

interface

uses
  WinApi.Windows,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  ym_3812,
  nz80,
  upd7759,
  sound_engine;

function start_snk68: boolean;

implementation

const
  // POW
  pow_rom: array [0 .. 1] of tipo_roms = ((n: 'dg1ver1.j14'; l: $20000; p: 0; crc: $8E71A8AF), (n: 'dg2ver1.l14'; l: $20000; p: $1; crc: $4287AFFC));
  pow_char: array [0 .. 1] of tipo_roms = ((n: 'dg9.l25'; l: $8000; p: 0; crc: $DF864A08), (n: 'dg10.m25'; l: $8000; p: $8000; crc: $9E470D53));
  pow_sound: tipo_roms = (n: 'dg8.e25'; l: $10000; p: 0; crc: $D1D61DA3);
  pow_upd: tipo_roms = (n: 'dg7.d20'; l: $10000; p: 0; crc: $ABA9A9D3);
  pow_sprites: array [0 .. 15] of tipo_roms = ((n: 'snk880.11a'; l: $20000; p: $0; crc: $E70FD906), (n: 'snk880.15a'; l: $20000; p: $1; crc: $7A90E957), (n: 'snk880.12a'; l: $20000; p: $40000;
    crc: $628B1AED), (n: 'snk880.16a'; l: $20000; p: $40001; crc: $E40A6C13), (n: 'snk880.13a'; l: $20000; p: $80000; crc: $19DC8868), (n: 'snk880.17a'; l: $20000; p: $80001; crc: $C7931CC2),
    (n: 'snk880.14a'; l: $20000; p: $C0000; crc: $47CD498B), (n: 'snk880.18a'; l: $20000; p: $C0001; crc: $EED72232), (n: 'snk880.19a'; l: $20000; p: $100000; crc: $1775B8DD), (n: 'snk880.23a';
    l: $20000; p: $100001; crc: $ADB6AD68), (n: 'snk880.20a'; l: $20000; p: $140000; crc: $F8E752EC), (n: 'snk880.24a'; l: $20000; p: $140001; crc: $DD41865A), (n: 'snk880.21a'; l: $20000; p: $180000;
    crc: $27E9FFFE), (n: 'snk880.25a'; l: $20000; p: $180001; crc: $055759AD), (n: 'snk880.22a'; l: $20000; p: $1C0000; crc: $AA9C00D8), (n: 'snk880.26a'; l: $20000; p: $1C0001; crc: $9BC261C5));
  // Street Smart
  streetsm_rom: array [0 .. 1] of tipo_roms = ((n: 's2-1ver2.14h'; l: $20000; p: 0; crc: $655F4773), (n: 's2-2ver2.14k'; l: $20000; p: $1; crc: $EFAE4823));
  streetsm_char: array [0 .. 1] of tipo_roms = ((n: 's2-9.25l'; l: $8000; p: 0; crc: $09B6AC67), (n: 's2-10.25m'; l: $8000; p: $8000; crc: $89E4EE6F));
  streetsm_sound: tipo_roms = (n: 's2-5.16c'; l: $10000; p: 0; crc: $CA4B171E);
  streetsm_upd: tipo_roms = (n: 's2-6.18d'; l: $20000; p: 0; crc: $47DB1605);
  streetsm_sprites: array [0 .. 5] of tipo_roms = ((n: 'stsmart.900'; l: $80000; p: $0; crc: $A8279A7E), (n: 'stsmart.902'; l: $80000; p: $80000; crc: $2F021AA1), (n: 'stsmart.904'; l: $80000;
    p: $100000; crc: $167346F7), (n: 'stsmart.901'; l: $80000; p: $200000; crc: $C305AF12), (n: 'stsmart.903'; l: $80000; p: $280000; crc: $73C16D35), (n: 'stsmart.905'; l: $80000; p: $300000;
    crc: $A5BEB4E2));
  // Ikari 3
  ikari3_rom: array [0 .. 1] of tipo_roms = ((n: 'ik3-2-ver1.c10'; l: $20000; p: 0; crc: $1BAE8023), (n: 'ik3-3-ver1.c9'; l: $20000; p: $1; crc: $10E38B66));
  ikari3_char: array [0 .. 1] of tipo_roms = ((n: 'ik3-7.bin'; l: $8000; p: 0; crc: $0B4804DF), (n: 'ik3-8.bin'; l: $8000; p: $8000; crc: $10AB4E50));
  ikari3_sound: tipo_roms = (n: 'ik3-5.bin'; l: $10000; p: 0; crc: $CE6706FC);
  ikari3_upd: tipo_roms = (n: 'ik3-6.bin'; l: $20000; p: 0; crc: $59D256A4);
  ikari3_sprites: array [0 .. 19] of tipo_roms = ((n: 'ik3-23.bin'; l: $20000; p: $000000; crc: $D0FD5C77), (n: 'ik3-13.bin'; l: $20000; p: $000001; crc: $9A56BD32), (n: 'ik3-22.bin'; l: $20000;
    p: $040000; crc: $4878D883), (n: 'ik3-12.bin'; l: $20000; p: $040001; crc: $0CE6A10A), (n: 'ik3-21.bin'; l: $20000; p: $080000; crc: $50D0FBF0), (n: 'ik3-11.bin'; l: $20000; p: $080001;
    crc: $E4E2BE43), (n: 'ik3-20.bin'; l: $20000; p: $0C0000; crc: $9A851EFC), (n: 'ik3-10.bin'; l: $20000; p: $0C0001; crc: $AC222372), (n: 'ik3-19.bin'; l: $20000; p: $100000; crc: $4EBDBA89),
    (n: 'ik3-9.bin'; l: $20000; p: $100001; crc: $C33971C2), (n: 'ik3-14.bin'; l: $20000; p: $200000; crc: $453BEA77), (n: 'ik3-24.bin'; l: $20000; p: $200001; crc: $E9B26D68), (n: 'ik3-15.bin';
    l: $20000; p: $240000; crc: $781A81FC), (n: 'ik3-25.bin'; l: $20000; p: $240001; crc: $073B03F1), (n: 'ik3-16.bin'; l: $20000; p: $280000; crc: $80BA400B), (n: 'ik3-26.bin'; l: $20000; p: $280001;
    crc: $9C613561), (n: 'ik3-17.bin'; l: $20000; p: $2C0000; crc: $0CC3CE4A), (n: 'ik3-27.bin'; l: $20000; p: $2C0001; crc: $16DD227E), (n: 'ik3-18.bin'; l: $20000; p: $300000; crc: $BA106245),
    (n: 'ik3-28.bin'; l: $20000; p: $300001; crc: $711715AE));
  ikari3_rom2: array [0 .. 1] of tipo_roms = ((n: 'ik3-1.c8'; l: $10000; p: 0; crc: $47E4D256), (n: 'ik3-4.c12'; l: $10000; p: $1; crc: $A43AF6B5));
  // Search and Rescue
  sar_rom: array [0 .. 1] of tipo_roms = ((n: 'bhw.2'; l: $20000; p: 0; crc: $E1430138), (n: 'bhw.3'; l: $20000; p: $1; crc: $EE1F9374));
  sar_char: array [0 .. 1] of tipo_roms = ((n: 'bh.7'; l: $8000; p: 0; crc: $B0F1B049), (n: 'bh.8'; l: $8000; p: $8000; crc: $174DDBA7));
  sar_sound: tipo_roms = (n: 'bh.5'; l: $10000; p: 0; crc: $53E2FA76);
  sar_upd: tipo_roms = (n: 'bh.v1'; l: $20000; p: 0; crc: $07A6114B);
  sar_sprites: array [0 .. 5] of tipo_roms = ((n: 'bh.c1'; l: $80000; p: $000000; crc: $1FB8F0AE), (n: 'bh.c3'; l: $80000; p: $080000; crc: $FD8BC407), (n: 'bh.c5'; l: $80000; p: $100000;
    crc: $1D30ACC3), (n: 'bh.c2'; l: $80000; p: $200000; crc: $7C803767), (n: 'bh.c4'; l: $80000; p: $280000; crc: $EEDE7C43), (n: 'bh.c6'; l: $80000; p: $300000; crc: $9F785CD9));
  sar_rom2: array [0 .. 1] of tipo_roms = ((n: 'bhw.1'; l: $20000; p: 0; crc: $62B60066), (n: 'bhw.4'; l: $20000; p: $1; crc: $16D8525C));

var
  rom, rom2: array [0 .. $1FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  video_ram: array [0 .. $7FF] of word;
  sprite_ram: array [0 .. $3FFF] of word;
  sound_latch, dsw1, sound_stat, protection: byte;
  fg_tile_offset: word;
  is_pow, sprite_flip: boolean;
  update_video_nmk68: procedure;

  { Primer bloque de $1000bytes
    0:$FF -> Fijo
    1:----xxxx xxxx----
    2 y 3:xxxxyyyy yyyyyyyy }
procedure poner_sprites(group: byte);
var
  f, i, nchar, atrib, color, x: word;
  tiledata_pos: word;
  y: integer;
  flipx, flipy: boolean;
begin
  tiledata_pos := $800 * 2 * group;
  for f := 0 to $1F do
  begin
    x := (sprite_ram[((f * $80) + 4 * group) shr 1] and $FF) shl 4;
    y := sprite_ram[(((f * $80) + 4 * group) shr 1) + 1];
    x := x or (y shr 12);
    x := (((x + 16) and $1FF) - 16) and $1FF;
    y := -y;
    // every sprite is a column 32 tiles (512 pixels) tall
    for i := 0 to $1F do
    begin
      y := y and $1FF;
      if ((y <= 256) and ((y + 15) >= 0)) then
      begin
        color := sprite_ram[tiledata_pos shr 1] and $7F;
        atrib := sprite_ram[(tiledata_pos shr 1) + 1];
        if is_pow then
        begin
          nchar := atrib and $3FFF;
          flipx := (atrib and $4000) <> 0;
          flipy := (atrib and $8000) <> 0;
          if nchar <> $FF then
          begin
            put_gfx_sprite(nchar, color shl 4, flipx, flipy, 1);
            update_gfx_sprite(x, y, 2, 1);
          end;
        end
        else
        begin
          if sprite_flip then
          begin
            flipx := false;
            flipy := (atrib and $8000) <> 0;
          end
          else
          begin
            flipx := (atrib and $8000) <> 0;
            flipy := false;
          end;
          nchar := atrib and $7FFF;
          if nchar <> $7FFF then
          begin
            put_gfx_sprite(nchar, color shl 4, flipx, flipy, 1);
            update_gfx_sprite(x, y, 2, 1);
          end;
        end;
      end;
      tiledata_pos := tiledata_pos + 4;
      y := y + 16;
    end;
  end;
end;

procedure update_video_pow;
var
  f: word;
  color: word;
  x, y, nchar: word;
begin
  fill_full_screen(2, $7FF);
  for f := $0 to $3FF do
  begin
    color := video_ram[((f * 4) shr 1) + 1] and $7;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f div 32;
      y := f mod 32;
      nchar := fg_tile_offset + (video_ram[(f * 4) shr 1] and $FF);
      put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  poner_sprites(2);
  poner_sprites(3);
  poner_sprites(1);
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  update_final_piece(0, 16, 256, 224, 2);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure update_video_ikari3;
var
  f, atrib, nchar: word;
  color, x, y: byte;
begin
  fill_full_screen(2, $7FF);
  for f := $0 to $3FF do
  begin
    atrib := video_ram[(f * 4) shr 1];
    color := (atrib shr 12) and $7;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f div 32;
      y := f mod 32;
      nchar := atrib and $7FF;
      if (atrib and $8000) <> 0 then
        put_gfx(x * 8, y * 8, nchar, color shl 4, 1, 0)
      else
        put_gfx_trans(x * 8, y * 8, nchar, color shl 4, 1, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  poner_sprites(2);
  poner_sprites(3);
  poner_sprites(1);
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  update_final_piece(0, 16, 256, 224, 2);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_pow;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // COIN
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
  end;
end;

procedure snk68_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // Sound CPU
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          m68000_0.irq[1] := HOLD_LINE;
          update_video_nmk68;
        end;
      end;
      events_pow;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function pow_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $3FFFF:
      pow_getword := rom[direccion shr 1];
    $40000 .. $43FFF:
      pow_getword := ram[(direccion and $3FFF) shr 1];
    $80000:
      pow_getword := (marcade.in1 shl 8) + marcade.in0;
    $C0000:
      pow_getword := marcade.in2;
    $F0000:
      pow_getword := (dsw1 shl 8) or $FF;
    $F0008:
      pow_getword := $00FF;
    $F8000:
      pow_getword := sound_stat shl 8;
    $100000 .. $101FFF:
      pow_getword := video_ram[(direccion and $FFF) shr 1] or $FF00;
    $200000 .. $207FFF:
      if (direccion and $2) = 0 then
        pow_getword := sprite_ram[(direccion and $7FFF) shr 1] or $FF00
      else
        pow_getword := sprite_ram[(direccion and $7FFF) shr 1];
    $400000 .. $400FFF:
      pow_getword := buffer_paleta[(direccion and $FFF) shr 1];
  end;
end;

procedure change_color(tmp_color, numero: word);
var
  color: tcolor;
begin
  color.r := pal5bit(((tmp_color shr 7) and $1E) or ((tmp_color shr 14) and $01));
  color.g := pal5bit(((tmp_color shr 3) and $1E) or ((tmp_color shr 13) and $01));
  color.b := pal5bit(((tmp_color shl 1) and $1E) or ((tmp_color shr 12) and $01));
  set_pal_color(color, numero);
  buffer_color[(numero shr 4) and $7] := true;
end;

procedure pow_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $3FFFF:
      ;
    $40000 .. $43FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $80000:
      begin
        sound_latch := valor shr 8;
        z80_0.change_nmi(PULSE_LINE);
      end;
    $C0000:
      begin
        fg_tile_offset := (valor and $70) shl 4;
        sprite_flip := (valor and 4) <> 0;
      end;
    $100000 .. $101FFF:
      if video_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        video_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) div 4] := true;
      end;
    $200000 .. $207FFF:
      sprite_ram[(direccion and $7FFF) shr 1] := valor;
    $400000 .. $400FFF:
      if buffer_paleta[(direccion and $FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color(valor, (direccion and $FFF) shr 1);
      end;
  end;
end;

function pow_snd_getbyte(direccion: word): byte;
begin
  if direccion = $F800 then
    pow_snd_getbyte := sound_latch
  else
    pow_snd_getbyte := mem_snd[direccion];
end;

procedure pow_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $EFFF:
      ;
    $F800:
      sound_stat := valor;
  else
    mem_snd[direccion] := valor;
  end;
end;

function pow_snd_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = 0 then
    pow_snd_inbyte := ym3812_0.status;
end;

procedure pow_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $00:
      ym3812_0.control(valor);
    $20:
      ym3812_0.write(valor);
    $40:
      begin
        upd7759_0.port_w(valor);
        upd7759_0.start_w(0);
        upd7759_0.start_w(1);
      end;
    $80:
      upd7759_0.reset_w(valor and $80);
  end;
end;

procedure snk68_sound_update;
begin
  ym3812_0.update;
  upd7759_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

// Ikari 3
function ikari3_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $3FFFF:
      ikari3_getword := rom[direccion shr 1];
    $40000 .. $43FFF:
      ikari3_getword := ram[(direccion and $3FFF) shr 1];
    $80000:
      ikari3_getword := marcade.in0 xor protection;
    $80002:
      ikari3_getword := marcade.in1 xor protection;
    $80004:
      ikari3_getword := marcade.in2 xor protection;
    $F0000:
      ikari3_getword := $00FF;
    $F0008:
      ikari3_getword := $80FF;
    $F8000:
      ikari3_getword := sound_stat shl 8;
    $100000 .. $107FFF:
      if (direccion and $2) = 0 then
        ikari3_getword := sprite_ram[(direccion and $7FFF) shr 1] or $FF00
      else
        ikari3_getword := sprite_ram[(direccion and $7FFF) shr 1];
    $200000 .. $201FFF:
      ikari3_getword := video_ram[(direccion and $FFF) shr 1];
    $300000 .. $33FFFF:
      ikari3_getword := rom2[(direccion and $3FFFF) shr 1];
    $400000 .. $400FFF:
      ikari3_getword := buffer_paleta[(direccion and $FFF) shr 1];
  end;
end;

procedure ikari3_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $3FFFF, $300000 .. $33FFFF:
      ;
    $40000 .. $43FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $80000:
      begin
        sound_latch := valor shr 8;
        z80_0.change_nmi(PULSE_LINE);
      end;
    $80006:
      if (valor = 7) then
        protection := $FF
      else
        protection := 0;
    $C0000:
      sprite_flip := (valor and $4) <> 0;
    $100000 .. $107FFF:
      sprite_ram[(direccion and $7FFF) shr 1] := valor;
    $200000 .. $201FFF:
      if video_ram[(direccion and $FFF) shr 1] <> valor then
      begin
        video_ram[(direccion and $FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) div 4] := true;
      end;
    $400000 .. $400FFF:
      if (buffer_paleta[(direccion and $FFF) shr 1] <> valor) then
      begin
        buffer_paleta[(direccion and $FFF) shr 1] := valor;
        change_color(valor, ((direccion and $FFF) shr 1));
      end;
  end;
end;

// Main
procedure reset_snk68;
begin
  m68000_0.reset;
  z80_0.reset;
  ym3812_0.reset;
  upd7759_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  fg_tile_offset := 0;
  sound_latch := 0;
  sound_stat := 0;
  protection := 0;
  sprite_flip := false;
end;

function start_snk68: boolean;
const
  pc_x: array [0 .. 7] of dword = (8 * 8 + 3, 8 * 8 + 2, 8 * 8 + 1, 8 * 8 + 0, 3, 2, 1, 0);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  ps_x: array [0 .. 15] of dword = (32 * 8 + 7, 32 * 8 + 6, 32 * 8 + 5, 32 * 8 + 4, 32 * 8 + 3, 32 * 8 + 2, 32 * 8 + 1, 32 * 8 + 0, 7, 6, 5, 4, 3, 2, 1, 0);
  ps_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
var
  memory_temp: pbyte;

  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $800);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 8, 0, 4, $800 * 16 * 8 + 0, $800 * 16 * 8 + 4);
    convert_gfx(0, 0, memory_temp, @pc_x, @pc_y, false, false);
  end;

  procedure convert_sprites(num: dword);
  begin
    init_gfx(1, 16, 16, num);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(4, 0, 64 * 8, 0, 8, num * 64 * 8 + 0, num * 64 * 8 + 8);
    convert_gfx(1, 0, memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  machine_calls.general_loop := snk68_loop;
  machine_calls.reset := reset_snk68;
  machine_calls.fps_max := 59.185606;
  start_snk68 := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_init(2, 512, 512, false, true);
  // SIEMPRE ANTES DE INICIAR EL VIDEO!!!
  if main_vars.machine_type = 150 then
    main_screen.rot90_screen := true;
  start_video(256, 224);
  // Main CPU
  getmem(memory_temp, $400000);
  m68000_0 := cpu_m68000.create(9000000, 264);
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, 264);
  z80_0.change_ram_calls(pow_snd_getbyte, pow_snd_putbyte);
  z80_0.change_io_calls(pow_snd_inbyte, pow_snd_outbyte);
  z80_0.init_sound(snk68_sound_update);
  // Sound Chips
  ym3812_0 := ym3812_chip.create(YM3812_FM, 4000000);
  ym3812_0.change_irq_calls(snd_irq);
  upd7759_0 := upd7759_chip.create(0.5);
  case main_vars.machine_type of
    136:
      begin // POW
        m68000_0.change_ram16_calls(pow_getword, pow_putword);
        // cargar roms
        if not(roms_load16w(@rom, pow_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, pow_sound)) then
          exit;
        // ADPCM Sounds
        if not(roms_load(upd7759_0.get_rom_addr, pow_upd)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, pow_char)) then
          exit;
        convert_chars;
        // sprites
        if not(roms_load16b(memory_temp, pow_sprites)) then
          exit;
        convert_sprites($4000);
        is_pow := true;
        dsw1 := $10;
        update_video_nmk68 := update_video_pow;
      end;
    137:
      begin // Street Smart
        m68000_0.change_ram16_calls(pow_getword, pow_putword);
        // cargar roms
        if not(roms_load16w(@rom, streetsm_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, streetsm_sound)) then
          exit;
        // ADPCM Sounds
        if not(roms_load(upd7759_0.get_rom_addr, streetsm_upd)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, streetsm_char)) then
          exit;
        convert_chars;
        // sprites
        if not(roms_load(memory_temp, streetsm_sprites)) then
          exit;
        convert_sprites($8000);
        is_pow := false;
        dsw1 := 0;
        update_video_nmk68 := update_video_pow;
      end;
    149:
      begin // Ikari 3
        m68000_0.change_ram16_calls(ikari3_getword, ikari3_putword);
        // cargar roms
        if not(roms_load16w(@rom, ikari3_rom)) then
          exit;
        if not(roms_load16w(@rom2, ikari3_rom2)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, ikari3_sound)) then
          exit;
        // ADPCM Sounds
        if not(roms_load(upd7759_0.get_rom_addr, ikari3_upd)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, ikari3_char)) then
          exit;
        convert_chars;
        // sprites
        if not(roms_load16b(memory_temp, ikari3_sprites)) then
          exit;
        convert_sprites($8000);
        is_pow := false;
        update_video_nmk68 := update_video_ikari3;
      end;
    150:
      begin // Search and Rescue
        m68000_0.change_ram16_calls(ikari3_getword, ikari3_putword);
        // cargar roms
        if not(roms_load16w(@rom, sar_rom)) then
          exit;
        if not(roms_load16w(@rom2, sar_rom2)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, sar_sound)) then
          exit;
        // ADPCM Sounds
        if not(roms_load(upd7759_0.get_rom_addr, sar_upd)) then
          exit;
        // convertir chars
        if not(roms_load(memory_temp, sar_char)) then
          exit;
        convert_chars;
        // sprites
        if not(roms_load(memory_temp, sar_sprites)) then
          exit;
        convert_sprites($8000);
        is_pow := false;
        update_video_nmk68 := update_video_ikari3;
      end;
  end;
  // final
  freemem(memory_temp);
  reset_snk68;
  start_snk68 := true;
end;

end.
