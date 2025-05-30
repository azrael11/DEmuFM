unit cavemanninja_hw;

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
  hu6280,
  deco_16ic,
  deco_common,
  deco_104,
  deco_146,
  misc_functions;

function start_cavemanninja: boolean;

implementation

const
  // Caveman Ninja
  cninja_rom: array [0 .. 5] of tipo_roms = ((n: 'gn-02-3.1k'; l: $20000; p: 0; crc: $39AEA12A), (n: 'gn-05-2.3k'; l: $20000; p: $1; crc: $0F4360EF), (n: 'gn-01-2.1j'; l: $20000; p: $40000; crc: $F740EF7E), (n: 'gn-04-2.3j'; l: $20000; p: $40001; crc: $C98FCB62), (n: 'gn-00.rom';
    l: $20000; p: $80000; crc: $0B110B16), (n: 'gn-03.rom'; l: $20000; p: $80001; crc: $1E28E697));
  cninja_sound: tipo_roms = (n: 'gl-07.rom'; l: $10000; p: $0; crc: $CA8BEF96);
  cninja_chars: array [0 .. 1] of tipo_roms = ((n: 'gl-09.rom'; l: $10000; p: $0; crc: $5A2D4752), (n: 'gl-08.rom'; l: $10000; p: 1; crc: $33A2B400));
  cninja_tiles1: tipo_roms = (n: 'mag-02.rom'; l: $80000; p: $0; crc: $DE89C69A);
  cninja_tiles2: array [0 .. 1] of tipo_roms = ((n: 'mag-00.rom'; l: $80000; p: $0; crc: $A8F05D33), (n: 'mag-01.rom'; l: $80000; p: $80000; crc: $5B399EED));
  cninja_oki2: tipo_roms = (n: 'mag-07.rom'; l: $80000; p: 0; crc: $08EB5264);
  cninja_oki1: tipo_roms = (n: 'gl-06.rom'; l: $20000; p: 0; crc: $D92E519D);
  cninja_sprites: array [0 .. 3] of tipo_roms = ((n: 'mag-03.rom'; l: $80000; p: 0; crc: $2220EB9F), (n: 'mag-05.rom'; l: $80000; p: $1; crc: $56A53254), (n: 'mag-04.rom'; l: $80000; p: $100000; crc: $144B94CC), (n: 'mag-06.rom'; l: $80000; p: $100001; crc: $82D44749));
  cninja_dip: array [0 .. 7] of def_dip2 = ((mask: $7; name: 'Coin A'; number: 8; val8: (0, 1, 7, 6, 5, 4, 3, 2); name8: ('3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 5C', '1C 6C')), (mask: $38; name: 'Coin B'; number: 8; val8: (0, 8, $38, $30, $28, $20, $18, $10);
    name8: ('3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 5C', '1C 6C')), (mask: $40; name: 'Flip Screen'; number: 2; val2: ($40, 0); name2: ('Off', 'On')), (mask: $300; name: 'Lives'; number: 4; val4: ($100, 0, $300, $200); name4: ('1', '2', '3', '4')), (mask: $C00;
    name: 'Difficulty'; number: 4; val4: ($800, $C00, $400, 0); name4: ('Easy', 'Normal', 'Hard', 'Very Hard')), (mask: $1000; name: 'Restore Live Meter'; number: 2; val2: ($1000, 0); name2: ('Off', 'On')), (mask: $8000; name: 'Demo Sounds'; number: 2; val2: ($8000, 0);
    name2: ('Off', 'On')), ());
  // Robocop 2
  robocop2_rom: array [0 .. 7] of tipo_roms = ((n: 'gq-03.k1'; l: $20000; p: 0; crc: $A7E90C28), (n: 'gq-07.k3'; l: $20000; p: $1; crc: $D2287EC1), (n: 'gq-02.j1'; l: $20000; p: $40000; crc: $6777B8A0), (n: 'gq-06.j3'; l: $20000; p: $40001; crc: $E11E27B5), (n: 'go-01-1.h1';
    l: $20000; p: $80000; crc: $AB5356C0), (n: 'go-05-1.h3'; l: $20000; p: $80001; crc: $CE21BDA5), (n: 'go-00.f1'; l: $20000; p: $C0000; crc: $A93369EA), (n: 'go-04.f3'; l: $20000; p: $C0001; crc: $EE2F6AD9));
  robocop2_char: array [0 .. 1] of tipo_roms = ((n: 'gp10-1.y6'; l: $10000; p: 1; crc: $D25D719C), (n: 'gp11-1.z6'; l: $10000; p: 0; crc: $030DED47));
  robocop2_sound: tipo_roms = (n: 'gp-09.k13'; l: $10000; p: $0; crc: $4A4E0F8D);
  robocop2_oki1: tipo_roms = (n: 'gp-08.j13'; l: $20000; p: 0; crc: $365183B1);
  robocop2_oki2: tipo_roms = (n: 'mah-11.f13'; l: $80000; p: 0; crc: $642BC692);
  robocop2_tiles1: array [0 .. 1] of tipo_roms = ((n: 'mah-04.z4'; l: $80000; p: $0; crc: $9B6CA18C), (n: 'mah-03.y4'; l: $80000; p: $80000; crc: $37894DDC));
  robocop2_tiles2: array [0 .. 2] of tipo_roms = ((n: 'mah-01.z1'; l: $80000; p: 0; crc: $26E0DFFF), (n: 'mah-00.y1'; l: $80000; p: $80000; crc: $7BD69E41), (n: 'mah-02.a1'; l: $80000; p: $100000; crc: $328A247D));
  robocop2_sprites: array [0 .. 5] of tipo_roms = ((n: 'mah-05.y9'; l: $80000; p: $000000; crc: $6773E613), (n: 'mah-08.y12'; l: $80000; p: $000001; crc: $88D310A5), (n: 'mah-06.z9'; l: $80000; p: $100000; crc: $27A8808A), (n: 'mah-09.z12'; l: $80000; p: $100001; crc: $A58C43A7),
    (n: 'mah-07.a9'; l: $80000; p: $200000; crc: $526F4190), (n: 'mah-10.a12'; l: $80000; p: $200001; crc: $14B770DA));
  robocop2_dip_a: array [0 .. 8] of def_dip2 = ((mask: $7; name: 'Coin A'; number: 8; val8: (0, 1, 7, 6, 5, 4, 3, 2); name8: ('3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 5C', '1C 6C')), (mask: $38; name: 'Coin B'; number: 8;
    val8: (0, 8, $38, $30, $28, $20, $18, $10); name8: ('3C 1C', '2C 1C', '1C 1C', '1C 2C', '1C 3C', '1C 4C', '1C 5C', '1C 6C')), (mask: $40; name: 'Flip Screen'; number: 2; val2: (0, $40); name2: ('Off', 'On')), (mask: $300; name: 'Lives'; number: 4; val4: ($100, 0, $300, $200);
    name4: ('1', '2', '3', '4')), (mask: $C00; name: 'Time'; number: 4; val4: ($800, $C00, $400, 0); name4: ('400 Seconds', '300 Seconds', '200 Seconds', '100 Seconds')), (mask: $3000; name: 'Health'; number: 4; val4: (0, $1000, $3000, $2000); name4: ('17', '24', '33', '40')),
    (mask: $4000; name: 'Continues'; number: 2; val2: (0, $4000); name2: ('Off', 'On')), (mask: $8000; name: 'Demo Sounds'; number: 2; val2: ($8000, 0); name2: ('Off', 'On')), ());
  robocop2_dip_b: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Bullets'; number: 4; val4: (0, 1, 3, 2); name4: ('Least', 'Less', 'Normal', 'More')), (mask: $C; name: 'Enemy Movement'; number: 4; val4: (8, $C, 4, 0); name4: ('Slow', 'Normal', 'Fast', 'Fastest')), (mask: $20;
    name: 'Enemy Strength'; number: 4; val4: ($20, $30, $10, 0); name4: ('Less', 'Normal', 'More', 'Most')), (mask: $40; name: 'Enemy Weapon Speed'; number: 2; val2: ($40, 0); name2: ('Normal', 'Fast')), (mask: $80; name: 'Game Over Message'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());

var
  rom: array [0 .. $7FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  irq_mask, irq_line: byte;
  screen_line, prioridad: word;
  raster_irq: boolean;
  proc_update_video: procedure;

procedure update_video_cninja;
begin
  deco16ic_1.update_pf_2(5, false);
  deco_sprites_0.draw_sprites($80);
  deco16ic_1.update_pf_1(5, true);
  deco_sprites_0.draw_sprites($40);
  deco16ic_0.update_pf_2(5, true);
  deco_sprites_0.draw_sprites($00);
  deco16ic_0.update_pf_1(5, true);
  update_final_piece(0, 8, 256, 240, 5);
end;

procedure update_video_robocop2;
begin
  if (prioridad and 4) = 0 then
    deco16ic_1.update_pf_2(5, false)
  else
  begin
    deco_sprites_0.draw_sprites($C0);
    fill_full_screen(5, $200);
  end;
  deco_sprites_0.draw_sprites($80);
  if (prioridad and $8) <> 0 then
  begin
    deco16ic_0.update_pf_2(5, true);
    deco_sprites_0.draw_sprites($40);
    deco16ic_1.update_pf_1(5, true);
  end
  else
  begin
    deco16ic_1.update_pf_1(5, (prioridad and 4) = 0);
    deco_sprites_0.draw_sprites($40);
    deco16ic_0.update_pf_2(5, true);
  end;
  deco_sprites_0.draw_sprites($00);
  deco16ic_0.update_pf_1(5, true);
  update_final_piece(0, 8, 320, 240, 5);
end;

procedure events_cninja;
begin
  if event.arcade then
  begin
    // P1
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
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
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
    if p_contrls.map_arcade.but1[1] then
      marcade.in0 := (marcade.in0 and $BFFF)
    else
      marcade.in0 := (marcade.in0 or $4000);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7FFF)
    else
      marcade.in0 := (marcade.in0 or $8000);
    // SYSTEM
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

procedure cninja_loop;
var
  frame_m, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := h6280_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for screen_line := 0 to 273 do
      begin
        case screen_line of
          0 .. 240:
            begin
              if raster_irq then
              begin
                if irq_line = screen_line then
                begin
                  if (irq_mask and $10) <> 0 then
                    m68000_0.irq[3] := ASSERT_LINE
                  else
                    m68000_0.irq[4] := ASSERT_LINE;
                  raster_irq := false;
                end;
              end;
              if screen_line = 8 then
                marcade.in1 := marcade.in1 and $F7;
            end;
          248:
            begin
              m68000_0.irq[5] := HOLD_LINE;
              proc_update_video;
              marcade.in1 := marcade.in1 or $8;
            end;
        end;
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        h6280_0.run(trunc(frame_s));
        frame_s := frame_s + h6280_0.tframes - h6280_0.contador;
      end;
      events_cninja;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function cninja_protection_deco_104_r(real_address: word): word;
var
  data, deco104_addr: word;
  cs: byte;
begin
  // int real_address = 0 + (offset *2);
  deco104_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
  cs := 0;
  data := deco104_0.read_data(deco104_addr, cs);
  cninja_protection_deco_104_r := data;
end;

function cninja_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $BFFFF:
      cninja_getword := rom[direccion shr 1];
    $144000 .. $144FFF:
      cninja_getword := deco16ic_0.pf1.data[(direccion and $FFF) shr 1];
    $146000 .. $146FFF:
      cninja_getword := deco16ic_0.pf2.data[(direccion and $FFF) shr 1];
    $14C000 .. $14C7FF:
      cninja_getword := deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1];
    $14E000 .. $14E7FF:
      cninja_getword := deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1];
    $154000 .. $154FFF:
      cninja_getword := deco16ic_1.pf1.data[(direccion and $FFF) shr 1];
    $156000 .. $156FFF:
      cninja_getword := deco16ic_1.pf2.data[(direccion and $FFF) shr 1];
    $15C000 .. $15C7FF:
      cninja_getword := deco16ic_1.pf1.rowscroll[(direccion and $7FF) shr 1];
    $15E000 .. $15E7FF:
      cninja_getword := deco16ic_1.pf2.rowscroll[(direccion and $7FF) shr 1];
    $184000 .. $187FFF:
      cninja_getword := ram[(direccion and $3FFF) shr 1];
    $190000 .. $190007:
      case ((direccion shr 1) and $7) of
        1:
          cninja_getword := screen_line; // Raster IRQ scanline position
        2:
          begin // Raster IRQ ACK
            m68000_0.irq[3] := CLEAR_LINE;
            m68000_0.irq[4] := CLEAR_LINE;
            cninja_getword := 0;
          end;
      else
        cninja_getword := 0;
      end;
    $19C000 .. $19DFFF:
      cninja_getword := buffer_paleta[(direccion and $1FFF) shr 1];
    $1A4000 .. $1A47FF:
      cninja_getword := buffer_sprites_w[(direccion and $7FF) shr 1];
    $1BC000 .. $1BFFFF:
      cninja_getword := cninja_protection_deco_104_r(direccion - $1BC000);
  end;
end;

procedure change_color(numero: word);
var
  color: tcolor;
begin
  color.b := buffer_paleta[numero shl 1] and $FF;
  color.g := buffer_paleta[(numero shl 1) + 1] shr 8;
  color.r := buffer_paleta[(numero shl 1) + 1] and $FF;
  set_pal_color(color, numero);
  case numero of
    $000 .. $0FF:
      deco16ic_0.pf1.buffer_color[(numero shr 4) and $F] := true;
    $100 .. $1FF:
      deco16ic_0.pf2.buffer_color[(numero shr 4) and $F] := true;
    $200 .. $2FF:
      deco16ic_1.pf1.buffer_color[(numero shr 4) and $F] := true;
    $500 .. $5FF:
      deco16ic_1.pf2.buffer_color[(numero shr 4) and deco16ic_1.color_mask[2]] := true;
  end;
end;

procedure cninja_protection_deco_104_w(real_address, data: word);
var
  deco104_addr: word;
  cs: byte;
begin
  // int real_address = 0 + (offset *2);
  deco104_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
  cs := 0;
  deco104_0.write_data(deco104_addr, data, cs);
end;

procedure cninja_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $BFFFF:
      ; // ROM
    $140000 .. $14000F:
      deco16ic_0.control_w((direccion and $F) shr 1, valor);
    $144000 .. $144FFF:
      if deco16ic_0.pf1.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_0.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $146000 .. $146FFF:
      if deco16ic_0.pf2.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_0.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $14C000 .. $14C7FF:
      deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $14E000 .. $14E7FF:
      deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
    $150000 .. $15000F:
      begin
        deco16ic_1.control_w((direccion and $F) shr 1, valor);
        if ((direccion and $F) = 0) then
          main_screen.flip_main_screen := (valor and $0080) <> 0
      end;
    $154000 .. $154FFF:
      if deco16ic_1.pf1.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_1.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_1.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $156000 .. $156FFF:
      if deco16ic_1.pf2.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_1.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_1.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $15C000 .. $15C7FF:
      deco16ic_1.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $15E000 .. $15E7FF:
      deco16ic_1.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
    $184000 .. $187FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $190000 .. $190007:
      case ((direccion shr 1) and 7) of
        0:
          irq_mask := valor and $FF; // IRQ enable:
        1:
          begin // Raster IRQ scanline position, only valid for values between 1 & 239 (0 and 240-256 do NOT generate IRQ's)
            irq_line := valor and $FF;
            raster_irq := (irq_line > 0) and (irq_line < 240) and ((irq_mask and $2) = 0);
          end;
      end;
    $19C000 .. $19DFFF:
      if (buffer_paleta[(direccion and $1FFF) shr 1] <> valor) then
      begin
        buffer_paleta[(direccion and $1FFF) shr 1] := valor;
        change_color((direccion and $1FFF) shr 2);
      end;
    $1A4000 .. $1A47FF:
      buffer_sprites_w[(direccion and $7FF) shr 1] := valor;
    $1B4000 .. $1B4001:
      copymemory(@deco_sprites_0.ram[0], @buffer_sprites_w[0], $400 * 2);
    $1B0002 .. $1B000F:
      ;
    $1BC000 .. $1BFFFF:
      cninja_protection_deco_104_w(direccion - $1BC000, valor);
  end;
end;

// Roboop 2
function robocop2_protection_deco_146_r(real_address: word): word;
var
  deco146_addr, data: word;
  cs: byte;
begin
  // int real_address = 0 + (offset *2);
  deco146_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
  cs := 0;
  data := deco146_0.read_data(deco146_addr, cs);
  robocop2_protection_deco_146_r := data;
end;

function robocop2_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $FFFFF:
      robocop2_getword := rom[direccion shr 1];
    $144000 .. $144FFF:
      robocop2_getword := deco16ic_0.pf1.data[(direccion and $FFF) shr 1];
    $146000 .. $146FFF:
      robocop2_getword := deco16ic_0.pf2.data[(direccion and $FFF) shr 1];
    $14C000 .. $14C7FF:
      robocop2_getword := deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1];
    $14E000 .. $14E7FF:
      robocop2_getword := deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1];
    $154000 .. $154FFF:
      robocop2_getword := deco16ic_1.pf1.data[(direccion and $FFF) shr 1];
    $156000 .. $156FFF:
      robocop2_getword := deco16ic_1.pf2.data[(direccion and $FFF) shr 1];
    $15C000 .. $15C7FF:
      robocop2_getword := deco16ic_1.pf1.rowscroll[(direccion and $7FF) shr 1];
    $15E000 .. $15E7FF:
      robocop2_getword := deco16ic_1.pf2.rowscroll[(direccion and $7FF) shr 1];
    $180000 .. $1807FF:
      robocop2_getword := buffer_sprites_w[(direccion and $7FF) shr 1];
    $18C000 .. $18FFFF:
      robocop2_getword := robocop2_protection_deco_146_r(direccion - $18C000);
    $1A8000 .. $1A9FFF:
      robocop2_getword := buffer_paleta[(direccion and $1FFF) shr 1];
    $1B0000 .. $1B0007:
      case ((direccion shr 1) and $7) of
        1:
          robocop2_getword := screen_line; // Raster IRQ scanline position
        2:
          begin // Raster IRQ ACK
            m68000_0.irq[3] := CLEAR_LINE;
            m68000_0.irq[4] := CLEAR_LINE;
            robocop2_getword := 0;
          end;
      else
        robocop2_getword := 0;
      end;
    $1B8000 .. $1BBFFF:
      robocop2_getword := ram[(direccion and $3FFF) shr 1];
    $1F8000 .. $1F8001:
      robocop2_getword := marcade.dswb;
  end;
end;

procedure robocop2_protection_deco_146_w(real_address, data: word);
var
  deco146_addr: word;
  cs: byte;
begin
  // int real_address = 0 + (offset *2);
  deco146_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
  cs := 0;
  deco146_0.write_data(deco146_addr, data, cs);
end;

procedure robocop2_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $140000 .. $14000F:
      deco16ic_0.control_w((direccion and $F) shr 1, valor);
    $144000 .. $144FFF:
      if deco16ic_0.pf1.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_0.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $146000 .. $146FFF:
      if deco16ic_0.pf2.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_0.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_0.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $14C000 .. $14C7FF:
      deco16ic_0.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $14E000 .. $14E7FF:
      deco16ic_0.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
    $150000 .. $15000F:
      begin
        deco16ic_1.control_w((direccion and $F) shr 1, valor);
        if ((direccion and $F) = 0) then
          main_screen.flip_main_screen := (valor and $0080) <> 0
      end;
    $154000 .. $154FFF:
      if deco16ic_1.pf1.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_1.pf1.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_1.pf1.buffer[(direccion and $FFF) shr 1] := true
      end;
    $156000 .. $156FFF:
      if deco16ic_1.pf2.data[(direccion and $FFF) shr 1] <> valor then
      begin
        deco16ic_1.pf2.data[(direccion and $FFF) shr 1] := valor;
        deco16ic_1.pf2.buffer[(direccion and $FFF) shr 1] := true
      end;
    $15C000 .. $15C7FF:
      deco16ic_1.pf1.rowscroll[(direccion and $7FF) shr 1] := valor;
    $15E000 .. $15E7FF:
      deco16ic_1.pf2.rowscroll[(direccion and $7FF) shr 1] := valor;
    $180000 .. $1807FF:
      buffer_sprites_w[(direccion and $7FF) shr 1] := valor;
    $18C000 .. $18FFFF:
      robocop2_protection_deco_146_w(direccion - $18C000, valor);
    $198000 .. $198001:
      copymemory(@deco_sprites_0.ram[0], @buffer_sprites_w[0], $400 * 2);
    $1A0002 .. $1A00FF:
      ;
    $1A8000 .. $1A9FFF:
      if buffer_paleta[(direccion and $1FFF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $1FFF) shr 1] := valor;
        change_color((direccion and $1FFF) shr 2);
      end;
    $1B0000 .. $1B0007:
      case ((direccion shr 1) and 7) of
        0:
          irq_mask := valor and $FF; // IRQ enable:
        1:
          begin // Raster IRQ scanline position, only valid for values between 1 & 239 (0 and 240-256 do NOT generate IRQ's)
            irq_line := valor and $FF;
            raster_irq := (irq_line > 0) and (irq_line < 240) and ((irq_mask and $2) = 0);
          end;
      end;
    $1B8000 .. $1BBFFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $1F0000 .. $1F0001:
      if prioridad <> valor then
      begin
        prioridad := valor;
        if (prioridad and 4) <> 0 then
        begin
          deco16ic_1.gfx_plane[2] := 4;
          deco16ic_1.color_mask[2] := 0;
        end
        else
        begin
          deco16ic_1.gfx_plane[2] := 2;
          deco16ic_1.color_mask[2] := $F;
        end;
        fillchar(deco16ic_1.pf1.buffer[0], $800, 1);
        fillchar(deco16ic_1.pf2.buffer[0], $800, 1);
      end;
  end;
end;

procedure sound_bank_rom(valor: byte);
begin
  copymemory(oki_6295_1.get_rom_addr, @oki_rom[valor and 1], $40000);
end;

function cninja_video_bank(bank: word): word;
begin
  if ((bank shr 4) and $F) <> 0 then
    cninja_video_bank := $0 // Only 2 banks
  else
    cninja_video_bank := $1000;
end;

function robocop2_video_bank(bank: word): word;
begin
  robocop2_video_bank := (bank and $30) shl 8;
end;

// Main
procedure reset_cninja;
begin
  m68000_0.reset;
  deco16ic_0.reset;
  deco16ic_1.reset;
  deco_sprites_0.reset;
  case main_vars.machine_type of
    162:
      deco104_0.reset;
    163:
      deco146_0.reset;
  end;
  deco16_snd_double_reset;
  copymemory(oki_6295_1.get_rom_addr, @oki_rom[0], $40000);
 reset_game_general;
  marcade.in0 := $FFFF;
  marcade.in1 := $F7;
  irq_mask := 0;
  raster_irq := false;
end;

function start_cavemanninja: boolean;
const
  pt_x: array [0 .. 15] of dword = (32 * 8 + 0, 32 * 8 + 1, 32 * 8 + 2, 32 * 8 + 3, 32 * 8 + 4, 32 * 8 + 5, 32 * 8 + 6, 32 * 8 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  pt_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  ps_x: array [0 .. 15] of dword = (64 * 8 + 0, 64 * 8 + 1, 64 * 8 + 2, 64 * 8 + 3, 64 * 8 + 4, 64 * 8 + 5, 64 * 8 + 6, 64 * 8 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
var
  memory_temp, memory_temp2, ptemp, ptemp2: pbyte;
  tempw: word;
  procedure cninja_convert_chars(num: word);
  begin
    init_gfx(0, 8, 8, num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 16 * 8, num * 16 * 8 + 8, num * 16 * 8 + 0, 8, 0);
    convert_gfx(0, 0, memory_temp, @pt_x[8], @pt_y, false, false);
  end;
procedure cninja_convert_tiles(ngfx:byte;num:word);
  begin
  init_gfx(ngfx,16,16,num);
    gfx[ngfx].trans[0] := true;
    gfx_set_desc_data(4, 0, 64 * 8, num * 64 * 8 + 8, num * 64 * 8 + 0, 8, 0);
    convert_gfx(ngfx, 0, memory_temp2, @pt_x, @pt_y, false, false);
  end;
procedure cninja_convert_sprites(num:dword);
  begin
  init_gfx(3,16,16,num);
    gfx[3].trans[0] := true;
    gfx_set_desc_data(4, 0, 128 * 8, 16, 0, 24, 8);
    convert_gfx(3, 0, memory_temp, @ps_x, @ps_y, false, false);
  end;

begin
  start_cavemanninja := false;
  machine_calls.general_loop := cninja_loop;
  machine_calls.reset := reset_cninja;
  machine_calls.fps_max := 58;
  start_audio(false);
  case main_vars.machine_type of
    162:
      begin
        tempw := 256;
        deco16ic_0 := chip_16ic.create(1, 2, $000, $000, $F, $F, 0, 1, 0, 16, nil, nil);
        deco16ic_1 := chip_16ic.create(3, 4, $000, $200, $F, $F, 0, 2, 0, 48, cninja_video_bank, cninja_video_bank);
        deco_sprites_0 := tdeco16_sprite.create(3, 5, 240, $300, $3FFF);
      end;
    163:
      begin
        tempw := 320;
        deco16ic_0 := chip_16ic.create(1, 2, $000, $000, $F, $F, 0, 1, 0, 16, nil, robocop2_video_bank);
        deco16ic_1 := chip_16ic.create(3, 4, $000, $200, $F, $F, 0, 2, 0, 48, robocop2_video_bank, robocop2_video_bank);
        deco_sprites_0 := tdeco16_sprite.create(3, 5, 304, $300, $7FFF);
      end;
  end;
  screen_init(5, 512, 512, false, true);
  start_video(tempw, 240);
  // Sound CPU
  deco16_snd_double_init(32220000 div 8, 32220000, sound_bank_rom, 274);
  getmem(memory_temp, $300000);
  case main_vars.machine_type of
    162:
      begin // Caveman Ninja
        // Main CPU
        m68000_0 := cpu_m68000.create(12000000, 274);
        m68000_0.change_ram16_calls(cninja_getword, cninja_putword);
        proc_update_video := update_video_cninja;
        // cargar roms
        if not(roms_load16w(@rom, cninja_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, cninja_sound)) then
          exit;
        // OKIs rom
        if not(roms_load(oki_6295_0.get_rom_addr, cninja_oki1)) then
          exit;
        if not(roms_load(memory_temp, cninja_oki2)) then
          exit;
        ptemp := memory_temp;
        copymemory(@oki_rom[0], ptemp, $40000);
        inc(ptemp, $40000);
        copymemory(@oki_rom[1], ptemp, $40000);
        // convertir chars
        if not(roms_load16b(memory_temp, cninja_chars)) then
          exit;
        cninja_convert_chars($1000);
        // Tiles
        getmem(memory_temp2, $100000);
        if not(roms_load(memory_temp2, cninja_tiles1)) then
          exit;
        cninja_convert_tiles(1,$1000);
        if not(roms_load(memory_temp, cninja_tiles2)) then
          exit;
        // ordenar
        ptemp := memory_temp2;
        ptemp2 := memory_temp;
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        inc(ptemp, $80000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        ptemp := memory_temp2;
        inc(ptemp, $40000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        inc(ptemp, $80000);
        copymemory(ptemp, ptemp2, $40000);
        cninja_convert_tiles(2,$2000);
        freemem(memory_temp2);
        // Sprites
        if not(roms_load16b(memory_temp, cninja_sprites)) then
          exit;
        cninja_convert_sprites($4000);
        // Proteccion deco104
        deco104_0 := cpu_deco_104.create(USE_MAGIC_ADDRESS_XOR);
        // Dip
        marcade.dswa := $7FFF;
        marcade.dswa_val2 := @cninja_dip;
      end;
    163:
      begin // Robocop 2
        // Main CPU
        m68000_0 := cpu_m68000.create(14000000, 274);
        m68000_0.change_ram16_calls(robocop2_getword, robocop2_putword);
        proc_update_video := update_video_robocop2;
        // cargar roms
        if not(roms_load16w(@rom, robocop2_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, robocop2_sound)) then
          exit;
        // OKIs rom
        if not(roms_load(oki_6295_0.get_rom_addr, robocop2_oki1)) then
          exit;
        if not(roms_load(memory_temp, robocop2_oki2)) then
          exit;
        ptemp := memory_temp;
        copymemory(@oki_rom[0], ptemp, $40000);
        inc(ptemp, $40000);
        copymemory(@oki_rom[1], ptemp, $40000);
        // convertir chars
        if not(roms_load16b(memory_temp, robocop2_char)) then
          exit;
        cninja_convert_chars($1000);
        // Tiles
        if not(roms_load(memory_temp, robocop2_tiles1)) then
          exit;
        getmem(memory_temp2, $180000);
        ptemp := memory_temp2;
        ptemp2 := memory_temp;
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        inc(ptemp, $80000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        ptemp := memory_temp2;
        inc(ptemp, $40000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        inc(ptemp, $80000);
        copymemory(ptemp, ptemp2, $40000);
        cninja_convert_tiles(1,$2000);
        // Tiles 2
        if not(roms_load(memory_temp, robocop2_tiles2)) then
          exit;
        ptemp := memory_temp2;
        ptemp2 := memory_temp;
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp, $C0000);
        inc(ptemp2, $40000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        ptemp := memory_temp2;
        inc(ptemp, $40000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        inc(ptemp, $C0000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        ptemp := memory_temp2;
        inc(ptemp, $80000);
        copymemory(ptemp, ptemp2, $40000);
        inc(ptemp2, $40000);
        inc(ptemp, $C0000);
        copymemory(ptemp, ptemp2, $40000);
        cninja_convert_tiles(2,$3000);
        // Tiles 8bbp
        init_gfx(4, 16, 16, $1000);
        gfx[4].trans[0] := true;
        gfx_set_desc_data(8, 0, 64 * 8, $100000 * 8 + 8, $100000 * 8, $40000 * 8 + 8, $40000 * 8, $C0000 * 8 + 8, $C0000 * 8, 8, 0);
        convert_gfx(4, 0, memory_temp2, @pt_x, @pt_y, false, false);
        freemem(memory_temp2);
        // Sprites
        if not(roms_load16b(memory_temp, robocop2_sprites)) then
          exit;
        cninja_convert_sprites($6000);
        // Proteccion deco146
        deco146_0 := cpu_deco_146.create(USE_MAGIC_ADDRESS_XOR);
        // Dip
        marcade.dswa := $7FBF;
        marcade.dswa_val2 := @robocop2_dip_a;
        marcade.dswb := $FF;
        marcade.dswb_val2 := @robocop2_dip_b;
      end;
  end;
  // final
  freemem(memory_temp);
  reset_cninja;
  start_cavemanninja := true;
end;

end.
