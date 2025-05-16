unit mcr_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  z80daisy,
  z80ctc,
  timer_engine,
  file_engine;

function start_mcr: boolean;

implementation
uses
  uDataModule;

const
  tapper_rom: array [0 .. 3] of tipo_roms = ((n: 'tapper_c.p.u._pg_0_1c_1-27-84.1c'; l: $4000; p: 0; crc: $BB060BB0), (n: 'tapper_c.p.u._pg_1_2c_1-27-84.2c'; l: $4000; p: $4000; crc: $FD9ACC22), (n: 'tapper_c.p.u._pg_2_3c_1-27-84.3c'; l: $4000; p: $8000; crc: $B3755D41),
    (n: 'tapper_c.p.u._pg_3_4c_1-27-84.4c'; l: $2000; p: $C000; crc: $77273096));
  tapper_snd: array [0 .. 3] of tipo_roms = ((n: 'tapper_sound_snd_0_a7_12-7-83.a7'; l: $1000; p: 0; crc: $0E8BB9D5), (n: 'tapper_sound_snd_1_a8_12-7-83.a8'; l: $1000; p: $1000; crc: $0CF0E29B), (n: 'tapper_sound_snd_2_a9_12-7-83.a9'; l: $1000; p: $2000; crc: $31EB6DC6),
    (n: 'tapper_sound_snd_3_a10_12-7-83.a10'; l: $1000; p: $3000; crc: $01A9BE6A));
  tapper_char: array [0 .. 1] of tipo_roms = ((n: 'tapper_c.p.u._bg_1_6f_12-7-83.6f'; l: $4000; p: 0; crc: $2A30238C), (n: 'tapper_c.p.u._bg_0_5f_12-7-83.5f'; l: $4000; p: $4000; crc: $394AB576));
  tapper_sprites: array [0 .. 7] of tipo_roms = ((n: 'tapper_video_fg_1_a7_12-7-83.a7'; l: $4000; p: 0; crc: $32509011), (n: 'tapper_video_fg_0_a8_12-7-83.a8'; l: $4000; p: $4000; crc: $8412C808), (n: 'tapper_video_fg_3_a5_12-7-83.a5'; l: $4000; p: $8000; crc: $818FFFD4),
    (n: 'tapper_video_fg_2_a6_12-7-83.a6'; l: $4000; p: $C000; crc: $67E37690), (n: 'tapper_video_fg_5_a3_12-7-83.a3'; l: $4000; p: $10000; crc: $800F7C8A), (n: 'tapper_video_fg_4_a4_12-7-83.a4'; l: $4000; p: $14000; crc: $32674EE6), (n: 'tapper_video_fg_7_a1_12-7-83.a1';
    l: $4000; p: $18000; crc: $070B4C81), (n: 'tapper_video_fg_6_a2_12-7-83.a2'; l: $4000; p: $1C000; crc: $A37AEF36));
  dotron_rom: array [0 .. 3] of tipo_roms = ((n: 'disc_tron_uprt_pg0_10-4-83.1c'; l: $4000; p: 0; crc: $40D00195), (n: 'disc_tron_uprt_pg1_10-4-83.2c'; l: $4000; p: $4000; crc: $5A7D1300), (n: 'disc_tron_uprt_pg2_10-4-83.3c'; l: $4000; p: $8000; crc: $CB89C9BE),
    (n: 'disc_tron_uprt_pg3_10-4-83.4c'; l: $2000; p: $C000; crc: $5098FAF4));
  dotron_snd: array [0 .. 3] of tipo_roms = ((n: 'disc_tron_uprt_snd0_10-4-83.a7'; l: $1000; p: 0; crc: $7FB54293), (n: 'disc_tron_uprt_snd1_10-4-83.a8'; l: $1000; p: $1000; crc: $EDEF7326), (n: 'disc_tron_uprt_snd2_9-22-83.a9'; l: $1000; p: $2000; crc: $E8EF6519),
    (n: 'disc_tron_uprt_snd3_9-22-83.a10'; l: $1000; p: $3000; crc: $6B5AEB02));
  dotron_char: array [0 .. 1] of tipo_roms = ((n: 'loc-bg2.6f'; l: $2000; p: 0; crc: $40167124), (n: 'loc-bg1.5f'; l: $2000; p: $2000; crc: $BB2D7A5D));
  dotron_sprites: array [0 .. 7] of tipo_roms = ((n: 'loc-g.cp4'; l: $2000; p: 0; crc: $57A2B1FF), (n: 'loc-h.cp3'; l: $2000; p: $2000; crc: $3BB4D475), (n: 'loc-e.cp6'; l: $2000; p: $4000; crc: $CE957F1A), (n: 'loc-f.cp5'; l: $2000; p: $6000; crc: $D26053CE), (n: 'loc-c.cp8';
    l: $2000; p: $8000; crc: $EF45D146), (n: 'loc-d.cp7'; l: $2000; p: $A000; crc: $5E8A3EF3), (n: 'loc-a.cp0'; l: $2000; p: $C000; crc: $B35F5374), (n: 'loc-b.cp9'; l: $2000; p: $E000; crc: $565A5C48));
  tron_rom: array [0 .. 5] of tipo_roms = ((n: 'scpu-pga_lctn-c2_tron_aug_9.c2'; l: $2000; p: 0; crc: $0DE0471A), (n: 'scpu-pgb_lctn-c3_tron_aug_9.c3'; l: $2000; p: $2000; crc: $8DDF8717), (n: 'scpu-pgc_lctn-c4_tron_aug_9.c4'; l: $2000; p: $4000; crc: $4241E3A0),
    (n: 'scpu-pgd_lctn-c5_tron_aug_9.c5'; l: $2000; p: $6000; crc: $035D2FE7), (n: 'scpu-pge_lctn-c6_tron_aug_9.c6'; l: $2000; p: $8000; crc: $24C185D8), (n: 'scpu-pgf_lctn-c7_tron_aug_9.c7'; l: $2000; p: $A000; crc: $38C4BBAF));
  tron_snd: array [0 .. 2] of tipo_roms = ((n: 'ssi-0a_lctn-a7_tron.a7'; l: $1000; p: 0; crc: $765E6EBA), (n: 'ssi-0b_lctn-a8_tron.a8'; l: $1000; p: $1000; crc: $1B90CCDD), (n: 'ssi-0c_lctn-a9_tron.a9'; l: $1000; p: $2000; crc: $3A4BC629));
  tron_char: array [0 .. 1] of tipo_roms = ((n: 'scpu-bgg_lctn-g3_tron.g3'; l: $2000; p: 0; crc: $1A9ED2F5), (n: 'lscpu-bgh_lctn-g4_tron.g4'; l: $2000; p: $2000; crc: $3220F974));
  tron_sprites: array [0 .. 3] of tipo_roms = ((n: 'vga_lctn-e1_tron.e1'; l: $2000; p: 0; crc: $BC036D1D), (n: 'vgb_lctn-dc1_tron.dc1'; l: $2000; p: $2000; crc: $58EE14D3), (n: 'vgc_lctn-cb1_tron.cb1'; l: $2000; p: $4000; crc: $3329F9D4), (n: 'vgd_lctn-a1_tron.a1'; l: $2000;
    p: $6000; crc: $9743F873));
  timber_rom: array [0 .. 3] of tipo_roms = ((n: 'timpg0.bin'; l: $4000; p: 0; crc: $377032AB), (n: 'timpg1.bin'; l: $4000; p: $4000; crc: $FD772836), (n: 'timpg2.bin'; l: $4000; p: $8000; crc: $632989F9), (n: 'timpg3.bin'; l: $2000; p: $C000; crc: $DAE8A0DC));
  timber_snd: array [0 .. 2] of tipo_roms = ((n: 'tima7.bin'; l: $1000; p: 0; crc: $C615DC3E), (n: 'tima8.bin'; l: $1000; p: $1000; crc: $83841C87), (n: 'tima9.bin'; l: $1000; p: $2000; crc: $22BCDCD3));
  timber_char: array [0 .. 1] of tipo_roms = ((n: 'timbg1.bin'; l: $4000; p: 0; crc: $B1CB2651), (n: 'timbg0.bin'; l: $4000; p: $4000; crc: $2AE352C4));
  timber_sprites: array [0 .. 7] of tipo_roms = ((n: 'timfg1.bin'; l: $4000; p: 0; crc: $81DE4A73), (n: 'timfg0.bin'; l: $4000; p: $4000; crc: $7F3A4F59), (n: 'timfg3.bin'; l: $4000; p: $8000; crc: $37C03272), (n: 'timfg2.bin'; l: $4000; p: $C000; crc: $E2C2885C),
    (n: 'timfg5.bin'; l: $4000; p: $10000; crc: $EB636216), (n: 'timfg4.bin'; l: $4000; p: $14000; crc: $B7105EB7), (n: 'timfg7.bin'; l: $4000; p: $18000; crc: $D9C27475), (n: 'timfg6.bin'; l: $4000; p: $1C000; crc: $244778E8));
  shollow_rom: array [0 .. 5] of tipo_roms = ((n: 'sh-pro.00'; l: $2000; p: 0; crc: $95E2B800), (n: 'sh-pro.01'; l: $2000; p: $2000; crc: $B99F6FF8), (n: 'sh-pro.02'; l: $2000; p: $4000; crc: $1202C7B2), (n: 'sh-pro.03'; l: $2000; p: $6000; crc: $0A64AFB9), (n: 'sh-pro.04';
    l: $2000; p: $8000; crc: $22FA9175), (n: 'sh-pro.05'; l: $2000; p: $A000; crc: $1716E2BB));
  shollow_snd: array [0 .. 2] of tipo_roms = ((n: 'sh-snd.01'; l: $1000; p: 0; crc: $55A297CC), (n: 'sh-snd.02'; l: $1000; p: $1000; crc: $46FC31F6), (n: 'sh-snd.03'; l: $1000; p: $2000; crc: $B1F4A6A8));
  shollow_char: array [0 .. 1] of tipo_roms = ((n: 'sh-bg.00'; l: $2000; p: 0; crc: $3E2B333C), (n: 'sh-bg.01'; l: $2000; p: $2000; crc: $D1D70CC4));
  shollow_sprites: array [0 .. 3] of tipo_roms = ((n: 'sh-fg.00'; l: $2000; p: 0; crc: $33F4554E), (n: 'sh-fg.01'; l: $2000; p: $2000; crc: $BA1A38B4), (n: 'sh-fg.02'; l: $2000; p: $4000; crc: $6B57F6DA), (n: 'sh-fg.03'; l: $2000; p: $6000; crc: $37EA9D07));
  domino_rom: array [0 .. 3] of tipo_roms = ((n: 'dmanpg0.bin'; l: $2000; p: 0; crc: $3BF3BB1C), (n: 'dmanpg1.bin'; l: $2000; p: $2000; crc: $85CF1D69), (n: 'dmanpg2.bin'; l: $2000; p: $4000; crc: $7DD2177A), (n: 'dmanpg3.bin'; l: $2000; p: $6000; crc: $F2E0AA44));
  domino_snd: array [0 .. 3] of tipo_roms = ((n: 'dm-a7.snd'; l: $1000; p: 0; crc: $FA982DCC), (n: 'dm-a8.snd'; l: $1000; p: $1000; crc: $72839019), (n: 'dm-a9.snd'; l: $1000; p: $2000; crc: $AD760DA7), (n: 'dm-a10.snd'; l: $1000; p: $3000; crc: $958C7287));
  domino_char: array [0 .. 1] of tipo_roms = ((n: 'dmanbg0.bin'; l: $2000; p: 0; crc: $9163007F), (n: 'dmanbg1.bin'; l: $2000; p: $2000; crc: $28615C56));
  domino_sprites: array [0 .. 3] of tipo_roms = ((n: 'dmanfg0.bin'; l: $2000; p: 0; crc: $0B1F9F9E), (n: 'dmanfg1.bin'; l: $2000; p: $2000; crc: $16AA4B9B), (n: 'dmanfg2.bin'; l: $2000; p: $4000; crc: $4A8E76B8), (n: 'dmanfg3.bin'; l: $2000; p: $6000; crc: $1F39257E));
  wacko_rom: array [0 .. 3] of tipo_roms = ((n: 'wackocpu.2d'; l: $2000; p: 0; crc: $C98E29B6), (n: 'wackocpu.3d'; l: $2000; p: $2000; crc: $90B89774), (n: 'wackocpu.4d'; l: $2000; p: $4000; crc: $515EDFF7), (n: 'wackocpu.5d'; l: $2000; p: $6000; crc: $9B01BF32));
  wacko_snd: array [0 .. 2] of tipo_roms = ((n: 'wackosnd.7a'; l: $1000; p: 0; crc: $1A58763F), (n: 'wackosnd.8a'; l: $1000; p: $1000; crc: $A4E3C771), (n: 'wackosnd.9a'; l: $1000; p: $2000; crc: $155BA3DD));
  wacko_char: array [0 .. 1] of tipo_roms = ((n: 'wackocpu.3g'; l: $2000; p: 0; crc: $33160EB1), (n: 'wackocpu.4g'; l: $2000; p: $2000; crc: $DAF37D7C));
  wacko_sprites: array [0 .. 3] of tipo_roms = ((n: 'wackovid.1e'; l: $2000; p: 0; crc: $DCA59BE7), (n: 'wackovid.1d'; l: $2000; p: $2000; crc: $A02F1672), (n: 'wackovid.1b'; l: $2000; p: $4000; crc: $7D899790), (n: 'wackovid.1a'; l: $2000; p: $6000; crc: $080BE3AD));
  // DIP
  tapper_dipa: array [0 .. 3] of def_dip2 = ((mask: 4; name: 'Demo Sounds'; number: 2; val2: (4, 0); name2: ('Off', 'On')), (mask: $40; name: 'Cabinet'; number: 2; val2: ($40, 0); name2: ('Upright', 'Cocktail')), (mask: $80; name: 'Coin Meters'; number: 2; val2: ($80, 0);
    name2: ('1', '2')), ());
  dotron_dipa: array [0 .. 1] of def_dip2 = ((mask: 1; name: 'Coin Meters'; number: 2; val2: (1, 0); name2: ('1', '2')), ());
  tron_dipa: array [0 .. 3] of def_dip2 = ((mask: 1; name: 'Coin Meters'; number: 2; val2: (1, 0); name2: ('1', '2')), (mask: 2; name: 'Cabinet'; number: 2; val2: (0, 2); name2: ('Upright', 'Cocktail')), (mask: 4; name: 'Allow Continue'; number: 2; val2: (4, 0);
    name2: ('No', 'Yes')), ());
  shollow_dipa: array [0 .. 2] of def_dip2 = ((mask: 1; name: 'Coin Meters'; number: 2; val2: (1, 0); name2: ('1', '2')), (mask: 2; name: 'Cabinet'; number: 2; val2: (0, 2); name2: ('Upright', 'Cocktail')), ());
  domino_dipa: array [0 .. 4] of def_dip2 = ((mask: 1; name: 'Music'; number: 2; val2: (1, 0); name2: ('Off', 'On')), (mask: 2; name: 'Skin Color'; number: 2; val2: (2, 0); name2: ('Light', 'Dark')), (mask: $40; name: 'Cabinet'; number: 2; val2: (0, $40);
    name2: ('Upright', 'Cocktail')), (mask: $80; name: 'Coin Meters'; number: 2; val2: ($80, 0); name2: ('1', '2')), ());
  CPU_SYNC = 4;

var
  nvram: array [0 .. $7FF] of byte;
  update_video_mcr, eventos_mcr: procedure;
  // Sonido
  ssio_status: byte;
  ssio_data: array [0 .. 3] of byte;
  ssio_14024_count: byte;

procedure update_video_tapper;
  procedure put_sprite;
  var
    f, color, atrib, x, y, pos_x, pos_y, pos_x_def: byte;
    pos: pbyte;
    dir_x, dir_y: integer;
    nchar, sx, sy: word;
    prio: array [0 .. $1FF, 0 .. $1FF] of boolean;
  begin
    fillchar(prio, 512 * 512, 1);
    for f := $7F downto 0 do
    begin
      sx := ((memory[$E803 + (f * 4)] - 3) * 2) and $1FF;
      sy := ((241 - memory[$E800 + (f * 4)]) * 2) and $1FF;
      atrib := memory[$E801 + (f * 4)];
      nchar := (memory[$E802 + (f * 4)] + ((atrib and 8) shl 5)) mod gfx[1].elements;
      color := (not(atrib) and 3) shl 4;
      pos := gfx[1].datos;
      inc(pos, nchar * 32 * 32);
      if (atrib and $20) <> 0 then
      begin
        dir_y := -1;
        pos_y := 31;
      end
      else
      begin
        dir_y := 1;
        pos_y := 0;
      end;
      if (atrib and $10) <> 0 then
      begin
        dir_x := -1;
        pos_x_def := 31;
      end
      else
      begin
        dir_x := 1;
        pos_x_def := 0;
      end;
      for y := 0 to 31 do
      begin
        pos_x := pos_x_def;
        for x := 0 to 31 do
        begin
          if prio[(sx + pos_x) and $1FF, (sy + pos_y) and $1FF] then
          begin
            if (pos^ and $F) <> 0 then
            begin
              prio[(sx + pos_x) and $1FF, (sy + pos_y) and $1FF] := false;
              if (pos^ and 7) <> 0 then
              begin
                punbuf^ := paleta[pos^ or color];
                putpixel_gfx_int(((sx + pos_x) and $1FF) + ADD_SPRITE, ((sy + pos_y) and $1FF) + ADD_SPRITE, 1, 2);
              end;
            end;
          end;
          inc(pos);
          inc(pos_x, dir_x);
        end;
        inc(pos_y, dir_y);
      end;
    end;
  end;

var
  x, y: byte;
  atrib, f, nchar, color: word;
begin
  for f := 0 to $3BF do
  begin
    atrib := memory[$F000 + (f * 2)] or (memory[$F001 + (f * 2)] shl 8);
    color := (atrib shr 12) and 3;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      x := f and $1F;
      y := f shr 5;
      nchar := atrib and $3FF;
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 1, 0, (atrib and $400) <> 0, (atrib and $800) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 512, 480, 1, 0, 0, 512, 480, 2);
  put_sprite;
  update_final_piece(0, 0, 512, 480, 2);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure update_video_tron;
var
  prio: array [0 .. $1F, 0 .. $1F] of byte;
  procedure put_sprite;
  var
    f, atrib, x, y, pos_x, pos_y, pos_x_def: byte;
    pos: pbyte;
    dir_x, dir_y: integer;
    nchar, sx, sy: word;
    punt_def: byte;
  begin
    for f := 0 to $7F do
    begin
      sx := ((memory[$E002 + (f * 4)] - 4) * 2) and $1FF;
      sy := ((240 - memory[$E000 + (f * 4)]) * 2) and $1FF;
      atrib := memory[$E001 + (f * 4)];
      nchar := memory[$E001 + (f * 4)] mod gfx[1].elements;
      pos := gfx[1].datos;
      inc(pos, nchar * 32 * 32);
      if (atrib and $80) <> 0 then
      begin
        dir_y := -1;
        pos_y := 31;
      end
      else
      begin
        dir_y := 1;
        pos_y := 0;
      end;
      if (atrib and $40) <> 0 then
      begin
        dir_x := -1;
        pos_x_def := 31;
      end
      else
      begin
        dir_x := 1;
        pos_x_def := 0;
      end;
      for y := 0 to 31 do
      begin
        pos_x := pos_x_def;
        for x := 0 to 31 do
        begin
          punt_def := (prio[((sx + pos_x) and $1FF) div 16, ((sy + pos_y) and $1FF) div 16] shl 4) or pos^;
          if (punt_def and 7) <> 0 then
          begin
            punbuf^ := paleta[punt_def];
            putpixel_gfx_int(((sx + pos_x) and $1FF) + ADD_SPRITE, ((sy + pos_y) and $1FF) + ADD_SPRITE, 1, 2);
          end;
          inc(pos);
          inc(pos_x, dir_x);
        end;
        inc(pos_y, dir_y);
      end;
    end;
  end;

var
  x, y: byte;
  atrib, f, nchar, color: word;
begin
  fillchar(prio, $20 * $20, 0);
  for f := 0 to $3BF do
  begin
    atrib := memory[$E800 + (f * 2)] or (memory[$E801 + (f * 2)] shl 8);
    color := (atrib shr 11) and 3;
    x := f and $1F;
    y := f shr 5;
    prio[x, y] := atrib shr 14;
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := atrib and $3FF;
      put_gfx_flip(x * 16, y * 16, nchar, color shl 4, 1, 0, (atrib and $200) <> 0, (atrib and $400) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 512, 480, 1, 0, 0, 512, 480, 2);
  put_sprite;
  update_final_piece(0, 0, 512, 480, 2);
  fillchar(buffer_color, MAX_COLOR_BUFFER, 0);
end;

procedure events_tapper;
begin
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    // ip1
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // ip2
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
  end;
end;

procedure events_dotron;
begin
  marcade.in1 := analog.c[0].x[0] or $80;
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // ip2
    if p_contrls.map_arcade.left[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but2[0] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but3[0] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
  end;
end;

procedure events_tron;
begin
  marcade.in1 := analog.c[0].x[1];
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // ip2
    if p_contrls.map_arcade.left[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
  end;
end;

procedure events_shollow;
begin
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    // ip1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure events_domino;
begin
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // ip1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    // ip2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure events_wacko;
begin
  marcade.in1 := analog.c[0].x[0];
  marcade.in2 := analog.c[0].y[0];
  if event.arcade then
  begin
    // ip0
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // ip2
    if p_contrls.map_arcade.right[0] then
      marcade.in3 := (marcade.in3 and $FE)
    else
      marcade.in3 := (marcade.in3 or 1);
    if p_contrls.map_arcade.left[0] then
      marcade.in3 := (marcade.in3 and $FD)
    else
      marcade.in3 := (marcade.in3 or 2);
    if p_contrls.map_arcade.down[0] then
      marcade.in3 := (marcade.in3 and $FB)
    else
      marcade.in3 := (marcade.in3 or 4);
    if p_contrls.map_arcade.up[0] then
      marcade.in3 := (marcade.in3 and $F7)
    else
      marcade.in3 := (marcade.in3 or 8);
    if p_contrls.map_arcade.right[1] then
      marcade.in3 := (marcade.in3 and $EF)
    else
      marcade.in3 := (marcade.in3 or $10);
    if p_contrls.map_arcade.left[1] then
      marcade.in3 := (marcade.in3 and $DF)
    else
      marcade.in3 := (marcade.in3 or $20);
    if p_contrls.map_arcade.down[1] then
      marcade.in3 := (marcade.in3 and $BF)
    else
      marcade.in3 := (marcade.in3 or $40);
    if p_contrls.map_arcade.up[1] then
      marcade.in3 := (marcade.in3 and $7F)
    else
      marcade.in3 := (marcade.in3 or $80);
  end;
end;

procedure mcr_loop;
var
  f: word;
  h: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 479 do
    begin
      eventos_mcr;
      case f of
        0:
          begin
            ctc_0.trigger(2, true);
            ctc_0.trigger(2, false);
            update_video_mcr;
            ctc_0.trigger(3, true);
            ctc_0.trigger(3, false);
          end;
        240:
          begin
            ctc_0.trigger(2, true);
            ctc_0.trigger(2, false);
          end;
      end;
      for h := 1 to CPU_SYNC do
      begin
        // Main CPU
        z80_0.run(frame_main);
        frame_main := frame_main + z80_0.tframes - z80_0.contador;
        // Sound
        z80_1.run(frame_sub);
        frame_sub := frame_sub + z80_1.tframes - z80_1.contador;
      end;
    end;
    video_sync;
  end;
end;

function tapper_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $DFFF, $F000 .. $F7FF:
      tapper_getbyte := memory[direccion];
    $E000 .. $E7FF:
      tapper_getbyte := nvram[direccion and $7FF];
    $E800 .. $EBFF:
      tapper_getbyte := memory[$E800 + (direccion and $1FF)];
    $EC00 .. $EFFF, $F800 .. $FFFF:
      tapper_getbyte := $FF;
  end;
end;

procedure cambiar_color(pos: byte; tmp_color: word);
var
  color: tcolor;
begin
  color.r := pal3bit(tmp_color shr 6);
  color.g := pal3bit(tmp_color shr 0);
  color.b := pal3bit(tmp_color shr 3);
  set_pal_color(color, pos);
  buffer_color[(pos shr 4) and 3] := true;
end;

procedure tapper_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ; // ROM
    $E000 .. $E7FF:
      nvram[direccion and $7FF] := valor;
    $E800 .. $EBFF:
      memory[$E800 + (direccion and $1FF)] := valor;
    $F000 .. $F7FF:
      begin
        memory[direccion] := valor;
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
      end;
    $F800 .. $FFFF:
      cambiar_color((direccion and $7F) shr 1, valor or ((direccion and 1) shl 8));
  end;
end;

function tapper_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0 .. $1F:
      case (puerto and 7) of
        0:
          tapper_inbyte := marcade.in0;
        1:
          tapper_inbyte := marcade.in1;
        2:
          tapper_inbyte := marcade.in2;
        3:
          tapper_inbyte := marcade.dswa;
        4:
          tapper_inbyte := marcade.in3;
        7:
          tapper_inbyte := ssio_status;
      end;
    $F0 .. $F3:
      tapper_inbyte := ctc_0.read(puerto and 3);
  end;
end;

procedure tapper_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0 .. 7:
      ;
    $1C .. $1F:
      ssio_data[puerto and 3] := valor;
    $E0:
      ; // wathcdog
    $E8:
      ;
    $F0 .. $F3:
      ctc_0.write(puerto and 3, valor);
  end;
end;

// Tron
function tron_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF:
      tron_getbyte := memory[direccion];
    $C000 .. $DFFF:
      tron_getbyte := nvram[direccion and $7FF];
    $E000 .. $FFFF:
      case (direccion and $FFF) of
        0 .. $7FF:
          tron_getbyte := memory[$E000 + (direccion and $1FF)];
        $800 .. $FFF:
          tron_getbyte := memory[$E800 + (direccion and $7FF)];
      end;
  end;
end;

procedure tron_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ; // ROM
    $C000 .. $DFFF:
      nvram[direccion and $7FF] := valor;
    $E000 .. $FFFF:
      case (direccion and $FFF) of
        0 .. $7FF:
          memory[$E000 + (direccion and $1FF)] := valor;
        $800 .. $FFF:
          begin
            memory[$E800 + (direccion and $7FF)] := valor;
            gfx[0].buffer[(direccion and $7FF) shr 1] := true;
            if (direccion and $780) = $780 then
              cambiar_color((direccion and $7F) shr 1, valor or ((direccion and 1) shl 8));
          end;
      end;
  end;
end;

function snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      snd_getbyte := mem_snd[direccion];
    $8000 .. $8FFF:
      snd_getbyte := mem_snd[$8000 or (direccion and $3FF)];
    $9000 .. $9FFF:
      snd_getbyte := ssio_data[direccion and $3];
    $A000 .. $AFFF:
      if (direccion and 3) = 1 then
        snd_getbyte := ay8910_0.read;
    $B000 .. $BFFF:
      if (direccion and 3) = 1 then
        snd_getbyte := ay8910_1.read;
    $E000 .. $EFFF:
      begin
        ssio_14024_count := 0;
        z80_1.change_irq(CLEAR_LINE);
      end;
    $F000 .. $FFFF:
      snd_getbyte := $FF; // DIP
  end;
end;

procedure snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $8000 .. $8FFF:
      mem_snd[$8000 or (direccion and $3FF)] := valor;
    $A000 .. $AFFF:
      case (direccion and 3) of
        0:
          ay8910_0.control(valor);
        2:
          ay8910_0.write(valor);
      end;
    $B000 .. $BFFF:
      case (direccion and 3) of
        0:
          ay8910_1.control(valor);
        2:
          ay8910_1.write(valor);
      end;
    $C000 .. $CFFF:
      ssio_status := valor;
  end;
end;

procedure z80ctc_int(state: byte);
begin
  z80_0.change_irq(state);
end;

procedure mcr_snd_irq;
begin
  //
  // /SINT is generated as follows:
  //
  // Starts with a 16MHz oscillator
  // /2 via 7474 flip-flop @ F11
  // /16 via 74161 binary counter @ E11
  // /10 via 74190 decade counter @ D11
  //
  // Bit 3 of the decade counter clocks a 14024 7-bit async counter @ C12.
  // This routine is called to clock this 7-bit counter.
  // Bit 6 of the output is inverted and connected to /SINT.
  //
  ssio_14024_count := (ssio_14024_count + 1) and $7F;
  // if the low 5 bits clocked to 0, bit 6 has changed state
  if ((ssio_14024_count and $3F) = 0) then
    if (ssio_14024_count and $40) <> 0 then
      z80_1.change_irq(ASSERT_LINE)
    else
      z80_1.change_irq(CLEAR_LINE);
end;

procedure mcr_update_sound;
begin
  tsample[ay8910_0.get_sample_num, sound_status.sound_position] := ay8910_0.update_internal^;
  tsample[ay8910_0.get_sample_num, sound_status.sound_position + 1] := ay8910_1.update_internal^;
end;

// Main
procedure mcr_reset;
begin
  z80_0.reset;
  z80_1.reset;
  frame_main := z80_0.tframes;
  frame_sub := z80_1.tframes;
  ctc_0.reset;
  ay8910_0.reset;
  ay8910_1.reset;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  marcade.in3 := $FF;
  // Sonido
  ssio_status := 0;
  fillchar(ssio_data[0], 4, 0);
  ssio_14024_count := 0;
end;

procedure close_mcr;
begin
  case main_vars.machine_type of
    324:
      write_file(dm.tConfignvram.AsString + 'tapper.nv', @nvram, $800);
    411:
      write_file(dm.tConfignvram.AsString + 'dotron.nv', @nvram, $800);
    412:
      write_file(dm.tConfignvram.AsString + 'tron.nv', @nvram, $800);
    413:
      write_file(dm.tConfignvram.AsString + 'timber.nv', @nvram, $800);
    414:
      write_file(dm.tConfignvram.AsString + 'shollow.nv', @nvram, $800);
    415:
      write_file(dm.tConfignvram.AsString + 'domino.nv', @nvram, $800);
    416:
      write_file(dm.tConfignvram.AsString+ 'wacko.nv', @nvram, $800);
  end;
end;

function start_mcr: boolean;
var
  memory_temp: array [0 .. $1FFFF] of byte;
  longitud: integer;
  procedure convert_chars(num: word);
  const
    pc_x: array [0 .. 15] of dword = (0 * 2, 0 * 2, 1 * 2, 1 * 2, 2 * 2, 2 * 2, 3 * 2, 3 * 2, 4 * 2, 4 * 2, 5 * 2, 5 * 2, 6 * 2, 6 * 2, 7 * 2, 7 * 2);
    pc_y: array [0 .. 15] of dword = (0 * 16, 0 * 16, 1 * 16, 1 * 16, 2 * 16, 2 * 16, 3 * 16, 3 * 16, 4 * 16, 4 * 16, 5 * 16, 5 * 16, 6 * 16, 6 * 16, 7 * 16, 7 * 16);
  begin
    init_gfx(0, 16, 16, num);
    gfx_set_desc_data(4, 0, 16 * 8, num * 16 * 8, (num * 16 * 8) + 1, 0, 1);
    convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
  end;
  procedure convert_sprites(num: word);
  const
    ps_x: array [0 .. 31] of dword = (0, 4, $100 * 32 * 32, $100 * 32 * 32 + 4, $200 * 32 * 32, $200 * 32 * 32 + 4, $300 * 32 * 32, $300 * 32 * 32 + 4, 8, 12, $100 * 32 * 32 + 8, $100 * 32 * 32 + 12, $200 * 32 * 32 + 8, $200 * 32 * 32 + 12, $300 * 32 * 32 + 8,
      $300 * 32 * 32 + 12, 16, 20, $100 * 32 * 32 + 16, $100 * 32 * 32 + 20, $200 * 32 * 32 + 16, $200 * 32 * 32 + 20, $300 * 32 * 32 + 16, $300 * 32 * 32 + 20, 24, 28, $100 * 32 * 32 + 24, $100 * 32 * 32 + 28, $200 * 32 * 32 + 24, $200 * 32 * 32 + 28, $300 * 32 * 32 + 24,
      $300 * 32 * 32 + 28);
    ps_x_dt: array [0 .. 31] of dword = (0, 4, $80 * 32 * 32, $80 * 32 * 32 + 4, $100 * 32 * 32, $100 * 32 * 32 + 4, $180 * 32 * 32, $180 * 32 * 32 + 4, 8, 12, $80 * 32 * 32 + 8, $80 * 32 * 32 + 12, $100 * 32 * 32 + 8, $100 * 32 * 32 + 12, $180 * 32 * 32 + 8, $180 * 32 * 32 + 12,
      16, 20, $80 * 32 * 32 + 16, $80 * 32 * 32 + 20, $100 * 32 * 32 + 16, $100 * 32 * 32 + 20, $180 * 32 * 32 + 16, $180 * 32 * 32 + 20, 24, 28, $80 * 32 * 32 + 24, $80 * 32 * 32 + 28, $100 * 32 * 32 + 24, $100 * 32 * 32 + 28, $180 * 32 * 32 + 24, $180 * 32 * 32 + 28);
    ps_x_t: array [0 .. 31] of dword = (0, 4, $40 * 32 * 32, $40 * 32 * 32 + 4, $80 * 32 * 32, $80 * 32 * 32 + 4, $C0 * 32 * 32, $C0 * 32 * 32 + 4, 8, 12, $40 * 32 * 32 + 8, $40 * 32 * 32 + 12, $80 * 32 * 32 + 8, $80 * 32 * 32 + 12, $C0 * 32 * 32 + 8, $C0 * 32 * 32 + 12, 16, 20,
      $40 * 32 * 32 + 16, $40 * 32 * 32 + 20, $80 * 32 * 32 + 16, $80 * 32 * 32 + 20, $C0 * 32 * 32 + 16, $C0 * 32 * 32 + 20, 24, 28, $40 * 32 * 32 + 24, $40 * 32 * 32 + 28, $80 * 32 * 32 + 24, $80 * 32 * 32 + 28, $C0 * 32 * 32 + 24, $C0 * 32 * 32 + 28);
    ps_y: array [0 .. 31] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32, 16 * 32, 17 * 32, 18 * 32, 19 * 32, 20 * 32, 21 * 32, 22 * 32, 23 * 32, 24 * 32, 25 * 32, 26 * 32, 27 * 32,
      28 * 32, 29 * 32, 30 * 32, 31 * 32);
  begin
    init_gfx(1, 32, 32, num);
    gfx_set_desc_data(4, 0, 32 * 32, 0, 1, 2, 3);
    case num of
      $40:
        convert_gfx(1, 0, @memory_temp, @ps_x_t, @ps_y, false, false);
      $80:
        convert_gfx(1, 0, @memory_temp, @ps_x_dt, @ps_y, false, false);
      $100:
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
    end;
  end;

begin
  machine_calls.general_loop := mcr_loop;
  machine_calls.reset := mcr_reset;
  machine_calls.fps_max := 30;
  machine_calls.close := close_mcr;
  start_mcr := false;
  start_audio(true);
  if ((main_vars.machine_type = 412) or (main_vars.machine_type = 414)) then
    main_screen.rot90_screen := true;
  screen_init(1, 512, 480);
  screen_init(2, 512, 512, false, true);
  start_video(512, 480);
  // Main CPU
  z80_0 := cpu_z80.create(5000000, 480 * CPU_SYNC);
  z80_0.change_io_calls(tapper_inbyte, tapper_outbyte);
  z80_0.enable_daisy;
  ctc_0 := tz80ctc.create(z80_0.numero_cpu, 5000000, z80_0.clock, 0, CTC0_TRG01);
  ctc_0.change_calls(z80ctc_int);
  z80daisy_init(Z80_CTC0_TYPE);
  // Sound CPU
  z80_1 := cpu_z80.create(2000000, 480 * CPU_SYNC);
  z80_1.change_ram_calls(snd_getbyte, snd_putbyte);
  z80_1.init_sound(mcr_update_sound);
  timers.init(z80_1.numero_cpu, 2000000 / (160 * 2 * 16 * 10), mcr_snd_irq, nil, true);
  // Sound Chip
  ay8910_0 := ay8910_chip.create(2000000, AY8910);
  ay8910_1 := ay8910_chip.create(2000000, AY8910, 1, true);
  case main_vars.machine_type of
    324:
      begin
        z80_0.change_ram_calls(tapper_getbyte, tapper_putbyte);
        eventos_mcr := events_tapper;
        update_video_mcr := update_video_tapper;
        // cargar roms
        if not(roms_load(@memory, tapper_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, tapper_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, tapper_char)) then
          exit;
        convert_chars($400);
        // sprites
        if not(roms_load(@memory_temp, tapper_sprites)) then
          exit;
        convert_sprites($100);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'tapper.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'tapper.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val2 := @tapper_dipa;
      end;
    411:
      begin
        z80_0.change_ram_calls(tapper_getbyte, tapper_putbyte);
        eventos_mcr := events_dotron;
        update_video_mcr := update_video_tapper;
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(50, 2, 0, $7F, 0, false, true, false, true);
        main_screen.flip_main_x := true;
        // cargar roms
        if not(roms_load(@memory, dotron_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, dotron_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, dotron_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, dotron_sprites)) then
          exit;
        convert_sprites($80);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'dotron.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'dotron.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := $FF;
        marcade.dswa_val2 := @dotron_dipa;
      end;
    412:
      begin
        z80_0.change_ram_calls(tron_getbyte, tron_putbyte);
        eventos_mcr := events_tron;
        update_video_mcr := update_video_tron;
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(50, 2, 0, $FF, 0, false, true, false, true);
        // cargar roms
        if not(roms_load(@memory, tron_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, tron_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, tron_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, tron_sprites)) then
          exit;
        convert_sprites($40);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'tron.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'tron.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := 0;
        marcade.dswa_val2 := @tron_dipa;
      end;
    413:
      begin
        z80_0.change_ram_calls(tapper_getbyte, tapper_putbyte);
        eventos_mcr := events_tapper;
        update_video_mcr := update_video_tapper;
        // cargar roms
        if not(roms_load(@memory, timber_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, timber_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, timber_char)) then
          exit;
        convert_chars($400);
        // sprites
        if not(roms_load(@memory_temp, timber_sprites)) then
          exit;
        convert_sprites($100);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'timber.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'timber.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := $C0;
        marcade.dswa_val2 := @tapper_dipa;
      end;
    414:
      begin
        z80_0.change_ram_calls(tron_getbyte, tron_putbyte);
        eventos_mcr := events_shollow;
        update_video_mcr := update_video_tron;
        // cargar roms
        if not(roms_load(@memory, shollow_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, shollow_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, shollow_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, shollow_sprites)) then
          exit;
        convert_sprites($40);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'shollow.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'shollow.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := $FD;
        marcade.dswa_val2 := @shollow_dipa;
      end;
    415:
      begin
        z80_0.change_ram_calls(tron_getbyte, tron_putbyte);
        eventos_mcr := events_domino;
        update_video_mcr := update_video_tron;
        // cargar roms
        if not(roms_load(@memory, domino_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, domino_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, domino_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, domino_sprites)) then
          exit;
        convert_sprites($40);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'domino.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'domino.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := $3E;
        marcade.dswa_val2 := @domino_dipa;
      end;
    416:
      begin
        z80_0.change_ram_calls(tron_getbyte, tron_putbyte);
        eventos_mcr := events_wacko;
        update_video_mcr := update_video_tron;
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(50, 2, 0, $7F, 0, false, true, true, false);
        // cargar roms
        if not(roms_load(@memory, wacko_rom)) then
          exit;
        // cargar roms sonido
        if not(roms_load(@mem_snd, wacko_snd)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, wacko_char)) then
          exit;
        convert_chars($200);
        // sprites
        if not(roms_load(@memory_temp, wacko_sprites)) then
          exit;
        convert_sprites($40);
        // Cargar NVRam
        if read_file_size(dm.tConfignvram.AsString + 'wacko.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'wacko.nv', @nvram, longitud)
        else
          fillchar(nvram, $800, 0);
        // DIP
        marcade.dswa := $3E;
        marcade.dswa_val2 := @domino_dipa;
      end;
  end;
  start_mcr := true;
end;

end.
