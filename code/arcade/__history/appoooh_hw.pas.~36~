unit appoooh_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  sn_76496,
  msm5205,
  sega_decrypt_2;

function start_appoooh: boolean;

implementation

const
  appoooh_rom: array [0 .. 8] of tipo_roms = ((n: 'epr-5906.bin'; l: $2000; p: 0; crc: $FFFAE7FE),
    (n: 'epr-5907.bin'; l: $2000; p: $2000; crc: $57696CD6), (n: 'epr-5908.bin'; l: $2000; p: $4000;
    crc: $4537CDDC), (n: 'epr-5909.bin'; l: $2000; p: $6000; crc: $CF82718D), (n: 'epr-5910.bin'; l: $2000;
    p: $8000; crc: $312636DA), (n: 'epr-5911.bin'; l: $2000; p: $A000; crc: $0BC2ACAA), (n: 'epr-5913.bin';
    l: $2000; p: $C000; crc: $F5A0E6A7), (n: 'epr-5912.bin'; l: $2000; p: $E000; crc: $3C3915AB),
    (n: 'epr-5914.bin'; l: $2000; p: $10000; crc: $58792D4A));
  appoooh_char1: array [0 .. 2] of tipo_roms = ((n: 'epr-5895.bin'; l: $4000; p: 0; crc: $4B0D4294),
    (n: 'epr-5896.bin'; l: $4000; p: $4000; crc: $7BC84D75), (n: 'epr-5897.bin'; l: $4000; p: $8000;
    crc: $745F3FFA));
  appoooh_char2: array [0 .. 2] of tipo_roms = ((n: 'epr-5898.bin'; l: $4000; p: 0; crc: $CF01644D),
    (n: 'epr-5899.bin'; l: $4000; p: $4000; crc: $885AD636), (n: 'epr-5900.bin'; l: $4000; p: $8000;
    crc: $A8ED13F3));
  appoooh_prom: array [0 .. 2] of tipo_roms = ((n: 'pr5921.prm'; l: $20; p: 0; crc: $F2437229),
    (n: 'pr5922.prm'; l: $100; p: $20; crc: $85C542BF), (n: 'pr5923.prm'; l: $100; p: $120; crc: $16ACBD53));
  appoooh_adpcm: array [0 .. 4] of tipo_roms = ((n: 'epr-5901.bin'; l: $2000; p: 0; crc: $170A10A4),
    (n: 'epr-5902.bin'; l: $2000; p: $2000; crc: $F6981640), (n: 'epr-5903.bin'; l: $2000; p: $4000;
    crc: $0439DF50), (n: 'epr-5904.bin'; l: $2000; p: $6000; crc: $9988F2AE), (n: 'epr-5905.bin'; l: $2000;
    p: $8000; crc: $FB5CD70E));
  robowres_rom: array [0 .. 2] of tipo_roms = ((n: 'epr-7540.13d'; l: $8000; p: 0; crc: $A2A54237),
    (n: 'epr-7541.14d'; l: $8000; p: $8000; crc: $CBF7D1A8), (n: 'epr-7542.15d'; l: $8000; p: $10000;
    crc: $3475FBD4));
  robowres_char1: array [0 .. 2] of tipo_roms = ((n: 'epr-7544.7h'; l: $8000; p: 0; crc: $07B846CE),
    (n: 'epr-7545.6h'; l: $8000; p: $8000; crc: $E99897BE), (n: 'epr-7546.5h'; l: $8000; p: $10000;
    crc: $1559235A));
  robowres_char2: array [0 .. 2] of tipo_roms = ((n: 'epr-7547.7d'; l: $8000; p: 0; crc: $B87AD4A4),
    (n: 'epr-7548.6d'; l: $8000; p: $8000; crc: $8B9C75B3), (n: 'epr-7549.5d'; l: $8000; p: $10000;
    crc: $F640AFBB));
  robowres_prom: array [0 .. 2] of tipo_roms = ((n: 'pr7571.10a'; l: $20; p: 0; crc: $E82C6D5C),
    (n: 'pr7572.7f'; l: $100; p: $20; crc: $2B083D0C), (n: 'pr7573.7g'; l: $100; p: $120; crc: $2B083D0C));
  robowres_adpcm: tipo_roms = (n: 'epr-7543.12b'; l: $8000; p: 0; crc: $4D108C49);
  // DIP
  appoooh_dip: array [0 .. 5] of def_dip = ((mask: $7; name: 'Coin A'; number: 8;
    dip: ((dip_val: $3; dip_name: '4C 1C'), (dip_val: $2; dip_name: '3C 1C'), (dip_val: $1;
    dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $7; dip_name: '2C 3C'), (dip_val: $4;
    dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 3C'), (dip_val: $6; dip_name: '1C 6C'), (), (), (), (),
    (), (), (), ())), (mask: $18; name: 'Coin B'; number: 4;
    dip: ((dip_val: $18; dip_name: '3C 1C'), (dip_val: $10; dip_name: '2C 1C'), (dip_val: $0;
    dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Cabinet';
    number: 2; dip: ((dip_val: $40; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Difficulty'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Easy'), (dip_val: $80; dip_name: 'Hard'), (), (), (), (), (), (), (), (),
    (), (), (), (), (), ())), ());
  robowres_dip: array [0 .. 4] of def_dip = ((mask: $7; name: 'Coin A'; number: 8;
    dip: ((dip_val: $3; dip_name: '4C 1C'), (dip_val: $2; dip_name: '3C 1C'), (dip_val: $1;
    dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $7; dip_name: '2C 3C'), (dip_val: $4;
    dip_name: '1C 2C'), (dip_val: $5; dip_name: '1C 3C'), (dip_val: $6; dip_name: '1C 6C'), (), (), (), (),
    (), (), (), ())), (mask: $18; name: 'Coin B'; number: 4;
    dip: ((dip_val: $18; dip_name: '3C 1C'), (dip_val: $10; dip_name: '2C 1C'), (dip_val: $0;
    dip_name: '1C 1C'), (dip_val: $8; dip_name: '1C 2C'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $20; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $20;
    dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Language';
    number: 2; dip: ((dip_val: $0; dip_name: 'Japanese'), (dip_val: $80; dip_name: 'English'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), ());

var
  adpcm_playing, nmi_vblank: boolean;
  priority, rom_bank: byte;
  memory_rom: array [0 .. 1, 0 .. $3FFF] of byte;
  // scroll_x:byte; Se usa???
  rom_dec: array [0 .. $7FFF] of byte;
  sprite_base: word;

procedure update_video_appoooh;
var
  x, y, atrib: byte;
  f, nchar, color: word;
  flip_x: boolean;
  procedure draw_sprites(bank: byte);
  var
    f, atrib2: byte;
    sx, sy: word;
  begin
    for f := $7 downto 0 do
    begin
      atrib := memory[$F001 + $800 * bank + (f * 4)];
      atrib2 := memory[$F002 + $800 * bank + (f * 4)];
      nchar := sprite_base + ((atrib shr 2) + ((atrib2 and $E0) shl 1));
      color := (atrib2 and $F) shl 3;
      sx := memory[$F003 + $800 * bank + (f * 4)];
      sy := 240 - memory[$F000 + $800 * bank + (f * 4)];
      put_gfx_sprite(nchar, color + $100 * bank, (atrib and 1) <> 0, false, bank + 2);
      update_gfx_sprite(sx, sy, 3, bank + 2);
    end;
  end;

begin
  for f := 0 to $3FF do
  begin
    y := f shr 5;
    x := f and $1F;
    // Background
    if gfx[1].buffer[f] then
    begin
      atrib := memory[$FC00 + f];
      color := (atrib and $F);
      nchar := memory[$F800 + f] + ((atrib shr 5) and 7) * 256;
      flip_x := (atrib and $10) <> 0;
      put_gfx_flip(x * 8, y * 8, nchar, (color shl 3) + 256, 1, 1, flip_x, false);
      gfx[1].buffer[f] := false;
    end;
    // Foreground
    if gfx[0].buffer[f] then
    begin
      atrib := memory[$F400 + f];
      color := (atrib and $F);
      nchar := memory[$F000 + f] + ((atrib shr 5) and 7) * 256;
      flip_x := (atrib and $10) <> 0;
      put_gfx_trans_flip(x * 8, y * 8, nchar, color shl 3, 2, 0, flip_x, false);
      gfx[0].buffer[f] := false;
    end;
  end;
  actualiza_trozo(0, 0, 256, 256, 1, 0, 0, 256, 256, 3);
  if priority = 0 then
    actualiza_trozo(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  if priority = 1 then
  begin
    draw_sprites(0);
    draw_sprites(1);
  end
  else
  begin
    draw_sprites(1);
    draw_sprites(0);
  end;
  if priority <> 0 then
    actualiza_trozo(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  actualiza_trozo_final(0, 16, 256, 224, 3);
end;

procedure events_appoooh;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or 1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or 2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or 4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or 8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    // p2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 or 1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or 2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 or 4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or 8)
    else
      marcade.in1 := (marcade.in1 and $F7);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 or $10)
    else
      marcade.in1 := (marcade.in1 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 or $80)
    else
      marcade.in1 := (marcade.in1 and $7F);
    // System
    if p_contrls.map_arcade.but2[0] then
      marcade.in2 := (marcade.in2 or 1)
    else
      marcade.in2 := (marcade.in2 and $FE);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 or 2)
    else
      marcade.in2 := (marcade.in2 and $FD);
  end;
end;

procedure appoooh_loop;
var
  frame: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to $FF do
    begin
      z80_0.run(frame);
      frame := frame + z80_0.tframes - z80_0.contador;
      if f = 239 then
      begin
        if nmi_vblank then
          z80_0.change_nmi(PULSE_LINE);
        update_video_appoooh;
      end;
    end;
    events_appoooh;
    video_sync;
  end;
end;

function appoooh_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $9FFF, $E000 .. $FFFF:
      appoooh_getbyte := memory[direccion];
    $A000 .. $DFFF:
      appoooh_getbyte := memory_rom[rom_bank, direccion - $A000];
  end;
end;

procedure appoooh_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $DFFF:
      ; // ROM
    $E000 .. $EFFF:
      memory[direccion] := valor;
    $F000 .. $F7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $F800 .. $FFFF:
      if memory[direccion] <> valor then
      begin
        gfx[1].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

function appoooh_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    0:
      appoooh_inbyte := marcade.in0;
    1:
      appoooh_inbyte := marcade.in1;
    3:
      appoooh_inbyte := marcade.dswa;
    4:
      appoooh_inbyte := marcade.in2;
  end;
end;

procedure appoooh_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    0:
      sn_76496_0.write(valor);
    1:
      sn_76496_1.write(valor);
    2:
      sn_76496_2.write(valor);
    3:
      begin
        msm5205_0.pos := (valor shl 8) * 2;
        msm5205_0.reset_w(false);
        adpcm_playing := true;
      end;
    4:
      begin
        nmi_vblank := (valor and $1) <> 0;
        main_screen.flip_main_screen := (valor and 2) <> 0;
        if priority <> ((valor and $30) shr 4) then
        begin
          priority := (valor and $30) shr 4;
          fillchar(gfx[0].buffer, $400, 1);
          fillchar(gfx[1].buffer, $400, 1);
        end;
        rom_bank := (valor and $40) shr 6;
      end;
    5:
      ; // scroll_x:=valor-16; Se usa???
  end;
end;

procedure snd_adpcm;
begin
  if not(adpcm_playing) then
    exit;
  msm5205_0.data_val := msm5205_0.rom_data[msm5205_0.pos div 2];
  if (msm5205_0.data_val = $70) then
  begin
    msm5205_0.reset_w(true);
    adpcm_playing := false;
  end
  else
  begin
    if (msm5205_0.pos and 1) <> 0 then
      msm5205_0.data_w(msm5205_0.data_val and $F)
    else
      msm5205_0.data_w(msm5205_0.data_val shr 4);
    msm5205_0.pos := (msm5205_0.pos + 1) and $1FFFF;
  end;
end;

procedure appoooh_update_sound;
begin
  sn_76496_0.update;
  sn_76496_1.update;
  sn_76496_2.update;
  msm5205_0.update;
end;

// Robo Wres
function robowres_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      if z80_0.opcode then
        robowres_getbyte := rom_dec[direccion]
      else
        robowres_getbyte := memory[direccion];
    $8000 .. $9FFF, $E000 .. $FFFF:
      robowres_getbyte := memory[direccion];
    $A000 .. $DFFF:
      robowres_getbyte := memory_rom[rom_bank, direccion - $A000];
  end;
end;

// Main
procedure appoooh_reset;
begin
  z80_0.reset;
  reset_audio;
  sn_76496_0.reset;
  sn_76496_1.reset;
  sn_76496_2.reset;
  msm5205_0.reset;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  nmi_vblank := false;
  adpcm_playing := false;
  // scroll_x:=0;
  priority := $FF;
  rom_bank := 0;
end;

function start_appoooh: boolean;
const
  pc_x: array [0 .. 15] of dword = (7, 6, 5, 4, 3, 2, 1, 0, 8 * 8 + 7, 8 * 8 + 6, 8 * 8 + 5, 8 * 8 + 4,
    8 * 8 + 3, 8 * 8 + 2, 8 * 8 + 1, 8 * 8 + 0);
  pc_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8,
    18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
var
  memory_temp: array [0 .. $2FFFF] of byte;
  colores: tpaleta;
  pen, bit0, bit1, bit2: byte;
  f: word;
  procedure chars_gfx(ngfx: byte; num: word);
  begin
    init_gfx(ngfx, 8, 8, num);
    gfx[ngfx].trans[0] := true;
    gfx_set_desc_data(3, 0, 8 * 8, num * 8 * 8 * 2, num * 8 * 8 * 1, num * 8 * 8 * 0);
    convert_gfx(ngfx, 0, @memory_temp, @pc_x, @pc_y, false, false);
  end;
  procedure sprites_gfx(ngfx: byte; num: word);
  begin
    init_gfx(ngfx, 16, 16, num);
    gfx[ngfx].trans[0] := true;
    gfx_set_desc_data(3, 0, 32 * 8, num * 8 * 8 * 2, num * 8 * 8 * 1, num * 8 * 8 * 0);
    convert_gfx(ngfx, 0, @memory_temp, @pc_x, @pc_y, false, false);
  end;

begin
  machine_calls.general_loop := appoooh_loop;
  machine_calls.reset := appoooh_reset;
  machine_calls.fps_max := 60;
  start_appoooh := false;
  start_audio(false);
  screen_init(1, 256, 256, true);
  // screen_mod_scroll(1,256,256,255,256,256,255);
  screen_init(2, 256, 256, true);
  // screen_mod_scroll(2,256,256,255,256,256,255);
  screen_init(3, 256, 256, false, true);
  start_video(256, 224);
  // Main CPU
  z80_0 := cpu_z80.create(18432000 div 6, 256);
  z80_0.change_ram_calls(appoooh_getbyte, appoooh_putbyte);
  z80_0.change_io_calls(appoooh_inbyte, appoooh_outbyte);
  z80_0.init_sound(appoooh_update_sound);
  // Sound Chip
  sn_76496_0 := sn76496_chip.create(18432000 div 6);
  sn_76496_1 := sn76496_chip.create(18432000 div 6);
  sn_76496_2 := sn76496_chip.create(18432000 div 6);
  msm5205_0 := MSM5205_chip.create(384000, MSM5205_S64_4B, 0.5, $10000);
  msm5205_0.change_advance(snd_adpcm);
  case main_vars.machine_type of
    364:
      begin // Appoooh
        if not(roms_load(msm5205_0.rom_data, appoooh_adpcm)) then
          exit;
        if not(roms_load(@memory_temp, appoooh_rom)) then
          exit;
        // Ponerlas en su sitio
        copymemory(@memory, @memory_temp, $A000);
        copymemory(@memory_rom[0, 0], @memory_temp[$A000], $4000);
        copymemory(@memory_rom[1, 0], @memory_temp[$E000], $4000);
        if not(roms_load(@memory_temp, appoooh_char1)) then
          exit;
        chars_gfx(0, $800);
        sprites_gfx(2, $800);
        if not(roms_load(@memory_temp, appoooh_char2)) then
          exit;
        chars_gfx(1, $800);
        sprites_gfx(3, $800);
        if not(roms_load(@memory_temp, appoooh_prom)) then
          exit;
        sprite_base := 0;
        // DIP
        marcade.dswa := $60;
        marcade.dswa_val := @appoooh_dip;
      end;
    365:
      begin // Robo Wres 2001
        z80_0.change_ram_calls(robowres_getbyte, appoooh_putbyte);
        if not(roms_load(msm5205_0.rom_data, robowres_adpcm)) then
          exit;
        if not(roms_load(@memory_temp, robowres_rom)) then
          exit;
        // Ponerlas en su sitio
        decode_sega_type2(@memory_temp, @rom_dec, S315_5179);
        copymemory(@memory, @memory_temp, $A000);
        copymemory(@memory_rom[0, 0], @memory_temp[$A000], $4000);
        copymemory(@memory_rom[1, 0], @memory_temp[$12000], $4000);
        if not(roms_load(@memory_temp, robowres_char1)) then
          exit;
        chars_gfx(0, $1000);
        sprites_gfx(2, $1000);
        if not(roms_load(@memory_temp, robowres_char2)) then
          exit;
        chars_gfx(1, $1000);
        sprites_gfx(3, $1000);
        if not(roms_load(@memory_temp, robowres_prom)) then
          exit;
        sprite_base := $200;
        // DIP
        marcade.dswa := $E0;
        marcade.dswa_val := @robowres_dip;
      end;
  end;
  // color
  for f := 0 to $1FF do
  begin
    pen := (memory_temp[$20 + f] and $F);
    if ((f > $FF) and (main_vars.machine_type = 364)) then
      pen := pen or $10;
    // red component
    bit0 := (memory_temp[pen] shr 0) and $1;
    bit1 := (memory[pen] shr 1) and $1;
    bit2 := (memory[pen] shr 2) and $1;
    colores[f].r := $21 * bit0 + $47 * bit1 + $97 * bit2;
    // green component
    bit0 := (memory[pen] shr 3) and $1;
    bit1 := (memory[pen] shr 4) and $1;
    bit2 := (memory[pen] shr 5) and $1;
    colores[f].g := $21 * bit0 + $47 * bit1 + $97 * bit2;
    // blue component
    bit1 := (memory[pen] shr 6) and $1;
    bit2 := (memory[pen] shr 7) and $1;
    colores[f].b := 0 + $47 * bit1 + $97 * bit2;
  end;
  set_pal(colores, $200);
  // final
  appoooh_reset;
  start_appoooh := true;
end;

end.
