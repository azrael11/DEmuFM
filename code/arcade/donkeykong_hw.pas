unit donkeykong_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  samples,
  rom_engine,
  pal_engine,
  sound_engine,
  n2a03;

function start_donkeykong: boolean;

implementation

const
  // Donkey Kong
  dkong_rom: array [0 .. 3] of tipo_roms = ((n: 'c_5et_g.bin'; l: $1000; p: 0; crc: $BA70B88B), (n: 'c_5ct_g.bin'; l: $1000; p: $1000; crc: $5EC461EC), (n: 'c_5bt_g.bin'; l: $1000; p: $2000;
    crc: $1C97D324), (n: 'c_5at_g.bin'; l: $1000; p: $3000; crc: $B9005AC0));
  dkong_pal: array [0 .. 2] of tipo_roms = ((n: 'c-2k.bpr'; l: $100; p: 0; crc: $E273EDE5), (n: 'c-2j.bpr'; l: $100; p: $100; crc: $D6412358), (n: 'v-5e.bpr'; l: $100; p: $200; crc: $B869B8F5));
  dkong_char: array [0 .. 1] of tipo_roms = ((n: 'v_5h_b.bin'; l: $800; p: 0; crc: $12C8C95D), (n: 'v_3pt.bin'; l: $800; p: $800; crc: $15E9C5E9));
  dkong_sprites: array [0 .. 3] of tipo_roms = ((n: 'l_4m_b.bin'; l: $800; p: 0; crc: $59F8054D), (n: 'l_4n_b.bin'; l: $800; p: $800; crc: $672E4714), (n: 'l_4r_b.bin'; l: $800; p: $1000;
    crc: $FEAA59EE), (n: 'l_4s_b.bin'; l: $800; p: $1800; crc: $20F2EF7E));
  dk_samples: array [0 .. 24] of tipo_nombre_samples = ((nombre: 'death.wav'), (nombre: 'tune01.wav'), (nombre: 'tune02.wav'), (nombre: 'tune03.wav'; restart: true), (nombre: 'tune04.wav';
    restart: false; loop: true), (nombre: 'tune05.wav'), (nombre: 'tune06.wav'), (nombre: 'tune07.wav'), (nombre: 'tune08_1.wav'), (nombre: 'tune08_2.wav'; restart: false; loop: true),
    (nombre: 'tune09_1.wav'), (nombre: 'tune09_2.wav'; restart: false; loop: true), (nombre: 'tune11_1.wav'), (nombre: 'tune11_2.wav'; restart: false; loop: true), (nombre: 'tune12.wav'),
    (nombre: 'tune13.wav'), (nombre: 'tune14.wav'), (nombre: 'tune15.wav'), (nombre: 'ef01_1.wav'), (nombre: 'ef01_2.wav'), (nombre: 'ef02.wav'), (nombre: 'ef03.wav'; restart: true),
    (nombre: 'ef04.wav'), (nombre: 'ef05.wav'), (nombre: 'ef06.wav'));
  dk_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '4'), (dip_val: $2; dip_name: '5'), (dip_val: $3;
    dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4; dip: ((dip_val: $0; dip_name: '7k'), (dip_val: $4; dip_name: '10k'), (dip_val: $8;
    dip_name: '15k'), (dip_val: $C; dip_name: '20k'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $70; name: 'Coinage'; number: 8;
    dip: ((dip_val: $70; dip_name: '5C 1C'), (dip_val: $50; dip_name: '4C 1C'), (dip_val: $30; dip_name: '3C 1C'), (dip_val: $10; dip_name: '2C 1C'), (dip_val: $0; dip_name: '1C 1C'), (dip_val: $20;
    dip_name: '1C 2C'), (dip_val: $40; dip_name: '1C 3C'), (dip_val: $40; dip_name: '1C 4C'), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Donkey Kong Jr.
  dkongjr_rom: array [0 .. 2] of tipo_roms = ((n: 'dkj.5b'; l: $2000; p: 0; crc: $DEA28158), (n: 'dkj.5c'; l: $2000; p: $2000; crc: $6FB5FAF6), (n: 'dkj.5e'; l: $2000; p: $4000; crc: $D042B6A8));
  dkongjr_pal: array [0 .. 2] of tipo_roms = ((n: 'c-2e.bpr'; l: $100; p: 0; crc: $463DC7AD), (n: 'c-2f.bpr'; l: $100; p: $100; crc: $47BA0042), (n: 'v-2n.bpr'; l: $100; p: $200; crc: $DBF185BF));
  dkongjr_char: array [0 .. 1] of tipo_roms = ((n: 'dkj.3n'; l: $1000; p: 0; crc: $8D51ACA9), (n: 'dkj.3p'; l: $1000; p: $1000; crc: $4EF64BA5));
  dkongjr_sprites: array [0 .. 3] of tipo_roms = ((n: 'v_7c.bin'; l: $800; p: 0; crc: $DC7F4164), (n: 'v_7d.bin'; l: $800; p: $800; crc: $0CE7DCF6), (n: 'v_7e.bin'; l: $800; p: $1000; crc: $24D1FF17),
    (n: 'v_7f.bin'; l: $800; p: $1800; crc: $0F8C083F));
  dkjr_samples: array [0 .. 21] of tipo_nombre_samples = ((nombre: 'death.wav'), (nombre: 'tune01.wav'; restart: false; loop: true), (nombre: 'tune02.wav'), (nombre: 'tune03.wav'),
    (nombre: 'tune04.wav'), (nombre: 'tune05.wav'), (nombre: 'tune06.wav'), (nombre: 'tune07.wav'), (nombre: 'tune08.wav'), (nombre: 'tune09.wav'), (nombre: 'tune10.wav'), (nombre: 'tune11.wav'),
    (nombre: 'tune12.wav'), (nombre: 'tune13.wav'), (nombre: 'tune14.wav'), (nombre: 'ef01.wav'; restart: true), (nombre: 'ef02.wav'; restart: true), (nombre: 'ef03.wav'; restart: true),
    (nombre: 'ef04.wav'), (nombre: 'ef05.wav'), (nombre: 'ef06.wav'), (nombre: 'ef07.wav'));
  // Donkey Kong 3
  dkong3_rom: array [0 .. 3] of tipo_roms = ((n: 'dk3c.7b'; l: $2000; p: 0; crc: $38D5F38E), (n: 'dk3c.7c'; l: $2000; p: $2000; crc: $C9134379), (n: 'dk3c.7d'; l: $2000; p: $4000; crc: $D22E2921),
    (n: 'dk3c.7e'; l: $2000; p: $8000; crc: $615F14B7));
  dkong3_pal: array [0 .. 2] of tipo_roms = ((n: 'dkc1-c.1d'; l: $200; p: 0; crc: $DF54BEFC), (n: 'dkc1-c.1c'; l: $200; p: $200; crc: $66A77F40), (n: 'dkc1-v.2n'; l: $100; p: $400; crc: $50E33434));
  dkong3_char: array [0 .. 1] of tipo_roms = ((n: 'dk3v.3n'; l: $1000; p: 0; crc: $415A99C7), (n: 'dk3v.3p'; l: $1000; p: $1000; crc: $25744EA0));
  dkong3_sprites: array [0 .. 3] of tipo_roms = ((n: 'dk3v.7c'; l: $1000; p: 0; crc: $8FFA1737), (n: 'dk3v.7d'; l: $1000; p: $1000; crc: $9AC84686), (n: 'dk3v.7e'; l: $1000; p: $2000; crc: $0C0AF3FB),
    (n: 'dk3v.7f'; l: $1000; p: $3000; crc: $55C58662));
  dkong3_snd1: tipo_roms = (n: 'dk3c.5l'; l: $2000; p: $E000; crc: $7FF88885);
  dkong3_snd2: tipo_roms = (n: 'dk3c.6h'; l: $2000; p: $E000; crc: $36D7200C);
  dk3_dip_a: array [0 .. 2] of def_dip = ((mask: $7; name: 'Coinage'; number: 8; dip: ((dip_val: $2; dip_name: '3C 1C'), (dip_val: $4; dip_name: '2C 1C'), (dip_val: $0;
    dip_name: '1C 1C'), (dip_val: $6; dip_name: '1C 2C'), (dip_val: $1; dip_name: '1C 3C'), (dip_val: $3; dip_name: '1C 4C'), (dip_val: $5; dip_name: '1C 5C'), (dip_val: $7;
    dip_name: '1C 6C'), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $80; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  dk3_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '3'), (dip_val: $1; dip_name: '4'), (dip_val: $2; dip_name: '5'), (dip_val: $3;
    dip_name: '6'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4; dip: ((dip_val: $0; dip_name: '30k'), (dip_val: $4; dip_name: '40k'), (dip_val: $8;
    dip_name: '50k'), (dip_val: $C; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Additinoal Bonus'; number: 4;
    dip: ((dip_val: $0; dip_name: '30k'), (dip_val: $10; dip_name: '40k'), (dip_val: $20; dip_name: '50k'), (dip_val: $30; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C0; name: 'Difficulty'; number: 4; dip: ((dip_val: $0; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Medium'), (dip_val: $80; dip_name: 'Hard'), (dip_val: $C0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  colores_char: array [0 .. $FF] of byte;
  haz_nmi: boolean;
  npaleta, latch1, latch2, latch3: byte;
  sprite_bank, char_bank: word;
  audio_tunes: procedure(valor: byte);
  audio_effects: procedure(direccion, valor: byte);
  // dkong
  tune01, tune08, tune09, tune11: byte;
  effect0, effect1, effect2: byte;

procedure update_video_dkong;
var
  f, color, nchar: word;
  x, y, atrib, atrib2: byte;
begin
  // Poner chars
  for f := $3FF downto 0 do
    if gfx[0].buffer[f] then
    begin
      y := f mod 32;
      x := f div 32;
      color := ((colores_char[y + 32 * (x shr 2)] and $F) shl 2) + (npaleta shl 6);
      nchar := memory[$7400 + f] + char_bank;
      put_gfx(248 - (x * 8), y * 8, nchar, color, 1, 0);
      gfx[0].buffer[f] := false;
    end;;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  // Sprites
  for f := 0 to $5F do
  begin
    atrib := memory[$7001 + (f * 4) + sprite_bank];
    atrib2 := memory[$7002 + (f * 4) + sprite_bank];
    color := ((atrib2 and $F) shl 2) + (npaleta shl 6);
    nchar := (atrib and $7F) + ((atrib2 and $40) shl 1);
    put_gfx_sprite(nchar, color, (atrib and $80) <> 0, (atrib2 and $80) <> 0, 1);
    x := memory[$7000 + (f * 4) + sprite_bank];
    y := memory[$7003 + (f * 4) + sprite_bank];
    update_gfx_sprite(x - 7, y - 8, 2, 1);
  end;
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_dkong;
begin
  if event.arcade then
  begin
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
    // SYS
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 or $4)
    else
      marcade.in2 := (marcade.in2 and $FB);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 or $8)
    else
      marcade.in2 := (marcade.in2 and $F7);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $80)
    else
      marcade.in2 := (marcade.in2 and $7F);
  end;
end;

procedure dkong_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          if haz_nmi then
            z80_0.change_nmi(PULSE_LINE);
          update_video_dkong;
        end;
      end;
      events_dkong;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function dkong_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $6BFF, $7000 .. $77FF:
      dkong_getbyte := memory[direccion];
    $7C00:
      dkong_getbyte := marcade.in0;
    $7C80:
      dkong_getbyte := marcade.in1;
    $7D00:
      dkong_getbyte := marcade.in2;
    $7D80:
      dkong_getbyte := marcade.dswa;
  end;
end;

procedure dkong_tune_sound(valor: byte);
begin
  case valor of
    1:
      begin
        stop_all_samples;
        start_sample(1);
      end;
    2:
      begin
        stop_all_samples;
        start_sample(2);
      end;
    3:
      start_sample(3);
    4:
      begin
        stop_sample(9);
        stop_sample(11);
        stop_sample(13);
        start_sample(4);
        tune08 := 0;
        tune09 := 0;
        tune11 := 0;
      end;
    5:
      begin
        stop_all_samples;
        start_sample(5);
      end;
    6:
      start_sample(6);
    7:
      begin
        stop_all_samples;
        start_sample(7);
      end;
    8:
      if tune08 = 0 then
      begin
        stop_all_samples;
        start_sample(8);
        tune08 := 1;
      end
      else
        start_sample(9);
    9:
      if tune09 = 0 then
      begin
        stop_all_samples;
        start_sample(10);
        tune09 := 1;
      end
      else
        start_sample(11);
    11:
      if tune11 = 0 then
      begin
        stop_all_samples;
        start_sample(12);
        tune11 := 1;
      end
      else
        start_sample(13);
    12:
      start_sample(14);
    13:
      start_sample(15);
    14:
      begin
        stop_sample(13);
        start_sample(16);
        tune11 := 0;
      end;
    15:
      start_sample(17);
  end;
end;

procedure dkong_effects_sound(direccion, valor: byte);
begin
  case direccion of
    $0:
      begin
        if ((effect0 = 0) and ((valor and 1) = 1)) then
          start_sample(18);
        if ((effect0 = 1) and ((valor and 1) = 0)) then
          start_sample(19);
        effect0 := valor and 1;
      end;
    $1:
      if (valor <> 0) then
        start_sample(20);
    $2:
      if (valor <> 0) then
        start_sample(21);
    $3:
      if (valor <> 0) then
        start_sample(22);
    $4:
      if (valor <> 0) then
        start_sample(23);
    $5:
      if (valor <> 0) then
        start_sample(24);
  end;
end;

procedure dkongjr_tune_sound(valor: byte);
begin
  case valor of
    1:
      if tune01 = 0 then
      begin
        stop_all_samples;
        tune01 := 1;
      end
      else
        start_sample(1);
    2:
      start_sample(2);
    3:
      start_sample(3);
    4:
      start_sample(4);
    5:
      start_sample(5);
    6:
      start_sample(6);
    7:
      start_sample(7);
    8:
      start_sample(8);
    9:
      begin
        stop_sample(1);
        start_sample(9);
        tune01 := 0;
      end;
    10:
      start_sample(10);
    11:
      begin
        stop_sample(3);
        start_sample(11);
      end;
    12:
      start_sample(12);
    13:
      start_sample(13);
    14:
      begin
        stop_sample(4);
        start_sample(14);
      end;
  end;
end;

procedure dkongjr_effects_sound(direccion, valor: byte);
begin
  case direccion of
    $0:
      begin
        if ((effect0 = 1) and ((valor and 1) = 0)) then
          start_sample(15);
        effect0 := valor and 1;
      end;
    $1:
      begin
        if ((effect1 = 1) and ((valor and 1) = 0)) then
          start_sample(16);
        effect1 := valor and 1;
      end;
    $2:
      begin
        if ((effect2 = 1) and ((valor and 1) = 0)) then
          start_sample(17);
        effect2 := valor and 1;
      end;
    $3:
      if (valor <> 0) then
        start_sample(18);
    $4:
      if (valor <> 0) then
        start_sample(19);
    $5:
      if (valor <> 0) then
        start_sample(20);
    $6:
      if (valor <> 0) then
        start_sample(21);
  end;

end;

procedure dkong_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF:
      ; // ROM
    $6000 .. $6BFF, $7000 .. $73FF:
      memory[direccion] := valor;
    $7400 .. $77FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $7C00:
      audio_tunes(valor);
    $7C80:
      if char_bank <> ((valor and 1) * $100) then
      begin
        fillchar(gfx[0].buffer[0], $400, 1);
        char_bank := (valor and 1) * $100;
      end;
    $7D00 .. $7D07:
      audio_effects(direccion and 7, valor);
    $7D80:
      if (valor <> 0) then
      begin // death
        stop_all_samples;
        start_sample(0);
      end;
    $7D82:
      main_screen.flip_main_screen := (valor and 1) = 0;
    $7D83:
      sprite_bank := $200 * (valor and 1);
    $7D84:
      haz_nmi := (valor = 1);
    $7D85:
      if (valor and 1) <> 0 then
        copymemory(@memory[$7000], @memory[$6900], $400);
    $7D86:
      if npaleta <> ((npaleta and 2) or (valor and 1)) then
      begin
        npaleta := (npaleta and 2) or (valor and 1);
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
    $7D87:
      if npaleta <> ((npaleta and 1) or ((valor and 1) shl 1)) then
      begin
        npaleta := (npaleta and 1) or ((valor and 1) shl 1);
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
  end;
end;

procedure dkong_sound_update;
begin
  samples_update;
end;

// Dkong 3
procedure events_dkong3;
begin
  if event.arcade then
  begin
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
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
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
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 or $20)
    else
      marcade.in1 := (marcade.in1 and $DF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 or $40)
    else
      marcade.in1 := (marcade.in1 and $BF);
  end;
end;

procedure dkong3_loop;
var
  frame_m, frame_s1, frame_s2: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := z80_0.tframes;
  frame_s1 := n2a03_0.m6502.tframes;
  frame_s2 := n2a03_1.m6502.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 263 do
      begin
        // Main CPU
        z80_0.run(frame_m);
        frame_m := frame_m + z80_0.tframes - z80_0.contador;
        // SND 1
        n2a03_0.m6502.run(frame_s1);
        frame_s1 := frame_s1 + n2a03_0.m6502.tframes - n2a03_0.m6502.contador;
        // SND 2
        n2a03_1.m6502.run(frame_s2);
        frame_s2 := frame_s2 + n2a03_1.m6502.tframes - n2a03_1.m6502.contador;
        if f = 239 then
        begin
          if haz_nmi then
          begin
            z80_0.change_nmi(PULSE_LINE);
            n2a03_0.m6502.change_nmi(PULSE_LINE);
            n2a03_1.m6502.change_nmi(PULSE_LINE);
          end;
          update_video_dkong;
        end;
      end;
      events_dkong3;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function dkong3_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $77FF, $8000 .. $9FFF:
      dkong3_getbyte := memory[direccion];
    $7C00:
      dkong3_getbyte := marcade.in0;
    $7C80:
      dkong3_getbyte := marcade.in1;
    $7D00:
      dkong3_getbyte := marcade.dswa;
    $7D80:
      dkong3_getbyte := marcade.dswb;
  end;
end;

procedure dkong3_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF, $8000 .. $9FFF:
      ; // ROM
    $6000 .. $73FF:
      memory[direccion] := valor;
    $7400 .. $77FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $7C00:
      latch1 := valor;
    $7C80:
      latch2 := valor;
    $7D00:
      latch3 := valor;
    $7D80:
      if ((valor and $1) <> 0) then
      begin
        n2a03_0.m6502.change_reset(CLEAR_LINE);
        n2a03_1.m6502.change_reset(CLEAR_LINE);
      end
      else
      begin
        n2a03_0.reset;
        n2a03_0.m6502.change_reset(ASSERT_LINE);
        n2a03_1.reset;
        n2a03_1.m6502.change_reset(ASSERT_LINE);
      end;
    $7E81:
      if char_bank <> ((not(valor) and 1) * $100) then
      begin
        fillchar(gfx[0].buffer[0], $400, 1);
        char_bank := (not(valor) and 1) * $100;
      end;
    $7E82:
      main_screen.flip_main_screen := (valor and 1) = 0;
    $7E83:
      sprite_bank := $200 * (valor and 1);
    $7E84:
      haz_nmi := (valor = 1);
    $7E85:
      if (valor and 1) <> 0 then
        copymemory(@memory[$7000], @memory[$6900], $400);
    $7E86:
      if npaleta <> ((npaleta and 2) or (valor and 1)) then
      begin
        npaleta := (npaleta and 2) or (valor and 1);
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
    $7E87:
      if npaleta <> ((npaleta and 1) or ((valor and 1) shl 1)) then
      begin
        npaleta := (npaleta and 1) or ((valor and 1) shl 1);
        fillchar(gfx[0].buffer[0], $400, 1);
      end;
  end;
end;

function dkong3_snd1_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FF, $E000 .. $FFFF:
      dkong3_snd1_getbyte := mem_snd[direccion];
    $4000 .. $4015:
      dkong3_snd1_getbyte := n2a03_0.read(direccion);
    $4016:
      dkong3_snd1_getbyte := latch1;
    $4017:
      dkong3_snd1_getbyte := latch2;
  end;
end;

procedure dkong3_snd1_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $4000 .. $4017:
      n2a03_0.write(direccion, valor);
    $E000 .. $FFFF:
      ; // ROM
  end;
end;

function dkong3_snd2_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $7FF, $E000 .. $FFFF:
      dkong3_snd2_getbyte := mem_misc[direccion];
    $4000 .. $4015, $4017:
      dkong3_snd2_getbyte := n2a03_1.read(direccion);
    $4016:
      dkong3_snd2_getbyte := latch3;
  end;
end;

procedure dkong3_snd2_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_misc[direccion] := valor;
    $4000 .. $4017:
      n2a03_1.write(direccion, valor);
    $E000 .. $FFFF:
      ; // ROM
  end;
end;

// Main
procedure reset_dkong;
begin
  z80_0.reset;
  case main_vars.machine_type of
    15:
      begin
        marcade.in2 := 0;
        reset_samples;
      end;
    168:
      begin
        marcade.in2 := $40;
        reset_samples;
      end;
    169:
      begin
        marcade.in2 := 0;
        n2a03_0.reset;
        n2a03_1.reset;
      end;
  end;
 reset_video;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  haz_nmi := false;
  npaleta := 0;
  sprite_bank := 0;
  char_bank := 0;
  tune08 := 0;
  tune09 := 0;
  tune11 := 0;
  effect0 := 0;
  effect1 := 0;
  effect2 := 0;
  latch1 := 0;
  latch2 := 0;
  latch3 := 0;
end;

function start_donkeykong: boolean;
var
  memory_temp: array [0 .. $5FFF] of byte;
const
  ps_dkong_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 64 * 16 * 16 + 0, 64 * 16 * 16 + 1, 64 * 16 * 16 + 2, 64 * 16 * 16 + 3, 64 * 16 * 16 + 4, 64 * 16 * 16 + 5, 64 * 16 * 16 + 6,
    64 * 16 * 16 + 7);
  ps_dkong3_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 128 * 16 * 16 + 0, 128 * 16 * 16 + 1, 128 * 16 * 16 + 2, 128 * 16 * 16 + 3, 128 * 16 * 16 + 4, 128 * 16 * 16 + 5, 128 * 16 * 16 + 6,
    128 * 16 * 16 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);

  procedure dkong_char_load(num_char: word);
  begin
    init_gfx(0, 8, 8, num_char);
    gfx_set_desc_data(2, 0, 8 * 8, num_char * 8 * 8, 0);
    convert_gfx(0, 0, @memory_temp, @ps_dkong_x, @ps_y, true, false);
  end;
  procedure dkong_sprites_load(num_spr: word);
  begin
    init_gfx(1, 16, 16, num_spr);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(2, 0, 16 * 8, num_spr * 16 * 16, 0);
    case main_vars.machine_type of
      15, 168:
        convert_gfx(1, 0, @memory_temp, @ps_dkong_x, @ps_y, true, false);
      169:
        convert_gfx(1, 0, @memory_temp, @ps_dkong3_x, @ps_y, true, false);
    end;
  end;
  procedure pal_dkong;
  var
    f, ctemp1, ctemp2: byte;
    colores: tpaleta;
  begin
    for f := 0 to 255 do
    begin
      ctemp1 := memory_temp[f + $100];
      ctemp2 := memory_temp[f];
      colores[f].r := not(($21 * ((ctemp1 shr 1) and 1)) + ($47 * ((ctemp1 shr 2) and 1)) + ($97 * ((ctemp1 shr 3) and 1)));
      colores[f].g := not(($21 * ((ctemp2 shr 2) and 1)) + ($47 * ((ctemp2 shr 3) and 1)) + ($97 * (ctemp1 and 1)));
      colores[f].b := not(($55 * (ctemp2 and 1)) + ($AA * ((ctemp2 shr 1) and 1)));
    end;
    set_pal(colores, 256);
    copymemory(@colores_char[0], @memory_temp[$200], $100);
  end;
  procedure pal_dkong3;
  var
    f, ctemp1, ctemp2: byte;
    colores: tpaleta;
  begin
    for f := 0 to 255 do
    begin
      ctemp1 := memory_temp[f];
      ctemp2 := memory_temp[f + $200];
      colores[f].r := not($0E * ((ctemp1 shr 4) and 1) + $1F * ((ctemp1 shr 5) and 1) + $43 * ((ctemp1 shr 6) and 1) + $8F * ((ctemp1 shr 7) and 1));
      colores[f].g := not($0E * ((ctemp1 shr 0) and 1) + $1F * ((ctemp1 shr 1) and 1) + $43 * ((ctemp1 shr 2) and 1) + $8F * ((ctemp1 shr 3) and 1));
      colores[f].b := not($0E * ((ctemp2 shr 0) and 1) + $1F * ((ctemp2 shr 1) and 1) + $43 * ((ctemp2 shr 2) and 1) + $8F * ((ctemp2 shr 3) and 1));
    end;
    set_pal(colores, 256);
    copymemory(@colores_char[0], @memory_temp[$400], $100);
  end;

begin
  machine_calls.reset := reset_dkong;
  machine_calls.fps_max := 60.6060606060606060;
  start_donkeykong := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, false, true);
  start_video(224, 256);
  case main_vars.machine_type of
    15:
      begin // Donkey Kong
        machine_calls.general_loop := dkong_loop;
        // Main CPU
        z80_0 := cpu_z80.create(3072000, 264);
        z80_0.change_ram_calls(dkong_getbyte, dkong_putbyte);
        // cargar roms
        if not(roms_load(@memory, dkong_rom)) then
          exit;
        // samples
        if load_samples(dk_samples) then
          z80_0.init_sound(dkong_sound_update);
        audio_tunes := dkong_tune_sound;
        audio_effects := dkong_effects_sound;
        // convertir chars
        if not(roms_load(@memory_temp, dkong_char)) then
          exit;
        dkong_char_load($100);
        // convertir sprites
        if not(roms_load(@memory_temp, dkong_sprites)) then
          exit;
        dkong_sprites_load($80);
        // poner la paleta
        if not(roms_load(@memory_temp, dkong_pal)) then
          exit;
        pal_dkong;
        // DIP
        marcade.dswa := $80;
        marcade.dswa_val := @dk_dip_a;
      end;
    168:
      begin // Donkey Kong Jr.
        machine_calls.general_loop := dkong_loop;
        // Main CPU
        z80_0 := cpu_z80.create(3072000, 264);
        z80_0.change_ram_calls(dkong_getbyte, dkong_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, dkongjr_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp[0], $1000);
        copymemory(@memory[$3000], @memory_temp[$1000], $1000);
        copymemory(@memory[$2000], @memory_temp[$2000], $800);
        copymemory(@memory[$4800], @memory_temp[$2800], $800);
        copymemory(@memory[$1000], @memory_temp[$3000], $800);
        copymemory(@memory[$5800], @memory_temp[$3800], $800);
        copymemory(@memory[$4000], @memory_temp[$4000], $800);
        copymemory(@memory[$2800], @memory_temp[$4800], $800);
        copymemory(@memory[$5000], @memory_temp[$5000], $800);
        copymemory(@memory[$1800], @memory_temp[$5800], $800);
        // samples
        if load_samples(dkjr_samples) then
          z80_0.init_sound(dkong_sound_update);
        audio_tunes := dkongjr_tune_sound;
        audio_effects := dkongjr_effects_sound;
        // convertir chars
        if not(roms_load(@memory_temp, dkongjr_char)) then
          exit;
        dkong_char_load($200);
        // convertir sprites
        if not(roms_load(@memory_temp, dkongjr_sprites)) then
          exit;
        dkong_sprites_load($80);
        // poner la paleta
        if not(roms_load(@memory_temp, dkongjr_pal)) then
          exit;
        pal_dkong;
        // DIP
        marcade.dswa := $80;
        marcade.dswa_val := @dk_dip_a;
      end;
    169:
      begin // Donkey Kong 3
        machine_calls.general_loop := dkong3_loop;
        // Main CPU
        z80_0 := cpu_z80.create(4000000, 264);
        z80_0.change_ram_calls(dkong3_getbyte, dkong3_putbyte);
        // cargar roms
        if not(roms_load(@memory, dkong3_rom)) then
          exit;
        // sound 1
        if not(roms_load(@mem_snd, dkong3_snd1)) then
          exit;
        n2a03_0 := cpu_n2a03.create(1789772, 264);
        n2a03_0.m6502.change_ram_calls(dkong3_snd1_getbyte, dkong3_snd1_putbyte);
        // sound 2
        if not(roms_load(@mem_misc, dkong3_snd2)) then
          exit;
        n2a03_1 := cpu_n2a03.create(1789772, 264);
        n2a03_1.m6502.change_ram_calls(dkong3_snd2_getbyte, dkong3_snd2_putbyte);
        // convertir chars
        if not(roms_load(@memory_temp, dkong3_char)) then
          exit;
        dkong_char_load($200);
        // convertir sprites
        if not(roms_load(@memory_temp, dkong3_sprites)) then
          exit;
        dkong_sprites_load($100);
        // poner la paleta
        if not(roms_load(@memory_temp, dkong3_pal)) then
          exit;
        pal_dkong3;
        // DIP
        marcade.dswa := $0;
        marcade.dswa_val := @dk3_dip_a;
        marcade.dswb := $0;
        marcade.dswb_val := @dk3_dip_b;
      end;
  end;
  // final
  reset_dkong;
  start_donkeykong := true;
end;

end.
