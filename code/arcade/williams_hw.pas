unit williams_hw;

interface

uses
  WinApi.Windows,
  m6809,
  m680x,
  main_engine,
  controls_engine,
  gfx_engine,
  dac,
  rom_engine,
  pal_engine,
  sound_engine,
  pia6821,
  file_engine,
  blitter_williams;

function start_williams: boolean;

implementation

const
  // Defender
  defender_rom: array [0 .. 10] of tipo_roms = ((n: 'defend.1'; l: $800; p: $0; crc: $C3E52D7E),
    (n: 'defend.4'; l: $800; p: $800; crc: $9A72348B), (n: 'defend.2'; l: $1000; p: $1000;
    crc: $89B75984), (n: 'defend.3'; l: $1000; p: $2000; crc: $94F51E9B), (n: 'defend.9'; l: $800;
    p: $3000; crc: $6870E8A5), (n: 'defend.12'; l: $800; p: $3800; crc: $F1F88938), (n: 'defend.8';
    l: $800; p: $4000; crc: $B649E306), (n: 'defend.11'; l: $800; p: $4800; crc: $9DEAF6D9),
    (n: 'defend.7'; l: $800; p: $5000; crc: $339E092E), (n: 'defend.10'; l: $800; p: $5800;
    crc: $A543B167), (n: 'defend.6'; l: $800; p: $9000; crc: $65F4EFD1));
  defender_snd: tipo_roms = (n: 'defend.snd'; l: $800; p: $F800; crc: $FEFD5B48);
  // Mayday
  mayday_rom: array [0 .. 6] of tipo_roms = ((n: 'mayday.c'; l: $1000; p: $0; crc: $A1FF6E62),
    (n: 'mayday.b'; l: $1000; p: $1000; crc: $62183AEA), (n: 'mayday.a'; l: $1000; p: $2000;
    crc: $5DCB113F), (n: 'mayday.d'; l: $1000; p: $3000; crc: $EA6A4EC8), (n: 'mayday.e'; l: $1000;
    p: $4000; crc: $0D797A3E), (n: 'mayday.f'; l: $1000; p: $5000; crc: $EE8BFCD6), (n: 'mayday.g';
    l: $1000; p: $9000; crc: $D9C065E7));
  mayday_snd: tipo_roms = (n: 'ic28-8.bin'; l: $800; p: $F800; crc: $FEFD5B48);
  // Colony7
  colony7_rom: array [0 .. 8] of tipo_roms = ((n: 'cs03.bin'; l: $1000; p: $0; crc: $7EE75AE5),
    (n: 'cs02.bin'; l: $1000; p: $1000; crc: $C60B08CB), (n: 'cs01.bin'; l: $1000; p: $2000;
    crc: $1BC97436), (n: 'cs06.bin'; l: $800; p: $3000; crc: $318B95AF), (n: 'cs04.bin'; l: $800;
    p: $3800; crc: $D740FAEE), (n: 'cs07.bin'; l: $800; p: $4000; crc: $0B23638B), (n: 'cs05.bin';
    l: $800; p: $4800; crc: $59E406A8), (n: 'cs08.bin'; l: $800; p: $5000; crc: $3BFDE87A),
    (n: 'cs08.bin'; l: $800; p: $5800; crc: $3BFDE87A));
  colony7_snd: tipo_roms = (n: 'cs11.bin'; l: $800; p: $F800; crc: $6032293C);
  colony7_dip_a: array [0 .. 2] of def_dip = ((mask: $1; name: 'Lives'; number: 2;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $1; dip_name: '3'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $2; name: 'Bonus At'; number: 2;
    dip: ((dip_val: $0; dip_name: '20K/40K or 30K/50K'), (dip_val: $2;
    dip_name: '30K/50K or 40K/70K'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // Joust
  joust_rom: array [0 .. 11] of tipo_roms = ((n: 'joust_rom_10b_3006-22.a7'; l: $1000; p: $0;
    crc: $3F1C4F89), (n: 'joust_rom_11b_3006-23.c7'; l: $1000; p: $1000; crc: $EA48B359),
    (n: 'joust_rom_12b_3006-24.e7'; l: $1000; p: $2000; crc: $C710717B),
    (n: 'joust_rom_1b_3006-13.e4'; l: $1000; p: $3000; crc: $FE41B2AF),
    (n: 'joust_rom_2b_3006-14.c4'; l: $1000; p: $4000; crc: $501C143C),
    (n: 'joust_rom_3b_3006-15.a4'; l: $1000; p: $5000; crc: $43F7161D),
    (n: 'joust_rom_4b_3006-16.e5'; l: $1000; p: $6000; crc: $DB5571B6),
    (n: 'joust_rom_5b_3006-17.c5'; l: $1000; p: $7000; crc: $C686BB6B),
    (n: 'joust_rom_6b_3006-18.a5'; l: $1000; p: $8000; crc: $FAC5F2CF),
    (n: 'joust_rom_7b_3006-19.e6'; l: $1000; p: $9000; crc: $81418240),
    (n: 'joust_rom_8b_3006-20.c6'; l: $1000; p: $A000; crc: $BA5359BA),
    (n: 'joust_rom_9b_3006-21.a6'; l: $1000; p: $B000; crc: $39643147));
  joust_snd: tipo_roms = (n: 'video_sound_rom_4_std_780.ic12'; l: $1000; p: $F000; crc: $F1835BDD);
  // Robotron
  robotron_rom: array [0 .. 11] of tipo_roms = ((n: '2084_rom_10b_3005-22.a7'; l: $1000; p: $0;
    crc: $13797024), (n: '2084_rom_11b_3005-23.c7'; l: $1000; p: $1000; crc: $7E3C1B87),
    (n: '2084_rom_12b_3005-24.e7'; l: $1000; p: $2000; crc: $645D543E),
    (n: '2084_rom_1b_3005-13.e4'; l: $1000; p: $3000; crc: $66C7D3EF), (n: '2084_rom_2b_3005-14.c4';
    l: $1000; p: $4000; crc: $5BC6C614), (n: '2084_rom_3b_3005-15.a4'; l: $1000; p: $5000;
    crc: $E99A82BE), (n: '2084_rom_4b_3005-16.e5'; l: $1000; p: $6000; crc: $AFB1C561),
    (n: '2084_rom_5b_3005-17.c5'; l: $1000; p: $7000; crc: $62691E77), (n: '2084_rom_6b_3005-18.a5';
    l: $1000; p: $8000; crc: $BD2C853D), (n: '2084_rom_7b_3005-19.e6'; l: $1000; p: $9000;
    crc: $49AC400C), (n: '2084_rom_8b_3005-20.c6'; l: $1000; p: $A000; crc: $3A96E88C),
    (n: '2084_rom_9b_3005-21.a6'; l: $1000; p: $B000; crc: $B124367B));
  robotron_snd: tipo_roms = (n: 'video_sound_rom_3_std_767.ic12'; l: $1000; p: $F000;
    crc: $C56C1D28);
  // Stargate
  stargate_rom: array [0 .. 11] of tipo_roms = ((n: 'stargate_rom_10-a_3002-10.a7'; l: $1000; p: $0;
    crc: $60B07FF7), (n: 'stargate_rom_11-a_3002-11.c7'; l: $1000; p: $1000; crc: $7D2C5DAF),
    (n: 'stargate_rom_12-a_3002-12.e7'; l: $1000; p: $2000; crc: $A0396670),
    (n: 'stargate_rom_1-a_3002-1.e4'; l: $1000; p: $3000; crc: $88824D18),
    (n: 'stargate_rom_2-a_3002-2.c4'; l: $1000; p: $4000; crc: $AFC614C5),
    (n: 'stargate_rom_3-a_3002-3.a4'; l: $1000; p: $5000; crc: $15077A9D),
    (n: 'stargate_rom_4-a_3002-4.e5'; l: $1000; p: $6000; crc: $A8B4BF0F),
    (n: 'stargate_rom_5-a_3002-5.c5'; l: $1000; p: $7000; crc: $2D306074),
    (n: 'stargate_rom_6-a_3002-6.a5'; l: $1000; p: $8000; crc: $53598DDE),
    (n: 'stargate_rom_7-a_3002-7.e6'; l: $1000; p: $9000; crc: $23606060),
    (n: 'stargate_rom_8-a_3002-8.c6'; l: $1000; p: $A000; crc: $4EC490C7),
    (n: 'stargate_rom_9-a_3002-9.a6'; l: $1000; p: $B000; crc: $88187B64));
  stargate_snd: tipo_roms = (n: 'video_sound_rom_2_std_744.ic12'; l: $800; p: $F800;
    crc: $2FCF6C4D);
  CPU_SYNC = 8;

var
  ram_bank, sound_latch, xoff: byte;
  rom_data: array [0 .. $F, 0 .. $FFF] of byte;
  nvram: array [0 .. $3FF] of byte;
  linea: word;
  palette: array [0 .. $F] of word;
  pal_lookup: array [0 .. $FF] of word;
  events_call: procedure;
  // joust
  ram_rom_set: boolean;
  player: boolean;

procedure update_video_williams(linea: word);
var
  x, pix: word;
  puntos: array [0 .. 303] of word;
begin
  if linea > 247 then
    exit;
  for x := 0 to 151 do
  begin
    pix := memory[linea + (x * 256)];
    puntos[x * 2] := pal_lookup[palette[pix shr 4]];
    puntos[(x * 2) + 1] := pal_lookup[palette[pix and $F]];
  end;
  putpixel(0 + ADD_SPRITE, linea + ADD_SPRITE, 304, @puntos, 1);
end;

procedure events_defender;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.but3[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.but4[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
  end;
end;

procedure events_mayday;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.but3[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
  end;
end;

procedure events_colony7;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
  end;
end;

procedure events_joust;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 or $4)
    else
      marcade.in1 := (marcade.in1 and $FB);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in3 := (marcade.in3 or $1)
    else
      marcade.in3 := (marcade.in3 and $FE);
    if p_contrls.map_arcade.right[1] then
      marcade.in3 := (marcade.in3 or $2)
    else
      marcade.in3 := (marcade.in3 and $FD);
    if p_contrls.map_arcade.but0[1] then
      marcade.in3 := (marcade.in3 or $4)
    else
      marcade.in3 := (marcade.in3 and $FB);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
  end;
end;

procedure events_robotron;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.up[1] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.down[1] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
  end;
end;

procedure events_stargate;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 or $1)
    else
      marcade.in0 := (marcade.in0 and $FE);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 or $2)
    else
      marcade.in0 := (marcade.in0 and $FD);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 or $4)
    else
      marcade.in0 := (marcade.in0 and $FB);
    if p_contrls.map_arcade.but5[0] then
      marcade.in0 := (marcade.in0 or $8)
    else
      marcade.in0 := (marcade.in0 and $F7);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $10)
    else
      marcade.in0 := (marcade.in0 and $EF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $20)
    else
      marcade.in0 := (marcade.in0 and $DF);
    if p_contrls.map_arcade.but3[0] then
      marcade.in0 := (marcade.in0 or $40)
    else
      marcade.in0 := (marcade.in0 and $BF);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 or $80)
    else
      marcade.in0 := (marcade.in0 and $7F);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or $1)
    else
      marcade.in1 := (marcade.in1 and $FE);
    if p_contrls.map_arcade.but4[0] then
      marcade.in1 := (marcade.in1 or $2)
    else
      marcade.in1 := (marcade.in1 and $FD);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 or $10)
    else
      marcade.in2 := (marcade.in2 and $EF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 or $20)
    else
      marcade.in2 := (marcade.in2 and $DF);
  end;
end;

procedure williams_loop;
var
  frame_m, frame_s: single;
  h: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := m6800_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for linea := 0 to 259 do
      begin
        for h := 1 to CPU_SYNC do
        begin
          // main
          m6809_0.run(frame_m);
          frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
          // snd
          m6800_0.run(frame_s);
          frame_s := frame_s + m6800_0.tframes - m6800_0.contador;
        end;
        update_video_williams(linea);
        case linea of
          0, 32, 64, 96, 128, 160, 192, 224:
            pia6821_1.cb1_w((linea and $20) <> 0);
          239:
            begin
              update_final_piece(xoff, 7, 292, 240, 1);
              pia6821_1.ca1_w(true);
            end;
          240:
            pia6821_1.ca1_w(false);
        end;
      end;
      events_call;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function williams_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $BFFF, $D000 .. $FFFF:
      williams_getbyte := memory[direccion];
    $C000 .. $CFFF:
      case ram_bank of
        0:
          case (direccion and $FFF) of
            $400 .. $7FF:
              williams_getbyte := nvram[direccion and $FF];
            $800 .. $BFF:
              if (linea < $100) then
                williams_getbyte := linea and $FC
              else
                williams_getbyte := $FC;
            $C00 .. $FFF:
              case (direccion and $1F) of
                0 .. 3:
                  williams_getbyte := pia6821_1.read(direccion and $3);
                4 .. 7:
                  williams_getbyte := pia6821_0.read(direccion and $3);
              end;
          end;
        1 .. $F:
          williams_getbyte := rom_data[ram_bank - 1, direccion and $FFF];
      end;
  end;
end;

procedure williams_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $BFFF:
      memory[direccion] := valor;
    $C000 .. $CFFF:
      case ram_bank of
        0:
          case (direccion and $FFF) of
            0 .. $3FE:
              case (direccion and $1F) of
                0 .. $F:
                  palette[direccion and $F] := valor;
                $10 .. $1F:
                  ; // m_cocktail
              end;
            $3FF:
              ; // Watch dog
            $400 .. $7FF:
              nvram[direccion and $FF] := $F0 or valor;
            $C00 .. $FFF:
              case (direccion and $1F) of
                0 .. 3:
                  pia6821_1.write(direccion and $3, valor);
                4 .. 7:
                  pia6821_0.write(direccion and $3, valor);
              end;
          end;
        $1 .. $F:
          ;
      end;
    $D000 .. $DFFF:
      ram_bank := valor and $F;
    $E000 .. $FFFF:
      ;
  end;
end;

function williams_snd_getbyte(direccion: word): byte;
begin
case direccion of
  $0..$ff,$b000..$ffff:williams_snd_getbyte:=mem_snd[direccion];
  $400..$403,$8400..$8403:williams_snd_getbyte:=pia6821_2.read(direccion and $3);
end;
end;

procedure williams_snd_putbyte(direccion: word; valor: byte);
begin
case direccion of
  $0..$ff:mem_snd[direccion]:=valor;
  $400..$403,$8400..$8403:pia6821_2.write(direccion and $3,valor);
  $b000..$ffff:;
end;
end;

procedure main_irq(state: boolean);
begin
  if (pia6821_1.irq_a_state or pia6821_1.irq_b_state) then
    m6809_0.change_irq(ASSERT_LINE)
  else
    m6809_0.change_irq(CLEAR_LINE);
end;

procedure snd_irq(state: boolean);
begin
  if (pia6821_2.irq_a_state or pia6821_2.irq_b_state) then
    m6800_0.change_irq(ASSERT_LINE)
  else
    m6800_0.change_irq(CLEAR_LINE);
end;

procedure snd_write_dac(valor: byte);
begin
  dac_0.data8_w(valor);
end;

procedure williams_sound;
begin
  dac_0.update;
end;

procedure sound_write(valor: byte);
begin
  sound_latch := valor or $C0;
  pia6821_2.portb_w(sound_latch);
  pia6821_2.cb1_w(not(sound_latch = $FF));
end;

function get_in0: byte;
begin
  get_in0 := marcade.in0;
end;

function get_in1: byte;
begin
  get_in1 := marcade.in1;
end;

function get_in2: byte;
begin
  get_in2 := marcade.in2 + marcade.dswa;
end;

// Mayday
function mayday_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $A192, $A195 .. $BFFF, $D000 .. $FFFF:
      mayday_getbyte := memory[direccion];
    $A193:
      mayday_getbyte := memory[$A190]; // Proteccion 1
    $A194:
      mayday_getbyte := memory[$A191]; // Proteccion 2
    $C000 .. $CFFF:
      case ram_bank of
        0:
          case (direccion and $FFF) of
            $400 .. $7FF:
              mayday_getbyte := nvram[direccion and $FF];
            $800 .. $BFF:
              if (linea < $100) then
                mayday_getbyte := linea and $FC
              else
                mayday_getbyte := $FC;
            $C00 .. $FFF:
              case (direccion and $1F) of
                0 .. 3:
                  mayday_getbyte := pia6821_1.read(direccion and $3);
                4 .. 7:
                  mayday_getbyte := pia6821_0.read(direccion and $3);
              end;
          end;
        1 .. $F:
          mayday_getbyte := rom_data[ram_bank - 1, direccion and $FFF];
      end;
  end;
end;

// Joust
function joust_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $8FFF:
      if ram_rom_set then
        joust_getbyte := rom_data[direccion shr 12, direccion and $FFF]
      else
        joust_getbyte := memory[direccion];
    $9000 .. $BFFF, $D000 .. $FFFF:
      joust_getbyte := memory[direccion];
    $C800 .. $C8FF:
      case (direccion and $F) of
        4 .. 7:
          joust_getbyte := pia6821_0.read(direccion and $3);
        $C .. $F:
          joust_getbyte := pia6821_1.read(direccion and $3);
      end;
    $CB00 .. $CBFF:
      if (linea < $100) then
        joust_getbyte := linea and $FC
      else
        joust_getbyte := $FC;
    $CC00 .. $CFFF:
      joust_getbyte := nvram[direccion and $3FF];
  end;
end;

procedure joust_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $BFFF:
      memory[direccion] := valor;
    $C000 .. $C3FF:
      palette[direccion and $F] := valor;
    $C800 .. $C8FF:
      case (direccion and $F) of
        4 .. 7:
          pia6821_0.write(direccion and $3, valor);
        $C .. $F:
          pia6821_1.write(direccion and $3, valor);
      end;
    $C900 .. $C9FF:
      ram_rom_set := (valor and 1) <> 0;
    $CA00 .. $CAFF:
      blitter_0.blitter_w(direccion and 7, valor);
    $CBFF:
      ; // watchdog
    $CC00 .. $CFFF:
      nvram[direccion and $3FF] := $F0 or valor;
    $D000 .. $FFFF:
      ;
  end;
end;

function joust_in0: byte;
begin
  if player then
    joust_in0 := marcade.in0 or marcade.in1
  else
    joust_in0 := marcade.in0 or marcade.in3;
end;

function joust_in1: byte;
begin
  joust_in1 := 0;
end;

procedure joust_cb2(state: boolean);
begin
  player := state;
end;

// Main
procedure reset_williams;
begin
  m6809_0.reset;
  m6800_0.reset;
  pia6821_0.reset;
  pia6821_1.reset;
  pia6821_2.reset;
  if (main_vars.machine_type = 321) or (main_vars.machine_type = 322) or
    (main_vars.machine_type = 323) then
    blitter_0.reset;
  dac_0.reset;
  reset_audio;
  marcade.in0 := 0;
  marcade.in1 := 0;
  marcade.in2 := 0;
  marcade.in3 := 0;
  ram_bank := 0;
  sound_latch := 0;
  ram_rom_set := false;
  player := false;
end;

procedure close_williams;
begin
  case main_vars.machine_type of
    246:
      write_file(Directory.Arcade_nvram + 'defender.nv', @nvram, $100);
    248:
      write_file(Directory.Arcade_nvram + 'mayday.nv', @nvram, $100);
    249:
      write_file(Directory.Arcade_nvram + 'colony7.nv', @nvram, $100);
    321:
      write_file(Directory.Arcade_nvram + 'joust.nv', @nvram, $400);
    322:
      write_file(Directory.Arcade_nvram + 'robotron.nv', @nvram, $400);
    323:
      write_file(Directory.Arcade_nvram + 'stargate.nv', @nvram, $400);
  end;
end;

function start_williams: boolean;
const
  resistances: array [0 .. 2] of integer = (1200, 560, 330);
var
  color: tcolor;
  f: byte;
  memory_temp: array [0 .. $FFFF] of byte;
  rweights, gweights, bweights: array [0 .. 2] of single;
  longitud: integer;
begin
  machine_calls.general_loop := williams_loop;
  machine_calls.reset := reset_williams;
  machine_calls.close := close_williams;
  machine_calls.fps_max := 60.096154;
  start_williams := false;
  start_audio(false);
  if main_vars.machine_type = 249 then
    main_screen.rot270_screen := true;
  screen_init(1, 304, 247, false, true);
  start_video(292, 240);
  // Main CPU
  m6809_0 := cpu_m6809.Create(12000000 div 3 div 4, 260 * CPU_SYNC, TCPU_MC6809E);
  // Sound CPU
  m6800_0 := cpu_m6800.Create(3579545, 260 * CPU_SYNC, TCPU_M6808);
  m6800_0.change_ram_calls(williams_snd_getbyte, williams_snd_putbyte);
  m6800_0.init_sound(williams_sound);
  // Misc
  pia6821_0 := pia6821_chip.Create;
  pia6821_0.change_in_out(get_in0, get_in1, nil, nil);
  pia6821_1 := pia6821_chip.Create;
  pia6821_1.change_in_out(get_in2, nil, nil, sound_write);
  pia6821_1.change_irq(main_irq, main_irq);
  pia6821_2 := pia6821_chip.Create;
  pia6821_2.change_in_out(nil, nil, snd_write_dac, nil);
  pia6821_2.change_irq(snd_irq, snd_irq);
  // Sound Chip
  dac_0 := dac_chip.Create;
  marcade.dswa := 0;
  xoff := 12;
  case main_vars.machine_type of
    246:
      begin // defender
        m6809_0.change_ram_calls(williams_getbyte, williams_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, defender_rom)) then
          exit;
        copymemory(@memory[$D000], @memory_temp[0], $3000);
        for f := 0 to 7 do
          copymemory(@rom_data[f, 0], @memory_temp[$3000 + (f * $1000)], $1000);
        // roms sonido
        if not(roms_load(@mem_snd, defender_snd)) then
          exit;
        events_call := events_defender;
        // Cargar NVRam
        if read_file_size(Directory.Arcade_nvram + 'defender.nv', longitud) then
          read_file(Directory.Arcade_nvram + 'defender.nv', @nvram, longitud);
      end;
    248:
      begin // mayday
        m6809_0.change_ram_calls(mayday_getbyte, williams_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, mayday_rom)) then
          exit;
        copymemory(@memory[$D000], @memory_temp[0], $3000);
        for f := 0 to 7 do
          copymemory(@rom_data[f, 0], @memory_temp[$3000 + (f * $1000)], $1000);
        // roms sonido
        if not(roms_load(@mem_snd, mayday_snd)) then
          exit;
        events_call := events_mayday;
        // Cargar NVRam
        if read_file_size(Directory.Arcade_nvram + 'mayday.nv', longitud) then
          read_file(Directory.Arcade_nvram + 'mayday.nv', @nvram, longitud);
      end;
    249:
      begin // colony 7
        m6809_0.change_ram_calls(williams_getbyte, williams_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, colony7_rom)) then
          exit;
        copymemory(@memory[$D000], @memory_temp[0], $3000);
        for f := 0 to 7 do
          copymemory(@rom_data[f, 0], @memory_temp[$3000 + (f * $1000)], $1000);
        // roms sonido
        if not(roms_load(@mem_snd, colony7_snd)) then
          exit;
        events_call := events_colony7;
        marcade.dswa := $1;
        marcade.dswa_val := @colony7_dip_a;
        // Cargar NVRam
        if read_file_size(Directory.Arcade_nvram + 'colony7.nv', longitud) then
          read_file(Directory.Arcade_nvram + 'colony7.nv', @nvram, longitud);
      end;
    321:
      begin // joust
        m6809_0.change_ram_calls(joust_getbyte, joust_putbyte);
        pia6821_0.change_in_out(joust_in0, joust_in1, nil, nil);
        pia6821_0.change_cb(joust_cb2);
        // cargar roms
        if not(roms_load(@memory_temp, joust_rom)) then
          exit;
        copymemory(@memory[$D000], @memory_temp[0], $3000);
        for f := 0 to 8 do
          copymemory(@rom_data[f, 0], @memory_temp[$3000 + (f * $1000)], $1000);
        // roms sonido
        if not(roms_load(@mem_snd, joust_snd)) then
          exit;
        events_call := events_joust;
        blitter_0 := williams_blitter.Create(4, false, $C000);
        blitter_0.set_read_write(joust_getbyte, joust_putbyte);
        xoff := 6;
        // Cargar NVRam
        if read_file_size(Directory.Arcade_nvram + 'joust.nv', longitud) then
          read_file(Directory.Arcade_nvram + 'joust.nv', @nvram, longitud);
      end;
    322:
      begin // robotron
        m6809_0.change_ram_calls(joust_getbyte, joust_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, robotron_rom)) then
          exit;
        copymemory(@memory[$D000], @memory_temp[0], $3000);
        for f := 0 to 8 do
          copymemory(@rom_data[f, 0], @memory_temp[$3000 + (f * $1000)], $1000);
        // roms sonido
        if not(roms_load(@mem_snd, robotron_snd)) then
          exit;
        events_call := events_robotron;
        blitter_0 := williams_blitter.Create(4, false, $C000);
        blitter_0.set_read_write(joust_getbyte, joust_putbyte);
        xoff := 6;
        // Cargar NVRam
        if read_file_size(Directory.Arcade_nvram + 'robotron.nv', longitud) then
          read_file(Directory.Arcade_nvram + 'robotron.nv', @nvram, longitud);
      end;
    323:
      begin // stargate
        m6809_0.change_ram_calls(joust_getbyte, joust_putbyte);
        // cargar roms
        if not(roms_load(@memory_temp, stargate_rom)) then
          exit;
        copymemory(@memory[$D000], @memory_temp[0], $3000);
        for f := 0 to 8 do
          copymemory(@rom_data[f, 0], @memory_temp[$3000 + (f * $1000)], $1000);
        // roms sonido
        if not(roms_load(@mem_snd, stargate_snd)) then
          exit;
        events_call := events_stargate;
        blitter_0 := williams_blitter.Create(4, false, $C000);
        blitter_0.set_read_write(joust_getbyte, joust_putbyte);
        xoff := 6;
        // Cargar NVRam
        if read_file_size(Directory.Arcade_nvram + 'stargate.nv', longitud) then
          read_file(Directory.Arcade_nvram + 'stargate.nv', @nvram, longitud);
      end;
  end;
  // Palette
  compute_resistor_weights(0, 255, -1.0, 3, @resistances[0], @rweights[0], 0, 0, 3, @resistances[0],
    @gweights[0], 0, 0, 2, @resistances[1], @bweights[0], 0, 0);
  for f := 0 to $FF do
  begin
    color.r := combine_3_weights(@rweights[0], (f shr 0) and 1, (f shr 1) and 1, (f shr 2) and 1);
    color.g := combine_3_weights(@gweights[0], (f shr 3) and 1, (f shr 4) and 1, (f shr 5) and 1);
    color.b := combine_2_weights(@bweights[0], (f shr 6) and 1, (f shr 7) and 1);
    pal_lookup[f] := convert_pal_color(color);
  end;
  // final
  reset_williams;
  start_williams := true;
end;

end.
