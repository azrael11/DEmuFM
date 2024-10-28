unit terracresta_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_2203,
  dac,
  rom_engine,
  pal_engine,
  sound_engine,
  qsnapshot,
  timer_engine,
  nb1412_m2,
  ym_3812;

function start_terracresta: boolean;

implementation

const
  terracre_rom: array [0 .. 7] of tipo_roms = ((n: '1a_4b.rom'; l: $4000; p: 1; crc: $76F17479), (n: '1a_4d.rom'; l: $4000; p: $0; crc: $8119F06E), (n: '1a_6b.rom'; l: $4000; p: $8001;
    crc: $BA4B5822), (n: '1a_6d.rom'; l: $4000; p: $8000; crc: $CA4852F6), (n: '1a_7b.rom'; l: $4000; p: $10001; crc: $D0771BBA), (n: '1a_7d.rom'; l: $4000; p: $10000; crc: $029D59D9),
    (n: '1a_9b.rom'; l: $4000; p: $18001; crc: $69227B56), (n: '1a_9d.rom'; l: $4000; p: $18000; crc: $5A672942));
  terracre_pal: array [0 .. 4] of tipo_roms = ((n: 'tc1a_10f.bin'; l: $100; p: 0; crc: $CE07C544), (n: 'tc1a_11f.bin'; l: $100; p: $100; crc: $566D323A), (n: 'tc1a_12f.bin'; l: $100; p: $200;
    crc: $7EA63946), (n: 'tc2a_2g.bin'; l: $100; p: $300; crc: $08609BAD), (n: 'tc2a_4e.bin'; l: $100; p: $400; crc: $2C43991F));
  terracre_char: tipo_roms = (n: '2a_16b.rom'; l: $2000; p: 0; crc: $591A3804);
  terracre_sound: array [0 .. 1] of tipo_roms = ((n: 'tc2a_15b.bin'; l: $4000; p: 0; crc: $790DDFA9), (n: 'tc2a_17b.bin'; l: $4000; p: $4000; crc: $D4531113));
  terracre_fondo: array [0 .. 1] of tipo_roms = ((n: '1a_15f.rom'; l: $8000; p: 0; crc: $984A597F), (n: '1a_17f.rom'; l: $8000; p: $8000; crc: $30E297FF));
  terracre_sprites: array [0 .. 3] of tipo_roms = ((n: '2a_6e.rom'; l: $4000; p: 0; crc: $BCF7740B), (n: '2a_7e.rom'; l: $4000; p: $4000; crc: $A70B565C), (n: '2a_6g.rom'; l: $4000; p: $8000;
    crc: $4A9EC3E6), (n: '2a_7g.rom'; l: $4000; p: $C000; crc: $450749FC));
        terracre_dip:array [0..10] of def_dip2=(
        (mask:$3;name:'Lives';number:4;val4:(3,2,1,0);name4:('3','4','5','6')),
        (mask:$c;name:'Bonus Life';number:4;val4:($c,8,4,0);name4:('20K 60K+','30K 70K+','40K 80K+','50K 90K+')),
        (mask:$10;name:'Demo Sounds';number:2;val2:(0,$10);name2:('Off','On')),
        (mask:$20;name:'Cabinet';number:2;val2:(0,$20);name2:('Upright','Cocktail')),
        (mask:$300;name:'Coin A';number:4;val4:($100,$300,$200,0);name4:('2C 1C','1C 1C','1C 2C','Free Play')),
        (mask:$c00;name:'Coin B';number:4;val4:(0,$400,$c00,$800);name4:('3C 1C','2C 3C','1C 3C','1C 6C')),
        (mask:$1000;name:'Difficulty';number:2;val2:($1000,0);name2:('Easy','Hard')),
        (mask:$2000;name:'Flip Screen';number:2;val2:($2000,0);name2:('Off','On')),
        (mask:$4000;name:'Complete Invulnerability';number:2;val2:($4000,0);name2:('Off','On')),
        (mask:$8000;name:'Base Ship Invulnerability';number:2;val2:($8000,0);name2:('Off','On')),());
  // Amazon
  amazon_rom: array [0 .. 3] of tipo_roms = ((n: '11.4d'; l: $8000; p: 0; crc: $6C7F85C5), (n: '9.4b'; l: $8000; p: $1; crc: $E1B7A989), (n: '12.6d'; l: $8000; p: $10000; crc: $4DE8A3EE), (n: '10.6b';
    l: $8000; p: $10001; crc: $D86BAD81));
  amazon_sound: array [0 .. 2] of tipo_roms = ((n: '1.15b'; l: $4000; p: 0; crc: $55A8B5E7), (n: '2.17b'; l: $4000; p: $4000; crc: $427A7CCA), (n: '3.18b'; l: $4000; p: $8000; crc: $B8CCEAF7));
  amazon_char: tipo_roms = (n: '8.16g'; l: $2000; p: 0; crc: $0CEC8644);
  amazon_fondo: array [0 .. 2] of tipo_roms = ((n: '13.15f'; l: $8000; p: 0; crc: $415FF4D9), (n: '14.17f'; l: $8000; p: $8000; crc: $492B5C48), (n: '15.18f'; l: $8000; p: $10000; crc: $B1AC0B9D));
  amazon_sprites: array [0 .. 3] of tipo_roms = ((n: '4.6e'; l: $4000; p: 0; crc: $F77CED7A), (n: '5.7e'; l: $4000; p: $4000; crc: $16EF1465), (n: '6.6g'; l: $4000; p: $8000; crc: $936EC941),
    (n: '7.7g'; l: $4000; p: $C000; crc: $66DD718E));
  amazon_pal: array [0 .. 4] of tipo_roms = ((n: 'clr.10f'; l: $100; p: 0; crc: $6440B341), (n: 'clr.11f'; l: $100; p: $100; crc: $271E947F), (n: 'clr.12f'; l: $100; p: $200; crc: $7D38621B),
    (n: '2g'; l: $100; p: $300; crc: $44CA16B9), (n: '4e'; l: $100; p: $400; crc: $035F2C7B));
  amazon_prot: tipo_roms = (n: '16.18g'; l: $2000; p: 0; crc: $1D8D592B);
        amazon_dip:array [0..10] of def_dip2=(
        (mask:$3;name:'Lives';number:4;val4:(3,2,1,0);name4:('3','4','5','6')),
        (mask:$c;name:'Bonus Life';number:4;val4:($c,8,4,0);name4:('20K 40K+','50K 40K+','20K 70K+','50K 70K+')),
        (mask:$10;name:'Demo Sounds';number:2;val2:(0,$10);name2:('Off','On')),
        (mask:$20;name:'Cabinet';number:2;val2:(0,$20);name2:('Upright','Cocktail')),
        (mask:$300;name:'Coin A';number:4;val4:($100,$300,$200,0);name4:('2C 1C','1C 1C','1C 2C','Free Play')),
        (mask:$c00;name:'Coin B';number:4;val4:(0,$800,$400,$c00);name4:('3C 1C','1C 1C','2C 3C','1C 3C')),
        (mask:$1000;name:'Difficulty';number:2;val2:($1000,0);name2:('Easy','Hard')),
        (mask:$2000;name:'Flip Screen';number:2;val2:($2000,0);name2:('Off','On')),
        (mask:$4000;name:'Level';number:2;val2:($4000,0);name2:('Low','High')),
        (mask:$8000;name:'Sprite Test';number:2;val2:($8000,0);name2:('Off','On')),());

var
 scroll_x,scroll_y:word;
  rom: array [0 .. $1FFFF] of word;
  ram: array [0 .. $1FFF] of word;
  ram2: array [0 .. $7FF] of word;
  spritebank: array [0 .. $FF] of byte;
  sound_latch: byte;
  prot_mem: array [0 .. $1FFF] of byte;

procedure update_video_terracre;
var
  f, color, x, y, nchar, atrib: word;
begin
  // background
  if (scroll_y and $2000) <> 0 then
  begin
    fill_full_screen(3, $100);
  end
  else
  begin
    for f := $0 to $7FF do
    begin
      if gfx[1].buffer[f] then
      begin
        x := f and $1F;
        y := 63 - (f shr 5);
        nchar := ram[$1000 + f];
        color := ((nchar shr 11) and $F) shl 4;
      put_gfx(x shl 4,y shl 4,nchar,color,1,1);
        gfx[1].buffer[f] := false;
      end;
    end;
    scroll_x_y(1, 3, scroll_x, (768 - (scroll_y and $FFF)) and $3FF);
  end;
  // sprites
  for f := 0 to $3F do
  begin
    atrib := buffer_sprites_w[(f * 4) + 2];
    nchar := (buffer_sprites_w[(f * 4) + 1] and $FF) + ((atrib and $2) shl 7);
    y := 240 - ((buffer_sprites_w[(f * 4) + 3] and $FF) - $80 + ((atrib and 1) shl 8));
    x := 240 - (buffer_sprites_w[f * 4] and $FF);
    color := ((atrib and $F0) shr 4) + 16 * (spritebank[(nchar shr 1) and $FF] and $0F);
    put_gfx_sprite(nchar, color shl 4, (atrib and 8) <> 0, (atrib and 4) <> 0, 2);
    update_gfx_sprite(x, y, 3, 2);
  end;
  // foreground
  if (scroll_y and $1000) = 0 then
  begin
    for f := $0 to $3FF do
    begin
      if gfx[0].buffer[f] then
      begin
        x := f and $1F;
        y := 31 - (f shr 5);
        nchar := ram2[f] and $FF;
        put_gfx_trans(x shl 3, y shl 3, nchar, 0, 2, 0);
        gfx[0].buffer[f] := false;
      end;
    end;
  end;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(16, 0, 224, 256, 3);
end;

procedure events_terracre;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FFFB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FFF7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FFFE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FFFD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FFFB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FFF7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $FFEF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $FFDF)
    else
      marcade.in2 := (marcade.in2 or $20);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $400);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $800);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $100);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $200);
  end;
end;

procedure terracre_loop;
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
        if f = 239 then
        begin
          update_video_terracre;
          copymemory(@buffer_sprites_w, @ram, $100 * 2);
          m68000_0.irq[1] := HOLD_LINE;
        end;
      end;
      events_terracre;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function terracre_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $1FFFF:
      terracre_getword := rom[direccion shr 1];
    $20000 .. $23FFF:
      terracre_getword := ram[(direccion and $3FFF) shr 1];
    $24000:
      terracre_getword := marcade.in1;
    $24002:
      terracre_getword := marcade.in2;
    $24004:
      terracre_getword := marcade.in0;
    $24006:
      terracre_getword := marcade.dswa;
    $28000 .. $287FF:
      terracre_getword := ram2[(direccion and $7FF) shr 1];
  end;
end;

procedure terracre_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $1FFFF:
      ;
    $20000 .. $21FFF, $23000 .. $23FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $22000 .. $22FFF:
      if ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        gfx[1].buffer[(direccion and $FFF) shr 1] := true;
        ram[(direccion and $3FFF) shr 1] := valor;
      end;
    $26000:
      main_screen.flip_main_screen := (valor and $4) <> 0;
    $26002:
      scroll_y := valor;
    $26004:
      scroll_x := valor and $1FF;
    $2600C:
      sound_latch := ((valor and $7F) shl 1) or 1;
    $28000 .. $287FF:
      if ram2[(direccion and $7FF) shr 1] <> valor then
      begin
        gfx[0].buffer[(direccion and $7FF) shr 1] := true;
        ram2[(direccion and $7FF) shr 1] := valor;
      end;
  end;
end;

function terracre_snd_getbyte(direccion: word): byte;
begin
  terracre_snd_getbyte := mem_snd[direccion];
end;

procedure terracre_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $BFFF:
      ;
    $C000 .. $CFFF:
      mem_snd[direccion] := valor;
  end;
end;

function terracre_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    4:
      begin
        sound_latch := 0;
        terracre_snd_inbyte := 0;
      end;
    6:
      terracre_snd_inbyte := sound_latch;
  end;
end;

procedure terracre_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      ym2203_0.Control(valor);
    $1:
      ym2203_0.Write(valor);
    $2:
      dac_0.signed_data8_w(valor);
    $3:
      dac_1.signed_data8_w(valor);
  end;
end;

procedure terracre_sound_update;
begin
  ym2203_0.Update;
  dac_0.Update;
  dac_1.Update;
end;

procedure terracre_snd_timer;
begin
  z80_0.change_irq(HOLD_LINE);
end;

function amazon_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $1FFFF:
      amazon_getword := rom[direccion shr 1];
    $40000 .. $42FFF:
      amazon_getword := ram[(direccion and $3FFF) shr 1];
    $44000:
      amazon_getword := marcade.in1;
    $44002:
      amazon_getword := marcade.in2;
    $44004:
      amazon_getword := marcade.in0;
    $44006:
      amazon_getword := marcade.dswa;
    $50000 .. $50FFF:
      amazon_getword := ram2[(direccion and $FFF) shr 1];
    $70000:
      amazon_getword := nb1412m2_0.read;
  end;
end;

procedure amazon_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $1FFFF:
      ;
    $40000 .. $41FFF:
      ram[(direccion and $3FFF) shr 1] := valor;
    $42000 .. $42FFF:
      if ram[(direccion and $3FFF) shr 1] <> valor then
      begin
        gfx[1].buffer[(direccion and $FFF) shr 1] := true;
        ram[(direccion and $3FFF) shr 1] := valor;
      end;
    $46000:
      main_screen.flip_main_screen := (valor and $4) <> 0;
    $46002:
      scroll_y := valor;
    $46004:
      scroll_x := valor and $1FF;
    $4600C:
      sound_latch := ((valor and $7F) shl 1) or 1;
    $50000 .. $50FFF:
      if ram2[(direccion and $FFF) shr 1] <> valor then
      begin
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
        ram2[(direccion and $FFF) shr 1] := valor;
      end;
    $70000:
      nb1412m2_0.Write(valor and $FF);
    $70002:
      nb1412m2_0.command(valor and $FF);
  end;
end;

procedure amazon_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $0:
      ym3812_0.Control(valor);
    $1:
      ym3812_0.Write(valor);
    $2:
      dac_0.signed_data8_w(valor);
    $3:
      dac_1.signed_data8_w(valor);
  end;
end;

procedure amazon_sound_update;
begin
  ym3812_0.Update;
  dac_0.Update;
  dac_1.Update;
end;

procedure terracre_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 6] of byte;
  size: word;
begin
  open_qsnapshot_save('terracresta' + nombre);
  getmem(data, 20000);
  // CPU
  size := m68000_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := z80_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ym2203_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := dac_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := dac_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@ram, $2000 * 2);
  savedata_qsnapshot(@ram2, $400 * 2);
  savedata_qsnapshot(@mem_snd[$C000], $1000);
  // MISC
  buffer[0] := 0;
  buffer[1] := scroll_x and $FF;
  buffer[2] := scroll_x shr 8;
  buffer[3] := scroll_y and $FF;
  buffer[4] := scroll_y shr 8;
  buffer[5] := sound_latch;
  savedata_qsnapshot(@buffer, 7);
  freemem(data);
  close_qsnapshot;
end;

procedure terracre_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 6] of byte;
begin
  if not(open_qsnapshot_load('terracresta' + nombre)) then
    exit;
  getmem(data, 20000);
  // CPU
  loaddata_qsnapshot(data);
  m68000_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  z80_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ym2203_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  dac_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  dac_1.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@ram);
  loaddata_qsnapshot(@ram2);
  loaddata_qsnapshot(@mem_snd[$C000]);
  // MISC
  loaddata_qsnapshot(@buffer);
  scroll_x := buffer[1] or (buffer[2] shl 8);
  scroll_y := buffer[3] or (buffer[4] shl 8);
  sound_latch := buffer[5];
  freemem(data);
  close_qsnapshot;
  // END
  fillchar(gfx[0].buffer[0], $400, 1);
  fillchar(gfx[1].buffer[0], $800, 1);
end;

// Main
procedure reset_terracre;
begin
  m68000_0.reset;
  z80_0.reset;
  if main_vars.machine_type = 41 then
    ym2203_0.reset
  else
    ym3812_0.reset;
  dac_0.reset;
  dac_1.reset;
  reset_audio;
  marcade.in0 := $FF00;
  marcade.in1 := $FFFF;
  marcade.in2 := $FFFF;
  scroll_x := 0;
  scroll_y := 0;
  sound_latch := 0;
end;

function start_terracresta: boolean;
var
  colores: tpaleta;
  f, j: word;
  memory_temp: array [0 .. $1FFFF] of byte;
const
  pc_x: array [0 .. 7] of dword = (1 * 4, 0 * 4, 3 * 4, 2 * 4, 5 * 4, 4 * 4, 7 * 4, 6 * 4);
  ps_x: array [0 .. 15] of dword = (4, 0, 4 + $8000 * 8, 0 + $8000 * 8, 12, 8, 12 + $8000 * 8, 8 + $8000 * 8, 20, 16, 20 + $8000 * 8, 16 + $8000 * 8, 28, 24, 28 + $8000 * 8, 24 + $8000 * 8);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
  pf_x: array [0 .. 15] of dword = (4, 0, 12, 8, 20, 16, 28, 24, 32 + 4, 32 + 0, 32 + 12, 32 + 8, 32 + 20, 32 + 16, 32 + 28, 32 + 24);
  pf_y: array [0 .. 15] of dword = (0 * 64, 1 * 64, 2 * 64, 3 * 64, 4 * 64, 5 * 64, 6 * 64, 7 * 64, 8 * 64, 9 * 64, 10 * 64, 11 * 64, 12 * 64, 13 * 64, 14 * 64, 15 * 64);
  procedure convert_chars;
  begin
    init_gfx(0, 8, 8, $100);
    gfx[0].trans[15] := true;
    gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
    convert_gfx(0, 0, @memory_temp, @pc_x, @ps_y, false, true);
  end;
  procedure convert_fg(num: word);
  begin
    init_gfx(1, 16, 16, num);
    gfx_set_desc_data(4, 0, 64 * 16, 0, 1, 2, 3);
    convert_gfx(1, 0, @memory_temp, @pf_x, @pf_y, false, true);
  end;
  procedure convert_sprites;
  begin
    init_gfx(2, 16, 16, $200);
    gfx[2].trans[0] := true;
    gfx_set_desc_data(4, 0, 32 * 16, 0, 1, 2, 3);
    convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  end;

begin
  machine_calls.general_loop := terracre_loop;
  machine_calls.reset := reset_terracre;
  machine_calls.save_qsnap := terracre_qsave;
  machine_calls.load_qsnap := terracre_qload;
  start_terracresta := false;
  start_audio(false);
  screen_init(1, 512, 1024);
  screen_mod_scroll(1, 512, 256, 511, 1024, 256, 1023);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 512, false, true);
  start_video(224, 256);
  // Main CPU
  m68000_0 := cpu_m68000.create(8000000, 256);
  // Sound CPU
  z80_0 := cpu_z80.create(4000000, 256);
  z80_0.change_ram_calls(terracre_snd_getbyte, terracre_snd_putbyte);
  case main_vars.machine_type of
    41:
      begin // Terra Cresta
        // cargar roms
        if not(roms_load16w(@rom, terracre_rom)) then
          exit;
        m68000_0.change_ram16_calls(terracre_getword, terracre_putword);
        // cargar sonido
        if not(roms_load(@mem_snd, terracre_sound)) then
          exit;
        z80_0.change_io_calls(terracre_snd_inbyte, terracre_snd_outbyte);
        z80_0.init_sound(terracre_sound_update);
        // Sound Chips
        ym2203_0 := ym2203_chip.create(4000000, 0.6, 1);
        // convertir chars
        if not(roms_load(@memory_temp, terracre_char)) then
          exit;
        convert_chars;
        // convertir fondo
        if not(roms_load(@memory_temp, terracre_fondo)) then
          exit;
        convert_fg($200);
        // convertir sprites
        if not(roms_load(@memory_temp, terracre_sprites)) then
          exit;
        convert_sprites;
        // DIP
        marcade.dswa := $FFDF;
      marcade.dswa_val2:=@terracre_dip;
        // poner la paleta
        if not(roms_load(@memory_temp, terracre_pal)) then
          exit;
        copymemory(@spritebank, @memory_temp[$400], $100);
      end;
    265:
      begin // Soldier Girl Amazon
        // cargar roms
        if not(roms_load16w(@rom, amazon_rom)) then
          exit;
        m68000_0.change_ram16_calls(amazon_getword, amazon_putword);
        // cargar sonido
        if not(roms_load(@mem_snd, amazon_sound)) then
          exit;
        z80_0.change_io_calls(terracre_snd_inbyte, amazon_snd_outbyte);
        z80_0.init_sound(amazon_sound_update);
        // Sound Chips
        ym3812_0 := ym3812_chip.create(YM3526_FM, 4000000, 1);
        // convertir chars
        if not(roms_load(@memory_temp, amazon_char)) then
          exit;
        convert_chars;
        // convertir fondo
        if not(roms_load(@memory_temp, amazon_fondo)) then
          exit;
        convert_fg($400);
        // convertir sprites
        if not(roms_load(@memory_temp, amazon_sprites)) then
          exit;
        convert_sprites;
        // prot
        if not(roms_load(@prot_mem, amazon_prot)) then
          exit;
        nb1412m2_0 := tnb1412_m2.create(@prot_mem);
        // DIP
        marcade.dswa := $FFDF;
      marcade.dswa_val2:=@amazon_dip;
        // poner la paleta
        if not(roms_load(@memory_temp, amazon_pal)) then
          exit;
        copymemory(@spritebank, @memory_temp[$400], $100);
      end;
  end;
  dac_0 := dac_chip.create(0.5);
  dac_1 := dac_chip.create(0.5);
  timers.init(z80_0.numero_cpu, 4000000 / (4000000 / 512), terracre_snd_timer, nil, true);
  for f := 0 to $FF do
  begin
    colores[f].r := pal4bit(memory_temp[f]);
    colores[f].g := pal4bit(memory_temp[f + $100]);
    colores[f].b := pal4bit(memory_temp[f + $200]);
    // color lookup de fondo
    if (f and 8) <> 0 then
      gfx[1].colores[f] := 192 + (f and $0F) + ((f and $C0) shr 2)
    else
      gfx[1].colores[f] := 192 + (f and $0F) + ((f and $30) shr 0);
    // color lookup de sprites
    for j := 0 to $F do
    begin
      if (f and $8) <> 0 then
        gfx[2].colores[f + j * $100] := $80 + ((j and $0C) shl 2) + (memory_temp[$300 + f] and $0F)
      else
        gfx[2].colores[f + j * $100] := $80 + ((j and $03) shl 4) + (memory_temp[$300 + f] and $0F);
    end;
  end;
  set_pal(colores, $100);
  // final
  reset_terracre;
  start_terracresta := true;
end;

end.
