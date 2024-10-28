unit mysteriousstones_hw;

interface

uses
  WinApi.Windows,
  m6502,
  main_engine,
  controls_engine,
  ay_8910,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  qsnapshot;

function start_mysteriousstones: boolean;

implementation

const
  ms_rom: array [0 .. 5] of tipo_roms = ((n: 'rom6.bin'; l: $2000; p: $4000; crc: $7BD9C6CD), (n: 'rom5.bin'; l: $2000; p: $6000; crc: $A83F04A6), (n: 'rom4.bin'; l: $2000; p: $8000; crc: $46C73714),
    (n: 'rom3.bin'; l: $2000; p: $A000; crc: $34F8B8A3), (n: 'rom2.bin'; l: $2000; p: $C000; crc: $BFD22CFC), (n: 'rom1.bin'; l: $2000; p: $E000; crc: $FB163E38));
  ms_char: array [0 .. 5] of tipo_roms = ((n: 'ms6'; l: $2000; p: $0000; crc: $85C83806), (n: 'ms9'; l: $2000; p: $2000; crc: $B146C6AB), (n: 'ms7'; l: $2000; p: $4000; crc: $D025F84D), (n: 'ms10';
    l: $2000; p: $6000; crc: $D85015B5), (n: 'ms8'; l: $2000; p: $8000; crc: $53765D89), (n: 'ms11'; l: $2000; p: $A000; crc: $919EE527));
  ms_sprite: array [0 .. 5] of tipo_roms = ((n: 'ms12'; l: $2000; p: $0000; crc: $72D8331D), (n: 'ms13'; l: $2000; p: $2000; crc: $845A1F9B), (n: 'ms14'; l: $2000; p: $4000; crc: $822874B0),
    (n: 'ms15'; l: $2000; p: $6000; crc: $4594E53C), (n: 'ms16'; l: $2000; p: $8000; crc: $2F470B0F), (n: 'ms17'; l: $2000; p: $A000; crc: $38966D1B));
  ms_pal: tipo_roms = (n: 'ic61'; l: $20; p: 0; crc: $E802D6CF);
  // Dip
        ms_dip_a:array [0..3] of def_dip2=(
        (mask:$1;name:'Lives';number:2;val2:(1,0);name2:('3','5')),
        (mask:$2;name:'Difficulty';number:2;val2:(2,0);name2:('Easy','Hard')),
        (mask:$4;name:'Demo Sounds';number:2;val2:(4,0);name2:('Off','On')),());
        ms_dip_b:array [0..4] of def_dip2=(
        (mask:$3;name:'Coin A';number:4;val4:(0,3,2,1);name4:('2C 1C','1C 1C','1C 2C','1C 3C')),
        (mask:$c;name:'Coin B';number:4;val4:(0,$c,8,4);name4:('2C 1C','1C 1C','1C 2C','1C 3C')),
        (mask:$20;name:'Flip Screen';number:2;val2:(0,$20);name2:('Off','On')),
        (mask:$40;name:'Cabinet';number:2;val2:(0,$40);name2:('Upright','Cocktail')),());

var
  scroll, soundlatch, last, char_color: byte;
  video_page: word;
  weights_rg: array [0 .. 2] of single;
  weights_b: array [0 .. 1] of single;
  ms_scanline: array [0 .. 271] of word;

procedure change_color(pos: byte);
var
  valor, bit0, bit1, bit2: byte;
  color: tcolor;
begin
  valor := buffer_paleta[pos];
  // red
  bit0 := (valor shr 0) and $01;
  bit1 := (valor shr 1) and $01;
  bit2 := (valor shr 2) and $01;
  color.r := combine_3_weights(@weights_rg[0], bit0, bit1, bit2);
  // green
  bit0 := (valor shr 3) and $01;
  bit1 := (valor shr 4) and $01;
  bit2 := (valor shr 5) and $01;
  color.g := combine_3_weights(@weights_rg[0], bit0, bit1, bit2);
  // blue
  bit0 := (valor shr 6) and $01;
  bit1 := (valor shr 7) and $01;
  color.b := combine_2_weights(@weights_b[0], bit0, bit1);
  set_pal_color(color, pos);
end;

procedure update_video_ms;
var
  f, nchar, color, x, y: word;
  atrib: byte;
begin
  for f := 0 to $1FF do
  begin
    if gfx[2].buffer[f + video_page] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := ((memory[$1A00 + video_page + f] and $1) shl 8) + memory[$1800 + f + video_page];
      put_gfx_flip(x * 16, y * 16, nchar, 16, 1, 2, (x and $10) <> 0, false);
      gfx[2].buffer[f + video_page] := false;
    end;
  end;
  scroll__x(1, 2, scroll);
  // Sprites
  for f := 0 to $17 do
  begin
    atrib := memory[$780 + (f * 4)];
    if (atrib and 1) <> 0 then
    begin
      x := 240 - memory[$782 + (f * 4)];
      y := memory[$783 + (f * 4)];
      nchar := memory[$781 + (f * 4)] + ((atrib and $10) shl 4);
      color := (atrib and $8) shl 1;
      put_gfx_sprite(nchar, color, (atrib and 2) <> 0, (atrib and 4) <> 0, 1);
      update_gfx_sprite(x and $FF, y, 2, 1);
    end;
  end;
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f mod 32;
      y := f div 32;
      nchar := ((memory[$1400 + f] and $07) shl 8) + memory[$1000 + f];
      put_gfx_trans(x * 8, y * 8, nchar, 24 + (char_color shl 3), 3, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 3, 0, 0, 256, 256, 2);
  update_final_piece(8, 0, 240, 256, 2);
end;

procedure events_ms;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := marcade.in0 and $FE
    else
      marcade.in0 := marcade.in0 or 1;
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := marcade.in0 and $FD
    else
      marcade.in0 := marcade.in0 or 2;
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := marcade.in0 and $FB
    else
      marcade.in0 := marcade.in0 or 4;
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := marcade.in0 and $F7
    else
      marcade.in0 := marcade.in0 or 8;
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := marcade.in0 and $EF
    else
      marcade.in0 := marcade.in0 or $10;
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := marcade.in0 and $DF
    else
      marcade.in0 := marcade.in0 or $20;
    if p_contrls.map_arcade.coin[0] then
    begin
      marcade.in0 := (marcade.in0 and $BF);
      m6502_0.change_nmi(ASSERT_LINE);
    end
    else
    begin
      marcade.in0 := (marcade.in0 or $40);
      if p_contrls.map_arcade.coin[1] then
      begin
        marcade.in0 := (marcade.in0 and $7F);
        m6502_0.change_nmi(ASSERT_LINE);
      end
      else
      begin
        marcade.in0 := (marcade.in0 or $80);
        m6502_0.change_nmi(CLEAR_LINE);
      end;
    end;
    // P2
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := marcade.in1 and $FE
    else
      marcade.in1 := marcade.in1 or 1;
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := marcade.in1 and $FD
    else
      marcade.in1 := marcade.in1 or 2;
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := marcade.in1 and $FB
    else
      marcade.in1 := marcade.in1 or 4;
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := marcade.in1 and $F7
    else
      marcade.in1 := marcade.in1 or 8;
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := marcade.in1 and $EF
    else
      marcade.in1 := marcade.in1 or $10;
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := marcade.in1 and $DF
    else
      marcade.in1 := marcade.in1 or $20;
    if p_contrls.map_arcade.start[0] then
      marcade.in1 := marcade.in1 and $BF
    else
      marcade.in1 := marcade.in1 or $40;
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := marcade.in1 and $7F
    else
      marcade.in1 := marcade.in1 or $80;
  end;
end;

procedure ms_loop;
var
  f: word;
  frame: single;
begin
  init_controls(false, false, false, true);
  frame := m6502_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to 271 do
      begin
        case ms_scanline[f] of
          $8:
            marcade.dswb := marcade.dswb and $7F;
          $F8:
            begin
              update_video_ms;
              marcade.dswb := marcade.dswb or $80;
            end;
        end;
        if ((ms_scanline[f] and $F) = 8) then
          m6502_0.change_irq(ASSERT_LINE);
    m6502_0.run(frame);
    frame:=frame+m6502_0.tframes-m6502_0.contador;
      end;
      events_ms;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function getbyte_ms(direccion: word): byte;
begin
  case direccion of
    0 .. $1FFF, $4000 .. $FFFF:
      getbyte_ms := memory[direccion];
    $2000 .. $3FFF:
      case (direccion and $7F) of
        $0 .. $F:
          getbyte_ms := marcade.in0;
        $10 .. $1F:
          getbyte_ms := marcade.in1;
        $20 .. $2F:
          getbyte_ms := marcade.dswa;
        $30 .. $3F:
          getbyte_ms := marcade.dswb;
        $60 .. $7F:
          getbyte_ms := buffer_paleta[direccion and $1F];
      end;
  end;
end;

procedure putbyte_ms(direccion: word; valor: byte);
var
  temp: byte;
begin
  case direccion of
    0 .. $FFF:
      memory[direccion] := valor;
    $1000 .. $17FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $1800 .. $1FFF:
      if memory[direccion] <> valor then
      begin
        gfx[2].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $2000 .. $3FFF:
      case (direccion and $7F) of
        $0 .. $F:
          begin
            temp := ((valor and $1) shl 1) + ((valor and $2) shr 1);
            if char_color <> temp then
            begin
              fillchar(gfx[0].buffer[0], $400, 1);
              char_color := temp;
            end;
            video_page := (valor and $4) shl 8;
          end;
        $10 .. $1F:
          m6502_0.change_irq(CLEAR_LINE);
        $20 .. $2F:
          scroll := valor;
        $30 .. $3F:
          soundlatch := valor;
        $40 .. $4F:
          begin
            if (((last and $20) = $20) and ((valor and $20) = 0)) then
            begin
              if (last and $10) <> 0 then
                ay8910_0.Control(soundlatch)
              else
                ay8910_0.write(soundlatch);
            end;
            if (((last and $80) = $80) and ((valor and $80) = 0)) then
            begin
              if (last and $40) <> 0 then
                AY8910_1.Control(soundlatch)
              else
                AY8910_1.write(soundlatch);
            end;
            last := valor;
          end;
        $60 .. $7F:
          if buffer_paleta[direccion and $1F] <> valor then
          begin
            buffer_paleta[direccion and $1F] := valor;
            change_color(direccion and $1F);
            if (direccion and $1F) >= $10 then
              fillchar(gfx[2].buffer[0], $400, 1);
          end;
      end;
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

procedure ms_sound_update;
begin
  ay8910_0.update;
  AY8910_1.update;
end;

// Main
procedure ms_qsave(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
  size: word;
begin
  open_qsnapshot_save('mysteriousstones' + nombre);
  getmem(data, 20000);
  // CPU
  size := m6502_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // SND
  size := ay8910_0.save_snapshot(data);
  savedata_qsnapshot(data, size);
  size := AY8910_1.save_snapshot(data);
  savedata_qsnapshot(data, size);
  // MEM
  savedata_qsnapshot(@memory, $4000);
  // MISC
  buffer[0] := scroll;
  buffer[1] := soundlatch;
  buffer[2] := last;
  buffer[3] := char_color;
  buffer[4] := video_page and $FF;
  buffer[5] := video_page shr 8;
  savedata_qsnapshot(@buffer, 6);
  freemem(data);
  close_qsnapshot;
end;

procedure ms_qload(nombre: string);
var
  data: pbyte;
  buffer: array [0 .. 5] of byte;
  f: word;
begin
  if not(open_qsnapshot_load('mysteriousstones' + nombre)) then
    exit;
  getmem(data, 20000);
  // CPU
  loaddata_qsnapshot(data);
  m6502_0.load_snapshot(data);
  // SND
  loaddata_qsnapshot(data);
  ay8910_0.load_snapshot(data);
  loaddata_qsnapshot(data);
  AY8910_1.load_snapshot(data);
  // MEM
  loaddata_qsnapshot(@memory);
  // MISC
  scroll := buffer[0];
  soundlatch := buffer[1];
  last := buffer[2];
  char_color := buffer[3];
  video_page := buffer[4] or (buffer[5] shl 8);
  freemem(data);
  close_qsnapshot;
  // END
  fillchar(gfx[0].buffer, $400, 1);
  fillchar(gfx[2].buffer, $1000, 1);
  for f := 0 to $1F do
    change_color(f);

end;

procedure reset_ms;
begin
  m6502_0.reset;
  ay8910_0.reset;
  AY8910_1.reset;
  reset_audio;
  scroll := 0;
  last := 0;
  soundlatch := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  char_color := 0;
end;

function start_mysteriousstones: boolean;
var
  f: byte;
  memory_temp: array [0 .. $BFFF] of byte;
const
  ps_x: array [0 .. 15] of dword = (16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
  resistances_rg: array [0 .. 2] of integer = (4700, 3300, 1500);
  resistances_b: array [0 .. 1] of integer = (3300, 1500);
begin
  machine_calls.general_loop := ms_loop;
  machine_calls.reset := reset_ms;
  machine_calls.fps_max := 12000000 / 2 / 384 / 272;
  machine_calls.save_qsnap := ms_qsave;
  machine_calls.load_qsnap := ms_qload;
  start_mysteriousstones := false;
  start_audio(false);
  screen_init(1, 512, 256);
  screen_mod_scroll(1, 512, 256, 511, 256, 256, 255);
  screen_init(2, 256, 512, false, true);
  screen_init(3, 256, 256, true);
  start_video(240, 256);
  // Main CPU
  m6502_0 := cpu_m6502.create(1500000, 272, TCPU_M6502);
  m6502_0.change_ram_calls(getbyte_ms, putbyte_ms);
  m6502_0.init_sound(ms_sound_update);
  // Sound Chip
ay8910_0:=ay8910_chip.create(1500000,AY8910);
ay8910_1:=ay8910_chip.create(1500000,AY8910);
  // cargar roms
  if not(roms_load(@memory, ms_rom)) then
    exit;
  // Cargar chars
  if not(roms_load(@memory_temp, ms_char)) then
    exit;
  init_gfx(0, 8, 8, 2048);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(3, 0, 8 * 8, $4000 * 8 * 2, $4000 * 8, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x[8], @ps_y, false, true);
  // sprites
  init_gfx(1, 16, 16, 512);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(3, 0, 32 * 8, $4000 * 8 * 2, $4000 * 8, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // Cargar sprites fondo
  if not(roms_load(@memory_temp, ms_sprite)) then
    exit;
  init_gfx(2, 16, 16, 512);
  convert_gfx(2, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // poner la paleta
  if not(roms_load(@memory_temp, ms_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @weights_rg, 0, 4700, 2, @resistances_b, @weights_b, 0, 4700, 0, nil, nil, 0, 0);
  for f := 24 to 63 do
  begin
    buffer_paleta[f] := memory_temp[f - 24];
    change_color(f);
  end;
  // init scanlines
  for f := 8 to $FF do
    ms_scanline[f - 8] := f; // 08,09,0A,0B,...,FC,FD,FE,FF
  for f := $E8 to $FF do
    ms_scanline[f + $10] := f + $100; // E8,E9,EA,EB,...,FC,FD,FE,FF
  // DIP
  marcade.dswa := $FB;
  marcade.dswb := $1F;
marcade.dswa_val2:=@ms_dip_a;
marcade.dswb_val2:=@ms_dip_b;
  // final
  reset_ms;
  start_mysteriousstones := true;
end;

end.
