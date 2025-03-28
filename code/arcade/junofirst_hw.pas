unit junofirst_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  konami_decrypt,
  ay_8910,
  mcs48,
  dac;

function start_junofirst: boolean;

implementation

const
  junofrst_rom: array [0 .. 2] of tipo_roms = ((n: 'jfa_b9.bin'; l: $2000; p: $A000; crc: $F5A7AB9D), (n: 'jfb_b10.bin'; l: $2000; p: $C000; crc: $F20626E0), (n: 'jfc_a10.bin'; l: $2000; p: $E000;
    crc: $1E7744A7));
  junofrst_bank_rom: array [0 .. 5] of tipo_roms = ((n: 'jfc1_a4.bin'; l: $2000; p: $0; crc: $03CCBF1D), (n: 'jfc2_a5.bin'; l: $2000; p: $2000; crc: $CB372372), (n: 'jfc3_a6.bin'; l: $2000; p: $4000;
    crc: $879D194B), (n: 'jfc4_a7.bin'; l: $2000; p: $6000; crc: $F28AF80B), (n: 'jfc5_a8.bin'; l: $2000; p: $8000; crc: $0539F328), (n: 'jfc6_a9.bin'; l: $2000; p: $A000; crc: $1DA2AD6E));
  junofrst_sound: tipo_roms = (n: 'jfs1_j3.bin'; l: $1000; p: 0; crc: $235A2893);
  junofrst_sound_sub: tipo_roms = (n: 'jfs2_p4.bin'; l: $1000; p: 0; crc: $D0FA5D5F);
  junofrst_blit: array [0 .. 2] of tipo_roms = ((n: 'jfs3_c7.bin'; l: $2000; p: $0; crc: $AEACF6DB), (n: 'jfs4_d7.bin'; l: $2000; p: $2000; crc: $206D954C), (n: 'jfs5_e7.bin'; l: $2000; p: $4000;
    crc: $1EB87A6E));
  // Dip
  junofrst_dip_a: array [0 .. 1] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), ());
  junofrst_dip_b: array [0 .. 4] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '4'), (dip_val: $1; dip_name: '5'), (dip_val: $0;
    dip_name: '256'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $70; name: 'Difficulty'; number: 8;
    dip: ((dip_val: $70; dip_name: '1 (Easiest)'), (dip_val: $60; dip_name: '2'), (dip_val: $50; dip_name: '3'), (dip_val: $40; dip_name: '4'), (dip_val: $30; dip_name: '5'), (dip_val: $20;
    dip_name: '6'), (dip_val: $10; dip_name: '7'), (dip_val: $0; dip_name: '8 (Hardest)'), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  rom_bank, rom_bank_dec: array [0 .. $F, 0 .. $FFF] of byte;
  mem_opcodes, blit_mem: array [0 .. $5FFF] of byte;
  irq_enable: boolean;
  i8039_status, xorx, xory, last_snd_val, sound_latch, sound_latch2, rom_nbank, scroll_y: byte;
  blit_data: array [0 .. 3] of byte;
  mem_snd_sub: array [0 .. $FFF] of byte;

procedure update_video_junofrst;
var
  y, x: word;
  effx, effy, vrambyte, shifted: byte;
  punt: array [0 .. $FFFF] of word;
begin
  for y := 0 to 255 do
  begin
    for x := 0 to 255 do
    begin
      effy := y xor xory;
      if effy < 192 then
        effx := (x xor xorx) + scroll_y
      else
        effx := (x xor xorx);
      vrambyte := memory[effx * 128 + effy shr 1];
      shifted := vrambyte shr (4 * (effy and 1));
      punt[y * 256 + x] := paleta[shifted and $0F];
    end;
  end;
  putpixel(0, 0, $10000, @punt, 1);
  update_region(16, 0, 224, 256, 1, 0, 0, 224, 256, PANT_TEMP);
end;

procedure events_junofrst;
begin
  if event.arcade then
  begin
    // marcade.in1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // marcade.in2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BF)
    else
      marcade.in2 := (marcade.in2 or $40);
    // service
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
  end;
end;

procedure junofrst_loop;
var
  frame_m, frame_s, frame_s_sub: single;
  irq_req: boolean;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := z80_0.tframes;
  frame_s_sub := mcs48_0.tframes;
  irq_req := false;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        // Main CPU
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // Sound CPU
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        // snd sub
        mcs48_0.run(frame_s_sub);
        frame_s_sub := frame_s_sub + mcs48_0.tframes - mcs48_0.contador;
        if f = 239 then
        begin
          if (irq_req and irq_enable) then
            m6809_0.change_irq(ASSERT_LINE);
          update_video_junofrst;
        end;
      end;
      irq_req := not(irq_req);
      events_junofrst;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function junofrst_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $800F, $8100 .. $8FFF:
      junofrst_getbyte := memory[direccion];
    $8010:
      junofrst_getbyte := marcade.dswb; // dsw2
    $8020:
      junofrst_getbyte := marcade.in0;
    $8024:
      junofrst_getbyte := marcade.in1;
    $8028:
      junofrst_getbyte := marcade.in2;
    $802C:
      junofrst_getbyte := marcade.dswa; // dsw1
    $9000 .. $9FFF:
      if m6809_0.opcode then
        junofrst_getbyte := rom_bank_dec[rom_nbank, direccion and $FFF]
      else
        junofrst_getbyte := rom_bank[rom_nbank, direccion and $FFF];
    $A000 .. $FFFF:
      if m6809_0.opcode then
        junofrst_getbyte := mem_opcodes[direccion - $A000]
      else
        junofrst_getbyte := memory[direccion];
  end;
end;

procedure junofrst_putbyte(direccion: word; valor: byte);
var
  color: tcolor;
  procedure draw_blitter;
  var
    i, j, copy, data: byte;
    src, dest: word;
  begin
    src := ((blit_data[2] shl 8) or blit_data[3]) and $FFFC;
    dest := (blit_data[0] shl 8) or blit_data[1];
    copy := blit_data[3] and $01;
    // 16x16 graphics */
    for i := 0 to 15 do
    begin
      for j := 0 to 15 do
      begin
        if (src and 1) <> 0 then
          data := blit_mem[src shr 1] and $0F
        else
          data := blit_mem[src shr 1] shr 4;
        src := src + 1;
        // if there is a source pixel either copy the pixel or clear the pixel depending on the copy flag */
        if (data <> 0) then
        begin
          if (copy = 0) then
            data := 0;
          if (dest and 1) <> 0 then
            memory[dest shr 1] := (memory[dest shr 1] and $0F) or (data shl 4)
          else
            memory[dest shr 1] := (memory[dest shr 1] and $F0) or data;
        end;
        dest := dest + 1;
      end; // del j
      dest := dest + 240;
    end; // del i
  end;

begin
  if direccion > $8FFF then
    exit;
  case direccion of
    $0 .. $7FFF, $8100 .. $8FFF:
      memory[direccion] := valor;
    $8000 .. $800F:
      begin
        color.r := pal3bit(valor shr 0);
        color.g := pal3bit(valor shr 3);
        color.b := pal2bit(valor shr 6);
        set_pal_color(color, direccion and $F);
      end;
    $8030:
      begin
        irq_enable := (valor and 1) <> 0;
        if not(irq_enable) then
          m6809_0.change_irq(CLEAR_LINE);
      end;
    $8031:
      ; // Coin counter...
    $8033:
      scroll_y := valor;
    $8034:
      xorx := ((valor and 1) xor 1) * $FF;
    $8035:
      xory := (valor and 1) * $FF;
    $8040:
      begin
        if ((last_snd_val = 0) and ((valor and 1) = 1)) then
          z80_0.change_irq(HOLD_LINE);
        last_snd_val := valor and 1;
      end;
    $8050:
      sound_latch := valor;
    $8060:
      rom_nbank := valor and $F;
    $8070 .. $8072:
      blit_data[direccion and $3] := valor;
    $8073:
      begin
        blit_data[$3] := valor;
        draw_blitter;
      end;
  end;
end;

function junofrst_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $FFF, $2000 .. $23FF:
      junofrst_snd_getbyte := mem_snd[direccion];
    $3000:
      junofrst_snd_getbyte := sound_latch;
    $4001:
      junofrst_snd_getbyte := ay8910_0.Read;
  end;
end;

procedure junofrst_snd_putbyte(direccion: word; valor: byte);
begin
  if direccion < $1000 then
    exit;
  case direccion of
    $2000 .. $23FF:
      mem_snd[direccion] := valor;
    $4000:
      ay8910_0.Control(valor);
    $4002:
      ay8910_0.Write(valor);
    $5000:
      sound_latch2 := valor;
    $6000:
      mcs48_0.change_irq(ASSERT_LINE);
  end;
end;

function junofrst_sound2_getbyte(direccion: word): byte;
begin
  if direccion < $1000 then
    junofrst_sound2_getbyte := mem_snd_sub[direccion];
end;

function junofrst_sound2_inport(puerto: word): byte;
begin
  if puerto < $100 then
    junofrst_sound2_inport := sound_latch2;
end;

procedure junofrst_sound2_outport(puerto: word; valor: byte);
begin
  case puerto of
    MCS48_PORT_P1:
      dac_0.data8_w(valor);
    MCS48_PORT_P2:
      begin
        if (valor and $80) = 0 then
          mcs48_0.change_irq(CLEAR_LINE);
        i8039_status := (valor and $70) shr 4;
      end;
  end;
end;

function junofrst_portar: byte;
var
  timer: byte;
begin
  timer := (z80_0.totalt div (1024 div 2)) and $F;
  junofrst_portar := (timer shl 4) or i8039_status;
end;

procedure junofrst_portbw(valor: byte); // filter RC
begin
end;

procedure junofrst_sound_update;
begin
  ay8910_0.update;
  dac_0.update;
end;

// Main
procedure reset_junofrst;
begin
  m6809_0.reset;
  z80_0.reset;
  mcs48_0.reset;
  ay8910_0.reset;
  dac_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_enable := false;
  fillchar(blit_data, 4, 0);
  xorx := 0;
  xory := 0;
  last_snd_val := 0;
  sound_latch := 0;
  rom_nbank := 0;
  scroll_y := 0;
  i8039_status := 0;
end;

function start_junofirst: boolean;
var
  f: byte;
  memory_temp, memory_temp_bank: array [0 .. $FFFF] of byte;
begin
  start_junofirst := false;
  machine_calls.general_loop := junofrst_loop;
  machine_calls.reset := reset_junofrst;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(1500000, $100, TCPU_M6809);
  m6809_0.change_ram_calls(junofrst_getbyte, junofrst_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(1789750, $100);
  z80_0.change_ram_calls(junofrst_snd_getbyte, junofrst_snd_putbyte);
  z80_0.init_sound(junofrst_sound_update);
  // Sound CPU 2
  mcs48_0 := cpu_mcs48.Create(8000000, $100, I8039);
  mcs48_0.change_ram_calls(junofrst_sound2_getbyte, nil);
  mcs48_0.change_io_calls(nil, junofrst_sound2_outport, junofrst_sound2_inport, nil);
  // Sound Chip
  ay8910_0 := ay8910_chip.Create(1789750, AY8910, 0.3);
  ay8910_0.change_io_calls(junofrst_portar, nil, nil, junofrst_portbw);
  dac_0 := dac_chip.Create(0.5);
  // cargar roms
  if not(roms_load(@memory, junofrst_rom)) then
    exit;
  konami1_decode(@memory[$A000], @mem_opcodes, $6000);
  if not(roms_load(@memory_temp, junofrst_bank_rom)) then
    exit;
  konami1_decode(@memory_temp, @memory_temp_bank, $C000);
  for f := 0 to $F do
  begin
    copymemory(@rom_bank[f, 0], @memory_temp[f * $1000], $1000);
    copymemory(@rom_bank_dec[f, 0], @memory_temp_bank[f * $1000], $1000);
  end;
  if not(roms_load(@blit_mem, junofrst_blit)) then
    exit;
  // Cargar roms sound
  if not(roms_load(@mem_snd, junofrst_sound)) then
    exit;
  if not(roms_load(@mem_snd_sub, junofrst_sound_sub)) then
    exit;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $7B;
  marcade.dswa_val := @junofrst_dip_a;
  marcade.dswb_val := @junofrst_dip_b;
  // final
  reset_junofrst;
  start_junofirst := true;
end;

end.
