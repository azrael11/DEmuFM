unit boogiewings_hw;

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
  deco_decr,
  deco_common,
  deco_104,
  misc_functions,
  FMX.Dialogs;

function start_boogiewings: boolean;

implementation

const
        boogwing_rom:array[0..3] of tipo_roms=(
        (n:'kn_00-2.2b';l:$40000;p:0;crc:$e38892b9),(n:'kn_02-2.2e';l:$40000;p:$1;crc:$8426efef),
        (n:'kn_01-2.4b';l:$40000;p:$80000;crc:$3ad4b54c),(n:'kn_03-2.4e';l:$40000;p:$80001;crc:$10b61f4a));
        boogwing_sound:tipo_roms=(n:'km06.18p';l:$10000;p:$0;crc:$3e8bc4e1);
        boogwing_char:array[0..1] of tipo_roms=(
        (n:'km05.9e';l:$10000;p:0;crc:$d10aef95),(n:'km04.8e';l:$10000;p:$1;crc:$329323a8));
        boogwing_tiles1:array[0..1] of tipo_roms=(
        (n:'mbd-01.9b';l:$100000;p:0;crc:$d7de4f4b),(n:'mbd-00.8b';l:$100000;p:$100000;crc:$adb20ba9));
        boogwing_tiles2:array[0..1] of tipo_roms=(
        (n:'mbd-03.13b';l:$100000;p:0;crc:$cf798f2c),(n:'mbd-04.14b';l:$100000;p:$100000;crc:$d9764d0b));
        boogwing_tiles1_1:tipo_roms=(n:'mbd-02.10e';l:$80000;p:0;crc:$b25aa721);
        boogwing_sprites1:array[0..1] of tipo_roms=(
        (n:'mbd-05.16b';l:$200000;p:$200000;crc:$1768c66a),(n:'mbd-06.17b';l:$200000;p:$0;crc:$7750847a));
        boogwing_sprites2:array[0..1] of tipo_roms=(
        (n:'mbd-07.18b';l:$200000;p:$200000;crc:$241faac1),(n:'mbd-08.19b';l:$200000;p:$0;crc:$f13b1e56));
        boogwing_oki1:tipo_roms=(n:'mbd-10.17p';l:$80000;p:0;crc:$f159f76a);
        boogwing_oki2:tipo_roms=(n:'mbd-09.16p';l:$80000;p:0;crc:$f44f2f87);
        boogwing_dip_a:array [0..7] of def_dip2=(
        (mask:7;name:'Coin A';number:8;val8:(0,1,7,6,5,4,3,2);name8:('3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C','1C 6C')),
        (mask:$38;name:'Coin B';number:8;val8:(0,8,$38,$30,$28,$20,$18,$10);name8:('3C 1C','2C 1C','1C 1C','1C 2C','1C 3C','1C 4C','1C 5C','1C 6C')),
        (mask:$40;name:'Flip Screen';number:2;val2:($40,0);name2:('Off','On')),
        (mask:$80;name:'Continue Coin';number:2;val2:($80,0);name2:('1 Start/1 Continue','2 Start/1 Continue')),
        (mask:$300;name:'Lives';number:4;val4:($100,0,$300,$200);name4:('1','2','3','4')),
        (mask:$c00;name:'Difficulty';number:4;val4:($800,$c00,$400,0);name4:('Easy','Normal','Hard','Very Hard')),
        (mask:$1000;name:'Free Play';number:2;val2:($1000,0);name2:('Off','On')),());

var
  rom_opcode, rom_data: array [0 .. $7FFFF] of word;
  ram: array [0 .. $7FFF] of word;
  oki1_mem, oki2_mem: pbyte;

procedure update_video_boogwing;
begin
  deco16ic_1.update_pf_1(5, false);
  deco16ic_1.update_pf_2(5, true);
  deco16ic_0.update_pf_2(5, true);
  deco16ic_0.update_pf_1(5, true);
  deco_sprites_0.draw_sprites;
  deco_sprites_1.draw_sprites;
  update_final_piece(0, 8, 320, 240, 5);
end;

procedure events_boogwing;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFFB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFF7)
    else
      marcade.in0 := (marcade.in0 or $8);
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
    if p_contrls.map_arcade.but2[1] then
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

procedure boogwing_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := h6280_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 273 do
      begin
	  	events_boogwing;
        case f of
          248:
            begin
              m68000_0.irq[6] := HOLD_LINE;
              update_video_boogwing;
              marcade.in1 := marcade.in1 or $8;
            end;
          8:
            marcade.in1 := marcade.in1 and $7;
        end;
        // Main
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // Sound
        h6280_0.run(trunc(frame_s));
        frame_s := frame_s + h6280_0.tframes - h6280_0.contador;
      end;      
      video_sync;
    end
    else
      pause_action;
  end;
end;

function boogwing_protection_region_0_104_r(real_address: word): word;
var
  deco146_addr, data: word;
  cs: byte;
begin
  // int real_address = 0 + (offset *2);
  deco146_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
  cs := 0;
  data := deco104_0.read_data(deco146_addr, cs);
  boogwing_protection_region_0_104_r := data;
end;

function boogwing_getword(direccion: dword): word;
begin
  case direccion of
    $0 .. $FFFFF:
      if m68000_0.opcode then
        boogwing_getword := rom_opcode[direccion shr 1]
      else
        boogwing_getword := rom_data[direccion shr 1];
    $200000 .. $20FFFF:
      boogwing_getword := ram[(direccion and $FFFF) shr 1];
    $242000 .. $2427FF:
      boogwing_getword := deco_sprites_0.ram[(direccion and $7FF) shr 1];
    $246000 .. $2467FF:
      boogwing_getword := deco_sprites_1.ram[(direccion and $7FF) shr 1];
    $24E000 .. $24EFFF:
      boogwing_getword := boogwing_protection_region_0_104_r(direccion and $FFF);
    $264000 .. $265FFF:
      boogwing_getword := deco16ic_0.pf1.data[(direccion and $1FFF) shr 1];
    $266000 .. $267FFF:
      boogwing_getword := deco16ic_0.pf2.data[(direccion and $1FFF) shr 1];
    $268000 .. $268FFF:
      boogwing_getword := deco16ic_0.pf1.rowscroll[(direccion and $FFF) shr 1];
    $26A000 .. $26AFFF:
      boogwing_getword := deco16ic_0.pf2.rowscroll[(direccion and $FFF) shr 1];
    $274000 .. $275FFF:
      boogwing_getword := deco16ic_1.pf1.data[(direccion and $1FFF) shr 1];
    $276000 .. $277FFF:
      boogwing_getword := deco16ic_1.pf2.data[(direccion and $1FFF) shr 1];
    $278000 .. $278FFF:
      boogwing_getword := deco16ic_1.pf1.rowscroll[(direccion and $FFF) shr 1];
    $27A000 .. $27AFFF:
      boogwing_getword := deco16ic_1.pf2.rowscroll[(direccion and $FFF) shr 1];
    $284000 .. $285FFF:
      ; // deco ace
    $3C0000 .. $3C004F:
      ; // deco ace
  end;
end;

procedure boogwing_protection_region_0_104_w(real_address, data: word);
var
  deco146_addr: word;
  cs: byte;
begin
  // int real_address = 0 + (offset *2);
  deco146_addr := BITSWAP32(real_address, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 13, 12, 11, 17, 16, 15, 14, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) and $7FFF;
  cs := 0;
  deco104_0.write_data(deco146_addr, data, cs);
end;

procedure boogwing_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $FFFFF:
      ;
    $200000 .. $20FFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
    $242000 .. $2427FF:
      deco_sprites_0.ram[(direccion and $7FF) shr 1] := valor;
    $246000 .. $2467FF:
      deco_sprites_1.ram[(direccion and $7FF) shr 1] := valor;
    $24E000 .. $24EFFF:
      boogwing_protection_region_0_104_w(direccion and $FFF, valor);
    $260000 .. $26000F:
      deco16ic_0.control_w((direccion and $F) shr 1, valor);
    $264000 .. $265FFF:
      begin
        deco16ic_0.pf1.data[(direccion and $1FFF) shr 1] := valor;
        deco16ic_0.pf1.buffer[(direccion and $1FFF) shr 1] := true
      end;
    $266000 .. $267FFF:
      begin
        deco16ic_0.pf2.data[(direccion and $1FFF) shr 1] := valor;
        deco16ic_0.pf2.buffer[(direccion and $1FFF) shr 1] := true
      end;
    $268000 .. $268FFF:
      deco16ic_0.pf1.rowscroll[(direccion and $FFF) shr 1] := valor;
    $26A000 .. $26AFFF:
      deco16ic_0.pf2.rowscroll[(direccion and $FFF) shr 1] := valor;
    $270000 .. $27000F:
      deco16ic_1.control_w((direccion and $F) shr 1, valor);
    $274000 .. $275FFF:
      begin
        deco16ic_1.pf1.data[(direccion and $1FFF) shr 1] := valor;
        deco16ic_1.pf1.buffer[(direccion and $1FFF) shr 1] := true
      end;
    $276000 .. $277FFF:
      begin
        deco16ic_1.pf2.data[(direccion and $1FFF) shr 1] := valor;
        deco16ic_1.pf2.buffer[(direccion and $1FFF) shr 1] := true
      end;
    $278000 .. $278FFF:
      deco16ic_1.pf1.rowscroll[(direccion and $FFF) shr 1] := valor;
    $27A000 .. $27AFFF:
      deco16ic_1.pf2.rowscroll[(direccion and $FFF) shr 1] := valor;
    $282008:
      ; // deco ace
    $284000 .. $285FFF:
      ; // deco ace
    $3C0000 .. $3C004F:
      ; // deco ace
  end;
end;

function boogwing_bank_callback(bank: word): word;
begin
  boogwing_bank_callback := ((bank shr 4) and $7) * $1000;
end;

function boogwing_bank_callback2(bank: word): word;
var
  offset: word;
begin
  offset := ((bank shr 4) and $7) * $1000;
  if ((bank and $F) = $A) then
    offset := offset + $800; // strange - transporter level
  boogwing_bank_callback2 := offset;
end;

procedure sound_bank_rom(valor: byte);
var
  temp: pbyte;
begin
  temp := oki1_mem;
  inc(temp, (valor and 1) * $40000);
  copymemory(oki_6295_0.get_rom_addr, temp, $40000);
end;

// Main
procedure reset_boogwing;
begin
  m68000_0.reset;
  deco16ic_0.reset;
  deco104_0.reset;
  copymemory(oki_6295_0.get_rom_addr, oki1_mem, $40000);
  deco16_snd_simple_reset;
  deco_sprites_0.reset;
  deco_sprites_1.reset;
  marcade.in0 := $FFFF;
  marcade.in1 := $7;
end;

procedure close_boogwing;
begin
  if oki1_mem <> nil then
    freemem(oki1_mem);
  if oki2_mem <> nil then
    freemem(oki2_mem);
  oki1_mem := nil;
  oki2_mem := nil;
end;

function start_boogiewings: boolean;
const
  pt_x: array [0 .. 15] of dword = (256, 257, 258, 259, 260, 261, 262, 263, 0, 1, 2, 3, 4, 5, 6, 7);
  pt_y: array [0 .. 15] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16, 8 * 16, 9 * 16, 10 * 16, 11 * 16, 12 * 16, 13 * 16, 14 * 16, 15 * 16);
  ps_x: array [0 .. 15] of dword = (512, 513, 514, 515, 516, 517, 518, 519, 0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 8 * 32, 9 * 32, 10 * 32, 11 * 32, 12 * 32, 13 * 32, 14 * 32, 15 * 32);
var
  memoria_temp, memoria_temp2: pbyte;
  memoria_temp_rom: pword;
  colores: tpaleta;
  f: word;
begin
  start_boogiewings := false;
  machine_calls.general_loop := boogwing_loop;
  machine_calls.close := close_boogwing;
  machine_calls.reset := reset_boogwing;
  machine_calls.fps_max := 58;
  start_audio(false);
  // Pantallas
  deco16ic_0 := chip_16ic.create(1, 2, $0, $0, $F, $F, 0, 1, 0, 0, nil, boogwing_bank_callback);
  deco16ic_1 := chip_16ic.create(3, 4, $0, $0, $F, $F, 0, 2, 0, 16, boogwing_bank_callback2, boogwing_bank_callback2);
  deco_sprites_0 := tdeco16_sprite.create(3, 5, 304, $500, $7FFF, true);
  deco_sprites_1 := tdeco16_sprite.create(4, 5, 304, $700, $7FFF, true);
  screen_init(5, 512, 512, false, true);
  start_video(320, 240);
  getmem(memoria_temp, $400000);
  getmem(memoria_temp2, $100000);
  getmem(memoria_temp_rom, $100000);
  // Main CPU
  m68000_0 := cpu_m68000.create(14000000, 274);
  m68000_0.change_ram16_calls(boogwing_getword, boogwing_putword);
  if not(roms_load16w(memoria_temp_rom, boogwing_rom)) then
    exit;
  deco102_decrypt_cpu(memoria_temp_rom, @rom_opcode, @rom_data, $42BA, $0, $18, $100000);
  // Sound CPU
  deco16_snd_simple_init(32220000 div 12, 32220000, sound_bank_rom, 274);
  if not(roms_load(@mem_snd, boogwing_sound)) then
    exit;
  // OKI rom
  getmem(oki1_mem, $80000);
  if not(roms_load(oki1_mem, boogwing_oki1)) then
    exit;
  getmem(oki2_mem, $80000);
  if not(roms_load(oki2_mem, boogwing_oki2)) then
    exit;
  // tiles1
  if not(roms_load16b(memoria_temp, boogwing_char)) then
    exit;
  deco56_decrypt_gfx(memoria_temp, $20000);
  init_gfx(0, 8, 8, $1000);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 16 * 8, $10000 * 8 + 8, $10000 * 8 + 0, 8, 0);
  convert_gfx(0, 0, memoria_temp, @pt_x[8], @pt_y, false, false);
  // tiles2
  if not(roms_load(memoria_temp, boogwing_tiles1)) then
    exit;
  if not(roms_load16b(memoria_temp2, boogwing_tiles1_1)) then
    exit;
  deco56_decrypt_gfx(memoria_temp, $200000);
  deco56_remap_gfx(memoria_temp2, $100000);
  copymemory(@memoria_temp[$200000], memoria_temp2, $100000);
  init_gfx(1, 16, 16, $4000);
  gfx[1].trans[0] := true;
  gfx_set_desc_data(5, 0, 32 * 16, $200000 * 8, $100000 * 8 + 8, $100000 * 8 + 0, 8, 0);
  convert_gfx(1, 0, memoria_temp, @pt_x, @pt_y, false, false);
  // tiles3
  if not(roms_load(memoria_temp, boogwing_tiles2)) then
    exit;
  deco56_decrypt_gfx(memoria_temp, $200000);
  init_gfx(2, 16, 16, $4000);
  gfx[2].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 16, $100000 * 8 + 8, $100000 * 8 + 0, 8, 0);
  convert_gfx(2, 0, memoria_temp, @pt_x, @pt_y, false, false);
  // Sprites
  if not(roms_load(memoria_temp, boogwing_sprites1)) then
    exit;
  init_gfx(3, 16, 16, $8000);
  gfx[3].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 16, $100000 * 8 + 8, $100000 * 8 + 0, 8, 0);
  convert_gfx(3, 0, memoria_temp, @pt_x, @pt_y, false, false);
  if not(roms_load(memoria_temp, boogwing_sprites2)) then
    exit;
  init_gfx(4, 16, 16, $8000);
  gfx[4].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 16, $100000 * 8 + 8, $100000 * 8 + 0, 8, 0);
  convert_gfx(4, 0, memoria_temp, @pt_x, @pt_y, false, false);
  deco104_0 := cpu_deco_104.create(INTERFACE_SCRAMBLE_REVERSE or USE_MAGIC_ADDRESS_XOR);
  for f := 0 to 4095 do
  begin
    colores[f].r := random(256);
    colores[f].g := random(256);
    colores[f].b := random(256);
  end;
  set_pal(colores, 4096);
  // Dip
  marcade.dswa := $FFFF;
  marcade.dswa_val2 := @boogwing_dip_a;
  // final
  freemem(memoria_temp);
  freemem(memoria_temp2);
  freemem(memoria_temp_rom);
  start_boogiewings := true;
end;

end.
