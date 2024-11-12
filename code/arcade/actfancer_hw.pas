unit actfancer_hw;

interface

uses
  WinApi.Windows,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  ym_3812,
  ym_2203,
  oki6295,
  m6502,
  sound_engine,
  hu6280,
  deco_bac06;

function start_actfancer: boolean;

implementation

const
        //Act Fancer
        actfancer_rom:array[0..2] of tipo_roms=(
        (n:'fe08-3.bin';l:$10000;p:0;crc:$35f1999d),(n:'fe09-3.bin';l:$10000;p:$10000;crc:$d21416ca),
        (n:'fe10-3.bin';l:$10000;p:$20000;crc:$85535fcc));
        actfancer_char:array[0..1] of tipo_roms=(
        (n:'15';l:$10000;p:0;crc:$a1baf21e),(n:'16';l:$10000;p:$10000;crc:$22e64730));
        actfancer_sound:tipo_roms=(n:'17-1';l:$8000;p:$8000;crc:$289ad106);
        actfancer_oki:tipo_roms=(n:'18';l:$10000;p:0;crc:$5c55b242);
        actfancer_tiles:array[0..3] of tipo_roms=(
        (n:'14';l:$10000;p:0;crc:$d6457420),(n:'12';l:$10000;p:$10000;crc:$08787b7a),
        (n:'13';l:$10000;p:$20000;crc:$c30c37dc),(n:'11';l:$10000;p:$30000;crc:$1f006d9f));
        actfancer_sprites:array[0..7] of tipo_roms=(
        (n:'02';l:$10000;p:0;crc:$b1db0efc),(n:'03';l:$8000;p:$10000;crc:$f313e04f),
        (n:'06';l:$10000;p:$18000;crc:$8cb6dd87),(n:'07';l:$8000;p:$28000;crc:$dd345def),
        (n:'00';l:$10000;p:$30000;crc:$d50a9550),(n:'01';l:$8000;p:$40000;crc:$34935e93),
        (n:'04';l:$10000;p:$48000;crc:$bcf41795),(n:'05';l:$8000;p:$58000;crc:$d38b94aa));
        actfancer_dip_a:array [0..5] of def_dip2=(
        (mask:3;name:'Coin A';number:4;val4:(0,1,3,2);name4:('3C 1C','2C 1C','1C 1C','1C 2C')),
        (mask:$c;name:'Coin B';number:4;val4:(0,4,$c,8);name4:('3C 1C','2C 1C','1C 1C','1C 2C')),
        (mask:$20;name:'Demo Sounds';number:2;val2:(0,$20);name2:('Off','On')),
        (mask:$40;name:'Flip Screen';number:2;val2:($40,0);name2:('Off','On')),
        (mask:$80;name:'Cabinet';number:2;val2:(0,$80);name2:('Upright','Cocktail')),());
        actfancer_dip_b:array [0..3] of def_dip2=(
        (mask:3;name:'Lives';number:4;val4:(3,2,1,0);name4:('3','4','5','100')),
        (mask:$c;name:'Difficulty';number:4;val4:(4,$c,8,0);name4:('Easy','Normal','Hard','Hardest')),
        (mask:$20;name:'Bonus_Life';number:2;val2:($20,0);name2:('80K','None')),());

var
  rom: array [0 .. $2FFFF] of byte;
  ram: array [0 .. $3FFF] of byte;
  sound_latch: byte;

procedure update_video_actfancer;
begin
  bac06_0.tile_1.update_pf(1, false, false);
  bac06_0.tile_2.update_pf(0, true, false);
  bac06_0.tile_1.show_pf;
  bac06_0.draw_sprites(0, 0, 2);
  bac06_0.tile_2.show_pf;
  update_final_piece(0, 8, 256, 240, 7);
end;

procedure events_actfancer;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $7F)
    else
      marcade.in2 := (marcade.in2 or $80);
    // SYSTEM
    if p_contrls.map_arcade.coin[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
  end;
end;

procedure actfancer_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if main_engine.EmulationPaused = false then
    begin
      if EmulationPaused = false then
      begin
        for f := 0 to $FF do
          case f of
            8:
              marcade.in1 := marcade.in1 and $7F;
            248:
              begin
                h6280_0.set_irq_line(0, HOLD_LINE);
                update_video_actfancer;
                marcade.in1 := marcade.in1 or $80;
              end;
          end;
        // Main
   h6280_0.run(frame_main);
   frame_main:=frame_main+h6280_0.tframes-h6280_0.contador;
        // Sound
   m6502_0.run(frame_snd);
   frame_snd:=frame_snd+m6502_0.tframes-m6502_0.contador;
      end;
      events_actfancer;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function actfancer_getbyte(direccion: dword): byte;
var
  tempw: word;
begin
  case direccion of
    0 .. $2FFFF:
      actfancer_getbyte := rom[direccion];
    $62000 .. $63FFF:
      begin
        tempw := bac06_0.tile_1.data[(direccion and $1FFF) shr 1];
        actfancer_getbyte := tempw shr (8 * (direccion and 1));
      end;
    $72000 .. $727FF:
      begin
        tempw := bac06_0.tile_2.data[(direccion and $7FF) shr 1];
        actfancer_getbyte := tempw shr (8 * (direccion and 1));
      end;
    $100000 .. $1007FF:
      actfancer_getbyte := buffer_sprites[direccion and $7FF];
    $120000 .. $1205FF:
      actfancer_getbyte := buffer_paleta[direccion and $7FF];
    $130000:
      actfancer_getbyte := marcade.in0;
    $130001:
      actfancer_getbyte := marcade.in2;
    $130002:
      actfancer_getbyte := marcade.dswa;
    $130003:
      actfancer_getbyte := marcade.dswb;
    $140000:
      actfancer_getbyte := marcade.in1;
    $1F0000 .. $1F3FFF:
      actfancer_getbyte := ram[direccion and $3FFF];
  end;
end;

procedure actfancer_putbyte(direccion: dword; valor: byte);
var
  tempw: word;
  procedure change_color(dir: word);
  var
    tmp_color: byte;
    color: tcolor;
  begin
    tmp_color := buffer_paleta[dir];
    color.r := pal4bit(tmp_color);
    color.g := pal4bit(tmp_color shr 4);
    tmp_color := buffer_paleta[dir + 1];
    color.b := pal4bit(tmp_color);
    dir := dir shr 1;
    set_pal_color(color, dir);
    case dir of
      0 .. $FF:
        bac06_0.tile_2.buffer_color[dir shr 4] := true;
      $100 .. $1FF:
        bac06_0.tile_1.buffer_color[(dir shr 4) and $F] := true;
    end;
  end;

begin
  case direccion of
    0 .. $2FFFF:
      ;
    $60000 .. $60007:
      bac06_0.tile_1.change_control0_8b(direccion, valor);
    $60010 .. $6001F:
      bac06_0.tile_1.change_control1_8b_swap(direccion, valor);
    $62000 .. $63FFF:
      bac06_0.tile_1.write_tile_data_8b_swap(direccion, valor, $1FFF);
    $70000 .. $70007:
      bac06_0.tile_2.change_control0_8b(direccion, valor);
    $70010 .. $7001F:
      bac06_0.tile_2.change_control1_8b_swap(direccion, valor);
    $72000 .. $727FF:
      bac06_0.tile_2.write_tile_data_8b_swap(direccion, valor, $7FF);
    $100000 .. $1007FF:
      buffer_sprites[direccion and $7FF] := valor;
    $110000:
      bac06_0.update_sprite_data(@buffer_sprites);
    $120000 .. $1205FF:
      if buffer_paleta[direccion and $7FF] <> valor then
      begin
        buffer_paleta[direccion and $7FF] := valor;
        change_color((direccion and $7FE));
      end;
    $150000:
      begin
        sound_latch := valor;
        m6502_0.change_nmi(PULSE_LINE);
      end;
    $160000:
      ;
    $1F0000 .. $1F3FFF:
      ram[direccion and $3FFF] := valor;
  end;
end;

function actfancer_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $3000:
      actfancer_snd_getbyte := sound_latch;
    $3800:
      actfancer_snd_getbyte := oki_6295_0.read;
    0 .. $7FF, $4000 .. $FFFF:
      actfancer_snd_getbyte := mem_snd[direccion];
  end;
end;

procedure actfancer_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      mem_snd[direccion] := valor;
    $800:
      ym2203_0.Control(valor);
    $801:
      ym2203_0.Write(valor);
    $1000:
      ym3812_0.Control(valor);
    $1001:
      ym3812_0.Write(valor);
    $3800:
      oki_6295_0.Write(valor);
    $4000 .. $FFFF:
      ;
  end;
end;

procedure actfancer_sound_update;
begin
  ym3812_0.update;
  ym2203_0.update;
  oki_6295_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  m6502_0.change_irq(irqstate);
end;

// Main
procedure reset_actfancer;
begin
  h6280_0.reset;
  m6502_0.reset;
 frame_main:=h6280_0.tframes;
 frame_snd:=m6502_0.tframes;
  ym3812_0.reset;
  ym2203_0.reset;
  oki_6295_0.reset;
  bac06_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $7F;
  marcade.in2 := $FF;
  sound_latch := 0;
  machine_calls.caption := 'Act-Fancer Cybernetick Hyper Weapon';
end;

function start_actfancer: boolean;
const
  pt_x: array [0 .. 15] of dword = (16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 16 * 8 + 4, 16 * 8 + 5, 16 * 8 + 6, 16 * 8 + 7, 0, 1, 2, 3, 4, 5, 6, 7);
  pt_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 8 * 8, 9 * 8, 10 * 8, 11 * 8, 12 * 8, 13 * 8, 14 * 8, 15 * 8);
var
  memory_temp: array [0 .. $7FFFF] of byte;
begin
  machine_calls.general_loop := actfancer_loop;
  machine_calls.reset := reset_actfancer;
  start_actfancer := false;
  start_audio(false);
  // El video se inicia en el chip bac06!!!
  bac06_0 := bac06_chip.create(false, false, false, $100, 0, 0, 2, 1, 1, $200);
  // Main CPU
  h6280_0 := cpu_h6280.create(21477200 div 3, $100);
  h6280_0.change_ram_calls(actfancer_getbyte, actfancer_putbyte);
  // Sound CPU
  m6502_0 := cpu_m6502.create(1500000, 256, TCPU_M6502);
  m6502_0.change_ram_calls(actfancer_snd_getbyte, actfancer_snd_putbyte);
  m6502_0.init_sound(actfancer_sound_update);
  // Sound Chips
ym3812_0:=ym3812_chip.create(YM3812_FM,3000000);
  ym3812_0.change_irq_calls(snd_irq);
ym2203_0:=ym2203_chip.create(15000000,0.5);
  oki_6295_0 := snd_okim6295.create(1024188, OKIM6295_PIN7_HIGH, 0.85);
  case main_vars.machine_type of
    165:
      begin // Act Fancer
        // check roms
        if not(roms_load(@rom, actfancer_rom)) then
          exit;
        // check audio
        if not(roms_load(@mem_snd, actfancer_sound)) then
          exit;
        // check OKI rom
        if not(roms_load(oki_6295_0.get_rom_addr, actfancer_oki)) then
          exit;
        // convert chars
        if not(roms_load(@memory_temp, actfancer_char)) then
          exit;
        init_gfx(0, 8, 8, $1000);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(4,0,8*8,$8000*8,$18000*8,0,$10000*8);
        convert_gfx(0, 0, @memory_temp, @pt_x[8], @pt_y, false, false);
        // tiles 1
        if not(roms_load(@memory_temp, actfancer_tiles)) then
          exit;
        init_gfx(1,16,16,$c00);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, $10000 * 8, $20000 * 8, $30000 * 8);
        convert_gfx(1, 0, @memory_temp, @pt_x, @pt_y, false, false);
        // sprites
        if not(roms_load(@memory_temp, actfancer_sprites)) then
          exit;
        init_gfx(2,16,16,$c00);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(4, 0, 32 * 8, 0, $18000 * 8, $30000 * 8, $48000 * 8);
        convert_gfx(2, 0, @memory_temp, @pt_x, @pt_y, false, false);
        // Dip
        marcade.dswa := $7F;
        marcade.dswa_val2 := @actfancer_dip_a;
        marcade.dswb := $FF;
        marcade.dswb_val2 := @actfancer_dip_b;
      end;
  end;
  // final
  reset_actfancer;
  start_actfancer := true;
end;

end.
