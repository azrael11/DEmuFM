unit ajax_hw;

interface

uses
  WinApi.Windows,
  nz80,
  konami,
  hd6309,
  main_engine,
  controls_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ym_2151,
  k052109,
  k051960,
  k007232,
  k051316,
  FMX.Dialogs;

function start_ajax: boolean;

implementation

const
  // ajax
  ajax_rom: array [0 .. 1] of tipo_roms = ((n: '770_m01.n11'; l: $10000; p: 0; crc: $4A64E53A),
    (n: '770_l02.n12'; l: $10000; p: $10000; crc: $AD7D592B));
  ajax_sub: array [0 .. 1] of tipo_roms = ((n: '770_l05.i16'; l: $8000; p: 0; crc: $ED64FBB2),
    (n: '770_f04.g16'; l: $10000; p: $8000; crc: $E0E4EC9C));
  ajax_sound: tipo_roms = (n: '770_h03.f16'; l: $8000; p: 0; crc: $2FFD2AFC);
  ajax_tiles: array [0 .. 7] of tipo_roms = ((n: '770c13-a.f3'; l: $10000; p: 0; crc: $4EF6FFF2),
    (n: '770c13-c.f4'; l: $10000; p: 1; crc: $97FFBAB6), (n: '770c12-a.f5'; l: $10000; p: 2;
    crc: $6C0ADE68), (n: '770c12-c.f6'; l: $10000; p: 3; crc: $61FC39CC), (n: '770c13-b.e3';
    l: $10000; p: $40000; crc: $86FDD706), (n: '770c13-d.e4'; l: $10000; p: $40001; crc: $7D7ACB2D),
    (n: '770c12-b.e5'; l: $10000; p: $40002; crc: $5F221CC6), (n: '770c12-d.e6'; l: $10000;
    p: $40003; crc: $F1EDB2F4));
  ajax_sprites: array [0 .. 15] of tipo_roms = ((n: '770c09-a.f8'; l: $10000; p: 0; crc: $76690FB8),
    (n: '770c09-e.f9'; l: $10000; p: 1; crc: $17B482C9), (n: '770c08-a.f10'; l: $10000; p: 2;
    crc: $EFD29A56), (n: '770c08-e.f11'; l: $10000; p: 3; crc: $6D43AFDE), (n: '770c09-b.e8';
    l: $10000; p: $40000; crc: $CD1709D1), (n: '770c09-f.e9'; l: $10000; p: $40001; crc: $CBA4B47E),
    (n: '770c08-b.e10'; l: $10000; p: $40002; crc: $F3374014), (n: '770c08-f.e11'; l: $10000;
    p: $40003; crc: $F5BA59AA), (n: '770c09-c.d8'; l: $10000; p: $80000; crc: $BFD080B8),
    (n: '770c09-g.d9'; l: $10000; p: $80001; crc: $77D58EA0), (n: '770c08-c.d10'; l: $10000;
    p: $80002; crc: $28E7088F), (n: '770c08-g.d11'; l: $10000; p: $80003; crc: $17DA8F6D),
    (n: '770c09-d.c8'; l: $10000; p: $C0000; crc: $6F955600), (n: '770c09-h.c9'; l: $10000;
    p: $C0001; crc: $494A9090), (n: '770c08-d.c10'; l: $10000; p: $C0002; crc: $91591777),
    (n: '770c08-h.c11'; l: $10000; p: $C0003; crc: $D97D4B15));
  ajax_zoom: array [0 .. 1] of tipo_roms = ((n: '770c06.f4'; l: $40000; p: 0; crc: $D0C592EE),
    (n: '770c07.h4'; l: $40000; p: $40000; crc: $0B399FB1));
  ajax_k007232_1: array [0 .. 3] of tipo_roms = ((n: '770c10-a.a7'; l: $10000; p: 0;
    crc: $E45EC094), (n: '770c10-b.a6'; l: $10000; p: $10000; crc: $349DB7D3), (n: '770c10-c.a5';
    l: $10000; p: $20000; crc: $71CB1F05), (n: '770c10-d.a4'; l: $10000; p: $30000;
    crc: $E8AB1844));
  ajax_k007232_2: array [0 .. 7] of tipo_roms = ((n: '770c11-a.c6'; l: $10000; p: 0;
    crc: $8CCCD9E0), (n: '770c11-b.c5'; l: $10000; p: $10000; crc: $0AF2FEDD), (n: '770c11-c.c4';
    l: $10000; p: $20000; crc: $7471F24A), (n: '770c11-d.c3'; l: $10000; p: $30000; crc: $A58BE323),
    (n: '770c11-e.b7'; l: $10000; p: $40000; crc: $DD553541), (n: '770c11-f.b6'; l: $10000;
    p: $50000; crc: $3F78BD0F), (n: '770c11-g.b5'; l: $10000; p: $60000; crc: $078C51B2),
    (n: '770c11-h.b4'; l: $10000; p: $70000; crc: $7300C2E1));
  // DIP
        ajax_dip_a:array [0..2] of def_dip2=(
        (mask:$0f;name:'Coin A';number:16;val16:(2,5,8,4,1,$f,3,7,$e,6,$d,$c,$b,$a,9,0);name16:('4C 1C','3C 1C','2C 1C','3C 2C','4C 3C','1C 1C','3C 4C','2C 3C','1C 2C','2C 5C','1C 3C','1C 4C','1C 5C','1C 6C','1C 7C','Free Play')),
        (mask:$f0;name:'Coin B';number:16;val16:($20,$50,$80,$40,$10,$f0,$30,$70,$e0,$60,$d0,$c0,$b0,$a0,$90,0);name16:('4C 1C','3C 1C','2C 1C','3C 2C','4C 3C','1C 1C','3C 4C','2C 3C','1C 2C','2C 5C','1C 3C','1C 4C','1C 5C','1C 6C','1C 7C','No Coin')),());
        ajax_dip_b:array [0..5] of def_dip2=(
        (mask:$3;name:'Lives';number:4;val4:(3,2,1,0);name4:('2','3','5','7')),
        (mask:$4;name:'Cabinet';number:2;val2:(0,4);name2:('Upright','Cocktail')),
        (mask:$18;name:'Bonus Life';number:4;val4:($18,$10,8,0);name4:('30K 150K','10K 200K','30K','50K')),
        (mask:$60;name:'Difficulty';number:4;val4:($60,$40,$20,0);name4:('Easy','Normal','Hard','Very Hard')),
        (mask:$80;name:'Demo Sounds';number:2;val2:($80,0);name2:('Off','On')),());
        ajax_dip_c:array [0..2] of def_dip2=(
        (mask:$1;name:'Flip Screen';number:2;val2:(1,0);name2:('Off','On')),
        (mask:$8;name:'Control in 3D Stages';number:2;val2:(8,0);name2:('Normal','Inverted')),());

var
  tiles_rom, sprite_rom, k007232_1_rom, k007232_2_rom, zoom_rom: pbyte;
  sound_latch, rom_bank1, rom_bank2: byte;
  sub_firq_enable, prioridad: boolean;
  rom_bank: array [0 .. 11, 0 .. $1FFF] of byte;
  rom_sub_bank: array [0 .. 8, 0 .. $1FFF] of byte;

procedure ajax_cb(layer, bank: word; var code: dword; var color: word; var flags: word;
  var priority: word);
const
  layer_colorbase: array [0 .. 2] of byte = (1024 div 16, 0 div 16, 512 div 16);
begin
  code := code or (((color and $0F) shl 8) or (bank shl 12));
  color := layer_colorbase[layer] + ((color and $F0) shr 4);
end;

procedure ajax_sprite_cb(var code: word; var color: word; var pri: word; var shadow: word);
begin
  { priority bits:
    4 over zoom (0 = have priority)
    5 over B    (0 = have priority)
    6 over A    (1 = have priority)
    never over F }
  if (color and $20) <> 0 then
    pri := 1 // Por debajo de B
  else
    pri := 0; // Por encima de B
  if (color and $10) <> 0 then
    pri := 3 // Z = 4
  else
    pri := 2;
  if (color and $40) = 0 then
    pri := 5 // A = 2
  else
    pri := 4;
  color := 16 + (color and $F);
end;

procedure ajax_k051316_cb(var code: word; var color: word; var priority_mask: word);
begin
  code := code or ((color and $07) shl 8);
  color := 6 + ((color and $08) shr 3);
end;

procedure ajax_k007232_cb_0(valor: byte);
begin
  k007232_0.set_volume(0, (valor shr 4) * $11, 0);
  k007232_0.set_volume(1, 0, (valor and $F) * $11);
end;

procedure ajax_k007232_cb_1(valor: byte);
begin
  k007232_1.set_volume(1, (valor and $0F) * ($11 shr 1), (valor shr 4) * ($11 shr 1));
end;

procedure ajax_k051960_cb(state: byte);
begin
  konami_0.change_irq(state);
end;

procedure update_video_ajax;
begin
  k052109_0.draw_tiles;
  k051960_0.update_sprites;
  k051960_0.draw_sprites(1, -1);
  k052109_0.draw_layer(2, 5); // B
  k051960_0.draw_sprites(0, -1);
  if prioridad then
  begin
    k051960_0.draw_sprites(3, -1);
    k051316_0.draw(5);
    k051960_0.draw_sprites(2, -1);
    k051960_0.draw_sprites(5, -1);
    k052109_0.draw_layer(1, 5); // A
    k051960_0.draw_sprites(4, -1);
  end
  else
  begin
    k051960_0.draw_sprites(5, -1);
    k052109_0.draw_layer(1, 5); // A
    k051316_0.draw(5);
    k051960_0.draw_sprites(4, -1);
    k051960_0.draw_sprites(3, -1);
    k051960_0.draw_sprites(2, -1);
  end;
  k052109_0.draw_layer(0, 5); // F
  update_final_piece(112, 16, 304, 224, 5);
end;

procedure events_ajax;
begin
  if main_vars.service1 then
    marcade.dswc := (marcade.dswc and $FB)
  else
    marcade.dswc := (marcade.dswc or $4);
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.but2[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.but2[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure ajax_loop;
var
  frame_m, frame_sub, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := konami_0.tframes;
  frame_sub := hd6309_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin

      for f := 0 to $FF do
      begin
        // main
        konami_0.run(frame_m);
        frame_m := frame_m + konami_0.tframes - konami_0.contador;
        // sub
        hd6309_0.run(frame_sub);
        frame_sub := frame_sub + hd6309_0.tframes - hd6309_0.contador;
        // sound
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        k051960_0.update_line(f);
        if f = 239 then
          update_video_ajax;
      end;
      events_ajax;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function ajax_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $1C0:
      case ((direccion and $1C0) shr 6) of
        0:
          ajax_getbyte := random(256);
        4:
          ajax_getbyte := marcade.in1;
        6:
          case (direccion and 3) of
            0:
              ajax_getbyte := marcade.in2; // system
            1:
              ajax_getbyte := marcade.in0; // p1
            2:
              ajax_getbyte := marcade.dswa; // dsw1
            3:
              ajax_getbyte := marcade.dswb; // dsw2
          end;
        7:
          ajax_getbyte := marcade.dswc; // DSW3
      else
        ajax_getbyte := $FF;
      end;
    $800 .. $807:
      ajax_getbyte := k051960_0.k051937_read(direccion and $7);
    $C00 .. $FFF:
      ajax_getbyte := k051960_0.read(direccion - $C00);
    $1000 .. $1FFF:
      ajax_getbyte := buffer_paleta[direccion and $FFF];
    $2000 .. $5FFF, $8000 .. $FFFF:
      ajax_getbyte := memory[direccion];
    $6000 .. $7FFF:
      ajax_getbyte := rom_bank[rom_bank1, direccion and $1FFF];
  end;
end;

procedure ajax_putbyte(direccion: word; valor: byte);

  procedure change_color(pos: word);
  var
    color: tcolor;
    valor: word;
  begin
    valor := (buffer_paleta[pos * 2] shl 8) + buffer_paleta[(pos * 2) + 1];
    color.b := pal5bit(valor shr 10);
    color.g := pal5bit(valor shr 5);
    color.r := pal5bit(valor);
    set_pal_color_alpha(color, pos);
    k052109_0.clean_video_buffer;
    k051316_0.clean_video_buffer;
  end;

begin
  case direccion of
    0 .. $1C0:
      case ((direccion and $1C0) shr 6) of
        0:
          if (direccion = 0) then
            if (sub_firq_enable) then
              hd6309_0.change_firq(HOLD_LINE);
        1:
          z80_0.change_irq(HOLD_LINE);
        2:
          sound_latch := valor;
        3:
          begin
            rom_bank1 := ((not(valor) and $80) shr 5) + (valor and 7);
            prioridad := (valor and 8) <> 0;
          end;
        5:
          ;
      end;
    $800 .. $807:
      k051960_0.k051937_write(direccion and $7, valor);
    $C00 .. $FFF:
      k051960_0.write(direccion - $C00, valor);
    $1000 .. $1FFF:
      if buffer_paleta[direccion and $FFF] <> valor then
      begin
        buffer_paleta[direccion and $FFF] := valor;
        change_color((direccion and $FFF) shr 1);
      end;
    $2000 .. $5FFF:
      memory[direccion] := valor;
    $6000 .. $FFFF:
      ; // ROM
  end;
end;

// Sub CPU
function ajax_sub_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FF:
      ajax_sub_getbyte := k051316_0.read(direccion);
    $1000 .. $17FF:
      ajax_sub_getbyte := k051316_0.rom_read(direccion and $7FF);
    $2000 .. $3FFF:
      ajax_sub_getbyte := memory[direccion];
    $4000 .. $7FFF:
      ajax_sub_getbyte := k052109_0.read(direccion and $3FFF);
    $8000 .. $9FFF:
      ajax_sub_getbyte := rom_sub_bank[rom_bank2, direccion and $1FFF];
    $A000 .. $FFFF:
      ajax_sub_getbyte := mem_misc[direccion];
  end;
end;

procedure ajax_sub_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FF:
      k051316_0.write(direccion, valor);
    $800 .. $80F:
      k051316_0.control_w(direccion and $F, valor);
    $1800:
      begin
        // enable char ROM reading through the video RAM
        if (valor and $40) <> 0 then
          k052109_0.set_rmrd_line(ASSERT_LINE)
        else
          k052109_0.set_rmrd_line(CLEAR_LINE);
        // bit 5 enables 051316 wraparound
        // m_k051316->wraparound_enable(data & 0x20);
        // FIRQ control
        sub_firq_enable := (valor and $10) <> 0;
        // bank # (ROMS G16 and I16)
        rom_bank2 := valor and $0F;
      end;
    $2000 .. $3FFF:
      memory[direccion] := valor;
    $4000 .. $7FFF:
      k052109_0.write(direccion and $3FFF, valor);
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

// Sound
function ajax_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $87FF:
      ajax_snd_getbyte := mem_snd[direccion];
    $A000 .. $A00D:
      ajax_snd_getbyte := k007232_0.read(direccion and $F);
    $B000 .. $B00D:
      ajax_snd_getbyte := k007232_1.read(direccion and $F);
    $C001:
      ajax_snd_getbyte := ym2151_0.status;
    $E000:
      ajax_snd_getbyte := sound_latch;
  end;
end;

procedure ajax_snd_bankswitch(valor: byte);
begin
  k007232_0.set_bank((valor shr 1) and 1, valor and 1);
  k007232_1.set_bank((valor shr 4) and 3, (valor shr 2) and 3);
end;

procedure ajax_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $7FFF:
      ; // ROM
    $8000 .. $87FF:
      mem_snd[direccion] := valor;
    $9000:
      ajax_snd_bankswitch(valor);
    $A000 .. $A00D:
      k007232_0.write(direccion and $F, valor);
    $B000 .. $B00D:
      k007232_1.write(direccion and $F, valor);
    $B80C:
      k007232_1.set_volume(0, (valor and $0F) * ($11 shr 1), (valor and $F) * ($11 shr 1));
    $C000:
      ym2151_0.reg(valor);
    $C001:
      ym2151_0.write(valor);
  end;
end;

procedure ajax_sound_update;
begin
  ym2151_0.update;
  k007232_0.update;
  k007232_1.update;
end;

// Main
procedure reset_ajax;
begin
  konami_0.reset;
  hd6309_0.reset;
  z80_0.reset;
  k052109_0.reset;
  k051316_0.reset;
  ym2151_0.reset;
  k051960_0.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  sound_latch := 0;
  rom_bank1 := 0;
  rom_bank2 := 0;
  sub_firq_enable := false;
end;

procedure close_ajax;
begin
  if k007232_1_rom <> nil then
    freemem(k007232_1_rom);
  if k007232_2_rom <> nil then
    freemem(k007232_2_rom);
  if sprite_rom <> nil then
    freemem(sprite_rom);
  if tiles_rom <> nil then
    freemem(tiles_rom);
  if zoom_rom <> nil then
    freemem(zoom_rom);
  k007232_1_rom := nil;
  k007232_2_rom := nil;
  sprite_rom := nil;
  tiles_rom := nil;
  zoom_rom := nil;
end;

function start_ajax: boolean;
var
  temp_mem: array [0 .. $1FFFF] of byte;
  f: byte;
begin
  start_ajax := false;
  machine_calls.close := close_ajax;
  machine_calls.reset := reset_ajax;
  machine_calls.general_loop := ajax_loop;
  machine_calls.fps_max := 59.185606;
  main_screen.rot90_screen := true;
  // Pantallas para el K052109
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  screen_mod_scroll(2, 512, 512, 511, 256, 256, 255);
  screen_init(3, 512, 256, false);
  screen_mod_scroll(3, 512, 512, 511, 256, 256, 255);
  screen_init(4, 512, 512, true); // Para el K051316
  screen_mod_scroll(4, 512, 512, 511, 512, 512, 511);
  screen_init(5, 1024, 1024, false, true);
  start_video(304, 224, true);
  start_audio(true);
  // cargar roms y ponerlas en su sitio...
  if not(roms_load(@temp_mem, ajax_rom)) then
    exit;
  copymemory(@memory[$8000], @temp_mem[$8000], $8000);
  for f := 0 to 3 do
    copymemory(@rom_bank[f, 0], @temp_mem[f * $2000], $2000);
  for f := 0 to 7 do
    copymemory(@rom_bank[4 + f, 0], @temp_mem[$10000 + (f * $2000)], $2000);
  // cargar roms de la sub cpu y ponerlas en su sitio...
  if not(roms_load(@temp_mem, ajax_sub)) then
    exit;
  copymemory(@mem_misc[$A000], @temp_mem[$2000], $6000);
  copymemory(@rom_sub_bank[8, 0], @temp_mem[0], $2000);
  for f := 0 to 7 do
    copymemory(@rom_sub_bank[f, 0], @temp_mem[$8000 + (f * $2000)], $2000);
  // cargar sonido
  if not(roms_load(@mem_snd, ajax_sound)) then
    exit;
  // Main CPU
  konami_0:=cpu_konami.create(12000000,256);
  konami_0.change_ram_calls(ajax_getbyte, ajax_putbyte);
  // Sub CPU
  hd6309_0 := cpu_hd6309.create(3000000, 256, TCPU_HD6309E);
  hd6309_0.change_ram_calls(ajax_sub_getbyte, ajax_sub_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.create(3579545, 256);
  z80_0.change_ram_calls(ajax_snd_getbyte, ajax_snd_putbyte);
  z80_0.init_sound(ajax_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(3579545);
  getmem(k007232_1_rom, $40000);
  if not(roms_load(k007232_1_rom, ajax_k007232_1)) then
    exit;
  k007232_0 := k007232_chip.create(3579545, k007232_1_rom, $40000, 0.20, ajax_k007232_cb_0);
  getmem(k007232_2_rom, $80000);
  if not(roms_load(k007232_2_rom, ajax_k007232_2)) then
    exit;
  k007232_1 := k007232_chip.create(3579545, k007232_2_rom, $80000, 0.50, ajax_k007232_cb_1, true);
  // Iniciar video
  getmem(tiles_rom, $80000);
  if not(roms_load32b_b(tiles_rom, ajax_tiles)) then
    exit;
  k052109_0 := k052109_chip.create(1, 2, 3, 0, ajax_cb, tiles_rom, $80000);
  getmem(sprite_rom, $100000);
  if not(roms_load32b_b(sprite_rom, ajax_sprites)) then
    exit;
  k051960_0 := k051960_chip.create(5, 1, sprite_rom, $100000, ajax_sprite_cb, 2);
  k051960_0.change_irqs(ajax_k051960_cb, nil, nil);
  getmem(zoom_rom, $80000);
  if not(roms_load(zoom_rom, ajax_zoom)) then
    exit;
  k051316_0 := k051316_chip.create(4, 2, ajax_k051316_cb, zoom_rom, $80000, BPP7);
  // DIP
  marcade.dswa := $FF;
marcade.dswa_val2:=@ajax_dip_a;
  marcade.dswb := $5A;
marcade.dswb_val2:=@ajax_dip_b;
  marcade.dswc := $FF;
marcade.dswc_val2:=@ajax_dip_c;
  // final
  reset_ajax;
  start_ajax := true;
end;

end.
