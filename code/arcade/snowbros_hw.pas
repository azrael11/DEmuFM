unit snowbros_hw;

interface

uses
  WinApi.Windows,
  nz80,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  ym_3812,
  rom_engine,
  pal_engine,
  kaneco_pandora,
  sound_engine,
  misc_functions,
  mcs51,
  ym_2151,
  oki6295;

function start_snowbros: boolean;

implementation

const
  snowbros_rom: array [0 .. 1] of tipo_roms = ((n: 'sn6.bin'; l: $20000; p: 0; crc: $4899DDCF), (n: 'sn5.bin'; l: $20000; p: $1; crc: $AD310D3F));
  snowbros_char: tipo_roms = (n: 'sbros-1.41'; l: $80000; p: 0; crc: $16F06B3A);
  snowbros_sound: tipo_roms = (n: 'sbros-4.29'; l: $8000; p: 0; crc: $E6EAB4E4);
  snowbros_dip_a: array [0 .. 6] of def_dip = ((mask: $1; name: 'Region'; number: 2; dip: ((dip_val: $0; dip_name: 'Europe'), (dip_val: $1; dip_name: 'America (Romstar license)'), (), (), (), (), (),
    (), (), (), (), (), (), (), (), ())), (mask: $2; name: 'Flip Screen'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $4; name: 'Service Mode'; number: 2; dip: ((dip_val: $4; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8;
    name: 'Demo Sounds'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $8; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Coin A';
    number: 7; dip: ((dip_val: $0; dip_name: '4C 1C EUR'), (dip_val: $10; dip_name: '3C 1C EUR'), (dip_val: $20; dip_name: '2C 1C EUR'), (dip_val: $10; dip_name: '2C 1C AME'), (dip_val: $30;
    dip_name: '1C 1C'), (dip_val: $0; dip_name: '2C 3C AME'), (dip_val: $20; dip_name: '2C 1C AME'), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Coin B'; number: 8;
    dip: ((dip_val: $40; dip_name: '2C 1C AME'), (dip_val: $C0; dip_name: '1C 1C AME'), (dip_val: $0; dip_name: '2C 3C AME'), (dip_val: $80; dip_name: '1C 2C AME'), (dip_val: $C0;
    dip_name: '1C 2C EUR'), (dip_val: $80; dip_name: '1C 3C EUR'), (dip_val: $40; dip_name: '1C 4C EUR'), (dip_val: $0; dip_name: '1C 6C EUR'), (), (), (), (), (), (), (), ())), ());
  snowbros_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4; dip: ((dip_val: $2; dip_name: 'Easy'), (dip_val: $3; dip_name: 'Normal'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $4; dip_name: '100k 200k+'), (dip_val: $C; dip_name: '100k'), (dip_val: $8; dip_name: '200k'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $30; name: 'Lives'; number: 4; dip: ((dip_val: $20; dip_name: '1'), (dip_val: $0; dip_name: '2'), (dip_val: $30; dip_name: '3'), (dip_val: $10; dip_name: '4'), (), (), (), (), (), (), (),
    (), (), (), (), ())), (mask: $40; name: 'Invulnerability'; number: 2; dip: ((dip_val: $40; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
    ), (mask: $80; name: 'Allow Continue'; number: 2; dip: ((dip_val: $0; dip_name: 'No'), (dip_val: $80; dip_name: 'Yes'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  toto_rom: array [0 .. 1] of tipo_roms = ((n: 'u60.5j'; l: $20000; p: 0; crc: $39203792), (n: 'u51.4j'; l: $20000; p: $1; crc: $7B846CD4));
  toto_char: array [0 .. 3] of tipo_roms = ((n: 'u107.8k'; l: $20000; p: 0; crc: $4486153B), (n: 'u108.8l'; l: $20000; p: $20000; crc: $3286CF5F), (n: 'u109.8m'; l: $20000; p: $40000; crc: $464D7251),
    (n: 'u110.8n'; l: $20000; p: $60000; crc: $7DEA56DF));
  toto_sound: tipo_roms = (n: 'u46.4c'; l: $8000; p: 0; crc: $77B1EF42);
  hyperpac_rom: array [0 .. 1] of tipo_roms = ((n: 'hyperpac.h12'; l: $20000; p: 1; crc: $2CF0531A), (n: 'hyperpac.i12'; l: $20000; p: $0; crc: $9C7D85B8));
  hyperpac_char: array [0 .. 2] of tipo_roms = ((n: 'hyperpac.a4'; l: $40000; p: 0; crc: $BD8673DA), (n: 'hyperpac.a5'; l: $40000; p: $40000; crc: $5D90CD82), (n: 'hyperpac.a6'; l: $40000; p: $80000;
    crc: $61D86E63));
  hyperpac_sound: tipo_roms = (n: 'hyperpac.u1'; l: $10000; p: 0; crc: $03FAF88E);
  hyperpac_mcu: tipo_roms = (n: 'at89c52.bin'; l: $2000; p: 0; crc: $291F9326);
  hyperpac_oki: tipo_roms = (n: 'hyperpac.j15'; l: $40000; p: 0; crc: $FB9F468D);

var
  rom: array [0 .. $1FFFF] of word;
  ram: array [0 .. $7FFF] of word;
  sound_latch: byte;
  // Hyper Pacman
  semicom_prot_base: word;
  semicom_prot_offset: byte;

procedure update_video_snowbros;
begin
  pandora_0.update_video(1, 0);
  update_final_piece(0, 16, 256, 224, 1);
end;

procedure events_snowbros;
begin
  if event.arcade then
  begin
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FEFF)
    else
      marcade.in1 := (marcade.in1 or $0100);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FDFF)
    else
      marcade.in1 := (marcade.in1 or $0200);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FBFF)
    else
      marcade.in1 := (marcade.in1 or $0400);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $F7FF)
    else
      marcade.in1 := (marcade.in1 or $0800);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EFFF)
    else
      marcade.in1 := (marcade.in1 or $1000);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DFFF)
    else
      marcade.in1 := (marcade.in1 or $2000);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 and $BFFF)
    else
      marcade.in1 := (marcade.in1 or $4000);
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FBFF)
    else
      marcade.in0 := (marcade.in0 or $0400);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $F7FF)
    else
      marcade.in0 := (marcade.in0 or $0800);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FEFF)
    else
      marcade.in0 := (marcade.in0 or $0100);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $FDFF)
    else
      marcade.in0 := (marcade.in0 or $0200);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FEFF)
    else
      marcade.in2 := (marcade.in2 or $0100);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FDFF)
    else
      marcade.in2 := (marcade.in2 or $0200);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FBFF)
    else
      marcade.in2 := (marcade.in2 or $400);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $F7FF)
    else
      marcade.in2 := (marcade.in2 or $0800);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EFFF)
    else
      marcade.in2 := (marcade.in2 or $1000);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DFFF)
    else
      marcade.in2 := (marcade.in2 or $2000);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 and $BFFF)
    else
      marcade.in2 := (marcade.in2 or $4000);
  end;
end;

procedure snowbros_loop;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to 261 do
      begin
        // Main CPU
        m68000_0.run(frame_m);
        frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
        // Sound CPU
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        case f of
          31:
            m68000_0.irq[4] := ASSERT_LINE;
          127:
            m68000_0.irq[3] := ASSERT_LINE;
          239:
            begin
              m68000_0.irq[2] := ASSERT_LINE;
              update_video_snowbros;
            end;
        end;
      end;
      events_snowbros;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function snowbros_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $FFFFF:
      snowbros_getword := rom[direccion shr 1];
    $100000 .. $10FFFF:
      snowbros_getword := ram[(direccion and $FFFF) shr 1];
    $300000:
      snowbros_getword := sound_latch;
    $500000:
      snowbros_getword := marcade.in1 + marcade.dswa;
    $500002:
      snowbros_getword := marcade.in2 + marcade.dswb;
    $500004:
      snowbros_getword := marcade.in0;
    $500006:
      snowbros_getword := $700; // Proteccion Toto
    $600000 .. $6001FF:
      snowbros_getword := buffer_paleta[(direccion and $1FF) shr 1];
    $700000 .. $701FFF:
      snowbros_getword := pandora_0.spriteram_r16(direccion and $1FFF);
  end;
end;

procedure change_color(tmp_color, numero: word); inline;
var
  color: tcolor;
begin
  color.b := pal5bit(tmp_color shr 10);
  color.g := pal5bit(tmp_color shr 5);
  color.r := pal5bit(tmp_color);
  set_pal_color(color, numero);
end;

procedure snowbros_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $10FFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
    $400000:
      main_screen.flip_main_screen := (valor and $8000) = 0;
    $300000:
      begin
        sound_latch := valor and $FF;
        z80_0.change_nmi(PULSE_LINE);
      end;
    $600000 .. $6001FF:
      if buffer_paleta[(direccion and $1FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $1FF) shr 1] := valor;
        change_color(valor, (direccion and $1FF) shr 1);
      end;
    $700000 .. $701FFF:
      pandora_0.spriteram_w16((direccion and $1FFF), valor and $FF);
    $800000:
      m68000_0.irq[4] := CLEAR_LINE;
    $900000:
      m68000_0.irq[3] := CLEAR_LINE;
    $A00000:
      m68000_0.irq[2] := CLEAR_LINE;
  end;
end;

function snowbros_snd_getbyte(direccion: word): byte;
begin
  snowbros_snd_getbyte := mem_snd[direccion];
end;

procedure snowbros_snd_putbyte(direccion: word; valor: byte);
begin
  if direccion > $7FFF then
    mem_snd[direccion] := valor;
end;

function snowbros_snd_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $2:
      snowbros_snd_inbyte := ym3812_0.status;
    $4:
      snowbros_snd_inbyte := sound_latch;
  end;
end;

procedure snowbros_snd_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $2:
      ym3812_0.control(valor);
    $3:
      ym3812_0.write(valor);
    $4:
      sound_latch := valor;
  end;
end;

procedure snowbros_sound_act;
begin
  ym3812_0.update;
end;

procedure snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

// Hyper Pacman
procedure eventos_hyperpac;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 or $0100)
    else
      marcade.in1 := (marcade.in1 and $FEFF);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 or $0200)
    else
      marcade.in1 := (marcade.in1 and $FDFF);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 or $0400)
    else
      marcade.in1 := (marcade.in1 and $FBFF);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 or $0800)
    else
      marcade.in1 := (marcade.in1 and $F7FF);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 or $1000)
    else
      marcade.in1 := (marcade.in1 and $EFFF);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 or $2000)
    else
      marcade.in1 := (marcade.in1 and $DFFF);
    if p_contrls.map_arcade.but2[0] then
      marcade.in1 := (marcade.in1 or $4000)
    else
      marcade.in1 := (marcade.in1 and $BFFF);
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 or $0400)
    else
      marcade.in0 := (marcade.in0 and $FBFF);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 or $0800)
    else
      marcade.in0 := (marcade.in0 and $F7FF);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 or $0100)
    else
      marcade.in0 := (marcade.in0 and $FEFF);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 or $0200)
    else
      marcade.in0 := (marcade.in0 and $FDFF);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 or $0100)
    else
      marcade.in2 := (marcade.in2 and $FEFF);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 or $0200)
    else
      marcade.in2 := (marcade.in2 and $FDFF);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 or $400)
    else
      marcade.in2 := (marcade.in2 and $FBFF);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 or $0800)
    else
      marcade.in2 := (marcade.in2 and $F7FF);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 or $1000)
    else
      marcade.in2 := (marcade.in2 and $EFFF);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 or $2000)
    else
      marcade.in2 := (marcade.in2 and $DFFF);
    if p_contrls.map_arcade.but2[1] then
      marcade.in2 := (marcade.in2 or $4000)
    else
      marcade.in2 := (marcade.in2 and $BFFF);
  end;
end;

procedure hyperpac_loop;
var
  frame_m, frame_s, frame_mcu: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  frame_mcu := mcs51_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 261 do
    begin
      // Main CPU
      m68000_0.run(frame_m);
      frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
      // Sound CPU
      z80_0.run(frame_s);
      frame_s := frame_s + z80_0.tframes - z80_0.contador;
      // MCU
      mcs51_0.run(frame_mcu);
      frame_mcu := frame_mcu + mcs51_0.tframes - mcs51_0.contador;
      case f of
        31:
          m68000_0.irq[4] := ASSERT_LINE;
        127:
          m68000_0.irq[3] := ASSERT_LINE;
        239:
          begin
            m68000_0.irq[2] := ASSERT_LINE;
            update_video_snowbros;
          end;
      end;
    end;
    eventos_hyperpac;
    video_sync;
  end;
end;

procedure hyperpac_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $FFFFF:
      ; // ROM
    $100000 .. $10FFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
    $300000:
      sound_latch := valor and $FF;
    $600000 .. $6001FF:
      if buffer_paleta[(direccion and $1FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $1FF) shr 1] := valor;
        change_color(valor, (direccion and $1FF) shr 1);
      end;
    $700000 .. $701FFF:
      pandora_0.spriteram_w16((direccion and $1FFF), valor and $FF);
    $800000:
      m68000_0.irq[4] := CLEAR_LINE;
    $900000:
      m68000_0.irq[3] := CLEAR_LINE;
    $A00000:
      m68000_0.irq[2] := CLEAR_LINE;
  end;
end;

function hyperpac_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $D7FF:
      hyperpac_snd_getbyte := mem_snd[direccion];
    $F001:
      hyperpac_snd_getbyte := ym2151_0.status;
    $F002:
      hyperpac_snd_getbyte := oki_6295_0.read;
    $F008:
      hyperpac_snd_getbyte := sound_latch;
  end;
end;

procedure hyperpac_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $CFFF:
      ;
    $D000 .. $D7FF:
      mem_snd[direccion] := valor;
    $F000:
      ym2151_0.reg(valor);
    $F001:
      ym2151_0.write(valor);
    $F002:
      oki_6295_0.write(valor);
  end;
end;

procedure hyperpac_sound_act;
begin
  ym2151_0.update;
  oki_6295_0.update;
end;

procedure ym2151_snd_irq(irqstate: byte);
begin
  z80_0.change_irq(irqstate);
end;

procedure hyperpac_out_port0(valor: byte);
var
  tempw: word;
begin
  tempw := ram[semicom_prot_base + semicom_prot_offset];
  tempw := (tempw and $FF00) or (valor shl 0);
  ram[semicom_prot_base + semicom_prot_offset] := tempw;
end;

procedure hyperpac_out_port1(valor: byte);
var
  tempw: word;
begin
  tempw := ram[semicom_prot_base + semicom_prot_offset];
  tempw := (tempw and $FF) or (valor shl 8);
  ram[semicom_prot_base + semicom_prot_offset] := tempw;
end;

procedure hyperpac_out_port2(valor: byte);
begin
  semicom_prot_offset := valor;
end;

// Main
procedure reset_snowbros;
begin
  m68000_0.reset;
  z80_0.reset;
  pandora_0.reset;
  if main_vars.machine_type = 387 then
  begin
    ym2151_0.reset;
    oki_6295_0.reset;
    marcade.in0 := 0;
    marcade.in1 := 0;
    marcade.in2 := 0;
  end
  else
  begin
    ym3812_0.reset;
    marcade.in0 := $FF00;
    marcade.in1 := $7F00;
    marcade.in2 := $7F00;
  end;
 reset_game_general;
  sound_latch := 0;
end;

function start_snowbros: boolean;
const
  pc_x: array [0 .. 15] of dword = (0, 4, 8, 12, 16, 20, 24, 28, 8 * 32 + 0, 8 * 32 + 4, 8 * 32 + 8, 8 * 32 + 12, 8 * 32 + 16, 8 * 32 + 20, 8 * 32 + 24, 8 * 32 + 28);
  pc_y: array [0 .. 15] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32, 16 * 32, 17 * 32, 18 * 32, 19 * 32, 20 * 32, 21 * 32, 22 * 32, 23 * 32);
  pc_x_hp: array [0 .. 15] of dword = (4, 0, 8 * 32 + 4, 8 * 32 + 0, 20, 16, 8 * 32 + 20, 8 * 32 + 16, 12, 8, 8 * 32 + 12, 8 * 32 + 8, 28, 24, 8 * 32 + 28, 8 * 32 + 24);
  pc_y_hp: array [0 .. 15] of dword = (0 * 32, 2 * 32, 1 * 32, 3 * 32, 16 * 32 + 0 * 32, 16 * 32 + 2 * 32, 16 * 32 + 1 * 32, 16 * 32 + 3 * 32, 4 * 32, 6 * 32, 5 * 32, 7 * 32, 16 * 32 + 4 * 32,
    16 * 32 + 6 * 32, 16 * 32 + 5 * 32, 16 * 32 + 7 * 32);
var
  memory_temp: array [0 .. $BFFFF] of byte;
  ptemp: pbyte;
  f: dword;
procedure convert_chars(num:word;tipo:byte);
  begin
  init_gfx(0,16,16,num);
    gfx[0].trans[0] := true;
    gfx_set_desc_data(4, 0, 32 * 32, 0, 1, 2, 3);
    if tipo = 0 then
      convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false)
    else
      convert_gfx(0, 0, @memory_temp, @pc_x_hp, @pc_y_hp, false, false);
  end;

begin
  machine_calls.reset := reset_snowbros;
  machine_calls.fps_max := 57.5;
  start_snowbros := false;
  start_audio(false);
  screen_init(1, 512, 512, true, true);
  start_video(256, 224);
  case main_vars.machine_type of
    54, 386:
      begin
        machine_calls.general_loop := snowbros_loop;
        // Main CPU
        m68000_0 := cpu_m68000.create(8000000, 262);
        m68000_0.change_ram16_calls(snowbros_getword, snowbros_putword);
        // Sound CPU
        z80_0 := cpu_z80.create(6000000, 262);
        z80_0.change_ram_calls(snowbros_snd_getbyte, snowbros_snd_putbyte);
        z80_0.change_io_calls(snowbros_snd_inbyte, snowbros_snd_outbyte);
        z80_0.init_sound(snowbros_sound_act);
        // Sound Chips
        ym3812_0 := ym3812_chip.create(YM3812_FM, 3000000);
        ym3812_0.change_irq_calls(snd_irq);
      end;
    387:
      begin
        machine_calls.general_loop := hyperpac_loop;
        // Main CPU
        m68000_0 := cpu_m68000.create(12000000, 262);
        m68000_0.change_ram16_calls(snowbros_getword, hyperpac_putword);
        // Sound CPU
        z80_0 := cpu_z80.create(4000000, 262);
        z80_0.change_ram_calls(hyperpac_snd_getbyte, hyperpac_snd_putbyte);
        z80_0.init_sound(hyperpac_sound_act);
        // Sound Chips
        ym2151_0 := ym2151_chip.create(16000000 div 4, 1);
        ym2151_0.change_irq_func(ym2151_snd_irq);
        oki_6295_0 := snd_okim6295.create(16000000 div 16, OKIM6295_PIN7_HIGH, 1);
      end;
  end;
pandora_0:=pandora_gfx.create(0,true);
  case main_vars.machine_type of
    54:
      begin // Snowbros
        // pandora
        // cargar roms
        if not(roms_load16w(@rom, snowbros_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, snowbros_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, snowbros_char)) then
          exit;
        convert_chars($1000,0);
        // DIP
        marcade.dswa := $FE;
        marcade.dswb := $FF;
        marcade.dswa_val := @snowbros_dip_a;
        marcade.dswb_val := @snowbros_dip_b;
      end;
    386:
      begin // Come Back Toto
        // cargar roms
        if not(roms_load16w(@rom, toto_rom)) then
          exit;
        ptemp := @rom;
        for f := 0 to $3FFFF do
          ptemp[f] := bitswap8(ptemp[f], 7, 6, 5, 3, 4, 2, 1, 0);
        // cargar sonido
        if not(roms_load(@mem_snd, toto_sound)) then
          exit;
        ptemp := @mem_snd;
        for f := 0 to $7FFF do
          ptemp[f] := bitswap8(ptemp[f], 7, 6, 5, 3, 4, 2, 1, 0);
        // convertir chars
        if not(roms_load(@memory_temp, toto_char)) then
          exit;
        for f := 0 to $7FFFF do
          memory_temp[f] := bitswap8(memory_temp[f], 7, 6, 5, 3, 4, 2, 1, 0);
        convert_chars($1000,0);
        // DIP
        marcade.dswa := $FE;
        marcade.dswb := $FF;
        marcade.dswa_val := @snowbros_dip_a;
        marcade.dswb_val := @snowbros_dip_b;
      end;
    387:
      begin // Hyper Pacman
        // MCU
        mcs51_0 := cpu_mcs51.create(I8XC52, 16000000, 262);
        mcs51_0.change_io_calls(nil, nil, nil, nil, hyperpac_out_port0, hyperpac_out_port1, hyperpac_out_port2, nil);
        if not(roms_load(mcs51_0.get_rom_addr, hyperpac_mcu)) then
          exit;
        // cargar roms
        if not(roms_load16w(@rom, hyperpac_rom)) then
          exit;
        // cargar sonido
        if not(roms_load(@mem_snd, hyperpac_sound)) then
          exit;
        if not(roms_load(oki_6295_0.get_rom_addr, hyperpac_oki)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, hyperpac_char)) then
          exit;
        convert_chars($1800,1);
        // DIP
        marcade.dswa := $FE;
        marcade.dswb := $FF;
        marcade.dswa_val := @snowbros_dip_a;
        marcade.dswb_val := @snowbros_dip_b;
        semicom_prot_base := $E000 shr 1;
      end;
  end;
  // final
  reset_snowbros;
  start_snowbros := true;
end;

end.
