unit rocnrope_hw;

interface

uses
  WinApi.Windows,
  m6809,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  konami_decrypt,
  konami_snd,
  sound_engine;

function start_rocnrope: boolean;

implementation

const
  rocnrope_rom: array [0 .. 4] of tipo_roms = ((n: 'rr1.1h'; l: $2000; p: $6000; crc: $83093134), (n: 'rr2.2h'; l: $2000; p: $8000; crc: $75AF8697), (n: 'rr3.3h'; l: $2000; p: $A000; crc: $B21372B1), (n: 'rr4.4h'; l: $2000; p: $C000; crc: $7ACB2A05), (n: 'rnr_h5.vid'; l: $2000;
    p: $E000; crc: $150A6264));
  rocnrope_snd: array [0 .. 1] of tipo_roms = ((n: 'rnr_7a.snd'; l: $1000; p: 0; crc: $75D2C4E2), (n: 'rnr_8a.snd'; l: $1000; p: $1000; crc: $CA4325AE));
  rocnrope_sprites: array [0 .. 3] of tipo_roms = ((n: 'rnr_a11.vid'; l: $2000; p: 0; crc: $AFDABA5E), (n: 'rnr_a12.vid'; l: $2000; p: $2000; crc: $054CAFEB), (n: 'rnr_a9.vid'; l: $2000; p: $4000; crc: $9D2166B2), (n: 'rnr_a10.vid'; l: $2000; p: $6000; crc: $AFF6E22F));
  rocnrope_chars: array [0 .. 1] of tipo_roms = ((n: 'rnr_h12.vid'; l: $2000; p: 0; crc: $E2114539), (n: 'rnr_h11.vid'; l: $2000; p: $2000; crc: $169A8F3F));
  rocnrope_pal: array [0 .. 2] of tipo_roms = ((n: 'a17_prom.bin'; l: $20; p: 0; crc: $22AD2C3E), (n: 'b16_prom.bin'; l: $100; p: $20; crc: $750A9677), (n: 'rocnrope.pr3'; l: $100; p: $120; crc: $B5C75A27));
  // Dip
  rocnrope_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Invalid')), ());
  rocnrope_dip_b: array [0 .. 4] of def_dip2 = ((mask: 3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '255')), (mask: 4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: $78; name: 'Difficulty'; number: 16;
    val16: ($78, $70, $68, $60, $58, $50, $48, $40, $38, $30, $28, $20, $18, $10, 8, 0); name16: ('1 Easy', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16 Difficult')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0);
    name2: ('Off', 'On')), ());
  rocnrope_dip_c: array [0 .. 3] of def_dip2 = ((mask: 7; name: 'First Bonus'; number: 8; val8: (6, 5, 4, 3, 2, 1, 0, 7); name8: ('20K', '30K', '40K', '50K', '60K', '70K', '80K', 'Invalid')), (mask: $38; name: 'Repeated Bonus'; number: 8;
    val8: ($20, $18, $10, 8, 0, $38, $30, $28); name8: ('40K', '50K', '60K', '70K', '80K', 'Invalid', 'Invalid', 'Invalid')), (mask: $40; name: 'Grant Repeated Bonus'; number: 2; val2: ($40, 0); name2: ('No', 'Yes')), ());

var
  irq_ena: boolean;
  mem_opcodes: array [0 .. $9FFF] of byte;

procedure update_video_rocnrope;
var
  x, y, atrib: byte;
  f: word;
  nchar, color: word;
  flip_x, flip_y: boolean;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      atrib := memory[$4800 + f];
      nchar := memory[$4C00 + f] + ((atrib and $80) shl 1);
      color := (atrib and $F) shl 4;
      put_gfx_flip(x * 8, y * 8, nchar, color + 256, 1, 0, (atrib and $20) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
  for f := $17 downto 0 do
  begin
    atrib := memory[$4000 + (f * 2)];
    nchar := memory[$4401 + (f * 2)];
    color := (atrib and $F) shl 4;
    if not(main_screen.flip_main_screen) then
    begin
      x := memory[$4001 + (f * 2)];
      y := memory[$4400 + (f * 2)];
      flip_x := (atrib and $80) = 0;
      flip_y := (atrib and $40) <> 0;
    end
    else
    begin
      x := 241 - memory[$4001 + (f * 2)];
      y := 240 - memory[$4400 + (f * 2)];
      flip_x := (atrib and $80) <> 0;
      flip_y := (atrib and $40) = 0;
    end;
    put_gfx_sprite_mask(nchar, color, flip_x, flip_y, 1, 0, $F);
    update_gfx_sprite(x, y, 2, 1);
  end;
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_rocnrope;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or 4);
    if p_contrls.map_arcade.down[0] then
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
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure rocnrope_loop;
var
  f: byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        if f = 240 then
        begin
          update_video_rocnrope;
          if irq_ena then
            m6809_0.change_irq(ASSERT_LINE);
        end;
        // main
        m6809_0.run(frame_main);
        frame_main := frame_main + m6809_0.tframes - m6809_0.contador;
        // snd
        konamisnd_0.run;
      end;
      events_rocnrope;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function rocnrope_getbyte(direccion: word): byte;
begin
  case direccion of
    $3000:
      rocnrope_getbyte := marcade.dswb;
    $3080:
      rocnrope_getbyte := marcade.in2;
    $3081:
      rocnrope_getbyte := marcade.in0;
    $3082:
      rocnrope_getbyte := marcade.in1;
    $3083:
      rocnrope_getbyte := marcade.dswa;
    $3100:
      rocnrope_getbyte := marcade.dswc;
    $4000 .. $5FFF:
      rocnrope_getbyte := memory[direccion];
    $6000 .. $FFFF:
      if m6809_0.opcode then
        rocnrope_getbyte := mem_opcodes[direccion - $6000]
      else
        rocnrope_getbyte := memory[direccion];
  end;
end;

procedure rocnrope_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $4000 .. $47FF, $5000 .. $5FFF:
      memory[direccion] := valor;
    $4800 .. $4FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $6000 .. $807F, $8190 .. $FFFF:
      ; // ROM
    $8080 .. $8087:
      begin
        valor := valor and 1;
        case (direccion and 7) of
          0:
            main_screen.flip_main_screen := (valor = 0);
          1:
            if valor <> 0 then
              konamisnd_0.pedir_irq := HOLD_LINE;
          2:
            konamisnd_0.enabled := (valor = 0);
          7:
            begin
              irq_ena := (valor <> 0);
              if not(irq_ena) then
                m6809_0.change_irq(CLEAR_LINE);
            end;
        end;
      end;
    $8100:
      konamisnd_0.sound_latch := valor;
    $8182 .. $818D:
      memory[$FFF2 + (direccion - $8182)] := valor;
  end;
end;

// Main
procedure reset_rocnrope;
begin
  m6809_0.reset;
  frame_main := m6809_0.tframes;
  konamisnd_0.reset;
 reset_video;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_ena := false;
end;

function start_rocnrope: boolean;
var
  colores: tpaleta;
  bit0, bit1, bit2: byte;
  f: word;
  memory_temp: array [0 .. $FFFF] of byte;
  rweights, gweights, bweights: array [0 .. 3] of single;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
begin
  machine_calls.general_loop := rocnrope_loop;
  machine_calls.reset := reset_rocnrope;
  start_rocnrope := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(18432000 div 3 div 4, $100, TCPU_M6809);
  m6809_0.change_ram_calls(rocnrope_getbyte, rocnrope_putbyte);
  if not(roms_load(@memory, rocnrope_rom)) then
    exit;
  konami1_decode(@memory[$6000], @mem_opcodes[0], $A000);
  mem_opcodes[$703D - $6000] := $98; // Patch
  // Sound Chip
  konamisnd_0 := konamisnd_chip.Create(4, TIPO_TIMEPLT, 1789772, $100);
  if not(roms_load(@konamisnd_0.memory, rocnrope_snd)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, rocnrope_chars)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx_set_desc_data(4, 0, 16 * 8, $2000 * 8 + 4, $2000 * 8 + 0, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // sprites
  if not(roms_load(@memory_temp, rocnrope_sprites)) then
    exit;
  init_gfx(1, 16, 16, 256);
  gfx_set_desc_data(4, 0, 64 * 8, $4000 * 8 + 4, $4000 * 8 + 0, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
  // paleta
  if not(roms_load(@memory_temp, rocnrope_pal)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rweights, 1000, 0, 3, @resistances_rg, @gweights, 1000, 0, 2, @resistances_b, @bweights, 1000, 0);
  for f := 0 to $1F do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and 1;
    bit1 := (memory_temp[f] shr 1) and 1;
    bit2 := (memory_temp[f] shr 2) and 1;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    // green component
    bit0 := (memory_temp[f] shr 3) and 1;
    bit1 := (memory_temp[f] shr 4) and 1;
    bit2 := (memory_temp[f] shr 5) and 1;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // blue component
    bit0 := (memory_temp[f] shr 6) and 1;
    bit1 := (memory_temp[f] shr 7) and 1;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $1FF do
  begin
    gfx[0].colores[f] := memory_temp[$20 + f] and $F; // chars
    gfx[1].colores[f] := memory_temp[$20 + f] and $F; // sprites
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $5B;
  marcade.dswc := $96;
  marcade.dswa_val2 := @rocnrope_dip_a;
  marcade.dswb_val2 := @rocnrope_dip_b;
  marcade.dswc_val2 := @rocnrope_dip_c;
  // final
  reset_rocnrope;
  start_rocnrope := true;
end;

end.
