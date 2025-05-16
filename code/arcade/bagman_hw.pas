unit bagman_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  ay_8910,
  bagman_pal;

function start_bagman: boolean;

implementation

const
  // bagman
  bagman_rom: array [0 .. 5] of tipo_roms = ((n: 'e9_b05.bin'; l: $1000; p: 0; crc: $E0156191), (n: 'f9_b06.bin'; l: $1000; p: $1000; crc: $7B758982), (n: 'f9_b07.bin'; l: $1000; p: $2000;
    crc: $302A077B), (n: 'k9_b08.bin'; l: $1000; p: $3000; crc: $F04293CB), (n: 'm9_b09s.bin'; l: $1000; p: $4000; crc: $68E83E4F), (n: 'n9_b10.bin'; l: $1000; p: $5000; crc: $1D6579F7));
  bagman_pal: array [0 .. 1] of tipo_roms = ((n: 'p3.bin'; l: $20; p: 0; crc: $2A855523), (n: 'r3.bin'; l: $20; p: $20; crc: $AE6F1019));
  bagman_char: array [0 .. 1] of tipo_roms = ((n: 'e1_b02.bin'; l: $1000; p: 0; crc: $4A0A6B55), (n: 'j1_b04.bin'; l: $1000; p: $1000; crc: $C680EF04));
  bagman_sprites: array [0 .. 1] of tipo_roms = ((n: 'c1_b01.bin'; l: $1000; p: 0; crc: $705193B2), (n: 'f1_b03s.bin'; l: $1000; p: $1000; crc: $DBA1EDA7));
  // Super Bagman
  sbagman_rom: array [0 .. 9] of tipo_roms = ((n: '5.9e'; l: $1000; p: 0; crc: $1B1D6B0A), (n: '6.9f'; l: $1000; p: $1000; crc: $AC49CB82), (n: '7.9j'; l: $1000; p: $2000; crc: $9A1C778D), (n: '8.9k';
    l: $1000; p: $3000; crc: $B94FBB73), (n: '9.9m'; l: $1000; p: $4000; crc: $601F34BA), (n: '10.9n'; l: $1000; p: $5000; crc: $5F750918), (n: '13.8d'; l: $1000; p: $6000; crc: $944A4453),
    (n: '14.8f'; l: $1000; p: $7000; crc: $83B10139), (n: '15.8j'; l: $1000; p: $8000; crc: $FE924879), (n: '16.8k'; l: $1000; p: $9000; crc: $B77EB1F5));
  sbagman_pal: array [0 .. 1] of tipo_roms = ((n: 'p3.bin'; l: $20; p: 0; crc: $2A855523), (n: 'r3.bin'; l: $20; p: $20; crc: $AE6F1019));
  sbagman_char: array [0 .. 1] of tipo_roms = ((n: '2.1e'; l: $1000; p: 0; crc: $F4D3D4E6), (n: '4.1j'; l: $1000; p: $1000; crc: $2C6A510D));
  sbagman_sprites: array [0 .. 1] of tipo_roms = ((n: '1.1c'; l: $1000; p: 0; crc: $A046FF44), (n: '3.1f'; l: $1000; p: $1000; crc: $A4422DA4));
        //DIP
        bagman_dip:array [0..6] of def_dip2=(
        (mask:$3;name:'Lives';number:4;val4:(3,2,1,0);name4:('2','3','4','5')),
        (mask:$4;name:'Coinage';number:2;val2:(0,4);name2:('2C/1C 1C/1C 1C/3C 1C/7C','1C/1C 1C/2C 1C/6C 1C/14C')),
        (mask:$18;name:'Difficulty';number:4;val4:($18,$10,8,0);name4:('Easy','Medium','Hard','Hardest')),
        (mask:$20;name:'Language';number:2;val2:($20,0);name2:('English','French')),
        (mask:$40;name:'Bonus Life';number:2;val2:($40,0);name2:('30K','40K')),
        (mask:$80;name:'Cabinet';number:2;val2:($80,0);name2:('Upright','Cocktail')),());
var
  irq_enable, video_enable: boolean;

procedure update_video_bagman;
var
  f, color, nchar: word;
  atrib, gfx_num, x, y: byte;
begin
  if video_enable then
  begin
    for f := 0 to $3FF do
    begin
      if gfx[0].buffer[f] then
      begin
        x := 31 - (f div 32);
        y := f mod 32;
        atrib := memory[$9800 + f];
        nchar := memory[$9000 + f] + ((atrib and $20) shl 3);
        gfx_num := (atrib and $10) shr 3;
        color := (atrib and $F) shl 2;
        put_gfx(x * 8, y * 8, nchar, color, 1, gfx_num);
        gfx[0].buffer[f] := false;
      end;
    end;
    update_region(0, 0, 256, 256, 1, 0, 0, 256, 256, 2);
    for f := 7 downto 0 do
    begin
      atrib := memory[$9800 + (f * 4)];
      color := (memory[$9801 + (f * 4)] and $1F) shl 2;
      nchar := (atrib and $3F) + ((memory[$9801 + (f * 4)] and $20) shl 1);
      y := memory[$9803 + (f * 4)];
      x := memory[$9802 + (f * 4)];
      if ((x <> 0) and (y <> 0)) then
      begin
        put_gfx_sprite(nchar, color, (atrib and $80) <> 0, (atrib and $40) <> 0, 1);
        update_gfx_sprite(x + 1, y - 1, 2, 1);
      end;
    end;
  end
  else
    fill_full_screen(2, $3FF);
  update_final_piece(16, 0, 224, 256, 2);
end;

procedure events_bagman;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.start[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $BF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $7F)
    else
      marcade.in1 := (marcade.in1 or $80);
  end;
end;

procedure bagman_loop;
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
          if irq_enable then
            z80_0.change_irq(HOLD_LINE);
          update_video_bagman;
        end;
      end;
      events_bagman;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function bagman_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $67FF, $9000 .. $93FF, $9800 .. $9BFF, $C000 .. $FFFF:
      bagman_getbyte := memory[direccion];
    $A000:
      bagman_getbyte := bagman_pal16r6_r;
    $B000:
      bagman_getbyte := marcade.dswa;
  end;
end;

procedure bagman_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF, $C000 .. $FFFF:
      ; // ROM
    $6000 .. $67FF:
      memory[direccion] := valor;
    $9000 .. $93FF, $9800 .. $9BFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $A000:
      irq_enable := (valor and 1) <> 0;
    $A001 .. $A002:
      main_screen.flip_main_screen := (valor and 1) <> 1;
    $A003:
      if video_enable <> ((valor and 1) <> 0) then
      begin
        video_enable := (valor and 1) <> 0;
        if video_enable then
          fillchar(gfx[0].buffer, $400, 1);
      end;
    $A800 .. $A805:
      bagman_pal16r6_w(direccion and $7, valor);
  end;
end;

function bagman_inbyte(puerto: word): byte;
begin
  if (puerto and $FF) = $C then
    bagman_inbyte := ay8910_0.Read;
end;

procedure bagman_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $08:
      ay8910_0.Control(valor);
    $09:
      ay8910_0.Write(valor);
  end;
end;

function bagman_portar: byte;
begin
  bagman_portar := marcade.in0;
end;

function bagman_portbr: byte;
begin
  bagman_portbr := marcade.in1;
end;

procedure bagman_sound;
begin
  ay8910_0.update;
end;

// Main
procedure reset_bagman;
begin
  z80_0.reset;
  ay8910_0.reset;
 reset_game_general;
  irq_enable := true;
  video_enable := true;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  // Reset PAL
  bagman_pal16r6_w(0, 1); // pin 2
  bagman_pal16r6_w(1, 1); // pin 3
  bagman_pal16r6_w(2, 1); // pin 4
  bagman_pal16r6_w(3, 1); // pin 5
  bagman_pal16r6_w(4, 1); // pin 6
  bagman_pal16r6_w(5, 1); // pin 7
  bagman_pal16r6_w(6, 1); // pin 8
  bagman_pal16r6_w(7, 1); // pin 9
  bagman_update_pal;
end;

function start_bagman: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1, bit2: byte;
  memory_temp: array [0 .. $9FFF] of byte;
  rweights, gweights: array [0 .. 3] of single;
  bweights: array [0 .. 2] of single;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 4, 5, 6, 7, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 8 * 8 + 4, 8 * 8 + 5, 8 * 8 + 6, 8 * 8 + 7);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 16 * 8, 17 * 8, 18 * 8, 19 * 8, 20 * 8, 21 * 8, 22 * 8, 23 * 8);
  resistances_rg: array [0 .. 2] of integer = (1000, 470, 220);
  resistances_b: array [0 .. 1] of integer = (470, 220);
  procedure conv_chars(num_gfx: byte);
  begin
    init_gfx(num_gfx, 8, 8, $200);
    gfx_set_desc_data(2, 0, 8 * 8, 0, 512 * 8 * 8);
    convert_gfx(num_gfx, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;
  procedure conv_sprites;
  begin
    init_gfx(1, 16, 16, $80);
    gfx[1].trans[0] := true;
    gfx_set_desc_data(2, 0, 32 * 8, 0, 128 * 16 * 16);
    convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  end;

begin
  machine_calls.general_loop := bagman_loop;
  machine_calls.reset := reset_bagman;
  machine_calls.fps_max := 60.6060606060;
  start_bagman := false;
  start_audio(false);
  screen_init(1, 256, 256);
  screen_init(2, 256, 512, false, true);
  start_video(224, 256);
  // Main CPU
  z80_0 := cpu_z80.create(3072000, 264);
  z80_0.change_ram_calls(bagman_getbyte, bagman_putbyte);
  z80_0.change_io_calls(bagman_inbyte, bagman_outbyte);
  z80_0.init_sound(bagman_sound);
ay8910_0:=ay8910_chip.create(1536000,AY8910);
  ay8910_0.change_io_calls(bagman_portar, bagman_portbr, nil, nil);
  case main_vars.machine_type of
    171:
      begin // bagman
        // cargar roms
        if not(roms_load(@memory, bagman_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, bagman_char)) then
          exit;
        conv_chars(0);
        conv_sprites;
        // convertir sprites
        if not(roms_load(@memory_temp, bagman_sprites)) then
          exit;
        conv_chars(2);
        // poner la paleta
        if not(roms_load(@memory_temp, bagman_pal)) then
          exit;
      end;
    172:
      begin // Super Bagman
        // cargar roms
        if not(roms_load(@memory_temp, sbagman_rom)) then
          exit;
        copymemory(@memory[0], @memory_temp, $6000);
        copymemory(@memory[$C000], @memory_temp[$6000], $E00);
        copymemory(@memory[$FE00], @memory_temp[$6E00], $200);
        copymemory(@memory[$D000], @memory_temp[$7000], $400);
        copymemory(@memory[$E400], @memory_temp[$7400], $200);
        copymemory(@memory[$D600], @memory_temp[$7600], $A00);
        copymemory(@memory[$E000], @memory_temp[$8000], $400);
        copymemory(@memory[$D400], @memory_temp[$8400], $200);
        copymemory(@memory[$E600], @memory_temp[$8600], $A00);
        copymemory(@memory[$F000], @memory_temp[$9000], $E00);
        copymemory(@memory[$CE00], @memory_temp[$9E00], $200);
        // convertir chars
        if not(roms_load(@memory_temp, sbagman_char)) then
          exit;
        conv_chars(0);
        conv_sprites;
        // convertir sprites
        if not(roms_load(@memory_temp, sbagman_sprites)) then
          exit;
        conv_chars(2);
        // poner la paleta
        if not(roms_load(@memory_temp, sbagman_pal)) then
          exit;
      end;
  end;
  compute_resistor_weights(0, 255, -1.0, 3, @resistances_rg, @rweights, 470, 0, 3, @resistances_rg, @gweights, 470, 0, 2, @resistances_b, @bweights, 470, 0);
  for f := 0 to $3F do
  begin
    // red component
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    colores[f].r := combine_3_weights(@rweights, bit0, bit1, bit2);
    // green component
    bit0 := (memory_temp[f] shr 3) and $01;
    bit1 := (memory_temp[f] shr 4) and $01;
    bit2 := (memory_temp[f] shr 5) and $01;
    colores[f].g := combine_3_weights(@gweights, bit0, bit1, bit2);
    // blue component
    bit0 := (memory_temp[f] shr 6) and $01;
    bit1 := (memory_temp[f] shr 7) and $01;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $40);
  // DIP
  marcade.dswa := $FE;
marcade.dswa_val2:=@bagman_dip;
  // final
  reset_bagman;
  start_bagman := true;
end;

end.
