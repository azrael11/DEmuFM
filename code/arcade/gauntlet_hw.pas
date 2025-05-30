unit gauntlet_hw;

interface

uses
  WinApi.Windows,
  m6502,
  m68000,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pokey,
  pal_engine,
  sound_engine,
  slapstic,
  ym_2151,
  atari_mo,
  file_engine;

function start_gauntlet: boolean;

implementation

uses
  uDataModule;

const
        gauntlet_rom:array[0..5] of tipo_roms=(
        (n:'136041-507.9a';l:$8000;p:0;crc:$8784133f),(n:'136041-508.9b';l:$8000;p:$1;crc:$2843bde3),
        (n:'136037-205.10a';l:$4000;p:$38000;crc:$6d99ed51),(n:'136037-206.10b';l:$4000;p:$38001;crc:$545ead91),
        (n:'136041-609.7a';l:$8000;p:$40000;crc:$5b4ee415),(n:'136041-610.7b';l:$8000;p:$40001;crc:$41f5c9e2));
        gauntlet_sound:array[0..1] of tipo_roms=(
        (n:'136037-120.16r';l:$4000;p:$4000;crc:$6ee7f3cc),(n:'136037-119.16s';l:$8000;p:$8000;crc:$fa19861f));
        gauntlet_char:tipo_roms=(n:'136037-104.6p';l:$4000;p:0;crc:$6c276a1d);
        gauntlet_back:array[0..7] of tipo_roms=(
        (n:'136037-111.1a';l:$8000;p:0;crc:$91700f33),(n:'136037-112.1b';l:$8000;p:$8000;crc:$869330be),
        (n:'136037-113.1l';l:$8000;p:$10000;crc:$d497d0a8),(n:'136037-114.1mn';l:$8000;p:$18000;crc:$29ef9882),
        (n:'136037-115.2a';l:$8000;p:$20000;crc:$9510b898),(n:'136037-116.2b';l:$8000;p:$28000;crc:$11e0ac5b),
        (n:'136037-117.2l';l:$8000;p:$30000;crc:$29a5db41),(n:'136037-118.2mn';l:$8000;p:$38000;crc:$8bf3b263));
        gauntlet_proms:array[0..2] of tipo_roms=(
        (n:'74s472-136037-101.7u';l:$200;p:0;crc:$2964f76f),(n:'74s472-136037-102.5l';l:$200;p:$200;crc:$4d4fec6c),
        (n:'74s287-136037-103.4r';l:$100;p:$400;crc:$6c5ccf08));
        //Gauntlet II
        gauntlet2_rom:array[0..7] of tipo_roms=(
        (n:'136037-1307.9a';l:$8000;p:0;crc:$46fe8743),(n:'136037-1308.9b';l:$8000;p:$1;crc:$276e15c4),
        (n:'136043-1105.10a';l:$4000;p:$38000;crc:$45dfda47),(n:'136043-1106.10b';l:$4000;p:$38001;crc:$343c029c),
        (n:'136044-2109.7a';l:$8000;p:$40000;crc:$1102ab96),(n:'136044-2110.7b';l:$8000;p:$40001;crc:$d2203a2b),
        (n:'136044-2121.6a';l:$8000;p:$50000;crc:$753982d7),(n:'136044-2122.6b';l:$8000;p:$50001;crc:$879149ea));
        gauntlet2_sound:array[0..1] of tipo_roms=(
        (n:'136043-1120.16r';l:$4000;p:$4000;crc:$5c731006),(n:'136043-1119.16s';l:$8000;p:$8000;crc:$dc3591e7));
        gauntlet2_char:tipo_roms=(n:'136043-1104.6p';l:$4000;p:0;crc:$bddc3dfc);
        gauntlet2_back:array[0..15] of tipo_roms=(
        (n:'136043-1111.1a';l:$8000;p:0;crc:$09df6e23),(n:'136037-112.1b';l:$8000;p:$8000;crc:$869330be),
        (n:'136043-1123.1c';l:$4000;p:$10000;crc:$e4c98f01),(n:'136043-1123.1c';l:$4000;p:$14000;crc:$e4c98f01),
        (n:'136043-1113.1l';l:$8000;p:$18000;crc:$33cb476e),(n:'136037-114.1mn';l:$8000;p:$20000;crc:$29ef9882),
        (n:'136043-1124.1p';l:$4000;p:$28000;crc:$c4857879),(n:'136043-1124.1p';l:$4000;p:$2c000;crc:$c4857879),
        (n:'136043-1115.2a';l:$8000;p:$30000;crc:$f71e2503),(n:'136037-116.2b';l:$8000;p:$38000;crc:$11e0ac5b),
        (n:'136043-1125.2c';l:$4000;p:$40000;crc:$d9c2c2d1),(n:'136043-1125.2c';l:$4000;p:$44000;crc:$d9c2c2d1),
        (n:'136043-1117.2l';l:$8000;p:$48000;crc:$9e30b2e9),(n:'136037-118.2mn';l:$8000;p:$50000;crc:$8bf3b263),
        (n:'136043-1126.2p';l:$4000;p:$58000;crc:$a32c732a),(n:'136043-1126.2p';l:$4000;p:$5c000;crc:$a32c732a));
        gauntlet2_proms:array[0..2] of tipo_roms=(
        (n:'74s472-136037-101.7u';l:$200;p:0;crc:$2964f76f),(n:'74s472-136037-102.5l';l:$200;p:$200;crc:$4d4fec6c),
        (n:'82s129-136043-1103.4r';l:$100;p:$400;crc:$32ae1fa9));
        //DIP
        gauntlet_dip:array [0..1] of def_dip2=(
        (mask:8;name:'Service';number:2;val2:(8,0);name2:('Off','On')),());
        gauntlet_mo_config:atari_motion_objects_config=(
        	gfxindex:1;               // index to which gfx system
	        bankcount:1;              // number of motion object banks
	        linked:true;              // are the entries linked?
	        split:true;               // are the entries split?
	        reverse:false;            // render in reverse order?
	        swapxy:false;             // render in swapped X/Y order?
	        nextneighbor:false;       // does the neighbor bit affect the next object?
	        slipheight:8;             // pixels per SLIP entry (0 for no-slip)
	        slipoffset:1;             // pixel offset for SLIPs
	        maxperline:0;             // maximum number of links to visit/scanline (0=all)
	        palettebase:$100;         // base palette entry
	        maxcolors:$100;           // maximum number of colors
	        transpen:0;               // transparent pen index
	        link_entry:(0,0,0,$03ff); // mask for the link
	        code_entry:(data_lower:($7fff,0,0,0);data_upper:(0,0,0,0)); // mask for the code index
	        color_entry:(data_lower:(0,$000f,0,0);data_upper:(0,0,0,0)); // mask for the color
	        xpos_entry:(0,$ff80,0,0); // mask for the X position
          ypos_entry:(0,0,$ff80,0); // mask for the Y position
	        width_entry:(0,0,$0038,0); // mask for the width, in tiles
	        height_entry:(0,0,$0007,0); // mask for the height, in tiles
	        hflip_entry:(0,0,$0040,0); // mask for the horizontal flip
	        vflip_entry:(0,0,0,0);     // mask for the vertical flip
	        priority_entry:(0,0,0,0); // mask for the priority
	        neighbor_entry:(0,0,0,0); // mask for the neighbor
	        absolute_entry:(0,0,0,0);// mask for absolute coordinates
	        special_entry:(0,0,0,0);  // mask for the special value
	        specialvalue:0;           // resulting value to indicate "special"
        );
        CPU_SYNC=4;

var
  rom: array [0 .. $3FFFF] of word;
  slapstic_rom: array [0 .. 3, 0 .. $FFF] of word;
  ram: array [0 .. $1FFF] of word;
  ram2: array [0 .. $5FFF] of word;
  eeprom_ram: array [0 .. $7FF] of byte;
  write_eeprom, sound_to_main_ready, main_to_sound_ready: boolean;
  rom_bank, vblank, sound_to_main_data, main_to_sound_data: byte;
  scroll_x, sound_reset_val: word;

procedure update_video_gauntlet;
var
  f, color, x, y, nchar, atrib, scroll_y: word;
  tile_bank: byte;
begin
  for f := 0 to $7FF do
  begin
    x := f mod 64;
    y := f div 64;
    atrib := ram2[($5000 + (f * 2)) shr 1];
    color := ((atrib shr 10) and $0F) or ((atrib shr 9) and $20);
    if (gfx[0].buffer[f] or buffer_color[color]) then
    begin
      nchar := atrib and $3FF;
      if (atrib and $8000) <> 0 then
      begin
        put_gfx(x * 8, y * 8, nchar, color shl 2, 2, 0);
        put_gfx_block_trans(x * 8, y * 8, 1, 8, 8);
      end
      else
      begin
        put_gfx_trans(x * 8, y * 8, nchar, color shl 2, 1, 0);
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  atrib := ram2[$5F6F shr 1];
  scroll_y := (atrib shr 7) and $1FF;
  tile_bank := atrib and $3;
  for f := 0 to $FFF do
  begin
    x := f div 64;
    y := f mod 64;
    atrib := ram2[f];
    color := (atrib shr 12) and 7;
    if (gfx[1].buffer[f] or buffer_color[$40 + color]) then
    begin
      nchar := ((tile_bank * $1000) + (atrib and $FFF)) xor $800;
      if (atrib and $8000) <> 0 then
      begin
        put_gfx_trans(x * 8, y * 8, nchar, ((color + $18) shl 4) + $100, 4, 1);
        put_gfx_block(x * 8, y * 8, 3, 8, 8, 0);
      end
      else
      begin
        put_gfx(x * 8, y * 8, nchar, ((color + $18) shl 4) + $100, 3, 1);
        put_gfx_block_trans(x * 8, y * 8, 4, 8, 8);
      end;
      gfx[1].buffer[f] := false;
    end;
  end;
  scroll_x_y(3, 5, scroll_x, scroll_y);
  update_region(0, 0, 512, 256, 1, 0, 0, 512, 256, 5);
  atari_mo_0.draw(scroll_x, scroll_y, -1);
  scroll_x_y(4, 5, scroll_x, scroll_y);
  update_region(0, 0, 512, 256, 2, 0, 0, 512, 256, 5);
  update_final_piece(0, 0, 336, 240, 5);
  fillchar(buffer_color[0], MAX_COLOR_BUFFER, 0);
end;

procedure events_gauntlet;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.but1[0] then
      marcade.in0 := (marcade.in0 and $FFFE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $FFFD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FFEF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FFDF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FFBF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $FF7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P2
    if p_contrls.map_arcade.but1[1] then
      marcade.in1 := (marcade.in1 and $FFFE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $FFFD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FFEF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FFDF)
    else
      marcade.in1 := (marcade.in1 or $20);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FFBF)
    else
      marcade.in1 := (marcade.in1 or $40);
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $FF7F)
    else
      marcade.in1 := (marcade.in1 or $80);
    // Audio CPU
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
  end;
end;

procedure gauntlet_loop;
var
  f:word;
  h:byte;
begin
  init_controls(false, false, false, true);
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
 for f:=0 to 261 do begin
    events_gauntlet;
    case f of
      0:begin
          vblank:=$40;
          m6502_0.change_irq(CLEAR_LINE);
        end;
      64,128,192,256:m6502_0.change_irq(CLEAR_LINE);
      32,96,160,224:m6502_0.change_irq(ASSERT_LINE);
      240:begin  //VBLANK
          update_video_gauntlet;
          vblank:=0;
          m68000_0.irq[4]:=ASSERT_LINE;
        end;
    end;
    for h:=1 to CPU_SYNC do begin
      //main
      m68000_0.run(frame_main);
      frame_main:=frame_main+m68000_0.tframes-m68000_0.contador;
      //sound
      m6502_0.run(frame_snd);
      frame_snd:=frame_snd+m6502_0.tframes-m6502_0.contador;
    end;
 end;
 video_sync;
    end
    else
      pause_action;
  end;
end;

function gauntlet_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $37FFF, $40000 .. $7FFFF:
      gauntlet_getword := rom[direccion shr 1];
    // Funciona asi... Creo...
    // De 38K a 3fK esta la proteccion
    // Cuando lee en este espacio, solo lee 2k del slapstic y recalcula el banco
    $38000 .. $3FFFF:
      begin // Slaptic!!
        gauntlet_getword := slapstic_rom[rom_bank, (direccion and $1FFF) shr 1];
        rom_bank := slapstic_0.slapstic_tweak((direccion and $7FFF) shr 1);
      end;
    $800000 .. $801FFF:
      gauntlet_getword := ram[(direccion and $1FFF) shr 1];
    $802000 .. $802FFF:
      gauntlet_getword := $FF00 or eeprom_ram[(direccion and $FFF) shr 1];
    $803000:
      gauntlet_getword := marcade.in0;
    $803002:
      gauntlet_getword := marcade.in1;
    $803004:
      gauntlet_getword := $FFFF;
    $803006:
      gauntlet_getword := $FFFF;
    $803008:
      gauntlet_getword := $FF87 or marcade.dswa or vblank or (byte(sound_to_main_ready) shl 4) or (byte(main_to_sound_ready) shl 5);
    $80300E:
      begin // main_response_r
        sound_to_main_ready := false;
        m68000_0.irq[6] := CLEAR_LINE;
        gauntlet_getword := $FF00 or sound_to_main_data;
      end;
    $900000 .. $905FFF:
      gauntlet_getword := ram2[(direccion and $7FFF) shr 1];
    $910000 .. $9107FF:
      gauntlet_getword := buffer_paleta[(direccion and $7FF) shr 1];
  end;
end;

procedure gauntlet_putword(direccion: dword; valor: word);
  procedure change_color(tmp_color, numero: word); inline;
  var
    color: tcolor;
  begin
    color.r := pal4bit_i(tmp_color shr 8, tmp_color shr 12);
    color.g := pal4bit_i(tmp_color shr 4, tmp_color shr 12);
    color.b := pal4bit_i(tmp_color, tmp_color shr 12);
    set_pal_color(color, numero);
    case numero of
      $0 .. $FF:
        buffer_color[(numero shr 2) and $3F] := true;
      $100 .. $3FF:
        buffer_color[$40 + (numero shr 4) and $7] := true;
    end;
  end;

var
  old: word;
begin
  case direccion of
    0 .. $37FFF, $40000 .. $7FFFF:
      ; // ROM
    $38000 .. $3FFFF:
      rom_bank := slapstic_0.slapstic_tweak((direccion and $7FFF) shr 1); // Slaptic!!
    $800000 .. $801FFF:
      ram[(direccion and $1FFF) shr 1] := valor;
    $802000 .. $802FFF:
      if write_eeprom then
      begin // eeprom
        eeprom_ram[(direccion and $FFF) shr 1] := valor and $FF;
        write_eeprom := false;
      end;
    $803100:
      ; // watch dog
    $803120 .. $80312E:
      begin // sound_reset_w
        old := sound_reset_val;
        sound_reset_val := valor;
        if ((old xor sound_reset_val) and 1) <> 0 then
        begin
          if (sound_reset_val and 1) <> 0 then
            m6502_0.change_reset(CLEAR_LINE)
          else
            m6502_0.change_reset(ASSERT_LINE);
          sound_to_main_ready := false;
          m68000_0.irq[6] := CLEAR_LINE;
          if (sound_reset_val and 1) <> 0 then
          begin
            ym2151_0.reset;
            // m_tms5220->reset();
            // m_tms5220->set_frequency(ATARI_CLOCK_14MHz/2 / 11);
            // m_ym2151->set_output_gain(ALL_OUTPUTS, 0.0f);
            // m_pokey->set_output_gain(ALL_OUTPUTS, 0.0f);
            // m_tms5220->set_output_gain(ALL_OUTPUTS, 0.0f);
          end;
        end;
      end;
    $803140:
      m68000_0.irq[4] := CLEAR_LINE; // video_int_ack_w
    $803150:
      write_eeprom := true; // eeprom_unlock
    $803170:
      begin // main_command_w
        main_to_sound_data := valor;
        main_to_sound_ready := true;
        m6502_0.change_nmi(ASSERT_LINE);
      end;
    $900000 .. $901FFF, $B00000 .. $B01FFF:
      begin
        ram2[(direccion and $7FFF) shr 1] := valor;
        gfx[1].buffer[(direccion and $1FFF) shr 1] := true;
      end;
    $902000 .. $904FFF, $B02000 .. $B04FFF:
      ram2[(direccion and $7FFF) shr 1] := valor;
    $905000 .. $905FFF, $B05000 .. $B05FFF:
      begin
        ram2[(direccion and $7FFF) shr 1] := valor;
        gfx[0].buffer[(direccion and $FFF) shr 1] := true;
      end;
    $910000 .. $9107FF:
      if buffer_paleta[(direccion and $7FF) shr 1] <> valor then
      begin
        buffer_paleta[(direccion and $7FF) shr 1] := valor;
        change_color(valor, (direccion and $7FF) shr 1);
      end;
    $930000, $B30000:
      scroll_x := valor;
  end;
end;

function gauntlet_snd_getbyte(direccion: word): byte;
var
  temp: byte;
begin
  case direccion of
    0 .. $FFF, $4000 .. $FFFF:
      gauntlet_snd_getbyte := mem_snd[direccion];
    $1010 .. $101F:
      begin // sound_command_r
        main_to_sound_ready := false;
        m6502_0.change_nmi(CLEAR_LINE);
        gauntlet_snd_getbyte := main_to_sound_data;
      end;
    $1020 .. $102F:
      gauntlet_snd_getbyte := marcade.in2; // COIN
    $1030 .. $103F:
      begin // switch_6502_r
        temp := $30;
        if main_to_sound_ready then
          temp := temp xor $80;
        if sound_to_main_ready then
          temp := temp xor $40;
        // if (!m_tms5220->readyq_r()) temp:=temp xor $20;
        if marcade.dswa = 8 then
          temp := temp xor $10;
        gauntlet_snd_getbyte := temp;
      end;
    $1800 .. $180F:
      gauntlet_snd_getbyte := pokey_0.read(direccion and $F);
    $1811:
      gauntlet_snd_getbyte := ym2151_0.status;
    $1830 .. $183F:
      begin // sound_irq_ack_r
        m6502_0.change_irq(CLEAR_LINE);
        gauntlet_snd_getbyte := 0;
      end;
  end;
end;

procedure gauntlet_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $FFF:
      mem_snd[direccion] := valor;
    $1000 .. $100F:
      begin // sound_response_w
        sound_to_main_data := valor;
        sound_to_main_ready := true;
        m68000_0.irq[6] := ASSERT_LINE;
      end;
    $1020 .. $102F:
      ; // mixer_w
    $1030 .. $103F:
      case (direccion and 7) of // sound_ctl_w
        0:
          if ((valor and $80) = 0) then
            ym2151_0.reset; // music reset, bit D7, low reset
        1:
          ; // m_tms5220->wsq_w(data >> 7); // speech write, bit D7, active low */
        2:
          ; // m_tms5220->rsq_w(data >> 7); // speech reset, bit D7, active low */
        3:
          ; { begin //* speech squeak, bit D7 */
          data = 5 | ((data >> 6) & 2);
          m_tms5220->set_frequency(ATARI_CLOCK_14MHz/2 / (16 - data));
          end; }
      end;
    $1800 .. $180F:
      pokey_0.write(direccion and $F, valor);
    $1810:
      ym2151_0.reg(valor);
    $1811:
      ym2151_0.write(valor);
    $1820 .. $182F:
      ; // tms5220_device, data_w
    $1830 .. $183F:
      m6502_0.change_irq(CLEAR_LINE); // sound_irq_ack_w
    $4000 .. $FFFF:
      ; // ROM
  end;
end;

procedure gauntlet_sound_update;
begin
  ym2151_0.update;
  pokey_0.update;
  // tms5220_update
end;

// Main
procedure reset_gauntlet;
begin
 slapstic_0.reset;
 rom_bank:=slapstic_0.current_bank;
  m68000_0.reset;
  m6502_0.reset;
  ym2151_0.reset;
  pokey_0.reset;
 frame_main:=m68000_0.tframes;
 frame_snd:=m6502_0.tframes;
  marcade.in0 := $FFFF;
  marcade.in1 := $FFFF;
  marcade.in2 := $FF;
  scroll_x := 0;
  main_to_sound_ready := false;
  sound_to_main_ready := false;
  sound_to_main_data := 0;
  main_to_sound_data := 0;
  sound_reset_val := 1;
  vblank := $40;
  write_eeprom := false;
end;

procedure close_gauntlet;
var
  nombre: string;
begin
  case main_vars.machine_type of
    236:
      nombre := 'gauntlet.nv';
    245:
      nombre := 'gaunt2.nv';
  end;
  write_file(dm.tConfignvram.AsString + nombre, @eeprom_ram, $800);
end;

function start_gauntlet: boolean;
var
  memory_temp: array [0 .. $7FFFF] of byte;
  f: dword;
  temp: pword;
  longitud: integer;
const
  pc_x: array [0 .. 7] of dword = (0, 1, 2, 3, 8, 9, 10, 11);
  pc_y: array [0 .. 7] of dword = (0 * 16, 1 * 16, 2 * 16, 3 * 16, 4 * 16, 5 * 16, 6 * 16, 7 * 16);
  ps_x: array [0 .. 7] of dword = (0, 1, 2, 3, 4, 5, 6, 7);
  ps_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  procedure load_and_change_roms;
  begin
    copymemory(@rom[0], @memory_temp[$8000], $8000);
    copymemory(@rom[$8000 shr 1], @memory_temp[$0], $8000);
    copymemory(@rom[$40000 shr 1], @memory_temp[$48000], $8000);
    copymemory(@rom[$48000 shr 1], @memory_temp[$40000], $8000);
    copymemory(@rom[$50000 shr 1], @memory_temp[$58000], $8000);
    copymemory(@rom[$58000 shr 1], @memory_temp[$50000], $8000);
    copymemory(@rom[$60000 shr 1], @memory_temp[$68000], $8000);
    copymemory(@rom[$68000 shr 1], @memory_temp[$60000], $8000);
    copymemory(@rom[$70000 shr 1], @memory_temp[$78000], $8000);
    copymemory(@rom[$78000 shr 1], @memory_temp[$70000], $8000);
    copymemory(@slapstic_rom[0, 0], @memory_temp[$38000], $2000);
    copymemory(@slapstic_rom[1, 0], @memory_temp[$3A000], $2000);
    copymemory(@slapstic_rom[2, 0], @memory_temp[$3C000], $2000);
    copymemory(@slapstic_rom[3, 0], @memory_temp[$3E000], $2000);
  end;

begin
  machine_calls.general_loop := gauntlet_loop;
  machine_calls.reset := reset_gauntlet;
  machine_calls.close := close_gauntlet;
  machine_calls.fps_max := 59.922743;
  start_gauntlet := false;
  start_audio(true);
  // Chars
  screen_init(1, 512, 256, true);
  screen_init(2, 512, 256, true);
  // Back
  screen_init(3, 512, 512, true);
  screen_mod_scroll(3, 512, 512, 511, 512, 256, 511);
  screen_init(4, 512, 512, true);
  screen_mod_scroll(4, 512, 512, 511, 512, 256, 511);
  // Final
  screen_init(5, 512, 512, false, true);
  start_video(336, 240);
  // Main CPU
  m68000_0 := cpu_m68000.create(14318180 div 2, 262 * CPU_SYNC, TCPU_68010);
  m68000_0.change_ram16_calls(gauntlet_getword, gauntlet_putword);
  // Sound CPU
  m6502_0 := cpu_m6502.create(14318180 div 8, 262 * CPU_SYNC, TCPU_M6502);
  m6502_0.change_ram_calls(gauntlet_snd_getbyte, gauntlet_snd_putbyte);
  m6502_0.init_sound(gauntlet_sound_update);
  // Sound Chips
  ym2151_0 := ym2151_chip.create(14318180 div 4);
  pokey_0 := pokey_chip.create(14318180 div 8);
  // TMS5220
  case main_vars.machine_type of
    236:
      begin // Gauntlet
        // Slapstic
        slapstic_0 := slapstic_type.create(107, true);
        // cargar roms
        if not(roms_load16w(@memory_temp, gauntlet_rom)) then
          exit;
        load_and_change_roms;
        // cargar sonido
        if not(roms_load(@mem_snd, gauntlet_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, gauntlet_char)) then
          exit;
        init_gfx(0, 8, 8, $400);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
        // convertir fondo
        if not(roms_load(@memory_temp, gauntlet_back)) then
          exit;
        for f := 0 to $3FFFF do
          memory_temp[f] := not(memory_temp[f]);
        init_gfx(1, 8, 8, $2000);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(4, 0, 8 * 8, $2000 * 8 * 8 * 3, $2000 * 8 * 8 * 2, $2000 * 8 * 8 * 1, $2000 * 8 * 8 * 0);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // eeprom
        if read_file_size(dm.tConfignvram.AsString + 'gauntlet.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'gauntlet.nv', @eeprom_ram, longitud)
        else
          fillchar(eeprom_ram[0], $800, $FF);
        // DIP
        marcade.dswa := 8;
        marcade.dswa_val2 := @gauntlet_dip;
      end;
    245:
      begin // Gauntlet II
        // Slapstic
        slapstic_0 := slapstic_type.create(106, true);
        // cargar roms
        if not(roms_load16w(@memory_temp, gauntlet2_rom)) then
          exit;
        load_and_change_roms;
        // cargar sonido
        if not(roms_load(@mem_snd, gauntlet2_sound)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, gauntlet2_char)) then
          exit;
        init_gfx(0, 8, 8, $400);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 0, 4);
        convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, false);
        // convertir fondo
        if not(roms_load(@memory_temp, gauntlet2_back)) then
          exit;
        for f := 0 to $7FFFF do
          memory_temp[f] := not(memory_temp[f]);
        init_gfx(1, 8, 8, $3000);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(4, 0, 8 * 8, $3000 * 8 * 8 * 3, $3000 * 8 * 8 * 2, $3000 * 8 * 8 * 1, $3000 * 8 * 8 * 0);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, false);
        // eeprom
        if read_file_size(dm.tConfignvram.AsString + 'gaunt2.nv', longitud) then
          read_file(dm.tConfignvram.AsString + 'gaunt2.nv', @eeprom_ram, longitud)
        else
          fillchar(eeprom_ram[0], $800, $FF);
        // DIP
        marcade.dswa := $8;
        marcade.dswa_val := @gauntlet_dip;
      end;
  end;
  // atari mo
  atari_mo_0 := tatari_mo.create(@ram2[$5F80 shr 1], @ram2[$2000 shr 1], gauntlet_mo_config, 5, 336 + 8, 240 + 8);
  // modify the motion object code lookup table to account for the code XOR
  temp := atari_mo_0.get_codelookup;
  for f := 0 to $7FFF do
  begin
    temp^ := temp^ xor $800;
    inc(temp);
  end;
  // final
  start_gauntlet := true;
end;

end.
