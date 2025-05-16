unit genesis;

interface

uses
  nz80,
  m68000,
  WinApi.Windows,
  main_engine,
  controls_engine,
  sega_315_5313,
  System.SysUtils,
  rom_engine,
  misc_functions,
  sound_engine,
  file_engine,
  FMX.Dialogs;

procedure cargar_genesis;

implementation

uses
  main;

var
  ram: array [0 .. $7FFF] of word;
  rom: array [0 .. $1FFFFF] of word;
  io_control: array [0 .. $F] of byte;
  z80_is_reset, z80_has_bus: boolean;

procedure genesis_principal;
var
  frame_m, frame_s: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame_m := m68000_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    for f := 0 to 261 do
    begin
      vdp_5313_0.handle_scanline(f);
      // main
      m68000_0.run(frame_m);
      frame_m := frame_m + m68000_0.tframes - m68000_0.contador;
      // sound
      z80_0.run(frame_s);
      frame_s := frame_s + z80_0.tframes - z80_0.contador;
    end;
    vdp_5313_0.handle_eof;
    // eventos_genesis;
    video_sync;
  end;
end;

function genesis_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $3FFF:
      genesis_snd_getbyte := memory[direccion and $1FFF];
    $4000 .. $5FFF:
      halt(0); // YM
    $6000 .. $7EFF:
      genesis_snd_getbyte := $FF;
    $7F00 .. $7F1F:
      halt(0);
    $7F20 .. $7FFF:
      halt(0);
    $8000 .. $FFFF:
      halt(0);
  end;
end;

procedure genesis_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      memory[direccion and $1FFF] := valor;
    $4000 .. $5FFF:
      halt(0); // YM
    $6000 .. $60FF:
      halt(0); // bank RAM
    $6100 .. $7EFF:
      ;
    $7F00 .. $7F1F:
      halt(0);
    $7F20 .. $7FFF:
      halt(0);
    $8000 .. $FFFF:
      halt(0);
  end;
end;

procedure check_z80_bus_reset;
begin
  // Is the z80 RESET line pulled?
  if z80_is_reset then
  begin
    z80_0.reset;
    z80_0.change_halt(ASSERT_LINE);
    // m_ymsnd->reset();
  end
  else
  begin
    // Check if z80 has the bus
    if z80_has_bus then
      z80_0.change_halt(CLEAR_LINE)
    else
      z80_0.change_halt(ASSERT_LINE);
  end;
end;

function genesis_getword(direccion: dword): word;
begin
  case direccion of
    0 .. $3FFFFF:
      genesis_getword := rom[direccion shr 1];
    $A00000 .. $A0FFFF:
      if (not(z80_has_bus) and not(z80_is_reset)) then
        genesis_getword := memory[((direccion and $7FFF) shl 1) or 1] or
          memory[(direccion and $7FFF) shl 1]
      else
        genesis_getword := random($10000);
    $A10000 .. $A1001F:
      genesis_getword := io_control[(direccion and $1F) shr 1];
    $A11100:
      if m68000_0.read_8bits_hi_dir then
        halt(0)
      else
      begin
        if (z80_has_bus or z80_is_reset) then
          genesis_getword := random($10000) or $100
        else
          genesis_getword := random($10000) and $FEFF;
      end; // megadriv_68k_check_z80_bus
    $C00000 .. $C0001F:
      genesis_getword := vdp_5313_0.read(direccion and $1F);
    $E00000 .. $FFFFFF:
      genesis_getword := ram[(direccion and $FFFF) shr 1];
  else
    halt(direccion);
  end;
end;

procedure genesis_putword(direccion: dword; valor: word);
begin
  case direccion of
    0 .. $3FFFFF:
      ;
    $A00000 .. $A0FFFF:
      if (not(z80_has_bus) and not(z80_is_reset)) then
      begin
        if m68000_0.write_8bits_hi_dir then
          memory[((direccion and $7FFF) shl 1) or 1] := valor and $FF
        else
          memory[(direccion and $7FFF) shl 1] := valor shr 8;
      end;
    $A10000 .. $A1001F:
      halt(0);
    $A11100:
      begin
        if (not(m68000_0.write_8bits_hi_dir) and not(m68000_0.write_8bits_lo_dir)) then
        begin
          if (valor and $0100) <> 0 then
            z80_has_bus := false
          else
            z80_has_bus := true;
        end
        else
          halt(0); // megadriv_68k_req_z80_bus
        check_z80_bus_reset;
      end;
    $A11200:
      begin
        if (not(m68000_0.write_8bits_hi_dir) and not(m68000_0.write_8bits_lo_dir)) then
        begin
          if (valor and $0100) <> 0 then
            z80_is_reset := false
          else
            z80_is_reset := true;
        end
        else
          halt(0); // megadriv_68k_req_z80_reset
        check_z80_bus_reset;
      end;
    $A14000 .. $A14FFF:
      ;
    $C00000 .. $C0001F:
      vdp_5313_0.write(direccion and $1F, valor);
    $E00000 .. $FFFFFF:
      ram[(direccion and $FFFF) shr 1] := valor;
  else
    halt(direccion);
  end;
end;

procedure genesis_irq4(state: boolean);
begin
  if state then
    m68000_0.irq[4] := HOLD_LINE
  else
    m68000_0.irq[4] := CLEAR_LINE;
end;

procedure genesis_irq6(state: boolean);
begin
  if state then
    m68000_0.irq[6] := HOLD_LINE
  else
    m68000_0.irq[6] := CLEAR_LINE;
end;

procedure reset_genesis;
begin
  m68000_0.reset;
  vdp_5313_0.reset;
  io_control[0] := $A1;
  io_control[1] := $7F;
  io_control[2] := $7F;
  io_control[3] := $40;
  io_control[4] := $0;
  io_control[5] := $0;
  io_control[6] := $0;
  io_control[7] := $FF;
  io_control[8] := $0;
  io_control[9] := $0;
  io_control[$A] := $FF;
  io_control[$B] := $0;
  io_control[$C] := $0;
  io_control[$D] := $FF;
  io_control[$E] := $0;
  io_control[$F] := $0;
  // z80
  z80_has_bus := true;
  z80_is_reset := true;
  check_z80_bus_reset;
end;

procedure abrir_genesis;
begin
end;

function iniciar_genesis: boolean;
var
  f, longitud: integer;
  tword: word;
begin
  iniciar_genesis := false;
  // if MessageDlg
  // ('Warning. This is a WIP driver, it''s not finished yet and bad things could happen!. Do you want to continue?',
  // mtWarning, [mbYes] + [mbNo], 0) = 7 then
  // exit;
  start_audio(true);
  screen_init(1, 284, 243);
  start_video(284, 243);
  // Main CPU
  m68000_0 := cpu_m68000.create(53693175 div 7, 262, TCPU_68000);
  m68000_0.change_ram16_calls(genesis_getword, genesis_putword);
  // sound cpu
  z80_0 := cpu_z80.create(53693175 div 15, 262);
  z80_0.change_ram_calls(genesis_snd_getbyte, genesis_snd_putbyte);
  // Video
  vdp_5313_0 := vdp_5313_chip.create(false);
  vdp_5313_0.change_irqs(nil, genesis_irq4, genesis_irq6);
  read_file('D:\Datos\dsp\genesis\Flicky (UE) [!].bin', pbyte(@rom), longitud);
  for f := 0 to ((longitud - 1) shr 1) do
  begin
    rom[f] := (rom[f] shr 8) or ((rom[f] and $FF) shl 8);
  end;
end;

procedure cerrar_genesis;
begin
end;

procedure cargar_genesis;
begin
  // principal1.BitBtn10.Glyph := nil;
  // principal1.imagelist2.GetBitmap(4, principal1.BitBtn10.Glyph);
  // principal1.BitBtn10.OnClick := principal1.fLoadCartucho;
  machine_calls.start := iniciar_genesis;
  machine_calls.general_loop := genesis_principal;
  machine_calls.close := cerrar_genesis;
  machine_calls.reset := reset_genesis;
  machine_calls.cartridges := abrir_genesis;
end;

end.
