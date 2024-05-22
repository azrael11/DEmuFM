unit emu_functions;

interface

uses
  System.Classes,
  umain_config,
  sdl2,
  vars_consts;

type
  TGAME_STATUS = (gs_working, gs_working_minor, gs_working_major, gs_not_working);

function Emulation_Name(emu: TEmulatorSelected): string;
function Get_Emulation_Num(emu: TEmulatorSelected): integer;
function Rom_Total_Time_Name(emu: TEmulatorSelected): string;
function Get_Bezel(emu: TEmulatorSelected; rom: string): PSDL_Surface;

implementation

uses
  uarcade_actions,
  unes_actions, uDataModule;

function Get_Emulation_Num(emu: TEmulatorSelected): integer;
begin
  case emu of
    emus_Arcade:
      Result := 0;
    emus_Nes:
      ;
    emus_Gameboy:
      ;
    emus_Gameboy_Color:
      ;
    emus_Colecovision:
      ;
    emus_Chip8:
      ;
    emus_MasterSystem:
      ;
    emus_SG1000:
      ;
    emus_Gamegear:
      ;
    emus_Epoch_SCV:
      ;
    emus_MegaDrive:
      ;
    emus_GandW:
      ;
    emus_Spectrum:
      ;
    emus_Amstrad:
      ;
    emus_Commodore64:
      ;
  end;
end;

function Emulation_Name(emu: TEmulatorSelected): string;
begin
  case emu of
    emus_Arcade:
      Result := 'arcade';
    emus_Nes:
      Result := 'nes';
    emus_Gameboy:
      ;
    emus_Gameboy_Color:
      ;
    emus_Colecovision:
      ;
    emus_Chip8:
      ;
    emus_MasterSystem:
      ;
    emus_SG1000:
      ;
    emus_Gamegear:
      ;
    emus_Epoch_SCV:
      ;
    emus_MegaDrive:
      ;
    emus_GandW:
      ;
    emus_Spectrum:
      ;
    emus_Amstrad:
      ;
    emus_Commodore64:
      ;
  end;
end;

function Rom_Total_Time_Name(emu: TEmulatorSelected): string;
begin
  case emu of
    emus_Arcade:
      Result := dm.tArcaderom.AsString;
    emus_Nes:
      Result := Nes_Action.rom_nes_selected;
    emus_Gameboy:
      ;
    emus_Gameboy_Color:
      ;
    emus_Colecovision:
      ;
    emus_Chip8:
      ;
    emus_MasterSystem:
      ;
    emus_SG1000:
      ;
    emus_Gamegear:
      ;
    emus_Epoch_SCV:
      ;
    emus_MegaDrive:
      ;
    emus_GandW:
      ;
    emus_Spectrum:
      ;
    emus_Amstrad:
      ;
    emus_Commodore64:
      ;
  end;
end;

function Get_Bezel(emu: TEmulatorSelected; rom: string): PSDL_Surface;
var
  emu_num: integer;
begin
  emu_num := Get_Emulation_Num(emu);
  case emu_num of
    0:
      begin

      end;
  else
    begin

    end;
  end;
end;

end.
