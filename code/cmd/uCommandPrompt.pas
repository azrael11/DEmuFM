unit uCommandPrompt;

interface

uses
  System.Classes,
  System.StrUtils,
  WinApi.Windows;

type
  TCOMMANDPROMPT = class(TObject)
  private

  protected

  public
    constructor Create;
    destructor Destroy;

    function runCMD(param: string): boolean;

    // program related
    procedure showHelp;
    procedure showVer;

    // games related
    procedure listGames(params: string);

    // emulators related
    procedure listMachinesEmulated(params: string);

  published

  end;

var
  cmd: TCOMMANDPROMPT;

implementation

uses
  prj_functions;

{ TCOMMANDPROMPT }

constructor TCOMMANDPROMPT.Create;
begin

end;

destructor TCOMMANDPROMPT.Destroy;
begin
  inherited;
end;

procedure TCOMMANDPROMPT.listGames(params: string);
begin
  if AttachConsole(ATTACH_PARENT_PROCESS) then
  begin
    Writeln('Game 1');
    Writeln('Game 2');
    Writeln('Game 3');
    Writeln('Press any key to exit...');
    Readln;
    FreeConsole;
    Halt(0);
  end;
end;

procedure TCOMMANDPROMPT.listMachinesEmulated(params: string);
begin

end;

function TCOMMANDPROMPT.runCMD(param: string): boolean;
begin
  result := false;
  if ContainsText(param, '-listgames') then
    cmd.listGames(ParamStr(1))
  else if ContainsText(param, '-ver') then
    cmd.showVer;

  result := true;
end;

procedure TCOMMANDPROMPT.showHelp;
begin

end;

procedure TCOMMANDPROMPT.showVer;
begin
  if AttachConsole(ATTACH_PARENT_PROCESS) then
  begin
    Writeln('');
    Writeln(GetVersion);
    Readln;
    FreeConsole;
    Halt(0)
  end;
end;

end.
