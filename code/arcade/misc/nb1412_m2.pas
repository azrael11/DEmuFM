unit nb1412_m2;

interface

uses
  WinApi.Windows,
  FMX.Dialogs,
  System.SysUtils;

type
  tnb1412_m2 = class
    constructor create(memoria: pbyte);
    destructor free;
  public
    procedure reset;
    function read: byte;
    procedure write(valor: byte);
    procedure command(valor: byte);
  private
    command_: byte;
    mem: pbyte;
    rom_address, adj_address: word;
    rom_op: byte;
  end;

var
  nb1412m2_0: tnb1412_m2;

implementation

constructor tnb1412_m2.create(memoria: pbyte);
begin
  self.mem := memoria;
  self.reset;
end;

destructor tnb1412_m2.free;
begin
end;

procedure tnb1412_m2.reset;
begin
  self.command_ := $FF;
  self.rom_address := 0;
  self.adj_address := 0;
  self.rom_op := 0;
end;

function tnb1412_m2.read: byte;
var
  res: byte;
begin
  case self.command_ of
    $32 .. $36:
      ;
    $37:
      begin // rom_decript
        res := ($43 - self.mem[self.adj_address]) and $FF;
        read := self.mem[self.rom_address and $1FFF] - res;
      end
  else
    begin
      // MessageDlg('NB1412 Read command: ' + inttostr(self.command_), mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure tnb1412_m2.write(valor: byte);
begin
  case self.command_ of
    $32:
      begin
        if valor = 2 then
        begin
          // MessageDlg('NB1412 command $32-2', mtInformation, [mbOk], 0);
        end;
        self.rom_op := valor;
      end;
    $33:
      self.rom_address := (self.rom_address and $FF) or (valor shl 8);
    $34:
      self.rom_address := (self.rom_address and $FF00) or valor;
    $35:
      self.adj_address := (self.adj_address and $FF) or (valor shl 8);
    $36:
      self.adj_address := (self.adj_address and $FF00) or valor;
  else
    begin
      // MessageDlg('NB1412 Write command: ' + inttostr(self.command_), mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure tnb1412_m2.command(valor: byte);
begin
  self.command_ := valor;
end;

end.
