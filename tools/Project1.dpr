program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  manage_dspfm_data in 'manage_dspfm_data.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
