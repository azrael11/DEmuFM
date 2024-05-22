program DSPFM_data_care;

uses
  System.StartUpCopy,
  FMX.Forms,
  manage_dspfm_data in 'manage_dspfm_data.pas' {frm_main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm_main, frm_main);
  Application.Run;
end.
