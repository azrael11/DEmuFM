unit splash;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Controls.Presentation, uDataModule;

type
  Tfrm_splash = class(TForm)
    rect_splash: TRectangle;
    img_splash_logo: TImage;
    pb_splash_progress: TProgressBar;
    lbl_splash_info_progress: TLabel;
    lbl_splash_progress: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_splash: Tfrm_splash;
  splash_start: boolean;

implementation

uses
  main,
  umain_actions,
  front_main,
  prj_functions;

{$R *.fmx}
{$R media/lure.res}

procedure Tfrm_splash.FormCreate(Sender: TObject);
begin
  splash_start := True;
  main_actions.main_form_create;
end;

procedure Tfrm_splash.FormShow(Sender: TObject);
begin
  frm_main.imgNotFound := TBitmap.Create;
  LoadImageFromResource(frm_main.imgNotFound, 'IMG_NOT_FOUND');
  front_action.splash := True;
  lbl_splash_info_progress.Text := 'Loading DEmuFM';
  Sleep(1000);

  // Arcade
  lbl_splash_progress.Text := '';
  lbl_splash_info_progress.Text := 'Loading Emulators, Settings and Games';
  Application.ProcessMessages;
  front_action.create_grid(dm.tConfigcurrent_emu.AsString);

  frm_main.Show;
  frm_main.Visible := True;
  self.Hide;
end;

end.
