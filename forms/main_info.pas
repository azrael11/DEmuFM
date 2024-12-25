unit main_info;

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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo, System.Skia, FMX.Skia;

type
  Tfrm_info = class(TForm)
    rect_info_main: TRectangle;
    rect_info_header: TRectangle;
    lbl_info_header: TLabel;
    img_info_logo: TImage;
    lbl_info_created: TLabel;
    lbl_info_created_value: TLabel;
    lbl_info_programming: TLabel;
    lbl_info_programming_value: TLabel;
    lbl_info_version_value: TLabel;
    lbl_info_version: TLabel;
    memo_info: TMemo;
    SkAnimatedImage1: TSkAnimatedImage;
    sbInfoExit: TSpeedButton;
    imgInfoExit: TImage;
    procedure FormShow(Sender: TObject);
    procedure txt_info_closeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure txt_info_closeMouseLeave(Sender: TObject);
    procedure txt_info_closeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure sbInfoExitClick(Sender: TObject);
    procedure sbInfoExitMouseEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_info: Tfrm_info;

implementation

{$R *.fmx}

uses
  main,
  umain_config,
  prj_functions,
  capsdefs,
  uDataModule,
  ulang;

procedure Tfrm_info.FormShow(Sender: TObject);
begin
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;
  Self.Caption := lang.getTransString(INFORMATION, dm.tConfiglang.AsInteger);
  lbl_info_version_value.Text := get_version;
  // Translations
  lbl_info_header.Text := lang.getTransString(INFORMATION, dm.tConfiglang.AsInteger);
  lbl_info_created.Text := lang.getTransString(CREATED_BY, dm.tConfiglang.AsInteger);
  lbl_info_created_value.Text := lang.getTransString(CREATOR, dm.tConfiglang.AsInteger);
  lbl_info_programming.Text := lang.getTransString(PROGRAMMING_LANGUAGE, dm.tConfiglang.AsInteger);
  lbl_info_version.Text := lang.getTransString(VERSION, dm.tConfiglang.AsInteger);
  memo_info.Lines.Text := lang.getTransString(INFO_MEMO, dm.tConfiglang.AsInteger);
  frm_Main.eff_blur_main.Enabled := true;
end;

procedure Tfrm_info.sbInfoExitClick(Sender: TObject);
begin
  frm_Main.eff_blur_main.Enabled := false;
  close;
end;

procedure Tfrm_info.sbInfoExitMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Cursor := crHandPoint;
end;

procedure Tfrm_info.txt_info_closeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  (Sender as TText).Scale.X := 1.0;
  (Sender as TText).Scale.Y := 1.0;
  (Sender as TText).Position.X := 3;
  (Sender as TText).Position.Y := 3;
end;

procedure Tfrm_info.txt_info_closeMouseLeave(Sender: TObject);
begin
  (Sender as TText).Scale.X := 1.1;
  (Sender as TText).Scale.Y := 1.1;
  (Sender as TText).Position.X := 0;
  (Sender as TText).Position.Y := 0;
end;

procedure Tfrm_info.txt_info_closeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  (Sender as TText).Scale.X := 1.1;
  (Sender as TText).Scale.Y := 1.1;
  (Sender as TText).Position.X := 0;
  (Sender as TText).Position.Y := 0;
  close;
end;

end.
