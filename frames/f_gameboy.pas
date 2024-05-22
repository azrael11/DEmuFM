unit f_gameboy;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.TabControl;

type
  Tgameboy = class(TFrame)
    od_gameboy_config: TOpenDialog;
    tc_gameboy: TTabControl;
    ti_gameboy_general: TTabItem;
    ti_gameboy_dirs: TTabItem;
    verts_gameboy_dirs: TVertScrollBox;
    rect_gameboy_dirs: TRectangle;
    edt_gameboy_dirs_images: TEdit;
    edt_gameboy_dirs_manuals: TEdit;
    edt_gameboy_dirs_roms: TEdit;
    edt_gameboy_dirs_video: TEdit;
    spb_gameboy_dirs_images: TSpeedButton;
    img_gameboy_dirs_images: TImage;
    spb_gameboy_dirs_manuals: TSpeedButton;
    img_gameboy_dirs_manuals: TImage;
    spb_gameboy_dirs_roms: TSpeedButton;
    img_gameboy_dirs_roms: TImage;
    spb_gameboy_dirs_video: TSpeedButton;
    img_gameboy_dirs_video: TImage;
    txt_gameboy_dirs_images: TText;
    txt_gameboy_dirs_manuals: TText;
    txt_gameboy_dirs_roms: TText;
    txt_gameboy_dirs_video: TText;
    pb_gameboy_dirs: TProgressBar;
    txt_gameboy_dirs: TText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
