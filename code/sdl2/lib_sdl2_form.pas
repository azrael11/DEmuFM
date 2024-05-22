unit lib_sdl2_form;

interface

uses
  System.Classes,
  FMX.Forms,
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  FMX.Objects;

type
  TFORM_LIB_SDL2 = class(TForm)
    Background: TImage;
  private
  public
  end;

implementation

{$R *.DFM}

end.
