unit multi_platform;

interface

uses
  System.Classes,
  System.Character,
  FMX.Platform,
  System.UITypes,
  System.SysUtils,
  FMX.Dialogs,
  FMX.DialogService,
  FMX.Controls,
  FMX.Types,
  FMX.Forms,
  FMX.StdCtrls,
  SDL2;

// dialogs
function mpMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn): Integer;
// Time
function int_to_time(secs: Integer): string;
// ZOrder Controls
function zzGetControlZOrder(aControl: TObject): Integer;
// only Controls are processed, but you can throw anything you like at this
// get the Z order of the control within its parent
// if not found or not applicable return -1

function zzSetControlZOrder(aControl: TObject; aNewZorder: Integer): boolean;
// ignore if this is -1
// only Controls are processed, but you can throw anything you like at this
// set the Z order of the control within its parent
// if a control already exists at this Z Order, the control will be placed underneath it (pushing the other control up)

function ContainsAnyChar(const str: string): boolean;
function IsNumeric(const str: string): boolean;


//SDL2 Relatives
function CreateSurfaceWithOpacity(width, height: Integer; opacity: Byte): PSDL_Surface;


implementation

function mpMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn): Integer;
var
  ASyncService: IFMXDialogServiceASync;
  before: Integer;
begin

end;

function int_to_time(secs: Integer): string;
var
  days, hours, minutes, seconds: Integer;
begin
  if secs <> 0 then
  begin
    seconds := secs mod SecsPerMin;
    minutes := secs div SecsPerMin;
    hours := secs div SecsPerHour;
    days := secs div MSecsPerDay;

    Result := '';

    if days <> 0 then
    begin
      if days = 1 then
        Result := '1 day, '
      else
        Result := days.ToString + ' days, ';
    end;

    if hours <> 0 then
    begin
      if hours = 1 then
        Result := Result + '1 hour, '
      else
        Result := Result + hours.ToString + ' hours, ';
    end;

    if minutes <> 0 then
    begin
      if minutes = 1 then
        Result := Result + '1 minute, '
      else
        Result := Result + minutes.ToString + ' minutes, ';
    end;

    if seconds = 1 then
    begin
      if Result = '' then
        Result := 'just 1 second'
      else
        Result := Result + '1 second.';
    end
    else
    begin
      if Result = '' then
        Result := seconds.ToString + ' seconds.'
      else
        Result := Result + seconds.ToString + ' seconds.';
    end;
  end
  else
    Result := 'Not play it yet.';
end;

function zzGetControlZOrder(aControl: TObject): Integer;
var
  i: Integer;
  vControl: TControl;
  vList: TList;
  vParent: TFmxObject;
begin
  Result := -1; // flag for not found or not relevent

  try

    if (aControl is TControl) // some of these might not be needed
      and ((aControl as TControl).Parent is TComponent) and ((aControl as TControl).Parent <> nil)
      and ((aControl as TControl).Parent is TFmxObject) then
    begin
      vControl := aControl as TControl;
      vParent := vControl.Parent as TFmxObject;

      vList := TList.Create;
      // determine current position in z-order
      for i := 0 to vParent.ChildrenCount - 1 do // loop through all children
        if vParent.Children[i] = aControl then // found the control
        begin
          Result := i;
          Break;
        end;

      vList.Free;
    end;
  except // ignore all errors
  end;
end;

function zzSetControlZOrder(aControl: TObject; aNewZorder: Integer): boolean;
var
  i: Integer;
  vControl: TControl;
  vZorderList: TList; // list of controls to be raised
  vParent: TFmxObject; // form, panel, button etc
  vCurrentZ: Integer;
  vFirstZ: Integer;
  vStart: Integer;
begin
  Result := FALSE;

  vCurrentZ := zzGetControlZOrder(aControl);

  if (aControl is TControl) and (vCurrentZ >= 0) and (aNewZorder >= 0) and (vCurrentZ <> aNewZorder)
    and ((aControl as TControl).Parent <> nil) then
  begin
    vZorderList := TList.Create;
    try
      vControl := aControl as TControl;
      vParent := vControl.Parent as TFmxObject;

      if aNewZorder > vCurrentZ // if moving higher in Z order
      then
        vFirstZ := aNewZorder + 1
      else
        vFirstZ := aNewZorder;

      // TForm and TPanel includes a child element #0 that is a TRectangle.
      // the control should not be moved underneath the TRectangle otherwise
      // it will be covered up by the TRectangle and wont be visible
      if (1 = 2) or (vParent is TForm) or (vParent is TPanel) then
        vStart := 1
      else
        vStart := 0;

      if vStart > vFirstZ then
        vFirstZ := vStart;

      // populate list of controls that need to be raised to the top
      vZorderList.Add(aControl); // source control
      for i := vFirstZ to vParent.ChildrenCount - 1 do
        if vParent.Children[i] <> aControl then // skip source control as already added
          vZorderList.Add(vParent.Children[i]); // other controls

      // loop through controls and bring each one to the front
      for i := 0 to vZorderList.count - 1 do
        TControl(vZorderList[i]).BringToFront;

      if vParent is TControl then
        (vParent as TControl).Repaint;

      Result := TRUE;
    finally
      vZorderList.Free;
    end;
  end;
end;

function ContainsAnyChar(const str: string): boolean;
const
  vChars = ['a' .. 'z', 'A' .. 'Z', '~' .. '?'];
var
  i: Integer;
  vStr: String;
begin
  Result := False;
  vStr := str.Trim;
  for i := 1 to Length(vStr) do
  begin
    if (vStr[i] in vChars) then
      Result := True;
//      raise Exception.Create('Non Correct');
  end;
end;

function IsNumeric(const str: string): boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(str) do
    if not IsDigit(str[i]) then
    begin
      Result := False;
      Exit;
    end;
end;


function CreateSurfaceWithOpacity(width, height: Integer; opacity: Byte): PSDL_Surface;
var
  format: UInt32;
  surface: PSDL_Surface;
  pixels: PByte;
  i: Integer;
begin
  format := SDL_PIXELFORMAT_ARGB2101010;
  surface := SDL_CreateRGBSurfaceWithFormat(0, width, height, 32, format);
  if (surface <> nil) then
  begin
    pixels := surface^.pixels;
    for i := 0 to width * height - 1 do
    begin
      pixels^ := 0; // R
      Inc(pixels);
      pixels^ := 0; // G
      Inc(pixels);
      pixels^ := 0; // B
      Inc(pixels);
      pixels^ := opacity; // A
      Inc(pixels);
    end;
  end;
  Result := surface;
end;

end.
