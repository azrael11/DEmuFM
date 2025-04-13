unit prj_functions;

interface

uses
  System.Classes,
  Winapi.Windows,
  System.SysUtils,
  System.UITypes,
  System.Math,
  System.UIConsts,
  FMX.Graphics,
  FMX.Types,
  FMX.Utils;

// Keyboard const keys
const
  keys: array [0 .. 101] of string = ('ESC', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9',
    'F10', 'F11', 'F12', '`', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', 'BSPACE',
    'TAB', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '[', ']', '\', 'CAPSLOCK', 'A', 'S',
    'D', 'F', 'G', 'H', 'J', 'K', 'L', ';', '''', 'ENTER', 'LSHIFT', 'Z', 'X', 'C', 'V', 'B', 'N',
    'M', ',', '.', '/', 'RSHIFT', 'LCTRL', 'WIN', 'LAlT', 'SPACE', 'RALT', 'FN', 'WINMENU', 'RCTRL',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '');

  // get version
function Version_Get_info(mFileName: string): TStringlist;
function Get_version: string;

// convert and image to grayscale
type
  Talgorithm = (algnone, algluminosity, algaverage, alglightness);

function ConvertToGrayscale(const aBitmap: TBitmap; const aMethod: Talgorithm = algnone)
  : TBitmap; overload;
function ConvertToGrayscale(const FileName: String; const aMethod: Talgorithm = algnone)
  : TBitmap; overload;

// convert keystrokes to string
// SDL_2
function key_name(num: word): string;
function key_num(name: string): word;
function convert_to_sdl2_key(num: byte): byte;
// Delphi
function key_to_string(key: word): String;

procedure set_key_control(player: byte; key: String; put_key: word);

// Resources
procedure LoadImageFromResource(Image: TBitmap; const ResourceName: string);


implementation

uses
  umain_config,
  controls_engine,
  f_dspfm,
  uDataModule;

function Version_Get_info(mFileName: string): TStringlist;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := TStringlist.Create;
  VerInfoSize := GetFileVersionInfoSize(PChar(mFileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(mFileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
        begin
          Result.Add(FloatToStr(HiWord(dwFileVersionMS)));
          Result.Add(FloatToStr(LoWord(dwFileVersionMS)));
          Result.Add(FloatToStr(HiWord(dwFileVersionLS)));
          Result.Add(FloatToStr(LoWord(dwFileVersionLS)));
        end;
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

function Get_version: string;
var
  temp_list: TStringlist;
begin
  temp_list := prj_functions.Version_Get_info(dm.tConfigprj_path.AsString + dm.tConfigprj_name.AsString);
  Result := temp_list[0];
  Result := Result + '.' + temp_list[1];
  Result := Result + '.' + temp_list[2];
  Result := Result + '.' + temp_list[3];
end;

function Colortogray(const aColor: Talphacolor; const aAlgo: Talgorithm = algnone): Talphacolor;
var
  H, S, L: Single;
  C: TAlphacolorRec;
  // https://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/
begin
  RGBToHSL(aColor, H, S, L);
  C.Color := aColor;
  case aAlgo of
    algluminosity:
      begin
        H := Trunc(0.2126 * C.R) + Trunc(0.7152 * C.G) + Trunc(0.0722 * C.B);
        Exit(HSLToRGB(H, S, L));
      end;
    algaverage:
      begin
        var
          mean: integer := (C.R + C.G + C.B) div 3;
        C.R := mean;
        C.G := mean;
        C.B := mean;
        Exit(C.Color);
      end;
    alglightness:
      begin
        H := (maxvalue([TAlphacolorRec(aColor).R, TAlphacolorRec(aColor).G,
          TAlphacolorRec(aColor).B]) + minvalue([TAlphacolorRec(aColor).R, TAlphacolorRec(aColor).G,
          TAlphacolorRec(aColor).B])) / 2;
        Exit(HSLToRGB(H, S, L));
      end;
  else
    Exit(HSLToRGB(0, 0, L));
  end;

end;

function ConvertToGrayscale(const aBitmap: TBitmap; const aMethod: Talgorithm = algnone): TBitmap;
var
  X, Y: integer;
  bd1, bd2: TBitmapData;
  p1, p2: PAlphaColorArray;
begin
  Result := TBitmap.Create(Round(aBitmap.Width), Round(aBitmap.Height));
  if (aBitmap.Map(TMapAccess.Read, bd1) and Result.Map(TMapAccess.Write, bd2)) then
  begin
    try
      for Y := 0 to (aBitmap.Height - 1) do
      begin
        p1 := PAlphaColorArray(bd1.GetScanline(Y));
        p2 := PAlphaColorArray(bd2.GetScanline(Y));
        for X := 0 to (aBitmap.Width - 1) do
        begin
          p2[X] := Colortogray(p1[X], aMethod);
        end;
      end;
    finally
      aBitmap.Unmap(bd1);
      Result.Unmap(bd2);
    end;
  end;
end;

function ConvertToGrayscale(const FileName: String; const aMethod: Talgorithm = algnone): TBitmap;

var
  X, Y: integer;
  bd1, bd2: TBitmapData;
  p1, p2: PAlphaColorArray;
  Source: TBitmap;
begin
  Source := TBitmap.Create;
  try
    Source.LoadFromFile(FileName);

    Result := TBitmap.Create(Round(Source.Width), Round(Source.Height));
    if (Source.Map(TMapAccess.Read, bd1) and Result.Map(TMapAccess.Write, bd2)) then
    begin
      try
        for Y := 0 to (Source.Height - 1) do
        begin
          p1 := PAlphaColorArray(bd1.GetScanline(Y));
          p2 := PAlphaColorArray(bd2.GetScanline(Y));
          for X := 0 to (Source.Width - 1) do
          begin
            p2[X] := Colortogray(p1[X], aMethod);
          end;
        end;
      finally
        Source.Unmap(bd1);
        Result.Unmap(bd2);
        Source.Free;
      end;
    end;
  except
    Source.Free;
    Result := nil;
  end;
end;

// convert keystrokes to string
function key_name(num: word): string;
begin
  case num of
    KEYBOARD_ESCAPE:
      Result := 'ESC';
    KEYBOARD_CAPSLOCK:
      Result := 'CAPSLOCK';
    KEYBOARD_TAB:
      Result := 'TAB';
    // KEYBOARD_SLASH:Result:='SLASH';
    // KEYBOARD_QUOTE:Result:='QUOTE';
    // KEYBOARD_SEMICOLON:Result:='SEMICOLON';
    // KEYBOARD_BACKSLASH:Result:='DELETE';
    // KEYBOARD_LESS:Result:='LESS';
    KEYBOARD_HOME:
      Result := 'HOME';
    KEYBOARD_RIGHT:
      Result := 'RIGHT';
    KEYBOARD_LEFT:
      Result := 'LEFT';
    KEYBOARD_DOWN:
      Result := 'DOWN';
    KEYBOARD_UP:
      Result := 'UP';
    KEYBOARD_RALT:
      Result := 'RIGHT' + chr(10) + chr(13) + 'ALT';
    KEYBOARD_LALT:
      Result := 'LEFT' + chr(10) + chr(13) + 'ALT';
    KEYBOARD_RSHIFT:
      Result := 'RIGHT' + chr(10) + chr(13) + 'SHIFT';
    KEYBOARD_LSHIFT:
      Result := 'LEFT' + chr(10) + chr(13) + 'SHIFT';
    KEYBOARD_RCTRL:
      Result := 'RIGHT' + chr(10) + chr(13) + 'CONTROL';
    KEYBOARD_LCTRL:
      Result := 'LEFT' + chr(10) + chr(13) + 'CONTROL';
    KEYBOARD_RETURN:
      Result := 'ENTER';
    KEYBOARD_SPACE:
      Result := 'SPACE';
    KEYBOARD_A:
      Result := 'A';
    KEYBOARD_B:
      Result := 'B';
    KEYBOARD_C:
      Result := 'C';
    KEYBOARD_D:
      Result := 'D';
    KEYBOARD_E:
      Result := 'E';
    KEYBOARD_F:
      Result := 'F';
    KEYBOARD_G:
      Result := 'G';
    KEYBOARD_H:
      Result := 'H';
    KEYBOARD_I:
      Result := 'I';
    KEYBOARD_J:
      Result := 'J';
    KEYBOARD_K:
      Result := 'K';
    KEYBOARD_L:
      Result := 'L';
    KEYBOARD_M:
      Result := 'M';
    KEYBOARD_N:
      Result := 'N';
    KEYBOARD_O:
      Result := 'O';
    KEYBOARD_P:
      Result := 'P';
    KEYBOARD_Q:
      Result := 'Q';
    KEYBOARD_R:
      Result := 'R';
    KEYBOARD_S:
      Result := 'S';
    KEYBOARD_T:
      Result := 'T';
    KEYBOARD_U:
      Result := 'U';
    KEYBOARD_V:
      Result := 'V';
    KEYBOARD_W:
      Result := 'W';
    KEYBOARD_X:
      Result := 'X';
    KEYBOARD_Y:
      Result := 'Y';
    KEYBOARD_Z:
      Result := 'Z';
    KEYBOARD_1:
      Result := '1';
    KEYBOARD_2:
      Result := '2';
    KEYBOARD_3:
      Result := '3';
    KEYBOARD_4:
      Result := '4';
    KEYBOARD_5:
      Result := '5';
    KEYBOARD_6:
      Result := '6';
    KEYBOARD_7:
      Result := '7';
    KEYBOARD_8:
      Result := '8';
    KEYBOARD_9:
      Result := '9';
    KEYBOARD_0:
      Result := '0';
    KEYBOARD_F1:
      Result := 'F1';
    KEYBOARD_F2:
      Result := 'F2';
    KEYBOARD_F3:
      Result := 'F3';
    KEYBOARD_F4:
      Result := 'F4';
    KEYBOARD_F5:
      Result := 'F5';
    KEYBOARD_F6:
      Result := 'F6';
    KEYBOARD_F7:
      Result := 'F7';
    KEYBOARD_F8:
      Result := 'F8';
    KEYBOARD_F9:
      Result := 'F9';
    KEYBOARD_F10:
      Result := 'F10';
    KEYBOARD_F11:
      Result := 'F11';
    KEYBOARD_F12:
      Result := 'F12';
    KEYBOARD_NONE:
      Result := 'N/D';
  else
    Result := 'N/D';
  end;
end;

function key_num(name: string): word;
begin
  if name = 'ESC' then
    Result := KEYBOARD_ESCAPE
  else if name = 'A' then
    Result := KEYBOARD_A
  else if name = 'B' then
    Result := KEYBOARD_B
  else if name = 'C' then
    Result := KEYBOARD_C
  else if name = 'D' then
    Result := KEYBOARD_D
  else if name = 'E' then
    Result := KEYBOARD_E
  else if name = 'F' then
    Result := KEYBOARD_F
  else if name = 'G' then
    Result := KEYBOARD_G
  else if name = 'H' then
    Result := KEYBOARD_H
  else if name = 'I' then
    Result := KEYBOARD_I
  else if name = 'J' then
    Result := KEYBOARD_J
  else if name = 'K' then
    Result := KEYBOARD_K
  else if name = 'L' then
    Result := KEYBOARD_L
  else if name = 'M' then
    Result := KEYBOARD_M
  else if name = 'N' then
    Result := KEYBOARD_N
  else if name = 'O' then
    Result := KEYBOARD_O
  else if name = 'P' then
    Result := KEYBOARD_P
  else if name = 'Q' then
    Result := KEYBOARD_Q
  else if name = 'R' then
    Result := KEYBOARD_R
  else if name = 'S' then
    Result := KEYBOARD_S
  else if name = 'T' then
    Result := KEYBOARD_T
  else if name = 'U' then
    Result := KEYBOARD_U
  else if name = 'V' then
    Result := KEYBOARD_V
  else if name = 'W' then
    Result := KEYBOARD_W
  else if name = 'X' then
    Result := KEYBOARD_X
  else if name = 'Y' then
    Result := KEYBOARD_Y
  else if name = 'Z' then
    Result := KEYBOARD_Z
  else if name = '1' then
    Result := KEYBOARD_1
  else if name = '2' then
    Result := KEYBOARD_2
  else if name = '3' then
    Result := KEYBOARD_3
  else if name = '4' then
    Result := KEYBOARD_4
  else if name = '5' then
    Result := KEYBOARD_5
  else if name = '6' then
    Result := KEYBOARD_6
  else if name = '7' then
    Result := KEYBOARD_7
  else if name = '8' then
    Result := KEYBOARD_8
  else if name = '9' then
    Result := KEYBOARD_9
  else if name = '0' then
    Result := KEYBOARD_0
  else if name = 'RETURN' then
    Result := KEYBOARD_RETURN
  else if name = 'BACKSPACE' then
    Result := KEYBOARD_BACKSPACE
  else if name = 'TAB' then
    Result := KEYBOARD_TAB
  else if name = 'SPACE' then
    Result := KEYBOARD_SPACE
  else if name = 'HOME' then
    Result := KEYBOARD_HOME
  else if name = 'END' then
    Result := KEYBOARD_END
  else if name = 'CAPSLOCK' then
    Result := KEYBOARD_CAPSLOCK
  else if name = '' then
    Result := KEYBOARD_AVPAG
  else if name = 'LCTRL' then
    Result := KEYBOARD_LCTRL
  else if name = 'LSHIFT' then
    Result := KEYBOARD_LSHIFT
  else if name = 'LALT' then
    Result := KEYBOARD_LALT
  else if name = 'LWIN' then
    Result := KEYBOARD_LWIN
  else if name = 'RCTRL' then
    Result := KEYBOARD_RCTRL
  else if name = 'RSHIFT' then
    Result := KEYBOARD_RSHIFT
  else if name = 'RALT' then
    Result := KEYBOARD_RALT
  else if name = 'RWIN' then
    Result := KEYBOARD_RWIN
  else if name = 'RIGHT' then
    Result := KEYBOARD_RIGHT
  else if name = 'LEFT' then
    Result := KEYBOARD_LEFT
  else if name = 'DOWN' then
    Result := KEYBOARD_DOWN
  else if name = 'UP' then
    Result := KEYBOARD_UP
    // else if name = '' then
    // Result := KEYBOARD_ROW0_T0
    // else if name = '' then
    // Result := KEYBOARD_ROW0_T1
    // else if name = '' then
    // Result := KEYBOARD_ROW0_T2
    // else if name = '' then
    // Result := KEYBOARD_ROW1_T1
    // else if name = '' then
    // Result := KEYBOARD_ROW1_T2
    // else if name = '' then
    // Result := KEYBOARD_ROW2_T1
    // else if name = '' then
    // Result := KEYBOARD_ROW2_T2
    // else if name = '' then
    // Result := KEYBOARD_ROW2_T3
    // else if name = '' then
    // Result := KEYBOARD_ROW3_T0
    // else if name = '' then
    // Result := KEYBOARD_ROW3_T1
    // else if name = '' then
    // Result := KEYBOARD_ROW3_T2
    // else if name = '' then
    // Result := KEYBOARD_ROW3_T3
  else if name = 'F1' then
    Result := KEYBOARD_F1
  else if name = 'F2' then
    Result := KEYBOARD_F2
  else if name = 'F3' then
    Result := KEYBOARD_F3
  else if name = 'F4' then
    Result := KEYBOARD_F4
  else if name = 'F5' then
    Result := KEYBOARD_F5
  else if name = 'F6' then
    Result := KEYBOARD_F6
  else if name = 'F7' then
    Result := KEYBOARD_F7
  else if name = 'F8' then
    Result := KEYBOARD_F8
  else if name = 'F9' then
    Result := KEYBOARD_F9
  else if name = 'F10' then
    Result := KEYBOARD_F10
  else if name = 'F11' then
    Result := KEYBOARD_F11
  else if name = 'F12' then
    Result := KEYBOARD_F12
  else if name = 'RETRUN' then
    Result := KEYBOARD_NRETURN
  else if name = 'NUM_1' then
    Result := KEYBOARD_N1
  else if name = 'NUM_2' then
    Result := KEYBOARD_N2
  else if name = 'NUM_3' then
    Result := KEYBOARD_N3
  else if name = 'NUM_4' then
    Result := KEYBOARD_N4
  else if name = 'NUM_5' then
    Result := KEYBOARD_N5
  else if name = 'NUM_6' then
    Result := KEYBOARD_N6
  else if name = 'NUM_7' then
    Result := KEYBOARD_N7
  else if name = 'NUM_8' then
    Result := KEYBOARD_N8
  else if name = 'NUM_9' then
    Result := KEYBOARD_N9
  else if name = 'NUM_0' then
    Result := KEYBOARD_N0
  else if name = '.' then
    Result := KEYBOARD_NDOT
  else if name = '' then
    Result := KEYBOARD_NONE;
end;

procedure set_key_control(player: byte; key: String; put_key: word);
begin
  if key = 'UP' then
    p_contrls.map_arcade.nup[player] := put_key
  else if key = 'DOWN' then
    p_contrls.map_arcade.ndown[player] := put_key
  else if key = 'LEFT' then
    p_contrls.map_arcade.nleft[player] := put_key
  else if key = 'RIGHT' then
    p_contrls.map_arcade.nright[player] := put_key
  else if key = 'BUTTON1' then
    p_contrls.map_arcade.nbut0[player] := put_key
  else if key = 'BUTTON2' then
    p_contrls.map_arcade.nbut1[player] := put_key
  else if key = 'BUTTON3' then
    p_contrls.map_arcade.nbut2[player] := put_key
  else if key = 'BUTTON4' then
    p_contrls.map_arcade.nbut3[player] := put_key
  else if key = 'BUTTON5' then
    p_contrls.map_arcade.nbut4[player] := put_key
  else if key = 'BUTTON6' then
    p_contrls.map_arcade.nbut5[player] := put_key
  else if key = 'COIN' then
    p_contrls.map_arcade.ncoin[player] := put_key
  else if key = 'START' then
    p_contrls.map_arcade.nstart[player] := put_key
end;

function convert_to_sdl2_key(num: byte): byte;
begin
  case num of
    13:
      Result := 40;
    16:
      Result := 225;
    17:
      Result := 224;
    18:
      Result := 226;
    27:
      Result := 41;
    35:
      Result := 77;
    36:
      Result := 74;
    37:
      Result := 80;
    38:
      Result := 82;
    39:
      Result := 79;
    40:
      Result := 81;
    41:
      Result := 79;
    91:
      Result := 227;
    112:
      Result := 58;
    113:
      Result := 59;
    114:
      Result := 60;
    115:
      Result := 61;
    116:
      Result := 62;
    117:
      Result := 63;
    118:
      Result := 64;
    119:
      Result := 65;
    120:
      Result := 66;
    121:
      Result := 67;
    122:
      Result := 68;
    123:
      Result := 69
  else
    Result := 255;
  end;
end;

function key_to_string(key: word): String;
begin
  case key of
    // vkLButton = $01; { 1 }
    // vkRButton = $02; { 2 }
    // vkCancel = $03; { 3 }
    // vkMButton = $04; { 4 }
    // vkXButton1 = $05; { 5 }
    // vkXButton2 = $06; { 6 }
    8:
      Result := 'BACKSPACE';
    9:
      Result := 'TAB';
    // vkLineFeed = $0A; { 10 }
    // vkClear = $0C; { 12 }
    13:
      Result := 'ENTER';
    // vkShift = $10; { 16 }
    // vkControl = $11; { 17 }
    // vkMenu = $12; { 18 }
    // vkPause = $13; { 19 }
    // vkCapital = $14; { 20 }
    // vkKana = $15; { 21 }
    // vkHangul = $15; { 21 }
    // vkJunja = $17; { 23 }
    // vkFinal = $18; { 24 }
    // vkHanja = $19; { 25 }
    // vkKanji = $19; { 25 }
    // vkConvert = $1C; { 28 }
    // vkNonConvert = $1D; { 29 }
    // vkAccept = $1E; { 30 }
    // vkModeChange = $1F; { 31 }
    27:
      Result := 'ESC';
    32:
      Result := 'SPACE';
    33:
      Result := 'PAGE_UP';
    34:
      Result := 'PAGE_DOWN';
    35:
      Result := 'END';
    36:
      Result := 'HOME';
    37:
      Result := 'LEFT';
    38:
      Result := 'UP';
    39:
      Result := 'RIGHT';
    40:
      Result := 'DOWN';
    // vkSelect = $29; { 41 }
    // vkPrint = $2A; { 42 }
    // vkExecute = $2B; { 43 }
    // vkSnapshot = $2C; { 44 }
    45:
      Result := 'INSERT';
    46:
      Result := 'DELETE';
    // vkHelp = $2F; { 47 }
    48:
      Result := '0';
    49:
      Result := '1';
    50:
      Result := '2';
    51:
      Result := '3';
    52:
      Result := '4';
    53:
      Result := '5';
    54:
      Result := '6';
    55:
      Result := '7';
    56:
      Result := '8';
    57:
      Result := '9';
    65:
      Result := 'A';
    66:
      Result := 'B';
    67:
      Result := 'C';
    68:
      Result := 'D';
    69:
      Result := 'E';
    70:
      Result := 'F';
    71:
      Result := 'G';
    72:
      Result := 'H';
    73:
      Result := 'I';
    74:
      Result := 'J';
    75:
      Result := 'K';
    76:
      Result := 'L';
    77:
      Result := 'M';
    78:
      Result := 'N';
    79:
      Result := 'O';
    80:
      Result := 'P';
    81:
      Result := 'Q';
    82:
      Result := 'R';
    83:
      Result := 'S';
    84:
      Result := 'T';
    85:
      Result := 'U';
    86:
      Result := 'V';
    87:
      Result := 'W';
    88:
      Result := 'X';
    89:
      Result := 'Y';
    90:
      Result := 'Z';
    91:
      Result := 'LWIN';
    92:
      Result := 'RWIN';
    // vkApps = $5D; { 93 }
    // vkSleep = $5F; { 95 }
    96:
      Result := 'Num_0';
    97:
      Result := 'Num_1';
    98:
      Result := 'Num_2';
    99:
      Result := 'Num_3';
    100:
      Result := 'Num_4';
    101:
      Result := 'Num_5';
    102:
      Result := 'Num_6';
    103:
      Result := 'Num_7';
    104:
      Result := 'Num_8';
    105:
      Result := 'Num_9';
    106:
      Result := 'Num_*';
    107:
      Result := 'Num_+';
    108:
      Result := 'Num_.';
    109:
      Result := 'Num_-';
    110:
      Result := 'Num_.';
    111:
      Result := 'Num_/';
    112:
      Result := 'F1';
    113:
      Result := 'F2';
    114:
      Result := 'F3';
    115:
      Result := 'F4';
    116:
      Result := 'F5';
    117:
      Result := 'F6';
    118:
      Result := 'F7';
    119:
      Result := 'F8';
    120:
      Result := 'F9';
    121:
      Result := 'F10';
    122:
      Result := 'F11';
    123:
      Result := 'F12';
    // vkF13 = $7C; { 124 }
    // vkF14 = $7D; { 125 }
    // vkF15 = $7E; { 126 }
    // vkF16 = $7F; { 127 }
    // vkF17 = $80; { 128 }
    // vkF18 = $81; { 129 }
    // vkF19 = $82; { 130 }
    // vkF20 = $83; { 131 }
    // vkF21 = $84; { 132 }
    // vkF22 = $85; { 133 }
    // vkF23 = $86; { 134 }
    // vkF24 = $87; { 135 }

    // vkCamera = $88; { 136 }
    // vkHardwareBack = $89; { 137 }

    // vkNumLock = $90; { 144 }
    // vkScroll = $91; { 145 }
    160:
      Result := 'LSHIFT';
    161:
      Result := 'RSHIFT';
    162:
      Result := 'LCONTROL';
    163:
      Result := 'RCONTROL';
    164:
      Result := 'MENU';
    165:
      Result := 'MENU';

    // vkBrowserBack = $A6; { 166 }
    // vkBrowserForward = $A7; { 167 }
    // vkBrowserRefresh = $A8; { 168 }
    // vkBrowserStop = $A9; { 169 }
    // vkBrowserSearch = $AA; { 170 }
    // vkBrowserFavorites = $AB; { 171 }
    // vkBrowserHome = $AC; { 172 }
    // vkVolumeMute = $AD; { 173 }
    // vkVolumeDown = $AE; { 174 }
    // vkVolumeUp = $AF; { 175 }
    // vkMediaNextTrack = $B0; { 176 }
    // vkMediaPrevTrack = $B1; { 177 }
    // vkMediaStop = $B2; { 178 }
    // vkMediaPlayPause = $B3; { 179 }
    // vkLaunchMail = $B4; { 180 }
    // vkLaunchMediaSelect = $B5; { 181 }
    // vkLaunchApp1 = $B6; { 182 }
    // vkLaunchApp2 = $B7; { 183 }

    186:
      Result := '`';
    187:
      Result := '=';
    188:
      Result := ',';
    189:
      Result := '-';
    // vkPeriod = $BE; { 190 }
    191:
      Result := '/';
    192:
      Result := '''';
    // vkTilde = $C0; { 192 }
    219:
      Result := '[';
    221:
      Result := ']';
    220:
      Result := '\';
    222:
      Result := '"';
    // vkPara = $DF; { 223 }

    // vkOem102 = $E2; { 226 }
    // vkIcoHelp = $E3; { 227 }
    // vkIco00 = $E4; { 228 }
    // vkProcessKey = $E5; { 229 }
    // vkIcoClear = $E6; { 230 }
    // vkPacket = $E7; { 231 }
    // vkAttn = $F6; { 246 }
    // vkCrsel = $F7; { 247 }
    // vkExsel = $F8; { 248 }
    // vkErEof = $F9; { 249 }
    // vkPlay = $FA; { 250 }
    // vkZoom = $FB; { 251 }
    // vkNoname = $FC; { 252 }
    // vkPA1 = $FD; { 253 }
    // vkOemClear = $FE; { 254 }
    255:
      Result := 'N\D';
  end;
end;


// Resources
procedure LoadImageFromResource(Image: TBitmap; const ResourceName: string);
var
  ResourceStream: TResourceStream;
begin
  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    Image.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;
end;

end.
