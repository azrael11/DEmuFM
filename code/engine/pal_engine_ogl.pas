unit pal_engine_ogl;

interface

uses
  dglOpenGL,
  math,
  Windows;

const
  MAX_COLORS = $8000;

type
  TColorRGBA = record
    r, g, b, a: byte;
  end;

  PFloatArray = ^TFloatArray;
  TFloatArray = array[0..MaxInt div SizeOf(Single) - 1] of Single;

  TPalette = array [0 .. MAX_COLORS] of TColorRGBA;

var
  Palette, BufferPalette: TPalette;
  Palette32, PaletteAlpha: array [0 .. MAX_COLORS] of GLuint;
  BufferColor: array [0 .. MAX_COLORS] of boolean;

procedure SetPalette(OpenGLPalette: TPalette; TotalColors: word);
procedure SetPaletteColor(Color: TColorRGBA; Position: word);
procedure SetPaletteColorAlpha(Color: TColorRGBA; Position: word);
function ConvertPaletteColor(Color: TColorRGBA): GLuint;
procedure ComputeResistorWeights(minval, maxval: integer; scaler: single; count_1: integer; resistances_1: pinteger; weights_1: psingle; pulldown_1, pullup_1: integer; count_2: integer; resistances_2: pinteger; weights_2: psingle; pulldown_2, pullup_2: integer; count_3: integer;
  resistances_3: pinteger; weights_3: psingle; pulldown_3, pullup_3: integer);
function Combine2Weights(tab: psingle; w0, w1: integer): integer;
function Combine3Weights(tab: psingle; w0, w1, w2: integer): integer;
function Combine4Weights(tab: psingle; w0, w1, w2, w3: integer): integer;
function Combine6Weights(tab: psingle; w0, w1, w2, w3, w4, w5: integer): integer;
function Pal1Bit(bits: byte): byte;
function Pal2Bit(bits: byte): byte;
function Pal3Bit(bits: byte): byte;
function Pal4Bit(bits: byte): byte;
function Pal4BitI(bits, i: byte): byte;
function Pal5Bit(bits: byte): byte;
function Pal6Bit(bits: byte): byte;

implementation

procedure ComputeResistorWeights(minval, maxval: integer; scaler: single; count_1: integer; resistances_1: pinteger; weights_1: psingle; pulldown_1, pullup_1: integer; count_2: integer; resistances_2: pinteger; weights_2: psingle; pulldown_2, pullup_2: integer; count_3: integer;
  resistances_3: pinteger; weights_3: psingle; pulldown_3, pullup_3: integer);
var
  i: integer;
  total_resistance, voltage: single;
begin
  for i := 0 to count_1 - 1 do
  begin
    total_resistance := 1 / ((1 / resistances_1^ + 1 / pulldown_1 + 1 / pullup_1));
    voltage := (maxval - minval) * (total_resistance / (total_resistance + pulldown_1)) + minval;
    weights_1^ := voltage * scaler;
    inc(weights_1);
    inc(resistances_1);
  end;
end;

procedure SetPalette(OpenGLPalette: TPalette; TotalColors: word);
var
  i: word;
begin
  for i := 0 to TotalColors - 1 do
    SetPaletteColor(OpenGLPalette[i], i);
end;

procedure SetPaletteColor(Color: TColorRGBA; Position: word);
var
  RGBA: array [0 .. 3] of byte;
  TextureID: GLuint;
begin
  RGBA[0] := Color.r;
  RGBA[1] := Color.g;
  RGBA[2] := Color.b;
  RGBA[3] := 255;

  glGenTextures(1, @TextureID);
  glBindTexture(GL_TEXTURE_2D, TextureID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @RGBA);
  Palette32[Position] := TextureID;
end;

procedure SetPaletteColorAlpha(Color: TColorRGBA; Position: word);
var
  RGBA: array [0 .. 3] of byte;
  TextureID: GLuint;
begin
  RGBA[0] := Color.r;
  RGBA[1] := Color.g;
  RGBA[2] := Color.b;
  RGBA[3] := 128;

  glGenTextures(1, @TextureID);
  glBindTexture(GL_TEXTURE_2D, TextureID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @RGBA);
  PaletteAlpha[Position] := TextureID;
end;

function ConvertPaletteColor(Color: TColorRGBA): GLuint;
var
  RGBA: array [0 .. 3] of byte;
  TextureID: GLuint;
begin
  RGBA[0] := Color.r;
  RGBA[1] := Color.g;
  RGBA[2] := Color.b;
  RGBA[3] := Color.a;

  glGenTextures(1, @TextureID);
  glBindTexture(GL_TEXTURE_2D, TextureID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @RGBA);
  Result := TextureID;
end;

function Pal1Bit(bits: byte): byte;
begin
  Result := $FF * (bits and 1);
end;

function Pal2Bit(bits: byte): byte;
begin
  bits := bits and 3;
  Result := (bits shl 6) or (bits shl 4) or (bits shl 2) or bits;
end;

function Pal3Bit(bits: byte): byte;
begin
  bits := bits and 7;
  Result := (bits shl 5) or (bits shl 2) or (bits shr 1);
end;

function Pal4Bit(bits: byte): byte;
begin
  bits := bits and $F;
  Result := (bits shl 4) or bits;
end;

function Pal4BitI(bits, i: byte): byte;
begin
  Result := (bits and $F) * (i + 1);
end;

function Pal5Bit(bits: byte): byte;
begin
  bits := bits and $1F;
  Result := (bits shl 3) or (bits shr 2);
end;

function Pal6Bit(bits: byte): byte;
begin
  bits := bits and $3F;
  Result := (bits shl 2) or (bits shr 4);
end;

function Combine2Weights(tab: psingle; w0, w1: integer): integer;
var
  res: single;
begin
  res := (PFloatArray(tab)^[0] * w0) + (PFloatArray(tab)^[1] * w1) + 0.5;
  Result := EnsureRange(Trunc(res), 0, 255);
end;

function Combine3Weights(tab: psingle; w0, w1, w2: integer): integer;
var
  res: single;
begin
  res := (PFloatArray(tab)^[0] * w0) + (PFloatArray(tab)^[1] * w1) + (PFloatArray(tab)^[2] * w2) + 0.5;
  Result := EnsureRange(Trunc(res), 0, 255);
end;

function Combine4Weights(tab: psingle; w0, w1, w2, w3: integer): integer;
var
  res: single;
begin
  res := (PFloatArray(tab)^[0] * w0) + (PFloatArray(tab)^[1] * w1) + (PFloatArray(tab)^[2] * w2) + (PFloatArray(tab)^[3] * w3) + 0.5;
  Result := EnsureRange(Trunc(res), 0, 255);
end;

function Combine6Weights(tab: psingle; w0, w1, w2, w3, w4, w5: integer): integer;
var
  res: single;
begin
  res := (PFloatArray(tab)^[0] * w0) + (PFloatArray(tab)^[1] * w1) + (PFloatArray(tab)^[2] * w2) + (PFloatArray(tab)^[3] * w3) + (PFloatArray(tab)^[4] * w4) + (PFloatArray(tab)^[5] * w5) + 0.5;
  Result := EnsureRange(Trunc(res), 0, 255);
end;

end.
