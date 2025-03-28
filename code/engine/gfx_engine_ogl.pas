unit gfx_engine_ogl;

interface

uses
  dglOpenGL, Windows;

const
  MAX_COLORS = $8000;
  MAX_PLANES = 8;
  MAX_GFX = 8;
  MAX_COLOR_BUFFER = $2000;

type
  TGfxData = record
    width, height: byte;
    elements: dword;
    buffer: array [0 .. $7FFF] of boolean;
    transparency: array [0 .. $1F] of boolean;
    shadow: array [0 .. $1F] of boolean;
    alpha: array [0 .. $1F] of byte;
    alt_transparency: array [0 .. 4, 0 .. $1F] of boolean;
    colors: array [0 .. MAX_COLORS - 1] of word;
    textureID: GLuint;
    data: Pointer;
  end;

  TGfxDesc = record
    pos_planes: array [0 .. MAX_PLANES - 1] of dword;
    bit_pixel: byte;
    sprite_length: dword;
    banks: byte;
  end;

type
  TGFX_ENGINE_OPENGL = class
  private

  public
    gfx: array [0 .. 255] of TGfxData;
    des_gfx: TGfxDesc;
    punbuf: Pointer;
    buffer_sprites: array [0 .. $1FFF] of byte;
    buffer_sprites_w: array [0 .. $1FFF] of byte;
    buffer_color: array [0 .. MAX_COLOR_BUFFER - 1] of boolean;

    constructor create;
    destructor destroy;

    procedure InitGfxOpenGL(index, width, height: byte; num_elements: dword);
    procedure ConvertGfxOpenGL(num_gfx: byte; increment: dword; SpriteRom: pbyte; cx, cy: pdword; rot90, rol90: boolean; invert: boolean = false);
    procedure ConvertGfxSingleOpenGL(num_gfx: byte; increment: dword; SpriteRom: pbyte; cx, cy: pdword; rot90, rol90: boolean; n: dword);
    procedure GfxSetDescData(bits_pixel, banks: byte; size, p0: dword; p1: dword = 0; p2: dword = 0; p3: dword = 0; p4: dword = 0; p5: dword = 0; p6: dword = 0; p7: dword = 0);
    // gfx put
    procedure PutGfxOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx: byte);
    procedure PutGfxMaskOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx, trans, mask: word);
    procedure PutGfxMaskFlipOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx, trans, mask: byte; flipx, flipy: boolean);
    procedure PutGfxTransOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx: byte);
    procedure PutGfxTransAltOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx, index: byte);
    procedure PutGfxBlockTransOpenGL(pos_x, pos_y: word; gscreen, size_x, size_y: byte);
    procedure PutGfxBlockOpenGL(pos_x, pos_y: word; gscreen, size_x, size_y: byte; color: word);
    procedure PutGfxFlipOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx: byte; flipx, flipy: boolean);
    procedure PutGfxTransFlipOpenGL(pos_x, pos_y, nchar: dword; color: word; gscreen, ngfx: byte; flipx, flipy: boolean);
    procedure PutGfxTransFlipAltOpenGL(pos_x, pos_y, nchar: dword; color: word; gscreen, ngfx: byte; flipx, flipy: boolean; trans_index: byte);
    // sprites put
    procedure PutGfxSpriteOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte);
    procedure PutGfxSpriteDiffOpenGL(nchar, color: word; flipx, flipy: boolean; ngfx: byte; x_diff, y_diff: word);
    procedure PutGfxSpriteMaskOpenGL(nchar, color: word; flipx, flipy: boolean; ngfx: byte; trans, mask: word);
    procedure PutGfxSpriteMaskDiffOpenGL(nchar, color: word; flipx, flipy: boolean; ngfx, trans, mask, x_diff, y_diff: byte);
    procedure PutGfxSpriteZoomOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte; zx, zy: single);
    procedure PutGfxSpriteShadowOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte; shadow_color: word);
    procedure PutGfxSpriteZoomAlphaOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte; zx, zy: single);
    procedure PutGfxSpriteAlphaOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte);
    // sprites update
    procedure UpdateGfxSpriteOpenGL(pos_x, pos_y: word; dest, ngfx: byte);
    procedure UpdateGfxSpriteLineOpenGL(pos_x, pos_y: word; dest, ngfx, line: byte);
    procedure UpdateGfxSpriteSizeOpenGL(pos_x, pos_y: word; dest: byte; x_size, y_size: word; ipos_x: word = 0; ipos_y: word = 0);
    procedure UpdateGfxSpriteZoomOpenGL(pos_x, pos_y: word; dest, ngfx: byte; zx, zy: single);
    procedure UpdateGfxSpriteAlphaOpenGL(pos_x, pos_y: word; dest, ngfx: byte);
    procedure UpdateGfxSpriteZoomAlphaOpenGL(pos_x, pos_y: word; dest, ngfx: byte; zx, zy: single);
    // scroll
    procedure ScrollXYOpenGL(porigen, pdestino: byte; scroll_x, scroll_y: word; diff_x: word = 0; diff_y: word = 0; adj_x: word = 0; adj_y: word = 0);
    procedure ScrollXOpenGL(porigen, pdestino: byte; scroll_x: word);
    procedure ScrollXPartOpenGL(porigen, pdestino: byte; scroll_x, scroll_y: word; orgy, sizey: word);
    procedure ScrollXPart2OpenGL(porigen, pdestino: byte; long_bloque_y: word; posicion_x: pword; scroll_x: word = 0; scroll_y: word = 0; inc_y: word = 0);
    procedure ScrollYOpenGL(porigen, pdestino: byte; scroll_y: word);
    procedure ScrollYPart2OpenGL(porigen, pdestino: byte; long_bloque_x: word; posicion_y: pword; scroll_x: word = 0; scroll_y: word = 0);
    procedure ScrollXYPartOpenGL(porigen, pdestino: byte; long_bloque_x, long_bloque_y: word; posicion_x, posicion_y: pword; scroll_x, scroll_y: word);
    // basic draw functions
    procedure PutPixelOpenGL(x, y: word; cantidad: dword; pixel: pword; sitio: byte);
    function GetPixelOpenGL(x, y: word; sitio: byte): word;
    procedure PutPixelAlphaOpenGL(x, y: word; cantidad: dword; pixel: pdword; sitio: byte);
    procedure SingleLineOpenGL(x, y, color, longitud: word; pant: byte);
    procedure DrawLineOpenGL(x0, y0, x1, y1: integer; color: word; pant: byte);
    // gscreen functions
    procedure FillFullScreenOpenGL(vgscreen: byte; color: word);
    procedure PutPixelGfxIntOpenGL(x, y, cantidad: word; sitio: byte);
    // misc
    procedure FillWordOpenGL(dest: pword; cantidad: cardinal; valor: word);
    procedure ResetVideoOpenGL;

  end;
var
  engine_gfx_opengl : TGFX_ENGINE_OPENGL;

implementation

uses
  vars_hide;

{ TGFX_ENGINE_OPENGL }

procedure TGFX_ENGINE_OPENGL.ConvertGfxOpenGL(num_gfx: byte; increment: dword; SpriteRom: pbyte; cx, cy: pdword; rot90, rol90, invert: boolean);
var
  n, elements, spriteIndex, index: dword;
  octet, bit, row, col, pixelBit: byte;
  temp_cx, temp_cy: pdword;
  changeOrientation: boolean;
  textureData: pbyte;
begin
  changeOrientation := false;
  if ((rot90 or rol90) and (increment <> 0)) then
  begin
    gfx[num_gfx].width := gfx[num_gfx].width xor gfx[num_gfx].height;
    gfx[num_gfx].height := gfx[num_gfx].width xor gfx[num_gfx].height;
    gfx[num_gfx].width := gfx[num_gfx].width xor gfx[num_gfx].height;
    changeOrientation := true;
  end;

  temp_cx := cx;
  temp_cy := cy;
  elements := gfx[num_gfx].elements;
  index := 0;

  // Allocate correct memory size for texture data
  GetMem(textureData, elements * gfx[num_gfx].width * gfx[num_gfx].height);

  for n := 0 to elements - 1 do
  begin
    spriteIndex := n * gfx[num_gfx].width * gfx[num_gfx].height;
    cy := temp_cy;
    for row := 0 to (gfx[num_gfx].height - 1) do
    begin
      cx := temp_cx;
      for col := 0 to (gfx[num_gfx].width - 1) do
      begin
        octet := 0;
        for pixelBit := 0 to 7 do
        begin
          bit := (SpriteRom[spriteIndex + cy^ + cx^] shr pixelBit) and 1;
          if invert then
            bit := not bit and 1;
          octet := octet or (bit shl (7 - pixelBit));
        end;
        textureData[index] := octet; // Use index without increment errors
        inc(index);
        inc(cx);
      end;
      inc(cy);
    end;
  end;

  glBindTexture(GL_TEXTURE_2D, gfx[num_gfx].textureID);

  // Properly update or initialize texture
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, gfx[num_gfx].width, gfx[num_gfx].height, 0, GL_RED, GL_UNSIGNED_BYTE, textureData);

  FreeMem(textureData);

  if changeOrientation then
  begin
    gfx[num_gfx].width := gfx[num_gfx].width xor gfx[num_gfx].height;
    gfx[num_gfx].height := gfx[num_gfx].width xor gfx[num_gfx].height;
    gfx[num_gfx].width := gfx[num_gfx].width xor gfx[num_gfx].height;
  end;
end;

procedure TGFX_ENGINE_OPENGL.ConvertGfxSingleOpenGL(num_gfx: byte; increment: dword; SpriteRom: pbyte; cx, cy: pdword; rot90, rol90: boolean; n: dword);
var
  octet, bit, row, col, pixelBit: byte;
  spriteIndex, index: dword;
  temp_cx: pdword;
  textureData: pbyte;
begin
  temp_cx := cx;
  index := n * gfx[num_gfx].height * gfx[num_gfx].width;
  spriteIndex := n * gfx[num_gfx].width * gfx[num_gfx].height;

  GetMem(textureData, gfx[num_gfx].width * gfx[num_gfx].height);

  for row := 0 to (gfx[num_gfx].height - 1) do
  begin
    cx := temp_cx;
    for col := 0 to (gfx[num_gfx].width - 1) do
    begin
      octet := 0;
      for pixelBit := 0 to 7 do
      begin
        bit := (SpriteRom[spriteIndex + cy^ + cx^] shr pixelBit) and 1;
        octet := octet or (bit shl (7 - pixelBit));
      end;
      textureData[index + increment] := octet;
      inc(index);
      inc(cx);
    end;
    inc(cy);
  end;

  glBindTexture(GL_TEXTURE_2D, gfx[num_gfx].textureID);
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, gfx[num_gfx].width, gfx[num_gfx].height, GL_RED, GL_UNSIGNED_BYTE, textureData);
  FreeMem(textureData);

  if rot90 then
  begin
    // Implement rotation 90 degrees right here
  end;
  if rol90 then
  begin
    // Implement rotation 90 degrees left here
  end;
end;

constructor TGFX_ENGINE_OPENGL.create;
begin

end;

destructor TGFX_ENGINE_OPENGL.destroy;
begin

end;

procedure TGFX_ENGINE_OPENGL.DrawLineOpenGL(x0, y0, x1, y1: integer; color: word; pant: byte);
begin
  glBindTexture(GL_TEXTURE_2D, gfx[pant].textureID);
  glColor3ub(color shr 8, color shr 4, color and $FF);
  glBegin(GL_LINES);
  glVertex2i(x0, y0);
  glVertex2i(x1, y1);
  glEnd;
  glFlush;
end;

procedure TGFX_ENGINE_OPENGL.FillFullScreenOpenGL(vgscreen: byte; color: word);
begin
  glBindTexture(GL_TEXTURE_2D, gfx[vgscreen].textureID);
  glClearColor((color shr 11) / 31.0, ((color shr 5) and $3F) / 63.0, (color and $1F) / 31.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TGFX_ENGINE_OPENGL.FillWordOpenGL(dest: pword; cantidad: cardinal; valor: word);
var
  f: cardinal;
begin
  if cantidad = 0 then
    Exit;
  for f := 1 to cantidad do
  begin
    dest^ := valor;
    inc(dest);
  end;
end;

function TGFX_ENGINE_OPENGL.GetPixelOpenGL(x, y: word; sitio: byte): word;
var
  pixelData: array [0 .. 0] of word;
begin
  glBindTexture(GL_TEXTURE_2D, gfx[sitio].textureID);
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_SHORT, @pixelData);
  Result := pixelData[0];
end;

procedure TGFX_ENGINE_OPENGL.GfxSetDescData(bits_pixel, banks: byte; size, p0, p1, p2, p3, p4, p5, p6, p7: dword);
begin
  des_gfx.pos_planes[0] := p0;
  des_gfx.pos_planes[1] := p1;
  des_gfx.pos_planes[2] := p2;
  des_gfx.pos_planes[3] := p3;
  des_gfx.pos_planes[4] := p4;
  des_gfx.pos_planes[5] := p5;
  des_gfx.pos_planes[6] := p6;
  des_gfx.pos_planes[7] := p7;
  des_gfx.bit_pixel := bits_pixel;
  des_gfx.sprite_length := size;
  des_gfx.banks := banks;
end;

procedure TGFX_ENGINE_OPENGL.InitGfxOpenGL(index, width, height: byte; num_elements: dword);
var
  i: word;
begin
  gfx[index].width := width;
  gfx[index].height := height;
  gfx[index].elements := num_elements;

  FillChar(gfx[index].buffer, SizeOf(gfx[index].buffer), 1);
  FillChar(gfx[index].transparency, SizeOf(gfx[index].transparency), 0);
  FillChar(gfx[index].shadow, SizeOf(gfx[index].shadow), 0);
  FillChar(gfx[index].alpha, SizeOf(gfx[index].alpha), 0);

  for i := 0 to 4 do
    FillChar(gfx[index].alt_transparency[i], SizeOf(gfx[index].alt_transparency[i]), 0);

  for i := 0 to MAX_COLORS - 1 do
    gfx[index].colors[i] := i;

  GetMem(gfx[index].data, num_elements * width * height);

  // OpenGL texture initialization
  glGenTextures(1, @gfx[index].textureID);
  glBindTexture(GL_TEXTURE_2D, gfx[index].textureID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxBlockOpenGL(pos_x, pos_y: word; gscreen, size_x, size_y: byte; color: word);
var
  y: byte;
  r, g, b: GLfloat;
begin
  r := ((color shr 16) and $FF) / 255.0;
  g := ((color shr 8) and $FF) / 255.0;
  b := (color and $FF) / 255.0;

  glDisable(GL_TEXTURE_2D);
  glColor3f(r, g, b);

  for y := 0 to size_y - 1 do
  begin
    glBegin(GL_QUADS);
    glVertex2f(pos_x, pos_y + y);
    glVertex2f(pos_x + size_x, pos_y + y);
    glVertex2f(pos_x + size_x, pos_y + y + 1);
    glVertex2f(pos_x, pos_y + y + 1);
    glEnd;
  end;

  glEnable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxBlockTransOpenGL(pos_x, pos_y: word; gscreen, size_x, size_y: byte);
var
  y: byte;
begin
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4f(0.0, 0.0, 0.0, 0.0);

  for y := 0 to size_y - 1 do
  begin
    glBegin(GL_QUADS);
    glVertex2f(pos_x, pos_y + y);
    glVertex2f(pos_x + size_x, pos_y + y);
    glVertex2f(pos_x + size_x, pos_y + y + 1);
    glVertex2f(pos_x, pos_y + y + 1);
    glEnd;
  end;

  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxFlipOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx: byte; flipx, flipy: boolean);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(pos_x + gfx[ngfx].width / 2, pos_y + gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-(pos_x + gfx[ngfx].width / 2), -(pos_y + gfx[ngfx].height / 2), 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(pos_x, pos_y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxMaskFlipOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx, trans, mask: byte; flipx, flipy: boolean);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, trans / 255.0);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(pos_x + gfx[ngfx].width / 2, pos_y + gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-(pos_x + gfx[ngfx].width / 2), -(pos_y + gfx[ngfx].height / 2), 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(pos_x, pos_y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxMaskOpenGL(pos_x, pos_y, nchar, color, gscreen, ngfx, trans, mask: word);
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, trans / 255.0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(pos_x, pos_y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  glEnd;

  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx: byte);
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(pos_x, pos_y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  glEnd;

  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteAlphaOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(gfx[ngfx].width / 2, gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-gfx[ngfx].width / 2, -gfx[ngfx].height / 2, 0);

  glBegin(GL_QUADS);
  if gfx[ngfx].alpha[nchar] = 1 then
    glColor4ubv(@gfx[ngfx].colors[nchar + color])
  else
    glColor4ubv(@gfx[ngfx].colors[nchar + color]);

  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(gfx[ngfx].width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(gfx[ngfx].width, gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteDiffOpenGL(nchar, color: word; flipx, flipy: boolean; ngfx: byte; x_diff, y_diff: word);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(x_diff + gfx[ngfx].width / 2, y_diff + gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-(x_diff + gfx[ngfx].width / 2), -(y_diff + gfx[ngfx].height / 2), 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(x_diff, y_diff);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(x_diff + gfx[ngfx].width, y_diff);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(x_diff + gfx[ngfx].width, y_diff + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(x_diff, y_diff + gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteMaskDiffOpenGL(nchar, color: word; flipx, flipy: boolean; ngfx, trans, mask, x_diff, y_diff: byte);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(x_diff + gfx[ngfx].width / 2, y_diff + gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-(x_diff + gfx[ngfx].width / 2), -(y_diff + gfx[ngfx].height / 2), 0);

  glBegin(GL_QUADS);
  if (color and mask) <> trans then
  begin
    glTexCoord2f(0.0, 0.0);
    glVertex2f(x_diff, y_diff);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(x_diff + gfx[ngfx].width, y_diff);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(x_diff + gfx[ngfx].width, y_diff + gfx[ngfx].height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(x_diff, y_diff + gfx[ngfx].height);
  end;
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteMaskOpenGL(nchar, color: word; flipx, flipy: boolean; ngfx: byte; trans, mask: word);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(gfx[ngfx].width / 2, gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-gfx[ngfx].width / 2, -gfx[ngfx].height / 2, 0);

  glBegin(GL_QUADS);
  if (color and mask) <> trans then
  begin
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(gfx[ngfx].width, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(gfx[ngfx].width, gfx[ngfx].height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(0, gfx[ngfx].height);
  end;
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(gfx[ngfx].width / 2, gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-gfx[ngfx].width / 2, -gfx[ngfx].height / 2, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(gfx[ngfx].width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(gfx[ngfx].width, gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteShadowOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte; shadow_color: word);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(gfx[ngfx].width / 2, gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-gfx[ngfx].width / 2, -gfx[ngfx].height / 2, 0);

  glBegin(GL_QUADS);
  if gfx[ngfx].shadow[nchar] then
    glColor4ubv(@shadow_color)
  else
    glColor4ubv(@gfx[ngfx].colors[nchar + color]);

  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(gfx[ngfx].width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(gfx[ngfx].width, gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteZoomAlphaOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte; zx, zy: single);
var
  scaleX, scaleY, offsetX, offsetY: GLfloat;
begin
  if (zx <= 0) or (zy <= 0) then
    Exit;

  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := zx;
  scaleY := zy;

  if flipx then
    scaleX := -scaleX;
  if flipy then
    scaleY := -scaleY;

  offsetX := gfx[ngfx].width * zx / 2;
  offsetY := gfx[ngfx].height * zy / 2;

  glPushMatrix;
  glTranslatef(offsetX, offsetY, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-offsetX, -offsetY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(gfx[ngfx].width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(gfx[ngfx].width, gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxSpriteZoomOpenGL(nchar: dword; color: word; flipx, flipy: boolean; ngfx: byte; zx, zy: single);
var
  scaleX, scaleY, offsetX, offsetY: GLfloat;
begin
  if (zx <= 0) or (zy <= 0) then
    Exit;

  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := zx;
  scaleY := zy;

  if flipx then
    scaleX := -scaleX;
  if flipy then
    scaleY := -scaleY;

  offsetX := gfx[ngfx].width * zx / 2;
  offsetY := gfx[ngfx].height * zy / 2;

  glPushMatrix;
  glTranslatef(offsetX, offsetY, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-offsetX, -offsetY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(gfx[ngfx].width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(gfx[ngfx].width, gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxTransAltOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx, index: byte);
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glBegin(GL_QUADS);
  if not gfx[ngfx].alt_transparency[index][nchar] then
  begin
    glTexCoord2f(0.0, 0.0);
    glVertex2f(pos_x, pos_y);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(pos_x + gfx[ngfx].width, pos_y);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  end;
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxTransFlipAltOpenGL(pos_x, pos_y, nchar: dword; color: word; gscreen, ngfx: byte; flipx, flipy: boolean; trans_index: byte);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(pos_x + gfx[ngfx].width / 2, pos_y + gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-(pos_x + gfx[ngfx].width / 2), -(pos_y + gfx[ngfx].height / 2), 0);

  glBegin(GL_QUADS);
  if not gfx[ngfx].alt_transparency[trans_index][nchar] then
  begin
    glTexCoord2f(0.0, 0.0);
    glVertex2f(pos_x, pos_y);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(pos_x + gfx[ngfx].width, pos_y);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  end;
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxTransFlipOpenGL(pos_x, pos_y, nchar: dword; color: word; gscreen, ngfx: byte; flipx, flipy: boolean);
var
  scaleX, scaleY: GLfloat;
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  scaleX := 1.0;
  scaleY := 1.0;

  if flipx then
    scaleX := -1.0;
  if flipy then
    scaleY := -1.0;

  glPushMatrix;
  glTranslatef(pos_x + gfx[ngfx].width / 2, pos_y + gfx[ngfx].height / 2, 0);
  glScalef(scaleX, scaleY, 1.0);
  glTranslatef(-(pos_x + gfx[ngfx].width / 2), -(pos_y + gfx[ngfx].height / 2), 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(pos_x, pos_y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  glEnd;

  glPopMatrix;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutGfxTransOpenGL(pos_x, pos_y, nchar, color: word; gscreen, ngfx: byte);
begin
  nchar := nchar mod gfx[ngfx].elements;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(pos_x, pos_y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(pos_x + gfx[ngfx].width, pos_y + gfx[ngfx].height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(pos_x, pos_y + gfx[ngfx].height);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.PutPixelAlphaOpenGL(x, y: word; cantidad: dword; pixel: pdword; sitio: byte);
begin
  glBindTexture(GL_TEXTURE_2D, gfx[sitio].textureID);
  glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, cantidad, 1, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, pixel);
end;

procedure TGFX_ENGINE_OPENGL.PutPixelGfxIntOpenGL(x, y, cantidad: word; sitio: byte);
begin
  glBindTexture(GL_TEXTURE_2D, gfx[sitio].textureID);
  glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, cantidad, 1, GL_RGBA, GL_UNSIGNED_SHORT, punbuf);
end;

procedure TGFX_ENGINE_OPENGL.PutPixelOpenGL(x, y: word; cantidad: dword; pixel: pword; sitio: byte);
begin
  glBindTexture(GL_TEXTURE_2D, gfx[sitio].textureID);
  glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, cantidad, 1, GL_RGBA, GL_UNSIGNED_SHORT, pixel);
end;

procedure TGFX_ENGINE_OPENGL.ResetVideoOpenGL;
var
  f: byte;
begin
  for f := 0 to MAX_GFX - 1 do
    FillChar(gfx[f].buffer, $8000, 1);
  FillChar(buffer_sprites, $2000, 0);
  FillChar(buffer_sprites_w, $2000, 0);
  FillChar(buffer_color, MAX_COLOR_BUFFER, 1);
end;

procedure TGFX_ENGINE_OPENGL.ScrollXOpenGL(porigen, pdestino: byte; scroll_x: word);
var
  long_x, long_x2, long_y: word;
begin
  long_y := p_final[porigen].scroll.max_y;
  scroll_x := scroll_x and p_final[porigen].scroll.mask_x;
  if ((scroll_x + p_final[porigen].scroll.max_x) > p_final[porigen].scroll.long_x) then
  begin
    long_x := p_final[porigen].scroll.long_x - scroll_x;
    long_x2 := p_final[porigen].scroll.max_x - long_x;
  end
  else
  begin
    long_x := p_final[porigen].scroll.max_x;
  end;

  glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
  glEnable(GL_TEXTURE_2D);

  glPushMatrix;
  glTranslatef(0, 0, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(scroll_x / gfx[porigen].width, 0);
  glVertex2f(0, 0);
  glTexCoord2f((scroll_x + long_x) / gfx[porigen].width, 0);
  glVertex2f(long_x, 0);
  glTexCoord2f((scroll_x + long_x) / gfx[porigen].width, long_y / gfx[porigen].height);
  glVertex2f(long_x, long_y);
  glTexCoord2f(scroll_x / gfx[porigen].width, long_y / gfx[porigen].height);
  glVertex2f(0, long_y);
  glEnd;

  glPopMatrix;
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.ScrollXPart2OpenGL(porigen, pdestino: byte; long_bloque_y: word; posicion_x: pword; scroll_x, scroll_y, inc_y: word);
var
  pos_y: word;
  temp_pos_x: pword;
  posicion_x_def, posicion_y_def, size_of_y, long_x, long_x2: word;
begin
  temp_pos_x := posicion_x;
  pos_y := inc_y;
  while (pos_y < p_final[porigen].scroll.max_x) do
  begin
    posicion_x_def := (temp_pos_x^ + scroll_x) and p_final[porigen].scroll.mask_x;
    posicion_y_def := (pos_y + scroll_y) and p_final[porigen].scroll.mask_y;
    if ((posicion_y_def + long_bloque_y) > p_final[porigen].scroll.max_y) then
      size_of_y := p_final[porigen].scroll.max_y - posicion_y_def
    else
      size_of_y := long_bloque_y;
    if ((posicion_x_def + p_final[porigen].scroll.max_x) > p_final[porigen].scroll.long_x) then
    begin
      long_x := p_final[porigen].scroll.long_x - posicion_x_def;
      long_x2 := p_final[porigen].scroll.max_x - long_x;
      glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
      glEnable(GL_TEXTURE_2D);
      glPushMatrix;
      glTranslatef(long_x, pos_y, 0);
      glBegin(GL_QUADS);
      glTexCoord2f(0, posicion_y_def / gfx[porigen].height);
      glVertex2f(0, posicion_y_def);
      glTexCoord2f(long_x2 / gfx[porigen].width, posicion_y_def / gfx[porigen].height);
      glVertex2f(long_x2, posicion_y_def);
      glTexCoord2f(long_x2 / gfx[porigen].width, (posicion_y_def + size_of_y) / gfx[porigen].height);
      glVertex2f(long_x2, posicion_y_def + size_of_y);
      glTexCoord2f(0, (posicion_y_def + size_of_y) / gfx[porigen].height);
      glVertex2f(0, posicion_y_def + size_of_y);
      glEnd;
      glPopMatrix;
      glDisable(GL_TEXTURE_2D);
    end;
    pos_y := pos_y + long_bloque_y;
    inc(temp_pos_x);
  end;
end;

procedure TGFX_ENGINE_OPENGL.ScrollXPartOpenGL(porigen, pdestino: byte; scroll_x, scroll_y, orgy, sizey: word);
var
  long_x, long_x2, scroll_y2: word;
begin
  scroll_x := scroll_x and p_final[porigen].scroll.mask_x;
  scroll_y := (p_final[porigen].scroll.long_y - scroll_y) and p_final[porigen].scroll.mask_y;
  scroll_y2 := scroll_y + orgy;
  if (scroll_y2 > p_final[porigen].scroll.long_y) then
    scroll_y2 := scroll_y2 - p_final[porigen].scroll.max_y;
  if ((scroll_x + p_final[porigen].scroll.max_x) >= p_final[porigen].scroll.long_x) then
  begin
    long_x := p_final[porigen].scroll.long_x - scroll_x;
    long_x2 := p_final[porigen].scroll.max_x - long_x;
    glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
    glEnable(GL_TEXTURE_2D);
    glPushMatrix;
    glTranslatef(long_x, scroll_y2, 0);
    glBegin(GL_QUADS);
    glTexCoord2f(0, orgy / gfx[porigen].height);
    glVertex2f(0, orgy);
    glTexCoord2f(long_x2 / gfx[porigen].width, orgy / gfx[porigen].height);
    glVertex2f(long_x2, orgy);
    glTexCoord2f(long_x2 / gfx[porigen].width, (orgy + sizey) / gfx[porigen].height);
    glVertex2f(long_x2, orgy + sizey);
    glTexCoord2f(0, (orgy + sizey) / gfx[porigen].height);
    glVertex2f(0, orgy + sizey);
    glEnd;
    glPopMatrix;
    glDisable(GL_TEXTURE_2D);
  end
  else
  begin
    long_x := p_final[porigen].scroll.max_x;
  end;

  glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
  glEnable(GL_TEXTURE_2D);
  glPushMatrix;
  glTranslatef(0, scroll_y2, 0);
  glBegin(GL_QUADS);
  glTexCoord2f(scroll_x / gfx[porigen].width, orgy / gfx[porigen].height);
  glVertex2f(0, orgy);
  glTexCoord2f((scroll_x + long_x) / gfx[porigen].width, orgy / gfx[porigen].height);
  glVertex2f(long_x, orgy);
  glTexCoord2f((scroll_x + long_x) / gfx[porigen].width, (orgy + sizey) / gfx[porigen].height);
  glVertex2f(long_x, orgy + sizey);
  glTexCoord2f(scroll_x / gfx[porigen].width, (orgy + sizey) / gfx[porigen].height);
  glVertex2f(0, orgy + sizey);
  glEnd;
  glPopMatrix;
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.ScrollXYOpenGL(porigen, pdestino: byte; scroll_x, scroll_y, diff_x, diff_y, adj_x, adj_y: word);
var
  long_x, long_y, long_x2, long_y2: word;
begin
  scroll_x := scroll_x and p_final[porigen].scroll.mask_x;
  scroll_y := scroll_y and p_final[porigen].scroll.mask_y;
  if ((scroll_x + p_final[porigen].scroll.max_x) > p_final[porigen].scroll.long_x) then
    long_x := p_final[porigen].scroll.long_x - scroll_x
  else
    long_x := p_final[porigen].scroll.max_x;
  if ((scroll_y + p_final[porigen].scroll.max_y) > p_final[porigen].scroll.long_y) then
    long_y := p_final[porigen].scroll.long_y - scroll_y
  else
    long_y := p_final[porigen].scroll.max_y;
  long_x2 := p_final[porigen].scroll.max_x - long_x;
  long_y2 := p_final[porigen].scroll.max_y - long_y;

  glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
  glEnable(GL_TEXTURE_2D);

  glPushMatrix;
  glTranslatef(diff_x, diff_y, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(scroll_x / gfx[porigen].width, scroll_y / gfx[porigen].height);
  glVertex2f(0, 0);
  glTexCoord2f((scroll_x + long_x) / gfx[porigen].width, scroll_y / gfx[porigen].height);
  glVertex2f(long_x, 0);
  glTexCoord2f((scroll_x + long_x) / gfx[porigen].width, (scroll_y + long_y) / gfx[porigen].height);
  glVertex2f(long_x, long_y);
  glTexCoord2f(scroll_x / gfx[porigen].width, (scroll_y + long_y) / gfx[porigen].height);
  glVertex2f(0, long_y);
  glEnd;

  glPopMatrix;
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.ScrollXYPartOpenGL(porigen, pdestino: byte; long_bloque_x, long_bloque_y: word; posicion_x, posicion_y: pword; scroll_x, scroll_y: word);
var
  pos_y, pos_x: word;
  temp_pos_x: pword;
  posicion_x_def, posicion_y_def: word;
  long_def_x, long_def_y: word;
begin
  pos_y := 0;
  while (pos_y < p_final[porigen].scroll.max_y) do
  begin
    temp_pos_x := posicion_x;
    pos_x := 0;
    while (pos_x < p_final[porigen].scroll.max_x) do
    begin
      posicion_x_def := (temp_pos_x^ + pos_y + scroll_x) and p_final[porigen].scroll.mask_x;
      posicion_y_def := (posicion_y^ + pos_x + scroll_y) and p_final[porigen].scroll.mask_y;
      if (posicion_y_def + long_bloque_x) > p_final[porigen].scroll.mask_y then
        long_def_y := p_final[porigen].scroll.mask_y - posicion_y_def
      else
        long_def_y := long_bloque_x;
      if (posicion_x_def + long_bloque_y) > p_final[porigen].scroll.mask_x then
        long_def_x := p_final[porigen].scroll.mask_x - posicion_x_def
      else
        long_def_x := long_bloque_y;
      glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
      glEnable(GL_TEXTURE_2D);
      glPushMatrix;
      glTranslatef(posicion_x_def, posicion_y_def, 0);
      glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex2f(0, 0);
      glTexCoord2f(long_def_x / gfx[porigen].width, 0);
      glVertex2f(long_def_x, 0);
      glTexCoord2f(long_def_x / gfx[porigen].width, long_def_y / gfx[porigen].height);
      glVertex2f(long_def_x, long_def_y);
      glTexCoord2f(0, long_def_y / gfx[porigen].height);
      glVertex2f(0, long_def_y);
      glEnd;
      glPopMatrix;
      glDisable(GL_TEXTURE_2D);
      pos_x := pos_x + long_bloque_x;
      inc(temp_pos_x);
    end;
    inc(posicion_y);
    pos_y := pos_y + long_bloque_y;
  end;
end;

procedure TGFX_ENGINE_OPENGL.ScrollYOpenGL(porigen, pdestino: byte; scroll_y: word);
var
  long_x, long_y, long_y2: word;
begin
  long_x := p_final[porigen].scroll.max_x;
  scroll_y := scroll_y and p_final[porigen].scroll.mask_y;
  if ((scroll_y + p_final[porigen].scroll.max_y) > p_final[porigen].scroll.long_y) then
  begin
    long_y := p_final[porigen].scroll.long_y - scroll_y;
    long_y2 := p_final[porigen].scroll.max_y - long_y;
    glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
    glEnable(GL_TEXTURE_2D);
    glPushMatrix;
    glTranslatef(0, long_y, 0);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(0, 0);
    glTexCoord2f(long_x / gfx[porigen].width, 0);
    glVertex2f(long_x, 0);
    glTexCoord2f(long_x / gfx[porigen].width, long_y2 / gfx[porigen].height);
    glVertex2f(long_x, long_y2);
    glTexCoord2f(0, long_y2 / gfx[porigen].height);
    glVertex2f(0, long_y2);
    glEnd;
    glPopMatrix;
    glDisable(GL_TEXTURE_2D);
  end
  else
  begin
    long_y := p_final[porigen].scroll.max_y;
  end;

  glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
  glEnable(GL_TEXTURE_2D);
  glPushMatrix;
  glTranslatef(0, scroll_y, 0);
  glBegin(GL_QUADS);
  glTexCoord2f(0, 0);
  glVertex2f(0, 0);
  glTexCoord2f(long_x / gfx[porigen].width, 0);
  glVertex2f(long_x, 0);
  glTexCoord2f(long_x / gfx[porigen].width, long_y / gfx[porigen].height);
  glVertex2f(long_x, long_y);
  glTexCoord2f(0, long_y / gfx[porigen].height);
  glVertex2f(0, long_y);
  glEnd;
  glPopMatrix;
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.ScrollYPart2OpenGL(porigen, pdestino: byte; long_bloque_x: word; posicion_y: pword; scroll_x, scroll_y: word);
var
  pos_x, long_y, long_y2, posicion_x_def, posicion_y_def, size_of_x: word;
  temp_pos_y: pword;
begin
  temp_pos_y := posicion_y;
  pos_x := 0;
  while (pos_x < p_final[porigen].scroll.max_x) do
  begin
    posicion_y_def := (temp_pos_y^ + scroll_y) and p_final[porigen].scroll.mask_y;
    posicion_x_def := (pos_x + scroll_x) and p_final[porigen].scroll.mask_x;
    if ((posicion_x_def + long_bloque_x) > p_final[porigen].scroll.max_x) then
      size_of_x := p_final[porigen].scroll.max_x - posicion_x_def
    else
      size_of_x := long_bloque_x;
    if ((posicion_y_def + p_final[porigen].scroll.max_y) > p_final[porigen].scroll.long_y) then
    begin
      long_y := p_final[porigen].scroll.long_y - posicion_y_def;
      long_y2 := p_final[porigen].scroll.max_y - long_y;
      glBindTexture(GL_TEXTURE_2D, gfx[porigen].textureID);
      glEnable(GL_TEXTURE_2D);
      glPushMatrix;
      glTranslatef(posicion_x_def, long_y, 0);
      glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex2f(0, 0);
      glTexCoord2f(size_of_x / gfx[porigen].width, 0);
      glVertex2f(size_of_x, 0);
      glTexCoord2f(size_of_x / gfx[porigen].width, long_y2 / gfx[porigen].height);
      glVertex2f(size_of_x, long_y2);
      glTexCoord2f(0, long_y2 / gfx[porigen].height);
      glVertex2f(0, long_y2);
      glEnd;
      glPopMatrix;
      glDisable(GL_TEXTURE_2D);
    end;
    pos_x := pos_x + long_bloque_x;
    inc(temp_pos_y);
  end;
end;

procedure TGFX_ENGINE_OPENGL.SingleLineOpenGL(x, y, color, longitud: word; pant: byte);
var
  pixelData: array of word;
begin
  SetLength(pixelData, longitud);
  FillChar(pixelData[0], longitud * SizeOf(word), color);
  glBindTexture(GL_TEXTURE_2D, gfx[pant].textureID);
  glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, longitud, 1, GL_RGBA, GL_UNSIGNED_SHORT, @pixelData[0]);
end;

procedure TGFX_ENGINE_OPENGL.UpdateGfxSpriteAlphaOpenGL(pos_x, pos_y: word; dest, ngfx: byte);
var
  srcX, srcY, width, height: GLfloat;
  destX, destY: GLfloat;
begin
  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  srcX := 0;
  srcY := 0;
  width := gfx[ngfx].width;
  height := gfx[ngfx].height;

  destX := pos_x and p_final[dest].sprite_mask_x;
  destY := pos_y and p_final[dest].sprite_mask_y;

  glPushMatrix;
  glTranslatef(destX, destY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(width, height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, height);
  glEnd;

  glPopMatrix;

  if (destX + width > p_final[dest].sprite_end_x) or (destY + height > p_final[dest].sprite_end_y) then
  begin
    if (destX + width) > p_final[dest].sprite_end_x then
      destX := destX - (p_final[dest].sprite_end_x - destX);
    if (destY + height) > p_final[dest].sprite_end_y then
      destY := destY - (p_final[dest].sprite_end_y - destY);

    glPushMatrix;
    glTranslatef(destX, destY, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(width, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(width, height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(0, height);
    glEnd;

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.UpdateGfxSpriteLineOpenGL(pos_x, pos_y: word; dest, ngfx, line: byte);
var
  lineWidth, lineHeight: GLfloat;
  destX, destY: GLfloat;
begin
  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  lineWidth := gfx[ngfx].width;
  lineHeight := 1.0;

  destX := pos_x and p_final[dest].sprite_mask_x;
  destY := (pos_y + line) and p_final[dest].sprite_mask_y;

  glPushMatrix;
  glTranslatef(destX, destY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, line / gfx[ngfx].height);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, line / gfx[ngfx].height);
  glVertex2f(lineWidth, 0);
  glTexCoord2f(1.0, (line + 1) / gfx[ngfx].height);
  glVertex2f(lineWidth, lineHeight);
  glTexCoord2f(0.0, (line + 1) / gfx[ngfx].height);
  glVertex2f(0, lineHeight);
  glEnd;

  glPopMatrix;

  if (destX + lineWidth > p_final[dest].sprite_end_x) or (destY + lineHeight > p_final[dest].sprite_end_y) then
  begin
    if (destX + lineWidth) > p_final[dest].sprite_end_x then
      destX := destX - (p_final[dest].sprite_end_x - destX);
    if (destY + lineHeight) > p_final[dest].sprite_end_y then
      destY := destY - (p_final[dest].sprite_end_y - destY);

    glPushMatrix;
    glTranslatef(destX, destY, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0, line / gfx[ngfx].height);
    glVertex2f(0, 0);
    glTexCoord2f(1.0, line / gfx[ngfx].height);
    glVertex2f(lineWidth, 0);
    glTexCoord2f(1.0, (line + 1) / gfx[ngfx].height);
    glVertex2f(lineWidth, lineHeight);
    glTexCoord2f(0.0, (line + 1) / gfx[ngfx].height);
    glVertex2f(0, lineHeight);
    glEnd;

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.UpdateGfxSpriteOpenGL(pos_x, pos_y: word; dest, ngfx: byte);
var
  spriteWidth, spriteHeight: GLfloat;
  destX, destY: GLfloat;
begin
  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  spriteWidth := gfx[ngfx].width;
  spriteHeight := gfx[ngfx].height;

  destX := pos_x and p_final[dest].sprite_mask_x;
  destY := pos_y and p_final[dest].sprite_mask_y;

  glPushMatrix;
  glTranslatef(destX, destY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(spriteWidth, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(spriteWidth, spriteHeight);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, spriteHeight);
  glEnd;

  glPopMatrix;

  if (destX + spriteWidth > p_final[dest].sprite_end_x) or (destY + spriteHeight > p_final[dest].sprite_end_y) then
  begin
    if (destX + spriteWidth) > p_final[dest].sprite_end_x then
      destX := destX - (p_final[dest].sprite_end_x - destX);
    if (destY + spriteHeight) > p_final[dest].sprite_end_y then
      destY := destY - (p_final[dest].sprite_end_y - destY);

    glPushMatrix;
    glTranslatef(destX, destY, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(spriteWidth, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(spriteWidth, spriteHeight);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(0, spriteHeight);
    glEnd;

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.UpdateGfxSpriteSizeOpenGL(pos_x, pos_y: word; dest: byte; x_size, y_size, ipos_x, ipos_y: word);
var
  destX, destY: GLfloat;
  srcX, srcY, width, height: GLfloat;
begin
  glBindTexture(GL_TEXTURE_2D, gfx[dest].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  srcX := ipos_x;
  srcY := ipos_y;
  width := x_size;
  height := y_size;

  destX := pos_x and p_final[dest].sprite_mask_x;
  destY := pos_y and p_final[dest].sprite_mask_y;

  glPushMatrix;
  glTranslatef(destX, destY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(srcX / gfx[dest].width, srcY / gfx[dest].height);
  glVertex2f(0, 0);
  glTexCoord2f((srcX + width) / gfx[dest].width, srcY / gfx[dest].height);
  glVertex2f(width, 0);
  glTexCoord2f((srcX + width) / gfx[dest].width, (srcY + height) / gfx[dest].height);
  glVertex2f(width, height);
  glTexCoord2f(srcX / gfx[dest].width, (srcY + height) / gfx[dest].height);
  glVertex2f(0, height);
  glEnd;

  glPopMatrix;

  if (destX + width > p_final[dest].sprite_end_x) or (destY + height > p_final[dest].sprite_end_y) then
  begin
    if (destX + width) > p_final[dest].sprite_end_x then
      destX := destX - (p_final[dest].sprite_end_x - destX);
    if (destY + height) > p_final[dest].sprite_end_y then
      destY := destY - (p_final[dest].sprite_end_y - destY);

    glPushMatrix;
    glTranslatef(destX, destY, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(srcX / gfx[dest].width, srcY / gfx[dest].height);
    glVertex2f(0, 0);
    glTexCoord2f((srcX + width) / gfx[dest].width, srcY / gfx[dest].height);
    glVertex2f(width, 0);
    glTexCoord2f((srcX + width) / gfx[dest].width, (srcY + height) / gfx[dest].height);
    glVertex2f(width, height);
    glTexCoord2f(srcX / gfx[dest].width, (srcY + height) / gfx[dest].height);
    glVertex2f(0, height);
    glEnd;

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.UpdateGfxSpriteZoomAlphaOpenGL(pos_x, pos_y: word; dest, ngfx: byte; zx, zy: single);
var
  srcX, srcY, width, height: GLfloat;
  destX, destY: GLfloat;
begin
  if (zx <= 0) or (zy <= 0) then
    Exit;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  srcX := 0;
  srcY := 0;
  width := gfx[ngfx].width * zx;
  height := gfx[ngfx].height * zy;

  destX := pos_x and p_final[dest].sprite_mask_x;
  destY := pos_y and p_final[dest].sprite_mask_y;

  glPushMatrix;
  glTranslatef(destX, destY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(width, height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, height);
  glEnd;

  glPopMatrix;

  if (destX + width > p_final[dest].sprite_end_x) or (destY + height > p_final[dest].sprite_end_y) then
  begin
    if (destX + width) > p_final[dest].sprite_end_x then
      destX := destX - (p_final[dest].sprite_end_x - destX);
    if (destY + height) > p_final[dest].sprite_end_y then
      destY := destY - (p_final[dest].sprite_end_y - destY);

    glPushMatrix;
    glTranslatef(destX, destY, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(width, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(width, height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(0, height);
    glEnd;

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TGFX_ENGINE_OPENGL.UpdateGfxSpriteZoomOpenGL(pos_x, pos_y: word; dest, ngfx: byte; zx, zy: single);
var
  srcX, srcY, width, height: GLfloat;
  destX, destY: GLfloat;
begin
  if (zx <= 0) or (zy <= 0) then
    Exit;

  glBindTexture(GL_TEXTURE_2D, gfx[ngfx].textureID);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  srcX := 0;
  srcY := 0;
  width := gfx[ngfx].width * zx;
  height := gfx[ngfx].height * zy;

  destX := pos_x and p_final[dest].sprite_mask_x;
  destY := pos_y and p_final[dest].sprite_mask_y;

  glPushMatrix;
  glTranslatef(destX, destY, 0);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(0, 0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(width, 0);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(width, height);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(0, height);
  glEnd;

  glPopMatrix;

  if (destX + width > p_final[dest].sprite_end_x) or (destY + height > p_final[dest].sprite_end_y) then
  begin
    if (destX + width) > p_final[dest].sprite_end_x then
      destX := destX - (p_final[dest].sprite_end_x - destX);
    if (destY + height) > p_final[dest].sprite_end_y then
      destY := destY - (p_final[dest].sprite_end_y - destY);

    glPushMatrix;
    glTranslatef(destX, destY, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(width, 0);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(width, height);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(0, height);
    glEnd;

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

end.
