unit main_engine_ogl;

interface

uses
  System.SysUtils,
  WinApi.Windows,
  FMX.Forms,
  FMX.Dialogs,
  dglOpenGL;

const
  OPENGL_VERSION_MAJOR = 4;
  OPENGL_VERSION_MINOR = 6;

  PANT_SPRITES = 20;
  PANT_DOBLE = 21;
  PANT_AUX = 22;
  PANT_TEMP = 23;
  PANT_SPRITES_ALPHA = 24;
  MAX_PANT_VISIBLE = 19;
  MAX_PANT_SPRITES = 256;

  MAX_DIP_VALUES = $F;
  MAX_PUNBUF = 768;
  SCREEN_DIF = 20;
  FULL_SCREEN_X = 1920;
  FULL_SCREEN_Y = 1080;
  // Cpu lines
  CLEAR_LINE = 0;
  ASSERT_LINE = 1;
  HOLD_LINE = 2;
  PULSE_LINE = 3;
  ALREADY_RESET = 4;
  IRQ_DELAY = 5;
  INPUT_LINE_NMI = $20;
  M_PI = 3.1415926535;

type
  TScrollSettings = record
    long_x, max_x, mask_x: word;
    long_y, max_y, mask_y: word;
  end;

  TSpriteSettings = record
    sprite_end_x, sprite_end_y: word;
    sprite_mask_x, sprite_mask_y: word;
  end;

  TRegion = record
    x, y, w, h: word;
  end;

  tmain_screen = record
    fullscreen: boolean;
    video_mode: byte;
    flip_main_screen, flip_main_x, flip_main_y, rot90_screen, rol90_screen, fast: boolean;
    rot180_screen, rot270_screen, rot180SpGame_screen, pantalla_completa: boolean;
    clear: boolean;
    pause: boolean;
    mouse_x, mouse_y: single;
  end;

  TGLOBAL_CALLS = record
    start: function: boolean;
    num: integer;
    general_loop, reset, close, take_snapshot, setup, accept_config, cartridges, tapes: procedure;
    caption, open_file: string;
    fps_max: single;
    save_qsnap, load_qsnap: procedure(name: string);
  end;

  def_dip_value = record
    dip_val: word;
    dip_name: string;
  end;

  def_dip = record
    mask: word;
    name: string;
    number: byte;
    dip: array [0 .. MAX_DIP_VALUES] of def_dip_value;
  end;

  def_dip2 = record
    mask: word;
    name: string;
    case number: byte of
      2:
        (val2: array [0 .. 1] of word; name2: array [0 .. 1] of string[30];);
      4:
        (val4: array [0 .. 3] of word; name4: array [0 .. 3] of string[30];);
      8:
        (val8: array [0 .. 7] of word; name8: array [0 .. 7] of string[30];);
      16:
        (val16: array [0 .. 15] of word; name16: array [0 .. 15] of string[30];);
      32:
        (val32: array [0 .. 31] of word; name32: array [0 .. 31] of string[30];);
  end;

  pdef_dip = ^def_dip;
  pdef_dip2 = ^def_dip2;
  TEmuStatus = (EsPause, EsRunning, EsStoped, EsClosed);

  TOPENGL_ENGINE = class
  private
    PixelData: Pointer;
    windowHandle: HWND;
    deviceContext: HDC;
    glRenderContext: HGLRC; // OpenGL Rendering Context
    ScrollSettings: array [0 .. 255] of TScrollSettings;
    SpriteSettings: array [0 .. 255] of TSpriteSettings;

    function MyInitOpenGL(windowHandle: HWND): boolean;

  protected

  public
    quadVAO, vertex_buffer: GLuint;
    shader_program: GLuint;
    vertices: array [0 .. 19] of GLfloat;
    EmulationPaused: boolean;
    OGL_MainScreen: tmain_screen;

    constructor Create;
    destructor Destroy; override;

    procedure start_video_OpenGL(x, y: word; alpha: boolean = false);
    procedure screen_init_OpenGL(num: byte; x, y: word; trans: boolean = false; final_mix: boolean = false; alpha: boolean = false);
    procedure exit_game_OpenGL;

    procedure fullscreen_OpenGL;

    procedure update_video_OpenGL;
    procedure video_sync_OpenGL;

    procedure clear_screen_OpenGL;
    procedure change_caption_OpenGL(new_caption: string);

    procedure update_final_piece_OpenGL(o_x1, o_y1, o_x2, o_y2: word; site: byte);
    procedure screen_mod_scroll_OpenGL(num: byte; long_x, max_x, mask_x, long_y, max_y, mask_y: word);
    procedure screen_mod_sprites_OpenGL(num: byte; sprite_end_x, sprite_end_y, sprite_mask_x, sprite_mask_y: word);
    procedure update_region_OpenGL(o_x1, o_y1, o_x2, o_y2: word; src_site: byte; d_x1, d_y1, d_x2, d_y2: word; dest_site: byte);
    procedure flip_surface_OpenGL(flipX, flipY: boolean);

  published

  end;

var
  engine_opengl: TOPENGL_ENGINE;
  memory, mem_snd, mem_misc: array [0 .. $FFFF] of byte;
  EmuStatus, EmuStatusTemp: TEmuStatus;
  machine_calls: TGLOBAL_CALLS;

implementation

{ TOPENGL_ENGINE }

procedure TOPENGL_ENGINE.change_caption_OpenGL(new_caption: string);
begin
  // Platform-specific window title change
end;

procedure TOPENGL_ENGINE.clear_screen_OpenGL;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

constructor TOPENGL_ENGINE.Create;
var
  ScreenWidth, ScreenHeight: Integer;
begin
  inherited Create;
  InitOpenGL;

  ScreenWidth := Trunc(Screen.Width);
  ScreenHeight := Trunc(Screen.Height);

  windowHandle := CreateWindowEx(0, 'STATIC', 'OpenGL Fullscreen Window', WS_POPUP or WS_CLIPCHILDREN or WS_CLIPSIBLINGS, 0, 0, Round(ScreenWidth), Round(ScreenHeight), 0, 0, HInstance, nil);

  if not MyInitOpenGL(windowHandle) then
    raise Exception.Create('Failed to initialize OpenGL');

  ReadExtensions;
  ReadImplementationProperties;

  glGenVertexArrays(1, @quadVAO);
  glBindVertexArray(quadVAO);
  glGenBuffers(1, @vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), @vertices, GL_DYNAMIC_DRAW);

  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), Pointer(0));
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), Pointer(3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  OGL_MainScreen.fullscreen := true;
end;

destructor TOPENGL_ENGINE.Destroy;
begin

  inherited;
end;

procedure TOPENGL_ENGINE.exit_game_OpenGL;
begin
  glFinish;
  // Clean up resources
end;

procedure TOPENGL_ENGINE.flip_surface_OpenGL(flipX, flipY: boolean);
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  if flipX then
    glScalef(-1.0, 1.0, 1.0);
  if flipY then
    glScalef(1.0, -1.0, 1.0);
end;

procedure TOPENGL_ENGINE.fullscreen_OpenGL;
var
  style: LongInt;
begin
  if not OGL_MainScreen.fullscreen then
  begin
    style := GetWindowLong(windowHandle, GWL_STYLE);
    SetWindowLong(windowHandle, GWL_STYLE, style and not(WS_OVERLAPPEDWINDOW));
    SetWindowPos(windowHandle, HWND_TOP, 0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN), SWP_NOZORDER or SWP_FRAMECHANGED);
    OGL_MainScreen.fullscreen := true;
  end
  else
  begin
    style := GetWindowLong(windowHandle, GWL_STYLE);
    SetWindowLong(windowHandle, GWL_STYLE, style or WS_OVERLAPPEDWINDOW);
    SetWindowPos(windowHandle, HWND_TOP, 100, 100, 800, 600, SWP_NOZORDER or SWP_FRAMECHANGED);
    OGL_MainScreen.fullscreen := false;
  end;
end;

function TOPENGL_ENGINE.MyInitOpenGL(windowHandle: HWND): boolean;
var
  pfd: PIXELFORMATDESCRIPTOR;
  pixelFormat: Integer;
begin
  Result := false;

  // Obtain the Device Context
  deviceContext := GetDC(windowHandle);
  if deviceContext = 0 then
    raise Exception.Create('Failed to get Device Context (HDC)');

  // Validate the Window Handle
  if not IsWindow(windowHandle) then
    raise Exception.Create('Invalid window handle (HWND)');

  // Describe the Pixel Format
  FillChar(pfd, sizeof(pfd), 0);
  pfd.nSize := sizeof(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 32;
  pfd.cDepthBits := 24;
  pfd.cStencilBits := 8;
  pfd.iLayerType := PFD_MAIN_PLANE;

  // Choose and Set the Pixel Format
  pixelFormat := ChoosePixelFormat(deviceContext, @pfd);
  if (pixelFormat = 0) or not SetPixelFormat(deviceContext, pixelFormat, @pfd) then
    raise Exception.Create('Failed to set pixel format for OpenGL');

  // Validate if wglCreateContext is loaded
  if not Assigned(@wglCreateContext) then
    raise Exception.Create('wglCreateContext function is not loaded properly.');

  try
    glRenderContext := wglCreateContext(deviceContext);
    if glRenderContext = 0 then
      raise Exception.Create('Failed to create OpenGL Rendering Context: ' + SysErrorMessage(GetLastError));

    if not wglMakeCurrent(deviceContext, glRenderContext) then
      raise Exception.Create('Failed to make OpenGL context current: ' + SysErrorMessage(GetLastError));
  except
    on E: Exception do
    begin
      ShowMessage('Error during OpenGL context creation: ' + E.Message);
      Exit;
    end;
  end;

  if glRenderContext = 0 then
    raise Exception.Create('Failed to create OpenGL Rendering Context (HGLRC)');

  // Make the Rendering Context Current
  if not wglMakeCurrent(deviceContext, glRenderContext) then
  begin
    wglDeleteContext(glRenderContext);
    raise Exception.Create('Failed to make OpenGL context current');
  end;

  Result := true;
end;

procedure TOPENGL_ENGINE.screen_init_OpenGL(num: byte; x, y: word; trans: boolean = false; final_mix: boolean = false; alpha: boolean = false);
begin
  glViewport(0, 0, x, y);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, x, y, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TOPENGL_ENGINE.screen_mod_scroll_OpenGL(num: byte; long_x, max_x, mask_x, long_y, max_y, mask_y: word);
begin
  ScrollSettings[num].long_x := long_x;
  ScrollSettings[num].max_x := max_x;
  ScrollSettings[num].mask_x := mask_x;
  ScrollSettings[num].long_y := long_y;
  ScrollSettings[num].max_y := max_y;
  ScrollSettings[num].mask_y := mask_y;
end;

procedure TOPENGL_ENGINE.screen_mod_sprites_OpenGL(num: byte; sprite_end_x, sprite_end_y, sprite_mask_x, sprite_mask_y: word);
begin
  SpriteSettings[num].sprite_end_x := sprite_end_x;
  SpriteSettings[num].sprite_end_y := sprite_end_y;
  SpriteSettings[num].sprite_mask_x := sprite_mask_x;
  SpriteSettings[num].sprite_mask_y := sprite_mask_y;
end;

procedure TOPENGL_ENGINE.start_video_OpenGL(x, y: word; alpha: boolean);
begin
  glViewport(0, 0, x, y);
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  if alpha then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;
  glEnable(GL_TEXTURE_2D);
end;

procedure TOPENGL_ENGINE.update_final_piece_OpenGL(o_x1, o_y1, o_x2, o_y2: word; site: byte);
var
  textureID: GLuint;
begin
  glGenTextures(1, @textureID);
  glBindTexture(GL_TEXTURE_2D, textureID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // Assuming PixelData holds the image data
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, o_x2 - o_x1, o_y2 - o_y1, 0, GL_RGBA, GL_UNSIGNED_BYTE, PixelData);

  // Handle rotations
  if OGL_MainScreen.rot90_screen then
  begin
    glPushMatrix;
    glTranslatef(o_x2 / 2, o_y2 / 2, 0);
    glRotatef(90, 0, 0, 1);
    glTranslatef(-o_x2 / 2, -o_y2 / 2, 0);
  end
  else if OGL_MainScreen.rot180_screen then
  begin
    glPushMatrix;
    glTranslatef(o_x2 / 2, o_y2 / 2, 0);
    glRotatef(180, 0, 0, 1);
    glTranslatef(-o_x2 / 2, -o_y2 / 2, 0);
  end
  else if OGL_MainScreen.rot270_screen then
  begin
    glPushMatrix;
    glTranslatef(o_x2 / 2, o_y2 / 2, 0);
    glRotatef(270, 0, 0, 1);
    glTranslatef(-o_x2 / 2, -o_y2 / 2, 0);
  end;

  // Render the texture
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(o_x1, o_y1);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(o_x2, o_y1);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(o_x2, o_y2);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(o_x1, o_y2);
  glEnd;

  // Restore matrix after rotation
  glPopMatrix;

  glDeleteTextures(1, @textureID);
end;

procedure TOPENGL_ENGINE.update_region_OpenGL(o_x1, o_y1, o_x2, o_y2: word; src_site: byte; d_x1, d_y1, d_x2, d_y2: word; dest_site: byte);
var
  srcTexture, destTexture: GLuint;
begin
  // Generate source texture
  glGenTextures(1, @srcTexture);
  glBindTexture(GL_TEXTURE_2D, srcTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // Upload pixel data to source texture (mocked by PixelData)
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, o_x2 - o_x1, o_y2 - o_y1, 0, GL_RGBA, GL_UNSIGNED_BYTE, PixelData);

  // Draw the source texture to the destination area
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(d_x1, d_y1);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(d_x2, d_y1);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(d_x2, d_y2);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(d_x1, d_y2);
  glEnd;

  // Clean up
  glDeleteTextures(1, @srcTexture);
end;

procedure TOPENGL_ENGINE.update_video_OpenGL;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  SwapBuffers(wglGetCurrentDC());
end;

procedure TOPENGL_ENGINE.video_sync_OpenGL;
begin
  glFinish;
end;

end.
