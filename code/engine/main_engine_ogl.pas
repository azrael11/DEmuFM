unit main_engine_ogl;

interface

uses
  System.SysUtils,
  WinApi.Windows,
  dglOpenGL;

const
  OPENGL_VERSION_MAJOR = 4;
  OPENGL_VERSION_MINOR = 6;

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

  TOPENGL_ENGINE = class
  private
    PixelData: Pointer;
    windowHandle: HWND;
    deviceContext: HDC;
    ScrollSettings: array [0 .. 255] of TScrollSettings;
    SpriteSettings: array [0 .. 255] of TSpriteSettings;
    main_screen: tmain_screen;

  protected

  public
    constructor Create(windowHandle: HWND);
    destructor Destroy; override;

    procedure start_video_OpenGL(x, y: word; alpha: Boolean = false);
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
    procedure flip_surface_OpenGL(flipX, flipY: Boolean);
  published

  end;

var
  engine_opengl: TOPENGL_ENGINE;

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

constructor TOPENGL_ENGINE.Create(windowHandle: HWND);
begin
  inherited Create;
  Self.windowHandle := windowHandle;
  deviceContext := GetDC(windowHandle);
  if not InitOpenGL then
    raise Exception.Create('Failed to initialize OpenGL');
  ReadExtensions;
  ReadImplementationProperties;
  main_screen.fullScreen := false;
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

procedure TOPENGL_ENGINE.flip_surface_OpenGL(flipX, flipY: Boolean);
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
  if not main_screen.fullScreen then
  begin
    style := GetWindowLong(windowHandle, GWL_STYLE);
    SetWindowLong(windowHandle, GWL_STYLE, style and not(WS_OVERLAPPEDWINDOW));
    SetWindowPos(windowHandle, HWND_TOP, 0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN), SWP_NOZORDER or SWP_FRAMECHANGED);
    main_screen.fullScreen := True;
  end
  else
  begin
    style := GetWindowLong(windowHandle, GWL_STYLE);
    SetWindowLong(windowHandle, GWL_STYLE, style or WS_OVERLAPPEDWINDOW);
    SetWindowPos(windowHandle, HWND_TOP, 100, 100, 800, 600, SWP_NOZORDER or SWP_FRAMECHANGED);
    main_screen.fullScreen := false;
  end;
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

procedure TOPENGL_ENGINE.start_video_OpenGL(x, y: word; alpha: Boolean);
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
  if main_screen.rot90_screen then
  begin
    glPushMatrix;
    glTranslatef(o_x2 / 2, o_y2 / 2, 0);
    glRotatef(90, 0, 0, 1);
    glTranslatef(-o_x2 / 2, -o_y2 / 2, 0);
  end
  else if main_screen.rot180_screen then
  begin
    glPushMatrix;
    glTranslatef(o_x2 / 2, o_y2 / 2, 0);
    glRotatef(180, 0, 0, 1);
    glTranslatef(-o_x2 / 2, -o_y2 / 2, 0);
  end
  else if main_screen.rot270_screen then
  begin
    glPushMatrix;
    glTranslatef(o_x2 / 2, o_y2 / 2, 0);
    glRotatef(270, 0, 0, 1);
    glTranslatef(-o_x2 / 2, -o_y2 / 2, 0);
  end;

  // Render the texture
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex2f(o_x1, o_y1);
    glTexCoord2f(1.0, 0.0); glVertex2f(o_x2, o_y1);
    glTexCoord2f(1.0, 1.0); glVertex2f(o_x2, o_y2);
    glTexCoord2f(0.0, 1.0); glVertex2f(o_x1, o_y2);
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
    glTexCoord2f(0.0, 0.0); glVertex2f(d_x1, d_y1);
    glTexCoord2f(1.0, 0.0); glVertex2f(d_x2, d_y1);
    glTexCoord2f(1.0, 1.0); glVertex2f(d_x2, d_y2);
    glTexCoord2f(0.0, 1.0); glVertex2f(d_x1, d_y2);
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
