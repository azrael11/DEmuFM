unit lib_sdl2;

interface

uses
  System.Classes,
  System.SysUtils,
  Fmx.Dialogs,
  WinApi.Windows,
  System.UITypes;
// {$IFDEF WINDOWS}
//
// {$ENDIF}
// {$IFNDEF MACOS}
/// /  dynlibs,
// {$ENDIF}
// {$IFNDEF LINUX}
/// /  xlib;
// {$ENDIF}

procedure Init_sdl_lib;
procedure close_sdl_lib;

const
  libSDL_DISABLE=0;
  libAUDIO_S16 = $8010;
  libSDL_JOYBUTTONDOWN = $603;
  libSDL_JOYBUTTONUP = $604;
  libSDL_JOYAXISMOTION = $600;
  libSDL_MOUSEMOTION = $400;
  libSDL_MOUSEBUTTONDOWN = $401;
  libSDL_MOUSEBUTTONUP = $402;
  libSDL_BUTTON_LEFT = 1;
  libSDL_BUTTON_RIGHT = 3;
  libSDL_KEYUP = $301;
  libSDL_KEYDOWN = $300;
  libSDL_COMMONEVENT = 1;
  libSDL_QUITEV = $100;
  libSDL_WINDOWEVENT = $200;
  libSDL_TSYSWMEVENT = $201;
  libSDL_TEXTEDITING = $302;
  libSDL_TEXTINPUT = $303;
  libSDL_MOUSEWHEEL = $403;
  libSDL_JOYBALLMOTION = $601;
  libSDL_JOYHATMOTION = $602;
  libSDL_JOYDEVICEADDED = $605;
  libSDL_JOYDEVICEREMOVED = $606;
  libSDL_CONTROLLERAXISMOTION = $650;
  libSDL_CONTROLLERBUTTONDOWN = $651;
  libSDL_CONTROLLERBUTTONUP = $652;
  libSDL_CONTROLLERDEVICEADDED = $653;
  libSDL_CONTROLLERDEVICEREMOVED = $654;
  libSDL_CONTROLLERDEVICEREMAPPED = $655;
  libSDL_FINGERDOWN = $700;
  libSDL_FINGERUP = $701;
  libSDL_FINGERMOTION = $702;
  libSDL_DOLLARGESTURE = $800;
  libSDL_DOLLARRECORD = $801;
  libSDL_MULTIGESTURE = $802;
  libSDL_DROPFILE = $1000;
  libSDL_TUSEREVENT = $8000;

  libSDL_INIT_VIDEO = $00000020;
  libSDL_INIT_JOYSTICK = $00000200;
  libSDL_INIT_NOPARACHUTE = $00100000;
  libSDL_INIT_AUDIO = $00000010;
  libSDL_WINDOWPOS_UNDEFINED = $1FFF0000;
  libSDL_WINDOWPOS_CENTERED = $2FFF0000;
  libSDL_WINDOW_FULLSCREEN = $00000001;
  libSDL_WINDOW_OPENGL = $0000002;
  libSDL_WINDOW_ALWAYS_ON_TOP = $00008000;
  libSDL_WINDOW_FULLSCREEN_DESKTOP = libSDL_WINDOW_FULLSCREEN or $1000;

  libSDL_WINDOWEVENT_SHOWN = 1; { **< Window has been shown * }
  libSDL_WINDOWEVENT_HIDDEN = 2; { **< Window has been hidden * }
  libSDL_WINDOWEVENT_EXPOSED = 3; { **< Window has been exposed and should be redrawn * }
  libSDL_WINDOWEVENT_MOVED = 4; { **< Window has been moved to data1; data2 * }
  libSDL_WINDOWEVENT_RESIZED = 5; { **< Window has been resized to data1xdata2 * }
  libSDL_WINDOWEVENT_SIZE_CHANGED = 6;
  { **< The window size has changed; either as a result of an API call or through the system or user changing the window size. * }
  libSDL_WINDOWEVENT_MINIMIZED = 7; { **< Window has been minimized * }
  libSDL_WINDOWEVENT_MAXIMIZED = 8; { **< Window has been maximized * }
  libSDL_WINDOWEVENT_RESTORED = 9; { **< Window has been restored to normal size and position * }
  libSDL_WINDOWEVENT_ENTER = 10; { **< Window has gained mouse focus * }
  libSDL_WINDOWEVENT_LEAVE = 11; { **< Window has lost mouse focus * }
  libSDL_WINDOWEVENT_FOCUS_GAINED = 12; { **< Window has gained keyboard focus * }
  libSDL_WINDOWEVENT_FOCUS_LOST = 13; { **< Window has lost keyboard focus * }
  libSDL_WINDOWEVENT_CLOSE = 14; { **< The window manager requests that the window be closed * }
  libSDL_WINDOWEVENT_TAKE_FOCUS = 15;
  { **< Window is being offered a focus (should SetWindowInputFocus() on itself or a subwindow, or ignore) * }
  libSDL_WINDOWEVENT_HIT_TEST = 16; { **< Window had a hit test that wasn't SDL_HITTEST_NORMAL. * }
  libSDL_PIXELTYPE_UNKNOWN=0;
  libSDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS='SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS';

{$I lib_sdl2.inc}

var
  sdl_dll_handle: int64;
  SDL_Init: function(flags: Cardinal): LongInt; cdecl;
  SDL_WasInit: function(flags: Cardinal): Cardinal; cdecl;
  SDL_Quit: procedure; cdecl;
  SDL_GetError:function:PAnsiChar;cdecl;
  SDL_LoadBMP_RW: function(src: libsdlp_RWops; freesrc: LongInt): libsdlp_Surface; cdecl;
  SDL_CreateRGBSurface: function(flags: Cardinal; width: LongInt; height: LongInt; depth: LongInt;
    Rmask: Cardinal; Gmask: Cardinal; Bmask: Cardinal; Amask: Cardinal): libsdlp_Surface; cdecl;
  SDL_UpperBlit: function(src: libsdlp_Surface; const srcrect: libsdlp_rect; dst: libsdlp_Surface;
    dstrect: libsdlp_rect): LongInt; cdecl;
    SDL_UpperBlitScaled:function(src:libsdlp_Surface;const srcrect:libsdlp_rect;dst:libsdlp_Surface;dstrect:libsdlp_rect):LongInt;cdecl;
  SDL_SetSurfaceRLE: function(surface: libsdlp_Surface; flag: LongInt): LongInt; cdecl;
  SDL_LockSurface: function(surface: libsdlp_Surface): LongInt; cdecl;
  SDL_UnlockSurface: function(surface: libsdlp_Surface): LongInt; cdecl;
  SDL_FreeSurface: procedure(surface: libsdlp_Surface); cdecl;
  SDL_SaveBMP_RW: function(surface: libsdlp_Surface; dst: libsdlp_RWops; freedst: LongInt)
    : LongInt; cdecl;
  SDL_SetColorKey: function(surface: libsdlp_Surface; flag: LongInt; key: Cardinal): LongInt; cdecl;
  SDL_JoystickGetAxis: function(joystick: libsdlp_joystick; axis: LongInt): smallint; cdecl;
  SDL_NumJoysticks: function: LongInt; cdecl;
  SDL_JoystickName: function(joystick: libsdlp_joystick): PAnsiChar; cdecl;
  SDL_JoystickNumButtons: function(joystick: libsdlp_joystick): LongInt; cdecl;
  SDL_JoystickOpen: function(device_index: LongInt): libsdlp_joystick; cdecl;
  SDL_JoystickClose: procedure(joystick: libsdlp_joystick); cdecl;
  SDL_JoystickGetButton: function(joystick: libsdlp_joystick; button: LongInt): byte; cdecl;
  SDL_JoystickNumHats: function(joystick: libsdlp_joystick): LongInt; cdecl;
  SDL_PollEvent: function(event: libSDLp_Event): LongInt; cdecl;
  SDL_CreateSystemCursor: function(id: word): libsdlP_cursor; cdecl;
  SDL_SetCursor: procedure(cursor: libsdlP_cursor); cdecl;
  SDL_ShowCursor: function(toggle: LongInt): LongInt; cdecl;
  SDL_DestroyWindow: procedure(window: libsdlP_Window); cdecl;
  SDL_VideoQuit: procedure; cdecl;
  SDL_SetWindowSize: procedure(window: libsdlP_Window; w: LongInt; h: LongInt); cdecl;
  SDL_SetWindowPosition:procedure(window:libsdlP_Window;w:LongInt;h:LongInt);cdecl;
  SDL_GetWindowSurface: function(window: libsdlP_Window): libsdlp_Surface; cdecl;
  SDL_CreateWindowFrom: function(const data: Pointer): libsdlP_Window; cdecl;
  SDL_CreateWindow: function(const title: PAnsiChar; x: LongInt; y: LongInt; w: LongInt; h: LongInt;
    flags: Cardinal): libsdlP_Window; cdecl;
  SDL_UpdateWindowSurface: function(window: libsdlP_Window): LongInt; cdecl;
  SDL_RWFromFile: function(const _file: PAnsiChar; const mode: PAnsiChar): libsdlp_RWops; cdecl;
  SDL_GetRGB: procedure(pixel: Cardinal; const format: libsdlp_PixelFormat; r: pbyte; g: pbyte;
    b: pbyte); cdecl;
  SDL_MapRGB: function(const format: libsdlp_PixelFormat; r: byte; g: byte; b: byte)
    : Cardinal; cdecl;
  SDL_MapRGBA: function(const format: libsdlp_PixelFormat; r: byte; g: byte; b: byte; a: byte)
    : Cardinal; cdecl;
  SDL_GetKeyboardState: function(numkeys: PInteger): pbyte; cdecl;
  SDL_SetHint:function(const title:PAnsiChar;const value:PAnsiChar):LongBool;cdecl;
  SDL_JoystickUpdate:procedure;cdecl;
  SDL_JoystickEventState:function(state:LongInt):LongInt;cdecl;
  SDL_JoystickGetAxisInitialState:function(joystick:libsdlp_joystick;axis:LongInt;state:psmallint):LongBool;cdecl;
  SDL_GetClosestDisplayMode:function(displayIndex:LongInt;const mode:libsdlp_DisplayMode;closest:libsdlp_DisplayMode):libsdlp_DisplayMode;cdecl;
  SDL_SetWindowDisplayMode:function(window:libsdlP_Window;const mode:libsdlp_DisplayMode):LongInt; cdecl;
  SDL_GetTicks:function:Cardinal;cdecl;
  SDL_SetWindowFullscreen:function(window:libsdlP_Window;flags:LongInt):LongInt;cdecl;

implementation

procedure Init_sdl_lib;
begin
  // {$IFDEF MACOS64}
  // sdl_dll_handle := LoadLibrary('libSDL2.dylib');
  // {$ENDIF}
  // {$IFDEF LINUX64}
  // sdl_dll_handle := LoadLibrary('libSDL2.so');
  // if sdl_dll_handle = 0 then
  // sdl_dll_handle := LoadLibrary('libSDL2.so.0');
  // if sdl_dll_handle = 0 then
  // sdl_dll_handle := LoadLibrary('libSDL2-2.0.so.0');
  // {$ENDIF}
  // {$IFDEF WIN64}
  sdl_dll_handle := LoadLibrary('lib' + PathDelim + 'sdl2.dll');
  // {$ENDIF}
  if sdl_dll_handle = 0 then
  begin
    MessageDlg('SDL2 library not found.' + chr(10) + chr(13) + 'Please read the documentation!',
      TMsgDlgType.mtError, mbAbortIgnore, 0);
    halt(0);
  end;
  // sdl
  @SDL_Init := GetProcAddress(sdl_dll_handle, 'SDL_Init');
  @SDL_WasInit := GetProcAddress(sdl_dll_handle, 'SDL_WasInit');
  @SDL_Quit := GetProcAddress(sdl_dll_handle, 'SDL_Quit');
@SDL_SetHint:=GetProcAddress(sdl_dll_Handle,'SDL_SetHint');
@SDL_GetError:=GetProcAddress(sdl_dll_Handle,'SDL_GetError');
  // surface
  @SDL_LoadBMP_RW := GetProcAddress(sdl_dll_handle, 'SDL_LoadBMP_RW');
  @SDL_CreateRGBSurface := GetProcAddress(sdl_dll_handle, 'SDL_CreateRGBSurface');
  @SDL_UpperBlit := GetProcAddress(sdl_dll_handle, 'SDL_UpperBlit');
@SDL_UpperBlitScaled:=GetProcAddress(sdl_dll_Handle,'SDL_UpperBlitScaled');
  @SDL_FreeSurface := GetProcAddress(sdl_dll_handle, 'SDL_FreeSurface');
  @SDL_SaveBMP_RW := GetProcAddress(sdl_dll_handle, 'SDL_SaveBMP_RW');
  @SDL_SetColorKey := GetProcAddress(sdl_dll_handle, 'SDL_SetColorKey');
  @SDL_LockSurface := GetProcAddress(sdl_dll_handle, 'SDL_LockSurface');
  @SDL_UnlockSurface := GetProcAddress(sdl_dll_handle, 'SDL_UnlockSurface');
  // joystick
  @SDL_JoystickGetAxis := GetProcAddress(sdl_dll_handle, 'SDL_JoystickGetAxis');
  @SDL_NumJoysticks := GetProcAddress(sdl_dll_handle, 'SDL_NumJoysticks');
  @SDL_JoystickName := GetProcAddress(sdl_dll_handle, 'SDL_JoystickName');
  @SDL_JoystickNumButtons := GetProcAddress(sdl_dll_handle, 'SDL_JoystickNumButtons');
  @SDL_JoystickOpen := GetProcAddress(sdl_dll_handle, 'SDL_JoystickOpen');
  @SDL_JoystickClose := GetProcAddress(sdl_dll_handle, 'SDL_JoystickClose');
  @SDL_JoystickGetButton := GetProcAddress(sdl_dll_handle, 'SDL_JoystickGetButton');
  @SDL_JoystickNumHats := GetProcAddress(sdl_dll_handle, 'SDL_JoystickNumHats');
  @SDL_JoystickUpdate:=GetProcAddress(sdl_dll_Handle,'SDL_JoystickUpdate');
@SDL_JoystickEventState:=GetProcAddress(sdl_dll_Handle,'SDL_JoystickEventState');
@SDL_JoystickGetAxisInitialState:=GetProcAddress(sdl_dll_Handle,'SDL_JoystickGetAxisInitialState');
  // events
  @SDL_PollEvent := GetProcAddress(sdl_dll_handle, 'SDL_PollEvent');
  // mouse
  @SDL_SetCursor := GetProcAddress(sdl_dll_handle, 'SDL_SetCursor');
  @SDL_ShowCursor := GetProcAddress(sdl_dll_handle, 'SDL_ShowCursor');
  @SDL_CreateSystemCursor := GetProcAddress(sdl_dll_handle, 'SDL_CreateSystemCursor');
  // video
  @SDL_DestroyWindow := GetProcAddress(sdl_dll_handle, 'SDL_DestroyWindow');
  @SDL_VideoQuit := GetProcAddress(sdl_dll_handle, 'SDL_VideoQuit');
  @SDL_SetWindowSize := GetProcAddress(sdl_dll_handle, 'SDL_SetWindowSize');
@SDL_SetWindowPosition:=GetProcAddress(sdl_dll_Handle,'SDL_SetWindowPosition');
  @SDL_GetWindowSurface := GetProcAddress(sdl_dll_handle, 'SDL_GetWindowSurface');
  @SDL_CreateWindowFrom := GetProcAddress(sdl_dll_handle, 'SDL_CreateWindowFrom');
  @SDL_CreateWindow := GetProcAddress(sdl_dll_handle, 'SDL_CreateWindow');
  @SDL_UpdateWindowSurface := GetProcAddress(sdl_dll_handle, 'SDL_UpdateWindowSurface');
@SDL_GetClosestDisplayMode:=GetProcAddress(sdl_dll_Handle,'SDL_GetClosestDisplayMode');
@SDL_SetWindowDisplayMode:=GetProcAddress(sdl_dll_Handle,'SDL_SetWindowDisplayMode');
  @SDL_SetWindowFullscreen := GetProcAddress(sdl_dll_handle, 'SDL_SetWindowFullscreen');
  // rwops
  @SDL_RWFromFile := GetProcAddress(sdl_dll_handle, 'SDL_RWFromFile');
  // pixels
  @SDL_GetRGB := GetProcAddress(sdl_dll_handle, 'SDL_GetRGB');
  @SDL_MapRGB := GetProcAddress(sdl_dll_handle, 'SDL_MapRGB');
  @SDL_MapRGBA := GetProcAddress(sdl_dll_handle, 'SDL_MapRGBA');
  // keyboard
  @SDL_GetKeyboardState := GetProcAddress(sdl_dll_handle, 'SDL_GetKeyboardState');
@SDL_GetTicks:=GetProcAddress(sdl_dll_Handle,'SDL_GetTicks');
end;

procedure close_sdl_lib;
begin
  if sdl_dll_handle <> 0 then
  begin
    FreeLibrary(sdl_dll_handle);
    sdl_dll_handle := 0;
  end;
end;

end.
