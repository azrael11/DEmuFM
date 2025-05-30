unit sdl2;

{
  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion
  Copyright (C) 2012-2020 Tim Blume aka End/EV1313

  SDL2-for-Pascal
  Copyright (C) 2020-2021 PGD Community

  SDL.pas is based on the files:
  "sdl.h",
  "sdl_audio.h",
  "sdl_blendmode.h",
  "sdl_clipboard.h",
  "sdl_cpuinfo.h",
  "sdl_events.h",
  "sdl_error.h",
  "sdl_filesystem.h",
  "sdl_gamecontroller.h",
  "sdl_gesture.h",
  "sdl_haptic.h",
  "sdl_hints.h",
  "sdl_joystick.h",
  "sdl_keyboard.h",
  "sdl_keycode.h",
  "sdl_loadso.h",
  "sdl_log.h",
  "sdl_pixels.h",
  "sdl_power.h",
  "sdl_main.h",
  "sdl_messagebox.h",
  "sdl_mouse.h",
  "sdl_mutex.h",
  "sdl_rect.h",
  "sdl_render.h",
  "sdl_rwops.h",
  "sdl_scancode.h",
  "sdl_shape.h",
  "sdl_stdinc.h",
  "sdl_surface.h",
  "sdl_system.h",
  "sdl_syswm.h",
  "sdl_thread.h",
  "sdl_timer.h",
  "sdl_touch.h",
  "sdl_version.h",
  "sdl_video.h"

  I will not translate:
  "sdl_opengl.h",
  "sdl_opengles.h"
  "sdl_opengles2.h"

  cause there's a much better OpenGL-Header avaible at delphigl.com:

  the dglopengl.pas

  You'll find it nowadays here: https://github.com/SaschaWillems/dglOpenGL

  Parts of the SDL.pas are from the SDL-1.2-Headerconversion from the JEDI-Team,
  written by Domenique Louis and others.

  I've changed the names of the dll for 32 & 64-Bit, so theres no conflict
  between 32 & 64 bit Libraries.

  This software is provided 'as-is', without any express or implied
  warranty.  In no case will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
  claim that you wrote the original software. If you use this software
  in a product, an acknowledgment in the product documentation would be
  appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
  misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Special Thanks to:

  - DelphiGL.com - Community
  - Domenique Louis and everyone else from the JEDI-Team
  - Sam Latinga and everyone else from the SDL-Team
}

{$DEFINE SDL}
{$I jedi.inc}

interface

{$IFDEF WINDOWS}

uses
{$IFDEF FPC}
  ctypes,
{$ENDIF}
  WinApi.Windows;
{$ENDIF}
{$IF DEFINED(UNIX) AND NOT DEFINED(ANDROID)}

uses
{$IFDEF FPC}
  ctypes,
{$ENDIF}
{$IFDEF DARWIN}
  CocoaAll;
{$ELSE}
X,
  XLib;
{$ENDIF}
{$ENDIF}
{$IF DEFINED(UNIX) AND DEFINED(ANDROID) AND DEFINED(FPC)}

uses
  ctypes;
{$ENDIF}

const

{$IFDEF WINDOWS}
  SDL_LibName = 'lib\SDL2.dll';
{$ENDIF}
{$IFDEF UNIX}
{$IFDEF DARWIN}
  SDL_LibName = 'libSDL2.dylib';
{$IFDEF FPC}
{$LINKLIB libSDL2}
{$ENDIF}
{$ELSE}
{$IFDEF FPC}
  SDL_LibName = 'libSDL2.so';
{$ELSE}
  SDL_LibName = 'libSDL2.so.0';
{$ENDIF}
{$MESSAGE HINT 'Known MESA bug may generate float-point exception in software graphics mode! See https://github.com/PascalGameDevelopment/SDL2-for-Pascal/issues/56 for reference.'}
{$ENDIF}
{$ENDIF}
{$IFDEF MACOS}
  SDL_LibName = 'SDL2';
{$IFDEF FPC}
{$LINKLIB libSDL2}
{$ENDIF}
{$ENDIF}
{$I ctypes.inc}                  // C data types

  { SDL2 version of the represented header file }
{$I sdlstdinc.inc}
{$I sdlversion.inc}              // 2.0.14
{$I sdlerror_c.inc}              // 2.0.14
{$I sdlerror.inc}                // 2.0.14
{$I sdlplatform.inc}             // 2.0.14
{$I sdlpower.inc}                // 2.0.14
{$I sdlthread.inc}
{$I sdlatomic.inc}               // 2.0.20
{$I sdlmutex.inc}                // 2.0.14 WIP
{$I sdltimer.inc}                // 2.0.18
{$I sdlpixels.inc}               // 2.0.14 WIP
{$I sdlrect.inc}                 // 2.0.14
{$I sdlrwops.inc}                // 2.0.14
{$I sdlaudio.inc}
{$I sdlblendmode.inc}            // 2.0.14
{$I sdlsurface.inc}              // 2.0.14
{$I sdlshape.inc}                // 2.0.14
{$I sdlvideo.inc}                // 2.0.14
{$I sdlhints.inc}                // 2.0.22
{$I sdlloadso.inc}
{$I sdlmessagebox.inc}           // 2.0.14
{$I sdlrenderer.inc}             // 2.0.22
{$I sdlscancode.inc}
{$I sdlkeyboard.inc}
{$I sdlmouse.inc}
{$I sdljoystick.inc}
{$I sdlgamecontroller.inc}
{$I sdlhaptic.inc}
{$I sdltouch.inc}
{$I sdlgesture.inc}
{$I sdlsensor.inc}
{$I sdlsyswm.inc}
{$I sdlevents.inc}
{$I sdllocale.inc}               // 2.0.14
{$I sdlclipboard.inc}
{$I sdlcpuinfo.inc}              // 2.0.14
{$I sdlfilesystem.inc}
{$I sdllog.inc}                  // 2.0.14
{$I sdlmisc.inc}                 // 2.0.14
{$I sdlsystem.inc}
{$I sdl.inc}                     // 2.0.14

implementation

// Macros from "sdl_version.h"
procedure SDL_VERSION(out X: TSDL_Version);
begin
  X.major := SDL_MAJOR_VERSION;
  X.minor := SDL_MINOR_VERSION;
  X.patch := SDL_PATCHLEVEL;
end;

function SDL_VERSIONNUM(X, Y, Z: cuint8): Cardinal;
begin
  Result := X * 1000 + Y * 100 + Z;
end;

function SDL_COMPILEDVERSION: Cardinal;
begin
  Result := SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL);
end;

function SDL_VERSION_ATLEAST(X, Y, Z: cuint8): Boolean;
begin
  Result := SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X, Y, Z);
end;

// from "sdl_mouse.h"
function SDL_Button(button: cint32): cint32;
begin
  Result := 1 shl (button - 1);
end;

{$IFDEF WINDOWS}
// from "sdl_thread.h"

function SDL_CreateThread(fn: TSDL_ThreadFunction; name: PAnsiChar; data: Pointer)
  : PSDL_Thread; overload;
begin
  Result := SDL_CreateThread(fn, name, data, nil, nil);
end;

{$ENDIF}

// from "sdl_rect.h"
function SDL_PointInRect(const p: PSDL_Point; const r: PSDL_Rect): Boolean;
begin
  Result := (p^.X >= r^.X) and (p^.X < (r^.X + r^.w)) and (p^.Y >= r^.Y) and (p^.Y < (r^.Y + r^.h))
end;

function SDL_RectEmpty(const r: PSDL_Rect): Boolean;
begin
  Result := (r^.w <= 0) or (r^.h <= 0);
end;

function SDL_RectEquals(const a, b: PSDL_Rect): Boolean;
begin
  Result := (a^.X = b^.X) and (a^.Y = b^.Y) and (a^.w = b^.w) and (a^.h = b^.h);
end;

// from "sdl_atomic.h"
function SDL_AtomicIncRef(atomic: PSDL_Atomic): cint;
begin
  Result := SDL_AtomicAdd(atomic, 1)
end;

function SDL_AtomicDecRef(atomic: PSDL_Atomic): Boolean;
begin
  Result := SDL_AtomicAdd(atomic, -1) = 1
end;

procedure SDL_CompilerBarrier();
{$IFDEF FPC}
begin
  ReadWriteBarrier()
{$ELSE}
var
  lock: TSDL_SpinLock;
begin
  lock := 0;
  SDL_AtomicLock(@lock);
  SDL_AtomicUnlock(@lock)
{$ENDIF}
end;

// from "sdl_audio.h"

function SDL_LoadWAV(_file: PAnsiChar; spec: PSDL_AudioSpec; audio_buf: ppcuint8;
  audio_len: pcuint32): PSDL_AudioSpec;
begin
  Result := SDL_LoadWAV_RW(SDL_RWFromFile(_file, 'rb'), 1, spec, audio_buf, audio_len);
end;

function SDL_AUDIO_BITSIZE(X: Cardinal): Cardinal;
begin
  Result := X and SDL_AUDIO_MASK_BITSIZE;
end;

function SDL_AUDIO_ISFLOAT(X: Cardinal): Cardinal;
begin
  Result := X and SDL_AUDIO_MASK_DATATYPE;
end;

function SDL_AUDIO_ISBIGENDIAN(X: Cardinal): Cardinal;
begin
  Result := X and SDL_AUDIO_MASK_ENDIAN;
end;

function SDL_AUDIO_ISSIGNED(X: Cardinal): Cardinal;
begin
  Result := X and SDL_AUDIO_MASK_SIGNED;
end;

function SDL_AUDIO_ISINT(X: Cardinal): Cardinal;
begin
  Result := not SDL_AUDIO_ISFLOAT(X);
end;

function SDL_AUDIO_ISLITTLEENDIAN(X: Cardinal): Cardinal;
begin
  Result := not SDL_AUDIO_ISLITTLEENDIAN(X);
end;

function SDL_AUDIO_ISUNSIGNED(X: Cardinal): Cardinal;
begin
  Result := not SDL_AUDIO_ISSIGNED(X);
end;

// from "sdl_pixels.h"

function SDL_PIXELFLAG(X: Cardinal): Cardinal;
begin
  Result := (X shr 28) and $0F;
end;

function SDL_PIXELTYPE(X: Cardinal): Cardinal;
begin
  Result := (X shr 24) and $0F;
end;

function SDL_PIXELORDER(X: Cardinal): Cardinal;
begin
  Result := (X shr 20) and $0F;
end;

function SDL_PIXELLAYOUT(X: Cardinal): Cardinal;
begin
  Result := (X shr 16) and $0F;
end;

function SDL_BITSPERPIXEL(X: Cardinal): Cardinal;
begin
  Result := (X shr 8) and $FF;
end;

function SDL_ISPIXELFORMAT_FOURCC(format: Variant): Boolean;
begin
  Result := (format and (SDL_PIXELFLAG(format) <> 1));
end;

// Macros from "sdl_surface.h"
function SDL_LoadBMP(_file: PAnsiChar): PSDL_Surface;
begin
  Result := SDL_LoadBMP_RW(SDL_RWFromFile(_file, 'rb'), 1);
end;

function SDL_SaveBMP(const surface: PSDL_Surface; const filename: AnsiString): cint;
begin
  Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(PAnsiChar(filename), 'wb'), 1)
end;

{ **
  *  Evaluates to true if the surface needs to be locked before access.
  * }
function SDL_MUSTLOCK(const S: PSDL_Surface): Boolean;
begin
  Result := ((S^.flags and SDL_RLEACCEL) <> 0)
end;

// Macros from "sdl_shape.h"
function SDL_SHAPEMODEALPHA(mode: TWindowShapeMode): Boolean;
begin
  Result := (mode = ShapeModeDefault) or (mode = ShapeModeBinarizeAlpha) or
    (mode = ShapeModeReverseBinarizeAlpha);
end;

// from "sdl_sysvideo.h"

function FULLSCREEN_VISIBLE(w: PSDL_Window): Variant;
begin
  Result := ((w^.flags and SDL_WINDOW_FULLSCREEN) and (w^.flags and SDL_WINDOW_SHOWN) and
    not(w^.flags and SDL_WINDOW_MINIMIZED));
end;

// from "sdl_video.h"

function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X: Variant): Variant;
begin
  Result := (SDL_WINDOWPOS_UNDEFINED_MASK or X);
end;

function SDL_WINDOWPOS_ISUNDEFINED(X: Variant): Variant;
begin
  Result := (X and $FFFF0000) = SDL_WINDOWPOS_UNDEFINED_MASK;
end;

function SDL_WINDOWPOS_CENTERED_DISPLAY(X: Variant): Variant;
begin
  Result := (SDL_WINDOWPOS_CENTERED_MASK or X);
end;

function SDL_WINDOWPOS_ISCENTERED(X: Variant): Variant;
begin
  Result := (X and $FFFF0000) = SDL_WINDOWPOS_CENTERED_MASK;
end;

// from "sdl_events.h"

function SDL_GetEventState(type_: TSDL_EventType): cuint8;
begin
  Result := SDL_EventState(type_, SDL_QUERY);
end;

// from "sdl_timer.h"
function SDL_TICKS_PASSED(const a, b: cint32): Boolean;
begin
  Result := ((b - a) <= 0);
end;

// from "sdl_gamecontroller.h"
{ **
  *  Load a set of mappings from a file, filtered by the current SDL_GetPlatform()
  * }
function SDL_GameControllerAddMappingsFromFile(const FilePath: PAnsiChar): cint32;
begin
  Result := SDL_GameControllerAddMappingsFromRW(SDL_RWFromFile(FilePath, 'rb'), 1)
end;

end.
