unit vars_hide;

interface

uses
  WinApi.Windows;

type
  scroll_tipo = record
    long_x, long_y: word; // Screen length
    max_x, max_y: word; // Length to copy
    mask_x, mask_y: word; // Scroll mask
  end;

  TSCREEN = record
    x: word;
    y: word;
    trans: boolean; // Are you transparent?
    final_mix, alpha: boolean;
    sprite_end_x, sprite_end_y, sprite_mask_x, sprite_mask_y: word;
    scroll: scroll_tipo;
  end;

  tipo_sprites = record
    pos_planos: array [0 .. 7] of dword;
    bit_pixel: byte;
    long_sprites: dword;
    banks: byte;
  end;

  parejas = record
    case byte of
      0:
        (l, h: byte);
      1:
        (w: word);
  end;

  pparejas = ^parejas;

  parejas680X = record
    case byte of
      0:
        (b, a: byte);
      1:
        (w: word);
  end;

  DParejas = record
    case byte of
      0:
        (l0, h0, l1, h1: byte);
      1:
        (wl, wh: word);
      2:
        (l: dword);
  end;

  pdparejas = ^DParejas;

const
  max_screens = 25;

var
  p_final: array [0 .. max_screens] of TSCREEN;
  des_gfx: tipo_sprites;

implementation

end.
