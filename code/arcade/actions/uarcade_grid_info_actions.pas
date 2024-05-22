unit uarcade_grid_info_actions;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  FireDAC.Stan.Param,
  FMX.Types;

type
  TARCADE_GRID_INFO_DATA = record
    game_name: string;
    img_path: string;
    developer: string;
    publisher: string;
    year: string;
    players: string;
    coop: string;
    genre: string;
    description: string;
  end;

type
  TARCADE_GRID_INFO_ACTIONS = class(TObject)
  private

  protected

  public
    constructor Create;
    destructor Destroy;

    procedure grid_info_img_doubleclick;
    procedure grid_info_dt_DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF;
      var Operation: TDragOperation);
    procedure grid_info_dt_Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);

    procedure grid_info_clear_and_return;

    procedure move_key_up;
    procedure move_key_down;
    procedure move_key_left;
    procedure move_key_right;

  published

  end;

var
  Keep_data: TARCADE_GRID_INFO_DATA;
  Arcade_Grid_Action: TARCADE_GRID_INFO_ACTIONS;

implementation

uses
  main,
  udata,
  umain_config,
  uarcade_actions,
  uTheGamesDatabase;

{ TGRID_INFO_ACTIONS }

constructor TARCADE_GRID_INFO_ACTIONS.Create;
begin

end;

destructor TARCADE_GRID_INFO_ACTIONS.Destroy;
begin

end;

procedure TARCADE_GRID_INFO_ACTIONS.move_key_down;
begin
//
end;

procedure TARCADE_GRID_INFO_ACTIONS.move_key_left;
begin
//
end;

procedure TARCADE_GRID_INFO_ACTIONS.move_key_right;
begin
//
end;

procedure TARCADE_GRID_INFO_ACTIONS.move_key_up;
begin
//
end;

end.
