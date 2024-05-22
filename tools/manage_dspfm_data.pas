unit manage_dspfm_data;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  System.Rtti,
  FMX.Grid.Style,
  Data.Bind.EngExt,
  FMX.Bind.DBEngExt,
  FMX.Bind.Grid,
  System.Bindings.Outputs,
  FMX.Bind.Editors,
  FMX.StdCtrls,
  Data.Bind.Components,
  FMX.Edit,
  Data.Bind.Grid,
  Data.Bind.DBScope,
  FMX.Bind.Navigator,
  System.Actions,
  FMX.ActnList,
  Data.DB,
  FireDAC.Comp.DataSet,
  FMX.ScrollBox,
  FMX.Grid,
  FireDAC.Comp.Client,
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.Objects;

type
  Tfrm_main = class(TForm)
    FlowLayout1: TFlowLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ActLst: TActionList;
    fdconn: TFDConnection;
    fdtbl: TFDTable;
    Grid1: TGrid;
    fdtblnum: TFDAutoIncField;
    fdtblrom: TWideMemoField;
    fdtblname: TWideMemoField;
    fdtblyear: TWideMemoField;
    fdtblmanufactor: TWideMemoField;
    fdtblrom_path: TWideMemoField;
    fdtblrom_global_path: TWideMemoField;
    fdtblexe_num: TIntegerField;
    LiveBindingsBindNavigateFirst1: TFMXBindNavigateFirst;
    LiveBindingsBindNavigatePrior1: TFMXBindNavigatePrior;
    LiveBindingsBindNavigateNext1: TFMXBindNavigateNext;
    LiveBindingsBindNavigateLast1: TFMXBindNavigateLast;
    LiveBindingsBindNavigateInsert1: TFMXBindNavigateInsert;
    LiveBindingsBindNavigateDelete1: TFMXBindNavigateDelete;
    LiveBindingsBindNavigateEdit1: TFMXBindNavigateEdit;
    LiveBindingsBindNavigatePost1: TFMXBindNavigatePost;
    LiveBindingsBindNavigateCancel1: TFMXBindNavigateCancel;
    LiveBindingsBindNavigateRefresh1: TFMXBindNavigateRefresh;
    LiveBindingsBindNavigateApplyUpdates1: TFMXBindNavigateApplyUpdates;
    LiveBindingsBindNavigateCancelUpdates1: TFMXBindNavigateCancelUpdates;
    BS_DB: TBindSourceDB;
    BLst: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    edt_db: TEdit;
    lbl_db: TLabel;
    sb_db_find: TSpeedButton;
    img_db_find: TImage;
    od: TOpenDialog;
    btn_db_connect: TButton;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    LinkControlToField7: TLinkControlToField;
    procedure btn_db_connectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sb_db_findClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_main: Tfrm_main;

implementation

{$R *.fmx}

procedure Tfrm_main.btn_db_connectClick(Sender: TObject);
begin
  if edt_db.Text <> '' then
  begin
    fdconn.Connected := True;
    fdtbl.Active := True;
  end;
end;

procedure Tfrm_main.FormCreate(Sender: TObject);
begin
  fdconn.Connected := False;
end;

procedure Tfrm_main.sb_db_findClick(Sender: TObject);
begin
  if od.Execute then
  begin
    edt_db.Text := od.FileName;
    fdconn.Params.Database := od.FileName;
    fdconn.Params.DriverID := 'SQLite';
  end;
end;

end.
