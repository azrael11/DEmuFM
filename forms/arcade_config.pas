unit arcade_config;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs;

type
  Tfrm_arcade_config = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure activate_arcade_config;

var
  frm_arcade_config: Tfrm_arcade_config;

implementation

{$R *.fmx}

uses
  main_engine,
  controls_engine;

procedure activate_arcade_config;
begin

end;

//procedure activate_arcade_config;
//var
//  dip_tmp: pdef_dip;
//  dip_pos, h: byte;
//begin
//  // Si no hay Dip Switch me salgo ya
//  if marcade.dswa_val = nil then
//    exit;
////  config_arcade.GroupBox2.Visible := false;
////  config_arcade.GroupBox3.Visible := false;
////  config_arcade.Width := 279;
////  config_arcade.Button1.Left := 9;
////  config_arcade.Button2.Left := 141;
//  // Poner los valores de los dip
//  dip_tmp := marcade.dswa_val;
//  dip_pos := 1;
//  while dip_tmp.name <> '' do
//  begin
//    ComboBox_dip[0, dip_pos] := TComboBox.create(config_arcade);
//    ComboBox_dip[0, dip_pos].Parent := config_arcade.GroupBox1;
//    ComboBox_dip[0, dip_pos].Left := 100;
//    ComboBox_dip[0, dip_pos].Top := 25 * dip_pos;
//    ComboBox_dip[0, dip_pos].TabStop := false;
//    ComboBox_dip[0, dip_pos].Width := 150;
//    Label_dip[0, dip_pos] := TLabel.create(config_arcade);
//    Label_dip[0, dip_pos].Parent := config_arcade.GroupBox1;
//    Label_dip[0, dip_pos].Left := 5;
//    Label_dip[0, dip_pos].Top := 25 * dip_pos;
//    Label_dip[0, dip_pos].AutoSize := true;
//    if (dip_tmp.number - 1) > MAX_DIP_VALUES then
//      MessageDlg('Warning: More Values in DIP A than available!', mtError, [mbOk], 0);
//    for h := 0 to dip_tmp.number - 1 do
//    begin
//      ComboBox_dip[0, dip_pos].Items.Add(dip_tmp.dip[h].dip_name);
//      if dip_tmp.dip[h].dip_val = (marcade.dswa and dip_tmp.mask) then
//        ComboBox_dip[0, dip_pos].ItemIndex := h;
//    end;
//    Label_dip[0, dip_pos].Caption := dip_tmp.name;
//    dip_pos := dip_pos + 1;
//    if dip_pos > MAX_COMP then
//      MessageDlg('Warning: More Values in DIP A than available!', mtError, [mbOk], 0);
//    inc(dip_tmp);
//  end;
//  if marcade.dswb_val <> nil then
//  begin
//    dip_tmp := marcade.dswb_val;
//    dip_pos := 1;
//    config_arcade.Width := 550;
//    config_arcade.Button1.Left := 64;
//    config_arcade.Button2.Left := 340;
//    config_arcade.GroupBox2.Visible := true;
//    while dip_tmp.name <> '' do
//    begin
//      ComboBox_dip[1, dip_pos] := TComboBox.create(config_arcade);
//      ComboBox_dip[1, dip_pos].Parent := config_arcade.GroupBox2;
//      ComboBox_dip[1, dip_pos].Left := 100;
//      ComboBox_dip[1, dip_pos].Top := 25 * dip_pos;
//      ComboBox_dip[1, dip_pos].TabStop := false;
//      ComboBox_dip[1, dip_pos].Width := 150;
//      Label_dip[1, dip_pos] := TLabel.create(config_arcade);
//      Label_dip[1, dip_pos].Parent := config_arcade.GroupBox2;
//      Label_dip[1, dip_pos].Left := 5;
//      Label_dip[1, dip_pos].Top := 25 * dip_pos;
//      Label_dip[1, dip_pos].AutoSize := true;
//      if (dip_tmp.number - 1) > MAX_DIP_VALUES then
//        MessageDlg('Warning: More Values in DIP B than available!', mtError, [mbOk], 0);
//      for h := 0 to dip_tmp.number - 1 do
//      begin
//        ComboBox_dip[1, dip_pos].Items.Add(dip_tmp.dip[h].dip_name);
//        if dip_tmp.dip[h].dip_val = (marcade.dswb and dip_tmp.mask) then
//          ComboBox_dip[1, dip_pos].ItemIndex := h;
//      end;
//      Label_dip[1, dip_pos].Caption := dip_tmp.name;
//      dip_pos := dip_pos + 1;
//      if dip_pos > MAX_COMP then
//        MessageDlg('Warning: More Values in DIP B than available!', mtError, [mbOk], 0);
//      inc(dip_tmp);
//    end;
//  end;
//  if marcade.dswc_val <> nil then
//  begin
//    dip_tmp := marcade.dswc_val;
//    dip_pos := 1;
//    config_arcade.Width := 823;
//    config_arcade.Button1.Left := 208;
//    config_arcade.Button2.Left := 496;
//    config_arcade.GroupBox3.Visible := true;
//    while dip_tmp.name <> '' do
//    begin
//      ComboBox_dip[2, dip_pos] := TComboBox.create(config_arcade);
//      ComboBox_dip[2, dip_pos].Parent := config_arcade.GroupBox3;
//      ComboBox_dip[2, dip_pos].Left := 100;
//      ComboBox_dip[2, dip_pos].Top := 25 * dip_pos;
//      ComboBox_dip[2, dip_pos].TabStop := false;
//      ComboBox_dip[2, dip_pos].Width := 150;
//      Label_dip[2, dip_pos] := TLabel.create(config_arcade);
//      Label_dip[2, dip_pos].Parent := config_arcade.GroupBox3;
//      Label_dip[2, dip_pos].Left := 5;
//      Label_dip[2, dip_pos].Top := 25 * dip_pos;
//      Label_dip[2, dip_pos].AutoSize := true;
//      if (dip_tmp.number - 1) > MAX_DIP_VALUES then
//        MessageDlg('Warning: More Values in DIP C than available!', mtError, [mbOk], 0);
//      for h := 0 to dip_tmp.number - 1 do
//      begin
//        ComboBox_dip[2, dip_pos].Items.Add(dip_tmp.dip[h].dip_name);
//        if dip_tmp.dip[h].dip_val = (marcade.dswc and dip_tmp.mask) then
//          ComboBox_dip[2, dip_pos].ItemIndex := h;
//      end;
//      Label_dip[2, dip_pos].Caption := dip_tmp.name;
//      dip_pos := dip_pos + 1;
//      if dip_pos > MAX_COMP then
//        MessageDlg('Warning: More Values in DIP C than available!', mtError, [mbOk], 0);
//      inc(dip_tmp);
//    end;
//  end;
//  // Mostrar ventana
//  config_arcade.Show;
//  config_arcade.SetFocus;
//  while config_arcade.Showing do
//    application.ProcessMessages;
//end;

{ TForm1 }

// procedure Tfrm_arcade_config.activate_arcade_config;
// var
// dip_tmp: pdef_dip;
// dip_pos, h: byte;
// begin
// // Si no hay Dip Switch me salgo ya
// if marcade.dswa_val = nil then
// exit;
// config_arcade.GroupBox2.Visible := false;
// config_arcade.GroupBox3.Visible := false;
// config_arcade.Width := 279;
// config_arcade.Button1.Left := 9;
// config_arcade.Button2.Left := 141;
// // Poner los valores de los dip
// dip_tmp := marcade.dswa_val;
// dip_pos := 1;
// while dip_tmp.name <> '' do
// begin
// Combobox_dip[0, dip_pos] := TComboBox.create(config_arcade);
// Combobox_dip[0, dip_pos].Parent := config_arcade.GroupBox1;
// Combobox_dip[0, dip_pos].Left := 100;
// Combobox_dip[0, dip_pos].Top := 25 * dip_pos;
// Combobox_dip[0, dip_pos].TabStop := false;
// Combobox_dip[0, dip_pos].Width := 150;
// Label_dip[0, dip_pos] := TLabel.create(config_arcade);
// Label_dip[0, dip_pos].Parent := config_arcade.GroupBox1;
// Label_dip[0, dip_pos].Left := 5;
// Label_dip[0, dip_pos].Top := 25 * dip_pos;
// Label_dip[0, dip_pos].AutoSize := true;
// if (dip_tmp.number - 1) > MAX_DIP_VALUES then
// MessageDlg('Warning: More Values in DIP A than available!', mtError, [mbOk], 0);
// for h := 0 to dip_tmp.number - 1 do
// begin
// Combobox_dip[0, dip_pos].Items.Add(dip_tmp.dip[h].dip_name);
// if dip_tmp.dip[h].dip_val = (marcade.dswa and dip_tmp.mask) then
// Combobox_dip[0, dip_pos].ItemIndex := h;
// end;
// Label_dip[0, dip_pos].Caption := dip_tmp.name;
// dip_pos := dip_pos + 1;
// if dip_pos > MAX_COMP then
// MessageDlg('Warning: More Values in DIP A than available!', mtError, [mbOk], 0);
// inc(dip_tmp);
// end;
// if marcade.dswb_val <> nil then
// begin
// dip_tmp := marcade.dswb_val;
// dip_pos := 1;
// config_arcade.Width := 550;
// config_arcade.Button1.Left := 64;
// config_arcade.Button2.Left := 340;
// config_arcade.GroupBox2.Visible := true;
// while dip_tmp.name <> '' do
// begin
// Combobox_dip[1, dip_pos] := TComboBox.create(config_arcade);
// Combobox_dip[1, dip_pos].Parent := config_arcade.GroupBox2;
// Combobox_dip[1, dip_pos].Left := 100;
// Combobox_dip[1, dip_pos].Top := 25 * dip_pos;
// Combobox_dip[1, dip_pos].TabStop := false;
// Combobox_dip[1, dip_pos].Width := 150;
// Label_dip[1, dip_pos] := TLabel.create(config_arcade);
// Label_dip[1, dip_pos].Parent := config_arcade.GroupBox2;
// Label_dip[1, dip_pos].Left := 5;
// Label_dip[1, dip_pos].Top := 25 * dip_pos;
// Label_dip[1, dip_pos].AutoSize := true;
// if (dip_tmp.number - 1) > MAX_DIP_VALUES then
// MessageDlg('Warning: More Values in DIP B than available!', mtError, [mbOk], 0);
// for h := 0 to dip_tmp.number - 1 do
// begin
// Combobox_dip[1, dip_pos].Items.Add(dip_tmp.dip[h].dip_name);
// if dip_tmp.dip[h].dip_val = (marcade.dswb and dip_tmp.mask) then
// Combobox_dip[1, dip_pos].ItemIndex := h;
// end;
// Label_dip[1, dip_pos].Caption := dip_tmp.name;
// dip_pos := dip_pos + 1;
// if dip_pos > MAX_COMP then
// MessageDlg('Warning: More Values in DIP B than available!', mtError, [mbOk], 0);
// inc(dip_tmp);
// end;
// end;
// if marcade.dswc_val <> nil then
// begin
// dip_tmp := marcade.dswc_val;
// dip_pos := 1;
// config_arcade.Width := 823;
// config_arcade.Button1.Left := 208;
// config_arcade.Button2.Left := 496;
// config_arcade.GroupBox3.Visible := true;
// while dip_tmp.name <> '' do
// begin
// Combobox_dip[2, dip_pos] := TComboBox.create(config_arcade);
// Combobox_dip[2, dip_pos].Parent := config_arcade.GroupBox3;
// Combobox_dip[2, dip_pos].Left := 100;
// Combobox_dip[2, dip_pos].Top := 25 * dip_pos;
// Combobox_dip[2, dip_pos].TabStop := false;
// Combobox_dip[2, dip_pos].Width := 150;
// Label_dip[2, dip_pos] := TLabel.create(config_arcade);
// Label_dip[2, dip_pos].Parent := config_arcade.GroupBox3;
// Label_dip[2, dip_pos].Left := 5;
// Label_dip[2, dip_pos].Top := 25 * dip_pos;
// Label_dip[2, dip_pos].AutoSize := true;
// if (dip_tmp.number - 1) > MAX_DIP_VALUES then
// MessageDlg('Warning: More Values in DIP C than available!', mtError, [mbOk], 0);
// for h := 0 to dip_tmp.number - 1 do
// begin
// Combobox_dip[2, dip_pos].Items.Add(dip_tmp.dip[h].dip_name);
// if dip_tmp.dip[h].dip_val = (marcade.dswc and dip_tmp.mask) then
// Combobox_dip[2, dip_pos].ItemIndex := h;
// end;
// Label_dip[2, dip_pos].Caption := dip_tmp.name;
// dip_pos := dip_pos + 1;
// if dip_pos > MAX_COMP then
// MessageDlg('Warning: More Values in DIP C than available!', mtError, [mbOk], 0);
// inc(dip_tmp);
// end;
// end;
// // Mostrar ventana
// config_arcade.Show;
// config_arcade.SetFocus;
// while config_arcade.Showing do
// application.ProcessMessages;
//
// end;

{ Tfrm_arcade_config }

//procedure Tfrm_arcade_config.activate_arcade_config;
//begin
//  //
//end;

end.
