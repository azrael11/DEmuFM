unit ulang;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TLANGUAGE_ACTIONS = class
  private

  protected

  public
    function getTransString(recNo: integer): string;
    function getTransStringPop(recNo: integer): string;

    constructor create;
    destructor destroy;

  end;

const
  lang_names: array [0 .. 7] of string = ('ellinika', 'catallan', 'castellano', 'english', 'francais', 'german', 'brazil', 'italian');

var
  lang: TLANGUAGE_ACTIONS;

implementation

uses
  umain_config,
  uDataModule;

{ TLANGUAGE_ACTIONS }

constructor TLANGUAGE_ACTIONS.create;
begin
  inherited;

end;

destructor TLANGUAGE_ACTIONS.destroy;
begin
  inherited;
end;

function TLANGUAGE_ACTIONS.getTransString(recNo: integer): string;
begin
  dm.tLanguage.Active := true;
  dm.tLanguage.recNo := recNo + 1;
  case dm.tConfiglang.AsInteger of
    0:
      result := dm.tLanguageellinika.AsString;
    1:
      result := dm.tLanguagespain.AsString;
    2:
      result := dm.tLanguagerussian.AsString;
    3:
      result := dm.tLanguageenglish.AsString;
    4:
      result := dm.tLanguagefrancais.AsString;
    5:
      result := dm.tLanguagegerman.AsString;
    6:
      result := dm.tLanguagebrazil.AsString;
    7:
      result := dm.tLanguageitalian.AsString;
  end;
  dm.tLanguage.Active := false;
end;

function TLANGUAGE_ACTIONS.getTransStringPop(recNo: integer): string;
begin
  dm.tLanguagePop.Active := true;
  dm.tLanguagePop.recNo := recNo;
  case dm.tConfiglang.AsInteger of
    0:
      result := dm.tLanguagePopellinika.AsString;
    1:
      result := dm.tLanguagePopcatallan.AsString;
    2:
      result := dm.tLanguagePopcastellano.AsString;
    3:
      result := dm.tLanguagePopenglish.AsString;
    4:
      result := dm.tLanguagePopfrancais.AsString;
    5:
      result := dm.tLanguagePopgerman.AsString;
    6:
      result := dm.tLanguagePopbrazil.AsString;
    7:
      result := dm.tLanguagePopitalian.AsString;
  end;
  dm.tLanguagePop.Active := true;
end;

end.
