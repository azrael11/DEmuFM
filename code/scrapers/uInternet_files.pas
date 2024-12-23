unit uInternet_files;

interface

uses
  System.Classes,
  System.JSON,
  System.Sysutils,
  System.AnsiStrings,
  System.Net.HttpClientComponent,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Forms,
  WinApi.Windows,
  WinApi.WinInet,
  WinApi.Winsock,
  NB30,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  IdSSLOpenSSL,
  IdExplicitTLSClientServerBase,
  IdFTP,
  IdSMTP,
  IdMessage,
  IdMessageBuilder,
  IdGlobal,
  REST.Client,
  REST.Types,
  System.Net.HttpClient;

const
  HTTP_RESPONSE_OK = 200;

{$M+}
function Internet_Connected: Boolean;

function IP_Get_Address: String;
function GetMACAdress: TStringList;

function ValidEmail(email: string): Boolean;
function GeoIP(out vCountry_Code: string; out vIP: string; out vLat, vLon: String): Boolean;

procedure Create_FTP_Folder(vFolder_Name: String);

function Send_HTML_Email(vEmail, vTheme: String): Boolean;
procedure HTML_Registration(vHTMLBuild: TIdMessageBuilderHtml);
procedure HTML_Password_Forgat(vHTMLBuild: TIdMessageBuilderHtml);

function Get_Image(vPath: String): FMX.Graphics.TBitmap;
function Get_Image_new(vPath: String): FMX.Graphics.TBitmap;
function DownloadImage(const URL: String): FMX.Graphics.TBitmap;

function GetPage(aURL: string): string;

function JSONValue(vRestName: String; vBaseUrl: String; vMethod: TRESTRequestMethod): TJSONValue;

function WebFileSize(const UserAgent, URL: string): cardinal;

type
  TIdHTTPProgress = class(TIdHTTP)
  private
    FProgress: Integer;
    FBytesToTransfer: Int64;
    FOnChange: TNotifyEvent;
    IOHndl: TIdSSLIOHandlerSocketOpenSSL;
    procedure HTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure HTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure SetProgress(const Value: Integer);
    procedure SetOnChance(const Value: TNotifyEvent);
  public
    Constructor Create(AOwner: TComponent);
    procedure DownloadFile(const aFileUrl: string; const aDestinationFile: String);
  published
    property Progress: Integer read FProgress write SetProgress;
    property BytesToTransfer: Int64 read FBytesToTransfer;
    property OnChange: TNotifyEvent read FOnChange write SetOnChance;
  end;

implementation

uses
  main;
{ TIdHTTPProgress }

constructor TIdHTTPProgress.Create(AOwner: TComponent);
begin
  inherited;
  IOHndl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Request.BasicAuthentication := True;
  HandleRedirects := True;
  IOHandler := IOHndl;
  ReadTimeout := 30000;
  OnWork := HTTPWork;
  OnWorkBegin := HTTPWorkBegin;
  OnWorkEnd := HTTPWorkEnd;
end;

procedure TIdHTTPProgress.DownloadFile(const aFileUrl: string; const aDestinationFile: String);
var
  LDestStream: TFileStream;
  aPath: String;
begin
  Progress := 0;
  FBytesToTransfer := 0;
  aPath := ExtractFilePath(aDestinationFile);
  if aPath <> '' then
    ForceDirectories(aPath);

  LDestStream := TFileStream.Create(aDestinationFile, fmCreate);
  try
    Get(aFileUrl, LDestStream);
  finally
    FreeAndNil(LDestStream);
  end;
end;

procedure TIdHTTPProgress.HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if BytesToTransfer = 0 then // No Update File
    Exit;

  Progress := Round((AWorkCount / BytesToTransfer) * 100);
end;

procedure TIdHTTPProgress.HTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  FBytesToTransfer := AWorkCountMax;
end;

procedure TIdHTTPProgress.HTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  FBytesToTransfer := 0;
  Progress := 100;
end;

procedure TIdHTTPProgress.SetOnChance(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TIdHTTPProgress.SetProgress(const Value: Integer);
begin
  FProgress := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function ValidEmail(email: string): Boolean;
const
  atom_chars = [#33 .. #255] - ['(', ')', '<', '>', '@', ',', ';', ':', '\', '/', '"', '.', '[',
    ']', #127];
  quoted_string_chars = [#0 .. #255] - ['"', #13, '\'];
  letters = ['A' .. 'Z', 'a' .. 'z'];
  letters_digits = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z'];
  subdomain_chars = ['-', '0' .. '9', 'A' .. 'Z', 'a' .. 'z'];

type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR, STATE_QUOTE, STATE_LOCAL_PERIOD,
    STATE_EXPECTING_SUBDOMAIN, STATE_SUBDOMAIN, STATE_HYPHEN);

var
  State: States;
  i, n, subdomains: Integer;
  c: char;

begin
  State := STATE_BEGIN;
  n := Length(email);
  i := 1;
  subdomains := 1;

  while (i <= n) do
  begin
    c := email[i];
    case State of
      STATE_BEGIN:
        if c in atom_chars then
          State := STATE_ATOM
        else if c = '"' then
          State := STATE_QTEXT
        else
          break;
      STATE_ATOM:
        if c = '@' then
          State := STATE_EXPECTING_SUBDOMAIN
        else if c = '.' then
          State := STATE_LOCAL_PERIOD
        else if not(c in atom_chars) then
          break;
      STATE_QTEXT:
        if c = '\' then
          State := STATE_QCHAR
        else if c = '"' then
          State := STATE_QUOTE
        else if not(c in quoted_string_chars) then
          break;
      STATE_QCHAR:
        State := STATE_QTEXT;
      STATE_QUOTE:
        if c = '@' then
          State := STATE_EXPECTING_SUBDOMAIN
        else if c = '.' then
          State := STATE_LOCAL_PERIOD
        else
          break;
      STATE_LOCAL_PERIOD:
        if c in atom_chars then
          State := STATE_ATOM
        else if c = '"' then
          State := STATE_QTEXT
        else
          break;
      STATE_EXPECTING_SUBDOMAIN:
        if c in letters then
          State := STATE_SUBDOMAIN
        else
          break;
      STATE_SUBDOMAIN:
        if c = '.' then
        begin
          inc(subdomains);
          State := STATE_EXPECTING_SUBDOMAIN
        end
        else if c = '-' then
          State := STATE_HYPHEN
        else if not(c in letters_digits) then
          break;
      STATE_HYPHEN:
        if c in letters_digits then
          State := STATE_SUBDOMAIN
        else if c <> '-' then
          break;
    end;
    inc(i);
  end;
  if i <= n then
    Result := False
  else
    Result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;

function GeoIP(out vCountry_Code: string; out vIP: string; out vLat, vLon: String): Boolean;
var
  vJSONValue: TJSONValue;
  vPosition: String;
  vOutValue: String;
  vIpos: Integer;
begin
  vJSONValue := JSONValue('IpInfo', 'http://ipinfo.io/json', TRESTRequestMethod.rmGET);

  if vJSONValue.TryGetValue('ip', vOutValue) then
  begin
    vIP := vOutValue;
    if vJSONValue.TryGetValue('country', vOutValue) then
      vCountry_Code := vOutValue;
    if vJSONValue.TryGetValue('loc', vOutValue) then
      vPosition := vOutValue;
    if vPosition <> '' then
    begin
      vIpos := Pos(',', vPosition);
      if vIpos <> 0 then
      begin
        vLat := Trim(Copy(vPosition, 0, vIpos - 1));
        vLon := Trim(Copy(vPosition, vIpos + 1, Length(vPosition) - vIpos));
      end;
    end;
  end
  else
    Result := False;
  FreeAndNil(vJSONValue);
end;

procedure Create_FTP_Folder(vFolder_Name: String);
const
  cPath = '/htdocs/users/int_pros/';
var
  vIdFTP: TIdFTP;
begin
  vIdFTP := TIdFTP.Create(main.frm_main);
  vIdFTP.Host := 'ftpupload.net';
  vIdFTP.Username := 'epiz_23299538';
  vIdFTP.Password := 'u4fbISfU';
  vIdFTP.Port := 21;

  vIdFTP.Connect;

  vIdFTP.ChangeDir(cPath);
  vIdFTP.MakeDir(vFolder_Name);
  vIdFTP.Disconnect;
end;

procedure HTML_Registration(vHTMLBuild: TIdMessageBuilderHtml);
var
  vBackground: string;
  vLogo: String;
begin
  // vBackground := ex_load.Path.Images + 'back.png';
  // vLogo := ex_load.Path.Images + 'logo.png';

  vHTMLBuild.HtmlFiles.Add(vBackground, 'back');
  vHTMLBuild.HtmlFiles.Add(vLogo, 'logo');

  vHTMLBuild.Html.Add('<html>');
  vHTMLBuild.Html.Add('<head>');
  vHTMLBuild.Html.Add('<style>body {' + 'background-image: url("cid:back");' +
    'background-repeat: repeat;' + 'background-color: #cccccc;' + '}</style>');
  vHTMLBuild.Html.Add('</head>');
  vHTMLBuild.Html.Add('<body>Congratulations on your enrollment in the ExtraFE world.<p>');
  vHTMLBuild.Html.Add('<p><img src="cid:logo" alt="Smiley face" height="260" width="500">');
  // vHTMLBuild.Html.Add('<p><b>Username</b> : ' + User_Reg.Username + '<br>');
  // vHTMLBuild.Html.Add('<b>Password</b> : ' + User_Reg.Password + '<p>');
  vHTMLBuild.Html.Add
    ('At the moment we sent you the e-mail the possibilities we provide are limited.<br> In the future we will add more and more services and benefits and of course it will be free forever.<p>');
  vHTMLBuild.Html.Add
    ('<b>Homepage</b>      : <a href="http://extrafe.epizy.com">http://extrafe.epizy.com</a><br>');
  vHTMLBuild.Html.Add
    ('<b>Documentation</b> : <a href="http://extrafe.epizy.com/doc/">http://extrafe.epizy.com/doc/</a><br>');
  vHTMLBuild.Html.Add
    ('<b>Forum</b>         : <a href="http://extrafe.epizy.com/smf/">http://extrafe.epizy.com/smf/</a><p>');
  vHTMLBuild.Html.Add
    ('Sincerely, the owner of ExraFE : <b>Nikos Kordas</b> AKA (azrael11).</body>');
  vHTMLBuild.Html.Add('</html>');
  vHTMLBuild.HtmlCharSet := 'utf-8';
  // if you want to add attachments images
  // vHTMLBuild.HtmlFiles.Add(imgfilename, 'load.png');
  // if you want to add unrelated HTML attachment like PDF file as mail attachment //
  // Htmlbuilder.Attachments.Add('c:\filename.pdf');
end;

procedure HTML_Password_Forgat(vHTMLBuild: TIdMessageBuilderHtml);
var
  vBackground: string;
  vLogo: String;
begin
  // vBackground := ex_load.Path.Images + 'back.png';
  // vLogo := ex_load.Path.Images + 'logo.png';

  vHTMLBuild.HtmlFiles.Add(vBackground, 'back');
  vHTMLBuild.HtmlFiles.Add(vLogo, 'logo');

  vHTMLBuild.Html.Add('<html>');
  vHTMLBuild.Html.Add('<head>');
  vHTMLBuild.Html.Add('<style>');
  vHTMLBuild.Html.Add('<style>body {' + 'background-image: url("cid:back");' +
    'background-repeat: repeat;' + 'background-color: #cccccc;' + '}</style>');
  vHTMLBuild.Html.Add('</head>');
  vHTMLBuild.Html.Add('<body> You forgat your password.<p>');
  vHTMLBuild.Html.Add('<p><img src="cid:logo" alt="" height="260" width="500">');
  vHTMLBuild.Html.Add
    ('<p>This email was sent to you because you asked us to enter the password you forgot.<p>');
  // vHTMLBuild.Html.Add('<p><b>Username</b> : ' + uDB_AUser.Online.Username + '<br>');
  // vHTMLBuild.Html.Add('<b>Password</b> : </b>' + uDB.Query_Select_Online('NUM', 'USERS', uDB_AUser.Local.USER.Num.ToString) + '</b><p>');
  vHTMLBuild.Html.Add
    ('If you continue to have trouble accessing the ExtraFE please with your username send e-mail at: spoooky11@hotmail.gr.<p>');
  vHTMLBuild.Html.Add
    ('<b>Homepage</b>      : <a href="http://extrafe.epizy.com">http://extrafe.epizy.com</a><br>');
  vHTMLBuild.Html.Add
    ('<b>Documentation</b> : <a href="http://extrafe.epizy.com/doc/">http://extrafe.epizy.com/doc/</a><br>');
  vHTMLBuild.Html.Add
    ('<b>Forum</b>         : <a href="http://extrafe.epizy.com/smf/">http://extrafe.epizy.com/smf/</a><p>');
  vHTMLBuild.Html.Add
    ('Sincerely, the owner of ExraFE : <b>Nikos Kordas</b> AKA (azrael11).</body>');
  vHTMLBuild.Html.Add('</html>');
  vHTMLBuild.HtmlCharSet := 'utf-8';
end;

function Send_HTML_Email(vEmail, vTheme: String): Boolean;
var
  vIdSMTP: TIdSMTP;
  vIdMessage: TIdMessage;
  vIOHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
  Htmlbuilder: TIdMessageBuilderHtml;
begin
  Result := False;
  vIdSMTP := TIdSMTP.Create(main.frm_main);
  vIdMessage := TIdMessage.Create(main.frm_main);
  vIOHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create(main.frm_main);

  with vIOHandlerSSL do
  begin
    Destination := 'smtp-mail.outlook.com:587';
    Host := 'smtp-mail.outlook.com';
    MaxLineAction := maException;
    Port := 587;
    SSLOptions.Method := sslvTLSv1;
    SSLOptions.Mode := sslmUnassigned;
    SSLOptions.VerifyMode := [];
    SSLOptions.VerifyDepth := 0;
  end;

  with vIdSMTP do
  begin
    Host := 'smtp-mail.outlook.com';
    Port := 587;
    Username := 'spoooky11@hotmail.gr';
    Password := '11spoooky';
    IOHandler := vIOHandlerSSL;
    AuthType := satDefault;
    UseTLS := utUseExplicitTLS;
  end;

  try
    Htmlbuilder := TIdMessageBuilderHtml.Create;
    try
      if vTheme = 'register_user' then
        HTML_Registration(Htmlbuilder)
      else if vTheme = 'forgat_password' then
        HTML_Password_Forgat(Htmlbuilder);
      vIdMessage := Htmlbuilder.NewMessage(nil);
      vIdMessage.From.Address := 'spoooky11@hotmail.gr';
      if vTheme = 'register_user' then
        vIdMessage.Subject := 'Thank you for your registration to ExtraFE'
      else if vTheme = 'forgat_password' then
        vIdMessage.Subject := 'ExtraFE, revive forgaten password';
      vIdMessage.Priority := mpHigh;
      with vIdMessage.Recipients.Add do
      begin
        // Name := User_Reg.Username;
        // Address := vEmail;
      end;

      vIdSMTP.Connect();
      try
        vIdSMTP.Send(vIdMessage);
        Result := True;
      finally
        vIdSMTP.Disconnect();
      end;

    finally
      Htmlbuilder.Free();
    end;

  except
    on E: Exception do
      // ShowMessage('Failed: ' + E.Message);
  end;
end;

function Get_Image(vPath: String): FMX.Graphics.TBitmap;
var
  MS: TMemoryStream;
  vIdHTTP: TIdHTTP;
  vIdSSLOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  MS := TMemoryStream.Create;
  Result := FMX.Graphics.TBitmap.Create;
  vIdHTTP := TIdHTTP.Create(nil); // ��� ����� ������ �� ������ Owner ����� �� ����������.
  vIdSSLOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    // ������� ��� SSL IOHandler
    vIdSSLOpenSSL.SSLOptions.SSLVersions := [sslvTLSv1_2]; // �������� TLSv1.3
    vIdHTTP.IOHandler := vIdSSLOpenSSL;

    // ����������� ��� ������
    vIdHTTP.Get(vPath, MS);
    MS.Seek(0, soFromBeginning);

    // ������� ��� ������� ��� �� MemoryStream
    Result.LoadFromStream(MS);
  finally
    // ������������ �����
    FreeAndNil(MS);
    FreeAndNil(vIdHTTP);
    FreeAndNil(vIdSSLOpenSSL);
  end;
end;
function GetPage(aURL: string): string;
var
  Response: TStringStream;
  HTTP: TIdHTTP;
begin
  Result := '';
  Response := TStringStream.Create('');
  try
    HTTP := TIdHTTP.Create(nil);
    try
      HTTP.Get(aURL, Response);
      if HTTP.ResponseCode = HTTP_RESPONSE_OK then
      begin
        Result := Response.DataString;
      end
      else
      begin
        // TODO -cLogging: add some logging
      end;
    finally
      HTTP.Free;
    end;
  finally
    Response.Free;
  end;
end;

function JSONValue(vRestName: String; vBaseUrl: String; vMethod: TRESTRequestMethod): TJSONValue;
var
  vRESTClient: TRESTClient;
  vRESTRequest: TRESTRequest;
  vRESTResponse: TRESTResponse;
begin
  Result := TJSONValue.Create;
  try
    try
      vRESTClient := TRESTClient.Create('');
      vRESTClient.Name := vRestName + '_RestClient';
      vRESTClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
      vRESTClient.AcceptCharset := 'utf-8, *;q=0.8';
      vRESTClient.BaseURL := vBaseUrl;
      vRESTClient.FallbackCharsetEncoding := 'utf-8';
      vRESTClient.AutoCreateParams := True;

      vRESTResponse := TRESTResponse.Create(vRESTClient);
      vRESTResponse.Name := vRestName + '_Response';

      vRESTRequest := TRESTRequest.Create(vRESTClient);
      vRESTRequest.Name := vRestName + '_Request';
      vRESTRequest.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
      vRESTRequest.AcceptCharset := 'utf-8, *;q=0.8';
      vRESTRequest.Client := vRESTClient;
      vRESTRequest.AutoCreateParams := True;
      vRESTRequest.Method := vMethod;
      vRESTRequest.Response := vRESTResponse;
      vRESTRequest.Timeout := 30000;

      vRESTRequest.Execute;

    except
      on E: Exception do
      begin
        { Show a message if Rest Request goes wrong }
        // ShowMessage(E.ToString);
      end;
    end;

  finally
    Result := vRESTResponse.JSONValue;
    FreeAndNil(vRESTRequest)
  end;

end;

function Internet_Connected: Boolean;
const
  // Local system has a valid connection to the Internet, but it might or might
  // not be currently connected.
  INTERNET_CONNECTION_CONFIGURED = $40;
  // Local system uses a local area network to connect to the Internet.
  INTERNET_CONNECTION_LAN = $02;
  // Local system uses a modem to connect to the Internet
  INTERNET_CONNECTION_MODEM = $01;
  // Local system is in offline mode.
  INTERNET_CONNECTION_OFFLINE = $20;
  // Local system uses a proxy server to connect to the Internet
  INTERNET_CONNECTION_PROXY = $04;
  // Local system has RAS installed.
  INTERNET_RAS_INSTALLED = $10;
var
  InetState: DWORD;
  hHttpSession, hReqUrl: HInternet;
begin
  try
    Result := InternetGetConnectedState(@InetState, 0);
    if (Result and (InetState and INTERNET_CONNECTION_CONFIGURED = INTERNET_CONNECTION_CONFIGURED))
    then
    begin
      // so far we ONLY know there's a valid connection. See if we can grab some
      // known URL ...
      hHttpSession := InternetOpen(PChar(Application.Title), INTERNET_OPEN_TYPE_PRECONFIG,
        nil, nil, 0);
      try
        hReqUrl := InternetOpenURL(hHttpSession,
          PChar('http://www.google.com' { the URL to check } ), nil, 0, 0, 0);
        Result := hReqUrl <> nil;
        InternetCloseHandle(hReqUrl);
      finally
        InternetCloseHandle(hHttpSession);
      end;
    end
    else if (InetState and INTERNET_CONNECTION_OFFLINE = INTERNET_CONNECTION_OFFLINE) then
      Result := False // we know for sure we are offline.
  except
    on E: Exception do
    begin
      // ShowMessage(E.ToString);
    end;
  end;
end;

function IP_Get_Address: String;
type
  TaPInAddr = array [0 .. 10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array [0 .. 63] of Ansichar;
  i: Integer;
  GInitData: TWSADATA;
begin
  WSAStartup($101, GInitData);
  Result := '';
  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(Buffer);
  if phe = nil then
    Exit;
  pptr := PaPInAddr(phe^.h_addr_list);
  i := 0;
  while pptr^[i] <> nil do
  begin
    Result := System.AnsiStrings.StrPas(inet_ntoa(pptr^[i]^));
    inc(i);
  end;
  WSACleanup;
end;

function GetMACAdress: TStringList;
var
  NCB: PNCB;
  Adapter: PAdapterStatus;

  RetCode: Ansichar;
  i: Integer;
  Lenum: PlanaEnum;
  _SystemID: string;

begin
  Result := TStringList.Create();
  _SystemID := '';
  Getmem(NCB, SizeOf(TNCB));
  Fillchar(NCB^, SizeOf(TNCB), 0);

  Getmem(Lenum, SizeOf(TLanaEnum));
  Fillchar(Lenum^, SizeOf(TLanaEnum), 0);

  Getmem(Adapter, SizeOf(TAdapterStatus));
  Fillchar(Adapter^, SizeOf(TAdapterStatus), 0);

  Lenum.Length := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer := Pointer(Lenum);
  NCB.ncb_length := SizeOf(Lenum);
  RetCode := Netbios(NCB);

  try
    i := 0;
    repeat
      Fillchar(NCB^, SizeOf(TNCB), 0);
      NCB.ncb_command := chr(NCBRESET);
      NCB.ncb_lana_num := Lenum.lana[i];
      RetCode := Netbios(NCB);

      Fillchar(NCB^, SizeOf(TNCB), 0);
      NCB.ncb_command := chr(NCBASTAT);
      NCB.ncb_lana_num := Lenum.lana[i];
      // Must be 16
      NCB.ncb_callname := '*               ';

      NCB.ncb_buffer := Pointer(Adapter);

      NCB.ncb_length := SizeOf(TAdapterStatus);
      RetCode := Netbios(NCB);
      // ---- calc _systemId from mac-address[2-5] XOR mac-address[1]...
      if (RetCode = chr(0)) or (RetCode = chr(6)) then
      begin
        _SystemID := IntToHex(Ord(Adapter.adapter_address[0]), 2) + '-' +
          IntToHex(Ord(Adapter.adapter_address[1]), 2) + '-' +
          IntToHex(Ord(Adapter.adapter_address[2]), 2) + '-' +
          IntToHex(Ord(Adapter.adapter_address[3]), 2) + '-' +
          IntToHex(Ord(Adapter.adapter_address[4]), 2) + '-' +
          IntToHex(Ord(Adapter.adapter_address[5]), 2);

        if (_SystemID <> '00-00-00-00-00-00') and (Result.IndexOf(_SystemID) = -1) then
          Result.Add(_SystemID);
      end;
      inc(i);
    until (i >= Ord(Lenum.Length));
  finally
    FreeMem(NCB);
    FreeMem(Adapter);
    FreeMem(Lenum);
  end;
end;

function WebFileSize(const UserAgent, URL: string): cardinal;
var
  hInet, hURL: HInternet;
  len: cardinal;
  index: cardinal;
begin
  Result := cardinal(-1);
  hInet := InternetOpen(PChar(UserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  index := 0;
  if hInet <> nil then
    try
      hURL := InternetOpenURL(hInet, PChar(URL), nil, 0, 0, 0);
      if hURL <> nil then
        try
          len := SizeOf(Result);
          if not HttpQueryInfo(hURL, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @Result,
            len, index) then
            RaiseLastOSError;
        finally
          InternetCloseHandle(hURL);
        end;
    finally
      InternetCloseHandle(hInet)
    end;
end;

function Get_Image_new(vPath: String): FMX.Graphics.TBitmap;
var
  vMem: TMemoryStream;
  vNet: TNetHTTPClient;
  vSSLProtocols : THTTPSecureProtocols;
begin

  vMem := TMemoryStream.Create;
  vNet := TNetHTTPClient.Create(main.frm_main);
  vNet.HandleRedirects := True;
  vNet.SecureProtocols :=  vSSLProtocols;

  vMem.Position := 0;

  try
    vNet.Get(vPath, vMem);
    Result := FMX.Graphics.TBitmap.Create;

    Result.LoadFromStream(vMem);

  finally
    FreeAndNil(vMem);
    vNet.Free;
  end;
end;

function DownloadImage(const URL: String): FMX.Graphics.TBitmap;
var
  MemoryStream: TMemoryStream;
  HttpClient: THttpClient;
  Response: IHTTPResponse;
begin
  Result := nil; // ������������ ��� ������������� �� nil
  MemoryStream := TMemoryStream.Create;
  HttpClient := THttpClient.Create;
  try
    try
      // ����������� ��� ������ ��� �� URL
      Response := HttpClient.Get(URL, MemoryStream);
      if Response.StatusCode = 200 then
      begin
        MemoryStream.Position := 0; // ��������� ��� ���� 0 ��� ��������
        Result := FMX.Graphics.TBitmap.Create;
        try
          Result.LoadFromStream(MemoryStream);
        except
          FreeAndNil(Result); // �� ��������, ����������� �� �����
          raise; // �������� ���� �� exception
        end;
      end
      else
        raise Exception.CreateFmt('Failed to download image. HTTP Status: %d', [Response.StatusCode]);
    except
      on E: Exception do
      begin
        raise Exception.CreateFmt('Error downloading image: %s', [E.Message]);
      end;
    end;
  finally
    MemoryStream.Free;
    HttpClient.Free;
  end;
end;

end.
