unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  UCustomMemoryStream,
  Net.Socket,
  Net.StreamSocket,
  FMX.Objects, FMX.Edit, FMX.SearchBox,
  FMX.DialogService,
  Unit2;

type
  TConnection = class
  public type
    TState = (None,Connecting,Connected,Excepted,Aborted,OK);
  private
    FState: TState;
    procedure SetState(Value: TState);
  public
    Client: TStreamSocket;
    ExceptionText: string;
    Address: string;
    NodeServer: string;
    [weak]Content: TItemFrame;
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    property State: TState read FState write SetState;
  end;

  TForm1 = class(TForm)
    ListBox: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    ConnectionsFileName: string;
    Connections: TObjectList<TConnection>;
    procedure AddConnection(const Address: string);
    function GetConnection(Socket: TObject): TConnection;
    procedure OnClientConnect(Sender: TObject);
    procedure OnClientReceived(Sender: TObject);
    procedure OnClientClose(Sender: TObject);
    procedure OnClientExcept(Sender: TObject);
    procedure SaveConnections;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

constructor TConnection.Create;
begin
  Client:=TStreamSocket.Create;
end;

destructor TConnection.Destroy;
begin
  Client.Terminate;
  Client.Free;
end;

procedure TConnection.SetState(Value: TState);
begin

  FState:=Value;

  case State of
  None: Content.SetInformation('Unknown');
  Connecting: Content.SetInformation('Connecting...');
  Connected: Content.SetInformation('Connected (wait response...)');
  Excepted: Content.SetException(ExceptionText);
  Aborted: Content.SetInformation('Aborted');
  OK: Content.SetOK(NodeServer);
  end;

end;

procedure TConnection.Connect;
begin
  State:=Connecting;
  ExceptionText:='';
  NodeServer:='';
  Client.Connect(Address,5555);
end;

{ TForm1 }

procedure TForm1.SaveConnections;
var
  C: TConnection;
  S: string;
begin
  S:='';
  for C in Connections do S:=S+C.Address+#13#10;
  TFile.WriteAllText(ConnectionsFileName,S.Trim);
end;

function TForm1.GetConnection(Socket: TObject): TConnection;
var C: TConnection;
begin
  for C in Connections do
  if C.Client=Socket then Exit(C);
  Result:=nil;
end;

procedure TForm1.Button1Click(Sender: TObject);
var C: TConnection;
begin
  for C in Connections do
  if C.State<>Connecting then
  begin
    if C.State=Connected then C.Client.Disconnect;
    C.Connect;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin

  //TDialogService.InputQuery('Add Node Address',['111'],['222'],
  TDialogService.InputQuery('Add Node Address',{$IFDEF MSWINDOWS}['Address']{$ELSE}['']{$ENDIF},[''],
  procedure(const AResult: TModalResult; const AValues: array of string)
  var S: string;
  begin
    if AResult=mrOk then
    begin
      for S in AValues do AddConnection(S);
      ListBox.ItemIndex:=ListBox.Items.Count-1;
      SaveConnections;
    end;
  end);

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(ListBox.Selected) then
  begin
    ListBox.Items.Delete(Connections.Remove(TItemFrame(ListBox.Selected.TagObject).TagObject as TConnection));
    SaveConnections;
  end;
end;

procedure TForm1.AddConnection(const Address: string);
var
  Connection: TConnection;
  Item: TListBoxItem;
  Content: TItemFrame;
begin

  if Address.IsEmpty then Exit;

  Connection:=TConnection.Create;
  Connection.Address:=Address;
  Connection.Client.OnConnect:=OnClientConnect;
  Connection.Client.OnReceived:=OnClientReceived;
  Connection.Client.OnClose:=OnClientClose;
  Connection.Client.OnExcept:=OnClientExcept;

  Item:=ListBox.ItemByIndex(ListBox.Items.Add(''));
  {$IFDEF MSWINDOWS}Item.Height:=36;{$ENDIF}

  Content:=TItemFrame.Create(Item);
  Content.Parent:=Item;
  Content.Label1.Text:=Connection.Address;
  {$IFDEF ANDROID}Content.Label1.Width:=140;{$ENDIF}
  Content.TagObject:=Connection;

  Item.TagObject:=Content;

  Connections.Add(Connection);

  Connection.Content:=Content;
  Connection.State:=None;

end;

constructor TForm1.Create(AOwner: TComponent);
var
  S: string;
  Connection: TConnection;
  Item: TListBoxItem;
begin
  inherited;

  {$IFDEF MSWINDOWS}
  ConnectionsFileName:=System.IOUtils.TPath.GetLibraryPath;
  {$ELSE}
  ConnectionsFileName:=System.IOUtils.TPath.GetDocumentsPath;
  {$ENDIF}

  ConnectionsFileName:=System.IOUtils.TPath.Combine(ConnectionsFileName,'connections.txt');

  Connections:=TObjectList<TConnection>.Create;

  if not TFile.Exists(ConnectionsFileName) then
  TFile.WriteAllText(ConnectionsFileName,
    '185.182.193.15'#13#10+
    '185.182.193.16'#13#10+
    '185.182.193.17'#13#10+
    '190.2.146.126'#13#10+
    '190.2.146.129'#13#10+
    '190.2.146.156');

  for S in TFile.ReadAllLines(ConnectionsFileName) do AddConnection(S);

end;

destructor TForm1.Destroy;
begin
  Connections.Free;
  inherited;
end;

procedure TForm1.OnClientClose(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
  if Connection.State=Connected then Connection.State:=Aborted;
end;

procedure TForm1.OnClientConnect(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
  if Connection.State=Connecting then Connection.State:=Connected;
end;

procedure TForm1.OnClientExcept(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
  Connection.ExceptionText:=Connection.Client.E.Message;
  Connection.State:=Excepted;
end;

procedure TForm1.OnClientReceived(Sender: TObject);
var
  Packages: TPackagesList;
  Package: TPackage;
  Connection: TConnection;
begin

  Connection:=GetConnection(Sender);

  Packages:=TPackagesList.Create;
  try

    Connection.Client.DataStream.StreamToList(Packages);

    for Package in Packages do
    case Package.typ of
    701:
    begin

      Connection.NodeServer:=TEncoding.ANSI.GetString(TBytes(Package.obj));
      Connection.State:=OK;
      Connection.Client.Disconnect;

    end;
    end;

  finally
    Packages.Free;
  end;

end;

end.
