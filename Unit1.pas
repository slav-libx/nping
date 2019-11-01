unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
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
  Net.StreamSocket;

type
  TConnection = class
    Client: TStreamSocket;
    ExceptionText: string;
    Address: string;
    NodeServer: string;
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    procedure Connect;
  end;

  TForm1 = class(TForm)
    ListBox: TListBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    Connections: TObjectList<TConnection>;
    function GetConnection(Sender: TObject): TConnection;
    procedure OnConnectionsChange(Sender: TObject; const Connection: TConnection; Action: TCollectionNotification);
    procedure OnClientConnect(Sender: TObject);
    procedure OnClientReceived(Sender: TObject);
    procedure OnClientClose(Sender: TObject);
    procedure OnClientExcept(Sender: TObject);
    procedure UpdateClients;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  ConnectedString: array[Boolean] of string = ('Disconnected','Connected');

constructor TConnection.Create;
begin
  Client:=TStreamSocket.Create;
end;

destructor TConnection.Destroy;
begin
  Client.Terminate;
  Client.Free;
end;

function TConnection.ToString: string;
begin
  Result:=Client.ClassName+' '+Client.RemoteAddress+' '+ConnectedString[Client.Connected]+' '+ExceptionText;
end;

procedure TConnection.Connect;
begin
  Client.Connect(Address,5555);
end;

{ TForm1 }

function TForm1.GetConnection(Sender: TObject): TConnection;
var C: TConnection;
begin
  for C in Connections do
  if C.Client=Sender then Exit(C);
  Result:=nil;
end;

procedure TForm1.Button1Click(Sender: TObject);
var C: TConnection;
begin

  for C in Connections do C.Connect;

//  Client:=TStreamSocket.Create;
//
//  Client.OnConnect:=OnClientConnect;
//  Client.OnReceived:=OnClientReceived;
//  Client.OnClose:=OnClientClose;
//  Client.OnExcept:=OnClientExcept;
//
//  Clients.Add(Client);
//
//  Client.Connect('185.182.193.15',5555);
//
//  Client:=TStreamSocket.Create;
//
//  Client.OnConnect:=OnClientConnect;
//  Client.OnReceived:=OnClientReceived;
//  Client.OnClose:=OnClientClose;
//  Client.OnExcept:=OnClientExcept;
//
//  Clients.Add(Client);
//
//  Client.Connect('190.2.146.129',5555);

end;

constructor TForm1.Create(AOwner: TComponent);
var
  S: string;
  Connection: TConnection;
begin
  inherited;

  Connections:=TObjectList<TConnection>.Create;
  Connections.OnNotify:=OnConnectionsChange;

  for S in TFile.ReadAllLines('connections.txt') do
  begin

    Connection:=TConnection.Create;
    Connection.Address:=S;
    Connection.Client.OnConnect:=OnClientConnect;
    Connection.Client.OnReceived:=OnClientReceived;
    Connection.Client.OnClose:=OnClientClose;
    Connection.Client.OnExcept:=OnClientExcept;

    Connections.Add(Connection);

  end;

  UpdateClients;

  //185.182.193.15
  //185.182.193.16
  //185.182.193.17
  //190.2.146.126
  //190.2.146.129
  //190.2.146.156

end;

destructor TForm1.Destroy;
begin
  Connections.Free;
  inherited;
end;

procedure TForm1.OnClientClose(Sender: TObject);
begin
  UpdateClients;
end;

procedure TForm1.OnClientConnect(Sender: TObject);
begin
  UpdateClients;
end;

procedure TForm1.OnClientExcept(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
  Connection.ExceptionText:=Connection.Client.E.Message;
  UpdateClients;
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
      Connection.Client.Disconnect;
      UpdateClients;

    end;
    end;

  finally
    Packages.Free;
  end;

end;

procedure TForm1.OnConnectionsChange(Sender: TObject; const Connection: TConnection;
  Action: TCollectionNotification);
begin
//  if Action=TCollectionNotification.cnRemoved then Client.Terminate;
//  UpdateClients;
end;

procedure TForm1.UpdateClients;
var C: TConnection;
begin

  if Connections=nil then Exit;

  ListBox.BeginUpdate;

  ListBox.Clear;

  for C in Connections do
  ListBox.Items.Add(C.ToString);

  ListBox.EndUpdate;

end;

end.
