unit Connection.Types;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils,
  FMX.Types,
  UCustomMemoryStream,
  Net.Socket,
  Net.StreamSocket,
  Connection.Intf;

type

  TNode = record
    ID: Integer;
    AccountID: Int64;
    OS: Integer;
    Address: string;
    Binded: Boolean;
    Server: string;
    AcceptTime: TDateTime;
    Connected: Boolean;
    class operator Implicit(const S: string): TNode;
    function GetOSName: string;
  end;

  TNodeEvent = procedure(const Node: TNode) of object;

  TConnection = class
  public type
    TState = (None,Connecting,Connected,Excepted,Aborted,OK);
  private
    FTimeout: TTimer;
    procedure OnTimeout(Sender: TObject);
  private
    Connections: TObjectList<TConnection>;
    FState: TState;
    FOnLog: TGetStrProc;
    FOnNode: TNodeEvent;
    procedure OnConnectionsNotify(Sender: TObject; const Item: TConnection; Action: TCollectionNotification);
    procedure ToLog(const Text: string);
    procedure DoNode(const Node: TNode);
    procedure SetState(Value: TState);
    procedure SetTimeout(Interval: Cardinal);
    procedure OnClientConnect(Sender: TObject);
    procedure OnClientReceived(Sender: TObject);
    procedure OnClientClose(Sender: TObject);
    procedure OnClientExcept(Sender: TObject);
  public
    Client: TStreamSocket;
    ExceptionText: string;
    Address: string;
    NodeServer: string;
    Network: string;
    Networks: string;
    Servers: string;
    RequestNetworks: Boolean;
    [weak]Content: IConnectionContent;
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    property State: TState read FState write SetState;
    property OnLog: TGetStrProc read FOnLog write FOnLog;
    property OnNode: TNodeEvent read FOnNode write FOnNode;
  end;

implementation

const
  CHANGE_SERVER = 701;
  NET_CLIENTS = 721;
  NET_SERVERS = 722;

class operator TNode.Implicit(const S: string): TNode;
begin

  var V:=(S+':::').Split([':']);

  Result.ID:=StrToInt64Def(V[3],0);
  Result.AccountID:=StrToInt64Def(V[2],0);
  Result.Address:=V[0];
  Result.Binded:=V[1]='1';
  Result.Server:='';
  Result.AcceptTime:=UnixToDateTime(StrToInt64Def(V[4],0),False);
  Result.OS:=StrToIntDef(V[5],0);

end;

function TNode.GetOSName: string;
begin

  Result:=OS.ToString;

  case OS of
  0: Result:='indef';
  1: Result:='Windows';
  2: Result:='MacOS';
  3: Result:='iOS';
  4: Result:='Android';
  5: Result:='WinRT';
  6: Result:='Linux';
  end;

end;

constructor TConnection.Create;
begin

  Connections:=TObjectList<TConnection>.Create;
  Connections.OnNotify:=OnConnectionsNotify;

  RequestNetworks:=False;

  Client:=TStreamSocket.Create;
  Client.OnConnect:=OnClientConnect;
  Client.OnReceived:=OnClientReceived;
  Client.OnClose:=OnClientClose;
  Client.OnExcept:=OnClientExcept;

  FTimeout:=TTimer.Create(nil);
  FTimeout.Enabled:=False;
  FTimeout.OnTimer:=OnTimeout;

end;

destructor TConnection.Destroy;
begin
  Connections.Free;
  if Assigned(Client) then
  begin
    Client.Terminate;
    Client.Free;
  end;
  FTimeout.Free;
end;

procedure TConnection.OnConnectionsNotify(Sender: TObject; const Item: TConnection;
  Action: TCollectionNotification);
begin
  if Connections.Count=0 then
//  beep;
end;

procedure TConnection.OnTimeout(Sender: TObject);
begin
  SetTimeout(0);
  ExceptionText:='Timeout';
  State:=Aborted;
  ToLog('Timeout');
  Client.Disconnect;
end;

procedure TConnection.SetTimeout(Interval: Cardinal);
begin
  FTimeout.Interval:=Interval;
  FTimeout.Enabled:=False;
  FTimeout.Enabled:=Interval>0;
end;

procedure TConnection.SetState(Value: TState);
begin

  FState:=Value;

  if Assigned(Content) then

  case State of
  None: Content.SetInformation('Unknown');
  Connecting: Content.SetInformation('Connecting...');
  Connected: Content.SetInformation('Connected (wait response...)');
  Excepted: Content.SetException(ExceptionText);
  Aborted: Content.SetInformation('Aborted');
  OK: Content.SetOK(NodeServer);
  end;

end;

procedure TConnection.ToLog(const Text: string);
begin
  if Assigned(FOnLog) then FOnLog(Text);
end;

procedure TConnection.DoNode(const Node: TNode);
begin
  if Assigned(FOnNode) then FOnNode(Node);
end;

procedure TConnection.Connect;
begin
  ToLog('Connection... to '+Address);
  State:=Connecting;
  ExceptionText:='';
  NodeServer:='';
  Networks:='';
  Client.Connect(Address,5555);
end;

procedure TConnection.OnClientClose(Sender: TObject);
begin
  SetTimeout(0);
  if State=Connected then State:=Aborted;
  ToLog('Disconnected '+Address);
end;

procedure TConnection.OnClientConnect(Sender: TObject);
begin
  if State=Connecting then State:=Connected;
  SetTimeout(5000);
  ToLog('Connected to '+Address);
end;

procedure TConnection.OnClientExcept(Sender: TObject);
begin
  ExceptionText:=Client.E.Message;
  State:=Excepted;
  ToLog(Client.E.Message);
end;

procedure TConnection.OnClientReceived(Sender: TObject);
var
  Packages: TPackagesList;
  Package: TPackage;
  Connection: TConnection;
  Node: TNode;
  S: string;
begin

  Packages:=TPackagesList.Create;
  try

    Client.DataStream.StreamToList(Packages);

    for Package in Packages do
    case Package.typ of

    CHANGE_SERVER:
    begin

      S:=TEncoding.ANSI.GetString(TBytes(Package.obj));

      Network:='';

      var A:=S.Split([' ']);

      if Length(A)>1 then
      begin
        Network:=A[0];
        S:=A[1];
      end;

      NodeServer:=S;
      State:=OK;

      if not RequestNetworks then
      begin

        SetTimeout(0);

        Client.Disconnect;

        ToLog('server of '+Address+': '+NodeServer);

      end else begin

        SetTimeout(5000);

        if Network<>'' then
        begin

          ToLog('Getting servers...');

          Client.SendPackage(nil,NET_SERVERS);

        end else begin

          ToLog('Getting network...');

          Client.SendPackage(nil,NET_CLIENTS);

        end;

      end;

    end;

    NET_SERVERS:
    begin

      Servers:=TEncoding.ANSI.GetString(TBytes(Package.obj));

      ToLog('Getting network...');

      Client.SendPackage(nil,NET_CLIENTS);

    end;

    NET_CLIENTS:
    begin

      SetTimeout(0);

      Networks:=TEncoding.ANSI.GetString(TBytes(Package.obj));
      Client.Disconnect;

      ToLog(
        'server of '+Address+': '+NodeServer+#13#10+
        'network of '+Address+': '#13#10+Networks.Replace(' ',#13#10));

      for S in Networks.Split([' ']) do
      begin

        Node:=S;
        Node.Server:=Address;
        Node.Connected:=True;

        DoNode(Node);

        if Address<>Node.Address then
        if Node.Binded then
        begin
          Connection:=TConnection.Create;
          Connection.RequestNetworks:=True;
          Connection.Address:=Node.Address;
          Connection.OnLog:=OnLog;
          Connection.OnNode:=OnNode;
          Connection.Connect;
          Connections.Add(Connection);
        end;
      end;

    end;

    end;

  finally
    Packages.Free;
  end;

end;

end.
