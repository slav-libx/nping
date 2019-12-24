unit Unit3;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  System.Generics.Collections,
  UCustomMemoryStream,
  Net.Socket,
  Net.StreamSocket, FMX.Objects, FMX.Edit, FMX.ComboEdit;

type
  TContent = record
    ContentText: string;
    procedure SetContentText(const S: string);
  end;

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
    NodePort: Word;
    NodeServer: string;
    Content: TContent;
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    property State: TState read FState write SetState;
  end;

  TForm1 = class(TForm)
    VertScrollBox1: TVertScrollBox;
    Button1: TButton;
    Timer1: TTimer;
    Layout1: TLayout;
    Label1: TLabel;
    Rectangle1: TRectangle;
    ComboEdit1: TComboEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Layout1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    Connections: TObjectList<TConnection>;
    procedure DoNeedUpdate;
    procedure AddConnection(const Address: string);
    function GetConnection(Socket: TObject): TConnection;
    function CompareConnections: Boolean;
    procedure OnClientConnect(Sender: TObject);
    procedure OnClientReceived(Sender: TObject);
    procedure OnClientClose(Sender: TObject);
    procedure OnClientExcept(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TContent.SetContentText(const S: string);
begin
  ContentText:=S;
end;

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
  None: Content.SetContentText('Unknown');
  Connecting: Content.SetContentText('Connecting...');
  Connected: Content.SetContentText('Connected (wait response...)');
  Excepted: Content.SetContentText(ExceptionText);
  Aborted: Content.SetContentText('Aborted');
  OK: Content.SetContentText(Address);
  end;

end;

procedure TConnection.Connect;
begin
  State:=Connecting;
  ExceptionText:='';
  NodeServer:='';
  Client.Connect(Address,NodePort);
end;

function TForm1.CompareConnections: Boolean;
var S: string;
begin
  Result:=True;
  if Connections.Count>0 then S:=Connections[0].Content.ContentText;
  for var C in Connections do
  if C.Content.ContentText<>S then Exit(False);
end;

procedure TForm1.DoNeedUpdate;
begin
  Layout1.Repaint;
  if CompareConnections then
    Rectangle1.Fill.Color:=claGreen
  else
    Rectangle1.Fill.Color:=claRed;
end;

procedure TForm1.Button1Click(Sender: TObject);
var I: Integer;
begin
  //for I:=0 to 200 do AddConnection('185.182.193.15'); '190.2.154.76'
  for I:=0 to StrToInt(Edit1.Text) do AddConnection(ComboEdit1.Text);
  Layout1.Height:=Connections.Count*20;
  Label1.Text:=Connections.Count.ToString;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  Connections:=TObjectList<TConnection>.Create;
end;

destructor TForm1.Destroy;
begin
  Connections.Free;
  inherited;
end;

procedure TForm1.AddConnection(const Address: string);
var Connection: TConnection;
begin

  if Address.IsEmpty then Exit;

  Connection:=TConnection.Create;
  Connection.Address:=Address;
  Connection.NodePort:=StrToInt(Edit2.Text);
  Connection.Client.OnConnect:=OnClientConnect;
  Connection.Client.OnReceived:=OnClientReceived;
  Connection.Client.OnClose:=OnClientClose;
  Connection.Client.OnExcept:=OnClientExcept;
  Connection.Content.ContentText:=Address;
  Connection.State:=None;

  Connections.Add(Connection);

end;

function TForm1.GetConnection(Socket: TObject): TConnection;
var C: TConnection;
begin
  for C in Connections do
  if C.Client=Socket then Exit(C);
  Result:=nil;
end;

procedure TForm1.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var R,V: TRectF;
begin

  R:=ARect;
  R.Height:=20;

  V:=TRectF.Create(VertScrollBox1.ViewportPosition,VertScrollBox1.ClientWidth,VertScrollBox1.ClientHeight);

  Canvas.Fill.Color:=claBlack;
  Canvas.Fill.Kind:=TBrushKind.Solid;

  for var C in Connections do
  begin
    if V.IntersectsWith(R) then
      Canvas.FillText(R,C.Content.ContentText,False,1,[],TTextAlign.Leading,TTextAlign.Center);
    R.Offset(0,R.Height);
  end;

end;

procedure TForm1.OnClientClose(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
  if Connection.State=Connected then Connection.State:=Aborted;
  DoNeedUpdate;
end;

procedure TForm1.OnClientConnect(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
//  Connection.Client.Send('Hello, Oleg!');
  if Connection.State=Connecting then Connection.State:=Connected;
  DoNeedUpdate;
end;

procedure TForm1.OnClientExcept(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=GetConnection(Sender);
  Connection.ExceptionText:=Connection.Client.E.Message;
  Connection.State:=Excepted;
  DoNeedUpdate;
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

      if Connection.NodeServer<>Connection.Address then
      begin
        Connection.Address:=Connection.NodeServer;
        Connection.Connect;
      end;

      DoNeedUpdate;

    end;
    end;

  finally
    Packages.Free;
  end;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  for var C in Connections do
  if (C.State<>Connecting) and not C.Client.Connected then C.Connect;
end;

end.
