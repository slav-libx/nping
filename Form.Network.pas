unit Form.Network;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  System.Math,
  System.DateUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects,
  FMX.Layouts,
  Connection.Types;

type

  TDrawNode = record
    Node: TNode;
    Rect: TRectF;
    Bottom: Single;
    FillColor: TAlphaColor;
    StrokeColor: TAlphaColor;
  end;

  TNetworkForm = class(TForm)
    ScrollBox1: TScrollBox;
    Memo1: TRectangle;
    procedure FormDestroy(Sender: TObject);
    procedure Memo1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
  private
    Connection: TConnection;
    RootNode: TDrawNode;
    Nodes: array of TDrawNode;
    ServersCount: Integer;
    ClientsCount: Integer;
    ConnectionsCount: Integer;
    procedure OnLog(const Text: string);
    procedure OnNode(const Node: TNode);
    procedure RecalcNodes;
    procedure DrawNode(Canvas: TCanvas; const Node: TDrawNode);
  public
    procedure Get(const Address: string);
  end;

var
  NetworkForm: TNetworkForm;

implementation

{$R *.fmx}

const
  NODE_SERVER = $FFDEFAD5;
  NODE_LOGIN = $FFF3FCC4;
  NODE_GRAY = $FFECECEC;
  GRAY_LOGIN = $FFD1EFFE;
  NODE_CONNECTED = claBlack;
  NODE_DISCONNECTED = claRed;

procedure TNetworkForm.FormCreate(Sender: TObject);
begin
  Connection:=TConnection.Create;
  Connection.RequestNetworks:=True;
//  Connection.OnLog:=OnLog;
  Connection.OnNode:=OnNode;
end;

procedure TNetworkForm.FormDestroy(Sender: TObject);
begin
  Connection.Free;
end;

procedure TNetworkForm.OnLog(const Text: string);
begin
//  Memo1.Lines.Add(Text);
end;

procedure TNetworkForm.OnNode(const Node: TNode);
const Binded: array[Boolean] of string=('-','+');
var DrawNode: TDrawNode;
begin
  DrawNode.Node:=Node;
  Nodes:=Nodes+[DrawNode];
  RecalcNodes;
  Memo1.Repaint;
//  Memo1.Lines.Add(Binded[Node.Binded]+Node.Address+' '+Node.Server+' '+
//    Node.AccountID.ToString+' '+DateTimeToStr(Node.AcceptTime));
end;

procedure TNetworkForm.Get(const Address: string);
begin

  ServersCount:=0;
  ClientsCount:=0;
  ConnectionsCount:=0;

  ScrollBox1.ViewportPosition:=TPointF.Zero;

  RootNode:=Default(TDrawNode);
  RootNode.FillColor:=NODE_LOGIN;
  RootNode.StrokeColor:=NODE_CONNECTED;
  RootNode.Node.Address:=Address;
  RootNode.Node.Binded:=True;
  RootNode.Node.Connected:=True;

  Nodes:=nil;

  Connection.Address:=Address;
  Connection.Connect;

  Show;

end;

procedure TNetworkForm.RecalcNodes;

function GetServerNode(const Server: string): Integer;
var I: Integer;
begin
  Result:=-1;
  for I:=0 to High(Nodes) do
  if Nodes[I].Node.Connected then
  if Nodes[I].Node.Address=Server then Exit(I);
end;

var
  Node: TDrawNode;
  R,D: TRectF;
  I: Integer;
  Size: TSizeF;
  ServerIndex: Integer;
begin

  TArray.Sort<TDrawNode>(Nodes,TComparer<TDrawNode>.Construct(
  function(const Left,Right: TDrawNode): Integer
  var LeftAccountID,RightAccountID: Int64;
  begin
    Result:=-CompareValue(Integer(Left.Node.Connected),Integer(Right.Node.Connected));
    if Result=EqualsValue then
    begin
      LeftAccountID:=Left.Node.AccountID;
      if LeftAccountID=0 then LeftAccountID:=MaxInt;
      RightAccountID:=Right.Node.AccountID;
      if RightAccountID=0 then RightAccountID:=MaxInt;
      Result:=CompareValue(LeftAccountID,RightAccountID);
      if Result=0 then
      Result:=CompareDateTime(Right.Node.AcceptTime,Left.Node.AcceptTime);
    end;
  end));

  Size:=TSizeF.Create(0,0);

  R:=TRectF.Create(PointF(10,30),100,60);

  RootNode.Rect:=R;
  RootNode.Bottom:=RootNode.Rect.Bottom;

  ServersCount:=0;

  for I:=0 to High(Nodes) do
  if Nodes[I].Node.Binded then
  begin

    Inc(ServersCount);

    if Nodes[I].Node.Address=Nodes[I].Node.Server then
      Nodes[I].FillColor:=NODE_SERVER
    else
    if Nodes[I].Node.AccountID<>0 then
      Nodes[I].FillColor:=NODE_LOGIN
    else
      Nodes[I].FillColor:=NODE_GRAY;

    if Nodes[I].Node.Connected then
      Nodes[I].StrokeColor:=NODE_CONNECTED
    else
      Nodes[I].StrokeColor:=NODE_DISCONNECTED;

    Nodes[I].Rect:=R;
    Nodes[I].Bottom:=Nodes[I].Rect.Bottom;

    Size.cx:=Max(Size.cx,R.Right+10);
    Size.cy:=Max(Size.cy,R.Bottom+10);

    R.SetLocation(R.Right+10,R.Top);

  end;

  ClientsCount:=0;
  ConnectionsCount:=0;

  for I:=0 to High(Nodes) do
  if not Nodes[I].Node.Binded then
  begin

    Inc(ConnectionsCount);

    ServerIndex:=GetServerNode(Nodes[I].Node.Server);

    if ServerIndex=-1 then
    begin
      R:=RootNode.Rect;
      R.SetLocation(R.Left,RootNode.Bottom+10);
      RootNode.Bottom:=R.Bottom;
    end else begin
      R:=Nodes[ServerIndex].Rect;
      R.SetLocation(R.Left,Nodes[ServerIndex].Bottom+10);
      Nodes[ServerIndex].Bottom:=R.Bottom;
    end;

    Nodes[I].Rect:=R;

    if Nodes[I].Node.AccountID<>0 then Inc(ClientsCount);

    if Nodes[I].Node.AccountID<>0 then
      Nodes[I].FillColor:=GRAY_LOGIN
    else
      Nodes[I].FillColor:=NODE_GRAY;

    if Nodes[I].Node.Connected then
      Nodes[I].StrokeColor:=NODE_CONNECTED
    else
      Nodes[I].StrokeColor:=NODE_DISCONNECTED;

    Nodes[I].Rect:=R;

    Size.cx:=Max(Size.cx,R.Right+10);
    Size.cy:=Max(Size.cy,R.Bottom+10);

  end;

  Size.cx:=Max(Size.cx,ScrollBox1.ClientWidth);

  Memo1.Size.Size:=Size;

end;

procedure TNetworkForm.DrawNode(Canvas: TCanvas; const Node: TDrawNode);
var
  R,D: TRectF;
begin

  R:=Node.Rect;

  Canvas.Stroke.Thickness:=1;
  Canvas.Stroke.Kind:=TBrushKind.Solid;
  Canvas.Stroke.Color:=Node.StrokeColor;

  Canvas.Fill.Kind:=TBrushKind.Solid;
  Canvas.Fill.Color:=Node.FillColor;

  Canvas.FillRect(R,5,5,AllCorners,1);
  Canvas.DrawRect(R,5,5,AllCorners,1);

  Canvas.Fill.Color:=claGray;
  Canvas.Font.Size:=10;

  D:=R;
  D.Inflate(-6,-6);

  Canvas.FillText(D,Node.Node.Address,False,1,[],TTextAlign.Leading,TTextAlign.Leading);

  D.Top:=D.Top+12;

  Canvas.FillText(D,Node.Node.Server,False,1,[],TTextAlign.Leading,TTextAlign.Leading);

  if Node.Node.AcceptTime=0 then Exit;

  D.Top:=D.Top+12;

  Canvas.FillText(D,Node.Node.AccountID.ToString+'  '+Node.Node.GetOSName,False,1,[],TTextAlign.Leading,TTextAlign.Leading);

  D.Top:=D.Top+12;
  Canvas.Font.Size:=8;

  Canvas.FillText(D,DateTimeToStr(Node.Node.AcceptTime),False,1,[],TTextAlign.Leading,TTextAlign.Leading);

end;

procedure TNetworkForm.Memo1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var R: TRectF;
begin

  R:=ScrollBox1.LocalRect;
  R.SetLocation(ScrollBox1.ViewportPosition);

  if ServersCount=0 then DrawNode(Canvas,RootNode);

  for var Node in Nodes do if R.IntersectsWith(Node.Rect) then DrawNode(Canvas,Node);

  R:=ARect;
  R.Top:=10;
  R.Left:=10;

  Canvas.Fill.Color:=clablack;
  Canvas.Font.Size:=12;
  Canvas.FillText(R,'Network: '+Connection.Network+
    '  Clients: '+ClientsCount.ToString+
    '  Total: '+ConnectionsCount.ToString+
    '  Servers: '+ServersCount.ToString,
    False,1,[],TTextAlign.Leading,TTextAlign.Leading);

end;

end.
