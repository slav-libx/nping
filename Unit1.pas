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
  FMX.Objects,
  FMX.Edit,
  FMX.SearchBox,
  FMX.DialogService,
  Connection.Types,
  Unit2,
  Form.Network;

type
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
    procedure SaveConnections;
    procedure OnItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

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

  Item:=ListBox.ItemByIndex(ListBox.Items.Add(''));
  {$IFDEF MSWINDOWS}Item.Height:=36;{$ENDIF}

  Content:=TItemFrame.Create(Item);
  Content.Parent:=Item;
  Content.Label1.Text:=Connection.Address;
  {$IFDEF ANDROID}Content.Label1.Width:=140;{$ENDIF}
  Content.TagObject:=Connection;

  Item.TagObject:=Content;
  Item.OnClick:=OnItemClick;

  Connections.Add(Connection);

  Connection.Content:=Content;
  Connection.State:=None;

end;

procedure TForm1.OnItemClick(Sender: TObject);
var Connection: TConnection;
begin
  Connection:=TConnection(TItemFrame(TFmxObject(Sender).TagObject).TagObject);
  NetworkForm.Get(Connection.Address);
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

end.
