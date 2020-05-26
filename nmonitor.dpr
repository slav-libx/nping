program nmonitor;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Net.Socket in '..\relictum.node\Library\Net.Socket.pas',
  Net.StreamSocket in '..\relictum.node\Library\Net.StreamSocket.pas',
  UCustomMemoryStream in '..\relictum.node\Library\UCustomMemoryStream.pas',
  Connection.Types in 'Connection.Types.pas',
  Unit2 in 'Unit2.pas' {ItemFrame: TFrame},
  Form.Network in 'Form.Network.pas' {NetworkForm},
  Connection.Intf in 'Connection.Intf.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TNetworkForm, NetworkForm);
  Application.Run;
end.
