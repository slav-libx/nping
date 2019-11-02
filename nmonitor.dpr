program nmonitor;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Net.Socket in '..\relictum.node\Net\Net.Socket.pas',
  Net.StreamSocket in '..\relictum.node\Net\Net.StreamSocket.pas',
  UCustomMemoryStream in '..\relictum.node\Library\UCustomMemoryStream.pas',
  Unit2 in 'Unit2.pas' {ItemFrame: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
