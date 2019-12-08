program nconnect;

uses
  System.StartUpCopy,
  FMX.Forms,
  Net.Socket in '..\relictum.node\Library\Net.Socket.pas',
  Net.StreamSocket in '..\relictum.node\Library\Net.StreamSocket.pas',
  UCustomMemoryStream in '..\relictum.node\Library\UCustomMemoryStream.pas',
  Unit3 in 'Unit3.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
