unit Unit2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  Connection.Intf;

type
  TItemFrame = class(TFrame,IConnectionContent)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
  public
    procedure SetInformation(const AText: string);
    procedure SetException(const AText: string);
    procedure SetOK(const AText: string);
  end;

implementation

{$R *.fmx}

procedure TItemFrame.SetInformation(const AText: string);
begin
  Label2.Visible:=False;
  Label4.Text:=AText;
  Label4.Visible:=True;
  Label3.Visible:=False;
end;

procedure TItemFrame.SetException(const AText: string);
begin
  Label2.Visible:=False;
  Label3.Text:=AText;
  Label3.Visible:=True;
  Label4.Visible:=False;
end;

procedure TItemFrame.SetOK(const AText: string);
begin
  Label2.Visible:=True;
  Label4.Text:=AText;
  Label4.Visible:=True;
  Label3.Visible:=False;
end;

end.
