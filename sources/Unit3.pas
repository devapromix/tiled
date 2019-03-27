unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm3 = class(TForm)
    MsgLog: TRichEdit;
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Render;
  end;

var
  Form3: TForm3;
  Msg: string = '';

implementation

{$R *.dfm}

uses Unit1;

{ TForm3 }

procedure TForm3.FormClick(Sender: TObject);
begin
  Form1.SetFocus;
end;

procedure TForm3.Render;
begin
  if Msg <> '' then
  begin
    MsgLog.Lines.Insert(0, Msg);
    Msg := '';
  end;
end;

end.
