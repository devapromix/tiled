unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Render;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses Unit1, Test.Player;

procedure TForm2.FormClick(Sender: TObject);
begin
  Form1.SetFocus;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
end;

procedure TForm2.Render;
begin
end;

end.
