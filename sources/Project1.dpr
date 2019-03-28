program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  TiledMap in 'TiledMap.pas',
  WorldMap in 'WorldMap.pas',
  Unit2 in 'Unit2.pas' {Form2},
  Creature in 'Creature.pas',
  Test.Player in 'Test.Player.pas',
  Unit3 in 'Unit3.pas' {Form3},
  Utils in 'Utils.pas',
  Mods in 'Mods.pas',
  Mobs in 'Mobs.pas',
  PathFind in 'PathFind.pas';

{$R *.res}

begin
  Randomize();
{$IFNDEF FPC}
{$IF COMPILERVERSION >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;

end.
