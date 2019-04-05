program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  TiledMap in 'TiledMap.pas',
  WorldMap in 'WorldMap.pas',
  Creature in 'Creature.pas',
  Utils in 'Utils.pas',
  Mods in 'Mods.pas',
  Mobs in 'Mobs.pas',
  PathFind in 'PathFind.pas',
  MsgLog in 'MsgLog.pas';

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
  Application.Run;

end.
