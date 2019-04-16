unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.PNGImage, TiledMap, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    Surface: TBitmap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math, WorldMap, Utils, Mods, Mobs, MsgLog, Creature;

{$R *.dfm}

procedure RefreshMap;
var
  W, H: Integer;
begin
  with Form1 do
  begin
    Caption := Format('%s (%d)', [Map.GetCurrentMap.Name, Map.GetCurrentMap.Level]);
    W := Map.GetCurrentMap.TileSize * Map.GetCurrentMap.Width;
    H := Map.GetCurrentMap.TileSize * (Map.GetCurrentMap.Height + 1);
    ClientWidth := Min(W, Screen.Width);
    ClientHeight := Min(H, Screen.Height);
    Surface.Width := ClientWidth;
    Surface.Height := ClientHeight;
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Map := TWorldMap.Create(Self);
  // GMods.SetCurrent('twilight_forest');
  // GMods.SetCurrent('elvion');
  Surface := TBitmap.Create;
  Surface.Canvas.Brush.Style := bsClear;
  Surface.Canvas.Font.Name := 'Courier New';
  Surface.Canvas.Font.Color := clWhite;
  Surface.Canvas.Font.Size := 10;
  // RefreshMap;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Surface);
  if Assigned(Map) then
    FreeAndNil(Map);
end;

procedure Use;
var
  ObjType: string;
  Player: TMobInfo;
begin
  if Map.GetCurrentMapMobs.Player.Idx = -1 then
    Exit;
  Player := Map.GetCurrentMapMobs.Get(Map.GetCurrentMapMobs.Player.Idx);
  ObjType := Map.GetCurrentMap.GetTileType(lrObjects, Player.X, Player.Y);
  if (ObjType = 'up_stairs') and Map.Go(drMapTop) then
  begin
    Map.GetCurrentMapMobs.Move(0, 0);
    Log.Add(Map.GetCurrentMap.Name);
  end;
  if (ObjType = 'down_stairs') and Map.Go(drMapBottom) then
  begin
    Map.GetCurrentMapMobs.Move(0, 0);
    Log.Add(Map.GetCurrentMap.Name);
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    Ord('I'):
      begin
        if Assigned(Map) then
          FreeAndNil(Map);
        Map := TWorldMap.Create(Self);
        GMods.SetCurrent('twilight_forest');
        RefreshMap;
      end;
    Ord('O'):
      begin
        if Assigned(Map) then
          FreeAndNil(Map);
        Map := TWorldMap.Create(Self);
        GMods.SetCurrent('elvion');
        RefreshMap;
      end;
    Ord('S'):
      begin
        Map.GetCurrentMapMobs.Player.Save;
      end;
    Ord('R'):
      begin
        Map.GetCurrentMapMobs.Player.Load;
      end;
    Ord('0'):
      begin
        if Assigned(Map) then
          FreeAndNil(Map);
        Map := TWorldMap.Create(Self);
        GMods.SetCurrent('twilight_forest', 'town.ini');
        RefreshMap;
      end;
    Ord('1'):
      begin
        if Assigned(Map) then
          FreeAndNil(Map);
        Map := TWorldMap.Create(Self);
        GMods.SetCurrent('twilight_forest', 'twilight_forest.ini');
        RefreshMap;
      end;
    Ord('2'):
      begin
        if Assigned(Map) then
          FreeAndNil(Map);
        Map := TWorldMap.Create(Self);
        GMods.SetCurrent('twilight_forest', 'dungeon.ini');
        RefreshMap;
      end;
    Ord('L'):
      Map.GetCurrentMapMobs.ChLook;
    37:
      begin
        Map.GetCurrentMapMobs.Move(-1, 0);
      end;
    39:
      begin
        Map.GetCurrentMapMobs.Move(1, 0);
      end;
    38:
      begin
        Map.GetCurrentMapMobs.Move(0, -1);
      end;
    40:
      begin
        Map.GetCurrentMapMobs.Move(0, 1);
      end;
    13, 32:
      Use;
    27:
      Close;
  end;
  FormPaint(Sender);
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Surface.Canvas.Brush.Color := clBlack;
  Surface.Canvas.FillRect(Rect(0, 0, Surface.Width, Surface.Height));
  if Assigned(Map) then
  begin
    Map.Render(Surface.Canvas);
    Map.GetCurrentMapMobs.Render(Surface.Canvas);
    Map.GetCurrentMapMobs.Player.Render(Surface.Canvas);
    Log.Render(Surface.Canvas);
  end;
  Canvas.Draw(0, 0, Surface);
end;

end.
