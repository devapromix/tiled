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

uses Math, WorldMap, Utils, Mods, Mobs;

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
  Map := TWorldMap.Create(Self);
  GMods.SetCurrent('twilight_forest');
  // GMods.SetCurrent('elvion');
  Surface := TBitmap.Create;
  RefreshMap;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Surface);
  FreeAndNil(Map);
end;

procedure Move(X, Y: Integer);
var
  TX, TY, I: Integer;
  TileType, ObjType, ItemType: string;

begin

  //
  if (TX < 0) or (TX > Map.GetCurrentMap.Width - 1) then
    Exit;
  if (TY < 0) or (TY > Map.GetCurrentMap.Height - 1) then
    Exit;

  TileType := Map.GetCurrentMap.GetTileType(lrTiles, TX, TY);
  ObjType := Map.GetCurrentMap.GetTileType(lrObjects, TX, TY);
  ItemType := Map.GetCurrentMap.GetTileType(lrItems, TX, TY);

  if not Map.GetCurrentMap.TiledObject[Map.GetCurrentMap.FMap[lrTiles][TX][TY]].Passable then
    Exit;

  if (ObjType = 'closed_door') or (ObjType = 'hidden_door') or (ObjType = 'closed_chest') or (ObjType = 'trapped_chest') then
  begin
    Inc(Map.GetCurrentMap.FMap[lrObjects][TX][TY]);
    if (ObjType = 'closed_chest') then
    begin
      Map.GetCurrentMap.FMap[lrItems][TX][TY] := RandomRange(Map.GetCurrentMap.Firstgid[lrItems], Map.GetCurrentMap.Firstgid[lrMonsters]) - 1;
    end;
    Exit;
  end;

  if (ItemType <> '') then
  begin
    Map.GetCurrentMap.FMap[lrItems][TX][TY] := -1;
  end;
end;

procedure Use;
var
  ObjType: string;
  M: TMobInfo;
begin
  if Map.GetCurrentMapMobs.PlayerID = -1 then
    Exit;
  M := Map.GetCurrentMapMobs.Get(Map.GetCurrentMapMobs.PlayerID);
  ObjType := Map.GetCurrentMap.GetTileType(lrObjects, M.X, M.Y);
  if (ObjType = 'up_stairs') and Map.Go(drMapTop) then;
  if (ObjType = 'down_stairs') and Map.Go(drMapBottom) then;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    Ord('I'):
      ;
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
var
  X, Y: Integer;
  L: TTiledMap.TLayerEnum;
begin
  for Y := 0 to Map.GetCurrentMap.Height - 1 do
    for X := 0 to Map.GetCurrentMap.Width - 1 do
    begin
      for L := Low(TTiledMap.TLayerEnum) to TTiledMap.TLayerEnum.lrItems do
        if (Map.GetCurrentMap.FMap[L][X][Y] >= 0) then
          Surface.Canvas.Draw(X * Map.GetCurrentMap.TileSize, Y * Map.GetCurrentMap.TileSize,
            Map.GetCurrentMap.TiledObject[Map.GetCurrentMap.FMap[L][X][Y]].Image);
    end;
  Map.GetCurrentMapMobs.Render(Surface.Canvas);
  Canvas.Draw(0, 0, Surface);
end;

end.
