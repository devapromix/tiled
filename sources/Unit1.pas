unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.PNGImage, TiledMap;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    Surface: TBitmap;
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.dfm}

var
  PNGPlayer: TPNGImage;
  PX: Integer;
  PY: Integer;

var
  Map: TTiledMap;

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  X, Y: Integer;
begin
  Map := TTiledMap.Create(Form1);
  Map.LoadFromFile('dungeon.tmx');
  //
  ClientWidth := Map.TileSize * Map.Width;
  ClientHeight := Map.TileSize * Map.Height;
  Surface := TBitmap.Create;
  Surface.Width := ClientWidth;
  Surface.Height := ClientHeight;
  //
  PNGPlayer := TPNGImage.Create;
  PNGPlayer.LoadFromFile(GetPath('resources\images\races') + 'human.png');
  //
  for Y := 0 to Map.Height - 1 do
    for X := 0 to Map.Width - 1 do
      if Map.GetTileType(lrObjects, X, Y) = 'up_stairs' then
      begin
        PX := X;
        PY := Y;
        Break;
      end;
  Exit;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Map.Free;
  PNGPlayer.Free;
  Surface.Free;
end;

procedure Move(X, Y: Integer);
var
  TX, TY: Integer;
  TileType, ObjType, ItemType, MonType: string;
begin
  TX := PX + X;
  TY := PY + Y;
  if (TX < 0) or (TX > Map.Width - 1) then
    Exit;
  if (TY < 0) or (TY > Map.Height - 1) then
    Exit;

  TileType := Map.GetTileType(lrTiles, TX, TY);
  ObjType := Map.GetTileType(lrObjects, TX, TY);
  ItemType := Map.GetTileType(lrItems, TX, TY);
  MonType := Map.GetTileType(lrMonsters, TX, TY);

  if not Map.TiledObject[Map.FMap[lrTiles][TX][TY]].Passable then
    Exit;

  if (ObjType = 'closed_door') or (ObjType = 'hidden_door') or
    (ObjType = 'closed_chest') or (ObjType = 'trapped_chest') then
  begin
    Inc(Map.FMap[lrObjects][TX][TY]);
    if (ObjType = 'closed_chest') then
      Map.FMap[lrItems][TX][TY] := RandomRange(Map.Firstgid[lrItems],
        Map.Firstgid[lrMonsters]) - 1;
    Exit;
  end;

  if (ObjType = 'up_stairs') or (ObjType = 'down_stairs') then
    Halt;
  if (ItemType <> '') then
  begin
    Map.FMap[lrItems][TX][TY] := -1;
  end;
  if (MonType <> '') then
  begin
    Map.FMap[lrMonsters][TX][TY] := -1;
  end;
  PX := TX;
  PY := TY;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    37:
      Move(-1, 0);
    39:
      Move(1, 0);
    38:
      Move(0, -1);
    40:
      Move(0, 1);
  end;
  FormPaint(Sender);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  X, Y: Integer;
  L: TLayerEnum;
begin
  for Y := 0 to Map.Height - 1 do
    for X := 0 to Map.Width - 1 do
    begin
      for L := Low(TLayerEnum) to High(TLayerEnum) do
        if (Map.FMap[L][X][Y] >= 0) then
          Surface.Canvas.Draw(X * Map.TileSize, Y * Map.TileSize,
            Map.TiledObject[Map.FMap[L][X][Y]].Image);
    end;
  Surface.Canvas.Draw(PX * Map.TileSize, PY * Map.TileSize, PNGPlayer);
  Canvas.Draw(0, 0, Surface);
end;

end.
