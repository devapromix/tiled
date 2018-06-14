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
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    Surface: TBitmap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math, WorldMap;

{$R *.dfm}

type
  TPlayer = record
    Image: TPNGImage;
    X: Integer;
    Y: Integer;
    procedure SetLocation(AX, AY: Integer);
  end;

var
  Map: TWorldMap;
  Player: TPlayer;

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

procedure RefreshMap;
begin
  with Form1 do
  begin
    Caption := Format('%s (%d)', [Map.GetCurrentMap.Name,
      Map.GetCurrentMap.Level]);
    ClientWidth := Map.GetCurrentMap.TileSize * Map.GetCurrentMap.Width;
    ClientHeight := Map.GetCurrentMap.TileSize * Map.GetCurrentMap.Height;
    Surface.Width := ClientWidth;
    Surface.Height := ClientHeight;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Map := TWorldMap.Create(Self);
  Map.LoadFromFile('forest_of_bears.ini');

  Surface := TBitmap.Create;

  Player.Image := TPNGImage.Create;
  Player.Image.LoadFromFile(GetPath('resources\images\races') + 'human.png');
  Player.SetLocation(1, 1);

  RefreshMap;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Player.Image);
  FreeAndNil(Surface);
  FreeAndNil(Map);
end;

procedure Move(X, Y: Integer);
var
  TX, TY: Integer;
  TileType, ObjType, ItemType, MonType: string;
begin
  TX := Player.X + X;
  TY := Player.Y + Y;
  //
  if (TX < 0) and Map.Go(drMapLeft) then
  begin
    RefreshMap;
    Player.SetLocation(Map.GetCurrentMap.Width - 1, Player.Y);
  end;
  if (TX > Map.GetCurrentMap.Width - 1) and Map.Go(drMapRight) then
  begin
    RefreshMap;
    Player.SetLocation(0, Player.Y);
  end;
  if (TY < 0) and Map.Go(drMapUp) then
  begin
    RefreshMap;
    Player.SetLocation(Player.X, Map.GetCurrentMap.Height - 1);
  end;
  if (TY > Map.GetCurrentMap.Height - 1) and Map.Go(drMapDown) then
  begin
    RefreshMap;
    Player.SetLocation(Player.X, 0);
  end;
  //
  if (TX < 0) or (TX > Map.GetCurrentMap.Width - 1) then
    Exit;
  if (TY < 0) or (TY > Map.GetCurrentMap.Height - 1) then
    Exit;

  TileType := Map.GetCurrentMap.GetTileType(lrTiles, TX, TY);
  ObjType := Map.GetCurrentMap.GetTileType(lrObjects, TX, TY);
  ItemType := Map.GetCurrentMap.GetTileType(lrItems, TX, TY);
  MonType := Map.GetCurrentMap.GetTileType(lrMonsters, TX, TY);

  if not Map.GetCurrentMap.TiledObject[Map.GetCurrentMap.FMap[lrTiles][TX][TY]].Passable
  then
    Exit;

  if (ObjType = 'closed_door') or (ObjType = 'hidden_door') or
    (ObjType = 'closed_chest') or (ObjType = 'trapped_chest') then
  begin
    Inc(Map.GetCurrentMap.FMap[lrObjects][TX][TY]);
    if (ObjType = 'closed_chest') then
      Map.GetCurrentMap.FMap[lrItems][TX][TY] :=
        RandomRange(Map.GetCurrentMap.Firstgid[lrItems],
        Map.GetCurrentMap.Firstgid[lrMonsters]) - 1;
    Exit;
  end;

  if (ItemType <> '') then
  begin
    Map.GetCurrentMap.FMap[lrItems][TX][TY] := -1;
  end;
  if (MonType <> '') then
  begin
    Map.GetCurrentMap.FMap[lrMonsters][TX][TY] := -1;
  end;
  Player.SetLocation(TX, TY);
end;

procedure Use;
var
  ObjType: string;
begin
  ObjType := Map.GetCurrentMap.GetTileType(lrObjects, Player.X, Player.Y);
  if (ObjType = 'up_stairs') and Map.Go(drMapTop) then
    RefreshMap;;
  if (ObjType = 'down_stairs') and Map.Go(drMapBottom) then
    RefreshMap;;
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
    13, 32:
      Use;
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
      for L := Low(TTiledMap.TLayerEnum) to High(TTiledMap.TLayerEnum) do
        if (Map.GetCurrentMap.FMap[L][X][Y] >= 0) then
          Surface.Canvas.Draw(X * Map.GetCurrentMap.TileSize,
            Y * Map.GetCurrentMap.TileSize, Map.GetCurrentMap.TiledObject
            [Map.GetCurrentMap.FMap[L][X][Y]].Image);
    end;
  Surface.Canvas.Draw(Player.X * Map.GetCurrentMap.TileSize,
    Player.Y * Map.GetCurrentMap.TileSize, Player.Image);
  Canvas.Draw(0, 0, Surface);
end;

{ TPlayer }

procedure TPlayer.SetLocation(AX, AY: Integer);
begin
  Self.X := AX;
  Self.Y := AY;
end;

end.
