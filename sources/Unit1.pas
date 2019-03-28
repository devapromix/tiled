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

uses Math, WorldMap, Utils, Unit2, Test.Player, Unit3, Mods, Mobs;

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
  GMods.SetCurrent('twilight_forest'); // GMods.SetCurrent('elvion');
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

  procedure DelMob();
  begin
    Mob.Del(I);
    Map.GetCurrentMap.FMap[lrMonsters][TX][TY] := -1;
  end;

begin
  { TX := Player.X + X;
    TY := Player.Y + Y;
    //
    if (TX < 0) and Map.Go(drMapLeft) then
    begin
    Form3.MsgLog.Clear;
    RefreshMap;
    Player.SetLocation(Map.GetCurrentMap.Width - 1, Player.Y);
    end;
    if (TX > Map.GetCurrentMap.Width - 1) and Map.Go(drMapRight) then
    begin
    Form3.MsgLog.Clear;
    RefreshMap;
    Player.SetLocation(0, Player.Y);
    end;
    if (TY < 0) and Map.Go(drMapUp) then
    begin
    Form3.MsgLog.Clear;
    RefreshMap;
    Player.SetLocation(Player.X, Map.GetCurrentMap.Height - 1);
    end;
    if (TY > Map.GetCurrentMap.Height - 1) and Map.Go(drMapDown) then
    begin
    Form3.MsgLog.Clear;
    RefreshMap;
    Player.SetLocation(Player.X, 0);
    end; }
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

  // Mobs
  I := Mob.IndexOf(TX, TY);
  if I >= 0 then
  begin
    if Mob.Get(I).Life > 0 then
    begin
      Mob.ModLife(I, -3);
      if Mob.Get(I).Life = 0 then
        DelMob()
      else
      begin
        // if Math.RandomRange(0, 3) = 0 then
        // Player.ModHP(-1);
        Exit;
      end
    end
    else
      DelMob();
  end;
  // Player.SetLocation(TX, TY);
  //
  // if Math.RandomRange(0, 9) = 0 then
  // Player.ModHP(1);
end;

procedure Use;
begin {
    ObjType := Map.GetCurrentMap.GetTileType(lrObjects, Player.X, Player.Y);
    if (ObjType = 'up_stairs') and Map.Go(drMapTop) then
    begin
    Form3.MsgLog.Clear;
    RefreshMap;
    end;
    if (ObjType = 'down_stairs') and Map.Go(drMapBottom) then
    begin
    Form3.MsgLog.Clear;
    RefreshMap;
    end; }
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    Ord('I'):
      ;
    37:
      begin
        Mob.Move(-1, 0);
      end;
    39:
      begin
        Mob.Move(1, 0);
      end;
    38:
      begin
        Mob.Move(0, -1);
      end;
    40:
      begin
        Mob.Move(0, 1);
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
  I, X, Y: Integer;
  L: TTiledMap.TLayerEnum;
  M: TMobInfo;
begin
  for Y := 0 to Map.GetCurrentMap.Height - 1 do
    for X := 0 to Map.GetCurrentMap.Width - 1 do
    begin
      for L := Low(TTiledMap.TLayerEnum) to TTiledMap.TLayerEnum.lrItems do
        if (Map.GetCurrentMap.FMap[L][X][Y] >= 0) then
          Surface.Canvas.Draw(X * Map.GetCurrentMap.TileSize, Y * Map.GetCurrentMap.TileSize,
            Map.GetCurrentMap.TiledObject[Map.GetCurrentMap.FMap[L][X][Y]].Image);
    end;
  // Mobs
  for I := 0 to Mob.Count - 1 do
  begin
    M := Mob.Get(I);
    Mob.MobLB.Assign(Mob.Lifebar);
    Mob.MobLB.Width := Mob.BarWidth(M.Life, M.MaxLife, 30);
    X := M.X * Map.GetCurrentMap.TileSize;
    Y := M.Y * Map.GetCurrentMap.TileSize;
    Surface.Canvas.Draw(X + 1, Y, Mob.MobLB);
    Surface.Canvas.Draw(X, Y, Map.GetCurrentMap.TiledObject[M.Id].Image);
  end;
  Canvas.Draw(0, 0, Surface);
end;

end.
