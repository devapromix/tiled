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
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    Surface: TBitmap;
    // Player
    procedure OnLevel;
    procedure OnAddExp(Value: Integer);
    procedure OnModHP(Value: Integer);
    procedure OnDead;
    procedure AddToLog(const M: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math, WorldMap, Utils, Unit2, Test.Player, Unit3;

{$R *.dfm}

var
  Map: TWorldMap;

procedure RefreshMap;
var
  W, H: Integer;
begin
  with Form1 do
  begin
    Caption := Format('%s (%d)', [Map.GetCurrentMap.Name, Map.GetCurrentMap.Level]);
    W := Map.GetCurrentMap.TileSize * Map.GetCurrentMap.Width;
    H := Map.GetCurrentMap.TileSize * Map.GetCurrentMap.Height;
     ClientWidth := Max(W, Screen.Width);
     ClientHeight := Max(H, Screen.Height);
    //ClientWidth := W;
    //ClientHeight := H;
    Surface.Width := ClientWidth;
    Surface.Height := ClientHeight;
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
  end;
end;

procedure TForm1.AddToLog(const M: string);
begin
  Msg := Trim(Msg + ' ' + M);
end;

function LevExp(const Level: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Level do
    Result := Result + (I * 50);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentModName('twforest');
  //
  Map := TWorldMap.Create(Self);
  Map.LoadFromFile('forest_of_bears.ini');
  //
  Surface := TBitmap.Create;
  // Player
  Player.OnLevel := OnLevel;
  Player.OnModHP := OnModHP;
  Player.OnBeforeAddExp := OnAddExp;
  Player.OnMinHP := OnDead;
  Player.OnLevel;
  Player.SetLocation(2, 3);
  //
  RefreshMap;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
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
  if (MonType <> '') then
  begin
    Map.GetCurrentMap.FMap[lrMonsters][TX][TY] := -1;
    Form1.AddToLog('Ты победил!');
    Player.AddExp(10);
  end;
  Player.SetLocation(TX, TY);
end;

procedure Use;
var
  ObjType: string;
begin
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
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    Ord('I'):
      ;
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
      for L := Low(TTiledMap.TLayerEnum) to High(TTiledMap.TLayerEnum) do
        if (Map.GetCurrentMap.FMap[L][X][Y] >= 0) then
          Surface.Canvas.Draw(X * Map.GetCurrentMap.TileSize, Y * Map.GetCurrentMap.TileSize,
            Map.GetCurrentMap.TiledObject[Map.GetCurrentMap.FMap[L][X][Y]].Image);
    end;
  Player.Render(Surface);
  Canvas.Draw(0, 0, Surface);
  Form2.Render;
  Form3.Render;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  MaxWidth: Integer;
begin
  if Assigned(Form2) then
  begin
    Form2.Top := Top;
    Form2.Left := Left + Width;
    Form2.Height := Height;
    Form2.Show;
    Form1.SetFocus;
  end;
  if Assigned(Form3) then
  begin
    Form3.Top := Top + Height;
    Form3.Left := Left;
    Form3.Width := Width + Form2.Width;
    Form3.Show;
    Form1.SetFocus;
  end;
end;

procedure TForm1.OnAddExp(Value: Integer);
begin
  Form1.AddToLog(Format('Опыт +%d.', [Value]));
end;

procedure TForm1.OnDead;
begin

end;

procedure TForm1.OnLevel;
begin
  Player.Exp := Player.Exp - Player.MaxExp;
  Player.MaxExp := LevExp(Player.Level);
  if Player.Level > 1 then
  begin
    Player.MaxHP := Player.MaxHP + 2;
    Player.MaxMP := Player.MaxMP + 5;
    Form1.AddToLog('Новый уровень!');
  end;
end;

procedure TForm1.OnModHP(Value: Integer);
begin

end;

end.
