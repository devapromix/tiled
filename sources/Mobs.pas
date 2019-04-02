unit Mobs;

interface

uses
  Classes, Vcl.Graphics, Vcl.Imaging.PNGImage;

type
  TMobInfo = record
    Force: Integer;
    X: Integer;
    Y: Integer;
    Id: Integer;
    Name: string;
    Life: Integer;
    MaxLife: Integer;
    Radius: Integer;
  end;

type
  TMobs = class(TObject)
  private
    FForce: TStringList;
    FCoord: TStringList;
    FID: TStringList;
    FName: TStringList;
    FLife: TStringList;
    FRad: TStringList;
  public
    MobLB: TBitmap;
    Lifebar: TPNGImage;
    PlayerIndex: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromMap(const N: Integer);
    procedure Add(const Force, X, Y, Id: Integer; N: string; L, MaxL, R: Integer); overload;
    procedure Add(const P: TMobInfo); overload;
    function BarWidth(CX, MX, GS: Integer): Integer;
    function Count: Integer;
    function Get(I: Integer): TMobInfo;
    function Del(I: Integer): Boolean;
    function IndexOf(const X, Y: Integer): Integer;
    procedure ModLife(const Index, Value: Integer);
    procedure Move(const AtkId, DX, DY: Integer); overload;
    procedure Move(const DX, DY: Integer); overload;
    procedure MoveToPosition(const I, DX, DY: Integer);
    procedure SetPosition(const I, X, Y: Integer);
    function GetDist(FromX, FromY, ToX, ToY: Single): Word;
    procedure Render(Canvas: TCanvas);
  end;

implementation

uses
  SysUtils, Math, Dialogs, WorldMap, Mods, TiledMap, PathFind;

const
  F = '%d=%d';

  { TMobs }

function IsTilePassable(X, Y: Integer): Boolean; stdcall;
begin
  with Map.GetCurrentMap do
  begin
    Result := TiledObject[FMap[lrTiles][X][Y]].Passable;
    if (FMap[lrObjects][X][Y] >= 0) then
      Result := Result and TiledObject[FMap[lrObjects][X][Y]].Passable;
  end;
end;

function TMobs.BarWidth(CX, MX, GS: Integer): Integer;
var
  I: Integer;
begin
  if (CX = MX) and (CX = 0) then
  begin
    Result := 0;
    Exit;
  end;
  if (MX <= 0) then
    MX := 1;
  I := (CX * GS) div MX;
  if I <= 0 then
    I := 0;
  if (CX >= MX) then
    I := GS;
  Result := I;
end;

procedure TMobs.Add(const Force, X, Y, Id: Integer; N: string; L, MaxL, R: Integer);
begin
  FForce.Append(Force.ToString);
  FCoord.Append(Format(F, [X, Y]));
  FID.Append(Id.ToString);
  FName.Append(N);
  FLife.Append(Format(F, [L, MaxL]));
  FRad.Append(R.ToString);
end;

procedure TMobs.Add(const P: TMobInfo);
begin
  Self.Add(P.Force, P.X, P.Y, P.Id, P.Name, P.Life, P.MaxLife, P.Radius);
end;

procedure TMobs.Clear;
begin
  FForce.Clear;
  FCoord.Clear;
  FID.Clear;
  FName.Clear;
  FLife.Clear;
  FRad.Clear;
end;

function TMobs.Count: Integer;
begin
  Result := FID.Count;
end;

constructor TMobs.Create;
begin
  PlayerIndex := -1;
  MobLB := TBitmap.Create;
  Lifebar := TPNGImage.Create;
  Lifebar.LoadFromFile(GMods.GetPath('images', 'lifebar.png'));
  FForce := TStringList.Create;
  FCoord := TStringList.Create;
  FID := TStringList.Create;
  FName := TStringList.Create;
  FLife := TStringList.Create;
  FRad := TStringList.Create;
end;

function TMobs.Del(I: Integer): Boolean;
begin
  FForce.Delete(I);
  FCoord.Delete(I);
  FID.Delete(I);
  FName.Delete(I);
  FLife.Delete(I);
  FRad.Delete(I);
  Result := True;
end;

destructor TMobs.Destroy;
var
  I: Integer;
  S: string;
begin
  {
    S := '';
    for I := 0 to Self.Count - 1 do
    begin
    S := S + Format('%s,%s,%s,%s,%s', [FForce[I], FCoord[I], FID[I], FName[I], FLife[I]]) + #13#10;
    end;
    ShowMessage(S);
  }
  FreeAndNil(MobLB);
  FreeAndNil(Lifebar);
  FreeAndNil(FForce);
  FreeAndNil(FCoord);
  FreeAndNil(FID);
  FreeAndNil(FName);
  FreeAndNil(FLife);
  FreeAndNil(FRad);
  inherited;
end;

function TMobs.Get(I: Integer): TMobInfo;
begin
  Result.Force := FForce[I].ToInteger;
  Result.X := FCoord.KeyNames[I].ToInteger;
  Result.Y := FCoord.ValueFromIndex[I].ToInteger;
  Result.Id := FID[I].ToInteger;
  Result.Name := FName[I];
  Result.Life := FLife.KeyNames[I].ToInteger;
  Result.MaxLife := FLife.ValueFromIndex[I].ToInteger;
  Result.Radius := FRad[I].ToInteger;
end;

function TMobs.GetDist(FromX, FromY, ToX, ToY: Single): Word;
begin
  Result := Round(SQRT(SQR(ToX - FromX) + SQR(ToY - FromY)));
end;

function TMobs.IndexOf(const X, Y: Integer): Integer;
begin
  Result := FCoord.IndexOf(Format(F, [X, Y]));
end;

procedure TMobs.LoadFromMap(const N: Integer);
var
  I, J, F, X, Y: Integer;
begin
  J := 0;
  for Y := 0 to Map.GetMap(N).Height - 1 do
    for X := 0 to Map.GetMap(N).Width - 1 do
    begin
      F := 0;
      I := Map.GetMap(N).FMap[lrMonsters][X][Y];
      if I >= 0 then
      begin
        with Map.GetMap(N).TiledObject[I] do
        begin
          if LowerCase(Name) = 'human' then
          begin
            PlayerIndex := J;
            F := 1;
          end;
          Add(F, X, Y, I, Name, Life, Life, Radius);
          Inc(J);
        end;
      end;
    end;
  //
end;

procedure TMobs.ModLife(const Index, Value: Integer);
var
  CurLife, MaxLife: Integer;
begin
  CurLife := FLife.KeyNames[Index].ToInteger + Value;
  MaxLife := FLife.ValueFromIndex[Index].ToInteger;
  CurLife := Math.EnsureRange(CurLife, 0, MaxLife);
  FLife[Index] := Format(F, [CurLife, MaxLife]);
end;

procedure TMobs.Move(const DX, DY: Integer);
var
  I, NX, NY: Integer;
  Plr, Enm: TMobInfo;
begin
  Move(PlayerIndex, DX, DY);
  for I := Count - 1 downto 0 do
  begin
    if PlayerIndex = -1 then
      Exit;
    if Get(I).Force = 0 then
    begin
      Plr := Get(PlayerIndex);
      Enm := Get(I);
      NX := 0;
      NY := 0;
      if (GetDist(Enm.X, Enm.Y, Plr.X, Plr.Y) > Enm.Radius) or not IsPathFind(Map.GetCurrentMap.Width, Map.GetCurrentMap.Height, Enm.X, Enm.Y, Plr.X,
        Plr.Y, @IsTilePassable, NX, NY) then
        Continue;
      MoveToPosition(I, NX, NY);
    end
    else
    begin
    end;
  end;
end;

procedure TMobs.MoveToPosition(const I, DX, DY: Integer);
var
  M: TMobInfo;
  NX, NY: Integer;
begin
  NX := 0;
  NY := 0;
  M := Get(I);
  if DX < M.X then
    NX := -1;
  if DX > M.X then
    NX := 1;
  if DY < M.Y then
    NY := -1;
  if DY > M.Y then
    NY := 1;
  Move(I, NX, NY);
end;

procedure TMobs.Render(Canvas: TCanvas);
var
  I, X, Y: Integer;
  M: TMobInfo;
begin
  for I := 0 to Map.GetCurrentMapMobs.Count - 1 do
  begin
    M := Map.GetCurrentMapMobs.Get(I);
    Map.GetCurrentMapMobs.MobLB.Assign(Map.GetCurrentMapMobs.Lifebar);
    Map.GetCurrentMapMobs.MobLB.Width := Map.GetCurrentMapMobs.BarWidth(M.Life, M.MaxLife, 30);
    X := M.X * Map.GetCurrentMap.TileSize;
    Y := M.Y * Map.GetCurrentMap.TileSize;
    Canvas.Draw(X + 1, Y, Map.GetCurrentMapMobs.MobLB);
    Canvas.Draw(X, Y, Map.GetCurrentMap.TiledObject[M.Id].Image);
  end;
end;

procedure TMobs.Move(const AtkId, DX, DY: Integer);
var
  NX, NY, DefId, I, Dam: Integer;
  Atk, Def: TMobInfo;
  ObjType, ItemType: string;
begin
  if PlayerIndex = -1 then
    Exit;
  Atk := Get(AtkId);
  if Atk.Life <= 0 then
    Exit;
  NX := Atk.X + DX;
  NY := Atk.Y + DY;

  if (NX < 0) and Map.Go(drMapLeft) then
  begin
    Map.GetCurrentMapMobs.SetPosition(Map.GetCurrentMapMobs.PlayerIndex, Map.GetCurrentMap.Width - 1, NY);
    Exit;
  end;
  if (NX > Map.GetCurrentMap.Width - 1) and Map.Go(drMapRight) then
  begin
    Map.GetCurrentMapMobs.SetPosition(Map.GetCurrentMapMobs.PlayerIndex, 0, NY);
    Exit;
  end;
  if (NY < 0) and Map.Go(drMapUp) then
  begin
    Map.GetCurrentMapMobs.SetPosition(Map.GetCurrentMapMobs.PlayerIndex, NX, Map.GetCurrentMap.Height - 1);
    Exit;
  end;
  if (NY > Map.GetCurrentMap.Height - 1) and Map.Go(drMapDown) then
  begin
    Map.GetCurrentMapMobs.SetPosition(Map.GetCurrentMapMobs.PlayerIndex, NX, 0);
    Exit;
  end;

  if (NX < 0) or (NX > Map.GetCurrentMap.Width - 1) then
    Exit;
  if (NY < 0) or (NY > Map.GetCurrentMap.Height - 1) then
    Exit;

  ObjType := Map.GetCurrentMap.GetTileType(lrObjects, NX, NY);
  ItemType := Map.GetCurrentMap.GetTileType(lrItems, NX, NY);

  if not IsTilePassable(NX, NY) then
    Exit;

  if (ObjType = 'closed_door') or (ObjType = 'hidden_door') or (ObjType = 'closed_chest') or (ObjType = 'trapped_chest') then
  begin
    Inc(Map.GetCurrentMap.FMap[lrObjects][NX][NY]);
    if (ObjType = 'closed_chest') then
    begin
      Map.GetCurrentMap.FMap[lrItems][NX][NY] := RandomRange(Map.GetCurrentMap.Firstgid[lrItems], Map.GetCurrentMap.Firstgid[lrMonsters]) - 1;
    end;
    Exit;
  end;

  if (ItemType <> '') then
  begin
    Map.GetCurrentMap.FMap[lrItems][NX][NY] := -1;
    Exit;
  end;

  DefId := Self.IndexOf(NX, NY);
  if DefId >= 0 then
  begin
    Def := Get(DefId);
    if Atk.Force <> Def.Force then
    begin
      begin
        if (Math.RandomRange(0, 3 + 1) >= Math.RandomRange(0, 3 + 1)) then
        begin
          Dam := Math.RandomRange(2, 3 + 1);
          ModLife(DefId, -Dam);
        end
        else
        begin
          // Miss
        end;
      end;
      if Get(DefId).Life = 0 then
      begin
        Del(DefId);
        Map.GetCurrentMap.FMap[lrMonsters][NX][NY] := -1;
        PlayerIndex := -1;
        for I := 0 to Count - 1 do
          if FForce[I] = '1' then
          begin
            PlayerIndex := I;
            Break;
          end;
      end;
    end;
    Exit;
  end;
  SetPosition(AtkId, NX, NY);
end;

procedure TMobs.SetPosition(const I, X, Y: Integer);
begin
  FCoord[I] := Format(F, [X, Y]);
end;

end.
