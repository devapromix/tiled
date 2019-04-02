unit WorldMap;

interface

uses System.Classes, Graphics, TiledMap, Mobs;

type
  TWorldMap = class(TObject)
  public type
    TDir = (drMapLeft, drMapUp, drMapRight, drMapDown, drMapTop, drMapBottom);
  public type
    TMapInfo = record
      FileName: string;
      Neighbors: array [TDir] of string;
    end;
  private
    FMapInfo: array of TMapInfo;
    FTiledMap: array of TTiledMap;
    FTiledMapMobs: array of TMobs;
    FCurrentMap: Integer;
    FSections: TStringList;
    FOwner: TComponent;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    function Count: Integer;
    property CurrentMap: Integer read FCurrentMap;
    function GetMapIndex(SectionName: string): Integer;
    function GetMap(I: Integer): TTiledMap;
    function GetCurrentMapInfo: TMapInfo;
    function GetCurrentMap: TTiledMap;
    function GetMapMobs(I: Integer): TMobs;
    function GetCurrentMapMobs: TMobs;
    function Go(Dir: TDir): Boolean;
    procedure Render(Canvas: TCanvas);
  end;

var
  Map: TWorldMap;

implementation

uses System.SysUtils, Dialogs, System.IniFiles, Utils, Mods;

{ TWorldMap }

function TWorldMap.Count: Integer;
begin
  Result := FSections.Count;
end;

constructor TWorldMap.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FSections := TStringList.Create;
  FCurrentMap := 0;
end;

destructor TWorldMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    FreeAndNil(FTiledMap[I]);
    FreeAndNil(FTiledMapMobs[I]);
  end;
  FreeAndNil(FSections);
  inherited;
end;

function TWorldMap.GetCurrentMap: TTiledMap;
begin
  Result := FTiledMap[FCurrentMap];
end;

function TWorldMap.GetCurrentMapInfo: TMapInfo;
begin
  Result := FMapInfo[FCurrentMap];
end;

function TWorldMap.GetCurrentMapMobs: TMobs;
begin
  Result := FTiledMapMobs[FCurrentMap];
end;

function TWorldMap.GetMap(I: Integer): TTiledMap;
begin
  Result := FTiledMap[I];
end;

function TWorldMap.GetMapIndex(SectionName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if (Trim(SectionName) = FSections[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function TWorldMap.GetMapMobs(I: Integer): TMobs;
begin
  Result := FTiledMapMobs[I];
end;

function TWorldMap.Go(Dir: TDir): Boolean;
var
  NextMapFileName: string;
  I, J: Integer;
  P: TMobInfo;
begin
  Result := False;
  NextMapFileName := GetCurrentMapInfo.Neighbors[Dir];
  if NextMapFileName <> '' then
  begin
    I := GetMapIndex(NextMapFileName);
    if (I < 0) then
      Exit;
    with GetCurrentMapMobs do
    begin
      P := Get(PlayerIndex);
      Del(PlayerIndex);
      PlayerIndex := -1;
      GetMapMobs(I).Add(P);
      for J := 0 to GetMapMobs(I).Count - 1 do
      begin
        P := GetMapMobs(I).Get(J);
        if P.Force = 1 then
        begin
          GetMapMobs(I).PlayerIndex := J;
          Break;
        end;
      end;
    end;
    FCurrentMap := I;
    Result := True;
  end;
end;

procedure TWorldMap.LoadFromFile(const FileName: string);
var
  I: Integer;
  F: TIniFile;
begin
  FCurrentMap := 0;
  F := TIniFile.Create(GMods.GetPath('maps', FileName));
  try
    FSections.Clear;
    F.ReadSections(FSections);
    SetLength(FMapInfo, Count);
    SetLength(FTiledMap, Count);
    SetLength(FTiledMapMobs, Count);
    for I := 0 to Count - 1 do
    begin
      FMapInfo[I].FileName := Trim(FSections[I]);
      FMapInfo[I].Neighbors[drMapLeft] := F.ReadString(FSections[I], 'MapLeft', '');
      FMapInfo[I].Neighbors[drMapUp] := F.ReadString(FSections[I], 'MapUp', '');
      FMapInfo[I].Neighbors[drMapRight] := F.ReadString(FSections[I], 'MapRight', '');
      FMapInfo[I].Neighbors[drMapDown] := F.ReadString(FSections[I], 'MapDown', '');
      FMapInfo[I].Neighbors[drMapTop] := F.ReadString(FSections[I], 'MapTop', '');
      FMapInfo[I].Neighbors[drMapBottom] := F.ReadString(FSections[I], 'MapBottom', '');
      FTiledMap[I] := TTiledMap.Create(FOwner);
      FTiledMap[I].LoadFromFile(Format('%s.tmx', [FMapInfo[I].FileName]));
      FTiledMapMobs[I] := TMobs.Create;
      FTiledMapMobs[I].LoadFromMap(I);
    end;
  finally
    FreeAndNil(F);
  end;
end;

procedure TWorldMap.Render(Canvas: TCanvas);
var
  Y: Integer;
  X: Integer;
  L: TTiledMap.TLayerEnum;
begin
  for Y := 0 to Map.GetCurrentMap.Height - 1 do
    for X := 0 to Map.GetCurrentMap.Width - 1 do
    begin
      for L := Low(TTiledMap.TLayerEnum) to TTiledMap.TLayerEnum.lrItems do
        if (Map.GetCurrentMap.FMap[L][X][Y] >= 0) then
          Canvas.Draw(X * Map.GetCurrentMap.TileSize, Y * Map.GetCurrentMap.TileSize,
            Map.GetCurrentMap.TiledObject[Map.GetCurrentMap.FMap[L][X][Y]].Image);
    end;
end;

end.
