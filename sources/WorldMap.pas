unit WorldMap;

interface

uses System.Classes, TiledMap;

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
    function GetCurrentMapInfo: TMapInfo;
    function GetCurrentMap: TTiledMap;
    function Go(Dir: TDir): Boolean;
  end;

var
  Map: TWorldMap;

implementation

uses System.SysUtils, System.IniFiles, Utils, Mods;

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
    FreeAndNil(FTiledMap[I]);
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

function TWorldMap.Go(Dir: TDir): Boolean;
var
  NextMapFileName: string;
  I: Integer;
begin
  Result := False;
  NextMapFileName := GetCurrentMapInfo.Neighbors[Dir];
  if NextMapFileName <> '' then
  begin
    I := GetMapIndex(NextMapFileName);
    if (I < 0) then
      Exit;
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
    end;
  finally
    FreeAndNil(F);
  end;
end;

end.
