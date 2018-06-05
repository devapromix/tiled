unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Imaging.PNGImage;

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
    procedure LoadMap(const FileName: string);
  end;

type
  TTile = record
    Index: Integer;
  end;

type
  TLayer = array of array of TTile;

type
  TLayerEnum = (lrTiles, lrObjects, lrItems, lrMonsters);

type
  TMap = array [TLayerEnum] of TLayer;

type
  TTiledObject = class(TObject)
  private
  public
    Image: TPNGImage; //
    TileType: string;
    Passable: Boolean; // проходимый или нет
    Transparent: Boolean; // видно сквозь или нет
    Openable: Boolean; // можно открыть или нет
    Placeable: Boolean; // можно поставить что-то или нет
    constructor Create;
    destructor Destroy; override;
  end;

var
  Map: TMap;
  MapWidth: Integer;
  MapHeight: Integer;
  TileSize: Integer = 32;

  TiledObject: array of TTiledObject;

  PNGPlayer: TPNGImage;
  PlayerSpawnID: Integer = 0;
  PX: Integer;
  PY: Integer;

var
  Form1: TForm1;

implementation

uses Xml.XMLDoc, Xml.XMLIntf, System.IOUtils;

{$R *.dfm}

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

function GetTileType(const L: TLayerEnum; const X, Y: Integer): string;
begin
  Result := '';
  if Map[L][X][Y].Index > 0 then
    Result := TiledObject[Map[L][X][Y].Index].TileType;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  X, Y: Integer;
begin
  LoadMap('dungeon.tmx');
  ClientWidth := TileSize * MapWidth;
  ClientHeight := TileSize * MapHeight;
  Surface := TBitmap.Create;
  Surface.Width := ClientWidth;
  Surface.Height := ClientHeight;
  //
  PNGPlayer := TPNGImage.Create;
  PNGPlayer.LoadFromFile(GetPath('resources\images\races') + 'human.png');
  for Y := 0 to MapHeight - 1 do
    for X := 0 to MapWidth - 1 do
      if GetTileType(lrObjects, X, Y) = 'up_stairs' then
      begin
        PX := X;
        PY := Y;
        Break;
      end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(TiledObject) do
    TiledObject[I].Free;
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
  if (TX < 0) or (TX > MapWidth - 1) then
    Exit;
  if (TY < 0) or (TY > MapHeight - 1) then
    Exit;
  TileType := GetTileType(lrTiles, TX, TY);
  ObjType := GetTileType(lrObjects, TX, TY);
  ItemType := GetTileType(lrItems, TX, TY);
  MonType := GetTileType(lrMonsters, TX, TY);
  if TileType = 'wall' then
    Exit;
  if (ObjType = 'closed_door') or (ObjType = 'hidden_door') then
  begin
    Map[lrObjects][TX][TY].Index := Map[lrObjects][TX][TY].Index + 1;
    Form1.FormPaint(Form1);
    Exit;
  end;
  if (ObjType = 'up_stairs') or (ObjType = 'down_stairs') then
    Halt;
  if (ItemType <> '') then
  begin
    Map[lrItems][TX][TY].Index := -1;
  end;
  if (MonType <> '') then
  begin
    Map[lrMonsters][TX][TY].Index := -1;
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
  for Y := 0 to MapHeight - 1 do
    for X := 0 to MapWidth - 1 do
    begin
      for L := Low(TLayerEnum) to High(TLayerEnum) do
        if (Map[L][X][Y].Index >= 0) then
          Surface.Canvas.Draw(X * TileSize, Y * TileSize,
            TiledObject[Map[L][X][Y].Index].Image);
    end;
  Surface.Canvas.Draw(PX * TileSize, PY * TileSize, PNGPlayer);
  Canvas.Draw(0, 0, Surface);
end;

procedure TForm1.LoadMap(const FileName: string);
var
  XMLDoc: TXMLDocument;
  Node: IXMLNode;
  S, F, LayerName: string;
  I, Count, ID: Integer;

  procedure LoadLayer(L: TLayerEnum);
  var
    X, Y: Integer;
    SL: TStringList;
    V: TArray<string>;
  begin
    SetLength(Map[L], MapWidth, MapHeight);
    Node := XMLDoc.DocumentElement.ChildNodes[I].ChildNodes['data'];
    SL := TStringList.Create;
    SL.Text := Trim(Node.Text);
    for Y := 0 to MapHeight - 1 do
    begin
      V := SL[Y].Split([',']);
      for X := 0 to MapWidth - 1 do
        Map[L][X][Y].Index := StrToIntDef(V[X], 0) - 1;
    end;
    SL.Free;
  end;

  procedure LoadTileset(const FileName: string);
  var
    XMLDoc: TXMLDocument;
    Node: IXMLNode;
    I, Count: Integer;
    Section: string;
    TileType: string;
  begin
    XMLDoc := TXMLDocument.Create(Self);
    XMLDoc.LoadFromFile(FileName);
    try
      Count := XMLDoc.DocumentElement.ChildNodes.Count;
      for I := 0 to Count - 1 do
      begin
        Section := TPath.GetFileNameWithoutExtension(FileName);
        Node := XMLDoc.DocumentElement.ChildNodes[I];
        if Node.NodeName = 'tile' then
        begin
          TileType := Trim(Node.Attributes['type']);
          Node := Node.ChildNodes['image'];
          SetLength(TiledObject, ID + 1);
          TiledObject[ID] := TTiledObject.Create;
          TiledObject[ID].TileType := TileType;
          TiledObject[ID].Image.LoadFromFile
            (GetPath('resources\images\' + Section) +
            Trim(Node.Attributes['source']));
          Inc(ID);
        end;
      end;
    finally
      XMLDoc.Free;
    end;
  end;

begin
  ID := 0;
  XMLDoc := TXMLDocument.Create(Self);
  F := GetPath('resources\maps');
  XMLDoc.LoadFromFile(F + FileName);
  try
    Count := XMLDoc.DocumentElement.ChildNodes.Count;
    for I := 0 to Count - 1 do
    begin
      Node := XMLDoc.DocumentElement.ChildNodes[I];
      if Node.NodeName = 'tileset' then
        LoadTileset(F + Node.Attributes['source']);
      if Node.NodeName = 'layer' then
      begin
        LayerName := Node.Attributes['name'];
        S := Node.Attributes['width'];
        MapWidth := StrToIntDef(S, 0);
        S := Node.Attributes['height'];
        MapHeight := StrToIntDef(S, 0);
        if (LayerName = 'Tiles') then
          LoadLayer(lrTiles);
        if (LayerName = 'Objects') then
          LoadLayer(lrObjects);
        if (LayerName = 'Items') then
          LoadLayer(lrItems);
        if (LayerName = 'Monsters') then
          LoadLayer(lrMonsters);
      end;
    end;
  finally
    XMLDoc.Free;
  end;
  // Halt;
end;

{ TTiledObject }

constructor TTiledObject.Create;
begin
  Image := TPNGImage.Create;
end;

destructor TTiledObject.Destroy;
begin
  Image.Free;
  inherited;
end;

end.
