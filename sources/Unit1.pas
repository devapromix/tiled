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
  private
    { Private declarations }
  public
    { Public declarations }
    Surface: TPNGImage;
    Tileset: TPNGImage;
    procedure LoadMap(const FileName: string);
  end;

type
  TLayer = array of array of Integer;

type
  TLayerEnum = (lrTiles, lrObjects, lrItems, lrMonsters);

type
  TMap = array [TLayerEnum] of TLayer;

var
  Map: TMap;
  MapWidth: Integer;
  MapHeight: Integer;
  Firstgid: Integer = 1;
  TilesetWidth: Integer = 8;
  TileSize: Integer = 32;
  PNGTile: array of TPNGImage;

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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Tileset := TPNGImage.Create;
  LoadMap('dungeon.tmx');
  ClientWidth := TileSize * MapWidth;
  ClientHeight := TileSize * MapHeight;
  Surface := TPNGImage.CreateBlank(COLOR_RGB, 16, ClientWidth, ClientHeight);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Tileset.Free;
  Surface.Free;
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
        if (Map[L][X][Y] >= 0) then
          Surface.Canvas.Draw(X * TileSize, Y * TileSize,
            PNGTile[Map[L][X][Y]]);
    end;
  Canvas.Draw(0, 0, Surface);
end;

procedure TForm1.LoadMap(const FileName: string);
var
  XMLDoc: TXMLDocument;
  Node: IXMLNode;
  L: TLayerEnum;
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
        Map[L][X][Y] := StrToIntDef(V[X], 0) - Firstgid;
    end;
    // SL.SaveToFile(IntToStr(Ord(L)));
    SL.Free;
  end;

  procedure LoadTileset(const FileName: string);
  var
    XMLDoc: TXMLDocument;
    Node: IXMLNode;
    I, Count: Integer;
  begin
    XMLDoc := TXMLDocument.Create(Self);
    XMLDoc.LoadFromFile(FileName);
    try
      Count := XMLDoc.DocumentElement.ChildNodes.Count;
      for I := 0 to Count - 1 do
      begin
        Node := XMLDoc.DocumentElement.ChildNodes[I];
        if Node.NodeName = 'tile' then
        begin
          Node := Node.ChildNodes['image'];
          SetLength(PNGTile, ID + 1);
          PNGTile[ID] := TPNGImage.Create;
          PNGTile[ID].LoadFromFile
            (GetPath('resources\images\' + TPath.GetFileNameWithoutExtension
            (FileName)) + Trim(Node.Attributes['source']));
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
      begin
        LoadTileset(F + Node.Attributes['source']);
        // ShowMessage(Node.Attributes['firstgid']);
      end;
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

end.
