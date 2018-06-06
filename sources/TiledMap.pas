unit TiledMap;

interface

uses System.Classes, Vcl.Imaging.PNGImage;

type
  TLayer = array of array of Integer;

type
  TLayerEnum = (lrTiles, lrObjects, lrItems, lrMonsters);

type
  TTiledObject = class(TObject)
  private
  public
    Image: TPNGImage;
    TileType: string;
    Passable: Boolean;
    Transparent: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

type
  TTiledMap = class(TObject)
  private
    FWidth: Integer;
    FHeight: Integer;
    FTileSize: Integer;
    FOwner: TComponent;
  public
    FMap: array [TLayerEnum] of TLayer;
    Firstgid: array [TLayerEnum] of Integer;
    TiledObject: array of TTiledObject;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property TileSize: Integer read FTileSize;
    function GetTileType(const L: TLayerEnum; const X, Y: Integer): string;
  end;

implementation

uses SysUtils, Math, Dialogs, Xml.XMLDoc, Xml.XMLIntf, System.IOUtils, Unit1;

{ TTiledMap }

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

constructor TTiledMap.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FWidth := 100;
  FHeight := 100;
  FTileSize := 32;
end;

destructor TTiledMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(TiledObject) do
    TiledObject[I].Free;
  inherited;
end;

function TTiledMap.GetTileType(const L: TLayerEnum;
  const X, Y: Integer): string;
begin
  Result := '';
  if (FMap[L][X][Y] > 0) then
    Result := TiledObject[FMap[L][X][Y]].TileType;
end;

procedure TTiledMap.LoadFromFile(const FileName: string);
var
  XMLDoc: TXMLDocument;
  Node: IXMLNode;
  S, F, LayerName: string;
  Section: string;
  I, Count, ID: Integer;

  procedure LoadLayer(L: TLayerEnum);
  var
    X, Y: Integer;
    SL: TStringList;
    V: TArray<string>;
  begin
    SetLength(FMap[L], FWidth, FHeight);
    Node := XMLDoc.DocumentElement.ChildNodes[I].ChildNodes['data'];
    SL := TStringList.Create;
    SL.Text := Trim(Node.Text);
//    ShowMessage(SL.Text);
    for Y := 0 to FHeight - 1 do
    begin
      V := SL[Y].Split([',']);
      for X := 0 to FWidth - 1 do
        FMap[L][X][Y] := StrToIntDef(V[X], 0) - 1;
    end;
    SL.Free;
  end;

  procedure LoadTileset(const FileName: string);
  var
    XMLDoc: TXMLDocument;
    Node, NodeProps, NodeProp: IXMLNode;
    I, J, Count, PropCount, Index: Integer;
    S, Name, Value, TileType: string;
  begin
    XMLDoc := TXMLDocument.Create(Form1);
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
          // Index := StrToIntDef(Trim(Node.Attributes['id']), 0);
          NodeProps := Node.ChildNodes['properties'];
          PropCount := NodeProps.ChildNodes.Count;
          Node := Node.ChildNodes['image'];
          SetLength(TiledObject, ID + 1);
          TiledObject[ID] := TTiledObject.Create;
          TiledObject[ID].TileType := TileType;
          TiledObject[ID].Image.LoadFromFile
            (GetPath('resources\images\' + Section) +
            Trim(Node.Attributes['source']));
          for J := 0 to PropCount - 1 do
          begin
            NodeProp := NodeProps.ChildNodes[J];
            if Section = 'tiles' then
            begin
              Name := NodeProp.Attributes['name'];
              Value := NodeProp.Attributes['value'];
              if (Name = 'passable') then
                TiledObject[ID].Passable := StrToBool(Value);
              if (Name = 'transparent') then
                TiledObject[ID].Transparent := StrToBool(Value);
              // ShowMessage();
            end;
          end;
          Inc(ID);
        end;
      end;
    finally
      XMLDoc.Free;
    end;
  end;

begin
  ID := 0;
  XMLDoc := TXMLDocument.Create(Form1);
  F := GetPath('resources\maps');
  XMLDoc.LoadFromFile(F + FileName);
  try
    FTileSize := StrToIntDef(XMLDoc.DocumentElement.Attributes
      ['tilewidth'], 32);
    FWidth := StrToIntDef(XMLDoc.DocumentElement.Attributes['width'], 100);
    FHeight := StrToIntDef(XMLDoc.DocumentElement.Attributes['height'], 100);
    Count := XMLDoc.DocumentElement.ChildNodes.Count;
    for I := 0 to Count - 1 do
    begin
      Node := XMLDoc.DocumentElement.ChildNodes[I];
      if Node.NodeName = 'tileset' then
      begin
        S := F + Trim(Node.Attributes['source']);
        Section := TPath.GetFileNameWithoutExtension(S);
        if Section = 'tiles' then
          Firstgid[lrTiles] := StrToInt(Trim(Node.Attributes['firstgid']));
        if Section = 'objects' then
          Firstgid[lrObjects] := StrToInt(Trim(Node.Attributes['firstgid']));
        if Section = 'items' then
          Firstgid[lrItems] := StrToInt(Trim(Node.Attributes['firstgid']));
        if Section = 'monsters' then
          Firstgid[lrMonsters] := StrToInt(Trim(Node.Attributes['firstgid']));
        LoadTileset(S);
      end;
      {if Node.NodeName = 'layer' then
      begin
        LayerName := Node.Attributes['name'];
        if (LayerName = 'tiles') then
          LoadLayer(lrTiles);
        if (LayerName = 'objects') then
          LoadLayer(lrObjects);
        if (LayerName = 'items') then
          LoadLayer(lrItems);
        if (LayerName = 'monsters') then
          LoadLayer(lrMonsters);
      end;}
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
