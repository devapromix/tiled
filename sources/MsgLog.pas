unit MsgLog;

interface

uses Classes, Graphics;

type
  TLog = class(TObject)
  private
    FMsg: string;
    FLog: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Turn;
    procedure Add(const Msg: string);
    function Get: string;
    procedure Render(Canvas: TCanvas);
  end;

var
  Log: TLog;

implementation

uses SysUtils, WorldMap;

{ TLog }

procedure TLog.Add(const Msg: string);
begin
  FMsg := FMsg + Trim(Msg) + ' ';
end;

constructor TLog.Create;
begin
  FLog := TStringList.Create;
  FMsg := '';
end;

destructor TLog.Destroy;
begin
  FreeAndNil(FLog);
  inherited;
end;

function TLog.Get: string;
begin
  Result := Trim(FMsg);
end;

procedure TLog.Render(Canvas: TCanvas);
begin
  Canvas.TextOut(0, Map.GetCurrentMap.TileSize * Map.GetCurrentMap.Height + 16, Get);
end;

procedure TLog.Turn;
begin
  if FMsg <> '' then
  begin
    FLog.Append(Get);
    FMsg := '';
  end;
end;

initialization

Log := TLog.Create;

finalization

FreeAndNil(Log);

end.
