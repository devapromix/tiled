unit Test.Player;

interface

uses Vcl.Imaging.PNGImage, Vcl.Graphics, Creature;

type
  TPlayer = class(TCreature)
  private
    FImage: TPNGImage;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(Surface: TBitmap);
  end;

var
  Player: TPlayer;

implementation

uses System.SysUtils, Utils, GameMod;

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited;
  FImage := TPNGImage.Create;
  FImage.LoadFromFile(GetModPath('races') + 'human.png');
  FWidth := FImage.Width;
  FHeight := FImage.Height;
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TPlayer.Render(Surface: TBitmap);
begin
  Surface.Canvas.Draw(X * FWidth, Y * FHeight, FImage);
end;

initialization

Player := TPlayer.Create;

finalization

FreeAndNil(Player);

end.
