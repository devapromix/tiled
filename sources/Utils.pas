unit Utils;

interface

function GetPath(SubDir: string): string;
procedure SetCurrentModName(const ModName: string);
function GetModPath(SubDir: string): string;

implementation

uses SysUtils;

const
  DefaultModName = 'elvion';

var
  GameModName: string = DefaultModName;

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

procedure SetCurrentModName(const ModName: string);
begin
  GameModName := ModName;
end;

function GetModPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + 'mods' + PathDelim + GameModName + PathDelim + SubDir);
end;

end.
