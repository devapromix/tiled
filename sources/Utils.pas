unit Utils;

interface

function GetPath(SubDir: string): string;
procedure SetCurrentModName(const ModName: string);
function GetModPath(const SubDir, FileName: string): string;

implementation

uses SysUtils, Dialogs;

const
  DefaultModName = 'elvion';

var
  CurrentModName: string = DefaultModName;

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

procedure SetCurrentModName(const ModName: string);
begin
  CurrentModName := ModName;
end;

function GetModPath(const SubDir, FileName: string): string;
begin
  Result := GetPath('mods' + PathDelim + CurrentModName + PathDelim + SubDir) + FileName;
  if not FileExists(Result) then
    Result := GetPath('mods' + PathDelim + DefaultModName + PathDelim + SubDir) + FileName;
end;

end.
