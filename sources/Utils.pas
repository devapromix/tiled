unit Utils;

interface

function GetPath(SubDir: string): string;

implementation

uses
  SysUtils,
  Dialogs;

function GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

end.
