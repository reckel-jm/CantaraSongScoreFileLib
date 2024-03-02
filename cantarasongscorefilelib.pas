unit cantarasongscorefilelib;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, fgl;

type

  TStringStringDictionary = specialize TFPGMap<String, String>;

  TCantaraSongScoreFileImportErrorType = (Syntax, Incomplete, Double);

  { A possibel Tpye of a part of a song }
  TCantaraSongPartType = (sptChorus, sptVerse, sptBridge);
  TCantaraSongContentType = (sctScore, sctChords, sctLyrics, sctLyricsDescription);

  TCantaraSongScoreFileImportError = record
    ErrorType: TCantaraSongScoreFileImportErrorType;
    Line: Integer;
  end;

  { TCantaraSongContentHeader }

  TCantaraSongContentHeader = record
    ContentPartName: String;
    ContentType: TCantaraSongContentType;
  end;

  { TCantaraSongPartHeader }

  TCantaraSongPartHeader = record
    SongPartName: String;
    Number: Integer;
    SongPartType: TCantaraSongPartType;
    IsRepitition: Boolean;
  end;


  { TCantaraSongScoreFile }

  TCantaraSongScoreFile = class(TObject)
    private
      InputFile: TStringList;
      SongPartArray: array of TCantaraSongPartHeader;
      ContentPartArray: array of TCantaraSongContentHeader;
      procedure ParseInputFile;
    public
      Properties: TStringStringDictionary;
      procedure AddContent(ContentAddress: String; Content: String);
      function FindPartIndex(ACantaraSongPart: TCantaraSongPartHeader): Integer;
      constructor Create;
      destructor  Destroy; override;
      procedure LoadFromFile(FileName: String);
  end;

const
  REGEX_PARSEPROPERTY = '#\s*(.+?):\s*(.+?)\n';
  REGEX_PARSECONTENTKEY  = '#\s*([^:]+?)$';

implementation

{ TCantaraSongScoreFile }

procedure TCantaraSongScoreFile.ParseInputFile;
var
  rePropertyParser, reContentParser: TRegExpr;
  ContentValue: String;
  KeyValue: String;
  line: Integer;
begin

  { Parse Properties }
  rePropertyParser := TRegExpr.Create(REGEX_PARSEPROPERTY);
  if rePropertyParser.Exec(InputFile.Text) then
  begin
    repeat
      Self.Properties.Add(rePropertyParser.Match[1], Trim(rePropertyParser.Match[2]));
    until not rePropertyParser.ExecNext;
  end;
  rePropertyParser.Destroy;

  { Parse Content }
  reContentParser := TRegExpr.Create(REGEX_PARSECONTENTKEY);

  line := 0;
  while line < Self.InputFile.Count-1 do
  begin
    if reContentParser.Exec(InputFile.Strings[line]) then
    begin
      KeyValue := reContentParser.Match[1];
      line := line + 1;
      ContentValue := '';
      while (line <= Self.InputFile.Count -1) and (not reContentParser.Exec(InputFile.Strings[line])) do
      begin
        ContentValue := ContentValue + InputFile.Strings[line] + LineEnding;
        line := line + 1;
      end;
      Self.AddContent(Trim(LowerCase(KeyValue)), Trim(ContentValue));
    end
    else line := line + 1;
  end;
  reContentParser.Destroy;

end;

procedure TCantaraSongScoreFile.AddContent(ContentAddress: String; Content: String);
const
  PartAddressRegex = '(chorus|verse|bridge|refrain)(\d*)';
var
  ContentAddressParts: array of String;
  rePartParser: TRegExpr;
  SongPart: TCantaraSongPartHeader;
begin
  rePartParser := TRegExpr.Create(PartAddressRegex);
  ContentAddressParts := ContentAddress.Split('.');

  if rePartParser.Exec(ContentAddressParts[0]) then
  begin
    SongPart.SongPartName:=ContentAddressParts[0];
    SongPart.Number:=StrToInt(rePartParser.Match[2]);
    case LowerCase(rePartParser.Match[1]) of
      'refrain': SongPart.SongPartType:= TCantaraSongPartType.sptChorus;
      'chorus' : SongPart.SongPartType:= TCantaraSongPartType.sptChorus;
      'verse'  : SongPart.SongPartType:= TCantaraSongPartType.sptVerse;
      'bridge' : SongPart.SongPartType:= TCantaraSongPartType.sptBridge;
    end;
  end;

  SongPart.IsRepitition:= (Self.FindPartIndex(SongPart) > -1);

  SetLength(SongPartArray, Length(SongPartArray)+1);
  SongPartArray[Length(SongPartArray)-1] := SongPart;
  WriteLn('SongPart: ', SongPart.SongPartName);

  rePartParser.Destroy;
end;

function TCantaraSongScoreFile.FindPartIndex(ACantaraSongPart: TCantaraSongPartHeader): Integer;
var
  i: Integer;
  CantaraSongPartHeader: TCantaraSongPartHeader;
begin
  for i := 0 to Length(Self.SongPartArray)-1 do
  begin
    CantaraSongPartHeader := Self.SongPartArray[i];
    if CantaraSongPartHeader.SongPartName = ACantaraSongPart.SongPartName then
       Exit(i);
  end;

  Result := -1;
end;

constructor TCantaraSongScoreFile.Create;
begin
  InputFile := TStringList.Create;

  Self.Properties := TStringStringDictionary.Create;
end;

destructor TCantaraSongScoreFile.Destroy;
begin
  inherited Destroy;

  InputFile.Destroy;
  Self.Properties.Destroy;
end;

procedure TCantaraSongScoreFile.LoadFromFile(FileName: String);
begin
  InputFile.LoadFromFile(FileName);
  Self.ParseInputFile;
end;

end.

