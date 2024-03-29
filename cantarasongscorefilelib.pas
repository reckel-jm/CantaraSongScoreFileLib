unit cantarasongscorefilelib;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, fgl;

type

  TStringStringDictionary = specialize TFPGMap<String, String>;

  TCantaraSongScoreFileImportErrorType = (etSyntax, etIncomplete, etDouble);

  { The TSongStructureType describes the structure of a song.
    - ssVerseRefrainBridgeRefrain: applies for any song which starts with a verse. If a refrain is available,
    the latest refrain will follow after every verse. If a bridge is available, the bridge will be included after the last refrain followed by the refrain as well.
    - similar to ssVerseRefrainBridgeRefrain, but a refrain is inserted before the first song.
    - ssCustom: a custom song structure which is not automatically generated and defined elsewhere in the song file.
  }
  TSongStructureType = (ssVerseRefrainBridgeRefrain,
                 ssRefrainVerseRefrainBridgeRefrain,
                 ssCustom);

  { A possible Type of a part of a song }
  TCantaraSongPartType = (sptRefrain, sptStanza, sptBridge);
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
  TCantaraSongPartCount = record
    RefrainCount: Integer;
    StanzaCount: Integer;
    BridgeCount: Integer;
    Total: Integer;
  end;


  { TCantaraSongScoreFile }

  TCantaraSongScoreFile = class(TObject)
    private
      InputFile: TStringList;
      Errors: array of TCantaraSongScoreFileImportError;
      ParsingLine: Integer;
      SongStructureType: TSongStructureType;
      SongStructures: array of array of TCantaraSongPartHeader;
      procedure ParseInputFile;
      procedure AutoGenerateSongStructure;
      function fGetPartCount: TCantaraSongPartCount;
    public
      Properties: TStringStringDictionary;
      SongPartArray: array of TCantaraSongPartHeader;
      SongContentArray: array of TCantaraSongContentHeader;
      PartContentMatrix: array of array of String;
      procedure AddContent(ContentAddress: String; Content: String);
      function FindPartIndex(ACantaraSongPart: TCantaraSongPartHeader): Integer;
      function FindContentIndex(AContentHeader: TCantaraSongContentHeader): Integer;
      function FindHighestNumerForPartType(ASongPartType: TCantaraSongPartType): Integer;
      constructor Create;
      destructor  Destroy; override;
      procedure LoadFromFile(FileName: String);

      property PartCount: TCantaraSongPartCount read fGetPartCount;
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
  idx: Integer;
begin

  { Parse Properties }
  rePropertyParser := TRegExpr.Create(REGEX_PARSEPROPERTY);
  if rePropertyParser.Exec(InputFile.Text) then
  begin
    repeat
      Self.Properties.Add(LowerCase(rePropertyParser.Match[1]), Trim(rePropertyParser.Match[2]));
    until not rePropertyParser.ExecNext;
  end;
  rePropertyParser.Destroy;

  { Catch pre-defined song structre }
  if Self.Properties.Find('structure', idx) = false then
    Self.SongStructureType:=TSongStructureType.ssVerseRefrainBridgeRefrain
  else if Self.Properties.KeyData['structure'] = 'verse-refrain' then
    Self.SongStructureType:=TSongStructureType.ssVerseRefrainBridgeRefrain
  else if Self.Properties.KeyData['structure'] = 'refrain-verse' then
    Self.SongStructureType:=TSongStructureType.ssRefrainVerseRefrainBridgeRefrain
  else
    Self.SongStructureType:=TSongStructureType.ssCustom;

  { Parse Content }
  reContentParser := TRegExpr.Create(REGEX_PARSECONTENTKEY);

  line := 0;
  while line < Self.InputFile.Count-1 do
  begin
    Self.ParsingLine:=line;
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

procedure TCantaraSongScoreFile.AutoGenerateSongStructure;
var
  EvaluatedPartCount: TCantaraSongPartCount;
  skipRefrain, skipBridge: boolean;
begin
  SetLength(SongStructures, Length(SongStructures)+1);
  EvaluatedPartCount := Self.PartCount;
  if EvaluatedPartCount.RefrainCount = 0 then skipRefrain := true;
  if EvaluatedPartCount.BridgeCount = 0 then skipBridge := true;

  if not skipRefrain and Self.SongStructureType = TSongStructureType.ss;
end;


function TCantaraSongScoreFile.fGetPartCount: TCantaraSongPartCount;
var i: Integer;
begin
  Result.RefrainCount:=0;
  Result.StanzaCount:=0;
  Result.BridgeCount:=0;
  Result.Total:=0;

  for i := 0 to Length(Self.SongPartArray)-1 do
    case Self.SongPartArray[i].SongPartType of
      TCantaraSongPartType.sptRefrain : Result.RefrainCount+=1;
      TCantaraSongPartType.sptStanza  : Result.StanzaCount+=1;
      TCantaraSongPartType.sptBridge  : Result.BridgeCount+=1;
    end;
  Result.Total:=Length(Self.SongPartArray);
end;

procedure TCantaraSongScoreFile.AddContent(ContentAddress: String; Content: String);
const
  PartAddressRegex = '(chorus|verse|bridge|refrain)(\d*)';
var
  ContentAddressParts: array of String;
  rePartParser: TRegExpr;
  SongPart: TCantaraSongPartHeader;
  SongContent: TCantaraSongContentHeader;
  ContentHeaderName: String;
  PartIndex, ContentIndex: Integer;
  i: Integer;
begin
  rePartParser := TRegExpr.Create(PartAddressRegex);
  ContentAddressParts := LowerCase(ContentAddress).Split('.');

  // TODO: Catch error if less then two parts (no content specified)

  if rePartParser.Exec(ContentAddressParts[0]) then
  begin
    SongPart.SongPartName:=ContentAddressParts[0];
    case LowerCase(rePartParser.Match[1]) of
      'refrain': SongPart.SongPartType:= TCantaraSongPartType.sptRefrain;
      'chorus' : SongPart.SongPartType:= TCantaraSongPartType.sptRefrain;
      'verse'  : SongPart.SongPartType:= TCantaraSongPartType.sptStanza;
      'bridge' : SongPart.SongPartType:= TCantaraSongPartType.sptBridge;
    end;

    if rePartParser.Match[2] = '' then
    begin
      // TODO: ggf. Konzept überdenken
      //SongPart.Number := Self.FindHighestNumerForPartType(SongPart.SongPartType) + 1;
      SongPart.Number := 1;
      SongPart.SongPartName:=SongPart.SongPartName + IntToStr(SongPart.Number);
    end
    else
      SongPart.Number:=StrToInt(rePartParser.Match[2]);
  end;
  rePartParser.Destroy;

  PartIndex := Self.FindPartIndex(SongPart);
  if PartIndex = -1 then
  begin
    SetLength(SongPartArray, Length(SongPartArray)+1);
    SongPartArray[Length(SongPartArray)-1] := SongPart;
    PartIndex := Length(SongPartArray)-1;
    WriteLn('SongPart: ', SongPart.SongPartName, ' ', SongPart.Number);
  end;

  // Now fill the content
  ContentHeaderName := '';
  for i := 1 to Length(ContentAddressParts)-1 do
    ContentHeaderName:=ContentHeaderName+ContentAddressParts[i];
  SongContent.ContentPartName:=ContentHeaderName;
  ContentIndex := Self.FindContentIndex(SongContent);

  If ContentIndex = -1 then
  begin
    SetLength(SongContentArray, Length(SongContentArray)+1);
    SongContentArray[Length(SongContentArray)-1].ContentPartName:=ContentHeaderName;
    { Detect and set ContentType }
    if pos('lyrics', ContentAddressParts[1]) > 0 then
      SongContentArray[Length(SongContentArray)-1].ContentType:=sctLyrics
    else if pos('score', ContentAddressParts[1]) > 0 then
      SongContentArray[Length(SongContentArray)-1].ContentType:=sctScore
    else if pos('chords', ContentAddressParts[1]) > 0 then
      SongContentArray[Length(SongContentArray)-1].ContentType:=sctChords
    else if pos('description', ContentAddressParts[1]) > 0 then
    SongContentArray[Length(SongContentArray)-1].ContentType:=sctLyricsDescription;
    ContentIndex := Length(SongContentArray)-1;
  end;

  SetLength(
    Self.PartContentMatrix,
    Length(Self.SongPartArray),
    Length(Self.SongContentArray)
  );
  Self.PartContentMatrix[PartIndex][ContentIndex] := Content;

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

function TCantaraSongScoreFile.FindContentIndex(
  AContentHeader: TCantaraSongContentHeader): Integer;
var i: Integer;
  CantaraSongContentHeader: TCantaraSongContentHeader;
begin
  for i := 0 to Length(Self.SongContentArray)-1 do
  begin
    CantaraSongContentHeader := Self.SongContentArray[i];
    if CantaraSongContentHeader.ContentPartName = AContentHeader.ContentPartName then
       Exit(i);
  end;

  Result := -1;
end;

function TCantaraSongScoreFile.FindHighestNumerForPartType(
  ASongPartType: TCantaraSongPartType): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to length(SongPartArray)-1 do
  begin
    if ((SongPartArray[i].SongPartType = ASongPartType) and
       (SongPartArray[i].Number > Result))
    then Result := SongPartArray[i].Number;
  end;
end;

constructor TCantaraSongScoreFile.Create;
begin
  InputFile := TStringList.Create;

  Self.Properties := TStringStringDictionary.Create;
  Self.Properties.Sorted:=True;
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

