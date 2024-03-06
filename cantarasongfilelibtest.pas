unit cantarasongfilelibtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, cantarasongscorefilelib;

type

  TCSSLTests= class(TTestCase)
  published
    procedure TestPropertyImport;
  end;

implementation

procedure TCSSLTests.TestPropertyImport;
var
  CantaraSongScoreFile: TCantaraSongScoreFile;
begin
  CantaraSongScoreFile := TCantaraSongScoreFile.Create;
  CantaraSongScoreFile.LoadFromFile('testfiles' + PathDelim + 'testfile01.cssf');

  AssertTrue(
    'Property title not correct. It is: ' + CantaraSongScoreFile.Properties.KeyData['title'],
       CantaraSongScoreFile.Properties.KeyData['title'] = 'Was man mit Geld nicht kaufen kann'
    );
  AssertTrue(
    'Property author not correct. It is: ' + CantaraSongScoreFile.Properties.KeyData['author'],
       CantaraSongScoreFile.Properties.KeyData['author'] = 'Jan Martin Reckel'
    );
  AssertTrue(
    'Property lang not correct. It is: ' + CantaraSongScoreFile.Properties.KeyData['lang'],
       CantaraSongScoreFile.Properties.KeyData['lang'] = 'de'
    );
  AssertTrue(
    'RefrainCount not correct. It is: ' + IntToStr(CantaraSongScoreFile.PartCount.RefrainCount),
       CantaraSongScoreFile.PartCount.RefrainCount = 1
    );
  WriteLn(CantaraSongScoreFile.SongPartArray[0].SongPartName, CantaraSongScoreFile.SongContentArray[0].ContentPartName);
  WriteLn(CantaraSongScoreFile.PartContentMatrix[0][0]);
  WriteLn(CantaraSongScoreFile.PartContentMatrix[1][0]);
  WriteLn(CantaraSongScoreFile.PartContentMatrix[2][0]);
  CantaraSongScoreFile.Destroy;
end;



initialization

  RegisterTest(TCSSLTests);
end.

