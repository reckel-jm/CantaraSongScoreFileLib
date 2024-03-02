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
  CantaraSongScoreFile.Destroy;
end;



initialization

  RegisterTest(TCSSLTests);
end.

