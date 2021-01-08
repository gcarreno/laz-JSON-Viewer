{ Implements JSON.Utils

  Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit LJV.JSON.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
, jsonparser
, jsonscanner
;

function GetJSONData(const aJSON: String): TJSONData;
function GetJSONData(const aStream: TStream): TJSONData;

implementation

function GetJSONData(const aJSON: String): TJSONData;
var
  jParser: TJSONParser;
begin
  Result := nil;
{$IF FPC_FULLVERSION >= 30002}
  jParser := TJSONParser.Create(aJSON, [joUTF8, joIgnoreTrailingComma]);
{$ELSE}
  jParser := TJSONParser.Create(aJSON, True);
{$ENDIF}
  try
    Result := jParser.Parse;
  finally
    jParser.Free;
  end;
end;

function GetJSONData(const aStream: TStream): TJSONData;
var
  jParser: TJSONParser;
begin
  Result:= nil;
  aStream.Position:= 0;
{$IF FPC_FULLVERSION >= 30002}
  jParser:= TJSONParser.Create(aStream, [joUTF8, joIgnoreTrailingComma]);
{$ELSE}
  jParser := TJSONParser.Create(aStream, True);
{$ENDIF}
  try
    Result:= jParser.Parse;
  finally
    jParser.Free;
  end;
end;

end.

