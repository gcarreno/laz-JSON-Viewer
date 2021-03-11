{ Implements Forms.Main

  This file is part of laz-JSON-Viewer

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
unit LJV.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, ExtCtrls
, StdCtrls
, PairSplitter
, JSONPropStorage
, ActnList
, StdActns, ComCtrls, SynEdit, SynHighlighterJScript
, fpjson
, VirtualTrees
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actViewTreeJSON: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    lblPath: TLabel;
    lblValue: TLabel;
    pcMain: TPageControl;
    panPath: TPanel;
    panValue: TPanel;
    JSONPropStorage: TJSONPropStorage;
    lblCount: TLabel;
    lblName: TLabel;
    lblType: TLabel;
    lbFiles: TListBox;
    psMain: TPairSplitter;
    pssTree: TPairSplitterSide;
    pssNode: TPairSplitterSide;
    Splitter1: TSplitter;
    SynEdit: TSynEdit;
    SynJScriptSyn: TSynJScriptSyn;
    tsTree: TTabSheet;
    tsJSON: TTabSheet;
    vstJSON: TVirtualStringTree;
    panItem: TPanel;
    procedure actViewTreeJSONExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure lbFilesSelectionChange(Sender: TObject; User: boolean);
    procedure vstJSONChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstJSONGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstJSONGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FJSON: TJSONData;
    FFileList: Array of String;

    procedure SetupShortcuts;

    procedure AddFile(const AFilename: String);
    procedure ClearTree;
    function FormatBytes(ABytes: Int64): String;

    procedure OpenPropStorage;
    procedure ClosePropStorage;

    procedure ClearLabels;
    procedure CorrectPairSplitterCursor;
    procedure ProcessParams;
    procedure UpdateFileList;
    procedure LoadFile(const AFilename: String);
    procedure UpdateTree;
    procedure UpdateTreeFromNode(const ANode: PVirtualNode;
      const AJSONData: TJSONData; const APath: String);
    procedure ShowValue(const AJSONData: TJSONData);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  StrUtils
, DateUtils
, LCLType
, LJV.Application.Version
, LJV.JSON.Utils
, LJV.Tree.Nodes
;

resourcestring
  rsFormCaption = 'JSON Viewer v%s';
  rsFormCaptionFile = 'JSON Viewer (%s) v%s';

  rsVSTUnknown = 'Unknown';
  rsVSTNoColumns = 'There are no Columns';

  rsLabelTypeEmpty = 'Node Type';
  rsLabelType = 'Type: %s';
  rsLabelNameEmpty = 'Node Name/Index';
  rsLabelNameName = 'Name: "%s"';
  rsLabelNameArrayItem = 'Item: %d';
  rsLabelCountEmpty = 'Member Count';
  rsLabelCountArray = 'Items: %d';
  rsLabelCountObject = 'Members: %d';
  rsLabelCountNA = 'N/A';

  rsCaptionValue = 'Value';
  rsLabelFormated = 'Formated';
  rsLabelBinary = 'Binary';
  rsLabelHexadecimal = 'Hexadecimal';
  rsLabelDateTime = 'Date';
  rsLabelBytes = 'Bytes';
  rsLabelScientific = 'Scientific';
  rsCaptionPath = 'Path';

  rsBytes = ' B';
  rsKiloBytes = ' KB';
  rsMegaBytes = ' MB';
  rsGigaBytes = ' GB';
  rsTeraBytes = ' TB';

  rsTypeUnknown = 'Unknown';
  rsTypeNumberInteger = 'Number (Integer)';
  rsTypeNumberInt64 = 'Number (Int64)';
  rsTypeNumberFloat = 'Number (Float)';
  rsTypeNumberQWord = 'Number (QWord)';
  rsTypeString = 'String';
  rsTypeBoolean = 'Boolean';
  rsTypeNull = 'Null';
  rsTypeArray = 'Array';
  rsTypeObject = 'Object';

const
  cDateTimeFormat = 'yyyy/mm/dd hh:nn:ss';

  cNumberFormatInteger = '#,##0';
  cNumberFormatFloat = '#,##0.###############';
  cNumberFormatFloatScientific = '0.###############E+0';

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OpenPropStorage;
  SetupShortcuts;
  Caption:= Format(rsFormCaption, [cVersion]);
  lblValue.Caption:= rsCaptionValue;
  pcMain.ActivePageIndex:= 0;
  ClearLabels;
  CorrectPairSplitterCursor;
  ProcessParams;
  UpdateFileList;
  {
    If anyone wants to select the first file upon launch
    un-comment the following lines
  }
  //if lbFiles.Items.Count > 0 then
  //begin
  //  lbFiles.ItemIndex:= 0;
  //end;
end;

procedure TfrmMain.actViewTreeJSONExecute(Sender: TObject);
begin
  if pcMain.ActivePageIndex = 0 then
  begin
    pcMain.ActivePageIndex:= 1;
  end
  else
  begin
    pcMain.ActivePageIndex:= 0;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FJSON) then
  begin
    FJSON.Free;
  end;
  ClosePropStorage;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  filename: String;
begin
  for filename in FileNames do
  begin
    AddFile(filename);
  end;
  UpdateFileList;
  ClearTree;
  ClearLabels;
end;

procedure TfrmMain.lbFilesSelectionChange(Sender: TObject; User: boolean);
var
  index: Integer;
begin
  if User then
  begin
    // Do nothing. Just to eliminate warning about variable User
  end;
  if lbFiles.ItemIndex > -1 then
  begin
    Caption:= Format(rsFormCaptionFile, [FFileList[lbFiles.ItemIndex], cVersion]);
    ClearLabels;
    for index:=0 to pred(panValue.ComponentCount) do
    begin
      if Assigned(panValue.Components[index]) then
      begin
        panValue.Components[index].Free;
      end;
    end;
    LoadFile(FFileList[lbFiles.ItemIndex]);
    UpdateTree;
  end;
end;

procedure TfrmMain.vstJSONChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  treeNode: PTreeNode;
  sName: String;
  iIndex: Int64;
  count: Integer;
begin
  if Assigned(Node) then
  begin
    treeNode:= vstJSON.GetNodeData(Node);
    if Assigned(treeNode) then
    begin
      lblPath.Caption:= Format('%s: %s', [rsCaptionPath, treeNode^.NodePath]);
      sName:= treeNode^.NodeName;
      iIndex:= treeNode^.NodeIndex;
      if Length(sName) > 0 then
      begin
        lblName.Caption:= Format(rsLabelNameName, [sName])
      end;
      if iIndex > -1 then
      begin
        lblName.Caption:= Format(rsLabelNameArrayItem, [iIndex])
      end;
      case treeNode^.NodeType of
        jtUnknown:begin
          lblType.Caption:= Format(rsLabelType, [rsTypeUnknown]);
          lblCount.Caption:= rsLabelCountNA;
        end;
        jtNumber:begin
          case TJSONNumber(treeNode^.NodeData).NumberType of
            ntInteger:begin
              lblType.Caption:= Format(rsLabelType, [rsTypeNumberInteger]);
            end;
            ntInt64:begin
              lblType.Caption:= Format(rsLabelType, [rsTypeNumberInt64]);
            end;
            ntFloat:begin
              lblType.Caption:= Format(rsLabelType, [rsTypeNumberFloat]);
            end;
            ntQWord:begin
              lblType.Caption:= Format(rsLabelType, [rsTypeNumberQWord]);
            end;
          end;
          lblCount.Caption:= rsLabelCountNA;
        end;
        jtString:begin
          lblType.Caption:= Format(rsLabelType, [rsTypeString]);
          lblCount.Caption:= rsLabelCountNA;
        end;
        jtBoolean:begin
          lblType.Caption:= Format(rsLabelType, [rsTypeBoolean]);
          lblCount.Caption:= rsLabelCountNA;
        end;
        jtNull:begin
          lblType.Caption:= Format(rsLabelType, [rsTypeNull]);
          lblCount.Caption:= rsLabelCountNA;
        end;
        jtArray:begin
          lblType.Caption:= Format(rsLabelType, [rsTypeArray]);
          count:= TJSONArray(treeNode^.NodeData).Count;
          lblCount.Caption:= Format(rsLabelCountArray, [count]);
        end;
        jtObject:begin
          lblType.Caption:= Format(rsLabelType, [rsTypeObject]);
          count:= TJSONObject(treeNode^.NodeData).Count;
          lblCount.Caption:= Format(rsLabelCountObject, [count]);
        end;
      end;
      ShowValue(treeNode^.NodeData);
    end;
  end;
end;

procedure TfrmMain.vstJSONGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:= SizeOf(TTreeNode);
end;

procedure TfrmMain.vstJSONGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  treeNode: PTreeNode;
  jNumber: TJSONNumber;
  jString: TJSONString;
  jBoolean: TJSONBoolean;
  jNull: TJSONNull;
  jArray: TJSONArray;
  jObject: TJSONObject;
begin
  if TextType = ttNormal then
  begin
    if Assigned(Node) then
    begin
      treeNode:= vstJSON.GetNodeData(Node);
      if Assigned(treeNode) then
      begin
        case Column of
          -1:begin
            CellText:= rsVSTNoColumns;
          end;
          0:begin
            if Assigned(treeNode^.NodeData) then
            begin
              case treeNode^.NodeType of
                jtUnknown:begin
                  CellText:= rsVSTUnknown;
                end;
                jtNumber:begin
                  jNumber:= treeNode^.NodeData as TJSONNumber;
                  if Length(treeNode^.NodeName) > 0 then
                  begin
                    case jNumber.NumberType of
                      ntInteger:begin
                        CellText:= Format('%s: %d', [treeNode^.NodeName, jNumber.AsInteger]);
                      end;
                      ntInt64:begin
                        CellText:= Format('%s: %d', [treeNode^.NodeName, jNumber.AsInt64]);
                      end;
                      ntFloat:begin
                        CellText:= Format('%s: %s', [treeNode^.NodeName, FloatToStr(jNumber.AsFloat)]);
                      end;
                      ntQWord:begin
                        CellText:= Format('%s: %d', [treeNode^.NodeName, jNumber.AsQWord]);
                      end;
                    end;
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      case jNumber.NumberType of
                        ntInteger:begin
                          CellText:= Format('%d: %d', [treeNode^.NodeIndex, jNumber.AsInteger]);
                        end;
                        ntInt64:begin
                          CellText:= Format('%d: %d', [treeNode^.NodeIndex, jNumber.AsInt64]);
                        end;
                        ntFloat:begin
                          CellText:= Format('%d: %s', [treeNode^.NodeIndex, FloatToStr(jNumber.AsFloat)]);
                        end;
                        ntQWord:begin
                          CellText:= Format('%d: %d', [treeNode^.NodeIndex, jNumber.AsQWord]);
                        end;
                      end;
                    end
                    else
                    begin
                      case jNumber.NumberType of
                        ntInteger:begin
                          CellText:= Format('%d', [jNumber.AsInteger]);
                        end;
                        ntInt64:begin
                          CellText:= Format('%d', [jNumber.AsInt64]);
                        end;
                        ntFloat:begin
                          CellText:= FloatToStr(jNumber.AsFloat);
                        end;
                        ntQWord:begin
                          CellText:= Format('%d', [jNumber.AsQWord]);
                        end;
                      end;
                    end;
                  end;
                end;
                jtString:begin
                  jString:= treeNode^.NodeData as TJSONString;
                  if Length(treeNode^.NodeName) > 0 then
                  begin
                    CellText:= Format('%s: %s', [treeNode^.NodeName, jString.AsString]);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: "%s"', [treeNode^.NodeIndex, jString.AsString]);
                    end
                    else
                    begin
                      CellText:= Format('"%s"', [jString.AsString]);
                    end;
                  end;
                end;
                jtBoolean:begin
                  jBoolean:= treeNode^.NodeData as TJSONBoolean;
                  if Length(treeNode^.NodeName) > 0 then
                  begin
                    CellText:= Format('%s: %s', [treeNode^.NodeName, jBoolean.AsString]);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: %s', [treeNode^.NodeIndex, jBoolean.AsString]);
                    end
                    else
                    begin
                      CellText:= Format('%s', [jBoolean.AsString]);
                    end;
                  end;
                end;
                jtNull:begin
                  jNull:= treeNode^.NodeData as TJSONNull;
                  if Length(treeNode^.NodeName) > 0 then
                  begin
                    CellText:= Format('%s: %s', [treeNode^.NodeName, 'null']);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: %s', [treeNode^.NodeIndex, 'null']);
                    end
                    else
                    begin
                      CellText:= Format('%s', [jNull.AsString]);
                    end;
                  end;
                end;
                jtArray:begin
                  jArray:= treeNode^.NodeData as TJSONArray;
                  if Length(treeNode^.NodeName) > 0 then
                  begin
                    CellText:= Format('%s: array(%d)', [treeNode^.NodeName, jArray.Count]);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: array(%d)', [treeNode^.NodeIndex, jArray.Count]);
                    end
                    else
                    begin
                      CellText:= Format('array(%d)', [jArray.Count]);
                    end;
                  end;
                end;
                jtObject:begin
                  jObject:= treeNode^.NodeData as TJSONObject;
                  if Length(treeNode^.NodeName) > 0 then
                  begin
                    CellText:= Format('%s: object(%d)', [treeNode^.NodeName, jObject.Count]);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: object(%d)', [treeNode^.NodeIndex, jObject.Count]);
                    end
                    else
                    begin
                      CellText:= Format('object(%d)', [jObject.Count]);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.SetupShortcuts;
begin
  actFileExit.SecondaryShortCuts.Add('Esc');
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

function TfrmMain.FormatBytes(ABytes: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if ABytes < 1024 then
  begin
    Result := IntToStr(ABytes) + rsBytes;
    exit;
  end;
  if ABytes < (1024*1024) then
  begin
    dSize := ABytes / 1024;
    Result := FormatFloat('0.##', dSize) + rsKiloBytes;
    exit;
  end;
  if ABytes < (1024*1024*1024) then
  begin
    dSize := ABytes / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + rsMegaBytes;
    exit;
  end;
  if ABytes < (1024*1024*1024*1024) then
  begin
    dSize := ABytes / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + rsGigaBytes;
    exit;
  end;
  if ABytes < (1024*1024*1024*1024*1024) then
  begin
    dSize := ABytes / 1024 / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + rsTeraBytes;
  end;
end;

procedure TfrmMain.AddFile(const AFilename: String);
var
  len: Integer;
  isDuplicate: Boolean;
  filename: String;
begin
  if Pos('*', AFilename) > 0 then
  begin
    // Get all files
  end
  else
  begin
    if FileExists(AFilename) then
    begin
      isDuplicate:= False;
      for filename in FFileList do
      begin
        if filename = AFilename then
        begin
          isDuplicate:= True;
          break;
        end;
      end;
      if not isDuplicate then
      begin
        len:= Length(FFileList);
        SetLength(FFileList, len + 1);
        FFileList[len]:= ExpandFileName(AFilename);
      end;
    end;
  end;
end;

procedure TfrmMain.ClearTree;
begin
  if vstJSON.RootNodeCount > 0 then
  begin
    vstJSON.BeginUpdate;
    vstJSON.Clear;
    vstJSON.EndUpdate;
  end;
end;

procedure TfrmMain.OpenPropStorage;
begin
  if not DirectoryExists(GetAppConfigDir(False)) then
  begin
    CreateDir(GetAppConfigDir(False));
  end;
  JSONPropStorage.JSONFileName:= GetAppConfigFile(False);
  JSONPropStorage.RootObjectPath:= 'Application';
  JSONPropStorage.Active:= True;
end;

procedure TfrmMain.ClosePropStorage;
begin
  JSONPropStorage.Active:= False;
end;

procedure TfrmMain.CorrectPairSplitterCursor;
begin
  psMain.Cursor:= crHSplit;
  pssTree.Cursor:= crDefault;
  pssNode.Cursor:= crDefault;
end;

procedure TfrmMain.ClearLabels;
begin
  lblName.Caption:= rsLabelNameEmpty;
  lblType.Caption:= rsLabelTypeEmpty;
  lblCount.Caption:= rsLabelCountEmpty;
  lblPath.Caption:= '';
end;

procedure TfrmMain.ProcessParams;
var
  index: Integer;
  params: Integer;
  param: String;
begin
  params:= ParamCount;
  for index:= 1 to params do
  begin
    param:=ParamStr(index);
    AddFile(param);
  end;
end;

procedure TfrmMain.UpdateFileList;
var
  filename: String;
begin
  if lbFiles.Count > 0 then
  begin
    lbFiles.Clear;
  end;
  for filename in FFileList do
  begin
    lbFiles.Items.Add(ExtractFileName(filename));
  end;
end;

procedure TfrmMain.LoadFile(const AFilename: String);
var
  JSONFileStream: TFileStream;
  JSONStrings: TStringList;
  index: Integer;
begin
  JSONFileStream:= TFileStream.Create(AFilename, fmOpenRead);
  try
    if Assigned(FJSON) then
    begin
      FJSON.Free;
    end;
    try
      FJSON:= GetJSONData(JSONFileStream);
      SynEdit.BeginUpdate(False);
      SynEdit.Clear;
      JSONStrings:= TStringList.Create;
      try
        FJSON.CompressedJSON:= False;
        JSONStrings.Text:= FJSON.FormatJSON;
        for index:= 0 to Pred(JSONStrings.Count) do
        begin
          SynEdit.Append(JSONStrings[index]);
        end;
      finally
        JSONStrings.Free;
      end;
      SynEdit.EndUpdate;
    except
      on E: Exception do
      begin
        ShowMessage(Format('Looks like "%s" isn''t a JSON file', [AFilename]));
        WriteLN('Error:');
        WriteLN(E.Message);
      end;
    end;
  finally
    JSONFileStream.Free;
  end;
end;

procedure TfrmMain.UpdateTree;
begin
  vstJSON.BeginUpdate;
  vstJSON.Clear;
  if Assigned(FJSON) then
  begin
    UpdateTreeFromNode(vstJSON.RootNode, FJSON, 'ROOT');
  end;
  vstJSON.EndUpdate;
end;

procedure TfrmMain.UpdateTreeFromNode(const ANode: PVirtualNode;
  const AJSONData: TJSONData; const APath: String);
var
  index: Integer;
  node: PVirtualNode;
  treeNode: PTreeNode;
begin
  for index:= 0 to Pred(AJSONData.Count) do
  begin
    node:= vstJSON.AddChild(ANode);
    if Assigned(node) then
    begin
      treeNode:= vstJSON.GetNodeData(node);
      if Assigned(treeNode) then
      begin
        treeNode^.NodeType:= AJSONData.Items[index].JSONType;
        treeNode^.NodeData:= AJSONData.Items[index];
        treeNode^.NodePath:= APath;

        if AJSONData.JSONType = jtObject then
        begin
          treeNode^.NodeName:= TJSONObject(AJSONData).Names[index];
          treeNode^.NodePath:= treeNode^.NodePath + '.' + treeNode^.NodeName;
        end
        else
        begin
          treeNode^.NodeName:= '';
        end;

        if AJSONData.JSONType = jtArray then
        begin
          treeNode^.NodeIndex:= index;
          treeNode^.NodePath:= treeNode^.NodePath + Format('[%d]', [index]);
        end
        else
        begin
          treeNode^.NodeIndex:= -1;
        end;

        if AJSONData.Items[index].JSONType = jtObject then
        begin
          UpdateTreeFromNode(node, AJSONData.Items[index], treeNode^.NodePath);
        end;

        if AJSONData.Items[index].JSONType = jtArray then
        begin
          UpdateTreeFromNode(node, AJSONData.Items[index], treeNode^.NodePath);
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.ShowValue(const AJSONData: TJSONData);
var
  posY: Integer;
  lbl, lblFormated, lblBin, lblHex, lblBytes, lblDateTime, lblScientific: TLabel;
  edt, edtFormated, edtBin, edtHex, edtBytes, edtDateTime, edtScientific: TEdit;
  mem: TMemo;
begin
  repeat
    panValue.Components[0].Free;
  until panValue.ComponentCount = 0;
  case AJSONData.JSONType of
    jtUnknown:begin
      lbl:= TLabel.Create(panValue);
      lbl.Parent:= panValue;
      lbl.Top:= 8;
      lbl.Left:= 8;
      lbl.Caption:= 'Unknown';
    end;
    jtNumber:begin
      posY:= 8;

      // Value
      edt:= TEdit.Create(panValue);
      edt.Parent:= panValue;
      edt.Top:= posY;
      edt.Left:= 8;
      edt.Width:= panValue.ClientWidth - 16;
      edt.Anchors:= [akTop, akLeft, akRight];
      edt.ReadOnly:= True;
      Inc(posY, 50);

      // Formated
      lblFormated:= TLabel.Create(panValue);
      lblFormated.Parent:= panValue;
      lblFormated.Top:= posY;
      lblFormated.Left:=8;
      if TJSONNumber(AJSONData).NumberType = ntFloat then
      begin
        lblFormated.Caption:= Format('%s (%s)',[rsLabelFormated, cNumberFormatFloat]);;
      end
      else
      begin
        lblFormated.Caption:= Format('%s (%s)',[rsLabelFormated, cNumberFormatInteger]);;
      end;
      Inc(posY, 17);

      edtFormated:= TEdit.Create(panValue);
      edtFormated.Parent:= panValue;
      edtFormated.Top:= posY;
      edtFormated.Left:= 8;
      edtFormated.Width:= panValue.ClientWidth - 16;
      edtFormated.Anchors:= [akTop, akLeft, akRight];
      edtFormated.ReadOnly:= True;
      Inc(posY, 34);

      // Scientific
      lblScientific:= TLabel.Create(panValue);
      lblScientific.Parent:= panValue;
      lblScientific.Top:= posY;
      lblScientific.Left:=8;
      lblScientific.Caption:= rsLabelScientific;
      lblScientific.Caption:= Format('%s (%s)',[rsLabelScientific, cNumberFormatFloatScientific]);;
      Inc(posY, 17);

      edtScientific:= TEdit.Create(panValue);
      edtScientific.Parent:= panValue;
      edtScientific.Top:= posY;
      edtScientific.Left:= 8;
      edtScientific.Width:= panValue.ClientWidth - 16;
      edtScientific.Anchors:= [akTop, akLeft, akRight];
      edtScientific.ReadOnly:= True;
      Inc(posY, 34);

      if TJSONNumber(AJSONData).NumberType in [ntInteger, ntInt64, ntQWord] then
      begin
        // Binary
        lblBin:= TLabel.Create(panValue);
        lblBin.Parent:= panValue;
        lblBin.Top:= posY;
        lblBin.Left:=8;
        lblBin.Caption:= rsLabelBinary;
        Inc(posY, 17);

        edtBin:= TEdit.Create(panValue);
        edtBin.Parent:= panValue;
        edtBin.Top:= posY;
        edtBin.Left:= 8;
        edtBin.Width:= panValue.ClientWidth - 16;
        edtBin.Anchors:= [akTop, akLeft, akRight];
        edtBin.ReadOnly:= True;
        Inc(posY, 34);

        // Hexadecimal
        lblHex:= TLabel.Create(panValue);
        lblHex.Parent:= panValue;
        lblHex.Top:= posY;
        lblHex.Left:=8;
        lblHex.Caption:= rsLabelHexadecimal;
        Inc(posY, 17);

        edtHex:= TEdit.Create(panValue);
        edtHex.Parent:= panValue;
        edtHex.Top:= posY;
        edtHex.Left:= 8;
        edtHex.Width:= panValue.ClientWidth - 16;
        edtHex.Anchors:= [akTop, akLeft, akRight];
        edtHex.ReadOnly:= True;
        Inc(posY, 34);

        // Bytes
        lblBytes:= TLabel.Create(panValue);
        lblBytes.Parent:= panValue;
        lblBytes.Top:= posY;
        lblBytes.Left:=8;
        lblBytes.Caption:= rsLabelBytes;
        Inc(posY, 17);

        edtBytes:= TEdit.Create(panValue);
        edtBytes.Parent:= panValue;
        edtBytes.Top:= posY;
        edtBytes.Left:= 8;
        edtBytes.Width:= panValue.ClientWidth - 16;
        edtBytes.Anchors:= [akTop, akLeft, akRight];
        edtBytes.ReadOnly:= True;
        Inc(posY, 34);

        // DateTime
        lblDateTime:= TLabel.Create(panValue);
        lblDateTime.Parent:= panValue;
        lblDateTime.Top:= posY;
        lblDateTime.Left:=8;
        lblDateTime.Caption:= rsLabelDateTime;
        Inc(posY, 17);

        edtDateTime:= TEdit.Create(panValue);
        edtDateTime.Parent:= panValue;
        edtDateTime.Top:= posY;
        edtDateTime.Left:= 8;
        edtDateTime.Width:= panValue.ClientWidth - 16;
        edtDateTime.Anchors:= [akTop, akLeft, akRight];
        edtDateTime.ReadOnly:= True;
        Inc(posY, 34);
      end;

      case TJSONNumber(AJSONData).NumberType of
        ntInteger:begin
          edt.Text:= Format('%d', [AJSONData.AsInteger]);
          edtFormated.Text:= FormatFloat(cNumberFormatInteger, AJSONData.AsFloat);
          edtScientific.Text:= FormatFloat(cNumberFormatFloatScientific, AJSONData.AsFloat);
          edtBin.Text:= IntToBin(AJSONData.AsInt64, 32);
          edtHex.Text:= IntToHex(AJSONData.AsInteger, 16);
          edtBytes.Text:= FormatBytes(AJSONData.AsInteger);
          edtDateTime.Text:= FormatDateTime(cDateTimeFormat, UnixToDateTime(AJSONData.AsInteger));
        end;
        ntInt64:begin
          edt.Text:= Format('%d', [AJSONData.AsInt64]);
          edtFormated.Text:= FormatFloat(cNumberFormatInteger, AJSONData.AsFloat);
          edtScientific.Text:= FormatFloat(cNumberFormatFloatScientific, AJSONData.AsFloat);
          edtBin.Text:= IntToBin(AJSONData.AsInt64, 64);
          edtHex.Text:= IntToHex(AJSONData.AsInt64, 16);
          edtBytes.Text:= FormatBytes(AJSONData.AsInt64);
          edtDateTime.Text:= FormatDateTime(cDateTimeFormat, UnixToDateTime(AJSONData.AsInt64));
        end;
        ntFloat:begin
          edt.Text:= FloatToStr(AJSONData.AsFloat);
          edtFormated.Text:= FormatFloat(cNumberFormatFloat, AJSONData.AsFloat);
          edtScientific.Text:= FormatFloat(cNumberFormatFloatScientific, AJSONData.AsFloat);
        end;
        ntQWord:begin
          edt.Text:= Format('%d', [AJSONData.AsQWord]);
          edtFormated.Text:= FormatFloat(cNumberFormatInteger, AJSONData.AsFloat);
          edtScientific.Text:= FormatFloat(cNumberFormatFloatScientific, AJSONData.AsFloat);
          edtBin.Text:= IntToBin(AJSONData.AsInt64, 64);
          edtHex.Text:= IntToHex(AJSONData.AsQWord, 16);
          edtBytes.Text:= FormatBytes(AJSONData.AsQWord);
          edtDateTime.Text:= FormatDateTime(cDateTimeFormat, UnixToDateTime(AJSONData.AsQWord));
        end;
      end;
    end;
    jtString:begin
      mem:= TMemo.Create(panValue);
      mem.Parent:= panValue;
      mem.Top:= 8;
      mem.Left:= 8;
      mem.Width:= panValue.ClientWidth - 16;
      mem.Height:= panValue.ClientHeight - 16;
      mem.Anchors:= [akTop, akLeft, akBottom, akRight];
      mem.ReadOnly:= True;
      mem.ScrollBars:= ssAutoVertical;
      mem.Lines.Text:= AJSONData.AsString;
    end;
    jtBoolean:begin
      lbl:= TLabel.Create(panValue);
      lbl.Parent:= panValue;
      lbl.Top:= 8;
      lbl.Left:= 8;
      lbl.Caption:= Format('%s', [AJSONData.AsString]);
    end;
    jtNull:begin
      lbl:= TLabel.Create(panValue);
      lbl.Parent:= panValue;
      lbl.Top:= 8;
      lbl.Left:= 8;
      lbl.Caption:= 'null';
    end;
    jtArray:begin

    end;
    jtObject:begin

    end;
  end;
end;

end.

