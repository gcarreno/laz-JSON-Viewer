{ Implements Forms.Main

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
, fpjson
, VirtualTrees
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    lblCount: TLabel;
    lblName: TLabel;
    lblType: TLabel;
    lbFiles: TListBox;
    psMain: TPairSplitter;
    pssTree: TPairSplitterSide;
    pssNode: TPairSplitterSide;
    Splitter1: TSplitter;
    vstJSON: TVirtualStringTree;
    panValue: TPanel;
    panItem: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFilesSelectionChange(Sender: TObject; User: boolean);
    procedure vstJSONChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstJSONGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstJSONGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FJSON: TJSONData;
    FFileList: Array of String;

    function FormatBytes(ABytes: Int64): String;

    procedure ClearLabels;
    procedure CorrectPSCursor;
    procedure ProcessParams;
    procedure UpdateFileList;
    procedure LoadFile(const AFilename: String);
    procedure UpdateTree;
    procedure UpdateTreeFromNode(const ANode: PVirtualNode;
      const AJSONData: TJSONData);
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

  rsLabelBinary = 'Binary';
  rsLabelHexadecimal = 'Hexadecimal';
  rsLabelDateTime = 'Date';
  rsLabelBytes = 'Bytes';

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

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= Format(rsFormCaption, [cVersion]);
  ClearLabels;
  CorrectPSCursor;
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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FJSON) then
  begin
    FJSON.Free;
  end;
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
      panValue.Components[index].Free;
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
                        CellText:= Format('%s: %n', [treeNode^.NodeName, jNumber.AsFloat]);
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
                          CellText:= Format('%d: %n', [treeNode^.NodeIndex, jNumber.AsFloat]);
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
                          CellText:= Format('%n', [jNumber.AsFloat]);
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

procedure TfrmMain.CorrectPSCursor;
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
end;

procedure TfrmMain.ProcessParams;
var
  index: Integer;
  len: Integer;
  params: Integer;
  param: String;
begin
  params:= ParamCount;
  for index:= 1 to params do
  begin
    param:=ParamStr(index);
    if Pos('*', param) > 0 then
    begin
      // Get all files
    end
    else
    begin
      if FileExists(param) then
      begin
        len:= Length(FFileList);
        SetLength(FFileList, len + 1);
        FFileList[len]:= param;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateFileList;
var
  filename: String;
begin
  for filename in FFileList do
  begin
    lbFiles.Items.Add(ExtractFileName(filename));
  end;
end;

procedure TfrmMain.LoadFile(const AFilename: String);
var
  JSONFileStream: TFileStream;
begin
  JSONFileStream:= TFileStream.Create(AFilename, fmOpenRead);
  try
    if Assigned(FJSON) then
    begin
      FJSON.Free;
    end;
    try
      FJSON:= GetJSONData(JSONFileStream);
    except
      on E: Exception do
      begin
        ShowMessage(Format('Looks like "%s" isn''t a JSON file', [AFilename]));
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
    UpdateTreeFromNode(vstJSON.RootNode, FJSON);
  end;
  vstJSON.EndUpdate;
end;

procedure TfrmMain.UpdateTreeFromNode(const ANode: PVirtualNode;
  const AJSONData: TJSONData);
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
        if AJSONData.JSONType = jtObject then
        begin
          treeNode^.NodeName:= TJSONObject(AJSONData).Names[index];
        end
        else
        begin
          treeNode^.NodeName:= '';
        end;

        if AJSONData.JSONType = jtArray then
        begin
          treeNode^.NodeIndex:= index;
        end
        else
        begin
          treeNode^.NodeIndex:= -1;
        end;

        if AJSONData.Items[index].JSONType = jtObject then
        begin
          UpdateTreeFromNode(node, AJSONData.Items[index]);
        end;

        if AJSONData.Items[index].JSONType = jtArray then
        begin
          UpdateTreeFromNode(node, AJSONData.Items[index]);
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.ShowValue(const AJSONData: TJSONData);
var
  posY: Integer;
  lbl, lblBin, lblHex, lblBytes, lblDateTime: TLabel;
  edt, edtBin, edtHex, edtBytes, edtDateTime, edtFloat: TEdit;
  //tmpInt64: Int64;
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

      //lbl:= TLabel.Create(panValue);
      //lbl.Parent:= panValue;
      //lbl.Top:= posY;
      //lbl.Left:=8;
      //lbl.Caption:= rsLabelValue;
      //Inc(posY, 17);

      edt:= TEdit.Create(panValue);
      edt.Parent:= panValue;
      edt.Top:= posY;
      edt.Left:= 8;
      edt.Width:= panValue.ClientWidth - 16;
      edt.Anchors:= [akTop, akLeft, akRight];
      edt.ReadOnly:= True;
      Inc(posY, 50);

      if TJSONNumber(AJSONData).NumberType in [ntInteger, ntQWord] then
      begin
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
      end;

      if TJSONNumber(AJSONData).NumberType in [ntInteger, ntInt64, ntQWord] then
      begin
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

      if TJSONNumber(AJSONData).NumberType = ntFloat then
      begin
        edtFloat:= TEdit.Create(panValue);
        edtFloat.Parent:= panValue;
        edtFloat.Top:= posY;
        edtFloat.Left:= 8;
        edtFloat.Width:= panValue.ClientWidth - 16;
        edtFloat.Anchors:= [akTop, akLeft, akRight];
        edtFloat.ReadOnly:= True;
        Inc(posY, 34);
      end;

      case TJSONNumber(AJSONData).NumberType of
        ntInteger:begin
          edt.Text:= Format('%d', [AJSONData.AsInteger]);
          edtBin.Text:= IntToBin(AJSONData.AsInteger, 32, 8);
          edtHex.Text:= IntToHex(AJSONData.AsInteger, 16);
          edtBytes.Text:= FormatBytes(AJSONData.AsInteger);
          edtDateTime.Text:= FormatDateTime(cDateTimeFormat, UnixToDateTime(AJSONData.AsInteger));
        end;
        ntInt64:begin
          //tmpInt64:= AJSONData.AsInt64;
          edt.Text:= Format('%d', [AJSONData.AsInt64]);

          { #todo -ogcarreno : Need to fix IntToBin only outputting 32 bits }
          //edtBin.Text:= IntToBin(AJSONData.AsInteger, 32, 8);

          edtHex.Text:= IntToHex(AJSONData.AsInt64, 16);
          edtBytes.Text:= FormatBytes(AJSONData.AsInt64);
          edtDateTime.Text:= FormatDateTime(cDateTimeFormat, UnixToDateTime(AJSONData.AsInt64));
        end;
        ntFloat:begin
          edt.Text:= Format('%n', [AJSONData.AsFloat]);
          edtFloat.Text:= AJSONData.AsString;
        end;
        ntQWord:begin
          edt.Text:= Format('%d', [AJSONData.AsQWord]);
          edtBin.Text:= IntToBin(AJSONData.AsQWord, 32, 8);
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

