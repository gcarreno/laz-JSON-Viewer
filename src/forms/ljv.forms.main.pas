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
, ExtCtrls, StdCtrls, PairSplitter
, fpjson
, laz.VirtualTrees
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    lblName: TLabel;
    lblType: TLabel;
    psMain: TPairSplitter;
    pssTree: TPairSplitterSide;
    pssNode: TPairSplitterSide;
    vstJSON: TLazVirtualStringTree;
    panProperties: TPanel;
    panItem: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstJSONChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstJSONGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstJSONGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FJSON: TJSONData;

    procedure LoadFile;
    procedure UpdateTree;
    procedure UpdateTreeFromNode(const ANode: PVirtualNode;
      const AJSONData: TJSONData);
  public

  end;

var
  frmMain: TfrmMain;

resourcestring
  rsVSTUnknown = 'Unknown';
  rsVSTNoColumns = 'There are no Columns';

implementation

{$R *.lfm}

uses
  LJV.JSON.Utils
, LJV.Tree.Nodes
;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadFile;
  UpdateTree;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FJSON) then
  begin
    FJSON.Free;
  end;
end;

procedure TfrmMain.vstJSONChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  treeNode: PTreeNode;
  sName: String;
  iIndex: Int64;
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
        lblName.Caption:= Format('Name: %s', [sName])
      end;
      if iIndex > -1 then
      begin
        lblName.Caption:= Format('Index: %d', [iIndex])
      end;
      case treeNode^.NodeType of
        jtUnknown:begin
          lblType.Caption:= 'Type: Unknown';
        end;
        jtNumber:begin
          lblType.Caption:= 'Type: Number';
        end;
        jtString:begin
          lblType.Caption:= 'Type: String';
        end;
        jtBoolean:begin
          lblType.Caption:= 'Type: Boolean';
        end;
        jtNull:begin
          lblType.Caption:= 'Type: Null';
        end;
        jtArray:begin
          lblType.Caption:= 'Type: Array';
        end;
        jtObject:begin
          lblType.Caption:= 'Type: Object';
        end;
      end;
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
                    CellText:= Format('%s: %d', [treeNode^.NodeName, jNumber.AsInt64]);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: %d', [treeNode^.NodeIndex, jNumber.AsInt64]);
                    end
                    else
                    begin
                      CellText:= Format('%d', [jNumber.AsInt64]);
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
                    CellText:= Format('%s: %s', [treeNode^.NodeName, jNull.AsString]);
                  end
                  else
                  begin
                    if treeNode^.NodeIndex > -1 then
                    begin
                      CellText:= Format('%d: %s', [treeNode^.NodeIndex, jNull.AsString]);
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

procedure TfrmMain.LoadFile;
const
  cTestFile = '/home/gcarreno/Programming/Fastpool/data/fastpool.pascal.stats-address.longpoll-true.json';
var
  JSONFileStream: TFileStream;
begin
  JSONFileStream:= TFileStream.Create(cTestFile, fmOpenRead);
  try
    FJSON:= GetJSONData(JSONFileStream);
  finally
    JSONFileStream.Free;
  end;
end;

procedure TfrmMain.UpdateTree;
begin
  vstJSON.BeginUpdate;
  UpdateTreeFromNode(vstJSON.RootNode, FJSON);
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

end.

