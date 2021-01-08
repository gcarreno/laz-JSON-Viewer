unit LJV.Tree.Nodes;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
;

type
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    NodeType: TJSONtype;
    NodeIndex: Int64;
    NodeName: String;
    NodeData: TJSONData;
  end;

implementation

end.

