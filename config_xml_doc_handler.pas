unit config_xml_doc_handler;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,xml_doc_handler;
type
  TConfigXMLHandler = class(TXMLDocumentHandler)
  private
  public

  end;

implementation

//For reference
//We want
//function TSudokuGame.generateGameDocument: TXMLDocument;
//var
//  doc:TXMLDocument;
//  cellsNode,regionsNode,constraintsNode:TDOMNode;
//begin
//  if length(fCells) = 0 then exit; //should throw error
//  doc:=TXMLDocument.Create;
//  addNode(doc,'','sudoku');
//  addNode(doc,'sudoku','name',fName);
//  addNode(doc,'sudoku','version',fVersion);
//  addNode(doc,'sudoku','base-game');
//  addNode(doc,'base-game','rows',dimensions.Y.ToString);
//  addNode(doc,'base-game','columns',dimensions.X.ToString);
//    begin
//    cellsNode:=getNode(doc,'cells',false,nil,true);
//    doc:= addCellsToDocument(doc,cellsNode, cells);
//    end;
//  if length(regions) > 0 then
//    begin
//    regionsNode:=getNode(doc,'regions',false,nil,true);
//    doc:= addRegionsToDocument(doc,regionsNode,regions);
//    end;
//  if length(constraints) > 0 then
//    begin
//    constraintsNode:=getNode(doc,'constraints',false,nil,true);
//    doc:= addConstraintsToDocument(doc, constraintsNode,constraints);
//    end;
//  fDocument:=doc;
//  result:=doc;
//end;
end.

