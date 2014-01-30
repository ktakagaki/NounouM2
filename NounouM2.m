(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouM2`", {"JLink`"}]


(* Exported symbols added here with SymbolName::usage *) 
General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";


$PackageDirectory = ParentDirectory[DirectoryName[FindFile["NounouM2`"]]];
$PackageNewestFileDate = DateString[Max @@ AbsoluteTime /@ FileDate /@ FileNames[ "*",$PackageDirectory,Infinity] ];


Print["Welcome to NounouM2, the Mathematica interface to nounou!"];
Print["(last updated:  "<> $PackageNewestFileDate <>")"];


Begin["`Private`"];

(* Implementation of the package *)

End[]


EndPackage[]
