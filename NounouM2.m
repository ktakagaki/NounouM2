(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouM2`", {"JLink`"}]


(* ::Section:: *)
(*Declarations*)


(* Exported symbols added here with SymbolName::usage *) 
General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";


$PackageDirectory = ParentDirectory[DirectoryName[FindFile["NounouM2`"]]];
$PackageNewestFileDate = DateString[Max @@ AbsoluteTime /@ FileDate /@ FileNames[ "*",$PackageDirectory,Infinity] ];


Print["Welcome to NounouM2, the Mathematica interface to nounou!"];
Print["(last updated:  "<> $PackageNewestFileDate <>")"];


(* ::Subsection::Closed:: *)
(*Common Option Declarations*)


NNStackAxes::usage=" ";
NNAxes::usage=" ";
NNBaselineCorrection::usage="";
NNUnit::usage="\"mS\" (default) or \"frames\"";
NNMasking::usage=" ";


(* ::Subsection::Closed:: *)
(*Global Utility Functions*)


NNPadZeros::usage =
"PadZeros[n] gives the numeral n string padded to 3 digits with zeros. " <>
"PadZeros[n,m] gives the numeral n string padded to m digits with zeros.";


NNNextPower::usage=" ";


NNFunctionQ::usage=
"returns whether a given symbol is a pure function, or a function rule.";


(* ::Subsection::Closed:: *)
(*Rule List and Option Handling*)


NNRuleListQ::usage=
	"RuleListQ[ruleList_List]... returns whether the argument ruleList is a list of rules.";
NNRuleQ::usage=
	"RuleQ[rule_]... returns whether the argument ruleList is a list of Rule or RuleDelayed objects.";


NNJoinOptionLists::usage=
"JoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y]]...   joins the two option lists, "<>
"and if an option specified in x is repeated in y, the specification in y is dropped.

JoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y], z_/;RuleQ[z]]...    joins the three option lists, "<>
"and if an option specified in x is repeated in y, the specification in y is dropped, etc.

JoinOptionLists[symbol_Symbol, x_/;RuleQ[x], y_/;RuleQ[y],... ]...    Does the "<>
"same as above, but filters the rules for Option[Symbol] before returning.";


NNAddOptions::usage=
"AddOptions[object, opts]...     returns the original object (e.g. NNMData[<<>>, opts]), "<>
"but with the specified option(s) appended or replaced. opts can be specified either as a Sequence "<>
"or a List of rules (i.e., brackets {opts} are optional).";


NNExtractRules::usage=
"Extracts options from an object, e.g. NNMData[<<>>,  opt->1] ==> {opt->1}";


(* ::Subsection::Closed:: *)
(*DataReader Java Object Handling*)


NNDataReaderJavaObjectQ::usage="";
NNXDataJavaObjectQ::usage="";


(* ::Subsection::Closed:: *)
(*ReloadPackage*)


NNReloadPackage::usage=
"ReloadPackage[string_String].... unloads and reloads any package which \
contains the specified string within its name. \
This function is convenient during package developemnt--the package under development can \
be reloaded to test functionality, without losing any precalculated results in the kernel.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNPadZeros*)


NNPadZeros[n_]:=NNPadZeros[n,3];
NNPadZeros[n_,m_]:=Apply[StringJoin,Map[ToString,IntegerDigits[n, 10, m] ]];


NNPadZeros[args___]:=Message[NNPadZeros::invalidArgs,{args}];


(* ::Subsection:: *)
(*NextPower/NextMultiple*)


NNNextPower[base_, n_]:= Ceiling[Log[base, n]];


NNNextPower[args___]:=Message[NNNextPower::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*NNFunctionQ*)


(*Tests whether the symbol is a function or not*)
(*FunctionQ[x_String]:=FunctionQ[ToExpression[x]];*)
NNFunctionQ[x_Function]:=True;
NNFunctionQ[x_Symbol]:=MemberQ[Attributes[x],NumericFunction] || (Length[Flatten[#[x]&/@{DownValues,UpValues}]]>0);
NNFunctionQ[x_, sampleArgs_]:=NNFunctionQ[x, sampleArgs, NumericQ];
NNFunctionQ[x_, sampleArgs_, questionFunc_]:=Quiet[Check[questionFunc[  x @@ sampleArgs],False]];
NNFunctionQ[_]:=False;


NNFunctionQ[args___]:=Message[NNFunctionQ::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*Rule List/Option Handling*)


(* ::Subsubsection::Closed:: *)
(*NNRuleListQ/NNRuleQ*)


NNRuleListQ[ruleList_List] := And @@ (NNRuleListQ /@ ruleList);
NNRuleListQ[rule_Rule] := True;
NNRuleListQ[rule_RuleDelayed] := True;
NNRuleListQ[rules__] := NNRuleListQ[{rules}];
NNRuleListQ[] := True;
NNRuleListQ[{}] := True;
NNRuleListQ[_] := False;
NNRuleListQ[args___]:=Message[NNRuleListQ::invalidArgs,{args}];


NNRuleQ[rule_Rule] := True;
NNRuleQ[rule_RuleDelayed] := True;
NNRuleQ[_] := False;
NNRuleQ[args___]:=Message[NNRuleQ::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(*NNJoinOptionLists*)


NNJoinOptionLists[x_/;NNRuleListQ[x], y_/;NNRuleListQ[y]]:=
	Module[{tempret},
		tempret=Join[Flatten[x], FilterRules[Flatten[y], Except[x]]];
		Return[tempret]
	];


NNJoinOptionLists[x_/;NNRuleListQ[x], y__/;(And@@(NNRuleListQ /@ {y}))]:=
	Module[{tempret},
		tempret=x;
		Do[tempret=NNJoinOptionLists[tempret, zz],{zz,{y}}];
		Return[tempret]
	];


NNJoinOptionLists[symbol_Symbol, x_/;NNRuleListQ[x]]:=
	Module[{},
		Return[FilterRules[x, Options[symbol]]]
	];


NNJoinOptionLists[symbol_Symbol, x_/;NNRuleListQ[x], y__/;(And@@(NNRuleListQ /@ {y}))]:=
	Module[{tempret},
		tempret = NNJoinOptionLists[x, y];
		Return[FilterRules[tempret, Options[symbol]]]
	];


NNJoinOptionLists[symbol_[contents_], x_/;NNRuleListQ[x], y__/;(And@@(NNRuleListQ /@ {y}))]:=
	Module[{tempret},
		tempret = NNJoinOptionLists[x, y];
		Return[FilterRules[tempret, Options[symbol]]]
	];


NNJoinOptionLists[args___]:=Message[NNJoinOptionLists::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(*AddOptions*)


NNAddOptions[symbol_[contents___], opts___]:=NNAddOptions[symbol[contents], {opts}];
NNAddOptions[symbol_[contents___], {opts___}]:=
	Module[{tempRet(*,oldRules*)},
		(*The following will strip off rules from the end.*)
		tempRet = Select[{contents}, !NNRuleQ[#]&];
		(*Append old rules which are not given in opts.*)

		tempRet = Append[tempRet, Hold[Sequence@@NNJoinOptionLists[symbol, {opts}, Select[{contents}, RuleQ]]] ];
		Return[ReleaseHold[ symbol[Sequence@@tempRet] ]]
		(*oldRules=FilterRules[Options[x], Except[opts]];
			If[Length[oldRules]>0, tempRet= Append[tempRet, Hold[Sequence@@oldRules]]];
		(*Append new rules given in opts*)
		tempRet= Append[tempRet, Hold[opts]];
		Return[ReleaseHold[tempRet]]*)
	];


NNAddOptions[args___]:=Message[NNAddOptions::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(*ExtractRules*)


NNExtractRules[x_[arg___]]:=Flatten[If[NNRuleQ[#],#,{}]& /@ {arg}];


NNExtractRules[args___]:=Message[NNExtractRules::invalidArgs,{args}];


(* ::Subsection::Closed:: *)
(*DataReader Java Object Handling*)


NNDataReaderJavaObjectQ[
	dataReaderJavaObj_/;(JavaObjectQ[dataReaderJavaObj] 
					&& InstanceOf[dataReaderJavaObj, "nounou.DataReader"])
						]:= True ;


NNDataReaderJavaObjectQ[args___]:= False ;


NNXDataJavaObjectQ[
	xDataJavaObj_/;(JavaObjectQ[xDataJavaObj] 
					&& InstanceOf[xDataJavaObj, "nounou.data.XData"])
						]:= True ;


NNXDataJavaObjectQ[args___]:= False ;


(* ::Subsection::Closed:: *)
(*ReloadPackage*)


NNReloadPackage[string_String]:=
	Module[{tempPackages},
		tempPackages=Select[ $Packages, StringMatchQ[#, (___ ~~ string ~~ ___)]& ];
		If[Length[tempPackages]>0,
			Unprotect[$Packages];
			($ContextPath=DeleteCases[$ContextPath, #])& /@ tempPackages;
			($Packages=DeleteCases[$Packages, # ])& /@ tempPackages;
			(Remove[Evaluate[#<>"*"]])& /@ tempPackages;
			Needs /@ tempPackages;
			Print["Reloaded: " <> # ]& /@ tempPackages;
		];
		(*Protect[$ContextPath];--$ContextPath is not Protected*)
		Protect[$Packages];
	];


NNReloadPackage[strings_List/;(And @@ StringQ /@ strings)]:= 
	Module[{},
		NNReloadPackage /@ strings;
	];


NNReloadPackage[args___]:=Message[NNReloadPackage::invalidArgs,{args}];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
