(* ::Package:: *)

(* Mathematica Init File *)

Get[ "NounouM2`NounouM2`"]


Needs[ "JLink`"];
(*SetOptions[JLink`InstallJava, JVMArguments -> "-Xmx1024m"];
SetOptions[JLink`ReinstallJava, JVMArguments -> "-Xmx1024m"];
ReinstallJava[];*)


NounouM2`IncreaseJavaStack[stackSize_Integer]:=
	Module[{tempOptStringI,tempOptStringR,tempReI=False, tempReR=False, 
		tempPrint, tempns},
		
		tempPrint=PrintTemporary["Checking Java stack size..."];

    	(*Extract the stack settings for InstallJava*)
		tempOptStringI=OptionValue[JLink`InstallJava, JLink`JVMArguments];
		If[tempOptStringI===None, tempReI=True,
		If[Head[tempOptStringI]===String,
			tempOptStringI=StringCases[tempOptStringI,"-"~~Shortest[__]~~tempns:NumberString..~~"m"->tempns];
			If[Length[tempOptStringI]>=1,
				tempOptStringI=ToExpression[tempOptStringI[[1]]];
				If[tempOptStringI<stackSize,tempReI=True]
			];
		]];

    	(*Extract the stack settings for ReinstallJava*)
		tempOptStringR=OptionValue[JLink`ReinstallJava, JLink`JVMArguments];
		If[tempOptStringR===None, tempReR=True,
		If[Head[tempOptStringR]===String,
			tempOptStringR=StringCases[tempOptStringR,"-"~~Shortest[__]~~tempns:NumberString..~~"m"->tempns];
			If[Length[tempOptStringR]>=1,
				tempOptStringR=ToExpression[tempOptStringR[[1]]];
				If[tempOptStringR<stackSize,tempReR=True]
			];
		]];

		(*Change and ReinstallJava as necessary*)
		If[tempReI,
			SetOptions[JLink`InstallJava, JLink`JVMArguments -> "-Xmx"<>ToString[stackSize]<>"m"]
		];
		If[tempReR,
			SetOptions[JLink`ReinstallJava, JLink`JVMArguments -> "-Xmx"<>ToString[stackSize]<>"m"]
		];
		If[tempReI || tempReR,
			JLink`ReinstallJava[];
			Print["<<Set JLink` java stack size to "<>ToString[stackSize]<>"Mb>>"];
		];

		NotebookDelete[tempPrint];
	]; (*Module for KKMInstallJava*)


NounouM2`IncreaseJavaStack[3072];


(*Convenience object for static methods*)
NN=LoadJavaClass["nounou.NN"(*, StaticsVisible\[Rule]True*)];


SetComplexClass["breeze.math.Complex"];


Needs["NounouM2`Plotting`"];
Needs["NounouM2`DataPlotting`"];


$NNReader = NN`newReader[]


(*LoadJavaClass["de.ifn_magdeburg.kazukazuj.K"];
LoadJavaClass["de.ifn_magdeburg.kazukazuj.Kc"];
LoadJavaClass["de.ifn_magdeburg.kazukazuj.Ks"];
SetComplexClass["de.ifn_magdeburg.kazukazuj.Complex"];*)


(*Needs[ "KazukazuM`KazukazuM`"];
Needs[ "KazukazuM`Signal`"];
Needs[ "KazukazuM`Filters`"];
Needs[ "KazukazuM`CircularStatistics`"];
Needs[ "KazukazuM`Utilities`"];
Needs[ "KazukazuM`Plots`"];
Needs["KazukazuM`CircularPlots`"];
(*Needs["KazukazuM`ImageAnalysis`"];*)
DeclarePackage["KazukazuM`Heartbeat`", 
	{"LocalMaxPositions","LocalMinPositions",
	"HeartbeatTrigger", "TriggeredAverage",
	"TriggeredAverageTrace","HeartbeatSubtract"}];*)
