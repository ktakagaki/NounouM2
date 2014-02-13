(* ::Package:: *)

BeginPackage["NounouM2`Plotting`", { "NounouM2`"}]
(* Exported symbols added here with SymbolName::usage *)  


(* ::Section:: *)
(*Declarations*)


(* ::Subsubsection:: *)
(*NNListLinePlotMean*)


NNListLinePlotMean::usage= "ListLinePlotMean plots multiple traces together, along with mean and standard error.";


NNTracePlot::usage= "Whether to plot traces in NNTracePlot.";

NNMeanPlot::usage= "Whether to plot a mean trace in NNListLinePlotMean.";
NNMeanPlotStyle::usage= "PlotStyle for mean trace in NNListLinePlotMean.";

NNErrorPlot::usage= "Whether to plot error bound traces in NNListLinePlotMean.";
NNErrorType::usage= "What type of Error to calculate in NNErrorPlot. (\"StandardError\")";
NNErrorPlotStyle::usage= "PlotStyle for error trace in NNErrorPlot. ({} if no traces and only shading)";

NNErrorPlotShaded::usage= "Whether to shade error ranges in NNListLinePlotMean.";
NNErrorPlotFillingStyle::usage= "Shading style for error shading in NNErrorPlot. (\"StandardError\" or \"StandardDeviation\")";


Options[NNListLinePlotMean] =
NNJoinOptionLists[
	{
	NNTracePlot->True,
	NNMeanPlot->True, NNMeanPlotStyle->{Opacity[0.5, Black]}, 
	NNErrorPlot->True, NNErrorType->"StandardError", NNErrorPlotStyle->{Opacity[0.25,Blue]},
	NNErrorPlotShaded->True, NNErrorPlotFillingStyle->{Opacity[0.25,Blue]} 
	},
	{PlotStyle->Opacity[0.05,Black](*, AspectRatio->1/5, PlotRange->All*)}
];


(* ::Subsection:: *)
(*NNStackTraces / NNListLinePlotStack*)


NNStackLists::usage="Augments a series of traces so that when plotted, they will be stacked. " <>
"Default for NNStackLists when given as an option is to stack lists every 2*(mean RMS value) apart.";


NNListLinePlotStack::usage=
"ListLinePlotMean plots multiple traces together, along with mean and standard error.";


Options[NNListLinePlotStack] =
NNJoinOptionLists[
	{NNStackLists->Automatic},
	{ListLinePlotOptions->{PlotStyle->Black, AspectRatio->1/2, PlotRange->All}}
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"] (* Begin Private Context *) 


(* ::Subsection::Closed:: *)
(*NNListLinePlotMean*)


NNListLinePlotMean[traces_/;Length[Dimensions[traces]]==2, opts:OptionsPattern[]]:=
Module[{(*tempDataS, *)tempMean, tempError,
		opMeanPlot, opErrorPlot, opErrorPlotShaded, opErrorType,
		grMain, grMean, grError},

	opMeanPlot=OptionValue[NNMeanPlot];
	opErrorPlot=OptionValue[NNErrorPlot];
	opErrorPlotShaded=OptionValue[NNErrorPlotShaded];
	opErrorType=OptionValue[NNErrorType];

	(*==========Process options and create graphics==========*)
	If[(opMeanPlot || opErrorPlot || opErrorPlotShaded ) && Length[traces]>=2,
		tempMean = Mean[ traces ];

		(*==========Mean trace plot==========*)
		If[opMeanPlot, 
			grMean=ListLinePlot[tempMean, 
				Sequence@@NNJoinOptionLists[ListLinePlot, {PlotStyle->OptionValue[NNMeanPlotStyle]}, {opts}]],
			grMean=Graphics[{}]
		](*If[opMean*);

		(*==========Error trace/shading plot==========*)
		If[opErrorPlot || opErrorPlotShaded,
			Switch[opErrorType,
				"StandardError", 			
					tempError=StandardDeviation[ traces ]/Sqrt[Length[traces]],
				"StandardDeviation",
					tempError=StandardDeviation[ traces ],
				_,
					Message[ NNListLinePlotMean::invalidOptionValue, {"NNErrorType", ToString[opErrorType]} ]
			]
		];
		grError=If[opErrorPlotShaded,
			ListLinePlot[{tempMean-tempError, tempMean+tempError}, 
				Sequence@@NNJoinOptionLists[ListLinePlot,
					{PlotStyle->None, Filling->{1->{2}}, FillingStyle->OptionValue[NNErrorPlotFillingStyle]},
					{opts}
					]
			],
			Graphics[]
		];
		If[opErrorPlot,
			PrependTo[grError,
				ListLinePlot[{tempMean-tempError, tempMean+tempError}, 
					Sequence@@NNJoinOptionLists[ListLinePlot,
						{PlotStyle->OptionValue[NNErrorPlotStyle]},
						{opts}
						]
				]
			]
		]
	];(*If[(opMeanPlot || opErrorPlot ) && Length[traces]>=2 *)

	(*==========Traces plot==========*)
	If[OptionValue[NNTracePlot],
		grMain = ListLinePlot[traces, 
				Sequence@@NNJoinOptionLists[ListLinePlot,{opts}, Options[NNListLinePlotMean]]
		],
		grMain = Graphics[]
	](*If[opErrorPlot]*);


	(*==========Combine plots==========*)
	Show[grMain, grError, grMean]
];


NNListLinePlotMean[args___] := Message[NNListLinePlotMean::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNStackLists / NNListLinePlotStack*)


NNStackLists[traces_ /; MatrixQ[traces], stackAmplitude_] :=
  Module[{addFactor},
   (*tempData = traces - (Mean /@ traces);*)
   addFactor = Range[Length[traces]]*stackAmplitude;
   addFactor = Reverse[addFactor];
   (*tempData*)traces + addFactor
   ];


NNStackLists[args___] := Message[NNStackLists::invalidArgs, {args}];


NNListLinePlotStack[traces_/;Length[Dimensions[traces]]==2, opts:OptionsPattern[]]:=
Module[{tempData, opStackLists, n},

	(*Stack Traces*)
	opStackLists=OptionValue[NNStackLists];

	If[opStackLists===Automatic,
		opStackLists=2*Mean[RootMeanSquare /@ traces ]
	];

	If[NumberQ[opStackLists],
		If[opStackLists==0 || opStackLists===None,
			tempData=traces,
			tempData={};
			For[n=1, n<=Length[traces], n++,
				AppendTo[tempData, traces[[n]]+opStackLists*(n-1)]
			](*For*)
		],
		Message[NNListLinePlotStack::invalidOptionValue, opStackLists]
	];


	(*==========Main plotting section==========*)
	ListLinePlot[
		tempData,
		Sequence@@NNJoinOptionLists[
			OptionValue[ListLinePlotOptions],
			OptionValue[Options[NNListLinePlotStack], ListLinePlotOptions]
		]
	]
];


NNListLinePlotStack[args___] := Message[NNListLinePlotStack::invalidArgs, {args}];


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)
EndPackage[]
