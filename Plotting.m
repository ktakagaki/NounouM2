(* ::Package:: *)

BeginPackage["NounouM2`Plotting`", { "NounouM2`"}]
(* Exported symbols added here with SymbolName::usage *)  


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
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


NNListLinePlotMean$UniqueOptions = NNJoinOptionLists[
	{
	NNTracePlot->True,
	NNMeanPlot->True, NNMeanPlotStyle->{Opacity[0.5, Black]}, 
	NNErrorPlot->True, NNErrorType->"StandardError", NNErrorPlotStyle->Opacity[0.25,Blue],
	NNErrorPlotShaded->True, NNErrorPlotFillingStyle->Opacity[0.25,Blue] 
	},
	{PlotStyle->Opacity[0.05,Black]}
];
 
Options[NNListLinePlotMean] =
NNJoinOptionLists[
	NNListLinePlotMean$UniqueOptions,
	Options[ListLinePlot]
];


(* ::Subsection:: *)
(*NNStackTraces / NNListLinePlotStack*)


NNStackLists::usage="Augments a series of traces so that when plotted, they will be stacked. " <>
"Default for NNStackLists when given as an option is to stack lists every 2*(mean RMS value) apart.";


Options[NNStackLists] = {NNBaselineCorrection -> Mean};


NNListLinePlotStack::usage=
"ListLinePlotMean plots multiple traces together, along with mean and standard error.";


NNListLinePlotStack$UniqueOptions = NNJoinOptionLists[
	{NNStackLists->Automatic, NNStackAxes->False, NNBaselineCorrection-> Mean},
	{ AspectRatio->1/2, PlotRange->All}
];
 
Options[NNListLinePlotStack] =
NNJoinOptionLists[
	NNListLinePlotStack$UniqueOptions,
	Options[NNStackLists],
	Options[ListLinePlot]
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"] (* Begin Private Context *) 


(*ColorData[1] // InputForm*)


(* ::Subsection::Closed:: *)
(*NNListLinePlotMean*)


NNListLinePlotMean[traces_/;Length[Dimensions[traces]]==2, opts:OptionsPattern[]]:=
Module[{(*tempDataS, *)tempMean, tempError,
		opMeanPlot, opErrorPlot, opErrorPlotShaded, opErrorType,
		grMain, grMean, grError, grErrorShading},

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
		grErrorShading=If[opErrorPlotShaded,
			ListLinePlot[{tempMean-tempError, tempMean+tempError}, 
				Sequence@@NNJoinOptionLists[ListLinePlot,
					{PlotStyle->None, Filling->{1->{2}}, FillingStyle->OptionValue[NNErrorPlotFillingStyle]},
					{opts}
					]
			],
			Graphics[]
		];
		grError=If[opErrorPlot,
			ListLinePlot[{tempMean-tempError, tempMean+tempError}, 
				Sequence@@NNJoinOptionLists[ListLinePlot,
					{PlotStyle->OptionValue[NNErrorPlotStyle]},
					{opts}
					]
			],
			Graphics[]
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
	Show[grMain, grErrorShading, grError,  grMean]
];


NNListLinePlotMean[args___] := Message[NNListLinePlotMean::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNStackLists / NNListLinePlotStack*)


NNStackLists[traces_ /; MatrixQ[traces], stackAmplitude_, opts:OptionsPattern[]] :=
  Block[{tempTraces, addFactor, opNNBaselineCorrection},
	
	opNNBaselineCorrection = OptionValue[NNBaselineCorrection];
	If[ opNNBaselineCorrection === None,
		tempTraces = traces,
	If[ opNNBaselineCorrection === Mean,
		tempTraces = traces - Mean /@ traces,
	If[ Head[opNNBaselineCorrection] === Span,
		tempTraces = traces - (Mean[#[[ opNNBaselineCorrection ]]]& /@ traces),
		Message[ NNStackLists::invalidOptionValue, NNBaselineCorrection, opNNBaselineCorrection]
	]]];

   (*tempData = traces - (Mean /@ traces);*)
   addFactor = (Range[Length[tempTraces]]-0.5)*stackAmplitude;
   addFactor = Reverse[addFactor];
   (*tempData*)tempTraces + addFactor
   ];


NNStackLists[args___] := Message[NNStackLists::invalidArgs, {args}];


NNListLinePlotStack[traces_/;Length[Dimensions[traces]]==2, opts:OptionsPattern[]]:=
Module[{tempData, opNNBaselineCorrection, 
		opNNStackLists, tempTickInterval,
		opPlotRange, opAxesOriginX, grStackAxes, n},
	
	tempData = traces;

	(*Correct baseline beforehand, since it changes NNStackLists handling.*)	
	opNNBaselineCorrection = OptionValue[NNBaselineCorrection];
	If[ opNNStackLists===Automatic && opNNBaselineCorrection === Mean,
		tempData = tempData - Mean /@ tempData,
	If[ Head[opNNBaselineCorrection] === Span,
		tempData = tempData - (Mean[#[[ opNNBaselineCorrection ]]]& /@ tempData)
	]];
	(*Stack Traces*)
	opNNStackLists=OptionValue[NNStackLists];

	If[opNNStackLists===Automatic,
		opNNStackLists = (Quantile[Flatten[Abs /@ tempData], 0.95])*2;
		If[opNNStackLists != 0,
			tempTickInterval = NNNextPower[10, opNNStackLists]/5;
			opNNStackLists = (Quotient[opNNStackLists, tempTickInterval] + 1)*tempTickInterval
		]
	];



	If[opNNStackLists!=0 && opNNStackLists=!=None && NumberQ[opNNStackLists],
		tempData = NNStackLists[tempData, opNNStackLists, 
				NNBaselineCorrection -> OptionValue[NNBaselineCorrection]   ]
	];

	opPlotRange = OptionValue[PlotRange]; 
	If[ opPlotRange === Automatic, 
			opPlotRange = {All, {Min[#],Max[#]}& [{- opNNStackLists, opNNStackLists*Length[traces]}]  } 
	];

	opAxesOriginX = OptionValue[DataRange];
	opAxesOriginX = 
		If[ opAxesOriginX === Automatic,
			0,
		If[ ListQ[opAxesOriginX] && Length[opAxesOriginX]==2,
			opAxesOriginX[[1]],
			Message[ NNListLinePlotStack::invalidOptionValue, opAxesOriginX, DataRange ]
		]];

	grStackAxes = 
		If[ OptionValue[NNStackAxes], 
			{
			GridLines -> {None, Table[n*opNNStackLists, {n, 0, Length[traces]-1}]}
			},
			{}
		];

	(*==========Main plotting section==========*)
	ListLinePlot[
		tempData,
		Sequence@@NNJoinOptionLists[ ListLinePlot,
			{opts},
			grStackAxes,
			{AxesOrigin -> {opAxesOriginX, Min[- opNNStackLists, opNNStackLists*Length[traces]]},
			PlotRange -> opPlotRange},
			NNListLinePlotStack$UniqueOptions
		]
	]
];


NNListLinePlotStack[args___] := Message[NNListLinePlotStack::invalidArgs, {args}];


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)
EndPackage[]
