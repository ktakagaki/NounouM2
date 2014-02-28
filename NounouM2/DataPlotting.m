(* ::Package:: *)

BeginPackage["NounouM2`DataPlotting`", { "JLink`", "NounouM2`", "NounouM2`Plotting`"}]
(* Exported symbols added here with SymbolName::usage *)  


(* ::Section:: *)
(*Declarations*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


NNTracePlot$UniqueOptions = NNJoinOptionLists[ 
	{NNStackLists->Automatic, (*ScaleBars->{None, None}, *)AspectRatio->Automatic,  
	NNBaselineCorrection->Mean, NNUnit->"ms", NNMasking->True},
	{PlotStyle->{Opacity[0.75]}, PlotRange->Automatic, BaseStyle->{FontFamily->"Helvetica"}, ImageSize->10*72}
];

Options[NNTracePlot] = NNJoinOptionLists[ 
	NNTracePlot$UniqueOptions,
	Options[NNListLinePlotStack]
];


NNEventPlot::usage="  ";


NNSpikesPlot::usage="  ";


(* ::Section:: *)
(*Private*)


Begin["`Private`"] (* Begin Private Context *) 


(* ::Subsection:: *)
(*NNTracePlot*)


NNTracePlot[ channels:{_Integer ..}, x___ ]:= NNTracePlot[ NounouM2`$NNReader@data[], channels, x];
NNTracePlot[ channel_Integer, x___ ]:= NNTracePlot[ NounouM2`$NNReader@data[], channel, x];


NNTracePlot[dataReader_/;NNDataReaderJavaObjectQ[dataReader], x___] := NNTracePlot[dataReader@data[], x]; 


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channel_Integer , times_Span, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, 0, opts];


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channel_Integer , times_Span, segment_/;NumberQ[segment], opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, segment, opts];


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channels:{_Integer ..}, span_Span, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, channels, span, 0, opts];


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], 
			channels:{_Integer ..}, span_Span, segment_/;NumberQ[segment], opts:OptionsPattern[]]:= 
Block[{traces, tracesWidth,
		tempFullSpan, grOptDataRange, frameSpan, frameSeg1, frameSeg2, opNNUnit,opNNUnitMs, 
		opAspectRatio, tempDataAbsUnit, tempMaskingEpilog },

	opNNUnit = OptionValue[NNUnit];
	opNNUnitMs = If[ opNNUnit === "ms", True,
				If[ opNNUnit === "frames", False,
					Message[ NNTracePlot::invalidOptionValue, opNNUnit, NNUnit ]
				]];

	(*==========Handle units and spans... must change DataRange and also frames to extract.==========*)
	(*no All specifications for the span!*)
	If[ ! (And@@NumberQ/@ span), Message[ NNTracePlot::invalidArgs, span ] ];
	If[ opNNUnitMs,
		frameSpan = {xData@msToFrame[span[[1]]], xData@msToFrame[span[[2]]]};
		If[Length[span]==2, frameSpan = AppendTo[frameSpan, 1],
		If[Length[span]==3, frameSpan = AppendTo[frameSpan, xData@msToFrame[span[[3]]]],
			Message[ NNTracePlot::invalidArgs, span ]
		]],
		frameSpan = {span[[1]], span[[2]]};
		If[Length[span]==2, frameSpan = AppendTo[frameSpan, 1],
		If[Length[span]==3, frameSpan = AppendTo[frameSpan, span[[3]]],
			Message[ NNTracePlot::invalidArgs, span ]
		]]
	];
	grOptDataRange = {span[[1]], span[[2]]};
Print[segment];
	(*==========Data==========*)
	traces = xData@readTraceAbsA[#, NN`fr@@frameSpan, segment]& /@ channels;
    tracesWidth = Max[traces]-Min[traces];

	(*==========Handle graphing options==========*)
	opAspectRatio = OptionValue[AspectRatio];
	If[ opAspectRatio === Automatic, opAspectRatio = 1/10*(Length[channels]+1) ];

	tempDataAbsUnit = xData@absUnit[];
	tempDataAbsUnit = StringReplace[tempDataAbsUnit, "micro" -> "\[Mu]" ];
	
	tempMaskingEpilog = (NounouM2`$NNReader@mask[])@getActiveMasksA[frameSpan[[1]], frameSpan[[2]], segment, xData];
	tempMaskingEpilog = If[Length[Flatten[tempMaskingEpilog]]==0,  
		Graphics[],
		Graphics[
			Flatten[Join[{Opacity[0.2, Black]},
				If[opNNUnitMs, 
			        {Rectangle[{(xData@tsToMs[#[[1]]]), - tracesWidth*5},
								{(xData@tsToMs[#[[2]]]), tracesWidth*30}]}& /@ tempMaskingEpilog,
					{Rectangle[{(xData@tsToFrameSegmentA[#[[1]]])[[1]], - tracesWidth*5},
								{(xData@tsToFrameSegmentA[#[[2]]])[[1]], tracesWidth*30}]}& /@ tempMaskingEpilog
				]
			]]
		]
	];

  (*==========Plot==========*)
  Show[
      NNListLinePlotStack[ traces,
		Sequence@@NNJoinOptionLists[ NNListLinePlotStack,
			{opts}, 
			{NNStackAxes->True, 
			NNBaselineCorrection->OptionValue[NNBaselineCorrection],
			AxesLabel->{opNNUnit, tempDataAbsUnit},
			AspectRatio->opAspectRatio,
			DataRange->grOptDataRange,
		PlotRange->Automatic},
			NNTracePlot$UniqueOptions
		]
      ],
	  tempMaskingEpilog
  ]
  
];


NNTracePlot[args___]:=Message[NNTracePlot::invalidArgs,{args}];


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)

EndPackage[]
