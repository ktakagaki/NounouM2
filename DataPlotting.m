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
	NNStackListsBaselineCorrection->Mean, NNTracePlotUnit->"mS"},
	{(*PlotStyle->Black,*) PlotRange->All, BaseStyle->{FontFamily->"Helvetica"}}
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


NNTracePlot[ channels:{_Integer ..}, frames_Span, segment_:0, opts:OptionsPattern[] ]:= 
	NNTracePlot[ Global`$NNReader, channels, frames, segment, opts];


NNTracePlot[ channel_Integer, frames_Span, segment_:0, opts:OptionsPattern[] ]:= 
	NNTracePlot[ Global`$NNReader, channel, frames, segment, opts];


NNTracePlot[dataReader_/;NNDataReaderJavaObjectQ[dataReader], 
			channels:{_Integer ..}, times_Span, segment_:0, opts:OptionsPattern[]]:= 
Block[{traces, opNNStackLists, 
		grOptDataRange, frameSpan, frameSeg1, frameSeg2, 
		opNNTracePlotUnit, opAspectRatio, tempDataAbsUnit },

  opNNStackLists = OptionValue[NNStackLists];
  If[opNNStackLists === Automatic, opNNStackLists = - 200];  (* //ToDo: Make default more fancy *)

  (*==========Handle units... must change DataRange and also frames to extract.==========*)
  frameSpan = {times[[1]], times[[2]]};
  opNNTracePlotUnit = OptionValue[NNTracePlotUnit];
  If[ opNNTracePlotUnit === "ms" || opNNTracePlotUnit === "mS" ,
	frameSeg1 = dataReader@dataMsToFrameSegment[frameSpan[[1]]];
	frameSeg2 = dataReader@dataMsToFrameSegment[frameSpan[[2]]];

	If[ (frameSeg1[[2]] == frameSeg2[[2]] && frameSeg1[[2]] == segment),
        grOptDataRange = frameSpan;
		frameSpan = {frameSeg1[[1]], frameSeg2[[1]]},
		Message[ NNTracePlot::invalidOptionValue, frameSpan, "times_Span argument" ]
	],
  If [ opNNTracePlotUnit =!= "frames",
    Message[ NNTracePlot::invalidOptionValue, opNNTracePlotUnit, NNTracePlotUnit ]
  ]];

  opAspectRatio = OptionValue[AspectRatio];
  If[ opAspectRatio === Automatic, opAspectRatio = 1/10*(Length[channels]+1) ];

  tempDataAbsUnit = dataReader@dataAbsUnit[];
  If[ tempDataAbsUnit === "microV", tempDataAbsUnit = "\[Mu]V" ];

  traces = dataReader@dataTraceAbs[#, NN`frameRange@@frameSpan, segment]& /@ channels;
  NNListLinePlotStack[ traces,
		Sequence@@NNJoinOptionLists[ NNListLinePlotStack,
			{opts}, 
			NNStackLists -> opNNStackLists, 
			NNStackAxes->True, 
			NNStackListsBaselineCorrection->OptionValue[NNStackListsBaselineCorrection],
			AxesLabel->{opNNTracePlotUnit, tempDataAbsUnit},
			AspectRatio->opAspectRatio,
			DataRange->grOptDataRange,
			NNTracePlot$UniqueOptions
		]
  ]
];


NNTracePlot[args___]:=Message[NNTracePlot::invalidArgs,{args}];


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)

EndPackage[]
