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
	NNBaselineCorrection->Mean, NNUnit->"mS", NNMasking->True},
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


NNTracePlot[ channels:{_Integer ..}, times_Span, segment_:0, opts:OptionsPattern[] ]:= 
							NNTracePlot[ NounouM2`$NNReader@data[], channels, times, segment, opts];


NNTracePlot[ channel_Integer, times_Span, segment_:0, opts:OptionsPattern[] ]:= 
							NNTracePlot[ NounouM2`$NNReader@data[], {channel}, times, segment, opts];


NNTracePlot[dataReader_/;NNDataReaderJavaObjectQ[dataReader], 
			channel_Integer, times_Span, segment_:0, opts:OptionsPattern[]]:= 
							NNTracePlot[dataReader@data[], {channel}, times, segment, opts]; 


NNTracePlot[dataReader_/;NNDataReaderJavaObjectQ[dataReader], 
			channels:{_Integer ..}, times_Span, segment_:0, opts:OptionsPattern[]]:= 
							NNTracePlot[dataReader@data[], channels, times, segment, opts]; 


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], 
			channel_Integer , times_Span, segment_:0, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, segment, opts];


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], 
			channels:{_Integer ..}, times_Span, segment_:0, opts:OptionsPattern[]]:= 
Block[{traces, tracesWidth,
		opNNStackLists, 
		grOptDataRange, frameSpan, frameSeg1, frameSeg2, opNNUnit, 
		opAspectRatio, tempDataAbsUnit, tempMaskingEpilog },

  opNNStackLists = OptionValue[NNStackLists];
  (*If[opNNStackLists === Automatic, opNNStackLists = - 200];   //ToDo: Make default more fancy *)

  (*==========Handle units... must change DataRange and also frames to extract.==========*)
  frameSpan = {times[[1]], times[[2]]};
  opNNUnit = OptionValue[NNUnit];
  If[ opNNUnit === "ms" || opNNUnit === "mS" ,
	frameSeg1 = xData@msToFrame[frameSpan[[1]]];
	frameSeg2 = xData@msToFrame[frameSpan[[2]]];

    grOptDataRange = frameSpan;
	frameSpan = {frameSeg1, frameSeg2},
  If[ opNNUnit === "frames",
	grOptDataRange = frameSpan,
    Message[ NNTracePlot::invalidOptionValue, opNNUnit, NNUnit ]
  ]];

  (*==========Data==========*)
  traces = xData@readTraceAbsA[#, NN`fr@@frameSpan, segment]& /@ channels;
    tracesWidth = Max[traces]-Min[traces];

  (*==========Handle graphing options==========*)
  opAspectRatio = OptionValue[AspectRatio];
  If[ opAspectRatio === Automatic, opAspectRatio = 1/10*(Length[channels]+1) ];

  tempDataAbsUnit = xData@absUnit[];
  tempDataAbsUnit = StringReplace[tempDataAbsUnit, "micro" -> "\[Mu]" ];

  tempMaskingEpilog = (NounouM2`$NNReader@mask[])@activeMasksA[frameSpan[[1]], frameSpan[[2]], segment, xData];

  tempMaskingEpilog = If[Length[Flatten[tempMaskingEpilog]]==0,  
		Graphics[],
		Graphics[
			Flatten[Join[{Opacity[0.2, Black]},
				If[opNNUnit === "ms", 
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
			{NNStackLists -> opNNStackLists, 
			NNStackAxes->True, 
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
