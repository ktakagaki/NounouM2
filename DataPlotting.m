(* ::Package:: *)

BeginPackage["NounouM2`DataPlotting`", { "JLink`", "NounouM2`", "NounouM2`Plotting`"}]
(* Exported symbols added here with SymbolName::usage *)  


(* ::Section:: *)
(*Declarations*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


Options[NNTracePlot] =
NNJoinOptionLists[
	{NNStackLists->Automatic, (*ScaleBars->{None, None}, *) NNAxes->{"mS", "mV"} , NNStackListsBaselineCorrection->Mean},
	{PlotStyle->Black, AspectRatio->1/5, PlotRange->All}
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
			channels:{_Integer ..}, frames_Span, segment_:0, opts:OptionsPattern[]]:= 
Block[{traces, opNNStackLists },

  opNNStackLists = OptionValue[NNStackLists];
  If[opNNStackLists === Automatic, opNNStackLists = - 100];  (* //ToDo: Make default more fancy *)

  traces = dataReader@dataTraceAbs[#, NN`frameRange@@frames, segment]& /@ channels;
  NNListLinePlotStack[ traces, 
		NNStackLists -> opNNStackLists, 
		NNStackAxes->True, 
		NNStackListsBaselineCorrection->OptionValue[NNStackListsBaselineCorrection]]
];


NNTracePlot[args___]:=Message[NNTracePlot::invalidArgs,{args}];


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)

EndPackage[]
