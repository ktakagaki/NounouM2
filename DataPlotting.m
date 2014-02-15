(* ::Package:: *)

BeginPackage["NounouM2`DataPlotting`", { "JLink`", "NounouM2`", "NounouM2`Plotting`"}]
(* Exported symbols added here with SymbolName::usage *)  


(* ::Section:: *)
(*Declarations*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


Options[NNTracePlot] =
JoinOptionLists[
	{NNStackTraces->Automatic, (*ScaleBars->{None, None}, *) NNAxes->{"mS", "mV"} },
	{PlotStyle->Black, AspectRatio->1/5, PlotRange->All}
];


NNEventPlot::usage="  ";


NNSpikesPlot::usage="  ";


(* ::Section:: *)
(*Private*)


Begin["`Private`"] (* Begin Private Context *) 


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)

EndPackage[]
