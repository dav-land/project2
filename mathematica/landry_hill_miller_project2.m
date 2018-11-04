(* ::Package:: *)

(* ::Title:: *)
(*APPM 2350 Project #2 *)


(* ::Subsection:: *)
(*By Davis Landry, Mahalie Hill, Mia Miller*)


(* ::Chapter:: *)
(*3 Defining the Mountain Range*)


(* ::Input:: *)
(*mR = {{{1,1},1,.9},{{3,1},3,1},{{3.75,1},2,.8},{{4,3},2,1},{{3,3.5},3,.75},{{2,3},2,.9},{{.75,3},4,.5}};*)
(**)
(*gaussian[\[Epsilon]_,r_] = E^(-(\[Epsilon]*r)^2);*)
(**)
(*m[x_,y_] = Sum[mR[[i,3]]* gaussian[mR[[i,2]],Abs[Sqrt[(mR[[i,1,1]]-x)^2 + (mR[[i,1,2]]-y)^2]]],{i,7}];*)
(**)
(*mountainPlot3D = Plot3D[m[x,y],{x,0,5},{y,0,5}, AxesLabel->{"x","y","z"}, PlotLabel->"Lagrange Mountain Range"]*)
(**)
(*mountainContour = ContourPlot[m[x,y],{x,0,5},{y,0,5}, PlotLegends->Automatic, AxesLabel->{"x","y"}, PlotLabel->"Lagrange Mountain Range Contour"]*)


(* ::Chapter:: *)
(*4 Analyzing the Mountain Range*)


(* ::Input:: *)
(*r[t_] = {2.5 + 1.8Cos[4t], 2+1.2Sin[4t]};*)
(*r3D[t_] = {2.5 + 1.8Cos[4t], 2+1.2Sin[4t],m[2.5 + 1.8Cos[4t], 2+1.2Sin[4t]]};*)
(**)
(*path3D = ParametricPlot3D[r3D[t],{t,0,\[Pi]/2}, PlotStyle->{Red,Thickness[.01]}];*)
(*path = ParametricPlot[r[t],{t,0,\[Pi]/2}, PlotStyle->Red];*)
(**)
(*Show[{mountainPlot3D,Graphics3D[{PointSize[Large],Point[r3D[0]]}],path3D}]*)
(*Show[{mountainContour, Graphics[{PointSize[Large],Blue,Point[r[0]]}] ,path}]*)



