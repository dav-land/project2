(* ::Package:: *)

(* ::Title:: *)
(*APPM 2350 Project #2 *)


(* ::Subsection:: *)
(*By Davis Landry, Mahalie Hill, Mia Miller*)


(* ::Chapter:: *)
(*3 Defining the Mountain Range*)


(* ::Input:: *)
(*mR = {{{1,1},1,.9,"K-13"},{{3,1},3,1,"Mount Adamore"},{{3.75,1},2,.8,"Issaquah Peak"},{{4,3},2,1,"Mount Jojo"},{{3,3.5},3,.75,"Soweroski Peak"},{{2,3},2,.9,"Leibs Peak"},{{.75,3},4,.5,"Jacobi Peak"}};*)
(**)
(*gaussian[\[Epsilon]_,r_] = E^(-(\[Epsilon]*r)^2);*)
(*m[x_,y_] := Sum[mR[[i,3]]* gaussian[mR[[i,2]],Sqrt[(Sqrt[(mR[[i,1,1]]-x)^2 + (mR[[i,1,2]]-y)^2])^2]],{i,7}];*)
(*mountainLabels = Table[Graphics[{White, Text[mR[[i,4]],mR[[i,1]]]}],{i,7}];*)
(*mountainLabels3D = Table[Graphics3D[{White,Text[mR[[i,4]],{mR[[i,1,1]],mR[[i,1,2]],m[mR[[i,1,1]],mR[[i,1,2]]]}]}],{i,7}];*)
(**)
(*mountainPlot3D = Plot3D[m[x,y],{x,0,5},{y,0,5}, AxesLabel->{"x in 1000ft","y in 1000ft","From 7000ft in 1000ft"}, PlotLabel->"Lagrange Mountain Range", ColorFunction->"GreenBrownTerrain"];*)
(**)
(*mountainContour = ContourPlot[m[x,y],{x,0,5},{y,0,5}, PlotLegends->Automatic, ColorFunction->"Rainbow",Frame->{True,True,False,False},*)
(*PlotLabel->"Lagrange Mountain Range Contour", Contours->20, FrameLabel->{"x in 1000ft","y in 1000ft"}];*)
(**)
(**)


(* ::Chapter:: *)
(*4 Analyzing the Mountain Range*)


(* ::Subchapter:: *)
(*Plotting Paths*)


(* ::Input:: *)
(*mt[t_] :=Sum[mR[[i,3]]* gaussian[mR[[i,2]],Sqrt[(Sqrt[(mR[[i,1,1]]-(2.5 + 1.8Cos[4t]))^2 + (mR[[i,1,2]]-(2+1.2Sin[4t]))^2])^2]],{i,7}];*)
(**)
(*r[t_] = {2.5 + 1.8Cos[4t], 2+1.2Sin[4t]};*)
(*r3D[t_] := {2.5 + 1.8Cos[4t], 2+1.2Sin[4t],mt[t]};*)
(**)
(*path3D = ParametricPlot3D[r3D[t],{t,0,\[Pi]/2}, PlotStyle->{Red,Thickness[.01]}];*)
(*path = ParametricPlot[r[t],{t,0,\[Pi]/2}, PlotStyle->Red];*)
(**)
(*point3D = Graphics3D[{PointSize[Large],Green,Point[r3D[0]]}];*)
(*point = Graphics[{PointSize[Large],Green,Point[r[0]]}];*)
(**)
(*Show[{mountainPlot3D  ,path3D,point3D, mountainLabels3D}]*)
(*Show[{mountainContour,path    ,point, mountainLabels}]*)


(* ::Subchapter:: *)
(*Honey Badger*)


(* ::Input:: *)
(*gradM[x_,y_] = Grad[m[x,y],{x,y}];*)
(*(* a = Table[Graphics3D[Arrow[Tube[{r3D[t],r3D[t]+gradr3D[t]}]]],{t,0,\[Pi]/2,\[Pi]/30}];*)
(*Show[{mountainPlot3D  ,path3D,point3D,a}] *)
(**)*)
(**)
(*Dot[gradM[2.5 + 1.8Cos[\[Pi]], 2+1.2Sin[\[Pi]]],Normalize[r'[\[Pi]/4]]]*)
(*mt'[\[Pi]/4]*)
(**)


(* ::Subchapter:: *)
(*Change of Elevation*)


(* ::Input:: *)
(*Maximize[{mt'[t],t>= 0},t]*)
(*Minimize[{mt'[t],0<=t<= \[Pi]/2},t]*)
(*Plot[mt'[t],{t,0,\[Pi]/2},PlotLabel->"Rate Change of Elevation",AxesLabel->{"hrs","1000ft/hr"}]*)


(* ::Input:: *)
(*1.221086074164095*60*)
(*0.5183504795465874`*60*)


(* ::Subchapter:: *)
(*Highest Elevation*)


(* ::Input:: *)
(*gradMount[x_,y_] := {16. E^(-16 ((0.75 -x)^2+(3-y)^2)) (0.75 -x)+1.8 E^(-(1-x)^2-(1-y)^2) (1-x)+7.2E^(-4 ((2-x)^2+(3-y)^2)) (2-x)+18 E^(-9 ((3-x)^2+(1-y)^2)) (3-x)+13.5 E^(-9 ((3-x)^2+(3.5-y)^2)) (3-x)+6.4 E^(-4 ((3.75-x)^2+(1-y)^2)) (3.75-x)+8 E^(-4 ((4-x)^2+(3-y)^2)) (4-x),1.8 E^(-(1-x)^2-(1-y)^2) (1-y)+18 E^(-9 ((3-x)^2+(1-y)^2)) (1-y)+6.4 E^(-4 ((3.75-x)^2+(1-y)^2)) (1-y)+16. E^(-16 ((0.75-x)^2+(3-y)^2)) (3-y)+7.2 E^(-4 ((2-x)^2+(3-y)^2)) (3-y)+8 E^(-4 ((4-x)^2+(3-y)^2)) (3-y)+13.5 E^(-9 ((3-x)^2+(3.5-y)^2)) (3.5 -y)};*)
(*carR[x_,y_] := (x-2.5)^2/3.24+(y-2)^2/1.44;*)
(*gradR[x_,y_] := {0.6172839506172839 (-2.5+x),1.3888888888888888 (-2+y)};*)
(**)
(*FindRoot[{gradMount[x,y] == \[Lambda]*gradR[x,y],carR[x,y]== 1},{{\[Lambda],-1},{x,3},{y,1}}]*)
(**)
(*(*Plot[mt[t],{t,0,\[Pi]/2}]*)*)
(*m[3.074084653389694,0.8626684621463107]*)
(**)


(* ::Text:: *)
(*Thus the highest point is at 7934 ft above sea level.*)


(* ::Subchapter:: *)
(*Lake Mochi*)


(* ::Input:: *)
(*FindRoot[gradMount[x,y]==0 ,{{x,3},{y,2}}]*)
(**)
(*mochi = Graphics[{Blue,Disk[{3.084891978358271,2.1326960054211934},1/3]}];*)
(*mochi3D = Graphics3D[{Blue,Cylinder[{{3.084891978358271,2.1326960054211934,0},{3.084891978358271,2.1326960054211934,0.025}},2/3]}];*)
(**)
(*Show[{mountainPlot3D  ,path3D,point3D, mochi3D, mountainLabels3D}]*)
(*Show[{mountainContour,path    ,point  , mochi, mountainLabels}]*)


(* ::Subchapter:: *)
(*Rock inside of trail*)


(* ::Input:: *)
(*NIntegrate[1,{x,0.7,4.3},{y,0.06666666666666667 (30. -1. Sqrt[-301.+500. x-100. x^2]),0.06666666666666667(30.+Sqrt[-301.+500. x-100. x^2])},{z,-7,m[x,y]}]*1000^3*)


(* ::Input:: *)
(**)


(* ::Subchapter:: *)
(*Clairautnium*)


(* ::Input:: *)
(*delta[x_,y_,z_] := E^(-.25((x-2.5)^2+(y-2.5)^2+(z-.2)^2));*)
(*NIntegrate[delta[x,y,z],{x,0,5},{y,0,5},{z,0,m[x,y]}]*1000^-3*)


(* ::Subchapter:: *)
(*For Intro*)


(* ::Input:: *)
(*delta[x_,y_,z_] := E^(-.25((x-2.5)^2+(y-2.5)^2+(z-.2)^2));*)
(*NIntegrate[delta[x,y,z],{x,0,5},{y,0,5},{z,0,m[x,y]}]*)
(*ArcLength[r3D[t],{t,0,\[Pi]/2}]*)


(* ::Input:: *)
(*NIntegrate[Abs[mt'[t]],{t,0,\[Pi]/2}]*1000*)


(* ::Subchapter:: *)
(*EC*)


(* ::Input:: *)
(*(* All of the Radial Functions *)*)
(*aussian[a_,r_] := E^(-(a*r)^2);*)
(*multiquadric[a_,r_] := Sqrt[1+(a*r)^2];*)
(*inverseQuadratic[a_,r_] := 1/(1+(a*r)^2);*)
(*inverseMultiquadric[a_,r_] := 1/Sqrt[1+(a*r)^2];*)
(**)
(*gau[l_,a_,x_,y_,xBar_,yBar_]    := l * aussian[a,Sqrt[(Sqrt[(xBar-x)^2 + (yBar-y)^2])^2]];*)
(*invM[l_,a_,x_,y_,xBar_,yBar_]  := l * inverseMultiquadric[a,Sqrt[(Sqrt[(xBar-x)^2 + (yBar-y)^2])^2]];*)
(*mult[l_,a_,x_,y_,xBar_,yBar_]  := l * multiquadric[a,Sqrt[(Sqrt[(xBar-x)^2 + (yBar-y)^2])^2]];invQ [l_,a_,x_,y_,xBar_,yBar_]:= l *inverseQuadratic[a,Sqrt[(Sqrt[(xBar-x)^2 + (yBar-y)^2])^2]];*)
(**)
(*funcTable[i_,l_,a_,x_,y_,xBar_,yBar_] := Switch[i,1,gau[l,a,x,y,xBar,yBar],2,invQ[l,a,x,y,xBar,yBar],3,invM[l,a,x,y,xBar,yBar],4,mult[l,a,x,y,xBar,yBar]];*)
(**)
(*(**)
(*List of point {{xMiddle,yMiddle},{epsilon},{lambda},{plot function}}}*)
(*plot function:*)
(*1:gaussian*)
(*2:inverseQuadratic*)
(*3:inverseMultiquadric*)
(*4:multiquadric*)
(**)*)
(**)
(*ecMount = Table[Table[{{Random[Real,{0,5},3],Random[Real,{0,5},3]},Random[Real,{1,5},3],Random[Real,{-.5,3},3],Random[Integer,{1,3}]},{k,Random[Integer,{10,40}]}],{m,50}];*)
(**)
(*ecM[x_,y_,j_] := Sum[funcTable[ecMount[[j,i,4]],ecMount[[j,i,3]],ecMount[[j,i,2]],x,y,ecMount[[j,i,1,1]],ecMount[[j,i,1,2]]],{i,Length[ecMount[[j]]]}];*)
(**)
(*allPlots = Table[Plot3D[ecM[x,y,n],{x,0,5},{y,0,5},PlotRange->All,ColorFunction->"GreenBrownTerrain"],{n,50}]*)
(**)
