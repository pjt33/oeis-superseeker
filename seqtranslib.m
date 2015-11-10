(* seqtranslib.m   version batch 0.10 -- 10 Jan 1998 *)
(* Integer Sequences Transformations Mathematica Library *)
(* by Olivier Gerard from original Maple code and ideas by N. J. A. Sloane *)
(* prim-ed Oct 30 2007 *)

(* Batch code *)
(* List of meaningful transformed sequences prefixed with indice of transform and without signs *)

SuperTrans[seq_List] := Abs[Cases[MapIndexed[ {First[#2],#1[seq]} & , EISTransTable ],{_Integer,{__Integer}}]]

(* Formatting routines *)

SeqString[seq_List] := StringTake[ToString[seq], {2, -2}]<>"\n";

WriteSeekerList[transeq_List,sortie_:$Output]:=Scan[ WriteString[sortie,"T",StringDrop[ToString[1000+#1[[1]]],1]," ",SeqString[#1[[2]]]]&, transeq]

WriteSeqList[transeq_List,sortie_:$Output]:=(MapIndexed[ WriteString[sortie,SeqString[#]]&, transeq];)





(* Utility and Transform Code *)

(* All programs are designed to work for Mathematica 2.0 and higher but Mathematica 3.0 is recommended and may be necessary in future versions. *)



(* Number Theory utilities *)



did[m_Integer, n_Integer] := If[Mod[m, n] == 0, 1, 0]; 
didsigned[m_Integer, n_Integer] := If[Mod[m, n] == 0, (-1)^(m/n), 0]; 
mob[m_Integer, n_Integer] := If[Mod[m, n] == 0, MoebiusMu[m/n], 0]; 

GCDNormalize[{}]:= {};
GCDNormalize[seq_List]:= seq/GCD@@seq;

LCMNormalize[{}]:={};
LCMNormalize[seq_List]:=DeleteCases[seq,0] // (LCM@@#/Reverse[#])&;

IntegerSequenceQ[seq_List]:= Union[IntegerQ/@seq]=={True}

FilterSequence[{}]:= {};
FilterSequence[seq_List]:=If[And@@(IntegerQ/@seq),seq,{}];



(* Difference Table utilities *)



GetDiff[{}] = {};
GetDiff[{elem_}] = {};
GetDiff[seq_List]:=Drop[seq,1]-Drop[seq,-1];
GetDiff[seq_List,n_Integer]:={}/;n>=Length[seq]
GetDiff[seq_List,n_Integer]:=
Plus@@Table[(-1)^(n+i)Binomial[n,i]Take[seq,{i+1,Length[seq]-n+i}],{i,0,n}]

GetOffsetDiff[seq_List]:=GetDiff[seq];
GetOffsetDiff[seq_List,n_Integer]:={}/;n>=Length[seq]
GetOffsetDiff[seq_List,n_Integer]:=
Take[seq,{n+1,-1}]-Take[seq,{1,-n-1}]

GetIntervalDiff[seq_List]:=GetDiff[seq];
GetIntervalDiff[seq_List,n_Integer]:={}/;n>=Length[seq]
GetIntervalDiff[seq_List,n_Integer]:=
Take[seq,{n+1,-1}]-Plus@@Table[Take[seq,{i+1,Length[seq]-n+i}],{i,0,n-1}]

GetSum[{}] = {};
GetSum[{elem_}] = {};
GetSum[seq_List]:=Drop[seq,1]+ Drop[seq,-1];
GetSum[seq_List,n_Integer]:={}/;n>=Length[seq]
GetSum[seq_List,n_Integer]:=
Plus@@Table[Binomial[n,i]Take[seq,{i+1,Length[seq]-n+i}],{i,0,n}]

GetOffsetSum[seq_List]:=GetSum[seq];
GetOffsetSum[seq_List,n_Integer]:={}/;n>=Length[seq]
GetOffsetSum[seq_List,n_Integer]:=
Take[seq,{1,-n-1}]+Take[seq,{n+1,-1}]

GetIntervalSum[seq_List]:=GetSum[seq];
GetIntervalSum[seq_List,n_Integer]:={}/;n>=Length[seq]
GetIntervalSum[seq_List,n_Integer]:=
Plus@@Table[Take[seq,{i+1,Length[seq]-n+i}],{i,0,n}]

DiffTable[{},___]={};
DiffTable[seq_List,1] := NestList[GetDiff,seq,Length[seq]-1];
DiffTable[seq_List,n_Integer] := NestList[GetDiff,DiffTable[seq,n-1][[Range[Length[seq]],1]],Length[seq]-1] /; n>1

PartialProducts[{}]={};
PartialProducts[seq_List]:=Rest[FoldList[#1 #2&,1,seq]];
PartialSums[{}]={};
PartialSums[seq_List]:=Rest[FoldList[#1+#2&,0,seq]];



(* Generating Function utilities *)





SeqToPoly[{}, ___] = {}; 
SeqToPoly[seq_List, var_Symbol:n] := 
   Expand[Plus @@ 
     (Table[Sum[(-1)^(i - 1 - k)*Binomial[i - 1, k]*seq[[k + 1]], {k, 0, i - 1}], 
        {i, 1, Length[seq]}]*Array[Binomial[var, #1 - 1] & , Length[seq]])]; 

GetPowerCoeffs[f_,var_Symbol:x,n_Integer]:=
  Block[{g=ExpandAll[f]},DeleteCases[Table[Coefficient[g,var,i],{i,0,n}],0]]

ListToSeries::unkn =
"This kind of generating function is unknown."; 
ListToSeries::nimp =
"This kind of generating function is not yet implemented. \
Sorry.";

ListToSeries[seq_List, var_Symbol:x, kind_String:"ogf"] := 
   Switch[kind,
   "ogf", SeriesData[var, 0, seq, 0, Length[seq], 1],
   "egf", SeriesData[var, 0, seq/Array[#1! & , Length[seq], 0], 0, Length[seq], 1],
   "lap", SeriesData[var, 0, seq*Array[#1! & , Length[seq], 0], 0, Length[seq], 1], 
    "lgdogf", ListToSeries::nimp,
    "lgdegf", ListToSeries::nimp, _, ListToSeries::unkn]; 

SeriesToList::unkn = ListToSeries::unkn;
SeriesToList::nimp = ListToSeries::nimp;

SeriesToList[ser_SeriesData, kind_String:"ogf"] := 
   Switch[kind,
   "ogf", Array[SeriesCoefficient[ser, #1] & , ser[[5]], 0],
   "egf", 
    Array[SeriesCoefficient[ser, #1]*#1! & , ser[[5]], 0],
   "lap", 
    Array[SeriesCoefficient[ser, #1]/#1! & , ser[[5]], 0],
    "lgdogf", SeriesToList::nimp,
    "lgdegf", SeriesToList::nimp,
    _, SeriesToList::unkn]; 

SeriesToSeries::unkn = ListToSeries::unkn; 

SeriesToSeries[ser_SeriesData, kind_String] := 
   If[kind == "ogf", ser, ListToSeries[Array[(SeriesCoefficient[ser, #1] & )*
       ser[[5]], 0], ser[[1]], kind], Null]; 

GetSeriesCoeff::"unkn"=ListToSeries::"unkn";
GetSeriesCoeff::"nimp"=ListToSeries::"nimp";
GetSeriesCoeff[ser_SeriesData,j_Integer, kind_String:"ogf"]:=
Switch[kind,
	"ogf", SeriesCoefficient[ser,j],
	"egf", SeriesCoefficient[ser,j ]*(j-1)!,
	"lap",SeriesCoefficient[ser,j]/(j-1)!,
	"lgdogf", GetSeriesCoeff::"nimp",
	"lgdegf", GetSeriesCoeff::"nimp",
	_,GetSeriesCoeff::"unkn"];

ListToListDiv[{}] := {}; 
ListToListDiv[seq_List] := seq/Array[#1! & , Length[seq], 0]; 
ListToListMult[{}] := {}; 
ListToListMult[seq_List] := seq*Array[#1! & , Length[seq], 0]; 

SeriesToListDiv[ser_SeriesData] := SeriesToList[ser, "egf"]; 
SeriesToListMult[ser_SeriesData] := SeriesToList[ser, "lap"]
SeriesToSeriesDiv[ser_SeriesData] := SeriesToSeries[ser, "egf"]
SeriesToSeriesMult[ser_SeriesData] := SeriesToList[ser, "lap"]



(* Binary and other bases utilities *)





MatchDigits[x_Integer,y_Integer,base_Integer:10]:=
Block[{bx,by,lx,ly},
{lx,ly}=Length/@(
	{bx,by}=(IntegerDigits[#1,base]&)/@{x,y});
If[lx-ly==0,
	{bx,by},
	(Join[Array[0&,Max[lx,ly]-Length[#1]],#1]&)/@{bx,by}]];

MatchBinary[x_Integer,y_Integer]:=MatchDigits[x,y,2];

DigitsToInteger[thedigits_List,base_Integer:10]:=
Plus@@(Reverse[thedigits] Array[base^#1&,Length[thedigits],0])
BinaryToInteger[bindig_List]:=DigitsToInteger[bindig,2]



Xcl[twodig_List]:=Length[Union[twodig]]-1;
NumAND[x_Integer,y_Integer]:=BinaryToInteger[Min/@Transpose[MatchBinary[x,y]]];
NumOR[x_Integer,y_Integer]:=BinaryToInteger[Max/@Transpose[MatchBinary[x,y]]];
Off[General::spell1]

NumXOR[x_Integer,y_Integer]:=BinaryToInteger[Xcl/@Transpose[MatchBinary[x,y]]];

On[General::spell1]
NimSum[numOne_Integer,numTwo_Integer]:=BinaryToInteger[Plus@@MatchBinary[numOne,numTwo]/.{2\[Rule]0}]

DigitSum[n_Integer,b_Integer:10]:=Plus@@IntegerDigits[n,b];
DigitRev[n_Integer,b_Integer:10]:=DigitsToInteger[Reverse[IntegerDigits[n,b]],b];



(* Set-Theoretical Transforms *)





$EISShortComplementSize=60;
$EISLongComplementSize=1000;
$EISUnsameCount=10;
$EISMaxCharSeq=100;



MinExcluded[seq_List]:=
Module[{theset=Union[seq]},
	Select[Range[0,Max[seq]+1],\[InvisibleSpace]!(MemberQ[theset,#1])&,1]]

Monotonous[{}]={};
Monotonous[seq_List]:=Union[Abs[seq]];

MonotonousDiff[{}]={};
MonotonousDiff[seq_List]:=Block[{seqres},If[seq==(seqres=Union[Abs[seq]])||Length[seq]-Length[seqres]<$EISUnsameCount,{},seqres]];

CompSequence[{}]={};
CompSequence[seq_List]:=Block[{seqres},
		If[
			Length[seqres=Complement[Range[Min[Max[seq],$EISShortComplementSize]],
						Monotonous[seq]]
			]<	$EISUnsameCount||
				Length[seqres]>$EISShortComplementSize-$EISUnsameCount,
		{},
		seqres]];
CompSequenceLong[{}]={};
CompSequenceLong[seq_List]:=Complement[Range[Min[Max[seq],$EISLongComplementSize]],Monotonous[seq]]



TwoValuesQ[seq_List]:= (Length[Union[seq]]==2)



CharSequence[{}]={};
CharSequence[seq_List]:=Block[{b,reslen},b=Monotonous[seq];reslen=Min[Max[b],$EISMaxCharSeq];
		Last[Transpose[Sort[Transpose[
						{Join[b,Complement[Range[0,reslen],b]],Join[Array[1&,Length[b]],Array[0&,1+reslen-Length[b]]]}]]]]]



(* SubSequence Extraction *)





SeqExtract[{}, ___] = {};
SeqExtract[seq_List, period_Integer:1, start_Integer:1] := 
   seq[[start + period*Range[0, Floor[(Length[seq] - start)/period]]]]



Decimate[{}, ___] = {}; 
Decimate[seq_List, k_Integer, j_Integer] := 
  With[{l = Floor[(Length[seq] + k - 1 - j)/k]}, 
   If[l < 1, {}, seq[[1 + j + k*Range[0, l - 1]]]]]
Bisect[seq_List, 0] := Decimate[seq, 2, 0]; 
  (Bisect[seq_List, 1] := Decimate[seq, 2, 1]; )
Bisect[seq_List, start_Integer] = {}; Trisect[seq_List, 0] := Decimate[seq, 3, 0]; 
  (Trisect[seq_List, 1] := Decimate[seq, 3, 1]; )
Trisect[seq_List, 2] := Decimate[seq, 3, 2]; 
Trisect[seq_List, start_Integer] = {}; 



(* Elementary Transforms *)



LeftTransform[{}]:={};
LeftTransform[seq_List]:=Rest[seq];
RightTransform[{}]:={};
RightTransform[seq_List]:=Prepend[seq,1];

MulTwoTransform[{}]={};
MulTwoTransform[seq_List]:=Join[{First[seq]},Rest[seq] 2];
DivTwoTransform[{}]={};
DivTwoTransform[seq_List]:=Join[{First[seq]},Rest[seq]/2];
NegateTransform[{}]={};
NegateTransform[seq_List]:=Join[{First[seq]},-Rest[seq]];



(* Difference Table (Binomial)  Transforms *)



BinomialTransform[{},___]={};
BinomialTransform[seq_List,way_:1]:=Table[Sum[way^(i - 1 - k)*Binomial[i - 1, k]*seq[[k + 1]], {k, 0, i - 1}],{i,1,Length[seq]}];
BinomialInvTransform[{},___]={};
BinomialInvTransform[seq_List,way_:1]:=BinomialTransform[seq,-way]



(* Rational Generating Function Transforms *)



GFProdaaaTransform[{},___]:={};
GFProdaaaTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[1/ListToSeries[seq,var,gftype]^2,gftype]]

GFProdbbbTransform[{},___]:={};
GFProdbbbTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]^2,gftype]]

GFProdcccTransform[{},___]:={};
GFProdcccTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[1/ListToSeries[seq,var,gftype],gftype]]

GFProddddTransform[{},___]:={};
GFProddddTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]*(1+var)/(1-var),gftype]]

GFProdeeeTransform[{},___]:={};
GFProdeeeTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]*(1-var)/(1+var),gftype]]

GFProdfffTransform[{},___]:={};
GFProdfffTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1-var),gftype]]

GFProdgggTransform[{},___]:={};
GFProdgggTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1-var)^2,gftype]]

GFProdhhhTransform[{},___]:={};
GFProdhhhTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1-var)^3,gftype]]

GFProdiiiTransform[{},___]:={};
GFProdiiiTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1+var),gftype]]

GFProdjjjTransform[{},___]:={};
GFProdjjjTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1+var^2),gftype]]

GFProdkkkTransform[{},___]:={};
GFProdkkkTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1+var+var^2),gftype]]

GFProdlllTransform[{},___]:={};
GFProdlllTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1-var^2),gftype]]

GFProdmmmTransform[{},___]:={};
GFProdmmmTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1-var-var^2),gftype]]

GFProdnnnTransform[{},___]:={};
GFProdnnnTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1-var+var^2),gftype]]

GFProdoooTransform[{},___]:={};
GFProdoooTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1+var-var^2),gftype]]

GFProdpppTransform[{},___]:={};
GFProdpppTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1+var)^2,gftype]]

GFProdqqqTransform[{},___]:={};
GFProdqqqTransform[seq_List,gftype_String:"ogf"]:=Module[{var},SeriesToList[ListToSeries[seq,var,gftype]/(1+var)^3,gftype]]



(* Diagonal Generating Function Transforms *)



GFDiagaaaTransform[{},___]:={};
GFDiagaaaTransform[seq_List,gftype_String:"ogf"]:=Module[{var,baseser},baseser = ListToSeries[Prepend[seq,0],var,gftype];
	Table[GetSeriesCoeff[baseser*(1-var)^j,j,gftype],{j,1,Length[seq]}]]

GFDiagbbbTransform[{},___]:={};
GFDiagbbbTransform[seq_List,gftype_String:"ogf"]:=Module[{var,baseser},baseser = ListToSeries[Prepend[seq,0],var,gftype];
	Table[GetSeriesCoeff[baseser*(1+var)^j,j,gftype],{j,1,Length[seq]}]]

GFDiagcccTransform[{},___]:={};
GFDiagcccTransform[seq_List,gftype_String:"ogf"]:=Module[{var,baseser},baseser = ListToSeries[Prepend[seq,0],var,gftype];
	Table[GetSeriesCoeff[baseser/(1-var)^j,j,gftype],{j,1,Length[seq]}]]

GFDiagdddTransform[{},___]:={};
GFDiagdddTransform[seq_List,gftype_String:"ogf"]:=Module[{var,baseser},baseser = ListToSeries[Prepend[seq,0],var,gftype];
	Table[GetSeriesCoeff[baseser/(1+var)^j,j,gftype],{j,1,Length[seq]}]]



(* Classical (Euler, M\[ODoubleDot]bius, Stirling) *)





EulerTransform[{}]={};
EulerTransform[seq_List]:=Module[{coeff,final={}},
coeff=Table[Sum[d*did[i, d]*seq[[d]], {d, 1, i}],{i,1,Length[seq]}];
For[i=1,i<=Length[seq],i++,AppendTo[final,(coeff[[i]] + Sum[coeff[[d]]*final[[i - d]], {d, 1, i - 1}])/i]];final];

EulerInvTransform[{}]={};
EulerInvTransform[seq_List]:=Module[{final={}},
For[i=1,i<=Length[seq],i++,AppendTo[final,i*seq[[i]] - Sum[final[[d]]*seq[[i - d]], {d, 1, i - 1}]]];
Table[Sum[mob[i, d]*final[[d]], {d, 1, i}]/i, {i, 1, Length[seq]}]];

MobiusTransform[{}]={};
MobiusTransform[seq_List]:=Table[Sum[mob[i, d]*seq[[d]], {d, 1, i}],{i,1,Length[seq]}]

MobiusInvTransform[{}]={};
MobiusInvTransform[seq_List]:=Table[Sum[did[i, d]*seq[[d]], {d, 1, i}],{i,1,Length[seq]}]

StirlingTransform[{}]={};
StirlingTransform[seq_List]:=Table[Sum[StirlingS2[i, k]*seq[[k]], {k, 1, i}],{i,1,Length[seq]}]
StirlingInvTransform[{}]={};
StirlingInvTransform[seq_List]:=Table[Sum[StirlingS1[i, k]*seq[[k]], {k, 1, i}],{i,1,Length[seq]}]



(* Number Theory Convolutions *)



LCMConvTransform[___List,{},___List]:={};
LCMConvTransform[seq_List]:=Table[Sum[LCM[seq[[k]], seq[[i - k + 1]]], {k, 1, i}],{i,1,Length[seq]}];

LCMConvTransform[seqOne_List,seqTwo_List]:=Table[Sum[LCM[seqOne[[k]], seqTwo[[i - k + 1]]], {k, 1, i}],
		{i,1,Min@@(Length/@{seqOne,seqTwo})}];

GCDConvTransform[___List,{},___List]:={};
GCDConvTransform[seq_List]:=Table[Sum[GCD[seq[[k]], seq[[i - k + 1]]], {k, 1, i}],{i,1,Length[seq]}];

GCDConvTransform[seqOne_List,seqTwo_List]:=Table[Sum[GCD[seqOne[[k]], seqTwo[[i - k + 1]]], {k, 1, i}],{i,1,Min@@(Length/@{seqOne,seqTwo})}];



(* Generating Function Transforms
(Cameron, Revert, RevertExp, Exp,  Log) *)



CameronTransform[{}]:={};
CameronTransform[seq_List]:=Module[{var},Rest[SeriesToList[1/(1 - var*ListToSeries[seq, var, "ogf"]),"ogf"]]];
CameronInvTransform[{}]:={};
CameronInvTransform[seq_List]:=Module[{var},Rest[SeriesToList[-1/(1 + var*ListToSeries[seq, var, "ogf"]),"ogf"]]];

RevertTransform[{}]:={};
RevertTransform[seq_List]:=Module[{var,l=Length[seq]},If[seq\[LeftDoubleBracket]1\[RightDoubleBracket]=!=1,{},Rest[SeriesToList[InverseSeries[ListToSeries[seq,var,"ogf"]],"ogf"] Array[(-1)^#1 &,l]]]]

RevertExpTransform[{}]:={};
RevertExpTransform[seq_List]:=Module[{var,l=Length[seq]},If[seq\[LeftDoubleBracket]1\[RightDoubleBracket]=!=1,{},Rest[SeriesToList[InverseSeries[ListToSeries[seq,var,"egf"]],"ogf"] Array[(-1)^#1 (#1-1)!&,l]]]];

ExpTransform[{}]:={};
ExpTransform[seq_List]:=Module[{var},Rest[SeriesToList[Exp[ListToSeries[Prepend[seq,0],var,"egf"]],"egf"]]]

LogTransform[{}]:={};
LogTransform[seq_List]:=Module[{var},Rest[SeriesToList[Log[ListToSeries[Prepend[seq,1],var,"egf"]],"egf"]]]



(* Convolution Transforms *)





ConvTransform[___List,{},___List]:={};
ConvTransform[seq_List]:=Table[Sum[seq[[k]]*seq[[i - k + 1]], {k, 1, i}], {i, 1, Length[seq]}];
ConvTransform[seqOne_List,seqTwo_List]:=Table[Sum[seqOne[[k]]*seqTwo[[i - k + 1]], {k, 1, i}], 
  {i, 1, Min @@ Length /@ {seqOne, seqTwo}}];

ConvInvTransform[seq_List]:=If[First[seq]=!=0,
Module[{a,aaseq=Table[a[i],{i,Length[seq]}]},
aaseq/.Last[Solve[ConvTransform[aaseq]==seq,aaseq]]]]

ExpConvTransform[___List,{},___List]:={};
ExpConvTransform[seq_List]:=Module[{var},SeriesToList[ListToSeries[seq, var, "egf"]^2,"egf"]]

ExpConvTransform[seqOne_List,seqTwo_List]:=
	Module[{var,tmplen=Min@@(Length/@{seqOne,seqTwo})},
		SeriesToList[Times@@(ListToSeries[Take[#1,tmplen],var,"egf"]&)/@{seqOne,seqTwo},"egf"]
	]

LogConvTransform[{}]:={};
LogConvTransform[seq_List]:=Module[{var},SeriesToList[ListToSeries[seq, var, "lap"]^2,"ogf"]]
LogConvTransform[seqOne_List,seqTwo_List]:=
	Module[{var,tmplen=Min[Length/@{seqOne,seqTwo}]},
		SeriesToList[Times@@(ListToSeries[Take[#1,tmplen],var,"lap"]&)/@{seqOne,seqTwo},"ogf"]
	]



(* Binary and other bases related Transforms *)





ANDConvTransform[{}]={};
ANDConvTransform[seq_List]:=Table[Sum[NumAND[seq[[k + 1]], seq[[i - k + 1]]], {k, 0, i}],{i,0,Length[seq]-1}]
ORConvTransform[{}]={};
ORConvTransform[seq_List]:=Table[Sum[NumOR[seq[[k + 1]], seq[[i - k + 1]]], {k, 0, i}],{i,0,Length[seq]-1}]
Off[General::spell1]  

XORConvTransform[{}]={};
XORConvTransform[seq_List]:=Table[Sum[NumXOR[seq[[k + 1]], seq[[i - k + 1]]], {k, 0, i}], {i, 0, Length[seq] - 1}]

On[General::spell1]   

DigitSumTransform[{},___]:={};
DigitSumTransform[seq_List,b_Integer:10]:=
	DigitSum[#,b]&/@seq;
DigitRevTransform[{},___]:={};
DigitRevTransform[seq_List,b_Integer:10]:=DigitRev[#,b]&/@seq;



(* Boustrophedon Transforms *)



$EISMaxTableWidth=60;

BoustrophedonBisTransformTable[{}]={};
BoustrophedonBisTransformTable[seq_List,way_Integer:1]:=Module[{n=Min[Length[seq],$EISMaxTableWidth],tritab},tritab=Transpose[{Take[seq,n]}];Table[tritab\[LeftDoubleBracket]i\[RightDoubleBracket]=Nest[Append[#1,#1\[LeftDoubleBracket]-1\[RightDoubleBracket]+way tritab\[LeftDoubleBracket]i-1,i-Length[#1]\[RightDoubleBracket]]&,tritab\[LeftDoubleBracket]i\[RightDoubleBracket],i-1],{i,1,n}]]

BoustrophedonTransformTable[seq_List]:=BoustrophedonBisTransformTable[Prepend[seq,1]]

BoustrophedonTransform[seq_List]:=Last/@BoustrophedonTransformTable[seq]

BoustrophedonBisTransform[seq_List]:=Last/@BoustrophedonBisTransformTable[seq]

BoustrophedonBisInvTransform[seq_List]:=Last/@BoustrophedonBisTransformTable[seq,-1]

BoustrophedonInvTransform[seq_List]:=Last/@Rest[BoustrophedonBisTransformTable[seq,-1]]



(* Partition and related Transforms *)





PartitionTransform[{},___]:={};
PartitionTransform[seq_List,n_Integer:-1]:=
Module[{var,valueset,lastindex,lastvalue},
valueset=Union[seq];
{lastindex,lastvalue}=If[n==-1,
	{Length[valueset],Max@@valueset},
	{First[Flatten[Position[Union[Append[valueset,n]],n]]],n}];
Rest[SeriesToList[Series[
	Product[1/(1-var^valueset[[i]]),{i,1,lastindex}],
	{var,0,lastvalue}],"ogf"]]];
		
PartitionInvTransform[{}]:={};
PartitionInvTransform[seq_List]:=
Flatten[MapIndexed[ Table[#2[[1]],{#1}]&, 	EulerInvTransform[seq]]]




(* Other Transforms (weigh, weighbisout, eulerbis) *)



WeighTransform[{},___]:={};
WeighTransform[seq_List,n_Integer:-1,gftype_String:"ogf"]:=Module[{var,lastvalue},lastvalue=If[n==-1,Max@@seq,n];Rest[SeriesToList[Series[Product[1 - var^seq[[i]], {i, 1, Length[seq]}], {var, 0, lastvalue + 1}],gftype]]]

WeighBisOutTransform[{}]={};
WeighBisOutTransform[seq_List]:=Module[{var},SeriesToList[Series[Product[(var^(-k) + 1 + var^k)^seq[[k]], {k, 1, Length[seq]}], 
  {var, 0, Length[seq] + 1}],"ogf"]];

EulerBisTransform[{}]={};
EulerBisTransform[seq_List]:=Module[{var},SeriesToList[Series[Sum[seq[[k]]*(var/(1 + var))^k, {k, 1, Length[seq]}]/(1 + var), 
  {var, 0, Length[seq]}],"ogf"]];



(* To be made: weighout, weighouti, etrans, ptrans, pairtrans *)



(* Function Management *)

RegularTransList = 
   {"Binomial", "Euler", "Mobius", "Stirling", "Cameron", "Boustrophedon", 
    "BoustrophedonBis", "Partition"}; 
((InverseFunction[Evaluate[ToExpression[StringJoin[#1, "Transform"]]]] ^= 
        ToExpression[StringJoin[#1, "InvTransform"]]; 
       InverseFunction[Evaluate[ToExpression[StringJoin[#1, "InvTransform"]]]] ^= 
        ToExpression[StringJoin[#1, "Transform"]]; ) & ) /@ RegularTransList; 

(* Superseeker Transforms list *)

(* Transforms 095,096,097,098 are currently not implemented and
return {} *)

(* Transform 008 must be executed before any other transform needing
the exp gen fun *)

EISTransTable = { Clear[vexpseq];
Trans[001]= Identity,
Trans[002]= CompSequence,
Trans[003]= GCDNormalize,
Trans[004]= (Rest[#]// Trans[003] )&,
Trans[005]= (Drop[#,2]// Trans[004]) &,
Trans[006]= Bisect[#,0]&,
Trans[007]= Bisect[#,1]&,
Trans[008]= (vexpseq = #/Array[#1!&,Length[#],0])&,
Trans[009]= #*Range[Length[#]]&,
Trans[010]= #/Array[#1!&,Length[#],1]&,
Trans[011]= 2#&,
Trans[012]= 3#&,
Trans[013]= GFProdaaaTransform,
Trans[014]= GFProdbbbTransform,
Trans[015]= GFProdcccTransform,
Trans[016]= GFProddddTransform,
Trans[017]= GFProdeeeTransform,
Trans[018]= GetDiff,
Trans[019]= GetDiff[#,2]&,
Trans[020]= GetDiff[#,3]&,
Trans[021]= GFProdfffTransform,
Trans[022]= GFProdgggTransform,
Trans[023]= GFProdhhhTransform,
Trans[024]= GetSum,
Trans[025]= GetOffsetSum[#,2]&,
Trans[026]= GFProdiiiTransform,
Trans[027]= GFProdjjjTransform,
Trans[028]= GetIntervalSum[#,2]&,
Trans[029]= GFProdkkkTransform,
Trans[030]= GetOffsetDiff[#,2]&,
Trans[031]= GFProdlllTransform,
Trans[032]= Take[#,{3,-1}]- Take[#,{2,-2}]-Take[#,{1,-3}]&,
Trans[033]= GFProdmmmTransform,
Trans[034]= # + Range[Length[#]]&,
Trans[035]= # +2&,
Trans[036]= # +3&,
Trans[037]= # - Range[Length[#]]&,
Trans[038]= # -2&,
Trans[039]= # -3&,
Trans[040]= # +1&,
Trans[041]= # -1&,
Trans[042]= GFProdnnnTransform,
Trans[043]= Take[#,{3,-1}]- Take[#,{2,-2}]+Take[#,{1,-3}]&,
Trans[044]= GFProdoooTransform,
Trans[045]= Take[#,{1,-3}]+ Take[#,{2,-2}]-Take[#,{3,-1}]&,
Trans[046]= GetSum[#,2]&,
Trans[047]= GetSum[#,3]&,
Trans[048]= GFProdpppTransform,
Trans[049]= GFProdqqqTransform,
Trans[050]= GFDiagaaaTransform,
Trans[051]= GFDiagbbbTransform,
Trans[052]= GFDiagcccTransform,
Trans[053]= GFDiagdddTransform,
Trans[054]= GFProdaaaTransform[#,"egf"]&,
Trans[055]= GFProdbbbTransform[#,"egf"]&,
Trans[056]= GFProdcccTransform[#,"egf"]&,
Trans[057]= GFProddddTransform[#,"egf"]&,
Trans[058]= GFProdeeeTransform[#,"egf"]&,
Trans[059]= GetDiff[vexpseq]&,
Trans[060]= GetDiff[vexpseq,2]&,
Trans[061]= GetDiff[vexpseq,3]&,
Trans[062]= GFProdfffTransform[#,"egf"]&,
Trans[063]= GFProdgggTransform[#,"egf"]&,
Trans[064]= GFProdhhhTransform[#,"egf"]&,
Trans[065]= GetSum[vexpseq]&,
Trans[066]= GetOffsetSum[vexpseq,2]&,
Trans[067]= GFProdiiiTransform[#,"egf"]&,
Trans[068]= GFProdjjjTransform[#,"egf"]&,
Trans[069]= GetIntervalSum[vexpseq,2]&,
Trans[070]= GFProdkkkTransform[#,"egf"]&,
Trans[071]= GetOffsetDiff[vexpseq,2]&,
Trans[072]= GFProdlllTransform[#,"egf"]&,
Trans[073]=(Take[#,{3,-1}]- Take[#,{2,-2}]-Take[#,{1,-3}]&[vexpseq])&,
Trans[074]= GFProdmmmTransform[#,"egf"]&,
Trans[075]= vexpseq+Range[Length[vexpseq]]&,
Trans[076]= vexpseq +2&,
Trans[077]= vexpseq +3&,
Trans[078]= vexpseq-Range[Length[vexpseq]]&,
Trans[079]= vexpseq -2&,
Trans[080]= vexpseq -3&,
Trans[081]= #+Table[j!,{j,Length[#]}]&,
Trans[082]= #-Table[j!,{j,Length[#]}]&,
Trans[083]= GFProdnnnTransform[#,"egf"]&,
Trans[084]= (Take[#,{3,-1}]- Take[#,{2,-2}]+Take[#,{1,-3}]&[vexpseq])&,
Trans[085]= GFProdoooTransform[#,"egf"]&,
Trans[086]= (Take[#,{1,-3}]+ Take[#,{2,-2}]-Take[#,{3,-1}]&[vexpseq])&,
Trans[087]= GetSum[vexpseq,2]&,
Trans[088]= GetSum[vexpseq,3]&,
Trans[089]= GFProdpppTransform[#,"egf"]&,
Trans[090]= GFProdqqqTransform[#,"egf"]&,
Trans[091]= GFDiagaaaTransform[#,"egf"]&,
Trans[092]= GFDiagbbbTransform[#,"egf"]&,
Trans[093]= GFDiagcccTransform[#,"egf"]&,
Trans[094]= GFDiagdddTransform[#,"egf"]&,
Trans[095]={}& (* WeighTransform and alii: not implemented yet *),
Trans[096]={}&,
Trans[097]={}& (* WeighTransform[vexpseq]& *),
Trans[098]={}&,
Trans[099]= Monotonous,
Trans[100]= BinomialTransform,
Trans[101]= BinomialInvTransform,
Trans[102]= BoustrophedonTransform,
Trans[103]= BoustrophedonInvTransform,
Trans[104]= EulerTransform,
Trans[105]= EulerInvTransform,
Trans[106]= ExpTransform,
Trans[107]= ExpConvTransform,
Trans[108]= CameronTransform,
Trans[109]= CameronInvTransform,
Trans[110]= LogTransform,
Trans[111]= MobiusTransform,
Trans[112]= MobiusInvTransform,
Trans[113]= MulTwoTransform,
Trans[114]= StirlingTransform,
Trans[115]= StirlingInvTransform
};
