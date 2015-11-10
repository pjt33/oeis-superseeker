(*Erraten von Zahlenfolgen*)

(*Rationale Interpolation*)

(* prim-ed Oct 30 2007 *)

RationalInterpolation::usage = 
"RationalInterpolation[func, {x, m, k}, {x1, x2, ..., xmk1}, (opts)] gives the
rational interpolant to func (a function of the variable x), where m and
k are the degrees of the numerator and denominator, respectively, and
{x1, x2, ..., xmk1} is a list of m+k+1 abscissas of the interpolation points."

RationalInterpolation[func_, {x_, m_Integer, k_Integer}, xlist_, opts___] :=
    Module[{xinfo, bias, answer, biasOK = True},
	answer /;
		((xinfo = {x, m, k};
		bias = xlist);
	    (answer = RI[func, xinfo, bias]; answer =!= Fail))
    ];

RI[f_, xinfo_, bias_] :=
    Module[{i, mk1, xx, fx, mat, tempvec, x, x0, x1, m, k},
	x = xinfo[[1]];
		(m = xinfo[[2]];
		k = xinfo[[3]];
		mk1 = m+k+1;
		xx = bias);
	fx = Table[f /. x->xx[[i]],{i,Length[xx]}];
	mat = Table[1,{i,mk1+1}];
	tempvec = Table[1,{i,mk1}];
	mat[[1]] = tempvec;
	mat[[m+2]] = -tempvec*fx;
	Do[tempvec *= xx;
		If[i <= m,mat[[i+1]] = tempvec,Null,Null];
		If[i <= k,mat[[i+m+2]] = -tempvec*fx],{i,Max[m,k]}];
If[$VersionNumber>=2.,$Messages=OutputStream["",1],$Messages={}];
	xx = Solve[Transpose[mat].Table[x[i],{i,1,m+k+2}]==Table[0,{i,1,m+k+1}],
                        Table[x[i],{i,1,m+k+2}]];
If[$VersionNumber>=2.,$Messages=OutputStream["stdout",1],$Messages={"stdout"}];
	xx = Table[x[i],{i,1,m+k+2}] /. xx[[1]];
	If[Head[xx] === Solve, Return[Fail]];
	Factor[(xx[[1]]+Sum[xx[[i+1]] x^i,{i,m}])/(Sum[xx[[i+m+1]] x^(i-1),{i,k+1}])]
    ];



RateFolge[x_List,t_]:=Module[{X=x,funk,var,L1,L2,ii,Erg},
   L1=Length[X];
   Do[funk[var]=X[[var]],{var,L1}];
   Erg={};
   For[L2=0,L2<=L1-2,L2++,
      X=RationalInterpolation[funk[var],{var,L1-L2-2,L2},Table[ii,{ii,L1-1}]];
      If[Factor[Denominator[X]/.var->L1]=!=0&&Factor[(X/.var->L1)-funk[L1]]===0,Erg=Union[{X/.var->t},Erg]]
      ];
   Erg
   ]


Rate[x___]:=Module[{X={x},L,Zaehler,Folge,var,ii,Erg={},i},
   i[0]=i0;i[1]=i1;i[2]=i2;i[3]=i3;i[4]=i4;i[5]=i5;i[6]=i6;i[7]=i7;i[8]=i8;
   i[9]=i9;i[10]=i10;i[11]=i11;i[12]=i12;i[13]=i13;i[14]=i14;i[15]=i15;
   i[16]=i16;i[17]=i17;i[18]=i18;i[19]=i19;i[20]=i20;
   L=Length[X];
   Folge=Table[0,{L-1}];
   For[Zaehler=1,Zaehler<=L-1,Zaehler++,
      Folge[[Zaehler]]=X;
      X=Table[X[[ii+1]]/X[[ii]],{ii,L-Zaehler}];
      ];
   For[Zaehler=1,Zaehler<=L-1,Zaehler++,
      X=RateFolge[Folge[[Zaehler]],i[Zaehler-1]];
      If[X=!={},
         Do[X=Table[Folge[[Zaehler-ii,1]]*
                Product[X[[var]],
                    Release[{i[Zaehler-ii],1,i[Zaehler-ii-1]-1}]],
                {var,Length[X]}],
            {ii,Zaehler-1}
           ];
        ];
      Erg=Union[Erg,X]
      ];
   Erg
   ]
Ratepol[x___]:=Module[{X={x},funk,var,L1,L2,ii,Erg},
   L1=Length[X];
   Do[funk[var]=X[[var]],{var,L1}];
   Erg={};
   X=RationalInterpolation[funk[var],{var,L1-2,0},Table[ii,{ii,L1-1}]];
   If[Factor[(X/.var->L1)-funk[L1]]===0,Erg=Union[{X/.var->i0},Erg]];
   Erg
   ]
Rateint[x___]:=Module[{X={x},L,Zaehler,Folge,var,ii,Erg={},i},
   i[0]=i0;
   L=Length[X];
   Folge=Table[0,{L-1}];
   For[Zaehler=1,Zaehler<=Min[1,L-1],Zaehler++,
      Folge[[Zaehler]]=X;
      X=Table[X[[ii+1]]/X[[ii]],{ii,L-Zaehler}];
      ];
   For[Zaehler=1,Zaehler<=Min[1,L-1],Zaehler++,
      X=RateFolge[Folge[[Zaehler]],i[Zaehler-1]];
      If[X=!={},
         Do[X=Table[Folge[[Zaehler-ii,1]]*
                Product[X[[var]],
                    Release[{i[Zaehler-ii],1,i[Zaehler-ii-1]-1}]],
                {var,Length[X]}],
            {ii,Zaehler-1}
           ];
        ];
      Erg=Union[Erg,X]
      ];
   Erg
   ]
Ratekurz[x___]:=Module[{X={x},L,Zaehler,Folge,var,ii,Erg={},i},
   i[0]=i0;i[1]=i1;i[2]=i2;
   L=Length[X];
   Folge=Table[0,{L-1}];
   For[Zaehler=1,Zaehler<=Min[3,L-1],Zaehler++,
      Folge[[Zaehler]]=X;
      X=Table[X[[ii+1]]/X[[ii]],{ii,L-Zaehler}];
      ];
   For[Zaehler=1,Zaehler<=Min[3,L-1],Zaehler++,
      X=RateFolge[Folge[[Zaehler]],i[Zaehler-1]];
      If[X=!={},
         Do[X=Table[Folge[[Zaehler-ii,1]]*
                Product[X[[var]],
                    Release[{i[Zaehler-ii],1,i[Zaehler-ii-1]-1}]],
                {var,Length[X]}],
            {ii,Zaehler-1}
           ];
        ];
      Erg=Union[Erg,X]
      ];
   Erg
   ]
Rateeins[x___]:=Module[{X={x},L,Zaehler,Folge,var,ii,Erg={},i},
   i[0]=i0;i[1]=i1;i[2]=i2;i[3]=i3;i[4]=i4;i[5]=i5;i[6]=i6;i[7]=i7;i[8]=i8;
   i[9]=i9;i[10]=i10;i[11]=i11;i[12]=i12;i[13]=i13;i[14]=i14;i[15]=i16;i[17]=i17;
   i[18]=i18;i[19]=i19;i[20]=i20;
   L=Length[X];
   Folge=Table[0,{L-1}];
   For[Zaehler=1,Zaehler<=Min[3,L-1],Zaehler++,
      Folge[[Zaehler]]=X;
      X=Table[X[[ii+1]]/X[[ii]],{ii,L-Zaehler}];
      ];
   For[Zaehler=1,Zaehler<=Min[3,L-1],Zaehler++,
      X=RateFolge[Folge[[Zaehler]],i[Zaehler-1]];
      If[X=!={},
         Do[X=Table[Folge[[Zaehler-ii,1]]*
                Product[X[[var]],
                    Release[{i[Zaehler-ii],1,i[Zaehler-ii-1]-1}]],
                {var,Length[X]}],
            {ii,Zaehler-1}
           ];
        ];
      If[X=!={},Return[X]];
      ];
   Erg
   ]
