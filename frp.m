(* a Mathematica package for generating full rook placements of a Ferrers board (shape), checking the number of sigma-avoiding full rook placements of a Ferrers board, generating 0-1-fillings of a Ferrers board, and checking the number of 0-1-fillings of a Ferrers board with weight n such that the longest ne-chain has length u and the longest se-chain has length v.
*)

BeginPackage["frp`"] 

`ConjugatePartition::usage = "ConjugatePartition[fb] gives the conjugate partition of fb in the list form, where fb is an integer partition in 
the reverse sorted list form";

`fbTableForm::usage = "fbTableForm[fb] gives the Ferrers board of fb in the table form, where fb is a Ferrers board 
in the reverse sorted list form";

`rpGenerator::usage = "rpGenerator[fb] gives a list of matrices, which reprents all the rook placements of fb, where fb 
is a Ferrers board in the reverse sorted list form";

`frpTest::usage = "frpTest[fb, rp] gives 1 if rp is a full rook placement of fb, otherwise 0. 
Here, fb is a Ferrrers board in the reverse sorted list form and rp is a rook placement of fb in the matrix form";

`frpTableForm::usage = "frpTableForm[fb, frp] gives the table form of frp, where frp is a full rook placement of fb in the matrix form, 
and fb is a Ferrers board in the reverse sorted list form";

`frpGenerator::usage = "frpGenerator[fb] gives a list of tables, which represents all the full rook placements of fb in the table form. 
Here, fb is a Ferrers board in the reverse sorted list form";

`SigmaMatrix::usage = "SigmaMatrix[sigma] gives the matrix of a given permutation sigma, where sigma is a list which represents a permutation";

`fbSubSquare::usage = "fbSubSquare[fb, n] gives a list of lists for indices of sub square matrices of fp of dimension n, where 
fb is a Ferrers board in the reverse sorted list form";

`SigmaAvoidingTest::usage = "SigmaAvoidingTest[frp, sigma, fbsq] gives 1 if frp is sigma-avoiding, otherwise 0. 
Here, frp is a full rook placement of some Ferrers board fb, the list sigma is a permutation, 
fbsq is a list of lists for indices of sub square marices of fp of dimension Length[sigma]";

`SigmaAvoidingNumber::usage = "SigmaAvoidingNumber[fb, sigma] gives the number of frps of fb which are sigma-avoiding. 
Here, fb is a Ferrers board in the reverse sorted list form, and sigma is a permutation";

`SigmaAvoidingfrp::usage = "SigmaAvoidingfrp[fb, sigma] gives a list of frps of fb which are sigma-avoiding. 
Here, fb is a Ferrers board in the reverse sorted list form, and sigma is a permutation";

`ZeroOneFilling::usage = "ZeroOneFilling[fb, n] gives a list of 0-1-fillings of fb with weight n. 
Here, fb is a Ferrers board in the reverse sorted list form, and n is a positive integer";

`zofGenerator::usage = "zofGenerator[fb, per] gives a list of 0-1-fillings of fb which fits per. 
Here, fb is a Ferrers board in the reverse sorted list form, and per is a list of positive integers such that per <= fb";

`TestNE::usage = "TestNE[zof, u, subuu, subu] gives 1 if the longest ne-chain of zof has length u, otherwise 0. 
Here, zof is a 0-1-filling of some Ferrrers board, u is a positive integer, subuu is a list of indices of submatrices of 
zof of dimension (u + 1), subu is a list of indices of submatrices of zof of dimension u";

`TestSE::usage = "TestNE[zof, v, subvv, subv] gives 1 if the longest se-chain of zof has length v, otherwise 0. 
Here, zof is a 0-1-filling of some Ferrrers board, v is a positive integer, subvv is a list of indices of submatrices of 
zof of dimension (v + 1), subv is a list of indices of submatrices of zof of dimension v";

`NumberNESE::usage = "NumberNESE[fb, n, u, v] gives the number of 0-1-fillings of fb with weight n such that the longest 
ne-chain has length u and the longest se-chian has length v. Here, fb is a Ferrers board in the reverse sorted list form, 
n, u and v are positive integers";

`frpNESE::usage = "frpNESE[fb, u, v] gives the number of full rook placements of fb such that the longest 
ne-chain has length u and the longest se-chian has length v. Here, fb is a Ferrers board in the reverse sorted list form, 
u and v are positive integers"; 

`frpNESElight::usage = "frpNESElight[fb, frp, u, v] gives the number of full rook placements frp of fb such that the longest 
ne-chain has length u and the longest se-chian has length v. Here, fb is a Ferrers board in the reverse sorted list form, 
u and v are positive integers";

`ParGenerator::usage = "ParGenerator[fb] gives a list of partitions of fb[[1]] into Length[fb] parts with par[[i]] <= fb[[i]], 
where fb is a Ferrers board in the reverse sorted list form";

`frpParTest::usage = "frpParTest[fb, frp, par] gives 1 if for each 1 <= i <= Length[fb], the sum of i-th row of frp is equal to par[[n + 1 - i]]. 
Here, fb is a Ferrers board in the reverse sorted list form, frp is a full rook placement of fb and par is a partition of fb[[1]] into Length[fb] 
with par[[i]] <= fb[[i]]. Otherwise, it returns 0";

`frpParNESE::usage = "frpParNESE[fb, par, u, v] gives the number of full rook placements of fb, whose sum of  the i-th row is par[[Length[fb] + 1 - i]]  
and the longest ne-chain has length u and the longest se-chian has length v. 
Here, fb is a Ferrers board in the reverse sorted list form, par is a partition of fb[[1]] into Length[fb] 
with par[[i]] <= fb[[i]], u and v are two positive integers";

Begin["Private`"]

(* Name:    ConjugatePartition
   Input:   l - a list of positive integers, which represents a Ferrers board
   Output:  c - a list of positive integers, which represents the conjugate partition of l 
   This code is borrowed from http://mathworld.wolfram.com/ConjugatePartition.html
 *)

ConjugatePartition[l_List] := 
  Module[{i, r = Reverse[l], n = Length[l]},
         Table[n + 1 - Position[r, _?(# >= i&), Infinity, 1][[1, 1]], {i, l[[1]]}]
	 ];

(* Name: fbTableForm
   Input: fb - a list of positive integers, which represents a Ferrers board
   Output: tfb - a Ferrers board in table form  
 *)

fbTableForm[fb_] :=
  Module[{n = Length[fb], tfb = {}},
         Do[tfb = Append[tfb, Table[0, {j, fb[[n - i + 1]]}]], {i, n}]; (* translate fb into table form as that in Ting note *)
         Return[tfb];
         ];

(* Name:   rpGenerator
   Input:  fb - a list of positive integers, which represents a Ferrers board
   Output: rp - a list of matrices, which are matrices whose each column 
           contains precisely one unit (rook) and dimension are compatible with fb 
           We may call rp a rook placement of fb
 *)

rpGenerator[fb_] := 
  Module[{cfb = ConjugatePartition[fb], l = {}}, 
	 Do[l = Append[l, Join[Table[0, {j, cfb[[1]] - cfb[[i]]}], #]& /@ IdentityMatrix[cfb[[i]]]]; , {i, fb[[1]]}]; 
	 (*this is to compute the set of unit vectors for each column  *)              
       Return[Transpose /@ Tuples[l]]; (* construct a list of matrices which represents rook placements of fb and return the results *)
      ];

(* Name:  frpTest
   Input: fb - a list of positive integers, which represents a Ferrers board
          rp - a matrix, which represents a rook placement of fb 
   Output: 1 - rp is a full rook placement of fb
           0 - rp is not a full rook placement of fb
 *)

 frpTest[fb_, rp_] := 
  Module[{i, n = Length[fb]},
	 For[i = 1, i <= n, i++, 
             If[Sum[rp[[i, j]], {j, fb[[1]]}] == 0 , Return[0]]; (* check whether the sum of each row is equal to 0 or not. 
if it is, then rp is not a full rook placement *)
	     ];
         Return[1];
	 ];

(* Name: frpTableForm 
   Input: fb - a list of positive integers, which represents a Ferrers board
          frp - a matrix, which reprensts a full rook placement of fb
   Output: tfrp - a table form of frp
 *)

frpTableForm[fb_, frp_] := 
  Module[{n = Length[fb], tfrp = {}},
         Do[tfrp = Append[tfrp, Table[frp[[i, j]], {j, fb[[n - i + 1]]}] ], {i, n}];
         Return[tfrp];
        ] 

(* Name:   frpGenerator
   Input:  fb - a list of positive integers, which represents a Ferrers board
   Output: frp - a list of tables, which represent full rook placements of fb
 *)

frpGenerator[fb_] := 
  Module[{rp = rpGenerator[fb], frp = {}, test},
         test = frpTest[fb, #]& /@ rp; (*do frpTest for each element of rp *)
         Do[If[test[[i]] == 1, frp = Append[frp, frpTableForm[fb, rp[[i]]]]], {i, Length[rp]}]; 
         (* if rp[[i]] is a full rook placement, then put it into frp with Table form *)
	 Return[frp];      
	];

(* Name: SigmaMatrix
   Input: sigma - a given permuation
   Output: m - a matrix which is corresponding to sigma
 *)

SigmaMatrix[sigma_] := 
  Module[{n = Length[sigma], i},
         m = Table[0, {k, n}, {j, n}]; (* construct a square zero matrix with dimension n *)
         Do[m[[n - sigma[[i]] + 1 ,i]] = 1, {i, n}];  (* evaluate m arrording to sigma *)
         Return[m];
	 ];

(* Name: fbSubSquare
   Input: fb - a list of positive integers, which represents a Ferrers board
          n - the dimension of the sub square matrix of frp
   Output: fbsq - a set of indices for sub square matrices of fp of dimension n
 *)

fbSubSquare[fb_, n_] := 
  Module[{tfb = fbTableForm[fb], tup},
	tup = Tuples[{Subsets[Range[Length[fb]], {n}], Subsets[Range[fb[[1]]], {n}]}];
        (* generate all the set of indices of sub square matrices of  a Length[fb] times fb[[1]] of dimension n *)
	 Return[Cases[Quiet[Check[tfb[[##]]; {##}, "error"]]& @@@ tup, _List]];
        (* pick all the the set of indices of sub square matrices of fb of dimension n *)
       ];

(* Name:  SigmaAvoidingTest
   Input: frp -  a given table, which represents a full rook placements of fb
          sigma - a given permutation
          fbsq - a set of indices for sub square matrices of frp of dimension Length[sigma]
   Output: 1 - frp is sigma-avoiding
           0 - frp is not sigma-avoiding
*)

 SigmaAvoidingTest[frp_, sigma_, fbsq_] :=
  Module[{n = Length[sigma], m, zero, l, i}, 
         m = SigmaMatrix[sigma]; (* generate the matrix for sigma *)
	 zero = Table[0, {i, n}, {j, n}]; (* generate the zero matrix of dimension n*)
         l = Length[fbsq];
         For[i = 1, i <= l, i++,
             If[Expand[frp[[fbsq[[i, 1]], fbsq[[i, 2]]]] - m] == zero, Return[0]]; 
             ];
         (* for each submatrix of frp, check whether it is equal to m or not *)
         Return[1];
	 ];

(* Name: SigmaAvoidingNumber
   Input: fb - a list of positive integers, which represents a Ferrers board 
          sigma - a given permutation
   Output: n - the number of sigma-avoiding matrices among frps of fb
 *)

SigmaAvoidingNumber[fb_, sigma_] := 
  Module[{n = Length[sigma], frp, fbsq}, 
	 frp = frpGenerator[fb]; (* generate frps of fb in table form *)
	 fbsq = fbSubSquare[fb, n]; (* generate sub square matrices of fb of dimension n *) 
	 Return[Sum[SigmaAvoidingTest[frp[[i]], sigma, fbsq] , {i, Length[frp]}]]; (*count the number of sigma-avoiding frps of fb *)
	  ];

(* Name: SigmaAvoidingfrp
   Input: fb - a list of positive integers, which represents a Ferrers board 
          sigma - a given permutation
   Output: frp - a list of frps of fb which are sigma-avoiding
*)

SigmaAvoidingfrp[fb_, sigma_] :=
  Module[{n = Length[sigma], frp, fbsq, sfrp = {}}, 
	 frp = frpGenerator[fb]; (* generate frps of fb in table form *)
	 fbsq = fbSubSquare[fb, n]; (* generate sub square matrices of fb of dimension n *)
         Do[If[SigmaAvoidingTest[frp[[i]], sigma, fbsq] == 1, sfrp = Append[sfrp, frp[[i]]]] , {i, Length[frp]}]; 
	 Return[sfrp]; 
	 ];

(* Name: zofGenerator
   Input: fb - a list of positive integers, which represents a Ferrers board 
          per - a list of positive integers, which has the same length as fb, 
                and each element of per is smaller than or equal to that of fb    
   Output:zof - a list of 0-1-fillings of fb, where the sum of 1 in each row of 
                each element zof is equal to that of per
*)

zofGenerator[fb_, per_] :=
  Module[{n = Length[fb], tup, cfb, m, tfb, flag, zof = {}},
	 tup = Tuples[Table[Subsets[Range[fb[[n + 1 - i]]], {per[[n + 1 - i]]}], {i, n}]]; (* generate all the indices for which we fill it with 1 *)
         cfb = ConjugatePartition[fb];
         m = Length[cfb];
         Do[tfb = fbTableForm[fb]; 
            Do[Do[tfb[[j, tup[[i, j, k]]]] = 1 , {k, per[[n + 1 - j]]}], {j, n}]; (*generate a filling of tfb with 1 *)
               flag = 1;
	       Do[If[Sum[tfb[[n + 1 - k, j]], {k, cfb[[j]]}] == 0, flag = 0; Break[]], {j, m}]; 
               (* check the sum of each column is equal to zero or not *)
               If[flag == 1, zof = Append[zof, tfb]]; (* if tfb is a 0-1-filling, then we add it to zof *)
           , {i, Length[tup]}];
	 Return[zof];
        ];

(* 
   Name: ZeroOneFilling
   Input: fb - a list of positive integers, which represents a Ferrers board 
          n  - a positive integer
   Output: zof - a list of 0-1-fillings of fb with weight n   
*)

ZeroOneFilling[fb_, n_] :=
  Module[{l = Length[fb], par, per, flag, zof = {}},
         par = IntegerPartitions[n, {l}]; (* generate all the partitions of n into l parts *)
         Do[per = Permutations[par[[i]]]; (* generate all the permutations of par[[i]] *) 
            Do[flag = 1; 
	       Do[If[per[[j, k]] > fb[[k]], flag = 0; Break[]], {k, l}];
               If[flag == 0, Break[]]; (* if flag == 0, then per[[j]] does not fit the size of fb *)
               zof = Join[zof, zofGenerator[fb, per[[j]]]]; (* select zof which fits fb and per[[j]] *)     
            , {j, Length[per]}];
	  , {i, Length[par]}];
	  Return[zof];
	 ];

(* 
   Name: TestNE
   Input: zof - a 0-1-filling of some Ferrers board
          u - a positive integer
          subuu - a list of indices of submatrices of zof of size u + 1
          subu - a list of indices of submatrices of zoef of size u
   Output: 1 - the longest ne-chain of zof has length u
           0 - otherwise 
 *)

TestNE[zof_, u_, subuu_, subu_] :=
  Module[{n = Length[subuu], m = Length[subu], i, mat},
         If[m == 0, Return[0]];
         If[n > 0, 
            For[i = 1, i <= n, i++, 
	        mat = zof[[subuu[[i]][[1]], subuu[[i]][[2]]]]; (* take the submatrix of zof with indices subuu[[i]] *)
	        If[Sum[mat[[j, u + 2 - j]], {j, u + 1}] == u + 1, Return[0]];
	       ];
           ];
	 For[i = 1, i<= m, i++,
             mat = zof[[subu[[i]][[1]], subu[[i]][[2]]]]; (* take the submatrix of zof with indices subu[[i]] *)
             If[Sum[mat[[j, u + 1 - j]], {j, u}] == u, Return[1]]; 
	     ];
         Return[0];
	 ];

(* Warning: In the Do[ .. ], if we use If[ Return[ .. ] ], then the command Return[ .. ] does not work. 
 In this case, we need to use For[ .. ] rather than Do[ .. ] *)

(* Name: TestSE
   Input: zof - a 0-1-filling
          v - a positive integer
          subvv - a list of indices of submatrices of zof of size v + 1
          subv - a list of indices of submatrices of zoef of size v
   Output: 1 - the longest se-chain of zof has length v
           0 - otherwise 
 *)

TestSE[zof_, v_, subvv_, subv_] :=
  Module[{n = Length[subvv], m = Length[subv], i, mat},
         If[m == 0, Return[0]];
         If[n > 0,
            For[i = 1, i <= n, i++,  
	        mat = zof[[subvv[[i]][[1]], subvv[[i]][[2]]]]; (* take the submatrix of zof with indices subvv[[i]] *)
	        If[Sum[mat[[j, j]], {j, v + 1}] == v + 1, Return[0]];
	       ];
           ];
	 For[i = 1, i <= m, i++,
             mat = zof[[subv[[i]][[1]], subv[[i]][[2]]]]; (* take the submatrix of zof with indices subv[[i]] *)
             If[Sum[mat[[j, j]], {j, v}] == v, Return[1]]; 
	    ];
         Return[0];
	 ];

(* 
   Name: NumberNESE
   Input: fb - a list of positive integers, which represents a Ferrers board 
          n  - a positive integer
          u  - a positive integer
          v - a positive integer
   Output: N - the number of 0-1-fillings of fb with weight n such that the longest 
               ne-chain has length u and the longest se-chian has length v   
 *)


NumberNESE[fb_, n_, u_, v_] := 
  Module[{zof, subuu, subu, subvv, subv, N = 0},
         zof = ZeroOneFilling[fb, n]; (* generate 0-1-fillings of fb with weight n *)
         subuu = fbSubSquare[fb, u + 1]; (* generate the indices of submatrices of fb of dimension u + 1 *)
         subu = fbSubSquare[fb, u];
         subvv = fbSubSquare[fb, v + 1];
         subv = fbSubSquare[fb, v];
         Do[N = N + If[TestNE[zof[[i]], u, subuu, subu] + TestSE[zof[[i]], v, subvv, subv] == 2, 1, 0], {i, Length[zof]}]; 
         (* count 0-1-fillings which satisfy the conditions in the specification *)
	 Return[N];
	 ];

(* 
   Name: frpNESE
   Input: fb - a list of positive integers, which represents a Ferrers board 
          u  - a positive integer 
          v - a positive integer
   Output: N  - the number of full rook placements of fb such that the longest 
               ne-chain has length u and the longest se-chian has length v  
*)

frpNESE[fb_, u_, v_] := 
  Module[{frp, subuu, subu, subvv, subv, N = 0}, 
         frp = frpGenerator[fb]; (* generate full rook placements of fb *)
         subuu = fbSubSquare[fb, u + 1]; (* generate the indices of submatrices of fb of dimension u + 1 *)
         subu = fbSubSquare[fb, u];
         subvv = fbSubSquare[fb, v + 1];
         subv = fbSubSquare[fb, v];
         Do[N = N + If[TestNE[frp[[i]], u, subuu, subu] + TestSE[frp[[i]], v, subvv, subv] == 2, 1, 0], {i, Length[frp]}]; 
         (* count full rook placements which satisfy the conditions in the specification *)
	 Return[N];
	 ];

(* 
   Name: frpNESElight
   Input: fb - a list of positive integers, which represents a Ferrers board
          frp - a list of full rook placements of fb 
          u  - a positive integer 
          v - a positive integer
   Output: N  - the number of full rook placements frp of fb such that the longest 
               ne-chain has length u and the longest se-chian has length v  
*)

frpNESElight[fb_, frp_, u_, v_] := 
  Module[{subuu, subu, subvv, subv, N = 0}, 
         subuu = fbSubSquare[fb, u + 1]; (* generate the indices of submatrices of fb of dimension u + 1 *)
         subu = fbSubSquare[fb, u];
         subvv = fbSubSquare[fb, v + 1];
         subv = fbSubSquare[fb, v];
         Do[N = N + If[TestNE[frp[[i]], u, subuu, subu] + TestSE[frp[[i]], v, subvv, subv] == 2, 1, 0], {i, Length[frp]}]; 
         (* count full rook placements which satisfy the conditions in the specification *)
	 Return[N];
	 ];

(*
  Name: ParGenerator
  Input: fb - a list of positive integers, which represents a Ferrers board
  Output: par - a list of partitions of fb[[1]] into Length[fb] parts with par[[i]] <= fb[[i]]
*)

ParGenerator[fb_] := 
  Module[{n = Length[fb], p, per, flag, par = {}},
         p = IntegerPartitions[fb[[1]], {n}]; (*compute all the integer partitions of fb[[1]] into Length[fb] parts *)
         Do[per = Permutations[p[[i]]]; 
            Do[flag = 1;
               Do[If[per[[j, k]] > fb[[k]], flag = 0; Break[]], {k, n}]; (* check whether per[[j, k]] > fb[[k]] or not *)
               If[flag == 1,  par = Append[par, per[[j]]]];
              , {j, Length[per]}];  
           , {i, Length[p]}];
         Return[par];
	 ];

(* 
   Name: frpParTest
   Input: fb - a list of positive integers, which represents a Ferrers board 
          frp - a full rook placement of fb
          par - a list of partitions of fb[[1]] into Length[fb] parts with par[[i]] <= fb[[i]]
   Output:  1 - for each 1 <= i <= Length[fb], the sum of i-th row of frp is equal to par[[n + 1 - i]]
            0 - Otherwise
 *)

frpParTest[fb_, frp_, par_] := 
  Module[{n = Length[fb], i},
         For[i = 1, i <= n, i++,
             If[Sum[frp[[i, j]], {j, fb[[n + 1 - i]]}] != par[[n + 1 - i]], Return[0]]; 
	     ];
         Return[1];
	];

(*
   Name: frpParNESE
   Input: fb - a list of positive integers, which represents a Ferrers board 
          par - a partition of fb[[1]] into Length[fb] parts with par[[i]] <= fb[[i]]
          u  - a positive integer 
          v - a positive integer
 Output: N - the number of full rook placements of fb, whose sum of  the i-th row is par[[Length[fb] + 1 - i]]  and  the longest 
               ne-chain has length u and the longest se-chian has length v  
*)

frpParNESE[fb_, par_, u_, v_] := 
  Module[{frp, fp = {}, subuu, subu, subvv, subv, N = 0},
         frp = frpGenerator[fb]; (* generate full rook placements of fb *)
         Do[If[frpParTest[fb, frp[[i]], par] == 1, fp = Append[fp, frp[[i]]]]; 
           , {i, Length[frp]}]; (* pick full rook placements of fb which fit par *)
         subuu = fbSubSquare[fb, u + 1]; (* generate the indices of submatrices of fb of dimension u + 1 *)
         subu = fbSubSquare[fb, u];
         subvv = fbSubSquare[fb, v + 1];
         subv = fbSubSquare[fb, v];
         Do[N = N + If[TestNE[fp[[i]], u, subuu, subu] + TestSE[fp[[i]], v, subvv, subv] == 2, 1, 0], {i, Length[fp]}]; 
         (* count full rook placements which satisfy the conditions in the specification *)
	 Return[N];
	 ];

End[]

EndPackage[]

(*
(* NonFerrersBoard
   Input: fb - a list of positive integers, which represents a Ferrers board
   Output: nfb - a list of tuples which are in the rectangular matrix of fb, but not in the Ferrers board of fb
 *)

NonFerrersBoard[fb_] :=
  Module[{n = Length[fb], nfb = {}, i, j},
         If[n == 1, Return[nfb]];
         For[i = n - 1, i >= 1, i--, 
             For[j = 1, j <= fb[[1]], j++,
                 If[j > fb[[n - i + 1]], nfb = Join[nfb, Table[{i, k}, {k, j, fb[[1]]}]]; Break[]]; 
(* determine the tuples that are not in the Ferrers board of fb *)
                 ]; 
	     ];
	 Return[nfb];
	 ];

(* Name: SubSquare
   Input: l - a list of dimension 2 which represents the dimensions of l[[1]] times l[[2]] matrices
          n - a positive integer which is smaller than or equal to min{l[[1]], l[[2]]}
   Output: ssq - a set of indices in sorted order for sub square matrices of dimension n of a l[[1]] times l[[2]] matrix
*)

SubSquare[l_, n_] := 
  Module[{s1, t1, s2, t2, ssq = {}, i, j},
         s1 = Subsets[Range[l[[1]]], {n}]; (* construct the subsets of {1, ..., l[[1]]} with n elements*)
         t1 = Length[s1];
         s2 = Subsets[Range[l[[2]]], {n}]; (* construct the subsets of {1, ..., l[[2]]} with n elements *)
	 t2 = Length[s2];
	 For[i = 1, i <= t1, i++,
             For[j = 1, j <= t2, j++,
		 ssq = Append[ssq, Tuples[{s1[[i]], s2[[j]]}]]; (* construct indices for a sub square matrix of dimension n *)
		 ]; 
	     ];
         Return[ssq];
	 ];

(* Name: IndicesToMatrix
   Input: m - a matrix
          n - a positive integer
          ind - a set of indices in sorted order of a square submatrix of m of dimension n 
 *)

IndicesToMatrix[m_, n_, ind_] := 
  Module[{sm = Table[0, {i, n}, {j, n}]},
         Do[Do[sm[[i, j]] = m[[ind[[(i - 1) n + j, 1]] , ind[[(i - 1) n + j, 2]]]] , {j, n}],{i, n}];
         Return[sm];
	 ];

(* Name: MemberTest
   Input: l - a list
          t - another list
   Output: 1 - there exists an element a in t such that a belongs to l
           0 - otherwise
 *)

MemberTest[l_, t_] :=
  Module[{i, n = Length[t]},
         For[i = 1, i <= n, i++, 
	     If[MemberQ[l, t[[i]]], Return[1]];
             ];
         Return[0];
	 ];
 *)
