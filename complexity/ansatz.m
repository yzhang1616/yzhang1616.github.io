BeginPackage["ansatz`"]

`GuessDegBivariatePolynomial::usage = "GuessDegBivariatePolynomial[n, G, x, y, d] gives a zero polynomial or 
a list of nonzero bivariate polynomials H of total degree d in K[x, y], such that H(x, G(x)) = 0 mod x^n. 
Here, n is a positive integer, G is a univariate the form Sum[a[i] x^i, {i, 0, n - 1}] in K[x], x and y are
two indeterminates, d is a positive integer, where K is a field with char(K) = 2";

`GuessBivariatePolynomial::usage = "GuessBivariatePolynomial[n, G, x, y] gives {d, H}, 
where d is a positive integer, H is a list of bivariate polynomials of total degree d in K[x, y], 
such that (i) H(x, G(x)) = 0 mod x^n; (ii) d := tdeg(H) is mimimal such that (i) holds. 
Here, n is a positive integer, G is a univariate the form Sum[a[i] x^i, {i, 0, n - 1}] in K[x], x and y are
two indeterminates, where K is a field with char(K) = 2";

`GuessBivariatePolynomialOptimal::usage = "GuessBivariatePolynomialOptimal[n, G, init, x, y] gives {d, H}, 
where d is a positive integer, H is a list of bivariate polynomials of total degree d in K[x, y], 
such that (i) H(x, G(x)) = 0 mod x^n; (ii) d := tdeg(H) is mimimal such that (i) holds. 
Here, n is a positive integer, G is a univariate the form Sum[a[i] x^i, {i, 0, n - 1}] in K[x], 
init is an initial value for the total degree d, x and y are
two indeterminates, where K is a field with char(K) = 2";

`MSeq::usage = "MSeq[k] gives a list of length k + 5, which satisfies (1) {ms[1], ..., ms[k]}, {ms[2], ..., ms[k + 1]}, 
..., {ms[5], ms[6], ..., ms[k + 4]} are pairwise different; (2) {ms[6], ms[7], ..., ms[k + 5]} 
is the same as one of previous subsequence of length k; (3) there are two identical subsequence of length 2 among {ms[1], ..., ms[k + 3]}";


Begin["Private`"] 

(*  Name: GuessDegBivariatePolynomial
    Input: n - a positive integer
           G - a nonzero univariate polynomial of the form Sum[a[i] x^i, {i, 0, n - 1}] in K[x], 
               where K is a field of char(K) = 2
           x - an indeterminate
           y - another indeterminate
           d - a positive integer
    Output: H - a zero polynomial or 
                a list of nonzero bivariate polynomials of total degree d in K[x, y], s.t. 
                (i) H(x, G(x)) = 0 mod x^n;
*)

GuessDegBivariatePolynomial[n_, G_, x_, y_, d_] := 
  Module[{A, c, coeff1, coeff2, sol, lsol, h, H = {}},
         If[d == 0, Return["the total degree must be a positive integer"]];
         If[G == 0, Return[{y}]];
         A = Expand[Sum[Sum[c[i, j - i] x^i G^(j - i) ,{i, 0, j}], {j, 0, d}], Modulus -> 2];
         (* generate an ansatz for a bivariate polynomial A with total degree d *) 
         coeff1 = Table[Coefficient[A, x, i] , {i, 0, n - 1}];
         (* take the coefficient of x^k in A for k = 0, ..., n - 1 *)
         coeff2 = Transpose[Coefficient[coeff1, #]& /@ Flatten[Table[Table[c[i, j - i] , {i, 0, j}], {j, 0, d}]]];
         (* take the coefficient matrix of c[i, j] *)
         sol = NullSpace[coeff2, Modulus -> 2];
         If[Length[sol] == 0, Return[0]]; (* H = 0 *)
	 Do[lsol = sol[[k]];
            h = 0;
            Do[h = h + Sum[lsol[[i + 1]] x^i y^(j - i), {i, 0, j}];
               lsol = Drop[lsol, j + 1];
              , {j, 0, d}];
            (* generate the corresponding polynomial for sol[[k]] *)
            H = Append[H, h];
            , {k, Length[sol]}];
	 Return[H];
	];

(*  Name: GuessBivariatePolynomial
    Input: n - a positive integer
           G - a univariate polynomial of the form Sum[c[i] x^i, {i, 0, n - 1}] in K[x], 
               where K is a field of char(K) = 2
           x - an inderterminate
           y - another inderterminate
    Output: {d, H} - d is a positive integer 
                     H is list of bivariate polynomials of total degree d in K[x, y], s.t. 
                     (i) H(x, G(x)) = 0 mod x^n;
                     (ii) d := tdeg(H) is mimimal such that (i) holds
*)

(* Purpose: find a positive integer n, such that tdeg(H) >= 6 *)

GuessBivariatePolynomial[n_, G_, x_, y_] := 
 Module[{d = 1, H = 0},
	While[H == 0, H = GuessDegBivariatePolynomial[n, G, x, y, d]; d = d + 1];
        Return[{d - 1, H}];
	 ];

(*  Name: GuessBivariatePolynomialOptimal
    Input: n - a positive integer
           G - a univariate polynomial of the form Sum[c[i] x^i, {i, 0, n - 1}] in K[x], 
               where K is a field of char(K) = 2
           init - an initial value for the total degree d
           x - an inderterminate
           y - another inderterminate
    Output: {d, H} - d is a positive integer 
                     H is list of bivariate polynomials of total degree d in K[x, y], s.t. 
                     (i) H(x, G(x)) = 0 mod x^n;
                     (ii) d := tdeg(H) is mimimal such that (i) holds
*)

(* Purpose: find a positive integer n, such that tdeg(H) >= 6 *)

GuessBivariatePolynomialOptimal[n_, G_, init_, x_, y_] := 
 Module[{d, A, c, coeff1, coeff2, sol, lsol, h, H = 0},
        d = init; (* set d to be the initial value *)
        If[d == 0, Return["the total degree must be a positive integer"]];
        If[G == 0, Return[{1, {y}}]];
        A = Expand[Sum[Sum[c[i, j - i] x^i G^(j - i) ,{i, 0, j}], {j, 0, d - 1}], Modulus -> 2]; 
        (* make an ansatz up to total degree d - 1 *)
	While[H == 0, 
              A = A + Sum[c[i, d - i] x^i G^(d - i), {i, 0, d}]; 
              (* make an ansatz up to total degree d *)
              coeff1 = Table[Coefficient[A, x, i] , {i, 0, n - 1}];
              (* take the coefficient of x^k in A for k = 0, ..., n - 1 *)
              coeff2 = Transpose[Coefficient[coeff1, #]& /@ Flatten[Table[Table[c[i, j - i] , {i, 0, j}], {j, 0, d}]]];
              (* take the coefficient matrix of c[i, j] *)
              sol = NullSpace[coeff2, Modulus -> 2];
              If[Length[sol] == 0, d = d + 1,
                 H = {}; 
                 Do[lsol = sol[[k]];
                    h = 0;
                    Do[h = h + Sum[lsol[[i + 1]] x^i y^(j - i), {i, 0, j}];
                       lsol = Drop[lsol, j + 1];
                       , {j, 0, d}];
                    (* generate the corresponding polynomial for sol[[k]] *)
                    H = Append[H, h];
                    , {k, Length[sol]}];
		 Break[];
		];
             ];
        Return[{d, H}];
	 ];

(* Example:

In[88]:= init = 1                                                                                        

Out[88]= 1

In[89]:= Do[G = Sum[seq[[i + 1]] x^i, {i, n - 1}]; init = GuessBivariatePolynomialOptimal[n, G, init, x, 
y][[1]]; Print[{n, init}], {n, Length[seq]}] 

 *)

(*  Name: MSeq
    Input: k - maximum order complexity
    Output: ms - a list of length k + 5, which satisfies 
                (1) {ms[1], ms[2], ..., ms[k]}, {ms[2], ms[3], .., ms[k + 1]}, ..., 
                    {ms[5], ms[6], ..., ms[k + 4]} are pairwise different;
                (2) {ms[6], ms[7], ..., ms[k + 5]} is the same as one of previous subsequence of length k;
                (3) there are two identical subsequence of length (k - 1) among {ms[1], ..., ms[k + 3]}
*)

MSeq[k_] := 
  Module[{seq, s5, ss5, ms = {}},
         seq = Tuples[Table[{0, 1}, {i, k + 5}]]; (* generate all the list of length k + 5 *)
         Do[s5 = Table[Table[seq[[i, j + l]], {l, 0, k - 1}], {j, 5}]; 
            (* generate the first five subsequences of seq of lengh k *)
            If[Length[DeleteDuplicates[s5]] != 5, Continue[]];
            (* Test item (1) of Output *)
            If[!MemberQ[s5, Table[seq[[i, j]], {j, 6, k + 5}]], Continue[]];
            (* Test item (2) of Output *)
            ss5 = Table[Table[seq[[i, j + l]] , {l, 0, k - 2}], {j, 5}]; 
            (* generate the first five subsequences of seq of length (k - 1) *)
            If[Length[DeleteDuplicates[ss5]] == 5, Continue[]];
            (* Test item (3) of Output *)
            ms = Append[ms, seq[[i]]];
            , {i, Length[seq]}];
         Return[ms];
	 ];

End[]

EndPackage[]
