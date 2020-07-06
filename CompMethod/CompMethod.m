(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jul 5, 2020 *)

BeginPackage["CompMethod`"]
(* Exported symbols added here with SymbolName::usage *) 

f::usage = "Apply the function f to the arguments {s, j} according to the algorithm specified in alg.";

runAlg::usage = "Repeatedly apply the function f to the initial state input according to the algorithm specified in alg until the result does not change.
				This signifies the termination of the algorithm encoded by alg. And the remaining string encodes the result.";
			
euclidsAlg::usage = "Encode the integers m and n into a string with m instances of 'a' followed by n instances of 'b'. Pass this state to runAlg 
					using a computational model of Euclid's algorithm to calculate the GCD of m and n. The result should be encoded in either a string
					of repeated 'a' or 'b', depending on the computational model provided."

Begin["`Private`"]
(* Implementation of the package *)

f[{s_String, j_Integer}, alg_List] := Piecewise[{
   {{s, j}, j == Length@alg},
   {{s, alg[[j + 1, 5]]}, !StringMatchQ[s, ___ ~~ alg[[j + 1, 2]] ~~ ___]},
   {{StringReplace[s, alg[[j + 1, 2]] -> alg[[j + 1, 3]], 1], alg[[j + 1, 4]]}, True}
  }]
  
runAlg[alg_List, input_String, (timeOut_DirectedInfinity | timeOut_Integer)] := FixedPoint[f[#, alg] &, {input, 0}, timeOut]

euclidsAlg[m_Integer, n_Integer, implementation_List] := Join[ConstantArray["a", m], ConstantArray["b", n]] // 
	StringJoin //
	runAlg[implementation, #, \[Infinity]] & // 
  	Max[StringCount[#[[1]], "b"], StringCount[#[[1]], "a"]] &


End[]

EndPackage[]

