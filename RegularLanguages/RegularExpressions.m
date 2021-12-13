(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: Regex *)
(* :Context: Regex` *)
(* :Author: Adam Smith *)
(* :Date: 2020-05-19 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2020 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

Package["RegularLanguages`"]

(* ::Section:: *)
(* Regex *)

PackageExport["Regex"]
Regex::usage = "Regex[x] represents the regular expression whose language is exactly {x}.
Used to indicate a literal outside a compound RE.";

(* ::Section:: *)
(* Properties *)

PackageExport["REQ"]
REQ::usage = "REQ[expr] yields True when expr has head Regex, or satisfies CompoundREQ.
REQ[expr, patt] gives True if expr is EmptyLanguage or Epsilon, or of the form Regex[x] where x matches patt, or is a compound regex where every subexpression at level -1 that is not EmptyLanguage or Epsilon matches patt.";
REQ[r : (EmptyLanguage | Epsilon | _Regex | _?CompoundREQ)] = True;
REQ[_] = False;
REQ[r_, patt_] := (MatchQ[r, _Regex] && MatchQ[First@r, patt]) || CompoundREQ[r, patt];

PackageExport["CompoundREQ"]
CompoundREQ::usage = "\
CompoundREQ[expr] returns True if expr has head REUnion, REConcat, or REClosure.
CompoundREQ[expr, patt] returns True if expr is a compound regex and every character in the standard alphabet of regex matches patt.";
CompoundREQ[_REConcat | _REUnion | _REClosure] = True;
CompoundREQ[_] = False;
CompoundREQ[r : (_REConcat | _REUnion | _REClosure), patt_] :=
  AllTrue[LanguageAlphabet[r, "IncludeEpsilon" -> False], MatchQ[patt]];

PackageExport["REMatchQ"]
REMatchQ::usage = "\
REMatchQ[expr, regex] returns True if expr is matched by regex.
REMatchQ[regex] represents an operator form of REMatchQ.";
REMatchQ[Epsilon, r_] := matchesEmptyQ[r];
REMatchQ[input_String, r_ /; CompoundREQ[r, _String]] := StringMatchQ[input, RegularExpression[ToREString[r]]];
REMatchQ[_, r_ /; CompoundREQ[r, _String]] = False;
REMatchQ[input_, r_] := ToNFA[r][input];
REMatchQ[r_][input_] := REMatchQ[input, r];

PackageExport["RENormal"]
RENormal::usage = "RENormal[regex] converts the regex into an expression with head RegularExpression recognizing strings from the same language.";
RENormal[EmptyLanguage] = RegularExpression["a^"];
RENormal[r_] := RegularExpression@ToString@Map[
  RightComposition[ToString,
    StringReplace[a : Characters[".$^?*+|{}()[]\\"] :> "\\" ~~ a],
    StringReplace[{s : (StartOfString ~~ _ | ("\\" ~~ _) ~~ EndOfString) :> s, s__ :> "(" ~~ s ~~ ")"}]],
  r, {-1}];

ToREString[r_] := ToString[
  With[{esc = StringReplace[a : Characters[".$^?*+|{}()[]\\"] :> "\\" ~~ a]},
    With[{ts = If[StringLength[#] > 1, "(" <> esc[#] <> ")", esc[#]] &},
      r /. {x : reExcludingPatt[EmptyLanguage] :> ts[ToString[x]], EmptyLanguage -> "(a^)"}]]];

PackageExport["RELength"]
RELength::usage = "RELength[regex] gives the number of characters in the regular expression regex. Note that the character Epsilon is considered to have length 0.";
RELength[r_] := Module[{i = 0}, r /. reExcludingPatt[EmptyLanguage, Epsilon] :> i++; i];

(* ::Section:: *)
(* Operators *)

(* ::Subsection:: *)
(* Union *)

PackageExport["REUnion"]
REUnion::usage = "REUnion[e1, e2, ...] represents a regex matching the union e1 | e2 | ... of the expressions ei.";
Default[REUnion] = EmptyLanguage;
REUnion[x_.] := x;
SetAttributes[REUnion, Orderless];
u : REUnion[x_, x_, ___] := Union[Unevaluated@u];
REUnion[Epsilon, c_REClosure , a___] := REUnion[c, a];
SetAttributes[REUnion, {OneIdentity, Flat}];
REUnion[EmptyLanguage, a_] := a;
REUnion[x_, REClosure[x_]] := REClosure[x];
REUnion /: ToString[u_REUnion] :=
  "(" <> StringRiffle[ToString /@ (List @@ u), "|"] <> ")";

(* ::Subsection:: *)
(* Concatenation *)

PackageExport["REConcat"]
REConcat::usage = "REConcat[e1, e2, ...] represents a regex matching the concatenation e1 e2 ... of the expressions ei.";
Default[REConcat] = Epsilon;
REConcat[___, EmptyLanguage, ___] = EmptyLanguage;
REConcat[x_] := x;
REConcat[] = EmptyLanguage;
c : REConcat[___, Epsilon, ___] := DeleteCases[Unevaluated@c, Epsilon];
SetAttributes[REConcat, {Flat, OneIdentity}];
REConcat /: ToString[c_REConcat] := StringRiffle[ToString /@ (List @@ c), ""];

(* ::Subsection:: *)
(* Closure *)

PackageExport["REClosure"]
REClosure::usage = "REClosure[e] represents a regex matching the closure of expression e with respect to concatenation. This is defined as the set {Epsilon, e, ee, eee, ...}.";
REClosure[EmptyLanguage | Epsilon] = Epsilon;
REClosure[REClosure[x_]] := REClosure[x];
(*REClosure /: MakeBoxes[REClosure[x_], form : (StandardForm | TraditionalForm)] := MakeBoxes[SuperStar@x, form];*)
REClosure /: ToString[REClosure[c_REConcat]] := "(" <> ToString[c] <> ")*";
REClosure /: ToString[c_REClosure] := ToString @@ c <> "*";
REClosure[_, x__] /; Message[REClosure::argx, REClosure, Length[{x}] + 1] = Null;
SyntaxInformation[REClosure] = {"ArgumentsPattern" -> {_}};

(* ::Section:: *)
(* Transformation *)

PackageExport["RESymbolIndex"]
RESymbolIndex::usage = "RESymbolIndex[r, i] is a symbolic wrapper representing the i-th occurrence of the symbol r in a linearized regex.";
RESymbolIndex /: MakeBoxes[RESymbolIndex[x_, i_], StandardForm] := ToBoxes[Subscript[x, i]];

PackageExport["LinearizeRE"]
LinearizeRE::usage = "LinearizeRE[regex] linearizes regex by indexing each character occurrence.
LinearizeRE[regex, i] linearizes regex by indexing each character occurrence, starting at i.
LinearizeRE[regex, i, True] returns a list {r', {a1, a2, ...}} where r' is the linearization of regex, and the ai are the alphabet of r'";
LinearizeRE[r_, starti_Integer : 1, returnAlphabet_ : False] := Module[
  {i = starti},
  If[returnAlphabet,
    Reap[r /. p : reExcludingPatt[Epsilon] :> Sow[RESymbolIndex[p, i++], "newalph"], "newalph", Sequence @@ ##2 &],
    r /. p : reExcludingPatt[Epsilon] :> RESymbolIndex[p, i++]]
];

PackageExport["SimplifyRE"]
SimplifyRE::usage = "SimplifyRE[r] attempts to simplify the provided regular expression using simple pattern matching.";
SimplifyRE[r_, opts : OptionsPattern[ReplaceRepeated]] :=
  ReplaceRepeated[r, $RESimplificationRules,
    filterOpts[{opts}, ReplaceRepeated]];

PackageExport["FactorRE"]
FactorRE::usage = "FactorRE[r] attempts to factor the given regular expression.";
FactorRE[r_, opts : OptionsPattern[ReplaceRepeated]] :=
  ReplaceRepeated[r,
    $REFactorizationRules,
    filterOpts[{opts}, ReplaceRepeated]];

PackageExport["AdvancedSimplifyRE"]
AdvancedSimplifyRE::usage = "AdvancedSimplifyRE[r] applies additional techniques of factorization and regular language equivalence to simplify the given regular expression.";
Options[AdvancedSimplifyRE] = {"Factorize" -> True};
AdvancedSimplifyRE[r_, opts : OptionsPattern[{AdvancedSimplifyRE, ReplaceRepeated}]] :=
  ReplaceRepeated[r, Dispatch[{
    $RESimplificationRules,
    $REAdvancedSimplificationRules,
    If[TrueQ[OptionValue["Factorize"]],
      $REFactorizationRules, Nothing]}],
    filterOpts[{opts}, ReplaceRepeated]];

PackageExport["ExpandRE"]
ExpandRE::usage = "ExpandRE[r] expands the given regular expression by distributing REConcat over REUnion.";
ExpandRE[c_REConcat] := If[FreeQ[c, _REUnion], c,
  Distribute[c, REUnion, REConcat, REUnion, ExpandRE /@ REConcat[##] &]];
ExpandRE[r_?CompoundREQ] := ExpandRE /@ r;
ExpandRE[a_] := a;

(* ::Section:: *)
(* Construction *)

PackageExport["RandomRE"]
RandomRE::usage = "RandomRE[n, k] returns a random regular expression on n symbols from an alphabet of length k.
RandomRE[n,k,p] returns a random regular expression of n symbols from an alphabet of length k, where p is the probability of grouping.";
RandomRE::probprm = "Parameter `1` at position `2` in `3` is expected to be a probability strictly less than 1.";
Options[RandomRE] = {
  "ClosureProbability" -> 0.2,
  "UnionProbability" -> 0.3,
  TimeConstraint -> 0.1,
  "AlphabetFunction" -> Automatic,
  "EpsilonProbability" -> 0
};
RandomRE[n_Integer, alphin : (_List | _Integer), p : _?NumericQ : 0.5, OptionsPattern[]] :=
  With[{k = when[alphin, _Integer, Length@alphin],
    c1 = N[{1 - p, p}] -> {True, False},
    c2 = N[{#, 1 - #}&@OptionValue["ClosureProbability"]] -> {REClosure, Identity},
    c3 = N[{#, 1 - #}&@OptionValue["UnionProbability"]] -> {REUnion, REConcat},
    tc = OptionValue[TimeConstraint]},
    Replace[
      NestWhile[ (* Iteratively group elements into lists of random length until the entire expression has length 1 *)
        Split[#, RandomChoice[c1] &] &,
        ReplacePart[ (* Randomly replace characters with Epsilon based on EpsilonProbability *)
          RandomChoice[1 ;; k, n], (* Use 1,2,...,k (since alphabet may contain lists we don't want to change) *)
          toAlternatives[
            RandomSample[1 ;; n, RandomVariate[BinomialDistribution[n, OptionValue["EpsilonProbability"]]]]
          ] -> Epsilon],
        Length[#] > 1 &,
        1, $IterationLimit, -1],
      {x__} :> RandomChoice[c2][ (* Choose whether this subexpression will be starred *)
        TimeConstrained[ (* Sometimes a large REUnion can be slow to evaluate, since it is a Flat, Orderless
                              function that requires some non-trivial pattern matching. *)
          RandomChoice[c3][x], (* Choose whether list becomes REUnion or REConcat *)
          tc, (* If evaluation takes too long (> 0.1s by default), it probably means REUnion[x] was selected *)
          REConcat[x]]], (* Default to concatenation *)
      All
    ] /. AssociationThread[Range[k] -> makeAlphabet[alphin, OptionValue["AlphabetFunction"]]] (* Replace indices with
      characters of the alphabet. *)
  ];

PackageExport["ParseRE"]
ParseRE::usage = "\
ParseRE[str] converts a regex in string form to an expression in terms of REUnion, REConcat, and REClosure.
Recognized constructs are (from greatest to least precedence)
  - Prefix \"\\\" escapes the next character.
  - Round parentheses \"(\" and \")\" indicate grouping.
  - Postfix \"*\" is parsed as closure.
  - Juxtaposition is interpreted as concatenation.
  - Infix \"|\" is parsed as union.
All other characters are interpreted as string literals of length 1.

Options:
\"FullParseTree\" -> True | False
  False: Result expression will use REUnion, REConcat, and REClosure as heads, which automatically reorder and simplify terms.
  True: Result expression will use Inactive[REUnion], Inactive[REConcat], and Inactive[REClosure].
    - The active form can be recovered by calling Activate on the returned expression.";
ParseRE::parsererr = "Something happened at input `1` in `2`";
ParseRE::badexpect = "Was expecting `1`, but recieved `2`.";
Options[ParseRE] = {"FullParseTree" -> False};
OptionChecks[ParseRE] = {"FullParseTree" -> True | False};
ParseRE[string_String, OptionsPattern[]?(validQ @ ParseRE)] :=
  With[{
    w = CreateDataStructure["Queue", Characters[string]]["Push", EndOfString],
    expr = With[{o = applyIf[OptionValue["FullParseTree"],
      Map@Inactive, <|"|" -> REUnion, "." -> REConcat, "*" -> REClosure|>]},
      o[#1][##2] &],
    binaryQ = MatchQ["|"],
    postfixQ = MatchQ["*"],
    varQ = (StringQ[#] && StringMatchQ[#, RegularExpression["[^*()|\\\\]"]]) &,
    concatableQ = StringQ[#] && StringMatchQ[#, RegularExpression["[^*)|]"]] &,
    prec = <|"|" -> 0, "." -> 1, "*" -> 2|>,
    rprec = <|"|" -> 1, "." -> 2, "*" -> 2|>,
    nprec = <|"|" -> 0, "." -> 1, "*" -> 1|>},
    Module[{A, expectAfter, escape},
      expectAfter[return_, c_] := With[{d = w["Pop"]},
        If[MatchQ[d, c], return,
          Throw[Failure["SyntaxError",
            <|"MessageTemplate" -> "Expected `1` at position `2` but received `3`.",
              "MessageParameters" -> {c, StringLength[string] - w["Length"], d}|>]]
        ]];
      escape[c_] := With[{d = w["Pop"]},
        If[StringQ[d] &&
          StringMatchQ[d, RegularExpression["[*()|\\\\]"]], d,
          Throw[Failure["SyntaxError",
            <|"MessageTemplate" -> "Unrecognized escape sequence `1``2` at position `3`.",
              "MessageParameters" -> {c, d, StringLength[string] - w["Length"]}|>]]]];

      A[p_] := Module[{
        t = Switch[w["Peek"],
          "(", expectAfter[w["Pop"]; A[0], ")"],
          "\\", escape[w["Pop"]],
          _?varQ, w["Pop"],
          _, Epsilon],
        nxt = w["Peek"], r = 2, s},
        While[True,
          t = Which[
            binaryQ[nxt] && p <= prec[nxt] <= r, expr[s = w["Pop"], t, A[rprec[s]]],
            postfixQ[nxt] && p <= prec[nxt] <= r, expr[s = w["Pop"], t],
            concatableQ[nxt] && p <= prec["."] <= r,
            expr[s = ".", t, A[rprec[s]]], True, Break[]];
          {r, nxt} = {nprec[s], w["Peek"]}];
        t];
      (*Algorithm start*)
      Catch@expectAfter[A[0], EndOfString]]];

PackageExport["ToRE"]
ToRE::usage = "ToRE[A] converts the automaton A to an equivalent regular expression.";
Options[ToRE] = {Method -> Automatic};
ToRE[A_?FAQ, opts : OptionsPattern[reduceREArray]] :=
  reduceREArray[toRegexArray[A], filterOpts[{opts}, reduceREArray]];
ToRE[r_, OptionsPattern[reduceREArray]] := r;

(* ::Section:: *)
(* Package Scope *)

PackageScope["pSet"]
pSet::usage = "pSet[regex] returns the set of prefix characters of strings recognized by regex.";
pSet[EmptyLanguage | Epsilon | RESymbolIndex[Epsilon, _] | PatternSequence[]] = {};
pSet[HoldPattern[REUnion[x__]]] := Catenate[pSet /@ {x}];
pSet[HoldPattern[REClosure[x_]]] := pSet[x];
pSet[HoldPattern[REConcat[
  Longest[x___?matchesEmptyQ],
  Longest[y : RepeatedNull[_, 1]],
  ___]]] := Catenate[{Catenate[pSet /@ {x}], pSet[y]}];
pSet[x_] := {x};

PackageScope["dSet"]
dSet::usage = "dSet[regex] returns the set of suffix characters of strings recognized by regex.";
dSet[EmptyLanguage | Epsilon | RESymbolIndex[Epsilon, _] | PatternSequence[]] = {};
dSet[HoldPattern[REUnion[x__]]] := Catenate[dSet /@ {x}];
dSet[HoldPattern[REClosure[x_]]] := dSet[x];
dSet[HoldPattern[REConcat[
  Shortest[___],
  RepeatedNull[x_, 1],
  Longest[y___?matchesEmptyQ]]]] := Catenate[{dSet[x], Catenate[dSet /@ {y}]}];
dSet[x_] := {x};

PackageScope["fSet"]
fSet::usage = "fSet[regex] returns the set of factors of length 2 in regex.";
fSet[HoldPattern[REUnion[x__]]] := Catenate[fSet /@ {x}];
fSet[HoldPattern[REClosure[x_]]] := Catenate[{fSet[x], Tuples[{dSet[x], pSet[x]}]}];
fSet[HoldPattern[REConcat[x_, y_]]] := Catenate[{fSet[x], fSet[y], Tuples[{dSet[x], pSet[y]}]}];
fSet[_] = {};

PackageScope["reExcludingPatt"]
reExcludingPatt::usage = "reExcludingPatt[stuff...] is equivalent to Except[_REUnion | _REClosure | _REConcat | _Regex | REUnion | REClosure | REConcat | Regex | stuff...]";
reExcludingPatt[stuff___] := Except@Alternatives[_REUnion, _REClosure, _REConcat, _Regex, REUnion, REClosure, REConcat, Regex, stuff];


(* ::Section:: *)
(* Private Functions *)

(* Convert to expression NFA represented as a SparseArray adjacency matrix.
   initial and terminal state of eNFA at positions 1 and -1 respectively  *)

toRegexArray[A_?FAQ] := With[{
  states = States[A],
  inits = IDs[A, "Initial"],
  terms = IDs[A, "Terminal"],
  alphidx = PositionIndex[LanguageAlphabet[A, "IncludeEpsilon" -> False]][[All, 1]],
  swap = Function[Null, {#1, #2} = {#2, #1}, HoldAll]},
  With[{
    addnewinit = Length@inits != 1,
    addnewterm = Length@terms != 1 || inits === terms}, (*init and term index must be distinct*)
    Module[{getidx,
      idxs = IDs[A, "Index"] + Boole[addnewinit],
      n = StateCount[A] + Boole[addnewinit] + Boole[addnewterm]},
      If[! addnewterm, (*If we're not adding a new terminal state*)
        swap[idxs[First@terms], idxs[[-1]]]]; (*Make the index of the old terminal state n*)
      If[! addnewinit, (*If we're not adding a new initial state*)
        If[idxs[[1]] == 1, (*True unless idx[[1]] got swapped with idx[[-1]] in previous step*)
          swap[idxs[First@inits], idxs[[1]]], (*Make the index of the old initial state 1*)
          swap[idxs[First@inits], idxs[[-1]]]]];(*Make the index of the old initial state idxs[[-1]]=1*)

      ReleaseHold[
        Hold[getidx[id_] :=
          With[{idx = (getidx[id] = idxs[id])},
            Function[
              If[InitialQ[#], Sow[Epsilon, {{1, idx}}]];
              If[TerminalQ[#], Sow[Epsilon, {{idx, n}}]];
              KeyValueMap[ Sow[Lookup[alphidx, Key[#1], #], \[FormalF][idx, #2]] &, Transitions[#]]
            ]@states@id;
            idx]
        ] /. {
          \[FormalF][a_, b_] /; FAType[A] === NFA :> ({a, getidx[#]} & /@ b),
          \[FormalF][a_, b_] :> {{a, getidx[b]}}}
      ];

      {
        SparseArray[DeleteCases[ (*Delete any \[CurlyEpsilon] self-transitions*)
          Last@Reap[Scan[getidx, inits], _, Rule[#1, REUnion @@ #2] &],
          HoldPattern[{x_, x_} -> Epsilon]],
          n, EmptyLanguage],
        Association[Reverse[Normal@alphidx, 2]]
      }]]];

Options[reduceREArray] = { Method -> Automatic, "SimplificationFunction" -> Automatic };
reduceREArray::badorder = "Method \"SpecificOrder\" must be specified as {\"SpecificOrder\", perm}, where perm \
  is a valid permutation of ``, and n is the length of the given array.";
reduceREArray[{array_?SquareMatrixQ, alphabet_}, OptionsPattern[]] := Module[{
  arr = array,
  simp = unless[OptionValue["SimplificationFunction"], Automatic, SimplifyRE, None, Identity]},
  With[{
    idxs = Range[2, Length@arr - 1],
    reduce = regexArrayEliminate[arr, simp],
    method = validatedMethod[OptionValue[Method],
      {Automatic, "Shortest", "LeastCommon", "MostCommon", "ForwardOrder",
        "ReverseOrder", "RandomOrder", "SpecificOrder"}, reduceREArray] },
    Switch[method,
      "ForwardOrder", Scan[reduce, idxs],
      "ReverseOrder", Scan[reduce, Reverse@idxs],
      "RandomOrder", Scan[reduce, RandomSample[idxs]],
      "SpecificOrder",
      With[{perm = Last[OptionValue[Method], Message[reduceREArray::badorder, idxs]; idxs]},
        If[Sort@perm =!= idxs,
          Message[reduceREArray::badorder, idxs]; Scan[reduce, idxs],
          Scan[reduce, perm]]],
      _, With[{next = nextEliminationFunction[arr, method, Break[]]}, While[True, reduce@next[]]]];
    reduce[0] /. alphabet]
];

matchesEmptyQ[Epsilon | _REClosure] = True;
matchesEmptyQ[HoldPattern[REUnion[x__]]] := AnyTrue[{x}, matchesEmptyQ];
matchesEmptyQ[HoldPattern[REConcat[x__]]] := AllTrue[{x}, matchesEmptyQ];
matchesEmptyQ[_] = False;

uwExpr[uw_, uv_, vv_, vw_] := REUnion[uw, REConcat[uv, REClosure[vv], vw]];
SetAttributes[regexArrayEliminate, HoldFirst];
regexArrayEliminate[arr_, simp_][0] := (* recover final expression from reduced array *)
  simp@REConcat[
    REClosure@simp@uwExpr[arr[[1, 1]], arr[[1, -1]], arr[[-1, -1]], arr[[-1, 1]]],
    arr[[1, -1]],
    REClosure@simp@uwExpr[arr[[-1, -1]], arr[[-1, 1]], arr[[1, 1]], arr[[1, -1]]]];
regexArrayEliminate[arr_, simp_][v_] := With[
  {vv = arr[[v, v]]},
  arr[[v, v]] = EmptyLanguage;
  Outer[(arr[[##]] = simp@uwExpr[arr[[##]], arr[[#1, v]], vv, arr[[v, #2]]]) &,
    Flatten@arr[[All, v]]["NonzeroPositions"],
    Flatten@arr[[v]]["NonzeroPositions"]];
  arr[[v, All]] = arr[[All, v]] = EmptyLanguage;
];

SetAttributes[nextEliminationFunction, HoldAll];
nextEliminationFunction[arr_, method_, default_] := With[
  {nonzeroVals = Function[ Join[arr[[#]], Delete[arr[[All, #]], #]]["NonzeroValues"]]},
  With[{nextf = Switch[method,
    Automatic | "Shortest", First@*MinimalBy[Total@*RELength@*nonzeroVals],
    "MostCommon", First@*MaximalBy[Length@*nonzeroVals],
    "LeastCommon", First@*MinimalBy[Length@*nonzeroVals]]},
    Module[{groups = CreateDataStructure["Queue",
      Switch[method,
        Automatic, findEliminationGroups[arr],
        _, {Range[2, Length@arr - 1]}]],
      indices = {}},
      (If[Length[indices] == 0 && ! groups["EmptyQ"],
        indices = groups["Pop"]];
      If[Length[indices] == 0, default,
        (indices = DeleteCases[indices, #]; #) &@ nextf[indices]]) &]]
];

findEliminationGroups[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  findEliminationGroups[arr, s, autoAlt[t, Length@arr], unless[subset, All, rangeOver@arr]];
findEliminationGroups[arr_SparseArray, s_, t_, subset_] := Module[
  {div, lvl},
  lvl[l_][x_] := (lvl[_][x] = l);

  div[l_][ss : {_, _}] := Scan[lvl[l], ss];
  div[l_][ss : {u_, v__, w_}] := (
    Scan[lvl[l], {u, w}];
    With[{bridges = bridgeStates[arr, u, w, ss]},
      Switch[Length@bridges,
        Length@{v}, Scan[lvl[l + 1], {v}],
        _?Positive,
        BlockMap[ (* For each pair of sequential bridge states *)
          Function[{pair},
            Scan[div[l + 1], (* recursively apply div to subautomata *)
              horizontalChop[arr, Sequence @@ pair, ss]]],
          Flatten@{u, bridges, w}, 2, 1],
        _, Null]]);

  div[0][arrangeFirstLast[subset, s, t]];
  ReverseSortBy[GatherBy[
    DeleteCases[subset, s | t],
    lvl[Infinity]], lvl[Infinity]@*First]
];

bridgeStates[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  bridgeStates[arr, s, autoAlt[t, Length@arr], subset];
bridgeStates[arr_SparseArray, s_, t_, subset_] := With[{
  adjlists = adjacencyIntersection[arr, subset],
  path = findPath[arr, s, t, subset],
  anc = CreateDataStructure["FixedArray", Length@arr],
  min = CreateDataStructure["FixedArray", Length@arr],
  max = CreateDataStructure["FixedArray", Length@arr]},
  If[Length@path == 0, {}, (* s and t disconnected *)
    Module[{dfs, onPath, setMinMax, notbridges},
      onPath[toAlternatives@path] = True; onPath[_] = False;

      notbridges = Function[{v},
        Flatten[Range @@@ {
          {min["Part", v] + 1, anc["Part", v]},
          {anc["Part", v] + 1, max["Part", v] - 1}}], Listable];

      setMinMax[v_, {}] :=
        min["Part", v] = max["Part", v] = anc["Part", v];
      setMinMax[v_, succVals_] :=
        MapThread[(#1["Part", v] =
          First[#2[#3, UpTo[1]], anc["Part", v]]) &,
          {{min, max}, {TakeSmallest, TakeLargest}, Transpose@succVals}];

      dfs[prev_ : 0][v_] := (dfs[_][v] = Null;
      With[{succs = adjlists[[v]],
        p = First[If[onPath@v, FirstPosition[path, v]], 0]},
        Scan[dfs[anc["Part", v] = Max[p, prev]],
          If[0 < p < Length@path,
            rotateToFront[succs, path[[p + 1]]],
            succs]];
        setMinMax[v,
          If[onPath@#,
            ConstantArray[anc["Part", #], 2],
            Through[{min, max}["Part", #]]] &
            /@ succs]]);

      dfs[]@s;
      Delete[path, Transpose@List@Flatten@{1, -1, notbridges@path}]]]
];

horizontalChop[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  horizontalChop[arr, s, autoAlt[t, Length@arr], subset];
horizontalChop[arr_SparseArray, s_, t_, subset_] := With[
  {groups = CreateDataStructure["DisjointSet"],
    adjlists = adjacencyIntersection[arr, subset]},
  Module[{assign},
    assign[v_] := (
      assign[v] = v;
      groups["Insert", v];
      Scan[If[# =!= t, groups["Unify", v, assign[#]]] &,
        adjlists[[v]]];
      v );

    Scan[assign, DeleteCases[adjlists[[s]], s | t]];
    If[groups["EmptyQ"], {{s, t}}, arrangeFirstLast[#, s, t] & /@ groups["Subsets"]]]
];

adjacencyIntersection[arr_SparseArray, subset_] :=
  If[subset === All, arr["AdjacencyLists"],
    Intersection[subset, #] & /@ arr["AdjacencyLists"]];

arrangeFirstLast[l_List, first_, last_] :=
  {first, Splice@DeleteDuplicates[l, first | last], last};
arrangeFirstLast[expr_, first_, last_] :=
  expr /. h_[ p : OrderlessPatternSequence[Except[first | last] ...]] :> h[first, p, last];

findPath[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  findPath[arr, s, autoAlt[t, Length@arr], subset];
findPath[arr_SparseArray, s_, t_, subset_] := With[
  {neighbors = adjacencyIntersection[arr, subset]},
  Module[{dfs},
    dfs[v_] := (dfs[v] = Null;
    Catch[If[MemberQ[neighbors[[v]], t],
      Throw[Sow[t], "path"],
      Scan[dfs, neighbors[[v]]]],
      "path", Throw[{v, #1}, #2] &]);
    Block[{$RecursionLimit = Max[(Length@arr)^2 + 1, $RecursionLimit]},
      unless[Catch[dfs[s], "path", Flatten[#1] &],
        Null, {}]]]
];

$REFactorizationRules = Dispatch[{
  HoldPattern[u : REUnion[x_, REConcat[x_, r_]]] :>
    REConcat[x, REUnion[r, Epsilon]],
  HoldPattern[u : REUnion[x_, REConcat[r_, x_]]] :>
    REConcat[REUnion[r, Epsilon], x],
  HoldPattern[u : REUnion[Repeated[
    REConcat[prefix_., Shortest[_.], suffix_.] /; !SameQ[prefix, suffix, Epsilon],
    {2, Infinity}]]
  ] :> REConcat[prefix, Replace[u, REConcat[prefix, x_., suffix] :> x, {1}], suffix]
}];

$RESimplificationRules = Dispatch[{
  HoldPattern[REConcat[c : REClosure[REConcat[(x_) ..] | x_], x_]
  ] :> REConcat[x, c], (*  x*x \[Rule] xx* and [xx...]*x -> x[ xx...]*  (standard form)  *)
  HoldPattern[REConcat[REClosure[x_], x_, REClosure[x_]]
  ] :> REConcat[x, REClosure[x]], (*  x*xx* -> xx*  *)
  HoldPattern[REConcat[c_REClosure ..]
  ] :> c, (*  x*x* -> x*  *)
  HoldPattern[REClosure[REUnion[Epsilon, x_]]
  ] :> REClosure[x], (* [\[CurlyEpsilon]|x]* -> x* *)
  HoldPattern[ REUnion[x_ | REClosure[x_], c : REClosure[REUnion[x_ | REClosure[x_], _.]] , a_.]
  ] :> REUnion[c, a], (*  x|[x|y]* -> [x|y]*  *)
  HoldPattern[REUnion[Epsilon, REConcat[x_, REClosure[x_]]]
  ] :> REClosure[x], (*  \[CurlyEpsilon]|xx* -> x* (this one seems to happen a lot converting FAs to regex *)
  HoldPattern[REClosure[REUnion[x_, REConcat[(x_) ..], a_.]]
  ] :> REClosure[REUnion[x, a]], (*  [x|[xx...]]* -> x* *)
  HoldPattern[ REClosure[REUnion[c : REConcat[x__], REConcat[(x__) ..], a_.]]
  ] :> REClosure[REUnion[c, a]], (*  [xy|[xyxy...]]* -> [xy]*  *)
  HoldPattern[REClosure[u : REUnion[__REClosure, _]]
  ] :> REClosure[Replace[u, REClosure[a_] :> a, {1}]], (*  [x| y*]* -> [x|y]*  *)
  HoldPattern[REClosure[c : REConcat[__REClosure]]
  ] :> REClosure[REUnion @@ Sequence @@@ c] (*  [x*y*]* -> [x| y]*  *)
}];

$REAdvancedSimplificationRules = Dispatch[{
  HoldPattern[REUnion[x_, y_] /; SubsetLanguageQ[x, y]] :> y ,
  HoldPattern[ REConcat[x_REClosure, y_REClosure] /; SubsetLanguageQ[x, y]] :> y,
  HoldPattern[(REConcat[x_, y_REClosure] | REConcat[y_REClosure, x_]) /; SubsetLanguageQ[x, y]] :> y
}];

