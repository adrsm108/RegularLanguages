(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: NFAs *)
(* :Context: NFAs` *)
(* :Author: Adam Smith *)
(* :Date: 2020-05-15 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2020 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

Package["RegularLanguages`"]

(* ::Section:: *)
(* NFAs *)

(* ::Section:: *)
(* Clear Symbols *)

(* ::Section:: *)
(* Exported Functions *)

PackageExport["NFAState"]
NFAState::usage = "NFAState[q, <|a -> {q_1, q_2 ...}, ...|>] represents the nonterminal state q in an NFA with transitions \[Delta](q, a) = {q_1, q_2, ...}.";
NFAState::invtrs = "The expression `1` is not a valid NFA transition table. A valid table maps symbols to lists of state ids.";
NFAState[id_, d : {(_ -> _List) ...}, p___] :=
  NFAState[id, Association @@ d, p];
NFAState[DFAState[id_, d_?AssociationQ, p___]] :=
  NFAState[id, List /@ d, p];
NFAState[id_, d_Association?validNFATransitionsQ, ___][a_] :=
  Lookup[d, Key[a], {}];
NFAState /: MakeBoxes[s : NFAState[_, _Association?validNFATransitionsQ, ___],
  form : (StandardForm | TraditionalForm)] := makeStateSummaryBoxes[s, form];
NFAState /: Keys[NFAState[_, d_, ___]] := Keys[d];
NFAState /: Values[NFAState[_, d_, ___]] := Values[d];

PackageExport["NFAQ"]
NFAQ::usage = "NFAQ[A] yields True if A is a valid NFA.";
NFAQ[NFA[_?nfaAscQ]] = True;
NFAQ[G_Graph] := NFAQ[AnnotationValue[G, "Automaton"]];
NFAQ[_] = False;

PackageExport["NFA"]
NFA::usage = "The head NFA represents a nondeterministic finite automaton.";
NFA[a___, "states" -> states : {__NFAState}, b___] := NFA[a, "states" -> GroupBy[states, StateID, First], b];
NFA[OrderlessPatternSequence[
  "states" -> states : <|(_ -> _NFAState) ..|>,
  "initial" -> initial_List,
  "terminal" -> terminal_List,
  Optional["alphabet" -> alphabet_List, "alphabet" -> Automatic]]] :=
  NFA[Association @@ {
    "states" -> states
    , "initial" -> initial
    , "terminal" -> terminal
    , "alphabet" -> autoAlt[alphabet, Union @@ (Keys /@ states)]
  }];

NFA[(List | Association)[stateRules : ((_ -> KeyValuePattern[{}]) ...)], initial_List, terminal_List] :=
  Check[
    With[{states = Association[{stateRules} /. {
      HoldPattern[id_ -> tf : KeyValuePattern[{}]]
        :> (id -> NFAState[id, Association@tf,
        {MemberQ[initial, id], MemberQ[terminal, id]}])}]},
      NFA[
        "states" -> KeySort@Join[states,
          AssociationMap[NFAState[#, <||>, {MemberQ[initial, #], MemberQ[terminal, #]}] &,
            Complement[
              Union[Flatten[Map[Values, states, {0, 1}], 2], initial, terminal],
              Keys@states]]],
        "initial" -> initial,
        "terminal" -> terminal]],
    $Failed,
    {NFAState::invtrs}];

NFA[nfa_NFA] := nfa; (* Evaporate, don't ask questions *)
NFA[A_?FAQ] := ToNFA[A];  (* Automatically cast other automata *)
NFA /: Graph[nfa : NFA[_?nfaAscQ], opts : OptionsPattern[{Graph, automatonGraph}]] :=
  Annotate[automatonGraph[nfa, opts], "Automaton" -> nfa];
NFA /: Graph3D[nfa_NFA?NFAQ, opts : OptionsPattern[{Graph3D, Graph, automatonGraph}]] :=
  Annotate[
    Graph3D[automatonGraph[nfa, filterOpts[{opts}, {Graph, automatonGraph}, Graph3D]],
      filterOpts[{opts}, Graph3D],
      (*        Lighting -> "Neutral",*)
      EdgeShapeFunction -> GraphElementData[{"Arrow", "ArrowSize" -> 0.015}]],
    "Automaton" -> nfa];
NFA /: ToRules[nfa : NFA[_?nfaAscQ]] := Sort@Normal[Normal@*Transitions /@ States@nfa];
NFA /: MakeBoxes[nfa : NFA[asc_?nfaAscQ], form : (StandardForm | TraditionalForm)] :=
  makeAutomatonSummaryBoxes[nfa, form];
(nfa_NFA?NFAQ)[w_List] := With[{states = States[nfa]},
  ContainsAny[IDs[nfa, "Terminal"],
    Fold[EpsilonClosure[Union @@ Through[Lookup[states, #1][#2]],
      states] &,
      EpsilonClosure[IDs[nfa, "Initial"], states], w]]];
(nfa_NFA?NFAQ)[w_List, All] := With[{states = States[nfa]},
  FoldList[
    EpsilonClosure[Union @@ Through[Lookup[states, #1][#2]], states] &,
    EpsilonClosure[IDs[nfa, "Initial"], states], w]];
(nfa_NFA?NFAQ)[w_List, n_Integer?NonNegative] := nfa[Take[w, n], All];
(nfa_NFA?NFAQ)[w_List, n_] := Take[nfa[w, All], n];
(nfa_NFA?NFAQ)[w_String, args___] := nfa[Characters[w], args];

(* ::Subsection:: *)
(* Constructors *)

PackageExport["RandomNFA"]
RandomNFA::usage = "RandomNFA[n, k] creates a random NFA with n states on an alphabet of k symbols.";
RandomNFA::nstates = "Cannot take `1` state names from `2`.";
RandomNFA::nsymbs = "Cannot take `1` symbols from `2`.";
Options[RandomNFA] = {
  EpsilonProbability -> 0.01,
  TerminalStates -> 0.3,
  InitialStates -> 1,
  AllStatesReachable -> True,
  AlphabetFunction -> Automatic,
  StatesFunction -> Automatic};
RandomNFA[statesin : (_List | _Integer), alphin : (_List | _Integer),
  Optional[pn : (Automatic | _?Positive), Automatic],
  Optional[pk : (Automatic | _?Positive), Automatic],
  opts : OptionsPattern[RandomNFA]] := With[{
  n = when[statesin, _Integer, Length@statesin],
  k = when[alphin, _Integer, Length@alphin]},
  With[{
    maxsymbols = intProp[autoAlt[pk, Ceiling@Log[k + 1]], k],
    maxstates = intProp[autoAlt[pn, Ceiling@Min[Log[n + 1], 0.18 n]], n],
    nterm = intProp[OptionValue[TerminalStates], n],
    ninit = intProp[OptionValue[InitialStates], n],
    alph = makeAlphabet[alphin, OptionValue[AlphabetFunction]],
    ids = makeStateIDs[statesin, OptionValue[StatesFunction]]},
    Module[{states, init, reachable, unreachable},
      init = RandomSample[ids, UpTo[ninit]];
      states = AssociationThread[ids,
        MapThread[
          Association@If[#2 <= 0, #1,
            Append[#1, Epsilon -> RandomSample[ids, Min[#2, maxstates]]]]&,
          {Thread[# -> randomSubset[ids, {1, Min[n, maxstates]}, Length@#]]&
            /@ randomSubset[alph, {0, Min[k, maxsymbols]}, n],
            RandomVariate[BinomialDistribution[n, OptionValue[EpsilonProbability]], n]}]];
      If[OptionValue[AllStatesReachable],
        unreachable = Complement[ids, reachable = TransitiveClosure[init, states]];
        While[Length@unreachable > 0,
          (If[KeyExistsQ[states[#1], #2],
            AppendTo[states[#1, #2], First@unreachable],
            states[#1, #2] = {First@unreachable}])&
            @@ (RandomChoice /@ {reachable, alph});
          unreachable = Complement[unreachable,
            reachable = Union[reachable, TransitiveClosure[{First@unreachable}, states]]]]];
      NFA[states, init, RandomSample[ids, UpTo[nterm]]]]]
];

PackageExport["NthFromLastNFA"]
NthFromLastNFA::usage = "NthFromLastNFA[n] returns an NFA accepting the language of strings over {\"a\", \"b\"} whose n-th from last character is \"a\"
NthFromLastNFA[n, c, {c_1, c_2, ...}] returns an NFA accepting the language of strings over {c_1, c_2, ...} whose n-th from last character is c.";
NthFromLastNFA[n_] := NthFromLastNFA[n, "a", {"a", "b"}];
NthFromLastNFA[n_, a_, alph_] := NFA[
  "states" -> Association[
    0 -> NFAState[0, Association[Thread[DeleteCases[alph, a] -> {0}, List, 1], {a -> {0, 1}}], {True, False}],
    Table[i -> NFAState[i, Association@Thread[alph -> {i + 1}, List, 1], {False, False}], {i, 1, n - 1}],
    n -> NFAState[n, <||>, {False, True}]],
  "initial" -> {0},
  "terminal" -> {n},
  "alphabet" -> alph
];

PackageExport["ToNFA"]
ToNFA::usage = "ToNFA[A] converts the automaton A into an NFA.
ToNFA[regex] converts the regular expression regex into an NFA.";
Options[ToNFA] = {Method -> Automatic};
ToNFA[nfa_NFA, OptionsPattern[ToNFA]] := nfa;
ToNFA[DFA[asc_?dfaAscQ], OptionsPattern[ToNFA]] := NFA[MapAt[NFAState, KeyDrop[asc, {"icon"}], {{"states", All}}]];
ToNFA[g_?FAGraphQ, OptionsPattern[ToNFA]] := ToNFA[FAExpression@g];
ToNFA[EmptyLanguage, OptionsPattern[ToNFA]] = NFA[{}, {1}, {}];
ToNFA[Epsilon, OptionsPattern[ToNFA]] = NFA[{}, {1}, {1}];
ToNFA[Regex[x_]] := NFA[
  "states" -> <|
    1 -> NFAState[1, <|x -> {2}|>, {True, False}],
    2 -> NFAState[2, <||>, {False, True}] |>,
  "initial" -> {1},
  "terminal" -> {2},
  "alphabet" -> {x}
];
ToNFA[r_?CompoundREQ, OptionsPattern[ToNFA]] :=
  Switch[validatedMethod[OptionValue[Method], {"Glushkov", "Thompson", Automatic}, ToNFA],
    "Glushkov" | Automatic, glushkovNFA[r],
    "Thompson" , thompsonNFA[r]];

(* ::Section:: *)
(* Package Scope *)

PackageScope["nfaAscQ"]
nfaAscQ::usage = "nfaAscQ[asc] returns True if asc is a valid association where asc[\"states\"] is an association whose values are NFAStates, and asc[\"initial\"], asc[\"terminal\"], and asc[\"alphabet\"] are lists";
nfaAscQ[KeyValuePattern[{"states" -> <|(_ -> _NFAState) ...|>,
  "initial" -> _List, "terminal" -> _List, "alphabet" -> _List}]] =
  True;
nfaAscQ[_] = False;

(* ::Section:: *)
(* Private Functions *)

thompsonNFA[regex_?CompoundREQ] := Module[
  {i = 1, states = <||>, newid, symb, convert, attach, socket, chain, close},
  newid[] := (states[i] = NFAState[i, <||>, False]; i++);
  attach[from_, to_, ret_ : Null, with_ : Epsilon] := (
    states[from] = AddTransitions[states[from], with -> when[to, _List, {to}]];
    unless[ret, 1, from, 2, to, All, {from, to}]);
  close[{q0_, f0_}] := {attach[newid[], {q0, #}, 1], Last@attach[f0, {q0, #}, 2]}&@newid[];
  close[symb[a_]] := attach[Sequence @@ ConstantArray[newid[], 2], All, a];
  chain[{q0_, f0_}, {q1_, f1_}] := (attach[f0, q1]; {q0, f1});
  chain[{q0_, f0_}, symb[a_]] := {q0, attach[f0, newid[], 2, a]};
  socket[{q0_, f0_}, {q1_, f1_}] := {attach[q0, q1, 1], attach[f1, f0, 2]};
  socket[{q0_, f0_}, symb[a_]] := attach[q0, f0, All, a];
  convert[REClosure[x_]] := close@convert@x;
  convert[HoldPattern[REUnion[x__]]] := Fold[socket, {newid[], newid[]}, convert /@ {x}];
  convert[HoldPattern[REConcat[x__]]] := Fold[chain, ConstantArray[newid[], 2], convert /@ {x}];
  convert[a_] := symb[a];
  (*Algorithm start*)
  NFA["states" -> states, "initial" -> {#1}, "terminal" -> {#2}]& @@ MapThread[
    (states[#1] = #2[states[#1], True]; #1)&, {convert[regex], {SetInitial, SetTerminal}}]
];

glushkovNFA[r_] := With[{e = LinearizeRE[r, 2]},
  With[{P = pSet@e, D = dSet@e, F = fSet@e},
    NFA[
      Append[GroupBy[F, {Last@*First -> Last, First -> Last}, Union],
        1 -> GroupBy[P, First -> Last, Union]],
      {1},
      applyIf[REMatchQ[Epsilon, r],
        Append[1],
        D[[All, 2]]]]]
];

validNFATransitionsQ[<|(_ -> _List) ...|>] = True;
validNFATransitionsQ[x_] /; Message[NFAState::invtrs, x] = False;

primeGrids[ram_SparseArray] := primeGrids[Normal@ram];
primeGrids[ram_] := With[{primes = CreateDataStructure["LinkedList"],
  queue =
    CreateDataStructure["Queue", Map[List, Position[ram, 1], {2}]],
  newRows =
    Complement[
      Catenate@
        Position[ram[[All, #2]], ConstantArray[1, Length@#2], {1},
          Heads -> False], #1] &,
  newCols =
    Complement[
      Catenate@
        Position[Transpose@ram[[#1, All]],
          ConstantArray[1, Length@#1], {1}, Heads -> False], #2] &},
  Module[{g, enqueue},
    enqueue[x_] := (enqueue[x] = Null; queue["Push", x]);
    While[! queue["EmptyQ"],
      g = queue["Pop"];
      With[{i = First@g, j = Last@g, rows = newRows @@ g,
        cols = newCols @@ g},
        If[SameQ[{}, rows, cols],
          primes["Append", g],
          (If[rows =!= {}, enqueue[{Union[i, {#}], j}] & /@ rows];
          If[cols =!= {}, enqueue[{i, Union[j, {#}]}] & /@ cols])]
      ]];
    Normal@primes
  ]];

PackageExport["MinimizeNFA"]
MinimizeNFA::usage = "MinimizeNFA[nfa] finds an equivalent NFA with fewer states than the original through exhaustive search using the Kameda-Weiner algorithm.
If a smaller NFA does not exist, the original is returned.";
MinimizeNFA[nfa_?NFAQ] :=
  Module[{B, nfaB, ram, rows, isCover, states, idxs, initidx, termidxs,
    grids, tryMakeLegitNFA, res},
    B = ToDFA[nfa, Method -> "MinimalSubset"];
    nfaB = NFA[B];
    ram = Table[Boole[IntersectingQ[p, q]],
      {p, rows = DeleteCases[IDs[B], {}]},
      {q, DeleteCases[
        IDs[ToDFA[FAReversal@nfa,
          Method -> "MinimalSubset"]], {}]}];
    states = Values@KeyDrop[States@B, {{}}];
    idxs = PositionIndex[rows ~ Append ~ {}][[All, 1]];
    initidx = idxs[First@IDs[B, "Initial"]];
    termidxs = Lookup[idxs, IDs[B, "Terminal"]];
    grids = primeGrids[ram];
    PrintTemporary[Length@grids, " prime grids computed."];
    With[{goals = Position[ram, 1]},
      isCover[x_] := ContainsAll[Catenate[Tuples /@ x], goals]];

    tryMakeLegitNFA[subset_] := If[! isCover[subset], $Failed,
      Module[{selps, newinits, newstates, newterms,
        f = With[{rang = rangeOver[subset], halfcov = subset[[All, 1]]},
          AssociationMap[
            Function[{z}, Select[rang, MemberQ[halfcov[[#]], z] &]],
            Range[Length@rows + 1]]]},
        newinits = f@initidx;
        {newstates, newterms} = Reap[Table[
          NFAState[i,
            Merge[
              Transitions /@
                states[[selps = Select[rangeOver[f], MemberQ[f[#], i] &]]],
              Intersection @@ (f@*idxs /@ #) &],
            {MemberQ[newinits, i],

              If[ContainsOnly[selps, termidxs], Sow[i, "terms"]; True,
                False]}],
          {i, Length@subset}], "terms"];
        With[{intRuleNFA = NFA[
          "states" -> newstates,
          "initial" -> newinits,
          "terminal" -> First[newterms, {}],
          "alphabet" -> LanguageAlphabet[B]]},
          If[EquivalentFAQ[intRuleNFA, nfaB],
            intRuleNFA, $Failed]]]];

    Catch[
      Do[throwIf[Not@*FailureQ,
        Quiet@ParallelTry[tryMakeLegitNFA, Subsets[grids, {i}]]],
        {i,
          Range[Ceiling[Log2[StateCount@B]],
            Min[Length@grids, StateCount[nfa] - 1, StateCount[B] - 1]]}];
      First@MinimalBy[{FAExpression@nfa, nfaB}, StateCount]]];

PackageExport["ReduceNFA"]
ReduceNFA::usage = "ReduceNFA[nfa] attempts to return an equivalent NFA with fewer states using a simulated annealing heuristic.
Results are non-deterministic, and if a smaller equivalent NFA is not found, the original is returned.";
Options[ReduceNFA] = {MaxIterations -> 250};
ReduceNFA[nfa_?NFAQ] :=
  Module[{B, nfaB, ram, rows, goals, isCover, states, idxs, initidx,
    termidxs, grids, tryMakeLegitNFAFromIdxs, res, goalComplement,
    feasibleNeighbor, whereIs, optimizeCover, covers, imax, iterations,
    flipProb = 0.05},
    iterations = OptionValue[MaxIterations];
    B = ToDFA[nfa, Method -> "MinimalSubset"];
    nfaB = NFA[B];
    ram = Table[Boole[IntersectingQ[p, q]],
      {p, rows = DeleteCases[IDs[B], {}]},
      {q, DeleteCases[
        IDs[ToDFA[FAReversal@nfa,
          Method -> "MinimalSubset"]], {}]}];
    states = Values@KeyDrop[States@B, {{}}];
    idxs = PositionIndex[rows ~ Append ~ {}][[All, 1]];
    initidx = idxs[First@IDs[B, "Initial"]];
    termidxs = Lookup[idxs, IDs[B, "Terminal"]];
    grids = primeGrids[ram];
    goals = Position[ram, 1];
    isCover[x_] :=
      And @@ MapThread[ContainsAll, {Join[Sequence @@ x, 2], goals}];
    whereIs = Module[{i = 1},
      Association@Last@Reap[
        Scan[With[{j = i++}, Outer[Sow[j, {{##}}] &, Sequence @@ #]] &,
          grids],
        _, Rule]];

    feasibleNeighbor[vec_] := Module[
      {newvec =
        Unitize[vec -
          RandomChoice[{flipProb, 1 - flipProb} -> {1, 0}, Length@vec]],
        comp, i = 0, bit},
      comp =
        Complement[goals, Catenate[Tuples /@ Pick[grids, newvec, 1]]];
      While[Length@comp > 0,
        bit = RandomChoice[whereIs[RandomChoice@comp]];
        newvec[[bit]] = 1;
        comp = Complement[comp, Tuples@grids[[bit]]]];
      newvec
    ];

    optimizeCover[
      initSolution : _?VectorQ :
        feasibleNeighbor@ConstantArray[0, Length@grids],
      initTemp : _?NumericQ : 10,
      alpha : _?(Between[{0, 1}]) : 0.9] :=
      Module[{k = 0, temp = initTemp,
        solution = initSolution, cost = Total@initSolution,
        bestSolutions = CreateDataStructure["HashSet"], bestCost,
        newSolution, newCost},
        bestSolutions["Insert", solution];
        bestCost = cost;
        While[k++ < iterations,
          newSolution = feasibleNeighbor[solution];
          newCost = Total@newSolution;
          If[newCost < bestCost,
            bestSolutions["RemoveAll"];
            bestCost = newCost
          ];
          If[newCost == bestCost,
            bestSolutions["Insert", newSolution]];
          If[newCost <= cost ||
            RandomReal[] <= Quiet@Exp[-(newCost - cost) / temp],
            solution = newSolution];
          temp *= alpha
        ];
        {bestCost, Normal@bestSolutions}
      ];

    tryMakeLegitNFAFromIdxs[is_] :=
      Module[{cover = Pick[grids, is, 1], selps, newinits, newstates,
        newterms, f},
        f = With[{rang = rangeOver[cover], halfcov = cover[[All, 1]]},
          AssociationMap[
            Function[{z}, Select[rang, MemberQ[halfcov[[#]], z] &]],
            Range[Length@rows + 1]]];
        newinits = f@initidx;
        {newstates, newterms} = Reap[Table[
          NFAState[i,
            Merge[
              Transitions /@
                states[[selps = Select[rangeOver[f], MemberQ[f[#], i] &]]],
              Intersection @@ (f@*idxs /@ #) &],
            {MemberQ[newinits, i],
              If[ContainsOnly[selps, termidxs], Sow[i, "terms"]; True,
                False]}],
          {i, Length@cover}], "terms"];
        With[{intRuleNFA = NFA[
          "states" -> newstates,
          "initial" -> newinits,
          "terminal" -> First[newterms, {}],
          "alphabet" -> LanguageAlphabet[B]]},
          If[EquivalentFAQ[intRuleNFA, nfaB], intRuleNFA, $Failed]]];

    Quiet@LaunchKernels[];
    imax = Min[Length@grids, StateCount[nfa] - 1, StateCount[nfaB] - 1];
    Catch[
      Do[throwIf[Not@*FailureQ,
        Quiet@ParallelTry[tryMakeLegitNFAFromIdxs, covs]],
        {covs,
          SortBy[ParallelTable[optimizeCover[], {Max[$KernelCount, 1]}],
            First][[All, 2]]}];
      First@MinimalBy[{FAExpression@nfa, nfaB}, StateCount]]]