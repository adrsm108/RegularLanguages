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
(* Exported Functions *)

PackageExport["NFAState"]
NFAState::usage = "\
NFAState[q$, <|a$ -> {q$1, q$2, $$}, $$|>] represents the nonterminal state q$ in an NFA with transitions \
\[Delta]$:40q$, a$) = {q$1, q$2, $$}, $$
NFAState[$$, True] represents a terminal state.
NFAState[$$, {init$, term$}] represents a state which is initial if init$ is True, and terminal if term$ is True.
NFAState[q$, \[Delta]$, $$][a$] gives the transition \[Delta]$:40q$, a$).
* Keys[NFAState[q$, \[Delta]$, $$]] is equivalent to Keys[\[Delta]$].
* Values[NFAState[q$, \[Delta]$, $$]] is equivalent to Values[\[Delta]$].
";
NFAState::invtrs = "The expression `1` is not a valid NFA transition table. A valid table maps symbols to lists of state ids.";
NFAState[id_, d : {(_ -> _List) ...}, p___] :=
  NFAState[id, Association @@ d, p];
NFAState[DFAState[id_, d_?AssociationQ, p___]] :=
  NFAState[id, List /@ d, p];
NFAState[id_, d_Association?validNFATransitionsQ, ___][a_] :=
  Lookup[d, Key @ a, {}];
NFAState /: MakeBoxes[s : NFAState[_, _Association?validNFATransitionsQ, ___],
  form : (StandardForm | TraditionalForm)] := makeStateSummaryBoxes[s, form];
NFAState /: Keys[NFAState[_, d_, ___]] := Keys @ d;
NFAState /: Values[NFAState[_, d_, ___]] := Values @ d;
NFAState /: Normal[NFAState[_, d_, ___]] := Normal @ d;

PackageExport["NFAQ"]
NFAQ::usage = "NFAQ[A$] yields True if A$ is a valid NFA.";
NFAQ[NFA[_?nfaAscQ]] = True;
NFAQ[g_Graph] := NFAQ[Quiet @ AnnotationValue[g, "Automaton"]];
NFAQ[_] = False;

PackageExport["NFA"]
NFA::usage = "\
The head NFA represents a nondeterministic finite automaton.
NFA[{q$1 -> t$1, q$2 -> t$2, $$}, {inits$$}, {terms$$}] specifies an NFA with states \
q$1, q$2, $$ initial states inits$, and terminal states terms$, where each t$i is a list or association of the form \
{a$ -> {s$1, s$2, $$}, $$} representing transitions \[Delta]$:40q$i, a$) = {s$1, s$2, $$}.
* Not all transitions must be explicitly specified; on any symbol a$ for which there is no key in t$i, it is assumed \[Delta]$:40q$i, a$) = { }.
* Not all states must be explicitly specified; states without keys are assumed to have empty transition sets for all symbols.
NFA[spec$$][{a$1, a$2, $$}] returns True if the given NFA accepts the string of symbols a$(1)a$(2)$$
NFA[spec$$][{symbs$$}, All] returns the sequence of transitions on the given symbols as a list of sets of states.
NFA[spec$$][{symbs$$}, spec$] returns a subset of the transition sequence, where spec$ is any sequence specification.";
NFA[OrderlessPatternSequence[
  "states" -> states : <|(_ -> _NFAState) ..|>,
  "initial" -> initial_List,
  "terminal" -> terminal_List,
  Optional[
    "alphabet" -> alphabet : (_List | Automatic),
    "alphabet" -> Automatic]]] :=
  NFA[Association @@ {
    "states" -> states,
    "initial" -> initial,
    "terminal" -> terminal,
    "alphabet" -> unlessAutomatic[alphabet, Union @@ (Keys /@ states)]
  }];
NFA[(List | Association)[stateRules : ((_ -> KeyValuePattern[{}]) ...)], initial_List, terminal_List] :=
  Check[
    With[{states = Association[{stateRules} /. {
      HoldPattern[id_ -> tf : KeyValuePattern[{}]] :>
        (id ->
          NFAState[
            id,
            Sort /@ KeySortBy[Association @ tf, {UnsameQ[Epsilon, #]&, Identity}],
            {
              MemberQ[initial, id],
              MemberQ[terminal, id]
            }
          ]
        )
    }]},
      NFA[
        "states" -> KeySort @ Join[states,
          AssociationMap[NFAState[#, <||>, {MemberQ[initial, #], MemberQ[terminal, #]}] &,
            Complement[
              Union[Flatten[Map[Values, states, {0, 1}], 2], initial, terminal],
              Keys @ states]]],
        "initial" -> Sort @ initial,
        "alphabet" -> Automatic,
        "terminal" -> Sort @ terminal]],
    $Failed,
    {NFAState::invtrs}];
NFA[nfa_NFA] := nfa; (* Evaporate, don't ask questions *)
NFA[A_?FAQ] := ToNFA @ A;  (* Automatically cast other automata *)
NFA /: Graph[nfa : NFA[_?nfaAscQ], opts : OptionsPattern[]] := FAGraph[nfa, opts];
NFA /: Graph3D[nfa_NFA?NFAQ, opts : OptionsPattern[]] := FAGraph3D[nfa, opts];
(*Annotate[
  Graph3D[toGraph[nfa, filteredOptionSequence[{opts}, {Graph, toGraph}, Graph3D]],
    filteredOptionSequence[{opts}, Graph3D],
    *)(*        Lighting -> "Neutral",*)(*
      EdgeShapeFunction -> GraphElementData[{"Arrow", "ArrowSize" -> 0.015}]],
    "Automaton" -> nfa];*)
NFA /: ToRules[nfa : NFA[_?nfaAscQ]] :=
  Sort @ Normal[
    Normal @* StateTransitions /@
      States @ nfa
  ];
NFA /: MakeBoxes[nfa : NFA[asc_?nfaAscQ], form : (StandardForm | TraditionalForm)] :=
  makeAutomatonSummaryBoxes[nfa, form];
(nfa_NFA?NFAQ)[w_List] := With[{states = States @ nfa},
  ContainsAny[StateNames[nfa, "Terminal"],
    Fold[EpsilonClosure[Union @@ Through[Lookup[states, #1][#2]],
      states] &,
      EpsilonClosure[StateNames[nfa, "Initial"], states], w]]];
(nfa_NFA?NFAQ)[w_List, All] := With[{states = States @ nfa},
  FoldList[
    EpsilonClosure[Union @@ Through[Lookup[states, #1][#2]], states] &,
    EpsilonClosure[StateNames[nfa, "Initial"], states], w]];
(nfa_NFA?NFAQ)[w_List, n_Integer?NonNegative] := nfa[Take[w, n], All];
(nfa_NFA?NFAQ)[w_List, n_] := Take[nfa[w, All], n];
(nfa_NFA?NFAQ)[w_String, args___] := nfa[Characters @ w, args];

(* ::Subsection:: *)
(* Constructors *)

PackageExport["RandomNFA"]
RandomNFA::usage = "\
RandomNFA[{states$$}, {symbols$$}] creates a random NFA with states {states$$} and alphabet {symbols$$}.
RandomNFA[n$, k$] creates a random NFA with n$ states on an alphabet of k$ symbols.
* Either n$ or k$ can be a list, as in the above case.
* Default state ids are 1, 2, $$, n$
* Default symbols are 'a', 'b', $$ (ascii character range 97 to 97 + k) if k \[LessEqual] 26, or 'x1', 'x2', $$, 'xk' otherwise.
* When a function f$ is provided for the 'StatesFunction' option, the state ids will be Array[f$, n$].
* When a function g$ is provided for the 'AlphabetFunction' option, the alphabet will be Array[g$, k$].
RandomNFA[$$, max$n, max$k] specifies each state of the returned NFA should transition to no more than max$n states \
on any one symbol, and define transitions for no more than max$k symbols.
* Non-integer values given for max$n and max$k are interpreted as factors of n$ and k$ respectively.

Options:
'InitialStates' -> _Integer | _Real | _List
Initial states in the returned NFA
* l$_List: initial states are exactly the states in l$.
* m$_Integer: m$ randomly selected initial states.
* x$_Real: Ceiling[xn$] randomly selected initial states, where n$ is the total number of states.

'TerminalStates' -> _Integer | _Real | _List
Terminal states in the returned NFA
* l$_List: terminal states are exactly the states in l$.
* m$_Integer: m$ terminal states.
* x$_Real: Ceiling[xn$] terminal states, where n$ is the total number of states.

'StatesFunction' -> _
Function to generate state names, applied to the list of states or Range[n$]

'AlphabetFunction' -> _
Function to generate alphabet symbols, applied to the list of symbols or Range[k$]

'AllStatesReachable' -> True | False
Whether the returned NFA must form a (weakly) connected graph.

'EpsilonProbability' -> _?(Between[{0, 1}])
The probability of a given state generating an Epsilon-transition.";
RandomNFA::nstates = "Cannot take `1` state names from `2`.";
RandomNFA::nsymbs = "Cannot take `1` symbols from `2`.";
Options[RandomNFA] = {
  "EpsilonProbability" -> 0.01,
  "TerminalStates" -> 0.3,
  "InitialStates" -> 1,
  "AllStatesReachable" -> True,
  "AlphabetFunction" -> Automatic,
  "StatesFunction" -> Automatic
};
OptionChecks[RandomNFA] = {
  "EpsilonProbability" -> _?(Between[{0, 1}]),
  "InitialStates" -> _List | _Integer | _Real,
  "TerminalStates" -> _List | _Integer | _Real,
  "AllStatesReachable" -> True | False
};
RandomNFA[
  statesin : (_List | _Integer),
  alphin : (_List | _Integer),
  pn : (Automatic | _?Positive) : Automatic,
  pk : (Automatic | _?Positive) : Automatic,
  opts : OptionsPattern[RandomNFA]?(validQ @ RandomNFA)
] :=
  With[{
    n = when[_Integer, statesin, Length @ statesin],
    k = when[_Integer, alphin, Length @ alphin],
    initOptVal = OptionValue["InitialStates"],
    termOptVal = OptionValue["TerminalStates"]
  },
    With[{
      maxstates = intOrMult[unlessAutomatic[pn, Ceiling @ Min[Log[n + 1], 0.18 n]], n],
      alph = makeAlphabet[alphin, OptionValue["AlphabetFunction"]],
      ids = makeStateIDs[statesin, OptionValue["StatesFunction"]]},
      Module[{reachable, unreachable,
        states = AssociationThread[ids,
          MapThread[
            Association @ If[#2 <= 0, #1,
              Append[#1, Epsilon -> RandomSample[ids, Min[#2, maxstates]]]]&,
            {Thread[# -> randomSubset[ids, {1, Min[n, maxstates]}, Length@#]]&
              /@ randomSubset[alph, {0, Min[k, intOrMult[unlessAutomatic[pk, Ceiling @ Log[k + 1]], k]]}, n],
              RandomVariate[BinomialDistribution[n, OptionValue["EpsilonProbability"]], n]}]],
        inits = when[_List, initOptVal, RandomSample[ids, UpTo[intOrMult[initOptVal, n]]]]},
        If[OptionValue["AllStatesReachable"],
          unreachable = Complement[ids, reachable = TransitiveClosure[inits, states]];
          While[Length @ unreachable > 0,
            (If[KeyExistsQ[states[#1], #2],
              AppendTo[states[#1, #2], First @ unreachable],
              states[#1, #2] = {First @ unreachable}])&
              @@ (RandomChoice /@ {reachable, alph});
            unreachable = Complement[unreachable,
              reachable = Union[reachable, TransitiveClosure[{First @ unreachable}, states]]]]];
        NFA[
          states,
          inits,
          when[_List,
            termOptVal,
            RandomSample[ids, UpTo[intOrMult[termOptVal, n]]]]
        ]]]
  ];
(*RandomNFA[*)
(*  statesin : (_List | _Integer),*)
(*  alphin : (_List | _Integer),*)
(*  opts : OptionsPattern[RandomNFA]*)
(*] := RandomNFA[statesin, alphin, Automatic, Automatic, opts];*)

PackageExport["NthFromLastNFA"]
NthFromLastNFA::usage = "NthFromLastNFA[n] returns an NFA accepting the language of strings over {\"a\", \"b\"} whose n-th from last character is \"a\"
NthFromLastNFA[n, c, {c1, c2, ...}] returns an NFA accepting the language of strings over {c1, c2, ...} whose n-th from last character is c.";
NthFromLastNFA[n_] := NthFromLastNFA[n, "a", {"a", "b"}];
NthFromLastNFA[n_, a_, alph_] := NFA[
  "states" -> Association[
    0 -> NFAState[0, Association[Thread[DeleteCases[alph, a] -> {0}, List, 1], {a -> {0, 1}}], {True, False}],
    Table[i -> NFAState[i, Association @ Thread[alph -> {i + 1}, List, 1], {False, False}], {i, 1, n - 1}],
    n -> NFAState[n, <||>, {False, True}]],
  "initial" -> {0},
  "terminal" -> {n},
  "alphabet" -> alph
];

PackageExport["ToNFA"]
ToNFA::usage = "\
ToNFA[A] converts the automaton A into an NFA.
ToNFA[regex] converts the regular expression regex into an NFA.

Options
Method -> \"Glushkov\" | \"Thompson\"
The algorithm to use when converting a regular expression to an NFA.
  - \"Glushkov\": Glushkov construction
    - Results in an Epsilon-free NFA with n + 1 states, where n is the number of symbols in the original regular expression.
  - \"Thompson\": Thompson construction" ;
OptionChecks[ToNFA] = { Method -> "Glushkov" | "Thompson" };
Options[ToNFA] = {Method -> "Glushkov"};
ToNFA[nfa_NFA, OptionsPattern[ToNFA]?(validQ @ ToNFA)] := nfa;
ToNFA[DFA[asc_?dfaAscQ], OptionsPattern[ToNFA]?(validQ @ ToNFA)] :=
  NFA[MapAt[NFAState, KeyDrop[asc, {"icon"}], {{"states", All}}]];
ToNFA[g_?FAGraphQ, OptionsPattern[ToNFA]?(validQ @ ToNFA)] := ToNFA[FAExpression @ g];
ToNFA[EmptyLanguage, OptionsPattern[ToNFA]?(validQ @ ToNFA)] = NFA[{}, {1}, {}];
ToNFA[Epsilon, OptionsPattern[ToNFA]?(validQ @ ToNFA)] = NFA[{}, {1}, {1}];
ToNFA[RE[x_]] := NFA[
  "states" -> <|
    1 -> NFAState[1, <|x -> {2}|>, {True, False}],
    2 -> NFAState[2, <||>, {False, True}] |>,
  "initial" -> {1},
  "terminal" -> {2},
  "alphabet" -> {x}
];
ToNFA[r_?CompoundREQ, OptionsPattern[ToNFA]?(validQ @ ToNFA)] :=
  Switch[OptionValue[Method],
    "Glushkov", glushkovNFA[r],
    "Thompson", thompsonNFA[r]];


PackageExport["MinimizeNFA"]
MinimizeNFA::usage = "\
MinimizeNFA[nfa] attempts to find an equivalent NFA with fewer states than the original.
If a NFA with fewer states is not found, the original is returned.

Options:

Method -> \"Exhaustive\" | \"SimulatedAnnealing\" | Automatic
The method to use for minimization.
  - \"Exhaustive\": Deterministic, exhaustive search.
    - State count of a Returned NFA is guarenteed to be minimal
    - The poor scaling of this algorithm renders it unsuitable for all but the simplest inputs.
  - \"SimulatedAnnealing\": Probabilistic local search based on simulated annealing.
    - Heuristic optimization suitable for small to medium NFAs.
    - Non-deterministic. In general, obtaining the same result on different runs is not to be expected
  - Automatic: Choose a suitable method automatically based on the number of prime grids identified.

MaxIterations -> _Integer?Positive
Maximum number of annealing steps to perform before returning when Method -> \"SimulatedAnnealing\".";
Options[MinimizeNFA] = { Method -> Automatic, MaxIterations -> 250 };
OptionChecks[MinimizeNFA] = { Method -> Automatic | "SimulatedAnnealing" | "Exhaustive", MaxIterations -> _Integer?Positive };
MinimizeNFA[nfa_?NFAQ, OptionsPattern[MinimizeNFA]?(validQ @ MinimizeNFA)] :=
  Module[{B, nfaB, ram, rows, isCover, states, idxs, initidx, termidxs, pGrids, tryMakeLegitNFA, res},
    B = ToDFA[nfa, Method -> "Minimal", "StateNames" -> "Subset"];
    nfaB = NFA[B];
    ram = Table[Boole[IntersectingQ[p, q]],
      {p, rows = DeleteCases[StateNames[B], {}]},
      {q, DeleteCases[
        StateNames[ToDFA[FAReversal @ nfa, Method -> "Minimal",
          "StateNames" -> "Subset"]], {}]}];
    states = Values @ KeyDrop[States @ B, {{}}];
    idxs = PositionIndex[rows ~ Append ~ {}][[All, 1]];
    initidx = idxs[First @ StateNames[B, "Initial"]];
    termidxs = Lookup[idxs, StateNames[B, "Terminal"]];
    pGrids = primeGrids[ram];
    PrintTemporary[Length @ pGrids, " prime grids computed."];

    Switch[OptionValue[Method],
      "Exhaustive", reduceGridsExhaustive,
      "SimulatedAnnealing", reduceGridsSA,
      Automatic,
      With[ {nGrids = Length @ pGrids},
        If[NSum[Binomial[nGrids, k],
          {k, Ceiling[Log2[StateCount @ B]],
            Min[nGrids, StateCount[nfa] - 1, StateCount[B] - 1]}]
          <= 256 , reduceGridsExhaustive, reduceGridsSA]]
    ][nfa, B, nfaB, ram, rows, states, idxs, initidx, termidxs, pGrids, OptionValue[MaxIterations]]
  ];

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
    states[from] = AddTransitions[states[from], with -> when[_List, to, {to}]];
    unless[ret, 1, from, 2, to, All, {from, to}]);
  close[{q0_, f0_}] := {attach[newid[], {q0, #}, 1], Last @ attach[f0, {q0, #}, 2]}&@newid[];
  close[symb[a_]] := attach[Sequence @@ ConstantArray[newid[], 2], All, a];
  chain[{q0_, f0_}, {q1_, f1_}] := (attach[f0, q1]; {q0, f1});
  chain[{q0_, f0_}, symb[a_]] := {q0, attach[f0, newid[], 2, a]};
  socket[{q0_, f0_}, {q1_, f1_}] := {attach[q0, q1, 1], attach[f1, f0, 2]};
  socket[{q0_, f0_}, symb[a_]] := attach[q0, f0, All, a];
  convert[REClosure[x_]] := close @ convert @ x;
  convert[HoldPattern[REUnion[x__]]] := Fold[socket, {newid[], newid[]}, convert /@ {x}];
  convert[HoldPattern[REConcat[x__]]] := Fold[chain, ConstantArray[newid[], 2], convert /@ {x}];
  convert[a_] := symb[a];
  (*Algorithm start*)
  NFA["states" -> states, "initial" -> {#1}, "terminal" -> {#2}]& @@ MapThread[
    (states[#1] = #2[states[#1], True]; #1)&, {convert[regex], {SetInitial, SetTerminal}}]
];

glushkovNFA[r_] := With[{e = LinearizeRE[r, 2]},
  With[{P = pSet @ e, D = dSet @ e, F = fSet @ e},
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

primeGrids[ram_SparseArray] := primeGrids[Normal @ ram];
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
        Position[Transpose @ ram[[#1, All]],
          ConstantArray[1, Length@#1], {1}, Heads -> False], #2] &},
  Module[{g, enqueue},
    enqueue[x_] := (enqueue[x] = Null; queue["Push", x]);
    While[! queue["EmptyQ"],
      g = queue["Pop"];
      With[{i = First @ g, j = Last @ g, rows = newRows @@ g,
        cols = newCols @@ g},
        If[SameQ[{}, rows, cols],
          primes["Append", g],
          (If[rows =!= {}, enqueue[{Union[i, {#}], j}] & /@ rows];
          If[cols =!= {}, enqueue[{i, Union[j, {#}]}] & /@ cols])]
      ]];
    Normal @ primes
  ]];


reduceGridsExhaustive[nfa_, B_, nfaB_, ram_, rows_, states_, idxs_, initidx_, termidxs_, grids_, iterations_] :=
  Module[{isCover, tryMakeLegitNFA, nCandidates, maxi,
    mini = Ceiling[Log2[StateCount @ B]],
    nGrids = Length @ grids},
    With[{goals = Position[ram, 1]},
      isCover[x_] := ContainsAll[Catenate[Tuples /@ x], goals]];

    tryMakeLegitNFA[subset_] := If[!isCover[subset], $Failed,
      Module[{selps, newinits, newstates, newterms,
        f = With[{rang = rangeOver[subset], halfcov = subset[[All, 1]]},
          AssociationMap[
            Function[{z}, Select[rang, MemberQ[halfcov[[#]], z] &]],
            Range[Length @ rows + 1]]]},
        newinits = f @ initidx;
        {newstates, newterms} = Reap[Table[
          NFAState[i,
            Merge[
              StateTransitions /@ states[[selps = Select[rangeOver[f], MemberQ[f[#], i] &]]],
              Intersection @@ (f@*idxs /@ #) &] ,
            {MemberQ[newinits, i],
              If[ContainsOnly[selps, termidxs],
                (Sow[i, "terms"]; True),
                False]}],
          {i, Length @ subset}], "terms"];
        With[{intRuleNFA = NFA[
          "states" -> newstates,
          "initial" -> newinits,
          "terminal" -> First[newterms, {}],
          "alphabet" -> LanguageAlphabet[B]]},
          If[EquivalentFAQ[intRuleNFA, nfaB], intRuleNFA, $Failed]]]];

    maxi = Min[nGrids, StateCount[nfa] - 1, StateCount[B] - 1];
    nCandidates = NSum[Binomial[nGrids, k], {k, mini, maxi}];
    PrintTemporary[nCandidates, " candidate NFAs must be checked for equivalence."];
    Catch[Do[
      throwIf[Not@*FailureQ, Quiet @ ParallelTry[tryMakeLegitNFA, Subsets[grids, {i}]]];
      PrintTemporary["No Equivalent NFAs with ", i, If[i === 1, " state", " states"],
        " exist. (", (nCandidates -= Binomial[nGrids, i]), " canditates remain)"]
      , {i, Range[mini, maxi]}];
    First @ MinimalBy[{FAExpression @ nfa, nfaB}, StateCount]]
  ];

reduceGridsSA[nfa_, B_, nfaB_, ram_, rows_, states_, idxs_, initidx_, termidxs_, grids_, iterations_] :=
  Module[{isCover, tryMakeLegitNFAFromIdxs, res, goalComplement,
    feasibleNeighbor, whereIs, optimizeCover, covers, imax,
    flipProb = 0.05, goals = Position[ram, 1]},

    isCover[x_] := And @@ MapThread[ContainsAll, {Join[Sequence @@ x, 2], goals}];

    whereIs = Module[{i = 1},
      Association @ Last @ Reap[
        Scan[With[{j = i++}, Outer[Sow[j, {{##}}] &, Sequence @@ #]] & , grids],
        _,
        Rule]];

    feasibleNeighbor[vec_] :=
      Module[{comp, bit,
        newvec = Unitize[
          vec - RandomChoice[{flipProb, 1 - flipProb} -> {1, 0}, Length @ vec]],
        i = 0},
        comp = Complement[goals, Catenate[Tuples /@ Pick[grids, newvec, 1]]];
        While[Length @ comp > 0,
          bit = RandomChoice[whereIs[RandomChoice @ comp]];
          newvec[[bit]] = 1;
          comp = Complement[comp, Tuples @ grids[[bit]]]];
        newvec
      ];

    With[{initTemp = 10, alpha = 0.9},
      optimizeCover[] :=
        Module[{newSolution, newCost, solution, cost, bestCost,
          k = 0,
          temp = initTemp,
          initSolution = feasibleNeighbor @ ConstantArray[0, Length @ grids],
          bestSolutions = CreateDataStructure["HashSet"]},
          solution = initSolution;
          cost = Total @ initSolution;
          bestSolutions["Insert", solution];
          bestCost = cost;
          While[k++ < iterations,
            newSolution = feasibleNeighbor[solution];
            newCost = Total @ newSolution;
            If[newCost < bestCost,
              bestSolutions["RemoveAll"];
              bestCost = newCost];
            If[newCost == bestCost,
              bestSolutions["Insert", newSolution]];
            If[newCost <= cost || RandomReal[] <= Quiet @ Exp[-(newCost - cost) / temp],
              solution = newSolution];
            temp *= alpha ];

          {bestCost, Normal @ bestSolutions}
        ]
    ];

    tryMakeLegitNFAFromIdxs[is_] :=
      Module[{selps, newinits, newstates, newterms, f,
        cover = Pick[grids, is, 1]},
        f = With[{rang = rangeOver[cover], halfcov = cover[[All, 1]]},
          AssociationMap[
            Function[{z}, Select[rang, MemberQ[halfcov[[#]], z] &]]
            , Range[Length @ rows + 1]]];
        newinits = f @ initidx;
        {newstates, newterms} = Reap[Table[
          NFAState[i,
            Merge[
              StateTransitions /@

                states[[selps = Select[rangeOver[f], MemberQ[f[#], i] &]]],
              Intersection @@ (f@*idxs /@ #) &],
            {MemberQ[newinits, i],

              If[ContainsOnly[selps, termidxs], Sow[i, "terms"]; True,
                False]}],
          {i, Length @ cover}], "terms"];
        With[{intRuleNFA = NFA[
          "states" -> newstates,
          "initial" -> newinits,
          "terminal" -> First[newterms, {}],
          "alphabet" -> LanguageAlphabet[B]]},

          If[EquivalentFAQ[intRuleNFA, nfaB], intRuleNFA, $Failed]]];

    Quiet @ LaunchKernels[];
    imax =
      Min[Length @ grids, StateCount[nfa] - 1, StateCount[nfaB] - 1];
    Catch[
      Do[throwIf[Not@*FailureQ,
        Quiet @ ParallelTry[tryMakeLegitNFAFromIdxs, covs]],
        {covs,
          SortBy[ParallelTable[optimizeCover[], {Max[$KernelCount, 1]}],
            First][[All, 2]]}];
      First @ MinimalBy[{FAExpression @ nfa, nfaB}, StateCount]]
  ]
