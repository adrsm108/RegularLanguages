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
(* Package Export *)
(* ::Subsection:: *)
(* NFA *)
PackageExport["NFA"]
NFA::usage = "\
The head NFA represents a nondeterministic finite automaton.
NFA[{q$1 -> {a$1 -> {s$11, s$12, $$}, $$}, $$}, {inits$$}, {terms$$}] specifies an NFA with initial states inits$, terminal states terms$, \
and transitions {q$1, a$1} -> {s$11, s$12, $$}, $$, where q$i and s$ij are states, and a$i are alphabet symbols.
* Not all transitions must be explicitly specified; on any symbol a$ for which there is no key in q$ -> {$$}, it is assumed {q$, a$} -> { }.
* Not all states must be explicitly specified; states without keys are assumed to have empty transition sets for all symbols.
nfa$[{a$1, a$2, $$}] returns True if the given NFA accepts the string of symbols a$1 a$2 $$.
nfa$['string$'] is equivalent to nfa$[Characters['string$']]";

(* ::Subsubsection:: *)
(* NFA DownValues *)

NFA[A_?FAQ] := ToNFA @ A;
NFA[
  rules : (List | Association)[(_ -> KeyValuePattern[{}])...],
  initial_List,
  terminal_List,
  alphabet : (_List | Automatic) : Automatic
] :=
  Check[
    Module[{trns = Association /@ Association @ rules},
      Scan[
        If[MissingQ @ trns @ #, trns[#] = <||>] &,
        Join[
          Map[
            (Union @@ Values @ #) &,
            trns,
            {0, 1}
          ],
          initial,
          terminal
        ]
      ];
      makeNFA[
        trns,
        initial,
        terminal,
        If[ListQ @ alphabet,
          Join[
            alphabet,
            Keys @ (Join @@ trns)
          ],
          alphabet
        ]
      ]
    ],
    $Failed
  ];

(* ::Subsubsection:: *)
(* NFA UpValues *)
NFA /: Graph[nfa : NFA[_?nfaAscQ], opts : OptionsPattern[]] :=
  FAGraph[nfa, opts];
NFA /: Graph3D[nfa_NFA?NFAQ, opts : OptionsPattern[]] :=
  FAGraph3D[nfa, opts];
NFA /: ToRules[nfa : NFA[_?nfaAscQ]] :=
  Normal[Normal /@ Transitions @ nfa];

(* ::Subsubsection:: *)
(* NFA SubValues *)
(nfa_NFA?NFAQ)[input : (_List | _String)] :=
  FAMatchQ[nfa, input];

(* ::Subsubsection:: *)
(* NFA FormatValues *)
NFA /: MakeBoxes[nfa : NFA[asc_?nfaAscQ], form : (StandardForm | TraditionalForm)] :=
  makeAutomatonSummaryBoxes[nfa, form];

(* ::Subsection:: *)
(* FAMatchQ *)

FAMatchQ[nfa_?NFAQ, input_List] :=
  With[{
    tf = TransitionFunction @ nfa
  },
    ContainsAny[
      States[nfa, "Terminal"],
      Fold[
        EpsilonClosure[
          Union @@ tf[#1, Key @ #2],
          tf
        ] &,
        EpsilonClosure[
          States[nfa, "Initial"],
          tf
        ],
        input
      ]
    ]
  ];

(* ::Subsection:: *)
(* TransitionSequence *)

TransitionSequence[nfa_?NFAQ, input_List, seq_ : All] :=
  Take[
    With[{tf = TransitionFunction @ nfa},
      FoldList[
        EpsilonClosure[
          Union @@ tf[#1, Key @ #2],
          tf
        ] &,
        EpsilonClosure[
          States[nfa, "Initial"],
          tf
        ],
        input
      ]
    ],
    seq
  ];

(* ::Subsection:: *)
(* NFAQ *)

PackageExport["NFAQ"]
NFAQ::usage = "NFAQ[A$] yields True if A$ is a valid NFA.";
NFAQ[NFA[_?nfaAscQ]] = True;
NFAQ[g_Graph] := NFAQ[Quiet @ AnnotationValue[g, "Automaton"]];
NFAQ[_] = False;

(* ::Subsection:: *)
(* EpsilonNFAQ *)

PackageExport["EpsilonNFAQ"]
EpsilonNFAQ::usage = "\
EpsilonNFAQ[nfa$] returns True if nfa$ is an NFA with Epsilon transitions, and False otherwise.";
EpsilonNFAQ[nfa_?NFAQ] :=
  AnyTrue[
    Transitions @ nfa,
    MatchQ[# @ Epsilon, {__}] &
  ];
EpsilonNFAQ[_] = False;

(* ::Subsection:: *)
(* RemoveEpsilonTransitions *)

PackageExport["RemoveEpsilonTransitions"]
RemoveEpsilonTransitions::usage = "\
RemoveEpsilonTransitions[nfa$] returns an equivalent NFA with no Epsilon transitions.";
RemoveEpsilonTransitions[nfa_?EpsilonNFAQ] :=
  With[
    {
      trns = Transitions @ nfa
    },
    {
      newStates = AssociationMap[
        EpsilonClosure[{#}, trns] &,
        ReachableStates @ nfa
      ]
    },
    makeNFA[
      AssociationMap[
        Merge[
          KeyDrop[
            Lookup[trns, #],
            Epsilon
          ],
          Union @ Lookup[
            newStates,
            EpsilonClosure[
              Union @@ #,
              trns
            ]
          ] &
        ] &,
        Union @ Values @ newStates
      ],
      Union @ Lookup[
        newStates,
        States[nfa, "Initial"]
      ],
      Union @ Lookup[
        newStates,
        States[nfa, "Terminal"]
      ]
    ]
  ];

(* ::Subsection:: *)
(* ToNFA *)

PackageExport["ToNFA"]
ToNFA::usage = "\
ToNFA[A$] converts the automaton A$ into an NFA$.
ToNFA[regex$] converts the regular expression regex$ into an NFA.

Options:
Method -> 'Glushkov' | 'Thompson'
The algorithm to use when converting a regular expression to an NFA.
* 'Glushkov': Glushkov construction
  - Results in an Epsilon-free NFA with n$ + 1 states, where n$ is the number of symbols in the original regular expression.
* 'Thompson': Thompson construction." ;
OptionChecks[ToNFA] = { Method -> "Glushkov" | "Thompson" };
Options[ToNFA] = {Method -> "Glushkov"};
ToNFA[nfa_NFA, ___] := nfa;
ToNFA[
  DFA[asc_?dfaAscQ],
  CheckedOptions @ ToNFA
] :=
  makeNFA[
    Map[List, asc @ "Transitions", {2}],
    asc @ "Initial",
    asc @ "Terminal",
    asc @ "Alphabet"
  ];
ToNFA[g_?FAGraphQ, opts : CheckedOptions @ ToNFA] :=
  ToNFA[FAExpression @ g, opts];
ToNFA[Epsilon, CheckedOptions @ ToNFA] =
  NFA[{}, {1}, {1}];
ToNFA[EmptyLanguage, CheckedOptions @ ToNFA] =
  NFA[{}, {1}, {}];
ToNFA[RELiteral[x_, ___], CheckedOptions @ ToNFA] :=
  NFA[{1 -> {x -> {2}}}, {1}, {2}];
ToNFA[r_?CompoundREQ, CheckedOptions @ ToNFA] :=
  Switch[OptionValue @ Method,
    "Glushkov", glushkovNFA[r],
    "Thompson", thompsonNFA[r]
  ];

PackageExport["CompleteNFA"]
CompleteNFA::usage = "\
CompleteNFA[nfa$] returns an equivalent NFA with one extra state and non-empty transition sets for every alphabet symbol in every state.";
CompleteNFA[nfa_?NFAQ] :=
  With[
    {
      sinkState = CreateUniqueState @ nfa,
      alph = LanguageAlphabet @ nfa
    },
    {
      completeAsc =
        Association @ Thread[
          DeleteCases[alph, Epsilon] -> {sinkState},
          List,
          1
        ]
    },
    makeNFA[
      Association[
        Association[completeAsc, #] & /@ Transitions @ nfa,
        sinkState -> completeAsc
      ],
      States[nfa, "Initial"],
      States[nfa, "Terminal"],
      alph
    ]
  ];

(* ::Subsection:: *)
(* MinimizeNFA *)

PackageExport["MinimizeNFA"]
MinimizeNFA::usage = "\
MinimizeNFA[nfa$] attempts to find an equivalent NFA with fewer states than the original.
* If a NFA with fewer states is not fougd, the original is returned.

Options:
Method -> 'Exhaustive' | 'SimulatedAnnealing' | Automatic
The method to use for minimization.
* 'Exhaustive': Deterministic, exhaustive search.
    - State count of a Returned NFA is guarenteed to be minimal
    - The poor scaling of this algorithm renders it unsuitable for all but the simplest inputs.
* 'SimulatedAnnealing': Probabilistic local search based on simulated annealing.
    - Heuristic optimization suitable for small to medium NFAs.
    - Non-deterministic. In general, obtaining the same result on different runs is not to be expected
* Automatic: Choose a suitable method automatically based on the number of prime grids identified.

MaxIterations -> _Integer?Positive
Maximum number of annealing steps to perform before returning when Method -> 'SimulatedAnnealing'.";
Options[MinimizeNFA] =
  {
    Method -> Automatic,
    MaxIterations -> 250
  };
OptionChecks[MinimizeNFA] =
  {
    Method -> Automatic | "SimulatedAnnealing" | "Exhaustive",
    MaxIterations -> _Integer?Positive
  };
MinimizeNFA[nfa_?NFAQ, CheckedOptions @ MinimizeNFA] :=
  Module[{B, nfaB, ram, rows, isCover, trns, idxs, initidx, termidxs, pGrids, tryMakeLegitNFA, res},
    B = ToDFA[nfa, Method -> "Minimal", "StateNames" -> "Subset"];
    nfaB = ToNFA @ B;
    ram =
      Table[
        Boole @ IntersectingQ[p, q],
        {
          p, rows = DeleteCases[States @ B, {}]
        },
        {
          q,
          DeleteCases[
            States @ ToDFA[
              FAReversal @ nfa,
              Method -> "Minimal",
              "StateNames" -> "Subset"
            ],
            {}
          ]
        }
      ];
    trns = KeyDrop[Transitions @ B, {{}}];
    (*    states = Values @ KeyDrop[States @ B, {{}}];*)
    idxs =
      PositionIndex[
        Append[rows, {}]
      ][[All, 1]];
    initidx =
      idxs @ First @ States[B, "Initial"];
    termidxs =
      Lookup[
        idxs,
        States[B, "Terminal"]
      ];
    pGrids = primeGrids @ ram;

    PrintTemporary[Length @ pGrids, " prime grids computed."];

    Switch[OptionValue @ Method,
      "Exhaustive",
      reduceGridsExhaustive,

      "SimulatedAnnealing",
      reduceGridsSA,

      Automatic,
      With[{nGrids = Length @ pGrids},
        If[
          NSum[Binomial[nGrids, k],
            {
              k,
              Ceiling @ Log2 @ StateCount @ B,
              Min[
                nGrids,
                StateCount @ nfa - 1,
                StateCount @ B - 1
              ]
            }
          ] <= 256,
          reduceGridsExhaustive,
          reduceGridsSA
        ]
      ]
    ][
      nfa,
      B,
      nfaB,
      ram,
      rows,
      trns,
      idxs,
      initidx,
      termidxs,
      pGrids,
      OptionValue @ MaxIterations
    ]
  ];

(* ::Subsection:: *)
(* RandomNFA *)

PackageExport["RandomNFA"]
RandomNFA::usage = "\
RandomNFA[{states$$}, {symbols$$}] creates a random NFA with states {states$$} and alphabet {symbols$$}.
RandomNFA[n$, k$] creates a random NFA with n$ states on an alphabet of k$ symbols.
* Either n$ or k$ can be a list, as in the above case.
* Default state names are 1, 2, $$, n$
* Default symbols are 'a', 'b', $$ (ascii character range 97 to 97 + k) if k$ \[LessEqual] 26, or 'x1', 'x2', $$, 'xk' otherwise.
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

'EpsilonFrequency' -> _?(Between[{0, 1}])
The frequency with which states contain Epsilon-transitions.";
RandomNFA::nstates = "Cannot take `1` state names from `2`.";
RandomNFA::nsymbs = "Cannot take `1` symbols from `2`.";
Options[RandomNFA] =
  {
    "EpsilonFrequency" -> 0.1,
    "TerminalStates" -> 0.3,
    "InitialStates" -> 1,
    "AllStatesReachable" -> True,
    "AlphabetFunction" -> Automatic,
    "StatesFunction" -> Automatic
  };
OptionChecks[RandomNFA] =
  {
    "EpsilonFrequency" -> _?(Between[{0, 1}]),
    "InitialStates" -> _List | _Integer | _Real,
    "TerminalStates" -> _List | _Integer | _Real,
    "AllStatesReachable" -> True | False
  };
RandomNFA[
  statesin : (_List | _Integer),
  alphin : (_List | _Integer),
  pn : (Automatic | _?Positive) : Automatic,
  pk : (Automatic | _?Positive) : Automatic,
  opts : CheckedOptions @ RandomNFA
] :=
  With[
    {
      n = IfMatch[_Integer, statesin, Length @ statesin],
      k = IfMatch[_Integer, alphin, Length @ alphin],
      initOptVal = OptionValue @ "InitialStates",
      termOptVal = OptionValue @ "TerminalStates"
    },
    {
      maxstates =
        Min[
          n,
          intOrMult[
            UnlessAutomatic[pn,
              Ceiling @ Min[
                Log[n + 1],
                0.18 n
              ]
            ],
            n
          ]
        ],
      alph = makeAlphabet[alphin, OptionValue @ "AlphabetFunction"],
      ids = makeStates[statesin, OptionValue @ "StatesFunction"]
    },
    Module[
      {
        reachable, unreachable,
        inits =
          IfMatch[
            _List,
            initOptVal,
            RandomSample[ids,
              UpTo @ intOrMult[initOptVal, n]
            ]
          ],
        trns =
          AssociationThread[
            ids,
            AssociationThread[
              # ->
                randomSubset[ids,
                  {1, maxstates},
                  Length @ #
                ]
            ] & /@
              randomSubset[alph,
                {
                  0,
                  Min[
                    k,
                    intOrMult[
                      UnlessAutomatic[pk,
                        Ceiling @ Log[k + 1]
                      ],
                      k
                    ]
                  ]
                },
                n
              ]
          ]
      },
      Do[
        trns[id, Epsilon] =
          randomSubset[ids, {1, maxstates}],
        {
          id,
          RandomSample[ids,
            RandomVariate @ BinomialDistribution[
              n,
              OptionValue @ "EpsilonFrequency"
            ]
          ]
        }
      ];
      If[OptionValue @ "AllStatesReachable",
        (
          unreachable =
            Complement[ids, reachable = ReachableStates[inits, trns]];
          While[Length @ unreachable > 0,
            With[
              {
                q = RandomChoice @ reachable,
                a = RandomChoice @ alph
              },
              If[KeyExistsQ[trns @ q, a],
                AppendTo[
                  trns[q, a],
                  First @ unreachable
                ],
                trns[q, a] = {First @ unreachable}
              ]
            ];
            reachable =
              Union[
                reachable,
                ReachableStates[{First @ unreachable}, trns]
              ];
            unreachable = Complement[unreachable, reachable];
          ]
        )
      ];
      NFA[
        trns,
        inits,
        IfMatch[_List,
          termOptVal,
          RandomSample[ids,
            UpTo @ intOrMult[termOptVal, n]
          ]
        ]
      ]
    ]
  ];


(* ::Subsection:: *)
(* NthFromLastNFA *)

PackageExport["NthFromLastNFA"]
NthFromLastNFA::usage = "\
NthFromLastNFA[n$] returns an NFA accepting the language of strings over {'a', 'b'} whose n$-th from last element is 'a'.
NthFromLastNFA[n$, a$, {symbs$$}] returns an NFA accepting the language of strings over symbs$ whose n$-th from last element is a$.
* The smallest equivalent DFA has 2^n states.";
NthFromLastNFA[n_] := NthFromLastNFA[n, "a", {"a", "b"}];
NthFromLastNFA[n_, a_, alph_] :=
  makeNFA[
    <|
      0 ->
        <|
          Thread[
            DeleteCases[alph, a] -> {0},
            List,
            1
          ],
          a -> {0, 1}
        |>,
      Table[
        i ->
          Association @ Thread[
            alph -> {i + 1},
            List,
            1
          ],
        {i, 1, n - 1}
      ],
      n -> <||>
    |>,
    {0},
    {n},
    alph
  ];

(* ::Section:: *)
(* Package Scope *)

(* ::Subsection:: *)
(* makeNFA *)
PackageScope["makeNFA"]
makeNFA::usage = "\
makeNFA[trns, inits, terms, alphabet] makes an NFA without checking options.
makeNFA[trns, inits, terms] does the same, automatically computing the alphabet from transitions.
Expects but does not check that:
  trns matches <|(_ -> <|(_ -> _List)...|>)...|>,
  inits matches _List,
  terms matches _List,
  alphabet, if given, matches _List.
Sorts arguments beforehand.";
makeNFA[trns_, inits_, terms_, alph_ : Automatic] :=
  With[
    {
      sortedTrns = KeySort[KeySort @* Map[Sort] /@ trns],
      alphabet = UnlessAutomatic[alph, Keys @ (Join @@ trns)]
    },
    NFA[
      <|
        "Transitions" -> sortedTrns,
        "States" -> Keys @ sortedTrns,
        "Initial" -> Union @ inits,
        "Terminal" -> Union @ terms,
        "Alphabet" -> Union @ alphabet
      |>
    ]
  ];

(* ::Subsection:: *)
(* nfaAscQ *)
PackageScope["nfaAscQ"]
nfaAscQ::usage = "nfaAscQ[asc] returns True if asc is a valid association where asc[\"states\"] is an association whose values are NFAStates, and asc[\"initial\"], asc[\"terminal\"], and asc[\"alphabet\"] are lists";
nfaAscQ[asc_ ] :=
  MatchQ[asc,
    KeyValuePattern @ {
      "States" -> _List,
      "Initial" -> _List,
      "Terminal" -> _List,
      "Transitions" -> <|(_ -> _Association)...|>,
      "Alphabet" -> _List
    }
  ];

(* ::Subsection:: *)
(* updateNFA *)

PackageScope["updateNFA"]
updateNFA::usage = "updateNFA[nfa, {trnRules, propRules}] updates the nfa.";
updateNFA[nfa_, {trnRules_, propRules_}] :=
  NFA[
    If[trnRules === {},
      Transitions @ nfa,
      Module[{trns = Transitions @ nfa},
        Scan[
          Scan[
            Switch[#[[1]],
              "Transitions",
              setTrns[trns, #] &,
              "AddTransitions",
              addTrns[trns, #] &,
              "RemoveTransitions",
              removeTrns[trns, #] &
            ],
            #[[2]]
          ] &,
          trnRules
        ];
        trns
      ]
    ],
    FirstCase[
      propRules,
      HoldPattern[
        "Initial" -> x_?(loudly[ListQ, UpdateFA::badNfaInit])
      ] :> x,
      States[nfa, "Initial"]
    ],
    FirstCase[
      propRules,
      HoldPattern[
        "Terminal" -> x_?(loudly[ListQ, UpdateFA::badTerm])
      ] :> x,
      States[nfa, "Terminal"]
    ],
    FirstCase[
      propRules,
      HoldPattern[
        "Alphabet" -> x_?(loudly[ListQ, UpdateFA::badAlphabet])
      ] :> x,
      LanguageAlphabet @ nfa
    ]
  ];

SetAttributes[setTrns, HoldFirst];
setTrns[asc_, {q_, a_} -> val_] :=
  If[ListQ @ val,
    If[KeyExistsQ[asc, q],
      asc[q][a] = val,
      asc[q] = <| a -> val |>
    ],
    Message[UpdateFA::badNfaTrnRule, {q, a} -> val]
  ];

SetAttributes[addTrns, HoldFirst];
addTrns[asc_, {q_, a_} -> val_] :=
  If[KeyExistsQ[asc, q],
    asc[q][a] = Union[
      Lookup[asc[q], Key @ a, {}],
      If[ListQ @ val, val, {val}]
    ],
    asc[q] = <| a -> If[ListQ @ val, val, {val}] |>
  ];

SetAttributes[removeTrns, HoldFirst];
removeTrns[asc_, {q_, a_} -> val_] :=
  If[KeyExistsQ[asc, q] && Quiet @ KeyExistsQ[asc[q], a],
    If[val === All,
      asc[q][a] =.,
      asc[q][a] =
        DeleteCases[
          asc[q][a],
          If[ListQ @ val,
            toAlternatives @ val,
            val
          ]
        ]
    ]
  ];

(* ::Section:: *)
(* Private *)

(* ::Subsection:: *)
(* thompsonNFA *)

thompsonNFA[regex_ (* assumes ?CompoundREQ *)] :=
  Block[
    {
      $thompTrns = <||>,
      $thompStateIdx = 1,
      levels
    },
    With[{res = thompConvert @ regex},
      levels = Range[$thompStateIdx - 1];

      Switch[#2,
        _Integer,
        (levels[[#2]] = #1) + 1,

        _parallel,
        (*        Max @@ With[{f = #0, i = #1}, f[i, #] &] /@ #2,*)
        Max @@ Thread[
          Unevaluated @ #0[#1, #2],
          parallel
        ],

        _List,
        Fold[#0, #1, #2]
      ] &[1, res];

      setGraphProperties[
        makeNFA[
          $thompTrns,
          {res[[1]]},
          {res[[-1]]}
        ],
        GraphLayout ->
          {
            "LayeredDigraphEmbedding",
            "RootVertex" -> res[[1]],
            "Orientation" -> Right,
            "VertexLayerPosition" -> levels
          },
        VertexSize -> Large
      ]
    ]
  ];

$thompTrns::usage = "transitions for thompsonNFA";
$thompStateIdx::usage = "state index for thompsonNFA";
parallel::usage = "represents parallel sequences";
SetAttributes[parallel, Flat];
dedup = If[SameQ @ ##, Nothing, #2] &;

thompNewState[] :=
  (
    $thompTrns[$thompStateIdx] = <||>;
    $thompStateIdx++
  );
thompConnect[from_, to_, with_ : Epsilon] :=
  $thompTrns[from] =
    Merge[
      {
        $thompTrns[from],
        with -> If[ListQ @ to, to, {to}]
      },
      Apply @ Union
    ];

thompSeq[{q0_, m0___, f0_}, {q1_, m1___, f1_}] :=
  (
    thompConnect[f0, q1];
    {
      q0, m0, dedup[q0, f0],
      q1, m1, dedup[q1, f1]
    }
  );
thompSeq[{q0_, m0___, f0_}, RELiteral[a_]] :=
  With[{p = thompNewState[]},
    thompConnect[f0, p, a];
    {q0, m0, dedup[q0, f0], p}
  ];

thompAlt[s0 : {q0_, ___, f0_}, RELiteral[a_]] :=
  (
    thompConnect[q0, f0, a];
    s0
  );
thompAlt[{q0_, m0_ : parallel[], f0_}, s1 : {q1_, ___, f1_}] :=
  (
    thompConnect[q0, q1];
    thompConnect[f1, f0];
    {q0, parallel[m0, s1], dedup[q0, f0]}
  );

thompClose[{p1_, p2_}, {q0_, m0___, f0_}] :=
  (
    thompConnect[p1, {q0, p2}];
    thompConnect[f0, {q0, p2}];
    {p1, q0, m0, dedup[q0, f0], p2}
  );
thompClose[{q_}, RELiteral[a_]] :=
  (
    thompConnect[q, q, a];
    {q, q}
  );

thompConvert[IgnoringInactive @ REStar[x : reHeadP]] :=
  With[
    {
      p1 = thompNewState[],
      converted = thompConvert @ x,
      p2 = thompNewState[]
    },
    thompClose[{p1, p2}, converted]
  ];
thompConvert[IgnoringInactive @ REStar[x_]] :=
  With[
    {
      p = thompNewState[]
    },
    thompClose[{p}, RELiteral @ x]
  ];
thompConvert[HoldIgnoringInactive @ REUnion[xs__]] :=
  With[
    {
      p1 = thompNewState[],
      converted = thompConvert /@ {xs},
      p2 = thompNewState[]
    },
    Fold[
      thompAlt,
      {p1, p2},
      converted
    ]
  ];
thompConvert[HoldIgnoringInactive @ REConcat[xs__]] :=
  With[
    {
      p = thompNewState[],
      converted = thompConvert /@ {xs}
    },
    Fold[
      thompSeq,
      {p, p},
      converted
    ]
  ];
thompConvert[a_] := RELiteral[a];

(* ::Subsection:: *)
(* glushkovNFA *)

glushkovNFA[r_] :=
  With[
    {
      e = linearizeRE[r, 2]
    },
    {
      P = (*EchoLabel["p"] @*) pSet @ e,
      D = (*EchoLabel["d"] @*) dSet @ e,
      F = (*EchoLabel["f"] @*) fSet @ e
    },
    NFA[
      Append[
        GroupBy[
          F,
          {Last@*First -> Last, First -> Last},
          Union
        ],
        1 ->
          GroupBy[
            P,
            First -> Last,
            Union
          ]
      ],
      {1},
      Cond[
        REMatchQ[Epsilon, r],
        Append @ 1
      ] @ D[[All, 2]]
    ]
  ];

(* ::Subsection:: *)
(* primeGrids *)

primeGrids[ram_SparseArray] :=
  primeGrids[Normal @ ram];
primeGrids[ram_] :=
  With[
    {
      primes = CreateDataStructure["LinkedList"],
      queue =
        CreateDataStructure["Queue",
          Map[List, Position[ram, 1], {2}]
        ],
      newRows =
        Complement[
          Catenate@
            Position[
              ram[[All, #2]],
              ConstantArray[1, Length@#2],
              {1},
              Heads -> False
            ],
          #1
        ] &,
      newCols =
        Complement[
          Catenate@
            Position[
              Transpose @ ram[[#1, All]],
              ConstantArray[1, Length@#1],
              {1},
              Heads -> False
            ],
          #2
        ] &
    },
    Module[{g, enqueue},
      enqueue[x_] :=
        (
          enqueue[x] = Null;
          queue["Push", x]
        );

      While[!queue @ "EmptyQ",
        g = queue @ "Pop";
        With[
          {
            i = First @ g,
            j = Last @ g,
            rows = newRows @@ g,
            cols = newCols @@ g
          },
          If[SameQ[{}, rows, cols],
            primes["Append", g],
            (
              If[rows =!= {}, enqueue[{Union[i, {#}], j}] & /@ rows];
              If[cols =!= {}, enqueue[{i, Union[j, {#}]}] & /@ cols]
            )
          ]
        ]
      ];
      Normal @ primes
    ]
  ];

(* ::Subsection:: *)
(* reduceGridsExhaustive *)

reduceGridsExhaustive[nfa_, B_, nfaB_, ram_, rows_, trns_, idxs_, initidx_, termidxs_, grids_, iterations_] :=
  Module[
    {
      isCover, tryMakeLegitNFA, nCandidates, maxi,
      mini = Ceiling @ Log2 @ StateCount @ B,
      nGrids = Length @ grids
    },
    trns;
    With[{goals = Position[ram, 1]},
      isCover[x_] :=
        ContainsAll[
          Catenate[Tuples /@ x],
          goals
        ]
    ];

    tryMakeLegitNFA[subset_] :=
      If[isCover @ subset,
        Module[
          {
            newInits, newTrns, newTerms,
            asc =
              With[
                {
                  rang = Range @ Length @ subset,
                  halfcov = subset[[All, 1]]
                },
                AssociationMap[
                  Function[z,
                    Select[
                      rang,
                      MemberQ[halfcov[[#]], z] &
                    ]
                  ],
                  Range[Length @ rows + 1]
                ]
              ]
          },
          newInits = asc @ initidx;
          {newTrns, newTerms} =
            Reap[
              Table[
                With[
                  {
                    slurps =
                      Select[
                        Range @ Length @ asc,
                        MemberQ[asc @ #, i]&
                      ]
                  },
                  If[ContainsOnly[slurps, termidxs], Sow[i, "terms"]];
                  i ->
                    Merge[
                      Values @ trns[[slurps]],
                      Intersection @@ (asc @* idxs /@ #) &
                    ]
                ],
                {i, Length @ subset}
              ],
              "terms"
            ];
          With[
            {
              intersectionRuleNFA =
                makeNFA[
                  Association @ newTrns,
                  newInits,
                  First[newTerms, {}],
                  LanguageAlphabet @ B
                ]
            },
            If[EquivalentFAQ[intersectionRuleNFA, nfaB],
              intersectionRuleNFA,
              $Failed
            ]
          ]
        ],
        $Failed
      ];

    maxi =
      Min[
        nGrids,
        StateCount @ nfa - 1,
        StateCount @ B - 1
      ];
    nCandidates = NSum[Binomial[nGrids, k], {k, mini, maxi}];
    PrintTemporary[nCandidates, " candidate NFAs must be checked for equivalence."];
    Catch[
      Do[
        ThrowIf[
          Not @* FailureQ,
          Quiet @ ParallelTry[
            tryMakeLegitNFA,
            Subsets[grids, {i}]
          ]
        ];
        PrintTemporary[
          "No Equivalent NFAs with ",
          i,
          If[i === 1, " state", " states"],
          " exist. (",
          nCandidates -= Binomial[nGrids, i],
          " canditates remain)"
        ],
        {i, mini, maxi}
      ];
      First @ MinimalBy[
        {
          FAExpression @ nfa,
          nfaB
        },
        StateCount
      ]
    ]
  ];

(* ::Subsection:: *)
(* reduceGridsSA *)

reduceGridsSA[nfa_, B_, nfaB_, ram_, rows_, trns_, idxs_, initidx_, termidxs_, grids_, iterations_] :=
  Module[{isCover, tryMakeLegitNFAFromIdxs, res, goalComplement,
    feasibleNeighbor, whereIs, optimizeCover, covers, imax,
    flipProb = 0.05, goals = Position[ram, 1]},

    isCover[x_] :=
      And @@ MapThread[
        ContainsAll,
        {
          Join[Sequence @@ x, 2],
          goals
        }
      ];

    whereIs =
      Module[{i = 1},
        Association @ Last @ Reap[
          Scan[
            With[{j = i++},
              Outer[
                Sow[j, {{##}}]&,
                Sequence @@ #
              ]
            ]&,
            grids
          ],
          _,
          Rule
        ]];

    feasibleNeighbor[vec_] :=
      Module[
        {
          comp, bit,
          i = 0,
          newvec =
            Unitize[
              vec -
                RandomChoice[
                  {flipProb, 1 - flipProb} -> {1, 0},
                  Length @ vec
                ]
            ]
        },
        comp =
          Complement[
            goals,
            Catenate[
              Tuples /@ Pick[grids, newvec, 1]
            ]
          ];
        While[Length @ comp > 0,
          bit = RandomChoice @ whereIs @ RandomChoice @ comp;
          newvec[[bit]] = 1;
          comp =
            Complement[
              comp,
              Tuples @ grids[[bit]]
            ]
        ];
        newvec
      ];

    With[{initTemp = 10, alpha = 0.9},
      optimizeCover[] :=
        Module[
          {
            newSolution, newCost, solution, cost, bestCost,
            k = 0,
            temp = initTemp,
            initSolution = feasibleNeighbor @ ConstantArray[0, Length @ grids],
            bestSolutions = CreateDataStructure @ "HashSet"
          },
          solution = initSolution;
          cost = Total @ initSolution;
          bestSolutions["Insert", solution];
          bestCost = cost;
          While[k++ < iterations,
            newSolution = feasibleNeighbor[solution];
            newCost = Total @ newSolution;
            Which[
              newCost < bestCost,
              (
                bestSolutions @ "RemoveAll";
                bestCost = newCost
              ),

              newCost == bestCost,
              bestSolutions["Insert", newSolution],

              True, Null
            ];
            If[newCost <= cost || RandomReal[] <= Quiet @ Exp[-(newCost - cost) / temp],
              solution = newSolution
            ];
            temp *= alpha
          ];
          {
            bestCost,
            Normal @ bestSolutions
          }
        ]
    ];

    tryMakeLegitNFAFromIdxs[is_] :=
      Module[
        {
          newInits, newTrns, newTerms, asc,
          cover = Pick[grids, is, 1]
        },
        asc =
          With[
            {
              rang = Range @ Length @ cover,
              halfcov = cover[[All, 1]]
            },
            AssociationMap[
              Function[z,
                Select[
                  rang,
                  MemberQ[halfcov[[#]], z]&
                ]
              ],
              Range[Length @ rows + 1]
            ]
          ];
        newInits = asc @ initidx;
        {newTrns, newTerms} =
          Reap[
            Table[
              With[
                {
                  slurps = Select[
                    Range @ Length @ asc,
                    MemberQ[asc @ #, i]&
                  ]
                },
                If[ContainsOnly[slurps, termidxs], Sow[i, "terms"]];
                i ->
                  Merge[
                    Values @ trns[[slurps]],
                    Intersection @@ (asc @* idxs /@ #) &
                  ]
              ],
              {i, Length @ cover}
            ],
            "terms"
          ];
        With[
          {
            intersectionRuleNFA =
              makeNFA[
                Association @ newTrns,
                newInits,
                First[newTerms, {}],
                LanguageAlphabet @ B
              ]
          },
          If[EquivalentFAQ[intersectionRuleNFA, nfaB],
            intersectionRuleNFA,
            $Failed
          ]
        ]
      ];

    Quiet @ LaunchKernels[];
    imax =
      Min[
        Length @ grids,
        StateCount @ nfa - 1,
        StateCount @ nfaB - 1
      ];
    Catch[
      Do[
        ThrowIf[Not@*FailureQ,
          Quiet @ ParallelTry[
            tryMakeLegitNFAFromIdxs,
            covs
          ]
        ],
        {covs,
          SortBy[
            ParallelTable[
              optimizeCover[],
              {Max[$KernelCount, 1]}
            ],
            First
          ][[All, 2]]
        }
      ];
      First @ MinimalBy[
        {
          FAExpression @ nfa,
          nfaB
        },
        StateCount
      ]
    ]
  ]
