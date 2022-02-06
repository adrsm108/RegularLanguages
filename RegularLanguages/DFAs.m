(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: DFAs *)
(* :Context: DFAs` *)
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
(* DFA *)
PackageExport["DFA"]
DFA::usage = "\
The head DFA represents a deterministic finite automaton.
DFA[{q$1 -> {a$1 -> s$1, a$2 -> s$2, $$}, $$}, {q$init}, {terms$$}] specifies a DFA with initial state q$init, terminal states terms$, \
and transitions {q$1, a$1} -> s$1, {q$1, a$2} -> s$2, $$, where q$i and s$i are states, and a$i are symbols of the alphabet.
DFA[{q$1 -> {s$1, s$2, $$}, $$}, {a$1, a$2, $$}, $$] is an alternate form for the above. Here, transitions are given as lists of states, and the alphabet is supplied as a second argument.
dfa$[{a$1, a$2, $$}] returns True if the given DFA accepts the string a$(1) a$(2) $$
dfa$['string$'] is equivalent to dfa$[Characters['string$']].";
DFA::missingtr = "Transition {`1`, `2`} not defined.";
DFA::invtrsym = "Unrecognized transition symbol `1` given for state `2`.";
DFA::invprod = "Unrecognized state `1` produced in transition {`2`,`3`}.";
DFA::invinit = "Unrecognized initial state `1`";
DFA::invterm = "Unrecognized terminal state `1`";
DFA::badcast = "Cannot automatically convert NFA `1` to DFA. Use ToDFA for subset construction.";
DFA::badinit = "Initial states parameter `1` is not a list of length 1.";
DFA::baddims = "Invalid dimensions. Values in `1` must have the same length as alphabet `2`.";

(* ::Subsubsection:: *)
(* DFA DownValues *)

DFA[
  rules : (List | Association)[(_ -> KeyValuePattern[{}])...],
  initial_,
  terminal_List
] :=
  With[
    {
      trns = Association /@ Association @ rules,
      alph = Union @@ (Keys @ Values @ rules)
    },
    {
      states = Keys @ trns
    },
    If[MemberQ[alph, Epsilon],
      Return @ Failure[
        "EpsilonTransiton",
        <|
          "MessageTemplate" -> "Epsilon transitions are not permitted in a DFA"
        |>
      ]
    ];
    If[!ListQ @ initial,
      Return @ Failure[
        "InitialStateForm",
        <|
          "MessageTemplate" -> "Initial states must be given as a list."
        |>
      ]
    ];
    If[Length @ initial != 1,
      Return @ Failure[
        "InitialStateCount",
        <|
          "MessageTemplate" -> "DFA must have exactly one initial state."
        |>
      ]
    ];
    If[!MemberQ[states, First @ initial],
      Return @ Failure[
        "InitialStateUnknown",
        <|
          "MessageTemplate" -> "Unknown initial state `init`.",
          "MessageParameters" -> <|"init" -> First @ initial|>
        |>
      ]
    ];
    KeyValueMap[
      Function[{q, ts},
        Scan[
          With[{symbols = Keys @ ts},
            If[!MemberQ[symbols, #],
              Message[DFA::missingtr, q, #]
            ]&
          ],
          alph
        ];
        KeyValueMap[
          (
            If[!MemberQ[alph, #1],
              Message[DFA::invtrsym, #1, q]
            ];
            If[!MemberQ[states, #2],
              Message[DFA::invprod, #2, q, #1]
            ];
          ) &,
          ts
        ]
      ],
      trns
    ];

    Scan[
      If[!MemberQ[states, #],
        Message[DFA::invterm, #]
      ] &,
      terminal
    ];
    makeDFA[
      trns,
      initial,
      terminal,
      alph
    ]
  ];
DFA[
  rules : (List | Association)[(_ -> _List)...],
  alphabet_List,
  initial_,
  terminal_List
] :=
  With[{rulesAsc = Association @ rules},
    DFA[
      Replace[
        rulesAsc,
        vals_List :> AssociationThread[alphabet -> vals],
        {1}
      ],
      initial,
      terminal
    ] /;
      With[
        {
          dims =
            Dimensions[rulesAsc,
              2,
              AllowedHeads -> {"ListLike", Association}
            ]
        },
        If[Length @ dims == 2 && dims[[2]] === Length @ alphabet,
          True,
          Message[DFA::baddims, rules, alphabet]
        ]
      ]
  ];
DFA[dfa_DFA] := dfa;
DFA[nfa_NFA] :=
  Failure[
    "BadCast",
    <|
      "MessageTemplate" -> "Cannot cast an NFA to a DFA. Use ToDFA for subset construction."
    |>
  ];
DFA[g_Graph?FAGraphQ] := DFA @ FAExpression @ g;

(* ::Subsubsection:: *)
(* DFA UpValues *)

DFA /: Graph[dfa_DFA?DFAQ, opts : OptionsPattern[]] :=
  FAGraph[dfa, opts];
DFA /: Graph3D[dfa_DFA?DFAQ, opts : OptionsPattern[]] :=
  FAGraph3D[dfa, opts];
DFA /: ToRules[dfa_DFA?DFAQ] :=
  Normal[Normal /@ Transitions @ dfa];

(* ::Subsubsection:: *)
(* DFA SubValues *)

(dfa_DFA?DFAQ)[input : (_List | _String)] :=
  FAMatchQ[dfa, input];

(* ::Subsubsection:: *)
(* DFA FormatValues *)

DFA /: MakeBoxes[dfa : DFA[asc_?dfaAscQ], form : (StandardForm | TraditionalForm)] :=
  makeAutomatonSummaryBoxes[dfa, form];

(* ::Subsection:: *)
(* FAMatchQ *)

FAMatchQ[dfa_?DFAQ, input_List] :=
  MemberQ[
    States[dfa, "Terminal"],
    Fold[
      TransitionFunction @ dfa,
      First @ States[dfa, "Initial"],
      input
    ]
  ];

(* ::Subsection:: *)
(* TransitionSequence *)
TransitionSequence[dfa_?DFAQ, input_List, seq_ : All] :=
  Take[
    FoldList[
      TransitionFunction @ dfa,
      First @ States[dfa, "Initial"],
      input
    ],
    seq
  ];

(* ::Subsection:: *)
(* DFAQ *)
PackageExport["DFAQ"]
DFAQ::usage = "DFAQ[x$] returns True if x$ is a valid DFA.";
DFAQ[DFA[_?dfaAscQ]] = True;
DFAQ[g_Graph] :=
  DFAQ @ Quiet @ AnnotationValue[g, "Automaton"];
DFAQ[_] = False;

(* ::Subsection:: *)
(* ToDFA *)

PackageExport["ToDFA"]
ToDFA::usage = "\
ToDFA[dfa$] returns the given DFA.
ToDFA[nfa$] converts an NFA into an equivalent DFA.
ToDFA[nfa$, alphabet$] converts an NFA into an equivalent DFA with the given LanguageAlphabet.
ToDFA[regex$] converts a regular expression into a DFA by way of an intermediate NFA.

Options:
Method -> Automatic | 'Minimal'
* 'Minimal': Returned DFA will have minimal states. Equivalent to calling MinimizeDFA on the result.
* Automatic: Use the subset method for NFAs and Regular Expressions.

'StateNames' -> Automatic | 'Subset'
* Automatic: The returned DFA has states 1, 2, $$.
* 'Subset': state ids correspond to subsets of the original set of states.
  - When constructed from an NFA with Method -> 'Automatic', these are the states from the subset method.";
Options[ToDFA] =
  {
    Method -> Automatic,
    "StateNames" -> Automatic
  };
OptionChecks[ToDFA] =
  {
    Method -> Automatic | "Minimal",
    "StateNames" -> "Subset" | Automatic
  };
ToDFA[dfa_?DFAQ, CheckedOptions @ ToDFA] :=
  Switch[OptionValue @ Method,
    "Minimal",
    Switch[OptionValue @ "StateNames",
      Automatic, MinimizeDFA @ dfa,
      "Subset", MinimizeDFA[dfa, "StateNames" -> "SubsetUnion"]
    ],
    Automatic,
    RenameStates[
      dfa,
      Switch[OptionValue @ "StateNames",
        Automatic, "Index",
        "Subset", List
      ]
    ]
  ];
ToDFA[
  nfa_?NFAQ,
  alphabet : (_List | Automatic) : Automatic,
  CheckedOptions @ ToDFA
] :=
  With[
    {
      init = EpsilonClosure @ nfa,
      alph = UnlessAutomatic[alphabet, LanguageAlphabet[nfa, "IncludeEpsilon" -> False]],
      trns = Transitions @ nfa,
      (* This is effectively ContainsAny @ States[nfa, "Terminal"],
      but I was experiencing a strange bug where ContainsAny would give the wrong answer in a way
      that seemed to depend on the names in the list. *)
      termQ = MemberQ @ toAlternatives @ States[nfa, "Terminal"]
    },
    Module[
      {
        namefn, convert,
        i = 1
      },
      namefn =
        Switch[OptionValue @ "StateNames",
          Automatic, (i++) &,
          "Subset", Identity
        ];
      convert[subset_] :=
        With[
          {
            name = convert @ subset = namefn @ subset
          },
          If[
            termQ @ subset,
            Sow[name, "terminal"]
          ];
          Sow[
            name ->
              AssociationMap[
                convert @ EpsilonClosure[
                  Union @@
                    Lookup[
                      Lookup[trns, subset, Nothing],
                      Key @ #,
                      {}
                    ],
                  trns
                ] &,
                alph
              ],
            "transitions"
          ];
          name
        ];
      Cond[OptionValue @ Method === "Minimal",
        If[OptionValue @ "StateNames" === Automatic,
          MinimizeDFA,
          MinimizeDFA[#, "StateNames" -> "SubsetUnion"] &
        ]
      ] @ Replace[
        ReapExactly[
          convert[init],
          {"transitions", "terminal"}
        ],
        {newInit_, {newTrns_, newTerms_}} :>
          makeDFA[
            Association @ newTrns,
            {newInit},
            newTerms,
            alph
          ]
      ]
    ]
  ];
ToDFA[regex_?REQ, opts : CheckedOptions @ ToDFA] :=
  ToDFA[ToNFA @ regex, opts];

(* ::Subsection:: *)
(* MinimizeDFA *)

PackageExport["MinimizeDFA"]
MinimizeDFA::usage = "\
MinimizeDFA[dfa$] returns an equivalent DFA with the minimum number of states.

Options:
'StateNames' -> Automatic | 'Subset' | 'SubsetUnion'
* Automatic: The returned DFA has states 1, 2, $$.
* 'Subset': States are subsets of the ids of the original, representing equivalence classes in its StatesPartition.
* 'SubsetUnion': Like 'Subset', but states are the unions of elements of subsets instead of the subsets themselves.
   - Assumes the states of the original automaton are lists.";
Options[MinimizeDFA] =
  {
    "StateNames" -> Automatic
  };
OptionChecks[MinimizeDFA] =
  {
    "StateNames" -> Automatic | "Subset" | "SubsetUnion"
  };
MinimizeDFA[dfa_?DFAQ, CheckedOptions @ MinimizeDFA] :=
  With[{rdfa = RemoveUnreachableStates[dfa]},
    Module[{newid, newinit, i = 2},
      With[
        {
          classes = StatesPartition @ rdfa,
          initQ = MemberQ[First @ States[rdfa, "Initial"]],
          terminals = States[rdfa, "Terminal"],
          trns = Transitions @ rdfa
        },
        Scan[
          Switch[OptionValue @ "StateNames",
            Automatic,
            (
              newid[toAlternatives @ #] =
                If[initQ[#], newinit = 1, i++]
            ) &,
            "Subset",
            (
              newid[toAlternatives @ #] =
                If[initQ[#], newinit = #, #]
            ) &,
            "SubsetUnion",
            (
              newid[toAlternatives @ #] =
                If[initQ[#], newinit = Union @@ #, Union @@ #]
            ) &
          ],
          classes
        ];
        makeDFA[
          Association @ Map[
            (newid @ # -> newid /@ trns @ #)&,
            classes[[All, 1]]
          ],
          {newinit},
          Union[newid /@ terminals],
          LanguageAlphabet @ rdfa
        ]
      ]
    ]
  ];

(* ::Subsection:: *)
(* RandomDFA *)
PackageExport["RandomDFA"]
RandomDFA::usage = "\
RandomDFA[{states$$}, {symbols$$}] creates a random DFA with states states$ and alphabet symbols$.
RandomDFA[n$, k$] creates a random DFA with n$ states on an alphabet of k$ symbols.
* Either n$ or k$ can be a list, as in the above case.
* Default state ids are 1, 2, $$, n$
* Default symbols are 'a', 'b', $$ (ascii character range 97 to 97 + k) if k$ \[LessEqual] 26, or 'x1', 'x2', $$, 'xk' otherwise.
* When a function f$ is provided for the 'StatesFunction' option, the state ids will be Array[f$, n$].
* When a function g$ is provided for the 'AlphabetFunction' option, the alphabet will be Array[g$, k$].

Options:
'InitialStates' -> {q$_} | Automatic
Initial state of the returned DFA
* {q$_}: initial state q$.
* Automatic: Randomly selected initial state.

'TerminalStates' -> _Integer | _Real | _List
Terminal states in the returned DFA
* l$_List: terminal states are exactly the states in l$.
* m$_Integer: m$ terminal states.
* x$_Real: Ceiling[xn$] terminal states, where n$ is the total number of states.

'StatesFunction' -> _
Function to generate state names, applied to the list of states or Range[n$]

'AlphabetFunction' -> _
Function to generate alphabet symbols, applied to the list of symbols or Range[k$]

'AllStatesReachable' -> True | False
Whether the returned DFA must form a (weakly) connected graph.";
Options[RandomDFA] =
  {
    "TerminalStates" -> 0.3,
    "InitialStates" -> Automatic,
    "AlphabetFunction" -> Automatic,
    "StatesFunction" -> Automatic,
    "AllStatesReachable" -> True
  };
OptionChecks[RandomDFA] =
  {
    "InitialStates" -> {_} | Automatic,
    "TerminalStates" -> _Integer | _Real | _List,
    "AllStatesReachable" -> True | False
  };
RandomDFA[
  statesin : (_Integer | _List),
  alphin : (_Integer | _List),
  CheckedOptions @ RandomDFA
] :=
  With[
    {
      n = IfMatch[_Integer, statesin, Length @ statesin],
      k = IfMatch[_Integer, alphin, Length @ alphin]
    },
    {
      nterm = intOrMult[OptionValue @ "TerminalStates", n],
      states = makeStates[statesin, OptionValue @ "StatesFunction"],
      alph = makeAlphabet[alphin, OptionValue @ "AlphabetFunction"],
      init = OptionValue @ "InitialStates"
    },
    {
      terms =
        UnlessMatch[
          OptionValue @ "TerminalStates",
          (_Integer | _Real),
          RandomSample[
            states,
            UpTo @ intOrMult[
              OptionValue @ "TerminalStates",
              n
            ]
          ]
        ]
    },
    If[OptionValue @ "AllStatesReachable",
      If[k == 1,
        With[
          {
            rs =
              Append[
                RandomSample @ states,
                RandomChoice @ states
              ]
          },
          DFA[
            BlockMap[
              First @ # -> Rest @ # &,
              rs,
              2,
              1
            ],
            alph,
            UnlessAutomatic[init, {First @ rs}],
            RandomSample[states, UpTo @ nterm]
          ]
        ],
        With[
          {
            asc =
              GroupBy[
                prufferDecode @ randomPrufferCode[n, k],
                Last -> First
              ],
            idxs =
              AssociationThread[
                Range @ n -> RandomSample @ states
              ]
          },
          DFA[
            Table[
              With[{egg = Lookup[asc, i, {}]},
                i ->
                  RandomSample @ Flatten @ Join[
                    egg,
                    RandomChoice[;; n, k - Length @ egg]
                  ]
              ],
              {i, n}
            ] /. idxs,
            alph,
            UnlessAutomatic[init, {idxs @ n}],
            terms
          ]
        ]
      ],
      DFA[
        Thread[
          states ->
            RandomChoice[
              states,
              {Length @ states, Length @ alph}
            ]
        ],
        alph,
        UnlessAutomatic[init, RandomSample[states, 1]],
        terms
      ]
    ]
  ];

(* ::Subsection:: *)
(* DecimalFactorDFA *)
PackageExport["DecimalFactorDFA"]
DecimalFactorDFA::usage = "\
DecimalFactorDFA[n$] returns a DFA accepting lists of digits whose decimal value is divisible by n$.
DecimalFactorDFA[n$, True] returns a DFA accepting lists of digits whose decimal value is divisible by n$, as well as the empty list.";
DecimalFactorDFA[n_, acceptEmpty_ : False] :=
  DFA[
    Cond[
      acceptEmpty,
      Append[
        "start" -> (Mod[#, n] & /@ Range[0, 9])
      ]
    ][
      Table[
        k ->
          Array[
            Mod[10 k + #, n] &,
            10,
            0
          ],
        {k, 0, n - 1}
      ]
    ],
    Range[0, 9],
    {If[acceptEmpty, 0, "start"]},
    {0}
  ];


(* ::Section:: *)
(* Package Scope *)

(* ::Subsection:: *)
(* makeDFA *)
PackageScope["makeDFA"]
makeDFA::usage = "\
makeDFA[trns, {init}, terms, alphabet] makes a DFA without checking options.
makeDFA[trns, {init}, terms] does the same, automatically computing the alphabet from transitions.
Sorts arguments beforehand.
";
makeDFA[trns_, inits_, terms_, alph_ : Automatic] :=
  With[
    {
      sortedTrns = KeySort /@ KeySort @ trns,
      alphabet = UnlessAutomatic[alph, Keys[Join @@ trns]]
    },
    DFA[
      <|
        "Transitions" -> sortedTrns,
        "States" -> Keys @ sortedTrns,
        "Initial" -> Union @ inits,
        "Terminal" -> Union @ terms,
        "Alphabet" -> Union @ alphabet
      |>
    ]
  ];

PackageScope["dfaAscQ"]
dfaAscQ::usage = "dfaAscQ[asc] returns True if asc is a valid association for a DFA, False otherwise.";
dfaAscQ[asc_] :=
  MatchQ[asc,
    KeyValuePattern @ {
      "Transitions" -> <|(_ -> _Association)...|>,
      "States" -> _List,
      "Initial" -> {_},
      "Terminal" -> _List,
      "Alphabet" -> _List
    }
  ];

PackageScope["productDFA"]
productDFA::usage = "\
productDFA[A$1, A$2, $$, q$0, pred$] returns the product DFA with initial states {q$0} and terminal states selected by pred$";
productDFA[As__, q0_, pred_] :=
  With[
    {
      prodTrns = productTransitions @ As
    },
    makeDFA[
      prodTrns,
      {q0},
      Select[Keys @ prodTrns, pred],
      LanguageAlphabet /@ Unevaluated @ Intersection @ As
    ]
  ];

PackageScope["scanProductDFA"]
scanProductDFA::usage = "scanProductDFA[f, A1, A2] applies f to the id of each reachable state in product DFA of A1 and A2, without explicitly constructing it.";
scanProductDFA[f_, dfa1_?DFAQ, dfa2_?DFAQ] :=
  With[
    {
      queue =
        CreateDataStructure[
          "Queue",
          {Catenate @ States[{dfa1, dfa2}, "Initial"]}
        ],
      trnTables = Transitions @ {dfa1, dfa2},
      alph =
        If[!SameAlphabetQ[dfa1, dfa2],
          Return @ False,
          LanguageAlphabet @ dfa1
        ]
    },
    Module[{enqueue},
      enqueue[tuple_] := (
        queue["Push", tuple];
        enqueue[tuple] = Null
      );
      While[!queue @ "EmptyQ",
        With[
          {
            tup =
              MapThread[Construct,
                {
                  trnTables,
                  aside[f, queue @ "Pop"]
                }
              ]
          },
          Scan[
            enqueue[Lookup[tup, #]]&,
            alph
          ]
        ]
      ]
    ]
  ];
scanProductDFA[f_, nfa1_?NFAQ, nfa2_?NFAQ] :=
  With[
    {
      queue =
        CreateDataStructure[
          "Queue",
          {EpsilonClosure /@ {nfa1, nfa2}}
        ],
      trnTables = Transitions @ {nfa1, nfa2},
      alph =
        DeleteCases[
          LanguageAlphabet /@ Unevaluated @ Union[nfa1, nfa2],
          Epsilon
        ]
    },
    Module[{enqueue},
      enqueue[tuple_] := (
        queue["Push", tuple];
        enqueue[tuple] = Null
      );
      While[!queue @ "EmptyQ",
        With[
          {
            tup =
              MapThread[
                Lookup,
                {
                  trnTables,
                  aside[f, queue @ "Pop"]
                }
              ]
          },
          Scan[
            Function[symb,
              enqueue @ MapThread[
                EpsilonClosure,
                {
                  Lookup[#, symb, {}] & /@
                    Map[
                      Merge @ Apply @ Union,
                      tup
                    ],
                  trnTables
                }
              ]
            ],
            alph
          ]
        ]
      ]
    ]
  ];
scanProductDFA[f_, A1_?FAQ, A2_?FAQ] :=
  scanProductDFA[f, NFA @ A1, NFA @ A2];

(* ::Subsection:: *)
(* updateDFA *)

PackageScope["updateDFA"]
updateDFA::usage = "updateDFA[dfa, {trnrules, proprules}] updates the given dfa.";
updateDFA[dfa_, {trnRules_, propRules_}] :=
  (
    If[
      MemberQ[propRules, "Alphabet" -> _],
      Message[UpdateFA::badDfaOp, "Alphabet"]
    ];
    DFA[
      If[trnRules === {},
        Transitions @ dfa,
        Module[{trns = Transitions @ dfa},
          Scan[
            Scan[
              Switch[#[[1]],
                "Transitions",
                setTrns[trns, #] &,

                "AddTransitions" | "RemoveTransitions",
                Message[
                  UpdateFA::badDfaOp,
                  #[[1]]
                ]
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
          "Initial" -> x_?(loudly[MatchQ[{_}], UpdateFA::badDfaInit])
        ] :> x,
        States[dfa, "Initial"]
      ],
      FirstCase[
        propRules,
        HoldPattern[
          "Terminal" -> x_?(loudly[ListQ, UpdateFA::badTerm])
        ] :> x,
        States[dfa, "Terminal"]
      ]
    ]
  );

SetAttributes[setTrns, HoldFirst];
setTrns[asc_, {q_, a_} -> val_] :=
  If[KeyExistsQ[asc, q],
    asc[q][a] = val,
    asc[q] = <| a -> val |>
  ];

(* ::Section:: *)
(* Private *)

prufferDecode[code_] :=
  Module[
    {
      deg =
        Lookup[
          Counts @ code + 1,
          Range[Length @ code + 2],
          1
        ]
    },
    Append[
      (deg[[#]]--; #) & /@
        {
          First @ FirstPosition[deg, 1],
          #
        } & /@ code,
      Flatten @ Position[
        deg,
        1,
        {1},
        2,
        Heads -> False
      ]
    ]
  ];

randomPrufferCode[n_, k_] :=
  Which[
    n <= 2,
    {},

    k == 1,
    RandomSample[;; n, n - 2],

    n - 2 <= k,
    RandomChoice[;; n, n - 2],

    True,
    With[{q = Quotient[n - 2, k - 1]},
      RandomSample @ Flatten @ {
        Table[
          RandomSample[;; n, q],
          k - 2
        ],
        RandomSample[
          ;; n,
          q + Mod[n - 2, k - 1]
        ]
      }
    ]
  ];

(* ::Subsection:: *)
(* productStates *)
productTransitions[dfas : Repeated[_?DFAQ, {2, Infinity}]] :=
  With[
    {
      queue =
        CreateDataStructure[
          "Queue",
          {
            Catenate @ States[{dfas}, "Initial"]
          }
        ],
      stateTrns = Transitions @ {dfas},
      alph = LanguageAlphabet /@ Unevaluated @ Intersection @ dfas,
      statesTag = CreateUUID @ "NewStates"
    },
    Module[{next, tup, enqueue},
      enqueue[tuple_] :=
        (
          queue["Push", tuple];
          enqueue[tuple] = tuple
        );
      Association @ First[
        Last @ Reap[
          While[!queue @ "EmptyQ",
            tup =
              MapThread[
                Construct,
                {
                  stateTrns,
                  next = queue @ "Pop"
                }
              ];
            Sow[
              next ->
                AssociationMap[
                  enqueue @ Lookup[tup, Key @ #] &,
                  alph
                ],
              statesTag
            ]
          ],
          statesTag
        ],
        {}
      ]
    ]
  ];
productTransitions[nfas : Repeated[_?NFAQ, {2, Infinity}]] :=
  With[
    {
      queue =
        CreateDataStructure[
          "Queue",
          {
            EpsilonClosure /@ {nfas}
          }
        ],
      stateTrns = Transitions @ {nfas},
      alph = DeleteCases[LanguageAlphabet /@ Unevaluated @ Union @ nfas, Epsilon],
      statesTag = CreateUUID @ "NewStates"
    },
    Module[{next, tup, enqueue},
      enqueue[tuple_] :=
        (
          queue["Push", tuple];
          enqueue[tuple] = tuple
        );
      Association @ First[
        Last @ Reap[
          While[!queue @ "EmptyQ",
            tup =
              MapThread[
                Lookup,
                {
                  stateTrns,
                  next = queue @ "Pop"
                }
              ];
            Sow[
              next ->
                AssociationMap[
                  enqueue @ MapThread[
                    EpsilonClosure,
                    {
                      Map[
                        Function[x, Lookup[x, #, {}]],
                        Merge[Apply @ Union] /@ tup
                      ],
                      stateTrns
                    }
                  ] &,
                  alph
                ],
              statesTag
            ]
          ],
          statesTag
        ],
        {}
      ]
    ]
  ];
productTransitions[As : Repeated[_?FAQ, {2, Infinity}]] :=
  productTransitions[Sequence @@ (ToNFA /@ {As})];

