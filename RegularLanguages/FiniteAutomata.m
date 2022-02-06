(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: Automata *)
(* :Context: Automata` *)
(* :Author: Adam Smith *)
(* :Date: 2020-05-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2020 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

Package["RegularLanguages`"]
PackageImport["Developer`"]

(* ::Section:: *)
(* Package Export *)

(* ::Subsection:: *)
(* FAQ *)

PackageExport["FAQ"]
FAQ::usage = "FAQ[A$] yields True if A$ is a valid representation of an NFA or DFA.";
FAQ[A_DFA] := DFAQ @ A;
FAQ[A_NFA] := NFAQ @ A;
FAQ[G_Graph] := FAGraphQ @ G;
FAQ[_] = False;

(* ::Subsection:: *)
(* FAGraphQ *)

PackageExport["FAGraphQ"]
FAGraphQ::usage = "FAGraphQ[G$] yields True if G$ is a graph with a valid 'Automaton' annotation.";
FAGraphQ[g_Graph] := FAQ @ Quiet @ AnnotationValue[g, "Automaton"];
FAGraphQ[_] = False;

(* ::Subsection:: *)
(* FAExpressionQ *)

PackageExport["FAExpressionQ"]
FAExpressionQ::usage = "FAExpressionQ[A$] returns True if A$ is a valid automaton with head NFA or DFA.";
FAExpressionQ[A_] :=
  MatchQ[A, NFA[_?nfaAscQ] | DFA[_?dfaAscQ]];


(* ::Subsection:: *)
(* UniversalFAQ *)

PackageExport["UniversalFAQ"]
UniversalFAQ::usage = "UniversalFAQ[A$] yields True if A$ is an automaton which accepts all strings over its alphabet.";
UniversalFAQ[A_?FAQ] := EmptyLanguageQ @ FAComplement @ A;
UniversalFAQ[_] = False;

PackageExport["EquivalentFAQ"]
EquivalentFAQ::usage = "\
EquivalentFAQ[A$1, A$2] is True if A$1 and A$2 are automata that recognize the same language.
EquivalentFAQ[A$1, A$2, $$] yields true if all A$i are equivalent automata.
EquivalentFAQ[A$] yields true if A$ is an automaton.";
EquivalentFAQ[A1_?FAQ, A2_?FAQ] := productStateTerminalPairNoneTrue[A1, A2, Xor];
EquivalentFAQ[A_?FAQ] = True;
EquivalentFAQ[_, _] = False;
EquivalentFAQ[Ai : Repeated[_, {3, Infinity}]] :=
  With[
    {
      m = First @ MinimalBy[{Ai}, StateCount]
    },
    AllTrue[
      DeleteCases[{Ai}, m],
      EquivalentFAQ[m, #]&
    ]
  ];

(* ::Subsection:: *)
(* SubsetFAQ *)

PackageExport["SubsetFAQ"]
SubsetFAQ::usage = "\
SubsetFAQ[A$1, A$2] returns True if the language recognized by automaton A1 is a subset of the language recognized by automaton A2.
SubsetFAQ[A$, A$1, A$2, $$] yields True if SubsetFAQ[A$, A$i] is true for all A$i.
SubsetFAQ[A$] represents an operator form of SubsetFAQ that can be applied to an expression.";
SubsetFAQ[A1_?FAQ, A2_?FAQ] := productStateTerminalPairNoneTrue[A1, A2, (#1 && !#2 &)];
SubsetFAQ[_, _] = False;
SubsetFAQ[A1_][A2_] := SubsetFAQ[A1, A2];
SubsetFAQ[A_, Ai : Repeated[_, {2, Infinity}]] := AllTrue[{Ai}, SubsetFAQ[A]];

(* ::Subsection:: *)
(* FAMatchQ *)

PackageExport["FAMatchQ"]
FAMatchQ::usage = "\
FAMatchQ[A$, {a$1, a$2, $$}] returns True if the finite automaton A$ accepts the string of symbols a$1 a$2 $$
FAMatchQ[A$, 'string'] is equivalent to FAMatchQ[A$, Characters['string']]";
FAMatchQ[A_, str_String] :=
  FAMatchQ[A, Characters @ str];

(* ::Subsection:: *)
(* FAExpression *)

PackageExport["FAExpression"]
FAExpression::usage = "FAExpression[A$] returns automaton A$ as an expression with head NFA or DFA.";
FAExpression::inv = "FAExpression expects an automaton expression, or Graph with an 'Automaton' annotation, but recieved `1`.";
FAExpression[A : (_NFA | _DFA)] := A;
FAExpression[g_] :=
  IfMatch[_?FAExpressionQ,
    Quiet @ AnnotationValue[g, "Automaton"],
    Failure[
      "InvalidInput",
      <|
        "MessageTemplate" -> "Input must be a valid automaton or a Graph with valid \"Automaton\" annotation."
      |>
    ]
  ];

(* ::Subsection:: *)
(* FAType *)

PackageExport["FAType"]
FAType::usage = "FAType[A$] returns DFA if A$ is a DFA, or NFA if A$ is an NFA.";
FAType[A_?FAQ] := Head @ FAExpression @ A;


(* ::Subsection:: *)
(* States *)

PackageExport["States"]
States::usage = "\
States[A$] returns a list of state names for the DFA or NFA A$.
States[A$, 'prop$'] returns the states of A$ with the given property.
* Valid properties include 'Initial', 'Noninitial', 'Terminal', 'Nonterminal', 'Reachable', and 'Unreachable'.
States['prop$'] represents an operator form of States.";
States::invprop = "Unknown property `1`.";
SetAttributes[States, Listable];
States[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  asc @ "States";
States[
  DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ],
  prop_?(
    loudly[
      MatchQ[
        "Initial"
          | "Terminal"
          | "Noninitial"
          | "Nonterminal"
          | "Reachable"
          | "Unreachable"
      ],
      States::invprop
    ]
  )
] :=
  Switch[prop,
    "Initial",
    asc @ "Initial",

    "Terminal",
    asc @ "Terminal",

    "Noninitial",
    Complement[
      asc @ "States",
      asc @ "Initial"
    ],

    "Nonterminal",
    Complement[
      asc @ "States",
      asc @ "Terminal"
    ],

    "Reachable",
    ReachableStates[
      asc @ "Initial",
      asc @ "Transitions"
    ],

    "Unreachable",
    Complement[
      asc @ "States",
      ReachableStates[
        asc @ "Initial",
        asc @ "Transitions"
      ]
    ]
  ];
States[g_Graph?FAGraphQ, rest___] := States[FAExpression @ g, rest];
States[prop_String][A_] := States[A, prop];

(* ::Subsection:: *)
(* StateCount *)

PackageExport["StateCount"]
StateCount::usage = "\
StateCount[A$] returns the number of states in the automaton A$.
StateCount[A$, 'prop$'] returns the number of states in A$ with the given property.
* See usage of States for valid properties.
StateCount['prop$'] represents an operator form of StateCount.";
StateCount::invprop = "Unknown property `1`.";
SetAttributes[StateCount, Listable];
StateCount[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  Length @ asc @ "States";
StateCount[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], prop_] :=
  Switch[prop,
    "Initial",
    Length @ asc @ "Initial",

    "Terminal",
    Length @ asc @ "Terminal",

    "Noninitial",
    Length @ asc @ "States" - Length @ asc @ "Initial",

    "Nonterminal",
    Length @ asc @ "States" - Length @ asc @ "Terminal",

    "Reachable",
    Length @ ReachableStates[
      asc @ "Initial",
      asc @ "Transitions"
    ],

    "Unreachable",
    Length @ asc @ "States" -
      Length @ ReachableStates[
        asc @ "Initial",
        asc @ "Transitions"
      ],

    _, Message[StateCount::invprop, prop]
  ];
StateCount[g_Graph?FAGraphQ, rest___] := StateCount[FAExpression @ g, rest];
StateCount[prop_String][A_] := StateCount[A, prop];

(* ::Subsection:: *)
(* ReachableStates *)

PackageExport["ReachableStates"]
ReachableStates::usage = "\
ReachableStates[{q$}, A$] returns the set of states reachable from state q$ in finite automaton A$ by any sequence of transitions.
ReachableStates[{q$1, q$2, $$}, A$] returns ReachableStates[q$1,A$] \[Union] ReachableStates[q$2, A$] \[Union] $$
ReachableStates[A$] returns the set of states reachable from the initial states of the finite automaton A$.
ReachableStates[states$, trns$] returns states reachable from $states according to the transitions trns$.
* trns$ can be a TransitionFunction, or a list or association with elements q$ -> trns$q, where trn$q is an association or list of rules of the form a$ -> res$ such that res$ is the result of transitioning from q$ on symbol a$.
ReachableStates[$$, {a$1, a$2, $$}] finds states reachable via any sequence of transitions restricted to the symbols a$1, a$2, $$.
Options:
'TransitionType' -> DFA | NFA | Automatic
Used to specify whether the entries in a transition table are states or sets of states. Automatically set when the input is a NFA or DFA.
* Automatic: Assume nondeterministic transitions if entries in transition table are lists. This fails if state names are themselves lists.";
ReachableStates::invstate = "State `1` not found.";
OptionChecks[ReachableStates] = { "TransitionType" -> NFA | DFA | Automatic };
Options[ReachableStates] = { "TransitionType" -> Automatic };
ReachableStates[{}, ___] = {};
ReachableStates[
  A_?FAQ,
  rest___
] :=
  ReachableStates[
    States[A, "Initial"],
    Transitions @ A,
    rest,
    "TransitionType" -> FAType @ A
  ];
ReachableStates[states_List, A_?FAQ, rest___] :=
  ReachableStates[
    states,
    Transitions @ A,
    rest,
    "TransitionType" -> FAType @ A
  ];
ReachableStates[states_List, TransitionFunction[type_, trns_], rest___] :=
  ReachableStates[states, trns, rest, "TransitionType" -> type];
ReachableStates[states_List, rules : {___Rule}, rest___] :=
  ReachableStates[states, Association @ rules, rest];
ReachableStates[
  ids_List,
  trns_Association,
  syms : (_List | All) : All,
  CheckedOptions @ ReachableStates
] :=
  Sort @ Module[{push},
    Reap[
      With[
        {
          successors =
            If[syms === All,
              Values,
              Lookup[#, syms, Nothing]&
            ],
          lvl =
            Switch[OptionValue @ "TransitionType",
              NFA, {2},
              DFA, {1},
              Automatic,
              Switch[trns,
                <|(_ -> _[(_ -> _List) ...])...|>, {2},
                _, {1}
              ]
            ] ,
          state = Lookup[
            trns,
            Key @ #,
            Message[ReachableStates::invstate, #];
            {}
          ] &,
          queue =
            CreateDataStructure["Queue",
              (push[Sow @ #] = Null; #)& /@ ids
            ]
        },
        push[id_] := (
          push[Sow @ id] = Null;
          queue["Push", id];
        );
        While[! queue @ "EmptyQ",
          Scan[
            push,
            successors @ state @ queue["Pop"],
            lvl
          ]
        ]
      ]
    ][[2, 1]]
  ];


(* ::Subsection:: *)
(* EpsilonReachableStates *)

PackageExport["EpsilonClosure"]
EpsilonClosure::usage = "\
EpsilonClosure[q$, nfa$] gives the set of states reachable from q$ in nfa$ via any sequence of Epsilon transitions.
EpsilonClosure[{q$1, q$2, $$}, nfa$] gives EpsilonClosure[q$1, nfa$] \[Union] EpsilonClosure[q$2, nfa$] \[Union] $$
EpsilonClosure[nfa$] gives EpsilonClosure[States[nfa$, 'Initial'], nfa$].
EpsilonClosure[states$, trns$] computes the epsilon closure of states$ using transition table trns$, which may be \
can be any transition specification recognized by ReachableStates.";
EpsilonClosure[nfa_?NFAQ] :=
  ReachableStates[nfa, {Epsilon}];
EpsilonClosure[states_, transitions_] :=
  ReachableStates[states, transitions, {Epsilon}, "TransitionType" -> NFA];

PackageExport["StatesPartition"]
StatesPartition::usage = "\
StatesPartition[dfa$] returns a list of partition blocks for the states of dfa$ according to the equivalence relation: \
p$ ~ q$ iff there exists no string over the alphabet that is accepted starting from p$ but rejected starting from q$, \
or rejected from p$ but accepted from q$.";
StatesPartition[dfa_?DFAQ, indices_ : False] :=
  Cond[indices, Map[States[dfa, "Index"], #, {-1}]&] @
    Module[{equivQ},
      equivQ[x_, x_] = True;
      equivQ[___] = False;
      SetAttributes[equivQ, Orderless];
      With[
        {
          alph = LanguageAlphabet @ dfa,
          trns = Transitions @ dfa,
          partition =
            CreateDataStructure[
              "DisjointSet", (* Apparently doesn't like packed arrays *)
              Developer`FromPackedArray @ Transpose @ List @ States @ dfa
            ]
        },
        Scan[
          Apply[partition["Unify", ##]&],
          FixedPoint[
            Select[
              (
                equivQ[Sequence @@ #] =
                  AllTrue[
                    Transpose @ Lookup[Lookup[trns, #], alph],
                    Apply @ equivQ
                  ]
              )&
            ],
            (equivQ[Sequence @@ #] = True; #)& /@
              Catenate[
                Subsets[#, {2}]& /@
                  States[dfa, {"Terminal", "Nonterminal"}]
              ]
          ]
        ];
        partition @ "Subsets"
      ]
    ];

(* ::Subsection:: *)
(* Transitions *)

PackageExport["Transitions"]
Transitions::usage = "\
Transitions[dfa$] returns the transitions of dfa$ as a nested association of the form <|q$ -> <|a$ -> r$, $$|>, $$|>, \
where q$ and r$ are states and a$ an alphabet symbol.
Transitions[nfa$] returns the transitions of nfa$ as a nested association of the form <|q$ -> <|a$ -> {r$1, r$2, $$}, $$|>, $$|>, \
where q$ and r$i are states and a$ an alphabet symbol.
Transitions[tf$] returns transitions for the TransitionFunction tf$.";
SetAttributes[Transitions, Listable];
Transitions[DFA[asc_?dfaAscQ]] := asc @ "Transitions";
Transitions[NFA[asc_?nfaAscQ]] := asc @ "Transitions";
Transitions[TransitionFunction[NFA | DFA, trns_]] := trns;
Transitions[g_Graph?FAGraphQ, rest___] := Transitions[FAExpression @ g, rest];

(* ::Subsection:: *)
(* TransitionFunction *)
PackageExport["TransitionFunction"]
TransitionFunction::usage = "\
TransitionFunction[A$] returns a TransitionFunction for the automaton A$.
TransitionFunction[dfa$][q$, a$] returns the state reached by transitioning from state q$ on symbol a$, or Undefined if no such transition exists.
TransitionFunction[nfa$][q$, a$] returns the list of states reached by transitioning from state q$ on symbol a$, or {} if no such transition exists.
TransitionFunction[$$][{q$, a$}] is equivalent to TransitionFunction[$$][q$, a$].
TransitionFunction automatically maps over lists of states and symbols:
* tf$[{q$1, q$2, $$}, a$] returns the list {tf$[q$1, a], tf$[q$2, a] $$}.
* tf$[q$, {a$1, a$2, $$}] returns the list {tf$[q$, a$1], tf$[q$, a$2] $$}.
* tf$[{q$1, q$2, $$}, {a$1, a$2, $$}] returns the array {{tf$[q$1, a$1], tf$[q$1, a$2], $$}, {tf$[q$2, a$1], tf$[q$2, a$2], $$}, $$}.
* Symbols or states which are themselves lists may be wrapped in Key to avoid threading, as in tf$[Key[{$$}], Key[{$$}]].";
TransitionFunction[A_?FAQ] :=
  TransitionFunction[FAType @ A, Transitions @ A];
TransitionFunction[type : (NFA | DFA), trns_][q_, a_] :=
  Lookup[
    Lookup[
      trns,
      q,
      <||>
    ],
    a,
    If[type === NFA, {}, Undefined]
  ];

tf_TransitionFunction[{q_, a_}] := tf[q, a];
TransitionFunction /: MakeBoxes[tf : TransitionFunction[DFA | NFA, _Association], form : (StandardForm | TraditionalForm)] :=
  makeTransitionFunctionSummaryBoxes[tf, form];

(* ::Subsection:: *)
(* TransitionSequence *)

PackageExport["TransitionSequence"]
TransitionSequence::usage = "\
TransitionSequence[nfa$, {a$1, a$2, $$}] returns the sequence of transitions for the input string a$1 a$2 $$ as a list of lists of states.
TransitionSequence[dfa$, {a$1, a$2, $$}] returns the sequence of transitions for the input string a$1 a$2 $$ as a list of states.
TransitionSequence[A$, 'string$'] is equivalent to TransitionSequence[A$, Characters['string$']]
TransitionSequence[spec$$, seq$] returns the subsequence of TransitionSequence[spec $$] specified by seq$ using the standard sequence specification.";
TransitionSequence[A_, str_String, rest___] :=
  TransitionSequence[A, Characters @ str, rest];

(* ::Subsection:: *)
(* UpdateFA *)

PackageExport["UpdateFA"]
UpdateFA::usage = "\
UpdateFA[A$, prop$ -> val$] updates the property prop$ in the finite automaton A$.
UpdateFA[A$, prop$1 -> val$1, prop$2 -> val$2, $$] updates multiple properties of A$ at once.

Recognized properties are:
* 'Initial' -> inits$: Set initial states of A$ to inits$.
* 'Terminal' -> terms$: Set terminal states of A$ to terms$
* 'Alphabet' -> {symbs$ $$}: Set the language alphabet of A$.
    - If there are transitions defined on symbols not in {symbs$ $$}, these symbols will still be included in LanguageAlphabet[A$].
    - Not supported for DFAs.
* 'Transitions' -> {{q$, a$} -> x$, $$}: Set the transition from state q$ on symbol a$ to be x$.
    - If A$ is a DFA, x$ should be a state.
    - If A$ is an NFA, x$ should be a list of states.
* 'AddTransitions' -> {{q$, a$} -> xs$, $$}: Add transitions from state q$ on symbol a$ to xs$.
    - xs$ should be a list of states.
    - Not supported for DFAs.
* 'RemoveTransitions' -> {{q$, a$} -> xs$, $$}: Remove transitions from state q$ on symbol a$ to xs$.
    - xs$ should be a list of states.
    - {q$, a$} -> All removes all transitions from q$ on a$.
    - Not supported for DFAs.";
UpdateFA::invprop = "Unknown property ``.";
UpdateFA::badNfaTrnRule = "Invalid transition specification. The right hand side of `` is not a list of states.";
UpdateFA::badDfaInit = "Property \"Initial\" expected a list with exactly one state, but received ``.";
UpdateFA::badNfaInit = "Property \"Initial\" expected a list of states, but received ``.";
UpdateFA::badTerm = "Property \"Terminal\" expected a list of states, but received ``.";
UpdateFA::badAlphabet = "Property \"Alphabet\" expected a list of symbols, but received ``.";
UpdateFA::badDfaOp = "`` is not supported for DFAs. Try converting to an NFA first.";
UpdateFA[
  A_?FAQ,
  props : __Rule?(
    If[
      MatchQ[
        #[[1]],
        "Transitions"
          | "AddTransitions"
          | "RemoveTransitions"
          | "Initial"
          | "Terminal"
          | "Alphabet"
      ],
      True,
      (
        Message[UpdateFA::invprop, #[[1]]];
        False
      )
    ] &
  )
] :=
  Switch[FAType @ A,
    NFA, updateNFA,
    DFA, updateDFA
  ][
    FAExpression @ A,
    Lookup[
      GroupBy[
        {props},
        MatchQ[_?(StringEndsQ["Transitions"]) -> _]
      ],
      {True, False},
      {}
    ]
  ];

(* ::Subsection:: *)
(* RenameStates *)

PackageExport["RenameStates"]
RenameStates::usage = "\
RenameStates[A$, f$] returns an automaton equivalent to A$, with states {f$[q$1], f$[q$2], $$}, where {q$1, q$2, $$} are the original states of A$.
RenameStates[A$, 'Index'] renames each state with its index in States[A$]
RenameStates[A$, 'DepthFirstIndex'] renames states with positive integers in depth-first search order.";
RenameStates[A_?FAQ, "Index"] :=
  RenameStates[A, stateIndices @ A];
RenameStates[A_?FAQ, "DepthFirstIndex"] :=
  Module[
    {
      newinits, convert,
      mapNames =
        If[isDFA @ A,
          Map,
          Map[#1, #2, {2}]&
        ] ,
      i = 1,
      trns = Transitions @ A,
      newTrns = <||>
    },
    convert[id_] :=
      With[{ newid = (convert[id] = i++) },
        newTrns[newid] = mapNames[convert, trns @ id];
        newid
      ];
    newinits = convert /@ States[A, "Initial"];
    While[True,
      convert @ First[
        Complement[
          States @ A,
          specificArguments @ convert
        ],
        Break[]
      ]
    ];
    makeFAType[A][
      newTrns,
      newinits,
      convert /@ States[A, "Terminal"],
      LanguageAlphabet @ A
    ]
  ];
RenameStates[A_?FAQ, f_] :=
  With[
    {
      type = FAType @ A,
      asc = FAExpression[A][[1]]
    },
    If[type === NFA,
      makeNFA,
      makeDFA
    ][
      KeyMap[f,
        Map[f,
          Transitions @ A,
          {
            If[type === NFA, 3, 2]
          }
        ]
      ],
      f /@ States[A, "Initial"],
      f /@ States[A, "Terminal"]
    ]
  ];

(* ::Subsection:: *)
(* RemoveUnreachableStates *)

PackageExport["RemoveUnreachableStates"]
RemoveUnreachableStates::usage = "\
RemoveUnreachableStates[A$] returns an automaton whose states are ReachableStates[A$], i.e. A$ with unreachable states removed.";
RemoveUnreachableStates[A_?FAQ] :=
  With[{reachables = ReachableStates @ A},
    If[Length @ reachables == StateCount @ A,
      A,
      makeFAType[A][
        KeyTake[Transitions @ A, reachables],
        States[A, "Initial"],
        Intersection[States[A, "Terminal"], reachables],
        LanguageAlphabet @ A
      ]
    ]
  ];

(* ::Subsection:: *)
(* CreateUniqueState *)

PackageExport["CreateUniqueState"]
CreateUniqueState::usage = "\
CreateUniqueState[] is equivalent to CreateUUID['state-'].
CreateUniqueState[A$] returns an integer, string, or symbol suitable for use as a state name in the automaton A$.
* The returned name attempts to follow the naming conventions of A$
  - If every state in A$ is an integer, the generated name will be the integer one greater than the maximum state name in A$.
  - If every state in A$ is a string, the generated name will be a string.
  - If every state in A$ is a symbol, the generated name will be a symbol.
  - If no simple convention is identified, the generated name will be a string obtained using CreateUUID.
CreateUniqueState[$$, n$] generates a list of n$ state names.";
CreateUniqueState[] := CreateUUID["state-"];
CreateUniqueState[n_Integer] := Table[CreateUniqueState[], {n}];
CreateUniqueState[A_?FAQ] := First @ CreateUniqueState[A, 1];
CreateUniqueState[A_?FAQ, n_Integer?Positive] :=
  With[{states = States @ A},
    Switch[states,
      {___Integer},
      Array[
        Identity,
        n,
        Max @ states + 1
      ],

      {___String},
      extendStateNames[states, n],

      {___Symbol},
      extendStateNames[SymbolName /@ states, n, True],

      _,
      Table[
        CreateUniqueState[],
        {n}
      ]
    ]
  ];

(* ::Subsection:: *)
(* FAComplement *)

PackageExport["FAComplement"]
FAComplement::usage = "\
FAComplement[A$] returns an automaton of the same type as A$ for the complement of the language of A$.";
FAComplement[A_?FAQ] :=
  With[
    {
      cfa = Cond[FAType @ A === NFA, CompleteNFA] @ A
    },
    makeFAType[cfa][
      Transitions @ cfa,
      States[cfa, "Initial"],
      States[cfa, "Nonterminal"],
      LanguageAlphabet @ cfa
    ]
  ];

(* ::Subsection:: *)
(* FAReversal *)

PackageExport["FAReversal"]
FAReversal::usage = "FAReversal[A$] returns an NFA recognizing the reversal of the language of automaton A$.";
FAReversal[A_?FAQ] :=
  NFA[
    Merge[
      Switch[FAType[A],
        NFA, Function[{x, lst}, (Thread[#2 -> {#1 -> x}, List, 1]) & @@@ lst],
        DFA, Function[{x, lst}, (#2 -> {#1 -> x}) & @@@ lst]
      ] @@@ ToRules @ FAExpression @ A,
      Merge[#, Identity] &
    ],
    States[A, "Terminal"],
    States[A, "Initial"]
  ];

(* ::Subsection:: *)
(* FAIntersection *)

PackageExport["FAIntersection"]
FAIntersection::usage = "\
FAIntersection[A$1, A$2, $$] returns a DFA for the intersection of the languages of the automata A$i.";
FAIntersection[A_?FAQ] := A;
FAIntersection[dfas : Repeated[_?DFAQ, {2, Infinity}]] :=
  productDFA[
    dfas,
    Catenate @ States[{dfas}, "Initial"],
    makeTerminalPredicate[
      States[{dfas}, "Terminal"],
      And,
      MemberQ
    ]
  ];
FAIntersection[As : Repeated[_?FAQ, {2, Infinity}]] :=
  productDFA[
    As,
    EpsilonClosure /@ {As},
    makeTerminalPredicate[
      States[{As}, "Terminal"],
      And,
      ContainsAny
    ]
  ];

(* ::Subsection:: *)
(* FAUnion *)

PackageExport["FAUnion"]
FAUnion::usage = "\
FAUnion[A$1, A$2, $$] returns a DFA for the union of the languages of the automata A$i.";
FAUnion[A_?FAQ] := A;
FAUnion[dfas : Repeated[_?DFAQ, {2, Infinity}]] :=
  productDFA[
    dfas,
    Catenate[States[{dfas}, "Initial"]],
    makeTerminalPredicate[
      States[{dfas}, "Terminal"],
      Or,
      MemberQ
    ]
  ];
FAUnion[As : Repeated[_?FAQ, {2, Infinity}]] :=
  productDFA[
    As,
    EpsilonClosure /@ {As},
    makeTerminalPredicate[
      States[{As}, "Terminal"],
      Or,
      ContainsAny
    ]
  ];

(* ::Subsection:: *)
(* FADifference *)

PackageExport["FADifference"]
FADifference::usage = "\
FADifference[A$, B$1, B$2, $$] returns an NFA for the language consisting of words accepted by A$ and not accepted by any of the automata B$i.";
FADifference[A_?FAQ, Bs__?FAQ] :=
  FAIntersection[
    A,
    FAComplement @ FAUnion @ Bs
  ];

(* ::Subsection:: *)
(* FASymmetricDifference *)

PackageExport["FASymmetricDifference"]
FASymmetricDifference::usage = "\
FASymmetricDifference[A$1, A$2] returns a DFA for the symmetric difference of the languages of automata A$1 and A$2.";
FASymmetricDifference[dfas : Repeated[_?DFAQ, {2}]] :=
  productDFA[
    dfas,
    Catenate[States[{dfas}, "Initial"]],
    makeTerminalPredicate[
      States[{dfas}, "Terminal"],
      Xor,
      MemberQ
    ]
  ];
FASymmetricDifference[As : Repeated[_?FAQ, {2}]] :=
  productDFA[
    As,
    EpsilonClosure /@ {As},
    makeTerminalPredicate[
      States[{As}, "Terminal"],
      Xor,
      ContainsAny
    ]
  ];

(* ::Subsection:: *)
(* FAConcatenation *)

PackageExport["FAConcatenation"]
FAConcatenation::usage = "\
FAConcatenation[A$1, A$2, $$] gives an NFA accepting the concatenation of the languages of automata A$i.";
FAConcatenation[As : Repeated[_?FAQ, {2, Infinity}]] :=
  With[
    {
      nfas =
        MapIndexed[
          RenameStates[
            ToNFA @ #1,
            With[{i = First @ #2},
              Subscript[#, i]&
            ]
          ]&,
          {As}
        ],
      n = Length @ Unevaluated @ As
    },
    NFA[
      Join @@ Table[
        If[i < n,
          With[
            {
              nextInits = States[nfas[[i + 1]], "Initial"]
            },
            MapAt[
              Merge[
                {
                  #,
                  <|Epsilon -> nextInits|>
                },
                Apply @ Union
              ] &,
              Transitions @ nfas[[i]],
              List @* Key /@ States[nfas[[i]], "Terminal"]
            ]
          ],
          Transitions @ nfas[[i]]
        ],
        {i, n}
      ] ,
      States[First @ nfas, "Initial"],
      States[Last @ nfas, "Terminal"]
    ]
  ];

(* ::Subsection:: *)
(* FAClosure *)

PackageExport["FAClosure"]
FAClosure::usage = "FAClosure[A$] returns an NFA for the closure of the language of automaton A$ with respect to concatenation.";
FAClosure[A_?FAQ] :=
  With[
    {
      q0 = CreateUniqueState @ A,
      nfa = ToNFA @ A,
      type = FAType @ A,
      terms = States[A, "Terminal"],
      inits = States[A, "Initial"]
    },
    makeNFA[
      Merge[
        {
          Transitions @ nfa,
          <|q0 -> <|Epsilon -> inits|>|>,
          AssociationMap[
            (<|Epsilon -> {q0}|>)&,
            terms
          ]
        },
        Merge @ Apply @ Union
      ],
      {q0},
      Append[terms, q0],
      Union[LanguageAlphabet @ A, {Epsilon}]]
  ];

isDFA[A_] := FAType @ A === DFA;
isNFA[A_] := FAType @ A === NFA;

(* ::Section:: *)
(* Package Scope *)

PackageScope["stateIndices"]
stateIndices::usage = "StateIndices[A] returns the association <|q -> i, ...|>, where q are states in A, and i is the position of q in States[A]";
stateIndices[A_] :=
  First /@ PositionIndex @ States @ A;

PackageScope["makeFAType"]
makeFAType::usage = "makeFAType[A} returns makeNFA if A is an NFA, or makeDFA if A is a DFA.";
makeFAType[A_] :=
  Switch[FAType @ A,
    NFA, makeNFA,
    DFA, makeDFA
  ];

PackageScope["emptyFAQ"]
emptyFAQ::usage = "emptyFAQ[A] returns true if A is an automaton whose language is {}";
emptyFAQ[A_?FAQ] :=
  With[
    {
      groupfn = If[DFAQ @ A, DeleteDuplicates, Apply @ Union],
      terms = States[A, "Terminal"],
      trns = Transitions @ A
    },
    Or[terms === {},
      Not @ Module[{mark},
        mark[id_] :=
          Or[
            mark[id] = MemberQ[terms, id],
            AnyTrue[groupfn @ Values @ trns @ id, mark]
          ];
        AnyTrue[States[A, "Initial"], mark]
      ]
    ]
  ];

(* ::Section:: *)
(* Private *)

transitionsType[transitions_] :=
  Switch[transitions,
    <|(_ -> <|(_ -> _?(KeyExistsQ[transitions, #]&))...|>)...|>, DFA,
    <|(_ -> <|(_ -> {___?(KeyExistsQ[transitions, #]&)})...|>)...|>, NFA,
    _, None
  ];

indexSlotPart[x_, {i_}] := Hold[x, #1[[i]]];

makeTerminalPredicate[termStates_, outer_, inner_] :=
  With[
    {
      body = MapIndexed[indexSlotPart, termStates]
    },
    outer @@@ Apply[
      inner,
      body &,
      {2}
    ]
  ];

productStateTerminalPairNoneTrue[A1_, A2_, pred_] :=
  Module[{t1, t2},
    t1[_] = False;
    Scan[
      (t1[#] = True) &,
      States[A1, "Terminal"]
    ];
    t2[_] = False;
    Scan[(t2[#] = True)&,
      States[A2, "Terminal"]
    ];

    Catch[
      scanProductDFA[
        Apply[
          If[FAType[A1] === DFA === FAType[A2],
            If[
              pred[
                t1 @ #1,
                t2 @ #2
              ],
              Throw @ False
            ]&,
            If[
              pred[
                AnyTrue[#1, t1],
                AnyTrue[#2, t2]
              ],
              Throw @ False
            ]&
          ]
        ],
        A1,
        A2
      ];
      True
    ]
  ];

commonPrefix[{s_, ss__String}] :=
  With[
    {
      n = StringLength @ s,
      strs = {ss}
    },
    Block[
      {
        np,
        prefix = "",
        i = 1
      },
      While[i <= n,
        If[
          AllTrue[
            strs,
            StringStartsQ[
              np = StringTake[s, i++]
            ]
          ],
          prefix = np,
          Return @ prefix
        ]
      ];
      prefix
    ]
  ];
commonPrefix[symbs : {__Symbol}] := commonPrefix[SymbolName /@ symbs];

extendStateNames[stringNames_, n_, symbols_ : False] :=
  If[
    !symbols &&
      AllTrue[
        stringNames,
        StringMatchQ @ WordCharacter
      ],
    With[
      {
        charRange = MinMax[ToCharacterCode @ stringNames]
      },
      If[
        Or[
          (97 <= charRange[[1]] && charRange[[2]] + n <= 122),
          (65 <= charRange[[1]] && charRange[[2]] + n <= 90)
        ],
        Array[
          FromCharacterCode,
          n,
          charRange[[2]]
        ],
        Table[
          CreateUUID[],
          {n}
        ]
      ]
    ],
    With[
      {
        prefix = commonPrefix[stringNames]
      },
      If[StringLength @ prefix > 0,
        If[symbols,
          Unique @ ConstantArray[prefix, n],
          With[
            {
              suffixes = StringReplace[stringNames, prefix -> ""]
            },
            If[
              AllTrue[
                suffixes,
                StringMatchQ[DigitCharacter..]
              ],
              Array[
                prefix <> ToString[#] &,
                n,
                Max[ToExpression /@ suffixes] + 1
              ],
              firstReaped[
                Block[{i = 1, generated = 0},
                  While[generated < n,
                    If[!MemberQ[suffixes, ToString @ i],
                      Sow[prefix <> ToString @ i, "gen"]
                    ];
                    i++
                  ]
                ],
                "gen"
              ]
            ]
          ]
        ],
        If[symbols,
          Table[
            Unique[],
            {n}
          ],
          Table[
            CreateUUID[],
            {n}
          ]
        ]
      ]
    ]
  ];
