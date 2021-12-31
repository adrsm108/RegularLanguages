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
(* Predicates *)

PackageExport["StateQ"]
StateQ::usage = "StateQ[expr] returns True if expr has head NFAState or DFAState";
StateQ[_DFAState | _NFAState] = True;
StateQ[_] = False;

PackageExport["TerminalStateQ"]
TerminalStateQ::usage = "TerminalStateQ[state] gives True if state is a terminal dfa or nfa state.";
TerminalStateQ[(DFAState | NFAState)[_, _, {_, True}]] = True;
TerminalStateQ[(DFAState | NFAState)[_, _, True]] = True;
TerminalStateQ[_] = False;

PackageExport["InitialStateQ"]
InitialStateQ::usage = "InitialStateQ[state] returns True if state is initial.";
InitialStateQ[(DFAState | NFAState)[_, _, {True, _}]] = True;
InitialStateQ[_] = False;

PackageExport["FAQ"]
FAQ::usage = "FAQ[A] yields True if A is a valid representation of a finite automaton.";
FAQ[A_DFA] := DFAQ @ A;
FAQ[A_NFA] := NFAQ @ A;
FAQ[G_Graph] := FAGraphQ @ G;
FAQ[_] = False;

PackageExport["FAGraphQ"]
FAGraphQ::usage = "FAGraphQ[G$] yields True if G$ is a graph with a valid 'Automaton' annotation.";
FAGraphQ[g_Graph] := FAQ[Quiet @ AnnotationValue[g, "Automaton"]];
FAGraphQ[_] = False;

PackageExport["FAExpressionQ"]
FAExpressionQ::usage = "FAExpressionQ[A] returns True if A is a valid Automaton with head NFA or DFA.";
FAExpressionQ[NFA[_?nfaAscQ] | DFA[_?dfaAscQ]] = True;
FAExpressionQ[_] = False;

PackageExport["EmptyFAQ"]
EmptyFAQ::usage = "EmptyFAQ[A] returns True if A is an automaton whose language an empty set.";
EmptyFAQ[A_?FAQ] :=
  With[{groupfn = If[DFAQ @ A, DeleteDuplicates, Apply[Union]],
    states = States @ A, terms = StateNames[A, "Terminal"]},
    Or[terms === {},
      ! Module[{mark},
        mark[id_] := Or[
          mark[id] = MemberQ[terms, id],
          AnyTrue[groupfn @ Values @ states @ id, mark]];
        AnyTrue[StateNames[A, "Initial"], mark]]]];
EmptyFAQ[_] = False;

PackageExport["UniversalFAQ"]
UniversalFAQ::usage = "UniversalFAQ[A] yields True if A is an automaton which accepts all strings over its alphabet.";
UniversalFAQ[A_?FAQ] := EmptyFAQ[FAComplement[A]];
UniversalFAQ[_] = False;

PackageExport["EquivalentFAQ"]
EquivalentFAQ::usage = "\
EquivalentFAQ[A1, A2] is True if A1 and A2 are automata that recognize the same language.
EquivalentFAQ[A1, A2, ...] yields true if all Ai are equivalent automata.
EquivalentFAQ[A] yields true if A is an automaton.";
EquivalentFAQ[A1_?FAQ, A2_?FAQ] := productStateTerminalPairNoneTrue[A1, A2, Xor];
EquivalentFAQ[A_?FAQ] = True;
EquivalentFAQ[_, _] = False;
EquivalentFAQ[Ai : Repeated[_, {3, Infinity}]] := With[
  {m = First @ MinimalBy[{Ai}, StateCount]},
  AllTrue[DeleteCases[{Ai}, m], EquivalentFAQ[m, #] &]
];

PackageExport["SubsetFAQ"]
SubsetFAQ::usage = "\
SubsetFAQ[A1, A2] returns True if the language recognized by automaton A1 is a subset of the language recognized by automaton A2.
SubsetFAQ[A, A1, A2, ...] yields True if SubsetFAQ[A, Ai] is true for all Ai.
SubsetFAQ[A] represents an operator form of SubsetFAQ that can be applied to an expression.";
SubsetFAQ[A1_?FAQ, A2_?FAQ] := productStateTerminalPairNoneTrue[A1, A2, #1 && ! #2 &];
SubsetFAQ[_, _] = False;
SubsetFAQ[A1_][A2_] := SubsetFAQ[A1, A2];
SubsetFAQ[A_, Ai : Repeated[_, {2, Infinity}]] := AllTrue[{Ai}, SubsetFAQ[A]];

(* ::Section:: *)
(* Accessors *)

PackageExport["FAExpression"]
FAExpression::usage = "FAExpression[A$] returns A$ as an automaton with head NFA or DFA.";
FAExpression::inv = "FAExpression expects an automaton expression, or Graph with an 'Automaton' annotation, but recieved `1`.";
FAExpression[A : (_NFA | _DFA)] := A;
FAExpression[g_] :=
  when[_?FAExpressionQ,
    Quiet @ AnnotationValue[g, "Automaton"],
    Message[FAExpression::inv, HoldForm[FAExpression[g]], g, "Automaton"]; $Failed
  ];

PackageExport["FAType"]
FAType::usage = "FAType[A$] returns DFA if A$ is a DFA, or NFA if A$ is an NFA.";
FAType[A_?FAQ] := Head @ FAExpression[A];

PackageExport["StateTransitions"]
StateTransitions::usage = "\
StateTransitions[dfastate$] gives the transition table for a DFAState as the association <|a$1 -> q$1, ...|>, \
where a$i is an alphabet symbol, and q$i is the name of a state.
StateTransitions[nfastate$] gives the transition table for an NFAState as the association <|a$1 -> list$1, ...|>, \
where a$i is an alphabet symbol, and list$i is a list of state names.
StateTransitions[state$, spec$$] is equivalent to Lookup[StateTransitions[state$], spec$$].
StateTransitions[{state$1, state$2, $$}, spec$$] is equivalent to \
Lookup[{StateTransitions[state$1], StateTransitions[state$2], $$}, spec$$], provided all state$i have the same head.";
StateTransitions[(DFAState | NFAState)[_, d_, ___]] := d;
StateTransitions[(DFAState | NFAState)[_, d_, ___], rest__] := Lookup[d, rest];
StateTransitions[states : {___DFAState | ___NFAState}, rest__] := Lookup[states[[All, 2]], rest];

PackageExport["StateName"]
StateName::usage = "StateName[state$] returns the name of state$, where state$ is a valid DFAState or NFAState.";
SetAttributes[StateName, Listable];
StateName[(DFAState | NFAState)[id_, _, ___]] := id;

PackageExport["StateSuccessors"]
StateSuccessors::usage = "\
StateSuccessors[state$] returns the names of states to which state$ has an outgoing transition.
StateSuccessors[state$, {symbols$$}] returns the names of states to which state$ has an outgoing transition on the given symbols$.";
StateSuccessors[NFAState[_, d_, ___], (All | PatternSequence[])] :=
  Union @@ Values @ d;
StateSuccessors[NFAState[_, d_, ___], symbols_List] :=
  Union @@ Lookup[d, symbols, {}];
StateSuccessors[DFAState[_, d_, ___], (All | PatternSequence[])] :=
  DeleteDuplicates @ Values @ d;
StateSuccessors[DFAState[_, d_, ___], symbols_List] :=
  DeleteDuplicates @ Lookup[d, symbols];
StateSuccessors[symbols : (_List | All)][s_?StateQ] :=
  StateSuccessors[s, symbols];

PackageExport["States"]
States::usage = "\
States[A$] returns an association <|id$ -> $state, ...|> of all states in the finite automaton A$.
States[A$, prop$] returns an association <|id$ -> state$, ...|> of states in A$ with the property prop$.
* Valid properties include 'Initial', 'Noninitial', 'Terminal', 'Nonterminal', 'Reachable', and 'Unreachable'.";
States::invprop = "Unknown property `1`.";
States[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  asc @ "states";
States[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], prop_] :=
  Switch[prop,
    "Initial", KeyTake[asc @ "states", asc @ "initial"],
    "Terminal", KeyTake[asc @ "states", asc @ "terminal"],
    "Noninitial", KeyDrop[asc @ "states", asc @ "initial"],
    "Nonterminal", KeyDrop[asc @ "states", asc @ "terminal"],
    "Reachable", KeyTake[asc @ "states", TransitiveClosure[asc @ "initial", asc @ "states"]],
    "Unreachable", KeyDrop[asc @ "states", TransitiveClosure[asc @ "initial", asc @ "states"]],
    _, Message[States::invprop, prop]
  ];

PackageExport["StateList"]
StateList::usage = "\
StateList[A$] returns a list of states in automaton A$.
StateList[A$, prop$] returns returns a list of states in automaton A$ with the property prop$.
* See usage of States for valid properties.
* StateList[A$, prop$] is equivalent to Values[States[A$, prop$]], but more efficient.";
StateList::invprop = "Unknown property `1`.";
StateList[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  Values @ asc @ "states";
StateList[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], prop_] :=
  Switch[prop,
    "Initial", Lookup[asc @ "states", asc @ "initial"],
    "Terminal", Lookup[asc @ "states", asc @ "terminal"],
    "Noninitial", Lookup[asc @ "states", Complement[Keys @ asc @ "states", asc @ "initial"]],
    "Nonterminal", Lookup[asc @ "states", Complement[Keys @ asc @ "states", asc @ "terminal"]],
    "Reachable", Lookup[asc @ "states", TransitiveClosure[asc @ "initial", asc @ "states"]],
    "Unreachable",
    Lookup[asc @ "states",
      Complement[
        Keys @ asc @ "states",
        TransitiveClosure[asc @ "initial", asc @ "states"]
      ]
    ],
    _, Message[StateNames::invprop, prop]
  ];
StateList[g_Graph?FAGraphQ, rest___] :=
  StateList[FAExpression @ g, rest];

PackageExport["StateNames"]
StateNames::usage = "\
StateNames[A$] returns a list of state names for the DFA or NFA A.
StateNames[A$, prop$] gives the StateNames for states with property prop.
* See usage of States for valid properties.";
StateNames::invprop = "Unknown property `1`.";
SetAttributes[StateNames, Listable];
StateNames[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  Keys @ asc @ "states";
StateNames[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], prop_] :=
  Switch[prop,
    "Initial", asc @ "initial",
    "Terminal", asc @ "terminal",
    "Noninitial", Complement[Keys @ asc @ "states", asc @ "initial"],
    "Nonterminal", Complement[Keys @ asc @ "states", asc @ "terminal"],
    "Reachable", TransitiveClosure[asc @ "initial", asc @ "states"],
    "Unreachable", Complement[Keys @ asc @ "states", TransitiveClosure[asc @ "initial", asc @ "states"]],
    _, Message[StateNames::invprop, prop]
  ];
StateNames[g_Graph?FAGraphQ, rest___] :=
  StateNames[FAExpression @ g, rest];

PackageExport["StateIndices"]
StateIndices::usage = "\
StateIndices[A$] returns the association <|q$1 -> index$1, q$2 -> index$2, $$|>, where q$i is the name of a state in A$, \
and index$i is its position in States[A$].";
SetAttributes[StateIndices, Listable];
StateIndices[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  First /@ PositionIndex[Keys @ asc @ "states"];
StateIndices[g_Graph?FAGraphQ] :=
  StateIndices[FAExpression @ g];

PackageExport["StateCount"]
StateCount::usage = "\
StateCount[A$] returns the number of states in the automaton A$.
StateCount[A$, prop] returns the number of states in A$ with property prop$.
* See usage of States for valid properties.";
StateCount::invprop = "Unknown property `1`.";
SetAttributes[StateCount, Listable];
StateCount[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] :=
  Length[asc @ "states"];
StateCount[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], prop_] :=
  Switch[prop,
    "Initial", Length @ asc @ "initial",
    "Terminal", Length @ asc @ "terminal",
    "Noninitial", Length @ asc @ "states" - Length @ asc @ "initial",
    "Nonterminal", Length @ asc @ "states" - Length @ asc @ "terminal",
    "Reachable", Length @ TransitiveClosure[asc @ "initial", asc @ "states"],
    "Unreachable", Length @ asc @ "states" - Length @ TransitiveClosure[asc @ "initial", asc @ "states"],
    _, Message[StateCount::invprop, prop]
  ];
StateCount[g_Graph?FAGraphQ, rest___] := StateCount[FAExpression @ g, rest];

(* ::Section:: *)
(* Mutators *)

PackageExport["AddTransitions"]
AddTransitions::usage = "\
AddTransitions[nfastate, a -> {q1, q2, ...}] returns an NFAState s where s[a] = Union[nfastate[a], {q1, q2, ...}]
AddTransitions[nfastate, {a1 -> {q1, q2, ...}, ...}] returns an NFAState with the specified transitions added.
AddTransitions[rules] returns an operator form of AddTransitions.";
AddTransitions[NFAState[id_, d_, rest___], trns : _Rule | KeyValuePattern[{}]] :=
  NFAState[id, Merge[{d, trns}, Apply[Union]], rest];
AddTransitions[trns : _Rule | KeyValuePattern[{}]] := OperatorApplied[AddTransitions][trns];

PackageExport["SetInitial"]
SetInitial::usage = "\
SetInitial[A$, {states$}] returns a copy of the finite automaton A$ whose initial states are {states$}.
SetInitial[state$, bool$] returns a copy of state$ where InitialStateQ[SetInitial[state$, bool$]] == bool$.
SetInitial[bool$] is an operator form of SetInitial that can be applied to states." ;
SetInitial[s : (NFAState | DFAState)[_, _], init : (True | False)] :=
  Append[s, {init, False}];
SetInitial[s : (NFAState | DFAState)[_, _, {_, term_} | term_], init : (True | False)] :=
  ReplacePart[s, 3 -> {init, term}];
SetInitial[init_][s_] :=
  SetInitial[s, init];

PackageExport["SetTerminal"]
SetTerminal::usage = "\
SetTerminal[state, bool] returns a copy of state with the property that TerminalStateQ[SetTerminal[state, bool]] = bool.
SetTerminal[bool] is an operator form of SetTerminal that can be applied to states.";
SetTerminal[s : (NFAState | DFAState)[_, _], term_] :=
  Append[s, {False, term}];
SetTerminal[s : (NFAState | DFAState)[_, _, {init_, _}], term_] :=
  ReplacePart[s, 3 -> {init, term}];
SetTerminal[s : (NFAState | DFAState)[_, _, _], term_] :=
  ReplacePart[s, 3 -> {False, term}];
SetTerminal[term_][s_] :=
  SetTerminal[s, term];

PackageExport["SetTerminal"]
SetInitialTerminal::usage = "\
SetInitialTerminal[state$, {init$, term$}] returns a copy of state$, s$, where InitialStateQ[s$] == init$ and TerminalStateQ[s$] == term$.
SetInitialTerminal[{init$, term$}] is an operator form of SetInitialTerminal that can be applied to states.";
SetInitialTerminal[s : (NFAState | DFAState)[_, _], term_] :=
  Append[s, {False, term}];
SetInitialTerminal[s : (NFAState | DFAState)[_, _, {init_, _}], term_] :=
  ReplacePart[s, 3 -> {init, term}];
SetInitialTerminal[s : (NFAState | DFAState)[_, _, _], term_] :=
  ReplacePart[s, 3 -> {False, term}];
SetInitialTerminal[term_][s_] :=
  SetTerminal[s, term];


PackageExport["UpdateFA"]
UpdateFA::usage = "UpdateFA[A$, prop$ -> val$, $$] returns a new automaton A$ with the given properties.";
UpdateFA::invprop = "Unknown property `1`.";
UpdateFA[A_?FAQ, newRules : KeyValuePattern[{}]?validUpdateRulesQ] :=
  With[{newInits = Lookup[newRules, "Initial"], newTerms = Lookup[newRules, "Terminal"]},
    Switch[FAType[A],
      NFA,
      DFA,
      With[{
        newInitsMissing = MissingQ @ newInits,
        newTermsMissing = MissingQ @ newTerms,
        states = States @ A,
        inits = StateNames[A, "Initial"],
        terms = StateNames[A, "Terminal"]},
        DFA[
          "initial" -> Check[
            If[newInitsMissing, inits,
              If[loudly[validDFAInitQ, DFA::badinit]@newInits,
                If[KeyExistsQ[states, First @ newInits],
                  newInits,
                  Message[DFA::invinit, First @ newInits]; Throw[$Failed]]
              ]],
            Throw[$Failed]
          ],
          "terminal" -> If[newTermsMissing, terms,
            If[And @@ loudly[KeyExistsQ[states, #]&, DFA::invterm] /@ newTerms,
              newTerms,
              Throw[$Failed]]],
          "alphabet" -> LanguageAlphabet[A],
          "states" -> MapAt[states,
            (applyIf[!newTermsMissing, Set])
            ,
            Key /@ Union[
              If[MissingQ @ newTerms, {}, Union[newTerms, terms]],
              If[MissingQ @ newInits, {}, Union[newInits, inits]]
            ]]

        ]

      ]
    ]
  ];

validUpdateRulesQ[x_] :=
  With[{remaining = KeyDrop[x, {"Initial", "Terminal"}]},
    If[Length @ remaining == 0, True,
      Scan[Message[UpdateFA::invprop, #]&, Keys @ remaining]; False]];


PackageExport["RenameStates"]
RenameStates::usage = "\
RenameStates[A$, f$] returns an automaton isomorphic to A$, with state ids {f$[q$1], f$[q$2], $$}, \
where {q$1, q$2, $$} are the original state ids of A$.";
RenameStates[A_?FAQ, f_] :=
  FAType[A][
    "states" -> Association @@ (updateStateRule[f] /@ States @ A),
    "initial" -> f /@ StateNames[A, "Initial"],
    "terminal" -> f /@ StateNames[A, "Terminal"],
    "alphabet" -> LanguageAlphabet @ A
  ];

PackageExport["DeleteUnreachableStates"]
DeleteUnreachableStates::usage = "\
DeleteUnreachableStates[A$] returns an automaton whose state set is exactly TransitiveClosure[A$]";
DeleteUnreachableStates[A_?FAQ] :=
  With[{tc = TransitiveClosure @ A},
    If[Length @ tc == StateCount @ A, A,
      FAType[A][
        "states" -> KeyTake[States @ A, tc],
        "initial" -> StateNames[A, "Initial"],
        "terminal" -> Intersection[StateNames[A, "Terminal"], tc],
        "alphabet" -> LanguageAlphabet @ A]]];

PackageExport["IndexFA"]
IndexFA::usage = "\
IndexFA[A$] returns a version of $A where the name of each state is its index in StateList[A$].";
IndexFA[A_?FAQ] := RenameStates[A, StateIndices @ A];

PackageExport["ReindexFA"]
ReindexFA::usage = "\
ReindexFA[A$] returns an automaton similar to A$, but whose states are renamed with positive integers in depth-first search order.
* By default, the returned automaton includes only those states which are reachable from the initial.
ReindexFA[A$, True] returns the same, but also keeps disconnected components. The resulting automaton is isomorphic to A$.";
ReindexFA[A_?FAQ, allComponents_ : False] :=
  Module[{newinits, convert, i = 1, oldstates = States[A],
    newstates = CreateDataStructure["HashTable"]},
    convert[id_] := With[{newid = (convert[id] = i++)},
      newstates["Insert", newid -> updateState[oldstates[id], convert]];
      newid];
    newinits = convert /@ StateNames[A, "Initial"];
    While[allComponents,
      convert @ First[Complement[Keys @ oldstates, specificArguments[convert]], Break[]]];
    With[{states = Normal @ newstates},
      FAType[A][
        "states" -> states,
        "initial" -> newinits,
        "terminal" -> (convert[id_] = Nothing; convert /@ StateNames[A, "Terminal"]) ,
        "alphabet" -> If[allComponents, LanguageAlphabet[A], Union @@ (Keys /@ states)]
      ]]
  ];

PackageExport["TransitiveClosure"]
TransitiveClosure::usage = "\
TransitiveClosure[q$, A$] returns the ids of states in the transitive closure of state q$ in the finite automaton A$.
* This is the set of states reachable from q$ by any sequence of transitions.
TransitiveClosure[{q$1, q$2, ...}, A$] returns TransitiveClosure[q$1,A$] \[Union] TransitiveClosure[q$2, A$] \[Union] ...
TransitiveClosure[A$] returns the transitive closure of the initial states of A$.
TransitiveClosure[states$, transitions$] returns the transitive closure of $states according to the given transition specifications.
* transitions$ should be an association or list with elements of the form id$ -> t$id, where t$id is the transition table of state id$ as an association or list of rules.
TransitiveClosure[$$, {a$1, a$2, $$}] finds a transitive closure over the symbols a$1, a$2, $$.
* This is a transitive closure restricted to transitions on a$i.";
TransitiveClosure::invstate = "State `1` not found.";
TransitiveClosure[{}, ___] = {};
TransitiveClosure[A_?FAQ, syms_List : All] :=
  TransitiveClosure[StateNames[A, "Initial"], States @ A, syms];
TransitiveClosure[states : {(_DFAState | _NFAState)..}, rest___] :=
  TransitiveClosure[StateName[states], rest];
TransitiveClosure[ids_List, A_?FAQ, syms_List : All] :=
  TransitiveClosure[ids, States @ A, syms];
TransitiveClosure[id : Except[_List], rest__] :=
  TransitiveClosure[{id}, rest];
TransitiveClosure[ids_List, rules : {(_ -> _) ...}, syms_List : All] :=
  TransitiveClosure[ids, Association @ rules, syms];
TransitiveClosure[ids_List, Q_Association, syms : (_List | All) : All] :=
  Reap[
    Module[{push},
      With[
        {
          succs = Which[
            syms === All,
            Values,

            AllTrue[Q, StateQ],
            StateTransitions[#, syms, Nothing]&,

            True,
            Lookup[#, syms, Nothing]&
          ],
          lvl = Switch[Q, <|(_ -> (_[(_ -> _List) ...] | _NFAState)) ...|>, {2}, _, {1}],
          state = Lookup[Q, Key @ #, Message[TransitiveClosure::invstate, #]; {}] &,
          queue = CreateDataStructure["Queue", (push[Sow[#]] = Null; #) & /@ ids]
        },
        push[id_] := push[Sow[id]] = (queue["Push", id];);
        While[!queue["EmptyQ"],
          Scan[push, succs @ state @ queue["Pop"], lvl]
        ]
      ]
    ]
  ][[2, 1]];

PackageExport["EpsilonClosure"]
EpsilonClosure::usage = "\
EpsilonClosure[q$, A$] gives the \[CurlyEpsilon]-closure---the transitive closure over Epsilon---of state q$ in automaton A$.
EpsilonClosure[{q$1, q$2, ...}, A$] gives EpsilonClosure[q$1, A$] \[Union] EpsilonClosure[q$2, A$] \[Union] $$
EpsilonClosure[A$] computes the \[CurlyEpsilon]-closure of the initial states of A$.
EpsilonClosure[states$, transitions$] finds the \[CurlyEpsilon]-closure of states$ in transitions$, where transitions$ \
can be any transition specification recognized by TransitiveClosure.";
EpsilonClosure[A_?FAQ] :=
  TransitiveClosure[StateNames[A, "Initial"], States @ A, {Epsilon}];
EpsilonClosure[states_, transitions_] :=
  TransitiveClosure[states, transitions, {Epsilon}];

PackageExport["StatesPartition"]
StatesPartition::usage = "\
StatesPartition[dfa$] returns a list of partition blocks for the states of dfa$ according to the equivalence relation: \
p$ ~ q$ iff there exists no string over the alphabet that is accepted starting from p$ but rejected starting from q$, \
or rejected from p$ but accepted from q$.";
StatesPartition[dfa_?DFAQ, indices_ : False] :=
  applyIf[indices, Map[StateNames[dfa, "Index"], #, {-1}]&,
    Module[{equivQ},
      equivQ[x_, x_] = True;
      equivQ[___] = False;
      SetAttributes[equivQ, Orderless];
      With[
        {
          alph = LanguageAlphabet @ dfa,
          states = States @ dfa,
          partition =
            CreateDataStructure[
              "DisjointSet", (* Apparently doesn't like packed arrays *)
              Developer`FromPackedArray @ Transpose @ List @ StateNames @ dfa
            ]
        },
        Scan[
          Apply[partition["Unify", ##]&],
          FixedPoint[
            Select[
              (equivQ[Sequence @@ #] =
                AllTrue[
                  Transpose @ Lookup[StateTransitions /@ Lookup[states, #], alph],
                  Apply @ equivQ
                ])&
            ],
            (equivQ[Sequence @@ #] = True; #)& /@
              Catenate[
                Subsets[#, {2}]& /@ StateNames[dfa, {"Terminal", "Nonterminal"}]
              ]
          ]
        ];
        partition @ "Subsets"
      ]
    ]
  ];

PackageExport["FAComplement"]
FAComplement::usage = "\
FAComplement[A$] returns an automaton of the same type as A$ for the complement of the language of A$.";
FAComplement[A_?FAQ] :=
  FAType[A][
    "states" ->
      Map[
        updateState[#, Identity, {Automatic, !TerminalStateQ @ #}]&,
        States @ A
      ],
    "initial" -> StateNames[A, "Initial"],
    "terminal" -> StateNames[A, "Nonterminal"],
    "alphabet" -> LanguageAlphabet @ A
  ];

PackageExport["FAReversal"]
FAReversal::usage = "FAReversal[A$] returns an NFA recognizing the reversal of the language of A$.";
FAReversal[A_?FAQ] :=
  NFA[
    Merge[
      Switch[FAType[A],
        NFA, Function[{x, lst}, (Thread[#2 -> {#1 -> x}, List, 1]) & @@@ lst],
        DFA, Function[{x, lst}, (#2 -> {#1 -> x}) & @@@ lst]
      ] @@@ ToRules @ FAExpression @ A,
      Merge[#, Identity] &
    ],
    StateNames[A, "Terminal"],
    StateNames[A, "Initial"]
  ];

PackageExport["FAIntersection"]
FAIntersection::usage = "\
FAIntersection[A$1, A$2, ...] returns a DFA for the intersection of the languages recognized by the A$i.";
FAIntersection[A_?FAQ] := A;
FAIntersection[dfas : Repeated[_?DFAQ, {2, Infinity}]] :=
  productDFA[dfas, {Catenate[StateNames[{dfas}, "Initial"]]}, AllTrue[TerminalStateQ]];
FAIntersection[Ai : Repeated[_?FAQ, {2, Infinity}]] :=
  productDFA[Ai, {EpsilonClosure /@ {Ai}}, AllTrue[AnyTrue[TerminalStateQ]]];

PackageExport["FAUnion"]
FAUnion::usage = "\
FAUnion[A$1, A$2, $$] returns a DFA for the union of the languages recognized by the A$i.";
FAUnion[A_?FAQ] := A;
FAUnion[dfas : Repeated[_?DFAQ, {2, Infinity}]] :=
  productDFA[dfas, {Catenate[StateNames[{dfas}, "Initial"]]}, AnyTrue[TerminalStateQ]];
FAUnion[Ai : Repeated[_?FAQ, {2, Infinity}]] :=
  productDFA[Ai, {EpsilonClosure /@ {Ai}}, AnyTrue[AnyTrue[TerminalStateQ]]];

PackageExport["FASymmetricDifference"]
FASymmetricDifference::usage = "\
FASymmetricDifference[A$1, A$2] returns a DFA for the symmetric difference of the languages recognized by A$1 and A$2.";
FASymmetricDifference[dfas : Repeated[_?DFAQ, {2}]] :=
  productDFA[dfas, {Catenate[StateNames[{dfas}, "Initial"]]}, Xor @@ (TerminalStateQ /@ #) &];
ASymmetricDifference[Ai : Repeated[_?FAQ, {2}]] :=
  productDFA[Ai, {EpsilonClosure /@ {Ai}}, Xor @@ (AnyTrue[TerminalStateQ] /@ #) &];

PackageExport["FAConcat"]
FAConcat::usage = "\
FAConcat[A$1, A$2, $$] gives an NFA accepting the concatenation of the languages recognized by the A$i.";
FAConcat[Ai : Repeated[_?FAQ, {2, Infinity}]] :=
  With[{nfas = NFA /@ {Ai}, n = Length @ {Ai}},
    NFA[
      "states" ->
        Join @@ MapIndexed[
          Function[{nfa, pos},
            With[
              {
                itrules =
                  Switch[pos,
                    {1}, {Automatic, False},
                    {n}, {False, Automatic},
                    _, {False, False}],
                i = First @ pos
              },
              applyIf[i < n,
                MapAt[
                  AddTransitions[
                    Epsilon -> (
                      Subscript[#, i + 1]& /@ StateNames[nfas[[i + 1]], "Initial"]
                    )
                  ],
                  {Key @ Subscript[#, i]}& /@ StateNames[nfa, "Terminal"]
                ],
                Association @ Map[
                  updateStateRule[Subscript[#, i]&, itrules], StateList @ nfa
                ]
              ]
            ]
          ],
          nfas
        ],
      "initial" -> Thread @ Subscript[StateNames[nfas[[1]], "Initial"], 1],
      "terminal" -> Thread @ Subscript[StateNames[nfas[[n]], "Terminal"], n],
      "alphabet" -> Union @@ (LanguageAlphabet /@ nfas)
    ]
  ];

PackageExport["FAClosure"]
FAClosure::usage = "FAClosure[A] returns an NFA for the closure of the language recognized by A with respect to concatenation.";
FAClosure[A_?FAQ] :=
  With[
    {
      startid = Unique[],
      terms = StateNames[A, "Terminal"],
      critrule = Epsilon -> StateNames[A, "Initial"]
    },
    NFA[
      "states" ->
        MapAt[
          SetInitial @ False,
          MapAt[
            AddTransitions @ critrule,
            States @ ToNFA @ A,
            List @* Key /@ terms
          ],
          Transpose @ {Key /@ StateNames[A, "Initial"]}
        ] ~ Append ~ (
          startid ->
            NFAState[startid, Association @ critrule, {True, True}]
        ),
      "initial" -> {startid},
      "terminal" -> terms ~ Append ~ startid,
      "alphabet" -> Union[LanguageAlphabet @ A, {Epsilon}]]
  ];
(*

NFATransitionFunction[trns_][stateArg_, symbArg_] :=
  With[
    {
      states = If[ListQ @ stateArg, stateArg, {stateArg}],
      symbols = If[ListQ @ symbArg, symbArg, {symbArg}]
    },


  ]

*)

PackageExport["TransitionFunction"]
TransitionFunction::usage = "TransitionFunction[NFA, trns$] represents a nondeterministic transition function.";
TransitionFunction[A_?FAQ] :=
  TransitionFunction[FAType[A], StateTransitions /@ States @ A];
TransitionFunction[type_, trns_][q_, a_] :=
  Lookup[
    Lookup[
      trns,
      when[_Key, q, Key @ q],
      <||>
    ],
    when[_Key, a, Key @ a],
    If[type === NFA, {}, Undefined]
  ];
TransitionFunction /: MakeBoxes[tf : TransitionFunction[_, _], form : (StandardForm | TraditionalForm)] :=
  makeTransitionFunctionSummaryBoxes[tf, form];



(* ::Section:: *)
(* Private Functions *)

transitionsType[transitions_] :=
  Switch[transitions,
    <|(_ -> <|(_ -> _?(KeyExistsQ[transitions, #]&))...|>)...|>, DFA,
    <|(_ -> <|(_ -> {___?(KeyExistsQ[transitions, #]&)})...|>)...|>, NFA,
    _, None
  ]

productStateTerminalPairNoneTrue[A1_, A2_, pred_] := Module[
  {t1, t2},
  t1[_] = False; Scan[(t1[#] = True) &, StateNames[A1, "Terminal"]];
  t2[_] = False; Scan[(t2[#] = True) &, StateNames[A2, "Terminal"]];
  Catch[scanProductDFA[
    Apply[If[FAType[A1] === DFA === FAType[A2],
      If[pred[t1[#1], t2[#2]], Throw[False]] &,
      If[pred[AnyTrue[#1, t1], AnyTrue[#2, t2]], Throw[False]] &]],
    A1, A2];
  True]
];

