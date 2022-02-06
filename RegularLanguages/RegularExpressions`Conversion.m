(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: RegularExpressions`Conversion *)
(* :Context: RegularExpressions`Conversion` *)
(* :Author: Adam Smith *)
(* :Date: 2022-02-01 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

(* Declare package context *)
Package["RegularLanguages`"]

(* ::Section:: *)
(* Patterns *)

stringFormP = OutputForm | InputForm | StandardForm | TraditionalForm;

(* ::Section:: *)
(* Package Export *)

(* ::Subsection:: *)
(* ParseRE *)

PackageExport["ParseRE"]
ParseRE::usage = "\
ParseRE[str] converts a regex in string form to an expression in terms of REUnion, REConcat, and REStar.
Recognized constructs are (from greatest to least precedence)
* Prefix '\\' escapes the next character.
* Round parentheses '(' and ')' indicate grouping.
* Postfix '*' is parsed as closure.
* Juxtaposition is interpreted as concatenation.
* Infix '|' is parsed as union.
All other characters are interpreted as string literals.

Options:
'InactiveHeads' -> True | False | Full
* False: The parsed expression uses REUnion, REConcat, and REStar, which may reorder and simplify terms.
* True: The parsed expression uses Inactive[REUnion], Inactive[REConcat], and Inactive[REStar] as heads.
  - n-ary unions/concatenations and nested stars are flattened
  - An active RELiteral can be obtained by calling Activate on the returned expression.
* Full: The parsed expression uses Inactive[REUnion], Inactive[REConcat], and Inactive[REStar] as heads, and \
union/concatenation are handled as binary operations.
  - Nested stars are not flattened.
  - n-ary unions/concatenations are nested with left-associativity.";
ParseRE::parsererr = "Something happened at input `1` in `2`";
ParseRE::badexpect = "Was expecting `1`, but recieved `2`.";
Options[ParseRE] = { "InactiveHeads" -> False };
OptionChecks[ParseRE] = { "InactiveHeads" -> True | False | Full };
ParseRE[string_String, CheckedOptions @ ParseRE] :=
  Block[{union, concat, star},
    SetAttributes[{union, concat, star}, Flat];
    With[
      {
        queue =
          CreateDataStructure[
            "Queue",
            Characters @ string
          ][
            "Push",
            EndOfString
          ],
        expr =
          With[
            {
              op =
                Switch[OptionValue @ "InactiveHeads",
                  True, <|"|" -> union, "." -> concat, "*" -> star|>,
                  False, <|"|" -> REUnion, "." -> REConcat, "*" -> REStar|>,
                  Full, <|"|" -> Inactive[REUnion], "." -> Inactive[REConcat], "*" -> Inactive[REStar]|>
                ]
            },
            op[#1][##2]&
          ],
        binaryQ = MatchQ["|"],
        postfixQ = MatchQ["*"],
        varQ = (StringQ[#] && StringMatchQ[#, RegularExpression["[^*()|\\\\]"]]) &,
        concatableQ = StringQ[#] && StringMatchQ[#, RegularExpression["[^*)|]"]] &,
        prec = <|"|" -> 0, "." -> 1, "*" -> 2|>,
        rprec = <|"|" -> 1, "." -> 2, "*" -> 2|>,
        nextPrec =
          <|
            "|" -> 0,
            "." -> 1,
            "*" -> 2 (* 1 to disallow repeated star *)
          |>
      },
      Module[{parse, expectAfter, escape},
        expectAfter[return_, c_] :=
          With[{d = queue @ "Pop"},
            If[MatchQ[d, c],
              return,
              Throw[
                Failure["SyntaxError",
                  <|
                    "MessageTemplate" -> "Expected `1` at position `2` but received `3`.",
                    "MessageParameters" -> {c, StringLength[string] - queue["Length"], d}
                  |>
                ]
              ]
            ]
          ];
        escape[c_] :=
          With[{d = queue @ "Pop"},
            If[StringQ @ d && StringMatchQ[d, RegularExpression["[*()|\\\\]"]],
              d,
              Throw[
                Failure["SyntaxError",
                  <|
                    "MessageTemplate" -> "Unrecognized escape sequence `1``2` at position `3`.",
                    "MessageParameters" -> {c, d, StringLength @ string - queue @ "Length"}
                  |>
                ]
              ]
            ]
          ];

        parse[p_] := Module[
          {
            s,
            r = 2,
            tree =
              Switch[queue @ "Peek",
                "(",
                expectAfter[
                  queue @ "Pop"; parse @ 0,
                  ")"
                ],

                "\\", escape @ queue @ "Pop",

                _?varQ, queue @ "Pop",

                _, Epsilon
              ],
            next = queue @ "Peek"
          },
          While[True,
            tree =
              Which[
                binaryQ @ next && p <= prec @ next <= r,
                expr[
                  s = queue @ "Pop",
                  tree,
                  parse @ rprec @ s
                ],

                postfixQ @ next && p <= prec @ next <= r,
                expr[
                  s = queue @ "Pop",
                  tree
                ],

                concatableQ @ next && p <= prec @ "." <= r,
                expr[
                  s = ".",
                  tree,
                  parse @ rprec @ s
                ],

                True, Break[]
              ];
            {r, next} = {nextPrec @ s, queue @ "Peek"}
          ];
          tree
        ];

        (*Algorithm start*)
        Cond[TrueQ @ OptionValue @ "InactiveHeads",
          ReplaceAll @ {
            union -> Inactive[REUnion],
            concat -> Inactive[REConcat],
            star -> Inactive[REStar]
          }
        ] @ Catch @ expectAfter[
          parse @ 0,
          EndOfString
        ]
      ]
    ]
  ];

(* ::Subsection:: *)
(* RandomRE *)

PackageExport["RandomRE"]
RandomRE::usage = "\
RandomRE[n$, {symbols$$}] returns a random regular expression of length n$ on the given symbols.
RandomRE[n$, k$] returns a random regular expression of length n$ on an alphabet of k$ symbols.
* Default symbols are 'a', 'b', $$ (ascii character range 97 to 97 + k) if k$ \[LessEqual] 26, or 'x1', 'x2', $$, 'xk' otherwise.

Options:
'EpsilonFrequency' -> _?(Between[{0, 1}])
The frequency with which Epsilon should appear as a child of REUnion.

'StarFrequency' -> _?(Between[{0, 1}])
The frequency with which subexpressions should be wrapped in REStar.

'AlphabetFunction' -> _
Function to generate alphabet symbols, applied to {symbols$$} or Range[k$]

'InactiveHeads' -> True | False | Full
* False: The generated expression uses heads REUnion, REConcat, and REStar.
  - These operators' may automatically simplify the generated expression, resulting in a length less than n$.
* True: The generated expression uses heads Inactive[REUnion], Inactive[REConcat], and Inactive[REStar].
  - Length of generated expressions guarenteed to be exactly n$.
  - Generated expressions have flat n-ary unions/concatenations, and no nested stars.
  - An active RELiteral can be obtained by calling Activate on the returned expression.
* Full: The parsed expression uses Inactive[REUnion], Inactive[REConcat], and Inactive[REStar] as heads with \
strictly binary union/concatenation.
  - Generated expressions have nested binary unions/concatenations and arbitrarily nested stars.";
Options[RandomRE] =
  {
    "EpsilonFrequency" -> 0.01,
    "StarFrequency" -> 0.5,
    "InactiveHeads" -> False,
    "AlphabetFunction" -> Automatic
  };
OptionChecks[RandomRE] =
  {
    "EpsilonFrequency" -> _?(Between @ {0, 1}),
    "StarFrequency" -> _?(Between @ {0, 1}),
    "InactiveHeads" -> True | False | Full
  };
RandomRE[n_, alphin_, CheckedOptions @ RandomRE] :=
  Block[{S, EU, EUe, EC, F, G, Gs, symb, union, concat, star},
    If[OptionValue @ "InactiveHeads" =!= Full,
      SetAttributes[{union, concat, star}, Flat]
    ];
    With[
      {
        gFreqs = OptionValue @ "StarFrequency" * {1, -1} + {0, 1},
        epsFreqs = OptionValue @ "EpsilonFrequency" * {1, -1} + {0, 1},
        alph = makeAlphabet[alphin, OptionValue @ "AlphabetFunction"]
      },
      {
        minRELength =
          (Count[#, _, {-1}] + Count[#, EU | EUe | EC, {-1}])&,
        maxRELength =
          (Count[#, _, {-1}] + Count[#, S | F, {-1}] + 2 Count[#, EU | EUe | EC, {-1}])&,
        nonTerminatingRules =
          {
            S :> RandomChoice[Unevaluated @ {RandomChoice[epsFreqs -> {EUe, EU}], EC, G}],
            EUe :> RandomChoice[{union[EU, Epsilon], union[G, Epsilon]}],
            EU :> RandomChoice[{union[EU, G], union[G, G]}],
            EC :> RandomChoice[{concat[EC, G], concat[G, G]}],
            F :> RandomChoice[{EC, G}],
            G :> RandomChoice[gFreqs -> {star[Gs], S}],
            Gs -> S
          },
        terminatingRules =
          {
            EUe -> union[G, Epsilon],
            EU -> union[G, G],
            EC -> concat[G, G],
            S | F | G :> RandomChoice[gFreqs -> {star[symb], symb}],
            Gs -> symb
          },
        lengthIncreasingRules =
          {
            S :> RandomChoice[Unevaluated @ {RandomChoice[epsFreqs -> {EU, EUe}], EC}],
            EUe -> union[EU, Epsilon],
            EU -> union[EU, G],
            EC -> concat[EC, G],
            F -> EC
          },
        headRules =
          If[OptionValue @ "InactiveHeads" === False,
            {
              union -> REUnion,
              concat -> REConcat,
              star -> REStar
            },
            {
              union -> Inactive @ REUnion,
              concat -> Inactive @ REConcat,
              star -> Inactive @ REStar
            }
          ]
      },
      {
        re =
          NestWhile[
            ReplaceAll[nonTerminatingRules],
            G,
            (maxRELength @ # < n) &
          ]
      },
      MapAt[
        Replace[lengthIncreasingRules],
        re,
        RandomSample[
          Position[re, S | F | EU | EUe | EC],
          n - minRELength @ re
        ]
      ] //.
        terminatingRules /.
        headRules /.
        symb :> RandomChoice[alph]
    ]
  ];


(* ::Subsection:: *)
(* ToPattern *)

PackageExport["ToPattern"]
ToPattern::usage = "\
ToPattern[regex$] converts regex$ into an equivalent pattern object.
* EmptyLanguage becomes (_ /; False)
* Epsilon becomes PatternSequence[]";
ToPattern[HoldIgnoringInactive @ REUnion[x___]] :=
  ToPattern /@ Alternatives @ x;
ToPattern[HoldIgnoringInactive @ REConcat[x___]] :=
  ToPattern /@ PatternSequence @ x;
ToPattern[HoldIgnoringInactive @ REStar[x_]] :=
  RepeatedNull @ ToPattern @ x;
ToPattern[RELiteral[x_, ___]] := x;
ToPattern[x_] := x;
ToPattern[EmptyLanguage] = Condition[_, False];
ToPattern[Epsilon] = PatternSequence[];

(* ::Subsection:: *)
(* ToStringPattern *)

PackageExport["ToStringPattern"]
ToStringPattern::usage = "\
ToStringPattern[regex$] converts regex$ into an equivalent string pattern object.
* ToStringPattern[regex$] produces output suitable for use anywhere a StringExpression is expected.
ToStringPattern[regex$, form$] uses ToString[literal$, form$] to convert literals in regex$ to strings.

Options:
Method -> 'RegularExpression' | 'StringExpression'
* 'RegularExpression': Return a RegularExpression
* 'StringExpression': Return a StringExpression";
Options[ToStringPattern] = { Method -> "StringExpression" };
OptionChecks[ToStringPattern] = { Method -> "StringExpression" | "RegularExpression" };
ToStringPattern[
  r_,
  form : stringFormP : InputForm,
  CheckedOptions @ ToStringPattern
] :=
  Switch[OptionValue @ Method,
    "StringExpression",
    Block[{$stringExpressionForm = form},
      toStringExpression @ r
    ],

    "RegularExpression",
    Block[{$escapedStringForm = form},
      RegularExpression @ toRegexString @ r
    ]
  ];

(* ::Subsubsection:: *)
(* toStringExpression *)

$stringExpressionForm = InputForm;
toStringExpression[HoldIgnoringInactive @ REUnion[x___]] :=
  toStringExpression /@ Alternatives @ x;
toStringExpression[HoldIgnoringInactive @ REConcat[x___]] :=
  toStringExpression /@ StringExpression @ x;
toStringExpression[HoldIgnoringInactive @ REStar[x_]] :=
  RepeatedNull @ toStringExpression @ x;
toStringExpression[s_String] := s;
toStringExpression[RELiteral[x_, ___]] :=
  ToString[x, $stringExpressionForm];
toStringExpression[x_] :=
  ToString[x, $stringExpressionForm];
toStringExpression[EmptyLanguage] = Condition[_, False];
toStringExpression[Epsilon] = "";

(* ::Subsubsection:: *)
(* toEscapedString *)

$regexStringForm = InputForm;
toRegexString[HoldIgnoringInactive @ REUnion[x___]] :=
  StringRiffle[
    toRegexString /@ {x},
    "|"
  ];
toRegexString[HoldIgnoringInactive @ REConcat[x___]] :=
  StringJoin @ Map[
    Switch[#,
      _REUnion,
      "(" <> toRegexString @ # <> ")",
      _,
      toRegexString @ #
    ] &,
    {x}
  ];
toRegexString[HoldIgnoringInactive @ REStar[x_]] :=
  Switch[x,
    IgnoringInactive[_REConcat | _REUnion],
    "(" <> toRegexString @ x <> ")*",
    _,
    toRegexString @ x <> "*"
  ];
toRegexString[s_String] :=
  If[StringLength @ s > 1,
    "(" <> StringReplace[s, $RegExpSpecialCharacters] <> ")",
    StringReplace[s, $RegExpSpecialCharacters]
  ];
toRegexString[RELiteral[x_, ___]] :=
  toRegexString @ ToString[x, $regexStringForm];
toRegexString[x_] :=
  toRegexString @ ToString[x, $regexStringForm];
toRegexString[EmptyLanguage] = "(.^)";
toRegexString[Epsilon] = "";




(* ::Subsection:: *)
(* ToRE *)

PackageExport["ToRE"]
ToRE::usage = "\
ToRE[A$] converts the automaton A$ to an equivalent regular expression.

Options:
'EliminationOrder' -> Automatic | 'ShortestFirst' | 'LongestFirst' | 'LeastConnectedFirst' | \
'MostConnectedFirst' | 'ForwardOrder' | 'ReverseOrder' | 'RandomOrder' | perm$_List
The heuristic used to determine the order in which states are eliminated while building the final expression.
* Automatic: Automatically select states to favor a shorter expression
* 'ShortestFirst': Eliminate states associated with sorter expressions first
* 'LongestFirst': Eliminate states associated with longer expressions first
* 'LeastConnectedFirst': Eliminate states with fewer incoming / outgoing transitions first
* 'MostConnectedFirst': Eliminate states with more incoming / outgoing transitions first
* 'ForwardOrder': Eliminate states in the order they appear in States[A$]
* 'ReverseOrder': Eliminate states in reverse order of appearance in States[A$]
* perm$_List : Eliminate states in the given order (perm$ must be a valid permutation of 1, 2, $$, StateCount[A$])

'LengthMethod' -> Automatic | 'Postfix' | 'Infix' | 'Alphabetic' | 'ExtendedAlphabetic'
Method used to measure regular expression length. \
Takes the same values as Method option of RELength. \
Only relevant when 'ReductionOrder' is 'ShortestFirst', 'LongestFirst', or Automatic.

'SimplificationFunction' -> _
Function applied to intermediate regular expressions after each reduction step. \
* None: disables simplification.";
ToRE::badorder = "The list `` passed to option \"ReductionOrder\" is not a valid permutation.";
Options[ToRE] =
  {
    "EliminationOrder" -> Automatic,
    "LengthMethod" -> Automatic,
    "SimplificationFunction" -> Automatic
  };
OptionChecks[ToRE] =
  {
    "EliminationOrder" ->
      Automatic
        | "ShortestFirst"
        | "LongestFirst"
        | "LeastConnectedFirst"
        | "MostConnectedFirst"
        | "ForwardOrder"
        | "ReverseOrder"
        | "RandomOrder"
        | _List,

    "LengthMethod" ->
      Automatic
        | "Postfix"
        | "Infix"
        | "Alphabetic"
        | "ExtendedAlphabetic"
  };
ToRE[A_?FAQ, CheckedOptions @ ToRE] :=
  With[
    {
      order =
        UnlessMatch[OptionValue @ "EliminationOrder",
          {__Integer},
          With[{perm = OptionValue @ "EliminationOrder"},
            If[Sort @ perm === Range @ StateCount @ A,
              perm,
              Message[
                ToRE::badorder,
                perm
              ];
              Automatic
            ]
          ]
        ]
    },
    {
      result =
        reduceREArray[
          toRegexArray[
            A,
            ListQ @ order
          ],
          order,
          OptionValue @ "LengthMethod",
          UnlessMatch[
            OptionValue @ "SimplificationFunction",

            Automatic,
            SimplifyRE,

            None,
            Identity
          ]
        ]
    },
    Cond[! REQ @ result, RELiteral] @ result
  ];
ToRE[r_?REQ, CheckedOptions @ ToRE] := r;
ToRE[r_, CheckedOptions @ ToRE] := RELiteral @ r;

(* ::Subsubsection:: *)
(* toRegexArray *)

swap = Function[Null, {#1, #2} = {#2, #1}, HoldAll];
toRegexArray::usage = "\
toRegexArray[A] returns a list { arr, alph }, where:

arr is a SparseArray representing an eNFA with unique initial and terminal states at positions 1 and -1 respectively.
arr[[i, j]] gives the RE associated with transitioning from state i to j. The REs in arr use integers as literals.

alph is an association from integer indices to symbols in the alphabet of A.";
toRegexArray[A_?FAQ, alwaysAddNewStates_ : False] :=
  With[
    {
      trns = Transitions @ A,
      inits = States[A, "Initial"],
      terms = States[A, "Terminal"],
      alphidx =
        PositionIndex[
          LanguageAlphabet[A, "IncludeEpsilon" -> False]
        ][[All, 1]]
    },
    {
      addNewInit =
        alwaysAddNewStates
          || Length @ inits != 1,
      addNewTerm =
        alwaysAddNewStates
          || Length @ terms != 1
          || inits === terms (*init and term index must be distinct*)
    },
    {
      n = StateCount @ A + Boole @ addNewInit + Boole @ addNewTerm
    },
    Module[
      {
        getidx, sowTag,
        idxs = stateIndices @ A + Boole @ addNewInit
      },
      If[!addNewTerm,
        (* We don't have to add a new terminal state *)
        swap[ (* Make the index of the old terminal state n *)
          idxs[First @ terms],
          idxs[[-1]]
        ]
      ];
      If[!addNewInit,
        (* We don't have to add a new initial state *)
        If[idxs[[1]] == 1,
          (* idx[[1]] was not swapped with idx[[-1]] *)
          swap[ (* Make the index of the old initial state 1 *)
            idxs[First @ inits],
            idxs[[1]]
          ],
          (* idx[[1]] was swapped with idx[[-1]] *)
          swap[ (* Make the index of the old initial state idxs[[-1]] *)
            idxs[First @ inits],
            idxs[[-1]]
          ]
        ]
      ];

      ReleaseHold[
        Hold[
          getidx[id_] :=
            With[
              {
                idx = getidx[id] = idxs[id]
              },
              Function[
                If[MemberQ[inits, id],
                  Sow[Epsilon, {{ 1, idx }}]
                ];
                If[MemberQ[terms, id],
                  Sow[Epsilon, {{ idx, n }}]
                ];
                KeyValueMap[
                  Sow[
                    Lookup[alphidx, Key[#1], #],
                    sowTag[idx, #2]
                  ] &,
                  #
                ]
              ] @ trns @ id;
              idx
            ]
        ] /. (* specify different behavior between NFAs vs DFAs *)
          If[FAType @ A === NFA,
            sowTag[a_, b_] :> ({ a, getidx @ # } & /@ b),
            sowTag[a_, b_] :> {{ a, getidx @ b }}
          ]
      ];

      (* Return value *)
      {
        SparseArray[
          DeleteCases[ (* Delete Epsilon self-transitions *)
            Last @ Reap[
              Scan[getidx, inits],
              _,
              Rule[#1, REUnion @@ #2] &
            ],
            HoldPattern[{x_, x_} -> Epsilon]
          ],
          n,
          EmptyLanguage
        ],
        Association @ Reverse[
          Normal @ alphidx,
          2
        ]
      }
    ]
  ];

(* ::Subsubsection:: *)
(* reduceREArray *)

reduceREArray::usage = "\
reduceREArray[{arr, alph}, order, lengthMethod, simp] reduces the output of toRegexArray with order order and length method
lengthMethod and simplification function simp.
arr should be a square matrix representing an eNFA,
alphabet should be an association that converts integers to symbols in the alphabet
";
reduceREArray[{array_?SquareMatrixQ, alphabet_}, order_, lengthMethod_, simp_] :=
  Module[{arr = array},
    With[
      {
        idxs = Range[2, Length @ arr - 1],
        reduce = regexArrayEliminate[arr, simp]
      },
      Switch[order,
        "ForwardOrder" | "ReverseOrder" | "RandomOrder" | {__},
        Scan[
          reduce,
          Switch[order,
            "ForwardOrder",
            Identity,
            "ReverseOrder",
            Reverse,
            "RandomOrder",
            RandomSample,
            {__},
            Permute[#, order] &
          ] @ idxs
        ],

        _,
        With[
          {
            next =
              nextEliminationFunction[
                arr,
                order,
                lengthMethod,
                Break[]
              ]
          },
          While[True,
            reduce @ next[]
          ]
        ]
      ];

      (* get reduced expression *)
      reduce[0] /. alphabet
    ]
  ];

uwExpr[uw_, uv_, vv_, vw_] :=
  REUnion[
    uw,
    REConcat[
      uv,
      REStar @ vv,
      vw
    ]
  ];

SetAttributes[regexArrayEliminate, HoldFirst];
regexArrayEliminate[arr_, simp_][0] := (* recover final expression from reduced array *)
  simp @ REConcat[
    REStar @ simp @ uwExpr[
      arr[[1, 1]],
      arr[[1, -1]],
      arr[[-1, -1]],
      arr[[-1, 1]]
    ],
    arr[[1, -1]],
    REStar @ simp @ uwExpr[
      arr[[-1, -1]],
      arr[[-1, 1]],
      arr[[1, 1]],
      arr[[1, -1]]
    ]
  ];
regexArrayEliminate[arr_, simp_][v_] :=
  With[{vv = arr[[v, v]]},
    arr[[v, v]] = EmptyLanguage;
    Outer[
      (
        arr[[##]] = simp @ uwExpr[
          arr[[##]],
          arr[[#1, v]],
          vv,
          arr[[v, #2]]
        ]
      ) &,
      Flatten @ arr[[All, v]] @ "NonzeroPositions",
      Flatten @ arr[[v]] @ "NonzeroPositions"
    ];
    arr[[v, All]] = arr[[All, v]] = EmptyLanguage;
  ];

SetAttributes[nextEliminationFunction, HoldAll];
nextEliminationFunction[arr_, order_, lengthMethod_, default_] :=
  With[
    {
      incidentExpressions =
        Join[
          Delete[arr[[#]], #] @ "NonzeroValues",
          arr[[All, #]] @ "NonzeroValues"
        ] &
    },
    {
      nextf = First @*
        Switch[order,
          Automatic | "ShortestFirst" | "LongestFirst",
          If[order === "LongestFirst", MaximalBy, MinimalBy][
            Total[
              RELength[#, Method -> lengthMethod] & /@
                incidentExpressions @ #
            ] &
          ],
          "MostConnectedFirst" | "LeastConnectedFirst",
          If[order === "MostConnectedFirst", MaximalBy, MinimalBy][
            Length @* incidentExpressions
          ]
        ]
    },
    Module[
      {
        groups =
          CreateDataStructure[
            "Queue",
            Switch[order,
              Automatic,
              findEliminationGroups @ arr, (* Use grouping heuristic *)

              _,
              { Range[2, Length @ arr - 1] } (* All indices in the same group *)
            ]
          ],
        currentGroup = {}
      },

      (* Return stateful function that yields next index to eliminate on each successive call *)
      Function[
        If[Length @ currentGroup === 0,
          If[groups @ "EmptyQ",
            default,
            (
              currentGroup = groups @ "Pop";
              #0[] (* recurse *)
            )
          ],
          With[{nexti = nextf @ currentGroup},
            currentGroup = DeleteCases[currentGroup, nexti];
            nexti
          ]
        ]
      ]
    ]
  ];

(* ::Subsubsection:: *)
(* findEliminationGroups *)

findEliminationGroups[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  findEliminationGroups[
    arr,
    s,
    UnlessAutomatic[t, Length @ arr],
    UnlessMatch[subset, All, Range @ Length @ arr]
  ];
findEliminationGroups[arr_SparseArray, s_, t_, subset_] :=
  Module[{div, lvl},
    lvl[l_][x_] := (lvl[_][x] = l);

    div[l_][ss : {_, _}] :=
      Scan[lvl @ l, ss];

    div[l_][ss : {u_, v__, w_}] := (
      Scan[lvl @ l, {u, w}];
      With[{bridges = bridgeStates[arr, u, w, ss]},
        Switch[Length @ bridges,
          Length@{v},
          Scan[lvl[l + 1], { v }],

          _?Positive,
          BlockMap[ (* For each pair of sequential bridge states *)
            Function[pair,
              Scan[
                div[l + 1], (* recursively apply div to subautomata *)
                horizontalChop[arr, Sequence @@ pair, ss]
              ]
            ],
            Flatten @ { u, bridges, w },
            2,
            1
          ],

          _, Null
        ]
      ]
    );

    div[0] @ arrangeFirstLast[subset, s, t];
    ReverseSortBy[
      GatherBy[
        DeleteCases[subset, s | t],
        lvl @ Infinity
      ],
      lvl[Infinity] @* First
    ]
  ];

bridgeStates[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  bridgeStates[
    arr,
    s,
    UnlessAutomatic[t, Length @ arr],
    subset
  ];
bridgeStates[arr_SparseArray, s_, t_, subset_] :=
  With[
    {len = Length @ arr},
    {
      adjlists = adjacencyIntersection[arr, subset],
      path = findPath[arr, s, t, subset],
      anc = CreateDataStructure["FixedArray", len],
      min = CreateDataStructure["FixedArray", len],
      max = CreateDataStructure["FixedArray", len]
    },
    If[Length @ path == 0, {}, (* s and t disconnected *)
      Module[{dfs, onPath, setMinMax, notbridges},
        onPath = MatchQ[toAlternatives @ path];

        notbridges =
          Function[v,
            Flatten[
              Range @@@ {
                { min["Part", v] + 1, anc["Part", v] },
                { anc["Part", v] + 1, max["Part", v] - 1 }
              }
            ],
            Listable
          ];
        setMinMax[v_, {}] :=
          min["Part", v] = max["Part", v] = anc["Part", v];
        setMinMax[v_, succVals_] :=
          MapThread[
            (
              #1["Part", v] =
                First[
                  #2[#3, UpTo @ 1],
                  anc["Part", v]
                ]
            ) &,
            {
              { min, max },
              { TakeSmallest, TakeLargest },
              Transpose @ succVals
            }
          ];
        dfs[prev_ : 0][v_] :=
          (
            dfs[_][v] = Null;
            With[
              {
                succs = adjlists[[v]],
                p =
                  First[
                    If[onPath @ v,
                      FirstPosition[path, v]
                    ],
                    0
                  ]
              },
              Scan[
                dfs[
                  anc["Part", v] = Max[p, prev]
                ],
                If[0 < p < Length @ path,
                  rotateToFront[
                    succs,
                    path[[p + 1]]
                  ],
                  succs
                ]
              ];
              setMinMax[
                v,
                If[onPath @ #,
                  ConstantArray[anc["Part", #], 2],
                  Through @ {min, max}["Part", #]
                ] & /@ succs
              ]
            ]
          );

        dfs[] @ s;
        Delete[
          path,
          Transpose @ List @ Flatten @ {
            1,
            -1,
            notbridges @ path
          }
        ]
      ]
    ]
  ];

horizontalChop[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  horizontalChop[arr, s, UnlessAutomatic[t, Length @ arr], subset];
horizontalChop[arr_SparseArray, s_, t_, subset_] :=
  With[
    {
      groups = CreateDataStructure @ "DisjointSet",
      adjlists = adjacencyIntersection[arr, subset]
    },
    Module[{assign},
      assign[v_] :=
        (
          assign[v] = v;
          groups["Insert", v];
          Scan[
            If[# =!= t,
              groups["Unify", v, assign @ #]
            ] &,
            adjlists[[v]]
          ];
          v
        );

      Scan[assign,
        DeleteCases[
          adjlists[[s]],
          s | t
        ]
      ];
      If[groups @ "EmptyQ",
        {{s, t}},
        arrangeFirstLast[#, s, t] & /@
          groups @ "Subsets"
      ]
    ]
  ];

adjacencyIntersection[arr_SparseArray, subset_] :=
  If[subset === All,
    arr["AdjacencyLists"],
    Intersection[subset, #] & /@
      arr @ "AdjacencyLists"
  ];

arrangeFirstLast[l_List, first_, last_] :=
  {
    first,
    Splice @ DeleteDuplicates[l, first | last],
    last
  };
arrangeFirstLast[expr_, first_, last_] :=
  expr /.
    h_[ p : OrderlessPatternSequence[Except[first | last] ...]] :>
      h[first, p, last];

findPath[arr_SparseArray, s_ : 1, t_ : Automatic, subset_ : All] :=
  findPath[
    arr,
    s,
    UnlessAutomatic[t, Length @ arr],
    subset
  ];
findPath[arr_SparseArray, s_, t_, subset_] :=
  With[
    {
      neighbors = adjacencyIntersection[arr, subset]
    },
    Module[{dfs},
      dfs[v_] := (
        dfs[v] = Null;
        Catch[
          If[MemberQ[neighbors[[v]], t],
            Throw[Sow @ t, "path"],
            Scan[dfs, neighbors[[v]]]
          ],
          "path",
          Throw[{v, #1}, #2] &
        ]
      );
      Block[
        {
          $RecursionLimit =
            Max[
              (Length @ arr)^2 + 1,
              $RecursionLimit
            ]
        },
        UnlessMatch[
          Catch[dfs @ s, "path", Flatten[#1] &],
          Null,
          {}
        ]
      ]
    ]
  ];

(* Extracted from StringPattern`Dump`$RegExpSpecialCharacters *)
$RegExpSpecialCharacters =
  {
    "." -> "\\.",
    "\\" -> "\\\\",
    "?" -> "\\?",
    "(" -> "\\(",
    ")" -> "\\)",
    "{" -> "\\{",
    "}" -> "\\}",
    "[" -> "\\[",
    "]" -> "\\]",
    "^" -> "\\^",
    "$" -> "\\$",
    "*" -> "\\*",
    "+" -> "\\+",
    "|" -> "\\|",
    "\.00" -> "\\000"
  };
