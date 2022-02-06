(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: RegularExpressions *)
(* :Context: RegularExpressions` *)
(* :Author: Adam Smith *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2020 Adam Smith *)

Package["RegularLanguages`"]

(* ::Section:: *)
(* Package Export *)

(* ::Subsection:: *)
(* REUnion *)

PackageExport["REUnion"]
REUnion::usage = "\
REUnion[e$1, e$2, $$] represents a regular expression for the union of expressions e$i.
* Matches the language L$[e$1] \[Union] L$[e$2] \[Union] $$, where L$[e$i] denotes the language of e$i.
e$1 \[VerticalSeparator] e$2 \[VerticalSeparator] $$ is equivalent to REUnion[e$1, e$2, $$] when extended notation is enabled.
* Aliases VerticalSeparator, which is a system symbol with built-in formatting but no default meaning.
* Use \\[VerticalSeparator] operator (alias \[AliasIndicator]|\[AliasIndicator]) for infix input.
* See LoadNotation for more details.";
Default[REUnion] = EmptyLanguage;
REUnion[x_.] := x;
SetAttributes[REUnion, Orderless];
u : REUnion[x_, x_, ___] := Union[Unevaluated @ u];
REUnion[Epsilon, c_REStar , a___] := REUnion[c, a];
SetAttributes[REUnion, {OneIdentity, Flat}];
REUnion[EmptyLanguage, a_] := a;
REUnion[x_, REStar[x_]] := REStar[x];
(*
REUnion /: ToString[u_REUnion] :=
  StringRiffle[ToString /@ (List @@ u), "|"];
*)

(* ::Subsection:: *)
(* REConcat *)

PackageExport["REConcat"]
REConcat::usage = "\
REConcat[e$1, e$2, $$] represents a regular expression for the concatenation of the expressions e$i.
* Matches the language L$[e$1]L$[e$2]$$, where L$[e$i] denotes the language of e$i.
e$1 \[CenterDot] e$2 \[CenterDot] e$3 \[CenterDot] $$ is equivalent to REConcat[e$1, e$2, $$] when extended notation is enabled.
* Aliases CenterDot, which is a system symbol with built-in formatting but no default meaning.
* Use \\[CenterDot] operator (alias \[AliasIndicator].\[AliasIndicator]) for infix input.
* See LoadNotation for more details.";
Default[REConcat] = Epsilon;
REConcat[___, EmptyLanguage, ___] = EmptyLanguage;
REConcat[x_] := x;
REConcat[] = Epsilon;
c : REConcat[___, Epsilon, ___] := DeleteCases[Unevaluated @ c, Epsilon];
SetAttributes[REConcat, {Flat, OneIdentity}];

(* ::Subsection:: *)
(* REStar *)

PackageExport["REStar"]
REStar::usage = "\
REStar[e$] represents a regular expression for closure of expression e$ with respect to concatenation.
* Matches the language {\[CurlyEpsilon]} \[Union] L$[e$] \[Union] L$[e$]L$[e$] \[Union] $$, where L$[e$] denotes the language of e$.
\!\(\*SuperscriptBox[StyleBox[\"e\", \"TI\"], \"*\"]\) is equivalent to REStar[e$] when extended notation is enabled.
* Aliases SuperStar, which is a system symbol with built-in formatting but no default meaning.
* Use shortcut Ctrl+^, * for postfix input.
* See LoadNotation for more details.";
REStar[EmptyLanguage | Epsilon] = Epsilon;
REStar[x_REStar] := Flatten @ x;
(* These only issue messages, never evaluate *)
REStar[_, x__] /; Message[REStar::argx, REStar, Length @ {x} + 1] = Null;
REStar[] /; Message[REStar::argx, REStar, 0] = Null;

(*
REStar /: ToString[REStar[c : (_REConcat | _REUnion)]] := "(" <> ToString @ c <> ")*";
REStar /: ToString[REStar[c_]] := ToString @ c <> "*";
*)

SyntaxInformation[REStar] = {"ArgumentsPattern" -> {_}};

(* ::Subsection:: *)
(* RELiteral *)

PackageExport["RELiteral"]
RELiteral::usage = "\
RELiteral is a symbolic wrapper used to explicitly designate its contents as a literal regular expression.
* Any expression wrapped in RELiteral satisfies REQ.
RELiteral[x$] represents regular expression whose language is {x$}.
RELiteral[x$, i$] is a symbolic wrapper for an indexed literal. Its language is the same as RELiteral[x$].
* Formatted as x$i.";
RELiteral /: MakeBoxes[re : RELiteral[x_, i_], form : (StandardForm | TraditionalForm)] :=
  With[{boxes = MakeBoxes[Subscript[x, i], form]},
    InterpretationBox[boxes, re]
  ];



(* ::Subsection:: *)
(* REQ *)

PackageExport["REQ"]
REQ::usage = "\
REQ[expr$] yields True when expr$ is EmptyLanguage or Epsilon, or has head RELiteral, or satisfies CompoundREQ.
REQ[expr$, patt$] gives True if expr$ is EmptyLanguage or Epsilon, or of the form RELiteral[x, $$] where x matches patt, or satisfies CompoundREQ[expr$, patt$].";
REQ[EmptyLanguage | Epsilon | _RELiteral | _?CompoundREQ] = True;
REQ[_] = False;
REQ[EmptyLanguage | Epsilon, _] = True;
REQ[RELiteral[r_, ___], patt_] := MatchQ[r, patt];
REQ[r_, patt_] := CompoundREQ[r, patt];

(* ::Subsection:: *)
(* CompoundREQ *)

PackageExport["CompoundREQ"]
CompoundREQ::usage = "\
CompoundREQ[expr$] returns True if expr$ has head REUnion, REConcat, or REStar.
CompoundREQ[expr$, patt$] returns True if expr$ has head REUnion, REConcat, or REStar, and every element of LanguageAlphabet[expr$] matches patt$.";
CompoundREQ[IgnoringInactive @ reHeadP] = True;
CompoundREQ[reHeadP] = True;
CompoundREQ[_] = False;
CompoundREQ[r : IgnoringInactive @ reHeadP, patt_] :=
  AllTrue[
    LanguageAlphabet[
      r,
      "IncludeEpsilon" -> False
    ],
    MatchQ[patt]
  ];

(* ::Subsection:: *)
(* REMatchQ *)

PackageExport["REMatchQ"]
REMatchQ::usage = "\
REMatchQ[{a$1, a$2, $$}, regex$] returns True if regex$ matches the string of symbols a$1 a$2 $$ .
REMatchQ['string$', regex$] returns True if regex$ matches the string string$.
REMatchQ[regex$] represents an operator form of REMatchQ.";
REMatchQ[Epsilon | "", r_] :=
  matchesEmptyQ @ r;
REMatchQ[input_String, r_ /; CompoundREQ[r, _String]] :=
  StringMatchQ[
    input,
    ToStringPattern @ r
  ];
REMatchQ[_, r_ /; CompoundREQ[r, _String]] =
  False;
REMatchQ[input_, r_] :=
  MatchQ[input, { ToPattern @ r }];

REMatchQ[r_][input_] :=
  REMatchQ[input, r];

(* ::Subsection:: *)
(* RELength *)

PackageExport["RELength"]
RELength::usage = "\
RELength[regex$] gives the length of the regular expression regex$.
* Equivalent to RELength[regex$, 'Postfix']
RELength[regex$, 'Postfix'] counts the number of symbols in the postfix form of regex$, or equivalently, the number of nodes in its syntax tree.
RELength[regex$, 'Infix'] gives the number of symbols in the standard infix form of regex$, where '|' denotes union and juxtaposition denotes concatenation.
* Parentheses, operators, Epsilon, and EmptyLanguage are all counted.
RELength[regex$, 'Alphabetic']: Counts the number of literals in regex$, not including Epsilon or EmptyLanguage.
Options:
Method -> Automatic | 'Postfix' | 'Infix' | 'Alphabetic' | 'ExtendedAlphabetic'
* Automatic: Same as 'Postfix'
* 'Postfix': Counts the number of symbols in the postfix form of regex$, or equivalently, the number of nodes in its syntax tree
* 'Infix': Counts the number of symbols in the infix form of regex$, using '|' for union and juxtaposition for concatenation
  - Includes parentheses, operators, Epsilon, and EmptyLanguage.
  - Equivalent to StringLength[ToString[regex$]] when literals are single characters.
* 'Alphabetic': Counts the number of literals in regex$, not including Epsilon or EmptyLanguage
* 'ExtendedAlphabetic': Counts the number of literals in regex$, including Epsilon and EmptyLanguage.";
Options[RELength] = {Method -> Automatic};
OptionChecks[RELength] =
  {
    Method -> Automatic | "Postfix" | "Infix" | "Alphabetic" | "ExtendedAlphabetic"
  };
RELength[r_, CheckedOptions @ RELength] :=
  Module[{count = 0},
    Switch[OptionValue @ Method,
      "Postfix" | Automatic,
      (
        count += 1;
        If[
          MatchQ[#, IgnoringInactive @ reHeadP] ,
          Scan[#0, #]
        ]
      ) &,

      "Infix",
      Switch[#1,
        IgnoringInactive @ _REUnion,
        (
          count += Length@#1 - 1;
          If[#2 > 0, count += 2];
          Scan[
            With[{f = #0}, f[#, 0] &],
            #1
          ]
        ),

        IgnoringInactive @ _REConcat,
        (
          (*count+=Length@#1-1;*)
          If[#2 > 1, count += 2];
          Scan[
            With[{f = #0}, f[#, 1] &],
            #1
          ]
        ),

        IgnoringInactive @ _REStar,
        (
          count += 1;
          If[#2 > 2, count += 2];
          Scan[
            With[{f = #0}, f[#, 2] &],
            #1
          ]
        ),

        _,
        count += 1
      ] &,

      "Alphabetic",
      Switch[#,
        IgnoringInactive @ reHeadP,
        Scan[#0, #],

        Epsilon | EmptyLanguage,
        Null,

        _,
        count += 1
      ]&,

      "ExtendedAlphabetic",
      If[MatchQ[#, IgnoringInactive @ reHeadP],
        Scan[#0, #],
        count += 1
      ] &
    ][r, 0] ;
    count
  ];
(* ::Subsection:: *)
(* REPower *)
PackageExport["REPower"]
REPower::usage = "REPower[regex$, n$] generates a regular expression consisting of n$ copies of regex$ concatenated together.";
REPower[r_, n_Integer?NonNegative] :=
  REConcat @@ ConstantArray[r, n];

(* ::Subsection:: *)
(* ActivateRE *)

PackageExport["ActivateRE"]
ActivateRE::usage = "ActivateRE[regex$] removes Inactive from the heads REUnion, REConcat, and REStar in regex$.";
ActivateRE[r_] := Activate[r, REUnion | REConcat | REStar];

(* ::Subsection:: *)
(* InactivateRE *)

PackageExport["InactivateRE"]
InactivateRE::usage = "\
InactivateRE[regex$] wraps the heads REUnion, REConcat, and REStar in regex$ with Inactive.
* The result is an inert regular expression that does not simplify automatically, but can still be used for most operations.";
SetAttributes[InactivateRE, HoldFirst];
InactivateRE[r_] := Inactivate[r, REUnion | REConcat | REStar];

(* ::Subsection:: *)
(* MapRE *)

PackageExport["MapRE"]
MapRE::usage = "\
MapRE[f$, regex$] returns regex$ with f$ applied to each literal.
* Literals are visited in the same order they appear in regex$.
* Literals are unwrapped from RELiteral before transformation: RELiteral[lit$, $$] -> f$[lit$].
MapRE[f$] returns an operator form of MapRE that can be applied to regular expressions.";
MapRE[f_, r_?REQ] :=
  Switch[#,
    IgnoringInactive @ reHeadP,
    Map[#0, #],

    RELiteral[__],
    f @ First @ #,

    _,
    f @ #
  ] & @ r;
MapRE[f_][r_?REQ] := MapRE[f, r];

(* ::Subsection:: *)
(* ScanRE *)

PackageExport["ScanRE"]
ScanRE::usage = "\
ScanRE[f$, regex$] applies f$ to each literal in regex$, returning Null.
* Literals are visited in the same order they appear in regex$.
* For wrapped literals RELiteral[lit$, $$], f$ is applied to lit$ itself.";
ScanRE[f_, r_?REQ] :=
  Switch[#,
    IgnoringInactive @ reHeadP,
    Scan[#0, #],

    RELiteral[__],
    f @ First @ #;,

    _,
    f @ #;
  ] & @ r;

(* ::Subsection:: *)
(* ExpandRE *)

PackageExport["ExpandRE"]
ExpandRE::usage = "\
ExpandRE[regex$] expands the given regular expression by distributing REConcat over REUnion.";
ExpandRE[r_?CompoundREQ] :=
  FixedPoint[Distribute[#, REUnion, REConcat] &, r];
ExpandRE[r_] := r;

(* ::Subsection:: *)
(* ExpandAllRE *)
PackageExport["ExpandAllRE"]
ExpandAllRE::usage = "\
ExpandAllRE[regex$] expands the given regular expression by distributing REConcat over REUnion at all levels.";
ExpandAllRE[r_?CompoundREQ] :=
  ReplaceRepeated[r,
    HoldPattern[c : Longest @ REConcat[___, _REUnion , ___]] :>
      Distribute[
        c, REUnion, REConcat
      ]
  ];
ExpandAllRE[r_] := r;

(* ::Subsection:: *)
(* FactorRE *)

PackageExport["FactorRE"]
FactorRE::usage = "\
FactorRE[regex$] attempts to factor the given regular expression by pulling REConcat from REUnion.
FactorRE is the inverse of ExpandRE, with some qualifications
* Regular expressions do not factor uniquely. Thus, FactorRE gives only one of potentially many factorizations.

Options:
Method -> 'Greedy' | 'Careful'
The method to use for factoring.
* 'Greedy': Return the first factorization found.
* 'Careful': Consider all possible factorizations, and select one which favors a shorter final expression.
   - This option is likely to be significantly more resource intensive.";

Options[FactorRE] =
  {
    Method -> "Greedy"
  };
OptionChecks[FactorRE] =
  {
    Method -> "Greedy" | "Careful"
  };
FactorRE[r_?CompoundREQ, CheckedOptions @ FactorRE] :=
  Switch[OptionValue @ Method,
    "Greedy",
    FixedPoint[Replace @ factorRERule, r],

    "Careful",
    Catch[
      NestWhile[
        assemble @ First[
          MaximalBy[
            ReplaceList[#,
              HoldPattern @ REUnion[
                composites : Repeated[
                  REConcat[
                    prefix_.,
                    _.,
                    suffix_.
                  ] /; prefix =!= Epsilon || suffix =!= Epsilon,
                  {2, Infinity}
                ],
                Shortest @ rest___
              ] :> {{composites}, {prefix, suffix}, {rest}}
            ],
            {
              Length @ #[[1]],
              Max[
                WithOptions[RELength, Method -> "Alphabetic"] /@
                  #[[2]]
              ],
              -Total[
                RELength /@ #[[3]]
              ]
            } &
          ],
          Throw[#]
        ] &,
        r,
        True &
      ]
    ]
  ] ;
FactorRE[r_, CheckedOptions @ FactorRE] := r;

factorRERule =
  HoldPattern @ REUnion[
    composites : Repeated[
      Longest @ REConcat[
        Longest @ prefix_.,
        Shortest @ _.,
        Longest @ suffix_.
      ] /;
        prefix =!= Epsilon || suffix =!= Epsilon,
      {2, Infinity}
    ],
    Shortest @ rest___
  ] :>
    REUnion[
      REConcat[
        prefix,
        Replace[
          REUnion @ composites,
          REConcat[prefix, x_., suffix] :> x,
          {1}
        ],
        suffix
      ],
      rest
    ];

assemble[{{composites__}, {prefix_, suffix_}, {rest___}}] :=
  REUnion[
    REConcat[
      FactorRE @ prefix,
      Replace[
        REUnion @ composites,
        REConcat[prefix, x_., suffix] :> x,
        {1}
      ],
      FactorRE @ suffix
    ],
    rest
  ];


(* ::Subsection:: *)
(* SimplifyRE *)
PackageExport["SimplifyRE"]
SimplifyRE::usage = "SimplifyRE[regex$] attempts to simplify regex$ using simple pattern matching.";
SimplifyRE[r_] := ReplaceRepeated[r, reSimplificationRules];

(* ::Subsection:: *)
(* FullSimplifyRE *)
PackageExport["FullSimplifyRE"]
FullSimplifyRE::usage = "\
FullSimplifyRE[regex$] attempts to simplify regex$ using pattern matching, along with more complex \
factorization and equivalence checking techniques.
FullSimplifyRE accepts many of the same options as FullSimplify.

Options:
ComplexityFunction -> 'PostfixLength' | 'InfixLength' | 'AlphabeticLength' | 'ExtendedAlphabeticLength' | _
Function used to determine expression complexity. This can be a function, or one of the following special values:
* 'PostfixLength': RELength with Method -> 'Postfix'
* 'InfixLength': RELength with Method -> 'Infix'
* 'AlphabeticLength': RELength with Method -> 'Alphabetic'
* 'ExtendedAlphabeticLength': RELength with Method -> 'ExtendedAlphabetic'";
Options[FullSimplifyRE] =
  {
    ComplexityFunction -> RELength,
    ExcludedForms -> { Except @ reHeadP }
  };
FullSimplifyRE[
  r_?CompoundREQ,
  opts : CheckedOptions[{FullSimplifyRE, FullSimplify}, FullSimplifyRE]
] :=
  FullSimplify[r,
    TransformationFunctions ->
      UnlessAutomatic[
        OptionValue @ TransformationFunctions,
        {simpRE, advSimpRE, FactorRE}
      ],
    ComplexityFunction ->
      UnlessMatch[OptionValue @ ComplexityFunction,
        Automatic,
        RELength,

        "PostfixLength"
          | "InfixLength"
          | "AlphabeticLength"
          | "ExtendedAlphabeticLength",
        WithOptions[
          RELength,
          Method ->
            StringReplace[
              OptionValue @ ComplexityFunction,
              "Length" -> ""
            ]
        ]
      ],
    ExcludedForms -> OptionValue @ ExcludedForms,
    Sequence @@ FilterRules[
      {opts},
      TimeConstraint | Assumptions
    ]
  ];
FullSimplifyRE[r_, rest___] := r;

(* ::Section:: *)
(* Package Scope *)

(* ::Subsection:: *)
(* pSet *)

PackageScope["pSet"]
pSet::usage = "pSet[regex] returns the set of prefix characters of strings recognized by regex.";
pSet[] = {};
pSet[EmptyLanguage | Epsilon] = {};
pSet[HoldIgnoringInactive @ REUnion[x__]] :=
  Union @@ pSet /@ {x};
pSet[HoldIgnoringInactive @ REStar[x_]] :=
  pSet @ x;
pSet[
  HoldIgnoringInactive @ REConcat[
    Longest[x___?matchesEmptyQ],
    Longest[y : RepeatedNull[_, 1]],
    ___
  ]
] :=
  Union[
    Catenate[
      pSet /@ {x}
    ],
    pSet @ y
  ];
pSet[x_] := {x};

(* ::Subsection:: *)
(* dSet *)

PackageScope["dSet"]
dSet::usage = "dSet[regex] returns the set of suffix characters of strings recognized by regex.";
dSet[] = {};
dSet[EmptyLanguage | Epsilon] = {};
dSet[HoldIgnoringInactive @ REUnion[x__]] :=
  Union @@ dSet /@ {x};
dSet[HoldIgnoringInactive @ REStar[x_]] :=
  dSet @ x;
dSet[
  HoldIgnoringInactive @ REConcat[
    Shortest[___],
    RepeatedNull[x_, 1],
    Longest[y___?matchesEmptyQ]
  ]
] :=
  Union[
    Catenate[
      dSet /@ {y}
    ],
    dSet @ x
  ];
dSet[x_] := {x};

(* ::Subsection:: *)
(* fSet *)

PackageScope["fSet"]
fSet::usage = "fSet[regex] returns the set of factors of length 2 in regex.";
fSet[HoldIgnoringInactive @ REUnion[x__]] :=
  Union @@ fSet /@ {x};
fSet[HoldIgnoringInactive @ REStar[x_]] :=
  Union[
    fSet @ x,
    Tuples[{dSet @ x, pSet @ x}]
  ];
fSet[HoldIgnoringInactive @ REConcat[x_, y_]] :=
  Union[
    fSet @ x,
    fSet @ y,
    Tuples[{dSet @ x, pSet @ y}]
  ];
fSet[_] = {};

(* ::Subsection:: *)
(* linearizeRE *)

PackageScope["linearizeRE"]
linearizeRE::usage = "\
linearizeRE[regex] linearizes regex by indexing each literal.
linearizeRE[regex, i] linearizes regex with indices starting at i.";
linearizeRE[r_, starti_Integer : 1] :=
  Module[{i = starti},
    Switch[#,
      IgnoringInactive @ reHeadP,
      Map[#0, #],

      EmptyLanguage | Epsilon,
      #,

      _,
      RELiteral[#, i++]
    ] & @ r
  ];

(* ::Section:: *)
(* Private *)

matchesEmptyQ[Epsilon | IgnoringInactive @ _REStar] = True;
matchesEmptyQ[HoldIgnoringInactive @ REUnion[x__]] :=
  AnyTrue[{x}, matchesEmptyQ];
matchesEmptyQ[HoldIgnoringInactive @ REConcat[x__]] :=
  AllTrue[{x}, matchesEmptyQ];
matchesEmptyQ[_] = False;

simpRE[x_] := Replace[x, reSimplificationRules];
advSimpRE[x_] := Replace[x, reAdvancedSimplificationRules];
(*faktorRE[x_] := FixedPoint[Replace @ factorRERule, x];*)

reSimplificationRules =
  Dispatch @ {
    HoldPattern[REConcat[c : REStar[REConcat[(x_)..] | x_], x_]] :>
      REConcat[x, c], (*  x*x -> xx* and [xx...]*x -> x[ xx...]*  (standard form)  *)
    HoldPattern[REConcat[REStar @ x_, x_, REStar @ x_]] :>
      REConcat[x, REStar @ x], (*  x*xx* -> xx*  *)
    HoldPattern[REConcat[Repeated[s_REStar, {2, Infinity}]]] :>
      s, (*  x*x*... -> x*  *)
    HoldPattern[REStar[REUnion[Epsilon, x_]]] :>
      REStar @ x, (* (ɛ|x)* -> x* *)
    HoldPattern[REUnion[x_ | REStar[x_], c : REStar[REUnion[x_ | REStar[x_], _.]] , a_.]] :>
      REUnion[c, a], (*  x|(x|y)* -> (x|y)*  *)
    HoldPattern[REUnion[Epsilon, REConcat[x_, REStar @ x_]]] :>
      REStar @ x, (*  ɛ|xx* -> x* (this one seems to happen a lot converting FAs to regex) *)
    HoldPattern[REStar @ REUnion[x_, REConcat[x_ ..], a_.]] :>
      REStar @ REUnion[x, a], (*  (x|xx...)* -> x* *)
    HoldPattern[REStar @ REUnion[c : REConcat[Longest @ x__], REConcat[(x__)..], a_.]] :>
      REStar @ REUnion[c, a], (*  (xy|(xyxy...))* -> (xy)*  *)
    HoldPattern[REStar[u : REUnion[_REStar, _]]] :>
      REStar @ Replace[u, REStar[a_] :> a, {1}], (*  (x*|y)* -> (x|y)*  *)
    HoldPattern[REStar[c : REConcat[__REStar]]] :>
      REStar[REUnion @@ Sequence @@@ c] (*  (x*y*z*...)* -> (x|y|z|...)*  *)
  };

reAdvancedSimplificationRules =
  Dispatch @ {
    HoldPattern[REUnion[x_, y_] /; SubsetLanguageQ[x, y]] :> y ,
    HoldPattern[REConcat[x_REStar, y_REStar] /; SubsetLanguageQ[x, y]] :> y,
    HoldPattern[(REConcat[x_, y_REStar] | REConcat[y_REStar, x_]) /; SubsetLanguageQ[x, y]] :> y
  };