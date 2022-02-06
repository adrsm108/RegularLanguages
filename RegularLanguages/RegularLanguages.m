(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: RegularLanguages *)
(* :Context: RegularLanguages` *)
(* :Author: Adam Smith *)
(* :Date: 2021-12-06 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2021 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

(* For new style packages see: https://mathematica.stackexchange.com/a/176489) *)
(* Declare package context *)
Package["RegularLanguages`"]
PackageImport["GeneralUtilities`"]

(* ::Section:: *)
(* Symbols *)

(* ::Subsection:: *)
(* UseNotation *)

PackageExport["UseNotation"]
UseNotation::usage = "\
UseNotation is a flag that controls whether LoadNotation[] will be called on package load.
Can be set before getting package (e.g. Block[{RegularLanguages`UseNotation = False}, << RegularLanguages`]) to disable extended notation.
Once package has been loaded, use UnloadNotation[] to clear notation-related definitions.";

(* ::Subsection:: *)
(* Epsilon *)

PackageExport["Epsilon"]
Epsilon::usage = "Epsilon is a symbol representing the string of length 0.";
Epsilon /: MakeBoxes[Epsilon, TraditionalForm] :=
  InterpretationBox["\[CurlyEpsilon]", Epsilon];

(* ::Subsection:: *)
(* EmptyLanguage *)

PackageExport["EmptyLanguage"]
EmptyLanguage::usage = "EmptyLanguage is a symbol representing the language with no elements. In various contexts, it can be viewed as the empty set, an automaton with no reachable accepting states, the regular expression matching nothing, etc.";
EmptyLanguage /: MakeBoxes[EmptyLanguage, TraditionalForm] :=
  InterpretationBox["\[EmptySet]", EmptyLanguage];

(* ::Subsection:: *)
(* LanguageAlphabet *)

PackageExport["LanguageAlphabet"]
LanguageAlphabet::usage = "\
LanguageAlphabet[L$] returns the alphabet of the language represented by L$, where L$ can be any automaton or regex.
* For an automaton A$, this is the union of the set of transition characters (which may include the empty string) over all states in A$.
* For a regular expression r$, this is the set of all characters in r$, where a character is defined to be any subexpression expr$ of r$ such that
      1. neither expr$ nor Head[expr$] is one of REUnion, REConcat, REStar, RELiteral, or EmptyLanguage and
      2. expr$ is not descended from any expression satisfying the previous rule.

Options:
'IncludeEpsilon' -> True | False | Automatic
* True: the returned list always includes Epsilon.
* False: the returned list never includes Epsilon.
* Automatic: the returned list only includes Epsilon when the language contains explicit Epsilon-productions.";
SetAttributes[LanguageAlphabet, Listable];
Options[LanguageAlphabet] =
  {
    "IncludeEpsilon" -> Automatic
  };
OptionChecks[LanguageAlphabet] =
  {
    "IncludeEpsilon" -> True | False | Automatic
  };
LanguageAlphabet[
  DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ],
  CheckedOptions @ LanguageAlphabet
] :=
  If[OptionValue @ "IncludeEpsilon",
    (* True *)
    Union[asc @ "Alphabet", {Epsilon}],
    (* False *)
    DeleteCases[asc @ "Alphabet", Epsilon],
    (* Automatic *)
    asc @ "Alphabet"
  ] ;
LanguageAlphabet[
  x : EmptyLanguage | Epsilon,
  CheckedOptions @ LanguageAlphabet
] :=
  If[OptionValue @ "IncludeEpsilon",
    (* True *)
    {Epsilon},
    (* False *)
    {},
    (* Automatic *)
    If[x === Epsilon,
      {Epsilon},
      {}
    ]
  ];
LanguageAlphabet[
  r_?REQ,
  CheckedOptions @ LanguageAlphabet
] :=
  With[
    {
      alph = firstReaped[
        "lit",
        ScanRE[
          Sow[#, "lit"] &,
          r
        ]
      ]
    },
    If[OptionValue @ "IncludeEpsilon",
      (* case True *)
      Union[
        alph, {Epsilon}
      ],
      (* case False *)
      DeleteCases[
        Union @ alph,
        Epsilon
      ],
      (* case Automatic *)
      Union @ alph
    ]
  ];
LanguageAlphabet[g_?FAGraphQ, rest___] :=
  LanguageAlphabet[FAExpression @ g, rest];

(* ::Subsection:: *)
(* SameAlphabetQ *)

PackageExport["SameAlphabetQ"]
SameAlphabetQ::usage = "\
SameAlphabetQ[A$1, A$2, $$] returns true if LanguageAlphabet[A$1], LanguageAlphabet[A$2], $$ are equivalent as sets." ;
SameAlphabetQ[A_] :=
  Quiet @ Check[
    ListQ @ LanguageAlphabet @ A,
    False
  ];
SameAlphabetQ[A1_, Ai__] :=
  Quiet @ Check[
    AllTrue[
      LanguageAlphabet @ {Ai},
      ContainsExactly[LanguageAlphabet @ A1]
    ],
    False
  ];

(* ::Subsection:: *)
(* SameLanguageQ *)

PackageExport["SameLanguageQ"]
SameLanguageQ::usage = "\
SameLanguageQ[L$1, L$2, $$] returns True if all L$i are automata or regular expressions that describe the same language.";
SameLanguageQ[Li___] := EquivalentFAQ @@ Replace[
  {Li},
  r_ /; !FAQ[r] :> ToNFA @ ToRE @ r,
  {1}
];

PackageExport["SubsetLanguageQ"]
SubsetLanguageQ::usage = "\
SubsetLanguageQ[L$1, L$2] yields True if the language recognized by automaton or regular expression L$1 is a subset of the language recognized by L$2.
SubsetLanguageQ[L$, L$1, L$2, $$] returns True if SubsetLanguageQ[L$, L$i] is True for all L$i.
SubsetLanguageQ[L$] represents an operator form of SubsetLanguageQ that can be applied to an expression.";
SubsetLanguageQ[L_, Li__] :=
  SubsetFAQ @@ Replace[
    {L, Li},
    r_ /; !FAQ[r] :> ToNFA @ ToRE @ r,
    {1}
  ];
SubsetLanguageQ[L_][Li__] :=
  SubsetLanguageQ[L, Li];

(* ::Subsection:: *)
(* EmptyFAQ *)

PackageExport["EmptyLanguageQ"]
EmptyLanguageQ::usage = "\
EmptyLanguageQ[L$] returns True if L$ is an automaton or regular expression whose language is the empty set.";
EmptyLanguageQ[EmptyLanguage] = True;
EmptyLanguageQ[A_] := emptyFAQ[A];

(* ::Subsection:: *)
(* LoadNotation *)

PackageExport["LoadNotation"]
LoadNotation::usage = "\
LoadNotation[] can be evaluated to add the following extra notational forms for the RegularLanguages package:
* REUnion[a$, b$, $$] formats as a$ \[VerticalSeparator] b$ \[VerticalSeparator] $$ (\\[VerticalSeparator], alias \[AliasIndicator]|\[AliasIndicator]). VerticalSeparator is redefined to reference REUnion.
* REConcat[a$, b$, $$] formats as a \[CenterDot] b \[CenterDot] $$ (\\[CenterDot], alias \[AliasIndicator].\[AliasIndicator]). CenterDot is redefined to reference REConcat.
* REStar[a$] formats as a$* (SuperStar[a$], shortcut Ctrl + ^, * ). SuperStar is redefined to reference REStar.
* Epsilon formats as \[CurlyEpsilon], (\\[CurlyEpsilon], alias \[AliasIndicator]ce\[AliasIndicator]) and \[CurlyEpsilon] will be set to Epsilon if it is not yet defined.
* EmptyLanguage formats as \[EmptySet] (\\[EmptySet], alias \[AliasIndicator]es\[AliasIndicator]), and \[EmptySet] will be set to EmptyLanguage if it is not yet defined.
LoadNotation[] is evaluated by default upon package load
* To disable this, set RegularLanguages`UseNotation = False before loading package.
UnloadNotation[] removes all extra definitions and formatting rules.";
LoadNotation::clobber = "`1` will not be used for notation because a previous definition exists.";
LoadNotation[] :=
  (
    UnloadNotation[];

    Unprotect[Epsilon, EmptyLanguage, REUnion, REConcat, REStar, Inactive];
    withNotation[Global`\[CurlyEpsilon] = Epsilon,
      Epsilon /: MakeBoxes[Epsilon, TraditionalForm | StandardForm] :=
        InterpretationBox["\[CurlyEpsilon]", Epsilon]
    ];
    withNotation[Global`\[EmptySet] = EmptyLanguage,
      EmptyLanguage /: MakeBoxes[EmptyLanguage, TraditionalForm | StandardForm] :=
        InterpretationBox["\[EmptySet]", EmptyLanguage]
    ];
    withNotation[VerticalSeparator[x___] := REUnion[x],
      REUnion /: MakeBoxes[e : REUnion[x___], form : (TraditionalForm | StandardForm)] :=
        MakeBoxes[VerticalSeparator[x], form],

      Inactive /: MakeBoxes[e : Inactive[REUnion][x___], form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes[Inactive[VerticalSeparator][x], form]},
          InterpretationBox[boxes, e]
        ]
    ];
    withNotation[CenterDot[x___] := REConcat[x],
      REConcat /: MakeBoxes[e : REConcat[x___], form : (TraditionalForm | StandardForm)] :=
        MakeBoxes[CenterDot[x], form],

      Inactive /: MakeBoxes[e : Inactive[REConcat][x___], form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes[Inactive[CenterDot][x], form]},
          InterpretationBox[boxes, e]
        ]
    ];
    withNotation[SuperStar[x_] := REStar[x],
      REStar /: MakeBoxes[e : REStar[x_], form : (TraditionalForm | StandardForm)] :=
        MakeBoxes[SuperStar[x], form],

      Inactive /: MakeBoxes[e : Inactive[REStar][x_], form : (TraditionalForm | StandardForm)] :=
        Replace[
          MakeBoxes[SuperStar[x], StandardForm],
          SuperscriptBox[stuff_, "*", opts___] :>
            InterpretationBox[
              SuperscriptBox[
                stuff,
                TagBox["*", "InactiveToken", BaseStyle -> "Inactive"],
                opts
              ],
              e,
              DeletionWarning -> True
            ]
        ]
    ];
    Protect[Epsilon, EmptyLanguage, REUnion, REConcat, REStar, Inactive];
  );

(* ::Subsection:: *)
(* UnloadNotation *)

PackageExport["UnloadNotation"]
UnloadNotation::usage = "\
UnloadNotation[] removes the extra notational definitions from the RegularLanguages package introduced by LoadNotation.";
UnloadNotation[] :=
  (
    Unprotect[Epsilon, EmptyLanguage, REUnion, REConcat, REStar, Inactive];
    ReleaseHold[$notationUnsets];
    $notationUnsets = {};
    Protect[Epsilon, EmptyLanguage, REUnion, REConcat, REStar, Inactive];
  );

(* ::Section:: *)
(* Private *)

SetAttributes[withNotation, HoldAll];
withNotation[def_, subsequentDefs___] :=
  With[
    {
      heldLhsHead = Extract[Unevaluated @ def, 1, GeneralUtilities`PatternHead]
    },
    If[GeneralUtilities`HasDefinitionsQ @@ heldLhsHead,
      Message[LoadNotation::clobber, HoldForm @@ heldLhsHead],
      AppendTo[$notationUnsets, MapAt[Unprotect, heldLhsHead, 1]];
      setNotation[def];
      Protect @@ heldLhsHead;
      Scan[setNotation, Hold[subsequentDefs]]
    ]
  ];

SetAttributes[setNotation, HoldAll];
setNotation[assignment : (TagSet | TagSetDelayed)[Inactive, patt_MakeBoxes, val_]] :=
  (
    AppendTo[$notationUnsets,
      Hold @ TagUnset[Inactive, HoldPattern @ patt]
    ];
    insertInactiveFormatRule[HoldPattern @ patt :> val]
  );
setNotation[assignment : (TagSet | TagSetDelayed)[h_, patt_, _]] :=
  (
    AppendTo[$notationUnsets,
      Hold @ TagUnset[h, patt]
    ];
    assignment
  );
setNotation[
  assignment : (Set | SetDelayed | UpSet | UpSetDelayed)[patt_, _]
] :=
  (
    AppendTo[$notationUnsets, Hold @ Unset @ patt];
    assignment
  );

insertInactiveFormatRule[rule_] :=
  (
    (* Ensure FormatValues have been loaded *)
    Once @ MakeBoxes @ Inactive[Inactive][];

    PrependTo[
      FormatValues @ Inactive,
      rule
    ];
  )

(*
arbitraryInactiveHeadFormatRuleP =
  RuleDelayed[
    _?(
      Not @* FreeQ[
        HoldPattern @ MakeBoxes[
          Inactive[
            Verbatim[Pattern][_, Verbatim[_]]
          ][_],
          _
        ]
      ]
    ),
    _
  ];
*)
