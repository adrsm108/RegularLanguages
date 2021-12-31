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

(*
EpsilonProbability::usage = "EpsilonProbability is an option for RandomNFA and RandomRE that specifies the probability a given symbol will be Epsilon.";
ClosureProbability::usage = "ClosureProbability is an option for RandomRE that specifies the probability a given subexpression will be wrapped with REClosure.";
UnionProbability::usage = "UnionProbability is an option for RandomRE that specifies the relative frequency of REUnion vs REConcat in the final expression.";
TerminalStates::usage = "TerminalStates is an option for RandomNFA and RandomDFA that specifies the number of terminal (accepting) states in the result.";
InitialStates::usage = "InitialStates is an option for RandomNFA that specifies the number of initial states in the result.";
AllStatesReachable::usage = "AllStatesReachable is an option for RandomDFA and RandomNFA that specifies whether to ensure all states in the result are reachable.";
StatesFunction::usage = "StatesFunction is an option for RandomDFA and RandomNFA that specifies the function to use for generating state ids.";
AlphabetFunction::usage = "AlphabetFunction is an option for RandomDFA, RandomNFA, and RandomRE that specifies the function to use for generating the alphabet of the output.";
SimplificationFunction::usage = "SimplificationFunction is an option for ToRE that specifies the function that should be used to simplify intermediate results.";
*)

PackageExport["UseNotation"]
UseNotation::usage = "UseNotation is a flag that controls whether LoadNotation[] will be called on package load.";

PackageExport["Epsilon"]
Epsilon::usage = "Epsilon is a symbol representing the string of length 0.";
ToString[Epsilon] ^= "";
Epsilon /: MakeBoxes[Epsilon, TraditionalForm] := InterpretationBox["\[CurlyEpsilon]", Epsilon];

PackageExport["EmptyLanguage"]
EmptyLanguage::usage = "EmptyLanguage is a symbol representing the language with no elements. In various contexts, it can be viewed as the empty set, an automaton with no reachable accepting states, the regular expression matching nothing, etc.";
ToString[EmptyLanguage] ^= "\[EmptySet]";
EmptyLanguage /: MakeBoxes[EmptyLanguage, TraditionalForm] := InterpretationBox["\[EmptySet]", EmptyLanguage];

PackageExport["LanguageAlphabet"]
LanguageAlphabet::usage = "\
LanguageAlphabet[L$] returns the alphabet of the language represented by L$, where L$ can be any automaton or regex.
* For an automaton A$, this is the union of the set of transition characters (which may include the empty string) over all states in A$.
* For a regular expression r$, this is the set of all characters in r$, where a character is defined to be any subexpression expr$ of r$ such that
      1. neither expr$ nor Head[expr$] is one of REUnion, REConcat, REClosure, RE, or EmptyLanguage and
      2. expr$ is not descended from any expression satisfying the previous rule.

Options:
'IncludeEpsilon' -> True | False | Automatic
* True: the returned list always includes Epsilon.
* False: the returned list never includes Epsilon.
* Automatic: the returned list only includes Epsilon when the language contains explicit Epsilon-productions.";
Options[LanguageAlphabet] = {"IncludeEpsilon" -> Automatic};
OptionChecks[LanguageAlphabet] = {"IncludeEpsilon" -> True | False | Automatic};
LanguageAlphabet[
  DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ],
  OptionsPattern[LanguageAlphabet]?(validQ @ LanguageAlphabet)
] :=
  Switch[OptionValue["IncludeEpsilon"],
    True, Union[asc["alphabet"], {Epsilon}],
    False, DeleteCases[asc["alphabet"], Epsilon],
    Automatic, asc["alphabet"]] ;
LanguageAlphabet[g_Graph?FAGraphQ, rest___] := LanguageAlphabet[FAExpression @ g, rest];
LanguageAlphabet[r_REQ, OptionsPattern[]?(validQ @ LanguageAlphabet)] :=
  Switch[OptionValue["IncludeEpsilon"],
    True, Union[firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage] :> Sow[x, "char"]], {Epsilon}],
    False, Union @ firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage, Epsilon] :> Sow[x, "char"]],
    Automatic, Union @ firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage] :> Sow[x, "char"]]] ;

PackageExport["SameAlphabetQ"]
SameAlphabetQ::usage = "\
SameAlphabetQ[A$1, A$2, $$] returns true if LanguageAlphabet[A$1], LanguageAlphabet[A$2], $$ are equivalent as sets." ;
SameAlphabetQ[A_] := Quiet[Check[ListQ @ LanguageAlphabet[A], False]];
SameAlphabetQ[A1_, Ai__] := Quiet[Check[AllTrue[LanguageAlphabet /@ {Ai}, ContainsExactly[LanguageAlphabet[A1]]], False]];

PackageExport["SameLanguageQ"]
SameLanguageQ::usage = "\
SameLanguageQ[L$1, L$2, $$] returns True if all L$i are automata or regular expressions that describe the same language.";
SameLanguageQ[Li___] := EquivalentFAQ @@ Replace[{Li}, r_?REQ :> ToNFA[r], {1}];

PackageExport["SubsetLanguageQ"]
SubsetLanguageQ::usage = "\
SubsetLanguageQ[L$1, L$2] yields True if the language recognized by automaton or regular expression L$1 is a subset of the language recognized by L$2.
SubsetLanguageQ[L$, L$1, L$2, $$] returns True if SubsetLanguageQ[L$, L$i] is True for all L$i.
SubsetLanguageQ[L$] represents an operator form of SubsetLanguageQ that can be applied to an expression.";
SubsetLanguageQ[L_, Li__] := SubsetFAQ @@ Replace[{L, Li}, r_?CompoundREQ :> ToNFA[r], {1}];
SubsetLanguageQ[L_?CompoundREQ] := SubsetLanguageQ[ToNFA[L]];
SubsetLanguageQ[L_][Li__] := SubsetLanguageQ[L, Li];

PackageExport["LoadNotation"]
LoadNotation::usage = "\
LoadNotation[] can be evaluated to add the following extra notational forms for the RegularLanguages package:
* REUnion[a$, b$, $$] formats as a$ \[VerticalSeparator] b$ \[VerticalSeparator] $$ (\\[VerticalSeparator], alias \[AliasIndicator]|\[AliasIndicator]). VerticalSeparator is redefined to reference REUnion.
* REConcat[a$, b$, $$] formats as a \[CenterDot] b \[CenterDot] $$ (\\[CenterDot], alias \[AliasIndicator].\[AliasIndicator]). CenterDot is redefined to reference REConcat.
* REClosure[a$] formats as a$* (SuperStar[a$], shortcut Ctrl + ^, * ). SuperStar is redefined to reference REClosure.
* Epsilon formats as \[CurlyEpsilon], (\\[CurlyEpsilon], alias \[AliasIndicator]ce\[AliasIndicator]) and \[CurlyEpsilon] will be set to Epsilon if it is not yet defined.
* EmptyLanguage formats as \[EmptySet] (\\[EmptySet], alias \[AliasIndicator]es\[AliasIndicator]), and \[EmptySet] will be set to EmptyLanguage if it is not yet defined.
LoadNotation[] is evaluated by default upon package load
* To disable this, set RegularLanguages`UseNotation = False before loading package.
UnloadNotation[] removes all extra definitions and formatting rules.";
LoadNotation::clobber = "`1` will not be used for notation because a previous definition exists.";
LoadNotation[] := (
  UnloadNotation[];
  Unprotect[Epsilon, EmptyLanguage, REUnion, REConcat, REClosure];
  withNotation[Global`\[CurlyEpsilon] = Epsilon,
    Epsilon /: MakeBoxes[Epsilon, form : (TraditionalForm | StandardForm)] :=
      InterpretationBox["\[CurlyEpsilon]", Epsilon]
  ];
  withNotation[Global`\[EmptySet] = EmptyLanguage,
    EmptyLanguage /: MakeBoxes[EmptyLanguage, form : (TraditionalForm | StandardForm)] :=
      InterpretationBox["\[EmptySet]", EmptyLanguage]
  ];
  withNotation[VerticalSeparator[x___] := REUnion[x],
    REUnion /: MakeBoxes[e : REUnion[x___], form : (TraditionalForm | StandardForm)] :=
      MakeBoxes[VerticalSeparator[x]]
    (*
    With[{boxes = MakeBoxes[VerticalSeparator[x], form]},
      InterpretationBox[boxes, e]]*)
  ];
  withNotation[CenterDot[x___] := REConcat[x],
    REConcat /: MakeBoxes[e : REConcat[x___], form : (TraditionalForm | StandardForm)] :=
      MakeBoxes[CenterDot[x]]
    (*With[{boxes = MakeBoxes[CenterDot[x], form]},
      InterpretationBox[boxes, e]]*)
  ];
  withNotation[SuperStar[x_] := REClosure[x],
    REClosure /: MakeBoxes[e : REClosure[x___], form : (TraditionalForm | StandardForm)] :=
      MakeBoxes[SuperStar[x]]
    (*With[{boxes = MakeBoxes[SuperStar[x], form]},
      InterpretationBox[boxes, e]]*)
  ];
  Protect[Epsilon, EmptyLanguage, REUnion, REConcat, REClosure];
);

PackageExport["UnloadNotation"]
UnloadNotation::usage = "\
UnloadNotation[] removes the extra notational forms for the RegularLanguages package introduced by LoadNotation.";
UnloadNotation[] := (
  Unprotect[Epsilon, EmptyLanguage, REUnion, REConcat, REClosure];
  ReleaseHold[$notationUnsets];
  $notationUnsets = {};
  Protect[Epsilon, EmptyLanguage, REUnion, REConcat, REClosure];
);

SetAttributes[withNotation, HoldAll];
withNotation[def_, subsequentDefs___] :=
  With[{heldLhsHead =
    Extract[Unevaluated @ def, 1, GeneralUtilities`PatternHead]},
    If[GeneralUtilities`HasDefinitionsQ @@ heldLhsHead,
      Message[LoadNotation::clobber, HoldForm @@ heldLhsHead],
      AppendTo[$notationUnsets, MapAt[Unprotect, heldLhsHead, 1]];
      setNotation[def];
      Protect @@ heldLhsHead;
      Scan[setNotation, Hold[subsequentDefs]]
    ]
  ];


SetAttributes[setNotation, HoldAll];
setNotation[assignment : (TagSet | TagSetDelayed)[h_, patt_, _]] := (
  AppendTo[$notationUnsets, Hold[TagUnset[h, patt]]];
  assignment
);
setNotation[
  assignment : (Set | SetDelayed | UpSet | UpSetDelayed)[
    patt_, _]] := (
  AppendTo[$notationUnsets, Hold[Unset[patt]]];
  assignment
);