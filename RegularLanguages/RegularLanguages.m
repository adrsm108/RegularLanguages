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

(* ::Section:: *)
(* Symbols *)

EpsilonProbability::usage = "EpsilonProbability is an option for RandomNFA and RandomRE that specifies the probability a given symbol will be Epsilon.";
ClosureProbability::usage = "ClosureProbability is an option for RandomRE that specifies the probability a given subexpression will be wrapped with REClosure.";
UnionProbability::usage = "UnionProbability is an option for RandomRE that specifies the relative frequency of REUnion vs REConcat in the final expression.";
TerminalStates::usage = "TerminalStates is an option for RandomNFA and RandomDFA that specifies the number of terminal (accepting) states in the result.";
InitialStates::usage = "InitialStates is an option for RandomNFA that specifies the number of initial states in the result.";
AllStatesReachable::usage = "AllStatesReachable is an option for RandomDFA and RandomNFA that specifies whether to ensure all states in the result are reachable.";
StatesFunction::usage = "StatesFunction is an option for RandomDFA and RandomNFA that specifies the function to use for generating state ids.";
AlphabetFunction::usage = "AlphabetFunction is an option for RandomDFA, RandomNFA, and RandomRE that specifies the function to use for generating the alphabet of the output.";
SimplificationFunction::usage = "SimplificationFunction is an option for ToRE that specifies the function that should be used to simplify intermediate results.";

PackageExport["Epsilon"]
Epsilon::usage = "Epsilon is a symbol representing the string of length 0.";
ToString[Epsilon] ^= "";
Epsilon /: MakeBoxes[Epsilon, TraditionalForm] :=
  With[{boxes = MakeBoxes["\[CurlyEpsilon]", TraditionalForm]},
    InterpretationBox[boxes, Epsilon]
  ];

PackageExport["EmptyLanguage"]
EmptyLanguage::usage = "EmptyLanguage is a symbol representing the language with no elements. In various contexts, it can be viewed as the empty set, an automaton with no reachable accepting states, the regular expression matching nothing, etc.";
ToString[EmptyLanguage] ^= "\[EmptySet]";
EmptyLanguage /: MakeBoxes[EmptyLanguage, TraditionalForm] :=
  With[{boxes = MakeBoxes["\[EmptySet]", TraditionalForm]},
    InterpretationBox[boxes, EmptyLanguage]
  ];


(*
Protect[
  EpsilonProbability,
  ClosureProbability,
  UnionProbability,
  TerminalStates,
  InitialStates,
  AllStatesReachable,
  StatesFunction,
  AlphabetFunction,
  SimplificationFunction,
  Epsilon,
  EmptyLanguage
];
*)


PackageExport["LanguageAlphabet"]
LanguageAlphabet::usage = "
LanguageAlphabet[L] returns the alphabet of the language represented by L, where L can be any automaton or regex.
  - For an automaton A, this is the union of the set of transition characters (which may include the empty string) over all states in A.
  - For a regular expression r, this is the set of all characters in r, where a character is defined to be any subexpression expr of r such that
      1. neither expr nor Head[expr] is one of REUnion, REConcat, REClosure, Regex, or EmptyLanguage and
      2. expr is not descended from any expression satisfying the previous rule.

Options:
\"IncludeEpsilon\" -> True | False | Automatic
  - True: the returned list always includes Epsilon.
  - False: the returned list never includes Epsilon.
  - Automatic: the returned list only includes Epsilon when the language contains explicit Epsilon-productions.";
Options[LanguageAlphabet] = {"IncludeEpsilon" -> Automatic};
OptionChecks[LanguageAlphabet] = {"IncludeEpsilon" -> True | False | Automatic};
LanguageAlphabet[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], OptionsPattern[]?(validQ@LanguageAlphabet)] :=
  Switch[OptionValue["IncludeEpsilon"],
    True, Union[asc["alphabet"], {Epsilon}],
    False, DeleteCases[asc["alphabet"], Epsilon],
    Automatic, DeleteCases[asc["alphabet"], Epsilon] ] ;
LanguageAlphabet[g_Graph?FAGraphQ, rest___] := LanguageAlphabet[FAExpression[g], rest];
LanguageAlphabet[r_RegexQ, OptionsPattern[]?(validQ@LanguageAlphabet)] :=
  Switch[OptionValue["IncludeEpsilon"],
    True, Union[firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage] :> Sow[x, "char"]], {Epsilon}],
    False, Union@firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage, Epsilon] :> Sow[x, "char"]],
    Automatic, Union@firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage] :> Sow[x, "char"]]] ;

PackageExport["SameAlphabetQ"]
SameAlphabetQ::usage = "SameAlphabetQ[A1, A2, ...] returns true if LanguageAlphabet[A1], LanguageAlphabet[A2], ... are equivalent as sets.";
SameAlphabetQ[A_] := Quiet[Check[ListQ@LanguageAlphabet[A], False]];
SameAlphabetQ[A1_, Ai__] := Quiet[Check[AllTrue[LanguageAlphabet /@ {Ai}, ContainsExactly[LanguageAlphabet[A1]]], False]];

PackageExport["EquivalentLanguageQ"]
EquivalentLanguageQ::usage = "EquivalentLanguageQ[L1, L2, ...] returns True if all Li are automata or regular expressions that describe the same language.";
EquivalentLanguageQ[L_, Li___] := EquivalentFAQ @@ Replace[{L}, r_?REQ :> ToNFA[r], {1}];

PackageExport["SubsetLanguageQ"]
SubsetLanguageQ::usage = "SubsetLanguageQ[L1, L2] yields True if the language recognized by automaton or regular expression L1 is a subset of the language recognized by L2.
SubsetLanguageQ[L, L1, L2, ...] returns True if SubsetLanguageQ[L, Li] is True for all Li.
SubsetLanguageQ[L] represents an operator form of SubsetLanguageQ that can be applied to an expression. ";
SubsetLanguageQ[L_, Li__] := SubsetFAQ @@ Replace[{L, Li}, r_?CompoundREQ :> ToNFA[r], {1}];
SubsetLanguageQ[L_?CompoundREQ] := SubsetLanguageQ[ToNFA[L]];
SubsetLanguageQ[L_][Li__] := SubsetLanguageQ[L, Li];

PackageExport["UseNotation"]
UseNotation::usage = "UseNotation[use] can be evaluated to add or remove extra notational forms.
UseNotation[True] is evaluated automatically on package load, and makes the following changes:
REUnion[a, b,...] formats as a \[VerticalSeparator] b \[VerticalSeparator] ... (\\[VerticalSeparator], alias \[AliasIndicator]|\[AliasIndicator]). VerticalSeparator is redefined to alias REUnion.
REConcat[a, b,...] formats as a \[CenterDot] b \[CenterDot] ... (\\[CenterDot], alias \[AliasIndicator].\[AliasIndicator]). CenterDot is redefined to alias REConcat.
REClosure[a] formats as a* (SuperStar[a], shortcut Ctrl + ^, * ). SuperStar is redefined to alias REClosure.
Epsilon formats as \[CurlyEpsilon], (\\[CurlyEpsilon], alias \[AliasIndicator]ce\[AliasIndicator]) and \[CurlyEpsilon] will be set to Epsilon if it is not yet defined.
EmptyLanguage formats as \[EmptySet] (\\[EmptySet], alias \[AliasIndicator]es\[AliasIndicator]), and \[EmptySet] will be set to EmptyLanguage if it is not yet defined.
UseNotation[False] removes all extra definitions and formatting rules.";
UseNotation::clobber = "Symbol `1` will not be set to `2` because a previous definition `3` exists.";
UseNotation[use : True | False] := (
  If[use,
    (
      setNoClobber[Global`\[CurlyEpsilon], Epsilon];
      setNoClobber[Global`\[EmptySet], EmptyLanguage];
      setNoClobber[VerticalSeparator, REUnion];
      setNoClobber[CenterDot, REConcat];
      setNoClobber[SuperStar, REClosure];
      Unprotect[REUnion, REConcat, REClosure, EmptyLanguage, Epsilon];
      Epsilon /: MakeBoxes[Epsilon, form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes["\[CurlyEpsilon]", form]},
          InterpretationBox[boxes, Epsilon]];
      EmptyLanguage /: MakeBoxes[EmptyLanguage, form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes["\[EmptySet]", form]},
          InterpretationBox[boxes, EmptyLanguage]];
      REUnion /: MakeBoxes[e : REUnion[x___], form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes[VerticalSeparator[x], form]},
          InterpretationBox[boxes, e]];
      REConcat /: MakeBoxes[e : REConcat[x___], form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes[CenterDot[x], form]},
          InterpretationBox[boxes, e]];
      REClosure /: MakeBoxes[e : REClosure[x___], form : (TraditionalForm | StandardForm)] :=
        With[{boxes = MakeBoxes[SuperStar[x], form]},
          InterpretationBox[boxes, e]];
      Protect[REUnion, REConcat, REClosure, EmptyLanguage, Epsilon]; ),
    (Quiet[
      unprotectAndUnset[Global`\[CurlyEpsilon]];
      unprotectAndUnset[Global`\[EmptySet]];
      unprotectAndUnset[VerticalSeparator];
      unprotectAndUnset[CenterDot];
      unprotectAndUnset[SuperStar];
      Unprotect[REUnion, REConcat, REClosure, EmptyLanguage, Epsilon];
      Epsilon /: MakeBoxes[Epsilon, form : (TraditionalForm | StandardForm)] =. ;
      EmptyLanguage /: MakeBoxes[EmptyLanguage, form : (TraditionalForm | StandardForm)] =. ;
      REUnion /: MakeBoxes[e : REUnion[x___], form : (TraditionalForm | StandardForm)] =. ;
      REConcat /: MakeBoxes[e : REConcat[x___], form : (TraditionalForm | StandardForm)] =. ;
      REClosure /: MakeBoxes[e : REClosure[x___], form : (TraditionalForm | StandardForm)] =. ;
      Protect[REUnion, REConcat, REClosure, EmptyLanguage, Epsilon];
    ]; )
  ];
use
);

UseNotation[True];

SetAttributes[setNoClobber, HoldFirst];
setNoClobber[symb_, val_] :=
  If[ValueQ[symb] && symb =!= val,
    Message[UseNotation::clobber, HoldForm[symb], val, symb],
    Unprotect[symb];
    symb = val;
    Protect[symb];
  ];

SetAttributes[unprotectAndUnset, HoldFirst];
unprotectAndUnset[symb_] :=
  If[ MemberQ[Attributes@symb, Protected],
    Unprotect[symb];
    symb =.
  ];
