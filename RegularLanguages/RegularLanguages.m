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


PackageExport["EpsilonProbability"]
EpsilonProbability::usage = "EpsilonProbability is an option for RandomNFA and RandomRE that specifies the probability a given symbol will be Epsilon.";

PackageExport["ClosureProbability"]
ClosureProbability::usage = "ClosureProbability is an option for RandomRE that specifies the probability a given subexpression will be wrapped with REClosure.";

PackageExport["UnionProbability"]
UnionProbability::usage = "UnionProbability is an option for RandomRE that specifies the relative frequency of REUnion vs REConcat in the final expression.";

PackageExport["TerminalStates"]
TerminalStates::usage = "TerminalStates is an option for RandomNFA and RandomDFA that specifies the number of terminal (accepting) states in the result.";

PackageExport["InitialStates"]
InitialStates::usage = "InitialStates is an option for RandomNFA that specifies the number of initial states in the result.";

PackageExport["AllStatesReachable"]
AllStatesReachable::usage = "AllStatesReachable is an option for RandomDFA and RandomNFA that specifies whether to ensure all states in the result are reachable.";

PackageExport["StatesFunction"]
StatesFunction::usage = "StatesFunction is an option for RandomDFA and RandomNFA that specifies the function to use for generating state ids.";

PackageExport["AlphabetFunction"]
AlphabetFunction::usage = "AlphabetFunction is an option for RandomDFA, RandomNFA, and RandomRE that specifies the function to use for generating the alphabet of the output.";

PackageExport["SimplificationFunction"]
SimplificationFunction::usage = "SimplificationFunction is an option for ToRE that specifies the function that should be used to simplify intermediate results.";

PackageExport["Epsilon"]
Epsilon::usage = "Epsilon is a symbol representing the string of length 0.";
ToString[Epsilon] ^= "";

PackageExport["EmptyLanguage"]
EmptyLanguage::usage = "EmptyLanguage is a symbol representing the language with no elements. In various contexts, it can be viewed as the empty set, an automaton with no reachable accepting states, the regular expression matching nothing, etc.";
ToString[EmptyLanguage] ^= "\[EmptySet]";

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

PackageExport["LanguageAlphabet"]
LanguageAlphabet::usage = "LanguageAlphabet[L] returns the (extended) alphabet of the language represented by L, where L can be any automaton or regex.
For an automaton A, the extended alphabet is the union of the set of transition characters (which may include the empty string) over all states in A.
For a regular expression R, the alphabet is the set of all characters in R, where a character is defined to be any subexpression expr of R such that (a) neither expr nor Head[expr] is one of REUnion, REConcat, REClosure, or EmptyLanguage and (b) expr is not descended from any expression satisfying (a).
LanguageAlphabet[L, \"Standard\"] returns the standard alphabet of L. If L is a NFA with epsilon transitions or a regex with explicit Epsilon characters, LanguageAlphabet[L, \"Standard\"] = LanguageAlphabet[L] \[Backslash] {Epsilon}. Otherwise, LanguageAlphabet[L \"Standard\"] is equivalent to LanguageAlphabet[L].";
LanguageAlphabet[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ]] := asc["alphabet"];
LanguageAlphabet[DFA[asc_?dfaAscQ] | NFA[asc_?nfaAscQ], "Standard"] := DeleteCases[asc["alphabet"], Epsilon];
LanguageAlphabet[g_Graph?FAGraphQ] := LanguageAlphabet[FAExpression[g]];
LanguageAlphabet[g_Graph?FAGraphQ, "Standard"] := LanguageAlphabet[FAExpression[g], "Standard"];
LanguageAlphabet[r_RegexQ] := Union@firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage] :> Sow[x, "char"]];
LanguageAlphabet[r_RegexQ, "Standard"] := Union@firstReaped["char", r /. x : reExcludingPatt[EmptyLanguage, Epsilon] :> Sow[x, "char"]];

PackageExport["SameAlphabetQ"]
SameAlphabetQ::usage = "SameAlphabetQ[A1, A2, ...] returns true if LanguageAlphabet[A1], LanguageAlphabet[A2], ... are equivalent as sets.";
SameAlphabetQ[A_] := Quiet[Check[ListQ@LanguageAlphabet[A], False]];
SameAlphabetQ[A1_, Ai__] := Quiet[Check[AllTrue[LanguageAlphabet /@ {Ai}, ContainsExactly[LanguageAlphabet[A1]]], False]];

PackageExport["EquivalentLanguageQ"]
EquivalentLanguageQ::usage = "EquivalentLanguageQ[L_1, L_2, ...] returns True if all L_i are automata or regular expressions that describe the same language.";
EquivalentLanguageQ[L_, Li___] := EquivalentFAQ @@ Replace[{L}, r_?REQ :> ToNFA[r], {1}];

PackageExport["SubsetLanguageQ"]
SubsetLanguageQ::usage = "SubsetLanguageQ[L1, L2] yields True if the language recognized by automaton or regular expression L1 is a subset of the language recognized by L2.
SubsetLanguageQ[L, L_1, L_2, ...] returns True if SubsetLanguageQ[L, L_i] is True for all L_i.
SubsetLanguageQ[L] represents an operator form of SubsetLanguageQ that can be applied to an expression. ";
SubsetLanguageQ[L_, Li__] := SubsetFAQ @@ Replace[{L, Li}, r_?CompoundREQ :> ToNFA[r], {1}];
SubsetLanguageQ[L_?CompoundREQ] := SubsetLanguageQ[ToNFA[L]];
SubsetLanguageQ[L_][Li__] := SubsetLanguageQ[L, Li];

