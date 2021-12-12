(* Mathematica Test File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

BeginTestSection["DFAs.mt"];

$testDFA =
  DFA[<|"states" -> <|
    1 -> DFAState[1, <|a -> 1, b -> 2, c -> 3|>, {True, False}],
    2 -> DFAState[2, <|a -> 3, b -> 2, c -> 1|>, {False, True}],
    3 -> DFAState[3, <|a -> 1, b -> 2, c -> 1|>, {False, True}]|>,
    "initial" -> {1}, "terminal" -> {2, 3}, "alphabet" -> {a, b, c}|>];

VerificationTest[
  DFA[{1 -> {a -> 1, b -> 2, c -> 3},
    2 -> {a -> 3, b -> 2, c -> 1},
    3 -> {a -> 1, b -> 2, c -> 1}}, {1}, {2, 3}],
  $testDFA
];

VerificationTest[
  DFA[{1 -> {1, 2, 3}, 2 -> {3, 2, 1}, 3 -> {1, 2, 1}},
    {a, b, c}, {1}, {2, 3}],
  $testDFA
];

VerificationTest[DFA[{1 -> {a -> 1}, 2 -> {a -> 2, b -> 1}}, {1}, {2}], $Failed, DFA::missingtr ];

VerificationTest[DFA[{1 -> {a -> 1, b -> 2}, 2 -> {a -> 2, b -> 1}}, {1}, {2, 3}], $Failed, DFA::invterm];

VerificationTest[DFA[{1 -> {a -> 1, b -> 2}, 2 -> {a -> 2, b -> 1}}, 1, {2}], $Failed, DFA::badinit];

VerificationTest[DFA[{1 -> {a -> 1, b -> 3}, 2 -> {a -> 2, b -> 1}}, {1}, {2}], $Failed, DFA::invprod];

VerificationTest[DFA[{1 -> {a -> 1, b -> 1}, 2 -> {a -> 2, b -> 1}}, 1, {2}], $Failed, DFA::badinit];

VerificationTest[DFA[{1 -> {1}, 2 -> {3, 2, 1}, 3 -> {1, 2, 1}}, {a, b, c}, {1}, {2, 3}], DFA_,
  Thread::tdlen,
  SameTest -> MatchQ];

VerificationTest[$testDFA[{a, a, b, c, c}]];

VerificationTest[$testDFA[{a, a, b, c, c, c}], False];

VerificationTest[$testDFA[{a, a, b, d, c, c, c}], False, DFAState::invtr];

EndTestSection[];
