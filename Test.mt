(* Mathematica Test File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

BeginTestSection["DFAs.mt"];

VerificationTest[
  DFA[{1 -> {1, 2, 3}, 2 -> {3, 2, 1}, 3 -> {1, 2, 1}}, {a, b, c}, {1}, {2, 3}],
  DFA[<|""|>]
]

EndTestSection[];
