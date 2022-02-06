(* ::Package:: *)

(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: Utils *)
(* :Context: Utils` *)
(* :Author: Adam Smith *)
(* :Date: 2020-05-19 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2020 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

(* Declare package context *)
(*Package["RegularLanguages`"]*)

Package["RegularLanguages`"]
PackageImport["GeneralUtilities`"]

(* ::Section:: *)
(* Patterns *)

PackageScope["reHeadP"]
reHeadP::usage = "Pattern that matches expressions with head REUnion, REConcat, or REStar";
reHeadP = _REUnion | _REConcat | _REStar;


(* ::Section:: *)
(* Macros *)

PackageScope["AbsoluteOption"]
AbsoluteOption::usage = "AbsoluteOption[expr, name] returns the value of the rule obtained from AbsoluteOptions[expr, name]";
AbsoluteOption[expr_, name_] :=
  AbsoluteOptions[expr, name][[1, 2]];

PackageScope["WithOptions"]
WithOptions::usage = "WithOptions[f, opts...] returns f[##, opts] & ";
WithOptions[f_, opts__] := f[##, opts] &;

PackageScope["ThrowIf"]
ThrowIf::usage = "ThrowIf[pred, val] evaluates to Throw[val] when pred[val] is True, and Null otherwise.
ThrowIf[pred, val, tag] evaluates to Throw[val, tag] when pred[val] is true, and Null otherwise.";
ThrowIf[pred_, val_] :=
  If[pred @ val, Throw @ val];
ThrowIf[pred_, val_, tag_] :=
  If[pred @ val, Throw[val, tag]];

OptionValue::invform = "The `1` option of `2` requires a value matching `3`, but received `4`.";
optValueMatchesQ[f_, patt_][k_ -> v_] :=
  MatchQ[v, patt] ||
    (
      Message[OptionValue::invform,
        k,
        f,
        Style[patt, ShowStringCharacters -> True],
        Style[v, ShowStringCharacters -> True]
      ];
      False
    );

PackageScope["OptionChecks"]
OptionChecks::usage = "OptionChecks[f] gives {} by default.
Setting OptionChecks[f] = {opt1 -> patt1, opt2 -> patt2, ...}, where the opt_i are options of f, and patt_i are patterns will ensure that \
OptionPatterns[f]?(validQ[f]) only matches the opti when their values match patti, issuing a message otherwise.";
OptionChecks[_] = {};

(*PackageScope["checkOpts"]*)
(*checkOpts::usage = "The pattern OptionsPattern[]?(Validate[f]) will only match a rule whose lhs is a key in optionChecks[f] \*)
(*when its rhs matches the associated pattern in OptionChecks[f].";*)
checkOpts[f_][opts___] :=
  AllTrue[
    OptionChecks @ f,
    AllTrue[
      FilterRules[
        {opts},
        #[[1]]
      ],
      optValueMatchesQ[
        f,
        #[[2]]
      ]
    ] &
  ];

PackageScope["HoldIgnoringInactive"]
HoldIgnoringInactive::usage = "HoldIgnoringInactive is equivalent to HoldPattern @* IgnoringInactive";
SetAttributes[HoldIgnoringInactive, HoldAll];
HoldIgnoringInactive[x_] := HoldPattern @ IgnoringInactive @ x;

PackageScope["CheckedOptions"]
CheckedOptions::usage = "CheckedOptions[f]";
CheckedOptions[f_] :=
  OptionsPattern[f]?(checkOpts @ f);
CheckedOptions[specs_, f_] :=
  OptionsPattern[specs]?(checkOpts @ f);

PackageScope["protectedQ"]
protectedQ::usage = "protectedQ[symbol] Returns true if symbol has attribute Protected.";
protectedQ[x_] := MemberQ[Attributes[x], Protected];

PackageScope["applyIf"]
SetAttributes[applyIf, HoldRest];
applyIf::usage = "applyIf[cond, f, expr] returns f[expr] if cond evaluates to True, and expr otherwise.";
applyIf[cond_, func_, expr_] :=
  If[TrueQ[cond], func @ expr, expr];

PackageScope["Cond"]
Cond::usage = "Cond[test, f][expr] returns f[expr] if test is True, otherwise expr.";
Cond[test_, f_][expr_] := If[TrueQ @ test, f @ expr, expr];

PackageScope["aside"]
aside::usage = "aside[f, expr] evaluates f[expr], then returns expr.
aside[{f1, f2, ...}, expr] evaluates f1[expr], f2[expr], ... in sequence, then returns expr.
aside[funcs] represents an operator form of aside that can be applied to expressions.";
aside[f_, expr_] := (f[expr]; expr);
aside[f_][expr_] := (f[expr]; expr);
aside[fs_List, expr_] := (Do[f[expr], {f, fs}]; expr);
aside[fs_List][expr_] := (Do[f[expr], {f, fs}]; expr);

PackageScope["UnlessMatch"]
UnlessMatch::usage =
  "UnlessMatch[expr, form1, alt1, form2, alt2, ...] evaluates expr, then compares it to each of the \
formi in turn, evaluating and returning the alti corresponding to the first match, or expr itself if no match is found.";
SetAttributes[UnlessMatch, HoldRest];
UnlessMatch[value_, alts : PatternSequence[_, _] ..] :=
  Switch[value, alts, _, value];

PackageScope["IfMatch"]
IfMatch::usage =
  "IfMatch[form, expr] evaluates expr and returns it if it matches form, or Null if it does not.
IfMatch[form, expr, alt] evaluates expr and returns it if it matches form, or evaluates and returns alt if not.";
SetAttributes[IfMatch, HoldRest];
IfMatch[form_, expr_, alt_ : Null] :=
  With[{val = expr}, If[MatchQ[val, form], val, alt]];

PackageScope["firstReaped"]
firstReaped::usage =
  "firstReaped[tag, expr] returns the first element of Last @ Reap[expr, tag], or {} if it is empty.
firstReaped[tag] returns an operator form of firstReaped that can be applied to expressions.";
SetAttributes[firstReaped, HoldRest];
firstReaped[tag_ : None, expr_] := First[Last @ Reap[expr, tag], {}];
firstReaped[tag_] := Function[{expr}, firstReaped[tag, expr], HoldFirst];

PackageScope["ReapExactly"]
ReapExactly::usage = "\
ReapExactly[expr, tag] returns {expr, {vals...}}, where Reap[...] would give {expr, {{vals...}}}.
ReapExactly[expr, {tag1, tag2, ...}] gives {expr, {{vals1...}, {vals2...}, ...}} where Reap[...] would give \
{expr, {{{vals1...}}, {{vals2...}}, ...}}.
If ReapExactly is called with patterns as tags, the returned values are the catenation of values sown from any matching tag.";
SetAttributes[ReapExactly, HoldFirst];
ReapExactly[expr_, tags_] :=
  MapAt[
    Catenate,
    Reap[expr, tags],
    If[ListQ @ tags, {2, All}, {2}]
  ];

PackageScope["loudly"]
SetAttributes[loudly, HoldRest];
loudly::usage = "loudly[pred, mess][args...] returns pred[args], calling Message[mess, args] if pred[args] is not True";
loudly[pred_, mess_][args___] :=
  With[{res = pred[args]},
    If[res =!= True, Message[mess, args]];
    res];

PackageScope["MessageFalse"]
MessageFalse::usage = "MessageFalse[msg, args...] calls Message[msg, args...] then returns False";
SetAttributes[MessageFalse, HoldFirst];
MessageFalse[msg_, args___] :=
  (
    Message[msg, args];
    False
  );

(* ::Section:: *)
(* Functions *)

PackageScope["filteredOptionSequence"]
filteredOptionSequence::usage = "
filteredOptionSequence[{opt1 -> v1, opt2 -> v2, ...}, f] returns a sequence opti -> vi where opti matches the left-hand side of a rule in Options[f].
filteredOptionSequence[{opt1 -> v1, ...}, {f1, f2, ...}] returns the sequence opti -> vi, where opti matches the lhs of a rule in any of Options[f1], Options[f2], ...
filteredOptionSequence[opts, funcs, g] returns the sequence of opti -> vi where opti matches some option of funcs, but does not match the lhs of any rule in Options[g]
filteredOptionSequence[opts, funcs, {g1, g2, ...}] returns the same, where opti does not match the lhs of any rule in Options[g1], Options[g2], ...";
filteredOptionSequence[opts_?OptionQ, f_List] :=
  Sequence @@ FilterRules[opts, Catenate[Options /@ f]];
filteredOptionSequence[opts_?OptionQ, f_] := filteredOptionSequence[opts, {f}];
filteredOptionSequence[opts_?OptionQ, f_List, except_List] :=
  Sequence @@ Fold[
    FilterRules,
    opts,
    {
      Catenate[Options /@ f],
      Except @ Catenate[Options /@ except]
    }
  ];
filteredOptionSequence[opts_?OptionQ, f_, except_] :=
  filteredOptionSequence[opts,
    If[ListQ @ f, f, {f}],
    If[ListQ @ except, except, {except}]];

PackageScope["validatedMethod"]
validatedMethod::usage = "validatedMethod[given, {m1, m2, ...}, caller] returns given if it is one of {m1, m2, ...}. \
Otherwise, it issues the message caller::moptx, and returns Automatic.
validatedMethod[..., default] returns default instead of Automatic.";
validatedMethod[given_, expected_, caller_, default_ : Automatic] := With[
  {expectedNames = methodName /@ expected},
  If[MemberQ[expectedNames, methodName @ given],
    methodName @ given,
    Message[caller::moptx, given, caller, expectedNames]; default]];

methodName[{name_, ___}] := name;
methodName[name_] := name;

PackageScope["intOrMult"]
intOrMult::usage = "intOrMult[x, total] returns x if x is an integer, and Ceiling[x*total] otherwise.";
intOrMult[p_Integer, _] := p;
intOrMult[p_, tot_] := Ceiling[p * tot];

PackageScope["specificArguments"]
specificArguments::usage = "specificArguments[f] returns all the non-pattern expressions e for which f[e] has been explicitly defined.
specificArguments[f, True] also returns non-pattern expressions e1, e2, ... where f[e1|e2|...] has been explicitly defined.";
specificArguments[f_, separateAlternatives_ : False] :=
  If[separateAlternatives,
    specificArguments[f] /. {Alternatives -> Sequence},
    Cases[DownValues[f], HoldPattern[f[x_?(FreeQ[Pattern])]] :> x, {3}]];

PackageScope["toAlternatives"]
toAlternatives::usage = "toAlternatives[{p1, p2, ...}] returns p1 | p2 | ...";
toAlternatives[{a_, b__}] := Alternatives[a, b];
toAlternatives[{a_}] := a;
toAlternatives[{}] := Alternatives[];

PackageScope["UnlessAutomatic"]
UnlessAutomatic::usage = "UnlessAutomatic[expr, alternative] returns alternative if expr === Automatic, and expr otherwise.";
SetAttributes[UnlessAutomatic, HoldRest];
UnlessAutomatic[value_, alt_] := If[value === Automatic, alt, value];

PackageScope["rotateToFront"]
rotateToFront::notfound = "No subexpression matching `1` was found in `2`.";
rotateToFront::usage = "rotateToFront[expr, form] cycles the elements in expr to put the first element matching form in position 1";
rotateToFront[expr_, form_] := Catch @ RotateLeft[expr,
  First @ FirstPosition[expr, form, Message[rotateToFront::notfound, form, expr]; Throw[expr]] - 1];

PackageScope["hasKeyQ"]
hasKeyQ::usage = "hasKeyQ[expr][key] is equivalent to KeyExistsQ[expr, key]";
hasKeyQ[expr_][key_] := KeyExistsQ[expr, key];

PackageScope["randomSubset"]
randomSubset::usage = "randomSubset[{e1, e2, ...}] gives a pseudorandom subset of the ei in pseudorandom order.
randomSubset[list, n] returns a list of n random subsets.
randomSubset[list, {imin, imax}] returns a pseudorandom subset of list with length between imin and imax.
randomSubset[list, {imin, imax}, n] returns n such random subsets.";
randomSubset::smplen = "randomSubset cannot generate subsets of maximum length `1`, which is greater than the length of the sample set `2`.";
randomSubset::invspec = "Invalid length specification `1` received at position 2 of randomSubset, where a pair of nondecreasing, nonnegative integers were expected.";
randomSubset[s_List] :=
  RandomSample[s, RandomVariate[BinomialDistribution[Length @ s, 0.5]]];
randomSubset[s_List, n_Integer] := RandomSample[s, #] & /@ RandomVariate[BinomialDistribution[Length @ s, 0.5], n];
randomSubset[s_List, {imin_Integer, imax_Integer}] :=
  RandomSample[s, RandomChoice[ Binomial[Length @ s, Range[imin, imax]] -> Range[imin, imax]]] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= Length @ s || Message[randomSubset::smplen, imax, s]];
randomSubset[s_List, {imin_Integer, imax_Integer}, n_Integer] :=
  RandomSample[s, #] & /@ RandomChoice[ Binomial[Length @ s, Range[imin, imax]] -> Range[imin, imax], n] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= Length @ s || Message[randomSubset::smplen, imax, s]];
randomSubset[s : (i_Integer ;; j_Integer ;; k_Integer : 1)] :=
  RandomSample[s, RandomVariate[BinomialDistribution[spanLength @ s, 0.5]]];
randomSubset[s : (i_Integer ;; j_Integer ;; k_Integer : 1), n_Integer] :=
  RandomSample[s, #] & /@ RandomVariate[BinomialDistribution[spanLength @ s, 0.5], n];
randomSubset[ s : (i_Integer ;; j_Integer ;; k_Integer : 1), {imin_Integer, imax_Integer}] :=
  RandomSample[s, RandomChoice[Binomial[spanLength @ s, Range[imin, imax]] -> Range[imin, imax]]] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= spanLength @ s || Message[randomSubset::smplen, imax, s]];
randomSubset[s : (i_Integer ;; j_Integer ;; k_Integer : 1), {imin_Integer, imax_Integer}, n_Integer] :=
  RandomSample[s, #] & /@ RandomChoice[ Binomial[spanLength @ s, Range[imin, imax]] -> Range[imin, imax], n] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= spanLength @ s || Message[randomSubset::smplen, imax, s]];

PackageScope["makeAutomatonSummaryBoxes"]
makeAutomatonSummaryBoxes::usage = "makeAutomatonSummaryBoxes[A] generates display boxes for the given DFA or NFA.";
makeAutomatonSummaryBoxes[A : (head : NFA | DFA)[asc_], form_] :=
  BoxForm`ArrangeSummaryBox[
    head, A, makeThumbnail[A],
    makeAutomatonUpperSummary[A],
    makeAutomatonStateSummary[A],
    form, "Interpretable" -> Automatic];

PackageScope["makeTransitionFunctionSummaryBoxes"]
makeTransitionFunctionSummaryBoxes::usage = "makeTransitionFunctionSummaryBoxes[tf] generates display boxes for the given transition function.";
makeTransitionFunctionSummaryBoxes[expr : TransitionFunction[type_, trns_], form_] :=
  BoxForm`ArrangeSummaryBox[
    TransitionFunction,
    expr,
    None,
    makeTransitionFunctionUpperSummary[expr],
    makeTransitionFunctionLowerSummary[expr],
    form
  ];

PackageScope["sowPredicate"]
sowPredicate::usage = "sowPredicate[pred, tags] represents an operator that, when applied to x, yields pred[x], with the side effect Sow[x,tags] if pred[x] is True";
sowPredicate[pred_ -> f_, tags_ : None] :=
  With[{pval = pred[#]},
    If[pval, Sow[f@#, tags]; True, False, pval]] &;
sowPredicate[pred_, tags_ : None] :=
  With[{pval = pred[#]}, If[pval, Sow[#, tags]; True, False, pval]] &;

PackageScope["makeAlphabet"]
makeAlphabet::usage = "makeAlphabet[n] returns an alphabet of length n, consisting of the first n letters in the \
English alphabet if n <= 26, or {\"a1\", \"a2\", ... , \"an\"} otherwise.
makeAlphabet[k, f] returns {f[1], f[2], ..., f[k]}.
makeAlphabet[{a1, a2, ...}, f] returns {f[a1], f[a2], ...}. If f is omitted, {a1, a2, ...} is returned unchanged.";
makeAlphabet[k_Integer, f_ : Automatic] := Which[
  f =!= Automatic, Array[f, k],
  k <= 26, Take[Alphabet[], k],
  True, Array[StringTemplate["a``"], k]];
makeAlphabet[k_List, f_ : Automatic] := Which[
  f =!= Automatic, f /@ k,
  True, k];

PackageScope["makeStates"]
makeStates::usage = "makeStates[n] returns {1, 2, ..., n}
makeStates[n, f] returns {f[1], f[2], ..., f[n]}.
makeStates[{q1, q2, ...}, f] returns {f[q1], f[q2], ...}. If f is omitted, {q1, q2, ...} is returned unchanged.";
makeStates[k : (_Integer | _List), f_ : Automatic] := If[ListQ @ k, Map, Array][UnlessAutomatic[f, Identity], k];

PackageScope["$notationUnsets"]
$notationUnsets::usage = "Notation to unset.";
$notationUnsets = {};

(* ::Section:: *)
(* Private functions *)


spanLength[i_ ;; j_ ;; k_ : 1] := 1 + Floor[(j - i) / k];

ellided[expr_, maxlen_, ellipsis_ : Style["\[Ellipsis]", ShowStringCharacters -> False]] :=
  Replace[expr, head_[xs : Repeated[_, {maxlen}], _, __, t_] :> head[xs, ellipsis, t]];

styleSummaryList = Style[#, ShowStringCharacters -> True, SpanMaxSize -> 1]&;

countRow = Row[{Spacer[4], "(", #, ")"}]&;

makeAutomatonUpperSummary[A_, displaymax_ : 3] :=
  With[{alph = LanguageAlphabet[A], states = States[A]},
    {
      BoxForm`SummaryItem[
        {
          "States: ",
          styleSummaryList @ ellided[states, displaymax],
          countRow @ StateCount @ A
        }
      ],
      BoxForm`SummaryItem[
        {
          "Alphabet: ",
          With[{hasEpsilon = MemberQ[alph, Epsilon]},
            styleSummaryList @
              Cond[hasEpsilon, Append[Epsilon]] @ ellided[
                If[hasEpsilon, DeleteCases[alph, Epsilon], alph],
                displaymax - Boole[hasEpsilon]
              ]
          ],
          countRow @ Length @ alph
        }
      ]
    }
  ];

makeAutomatonStateSummary[A_, displaymax_ : 3] :=
  With[{inits = States[A, "Initial"], terms = States[A, "Terminal"]},
    {
      BoxForm`SummaryItem[
        Switch[FAType[A],
          NFA,
          {
            "Initial States: ",
            styleSummaryList @ ellided[inits, displaymax],
            countRow @ Length @ inits
          },
          DFA,
          {
            "Initial State: ",
            styleSummaryList @ First @ inits
          }
        ]
      ],
      BoxForm`SummaryItem[
        {
          "Terminal States: ",
          styleSummaryList @ ellided[terms, displaymax],
          countRow @ Length @ terms
        }
      ]
    }
  ];

makeTransitionFunctionUpperSummary[TransitionFunction[type_, trns_], displaymax_ : 3] :=
  {
    BoxForm`SummaryItem[
      {
        "Type: ",
        Switch[type, DFA, "Deterministic", NFA, "Nondeterministic", _, "Unknown"]
      }
    ],
    BoxForm`SummaryItem[
      {
        "Transitions: ",
        Total[Length /@ trns]
      }
    ]
  };

makeTransitionFunctionLowerSummary[TransitionFunction[type_, trns_], displaymax_ : 3] :=
  With[{alph = Sort @ Keys @ (Join @@ trns)},
    {
      BoxForm`SummaryItem[
        {
          "States: ",
          styleSummaryList @ ellided[Keys @ trns, displaymax],
          countRow @ Length @ trns
        }
      ],
      BoxForm`SummaryItem[
        {
          "Alphabet: ",
          With[{hasEpsilon = MemberQ[alph, Epsilon]},
            styleSummaryList @ applyIf[
              hasEpsilon,
              Append[Epsilon],
              ellided[
                If[hasEpsilon, DeleteCases[alph, Epsilon], alph],
                displaymax - Boole[hasEpsilon]
              ]
            ]
          ],
          countRow @ Length @ alph
        }
      ]
    }
  ];




