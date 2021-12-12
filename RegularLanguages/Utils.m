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

(* ::Section:: *)
(* Macros *)

PackageScope["absOpt"]
absOpt::usage = "absOpt[expr, name] returns the value of the rule obtained from AbsoluteOptions[expr, name]";
absOpt[expr_, name_] := AbsoluteOptions[expr, name][[1, 2]];

PackageScope["throwIf"]
throwIf::usage = "throwIf[pred, val] evaluates to Throw[val] when pred[val] is True, and Null otherwise.
throwIf[pred, val, tag] evaluates to Throw[val, tag] when pred[val] is true, and Null otherwise.";
throwIf[pred_, val_] := If[pred[val], Throw[val]];
throwIf[pred_, val_, tag_] := If[pred[val], Throw[val, tag]];

(*PackageExport["ruleValueMatchQ"]*)
OptionValue::invform = "Option `1` for `2` received unrecognized form `3`. Values for this option should match `4`.";

optValueMatchesQ[f_, patt_][k_ -> v_] :=
  MatchQ[v, patt] || Message[OptionValue::invform, k, f, v, patt] || False;

PackageExport["OptionChecks"]
OptionChecks::usage = "OptionChecks[f] gives {} by default.
Setting OptionChecks[f] = {opt1 -> patt1, opt2 -> patt2, ...}, where the opt_i are options of f, and patt_i are patterns will ensure that \
OptionPatterns[f]?(validQ[f]) only matches the opti when their values match patti, issuing a message otherwise.";
optionChecks[_] = {};

(*PackageExport["attachCheckMessages"]*)
(*attachCheckMessage[f_] := If[MemberQ[Attributes@f, Protected]*)
(*    (Unprotect[f];*)
(*    f::invopt = "Unrecognized value for option `1`. `2` does not match `3`.";*)
(*    attachCheckMessage[f] ^= True;*)
(*    Protect[f];*)
(*    True),*)
(*    f::invopt = "Unrecognized value for option `1`. `2` does not match `3`.";*)
(*    attachCheckMessage[f] ^= True*)
(*  ];*)

PackageExport["validQ"]
validQ::usage = "The pattern OptionsPattern[]?(validQ[f]) will only match a rule whose lhs is a key in optionChecks[f] \
when its rhs matches the associated pattern in optionChecks[f].";
validQ[f_][opts___] := AllTrue[OptionChecks[f],
  AllTrue[FilterRules[{opts}, #[[1]]],
    optValueMatchesQ[f, #[[2]]]] &];

PackageScope["applyIf"]
SetAttributes[applyIf, HoldRest];
applyIf::usage = "applyIf[cond, f, expr] returns f[expr] if cond evaluates to True, and expr otherwise.";
applyIf[cond_, func_, expr_] := If[TrueQ[cond], func@expr, expr];

PackageScope["rangeOver"]
rangeOver::usage = "rangeOver[expr] returns Range[Length[expr]]
rangeOver[expr, i] returns Range[di], where di is the size of dimension i in expr.";
rangeOver[expr_] := Range[Length[expr]];
rangeOver[expr_, dim_] := Range[Extract[Dimensions[expr], dim]];

PackageScope["aside"]
aside::usage = "aside[f, expr] evaluates f[expr], then returns expr.
aside[{f1, f2, ...}, expr] evaluates f1[expr], f2[expr], ... in sequence, then returns expr.
aside[funcs] represents an operator form of aside that can be applied to expressions.";
aside[f_, expr_] := (f[expr]; expr);
aside[f_][expr_] := (f[expr]; expr);
aside[fs_List, expr_] := (Do[f[expr], {f, fs}]; expr);
aside[fs_List][expr_] := (Do[f[expr], {f, fs}]; expr);

PackageScope["unless"]
unless::usage = "unless[expr, form1, alt1, form2, alt2, ...] evaluates expr, then compares it to each of the \
formi in turn, evaluating and returning the alti corresponding to the first match, or expr itself if no match is found.";
SetAttributes[unless, HoldRest];
unless[value_, alts : PatternSequence[_, _] ..] := Switch[value, alts, _, value];
SyntaxInformation[unless] = {"ArgumentsPattern" -> {_, _, __}};

PackageScope["when"]
when::usage = "when[expr, form] returns expr if it matches form, and Null otherwise.
when[expr, form, alt] returns expr if it matches form, and alt otherwise.";
SetAttributes[when, HoldRest];
when[expr_, form_] := If[MatchQ[expr, form], expr];
when[expr_, form_, alt_] := If[MatchQ[expr, form], expr, alt];

PackageScope["firstReaped"]
firstReaped::usage = "firstReaped[tag, expr] returns the first element of Last@Reap[expr, tag], or {} if it is empty.
firstReaped[tag] returns an operator form of firstReaped that can be applied to expressions.";
SetAttributes[firstReaped, HoldRest];
firstReaped[tag_ : None, expr_] := First[Last@Reap[expr, tag], {}];
firstReaped[tag_] := Function[{expr}, firstReaped[tag, expr], HoldFirst];

(* ::Section:: *)
(* Functions *)

PackageScope["filterOpts"]
filterOpts::usage = "
filterOpts[{opt1 -> v1, opt2 -> v2, ...}, f] returns a sequence opti -> vi where opti matches the left-hand side of a rule in Options[f].
filterOpts[{opt1 -> v1, ...}, {f1, f2, ...}] returns the sequence opti -> vi, where opti matches the lhs of a rule in any of Options[f1], Options[f2], ...
filterOpts[opts, funcs, g] returns the sequence of opti -> vi where opti matches some option of funcs, but does not match the lhs of any rule in Options[g]
filterOpts[opts, funcs, {g1, g2, ...}] returns the same, where opti does not match the lhs of any rule in Options[g1], Options[g2], ...";
filterOpts[opts_?OptionQ, f_List] :=
  Sequence @@ FilterRules[opts, Catenate[Options /@ f]];
filterOpts[opts_?OptionQ, f_] := filterOpts[opts, {f}];
filterOpts[opts_?OptionQ, f_List, except_List] :=
  Sequence @@ Fold[FilterRules, opts,
    {Catenate[Options /@ f], Except[Catenate[Options /@ except]]}];
filterOpts[opts_?OptionQ, f_, except_] :=
  filterOpts[opts,
    If[ListQ@f, f, {f}],
    If[ListQ@except, except, {except}]];

PackageScope["validatedMethod"]
validatedMethod::usage = "validatedMethod[given, {m1, m2, ...}, caller] returns given if it is one of {m1, m2, ...}. \
Otherwise, it issues the message caller::moptx, and returns Automatic.
validatedMethod[..., default] returns default instead of Automatic.";
validatedMethod[given_, expected_, caller_, default_ : Automatic] := With[
  {expectedNames = methodName /@ expected},
  If[MemberQ[expectedNames, methodName@given],
    methodName@given,
    Message[caller::moptx, given, caller, expectedNames]; default]];

methodName[{name_, ___}] := name;
methodName[name_] := name;

PackageScope["intProp"]
intProp::usage = "intProp[x, total] returns x if x is an integer, and Ceiling[x*total] otherwise.";
intProp[p_Integer, _] := p;
intProp[p_, tot_] := Ceiling[p * tot];

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

PackageScope["autoAlt"]
autoAlt::usage = "autoAlt[expr, alternative] returns alternative if expr === Automatic, and expr otherwise.";
SetAttributes[autoAlt, HoldRest];
autoAlt[value_, alt_] := If[value === Automatic, alt, value];

PackageScope["rotateToFront"]
rotateToFront::notfound = "No subexpression matching `1` was found in `2`.";
rotateToFront::usage = "rotateToFront[expr, form] cycles the elements in expr to put the first element matching form in position 1";
rotateToFront[expr_, form_] := Catch@RotateLeft[expr,
  First@FirstPosition[expr, form, Message[rotateToFront::notfound, form, expr]; Throw[expr]] - 1];

PackageScope["randomSubset"]
randomSubset::usage = "randomSubset[{e1, e2, ...}] gives a pseudorandom subset of the ei in pseudorandom order.
randomSubset[list, n] returns a list of n random subsets.
randomSubset[list, {imin, imax}] returns a pseudorandom subset of list with length between imin and imax.
randomSubset[list, {imin, imax}, n] returns n such random subsets.";
randomSubset::smplen = "randomSubset cannot generate subsets of maximum length `1`, which is greater than the length of the sample set `2`.";
randomSubset::invspec = "Invalid length specification `1` received at position 2 of randomSubset, where a pair of nondecreasing, nonnegative integers were expected.";
randomSubset[s_List] :=
  RandomSample[s, RandomVariate[BinomialDistribution[Length@s, 0.5]]];
randomSubset[s_List, n_Integer] := RandomSample[s, #] & /@ RandomVariate[BinomialDistribution[Length@s, 0.5], n];
randomSubset[s_List, {imin_Integer, imax_Integer}] :=
  RandomSample[s, RandomChoice[ Binomial[Length@s, Range[imin, imax]] -> Range[imin, imax]]] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= Length@s || Message[randomSubset::smplen, imax, s]];
randomSubset[s_List, {imin_Integer, imax_Integer}, n_Integer] :=
  RandomSample[s, #] & /@ RandomChoice[ Binomial[Length@s, Range[imin, imax]] -> Range[imin, imax], n] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= Length@s || Message[randomSubset::smplen, imax, s]];
randomSubset[s : (i_Integer ;; j_Integer ;; k_Integer : 1)] :=
  RandomSample[s, RandomVariate[BinomialDistribution[spanLength@s, 0.5]]];
randomSubset[s : (i_Integer ;; j_Integer ;; k_Integer : 1), n_Integer] :=
  RandomSample[s, #] & /@ RandomVariate[BinomialDistribution[spanLength@s, 0.5], n];
randomSubset[ s : (i_Integer ;; j_Integer ;; k_Integer : 1), {imin_Integer, imax_Integer}] :=
  RandomSample[s, RandomChoice[Binomial[spanLength@s, Range[imin, imax]] -> Range[imin, imax]]] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= spanLength@s || Message[randomSubset::smplen, imax, s]];
randomSubset[s : (i_Integer ;; j_Integer ;; k_Integer : 1), {imin_Integer, imax_Integer}, n_Integer] :=
  RandomSample[s, #] & /@ RandomChoice[ Binomial[spanLength@s, Range[imin, imax]] -> Range[imin, imax], n] /;
    And[ 0 <= imin <= imax || Message[randomSubset::invspec, {imin, imax}],
      imax <= spanLength@s || Message[randomSubset::smplen, imax, s]];

PackageScope["makeStateSummaryBoxes"]
makeStateSummaryBoxes::usage = "makeStateSummaryBoxes[state] generates display boxes for the given DFA or NFA state.";
makeStateSummaryBoxes[s : (head : DFAState | NFAState)[___], form_] :=
  With[{color = Switch[head,
    DFAState, If[InitialQ@s, RGBColor[
      Rational[2, 3], 0.33333333333333337`, 0.33333333333333337`], RGBColor[
      0.275184, 0.392896, 0.719488]],
    NFAState,
    If[InitialQ@s, RGBColor[0.1454912, 0.533312, 0.6958304],
      RGBColor[0.9215, 0.5757, 0.07695]]]},
    BoxForm`ArrangeSummaryBox[
      None, s,
      makeStateIcon[s],
      makeStateUpperSummary[s],
      makeStateTransitionSummary[s],
      form, "Interpretable" -> True] /. {
      RowBox[{TagBox["None", "SummaryHead"], "[", \[FormalA]_,
        "]"}] -> \[FormalA]} /. {
      TemplateBox[\[FormalA]_, "SummaryPanel"] ->
        TemplateBox[\[FormalA],
          "StateSummaryPanel",
          DisplayFunction -> (FrameBox[#,
            Alignment -> {Left, Center},
            Appearance -> {"Default" -> None},
            FrameMargins -> {{7.5, 5}, {2.5, 5}},
            FrameStyle -> color,
            RoundingRadius -> 3,

            BaseStyle -> {Deployed -> False, Selectable -> False,
              Background -> GrayLevel[1, 0.8]},
            DefaultBaseStyle -> {"Panel", Background -> None},
            BaselinePosition -> Baseline] &)]}];

PackageScope["makeAutomatonSummaryBoxes"]
makeAutomatonSummaryBoxes::usage = "makeAutomatonSummaryBoxes[A] generates display boxes for the given DFA or NFA.";
makeAutomatonSummaryBoxes[A : (head : NFA | DFA)[asc_],
  form : (StandardForm | TraditionalForm)] :=
  BoxForm`ArrangeSummaryBox[
    head, A, makeThumbnail[A],
    makeAutomatonUpperSummary[A],
    makeAutomatonStateSummary[A],
    form, "Interpretable" -> Automatic];

PackageScope["mergeTransitions"]
mergeTransitions::usage = "mergeTransitions[{nfastate1, nfastate2, ...}] returns the association <|a1 -> l1 ... |>, where li = Union[nfastate1[ai], nfastate2[ai], ...].";
mergeTransitions[states : {NFAState[_, _?AssociationQ, _] ...}] := Merge[states[[All, 2]], Apply[Union]];

PackageScope["sowPredicate"]
sowPredicate::usage = "sowPredicate[pred, tags] represents an operator that, when applied to x, yields pred[x], with the side effect Sow[x,tags] if pred[x] is True";
sowPredicate[pred_ -> f_, tags_ : None] :=
  With[{pval = pred[#]},
    If[pval, Sow[f@#, tags]; True, False, pval]] &;
sowPredicate[pred_, tags_ : None] :=
  With[{pval = pred[#]}, If[pval, Sow[#, tags]; True, False, pval]] &;

PackageScope["transitionLookup"]
transitionLookup::usage = "transitionLookup[expr, {a1, a2, ...}] returns Transitions[expr, {a1, a2, ...}, Nothing] if\
expr is an explicit DFA or NFA state, and Lookup[expr, {a1, a2, ...}, Nothing] if expr is an association.";
transitionLookup[s_, All] := Values[s];
transitionLookup[s_?StateQ, symbols_List] := Transitions[s, symbols, Nothing];
transitionLookup[s_Association, symbols_List] := Lookup[s, symbols, Nothing];

PackageScope["updateState"]
updateState::usage = "updateState[state, f] returns a copy of state whose ID and transitions are renamed according to f
updateState[state, f, spec] returns a copy of state whose ID and transitions are renamed according to f, and whose initial/terminal specification is spec.
updateState[f] and updateState[f, spec] return operator forms of updateState that can be applied to states.";
updateState[DFAState[id_, d_, rest___], namefn_] := DFAState[namefn[id], namefn /@ d, rest];
updateState[s : DFAState[id_, d_, ___], namefn_, {init_, term_}] :=
  DFAState[namefn[id], namefn /@ d, {autoAlt[init, InitialQ@s], autoAlt[term, TerminalQ@s]}];
updateState[DFAState[id_, d_, ___], namefn_, rest_] := DFAState[namefn[id], namefn /@ d, rest];
updateState[NFAState[id_, d_, rest___], namefn_] := NFAState[namefn[id], Map[namefn, d, {2}], rest];
updateState[s : NFAState[id_, d_, ___], namefn_, {init_, term_}] :=
  NFAState[namefn[id], Map[namefn, d, {2}], {autoAlt[init, InitialQ@s], autoAlt[term, TerminalQ@s]}];
updateState[s : NFAState[id_, d_, ___], namefn_, rest_] := NFAState[namefn[id], Map[namefn, d, {2}], rest];
updateState[namefn_] := OperatorApplied[updateState][namefn];
updateState[namefn_, rest_] := OperatorApplied[updateState, {3, 1, 2}][namefn, rest];

PackageScope["updateStateRule"]
updateStateRule::usage = "updateStateRule[state, f] returns a rule f[StateID[state]] -> updateState[state,f]
updateStateRule[state, f, spec] returns a rule f[StateID[state]] -> updateState[state, f, spec]
updateStateRule[f] and updateStateRule[f, spec] return an operator forms of updateStateRule that can be applied to states.";
updateStateRule[s : (DFAState | NFAState)[id_, ___], namefn_, rest___] := namefn[id] -> updateState[s, namefn, rest];
updateStateRule[namefn : Except[_NFAState | _DFAState]] := OperatorApplied[updateStateRule][namefn];
updateStateRule[namefn : Except[_NFAState | _DFAState], rest_] :=
  OperatorApplied[updateStateRule, {3, 1, 2}][namefn, rest];

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

PackageScope["makeStateIDs"]
makeStateIDs::usage = "makeStateIDs[n] returns {1, 2, ..., n}
makeStateIDs[n, f] returns {f[1], f[2], ..., f[n]}.
makeStateIDs[{q1, q2, ...}, f] returns {f[q1], f[q2], ...}. If f is omitted, {q1, q2, ...} is returned unchanged.";
makeStateIDs[k : (_Integer | _List), f_ : Automatic] := If[ListQ@k, Map, Array][autoAlt[f, Identity], k];

(* ::Section:: *)
(* Private functions *)

spanLength[i_ ;; j_ ;; k_ : 1] := 1 + Floor[(j - i) / k];

makeStateIcon[input : (NFAState | DFAState)[id_, ___]] := makeStateIcon[input] =
  Deploy@Pane[Style[id, 12, ShowSyntaxStyles -> False],
    Alignment -> {Center, Center},
    ContentPadding -> False,
    FrameMargins -> {{1, 1}, {0, 0}},
    ImageSizeAction -> "ShrinkToFit",
    ImageSize ->
      Dynamic[{Automatic,
        3.5` CurrentValue["FontCapHeight"] /
          AbsoluteCurrentValue[Magnification]}]];

makeStateUpperSummary[state_] := {
  BoxForm`SummaryItem[{"Transitions: ", Length@Transitions@state}],
  BoxForm`SummaryItem[{"Terminal: ", If[TerminalQ@state, "Yes", "No"]}]};

makeStateTransitionSummary[(NFAState | DFAState)[_, <||>, ___], ___] = {};
makeStateTransitionSummary[NFAState[_, d_, ___], displaymax_ : 5] := {
  BoxForm`SummaryItem[{"Transitions: ", ""}],
  Grid[
    KeyValueMap[{#1, "\[Rule]",
      If[Length@#2 == 1, First@#2,
        Column[#2, {Left, Automatic}, {0, 0},
          BaselinePosition -> {1, Automatic}]]} &,
      Take[d, UpTo[displaymax]]
    ] ~ Append ~ If[Length@d > displaymax,
      {"", "\[VerticalEllipsis]", ""}, Nothing],
    Alignment -> {{Right, Center, Left}, Baseline},
    Spacings -> {{0, 0.3, 0.3, 0}, {0, {}, 0}}]};

makeStateTransitionSummary[DFAState[_, d_, ___], displaymax_ : 5] := {
  BoxForm`SummaryItem[{"Transitions: ", ""}],
  Splice[Normal[Take[d, UpTo[displaymax]]]],
  If[Length@d > displaymax,
    Item["\[VerticalEllipsis]", Alignment -> Center], Nothing]};

makeAutomatonUpperSummary[A_, displaymax_ : 3] :=
  With[{alph = LanguageAlphabet[A]}, {
    BoxForm`SummaryItem[{
      "\[CapitalSigma]: ",
      With[{es = If[MemberQ[alph, Epsilon], Epsilon, Nothing]},
        Replace[If[es =!= Nothing, DeleteCases[alph, Epsilon], alph], {
          {h : Repeated[_, {displaymax}], _, __, t_} :> {h, "\[Ellipsis]", t, es},
          {x___} :> {x, es}
        }]],
      StringForm["  (``)", Length@alph]
    }],
    BoxForm`SummaryItem[{"States: ", StateCount[A]}]
  }];

makeAutomatonStateSummary[A_, displaymax_ : 3] := {
  Row[{
    BoxForm`SummaryItem[{"Initial: ", StateCount[A, "Initial"]}],
    BoxForm`SummaryItem[{"Terminal: ", StateCount[A, "Terminal"]}]},
    Spacer[3]],
  Splice@Take[States[A, "Initial", "Values"], UpTo[displaymax]],
  Splice@Lookup[States[A],
    Cases[IDs[A], Except[Alternatives @@ IDs[A, "Initial"]],
      {1}, Max[displaymax - StateCount[A, "Initial"], 0]]],
  If[StateCount[A] > displaymax,
    Item["\[VerticalEllipsis]", Alignment -> Center], Nothing]};
