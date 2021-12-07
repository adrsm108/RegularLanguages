(* Mathematica Init file *)

With[{names = Table[ToString[c] <> "*", {c, Contexts["RegularLanguages`*"]}]},
  Unprotect @@ names;
  Remove @@ names;
];
(*Get[FileNameJoin[{ParentDirectory@DirectoryName@$InputFileName, "Utils.m"}]];*)

Block[{Notation`AutoLoadNotationPalette = False},
  Get[FileNameJoin[{ParentDirectory@DirectoryName@$InputFileName, "Utils.m"}]];
  Quiet@Needs["Notation`"];
  With[{
    style = StyleBox[#, FontWeight -> Bold, FontColor -> RGBColor[0.25098, 0.501961, 0.501961]] &,
    noEdit = Sequence @@ {Editable -> False, Selectable -> False},
    pbw = Notation`ParsedBoxWrapper, infix = Notation`InfixNotation,
    alias = Notation`AddInputAlias, note = Notation`Notation, wft = Notation`WorkingForm -> TraditionalForm,
    lr = DoubleLongLeftRightArrow},
    With[{
(*      unionOp = pbw[TagBox[style["|"], Identity, noEdit, SyntaxForm -> "+"]],*)
            unionOp = pbw[TagBox["|", "REUnionOp", noEdit, SyntaxForm -> "+"]],
      tradUnionOp = pbw[TagBox["|", "REUnionOp", SyntaxForm -> "+"]],
      (*      concatOp = pbw[TagBox[style["\[RoundSpaceIndicator\]"], Identity, noEdit, SyntaxForm -> "*"]],*)
      (*       concatOp = pbw[TagBox[style["\[RoundSpaceIndicator]"], Identity, noEdit, SyntaxForm -> "\[InvisibleTimes]"]],*)
      concatOp = pbw[TagBox["\[InvisibleSpace]", "REConcatOp", noEdit, SyntaxForm -> "*"]],
      tradConcatOp = pbw[TagBox["\[InvisibleSpace]", "REConcatOp", SyntaxForm -> "*"]],
      (*       closeOp = TagBox[SuperscriptBox["\[InvisiblePostfixScriptBase]", style["*"]], Identity, noEdit, SyntaxForm -> "\[HermitianConjugate]"],*)
      closeOp = TagBox[SuperscriptBox["\[InvisiblePostfixScriptBase]", "*"], "REClosureOp", noEdit, SyntaxForm -> "\[HermitianConjugate]"],
      tradCloseOp = TagBox[SuperscriptBox["\[InvisiblePostfixScriptBase]", "*"], "REClosureOp", SyntaxForm -> "\[HermitianConjugate]"],
      esSym = pbw[TagBox[style["\[CurlyEpsilon]"], RegularLanguages`Epsilon, noEdit]],
      tradEsSym = pbw[TagBox["\[CurlyEpsilon]", RegularLanguages`Epsilon]],
      nullSym = pbw[TagBox[style["\[DoubleStruckZero]"], RegularLanguages`RENull, noEdit]],
      tradNullSym = pbw[TagBox["\[EmptySet]", RegularLanguages`RENull]]},
      infix[unionOp, RegularLanguages`REUnion];
      infix[tradUnionOp, RegularLanguages`REUnion, wft];
      infix[concatOp, RegularLanguages`REConcat];
      infix[tradConcatOp, RegularLanguages`REConcat, wft];
      note[lr[pbw[RowBox[{"a_", closeOp}]], pbw[RowBox[{"RegularLanguages`REClosure", "[", "a_", "]"}]]]];
      note[lr[pbw[RowBox[{"a_", closeOp}]], pbw[RowBox[{"RegularLanguages`REClosure", "[", "a_", "]"}]]], wft];
      note[lr[esSym, pbw["Epsilon"]]];
      note[lr[tradEsSym, pbw["Epsilon"]], wft];
      note[lr[nullSym, pbw["EmptyLanguage"]]];
      note[lr[tradNullSym, pbw["EmptyLanguage"]], wft];
      note[lr[ pbw[OverscriptBox["a_", "\[LeftArrow]"]], pbw[RowBox[{"RegularLanguages`FAReversal", "[", "a_", "]"}]]]];
(*      Quiet@alias[",." -> concatOp];*)
(*      Quiet@alias[",|" -> unionOp];*)
(*      Quiet@alias[",*" -> pbw[closeOp]];*)
(*      Quiet@alias[",e" -> esSym];*)
(*      Quiet@alias[",0" -> nullSym];*)
(*      Quiet@alias[",ar" -> pbw[OverscriptBox["\[SelectionPlaceholder]", "\[LeftArrow]"]]];*)
    ]]]

