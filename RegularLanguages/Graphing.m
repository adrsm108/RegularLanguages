(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: Graphing *)
(* :Context: Graphing` *)
(* :Author: Adam Smith *)
(* :Date: 2020-06-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2020 Adam Smith *)
(* :Keywords: *)
(* :Discussion: *)

Package["RegularLanguages`"]

PackageScope["automatonGraph"]
automatonGraph::usage = "automatonGraph[A] returns a graph representation of the automaton A.";
Options[automatonGraph] = {"EdgeTagWrapper" -> Identity};
automatonGraph[A_, opts : OptionsPattern[{Graph, automatonGraph}]] :=
  With[{n = StateCount[A], edges = Catenate[toEdges /@ States[A, "Values"]],
    elabels = OptionValue[EdgeLabels], vlabels = OptionValue[VertexLabels],
    isdfa = MatchQ[FAType[A], DFA]},
    Graph[ IDs@A, edges,
      filterOpts[
        FilterRules[{opts}, Except[
          Pick[{VertexLabels, EdgeLabels},
            MatchQ["Automaton"] /@ {vlabels, elabels}]]],
        Graph],
      VertexStyle -> If[isdfa,
        Directive[RGBColor[0.47518400000000005`, 0.592896, 0.9194880000000001],
          EdgeForm[RGBColor[0.275184, 0.392896, 0.719488]]],
        Directive[RGBColor[0.97, 0.606, 0.081],
          EdgeForm[RGBColor[0.43111111111111117`, 0.26933333333333337`, 0.036000000000000004`]]]],
      VertexSize -> Which[
        n <= 1, 1,
        n <= 10, 0.4,
        n <= 20, 0.5,
        n <= 30, 0.6,
        True, 0.7],
      VertexLabels -> Placed[Automatic,
        If[vlabels === "Automaton" || n <= 25,
          Center, Tooltip]],
      VertexLabelStyle -> {Directive[AutoSpacing -> False]},
      EdgeStyle -> Directive[GrayLevel[0, 0.7], Arrowheads[Small]],
      EdgeLabels -> Placed["EdgeTag",
        If[elabels === "Automaton" || Length@edges <= 150, {0.3, {.5, .5}}, Tooltip],
        automatonEdgeLabel[OptionValue["EdgeTagWrapper"], isdfa]],
      AnnotationRules -> {
        toAlternatives@IDs[A, "Initial"] -> {
          VertexStyle -> If[isdfa,
            Directive[RGBColor[1, 0.5, 0.5],
              EdgeForm[RGBColor[0.6666666666666666`, 0.33333333333333337`, 0.33333333333333337`]]],
            Directive[RGBColor[0.181864, 0.66664, 0.869788],
              EdgeForm[RGBColor[0.04548, 0.3148, 0.42766]]]]},
        toAlternatives@IDs[A, "Terminal"] -> {VertexShapeFunction -> doubleCircle[0.2]},
        If[isdfa, Nothing,
          DirectedEdge[_, _, {Epsilon}] -> {EdgeStyle -> GrayLevel[0.5, 0.7]}] },
      PerformanceGoal -> "Quality"]];

PackageScope["makeThumbnail"]
makeThumbnail::usage = "makeThumbnail[A] makes a thumbnail graph of the automaton A.";
makeThumbnail[A_?FAQ] := If[StateCount[A] <= 15, (* maximum of 15 states *)
  makeThumbnail[A] =
    Deploy@With[{graph = quickGraph[FAExpression[A]]},
      Show[graph,
        PlotRange -> squareRange[absOpt[graph, PlotRange], "Max"],
        PlotRangePadding -> Scaled[.05],
        PlotRangeClipping -> True,
        Frame -> True,
        Background -> RGBColor[0.93, 0.93, 0.93],
        ImageSize -> Dynamic@{Automatic,
          3.5` CurrentValue["FontCapHeight"] /
            AbsoluteCurrentValue[Magnification]}]],
  Switch[FAType[A], DFA, $defaultDFAIcon, NFA, $defaultNFAIcon]
];

recomputeThumbnail[A_?FAQ] := (makeThumbnail[A] =. ; A);

squareRange[{{x0_, x1_}, {y0_, y1_}}, opt_ : "Max"] :=
  With[{dx = Abs[x1 - x0], dy = Abs[y1 - y0]},
    Switch[opt,
      "Max", If[dx >= dy,
      {{x0, x1}, ({-dx, dx} + (y0 + y1)) / 2},
      {({-dy, dy} + (x0 + x1)) / 2, {y0, y1}}],
      "Min", If[dx >= dy,
      {({-dy, dy} + (x0 + x1)) / 2, {y0, y1}},
      {{x0, x1}, ({-dx, dx} + (y0 + y1)) / 2}],
      {"Clamped", _}, With[{d = opt[[2]]},
      {({-d, d} + (x0 + x1)) / 2, ({-d, d} + (y0 + y1)) / 2}],
      {"Centered", __},
      With[{c = opt[[2]],
        d = If[Length@opt >= 3, Min[dx, dy, opt[[3]]], Min[dx, dy]] / 2},
        ConstantArray[{-d, d}, 2] + c]
    ]];

doubleCircle[{x_, y_}, v_, {w_, h_}, d_ : 0.05] :=
  Dynamic@With[{fc = CurrentValue["Color"]},
    {{FaceForm[Darker@fc], Disk[{x, y}, {w, h}]},
      {EdgeForm[], Disk[{x, y}, (1 - d) * {w, h}]}}];
doubleCircle[{x_, y_, z_}, v_, {w_, h_, b_}, d_ : 0.05] :=
  Dynamic@With[{fc = CurrentValue["Color"]},
    {{Opacity[1], FaceForm[Darker@fc],
      Ellipsoid[{x, y, z}, (1 - d) * {w, h, b}]},
      {Opacity[0.5], Ellipsoid[{x, y, z}, {w, h, b}]}}];
doubleCircle[d_] := doubleCircle[##, d] &;

automatonEdgeLabel[wrapper_, isdfa_ : True][elabel_] :=
  Framed[Row[wrapper /@ elabel, ","],
    BaseStyle -> {FontFamily -> "Sans", AutoSpacing -> False,
      If[isdfa || FreeQ[elabel, Epsilon], Nothing,
        GrayLevel[0.5, 0.7]]},
    FrameStyle -> None, Background -> White, FrameMargins -> 2,
    ContentPadding -> False, RoundingRadius -> Scaled[0.5]];

toEdges[DFAState[id_, d_, ___]] := KeyValueMap[
  DirectedEdge[id, #1, #2] &, GroupBy[d, Identity, Keys]];
toEdges[NFAState[id_, d_, ___]] := Catenate[KeyValueMap[
  DirectedEdge[id, ##] &,
  Merge[Catenate[Thread[Reverse@#, List, 1] & /@ Normal@#], Identity]] &
  /@ Through[{KeyDrop, KeyTake}[d, Epsilon]]];

quickGraph[nfa_NFA, opts : OptionsPattern[Graph]] :=
  With[{n = StateCount[nfa]},
    Graph[IDs@nfa,
      (*for each state, convert \[Delta] to list of distinct untagged edges, removing loops*)
      Catenate[Function[{s},
        DirectedEdge[StateID@s, #] &
          /@ DeleteCases[
          Join[Union @@ Transitions[s, DeleteCases[Keys@s, Epsilon]],
            s[Epsilon]],
          StateID@s]]
        /@ States[nfa, "Values"]],
      opts,
      PlotTheme -> "Minimal",
      BaseStyle -> {EdgeForm[]},
      VertexStyle -> {RGBColor[0.97, 0.606, 0.081],
        toAlternatives@IDs[nfa, "Initial"] -> RGBColor[
          0.181864, 0.66664, 0.869788]},
      VertexSize -> If[n == 1, 1, {"Scaled", 1 / (2 n^0.8)}],
      EdgeStyle -> Directive[Thickness[0.02], GrayLevel[0.3, 1 / n^0.5]],
      EdgeShapeFunction -> "Line",
      PerformanceGoal -> "Speed"]];

quickGraph[dfa_DFA, opts : OptionsPattern[Graph]] :=
  With[{n = StateCount[dfa]},
    Graph[ IDs@dfa,
      (*for each state, convert \[Delta] to list of distinct untagged edges, removing loops*)
      Catenate[
        Function[{s}, DirectedEdge[StateID@s, #]&
          /@ DeleteDuplicates@DeleteCases[Values@s, StateID[s]]]
          /@ States[dfa, "Values"]],
      opts,
      PlotTheme -> "Minimal",
      BaseStyle -> {EdgeForm[]},
      VertexStyle -> {RGBColor[
        0.47518400000000005`, 0.592896, 0.9194880000000001],
        toAlternatives@IDs[dfa, "Initial"] -> RGBColor[1, 0.5, 0.5]},
      VertexSize -> If[n == 1, 1, {"Scaled", 1 / (2 n^0.8)}],
      EdgeStyle -> Directive[Thickness[0.02], GrayLevel[0.3, 1 / n^0.5]],
      EdgeShapeFunction -> "Line",
      PerformanceGoal -> "Speed"]];

graphRegexArray[arr_] :=
  Graph[DirectedEdge[##, arr[[##]]] & @@@ arr["NonzeroPositions"],
    VertexLabels -> "Name", EdgeLabels -> Placed["EdgeTag", Tooltip]];

{$defaultDFAIcon, $defaultNFAIcon} = Uncompress[
  "1:eJztVntMW1UcrrxK68hE0WQLusxlExIzKZRX4nJ4DSjCBqXOSOb00h7aOy693Wkr6\
  /5YMFOnzqBxEmXxlYjRCVEXXUYihowJk8BQeciIi2MbBCYwB8pYzRjec2/p85zijE\
  2MepKe3nO+3+/7Pe5pz7e+ktdWhclkMmuEMBWzVlvVbXgVJUx50MLxjqrb8TpamAo\
  QYzGxeqtkv9prJ5evsXBwL8SAUvjgb9Wlh9dODTjBRyblI0MHZFk/9/fV72m/CTYN\
  XY8b2DoPytfdvdjywCyQzxa0957+Fbz7Vrbiyykn+CRx8vHdGyeAzDVK1pkaimaug\
  vaxh85Ziufd+/MPmvMSD44CvWnu29ZZJ3ipcdtIw/wYmPj8And07iJ4daIu6vD678\
  H18J7sN0s7wPMd36zVhC+BpMRPD+oGW4Bf1RGBPVBIFTqK4dOQQyniGAASJhem7RZ\
  Gz9ocaLWmsPKuLYMuBHdqq8EI83lUIxJu481QgiR6YbXGJ5hPIhpsqRLnZCqSQkCS\
  JZyK0H3UBCRFnFOpSBoBSafGkZAMApJBrTSDmkGqxElFMglIJjW3TGpumdRK06hZp\
  1EzSNOIv4okP0jaJPJ5Qf7peUGkt6emVqum9k7tqjbG52yG4xWeCu0QTZe13vfc1C\
  WATs0n7HylQ3g404vHNOHo/3VPz4/GnWQea61msSH67sQbJ+WXe4Afhle+Nbkt0Wi\
  Ai6cyHFdbkJPLczySAsjEMQrcD36RsBvqIWchD4JFB8Eig2CKIFhUEEwZBIsgY1o7\
  B63YL4fRVxsRbzcbArvkeoMzIPDBj0khMllhuc3B+fwBCs2XBRyXwCzwNmPTOSxQv\
  Gt0iDGwNpY3MxzBAbcwHzE1UNzSITskMWIDHauvthLOmGi1SnjQ1DBGWMoYDKzZKN\
  5oy5/lE0H7JtQvcpWz+6Dn1shzmJkaVu97GkXjbLuNFypexnBJOlaoyHUUV2X57Uu\
  cOOVcO0LQbNvBcHZYHiP2zmzLZSyFkDWabB63Ur4WIsktDgestPKc3Qa93a3YvYQx\
  mtkqVs/gfrNLwiDUVsrxNi0j9MinSdUj+zrXVPSDSEYRr8peAnWKCzfUMTmgJ7+ob\
  Lrld/8mxXoT5XKsxSJ0nfYO7/A2dr0hv875ixfl8kG7NfFy5dn7lVl55wE61Nw8O7\
  4IBiKeABurJgE07Ak7MDkGVE0fdDW0zgH5rvi9ju6rYK46/eN7Yi6DivoX3jnZfQ1\
  oozbI9SeugbbN6ZqY2iW3aGkrLtK+1/kbqKz/sDvpsZvgqez+K8fLpkD3HN84dt7p\
  tivaeawu9TNZ1jDsdMbFOsHLx0Z2oI6vwf7mrl7zT2f/deJFRb3KVFQZsBJClyik3\
  OgSZSVZQ8ognSoD6EgyNQO6TEsmiwqPE6kJGdSCUuh0KwlCUkV0qaimB1JTA6mpgf\
  7J2oUsNpBy+H3LFuUvAKmiW+IrDALrhh+d+3e3feF/WYuqJPbOU9t3xZ1ZUcP8eVr\
  5LdE+OZPQV1ZwGqCF17pM9z46DlDYM31NJUNTJCEWgmyjQ0MbHhraiNDQKkNDGxka\
  WkVoaKPItP9r1/+o3hx8/fDmytp+kDredOjs2zdAwosLR84N/wCKLn61aeH44t+uN\
  z0Ks1zPcNCAjjTicRT8Ac8WyVA="
];
Protect[$defaultDFAIcon, $defaultNFAIcon];
