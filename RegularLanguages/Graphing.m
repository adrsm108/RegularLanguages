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

$epsilonGray = GrayLevel[0.5`, 0.7`];

PackageExport["ToTaggedEdges"]
ToTaggedEdges::usage = "\
ToTaggedEdges[A$] converts A$ to a list with one edge DirectedEdge[q$1, q$2, a$] for each transition \
from state q$1 to state q$2 on symbol a$ in A$.";
ToTaggedEdges[A_?DFAQ] :=
  Replace[ToRules @ A,
    (s_ -> ts_) :> Splice[DirectedEdge[s, #2, #1]& @@@ ts],
    {1}
  ];
ToTaggedEdges[A_?NFAQ] :=
  Replace[ToRules @ A,
    (s_ -> rs_) :> Splice[
      Replace[rs,
        (a_ -> ts_) :> Splice[
          DirectedEdge[s, #, a]& /@ ts
        ],
        {1}
      ]
    ],
    {1}
  ];

PackageExport["FAGraph"]
FAGraph::usage = "\
FAGraph[A$, opts$$] produces a Graph of the automaton A$ with options opts$.
* opts$ can contain any option recognized by Graph.
* This is equivalent to Graph[A$, opts$] when A$ satisfies FAExpressionQ.

Options:
'DisableStyling' -> True | False
When True, the returned graph has no styling applied.

'InitialVertexStyle' -> _
Styles applied to initial state vertices

'InitialVertexRules' -> _
List of annotation rules applied to initial state vertices
* Overrides 'InitialVertexStyle' option in favor of VertexStyle rule.

'TerminalVertexStyle' -> _
Styles applied to terminal state vertices

'TerminalVertexRules' -> _
List of annotation rules applied to terminal state vertices
* Overrides 'TerminalVertexStyle' option in favor of VertexStyle rule.

'EpsilonEdgeStyle' -> _
Styles applied to edges representing Epsilon-transitions

'InitialArrows' -> True | False | Automatic
Whether initial states should be drawn with an incoming arrow

'InitialArrowStyle' -> _
Styles applied to incoming arrows on initial states
";
Options[FAGraph] =
  {
    "DisableStyling" -> False,
    "InitialVertexRules" -> Automatic,
    "TerminalVertexRules" -> Automatic,
    "InitialVertexStyle" -> Automatic,
    "TerminalVertexStyle" -> Automatic,
    "EpsilonEdgeStyle" -> Automatic,
    "InitialArrows" -> Automatic,
    "InitialArrowStyle" -> Automatic,
    VertexLabels -> Automatic,
    EdgeLabels -> Automatic
  };
OptionChecks[FAGraph] =
  {
    "InitialArrows" -> True | False | Automatic,
    "DisableStyling" -> True | False
  };
FAGraph[A_?FAGraphQ, opts___] := FAGraph[FAExpression @ A, opts];
FAGraph[A_?FAExpressionQ, opts : CheckedOptions[{FAGraph, Graph}, FAGraph]] :=
  With[
    {
      n = StateCount @ A,
      inits = States[A, "Initial"],
      verts = stateIndices @ A,
      edges =
        Catenate @ KeyValueMap[
          With[{type = FAType @ A},
            toEdges[type, ##]&
          ],
          Transitions @ A
        ],
      isDFA = MatchQ[FAType @ A, DFA],
      perfGoal =
        UnlessAutomatic[OptionValue @ PerformanceGoal, "Quality"],
      initEdgeRules =
        {
          EdgeStyle -> UnlessAutomatic[OptionValue @ "InitialArrowStyle", $epsilonGray],
          EdgeLabels -> None
        },
      epsilonEdgeStyle =
        UnlessAutomatic[OptionValue @ "EpsilonEdgeStyle", $epsilonGray]
    },
    {
      showInitArrows =
        UnlessAutomatic[OptionValue @ "InitialArrows", Length @ edges <= 150]
    },
    Annotate[
      If[OptionValue @ "DisableStyling",
        Graph[ Keys @ verts, edges ],
        Graph[
          Keys @ verts,
          If[showInitArrows,
            Join[edges, DirectedEdge[#, #, "Start"] & /@ inits],
            edges
          ],
          filteredOptionSequence[{opts}, Graph, FAGraph],
          PerformanceGoal -> perfGoal,
          GraphLayout ->
            getGraphProperty[A, GraphLayout, Automatic],
          VertexStyle ->
            If[isDFA,
              Directive[
                RGBColor[0.475184`, 0.592896`, 0.919488`],
                EdgeForm @ RGBColor[0.275184`, 0.392896`, 0.719488`]
              ],
              Directive[
                RGBColor[0.970000`, 0.606000`, 0.081000`],
                EdgeForm @ RGBColor[0.431111`, 0.269333`, 0.0360000`]
              ]
            ],
          VertexSize ->
            getGraphProperty[
              A,
              VertexSize,
              Which[
                n <= 1, 1,
                n <= 5, 0.3`,
                n <= 10, 0.4`,
                n <= 20, 0.5`,
                n <= 30, 0.6`,
                True, 0.7`
              ]
            ],
          VertexLabels ->
            Replace[
              OptionValue @ VertexLabels,
              {
                Automatic ->
                  Placed[
                    "Name",
                    If[StateCount @ A <= 25, Center, Tooltip],
                    vertexLabelFunction
                  ],
                Placed[Automatic, pos_, f_ : vertexLabelFunction] :>
                  Placed[
                    "Name",
                    pos,
                    vertexLabelFunction
                  ]
              }
            ],
          VertexLabelStyle ->
            {
              Directive[AutoSpacing -> False]
            },
          EdgeStyle ->
            UnlessAutomatic[OptionValue @ EdgeStyle,
              Directive[GrayLevel[0, 0.7`], Arrowheads @ Small]
            ],
          EdgeLabels ->
            Replace[
              OptionValue @ EdgeLabels,
              {
                Automatic ->
                  Placed[
                    "EdgeTag",
                    {0.3, {.5, .5}},
                    edgeLabelFunction @ OptionValue @ "Background"
                  ],
                Placed[
                  Automatic,
                  pos_,
                  f : edgeLabelFunction @ OptionValue @ "Background"
                ] :>
                  Placed["EdgeTag", pos, f]
              }
            ],
          AnnotationRules ->
            {
              toAlternatives @ inits ->
                UnlessAutomatic[OptionValue @ "InitialVertexRules",
                  {
                    VertexStyle ->
                      UnlessAutomatic[OptionValue @ "InitialVertexStyle",
                        If[isDFA,
                          Directive[
                            RGBColor[1`, 0.5`, 0.5`],
                            EdgeForm @ RGBColor[0.666666`, 0.333333`, 0.333333`]
                          ],
                          Directive[
                            RGBColor[0.181864`, 0.666640`, 0.869788`],
                            EdgeForm @ RGBColor[0.045480`, 0.314800`, 0.427660`]
                          ]
                        ]
                      ]
                  }
                ] ,
              toAlternatives @ States[A, "Terminal"] ->
                UnlessAutomatic[OptionValue@"TerminalVertexRules",
                  {
                    VertexShapeFunction -> doubleCircle[0.3],
                    VertexStyle -> UnlessAutomatic[OptionValue @ "TerminalVertexStyle", {}]
                  }
                ],
              If[isDFA,
                Nothing,
                DirectedEdge[_, _, {Epsilon}] -> {
                  EdgeStyle -> epsilonEdgeStyle,
                  EdgeLabels ->
                    Placed[
                      "EdgeTag",
                      If[Length @ edges <= 150, {0.3, {.5, .5}}, Tooltip],
                      edgeLabelFunction[OptionValue @ "Background", epsilonEdgeStyle]
                    ]
                }
              ],
              If[! showInitArrows,
                Nothing,
                If[perfGoal === "Quality",
                  If[
                    Quiet @ Lookup[
                      OptionValue @ GraphLayout,
                      "Dimension"
                    ] === 3,
                    DirectedEdge[x_, _, "Start"] ->
                      {
                        EdgeStyle ->
                          {
                            Arrowheads @ {
                              {
                                -1,
                                Automatic,
                                {Graphics[], 0.5}
                              },
                              Small
                            },
                            $epsilonGray
                          },
                        Splice @ initEdgeRules
                      },
                    DirectedEdge[x_, _, "Start"] :>
                      {
                        Splice @ initEdgeRules,
                        EdgeShapeFunction ->
                          (
                            Arrow @ {
                              getStartPoint @ #1,
                              UnlessMatch[First @ #1,
                                {_, _},
                                DynamicLocation[
                                  "VertexID$" <> ToString @ verts @ x,
                                  Automatic,
                                  Automatic
                                ]
                              ]
                            } &
                          )
                      }
                  ],
                  DirectedEdge[_, _, "Start"] ->
                    {
                      Splice @ initEdgeRules,
                      EdgeShapeFunction ->
                        (
                          Arrow @ {
                            getStartPoint @ #1,
                            First @ #1
                          } &
                        )
                    }
                ]
              ]
            }
        ]
      ],
      "Automaton" -> A
    ]
  ];

PackageExport["FAGraph3D"]
FAGraph3D::usage = "\
FAGraph3D[A$, opts$$] produces a Graph3D of the automaton A$ with options opts$.
* opts$ can contain any option recognized by Graph3D.
* This is equivalent to Graph3D[A$, opts$] when A$ satisfies FAExpressionQ.
FAGraph3D takes the same options as FAGraph.";
Options[FAGraph3D] =
  {
    "InitialVertexRules" -> Automatic,
    "TerminalVertexRules" -> Automatic,
    "InitialVertexStyle" -> Automatic,
    "TerminalVertexStyle" -> Automatic,
    "EpsilonEdgeStyle" -> Automatic,
    "InitialArrows" -> False,
    "InitialArrowStyle" -> Automatic,
    VertexLabels -> Automatic,
    EdgeLabels -> Automatic
  };
OptionChecks[FAGraph3D] = {"InitialArrows" -> True | False | Automatic};
FAGraph3D[A_?FAQ, opts : OptionsPattern[{FAGraph3D, Graph3D}]] :=
  Graph3D @ FAGraph[
    A,
    EdgeStyle -> UnlessAutomatic[OptionValue @ EdgeStyle, GrayLevel[0, 0.7`]],
    "EpsilonEdgeStyle" -> UnlessAutomatic[OptionValue @ "EpsilonEdgeStyle", $epsilonGray],
    "InitialArrows" -> OptionValue @ "InitialArrows",
    "InitialArrowStyle" -> UnlessAutomatic[OptionValue @ "InitialArrowStyle", $epsilonGray],
    opts
  ];

getStartPoint[pts_, s_ : 0.66] :=
  With[
    {
      a = pts[[1]],
      b = pts[[Ceiling[Length @ pts / 2]]]
    },
    a + (b - a) * s
  ];

PackageScope["makeThumbnail"]
makeThumbnail::usage = "makeThumbnail[A] makes a thumbnail graph of the automaton A.";
makeThumbnail[A_?FAQ] :=
  If[StateCount @ A <= 15, (* maximum of 15 states *)
    makeThumbnail[A] =
      Deploy @ With[{graph = quickGraph @ FAExpression @ A},
        Show[graph,
          PlotRange -> squareRange[AbsoluteOption[graph, PlotRange], "Max"],
          PlotRangePadding -> Scaled[.05],
          PlotRangeClipping -> True,
          Frame -> True,
          Background -> RGBColor[0.93`, 0.93`, 0.93`],
          ImageSize -> Dynamic@{Automatic,
            3.5` CurrentValue["FontCapHeight"] /
              AbsoluteCurrentValue @ Magnification}]],
    Switch[
      FAType @ A,
      DFA, $defaultDFAIcon,
      NFA, $defaultNFAIcon
    ]
  ];

PackageScope["setGraphProperties"]
setGraphProperties::usage = "setGraphProperties[A, prop -> val, ...] sets GraphProperties for A used by FAGraph";
setGraphProperties[(head : NFA | DFA)[asc_], rules__] :=
  head @ Association[
    asc,
    "GraphProperties" ->
      Association[
        Lookup[asc, "GraphProperties", <||>],
        rules
      ]
  ];

PackageScope["getGraphProperty"]
SetAttributes[getGraphProperty, HoldRest];
getGraphProperty[(head : NFA | DFA)[asc_], prop_, default_] :=
  If[KeyExistsQ[asc, "GraphProperties"],
    Lookup[
      asc @ "GraphProperties",
      prop,
      default
    ],
    default
  ];

recomputeThumbnail[A_?FAQ] := (makeThumbnail[A] =. ; A);

squareRange[{{x0_, x1_}, {y0_, y1_}}, opt_ : "Max"] :=
  With[{dx = Abs[x1 - x0], dy = Abs[y1 - y0]},
    Switch[opt,
      "Max",
      If[dx >= dy,
        {{x0, x1}, ({-dx, dx} + (y0 + y1)) / 2},
        {({-dy, dy} + (x0 + x1)) / 2, {y0, y1}}
      ],
      "Min",
      If[dx >= dy,
        {({-dy, dy} + (x0 + x1)) / 2, {y0, y1}},
        {{x0, x1}, ({-dx, dx} + (y0 + y1)) / 2}
      ],
      {"Clamped", _},
      With[{d = opt[[2]]},
        {({-d, d} + (x0 + x1)) / 2, ({-d, d} + (y0 + y1)) / 2}
      ],
      {"Centered", __},
      With[
        {
          c = opt[[2]],
          d = If[Length @ opt >= 3, Min[dx, dy, opt[[3]]], Min[dx, dy]] / 2
        },
        ConstantArray[{-d, d}, 2] + c]
    ]
  ];

doubleCircle[{x_, y_}, v_, {w_, h_}, d_ : 0.03] :=
  {
    {
      FaceForm[Opacity[0.5]],
      Disk[{x, y}, {w, h}]
    },
    Annulus[{x, y}, {w * (1 - d), w}]
  };
doubleCircle[{x_, y_, z_}, v_, {w_, h_, b_}, d_ : 0.1] :=
  {
    Ellipsoid[{x, y, z}, (1 - d) * {w, h, b}],
    {
      Opacity[0.5],
      Ellipsoid[{x, y, z}, {w, h, b}]
    }
  };

doubleCircle[d_] := doubleCircle[##, d] &;

edgeLabelFunction[background_, style_ : Nothing] :=
  Framed[
    Replace[#,
      {
        x_List :> Row[x, ","],
        Style[x_List, rest___] :> Style[Row[x, ","], rest]
      }
    ],
    BaseStyle -> {
      style,
      AutoSpacing -> False
    },
    FrameStyle -> None,
    Background -> UnlessMatch[background, None, White],
    FrameMargins -> 2,
    ContentPadding -> False,
    RoundingRadius -> Scaled[0.5]
  ] &;

vertexLabelFunction =
  Which[ (* lists of lists are common, we don't want sporadic matrix formatting *)
    MatrixQ[#], Style /@ #,
    MatchQ[#, _Style] && MatrixQ @ First[#], MapAt[Map @ Style, #, 1],
    True, #
  ] &;


toEdges[DFA, q_, trns_] :=
  KeyValueMap[
    DirectedEdge[q, #1, #2]&,
    GroupBy[trns, Identity, Keys]
  ];
toEdges[NFA, q_, trns_] :=
  Catenate[
    KeyValueMap[DirectedEdge[q, ##] &,
      Merge[
        Catenate[
          Thread[Reverse@#, List, 1]& /@
            Normal @ #
        ],
        Identity]
    ]& /@
      {
        KeyDrop[trns, Epsilon],
        KeyTake[trns, Epsilon]
      }
  ];

quickGraph[nfa_NFA, opts : OptionsPattern[Graph]] :=
  With[{n = StateCount @ nfa},
    Graph[States @ nfa,
      (*for each state, convert \[Delta] to list of distinct untagged edges, removing loops*)
      Catenate[
        KeyValueMap[
          Function[{q, trs},
            DirectedEdge[q, #]& /@
              DeleteCases[
                Union @@ trs,
                q
              ]
          ],
          Transitions @ nfa
        ]
      ],
      opts,
      PlotTheme -> "Minimal",
      BaseStyle -> {EdgeForm[]},
      VertexStyle -> {RGBColor[0.97`, 0.606`, 0.081`],
        toAlternatives @ States[nfa, "Initial"] ->
          RGBColor[0.181864`, 0.66664`, 0.869788`]},
      VertexSize ->
        If[n == 1,
          1,
          {"Scaled", 1 / (2 n^0.8)}
        ],
      EdgeStyle -> Directive[Thickness[0.02], GrayLevel[0.3, 1 / n^0.5]],
      EdgeShapeFunction -> "Line",
      PerformanceGoal -> "Speed",
      AnnotationRules ->
        With[{gprops = nfa[[1]]["Annotations", "GraphProperties"]},
          If[MissingQ @ gprops,
            {},
            {"GraphProperties" -> gprops}
          ]
        ]
    ]
  ];

quickGraph[dfa_DFA, opts : OptionsPattern[Graph]] :=
  With[{n = StateCount[dfa]},
    Graph[
      States @ dfa,
      (*for each state, convert to list of distinct untagged edges, removing loops*)
      Catenate[
        KeyValueMap[
          Table[
            DirectedEdge[#1, t],
            {t, DeleteDuplicates @ DeleteCases[#2, #1]}
          ]&,
          Transitions @ dfa
        ]
      ],
      opts,
      GraphLayout -> UnlessMatch[
        dfa[[1]]["Annotations", GraphLayout],
        _Missing,
        Automatic
      ],
      PlotTheme -> "Minimal",
      BaseStyle -> {EdgeForm[]},
      VertexStyle ->
        {
          RGBColor[0.475184`, 0.592896`, 0.919488`],
          toAlternatives @ States[dfa, "Initial"] -> RGBColor[1, 0.5, 0.5]
        },
      VertexSize -> If[n == 1, 1, {"Scaled", 1 / (2 n^0.8)}],
      EdgeStyle -> Directive[Thickness[0.02], GrayLevel[0.3`, 1 / n^0.5]],
      EdgeShapeFunction -> "Line",
      PerformanceGoal -> "Speed",
      AnnotationRules ->
        With[{gprops = dfa[[1]]["Annotations", "GraphProperties"]},
          If[MissingQ @ gprops,
            {},
            {"GraphProperties" -> gprops}
          ]
        ]
    ]
  ];


graphRegexArray[arr_] :=
  Graph[
    DirectedEdge[##, arr[[##]]] & @@@ arr["NonzeroPositions"],
    VertexLabels -> "Name",
    EdgeLabels -> Placed["EdgeTag", Tooltip]
  ];

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