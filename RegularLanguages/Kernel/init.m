(* Mathematica Init file *)

(*
With[{
  useNotation = TrueQ @ RegularLanguages`UseNotation,
  names = Table[ToString[c] <> "*", {c, Contexts["RegularLanguages`*"]}],
  unprotectAndUnset = Function[{symb},
    If[ MemberQ[Attributes @ symb, Protected],
      Unprotect[symb];
      symb =.
    ], HoldRest ] },
  If[Length @ names > 0,
    unprotectAndUnset[Global`\[CurlyEpsilon]];
    unprotectAndUnset[Global`\[EmptySet]];
    unprotectAndUnset[VerticalSeparator];
    unprotectAndUnset[CenterDot];
    unprotectAndUnset[SuperStar];
    Unprotect @@ names;
    Remove @@ names;
  ];
];
*)

Get[FileNameJoin[{ParentDirectory @ DirectoryName@$InputFileName, "Utils.m"}]];

If[UseNotation =!= False, LoadNotation[]];

Needs["GeneralUtilities`"];
Scan[
  Function[name,
    If[GeneralUtilities`HasUsageStringQ[name],
      Function[x,
        x::usage = GeneralUtilities`SetUsage[x, x::usage], HoldAll
      ] @@ MakeExpression[name, StandardForm];
    ];
    SetAttributes[name, {Protected, ReadProtected}];
  ],
  Names["RegularLanguages`*"]
];

ClearAttributes[UseNotation, {Protected, ReadProtected}];

