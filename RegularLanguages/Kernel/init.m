(* Mathematica Init file *)

With[{names = Table[ToString[c] <> "*", {c, Contexts["RegularLanguages`*"]}],
  unprotectAndUnset = Function[{symb},
    If[ MemberQ[Attributes@symb, Protected],
      Unprotect[symb];
      symb =.
    ], HoldRest ] },
  If[Length@names > 0,
    unprotectAndUnset[Global`\[CurlyEpsilon]];
    unprotectAndUnset[Global`\[EmptySet]];
    unprotectAndUnset[VerticalSeparator];
    unprotectAndUnset[CenterDot];
    unprotectAndUnset[SuperStar];
    Unprotect @@ names;
    Remove @@ names;
  ]
];
(*Get[FileNameJoin[{ParentDirectory@DirectoryName@$InputFileName, "Utils.m"}]];*)

Get[FileNameJoin[{ParentDirectory@DirectoryName@$InputFileName, "Utils.m"}]];


