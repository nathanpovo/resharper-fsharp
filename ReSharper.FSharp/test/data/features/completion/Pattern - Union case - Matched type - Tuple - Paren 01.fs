// ${COMPLETE_ITEM:U.B}
module Module

[<RequireQualifiedAccess>]
type U =
    | A
    | B of int

match (1, U.A) with
| _, {caret}
