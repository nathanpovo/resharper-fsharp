// ${COMPLETE_ITEM:U.B}
module Module

module Nested =
    [<RequireQualifiedAccess>]
    type U =
        | A
        | B of int

match Nested.U.A with
| {caret}
