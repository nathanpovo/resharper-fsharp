type Record0 = { Foo: int; Bar: int }
type Record1 = { Foo: int; Bar: int; Zoo: Record0 }

[<AutoOpen>]
module Module =
    type Record2 = { Foo: Record1; Bar: Record1 }
    let item: Record2 = null

{ (Module.item) with Foo ={caret} ({ Module.item.Foo with Foo = 3 }) }
