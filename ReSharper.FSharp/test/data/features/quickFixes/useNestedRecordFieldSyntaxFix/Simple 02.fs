type Record1 = { Foo: int; Bar: int }
type Record2 = { Foo: Record1; Bar: Record1 }

let item: Record2 = null

{ item with Foo ={caret} { item.Foo with Foo = 3 }; Bar.Foo = 3 }
