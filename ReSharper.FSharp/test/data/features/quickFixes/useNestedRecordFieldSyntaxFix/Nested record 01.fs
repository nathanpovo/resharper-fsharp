type Record0 = { Foo: int; Bar: int }
type Record1 = { Foo: int; Bar: int; Zoo: Record0 }
type Record2 = { Foo: Record1; Bar: Record1 }

let item: Record2 = null

{ item with Bar = { item.Foo with Zoo ={caret} { item.Foo.Zoo with Bar = 3 } } }
