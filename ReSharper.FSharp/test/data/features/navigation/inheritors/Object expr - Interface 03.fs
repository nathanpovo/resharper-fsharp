type I2<'KeyT> =
    abstract M{on}: 'KeyT -> int

{ new I2<obj> with member _.M _ = 0 } |> ignore
