type I<'T> =
    abstract P{on}: 'T

{ new I<int> with
      member x.P = 1 }
