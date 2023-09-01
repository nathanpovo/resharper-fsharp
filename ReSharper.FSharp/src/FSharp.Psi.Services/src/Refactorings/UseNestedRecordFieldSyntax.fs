namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Refactorings

open FSharp.Compiler.Symbols
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree

module UseNestedRecordFieldSyntax =
   let getFieldPath (reference: IReferenceName) =
       reference.GetSuffix(fun ref -> ref.Reference.HasFcsSymbol && ref.Reference.GetFcsSymbol() :? FSharpField)
