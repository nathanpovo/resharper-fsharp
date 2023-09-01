namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Analyzers

open System.Collections.Generic
open System.Linq
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Analyzers
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Refactorings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl
open JetBrains.ReSharper.Plugins.FSharp.Psi.PsiUtil
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree

[<ElementProblemAnalyzer([| typeof<IRecordExpr> |], HighlightingTypes = [| typeof<NestedWithCanBeSimplifiedWarning> |])>]
type RecordExprAnalyzer() =
    inherit ElementProblemAnalyzer<IRecordExpr>()

    let collectRecordExprsToSimplify (expr: IRecordExpr) =
        let exprsToSimplify = List()

        let rec collectRecordExprsToSimplifyInner
            (expr: IRecordExpr)
            (fieldBinding: IRecordFieldBinding)
            (currentFieldPath: IList<string>)
            (exprsToSimplify: List<IRecordFieldBinding>) =

            match expr.CopyInfoExpression.IgnoreInnerParens() with
            | :? IReferenceExpr as copyExprReference when isSimpleQualifiedName copyExprReference ->
                let copyExprReferencePath = copyExprReference.GetNames()
                if isNull fieldBinding || not (Enumerable.SequenceEqual(currentFieldPath, copyExprReferencePath)) then
                    for fieldBinding in expr.FieldBindingsEnumerable do
                        match fieldBinding.Expression.IgnoreInnerParens() with
                        | :? IRecordExpr as recordExpr ->
                            let currentFieldPath = List(copyExprReferencePath)

                            fieldBinding.ReferenceName
                            |> UseNestedRecordFieldSyntax.getFieldPath
                            |> currentFieldPath.AddRange

                            collectRecordExprsToSimplifyInner recordExpr fieldBinding currentFieldPath exprsToSimplify
                        | _ -> ()
                else if expr.FieldBindings.Count = 1 then exprsToSimplify.Add(fieldBinding)
            | _ -> ()

        collectRecordExprsToSimplifyInner expr null null exprsToSimplify
        exprsToSimplify

    override this.Run(recordExpr, data, consumer) =
        if not data.IsFSharp80Supported || not recordExpr.IsSingleLine then () else

        let isInnerRecordExpr =
            recordExpr.IgnoreParentParens()
            |> RecordFieldBindingNavigator.GetByExpression
            |> isNotNull

        if isInnerRecordExpr then () else

        let recordExprsToSimplify = collectRecordExprsToSimplify recordExpr

        for recordExpr in recordExprsToSimplify do
            consumer.AddHighlighting(NestedWithCanBeSimplifiedWarning(recordExpr))
