namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.QuickFixes

open System.Collections.Generic
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Refactorings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Psi.ExtensionsAPI
open JetBrains.ReSharper.Resources.Shell

type UseNestedRecordFieldSyntaxFix(warning: NestedWithCanBeSimplifiedWarning) =
    inherit FSharpScopedQuickFixBase(warning.Binding)

    let bindingExpr = warning.Binding

    let rec getFieldToUpdateData (expr: IFSharpExpression) (currentFieldPath: List<string>) =
        match expr.IgnoreInnerParens() with
        | :? IRecordExpr as recordExpr ->
            let field = recordExpr.FieldBindings.SingleItem

            field.ReferenceName
            |> UseNestedRecordFieldSyntax.getFieldPath
            |> currentFieldPath.AddRange

            getFieldToUpdateData field.Expression currentFieldPath
        | _ -> expr

    let updateExpr, fieldName =
        let fieldPath = List()
        fieldPath.AddRange (bindingExpr.ReferenceName.GetNames())
        getFieldToUpdateData bindingExpr.Expression fieldPath, String.concat "." fieldPath

    override this.IsAvailable _ = isValid bindingExpr
    override this.Text = $"Replace with '{fieldName} = ...'"
    override this.ScopedText = "Use nested record field syntax"

    override x.ExecutePsiTransaction _ =
        use writeCookie = WriteLockCookie.Create(bindingExpr.IsPhysical())
        use disableFormatter = new DisableCodeFormatter()
        let factory = bindingExpr.CreateElementFactory()

        let expr = factory.CreateRecordFieldBinding(fieldName, isNotNull bindingExpr.Semicolon, updateExpr.GetText())
        replace bindingExpr expr
