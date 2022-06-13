namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.ParameterInfo

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open JetBrains.Diagnostics
open JetBrains.DocumentModel
open JetBrains.Metadata.Reader.API
open JetBrains.ProjectModel
open JetBrains.ReSharper.Feature.Services.ParameterInfo
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Util
open JetBrains.ReSharper.Plugins.FSharp.Util
open JetBrains.ReSharper.Plugins.FSharp.Util.FcsTaggedText
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Files
open JetBrains.ReSharper.Psi.Modules
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.Util
open JetBrains.ReSharper.Psi.Xml.XmlDocComments
open JetBrains.ReSharper.Resources.Shell
open JetBrains.UI.RichText
open JetBrains.Util
open JetBrains.Util.Extension

module FcsParameterInfoCandidate =
    let canBeNullAttrTypeName = clrTypeName "JetBrains.Annotations.CanBeNullAttribute"
    let notNullAttrTypeName = clrTypeName "JetBrains.Annotations.NotNullAttribute"

type FcsParameterInfoCandidate(range: range, fcsSymbolUse: FSharpSymbolUse, checkResults: FSharpCheckFileResults, expr: IFSharpExpression, resolvedMfv: FSharpMemberOrFunctionOrValue) =
    let mfv = fcsSymbolUse.Symbol :?> FSharpMemberOrFunctionOrValue
    let displayContext = fcsSymbolUse.DisplayContext.WithShortTypeNames(true)

    let psiModule = expr.GetPsiModule()
    let declaredElement = mfv.GetDeclaredElement(psiModule)
    let parametersOwner = declaredElement.As<IParametersOwnerWithAttributes>()
    let isExtensionMember = mfv.IsExtensionMember

    let getParameterIncludingThis index =
        if isNull parametersOwner then null else

        let parameters = parametersOwner.Parameters
        if parameters.Count < index then null else parameters[index]

    let getParameter index =
        let index = if isExtensionMember then index + 1 else index
        getParameterIncludingThis index

    member this.Mfv = mfv
    member this.ParameterOwner = parametersOwner

    interface ICandidate with
        member this.GetDescription() =
            let substitution = fcsSymbolUse.GenericArguments
            match checkResults.GetDescription(mfv, substitution, false, range) with
            | ToolTipText [ ToolTipElement.Group [ elementData ] ] ->
                let xmlDocService = expr.GetSolution().GetComponent<FSharpXmlDocService>().NotNull()
                xmlDocService.GetXmlDocSummary(elementData.XmlDoc)
            | _ -> null

        member this.GetParametersInfo(paramInfos, paramArrayIndex) =
            paramArrayIndex <- -1

            let paramGroups = mfv.CurriedParameterGroups
            let curriedParamsCount = paramGroups |> Seq.sumBy Seq.length
            let groupParameters = paramGroups.Count
            let paramsCount = curriedParamsCount + groupParameters

            let parameters = parametersOwner.Parameters

            let paramInfos =
                paramInfos <- Array.zeroCreate paramsCount
                paramInfos

            let mutable paramIndex = 0
            for groupIndex = 0 to paramGroups.Count - 1 do
                let paramGroup = paramGroups[groupIndex]

                let paramGroupInfoText = RichText()

                for i = 0 to paramGroup.Count - 1 do
                    if paramIndex >= parameters.Count then () else

                    let parameter = parameters[paramIndex]
                    let name = parameter.ShortName

                    let summary =
                        if parameter.PresentationLanguage.Is<FSharpLanguage>() then
                            // todo: implement providing xml in declared element, remove this code
                            match mfv.XmlDoc with
                            | FSharpXmlDoc.FromXmlText xmlDoc ->
                                match DocCommentBlockUtil.TryGetXml(xmlDoc.UnprocessedLines, null) with
                                | true, node -> XMLDocUtil.ExtractParameterSummary(node, name)
                                | _ -> null
                            | _ -> null
                        else
                            parameter.GetXMLDescriptionSummary(true)

                    let description = XmlDocRichTextPresenter.Run(summary, false, CSharpLanguage.Instance)
                    paramInfos[paramIndex] <- ParamPresentationInfo(Name = name, Description = description)

                    if not (RichTextBlock.IsNullOrEmpty(description)) then
                        paramGroupInfoText.Append($"{name}: ", TextStyle(JetFontStyles.Bold)) |> ignore
                        let description = XmlDocRichTextPresenter.Run(summary, false, CSharpLanguage.Instance)
                        for line in description.Lines do
                            paramGroupInfoText.Append(line) |> ignore
                        paramGroupInfoText.Append("\n") |> ignore
                        // paramGroupInfoText.Append(description)
                        // let paramInfo = RichText()
                        
                        // description.Lines[0].Prepend(paramInfo) |> ignore
                        // ParamPresentationInfo(Description = RichTextBlock(paramGroupInfoText)).Description.AddLines(description.Lines)


                    paramIndex <- paramIndex + 1

                // paramInfos[curriedParamsCount + groupIndex] <-
                //     ParamPresentationInfo(Description = RichTextBlock(paramGroupInfoText))

            ()
            // paramGroups
            // |> Seq.concat
            // |> Seq.iteri (fun index fcsParam ->
            //     
            //
            //     let parameter = parameters[index]
            //     let name = parameter.ShortName
            //
            //     let summary =
            //         if parameter.PresentationLanguage.Is<FSharpLanguage>() then
            //             // todo: implement providing xml in declared element, remove this code
            //             match mfv.XmlDoc with
            //             | FSharpXmlDoc.FromXmlText xmlDoc ->
            //                 match DocCommentBlockUtil.TryGetXml(xmlDoc.UnprocessedLines, null) with
            //                 | true, node -> XMLDocUtil.ExtractParameterSummary(node, name)
            //                 | _ -> null
            //             | _ -> null
            //         else
            //             parameter.GetXMLDescriptionSummary(true)
            //
            //     let description = XmlDocRichTextPresenter.Run(summary, false, CSharpLanguage.Instance)
            //     paramInfos[index] <- ParamPresentationInfo(Name = name, Description = description)
            // )

        member this.GetSignature(namedArguments, showAnnotations, parameterRanges, mapToOriginalOrder, extensionMethodInfo) =
            let paramGroups = mfv.CurriedParameterGroups
            if paramGroups.Count = 0 then RichText() else

            let curriedParamsCount = paramGroups |> Seq.sumBy Seq.length 
            let groupParameters = paramGroups.Count

            // Add additional group parameters to highlight group ranges
            let paramsCount = curriedParamsCount + groupParameters
            parameterRanges <- Array.zeroCreate paramsCount

            let text = RichText()

            let appendNullabilityAttribute (attrOwner: IAttributesOwner) (attrName: IClrTypeName) =
                let hasAttrInstance = attrOwner.HasAttributeInstance(attrName, true)
                if hasAttrInstance then
                    let attrShortName = attrName.ShortName.SubstringBeforeLast("Attribute")

                    text.Append("[", TextStyle.Default) |> ignore
                    text.Append(attrShortName, TextStyle FSharpHighlightingAttributeIds.Class) |> ignore
                    text.Append("] ", TextStyle.Default) |> ignore

                hasAttrInstance

            let appendNullabilityAttribute (attrOwner: IAttributesOwner) =
                appendNullabilityAttribute attrOwner FcsParameterInfoCandidate.canBeNullAttrTypeName ||
                appendNullabilityAttribute attrOwner FcsParameterInfoCandidate.notNullAttrTypeName

            use _ = ReadLockCookie.Create()
            use _ = CompilationContextCookie.GetOrCreate(psiModule.GetContextFromModule())

            let mutable paramIndex = 0
            for i = 0 to paramGroups.Count - 1 do
                text.Append("(", TextStyle.Default) |> ignore
                let groupStart = text.Length

                let paramGroup = paramGroups[i]

                if paramIndex = 0 && isExtensionMember then
                    let parameter = getParameterIncludingThis paramIndex
                    if isNotNull parameter then
                        appendNullabilityAttribute parameter |> ignore

                    text.Append("this", TextStyle FSharpHighlightingAttributeIds.Keyword) |> ignore
                    text.Append(" ", TextStyle.Default) |> ignore

                    // todo: type arg is not provided by FCS, add it to the symbols API
                    text.Append(mfv.ApparentEnclosingEntity.AsType().FormatLayout(displayContext) |> richText) |> ignore
                    if paramGroup.Count > 0 then
                        text.Append(", ", TextStyle.Default) |> ignore

                for i = 0 to paramGroup.Count - 1 do
                    let fcsParameter = paramGroup[i]
                    let parameter = getParameter paramIndex

                    let paramStart = text.Length

                    if isNotNull parameter then
                        appendNullabilityAttribute parameter |> ignore

                        if parameter.IsParameterArray then
                            text.Append("params", TextStyle FSharpHighlightingAttributeIds.Keyword) |> ignore
                            text.Append(" ", TextStyle.Default) |> ignore

                    match fcsParameter.Name with
                    | Some name ->
                        text.Append(name, TextStyle FSharpHighlightingAttributeIds.Parameter) |> ignore
                        text.Append(": ", TextStyle.Default) |> ignore
                    | _ -> ()

                    text.Append(fcsParameter.Type.FormatLayout(displayContext) |> richText) |> ignore

                    if isNotNull parameter && parameter.IsOptional then
                        let constantValue = parameter.GetDefaultValue().ConstantValue
                        let presentation = constantValue.GetPresentation(FSharpLanguage.Instance, TypePresentationStyle.Default)
                        text.Append(" = ", TextStyle.Default) |> ignore
                        text.Append(presentation) |> ignore

                    let paramEnd = text.Length
                    parameterRanges[paramIndex] <- TextRange(paramStart, paramEnd)

                    if i < paramGroup.Count - 1 then
                        text.Append(", ", TextStyle.Default) |> ignore

                    paramIndex <- paramIndex + 1

                let groupEnd = text.Length
                parameterRanges[curriedParamsCount + i] <- TextRange(groupStart, groupEnd)

                text.Append(")", TextStyle.Default) |> ignore

                if i < paramGroups.Count - 1 then
                    text.Append(" ", TextStyle.Default) |> ignore

            if not mfv.IsConstructor then
                text.Append(" : ", TextStyle.Default) |> ignore

                if isNotNull parametersOwner then
                    appendNullabilityAttribute parametersOwner |> ignore
                
                text.Append(mfv.ReturnParameter.Type.FormatLayout(displayContext) |> richText) |> ignore

            text

        member this.Matches _ =
            mfv.IsEffectivelySameAs(resolvedMfv)

        member this.IsFilteredOut = false
        member this.IsObsolete = false
        member this.ObsoleteDescription = RichTextBlock()
        member this.PositionalParameterCount = 0
        member this.IsFilteredOut with set _ = ()


[<AllowNullLiteral>]
type FSharpParameterInfoContext2(caretOffset: DocumentOffset, appExpr: IFSharpExpression, methods: FSharpSymbolUse list, checkResults, fcsRange, mfv) =
    let candidates =
        methods
        |> List.choose (fun item ->
            match item.Symbol with
            | :? FSharpMemberOrFunctionOrValue ->
                Some(FcsParameterInfoCandidate(fcsRange, item, checkResults, appExpr, mfv) :> ICandidate)
            | _ -> None)
        |> Array.ofList

    interface IParameterInfoContext with
        member this.Range =
            appExpr.GetDocumentRange().TextRange

        member this.GetArgument(candidate) =
            let candidate = candidate :?> FcsParameterInfoCandidate
            let parameterGroups = candidate.Mfv.CurriedParameterGroups
            let allParametersCount = parameterGroups |> Seq.sumBy Seq.length
            let invalidArg = allParametersCount + parameterGroups.Count

            let rec getArgs (expr: IFSharpExpression) acc =
                match expr with
                | :? IPrefixAppExpr as prefixAppExpr ->
                    match prefixAppExpr.ArgumentExpression with
                    | null -> getArgs prefixAppExpr acc
                    | argExpr -> getArgs prefixAppExpr.FunctionExpression (argExpr :: acc)
                | _ -> acc

            let args = getArgs appExpr []

            let rec loop argIndex (acc: int) (args: IFSharpExpression list) =
                match args with
                | [] ->
                    if argIndex >= parameterGroups.Count then
                        invalidArg
                    elif argIndex < parameterGroups.Count && parameterGroups[argIndex].Count = 1 then
                        acc
                    else
                        allParametersCount + argIndex

                | arg :: args ->
                    let argRange = arg.GetDocumentRange()
                    let argEnd = argRange.EndOffset
                    let argStart = argRange.StartOffset

                    if caretOffset.Offset > argEnd.Offset then
                        loop (argIndex + 1) (acc + parameterGroups[argIndex].Count) args

                    elif caretOffset = argEnd && (match args with nextArg :: _ -> nextArg.GetDocumentStartOffset() = caretOffset | _ -> false) then
                        loop (argIndex + 1) (acc + parameterGroups[argIndex].Count) args

                    elif argRange.Contains(caretOffset) && argStart <> caretOffset && argEnd <> caretOffset then
                        if parameterGroups[argIndex].Count = 1 then acc else
                        if arg :? IUnitExpr then acc else

                        match arg.IgnoreSingleInnerParens() with
                        | :? ITupleExpr as tupleExpr ->
                            let commaIndex =
                                let commas = tupleExpr.Commas
                                if commas.IsEmpty then 0 else

                                commas
                                |> Seq.tryFindIndex (fun comma -> caretOffset.Offset <= comma.GetDocumentRange().StartOffset.Offset)
                                |> Option.defaultValue commas.Count

                            let paramGroup = parameterGroups[argIndex]
                            if commaIndex >= paramGroup.Count then
                                if paramGroup.Count = 0 then invalidArg else

                                let lastParamIndex = acc + paramGroup.Count - 1
                                let parameters = candidate.ParameterOwner.Parameters
                                if lastParamIndex < parameters.Count && parameters[lastParamIndex].IsParameterArray then
                                    lastParamIndex
                                else
                                    invalidArg
                            else
                                let count = parameterGroups[argIndex].Count - 1
                                let count = max count 0
                                let min = min count commaIndex
                                acc + min
                        | innerArg ->
                            if innerArg != arg then
                                acc
                            else
                                allParametersCount + argIndex

                    elif argIndex < parameterGroups.Count && parameterGroups[argIndex].Count = 1 then
                        acc

                    else
                        allParametersCount + argIndex

            loop 0 0 args

        member this.Candidates = candidates

        member this.DefaultCandidate =
            candidates
            |> Array.tryFind (function
                | :? FcsParameterInfoCandidate as candidate -> candidate.Mfv.IsEffectivelySameAs(mfv)
                | _ -> false)
            |> Option.defaultValue null

        member val NamedArguments = [||] with get, set
        member this.ParameterListNodeType = null
        member this.ParameterNodeTypes = null


module FSharpParameterInfoContextFactory =
    let shouldShowPopup (caretOffset: DocumentOffset) (exprRange: DocumentRange) =
        if exprRange.Contains(caretOffset) then true else
        if caretOffset.Offset < exprRange.StartOffset.Offset then false else
        caretOffset.ToDocumentCoords().Column > exprRange.StartOffset.ToDocumentCoords().Column

    let rec tryCreateContext (caretOffset: DocumentOffset) (expr: IFSharpExpression) =
        if isNull expr then null else

        let range = expr.GetDocumentRange()
        let expr =
            match expr with
            | :? IParenExpr as parenExpr when range.StartOffset <> caretOffset && range.EndOffset <> caretOffset ->
                parenExpr.InnerExpression
            | _ -> expr

        match expr with
        | :? IPrefixAppExpr as appExpr ->
             // todo: allow on non-refExpr invoked expressions (lambdas, other apps)
            match create caretOffset appExpr.InvokedReferenceExpression appExpr with
            | null ->
                let parentExpr = expr.IgnoreParentParens().GetContainingNode<IFSharpExpression>()
                tryCreateContext caretOffset parentExpr
            | context -> context

        | :? IReferenceExpr as refExpr ->
            match PrefixAppExprNavigator.GetByArgumentExpression(refExpr.IgnoreParentParens()) with
            | null -> create caretOffset refExpr refExpr
            | prefixAppExpr -> tryCreateContext caretOffset prefixAppExpr

        | _ ->
            // null
            let parentExpr = expr.IgnoreParentParens().GetContainingNode<IFSharpExpression>()
            tryCreateContext caretOffset parentExpr

    and create (caretOffset: DocumentOffset) (invokedExpr: IReferenceExpr) (contextExpr: IFSharpExpression) =
        if isNull invokedExpr || isNull contextExpr then null else

        let range = invokedExpr.GetDocumentRange()
        let appExpr = getOutermostPrefixAppExpr contextExpr
        let appExpr = appExpr.IgnoreParentParens()

        if caretOffset.Offset >= range.StartOffset.Offset && caretOffset.Offset < range.EndOffset.Offset ||
                caretOffset = range.EndOffset && isNotNull (PrefixAppExprNavigator.GetByArgumentExpression(appExpr)) then
            let parentExpr = appExpr.IgnoreParentParens().GetContainingNode<IFSharpExpression>()
            tryCreateContext caretOffset parentExpr else

        let symbolUse = invokedExpr.Reference.GetSymbolUse()
        if isNull symbolUse then null else

        let mfv = symbolUse.Symbol.As<FSharpMemberOrFunctionOrValue>()
        if isNull mfv then null else

        let appExpr = getOutermostPrefixAppExpr contextExpr
        if not (shouldShowPopup caretOffset (appExpr.GetDocumentRange())) then null else

        match contextExpr.FSharpFile.GetParseAndCheckResults(true, "FSharpParameterInfoContextFactory") with
        | None -> null
        | Some results ->

        let endOffset = invokedExpr.GetDocumentEndOffset()
        let endCoords = endOffset.ToDocumentCoords()
        let line = int endCoords.Line + 1
        let column = int endCoords.Column + 1
        let names = List.ofSeq invokedExpr.Names

        match results.CheckResults.GetMethodsAsSymbols(line, column, "", names) with
        | Some symbolUses when not symbolUses.IsEmpty ->
            let documentRange = DocumentRange(endOffset.Document, endOffset.Offset)
            let fcsRange = FSharpRangeUtil.ofDocumentRange documentRange
            
            FSharpParameterInfoContext2(caretOffset, appExpr, symbolUses, results.CheckResults, fcsRange, mfv)
        | _ -> null

[<ParameterInfoContextFactory(typeof<FSharpLanguage>)>]
type FSharpParameterInfoContextFactory2() =
    let popupChars = [| ' '; '('; ',' |]

    interface IParameterInfoContextFactory with
        member this.Language = FSharpLanguage.Instance

        member this.ImportantChars = []
        member this.IsIntellisenseEnabled(_, _) = true // todo: settings
        member this.ShouldPopup(_, char, _, _) = Array.contains char popupChars // todo: settings

        member this.CreateContext(solution, caretOffset, _, _, _) =
            let fsFile = solution.GetPsiServices().GetPsiFile<FSharpLanguage>(caretOffset).As<IFSharpFile>()
            if isNull fsFile then null else

            let mutable wentLeft = false
            let mutable token = fsFile.FindTokenAt(caretOffset (*- 1*))
            if isNull token then null else

            while isNotNull token && token.IsFiltered() do
                wentLeft <- true
                token <- token.GetPreviousToken()

            let expr = token.GetContainingNode<IFSharpExpression>(true)

            FSharpParameterInfoContextFactory.tryCreateContext caretOffset expr
