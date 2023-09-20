namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Intentions

open JetBrains.Application.BuildScript.Application.Zones
open JetBrains.ProjectModel
open JetBrains.RdBackend.Common.Env
open JetBrains.ReSharper.Feature.Services.ContextActions
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Psi.ExtensionsAPI
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Resources.Shell
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util.FSharpModulesUtil
open JetBrains.Rider.Backend.Env
open JetBrains.TextControl

[<ContextAction(Name = "ToModuleNamespace", Group = "F#", Description = "To module/namespace")>]
[<ZoneMarker(typeof<ILanguageFSharpZone>, typeof<IProjectModelZone>, typeof<IResharperHostCoreFeatureZone>, typeof<IRiderFeatureEnvironmentZone>, typeof<ITextControlsZone>, typeof<PsiFeaturesImplZone>)>]
type ToModuleNamespaceDeclarationAction(dataProvider: FSharpContextActionDataProvider) =
    inherit ContextActionBase()

    let isNamespace (declaration: IModuleLikeDeclaration) =
        declaration :? INamespaceDeclaration

    override x.Text =
        if isNamespace (dataProvider.GetSelectedElement()) then "To module" else "To namespace"

    override x.IsAvailable _ =
        let moduleDeclaration = dataProvider.GetSelectedElement<IQualifiableModuleLikeDeclaration>()
        if not (isAtModuleDeclarationKeyword dataProvider moduleDeclaration) then false else

        true

    override x.ExecutePsiTransaction(_, _) =
        let moduleDeclaration = dataProvider.GetSelectedElement<IDeclaredModuleLikeDeclaration>()
        use writeCookie = WriteLockCookie.Create(moduleDeclaration.IsPhysical())
        use disableFormatter = new DisableCodeFormatter()

        if isNamespace moduleDeclaration then convertNamespaceToModule moduleDeclaration
        else convertModuleToNamespace moduleDeclaration

        null
