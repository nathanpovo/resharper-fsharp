namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Intentions.QuickFixes

open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.QuickFixes
open JetBrains.ReSharper.Plugins.FSharp.Tests
open NUnit.Framework

[<FSharpTest>]
type UpdateRecordFieldTypeInSignatureFixTest() =
    inherit FSharpQuickFixTestBase<UpdateRecordFieldTypeInSignatureFix>()
    override x.RelativeTestDataPath = "features/quickFixes/updateRecordFieldTypeInSignatureFix"

    [<Test>] member x.``Wrong Record Field Type - 01`` () = x.DoNamedTestWithSignature()
    [<Test>] member x.``Type Alias - 01`` () = x.DoNamedTestWithSignature()
    [<Test>] member x.``Generic argument - 01`` () = x.DoNamedTestWithSignature()
    [<Test>] member x.``Update all fields - 01`` () = x.DoNamedTestWithSignature()
    [<Test>] member x.``Mutable field - 01`` () = x.DoNamedTestWithSignature()
    [<Test>] member x.``Mutable field - 02`` () = x.DoNamedTestWithSignature()
