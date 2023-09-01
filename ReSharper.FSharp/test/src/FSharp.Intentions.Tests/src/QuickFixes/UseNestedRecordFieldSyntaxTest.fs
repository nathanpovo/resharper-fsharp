namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Intentions.QuickFixes

open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.QuickFixes
open JetBrains.ReSharper.Plugins.FSharp.Tests
open NUnit.Framework

[<FSharpTest>]
type UseNestedRecordFieldSyntaxTest() =
    inherit FSharpQuickFixTestBase<UseNestedRecordFieldSyntaxFix>()

    override x.RelativeTestDataPath = "features/quickFixes/useNestedRecordFieldSyntaxFix"

    [<Test>] member x.``Simple 01`` () = x.DoNamedTest()
    [<Test>] member x.``Simple 02`` () = x.DoNamedTest()
    [<Test>] member x.``Qualified field name 01`` () = x.DoNamedTest()
    [<Test>] member x.``Qualified field name 02`` () = x.DoNamedTest()
    [<Test>] member x.``Nested record 01`` () = x.DoNamedTest()
