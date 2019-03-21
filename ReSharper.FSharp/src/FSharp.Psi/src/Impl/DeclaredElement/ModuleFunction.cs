﻿using FSharp.Compiler.SourceCodeServices;
using JetBrains.Annotations;
using JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree;
using JetBrains.ReSharper.Psi.Tree;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.DeclaredElement
{
  internal class ModuleFunction : FSharpMethodBase<TopPatternDeclarationBase>
  {
    public ModuleFunction([NotNull] ITypeMemberDeclaration declaration, [NotNull] FSharpMemberOrFunctionOrValue mfv)
      : base(declaration, mfv)
    {
    }
  }
}
