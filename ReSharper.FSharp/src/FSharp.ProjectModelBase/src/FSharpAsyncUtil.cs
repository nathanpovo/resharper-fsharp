using System;
using System.Threading;
using JetBrains.Annotations;
using JetBrains.Application;
using JetBrains.Threading;
using Microsoft.FSharp.Control;

namespace JetBrains.ReSharper.Plugins.FSharp
{
  public static class FSharpAsyncUtil
  {
    private const int InterruptCheckTimeout = 30;

    private static readonly Action ourDefaultInterruptCheck =
      () => Interruption.Current.CheckAndThrow();

    [CanBeNull]
    public static T RunAsTask<T>([NotNull] this FSharpAsync<T> async, [CanBeNull] Action interruptChecker = null) =>
      RunInterrupting(async, interruptChecker);

    private static TResult RunInterrupting<TResult>([NotNull] FSharpAsync<TResult> async,
      [CanBeNull] Action interruptChecker)
    {
      interruptChecker ??= ourDefaultInterruptCheck;

      var cancellationTokenSource = new CancellationTokenSource();
      var cancellationToken = cancellationTokenSource.Token;
      var task = FSharpAsync.StartAsTask(async, null, cancellationToken);

      while (!task.IsCompleted)
      {
        var finished = task.Wait(InterruptCheckTimeout, cancellationToken);
        if (finished) break;

        if (FSharpLocks.ReadRequests.TryDequeue(out var request))
          request.RunSynchronously();

        try
        {
          interruptChecker();
        }
        catch (Exception e) when (e.IsOperationCanceled())
        {
          cancellationTokenSource.Cancel();
          throw;
        }
      }

      return task.Result;
    }
  }
}
