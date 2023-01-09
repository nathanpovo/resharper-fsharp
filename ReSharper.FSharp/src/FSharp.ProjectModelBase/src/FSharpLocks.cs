using System;
using System.Collections.Concurrent;
using System.Threading.Tasks;
using JetBrains.Application;
using JetBrains.Application.Threading;

namespace JetBrains.ReSharper.Plugins.FSharp
{
  [ShellComponent]
  public class FSharpLocks
  {
    public static volatile ConcurrentQueue<Task> ReadRequests = new();

    public static void UsingReadLock(IShellLocks locks, Action action)
    {
      if (locks.TryExecuteWithReadLock(action))
        return;

      var task = new Task(action);
      ReadRequests.Enqueue(task);

      task.Wait();

      if (task.Exception is { } exception)
        throw exception;
    }
  }
}
