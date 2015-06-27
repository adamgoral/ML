using System.Collections.Concurrent;
using System.IO;

namespace ML.Common
{
    public class SynchronisedCsvLogger
    {
        private readonly string path;

        private ConcurrentQueue<string[]> queue = new ConcurrentQueue<string[]>(); 

        private readonly object lockObj = new object();

        public SynchronisedCsvLogger(string path)
        {
            this.path = path;
        }

        public void Write(params string[] line)
        {
            queue.Enqueue(line);
            try
            {
                using (var writer = File.AppendText(this.path))
                {
                    lock (this.lockObj)
                    {
                        var toProces = this.queue;
                        this.queue = new ConcurrentQueue<string[]>();
                        string[] item = null;
                        while (toProces.TryDequeue(out item))
                        {
                            writer.WriteLine(string.Join(",", item));
                        }
                    }
                }
            }
            catch
            {
            }
        }
    }
}
