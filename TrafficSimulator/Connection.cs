using System;

namespace TrafficSimulator.Core
{
    internal class Connection
    {
        public Crossing Start{ get;  }
        public Crossing End { get; }

        public Connection(Crossing start, Crossing end)
        {
            Start = start ?? throw new ArgumentNullException(nameof(start));
            End = end ?? throw new ArgumentNullException(nameof(end));
        }
    }
}