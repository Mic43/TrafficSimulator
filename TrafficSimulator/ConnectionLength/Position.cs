using System.Numerics;

namespace TrafficSimulator.Core.ConnectionLength
{
    internal abstract class Position<T> where T:struct
    {
        public abstract int Dimension { get; }
        public abstract T this[int dimension] { get; }
    }
}