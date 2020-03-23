namespace TrafficSimulator.Core.ConnectionLength
{
    internal interface IDistanceCalculator<T> where T : struct
    {
        T Calculate(Position<T> first, Position<T> second);
    }
}