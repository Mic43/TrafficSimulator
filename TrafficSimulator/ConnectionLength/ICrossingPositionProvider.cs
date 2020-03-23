namespace TrafficSimulator.Core.ConnectionLength
{
    internal interface ICrossingPositionProvider<T> where T : struct
    {
        Position<T> Get(Crossing crossing) ;
    }
}