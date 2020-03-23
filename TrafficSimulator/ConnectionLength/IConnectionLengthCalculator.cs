namespace TrafficSimulator.Core.ConnectionLength
{
    internal interface IConnectionLengthCalculator<T> where T : struct
    {
        T Calculate(Connection connection) ; 
    }

        
}