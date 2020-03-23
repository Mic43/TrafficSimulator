namespace TrafficSimulator.Core.ConnectionLength
{
    class ConnectionLengthCalculator<T> : IConnectionLengthCalculator<T> where T : struct
    {
        private readonly ICrossingPositionProvider<T> _crossingPositionProvider;
        private readonly IDistanceCalculator<T> _distanceCalculator;

        public ConnectionLengthCalculator(ICrossingPositionProvider<T> crossingPositionProvider,
            IDistanceCalculator<T> distanceCalculator)
        {
            _crossingPositionProvider = crossingPositionProvider;
            _distanceCalculator = distanceCalculator;
        }

        public T Calculate(Connection connection)
        {
            var startPosition = _crossingPositionProvider.Get(connection.Start);
            var endPosition = _crossingPositionProvider.Get(connection.Start);

            return _distanceCalculator.Calculate(startPosition, endPosition);
        }
    }
}