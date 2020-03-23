namespace TrafficSimulator.Core.ConnectionLength
{
    abstract class DoubleDistanceCalculator : IDistanceCalculator<double>
    {
        public abstract double Calculate(Position<double> first, Position<double> second);
    }

    class EuclideanDistanceCalculator : DoubleDistanceCalculator
    {
        public override double Calculate(Position<double> first, Position<double> second)
        {
            double distance = 0;

            return distance;
        }
    }
}