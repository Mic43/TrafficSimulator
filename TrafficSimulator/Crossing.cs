using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace TrafficSimulator.Core
{
    internal class Crossing
    {
        private readonly List<Connection> _input;
        private readonly List<Connection> _output;

        public IReadOnlyList<Connection> Output => _output;
        public IReadOnlyList<Connection> Input => _input;

        protected Crossing()
        {
            _input = new List<Connection>();
            _output = new List<Connection>();
        }

        public void AddInput(Connection connection)
        {
            if (connection == null) throw new ArgumentNullException(nameof(connection));
            if(connection.End != this)
                throw new ArgumentException("Connection must end at this crossing, to be valid input", nameof(connection));
            _input.Add(connection);
        }
        public void AddOutput(Connection connection)
        {
            if (connection == null) throw new ArgumentNullException(nameof(connection));
            if (connection.Start != this)
                throw new ArgumentException("Connection must start at this crossing, to be valid output", nameof(connection));
            _output.Add(connection);
        }

        public static Crossing Empty()
        {
            return new Crossing();
        }

    }
}