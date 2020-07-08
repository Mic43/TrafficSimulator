namespace TrafficSimulator

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols     
open DomainModel
open DomainFunctions

module Setup = 
    let distancePerUnit = 1.0<m>
    let  simpleUpdater (connectionsGraph:ConnectionsGraph) = 
        let progressCalc  = progressTravelledCalculator (LenghtProviders.lenghtProvider distancePerUnit connectionsGraph)
        let locationUpdater timeChange vehicle = VehicleLocationUpdaters.simpleLocationUpdater (progressCalc vehicle timeChange)
        let vehicleUpdater timeChange vehicle = vehicle |> simpleVehicleUpdater (locationUpdater timeChange vehicle)
        vehicleUpdater

    let simpleUpdater2 (connectionsGraph:ConnectionsGraph) = 
        let progressCalc  = progressTravelledCalculator (LenghtProviders.lenghtProvider distancePerUnit connectionsGraph)
        let locationUpdater timeChange vehicle = VehicleLocationUpdaters.locationUpdater connectionsGraph (progressCalc vehicle timeChange)
        let vehicleUpdater timeChange vehicle = vehicle |> simpleVehicleUpdater (locationUpdater timeChange vehicle)
        vehicleUpdater

    let updateSimulationState simulationState timeInterval = 
        let newVehicles = DomainFunctions.VehiclesUpdaters.update (simpleUpdater2 simulationState.ConnectionsGraph) 
                            timeInterval simulationState.Vehicles
        { simulationState with Vehicles = newVehicles}
    let init () = 
        let crossings =
            Map.empty (* Start with empty Map *)
               .Add( CrossingId 1, {Name = Some "aaa";Position = Position2d {X = 1.0;Y =2.0}})
               .Add( CrossingId 2, {Name = None ;Position = Position2d {X = 1.5;Y =4.0}})
        let connections = [{ConnectionType = Linear ;StartId= CrossingId 1;EndId = CrossingId 2};
                           {ConnectionType = Linear ;StartId= CrossingId 2;EndId = CrossingId 1}]    

                                   
        let vehicles = [{Vehicle.CurrentSpeed = 10.0<m/s>; 
                        Vehicle.Location = 
                            {VehicleLocation.CurrentProgress = (Fraction.zero);
                             Placing = connections.[0]} }]
        let connectionsGraph = ConnectionsGraph.create crossings connections      
        {SimulationState.Vehicles = vehicles; SimulationState.ConnectionsGraph = connectionsGraph.Value}