// Learn more about F# at http://fsharp.org

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols  
open TrafficSimulator
open DomainFunctions
open Option
open Common
[<EntryPoint>]
let main argv =    

    let distancePerUnit = 1.0<m>
    let simpleUpdater (connectionsGraph:ConnectionsGraph) = 
        let progressCalc  = progressTravelledCalculator (LenghtProviders.lenghtProvider distancePerUnit connectionsGraph)
        let locationUpdater timeChange vehicle = VehicleLocationUpdaters.simpleLocationUpdater (progressCalc vehicle timeChange)
        let vehicleUpdater timeChange vehicle = vehicle |> simpleVehicleUpdater (locationUpdater timeChange vehicle)
        vehicleUpdater

    let simpleUpdater2 (connectionsGraph:ConnectionsGraph) = 
        let progressCalc  = progressTravelledCalculator (LenghtProviders.lenghtProvider distancePerUnit connectionsGraph)
        let locationUpdater timeChange vehicle = VehicleLocationUpdaters.locationUpdater connectionsGraph (progressCalc vehicle timeChange)
        let vehicleUpdater timeChange vehicle = vehicle |> simpleVehicleUpdater (locationUpdater timeChange vehicle)
        vehicleUpdater

    let crossings =
        Map.empty (* Start with empty Map *)
           .Add( CrossingId 1, {Name = Some "aaa";Position = Position2d {X = 1.0;Y =2.0}})
           .Add( CrossingId 2, {Name = None ;Position = Position2d {X = 1.5;Y =4.0}})
    let connections = [{ConnectionType = Linear ;StartId= CrossingId 1;EndId = CrossingId 2};
                       {ConnectionType = Linear ;StartId= CrossingId 2;EndId = CrossingId 1}]    
    
                               
    let vehicles = [{Vehicle.CurrentSpeed = 10.0<m/s>; 
                    Vehicle.Location = 
                        {VehicleLocation.CurrentProgress = (Fraction 0.0);
                         Placing = connections.[0]} }]
    let connectionsGraph = ConnectionsGraph.create crossings connections      
    
    let sim  = connectionsGraph                         
                        |> Option.bind (fun connectionsGraph -> Some(simpleUpdater connectionsGraph))
                        |> Option.bind (fun updater -> 
                                            let res timeChange = VehiclesUpdaters.update updater timeChange vehicles
                                            Some(res))   

    //0.1<s> |> Seq.replicate 20 |> Seq.map |> Option.map ( fun sim -> sim 0.1<s> ) |> printf "%A"

   // Simulation.updat 
    // let res = optional {
    //     let! simulation = {Simulation.T.Vehicles = vehicles;Simulation.T.ConnectionsGraph = connectionsGraph }     
    // }
    
    
     
//    Option.orElseWith

   
    let z  = {X = 1.0;Y = 2.0}
    let z2  = {X = 1.0;Y = 2.0}

  
   // printfn "%A" res
    //printfn "%A" DomainModel.z
    0 // return an integer exit code
