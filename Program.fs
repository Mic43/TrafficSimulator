// Learn more about F# at http://fsharp.org

open System
open TrafficSimulator
open DomainFunctions

[<EntryPoint>]
let main argv =    

    let composeSimpleUpdater = 
        let progressCalc  = progressTravelledCalculator roadLenghtProvider 
        let locationUpdater vehicle timeChange = simpleLocationUpdater (progressCalc vehicle timeChange)
        let vehicleUpdater vehicle timeChange = vehicle |> simpleVehicleUpdater (locationUpdater vehicle timeChange)
        vehicleUpdater
    let updater = composeSimpleUpdater    
    let z  = {X = 1.0;Y = 2.0}
    let z2  = {X = 1.0;Y = 2.0}

    let crosings =
        Map.empty (* Start with empty Map *)
           .Add( CrossingId 1, {Name = Some "aaa";Position = Position2d {X = 1.0;Y =2.0}})
           .Add( CrossingId 2, {Name = None ;Position = Position2d {X = 1.5;Y =4.0}})
    let connections = [{ConnectionType = Linear ;Start= CrossingId 1;End = CrossingId 2};
                       {ConnectionType = Linear ;Start= CrossingId 2;End = CrossingId 1}]    
    
    let res = match (ConnectionsGraph.create crosings connections) with
                    | Some  -> "Ok"
                    | None -> "Error"
    printfn "%A" res
    //printfn "%A" DomainModel.z
    0 // return an integer exit code
