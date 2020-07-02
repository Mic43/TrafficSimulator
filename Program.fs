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
    
    printfn "Hello World from F#!"
    //printfn "%A" DomainModel.z
    0 // return an integer exit code
