// Learn more about F# at http://fsharp.org

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols  
open TrafficSimulator

[<EntryPoint>]
let main argv =    

    let timeInterval = DomainModel.TimeInterval 0.1<s>   
    let initialState = Api.runQuery Api.Init

    let updateSimulation stateLogger initialState stepsCount timeInterval = 
        seq {1..stepsCount} 
            |> Seq.fold (fun simulationState _ -> 
                                            stateLogger simulationState
                                            Api.handleCommand (Api.Update timeInterval) simulationState) 
                                initialState

    let simplelog (simState:DomainModel.SimulationState) = (printfn "%A" simState.Vehicles)
    let emptyLog (simState:DomainModel.SimulationState) = simState |> ignore

    let log = simplelog
    let updater stepsCount = updateSimulation log initialState stepsCount timeInterval

    updater 10 |> log
    //printfn "%A" DomainModel.z
    0 // return an integer exit code
