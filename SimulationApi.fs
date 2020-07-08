namespace TrafficSimulator

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols     
open DomainModel

module Api = 
    type Command = Update of TimeInterval
    type Query = Init

    type CommandsHandler = Command -> SimulationState -> SimulationState
    type QueryProcessor = Query -> SimulationState
 
    let handleCommand:CommandsHandler = fun command simulationState -> 
        match command with 
            | Update timeInterval -> Setup.updateSimulationState simulationState timeInterval
    let processQuery:QueryProcessor = fun query ->
        match query with
            | Init -> Setup.init ()