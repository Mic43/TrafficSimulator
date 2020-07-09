namespace TrafficSimulator
  
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols             
open TrafficSimulator.DomainModel

module Api =
    type Command = Update of TimeInterval
    type Query = Init

    type CommandsHandler = Command -> SimulationState -> SimulationState
    type QueryRunner = Query -> SimulationState

    let handleCommand:CommandsHandler = fun command simulationState -> 
        match command with 
            | Update timeInterval -> Setup.updateSimulationState simulationState timeInterval
    let runQuery:QueryRunner = fun query ->
        match query with
            | Init -> Setup.init ()