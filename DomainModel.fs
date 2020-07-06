namespace TrafficSimulator

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols     
[<AutoOpen>]
module DomainModel =    

    type CrossingId = CrossingId of int    
    type Curve = float

    type ConnectionType = Linear | Curved of Curve

    type Position2d= {X:float;Y:float}
    type Position = Position2d of Position2d

    type Crossing = { Name:option<string> ; Position:Position} 
   
    type Fraction =  Fraction of float    
    type Progress = {Unities:int;Fractions:Fraction}
    module Progress =       
        let create (value:float) = 
            {Unities = int value;Fractions = Fraction (value - float (int value))}
        let toFloat progress =
            let (Fraction v) = progress.Fractions 
            float progress.Unities + v
        let fromFraction fraction =  
            let (Fraction v) = fraction 
            create v
        let add prog1 prog2 =
            create ((toFloat prog1) + (toFloat prog2))
        // let (|Progress|) {Unities=unities; Fractions=fractions} = struct (unities, fractions)
        // let value progress =           
        //     (progress.Unities,progress.Fractions) 

   // type Connection = {Start:Crossing;End:Crossing}
    type Connection = {ConnectionType:ConnectionType;Start:CrossingId;End:CrossingId}

    type VehicleLocation = {Placing:Connection;CurrentProgress:Fraction}  
    type Vehicle = {Location:VehicleLocation;CurrentSpeed:float<m/s>}

    type ConnectionsGraph = private { Crossings:Map<CrossingId,Crossing>; CrossingsOutput: Map<CrossingId,Connection seq>;} 
    module ConnectionsGraph =         
        let create (crossings: Map<CrossingId,Crossing>) (connections: Connection seq)  = 
            if not (connections |> Seq.forall (fun connection -> crossings.ContainsKey connection.Start && crossings.ContainsKey connection.End))
                then None
                else
                    let crossingOut = connections |> Seq.groupBy (fun con -> con.Start ) |> Map.ofSeq
                    Some({Crossings = crossings;CrossingsOutput = crossingOut})



   // type Roads = list<ConnectedRoad>
  //  let a = dict<Road>      
       
 module DomainFunctions = 
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols    
    let distanceToProgress (distance:float<m>) (connectionLenght:float<m>) = 
        Progress.create (distance / connectionLenght)
    
    type RoadLenghtProvider = Connection -> float<m>    
    type VehicleUpdater = float<s>->Vehicle->Vehicle
    type VehicleLocationUpdater = VehicleLocation->VehicleLocation
    type ProgressTravelledCalculator = unit -> Progress

    let roadLenghtProvider (connectionsGraph:ConnectionsGraph) connection  =
        match connection.ConnectionType with 
            |  Curved -> 100.0<m>
            |  Linear -> 100.0<m>

    let progressTravelledCalculator roadLenghtProvider vehicle timeChange  = 
        let distanceTravelled = vehicle.CurrentSpeed * timeChange
        let roadLenght = (roadLenghtProvider vehicle.Location.Placing)
        distanceToProgress distanceTravelled roadLenght

    let simpleLocationUpdater  (progressTravelled: Progress) (vehicleLocation:VehicleLocation)= 
        let newProgres = Progress.add (Progress.fromFraction vehicleLocation.CurrentProgress) progressTravelled      
        {vehicleLocation with CurrentProgress = newProgres.Fractions}

    let simpleVehicleUpdater (vehicleLocationUpdater:VehicleLocationUpdater) vehicle =  
        let newVehicleLocation = vehicleLocationUpdater vehicle.Location
        {vehicle with Location = newVehicleLocation}

type Simulation = {ConnectionsGraph:ConnectionsGraph; Vehicles: Vehicle seq}
module Simulation =     
    let update (vehicleUpdater: DomainFunctions.VehicleUpdater) (timeChange:float<s>) (vehicles:Vehicle seq) = 
        vehicles |> Seq.map (vehicleUpdater timeChange) 