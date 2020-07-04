namespace TrafficSimulator
    
[<AutoOpen>]
module DomainModel =    
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols    

    type CrossingId = CrossingId of int    
    type Curve = float

    type ConnectionType = Linear | Curved of Curve

    type Position2d= {X:float;Y:float}
    type Position = Position2d of Position2d

    type Crossing = { Name:option<string> ; Position:Position} 
   
    type Fraction = private Fraction of float
    
    module Progress =
        type T = {Unities:int;Fractions:Fraction}
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

   // type Connection = {Start:Crossing;End:Crossing}
    type Connection = {ConnectionType:ConnectionType;Start:CrossingId;End:CrossingId}

    type VehicleLocation = {Placing:Connection;CurrentProgress:Fraction}  
    type Vehicle = {Location:VehicleLocation;CurrentSpeed:float<m/s>}

    module ConnectionsGraph = 
        type T = private { Crossings:Map<CrossingId,Crossing>; CrossingsOutput: Map<CrossingId,Connection seq>;} 
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
    type VehicleUpdater = Vehicle->Vehicle
    type VehicleLocationUpdater = VehicleLocation->VehicleLocation
    type ProgressTravelledCalculator = unit -> Progress.T

    let roadLenghtProvider road = 
        match road.ConnectionType with 
            |  Curved -> 0.0<m>
            |  Linear -> 0.0<m>

    let progressTravelledCalculator roadLenghtProvider vehicle timeChange  = 
        let distanceTravelled = vehicle.CurrentSpeed * timeChange
        let roadLenght = (roadLenghtProvider vehicle.Location.Placing)
        distanceToProgress distanceTravelled roadLenght

    let simpleLocationUpdater  (progressTravelled: Progress.T) (vehicleLocation:VehicleLocation)= 
        let newProgres = Progress.add (Progress.fromFraction vehicleLocation.CurrentProgress) progressTravelled
        {vehicleLocation with CurrentProgress = newProgres.Fractions}


    let simpleVehicleUpdater (vehicleLocationUpdater:VehicleLocationUpdater) vehicle =  
        let newVehicleLocation = vehicleLocationUpdater vehicle.Location
        {vehicle with Location = newVehicleLocation}
