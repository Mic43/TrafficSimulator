namespace TrafficSimulator

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols     
[<AutoOpen>]
module DomainModel =    

    type CrossingId = CrossingId of int    
    type Curve = float

    type ConnectionType = Linear | Curved of Curve

    type Position2d= {X:float;Y:float}
    type Position = Position2d of Position2d 
    module Position = 
        let distance (point:Position) (point2:Position) =
            let p = (point,point2)  
            match p with
                | (Position2d p1,Position2d p2) -> sqrt ((p1.X - p2.X) * (p1.X - p2.X) - (p1.Y - p2.Y) * (p1.Y - p2.Y)) 
                
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

    type Connection = {ConnectionType:ConnectionType;StartId:CrossingId;EndId:CrossingId}

    type VehicleLocation = {Placing:Connection;CurrentProgress:Fraction}  
    type Vehicle = {Location:VehicleLocation;CurrentSpeed:float<m/s>}

    type ConnectionsGraph = private { Crossings:Map<CrossingId,Crossing>; CrossingsOutput: Map<CrossingId,Connection seq>;} 
    module ConnectionsGraph =         
        let create (crossings: Map<CrossingId,Crossing>) (connections: Connection seq)  = 
            if not (connections |> Seq.forall (fun connection -> crossings.ContainsKey connection.StartId && crossings.ContainsKey connection.EndId))
                then None
                else
                    let crossingOut = connections |> Seq.groupBy (fun con -> con.StartId ) |> Map.ofSeq
                    Some({Crossings = crossings;CrossingsOutput = crossingOut})
        let crossings connectionsGraph = connectionsGraph.Crossings
        let crossing connectionsGraph crossingId = connectionsGraph.Crossings  |> Map.find  crossingId    
        let crossingOutputs connectionsGraph crossingId = connectionsGraph.CrossingsOutput.Item crossingId

   // type Roads = list<ConnectedRoad>
  //  let a = dict<Road>      
       
 module DomainFunctions = 
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols    
    let distanceToProgress (distance:float<m>) (connectionLenght:float<m>) = 
        Progress.create (distance / connectionLenght)
    
    type ConnectionLenghtProvider = Connection -> float<m>    
    type VehicleUpdater = float<s>->Vehicle->Vehicle
    type VehicleLocationUpdater = VehicleLocation->VehicleLocation
    type ProgressTravelledCalculator = unit -> Progress
    type VehiclesUpdater = float<s>->Vehicle seq->Vehicle seq

    module LenghtProviders = 
        let constantLenghtProvider (connectionsGraph:ConnectionsGraph) connection  =
            match connection.ConnectionType with 
                |  Curved -> 100.0<m>
                |  Linear -> 100.0<m>
        let lenghtProvider (distancePerUnit:float<m>) (connectionsGraph:ConnectionsGraph) connection  =
            match connection.ConnectionType with 
                |  Curved -> failwith "Not implemented"
                |  Linear -> let endC = ConnectionsGraph.crossing connectionsGraph connection.EndId
                             let startC = ConnectionsGraph.crossing connectionsGraph connection.StartId                             
                             let distance = Position.distance endC.Position startC.Position 
                             distance * distancePerUnit
    let progressTravelledCalculator roadLenghtProvider vehicle timeChange  = 
        let distanceTravelled = vehicle.CurrentSpeed * timeChange
        let roadLenght = (roadLenghtProvider vehicle.Location.Placing)
        distanceToProgress distanceTravelled roadLenght

    module VehicleLocationUpdaters = 
        let simpleLocationUpdater  (progressTravelled: Progress) (vehicleLocation:VehicleLocation)= 
            let newProgres = Progress.add (Progress.fromFraction vehicleLocation.CurrentProgress) progressTravelled      
            {vehicleLocation with CurrentProgress = newProgres.Fractions}

        let locationUpdater (connectionsGraph:ConnectionsGraph) (progressTravelled: Progress) (vehicleLocation:VehicleLocation)= 
            let newProgres = Progress.add (Progress.fromFraction vehicleLocation.CurrentProgress) progressTravelled      
            let destination = ConnectionsGraph.crossingOutputs connectionsGraph vehicleLocation.Placing.EndId |> Seq.tryHead
            let newPlacing = if newProgres.Unities < 1      
                                then vehicleLocation.Placing 
                                else destination |> Option.defaultValue vehicleLocation.Placing
            {CurrentProgress = newProgres.Fractions;Placing = newPlacing}

    let simpleVehicleUpdater (vehicleLocationUpdater:VehicleLocationUpdater) vehicle =  
        let newVehicleLocation = vehicleLocationUpdater vehicle.Location
        {vehicle with Location = newVehicleLocation}

    module VehiclesUpdaters =     
        let update (vehicleUpdater: VehicleUpdater) (timeChange:float<s>) (vehicles:Vehicle seq) = 
            vehicles |> Seq.map (vehicleUpdater timeChange) 
        let updateByPlacing (vehiclesUpdater: VehiclesUpdater) (timeChange:float<s>) (vehicles:Vehicle seq) = 
            vehicles |> Seq.groupBy (fun v -> v.Location.Placing) 
                     |> Seq.map (fun (_,vehicles) ->  vehicles |> Seq.sortByDescending (fun v->v.Location.CurrentProgress)) 
                     |> Seq.map (fun vehiclesOnSameConnection -> vehiclesOnSameConnection |> vehiclesUpdater timeChange)
                     |> Seq.fold (fun acc cur -> acc |> Seq.append cur) Seq.empty
                                                                
    type Simulation = {ConnectionsGraph:ConnectionsGraph; Vehicles: Vehicle seq}

    