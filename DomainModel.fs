namespace TrafficSimulator
    
[<AutoOpen>]
module DomainModel =    
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols    

    type Id = int
    type Line = { Id:Id}
    type Curve = float

    type Road = Straight of Line | Curved of Curve

    type Position2d= {X:float;Y:float}
    type Position = Position2d of Position2d

    type Crossing = {Name:option<string> ; Position:Position} 
   
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

    type Connection = {Start:Crossing;End:Crossing}
    type ConnectedRoad = {Road:Road;Connection:Connection}

    type VehicleLocation = {Placing:ConnectedRoad;CurrentProgress:Fraction}  
    type Vehicle = {Location:VehicleLocation;CurrentSpeed:float<m/s>}
       
    type Roads = list<ConnectedRoad>
          
       
 module DomainFunctions = 
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols    
    let distanceToProgress (distance:float<m>) (connectionLenght:float<m>) = 
        Progress.create (distance / connectionLenght)
    
    // let getPosition connection road progress
    //     = match road with
    //         | Curved -> 1
    //         | Straight -> 0
    // let updateProgress progress change = 
    //     Progress.create progress + change
    // let changeVehicleLocation vehicleLocation change= 
    //     {vehicleLocation with CurrentProgress = Progress.create }
    type RoadLenghtProvider = ConnectedRoad -> float<m>    
    type VehicleUpdater = Vehicle->Vehicle
    type VehicleLocationUpdater = VehicleLocation->VehicleLocation
    type ProgressTravelledCalculator = unit -> Progress.T

    let roadLenghtProvider road = 
        match road.Road with 
            |  Curved -> 0.0<m>
            |  Straight -> 0.0<m>

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
