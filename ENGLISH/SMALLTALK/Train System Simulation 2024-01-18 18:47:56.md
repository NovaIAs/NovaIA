```smalltalk
Class RailwayCar superclass: Object
    instanceVariableNames: 'carNumber seatingCapacity'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class RailroadTrack superclass: Object
    instanceVariableNames: 'trackNumber trainCount'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class Locomotive superclass: RailwayCar
    instanceVariableNames: 'engineNumber'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class PassengerCar superclass: RailwayCar
    instanceVariableNames: 'numberOfPassengers'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class FreightCar superclass: RailwayCar
    instanceVariableNames: 'freightTons'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class Train superclass: Object
    instanceVariableNames: 'locomotive numberOfCars totalCars passengers passengersAboard'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class TrainStation superclass: Object
    instanceVariableNames: 'stationName trackCount trains'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Class TrainController superclass: Object
    instanceVariableNames: 'trains stations'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

Object subclass: #main
    instanceVariableNames: 'theTrainController'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Simulation'

main>>startUp
    ^ self new startUp

main>>startUp
    self createTrainController.
    self createStations.
    self createInitialTrains.
    self runSimulation.

main>>createTrainController
    self theTrainController := TrainController new.

main>>createStations
    | station |
    station := TrainStation new stationName: 'Grand Central Station'.
    self theTrainController stations add: station.
    station := TrainStation new stationName: 'Penn Station'.
    self theTrainController stations add: station.
    station := TrainStation new stationName: '30th Street Station'.
    self theTrainController stations add: station.
    station := TrainStation new stationName: 'Union Station'.
    self theTrainController stations add: station.

main>>createInitialTrains
    | station train trainNumber stationIndex |
    stationIndex := 1.
    [stationIndex <= 4] whileTrue:
        [ station := self theTrainController stations at: stationIndex.
          self createTrainAt: station trainNumber: stationIndex ].
    ^ self.

main>>createTrainAt: aStation trainNumber: aTrainNumber
    | train locomotive |
    locomotive := Locomotive new engineNumber: aTrainNumber.
    train := Train new.
    train locomotive: locomotive.
    train addPassengers: 10.
    aStation addTrain: train.
    self theTrainController addTrain: train.
    ^ self.

main>>runSimulation
    | i timer running |
    i := 1.
    timer := SmalltalkTimer new seconds: 1.
    running := true.
    [running] whileTrue:
        [ self displaySimulation.
          timer start.
          timer wait.
          running := (self updateSimulation and: [i <= 20]) ].
    ^ self.

main>>updateSimulation
    | stationIndex train |
    stationIndex := 1.
    [stationIndex <= 4] whileTrue:
        [ station := self theTrainController stations at: stationIndex.
          train := station trains first.
          train moveTrain.
          stationIndex := stationIndex + 1 ].
    ^ true.

main>>displaySimulation
    | stationIndex station train |
    stationIndex := 1.
    [stationIndex <= 4] whileTrue:
        [ station := self theTrainController stations at: stationIndex.
          train := station trains first.
          Transcript show: 'Station: ', station stationName, '. Train: ', train locomotive engineNumber, ' with ', train totalCars, ' cars and ', train passengersAboard, ' passengers.' asString; cr].
    ^ self.

main>>addPassengers: aNumber
    | i |
    i := 1.
    [i <= aNumber] whileTrue:
        [ self passengers add: Passenger new ].
    ^ self.

Train>>initialize
    self inherited.
    self totalCars := 0.
    self passengers := OrderedCollection new.
    self passengersAboard := 0.

Train>>locomotive
    ^ locomotive.

Train>>locomotive: locomotive
    locomotive := locomotive.

Train>>addPassenger
    passengers add: Passenger new.

Train>>addPassengers: aNumber
    | i |
    i := 1.
    [i <= aNumber] whileTrue:
        [ self addPassenger ].
    ^ self.

Train>>totalCars
    ^ totalCars.

Train>>totalCars: aNumber
    self totalCars := aNumber.

Train>>passengers
    ^ passengers.

Train>>passengers: passengers
    passengers := passengers.

Train>>passengersAboard
    ^ passengersAboard.

Train>>passengersAboard: aNumber
    self passengersAboard := aNumber.

Train>>moveTrain
    | station |
    station := self locomotive track;
    self locomotive track: station nextTrack.
    station removeTrain: self.
    self locomotive track trainCount: self locomotive track trainCount + 1.
    ^ self.

RailwayCar>>initialize
    self inherited.
    self carNumber := Random random.
    self seatingCapacity := Random random * 100.

RailwayCar>>carNumber
    ^ carNumber.

RailwayCar>>carNumber: aNumber
    carNumber := aNumber.

RailwayCar>>seatingCapacity
    ^ seatingCapacity.

RailwayCar>>seatingCapacity: aNumber
    seatingCapacity := aNumber.

Locomotive>>initialize
    super initialize.
    self engineNumber := Random random.

Locomotive>>engineNumber
    ^ engineNumber.

Locomotive>>engineNumber: aNumber
    engineNumber := aNumber.

PassengerCar>>initialize
    super initialize.
    self numberOfPassengers := 0.

PassengerCar>>numberOfPassengers
    ^ numberOfPassengers.

PassengerCar>>numberOfPassengers: aNumber
    numberOfPassengers := aNumber.

FreightCar>>initialize
    super initialize.
    self freightTons := 0.

FreightCar>>freightTons
    ^ freightTons.

FreightCar>>freightTons: aNumber
    freightTons := aNumber.

RailroadTrack>>initialize
    self inherited.
    self trackNumber := Random random.
    self trainCount := 0.

RailroadTrack>>trackNumber
    ^ trackNumber.

RailroadTrack>>trackNumber: aNumber
    trackNumber := aNumber.

RailroadTrack>>trainCount
    ^ trainCount.

RailroadTrack>>trainCount: aNumber
    trainCount := aNumber.

RailroadTrack>>nextTrack
    | nextTrack |
    tracks := OrderedCollection new.
    track := self.
    [track isNil] whileFalse:
        [ tracks add: track.
          track := track nextTrack ].
    nextTrack := tracks at: (tracks size + 1) mod tracks size.
    ^ nextTrack.

TrainStation>>initialize
    self inherited.
    self stationName := ''.
    self trackCount := 0.
    self trains := OrderedCollection new.

TrainStation>>stationName
    ^ stationName.

TrainStation>>stationName: aString
    stationName := aString.

TrainStation>>trackCount
    ^ trackCount.

TrainStation>>trackCount: aNumber
    trackCount := aNumber.

TrainStation>>trains
    ^ trains.

TrainStation>>trains: trains
    trains := trains.

TrainStation>>addTrain: aTrain
    trains add: aTrain.

TrainStation>>removeTrain: aTrain
    trains remove: aTrain.

TrainController>>initialize
    self inherited.
    self trains := OrderedCollection new.
    self stations := OrderedCollection new.

TrainController>>trains
    ^ trains.

TrainController>>trains: trains
    trains := trains.

TrainController>>stations
    ^ stations.

TrainController>>stations: stations
    stations := stations.

TrainController>>addTrain: aTrain
    trains add: aTrain.

Passenger>>initialize
    self inherited.

Passenger>>passengersAboard: aNumber
    self passengersAboard := aNumber.

SmalltalkTimer>>initialize
    self inherited.
    self seconds := 0.
    self timer := nil.

```

This code simulates a train system with stations, trains, and tracks. It includes classes for RailwayCar, RailroadTrack, Locomotive, PassengerCar, FreightCar, Train, TrainStation, and TrainController. The main class, `main` initializes the simulation and creates train controllers, stations, trains, and passengers. The `runSimulation` method runs the simulation for a specified number of seconds, updating the positions of trains and displaying the simulation state. Overall, this code provides a detailed simulation of a train system, allowing for the creation of various scenarios and the observation of train movements.