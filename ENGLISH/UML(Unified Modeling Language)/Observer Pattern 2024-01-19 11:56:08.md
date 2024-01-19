```
+----------------------------------------------------------------------+
|                                 <<interface>>                         |
|                                                                      |
|  +------------------------+  +------------------------+  +---------------------+ |
|  | ISubject              |  | IObserver              |  | IDisplayElement    | |
|  +------------------------+  +------------------------+  +---------------------+ |
|  | + observe ------------+ |  | + update -------------+ |  | + display ---------+ |
|  |                       | |                          | |                      | |
|  +------------------------+ |  +------------------------+ |  +---------------------+ |
|                                                                      |
+----------------------------------------------------------------------+
                                                                        |
                                        +----------------------------------+
                                        |                                   |
                                        |  WeatherStationWithObserver       |
                                        |                                   |
                                        +----------------------------------+
                                                |
                                                v
+----------------------------------------------------------------------+
|                                                                      |
|  +---------------------+  +---------------------+  +---------------------+ |
|  | CurrentConditions   |  | StatisticsDisplay   |  | ForecastDisplay      | |
|  +---------------------+  +---------------------+  +---------------------+ |
|  | + update(temp, hum, pres)|  | + update(temp, hum, pres)|  | + update(temp, hum, pres)| |
|  | + display             |  | + display             |  | + display             | |
|  +---------------------+  +---------------------+  +---------------------+ |
|                                                                      |
+----------------------------------------------------------------------+

```

Explanation:

* **ISubject**: This is an interface that defines the methods that all subjects must implement. In this case, the subject is the weather station.
* **IObserver**: This is an interface that defines the methods that all observers must implement. In this case, the observers are the current conditions display, the statistics display, and the forecast display.
* **IDisplayElement**: This is an interface that defines the method that all display elements must implement. In this case, the display elements are the current conditions display, the statistics display, and the forecast display.
* **WeatherStationWithObserver**: This is the main class that creates the weather station and the observers. It also sets up the relationships between the weather station and the observers.
* **CurrentConditions**: This is a class that implements the IObserver interface. It displays the current temperature, humidity, and pressure.
* **StatisticsDisplay**: This is a class that implements the IObserver interface. It displays the average, maximum, and minimum temperature, humidity, and pressure.
* **ForecastDisplay**: This is a class that implements the IObserver interface. It displays the forecast for the next 24 hours.

The code works as follows:

1. The weather station creates the observers.
2. The weather station registers the observers with itself.
3. The weather station takes measurements of the temperature, humidity, and pressure.
4. The weather station notifies the observers of the new measurements.
5. The observers update their displays with the new measurements.