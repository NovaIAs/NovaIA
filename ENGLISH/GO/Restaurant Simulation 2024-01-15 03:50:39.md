```go
package main

import (
    "fmt"
    "math/rand"
    "sync"
    "time"
)

// Constants representing the different types of events that can occur.
const (
    ARRIVAL = iota
    DEPARTURE
)

// Event represents an event that occurs at a particular time.
type Event struct {
    time    int       // The time at which the event occurs.
    eventType int       // The type of event (ARRIVAL or DEPARTURE).
    customer int       // The customer associated with the event.
    queue    *Queue    // The queue to which the customer belongs.
    server   *Server   // The server that is serving the customer.
    system   *System   // The system in which the event occurs.
)

// Queue represents a queue of customers waiting to be served.
type Queue struct {
    customers []*Customer // The customers in the queue.
    mutex     sync.Mutex   // A mutex to protect the queue from concurrent access.
}

// Server represents a server that serves customers.
type Server struct {
    customer *Customer // The customer that the server is currently serving.
    mutex     sync.Mutex   // A mutex to protect the server from concurrent access.
}

// System represents a system of queues and servers.
type System struct {
    queues    []*Queue  // The queues in the system.
    servers    []*Server // The servers in the system.
    customers  []*Customer // The customers in the system.
    numArrivals int       // The number of arrivals that have occurred.
    numDepartures int       // The number of departures that have occurred.
    totalWaitTime int       // The total amount of time that customers have spent waiting in queues.
}

// Customer represents a customer in the system.
type Customer struct {
    id       int // The unique ID of the customer.
    arrivalTime int // The time at which the customer arrived in the system.
    serviceTime int // The amount of time that the customer requires service.
}

// NewSystem creates a new system with the specified number of queues and servers.
func NewSystem(numQueues, numServers int) *System {
    system := &System{
        queues:    make([]*Queue, numQueues),
        servers:    make([]*Server, numServers),
        customers: make([]*Customer, 0),
    }
    for i := 0; i < numQueues; i++ {
        system.queues[i] = &Queue{}
    }
    for i := 0; i < numServers; i++ {
        system.servers[i] = &Server{}
    }
    return system
}

// AddCustomer adds a new customer to the system.
func (system *System) AddCustomer(customer *Customer) {
    system.customers = append(system.customers, customer)
}

// GenerateArrivals generates a sequence of arrival events.
func GenerateArrivals(system *System, arrivalRate float64) {
    for i := 0; i < 1000; i++ {
        // Generate the arrival time.
        arrivalTime := int(rand.ExpFloat64() / arrivalRate)

        // Create a new customer.
        customer := &Customer{
            id:       i,
            arrivalTime: arrivalTime,
            serviceTime: int(rand.ExpFloat64() * 10),
        }

        // Add the customer to the system.
        system.AddCustomer(customer)

        // Create an arrival event.
        event := &Event{
            time:    arrivalTime,
            eventType: ARRIVAL,
            customer: customer,
            queue:    nil,
            server:   nil,
            system:   system,
        }

        // Add the event to the system.
        system.AddEvent(event)
    }
}

// AddEvent adds an event to the system.
func (system *System) AddEvent(event *Event) {
    // Insert the event into the system's event queue.
    // The event queue is sorted by time.
    for i, e := range system.events {
        if event.time < e.time {
            system.events = append(system.events[:i], append([]*Event{event}, system.events[i:]...)...)
            return
        }
    }
    system.events = append(system.events, event)
}

// ProcessEvents processes the events in the system's event queue.
func (system *System) ProcessEvents() {
    for len(system.events) > 0 {
        // Get the next event from the event queue.
        event := system.events[0]
        system.events = system.events[1:]

        // Process the event.
        switch event.eventType {
        case ARRIVAL:
            // If there is a server available, assign the customer to the server.
            if system.HasAvailableServer() {
                system.AssignCustomerToServer(event.customer)
            } else {
                // Otherwise, add the customer to the queue.
                system.AddCustomerToQueue(event.customer)
            }
            break
        case DEPARTURE:
            // Remove the customer from the server.
            system.RemoveCustomerFromServer(event.customer)

            // If there is a customer in the queue, assign the customer to the server.
            if system.HasCustomerInQueue() {
                system.AssignCustomerToServer(system.NextCustomerInQueue())
            }
            break
        }
    }
}

// HasAvailableServer returns true if there is an available server in the system.
func (system *System) HasAvailableServer() bool {
    for _, server := range system.servers {
        if server.customer == nil {
            return true
        }
    }
    return false
}

// AssignCustomerToServer assigns a customer to a server.
func (system *System) AssignCustomerToServer(customer *Customer) {
    for _, server := range system.servers {
        if server.customer == nil {
            server.customer = customer
            customer.serviceStartTime = system.currentTime
            break
        }
    }
}

// AddCustomerToQueue adds a customer to the queue.
func (system *System) AddCustomerToQueue(customer *Customer) {
    // Find the shortest queue.
    shortestQueue := system.queues[0]
    for _, queue := range system.queues {
        if len(queue.customers) < len(shortestQueue.customers) {
            shortestQueue = queue
        }
    }

    // Add the customer to the shortest queue.
    shortestQueue.customers = append(shortestQueue.customers, customer)
}

// HasCustomerInQueue returns true if there is at least one customer in the queue.
func (system *System) HasCustomerInQueue() bool {
    for _, queue := range system.queues {
        if len(queue.customers) > 0 {
            return true
        }
    }
    return false
}

// NextCustomerInQueue gets the next customer in the queue.
func (system *System) NextCustomerInQueue() *Customer {
    // Find the shortest queue.
    shortestQueue := system.queues[0]
    for _, queue := range system.queues {
        if len(queue.customers) < len(shortestQueue.customers) {
            shortestQueue = queue
        }
    }

    // Get the next customer in the shortest queue.
    customer := shortestQueue.customers[0]
    shortestQueue.customers = shortestQueue.customers[1:]

    return customer
}

// RemoveCustomerFromServer removes a customer from a server.
func (system *System) RemoveCustomerFromServer(customer *Customer) {
    for _, server := range system.servers {
        if server.customer == customer {
            server.customer = nil
            break