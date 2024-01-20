```swift
// Definición de una clase compleja en SWIFT
class SistemaDeViajes {

    // Propiedades
    private var vuelos: [Vuelo] = []
    private var hoteles: [Hotel] = []
    private var clientes: [Cliente] = []

    // Métodos
    func agregarVuelo(vuelo: Vuelo) {
        vuelos.append(vuelo)
    }

    func agregarHotel(hotel: Hotel) {
        hoteles.append(hotel)
    }

    func agregarCliente(cliente: Cliente) {
        clientes.append(cliente)
    }

    func buscarVuelo(origen: String, destino: String, fechaDePartida: Date) -> [Vuelo] {
        return vuelos.filter { $0.origen == origen && $0.destino == destino && $0.fechaDePartida == fechaDePartida }
    }

    func buscarHotel(ciudad: String, fechaDeLlegada: Date, fechaDeSalida: Date) -> [Hotel] {
        return hoteles.filter { $0.ciudad == ciudad && $0.fechaDeLlegada >= fechaDeLlegada && $0.fechaDeSalida <= fechaDeSalida }
    }

    func reservarVuelo(cliente: Cliente, vuelo: Vuelo) {
        cliente.vuelosReservados.append(vuelo)
        vuelo.pasajeros.append(cliente)
    }

    func reservarHotel(cliente: Cliente, hotel: Hotel) {
        cliente.hotelesReservados.append(hotel)
        hotel.huéspedes.append(cliente)
    }

    func cancelarVuelo(cliente: Cliente, vuelo: Vuelo) {
        cliente.vuelosReservados.remove(vuelo)
        vuelo.pasajeros.remove(cliente)
    }

    func cancelarHotel(cliente: Cliente, hotel: Hotel) {
        cliente.hotelesReservados.remove(hotel)
        hotel.huéspedes.remove(cliente)
    }
}

// Definición de una clase que representa un vuelo
class Vuelo {

    // Propiedades
    let origen: String
    let destino: String
    let fechaDePartida: Date
    let fechaDeLlegada: Date
    var pasajeros: [Cliente] = []

    // Constructor
    init(origen: String, destino: String, fechaDePartida: Date, fechaDeLlegada: Date) {
        self.origen = origen
        self.destino = destino
        self.fechaDePartida = fechaDePartida
        self.fechaDeLlegada = fechaDeLlegada
    }
}

// Definición de una clase que representa un hotel
class Hotel {

    // Propiedades
    let ciudad: String
    let fechaDeLlegada: Date
    let fechaDeSalida: Date
    var huéspedes: [Cliente] = []

    // Constructor
    init(ciudad: String, fechaDeLlegada: Date, fechaDeSalida: Date) {
        self.ciudad = ciudad
        self.fechaDeLlegada = fechaDeLlegada
        self.fechaDeSalida = fechaDeSalida
    }
}

// Definición de una clase que representa un cliente
class Cliente {

    // Propiedades
    let nombre: String
    let apellido: String
    var vuelosReservados: [Vuelo] = []
    var hotelesReservados: [Hotel] = []

    // Constructor
    init(nombre: String, apellido: String) {
        self.nombre = nombre
        self.apellido = apellido
    }
}

// Uso del sistema de viajes

// Crear un nuevo sistema de viajes
let sistemaDeViajes = SistemaDeViajes()

// Agregar algunos vuelos al sistema
sistemaDeViajes.agregarVuelo(vuelo: Vuelo(origen: "Madrid", destino: "Barcelona", fechaDePartida: Date(), fechaDeLlegada: Date()))
sistemaDeViajes.agregarVuelo(vuelo: Vuelo(origen: "Barcelona", destino: "París", fechaDePartida: Date(), fechaDeLlegada: Date()))

// Agregar algunos hoteles al sistema
sistemaDeViajes.agregarHotel(hotel: Hotel(ciudad: "Madrid", fechaDeLlegada: Date(), fechaDeSalida: Date()))
sistemaDeViajes.agregarHotel(hotel: Hotel(ciudad: "Barcelona", fechaDeLlegada: Date(), fechaDeSalida: Date()))

// Agregar algunos clientes al sistema
sistemaDeViajes.agregarCliente(cliente: Cliente(nombre: "Juan", apellido: "García"))
sistemaDeViajes.agregarCliente(cliente: Cliente(nombre: "María", apellido: "Pérez"))

// Buscar un vuelo desde Madrid a Barcelona con fecha de partida hoy
let vuelosMadridBarcelona = sistemaDeViajes.buscarVuelo(origen: "Madrid", destino: "Barcelona", fechaDePartida: Date())

// Imprimir los detalles del primer vuelo encontrado
if let vueloMadridBarcelona = vuelosMadridBarcelona.first {
    print("Origen:", vueloMadridBarcelona.origen)
    print("Destino:", vueloMadridBarcelona.destino)
    print("Fecha de partida:", vueloMadridBarcelona.fechaDePartida)
    print("Fecha de llegada:", vueloMadridBarcelona.fechaDeLlegada)
}

// Reservar el primer vuelo encontrado para el primer cliente
sistemaDeViajes.reservarVuelo(cliente: sistemaDeViajes.clientes.first!, vuelo: vuelosMadridBarcelona