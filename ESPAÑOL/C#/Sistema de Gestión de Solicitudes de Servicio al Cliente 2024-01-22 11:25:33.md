```c#
// Objetivo: Crear un software que ayude a gestionar las solicitudes de servicio al cliente de una empresa.

// Clase para representar las solicitudes de servicio al cliente
public class Solicitud
{
    // Propiedades de la solicitud
    public int Id { get; set; }
    public string Cliente { get; set; }
    public string Descripcion { get; set; }
    public DateTime FechaSolicitud { get; set; }
    public DateTime FechaVencimiento { get; set; }
    public string Estado { get; set; }
    public string Prioridad { get; set; }
    public string AsignadoA { get; set; }
    public string Notas { get; set; }

    // Constructor de la clase
    public Solicitud(int id, string cliente, string descripcion, DateTime fechaSolicitud, DateTime fechaVencimiento, string estado, string prioridad, string asignadoA, string notas)
    {
        Id = id;
        Cliente = cliente;
        Descripcion = descripcion;
        FechaSolicitud = fechaSolicitud;
        FechaVencimiento = fechaVencimiento;
        Estado = estado;
        Prioridad = prioridad;
        AsignadoA = asignadoA;
        Notas = notas;
    }
}

// Clase para representar el repositorio de solicitudes de servicio al cliente
public class SolicitudRepositorio
{
    // Método para obtener todas las solicitudes
    public List<Solicitud> ObtenerTodas()
    {
        // Obtener las solicitudes de la base de datos o de un servicio web
        List<Solicitud> solicitudes = new List<Solicitud>();
        return solicitudes;
    }

    // Método para obtener una solicitud por su Id
    public Solicitud ObtenerPorId(int id)
    {
        // Obtener la solicitud de la base de datos o de un servicio web
        Solicitud solicitud = new Solicitud();
        return solicitud;
    }

    // Método para crear una nueva solicitud
    public void Crear(Solicitud solicitud)
    {
        // Insertar la solicitud en la base de datos o en un servicio web
    }

    // Método para actualizar una solicitud existente
    public void Actualizar(Solicitud solicitud)
    {
        // Actualizar la solicitud en la base de datos o en un servicio web
    }

    // Método para eliminar una solicitud
    public void Eliminar(int id)
    {
        // Eliminar la solicitud de la base de datos o de un servicio web
    }
}

// Clase para representar el servicio de solicitudes de servicio al cliente
public class SolicitudServicio
{
    // Método para obtener todas las solicitudes
    public List<Solicitud> ObtenerTodas()
    {
        // Obtener las solicitudes del repositorio
        SolicitudRepositorio repositorio = new SolicitudRepositorio();
        List<Solicitud> solicitudes = repositorio.ObtenerTodas();
        return solicitudes;
    }

    // Método para obtener una solicitud por su Id
    public Solicitud ObtenerPorId(int id)
    {
        // Obtener la solicitud del repositorio
        SolicitudRepositorio repositorio = new SolicitudRepositorio();
        Solicitud solicitud = repositorio.ObtenerPorId(id);
        return solicitud;
    }

    // Método para crear una nueva solicitud
    public void Crear(Solicitud solicitud)
    {
        // Crear la solicitud en el repositorio
        SolicitudRepositorio repositorio = new SolicitudRepositorio();
        repositorio.Crear(solicitud);
    }

    // Método para actualizar una solicitud existente
    public void Actualizar(Solicitud solicitud)
    {
        // Actualizar la solicitud en el repositorio
        SolicitudRepositorio repositorio = new SolicitudRepositorio();
        repositorio.Actualizar(solicitud);
    }

    // Método para eliminar una solicitud
    public void Eliminar(int id)
    {
        // Eliminar la solicitud del repositorio
        SolicitudRepositorio repositorio = new SolicitudRepositorio();
        repositorio.Eliminar(id);
    }
}

// Clase para representar la interfaz de usuario del software
public class InterfazUsuario
{
    // Método para mostrar el menú principal del software
    public void MostrarMenuPrincipal()
    {
        Console.WriteLine("Menú principal");
        Console.WriteLine("1. Listar solicitudes");
        Console.WriteLine("2. Crear una nueva solicitud");
        Console.WriteLine("3. Actualizar una solicitud existente");
        Console.WriteLine("4. Eliminar una solicitud");
        Console.WriteLine("5. Salir");
        Console.Write("Seleccione una opción: ");
        int opcion = int.Parse(Console.ReadLine());

        switch (opcion)
        {
            case 1:
                MostrarListaSolicitudes();
                break;
            case 2:
                CrearNuevaSolicitud();
                break;
            case 3:
                ActualizarSolicitudExistente();
                break;
            case 4:
                EliminarSolicitud();
                break;
            case 5:
                Salir();
                break;
            default:
                Console.WriteLine("Opción no válida");
                break;
        }
    }

    // Método para mostrar la lista de solicitudes
    public void MostrarListaSolicitudes()
    {
        // Obtener la lista de solicitudes del servicio
        SolicitudServicio servicio = new SolicitudServicio();
        List<Solicitud> solicitudes = servicio.ObtenerTodas();

        // Mostrar la lista de solicitudes en la consola
        Console.WriteLine("Lista de solicitudes");
        foreach (Solicitud solicitud in solicitudes)
        {
            Console.WriteLine($"Id: {solicitud.Id}");
            Console.WriteLine($"Cliente: {solicitud.Cliente}");
            Console.WriteLine($"Descripción: {solicitud.Descripcion}");
            Console.WriteLine($"Fecha de solicitud: {solicitud.FechaSolicitud}");
            Console.WriteLine($"Fecha de vencimiento: {solicitud.FechaVencimiento}");
            Console.WriteLine($"Estado: {solicitud.Estado}");
            Console.WriteLine($"Prioridad: {solicitud.Prioridad}");
            Console.WriteLine($"Asignado a: {solicitud.AsignadoA}");
            Console.WriteLine($"Notas: {solicitud.Notas}");
            Console.WriteLine("------------------------------------------------------");
        }
    }

    // Método para crear una nueva