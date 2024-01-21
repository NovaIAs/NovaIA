```c#
// Este programa es un simulador de un sistema operativo básico.
// Define una estructura de datos para representar los procesos.
struct Proceso
{
    int id;         // Identificador del proceso.
    int estado;     // Estado del proceso (0 = Nuevo, 1 = Ejecutando, 2 = Bloqueado).
    int prioridad;  // Prioridad del proceso (1 = Alta, 2 = Media, 3 = Baja).
    int tiempoEjecucion;  // Tiempo de ejecución restante del proceso.
}

// Define una estructura de datos para representar la memoria.
struct Memoria
{
    int tamaño;       // Tamaño total de la memoria.
    int espacioLibre;  // Espacio libre en la memoria.
    int[] procesos;    // Arreglo de identificadores de procesos que están en memoria.
}

// Define una estructura de datos para representar el disco.
struct Disco
{
    int tamaño;       // Tamaño total del disco.
    int espacioLibre;  // Espacio libre en el disco.
}

// Crea una nueva instancia de un proceso.
Proceso CrearProceso(int id, int prioridad, int tiempoEjecucion)
{
    Proceso proceso;
    proceso.id = id;
    proceso.estado = 0;
    proceso.prioridad = prioridad;
    proceso.tiempoEjecucion = tiempoEjecucion;
    return proceso;
}

// Crea una nueva instancia de la memoria.
Memoria CrearMemoria(int tamaño)
{
    Memoria memoria;
    memoria.tamaño = tamaño;
    memoria.espacioLibre = tamaño;
    memoria.procesos = new int[0];
    return memoria;
}

// Crea una nueva instancia del disco.
Disco CrearDisco(int tamaño)
{
    Disco disco;
    disco.tamaño = tamaño;
    disco.espacioLibre = tamaño;
    return disco;
}

// Ejecuta el proceso especificado.
void EjecutarProceso(Proceso proceso, Memoria memoria, Disco disco)
{
    if (proceso.estado == 1)
    {
        // El proceso ya está en ejecución.
        return;
    }

    // Comprueba si hay suficiente espacio en la memoria para ejecutar el proceso.
    if (memoria.espacioLibre < proceso.tiempoEjecucion)
    {
        // No hay suficiente espacio en la memoria.
        proceso.estado = 2;  // Bloquea el proceso.
        return;
    }

    // Carga el proceso en la memoria.
    memoria.procesos = memoria.procesos.Append(proceso.id).ToArray();
    memoria.espacioLibre -= proceso.tiempoEjecucion;

    // Ejecuta el proceso.
    proceso.estado = 1;  // Ejecutando el proceso.
    proceso.tiempoEjecucion -= 1;

    // Comprueba si el proceso ha terminado de ejecutarse.
    if (proceso.tiempoEjecucion <= 0)
    {
        // El proceso ha terminado de ejecutarse.
        proceso.estado = 0;  // Nuevo.

        // Libera el espacio que ocupaba el proceso en la memoria.
        memoria.espacioLibre += proceso.tiempoEjecucion;
    }
}

// Simula el sistema operativo.
void SimularSistemaOperativo(int numProcesos, int tamañoMemoria, int tamañoDisco)
{
    // Crea una lista de procesos.
    List<Proceso> procesos = new List<Proceso>();
    for (int i = 0; i < numProcesos; i++)
    {
        procesos.Add(CrearProceso(i, Random.Shared.Next(1, 4), Random.Shared.Next(1, 10)));
    }

    // Crea la memoria.
    Memoria memoria = CrearMemoria(tamañoMemoria);

    // Crea el disco.
    Disco disco = CrearDisco(tamañoDisco);

    // Ejecuta los procesos.
    while (procesos.Any(proceso => proceso.estado != 0))
    {
        // Obtiene el proceso con mayor prioridad que esté listo para ejecutarse.
        Proceso proceso = procesos.Where(proceso => proceso.estado == 0).OrderByDescending(proceso => proceso.prioridad).FirstOrDefault();

        // Ejecuta el proceso.
        EjecutarProceso(proceso, memoria, disco);
    }

    // Imprime los resultados de la simulación.
    Console.WriteLine("Resultados de la simulación:");
    Console.WriteLine($"Número de procesos: {numProcesos}");
    Console.WriteLine($"Tamaño de la memoria: {tamañoMemoria}");
    Console.WriteLine($"Tamaño del disco: {tamañoDisco}");
    Console.WriteLine($"Tiempo total de ejecución: {procesos.Sum(proceso => proceso.tiempoEjecucion)}");
}

// Punto de entrada del programa.
public static void Main(string[] args)
{
    // Simula el sistema operativo con 10 procesos, 100 MB de memoria y 1 GB de disco.
    SimularSistemaOperativo(10, 100, 1000);
}
```

Este programa crea una lista de procesos, una instancia de la memoria y una instancia del disco. Luego, ejecuta los procesos en un bucle, dando prioridad a los procesos con mayor prioridad. Después de que todos los procesos hayan terminado de ejecutarse, el programa imprime los resultados de la simulación.