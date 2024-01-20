```C#
// Importación de bibliotecas de C#
using System;
using System.Collections.Generic;
using System.Linq;

// Definición de una clase "Tarea" con propiedades de nombre y estado.
public class Tarea
{
    public string Nombre { get; set; }
    public bool Estado { get; set; }

    // Constructor de la clase "Tarea".
    public Tarea(string nombre)
    {
        Nombre = nombre;
        Estado = false;
    }
}

// Definición de una clase "ListaDeTareas" que maneja una lista de tareas.
public class ListaDeTareas
{
    // Lista de tareas interna de la clase "ListaDeTareas".
    private List<Tarea> _tareas = new List<Tarea>();

    // Propiedad de la lista de tareas.
    public List<Tarea> Tareas
    {
        get { return _tareas; }
    }

    // Método para agregar una tarea a la lista.
    public void AgregarTarea(Tarea tarea)
    {
        _tareas.Add(tarea);
    }

    // Método para eliminar una tarea de la lista.
    public void EliminarTarea(Tarea tarea)
    {
        _tareas.Remove(tarea);
    }

    // Método para marcar una tarea como completada.
    public void MarcarCompletada(Tarea tarea)
    {
        tarea.Estado = true;
    }

    // Método para obtener todas las tareas completadas.
    public List<Tarea> ObtenerCompletadas()
    {
        return _tareas.Where(tarea => tarea.Estado == true).ToList();
    }

    // Método para imprimir las tareas pendientes.
    public void ImprimirPendientes()
    {
        Console.WriteLine("Tareas pendientes:");
        foreach (Tarea tarea in _tareas)
        {
            if (!tarea.Estado)
            {
                Console.WriteLine(tarea.Nombre);
            }
        }
    }
}

// Main Class.
public class Program
{
    // Método principal del programa "Main".
    public static void Main(string[] args)
    {
        // Creamos una nueva lista de tareas.
        ListaDeTareas listaDeTareas = new ListaDeTareas();

        // Añadimos algunas tareas a la lista.
        listaDeTareas.AgregarTarea(new Tarea("Comprar comida"));
        listaDeTareas.AgregarTarea(new Tarea("Ir al gimnasio"));
        listaDeTareas.AgregarTarea(new Tarea("Estudiar para el examen"));

        // Imprimimos las tareas pendientes.
        Console.WriteLine("Tareas pendientes:");
        listaDeTareas.ImprimirPendientes();

        // Marcamos una tarea como completada.
        listaDeTareas.MarcarCompletada(listaDeTareas.Tareas[0]);

        // Obtenemos todas las tareas completadas.
        List<Tarea> tareasCompletadas = listaDeTareas.ObtenerCompletadas();

        // Imprimimos las tareas completadas.
        Console.WriteLine("Tareas completadas:");
        foreach (Tarea tarea in tareasCompletadas)
        {
            Console.WriteLine(tarea.Nombre);
        }
    }
}
```

Explicación del Código:

El código define una clase "Tarea" con propiedades de nombre y estado. Esta clase representa una tarea individual con un nombre y un estado que indica si la tarea está completada o no.

Luego define una clase "ListaDeTareas" que maneja una lista de tareas. Esta clase tiene una propiedad "_tareas" que es una lista de objetos "Tarea".

La clase "ListaDeTareas" tiene varios métodos para agregar, eliminar, marcar como completadas y obtener todas las tareas completadas.

En la clase "Program", creamos una nueva instancia de "ListaDeTareas" y añadimos algunas tareas a la lista.

Luego imprimimos las tareas pendientes y marcamos una tarea como completada.

Finalmente, obtenemos todas las tareas completadas y las imprimimos.

Este código es un ejemplo de cómo se puede usar una clase para representar y manipular una lista de objetos.