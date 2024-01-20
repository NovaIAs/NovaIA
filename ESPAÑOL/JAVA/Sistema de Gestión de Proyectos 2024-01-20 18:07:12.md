```java
import java.util.*;
import java.io.*;

public class SistemaDeGestiónDeProyectos {

    private List<Proyecto> proyectos;
    private List<Tarea> tareas;
    private List<Recurso> recursos;
    private List<Asignación> asignaciones;

    public SistemaDeGestiónDeProyectos() {
        this.proyectos = new ArrayList<>();
        this.tareas = new ArrayList<>();
        this.recursos = new ArrayList<>();
        this.asignaciones = new ArrayList<>();
    }

    public void añadirProyecto(Proyecto proyecto) {
        this.proyectos.add(proyecto);
    }

    public void añadirTarea(Tarea tarea) {
        this.tareas.add(tarea);
    }

    public void añadirRecurso(Recurso recurso) {
        this.recursos.add(recurso);
    }

    public void añadirAsignación(Asignación asignación) {
        this.asignaciones.add(asignación);
    }

    public List<Proyecto> obtenerProyectos() {
        return this.proyectos;
    }

    public List<Tarea> obtenerTareas() {
        return this.tareas;
    }

    public List<Recurso> obtenerRecursos() {
        return this.recursos;
    }

    public List<Asignación> obtenerAsignaciones() {
        return this.asignaciones;
    }

    public Proyecto obtenerProyectoPorId(int id) {
        for (Proyecto proyecto : this.proyectos) {
            if (proyecto.getId() == id) {
                return proyecto;
            }
        }
        return null;
    }

    public Tarea obtenerTareaPorId(int id) {
        for (Tarea tarea : this.tareas) {
            if (tarea.getId() == id) {
                return tarea;
            }
        }
        return null;
    }

    public Recurso obtenerRecursoPorId(int id) {
        for (Recurso recurso : this.recursos) {
            if (recurso.getId() == id) {
                return recurso;
            }
        }
        return null;
    }

    public Asignación obtenerAsignaciónPorId(int id) {
        for (Asignación asignación : this.asignaciones) {
            if (asignación.getId() == id) {
                return asignación;
            }
        }
        return null;
    }

    public void eliminarProyecto(int id) {
        Proyecto proyecto = this.obtenerProyectoPorId(id);
        if (proyecto != null) {
            this.proyectos.remove(proyecto);
        }
    }

    public void eliminarTarea(int id) {
        Tarea tarea = this.obtenerTareaPorId(id);
        if (tarea != null) {
            this.tareas.remove(tarea);
        }
    }

    public void eliminarRecurso(int id) {
        Recurso recurso = this.obtenerRecursoPorId(id);
        if (recurso != null) {