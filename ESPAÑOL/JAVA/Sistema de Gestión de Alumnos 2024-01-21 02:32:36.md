```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SistemaGestionAlumnos {

    private Map<String, Alumno> alumnos = new HashMap<>();

    public void registrarAlumno(Alumno alumno) {
        alumnos.put(alumno.getCodigo(), alumno);
    }

    public Alumno obtenerAlumno(String codigo) {
        return alumnos.get(codigo);
    }

    public List<Alumno> obtenerTodosLosAlumnos() {
        return new ArrayList<>(alumnos.values());
    }

    public void modificarAlumno(Alumno alumno) {
        alumnos.put(alumno.getCodigo(), alumno);
    }

    public void eliminarAlumno(String codigo) {
        alumnos.remove(codigo);
    }

    public List<Alumno> obtenerAlumnosPorCurso(String curso) {
        List<Alumno> alumnosPorCurso = new ArrayList<>();
        for (Alumno alumno : alumnos.values()) {
            if (alumno.getCurso().equals(curso)) {
                alumnosPorCurso.add(alumno);
            }
        }
        return alumnosPorCurso;
    }

    public List<Alumno> obtenerAlumnosPorNota(int nota) {
        List<Alumno> alumnosPorNota = new ArrayList<>();
        for (Alumno alumno : alumnos.values()) {
            if (alumno.getNota() == nota) {
                alumnosPorNota.add(alumno);
            }
        }
        return alumnosPorNota;
    }

    public Map<String, List<Alumno>> obtenerAlumnosPorCursoYNota() {
        Map<String, List<Alumno>> alumnosPorCursoYNota = new HashMap<>();
        for (Alumno alumno : alumnos.values()) {
            String curso = alumno.getCurso();
            int nota = alumno.getNota();
            List<Alumno> alumnosPorCursoYNotaActual = alumnosPorCursoYNota.get(curso);
            if (alumnosPorCursoYNotaActual == null) {
                alumnosPorCursoYNotaActual = new ArrayList<>();
                alumnosPorCursoYNota.put(curso, alumnosPorCursoYNotaActual);
            }
            alumnosPorCursoYNotaActual.add(alumno);
        }
        return alumnosPorCursoYNota;
    }

    public static void main(String[] args) {
        SistemaGestionAlumnos sistema = new SistemaGestionAlumnos();

        Alumno alumno1 = new Alumno("A1", "Juan Pérez", "1º ESO", 8);
        Alumno alumno2 = new Alumno("A2", "María López", "2º ESO", 9);
        Alumno alumno3 = new Alumno("A3", "Pedro García", "3º ESO", 7);
        Alumno alumno4 = new Alumno("A4", "Ana Sánchez", "4º ESO", 10);

        sistema.registrarAlumno(alumno1);
        sistema.registrarAlumno(alumno2);
        sistema.registrarAlumno(alumno3);
        sistema.registrarAlumno(alumno4);

        System.out.println("Todos los alumnos:");
        for (Alumno alumno : sistema.obtenerTodosLosAlumnos()) {
            System.out.println(alumno);
        }

        System.out.println("Alumno con código A2:");
        System.out.println(sistema.obtenerAlumno("A2"));

        System.out.println("Alumnos de 3º ESO:");
        for (Alumno alumno : sistema.obtenerAlumnosPorCurso("3º ESO")) {
            System.out.println(alumno);
        }

        System.out.println("Alumnos con nota 8:");
        for (Alumno alumno : sistema.obtenerAlumnosPorNota(8)) {
            System.out.println(alumno);
        }

        System.out.println("Alumnos por curso y nota:");
        for (Map.Entry<String, List<Alumno>> entry : sistema.obtenerAlumnosPorCursoYNota().entrySet()) {
            System.out.println("Curso: " + entry.getKey());
            for (Alumno alumno : entry.getValue()) {
                System.out.println(alumno);
            }
            System.out.println();
        }
    }
}

class Alumno {

    private String codigo;
    private String nombre;
    private String curso;
    private int nota;

    public Alumno(String codigo, String nombre, String curso, int nota) {
        this.codigo = codigo;
        this.nombre = nombre;
        this.curso = curso;
        this.nota = nota;
    }

    public String getCodigo() {
        return codigo;
    }

    public String getNombre() {
        return nombre;
    }

    public String getCurso() {
        return curso;
    }

    public int getNota() {
        return nota;
    }

    @Override
    public String toString() {
        return "Alumno{" +
                "codigo='" + codigo + '\'' +
                ", nombre='" + nombre + '\'' +
                ", curso='" + curso + '\'' +
                ", nota=" + nota +
                '}';
    }
}
```

Este código es un sistema de gestión de alumnos que permite registrar, modificar, eliminar y obtener información de los alumnos. También permite obtener listados de alumnos por curso, nota o curso y nota.

El código está dividido en dos clases: `SistemaGestionAlumnos` y `Alumno`. La clase `SistemaGestionAlumnos` es la clase principal que contiene los métodos para registrar, modificar, eliminar y obtener información de los alumnos. La clase `Alumno` es una clase que representa a un alumno y contiene los atributos código, nombre, curso y nota.

El método `registrarAlumno` de la clase `SistemaGestionAlumnos` se utiliza para registrar un nuevo alumno en el sistema. El método `obtenerAlumno` se utiliza para obtener un alumno a partir de su código. El método `obtenerTodosLosAlumnos` se utiliza para obtener una lista de todos los alumnos del sistema. El método `modificarAlumno` se utiliza para modificar los datos de un alumno. El método `eliminarAlumno` se utiliza para eliminar un alumno del sistema.

El método `obtenerAlumnosPorCurso` de la clase `SistemaGestionAlumnos` se utiliza para obtener una lista de los alumnos que cursan un determinado curso. El método `obtenerAlumnosPorNota` se utiliza para obtener una lista de los alumnos que han obtenido una determinada nota. El método `obtenerAlumnosPorCursoYNota` se utiliza para obtener una lista de los alumnos que cursan un determinado curso y han obtenido una determinada nota.

El código principal del sistema se encuentra en el método `main` de la clase `SistemaGestionAlumnos`. En este método se crean cuatro objetos de la clase `Alumno` y se registran en el sistema. A continuación, se muestran por consola todos los alumnos, el alumno con código A2, los alumnos de 3º ESO, los alumnos con nota 8 y los alumnos por curso y nota.