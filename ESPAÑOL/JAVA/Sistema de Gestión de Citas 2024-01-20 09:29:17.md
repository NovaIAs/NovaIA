```java

import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

public class SistemaGestionCitas {

    public static void main(String[] args) {
        // Crear un escáner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Crear una lista para almacenar las citas
        List<Cita> citas = new ArrayList<>();

        // Crear un mapa para almacenar los médicos y sus horarios de trabajo
        Map<Medico, List<Horario>> horariosMedicos = new HashMap<>();

        // Crear algunos médicos y horarios de trabajo de ejemplo
        Medico medico1 = new Medico("Dr. Juan Pérez");
        Medico medico2 = new Medico("Dra. Ana García");

        List<Horario> horariosMedico1 = new ArrayList<>();
        horariosMedico1.add(new Horario(Lunes, 9, 12));
        horariosMedico1.add(new Horario(Miercoles, 14, 17));

        List<Horario> horariosMedico2 = new ArrayList<>();
        horariosMedico2.add(new Horario(Martes, 10, 13));
        horariosMedico2.add(new Horario(Jueves, 15, 18));

        horariosMedicos.put(medico1, horariosMedico1);
        horariosMedicos.put(medico2, horariosMedico2);

        // Mostrar un mensaje de bienvenida al usuario
        System.out.println("Bienvenido al sistema de gestión de citas.");

        // Bucle principal del programa
        while (true) {
            // Mostrar un menú de opciones al usuario
            System.out.println("¿Qué desea hacer?");
            System.out.println("1. Reservar una cita");
            System.out.println("2. Cancelar una cita");
            System.out.println("3. Modificar una cita");
            System.out.println("4. Ver todas las citas");
            System.out.println("5. Salir");

            // Leer la opción elegida por el usuario
            int opcion = scanner.nextInt();

            // Ejecutar la opción elegida por el usuario
            switch (opcion) {
                case 1:
                    // Reservar una cita
                    reservarCita(citas, horariosMedicos, scanner);
                    break;
                case 2:
                    // Cancelar una cita
                    cancelarCita(citas, scanner);
                    break;
                case 3:
                    // Modificar una cita
                    modificarCita(citas, scanner);
                    break;
                case 4:
                    // Ver todas las citas
                    verCitas(citas);
                    break;
                case 5:
                    // Salir del programa
                    System.exit(0);
                    break;
                default:
                    // Mostrar un mensaje de error si el usuario introduce una opción no válida
                    System.out.println("Opción no válida.");
            }
        }
    }

    private static void reservarCita(List<Cita> citas, Map<Medico, List<Horario>> horariosMedicos, Scanner scanner) {
        // Mostrar una lista de los médicos disponibles
        System.out.println("Elija un médico:");
        for (Medico medico : horariosMedicos.keySet()) {
            System.out.println(medico.getNombre());
        }

        // Leer el médico elegido por el usuario
        String medicoElegido = scanner.nextLine();

        // Obtener los horarios de trabajo del médico elegido
        List<Horario> horarios = horariosMedicos.get(medicoElegido);

        // Mostrar una lista de los horarios disponibles del médico elegido
        System.out.println("Elija un horario:");
        for (Horario horario : horarios) {
            System.out.println(horario.toString());
        }

        // Leer el horario elegido por el usuario
        String horarioElegido = scanner.nextLine();

        // Obtener el objeto Horario correspondiente al horario elegido
        Horario horario = null;
        for (Horario h : horarios) {
            if (h.toString().equals(horarioElegido)) {
                horario = h;
                break;
            }
        }

        // Crear una nueva cita
        Cita cita = new Cita(medicoElegido, horario);

        // Añadir la cita a la lista de citas
        citas.add(cita);

        // Mostrar un mensaje de confirmación al usuario
        System.out.println("Cita reservada correctamente.");
    }

    private static void cancelarCita(List<Cita> citas, Scanner scanner) {
        // Mostrar una lista de las citas existentes
        System.out.println("Elija una cita para cancelar:");
        for (Cita cita : citas) {
            System.out.println(cita.toString());
        }

        // Leer la cita elegida por el usuario
        String citaElegida = scanner.nextLine();

        // Obtener el objeto Cita correspondiente a la cita elegida
        Cita cita = null;
        for (Cita c : citas) {
            if (c.toString().equals(citaElegida)) {
                cita = c;
                break;
            }
        }

        // Eliminar la cita de la lista de citas
        citas.remove(cita);

        // Mostrar un mensaje de confirmación al usuario
        System.out.println("Cita cancelada correctamente.");
    }

    private static void modificarCita(List<Cita> citas, Scanner scanner) {
        // Mostrar una lista de las citas existentes
        System.out.println("Elija una cita para modificar:");
        for (Cita cita : citas) {
            System.out.println(cita.toString());
        }

        // Leer la cita elegida por el usuario
        String citaElegida = scanner.nextLine();

        // Obtener el objeto Cita correspondiente a la cita elegida
        Cita cita = null;
        for (Cita c : citas) {
            if (c.toString().equals(citaElegida)) {
                cita = c;
                break;
            }
        }

        // Mostrar las opciones de modificación disponibles
        System.out.println("Elija qué desea modificar:");
        System.out.println("1. Médico");
        System.out.println("2. Horario");

        // Leer la opción elegida por el usuario
        int opcion = scanner.nextInt();

        // Ejecutar la opción elegida por el usuario
        switch (opcion) {
            case 1:
                // Modificar el médico de la cita
                System.out.println("Elija un nuevo médico:");
                for (Medico medico : horariosMedicos.keySet()) {
                    System.out.println(medico.getNombre());
                }

                // Leer el nuevo médico elegido por el usuario
                String nuevoMedico = scanner.nextLine();

                // Obtener los horarios de trabajo del nuevo médico elegido
                List<Horario> nuevosHorarios = horariosMedicos.get(nuevoMedico);

                // Mostrar una lista de los nuevos horarios disponibles del nuevo médico elegido
                System.out.println("Elija un nuevo horario:");
                for (Horario horario : nuevosHorarios) {
                    System.out.println(horario.toString());
                }

                // Leer el nuevo horario elegido por el usuario
                String nuevoHorario = scanner.nextLine();

                // Obtener el objeto Horario correspondiente al nuevo horario elegido
                Horario nuevoHorario = null;
                for (Horario h : nuevosHorarios) {
                    if (h.toString().equals(nuevoHorario)) {
                        nuevoHorario = h;
                        break;
                    }
                }

                // Modificar el médico y el horario de la cita
                cita.setMedico(nuevoMedico);
                cita.setHorario(nuevoHorario);

                // Mostrar un mensaje de confirmación al usuario
                System.out.println("Cita modificada correctamente.");
                break;
            case 2:
                // Modificar el horario de la cita
                System.out.println("Elija un nuevo horario:");
                for (Horario horario : horariosMedicos.get(cita.getMedico())) {
                    System.out.println(horario.toString());
                }

                // Leer el nuevo horario elegido por el usuario
                String nuevoHorario = scanner.nextLine();

                // Obtener el objeto Horario correspondiente al nuevo horario elegido
                Horario nuevoHorario = null;
                for (Horario h : horariosMedicos.get(cita.getMedico())) {
                    if (h.toString().equals(nuevoHorario)) {
                        nuevoHorario = h;
                        break;
                    }
                }

                // Modificar el horario de la cita
                cita.setHorario(nuevoHorario);

                // Mostrar un mensaje de confirmación al usuario
                System.out.println("Cita modificada correctamente.");
                break;
            default:
                // Mostrar un mensaje de error si el usuario introduce una opción no válida
                System.out.println("Opción no válida.");
        }
    }

    private static void verCitas(List<Cita> citas) {
        // Ordenar las citas por fecha y hora
        Collections.sort(citas, (Cita c1, Cita c2) -> c1.get