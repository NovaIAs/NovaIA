```modula-2
MODULE SistemaDeControlDeProcesos;

FROM Terminal IMPORT
  Pantalla, Teclado;

FROM Procesos IMPORT
  Tarea, Proceso;

FROM Memoria IMPORT
  EspacioDeMemoria, Direccion;

FROM Utilidades IMPORT
  EnteroAString;

VAR
  tareas: ARRAY [1..10] OF Tarea;
  procesos: ARRAY [1..10] OF Proceso;
  memoria: EspacioDeMemoria;
  tiempo: CARDINAL;

PROCEDURE CrearTarea(nombre: STRING; tamano: CARDINAL);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO 10 DO
    IF tareas[i].estado = LISTA THEN
      tareas[i].nombre := nombre;
      tareas[i].tamano := tamano;
      tareas[i].estado := CARGADA;
      tareas[i].direccion := Direccion(0);
      RETURN;
    END;
  END;
  Pantalla.EscribirLinea("No hay espacio disponible para la tarea.");
END CrearTarea;

PROCEDURE CrearProceso(nombre: STRING);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO 10 DO
    IF procesos[i].estado = LISTA THEN
      procesos[i].nombre := nombre;
      procesos[i].estado := EJECUTANDO;
      procesos[i].tareas := ARRAY [1..10] OF Tarea;
      RETURN;
    END;
  END;
  Pantalla.EscribirLinea("No hay espacio disponible para el proceso.");
END CrearProceso;

PROCEDURE CargarTarea(tarea: Tarea);
VAR
  direccion: Direccion;
BEGIN
  direccion := memoria.Asignar(tarea.tamano);
  IF direccion <> Direccion(0) THEN
    tarea.direccion := direccion;
    tarea.estado := EJECUTANDO;
  ELSE
    Pantalla.EscribirLinea("No hay espacio disponible en la memoria.");
  END;
END CargarTarea;

PROCEDURE EjecutarTarea(tarea: Tarea);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO 10 DO
    IF procesos[i].estado = EJECUTANDO THEN
      procesos[i].tareas[i] := tarea;
      RETURN;
    END;
  END;
  Pantalla.EscribirLinea("No hay ningún proceso en ejecución.");
END EjecutarTarea;

PROCEDURE FinalizarTarea(tarea: Tarea);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO 10 DO
    IF tareas[i].estado = EJECUTANDO THEN
      tareas[i].estado := FINALIZADA;
      RETURN;
    END;
  END;
  Pantalla.EscribirLinea("La tarea no está en ejecución.");
END FinalizarTarea;

PROCEDURE FinalizarProceso(proceso: Proceso);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO 10 DO
    IF procesos[i].estado = EJECUTANDO THEN
      procesos[i].estado := FINALIZADA;
      RETURN;
    END;
  END;
  Pantalla.EscribirLinea("El proceso no está en ejecución.");
END FinalizarProceso;

PROCEDURE LiberarTarea(tarea: Tarea);
VAR
  direccion: Direccion;
BEGIN
  direccion := tarea.direccion;
  memoria.Liberar(direccion);
  tarea.estado := LISTA;
END LiberarTarea;

PROCEDURE LiberarProceso(proceso: Proceso);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO 10 DO
    LiberarTarea(proceso.tareas[i]);
  END;
  proceso.estado := LISTA;
END LiberarProceso;

PROCEDURE ImprimirTareas();
VAR
  i: CARDINAL;
BEGIN
  Pantalla.EscribirLinea("Tareas:");
  FOR i := 1 TO 10 DO
    Pantalla.EscribirLinea(tareas[i].nombre + ": " + EnteroAString(tareas[i].tamano) + " bytes");
  END;
END ImprimirTareas;

PROCEDURE ImprimirProcesos();
VAR
  i: CARDINAL;
BEGIN
  Pantalla.EscribirLinea("Procesos:");
  FOR i := 1 TO 10 DO
    Pantalla.EscribirLinea(procesos[i].nombre + ": " + EnteroAString(procesos[i].tareas.Length) + " tareas");
  END;
END ImprimirProcesos;

PROCEDURE ImprimirMemoria();
BEGIN
  Pantalla.EscribirLinea("Memoria:");
  Pantalla.EscribirLinea("Espacio total: " + EnteroAString(memoria.tamano) + " bytes");
  Pantalla.EscribirLinea("Espacio libre: " + EnteroAString(memoria.espacioLibre) + " bytes");
END ImprimirMemoria;

PROCEDURE Principal();
VAR