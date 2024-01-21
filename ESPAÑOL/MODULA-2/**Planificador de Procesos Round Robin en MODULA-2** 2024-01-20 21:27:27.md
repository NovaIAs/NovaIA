```
MODULE PlanificadorProcesos;
FROM ModulosCargados IMPORTAR Modulos;

TYPE
  Proceso = RECORD
    ID: CARDINAL;
    Nombre: ARRAY[1..20] OF CHAR;
    Prioridad: CARDINAL;
    Estado: ENUMERATION(INACTIVO, ACTIVO, ESPERA);
    TiempoEjecucion: INTEGER;
    TiempoEspera: INTEGER;
  END;

VAR
  Procesos: ARRAY[1..10] OF Proceso;
  NumProcesos: INTEGER;
  IndiceActual: INTEGER;

PROCEDURE AgregarProceso(ID: CARDINAL; Nombre: ARRAY[1..20] OF CHAR; Prioridad: CARDINAL);
  VAR
    I: INTEGER;
  BEGIN
    I := NumProcesos + 1;
    Procesos[I].ID := ID;
    FOR J := 1 TO 20 DO Procesos[I].Nombre[J] := Nombre[J] END;
    Procesos[I].Prioridad := Prioridad;
    Procesos[I].Estado := INACTIVO;
    Procesos[I].TiempoEjecucion := 0;
    Procesos[I].TiempoEspera := 0;
    NumProcesos := I;
  END AgregarProceso;

PROCEDURE MostrarProcesos;
  VAR
    I: INTEGER;
  BEGIN
    FOR I := 1 TO NumProcesos DO
      WRITE(Procesos[I].ID, ' ', Procesos[I].Nombre, ' ', Procesos[I].Prioridad, ' ', Ordenar(Procesos[I].Estado),
            ' ', Procesos[I].TiempoEjecucion, ' ', Procesos[I].TiempoEspera);
      NEWLINE;
    END;
  END MostrarProcesos;

PROCEDURE Ordenar(Estado: ENUMERATION(INACTIVO, ACTIVO, ESPERA)): ARRAY[1..10] OF CHAR;
BEGIN
  CASE Estado OF
    INACTIVO: RETURN 'INACTIVO';
    ACTIVO: RETURN 'ACTIVO';
    ESPERA: RETURN 'ESPERA';
  END;
END Ordenar;

PROCEDURE SeleccionarProceso;
  VAR
    I, J: INTEGER;
    ProcesoSeleccionado: INTEGER;
  BEGIN
    ProcesoSeleccionado := 0;
    FOR I := 1 TO NumProcesos DO
      IF Procesos[I].Estado = ACTIVO THEN
        IF ProcesoSeleccionado = 0 THEN
          ProcesoSeleccionado := I;
        ELSE
          IF Procesos[I].Prioridad > Procesos[ProcesoSeleccionado].Prioridad THEN
            ProcesoSeleccionado := I;
          END;
        END;
      END;
    END;

    IF ProcesoSeleccionado = 0 THEN
      FOR I := 1 TO NumProcesos DO
        IF Procesos[I].Estado = INACTIVO THEN
          ProcesoSeleccionado := I;
          EXIT;
        END;
      END;
    END;

    IF ProcesoSeleccionado = 0 THEN
      FOR I := 1 TO NumProcesos DO
        IF Procesos[I].Estado = ESPERA THEN
          ProcesoSeleccionado := I;
          EXIT;
        END;
      END;
    END;

    IF ProcesoSeleccionado <> 0 THEN
      FOR J := 1 TO NumProcesos DO
        IF J <> ProcesoSeleccionado THEN
          Procesos[J].Estado := ESPERA;
        END;
      END;
      Procesos[ProcesoSeleccionado].Estado := ACTIVO;
    END;
  END SeleccionarProceso;

PROCEDURE EjecutarProceso(Indice: INTEGER);
BEGIN
  Procesos[Indice].TiempoEjecucion := Procesos[Indice].TiempoEjecucion + 1;
END EjecutarProceso;

PROCEDURE RoundRobin;
BEGIN
  WHILE TRUE DO
    SeleccionarProceso;
    EjecutarProceso(IndiceActual);
    IndiceActual := IndiceActual + 1;
    IF IndiceActual > NumProcesos THEN IndiceActual := 1 END;
  END;
END RoundRobin;

BEGIN
  NumProcesos := 0;
  IndiceActual := 1;
  AgregarProceso(1, 'Proceso 1', 5);
  AgregarProceso(2, 'Proceso 2', 3);
  AgregarProceso(3, 'Proceso 3', 1);
  AgregarProceso(4, 'Proceso 4', 7);
  AgregarProceso(5, 'Proceso 5', 2);
  MostrarProcesos;
  RoundRobin;
END PlanificadorProcesos.
```

Este código es un completo planificador de procesos escrito en MODULA-2. El planificador utiliza el algoritmo de Round Robin para seleccionar los procesos que se ejecutarán.

El código está dividido en varios procedimientos y módulos, cada uno con su propia función.

El procedimiento `AgregarProceso` añade un nuevo proceso al sistema.

El procedimiento `MostrarProcesos` muestra todos los procesos del sistema, junto con su estado, prioridad y tiempo de ejecución.

El procedimiento `Ordenar` convierte un valor de enumeración en una cadena de caracteres, para mostrar el estado del proceso.

El procedimiento `SeleccionarProceso` selecciona el siguiente proceso que se ejecutará, utilizando el algoritmo de Round Robin.

El procedimiento `EjecutarProceso` ejecuta el proceso seleccionado, aumentando su tiempo de ejecución.

El procedimiento `RoundRobin` es el procedimiento principal del planificador, que ejecuta los procesos en un bucle infinito.

El código también incluye un módulo llamado `ModulosCargados`, que se utiliza para importar el módulo `Modulos`. Esto permite al planificador utilizar los módulos del sistema, como el módulo de entrada/salida.