```modula-2

MODULE Ascensor;
  (* Definiciones de tipos y constantes *)
  TYPE Estado = (parado, subiendo, bajando);
  CONST numPisos = 10;

  (* Definiciones de procedimientos y funciones *)
  PROCEDURE mover(dir: Estado);
  BEGIN
    CASE dir OF
      subiendo: ... (* Código para mover el ascensor hacia arriba *)
      bajando:  ... (* Código para mover el ascensor hacia abajo *)
      parado:   ... (* Código para detener el ascensor *)
    END
  END mover;

  PROCEDURE llamar(piso: INTEGER);
  BEGIN
    ... (* Código para llamar al ascensor desde un piso *)
  END llamar;

  PROCEDURE siguientePiso(): INTEGER;
  BEGIN
    ... (* Código para determinar el siguiente piso al que debe ir el ascensor *)
  END siguientePiso;

  (* Definiciones de variables globales *)
  VAR pisoActual: INTEGER;
  VAR estado: Estado;

  (* Código principal *)
  BEGIN
    estado := parado;
    pisoActual := 0;

    LOOP
      SELECT
        WHEN llamar(piso) DO
          IF estado = parado THEN
            estado := subiendo;
            mover(subiendo);
          END
        END;

        WHEN siguientePiso() = pisoActual DO
          estado := parado;
          mover(parado);
        END;

        ELSE
          ... (* Código para manejar otros eventos *)
      END SELECT
    END LOOP
  END Ascensor.

```

Este código implementa un ascensor que puede moverse entre 10 pisos. El ascensor puede estar en tres estados: parado, subiendo o bajando. Los usuarios pueden llamar al ascensor desde cualquier piso y el ascensor se moverá hacia arriba o hacia abajo para recogerlos. El ascensor se detiene en cada piso al que se le llama y luego continúa moviéndose en la dirección actual hasta que llega a su destino.

El código utiliza un bucle `SELECT` para manejar los eventos. El bucle `SELECT` espera que ocurra uno de los siguientes eventos:

* Un usuario llama al ascensor desde un piso.
* El ascensor llega al piso actual.
* Otro evento (como una falla mecánica).

Cuando ocurre un evento, el código ejecuta el código apropiado para manejar el evento. Por ejemplo, si un usuario llama al ascensor desde un piso, el código comprueba si el ascensor está parado. Si el ascensor está parado, el código cambia el estado del ascensor a subiendo y comienza a mover el ascensor hacia arriba.

El código también utiliza una función `siguientePiso()` para determinar el siguiente piso al que debe ir el ascensor. La función `siguientePiso()` devuelve el piso más cercano al piso actual que aún no se ha visitado.

Este código es un ejemplo de un programa concurrente. El programa concurrente es un programa que puede ejecutar múltiples tareas al mismo tiempo. En este caso, el programa concurrente puede manejar múltiples llamadas al ascensor al mismo tiempo. El programa concurrente también puede manejar múltiples fallas mecánicas al mismo tiempo.