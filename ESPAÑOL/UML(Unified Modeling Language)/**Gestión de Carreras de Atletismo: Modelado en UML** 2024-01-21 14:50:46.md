```
Dominio del Problema: Gestión de Carreras de Atletismo

Diagrama de Clases:

1. Competidor:
    - Atributos:
        - idCompetidor: int
        - nombre: string
        - fechaNacimiento: date
        - nacionalidad: string
        - categoria: string
    - Métodos:
        - correr(): void
        - saltar(): void
        - lanzar(): void

2. Carrera:
    - Atributos:
        - idCarrera: int
        - nombre: string
        - fecha: date
        - lugar: string
        - distancia: float
        - tipo: string
    - Métodos:
        - iniciarCarrera(): void
        - finalizarCarrera(): void
        - registrarResultado(): void

3. Inscripción:
    - Atributos:
        - idInscripción: int
        - idCompetidor: int
        - idCarrera: int
        - fechaInscripción: date
        - estado: string
    - Métodos:
        - inscribirse(): void
        - cancelarInscripción(): void

4. Evento:
    - Atributos:
        - idEvento: int
        - nombre: string
        - fecha: date
        - lugar: string
        - tipo: string
    - Métodos:
        - crearEvento(): void
        - modificarEvento(): void
        - eliminarEvento(): void

5. Resultado:
    - Atributos:
        - idResultado: int
        - idCompetidor: int
        - idCarrera: int
        - tiempo: float
        - posición: int
    - Métodos:
        - registrarResultado(): void
        - modificarResultado(): void
        - eliminarResultado(): void

Diagrama de Secuencia:

1. El usuario se registra en la aplicación.
2. El usuario busca una carrera en la que inscribirse.
3. El usuario se inscribe en la carrera.
4. El usuario recibe una confirmación de su inscripción.
5. El día de la carrera, el usuario se presenta en el lugar y la hora indicados.
6. El juez inicia la carrera.
7. Los competidores corren la carrera.
8. El juez registra los resultados de la carrera.
9. Los resultados se publican en la aplicación.
10. El usuario puede consultar sus resultados en la aplicación.

Diagrama de Casos de Uso:

1. Registrarse:
    - Actor: Usuario
    - Descripción: El usuario se registra en la aplicación.
2. Buscar Carrera:
    - Actor: Usuario
    - Descripción: El usuario busca una carrera en la que inscribirse.
3. Inscribirse en Carrera:
    - Actor: Usuario
    - Descripción: El usuario se inscribe en la carrera.
4. Iniciar Carrera:
    - Actor: Juez
    - Descripción: El juez inicia la carrera.
5. Registrar Resultados:
    - Actor: Juez
    - Descripción: El juez registra los resultados de la carrera.
6. Publicar Resultados:
    - Actor: Organizador de la Carrera
    - Descripción: El organizador de la carrera publica los resultados en la aplicación.
7. Consultar Resultados:
    - Actor: Usuario
    - Descripción: El usuario consulta sus resultados en la aplicación.
```

Explicación del Código:

* El diagrama de clases define las clases y sus relaciones en el dominio del problema.
* El diagrama de secuencia muestra la secuencia de mensajes entre los objetos en un escenario específico.
* El diagrama de casos de uso define los casos de uso y los actores involucrados en el sistema.

El código está escrito en español para facilitar su comprensión.