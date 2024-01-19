```go
// Importar las bibliotecas necesarias
import (
    "fmt"
    "github.com/go-co-op/gocron"
)

// Definir la función que queremos que se ejecute cada día
func tareaDiaria() {
    fmt.Println("¡Hola mundo! Es un nuevo día.")
}

// Definir la función que queremos que se ejecute cada hora
func tareaHoraria() {
    fmt.Println("¡Hola mundo! Es una nueva hora.")
}

// Definir la función que queremos que se ejecute cada minuto
func tareaMinutera() {
    fmt.Println("¡Hola mundo! Es un nuevo minuto.")
}

// Crear un nuevo programador de tareas
programador := gocron.NewScheduler(time.UTC)

// Añadir las tareas al programador
programador.Every(24).Hours().Do(tareaDiaria)
programador.Every(1).Hour().Do(tareaHoraria)
programador.Every(1).Minute().Do(tareaMinutera)

// Iniciar el programador de tareas
programador.StartAsync()

// Esperar a que el programador de tareas termine
<-programador.Done()
```

Explicación:

1. Importamos las bibliotecas necesarias.
2. Definimos las funciones que queremos que se ejecuten cada día, cada hora y cada minuto.
3. Creamos un nuevo programador de tareas.
4. Añadimos las tareas al programador.
5. Iniciamos el programador de tareas.
6. Esperamos a que el programador de tareas termine.

Este código creará un programador de tareas que ejecutará las funciones `tareaDiaria`, `tareaHoraria` y `tareaMinutera` cada día, cada hora y cada minuto, respectivamente. El programador de tareas se iniciará de forma asíncrona, lo que significa que no bloqueará el hilo principal. El programa principal esperará a que el programador de tareas termine antes de salir.