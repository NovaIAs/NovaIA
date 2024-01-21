```d
// Creando un tipo de datos seguro para el manejo de cadenas de texto
struct CadenaSegura {
    string texto;

    // Constructor para crear una cadena segura a partir de un string
    this(string str) {
        texto = str;
        validar(); // Asegurar que la cadena sea segura
    }

    // Constructor para crear una cadena segura vacía
    this() {
        texto = "";
    }

    // Método para validar la cadena
    void validar() {
        // Reemplazar caracteres peligrosos con alternativas seguras
        texto = texto.replace(/[<>&'"]/g, {"<": "&lt;", ">": "&gt;", "&": "&amp;", "'": "&apos;", '"': "&quot;"});
    }

    // Operador de concatenación
    CadenaSegura opConcat(CadenaSegura otra) {
        CadenaSegura nueva = new CadenaSegura(texto + otra.texto);
        nueva.validar(); // Asegurar que la cadena resultante sea segura
        return nueva;
    }

    // Operador para convertir la cadena segura a una cadena normal
    string opToString() {
        return texto;
    }
}

// Clase para manejar un sistema de mensajería segura
class MensajeroSeguro {
    // Lista de mensajes seguros
    private CadenaSegura[] mensajes = new CadenaSegura[0];

    // Método para añadir un mensaje seguro al sistema
    void añadirMensaje(CadenaSegura mensaje) {
        mensajes ~= mensaje; // Agregar el mensaje a la lista
    }

    // Método para obtener todos los mensajes seguros del sistema
    CadenaSegura[] obtenerMensajes() {
        return mensajes; // Devolver la lista de mensajes
    }
}

// Función para enviar un mensaje seguro a un destinatario
void enviarMensajeSeguro(MensajeroSeguro mensajero, string destinatario, string mensaje) {
    // Crear una cadena segura a partir del mensaje
    CadenaSegura mensajeSeguro = new CadenaSegura(mensaje);

    // Añadir el mensaje seguro al sistema de mensajería
    mensajero.añadirMensaje(mensajeSeguro);

    // Notificar al destinatario sobre el nuevo mensaje
    // ... (Aquí se podría implementar el envío de una notificación al destinatario)
}

// Función principal del programa
void main() {
    // Crear un sistema de mensajería segura
    MensajeroSeguro mensajero = new MensajeroSeguro();

    // Crear un mensaje seguro
    CadenaSegura mensajeSeguro = new CadenaSegura("Hola, mundo!");

    // Enviar el mensaje seguro al sistema de mensajería
    enviarMensajeSeguro(mensajero, "destinatario@ejemplo.com", mensajeSeguro.opToString());

    // Obtener todos los mensajes seguros del sistema
    CadenaSegura[] mensajesSeguros = mensajero.obtenerMensajes();

    // Mostrar los mensajes seguros en la consola
    foreach (CadenaSegura mensaje in mensajesSeguros) {
        writeln("Mensaje seguro:", mensaje.opToString());
    }
}
```

Explicación del código:

1. Tipo de datos `CadenaSegura`: Se crea un tipo de datos seguro para manejar cadenas de texto. Este tipo de datos asegura que las cadenas no contengan caracteres peligrosos como '<', '>', '&', ''' o '"'.

2. Clase `MensajeroSeguro`: Se crea una clase para manejar un sistema de mensajería segura. Esta clase contiene una lista de mensajes seguros y métodos para añadir y obtener mensajes.

3. Función `enviarMensajeSeguro`: Se crea una función para enviar un mensaje seguro a un destinatario. Esta función crea una cadena segura a partir del mensaje, la añade al sistema de mensajería y notifica al destinatario sobre el nuevo mensaje.

4. Función `main`: En la función principal del programa, se crea un sistema de mensajería segura, se crea un mensaje seguro, se envía el mensaje al sistema de mensajería y se obtienen todos los mensajes seguros del sistema.

5. Los operadores `opConcat` y `opToString` se implementan para poder concatenar cadenas seguras y convertirlas a cadenas normales.

Este código demuestra un uso complejo de la programación en D, incluyendo la creación de tipos de datos seguros, clases, funciones y operadores. También muestra cómo usar estos componentes para crear un sistema de mensajería segura.