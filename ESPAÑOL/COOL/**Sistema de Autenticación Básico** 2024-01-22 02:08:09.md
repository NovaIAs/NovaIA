```cool
class Funcionalidad {

    property username: String;
    property password: String;

    method inicializar() {
        self.username = "admin";
        self.password = "password";
    }

    method verificar_credenciales(username: String, password: String): Boolean {
        return self.username == username && self.password == password;
    }
}

class Interfaz {

    property funcionalidad: Funcionalidad;

    method inicializar() {
        self.funcionalidad = new Funcionalidad();
    }

    method mostrar_login() {
        print("Ingrese su nombre de usuario:");
        let username = input();
        print("Ingrese su contraseña:");
        let password = input();
        return self.funcionalidad.verificar_credenciales(username, password);
    }

    method ejecutar() {
        let resultado_login = self.mostrar_login();
        if (resultado_login) {
            print("Bienvenido!");
        } else {
            print("Credenciales incorrectas.");
        }
    }
}

class Main {

    property interfaz: Interfaz;

    method inicializar() {
        self.interfaz = new Interfaz();
    }

    method ejecutar() {
        self.interfaz.inicializar();
        self.interfaz.ejecutar();
    }
}

let main = new Main();
main.ejecutar();
```

**Explicación del código:**

* La clase `Funcionalidad` define la lógica de negocio de la aplicación. Tiene dos propiedades, `username` y `password`, que se utilizan para almacenar las credenciales de un usuario. También tiene un método `verificar_credenciales()` que comprueba si las credenciales proporcionadas por un usuario son correctas.
* La clase `Interfaz` define la interfaz de usuario de la aplicación. Tiene una propiedad `funcionalidad` que se utiliza para acceder a la lógica de negocio. También tiene un método `mostrar_login()` que muestra un formulario de inicio de sesión y devuelve si las credenciales introducidas son correctas.
* La clase `Main` es el punto de entrada de la aplicación. Crea una instancia de la clase `Interfaz` y llama a su método `ejecutar()`.

El código anterior crea una aplicación sencilla que pide al usuario su nombre de usuario y contraseña. Si las credenciales son correctas, la aplicación muestra un mensaje de bienvenida. De lo contrario, muestra un mensaje de error.