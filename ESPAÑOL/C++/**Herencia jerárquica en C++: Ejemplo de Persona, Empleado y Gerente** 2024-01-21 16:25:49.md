```c++
// Definición de la clase Persona
class Persona {
public:
    // Constructor de la clase
    Persona(string nombre, int edad) : nombre(nombre), edad(edad) {}

    // Métodos de la clase
    string getNombre() const { return nombre; }
    int getEdad() const { return edad; }

private:
    // Atributos de la clase
    string nombre;
    int edad;
};

// Definición de la clase Empleado
class Empleado : public Persona {
public:
    // Constructor de la clase
    Empleado(string nombre, int edad, double salario) : Persona(nombre, edad), salario(salario) {}

    // Métodos de la clase
    double getSalario() const { return salario; }

private:
    // Atributos de la clase
    double salario;
};

// Definición de la clase Gerente
class Gerente : public Empleado {
public:
    // Constructor de la clase
    Gerente(string nombre, int edad, double salario, string departamento) : Empleado(nombre, edad, salario), departamento(departamento) {}

    // Métodos de la clase
    string getDepartamento() const { return departamento; }

private:
    // Atributos de la clase
    string departamento;
};

// Función principal del programa
int main() {
    // Creación de objetos de la clase Persona, Empleado y Gerente
    Persona persona1("Juan Pérez", 25);
    Empleado empleado1("María González", 30, 2000.0);
    Gerente gerente1("Pedro Sánchez", 40, 3000.0, "Ventas");

    // Impresión de los datos de los objetos
    cout << "Persona 1: " << persona1.getNombre() << ", " << persona1.getEdad() << endl;
    cout << "Empleado 1: " << empleado1.getNombre() << ", " << empleado1.getEdad() << ", " << empleado1.getSalario() << endl;
    cout << "Gerente 1: " << gerente1.getNombre() << ", " << gerente1.getEdad() << ", " << gerente1.getSalario() << ", " << gerente1.getDepartamento() << endl;

    return 0;
}
```

Este código define tres clases: Persona, Empleado y Gerente. La clase Persona es la clase base, y las clases Empleado y Gerente son clases derivadas de Persona.

La clase Persona tiene dos atributos privados: nombre y edad. También tiene dos métodos públicos para obtener el nombre y la edad de una persona.

La clase Empleado hereda todos los atributos y métodos de la clase Persona. Además, tiene un atributo privado llamado salario. También tiene un método público para obtener el salario de un empleado.

La clase Gerente hereda todos los atributos y métodos de la clase Empleado. Además, tiene un atributo privado llamado departamento. También tiene un método público para obtener el departamento de un gerente.

La función main() crea objetos de las clases Persona, Empleado y Gerente. Luego, imprime los datos de los objetos en la consola.

Este código es un ejemplo de la herencia jerárquica, en la que una clase base (Persona) tiene varias clases derivadas (Empleado y Gerente). La herencia jerárquica se utiliza a menudo para modelar relaciones entre objetos en el mundo real, como la relación entre una persona y un empleado o un gerente.