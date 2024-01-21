```c++
// Importamos las librerías necesarias
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>

// Definimos una estructura para representar a un empleado
struct Empleado {
    std::string nombre;
    std::string apellido;
    int edad;
    float salario;
};

// Definimos una función para comparar empleados por edad
bool comparar_empleados_por_edad(const Empleado& empleado1, const Empleado& empleado2) {
    return empleado1.edad < empleado2.edad;
}

// Definimos una función para comparar empleados por salario
bool comparar_empleados_por_salario(const Empleado& empleado1, const Empleado& empleado2) {
    return empleado1.salario > empleado2.salario;
}

// Creamos una función para imprimir los datos de un empleado
void imprimir_empleado(const Empleado& empleado) {
    std::cout << "Nombre: " << empleado.nombre << "\n";
    std::cout << "Apellido: " << empleado.apellido << "\n";
    std::cout << "Edad: " << empleado.edad << "\n";
    std::cout << "Salario: " << empleado.salario << "\n\n";
}

// Creamos una función para imprimir los datos de todos los empleados
void imprimir_empleados(const std::vector<Empleado>& empleados) {
    for (const Empleado& empleado : empleados) {
        imprimir_empleado(empleado);
    }
}

// Creamos una función para buscar un empleado por su nombre
Empleado buscar_empleado_por_nombre(const std::vector<Empleado>& empleados, const std::string& nombre) {
    for (const Empleado& empleado : empleados) {
        if (empleado.nombre == nombre) {
            return empleado;
        }
    }
    return Empleado{}; // Devolvemos un empleado vacío si no encontramos al empleado
}

// Creamos una función para calcular el salario total de todos los empleados
float calcular_salario_total(const std::vector<Empleado>& empleados) {
    float salario_total = 0;
    for (const Empleado& empleado : empleados) {
        salario_total += empleado.salario;
    }
    return salario_total;
}

// Creamos una función para calcular la edad promedio de todos los empleados
float calcular_edad_promedio(const std::vector<Empleado>& empleados) {
    int edad_total = 0;
    for (const Empleado& empleado : empleados) {
        edad_total += empleado.edad;
    }
    return edad_total / empleados.size();
}

// Creamos una función para crear un mapa con los empleados agrupados por edad
std::map<int, std::vector<Empleado>> agrupar_empleados_por_edad(const std::vector<Empleado>& empleados) {
    std::map<int, std::vector<Empleado>> empleados_agrupados;
    for (const Empleado& empleado : empleados) {
        empleados_agrupados[empleado.edad].push_back(empleado);
    }
    return empleados_agrupados;
}

// Creamos una función para crear un mapa con los empleados agrupados por salario
std::map<float, std::vector<Empleado>> agrupar_empleados_por_salario(const std::vector<Empleado>& empleados) {
    std::map<float, std::vector<Empleado>> empleados_agrupados;
    for (const Empleado& empleado : empleados) {
        empleados_agrupados[empleado.salario].push_back(empleado);
    }
    return empleados_agrupados;
}

// Creamos una función principal
int main() {
    // Creamos un vector con los datos de los empleados
    std::vector<Empleado> empleados = {
        {"Juan", "García", 25, 1500.0},
        {"María", "Pérez", 30, 2000.0},
        {"Carlos", "López", 35, 2500.0},
        {"Ana", "Fernández", 40, 3000.0},
        {"Pedro", "Martínez", 45, 3500.0}
    };

    // Imprimimos los datos de todos los empleados
    std::cout << "Empleados:\n";
    imprimir_empleados(empleados);

    // Buscamos un empleado por su nombre
    std::string nombre_empleado_a_buscar = "Carlos";
    Empleado empleado_encontrado = buscar_empleado_por_nombre(empleados, nombre_empleado_a_buscar);
    std::cout << "Empleado encontrado:\n";
    imprimir_empleado(empleado_encontrado);

    // Calculamos el salario total de todos los empleados
    float salario_total = calcular_salario_total(empleados);
    std::cout << "Salario total: " << salario_total << "\n";

    // Calculamos la edad promedio de todos los empleados
    float edad_promedio = calcular_edad_promedio(empleados);
    std::cout << "Edad promedio: " << edad_promedio << "\n";

    // Agrupamos los empleados por edad
    std::map<int, std::vector<Empleado>> empleados_agrupados_por_edad = agrupar_empleados_por_edad(empleados);
    std::cout << "Empleados agrupados por edad:\n";
    for (const auto& [edad, empleados] : empleados_agrupados_por_edad) {
        std::cout << "Edad: " << edad << "\n";
        imprimir_empleados(empleados);
    }

    // Agrupamos los empleados por salario
    std::map<float, std::vector<Empleado>> empleados_agrupados_por_salario = agrupar_empleados_por_salario(empleados);
    std::cout << "Empleados agrupados por salario:\n";
    for (const auto& [salario, empleados] : empleados_agrupados_por_salario) {
        std::cout << "Salario: " << salario << "\n";
        imprimir_empleados(empleados);
    }

    // Ordenamos los empleados por edad
    std::sort(empleados.begin(), empleados.end(), comparar_empleados_por_edad);
    std::cout << "Empleados ordenados por edad:\n";
    imprimir_empleados(empleados);

    // Ordenamos los empleados por salario
    std::sort(empleados.begin(), empleados.end(), comparar_empleados_por_salario);
    std::cout << "Empleados ordenados por salario:\n";
    imprimir_empleados(empleados);

    return 0;
}
```

Explicación del código:

* Creamos una estructura `Empleado` para representar a los empleados.
* Definimos dos funciones para comparar empleados por edad y salario.
* Creamos una función para imprimir los datos de un empleado.
* Creamos una función para imprimir los datos de todos los empleados.
* Creamos una función para buscar un empleado por su nombre.
* Creamos una función para calcular el salario total de todos los empleados.
* Creamos una función para calcular la edad promedio de todos los empleados.
* Creamos una función para crear un mapa con los empleados agrupados por edad.
* Creamos una función para crear un mapa con los empleados agrupados por salario.
* Creamos una función principal para ejecutar el programa.
* En la función principal, creamos un vector con los datos de los empleados.
* Imprimimos los datos de todos los empleados.
* Buscamos un empleado por su nombre.
* Calculamos el salario total de todos los empleados.
* Calculamos la edad promedio de todos los empleados.
* Agrupamos los empleados por edad y salario.
* Ordenamos los empleados por edad y salario.
* Imprimimos los datos de los empleados ordenados.