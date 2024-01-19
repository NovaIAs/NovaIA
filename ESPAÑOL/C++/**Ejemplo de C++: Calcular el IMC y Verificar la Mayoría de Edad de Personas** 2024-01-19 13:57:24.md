```c++
// Biblioteca principal
#include <iostream>

// Espacio de nombres estándar
using namespace std;

// Función principal
int main() {
  // Creando una estructura llamada Persona
  struct Persona {
    string nombre;
    int edad;
    float altura;
  };

  // Creando una matriz de estructuras Persona
  Persona personas[3];

  // Leyendo los datos de las personas
  for (int i = 0; i < 3; i++) {
    cout << "Ingrese el nombre de la persona " << i + 1 << ": ";
    cin >> personas[i].nombre;

    cout << "Ingrese la edad de la persona " << i + 1 << ": ";
    cin >> personas[i].edad;

    cout << "Ingrese la altura de la persona " << i + 1 << ": ";
    cin >> personas[i].altura;
  }

  // Imprimiendo los datos de las personas
  cout << endl;
  for (int i = 0; i < 3; i++) {
    cout << "Nombre: " << personas[i].nombre << endl;
    cout << "Edad: " << personas[i].edad << endl;
    cout << "Altura: " << personas[i].altura << endl;

    cout << endl;
  }

  // Creando una función para calcular el IMC de una persona
  float calcularIMC(float altura, float peso) {
    return peso / (altura * altura);
  }

  // Calculando el IMC de las personas
  for (int i = 0; i < 3; i++) {
    float peso;

    cout << "Ingrese el peso de la persona " << i + 1 << ": ";
    cin >> peso;

    float imc = calcularIMC(personas[i].altura, peso);

    cout << "El IMC de la persona " << i + 1 << " es: " << imc << endl;

    cout << endl;
  }

  // Creando una función para verificar si una persona es mayor de edad
  bool esMayorDeEdad(int edad) {
    return edad >= 18;
  }

  // Verificando si las personas son mayores de edad
  for (int i = 0; i < 3; i++) {
    cout << "La persona " << personas[i].nombre << " es ";

    if (esMayorDeEdad(personas[i].edad)) {
      cout << "mayor de edad" << endl;
    } else {
      cout << "menor de edad" << endl;
    }

    cout << endl;
  }

  return 0;
}
```

Este código en C++ es un programa que nos permite leer los datos de tres personas (nombre, edad, altura y peso) y nos proporciona información sobre su IMC (Índice de Masa Corporal) y si son mayores de edad o no.

El programa sigue esta estructura:

1. Se crea una estructura llamada **Persona** que contiene los campos **nombre**, **edad** y **altura**.
2. Se crea una matriz de estructuras Persona llamada **personas** con capacidad para almacenar los datos de tres personas.
3. Se lee la información de cada persona (nombre, edad y altura) y se almacena en la matriz **personas**.
4. Se imprime la información de cada persona en la consola.
5. Se crea una función llamada **calcularIMC** que calcula el IMC de una persona a partir de su altura y peso.
6. Se calcula el IMC de cada persona y se imprime en la consola.
7. Se crea una función llamada **esMayorDeEdad** que verifica si una persona es mayor de edad o no.
8. Se verifica si cada persona es mayor de edad o no y se imprime en la consola.

Este es un programa complejo que cubre una variedad de conceptos en C++, como estructuras, matrices, funciones y condicionales.