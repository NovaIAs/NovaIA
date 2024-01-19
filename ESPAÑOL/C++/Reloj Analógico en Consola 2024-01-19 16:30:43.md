```c++
// Programa de ejemplo para mostrar un reloj analógico en una ventana de consola
// Incluye las bibliotecas necesarias
#include <iostream>
#include <windows.h>
#include <cmath>
#include <vector>
#include <string>

// Define el tamaño del reloj
#define RELOJ_RADIO 10

// Función para dibujar el reloj
void dibujarReloj()
{
  // Borra la pantalla
  system("cls");

  // Dibuja el círculo del reloj
  for (int x = -RELOJ_RADIO; x <= RELOJ_RADIO; x++)
  {
    for (int y = -RELOJ_RADIO; y <= RELOJ_RADIO; y++)
    {
      if (x * x + y * y <= RELOJ_RADIO * RELOJ_RADIO)
      {
        std::cout << "*";
      }
      else
      {
        std::cout << " ";
      }
    }
    std::cout << std::endl;
  }

  // Dibuja las marcas de las horas
  for (int hora = 1; hora <= 12; hora++)
  {
    double angulo = hora * 30 * M_PI / 180; // Convierte la hora a radianes
    int x = round(RELOJ_RADIO * cos(angulo));
    int y = round(RELOJ_RADIO * sin(angulo));
    std::cout << " " << hora << " ";
  }

  // Dibuja las agujas del reloj
  // Calcula el ángulo actual de la aguja de las horas
  SYSTEMTIME tiempo;
  GetSystemTime(&tiempo);
  int hora = tiempo.wHour % 12; // Convierte la hora a una hora de 12 horas
  int minuto = tiempo.wMinute;
  int segundo = tiempo.wSecond;
  double anguloHora = (hora * 60 + minuto) * M_PI / 3600;
  double anguloMinuto = minuto * 60 + segundo * M_PI / 3600;

  // Dibuja la aguja de las horas
  int xHora = round(RELOJ_RADIO * 0.75 * cos(anguloHora));
  int yHora = round(RELOJ_RADIO * 0.75 * sin(anguloHora));
  std::cout << " ";
  for (int i = 0; i < xHora; i++)
  {
    std::cout << "-";
  }
  std::cout << "H";
  for (int i = 0; i < RELOJ_RADIO - xHora; i++)
  {
    std::cout << "-";
  }
  std::cout << " " << std::endl;

  // Dibuja la aguja de los minutos
  int xMinuto = round(RELOJ_RADIO * 0.9 * cos(anguloMinuto));
  int yMinuto = round(RELOJ_RADIO * 0.9 * sin(anguloMinuto));
  for (int i = 0; i < RELOJ_RADIO; i++)
  {
    std::cout << " ";
  }
  std::cout << "M";
  for (int i = 0; i < RELOJ_RADIO - 1; i++)
  {
    std::cout << "-";
  }
  std::cout << std::endl;

  // Dibuja la aguja de los segundos
  int xSegundo = round(RELOJ_RADIO * 0.95 * cos(anguloMinuto));
  int ySegundo = round(RELOJ_RADIO * 0.95 * sin(anguloMinuto));
  for (int i = 0; i < RELOJ_RADIO; i++)
  {
    std::cout << " ";
  }
  std::cout << "S";
  for (int i = 0; i < RELOJ_RADIO - 1; i++)
  {
    std::cout << "-";
  }
  std::cout << std::endl;
}

// Función principal
int main()
{
  // Dibuja el reloj cada segundo
  while (true)
  {
    dibujarReloj();
    Sleep(1000); // Espera un segundo antes de dibujar el reloj de nuevo
  }

  return 0;
}
```

**Explicación del código:**

* La función `dibujarReloj` dibuja el reloj en la ventana de la consola.
* La función `main` es la función principal del programa. Inicializa el programa y llama a la función `dibujarReloj` cada segundo.

**Detalles del código:**

* Las bibliotecas `iostream`, `windows.h`, `cmath`, `vector` y `string` se incluyen para proporcionar las funciones necesarias para dibujar el reloj.
* La variable `RELOJ_RADIO` define el tamaño del reloj.
* El bucle while en la función `main` dibuja el reloj cada segundo.
* La función `dibujarReloj` dibuja el círculo del reloj, las marcas de las horas y las agujas del reloj.
* La función `GetSystemTime` se utiliza para obtener la hora actual del sistema.
* La variable `anguloHora` calcula el ángulo actual de la aguja de las horas.
* La variable `anguloMinuto` calcula el ángulo actual de la aguja de los minutos.
* Las variables `xHora`, `yHora`, `xMinuto`, `yMinuto`, `xSegundo` e `ySegundo` se utilizan para calcular las posiciones de las agujas del reloj.
* El símbolo `*` se utiliza para dibujar el círculo del reloj.
* Los números del 1 al 12 se utilizan para dibujar las marcas de las horas.
* Los símbolos `H`, `M` y `S` se utilizan para dibujar las agujas del reloj.
* El símbolo `-` se utiliza para dibujar las líneas de las agujas del reloj.
* El símbolo ` ` se utiliza para dibujar los espacios vacíos.

**Resultado del programa:**

El programa muestra un reloj analógico en la ventana de la consola. Las agujas del reloj se mueven en tiempo real.