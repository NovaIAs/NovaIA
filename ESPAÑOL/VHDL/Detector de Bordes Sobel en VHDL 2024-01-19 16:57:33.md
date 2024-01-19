```vhdl
--Código VHDL para un detector de bordes Sobel
--Este código implementa el algoritmo de Sobel para detectar bordes en una imagen digital.
--El algoritmo utiliza dos máscaras de convolución, una para la dirección horizontal y otra para la dirección vertical.
--La salida del algoritmo es una imagen en escala de grises que muestra la intensidad de los bordes en cada píxel.

--Librerías
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

--Constantes
constant ancho_imagen : integer := 640; --Ancho de la imagen en píxeles
constant alto_imagen : integer := 480; --Alto de la imagen en píxeles

--Definición de tipos
type imagen is array (integer range <>, integer range <>) of unsigned(7 downto 0); --Tipo de imagen en escala de grises

--Señales
signal imagen_entrada : imagen; --Imagen de entrada
signal resultado_sobel : imagen; --Imagen de salida con los bordes detectados

--Procesos
process
begin
  --Calcular la imagen de salida utilizando el algoritmo de Sobel
  for x in 0 to ancho_imagen-1 loop
    for y in 0 to alto_imagen-1 loop
      --Máscaras de convolución para las direcciones horizontal y vertical
      constant mascara_horizontal : array (integer range <>, integer range <>) of unsigned(7 downto 0) := (
        (0, 0, 0),
        (1, 2, 1),
        (0, 0, 0)
      );
      constant mascara_vertical : array (integer range <>, integer range <>) of unsigned(7 downto 0) := (
        (0, 1, 0),
        (0, 2, 0),
        (0, 1, 0)
      );

      --Calcular el valor del píxel de salida
      resultado_sobel(x, y) := abs(convolucion(mascara_horizontal, imagen_entrada(x-1, y-1:x+1, y-1:y+1))) + abs(convolucion(mascara_vertical, imagen_entrada(x-1, y-1:x+1, y-1:y+1)));
    end loop;
  end loop;
end process;

--Función para calcular la convolución de una máscara con una imagen
function convolucion(mascara : array (integer range <>, integer range <>) of unsigned(7 downto 0); imagen : imagen) return unsigned(7 downto 0) is
begin
  --Calcular la suma de los productos de los valores de la máscara y la imagen
  return suma_productos(mascara, imagen);
end function;

--Función para calcular la suma de los productos de dos matrices
function suma_productos(mascara : array (integer range <>, integer range <>) of unsigned(7 downto 0); imagen : imagen) return unsigned(7 downto 0) is
begin
  --Variables locales
  variable suma : unsigned(7 downto 0) := 0; --Variable para almacenar la suma de los productos

  --Recorrer la matriz y multiplicar cada elemento de la máscara con el correspondiente elemento de la imagen
  for i in mascara'range(1) to mascara'range(2) loop
    for j in mascara'range(1) to mascara'range(2) loop
      suma := suma + mascara(i, j) * imagen(i, j);
    end loop;
  end loop;

  --Devolver la suma de los productos
  return suma;
end function;
```

Este código implementa el algoritmo de Sobel para detectar bordes en una imagen digital. El algoritmo utiliza dos máscaras de convolución, una para la dirección horizontal y otra para la dirección vertical. La salida del algoritmo es una imagen en escala de grises que muestra la intensidad de los bordes en cada píxel.

El código VHDL consta de varios componentes:

* **Librerías:** Este código utiliza las librerías IEEE.STD_LOGIC_1164.ALL e IEEE.NUMERIC_STD.ALL. Estas librerías proporcionan funciones y constantes que se utilizan en el código.
* **Constantes:** Este código define la constante ancho_imagen y alto_imagen, que se utilizan para especificar el tamaño de la imagen de entrada.
* **Tipos:** Este código define el tipo imagen, que es un tipo de datos para representar una imagen en escala de grises.
* **Señales:** Este código define las señales imagen_entrada y resultado_sobel. La señal imagen_entrada es la imagen de entrada que se va a procesar, y la señal resultado_sobel es la imagen de salida que contiene los bordes detectados.
* **Procesos:** Este código define un proceso que se ejecuta de forma concurrente con el resto del código. Este proceso calcula la imagen de salida utilizando el algoritmo de Sobel.
* **Funciones:** Este código define las funciones convolucion y suma_productos. La función convolución calcula la convolución de una máscara con una imagen, y la función suma_productos calcula la suma de los productos de dos matrices.

El código VHDL se puede utilizar para generar un circuito digital que implemente el algoritmo de Sobel. Este circuito se puede utilizar para detectar bordes en imágenes digitales en tiempo real.