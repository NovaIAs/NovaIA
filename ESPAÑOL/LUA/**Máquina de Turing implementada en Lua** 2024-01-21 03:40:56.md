```lua
-- Este código es un programa que simula una máquina de Turing.
-- Una máquina de Turing es un modelo matemático de computación que consiste en una cinta infinita dividida en celdas, un cabezal de lectura/escritura que puede moverse a lo largo de la cinta y un conjunto de instrucciones que le indican al cabezal qué hacer en cada estado.

-- Esta máquina de Turing tiene un alfabeto de dos símbolos: 0 y 1.
-- La cinta está inicialmente vacía.
-- El cabezal de lectura/escritura comienza en la primera celda de la cinta.
-- Las instrucciones de la máquina de Turing están codificadas en una tabla.
-- Cada instrucción tiene tres partes:
--   * El estado actual del cabezal de lectura/escritura.
--   * El símbolo que el cabezal de lectura/escritura está leyendo.
--   * La acción que el cabezal de lectura/escritura debe realizar.

-- La acción puede ser:
--   * Escribir un símbolo en la celda actual.
--   * Mover el cabezal de lectura/escritura a la izquierda o a la derecha.
--   * Cambiar el estado del cabezal de lectura/escritura.
--   * Aceptar o rechazar la entrada.

-- El programa comienza leyendo la entrada del usuario.
-- Luego, inicializa la máquina de Turing con la cinta vacía, el cabezal de lectura/escritura en la primera celda y el estado inicial del cabezal de lectura/escritura.
-- Luego, ejecuta las instrucciones de la máquina de Turing una a una hasta que la máquina de Turing acepte o rechace la entrada.

-- Si la máquina de Turing acepta la entrada, el programa imprime un mensaje de aceptación.
-- Si la máquina de Turing rechaza la entrada, el programa imprime un mensaje de rechazo.

-- Aquí está el código del programa:

local cinta = {}
local cabezal = 1
local estado = 1
local instrucciones = {
  {1, 0, {1, '1', 2}},
  {2, 1, {1, '0', 3}},
  {3, 0, {1, '1', 4}},
  {4, 1, {1, '0', 5}},
  {5, 0, {2, '1', 6}},
  {6, 1, {2, '0', 7}},
  {7, 0, {2, '1', 8}},
  {8, 1, {2, '0', 9}},
  {9, 0, {3, '1', 10}},
  {10, 1, {3, '0', 11}},
  {11, 0, {3, '1', 12}},
  {12, 1, {3, '0', 13}},
  {13, 0, {4, '1', 14}},
  {14, 1, {4, '0', 15}},
  {15, 0, {4, '1', 16}},
  {16, 1, {4, '0', 17}},
  {17, 0, {5, '1', 18}},
  {18, 1, {5, '0', 19}},
  {19, 0, {5, '1', 20}},
  {20, 1, {5, '0', 21}},
  {21, 0, {6, '1', 22}},
  {22, 1, {6, '0', 23}},
  {23, 0, {6, '1', 24}},
  {24, 1, {6, '0', 25}},
  {25, 0, {7, '1', 26}},
  {26, 1, {7, '0', 27}},
  {27, 0, {7, '1', 28}},
  {28, 1, {7, '0', 29}},
  {29, 0, {8, '1', 30}},
  {30, 1, {8, '0', 31}},
  {31, 0, {8, '1', 32}},
  {32, 1, {8, '0', 33}},
  {33, 0, {9, '1', 34}},
  {34, 1, {9, '0', 35}},
  {35, 0, {9, '1', 36}},
  {36, 1, {9, '0', 37}},
  {37, 0, {10, '1', 38}},
  {38, 1, {10, '0', 39}},
  {39, 0, {10, '1', 40}},
  {40, 1, {10, '0', 41}},
  {41, 0, {11, '1', 42}},
  {42, 1, {11, '0', 43}},
  {43, 0, {11, '1', 44}},
  {44, 1, {11, '0', 45}},
  {45, 0, {12, '1', 46}},
  {46, 1, {12, '0', 47}},
  {47, 0, {12, '1', 48}},
  {48, 1, {12, '0', 49}},
  {49, 0, {13, '1', 50}},
  {50, 1, {13, '0', 51}},
  {51, 0, {13, '1', 52}},
  {52, 1, {13, '0', 53}},
  {53, 0, {14, '1', 54}},
  {54, 1, {14, '0', 55}},
  {55, 0, {14, '1', 56}},
  {56, 1, {14, '0', 57}},
  {57, 0, {15, '1', 58}},
  {58, 1, {15, '0', 59}},
  {59, 0, {15, '1', 60}},
  {60, 1, {15, '0', 61}},
  {61, 0, {16, '1', 62}},
  {62, 1, {16, '0', 63}},
  {63, 0, {16, '1', 64}},
  {64, 1, {16, '0', 65}},
  {65, 0, {17, '1', 66}},
  {66, 1, {17, '0', 67}},
  {67, 0, {17, '1', 68}},
  {68, 1, {17, '0', 69}},
  {69, 0, {18, '1', 70}},
  {70, 1, {18, '0', 71}},
  {71, 0, {18, '1', 72}},
  {72, 1, {18, '0', 73}},
  {73, 0, {19, '1', 74}},
  {74, 1, {19, '0', 75}},
  {75, 0, {19, '1', 76}},
  {76, 1, {19, '0', 77}},
  {77, 0, {20, '1', 78}},
  {78, 1, {20, '0', 79}},
  {79, 0, {20, '1', 80}},
  {80, 1, {20, '0', 81}},
  {81, 0, {21, '1', 82}},
  {82, 1, {21, '0', 83}},
  {83, 0, {21, '1', 84}},
  {84, 1, {21, '0', 85}},
  {85, 0, {22, '1', 86}},
  {86, 1, {22, '0', 87}},
  {87, 0, {22, '1', 88}},
  {88, 1, {22, '0', 89}},
  {89, 0, {23, '1', 90}},
  {90, 1, {23, '0', 91}},
  {91, 0, {23, '1', 92}},
  {92, 1, {23, '0', 93}},
  {93, 0, {24, '1', 94}},
  {94, 1, {24, '0', 95}},
  {95, 0, {24, '1', 96}},
  {96, 1, {24, '0', 97}},
  {97, 0, {25, '1', 