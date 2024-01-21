**Código:**

```haskell
-- Módulo principal del programa
module Main where

-- Función principal del programa
main :: IO ()
main = do
  -- Obtiene el nombre del usuario
  putStr "Ingrese su nombre: "
  nombre <- getLine

  -- Crea un mensaje de bienvenida
  mensaje <- return ("¡Bienvenido, " ++ nombre ++ "!")

  -- Imprime el mensaje de bienvenida
  putStrLn mensaje
```

**Explicación del código:**

El código anterior es un programa sencillo en Haskell que solicita al usuario su nombre y le da la bienvenida. El código está dividido en dos partes: el módulo principal y la función principal.

El módulo principal define el tipo de datos que se utilizará en el programa. En este caso, se utiliza el tipo de datos `IO`, que representa una acción que puede tener efectos secundarios.

La función principal es la función principal del programa. Esta función toma un argumento de tipo `IO ()` y devuelve un valor de tipo `IO ()`. El valor de tipo `IO ()` representa una acción que no devuelve ningún valor.

En la función principal, se utiliza la función `putStr` para imprimir un mensaje en la consola. La función `getLine` se utiliza para obtener una línea de texto del usuario. La función `return` se utiliza para crear un valor de tipo `IO a`, donde `a` es un tipo de datos arbitrario.

El código anterior es un ejemplo sencillo de un programa en Haskell. Haskell es un lenguaje de programación funcional que es muy diferente de otros lenguajes de programación imperativos, como C o Java. Haskell es un lenguaje muy poderoso y expresivo, y se utiliza para desarrollar una amplia variedad de aplicaciones, desde sistemas operativos hasta aplicaciones web.