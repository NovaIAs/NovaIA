```scala
// Definiciones de tipos
type Variable = String
type Valor = Int
type Expresion = Variable | Valor | (Expresion, Operador, Expresion)
type Operador = "+" | "-" | "*" | "/"

// Definición de la función de evaluación de expresiones
def evaluar(expresion: Expresion): Valor = expresion match {
  case Variable(v) => variables(v)
  case Valor(v) => v
  case (e1, op, e2) => evaluar(e1) op evaluar(e2)
}

// Definición de la función de interpretación de expresiones
def interpretar(expresion: String): Valor = {
  // Crear el árbol de expresión a partir de la cadena de entrada
  val arbol = crearArbol(expresion)

  // Evaluar el árbol de expresión
  evaluar(arbol)
}

// Definición de la función de creación del árbol de expresión
def crearArbol(expresion: String): Expresion = {
  // Separar la expresión en tokens
  val tokens = expresion.split(/\s+/)

  // Construir el árbol de expresión recursivamente
  crearArbol(tokens)
}

// Definición de la función de construcción del árbol de expresión recursiva
def crearArbol(tokens: Array[String]): Expresion = {
  // Obtener el primer token de la lista
  val token = tokens.head

  // Si el token es un operador, crear un nodo de expresión binaria
  if (token == "+" || token == "-" || token == "*" || token == "/") {
    (crearArbol(tokens.tail), token, crearArbol(tokens.tail.tail))
  }
  // Si el token es una variable o un valor, crear un nodo de expresión unaria
  else {
    token.toIntOption match {
      case Some(valor) => valor
      case None => Variable(token)
    }
  }
}

// Definición del mapa de variables y sus valores
val variables: Map[Variable, Valor] = Map(
  "x" -> 1,
  "y" -> 2,
  "z" -> 3
)

// Leer la expresión del usuario
val expresion = scala.io.StdIn.readLine("Introduzca una expresión: ")

// Interpretar la expresión y mostrar el resultado
println(interpretar(expresión))
```

Explicación del código:

* El código define una serie de tipos personalizados para representar las expresiones matemáticas.
* La función `evaluar` evalúa una expresión matemática y devuelve su valor.
* La función `interpretar` toma una cadena de texto que representa una expresión matemática y la interpreta, devolviendo su valor.
* La función `crearArbol` toma una cadena de texto que representa una expresión matemática y crea un árbol de expresión a partir de ella.
* La función `crearArbol` recursiva toma una lista de tokens que representan una expresión matemática y crea un árbol de expresión a partir de ella.
* El código define un mapa de variables y sus valores.
* El código lee la expresión del usuario y la interpreta, mostrando el resultado.