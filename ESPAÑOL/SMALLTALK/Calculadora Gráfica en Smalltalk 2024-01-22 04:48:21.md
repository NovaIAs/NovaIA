```smalltalk
Objetivo: Crear una calculadora simple con una interfaz gráfica en Smalltalk.

**Clase Calculadora:**

```smalltalk
Clase Calculadora
    variables
        expresion: ''
        resultado: 0
    metodos
        calcular (unString)
            expresion := unString.
            resultado := eval(expresion).
        setResultado (unNumero)
            resultado := unNumero.
        getResultado ()
            ^ resultado.
        getExpresion ()
            ^ expresion.
```

**Clase VentanaCalculadora:**

```smalltalk
Clase VentanaCalculadora
    variables
        calculadora: Calculadora nueva.
    metodos
        initialize ()
            super initialize.
            self setBordered.
            self setTitle 'Calculadora'.
            self setSize 300@400.
            self setLayout [GridLayout nueva filas: 5 columnas: 4].

            self add [Label nueva texto: 'Expresión'].
            self add [TextField nueva].
            self add [Button nueva texto: '=' onClick: #calcular].
            self add [TextField nueva].
            self add [Button nueva texto: '7' onClick: #addNumero].
            self add [Button nueva texto: '8' onClick: #addNumero].
            self add [Button nueva texto: '9' onClick: #addNumero].
            self add [Button nueva texto: '+' onClick: #addOperador].
            self add [Button nueva texto: '4' onClick: #addNumero].
            self add [Button nueva texto: '5' onClick: #addNumero].
            self add [Button nueva texto: '6' onClick: #addNumero].
            self add [Button nueva texto: '-' onClick: #addOperador].
            self add [Button nueva texto: '1' onClick: #addNumero].
            self add [Button nueva texto: '2' onClick: #addNumero].
            self add [Button nueva texto: '3' onClick: #addNumero].
            self add [Button nueva texto: '*' onClick: #addOperador].
            self add [Button nueva texto: '0' onClick: #addNumero].
            self add [Button nueva texto: '.' onClick: #addNumero].
            self add [Button nueva texto: '(' onClick: #addParentesis].
            self add [Button nueva texto: ')' onClick: #addParentesis].
            self add [Button nueva texto: '/' onClick: #addOperador].
            self add [Button nueva texto: 'C' onClick: #limpiar].
```

**Métodos de la clase VentanaCalculadora:**

* `calcular`: Evalúa la expresión ingresada en el campo de texto y muestra el resultado en el otro campo de texto.
* `addNumero`: Agrega un dígito a la expresión ingresada.
* `addOperador`: Agrega un operador a la expresión ingresada.
* `addParentesis`: Agrega un paréntesis a la expresión ingresada.
* `limpiar`: Limpia la expresión ingresada y el resultado.

**Uso:**

* Para usar la calculadora, simplemente ingresa una expresión en el campo de texto y haz clic en el botón '='.
* El resultado se mostrará en el otro campo de texto.
* También puedes usar los botones para agregar dígitos, operadores y paréntesis a la expresión.
* El botón 'C' borra la expresión y el resultado.