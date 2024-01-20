```smalltalk
"Clase Rectangulo"

Rectangulo nuevaCon: unaEsquina y: unLado
    | ancho alto |
    ancho := unLado.
    alto := unLado.
    unaEsquina nuevoCon: unaEsquina.
    super nueva.

ancho
    ^ ancho.

alto
    ^ alto.

unaEsquina
    ^ unaEsquina.

muevetePor: unPunto
    unaEsquina muevetePor: unPunto.

redimensionateA: nuevoAncho y: nuevoAlto
    ancho := nuevoAncho.
    alto := nuevoAlto.
```

```smalltalk
"Clase Cuadrado"

Cuadrado nuevaCon: unaEsquina y: unLado
    ^ Rectangulo nuevaCon: unaEsquina y: unLado.

ancho
    ^ lado.

alto
    ^ lado.

dondeEstaElCentro
    ^ unaEsquina suma: (lado / 2).

muevetePor: unPunto
    unaEsquina muevetePor: unPunto.

redimensionateA: nuevoLado
    ancho := nuevoLado.
    alto := nuevoLado.
```

```smalltalk
"Clase Circulo"

Circulo nuevaCon: unCentro y: unRadio
    ^ nuevaCon: unCentro radio: unRadio.

nuevaCon: unCentro radio: unRadio
    unaEsquina nuevoCon: unCentro.
    lado := unRadio.
    super nueva.

dondeEstaElCentro
    ^ unaEsquina.

muevetePor: unPunto
    unaEsquina muevetePor: unPunto.

redimensionateA: nuevoRadio
    lado := nuevoRadio.
```

```smalltalk
"Clase Lienzo"

Lienzo nuevoCon: unMundo
    mundo := unMundo.
    super nuevo.

dibuja
    mundo dibujateEn: self.
```

```smalltalk
"Clase Mundo"

Mundo nuevaCon: unAncho y: unAlto
    ancho := unAncho.
    alto := unAlto.
    super nuevo.

dibujateEn: unLienzo
    super dibujateEn: unLienzo.
    unLienzo dibujaForma: cuadrado.
    unLienzo dibujaForma: rectangulo.
    unLienzo dibujaForma: circulo.
```

```smalltalk
"Clase Forma"

Forma nueva
    super nueva.

dibujateEn: unLienzo
    "Código para dibujar la forma en el lienzo"

muevetePor: unPunto
    "Código para mover la forma"

redimensionateA: nuevoAncho y: nuevoAlto
    "Código para redimensionar la forma"
```

```smalltalk
"Simulador"

Simulador nuevaCon: unLienzo
    lienzo := unLienzo.
    mundo := Mundo nuevaCon: 500 y: 500.

inicia
    mundo nuevaCon: 500 y: 500.
    loop [lienzo dibuja; Delay wait: 0.1].

tecladoPulsado: unEvento
    si [unEvento tecla = keyCodeEscape] [^ self quit].
    si [unEvento tecla = keyCodeUp] [cuadrado muevetePor: Punto nuevoCon: 0 y: -10].
    si [unEvento tecla = keyCodeDown] [cuadrado muevetePor: Punto nuevoCon: 0 y: 10].
    si [unEvento tecla = keyCodeLeft] [cuadrado muevetePor: Punto nuevoCon: -10 y: 0].
    si [unEvento tecla = keyCodeRight] [cuadrado muevetePor: Punto nuevoCon: 10 y: 0].
```

Este código crea una jerarquía de clases para representar formas geométricas (Rectángulo, Cuadrado, Círculo), un lienzo para dibujarlas (Lienzo) y un mundo que contiene las formas (Mundo). También incluye un simulador (Simulador) que crea un mundo, lo dibuja en un lienzo y responde a los eventos del teclado para mover las formas.

El código está escrito en Smalltalk, un lenguaje de programación orientado a objetos. Smalltalk es un lenguaje muy expresivo y fácil de leer, lo que lo hace ideal para crear simulaciones y otros tipos de aplicaciones gráficas.

El código comienza definiendo la clase Rectángulo. La clase Rectángulo tiene tres atributos: una esquina, un ancho y un alto. La clase Rectángulo también tiene métodos para mover y redimensionar el rectángulo.

La clase Cuadrado es una subclase de la clase Rectángulo. La clase Cuadrado tiene un solo atributo: un lado. La clase Cuadrado también tiene métodos para mover y redimensionar el cuadrado.

La clase Círculo es otra subclase de la clase Rectángulo. La clase Círculo tiene dos atributos: un centro y un radio. La clase Círculo también tiene métodos para mover y redimensionar el círculo.

La clase Lienzo es una clase que representa un lienzo de dibujo. La clase Lienzo tiene un solo atributo: un mundo. La clase Lienzo también tiene un método para dibujar el mundo en el lienzo.

La clase Mundo es una clase que representa un mundo que contiene formas geométricas. La clase Mundo tiene dos atributos: un ancho y un alto. La clase Mundo también tiene un método para dibujar el mundo en un lienzo.

La clase Forma es una clase abstracta que representa una forma geométrica. La clase Forma tiene métodos para dibujar la forma, mover la forma y redimensionar la forma.

La clase Simulador es una clase que crea un mundo, lo dibuja en un lienzo y responde a los eventos del teclado para mover las formas. La clase Simulador tiene un solo atributo: un lienzo. La clase Simulador también tiene un método para iniciar el simulador.

El código termina creando un simulador, iniciando el simulador y respondiendo a los eventos del teclado para mover las formas.