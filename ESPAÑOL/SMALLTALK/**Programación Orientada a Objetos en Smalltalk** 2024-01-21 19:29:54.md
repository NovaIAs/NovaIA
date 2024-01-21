```smalltalk
;; Clase para crear rectángulos
Clase Rectángulo:
    # Crear un rectángulo con las dimensiones especificadas
    new: unAncho unaAltura [
        super new.
        ancho := unAncho.
        altura := unaAltura
    ]

    # Obtener el ancho del rectángulo
    ancho:
        ^ancho

    # Obtener la altura del rectángulo
    altura:
        ^altura

    # Obtener el área del rectángulo
    área:
        ^ancho * altura
```

;; Clase para crear círculos

```smalltalk
Clase Círculo:
    # Crear un círculo con el radio especificado
    new: unRadio [
        super new.
        radio := unRadio
    ]

    # Obtener el radio del círculo
    radio:
        ^radio

    # Obtener el área del círculo
    área:
        ^Pi * radio * radio
```

;; Clase para crear formas compuestas

```smalltalk
Clase FormaCompuesta:
    # Crear una forma compuesta con las formas especificadas
    new: unasFormas [
        super new.
        formas := unasFormas
    ]

    # Obtener las formas de la forma compuesta
    formas:
        ^formas

    # Obtener el área de la forma compuesta
    área:
        ^formas collect: [:forma | forma área] sum
```

;; Clase para crear cuadrados

```smalltalk
Clase Cuadrado:
    # Crear un cuadrado con el lado especificado
    new: unLado [
        super new: unLado unLado
    ]
```

;; Clase para crear triángulos

```smalltalk
Clase Triángulo:
    # Crear un triángulo con las dimensiones especificadas
    new: unLadoUnaBase unaAltura [
        super new: unLado unaLado.
        base := unaBase.
        altura := unaAltura
    ]

    # Obtener la base del triángulo
    base:
        ^base

    # Obtener la altura del triángulo
    altura:
        ^altura

    # Obtener el área del triángulo
    área:
        ^0.5 * base * altura
```

;; Clase para crear un lienzo para dibujar formas

```smalltalk
Clase Lienzo:
    # Crear un lienzo
    new [
        super new.
        formas := Array new
    ]

    # Agregar una forma al lienzo
    agregarForma: unaForma [
        formas addLast: unaForma
    ]

    # Dibujar el lienzo
    dibujar [
        formas do: [:forma | forma dibujar]
    ]
```

;; Código de ejemplo para crear y dibujar formas

```smalltalk
unLienzo := Lienzo new.
unRectángulo := Rectángulo new: 100 50.
unCírculo := Círculo new: 25.
unCuadrado := Cuadrado new: 30.
unTriángulo := Triángulo new: 50 60 30.
unLienzo agregarForma: unRectángulo.
unLienzo agregarForma: unCírculo.
unLienzo agregarForma: unCuadrado.
unLienzo agregarForma: unTriángulo.
unLienzo dibujar.
```

Este código crea un lienzo y luego agrega varias formas al lienzo, incluyendo un rectángulo, un círculo, un cuadrado y un triángulo. Luego, dibuja el lienzo en la pantalla.