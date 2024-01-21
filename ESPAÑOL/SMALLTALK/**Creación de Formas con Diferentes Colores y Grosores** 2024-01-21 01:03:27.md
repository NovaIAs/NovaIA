```smalltalk
[ | gomaLínea1 gomaLínea2 gomaColor |
  | goma1 goma2 |
  gomaLínea1 := Color con tono: 0.1 saturación: 0.5 brillo: 1.0.
  gomaLínea2 := Color con tono: 0.3 saturación: 0.8 brillo: 0.8.
  gomaColor := Color con tono: 0.9 saturación: 0.2 brillo: 0.9.

  goma1 := Goma con color: gomaLínea1 grosor: 2.0.
  goma2 := Goma con color: gomaLínea2 grosor: 1.0.

  gomaLínea1 dibujarLínea: Punto con x: 100.0 y: 100.0 a: Punto con x: 200.0 y: 200.0.
  gomaLínea2 dibujarLínea: Punto con x: 200.0 y: 100.0 a: Punto con x: 100.0 y: 200.0.
  gomaColor dibujarRectángulo: Rectángulo con origen: Punto con x: 120.0 y: 120.0 ancho: 60.0 alto: 60.0.

  goma1 dibujarLínea: Punto con x: 130.0 y: 130.0 a: Punto con x: 170.0 y: 170.0.
  goma2 dibujarLínea: Punto con x: 130.0 y: 170.0 a: Punto con x: 170.0 y: 130.0.

  gomaColor dibujarRectángulo: Rectángulo con origen: Punto con x: 220.0 y: 120.0 ancho: 60.0 alto: 60.0.

  goma1 dibujarLínea: Punto con x: 230.0 y: 130.0 a: Punto con x: 270.0 y: 170.0.
  goma2 dibujarLínea: Punto con x: 230.0 y: 170.0 a: Punto con x: 270.0 y: 130.0. ]
```

Explicación del código:

1. Se definen tres variables locales: `gomaLínea1`, `gomaLínea2` y `gomaColor`.
2. Se crean dos objetos `Goma` con diferentes colores y grosores.
3. Se dibujan dos líneas con los colores y grosores especificados.
4. Se dibuja un rectángulo con el color `gomaColor`.
5. Se dibujan dos líneas más con los mismos colores y grosores.
6. Se dibuja otro rectángulo con el color `gomaColor`.
7. Se dibujan dos líneas más con los mismos colores y grosores.