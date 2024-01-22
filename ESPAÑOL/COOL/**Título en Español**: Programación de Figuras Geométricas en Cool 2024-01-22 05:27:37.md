```cool

clase Punto {
    atributos:
        x: entero
        y: entero

    métodos:
        constructor(x, y) {
            self.x = x
            self.y = y
        }

        suma(otroPunto) {
            return new Punto(self.x + otroPunto.x, self.y + otroPunto.y)
        }

        resta(otroPunto) {
            return new Punto(self.x - otroPunto.x, self.y - otroPunto.y)
        }

        distancia(otroPunto) {
            return Math.sqrt(((self.x - otroPunto.x) ** 2) + ((self.y - otroPunto.y) ** 2))
        }

        esIgual(otroPunto) {
            return self.x == otroPunto.x and self.y == otroPunto.y
        }

        toString() {
            return "(" + self.x + ", " + self.y + ")"
        }
}

clase Rectangulo {
    atributos:
        esquinaSuperiorIzquierda: Punto
        esquinaInferiorDerecha: Punto

    métodos:
        constructor(esquinaSuperiorIzquierda, esquinaInferiorDerecha) {
            self.esquinaSuperiorIzquierda = esquinaSuperiorIzquierda
            self.esquinaInferiorDerecha = esquinaInferiorDerecha
        }

        ancho() {
            return self.esquinaInferiorDerecha.x - self.esquinaSuperiorIzquierda.x
        }

        alto() {
            return self.esquinaInferiorDerecha.y - self.esquinaSuperiorIzquierda.y
        }

        area() {
            return self.ancho() * self.alto()
        }

        perimetro() {
            return 2 * (self.ancho() + self.alto())
        }

        contiene(punto) {
            return punto.x >= self.esquinaSuperiorIzquierda.x and punto.x <= self.esquinaInferiorDerecha.x and punto.y >= self.esquinaSuperiorIzquierda.y and punto.y <= self.esquinaInferiorDerecha.y
        }

        interseca(otroRectangulo) {
            return self.contiene(otroRectangulo.esquinaSuperiorIzquierda) or self.contiene(otroRectangulo.esquinaInferiorDerecha) or otroRectangulo.contiene(self.esquinaSuperiorIzquierda) or otroRectangulo.contiene(self.esquinaInferiorDerecha)
        }

        toString() {
            return "Rectangulo(" + self.esquinaSuperiorIzquierda.toString() + ", " + self.esquinaInferiorDerecha.toString() + ")"
        }
}

clase Circulo {
    atributos:
        centro: Punto
        radio: entero

    métodos:
        constructor(centro, radio) {
            self.centro = centro
            self.radio = radio
        }

        area() {
            return Math.PI * self.radio ** 2
        }

        perimetro() {
            return 2 * Math.PI * self.radio
        }

        contiene(punto) {
            return self.centro.distancia(punto) <= self.radio
        }

        interseca(otroCirculo) {
            return self.centro.distancia(otroCirculo.centro) <= self.radio + otroCirculo.radio
        }

        toString() {
            return "Circulo(" + self.centro.toString() + ", " + self.radio + ")"
        }
}

clase Poligono {
    atributos:
        puntos: lista de Punto

    métodos:
        constructor(puntos) {
            self.puntos = puntos
        }

        area() {
            // Calcula el área del polígono usando la fórmula de Shoelace
            area = 0
            for (i = 0; i < self.puntos.length - 1; i++) {
                area += self.puntos[i].x * self.puntos[i + 1].y - self.puntos[i + 1].x * self.puntos[i].y
            }
            area += self.puntos[self.puntos.length - 1].x * self.puntos[0].y - self.puntos[0].x * self.puntos[self.puntos.length - 1].y
            return Math.abs(area) / 2
        }

        perimetro() {
            // Calcula el perímetro del polígono sumando las distancias entre los puntos
            perimetro = 0
            for (i = 0; i < self.puntos.length - 1; i++) {
                perimetro += self.puntos[i].distancia(self.puntos[i + 1])
            }
            perimetro += self.puntos[self.puntos.length - 1].distancia(self.puntos[0])
            return perimetro
        }

        contiene(punto) {
            // Comprueba si el polígono contiene al punto usando el algoritmo del rayo
            rayo = new Punto(-1000, punto.y)
            intersecciones = 0
            for (i = 0; i < self.puntos.length - 1; i++) {
                if (seIntersectan(self.puntos[i], self.puntos[i + 1], rayo)) {
                    intersecciones += 1
                }
            }
            if (seIntersectan(self.puntos[self.puntos.length - 1], self.puntos[0], rayo)) {
                intersecciones += 1
            }
            return intersecciones % 2 == 1
        }

        seIntersectan(a, b, rayo) {
            // Comprueba si los segmentos de línea (a, b) y (rayo, infinito) se intersectan
            if (a.y > b.y) {
                a, b = b, a
            }
            if (rayo.