```python
# Requerir bibliotecas
import random
import time
import string

# Definir la clase Juego de palabras
class JuegoDePalabras:

    # Inicializar el juego
    def __init__(self):
        self.palabras = ["manzana", "naranja", "pera", "uva", "plátano"]
        self.palabra_secreta = random.choice(self.palabras)
        self.vidas = 10
        self.letras_adivinadas = []

    # Obtener la palabra secreta
    def get_palabra_secreta(self):
        return self.palabra_secreta

    # Obtener las vidas restantes
    def get_vidas(self):
        return self.vidas

    # Obtener las letras adivinadas
    def get_letras_adivinadas(self):
        return self.letras_adivinadas

    # Comprobar si la letra está en la palabra secreta
    def comprobar_letra(self, letra):
        if letra not in string.ascii_lowercase:
            print("No puedes adivinar caracteres especiales.")
            return False

        if letra in self.letras_adivinadas:
            print("Ya has adivinado esa letra.")
            return False

        if letra in self.palabra_secreta:
            self.letras_adivinadas.append(letra)
            return True
        else:
            self.vidas -= 1
            return False

    # Comprobar si el juego ha terminado
    def has_terminado(self):
        if self.vidas <= 0:
            return True

        if set(self.palabra_secreta) == set(self.letras_adivinadas):
            return True

        return False

    # Mostrar el estado del juego
    def mostrar_estado(self):
        print("Vidas restantes:", self.vidas)
        print("Letras adivinadas:", self.letras_adivinadas)

        palabra_temporal = ""

        for letra in self.palabra_secreta:
            if letra in self.letras_adivinadas:
                palabra_temporal += letra
            else:
                palabra_temporal += "_"

        print("Palabra:", palabra_temporal)


# Definir la clase Jugador
class Jugador:

    # Inicializar al jugador
    def __init__(self, nombre):
        self.nombre = nombre
        self.puntuacion = 0

    # Obtener el nombre del jugador
    def get_nombre(self):
        return self.nombre

    # Obtener la puntuación del jugador
    def get_puntuacion(self):
        return self.puntuacion

    # Aumentar la puntuación del jugador
    def aumentar_puntuacion(self):
        self.puntuacion += 1


# Definir la función principal
def main():

    # Bienvenida al juego
    print("¡Bienvenido al juego de palabras!")

    # Crear una instancia del juego
    juego = JuegoDePalabras()

    # Crear una instancia del jugador
    jugador = Jugador("Jugador 1")

    # Bucle principal del juego
    while not juego.has_terminado():

        # Mostrar el estado del juego
        juego.mostrar_estado()

        # Pedirle al jugador que adivine una letra
        letra = input("Adivina una letra: ")

        # Comprobar si la letra es válida
        if not juego.comprobar_letra(letra):
            continue

        # Comprobar si el juego ha terminado
        if juego.has_terminado():
            break

        # Aumentar la puntuación del jugador
        jugador.aumentar_puntuacion()

    # Mostrar el resultado del juego
    if juego.vidas <= 0:
        print("¡Has perdido!")
    else:
        print("¡Has ganado!")

    print("Tu puntuación:", jugador.get_puntuacion())


# Llamada a la función principal
if __name__ == "__main__":
    main()
```

Explicación del código:

* La clase `JuegoDePalabras` representa el juego de palabras. Contiene la palabra secreta, las vidas restantes y las letras adivinadas.
* La clase `Jugador` representa al jugador que juega al juego. Contiene el nombre del jugador y la puntuación.
* La función `main()` es la función principal del juego. Crea una instancia del juego y una instancia del jugador, y luego entra en el bucle principal del juego.
* El bucle principal del juego primero muestra el estado del juego. Luego, le pide al jugador que adivine una letra. Si la letra es válida, comprueba si la letra está en la palabra secreta. Si la letra está en la palabra secreta, aumenta la puntuación del jugador. Si la letra no está en la palabra secreta, disminuye las vidas del jugador.
* El bucle principal del juego comprueba si el juego ha terminado. Si el jugador no tiene más vidas, pierde el juego. Si el jugador ha adivinado todas las letras de la palabra secreta, gana el juego.
* Al final del juego, se muestra el resultado del juego. Si el jugador perdió, se le muestra el mensaje "Has perdido!". Si el jugador ganó, se le muestra el mensaje "Has ganado!". También se muestra la puntuación del jugador.