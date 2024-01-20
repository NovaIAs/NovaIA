```python
import random

# Definición de la clase Perro
class Perro:
  def __init__(self, nombre, raza, edad, peso):
    self.nombre = nombre
    self.raza = raza
    self.edad = edad
    self.peso = peso

  def ladrar(self):
    return "Woof!"

  def comer(self, comida):
    return f"¡{self.nombre} está comiendo {comida}!"

  def jugar(self):
    return f"{self.nombre} está jugando con una pelota."

# Definición de la clase Gato
class Gato:
  def __init__(self, nombre, raza, edad, peso):
    self.nombre = nombre
    self.raza = raza
    self.edad = edad
    self.peso = peso

  def maullar(self):
    return "Meow!"

  def comer(self, comida):
    return f"{self.nombre} está comiendo {comida}!"

  def jugar(self):
    return f"{self.nombre} está jugando con un ratón de juguete."

# Definición de la clase Animal
class Animal:
  def __init__(self, nombre, edad, peso):
    self.nombre = nombre
    self.edad = edad
    self.peso = peso

  def respirar(self):
    return "Estoy respirando."

  def dormir(self):
    return "Estoy durmiendo."

  def moverse(self):
    return "Me estoy moviendo."

# Creamos una lista de perros y gatos
perros = [
  Perro("Toby", "Golden Retriever", 3, 25),
  Perro("Luna", "Labrador", 2, 20),
  Perro("Rocky", "Pitbull", 5, 30)
]

gatos = [
  Gato("Kitty", "Siamés", 1, 5),
  Gato("Fluffy", "Persa", 4, 8),
  Gato("Whiskers", "Maine Coon", 6, 12)
]

# Creamos una lista de todos los animales
animales = perros + gatos

# Recorremos la lista de animales y llamamos a los métodos de cada uno
for animal in animales:
  print(f"{animal.nombre} es un {animal.raza} de {animal.edad} años y pesa {animal.peso} kilos.")
  print(animal.respirar())
  print(animal.dormir())
  print(animal.moverse())

  if isinstance(animal, Perro):
    print(animal.ladrar())
    print(animal.comer("croquetas"))
    print(animal.jugar())
  elif isinstance(animal, Gato):
    print(animal.maullar())
    print(animal.comer("comida para gatos"))
    print(animal.jugar())

# Elegimos un animal al azar de la lista
animal_aleatorio = random.choice(animales)

# Imprimimos el nombre del animal aleatorio
print(f"El animal aleatorio es {animal_aleatorio.nombre}.")
```

Explicación del código:

1. Se definen las clases Perro, Gato y Animal.
2. Se crean listas de perros y gatos.
3. Se crea una lista de todos los animales.
4. Se recorre la lista de animales y se llama a los métodos de cada uno.
5. Se elige un animal al azar de la lista.
6. Se imprime el nombre del animal aleatorio.

Este código es un ejemplo de polimorfismo en Python. El polimorfismo permite que diferentes objetos respondan a los mismos métodos de diferentes maneras. En este caso, los objetos Perro y Gato responden al método comer() de diferentes maneras. El objeto Perro come croquetas y el objeto Gato come comida para gatos.