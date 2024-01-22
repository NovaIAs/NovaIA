```lua
-- DEFINIENDO MÓDULOS Y BIBLIOTECAS EXTERNAS
local require = require
local json = require("json")
local uuid = require("uuid")

-- CREANDO UNA CLASE PADRE "PERSONA"
local Persona = {}
Persona.nombre = nil
Persona.apellido = nil
Persona.edad = nil

-- MÉTODO CONSTRUCTOR PARA LA CLASE "PERSONA"
function Persona:new(nombre, apellido, edad)
    self.nombre = nombre
    self.apellido = apellido
    self.edad = edad
end

-- MÉTODO "PRESENTARSE" PARA LA CLASE "PERSONA"
function Persona:presentarse()
    print("Hola, mi nombre es " .. self.nombre .. " " .. self.apellido .. " y tengo " .. self.edad .. " años.")
end

-- CREANDO UNA CLASE HIJA "ESTUDIANTE" QUE HEREDA DE "PERSONA"
local Estudiante = {}
Estudiante.__index = Persona

-- MÉTODO CONSTRUCTOR PARA LA CLASE "ESTUDIANTE"
function Estudiante:new(nombre, apellido, edad, matricula)
    Persona:new(self, nombre, apellido, edad)
    self.matricula = matricula
end

-- MÉTODO "ESTUDIAR" PARA LA CLASE "ESTUDIANTE"
function Estudiante:estudiar()
    print("Estoy estudiando mucho para mi examen de mañana.")
end

-- CREANDO OBJETOS DE LAS CLASES "PERSONA" Y "ESTUDIANTE"
local persona1 = Persona:new("Juan", "García", 35)
local estudiante1 = Estudiante:new("María", "López", 20, "123456")

-- LLAMANDO A MÉTODOS DE OBJETOS DE LAS CLASES "PERSONA" Y "ESTUDIANTE"
persona1:presentarse()
estudiante1:presentarse()
estudiante1:estudiar()

-- CREANDO UN VECTOR (ARRAY) DE NÚMEROS
local vectorNumeros = {1, 2, 3, 4, 5}

-- ITERANDO POR EL VECTOR DE NÚMEROS Y MOSTRÁNDOLOS
for numero in vectorNumeros do
    print(numero)
end

-- CREANDO UN DICCIONARIO (HASH) DE CADENAS
local diccionarioCadenas = {["clave1"] = "valor1", ["clave2"] = "valor2"}

-- ITERANDO POR EL DICCIONARIO DE CADENAS Y MOSTRANDO LAS CLAVES Y VALORES
for clave, valor in pairs(diccionarioCadenas) do
    print(clave, valor)
end

-- CREANDO UNA FUNCIÓN QUE CALCULA EL FACTORIAL DE UN NÚMERO
local function factorial(numero)
    if numero == 0 then
        return 1
    else
        return numero * factorial(numero - 1)
    end
end

-- CALCULAR Y MOSTRAR EL FACTORIAL DE 5
print("Factorial de 5:", factorial(5))

-- CREANDO UN MÓDULO CON FUNCIONES DE MANIPULACIÓN DE CADENAS
local Cadena = {}
Cadena.longitud = function(cadena)
    return #cadena
end
Cadena.invertir = function(cadena)
    return string.reverse(cadena)
end

-- CARGAR Y PARSEAR UN ARCHIVO JSON
local archivoJSON = io.open("datos.json", "r")
local datosJSON = json.decode(archivoJSON:read("*a"))
archivoJSON:close()

-- ITERANDO POR LOS DATOS JSON Y MOSTRANDO LOS NOMBRES DE LAS PERSONAS
for persona in datosJSON.personas do
    print(persona.nombre)
end

-- GENERAR UN IDENTIFICADOR ÚNICO UNIVERSAL (UUID)
local uuid = uuid.generate()

-- MOSTRAR EL UUID GENERADO
print("UUID:", uuid)
```

Explicación del código:

* Se definen módulos y bibliotecas externas, como `json`, `uuid` y `string`.
* Se crea una clase padre "Persona" con propiedades `nombre`, `apellido` y `edad`, y un método constructor y un método `presentarse()`.
* Se crea una clase hija "Estudiante" que hereda de "Persona", con una propiedad adicional `matricula` y un método `estudiar()`.
* Se crean objetos de las clases "Persona" y "Estudiante" y se llaman a sus métodos.
* Se crea un vector (array) de números y se itera por él, mostrando los números.
* Se crea un diccionario (hash) de cadenas y se itera por él, mostrando las claves y valores.
* Se crea una función que calcula el factorial de un número.
* Se crea un módulo `Cadena` con funciones de manipulación de cadenas.
* Se carga y parsea un archivo JSON, y se itera por los datos JSON, mostrando los nombres de las personas.
* Se genera un identificador único universal (UUID).