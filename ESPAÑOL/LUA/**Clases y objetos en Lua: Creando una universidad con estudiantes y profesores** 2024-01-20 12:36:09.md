```lua
-- Definición de la clase Persona
Persona = {}

-- Constructor de la clase Persona
function Persona:__init(nombre, edad, sexo)
  self.nombre = nombre
  self.edad = edad
  self.sexo = sexo
end

-- Método que devuelve el nombre de la persona
function Persona:getNombre()
  return self.nombre
end

-- Método que devuelve la edad de la persona
function Persona:getEdad()
  return self.edad
end

-- Método que devuelve el sexo de la persona
function Persona:getSexo()
  return self.sexo
end

-- Método que devuelve una cadena con los datos de la persona
function Persona:toString()
  return string.format("Nombre: %s, Edad: %d, Sexo: %s", self.nombre, self.edad, self.sexo)
end

-- Definición de la clase Estudiante
Estudiante = {}

-- Constructor de la clase Estudiante
function Estudiante:__init(nombre, edad, sexo, matricula, carrera)
  Persona:__init(self, nombre, edad, sexo)
  self.matricula = matricula
  self.carrera = carrera
end

-- Método que devuelve la matrícula del estudiante
function Estudiante:getMatricula()
  return self.matricula
end

-- Método que devuelve la carrera del estudiante
function Estudiante:getCarrera()
  return self.carrera
end

-- Método que devuelve una cadena con los datos del estudiante
function Estudiante:toString()
  return string.format("%s, Matrícula: %s, Carrera: %s", Persona:toString(self), self.matricula, self.carrera)
end

-- Definición de la clase Profesor
Profesor = {}

-- Constructor de la clase Profesor
function Profesor:__init(nombre, edad, sexo, materia, años_experiencia)
  Persona:__init(self, nombre, edad, sexo)
  self.materia = materia
  self.años_experiencia = años_experiencia
end

-- Método que devuelve la materia que imparte el profesor
function Profesor:getMateria()
  return self.materia
end

-- Método que devuelve los años de experiencia del profesor
function Profesor:getAñosExperiencia()
  return self.años_experiencia
end

-- Método que devuelve una cadena con los datos del profesor
function Profesor:toString()
  return string.format("%s, Materia: %s, Años de experiencia: %d", Persona:toString(self), self.materia, self.años_experiencia)
end

-- Definición de la clase Universidad
Universidad = {}

-- Constructor de la clase Universidad
function Universidad:__init(nombre, ciudad, cantidad_estudiantes, cantidad_profesores)
  self.nombre = nombre
  self.ciudad = ciudad
  self.cantidad_estudiantes = cantidad_estudiantes
  self.cantidad_profesores = cantidad_profesores
end

-- Método que devuelve el nombre de la universidad
function Universidad:getNombre()
  return self.nombre
end

-- Método que devuelve la ciudad donde se encuentra la universidad
function Universidad:getCiudad()
  return self.ciudad
end

-- Método que devuelve la cantidad de estudiantes de la universidad
function Universidad:getCantidadEstudiantes()
  return self.cantidad_estudiantes
end

-- Método que devuelve la cantidad de profesores de la universidad
function Universidad:getCantidadProfesores()
  return self.cantidad_profesores
end

-- Método que devuelve una cadena con los datos de la universidad
function Universidad:toString()
  return string.format("Nombre: %s, Ciudad: %s, Cantidad de estudiantes: %d, Cantidad de profesores: %d", self.nombre, self.ciudad, self.cantidad_estudiantes, self.cantidad_profesores)
end

-- Creación de una universidad
universidad = Universidad:new("Universidad de Buenos Aires", "Buenos Aires", 10000, 500)

-- Creación de algunos estudiantes
estudiante1 = Estudiante:new("Juan", 20, "Masculino", "123456", "Ingeniería en Informática")
estudiante2 = Estudiante:new("María", 21, "Femenino", "654321", "Medicina")

-- Creación de algunos profesores
profesor1 = Profesor:new("Carlos", 50, "Masculino", "Matemáticas", 20)
profesor2 = Profesor:new("Ana", 45, "Femenino", "Física", 15)

-- Añadir los estudiantes y profesores a la universidad
universidad:addEstudiante(estudiante1)
universidad:addEstudiante(estudiante2)
universidad:addProfesor(profesor1)
universidad:addProfesor(profesor2)

-- Imprimir los datos de la universidad
print(universidad:toString())
```

Este código crea una clase Persona, una clase Estudiante, una clase Profesor y una clase Universidad. La clase Persona tiene un constructor que recibe el nombre, la edad y el sexo de la persona. La clase Estudiante hereda de la clase Persona y añade un constructor que recibe la matrícula y la carrera del estudiante. La clase Profesor también hereda de la clase Persona y añade un constructor que recibe la materia que imparte el profesor y los años de experiencia. La clase Universidad tiene un constructor que recibe el nombre, la ciudad, la cantidad de estudiantes y la cantidad de profesores de la universidad.

El código también crea una universidad llamada "Universidad de Buenos Aires" y añade algunos estudiantes y profesores a la misma. Finalmente, imprime los datos de la universidad.