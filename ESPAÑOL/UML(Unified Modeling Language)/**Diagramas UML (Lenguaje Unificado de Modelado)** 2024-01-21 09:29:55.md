**Diagrama de Clases**

```
+----------------+
| Clase Persona |
+----------------+
| - nombre       |
| - edad          |
| - dirección     |
| - teléfono      |
| - correo        |
| ----------------- |
| + Persona(nombre, edad, dirección, teléfono, correo) |
| + hablar()       |
| + comer()        |
| + dormir()       |
+------------------+

+------------------+
| Clase Empleado |
+------------------+
| - nombre       |
| - edad          |
| - dirección     |
| - teléfono      |
| - correo        |
| - cargo         |
| - salario       |
| ----------------- |
| + Empleado(nombre, edad, dirección, teléfono, correo, cargo, salario) |
| + trabajar()       |
| + cobrar()        |
+-------------------

+-------------------+
| Clase Estudiante |
+-------------------+
| - nombre       |
| - edad          |
| - dirección     |
| - teléfono      |
| - correo        |
| - matrícula     |
| - carrera       |
| ----------------- |
| + Estudiante(nombre, edad, dirección, teléfono, correo, matrícula, carrera) |
| + estudiar()       |
| + экзамены()       |
+--------------------

+----------------+
| Clase Médico |
+----------------+
| - nombre       |
| - edad          |
| - dirección     |
| - teléfono      |
| - correo        |
| - especialidad  |
| - hospital      |
| ----------------- |
| + Médico(nombre, edad, dirección, teléfono, correo, especialidad, hospital) |
| + atender()       |
| + recetar()       |
+------------------

+-------------------+
| Clase Paciente |
+-------------------+
| - nombre       |
| - edad          |
| - dirección     |
| - teléfono      |
| - correo        |
| - historia      |
| - médico         |
| ----------------- |
| + Paciente(nombre, edad, dirección, teléfono, correo, historia, médico) |
| + consultar()       |
| + pagar()        |
+--------------------
```

**Diagrama de Secuencia**

```
Persona -> Empleado: trabajar()
Empleado -> Persona: cobrar()

Persona -> Estudiante: estudiar()
Estudiante -> Persona: экзамены()

Persona -> Médico: atender()
Médico -> Persona: recetar()

Persona -> Paciente: consultar()
Paciente -> Persona: pagar()
```

**Diagrama de Colaboración**

```
Persona <----------> Empleado
    emplear

Persona <----------> Estudiante
    estudiar

Persona <----------> Médico
    atender

Persona <----------> Paciente
    consultar
```

**Explicación**

El diagrama de clases muestra las clases del sistema, sus atributos y métodos. Las clases Persona, Empleado, Estudiante, Médico y Paciente tienen atributos comunes, como nombre, edad, dirección, teléfono y correo. Las clases Empleado, Estudiante, Médico y Paciente tienen atributos específicos, como cargo, salario, matrícula, carrera, especialidad y hospital. Las clases Persona, Empleado, Estudiante, Médico y Paciente tienen métodos comunes, como hablar(), comer(), dormir(), trabajar(), cobrar(), estudiar(), экзамены(), atender() y recetar(). Las clases Persona, Empleado, Estudiante, Médico y Paciente tienen métodos específicos, como emplear(), estudiar(), consultar() y pagar().

El diagrama de secuencia muestra la secuencia de mensajes entre los objetos del sistema. El objeto Persona envía un mensaje trabajar() al objeto Empleado. El objeto Empleado envía un mensaje cobrar() al objeto Persona. El objeto Persona envía un mensaje estudiar() al objeto Estudiante. El objeto Estudiante envía un mensaje экзамены() al objeto Persona. El objeto Persona envía un mensaje atender() al objeto Médico. El objeto Médico envía un mensaje recetar() al objeto Persona. El objeto Persona envía un mensaje consultar() al objeto Paciente. El objeto Paciente envía un mensaje pagar() al objeto Persona.

El diagrama de colaboración muestra los objetos del sistema y sus relaciones. El objeto Persona está conectado con el objeto Empleado por la relación emplear. El objeto Persona está conectado con el objeto Estudiante por la relación estudiar. El objeto Persona está conectado con el objeto Médico por la relación atender. El objeto Persona está conectado con el objeto Paciente por la relación consultar.