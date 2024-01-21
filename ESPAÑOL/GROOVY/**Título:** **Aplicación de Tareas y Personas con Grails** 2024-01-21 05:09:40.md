```groovy
// Definir una clase de dominio Persona
class Persona {
    String nombre
    String apellido
    int edad
}

// Definir una clase de servicio PersonaService
class PersonaService {

    def buscarPorNombre(String nombre) {
        Persona.findByNombre(nombre)
    }

    def buscarPorApellido(String apellido) {
        Persona.findByApellido(apellido)
    }

    def buscarPorEdad(int edad) {
        Persona.findByEdad(edad)
    }

    def crear(Persona persona) {
        persona.save()
    }

    def actualizar(Persona persona) {
        persona.save()
    }

    def eliminar(Persona persona) {
        persona.delete()
    }
}

// Definir un controlador PersonaController
class PersonaController {

    def personaService

    def index() {
        [personas: personaService.findAll()]
    }

    def crear() {
        [persona: new Persona()]
    }

    def guardar() {
        def persona = new Persona(params)
        if (personaService.crear(persona)) {
            redirect(action: "index")
        } else {
            render(view: "crear", model: [persona: persona])
        }
    }

    def editar(Long id) {
        [persona: personaService.findById(id)]
    }

    def actualizar() {
        def persona = personaService.findById(params.id)
        persona.properties = params
        if (personaService.actualizar(persona)) {
            redirect(action: "index")
        } else {
            render(view: "editar", model: [persona: persona])
        }
    }

    def eliminar(Long id) {
        personaService.eliminar(personaService.findById(id))
        redirect(action: "index")
    }
}

// Definir una clase de dominio Tarea
class Tarea {
    String descripcion
    boolean completada
    Persona asignadoA
}

// Definir una clase de servicio TareaService
class TareaService {

    def buscarPorDescripcion(String descripcion) {
        Tarea.findByDescripcion(descripcion)
    }

    def buscarPorCompletada(boolean completada) {
        Tarea.findByCompletada(completada)
    }

    def buscarPorAsignadoA(Persona asignadoA) {
        Tarea.findByAsignadoA(asignadoA)
    }

    def crear(Tarea tarea) {
        tarea.save()
    }

    def actualizar(Tarea tarea) {
        tarea.save()
    }

    def eliminar(Tarea tarea) {
        tarea.delete()
    }
}

// Definir un controlador TareaController
class TareaController {

    def tareaService

    def index() {
        [tareas: tareaService.findAll()]
    }

    def crear() {
        [tarea: new Tarea()]
    }

    def guardar() {
        def tarea = new Tarea(params)
        if (tareaService.crear(tarea)) {
            redirect(action: "index")
        } else {
            render(view: "crear", model: [tarea: tarea])
        }
    }

    def editar(Long id) {
        [tarea: tareaService.findById(id)]
    }

    def actualizar() {
        def tarea = tareaService.findById(params.id)
        tarea.properties = params
        if (tareaService.actualizar(tarea)) {
            redirect(action: "index")
        } else {
            render(view: "editar", model: [tarea: tarea])
        }
    }

    def eliminar(Long id) {
        tareaService.eliminar(tareaService.findById(id))
        redirect(action: "index")
    }
}

// Ejecutar la aplicación desde un script principal
new Script("grails-app/conf/BootStrap.groovy").run()
```

Este código crea una aplicación web completa utilizando el framework Grails. La aplicación tiene dos entidades principales: Persona y Tarea. Cada entidad tiene un servicio asociado que proporciona métodos para crear, leer, actualizar y eliminar instancias de la entidad. La aplicación también tiene dos controladores que manejan las solicitudes HTTP entrantes y devuelven las vistas adecuadas.

El código es bastante complejo y sería difícil para un principiante entenderlo todo. Sin embargo, proporciona una buena visión general de cómo construir una aplicación web utilizando Grails.

Aquí tienes algunas explicaciones adicionales sobre el código:

* El código utiliza el lenguaje de programación Groovy, que es un lenguaje de programación dinámico y orientado a objetos.
* El framework Grails es un marco de trabajo de código abierto para el desarrollo de aplicaciones web en Groovy.
* Las clases Persona y Tarea son clases de dominio que representan las entidades principales de la aplicación.
* Las clases PersonaService y TareaService son clases de servicio que proporcionan métodos para crear, leer, actualizar y eliminar instancias de las clases de dominio.
* Las clases PersonaController y TareaController son controladores que manejan las solicitudes HTTP entrantes y devuelven las vistas adecuadas.
* El script BootStrap.groovy se utiliza para inicializar la aplicación.

Espero que esto te ayude.