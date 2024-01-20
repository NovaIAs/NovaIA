```groovy

// Importar las bibliotecas necesarias
import groovy.transform.CompileStatic
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.*

// Crear una clase controladora
@RestController
@RequestMapping("/api")
@CompileStatic
class Controlador {

  // Inyectar el servicio
  @Autowired
  Servicio servicio

  // Crear un método GET para obtener todos los recursos
  @GetMapping("/")
  def obtenerTodos() {
    servicio.obtenerTodos()
  }

  // Crear un método POST para crear un nuevo recurso
  @PostMapping("/")
  def crear(@RequestBody Recurso recurso) {
    servicio.crear(recurso)
  }

  // Crear un método GET para obtener un recurso por su ID
  @GetMapping("/{id}")
  def obtenerPorId(@PathVariable("id") Long id) {
    servicio.obtenerPorId(id)
  }

  // Crear un método PUT para actualizar un recurso
  @PutMapping("/{id}")
  def actualizar(@PathVariable("id") Long id, @RequestBody Recurso recurso) {
    servicio.actualizar(id, recurso)
  }

  // Crear un método DELETE para eliminar un recurso
  @DeleteMapping("/{id}")
  def eliminar(@PathVariable("id") Long id) {
    servicio.eliminar(id)
  }
}

// Crear una clase de servicio
class Servicio {

  // Inyectar el repositorio
  @Autowired
  Repositorio repositorio

  // Crear un método para obtener todos los recursos
  def obtenerTodos() {
    repositorio.findAll()
  }

  // Crear un método para crear un nuevo recurso
  def crear(Recurso recurso) {
    repositorio.save(recurso)
  }

  // Crear un método para obtener un recurso por su ID
  def obtenerPorId(Long id) {
    repositorio.findById(id).orElse(null)
  }

  // Crear un método para actualizar un recurso
  def actualizar(Long id, Recurso recurso) {
    repositorio.findById(id).ifPresent({ recursoExistente ->
      recursoExistente.setNombre(recurso.getNombre())
      recursoExistente.setDescripcion(recurso.getDescripcion())
      repositorio.save(recursoExistente)
    })
  }

  // Crear un método para eliminar un recurso
  def eliminar(Long id) {
    repositorio.deleteById(id)
  }
}

// Crear una clase de repositorio
class Repositorio extends CrudRepository<Recurso, Long> {
}

// Crear una clase de recurso
class Recurso {

  // Crear las propiedades del recurso
  Long id
  String nombre
  String descripcion
}

// Crear la clase de aplicación principal
class Aplicacion {

  // Crear el método main
  static void main(String[] args) {
    // Crear un contexto de Spring
    ApplicationContext contexto = new SpringApplication(Aplicacion.class).run(*args)

    // Obtener un bean del contexto
    Controlador controlador = contexto.getBean(Controlador.class)

    // Utilizar el bean para realizar operaciones con los recursos
    def recursos = controlador.obtenerTodos()
    println("Recursos: ${recursos}")

    def recurso = new Recurso(nombre: "Recurso 1", descripcion: "Descripción del recurso 1")
    def recursoGuardado = controlador.crear(recurso)
    println("Recurso guardado: ${recursoGuardado}")

    def recursoObtenido = controlador.obtenerPorId(recursoGuardado.id)
    println("Recurso obtenido: ${recursoObtenido}")

    recursoObtenido.setNombre("Recurso 1 actualizado")
    def recursoActualizado = controlador.actualizar(recursoObtenido.id, recursoObtenido)
    println("Recurso actualizado: ${recursoActualizado}")

    controlador.eliminar(recursoActualizado.id)
    println("Recurso eliminado")
  }
}

```

Este código es un ejemplo de una aplicación REST completa en Groovy, utilizando Spring Boot y Spring Data JPA. La aplicación permite crear, leer, actualizar y eliminar recursos de una base de datos. El código está dividido en varias clases, cada una con una responsabilidad específica.

La clase `Controlador` es responsable de manejar las solicitudes HTTP y de delegar las operaciones a la capa de servicio. La clase `Servicio` es responsable de implementar la lógica de negocio de la aplicación. La clase `Repositorio` es responsable de realizar las operaciones CRUD (crear, leer, actualizar y eliminar) sobre la base de datos. La clase `Recurso` representa los datos que se almacenan en la base de datos. Y la clase `Aplicacion` es el punto de entrada de la aplicación, que crea un contexto de Spring y utiliza los beans del contexto para realizar operaciones con los recursos.

Este código es complejo porque cubre una amplia gama de conceptos y tecnologías, incluyendo Spring Boot, Spring Data JPA, Groovy, y REST. Sin embargo, el código está bien organizado y es fácil de leer y entender.