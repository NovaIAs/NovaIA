```dart
// Importamos librerías necesarias
import 'dart:async';
import 'dart:convert';
import 'package:http/http.dart' as http;

// Creamos una clase para manejar nuestros datos
class Persona {
  String nombre;
  String apellido;
  int edad;

  Persona({required this.nombre, required this.apellido, required this.edad});

  // Método para convertir el objeto a JSON
  String toJson() {
    return jsonEncode({
      'nombre': nombre,
      'apellido': apellido,
      'edad': edad,
    });
  }

  // Método para crear un objeto a partir de JSON
  factory Persona.fromJson(Map<String, dynamic> json) {
    return Persona(
      nombre: json['nombre'],
      apellido: json['apellido'],
      edad: json['edad'],
    );
  }
}

// Creamos una clase para manejar nuestra API
class ApiPersona {
  // URL base de nuestra API
  final String baseUrl = 'http://localhost:3000/personas';

  // Método para obtener todas las personas
  Future<List<Persona>> getAll() async {
    // Enviamos una petición GET a nuestra API
    final response = await http.get(Uri.parse(baseUrl));

    // Decodificamos la respuesta JSON
    final json = jsonDecode(response.body);

    // Creamos una lista de objetos Persona a partir del JSON
    final personas = json.map((json) => Persona.fromJson(json)).toList();

    // Devolvemos la lista de objetos Persona
    return personas;
  }

  // Método para obtener una persona por ID
  Future<Persona> getById(int id) async {
    // Enviamos una petición GET a nuestra API
    final response = await http.get(Uri.parse('$baseUrl/$id'));

    // Decodificamos la respuesta JSON
    final json = jsonDecode(response.body);

    // Creamos un objeto Persona a partir del JSON
    final persona = Persona.fromJson(json);

    // Devolvemos el objeto Persona
    return persona;
  }

  // Método para crear una nueva persona
  Future<Persona> create(Persona persona) async {
    // Enviamos una petición POST a nuestra API
    final response = await http.post(
      Uri.parse(baseUrl),
      headers: {'Content-Type': 'application/json'},
      body: persona.toJson(),
    );

    // Decodificamos la respuesta JSON
    final json = jsonDecode(response.body);

    // Creamos un objeto Persona a partir del JSON
    final nuevaPersona = Persona.fromJson(json);

    // Devolvemos el objeto Persona
    return nuevaPersona;
  }

  // Método para actualizar una persona
  Future<Persona> update(Persona persona) async {
    // Enviamos una petición PUT a nuestra API
    final response = await http.put(
      Uri.parse('$baseUrl/${persona.id}'),
      headers: {'Content-Type': 'application/json'},
      body: persona.toJson(),
    );

    // Decodificamos la respuesta JSON
    final json = jsonDecode(response.body);

    // Creamos un objeto Persona a partir del JSON
    final personaActualizada = Persona.fromJson(json);

    // Devolvemos el objeto Persona
    return personaActualizada;
  }

  // Método para eliminar una persona
  Future<void> delete(int id) async {
    // Enviamos una petición DELETE a nuestra API
    await http.delete(Uri.parse('$baseUrl/$id'));
  }
}

// Creamos una instancia de nuestra clase ApiPersona
final apiPersona = ApiPersona();

// Utilizamos nuestra API para obtener todas las personas
apiPersona.getAll().then((personas) {
  // Imprimimos los nombres de las personas
  for (final persona in personas) {
    print(persona.nombre);
  }
});

// Utilizamos nuestra API para obtener una persona por ID
apiPersona.getById(1).then((persona) {
  // Imprimimos el nombre de la persona
  print(persona.nombre);
});

// Utilizamos nuestra API para crear una nueva persona
apiPersona
    .create(Persona(nombre: 'Juan', apellido: 'Pérez', edad: 20))
    .then((persona) {
  // Imprimimos el nombre de la persona
  print(persona.nombre);
});

// Utilizamos nuestra API para actualizar una persona
apiPersona
    .update(Persona(id: 1, nombre: 'Juan', apellido: 'Pérez', edad: 21))
    .then((persona) {
  // Imprimimos el nombre de la persona
  print(persona.nombre);
});

// Utilizamos nuestra API para eliminar una persona
apiPersona.delete(1).then((_) {
  // Imprimimos un mensaje de confirmación
  print('Persona eliminada');
});
```

Explicación del código:

* Creamos una clase `Persona` para manejar nuestros datos. Esta clase tiene tres propiedades: `nombre`, `apellido` y `edad`.
* Creamos una clase `ApiPersona` para manejar nuestra API. Esta clase tiene varios métodos para obtener, crear, actualizar y eliminar personas.
* Creamos una instancia de nuestra clase `ApiPersona` llamada `apiPersona`.
* Utilizamos nuestra API para obtener todas las personas, obtener una persona por ID, crear una nueva persona, actualizar una persona y eliminar una persona.
* Imprimimos los resultados de nuestras peticiones a la API en la consola.

Este código es complejo porque utiliza varias características avanzadas de Dart, como clases, métodos, funciones asíncronas y manejo de JSON. Sin embargo, también es muy flexible y extensible, lo que lo hace ideal para desarrollar aplicaciones web y móviles complejas.