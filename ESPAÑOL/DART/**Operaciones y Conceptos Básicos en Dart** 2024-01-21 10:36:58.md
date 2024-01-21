```dart
// Función que calcula el factorial de un número.
int factorial(int numero) {
  if (numero == 0) {
    return 1;
  } else {
    return numero * factorial(numero - 1);
  }
}

// Función que determina si un número es primo.
bool esPrimo(int numero) {
  if (numero <= 1) {
    return false;
  } else if (numero <= 3) {
    return true;
  } else if (numero % 2 == 0 || numero % 3 == 0) {
    return false;
  } else {
    int i = 5;
    while (i * i <= numero) {
      if (numero % i == 0 || numero % (i + 2) == 0) {
        return false;
      }
      i += 6;
    }
    return true;
  }
}

// Función que calcula la suma de los dígitos de un número.
int sumaDigitos(int numero) {
  if (numero == 0) {
    return 0;
  } else {
    return numero % 10 + sumaDigitos(numero ~/ 10);
  }
}

// Función que determina si un número es capicúa.
bool esCapicua(int numero) {
  return numero == numero.toString().split('').reversed.join('');
}

// Función principal.
void main() {
  // Declaración de variables.
  int numero1 = 10;
  int numero2 = 20;
  double numero3 = 3.14;
  String texto1 = 'Hola mundo!';
  String texto2 = 'Adiós mundo!';
  bool booleano1 = true;
  bool booleano2 = false;
  List<int> lista1 = [1, 2, 3, 4, 5];
  List<String> lista2 = ['Hola', 'Mundo', '!'];
  Map<String, int> mapa1 = {'Nombre': 'Juan', 'Edad': 20};
  Set<int> conjunto1 = {1, 2, 3, 4, 5};

  // Operaciones aritméticas.
  int suma = numero1 + numero2;
  int resta = numero2 - numero1;
  int producto = numero1 * numero2;
  double division = numero3 / numero1;
  int modulo = numero2 % numero1;

  // Operaciones lógicas.
  bool and = booleano1 && booleano2;
  bool or = booleano1 || booleano2;
  bool not = !booleano1;

  // Operaciones de comparación.
  bool igual = numero1 == numero2;
  bool diferente = numero1 != numero2;
  bool mayorQue = numero1 > numero2;
  bool menorQue = numero1 < numero2;
  bool mayorOIgualQue = numero1 >= numero2;
  bool menorOIgualQue = numero1 <= numero2;

  // Operaciones de asignación.
  numero1 += numero2;
  numero2 -= numero1;
  numero3 *= numero2;
  numero1 /= numero3;
  numero2 %= numero1;

  // Operaciones de incremento y decremento.
  numero1++;
  numero2--;

  // Operaciones de concatenación.
  String texto3 = texto1 + texto2;

  // Operaciones de acceso a elementos.
  int elemento1 = lista1[0];
  String elemento2 = lista2[1];
  int elemento3 = mapa1['Nombre'];

  // Operaciones de adición y eliminación de elementos.
  lista1.add(6);
  lista1.remove(2);
  lista2.add('!');
  lista2.remove('Hola');
  mapa1['Apellido'] = 'Pérez';
  mapa1.remove('Nombre');
  conjunto1.add(6);
  conjunto1.remove(2);

  // Iteración sobre colecciones.
  for (int elemento in lista1) {
    print(elemento);
  }
  for (String elemento in lista2) {
    print(elemento);
  }
  for (MapEntry<String, int> elemento in mapa1.entries) {
    print('${elemento.key}: ${elemento.value}');
  }
  for (int elemento in conjunto1) {
    print(elemento);
  }

  // Control de flujo.
  if (numero1 > 10) {
    print('El número es mayor que 10');
  } else if (numero1 < 10) {
    print('El número es menor que 10');
  } else {
    print('El número es igual a 10');
  }

  switch (numero1) {
    case 10:
      print('El número es 10');
      break;
    case 20:
      print('El número es 20');
      break;
    default:
      print('El número no es 10 ni 20');
  }

  while (numero1 > 0) {
    print('El número es mayor que 0');
    numero1--;
  }

  do {
    print('El número es mayor o igual que 0');
    numero2--;
  } while (numero2 >= 0);

  for (int i = 0; i < 10; i++) {
    print('El número es $i');
  }

  // Manejo de errores.
  try {
    int resultado = numero1 / 0;
    print('El resultado es $resultado');
  } catch (e) {
    print('Ha ocurrido un error: $e');
  } finally {
    print('Siempre se ejecuta este bloque');
  }

  // Funciones.
  int factorial10 = factorial(10);
  print('El factorial de 10 es $factorial10');

  bool esPrimo7 = esPrimo(7);
  print('¿7 es primo? $esPrimo7');

  int sumaDigitos123 = sumaDigitos(123);
  print('La suma de los dígitos de 123 es $sumaDigitos123');

  bool esCapicua121 = esCapicua(121);
  print('¿121 es capicúa? $esCapicua121');

  // Clases y objetos.
  class Persona {
    String nombre;
    int edad;

    Persona(this.nombre, this.edad);

    String saludar() {
      return 'Hola, mi nombre es $nombre y tengo $edad años';
    }
  }

  Persona persona1 = Persona('Juan', 20);
  print(persona1.saludar());

  // Archivos.
  File archivo = File('archivo.txt');
  archivo.writeAsString('Hola mundo!');
  String contenido = archivo.readAsStringSync();
  print(contenido);

  // Procesos.
  Process process = Process.start('ls', ['-l']);
  process.stdout.transform(utf8.decoder).listen((data) {
    print(data);
  });

  // HTTP.
  HttpClient client = HttpClient();
  client.getUrl(Uri.parse('https://google.com')).then((request) {
    return request.close();
  }).then((response) {
    response.transform(utf8.decoder).listen((data) {
      print(data);
    });
  });

  // Sockets.
  ServerSocket server = await ServerSocket.bind('127.0.0.1', 8080);
  server.listen((Socket socket) {
    socket.write('Hola mundo!');
    socket.close();
  });

  // Concurrencia.
  Future<int> sumar(int a, int b) async {
    return a + b;
  }

  Future<int> suma = sumar(10, 20);
  print(await suma);

  // Internacionalización.
  Intl.defaultLocale = 'es_ES';
  NumberFormat format = NumberFormat.currency(locale: 'es_ES');
  String precioFormateado = format.format(1000);
  print(precioFormateado);

  // Pruebas unitarias.
  group('Suma de números', () {
    test('Suma de 1 y 2 es 3', () {
      expect(sumar(1, 2), equals(3));
    });

    test('Suma de 3 y 4 es 7', () {
      expect(sumar(3, 4), equals(7));
    });
  });
}
```