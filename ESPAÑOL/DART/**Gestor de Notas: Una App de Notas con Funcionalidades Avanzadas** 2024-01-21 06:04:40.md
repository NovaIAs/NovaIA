```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:path_provider/path_provider.dart';
import 'package:shared_preferences/shared_preferences.dart';

// Modelo de datos para la nota
class Nota {
  int id;
  String titulo;
  String contenido;
  DateTime fechaCreacion;
  DateTime fechaUltimaModificacion;

  Nota({this.id, this.titulo, this.contenido, this.fechaCreacion, this.fechaUltimaModificacion});

  factory Nota.fromJson(Map<String, dynamic> json) {
    return Nota(
      id: json['id'],
      titulo: json['titulo'],
      contenido: json['contenido'],
      fechaCreacion: DateTime.parse(json['fechaCreacion']),
      fechaUltimaModificacion: DateTime.parse(json['fechaUltimaModificacion']),
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'titulo': titulo,
      'contenido': contenido,
      'fechaCreacion': fechaCreacion.toIso8601String(),
      'fechaUltimaModificacion': fechaUltimaModificacion.toIso8601String(),
    };
  }
}

// Servicio para el manejo de las notas
class NotasService {
  // Lista de notas
  List<Nota> notas = [];

  // Cargar las notas del almacenamiento local
  Future<void> cargarNotas() async {
    // Obtener el directorio de documentos
    Directory directory = await getApplicationDocumentsDirectory();

    // Crear el archivo de notas
    File file = File('${directory.path}/notas.json');

    // Leer el archivo de notas
    String json = await file.readAsString();

    // Decodificar JSON
    List<dynamic> notasJson = jsonDecode(json);

    // Convertir a objetos Nota
    notas = notasJson.map((notaJson) => Nota.fromJson(notaJson)).toList();
  }

  // Guardar las notas en el almacenamiento local
  Future<void> guardarNotas() async {
    // Obtener el directorio de documentos
    Directory directory = await getApplicationDocumentsDirectory();

    // Crear el archivo de notas
    File file = File('${directory.path}/notas.json');

    // Codificar JSON
    String json = jsonEncode(notas);

    // Escribir el archivo de notas
    await file.writeAsString(json);
  }

  // Crear una nueva nota
  Future<void> crearNota(Nota nota) async {
    // Añadir la nota a la lista
    notas.add(nota);

    // Guardar las notas en el almacenamiento local
    await guardarNotas();
  }

  // Editar una nota existente
  Future<void> editarNota(Nota nota) async {
    // Encontrar el índice de la nota
    int index = notas.indexWhere((n) => n.id == nota.id);

    // Actualizar la nota
    notas[index] = nota;

    // Guardar las notas en el almacenamiento local
    await guardarNotas();
  }

  // Eliminar una nota existente
  Future<void> eliminarNota(Nota nota) async {
    // Encontrar el índice de la nota
    int index = notas.indexWhere((n) => n.id == nota.id);

    // Eliminar la nota
    notas.removeAt(index);

    // Guardar las notas en el almacenamiento local
    await guardarNotas();
  }
}

// Pantalla principal de la aplicación
class NotasApp extends StatelessWidget {
  // Servicio para el manejo de las notas
  final NotasService notasService = NotasService();

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Notas',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: NotasHomePage(notasService: notasService),
    );
  }
}

// Pantalla de inicio de la aplicación
class NotasHomePage extends StatefulWidget {
  // Servicio para el manejo de las notas
  final NotasService notasService;

  NotasHomePage({Key key, this.notasService}) : super(key: key);

  @override
  _NotasHomePageState createState() => _NotasHomePageState();
}

class _NotasHomePageState extends State<NotasHomePage> {
  // Lista de notas
  List<Nota> notas;

  // Inicializar el estado
  @override
  void initState() {
    super.initState();

    // Cargar las notas del almacenamiento local
    widget.notasService.cargarNotas().then((_) {
      setState(() {
        notas = widget.notasService.notas;
      });
    });
  }

  // Crear una nueva nota
  void crearNota() {
    // Crear un nuevo objeto Nota
    Nota nota = Nota(titulo: '', contenido: '', fechaCreacion: DateTime.now(), fechaUltimaModificacion: DateTime.now());

    // Abrir el diálogo de edición de notas
    Navigator.push(context, MaterialPageRoute(builder: (context) => EditarNotaPage(nota: nota, notasService: widget.notasService))).then((value) {
      // Si se guardó la nota, actualizar la lista de notas
      if (value != null) {
        setState(() {
          notas = widget.notasService.notas;
        });
      }
    });
  }

  // Editar una nota existente
  void editarNota(Nota nota) {
    // Abrir el diálogo de edición de notas
    Navigator.push(context, MaterialPageRoute(builder: (context) => EditarNotaPage(nota: nota, notasService: widget.notasService))).then((value) {
      // Si se guardó la nota, actualizar la lista de notas
      if (value != null) {
        setState(() {
          notas = widget.notasService.notas;
        });
      }
    });
  }

  // Eliminar una nota existente
  void eliminarNota(Nota nota) {
    // Mostrar un diálogo de confirmación
    showDialog(
      context: context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: Text('Eliminar nota'),
          content: Text('¿Está seguro de que desea eliminar esta nota?'),
          actions: <Widget>[
            TextButton(
              child: Text('Cancelar'),
              onPressed: () {
                Navigator.of(context).pop();