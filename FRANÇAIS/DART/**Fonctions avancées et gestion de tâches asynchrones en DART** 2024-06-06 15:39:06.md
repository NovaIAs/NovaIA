**Code en DART**

```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

typedef Mapper<T1, T2> = T2 Function(T1);
typedef Predicate<T> = bool Function(T);

extension IterableX<T> on Iterable<T> {
  Iterable<T> filter(Predicate<T> fn) => where(fn);
  T? find(Predicate<T> fn) => firstWhere(fn, orElse: () => null);
  Map<K, V> toMap<K, V>({Mapper<T, K>? keyFn, Mapper<T, V>? valueFn}) => {
        for (var t in this) keyFn?.call(t): valueFn?.call(t),
      };
  Iterable<T> distinctBy<TK>(Mapper<T, TK> keyFn) => LinkedHashSet<TK>.from(map(keyFn)).map((k) => firstWhere((t) => k == keyFn(t)));
  Iterable<List<T>> partition(int n) => {
        for (var i = 0; i < length; i += n)
          yield skip(i).take(n),
      };
  Map<K, List<T>> groupBy<K>(Mapper<T, K> keyFn) => toMap(keyFn: keyFn, valueFn: (_) => []).update((k, v) => [...v, _], ifAbsent: () => []);
}

extension StringX on String {
  bool containsIgnoreCase(String other) => toLowerCase().contains(other.toLowerCase());
  String replaceAllIgnoreCase(String search, String replacement) => replaceAll(RegExp(search, caseSensitive: false), replacement);
}

extension FutureX<T> on Future<T> {
  Future<R> thenX<R>(FutureOr<R> Function(T) f) => then((t) => Future.value(f(t))) as Future<R>;
}

extension TimerX on Timer {
  Timer thenAfter(Duration time, Function() f) => then((_) => Timer(time, f));
}

extension FileX on File {
  Future<String> readAllText() => readAsString();
  Future<List<int>> readAllBytes() => readAsBytes();
}

extension ProcessResultX on ProcessResult {
  // Décode la sortie standard et la sortie d'erreur en tant qu'objet JSON
  Future<dynamic> decodeJson() async {
    var stdout = await stdout.toString();
    var stderr = await stderr.toString();
    if (exitCode != 0) {
      throw ProcessException(stderr);
    }
    if (stdout.isNotEmpty) {
      return jsonDecode(stdout);
    } else if (stderr.isNotEmpty) {
      return jsonDecode(stderr);
    } else {
      throw ProcessException('Process exited without output.');
    }
  }
}

// Classe représentant une tâche asynchrone avec un état
class AsyncState<T> {
  late Future<T> future;
  T? data;
  Object? error;
  bool get isLoading => future == null;
  bool get isError => error != null;
  bool get isLoaded => data != null;
  AsyncState.loading() {
    future = Future.delayed(Duration(milliseconds: 500), () => T as T);
  }
  AsyncState.loaded(T data) {
    this.data = data;
    future = Future.value(data);
  }
  AsyncState.error(Object error) {
    this.error = error;
    future = Future.error(error);
  }
  AsyncState.empty() {
    future = Future.value(null);
  }
}

main() async {
  // Créer une liste de nombres
  var numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  // Filtrer les nombres pairs
  var evenNumbers = numbers.filter((n) => n % 2 == 0);

  // Trouver le nombre le plus grand
  var maxNumber = numbers.find((n) => n == numbers.max);

  // Mapper les nombres en leur carré
  var squares = numbers.map((n) => n * n);

  // Réduire les nombres en leur somme
  var sum = numbers.reduce((a, b) => a + b);

  // Grouper les nombres par leurs chiffres
  var groupedByDigits = numbers.groupBy((n) => n % 10);

  // Afficher les résultats
  print('Nombres pairs : $evenNumbers');
  print('Nombre le plus grand : $maxNumber');
  print('Carrés des nombres : $squares');
  print('Somme des nombres : $sum');
  print('Nombres groupés par chiffres : $groupedByDigits');

  // Charger un fichier JSON
  var json = await File('data.json').readAllText();
  var data = jsonDecode(json);

  // Exécuter une commande système
  var process = await Process.start('ls', ['-l']);
  var result = await process.result;
  print(result.stdout);

  // Créer une tâche asynchrone avec un état
  var task = AsyncState<int>.loading();
  await Future.delayed(Duration(seconds: 2), () => task = AsyncState.loaded(42));
  if (task.isLoaded) print('Tâche chargée : ${task.data}');
  else if (task.isError) print('Tâche en erreur : ${task.error}');
  else print('Tâche en cours de chargement');
}
```

**Explication du code**

Ce code est un exemple complexe et complet de programmation en DART, qui couvre un large éventail de concepts et de fonctionnalités.

**1. Fonctions lambda et méthodes d'extension**

Le code utilise des fonctions lambda et des méthodes d'extension pour étendre les fonctionnalités de classes existantes. Par exemple, la méthode `filter` est ajoutée à la classe `Iterable` pour permettre le filtrage des éléments en fonction d'un prédicat.

**2. Extensions de chaînes**

La classe `String` est étendue pour inclure les méthodes `containsIgnoreCase` et `replaceAllIgnoreCase`, qui effectuent des opérations de comparaison et de remplacement de chaînes sans distinction de casse.

**3. Extensions de futures**

La classe `Future` est étendue pour inclure la méthode `thenX`, qui permet de chaîner des futures et de renvoyer une nouvelle future.

**4. Extensions de minuteries**

La classe `Timer` est étendue pour inclure la méthode `thenAfter`, qui permet de planifier l'exécution d'une fonction après un certain délai.

**5. Extensions de fichiers**

La classe `File` est étendue pour inclure les méthodes `readAllText` et `readAllBytes`, qui permettent de lire respectivement le contenu d'un fichier sous forme de texte et d'octets.

**6. Extensions de résultats de processus**

La classe `ProcessResult` est étendue pour inclure la méthode `decodeJson`, qui décode la sortie standard et la sortie d'erreur d'un processus en tant qu'objet JSON.

**7. Classe d'état asynchrone**

La classe `AsyncState` est définie pour représenter une tâche asynchrone avec un état (chargement, chargement, erreur ou vide).

**8. Fonctions principales**

La fonction `main` est le point d'entrée du programme. Elle exécute une série de tâches, notamment :

* Filtrer les nombres pairs à partir d'une liste de nombres
* Trouver le nombre le plus grand dans une liste
* Mapper les nombres en leur carré
* Réduire les nombres en leur somme
* Grouper les nombres par leurs chiffres
* Charger un fichier JSON
* Exécuter une commande système
* Créer une tâche asynchrone avec un état

Ce code illustre la puissance et la flexibilité du langage DART, ainsi que sa capacité à gérer facilement des tâches complexes.