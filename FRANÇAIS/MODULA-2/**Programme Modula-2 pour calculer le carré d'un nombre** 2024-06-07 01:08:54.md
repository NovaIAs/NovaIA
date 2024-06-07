**Module principal**

```modula-2
MODULE ModulePrincipal;

IMPORT IO;

BEGIN
  IO.Write("Entrez un nombre entier : ");
  INTEGER nombre := IO.ReadInteger;
  IO.WriteLine("Le carré de", nombre, "est", nombre * nombre);
END ModulePrincipal.
```

**Module d'entrée/sortie**

```modula-2
MODULE IO;

FROM Terminal IMPORT Write, WriteString, ReadLine, ReadInteger;

PROCEDURE Write(s : STRING);
BEGIN
  Terminal.WriteString(s);
END Write;

PROCEDURE WriteString(s : ARRAY OF CHAR);
BEGIN
  Terminal.WriteString(s);
END WriteString;

PROCEDURE ReadLine(VAR s : ARRAY OF CHAR);
BEGIN
  Terminal.ReadLine(s);
END ReadLine;

PROCEDURE ReadInteger(VAR i : INTEGER);
BEGIN
  Terminal.ReadInteger(i);
END ReadInteger;

END IO.
```

**Module Terminal**

```modula-2
MODULE Terminal;

IMPORT IO;

PROCEDURE Write(s : STRING);
VAR buffer := [s];

BEGIN
  IO.WriteString(buffer);
END Write;

PROCEDURE WriteString(s : ARRAY OF CHAR);
BEGIN
  IO.WriteString(s);
END WriteString;

PROCEDURE ReadLine(VAR s : ARRAY OF CHAR);
VAR i, n := 0;
CHAR c;

BEGIN
  REPEAT
    c := IO.ReadChar;
    s[i] := c;
    i := i + 1;
  UNTIL c = #10;
  s[i - 1] := #0;
END ReadLine;

PROCEDURE ReadInteger(VAR i : INTEGER);
BEGIN
  ReadLine(s);
  i := Val(s, 10);
END ReadInteger;

END Terminal.
```

**Module de conversion de caractères en entiers**

```modula-2
MODULE Val;

IMPORT Char, IO;

PROCEDURE Value(s : ARRAY OF CHAR; radix : CARDINAL) : INTEGER;
VAR i, n := 0;
CHAR c;

BEGIN
  i := 0;
  n := 0;
  REPEAT
    c := s[i];
    i := i + 1;
    IF c >= '0' AND c <= '9' THEN
      n := n * radix + (c - '0');
    ELIF c >= 'a' AND c <= 'z' THEN
      n := n * radix + (c - 'a' + 10);
    ELIF c >= 'A' AND c <= 'Z' THEN
      n := n * radix + (c - 'A' + 10);
    ENDIF;
  UNTIL c = #0;
  RETURN n;
END Value;

END Val.
```

**Explications**

Ce code implémente un simple programme qui demande à l'utilisateur d'entrer un nombre entier, puis affiche le carré de ce nombre. Le programme est divisé en plusieurs modules, chacun ayant une fonctionnalité spécifique :

* **ModulePrincipal** : Le module principal qui demande à l'utilisateur d'entrer un nombre, calcule son carré et affiche le résultat.
* **Module IO** : Le module d'entrée/sortie qui fournit des fonctions pour lire et écrire des données dans la console.
* **Module Terminal** : Le module Terminal qui fournit des fonctions spécifiques à la console, telles que `Write()`, `WriteString()`, `ReadLine()` et `ReadInteger()`.
* **Module Val** : Le module de conversion de caractères en entiers qui fournit une fonction `Value()` pour convertir une chaîne de caractères en un nombre entier.

Le programme fonctionne comme suit :

1. Le module principal demande à l'utilisateur d'entrer un nombre entier à l'aide de la fonction `Write()` du module d'entrée/sortie.
2. L'utilisateur entre un nombre et appuie sur Entrée.
3. Le module principal lit le nombre entré par l'utilisateur à l'aide de la fonction `ReadInteger()` du module d'entrée/sortie et le stocke dans une variable `nombre`.
4. Le module principal calcule le carré du nombre à l'aide de l'expression `nombre * nombre`.
5. Le module principal affiche le carré du nombre à l'aide de la fonction `Write()` du module d'entrée/sortie.