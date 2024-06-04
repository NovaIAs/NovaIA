**Module Principal (main.d)**

```d
import std.stdio, std.traits, std.typecons;

module Main {

    immutable Fonction(in int, out int) add = { |x, out y| y = x + 2; };

    void main() {
        immutable auto cons = add.cons!(32);

        int value = 0;
        cons(value);
        writeln("Value est maintenant :", value);
    }
}
```

**Implémentation de l'interface Fonction (fonction.d)**

```d
import std.traits;

interface Fonction(in T, out U) {
    void apply(in T, out U);
}

immutable struct FonctionCons(in T, in Fonction(in T, out U), mutable U) {
    immutable in T arg = T;
    immutable in Fonction(in T, out U) impl = Fonction(in T, out U);
    mutable in U out = U;

    void apply(in T, out U) pure nothrow {
        impl(arg, out);
    }
}
```

**Macros (macro.d)**

```d
import std.typecons;

macro cons(in Fonction(in T, out U)) {
    immutable FonctionCons(T, in Fonction(in T, out U), U) cons =
        immutable FonctionCons(T, Fonction(in T, out U), U);
    return cons;
}
```

**Utilisation de macros (macro_test.d)**

```d
import std.stdio, std.traits, macro;

module Main {

    immutable Fonction(in int, out int) add = { |x, out y| y = x + 2; };

    void main() {
        immutable auto cons = macro.cons!(add);

        int value = 0;
        cons(value);
        writeln("Value est maintenant :", value);
    }
}
```

**Génération de code à partir de métadonnées (metadata.d)**

```d
import std.traits;

class MetaData {
    string name;
    immutable inout(string) value;

    override string toString() const pure nothrow {
        return immutable("__metadata: {name: ", name, ", value: ", value, "}");
    }
}

ImmutableTuple!(MetaData) getMetadata(in T @fieldify T)(in T value) pure nothrow @nogc {
    immutable(metaData = MetaData(string, string)) = new MetaData();
    metaData.name = __traits(field, name);
    metaData.value = value.stringof;
    return metaData;
}
```

**Utilisation de la génération de code (metadata_test.d)**

```d
import std.stdio, std.traits, metadata;

class Personne {
    string nom;
    int age;
}

void main() {
    immutable Personne anne = Personne("Anne", 32);

    write("Nom en majuscule : ");
    writeln(metadata.getMetadata!anne.nom.toUpperCase);
}
```

**Explication du code**

Ce code complexe démontre plusieurs fonctionnalités avancées du langage D :

* **Fonctions avec des paramètres de sortie et des blocs génériques** : Les fonctions peuvent accepter des paramètres de sortie. Les blocs génériques permettent de définir des blocs de code qui peuvent être passés aux fonctions.
* **Interfaces** : Les interfaces définissent des contrats pour les classes ou les structs. Elles garantissent que les implémentations fournissent une fonctionnalité spécifique.
* **Macros** : Les macros permettent de générer du code à partir de métadonnées. Elles peuvent être utilisées pour créer des fonctions génériques ou pour étendre la syntaxe du langage.
* **Métadonnées** : Les métadonnées fournissent des informations supplémentaires sur les types et les membres. Elles peuvent être utilisées pour générer du code ou pour l'introspection.