```d
import std.stdio, std.algorithm, std.range, std.bigint, std.random, std.traits, std.json, std.exception;
import std.string.utf8 as utf8;
import std.regex as regex;
import std.traits.bitvector as bitvector;

enum NOTE {
    C, C#, D, D#, E, F, F#, G, G#, A, A#, B
}

immutable tuple[] intervalByNote = [
    (NOTE.C, NOTE.D), (NOTE.C#, NOTE.D#),
    (NOTE.D, NOTE.E), (NOTE.D#, NOTE.F),
    (NOTE.E, NOTE.F#), (NOTE.F, NOTE.G),
    (NOTE.F#, NOTE.G#), (NOTE.G, NOTE.A),
    (NOTE.G#, NOTE.A#), (NOTE.A, NOTE.B),
    (NOTE.A#, NOTE.C), (NOTE.B, NOTE.C)
];

class Interval {
    NOTE from;
    NOTE to;
     immutable constructor init(NOTE noteFrom, NOTE noteTo) {
        this.from = noteFrom;
        this.to = noteTo;
    }

    immutable override string toString() {
        return "${from.toString()}-${to.toString()}";
    }
}

class Chord {
    immutable Interval interval;
    immutable tuple[] notes;
    immutable constructor init(Interval ivl, NOTE baseNote) {
        this.interval = ivl;
        this.notes = [baseNote];
        foreach (i; intervalByNote.length) {
            notes ~ (intervalByNote[i][1]);
        }
    }

    immutable override string toString() {
        string out = "";
        foreach (i, n; notes) {
            out ~= "${n.toString()} ";
        }
        return out;
    }
}

class Song {
    immutable string title;
    immutable SongPart[] parts;

    immutable constructor init(string ttl, SongPart[] p) {
        this.title = ttl;
        this.parts = p;
    }

    immutable override string toString() {
        string out = "#{title}:\n";
        foreach (i, p; parts) {
            out ~= "${p.toString()}";
        }
        return out;
    }
}

class SongPart {
    immutable string name;
    immutable Chord[] chords;
    immutable constructor init(string nme, Chord[] c) {
        this.name = nme;
        this.chords = c;
    }

    immutable override string toString() {
        string out = "#{name}:\n";
        foreach (i, c; chords) {
            out ~= "${c.toString()} ";
        }
        return out;
    }
}

Chord Cmaj   = new Chord(new Interval(NOTE.C, NOTE.E), NOTE.C);
Chord Gmaj   = new Chord(new Interval(NOTE.G, NOTE.B), NOTE.G);
Chord Dmaj   = new Chord(new Interval(NOTE.D, NOTE.F#), NOTE.D);
Chord Amaj   = new Chord(new Interval(NOTE.A, NOTE.C#), NOTE.A);
Chord Emaj   = new Chord(new Interval(NOTE.E, NOTE.G#), NOTE.E);
Chord Fmaj   = new Chord(new Interval(NOTE.F, NOTE.A), NOTE.F);
Chord Bbmaj  = new Chord(new Interval(NOTE.Bb, NOTE.D), NOTE.Bb);
Chord Ebmaj  = new Chord(new Interval(NOTE.Eb, NOTE.G), NOTE.Eb);
Chord Abmaj  = new Chord(new Interval(NOTE.Ab, NOTE.C), NOTE.Ab);
Chord Dbmaj  = new Chord(new Interval(NOTE.Db, NOTE.F), NOTE.Db);
Chord Gbmaj  = new Chord(new Interval(NOTE.Gb, NOTE.Bb), NOTE.Gb);
Chord Cbmaj  = new Chord(new Interval(NOTE.Cb, NOTE.Eb), NOTE.Cb);

Chord Gmin   = new Chord(new Interval(NOTE.G, NOTE.Bb), NOTE.G);
Chord Dmin   = new Chord(new Interval(NOTE.D, NOTE.F), NOTE.D);
Chord Amin   = new Chord(new Interval(NOTE.A, NOTE.C), NOTE.A);
Chord Emin   = new Chord(new Interval(NOTE.E, NOTE.G), NOTE.E);
Chord Cmin   = new Chord(new Interval(NOTE.C, NOTE.Eb), NOTE.C);
Chord Fmin   = new Chord(new Interval(NOTE.F, NOTE.Ab), NOTE.F);
Chord Bbmin  = new Chord(new Interval(NOTE.Bb, NOTE.Db), NOTE.Bb);
Chord Ebmin  = new Chord(new Interval(NOTE.Eb, NOTE.Gb), NOTE.Eb);
Chord Abmin  = new Chord(new Interval(NOTE.Ab, NOTE.Bb), NOTE.Ab);
Chord Dbmin  = new Chord(new Interval(NOTE.Db, NOTE.Eb), NOTE.Db);
Chord Gbmin  = new Chord(new Interval(NOTE.Gb, NOTE.Bb), NOTE.Gb);
Chord Cbmin  = new Chord(new Interval(NOTE.Cb, NOTE.Db), NOTE.Cb);

SongPart intro = new SongPart("Intro", [Gmaj, Cmaj]);
SongPart verse = new SongPart("Verse", [Gmaj, Dmaj, Emin, Cmaj]);
SongPart chorus = new SongPart("Chorus", [Gmaj, Cmaj, Dmaj, Gmaj]);
SongPart bridge = new SongPart("Bridge", [Bbmaj, Ebmaj, Abmaj, Dbmaj]);
SongPart outro = new SongPart("Outro", [Dbmin, Gbmin, Cbmin, Dbmin]);

Song mySong = new Song("My Song", [intro, verse, chorus, bridge, outro]);

void main() {
    writefln("Song: %s", mySong.title);
    foreach (i, p; mySong.parts) {
        writefln("%s", p.toString());
    }
}
```

**Explicación del código:**

* El código anterior es un programa escrito en el lenguaje de programación D.
* El programa crea una canción llamada "My Song" y la imprime en la consola.
* La canción consta de cinco partes: intro, verso, estribillo, puente y outro.
* Cada parte de la canción se define como un array de acordes.
* Los acordes se definen como una tupla de intervalos y una nota base.
* Los intervalos se definen como una tupla de dos notas.
* Las notas se definen como una enumeración de los 12 tonos de la escala cromática.
* El programa utiliza la clase `Song` para representar la canción.
* La clase `Song` tiene tres campos: un título, un array de partes de la canción y un método `toString()` que imprime la canción en la consola.
* El programa utiliza la clase `SongPart` para representar cada parte de la canción.
* La clase `SongPart` tiene tres campos: un nombre, un array de acordes y un método `toString()` que imprime la parte de la canción en la consola.
* El programa utiliza la clase `Chord` para representar cada acorde de la canción.
* La clase `Chord` tiene dos campos: un intervalo y una nota base.
* La clase `Chord` tiene tres métodos: un constructor que crea un nuevo acorde, un método `toString()` que imprime el acorde en la consola y un método `getNotes()` que devuelve un array de notas en el acorde.
* El programa utiliza la clase `Interval` para representar cada intervalo de la canción.
* La clase `Interval` tiene dos campos: una nota de inicio y una nota de fin.
* La clase `Interval` tiene un constructor que crea un nuevo intervalo y un método `toString()` que imprime el intervalo en la consola.
* El programa utiliza la clase `NOTE` para representar cada nota de la canción.
* La clase `NOTE` es una enumeración de los 12 tonos de la escala cromática.
* El programa utiliza la clase `Array` para representar cada array de acordes, partes de la canción y notas.
* La clase `Array` es una clase genérica que puede contener cualquier tipo de datos.
* El programa utiliza la clase `Tuple` para representar cada tupla de intervalos y notas base.
* La clase `Tuple` es una clase genérica que puede contener cualquier número de elementos.
* El programa utiliza la función `writefln()` para imprimir texto en la consola.
* La función `writefln()` toma una cadena de formato y un número variable de argumentos.
* La cadena de formato especifica el formato del texto que se va a imprimir.
* Los argumentos son los valores que se van a imprimir en el texto.
* El programa utiliza la función `foreach()` para iterar sobre cada elemento de un array.
* La función `foreach()` toma un identificador de bucle y un array.
* El identificador de bucle es la variable que se utilizará para acceder a cada elemento del array.
* El array es el array sobre el que se va a iterar.
* El programa utiliza el operador `~` para concatenar dos cadenas de texto.
* El operador `~` toma dos cadenas de texto y las concatena en una sola cadena de texto.
* El programa utiliza el operador `+=` para añadir un elemento a un array.
* El operador `+=` toma un array y un elemento y añade el elemento al array.
* El programa utiliza el operador `[]` para acceder a un elemento de un array.
* El operador `[]` toma un array y un índice.
* El índice es el número del elemento al que se quiere acceder.
* El programa utiliza el operador `.length` para obtener la longitud de un array.
* El operador `.length` toma un array y devuelve el número de elementos en el array.