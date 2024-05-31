**Analyse Récursive d'Expressions Régulières**

```d
import std.stdio, std.bigint, std.range, std.array, std.algorithm;

enum RegExpSyntax {
    RE_NONE,
    RE_CHAR,
    RE_EMPTY,
    RE_ANY,
    RE_BOL,
    RE_EOL,
    RE_GROUP,
    RE_CHOICE,
    RE_PLUS,
    RE_STAR,
    RE_QMARK,
    RE_BACKREF,
    RE_DOT,
}

class RegExpState {
    // État du NFA
    immutable RegExpSyntax tipo;
    immutable byte[] chars;
    immutable RegExpState[] substates;

    this(RegExpSyntax tipo, ...) {
        this.tipo = tipo;
        this.chars = ...;
        this.substates = ...;
    }
}

immutable RegExpToken(RegExpState state, int pos)

class NFA {
    immutable RegExpState[] reStates;
    int startState;
    int[] acceptStates;

    immutable int nStates() @safe {
        reStates.length;
    }

    immutable RegExpToken next(int state, byte input) pure @safe {
        immutable states := reStates[state].substates;
        foreach (s; states) {
            foreach (c; reStates[s].chars) {
                if ((s == startState) || (input & c == input)) {
                    return RegExpToken(reStates[s], state);
                }
            }
        }

        return RegExpToken(reStates[0], state);
    }

    immutable RegExpToken match(in byte[] string) pure nothrow nofail {
        int state := startState;
        foreach (byte c; string) {
            state = next(state, c);
            if (acceptStates.indexOf(state) != -1) {
                return RegExpToken(reStates[state], state);
            }
        }

        return RegExpToken(reStates[startState], startState);
    }
}

void main() {
    import std.strformat;

    immutable reString := "ab(c|d)*?e";

    immutable reAST := [
        RegExpState(RE_ANY, []),
        RegExpState(RE_CHAR, 0x61), // a
        RegExpState(RE_CHAR, 0x62), // b
        RegExpState(RE_GROUP, []),
        RegExpState(RE_CHAR, 0x63), // c
        RegExpState(RE_CHOICE, []),
        RegExpState(RE_CHAR, 0x64), // d
        RegExpState(RE_GROUP, []),
        RegExpState(RE_CHAR, 0x65), // e
        RegExpState(RE_STAR, []),
        RegExpState(RE_QMARK, [],),
    ];

    immutable reState := RegExpState(RE_ROOT, [2]);

    immutable nfa := compileRE(reState, reAST, 0), nfa;
    immutable reMatch := nfa.match(reString.bytes);

    writeln("Expression : {}", reString);
    if (reMatch.pos == -1) {
        writeln("Aucune correspondance");
    } else {
        writeln("Correspondance");
        writeln("Début : {}", reMatch.pos + 1);
        writeln("Fin : {}", reMatch.state + 1);
        writeln("Groupe : {}", reMatch.state == 3);
    }
}
```

**Explication :**

Ce code crée un automate fini non déterministe (AFND) à partir d'une expression régulière et l'utilise pour rechercher une correspondance dans une chaîne donnée.

**Classe RegExpState :** représente l'état d'un AFND. Il contient des informations sur le type d'état (caractère, groupe, choix, etc.), les caractères sur lesquels il peut se déclencher et les états suivants.

**Classe RegExpToken :** représente un jeton dans l'AFND, contenant l'état actuel et la position dans la chaîne d'entrée.

**Classe NFA :** stocke l'AFND lui-même. Il contient un tableau d'états, un état de démarrage et un tableau d'états d'acceptation.

**next(state, input) :** calcule l'état suivant dans l'AFND pour une entrée donnée.

**match(string) :** explore l'AFND pour trouver une correspondance dans la chaîne donnée. Il renvoie un jeton si une correspondance est trouvée.

**main() :** est la fonction principale du programme. Elle compile l'expression régulière, crée un AFND et l'utilise pour rechercher une correspondance dans une chaîne d'entrée.