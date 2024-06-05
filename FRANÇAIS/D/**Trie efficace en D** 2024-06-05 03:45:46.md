**Code complexe en langage D**

```d
import std.algorithm, std.stdio;

// Construire un trie pour un ensemble de chaînes
struct TrieNode {
    immutable[]char key;
    TrieNode?[] children = new[26]!TrieNode; // Tableau de 26 enfants (un pour chaque lettre de l'alphabet)
    bool isEnd = false; // True si c'est un noeud terminal (c'est-à-dire si le mot correspondant est dans l'ensemble)
}

TrieNode buildTrie(in[]string words) {
    auto trie = TrieNode();
    foreach (word; words) {
        auto node = &trie;
        foreach (c; word) {
            immutable index = c.toUpper() - 'A';
            if (!node.children[index])
                node.children[index] = new TrieNode(c);
            node = node.children[index];
        }
        node.isEnd = true;
    }
    return trie;
}

// Rechercher un mot dans un trie
bool findWord(TrieNode trie, in string word) {
    foreach (c; word) {
        immutable index = c.toUpper() - 'A';
        if (!trie.children[index])
            return false;
        trie = trie.children[index];
    }
    return trie.isEnd;
}

// Imprimer un trie (à des fins de débogage)
void printTrie(TrieNode trie, int depth = 0) {
    foreach (child; trie.children) {
        immutable level = "  " * depth;
        writefln("%s%s", level, child?.key ?? "");
        if (child)
            printTrie(child, depth + 1);
    }
}

void main() {
    auto words = ["hello", "world", "amazing", "beautiful"];
    auto trie = buildTrie(words);

    // Tester la recherche
    writefln("Recherche de \"hello\" : %s", findWord(trie, "hello") ? "trouvé" : "non trouvé");
    writefln("Recherche de \"world\" : %s", findWord(trie, "world") ? "trouvé" : "non trouvé");
    writefln("Recherche de \"amazing\" : %s", findWord(trie, "amazing") ? "trouvé" : "non trouvé");
    writefln("Recherche de \"beautiful\" : %s", findWord(trie, "beautiful") ? "trouvé" : "non trouvé");
    writefln("Recherche de \"inexistant\" : %s", findWord(trie, "inexistant") ? "trouvé" : "non trouvé");

    // Imprimer le trie (à des fins de débogage)
    writefln("\nStructure du trie :");
    printTrie(trie);
}
```

**Explication du code**

Ce code complexe en langage D implémente un trie, qui est une structure de données utilisée pour stocker efficacement des ensembles de chaînes de caractères.

La structure `TrieNode` représente un noeud du trie. Chaque noeud a une clé (une lettre de l'alphabet), un tableau d'enfants (un pour chaque lettre de l'alphabet) et un indicateur indiquant s'il s'agit d'un noeud terminal (c'est-à-dire si le mot correspondant est dans l'ensemble).

La fonction `buildTrie` construit un trie à partir d'un ensemble de chaînes de caractères. Elle insère chaque chaîne dans le trie en créant les noeuds nécessaires et en définissant l'indicateur `isEnd` sur `true` pour le noeud terminal.

La fonction `findWord` recherche un mot dans un trie. Elle parcourt le trie caractère par caractère, suivant les enfants correspondant à chaque lettre. Si elle trouve un noeud terminal avec le même mot que celui recherché, elle renvoie `true`. Sinon, elle renvoie `false`.

La fonction `printTrie` (utilisée à des fins de débogage) imprime la structure du trie en profondeur. Elle affiche les clés des noeuds et leur niveau dans le trie.

Dans la fonction `main`, nous construisons un trie à partir d'un ensemble de mots, effectuons des recherches sur le trie et affichons sa structure à des fins de débogage.