```d
import std.algorithm, std.array, std.bigint, std.date, std.format, std.math, std.stdio, std.string;
import dependencies.dmatrix;
import dependencies.log2;
import dependencies.monsoon;

class Fichier {
    // Attributs
    string nom;
    string texte;

    // Constructeurs
    @nogc
    this(string nom = @"", string texte = @""):
        this.nom = nom;
        this.texte = texte;
}

class ListeFichiers {
    // Attributs
    immutable[int] taille;
    immutable[string] noms;
    immutable[string] textes;

    // Constructeurs
    this(@immutable[string] noms, @immutable[string] textes):
        foreach (r; noms; a)
            this.taille += r.length;

        this.noms = noms;
        this.textes = textes;
}

class Facture {
    // Attributs
    string numero;
    Date dateFacture;
    string nomClient;
    string adresseClient;
    string codePostalClient;
    string villeClient;
    string paysClient;
    immutable[string] produits;
    immutable[int] quantites;
    immutable[float] prix;
    immutable[float] montants;
    float totalHT;
    float tva;
    immutable[float] tauxTVA;
    float totalTTC;

    // Constructeurs
    this(@immutable[string] produits, @immutable[int] quantites, @immutable[float] prix, @immutable[float] tauxTVA = @immutable[float](20.0f)):
        this.produits = produits;
        this.quantites = quantites;
        this.prix = prix;
        this.tauxTVA = tauxTVA;
        this.montants = prix * quantites;
        this.totalHT = montants[0 ..< taille(montants)];
        this.tva = totalHT * tauxTVA / 100.0f;
        this.totalTTC = totalHT + tva;
}

void main() {
    // Déclaration des variables
    string nomFichier = "facture.txt";
    File fichier = openFile(nomFichier);
    string texteFichier = fichier.readAll();
    Fichier facture = new Fichier(nomFichier, texteFichier);

    // Traitement du fichier
    string[] lignes = facture.texte.split("\n");
    string numeroFacture = lignes[0];
    Date dateFacture = Date.fromString(lignes[1]);

    int tailleListeProduits = (lignes.length - 3) / 3;
    string[] nomsProduits = new string[tailleListeProduits];
    int[] quantitesProduits = new int[tailleListeProduits];
    float[] prixProduits = new float[tailleListeProduits];

    for (int i = 0; i < tailleListeProduits; i++) {
        nomsProduits[i] = lignes[2 + i * 3];
        quantitesProduits[i] = lignes[2 + i * 3 + 1].cast(int);
        prixProduits[i] = lignes[2 + i * 3 + 2].cast(float);
    }

    // Création de la facture
    Facture fact = new Facture(nomsProduits, quantitesProduits, prixProduits);

    // Affichage de la facture
    printlni("Facture n° ", fact.numero);
    printlni("Date de la facture : ", fact.dateFacture.tostring());

    printlni("Nom du client : ", fact.nomClient);
    printlni("Adresse du client : ", fact.adresseClient);
    printlni("Code postal du client : ", fact.codePostalClient);
    printlni("Ville du client : ", fact.villeClient);
    printlni("Pays du client : ", fact.paysClient);

    printlni("Produits commandés :");
    printlni("----------------------");
    for (int i = 0; i < tailleListeProduits; i++) {
        printlni(nomsProduits[i], "\t", quantitesProduits[i], "\t", prixProduits[i]);
    }

    printlni("Total HT : ", fact.totalHT);
    printlni("TVA : ", fact.tva);
    printlni("Total TTC : ", fact.totalTTC);
}
```

**Explication du code**

Ce code écrit une facture à partir d'un fichier texte.

* Le fichier texte doit être au format suivant :

```
Numéro de facture
Date de la facture (format ISO 8601)
Nom du client
Adresse du client
Code postal du client
Ville du client
Pays du client
Nom du produit 1
Quantité du produit 1
Prix du produit 1
Nom du produit 2
Quantité du produit 2
Prix du produit 2
...
```

* Le code lit le fichier texte et en extrait les informations nécessaires.
* Il crée ensuite une instance de la classe `Facture` avec les informations extraites.
* Enfin, il affiche la facture à l'écran.