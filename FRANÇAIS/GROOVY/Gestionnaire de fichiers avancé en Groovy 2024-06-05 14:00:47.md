**Gestionnaire de fichier avancé en Groovy**

Ce code avancé en Groovy permet la manipulation complète des fichiers, y compris la création, la lecture, l'écriture, la copie et la suppression.

```groovy
import java.nio.file.*

class GestionnaireFichier {

    static void main(String[] args) {

        // Créer un fichier
        Path fichier = Paths.get("fichier.txt")
        Files.createFile(fichier)

        // Écrire dans le fichier
        BufferedWriter writer = Files.newBufferedWriter(fichier)
        writer.write("Ceci est une ligne de texte.")
        writer.close()

        // Lire le fichier
        String contenu = Files.readAllLines(fichier).join("\n")

        // Copier un fichier
        Path fichierCopie = Paths.get("copie.txt")
        Files.copy(fichier, fichierCopie)

        // Supprimer un fichier
        Files.delete(fichier)

        // Parcourir un répertoire
        Path repertoire = Paths.get("repertoire")
        Files.walk(repertoire)
                .filter(Files::isRegularFile)
                .forEach({ path -> println path.getFileName() })

        // Compacter un fichier
        Path fichierCompacte = Paths.get("fichier.zip")
        ZipFile zipFile = new ZipFile(fichierCompacte)
        zipFile.addFile(fichier, "fichier.txt")
        zipFile.close()

        // Décompacter un fichier
        Path fichierDecompacte = Paths.get("fichierDecompacte.txt")
        ZipFile zipFileDecompacte = new ZipFile(fichierCompacte)
        zipFileDecompacte.extractFile("fichier.txt", fichierDecompacte)
        zipFileDecompacte.close()
    }
}
```

**Explication du code :**

* **Création d'un fichier :** `Files.createFile(fichier)` crée un nouveau fichier nommé "fichier.txt".

* **Écriture dans le fichier :** `BufferedWriter writer = Files.newBufferedWriter(fichier)` ouvre un flux d'écriture dans le fichier. `writer.write("Ceci est une ligne de texte.")` écrit une ligne de texte dans le fichier. `writer.close()` ferme le flux d'écriture.

* **Lecture du fichier :** `String contenu = Files.readAllLines(fichier).join("\n")` lit toutes les lignes du fichier et les joint en une seule chaîne de caractères.

* **Copie d'un fichier :** `Files.copy(fichier, fichierCopie)` copie le fichier "fichier.txt" dans un nouveau fichier nommé "copie.txt".

* **Suppression d'un fichier :** `Files.delete(fichier)` supprime le fichier "fichier.txt".

* **Parcours d'un répertoire :** `Files.walk(repertoire)` génère un flux de tous les fichiers et répertoires dans le répertoire donné. `filter(Files::isRegularFile)` filtre les fichiers réguliers. `forEach({ path -> println path.getFileName() })` imprime les noms des fichiers.

* **Compactage d'un fichier :** `ZipFile zipFile = new ZipFile(fichierCompacte)` crée un nouveau fichier ZIP. `zipFile.addFile(fichier, "fichier.txt")` ajoute le fichier "fichier.txt" au fichier ZIP. `zipFile.close()` ferme le fichier ZIP.

* **Décompactage d'un fichier :** `ZipFile zipFileDecompacte = new ZipFile(fichierCompacte)` ouvre le fichier ZIP. `zipFileDecompacte.extractFile("fichier.txt", fichierDecompacte)` extrait le fichier "fichier.txt" du fichier ZIP. `zipFileDecompacte.close()` ferme le fichier ZIP.