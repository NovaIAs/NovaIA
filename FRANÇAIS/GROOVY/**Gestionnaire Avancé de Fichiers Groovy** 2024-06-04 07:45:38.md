**Gestionnaire de Fichiers Groovy Avancé**

**Objectif:**

Ce code Groovy fournit un gestionnaire de fichiers avancé qui exécute une multitude d'opérations sur les fichiers et les répertoires.

**Code:**

```groovy
// Importer les bibliothèques nécessaires
import java.nio.file.*
import static java.nio.file.StandardCopyOption.*

// Créer un objet Path représentant le répertoire de travail
Path workingDir = Paths.get(System.getProperty("user.dir"))

// Enumérer les fichiers et répertoires dans le répertoire de travail
println("Fichiers et répertoires dans " + workingDir)
workingDir.toFile().eachFile(new FileVisitor() {
    @Override
    boolean visitFile(File file, Attributes attrs) {
        println(file.getName())
        return true
    }

    @Override
    boolean preVisitDirectory(File dir, Attributes attrs) {
        println(dir.getName())
        return true
    }

    @Override
    void postVisitDirectory(File dir, IOException exc) { }
})

// Créer un nouveau fichier
Path newFile = Paths.get(workingDir.toString(), "nouveauFichier.txt")
Files.createFile(newFile)
println("Nouveau fichier créé à " + newFile)

// Copier un fichier dans un autre emplacement
Path sourceFile = Paths.get(workingDir.toString(), "source.txt")
Path targetFile = Paths.get(workingDir.toString(), "cible.txt")
Files.copy(sourceFile, targetFile, REPLACE_EXISTING)
println("Fichier copié de " + sourceFile + " à " + targetFile)

// Déplacer un fichier vers un autre emplacement
Files.move(targetFile, Paths.get(workingDir.toString(), "nouveauRépertoire", "cible.txt"))
println("Fichier déplacé vers " + Paths.get(workingDir.toString(), "nouveauRépertoire", "cible.txt"))

// Supprimer un fichier
Files.delete(newFile)
println("Fichier supprimé : " + newFile)

// Créer un nouveau répertoire
Path newDirectory = Paths.get(workingDir.toString(), "nouveauRépertoire")
Files.createDirectory(newDirectory)
println("Nouveau répertoire créé à " + newDirectory)

// Supprimer un répertoire
Files.delete(newDirectory)
println("Répertoire supprimé : " + newDirectory)

// Compresser un fichier
Path zipFile = Paths.get(workingDir.toString(), "archive.zip")
Files.walkFileTree(workingDir, new ZipFileVisitor(zipFile))
println("Fichiers compressés dans " + zipFile)
```

**Explication:**

* Ce code Groovy se sert des API Java NIO.2 pour gérer les fichiers et les répertoires.
* Il définit un objet `Path` représentant le répertoire de travail.
* Il énumère les fichiers et les répertoires du répertoire de travail à l'aide d'un `FileVisitor`.
* Il crée, copie, déplace, supprime et compresse des fichiers et des répertoires en utilisant les méthodes appropriées de l'API NIO.2.
* Il définit un `ZipFileVisitor` personnalisé pour compresser des fichiers dans un fichier ZIP.
* Les messages d'état sont imprimés à chaque étape pour fournir une information claire sur l'exécution du code.