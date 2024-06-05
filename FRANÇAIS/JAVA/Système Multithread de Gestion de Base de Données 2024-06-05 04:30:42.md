**Système de Gestion de Base de Données Multi-Thread**

**Objectif :**
Créer un système multi-thread pour gérer une base de données, permettant des lectures et écritures simultanées par plusieurs utilisateurs.

**Code :**

```java
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

public class SystemeGestionBDDMultiThread {

    private ConcurrentHashMap<Integer, Enregistrement> baseDeDonnees;
    private ReentrantLock verrou = new ReentrantLock();
    private int nbLecteurs = 0;

    public SystemeGestionBDDMultiThread() {
        baseDeDonnees = new ConcurrentHashMap<>();
    }

    public void ecrireEnregistrement(int id, Enregistrement enregistrement) {
        verrou.lock();
        baseDeDonnees.put(id, enregistrement);
        verrou.unlock();
    }

    public Enregistrement lireEnregistrement(int id) {
        while (verrou.isLocked()) {
            // Si l'écriture est en cours, attendre
            try {
                Thread.sleep(50);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        verrou.lock();
        nbLecteurs++;
        verrou.unlock();

        Enregistrement enregistrement = baseDeDonnees.get(id);

        verrou.lock();
        nbLecteurs--;
        if (nbLecteurs == 0) {
            // Si aucun lecteur, libérer le verrou pour l'écriture
            verrou.unlock();
        }
        verrou.unlock();

        return enregistrement;
    }
}
```

**Fonctionnement :**

* Le système utilise une mémoire tampon `ConcurrentHashMap` pour stocker les enregistrements de la base de données.
* Un `ReentrantLock` est utilisé pour gérer l'accès exclusif à la base de données pour les écritures.
* Un compteur `nbLecteurs` est utilisé pour permettre des lectures simultanées tant qu'aucune écriture n'est en cours.
* Les threads lecteurs attendent si une écriture est en cours (grâce à `isLocked()`).
* Lorsqu'un thread lecteur accède à la base de données, il incrémente le compteur `nbLecteurs` et libère le verrou (mais attend si une écriture est en cours).
* Lorsqu'un thread lecteur termine, il décrémente le compteur `nbLecteurs`. Lorsque le compteur atteint 0, le verrou est libéré pour une éventuelle écriture.

**Avantages :**

* Permet des lectures et écritures simultanées dans une base de données partagée.
* Garantit l'intégrité des données grâce au verrouillage exclusif pour les écritures.
* Utilise une approche non bloquante pour les lectures, améliorant les performances.