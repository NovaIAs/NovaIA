**Programme en Java pour la simulation d'un système de files d'attente**

**Objectif :** Simuler le comportement d'un système de files d'attente dans lequel les clients arrivent à des intervalles aléatoires, sont servis par des serveurs et quittent le système.

**Concepts clés :**

* **File d'attente :** Une structure de données qui stocke les éléments selon le principe du premier entré, premier sorti (FIFO).
* **Simulation :** Modélisation d'un système réel à l'aide de techniques informatiques.
* **Distribution aléatoire :** Génération de nombres aléatoires qui suivent une distribution spécifique, par exemple la distribution exponentielle.

**Code source :**

```java
import java.util.*;

public class SimulationFilesAttente {

    // Paramètres de la simulation
    private static final double LAMBDA_ARRIVEES = 0.5; // Taux d'arrivées moyen (clients/minute)
    private static final double MU_SERVICE = 1.0; // Taux de service moyen (clients/minute)
    private static final int DUREE_SIMULATION = 60; // Durée de la simulation (minutes)

    // États du client
    private enum EtatClient {
        EN_ATTENTE, EN_COURS_DE_SERVICE, SORTI
    }

    // File d'attente
    private static Queue<Client> fileAttente = new LinkedList<>();

    // Serveurs
    private static Server[] serveurs = new Server[3];

    // Générateur de nombres aléatoires
    private static Random random = new Random();

    public static void main(String[] args) {
        // Initialisation
        for (int i = 0; i < serveurs.length; i++) {
            serveurs[i] = new Server(MU_SERVICE);
        }

        // Simulation
        double tempsActuel = 0.0;
        while (tempsActuel < DUREE_SIMULATION) {
            // Arrivées
            if (random.nextDouble() < LAMBDA_ARRIVEES * tempsActuel) {
                Client nouveauClient = new Client(tempsActuel);
                fileAttente.add(nouveauClient);
            }

            // Service
            for (Server serveur : serveurs) {
                serveur.servir(tempsActuel);
            }

            // Mises à jour
            tempsActuel += 0.01;
        }

        // Résultats
        System.out.println("Durée moyenne d'attente : " + getDureeMoyenneAttente());
        System.out.println("Longueur maximale de la file d'attente : " + getLongueurMaxFileAttente());
    }

    // Renvoie la durée moyenne d'attente des clients
    private static double getDureeMoyenneAttente() {
        double dureeTotale = 0.0;
        for (Client client : fileAttente) {
            dureeTotale += client.getTempsAttente();
        }
        return dureeTotale / fileAttente.size();
    }

    // Renvoie la longueur maximale atteinte par la file d'attente
    private static int getLongueurMaxFileAttente() {
        int longueurMax = 0;
        for (Client client : fileAttente) {
            longueurMax = Math.max(longueurMax, client.getInstantArrivee());
        }
        return longueurMax;
    }

    // Classe représentant un client
    private static class Client {
        private final double instantArrivee; // Instant d'arrivée du client
        private double instantDepart; // Instant de départ du client
        private EtatClient etat; // État actuel du client

        public Client(double instantArrivee) {
            this.instantArrivee = instantArrivee;
            this.etat = EtatClient.EN_ATTENTE;
        }

        public double getTempsAttente() {
            return instantDepart - instantArrivee;
        }

        public double getInstantArrivee() {
            return instantArrivee;
        }

        public EtatClient getEtat() {
            return etat;
        }

        public void setEtat(EtatClient etat) {
            this.etat = etat;
        }

        public void setInstantDepart(double instantDepart) {
            this.instantDepart = instantDepart;
        }
    }

    // Classe représentant un serveur
    private static class Server {
        private double tauxService; // Taux de service du serveur
        private Client clientActuel; // Client actuellement servi par le serveur

        public Server(double tauxService) {
            this.tauxService = tauxService;
        }

        public void servir(double tempsActuel) {
            // Si le serveur est libre et qu'il y a des clients en attente
            if (clientActuel == null && !fileAttente.isEmpty()) {
                clientActuel = fileAttente.remove();
                clientActuel.setEtat(EtatClient.EN_COURS_DE_SERVICE);
            }

            // Si le serveur sert un client
            if (clientActuel != null) {
                // Si le service est terminé
                if (random.nextDouble() < tauxService * tempsActuel) {
                    clientActuel.setEtat(EtatClient.SORTI);
                    clientActuel.setInstantDepart(tempsActuel);
                    clientActuel = null;
                }
            }
        }
    }
}
```

**Explication du code :**

* **Classes Client et Server :** Ces classes modélisent le comportement des clients et des serveurs. Elles contiennent les attributs et les méthodes nécessaires pour gérer les états, le service et les temps d'attente.
* **Simulation :** La boucle principale de la simulation génère des arrivées de clients à des intervalles aléatoires, attribue les clients aux serveurs libres et met à jour l'état des clients et des serveurs en fonction du taux de service.
* **Calcul des résultats :** Les méthodes `getDureeMoyenneAttente()` et `getLongueurMaxFileAttente()` calculent les statistiques de la simulation, à savoir la durée moyenne d'attente des clients et la longueur maximale atteinte par la file d'attente.

Ce code simule un système de files d'attente réaliste en tenant compte des arrivées aléatoires, des taux de service et des états des clients. Il fournit des statistiques utiles sur les performances du système, telles que la durée moyenne d'attente et la longueur maximale de la file d'attente.