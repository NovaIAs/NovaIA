**Gestionnaire d'événements d'annonces multicanaux en Groovy**

```groovy
class MultiCanalAdEventManager {

    // Dépôt pour stocker les événements d'annonce
    private Map<String, List<AdEvent>> evenements = [:]

    // Liste de canaux d'annonce pris en charge
    private List<String> canaux = ['google', 'facebook', 'twitter']

    // Méthode pour ajouter un événement d'annonce
    public void ajouterEvenement(AdEvent evenement) {
        // Vérifier si le canal est pris en charge
        if (!canaux.contains(evenement.canal)) {
            throw new IllegalArgumentException("Canal d'annonce non pris en charge : ${evenement.canal}")
        }

        // Initialiser la liste d'événements pour le canal si nécessaire
        if (!evenements.containsKey(evenement.canal)) {
            evenements[evenement.canal] = []
        }

        // Ajouter l'événement à la liste
        evenements[evenement.canal] << evenement
    }

    // Méthode pour récupérer les événements d'annonce pour un canal donné
    public List<AdEvent> getEvenements(String canal) {
        // Vérifier si le canal est pris en charge
        if (!canaux.contains(canal)) {
            throw new IllegalArgumentException("Canal d'annonce non pris en charge : ${canal}")
        }

        // Renvoyer la liste d'événements pour le canal
        return evenements[canal] ?: []
    }

    // Méthode pour supprimer tous les événements d'annonce
    public void supprimerTousLesEvenements() {
        evenements = [:]
    }

    // Méthode pour filtrer les événements d'annonce en fonction d'un prédicat
    public List<AdEvent> filtrerEvenements(Closure<Boolean> predicat) {
        return evenements.values().flatten().findAll(predicat)
    }

    // Méthode pour regrouper les événements d'annonce par canal
    public Map<String, List<AdEvent>> regrouperParCanal() {
        return evenements
    }

    // Méthode pour compter les événements d'annonce pour un canal donné
    public int compterEvenements(String canal) {
        return getEvenements(canal).size()
    }

    // Méthode pour calculer le coût total des événements d'annonce pour un canal donné
    public BigDecimal calculerCout(String canal) {
        return getEvenements(canal).sum { it.cout }
    }

    // Méthode pour calculer le coût moyen des événements d'annonce pour un canal donné
    public BigDecimal calculerCoutMoyen(String canal) {
        def nbEvenements = compterEvenements(canal)
        def coutTotal = calculerCout(canal)

        return nbEvenements == 0 ? 0 : coutTotal / nbEvenements
    }
}
```

**Classe d'événement d'annonce**

```groovy
class AdEvent {

    String canal // Canal d'annonce (par exemple, google, facebook, twitter)
    String nom // Nom de l'événement (par exemple, impression, clic, conversion)
    BigDecimal cout // Coût de l'événement

    // Constructeur
    AdEvent(String canal, String nom, BigDecimal cout) {
        this.canal = canal
        this.nom = nom
        this.cout = cout
    }
}
```

**Utilisation**

```groovy
// Créer un gestionnaire d'événements d'annonces multicanaux
def gestionnaire = new MultiCanalAdEventManager()

// Ajouter quelques événements d'annonce
gestionnaire.ajouterEvenement(new AdEvent('google', 'impression', 0.10))
gestionnaire.ajouterEvenement(new AdEvent('facebook', 'clic', 0.25))
gestionnaire.ajouterEvenement(new AdEvent('twitter', 'conversion', 1.00))

// Récupérer les événements d'annonce pour un canal donné
def evenementsGoogle = gestionnaire.getEvenements('google')

// Filtrer les événements d'annonce en fonction d'un prédicat
def evenementsFiltres = gestionnaire.filtrerEvenements { it.nom == 'clic' }

// Regrouper les événements d'annonce par canal
def evenementsParCanal = gestionnaire.regrouperParCanal()

// Compter les événements d'annonce pour un canal donné
def nbEvenementsGoogle = gestionnaire.compterEvenements('google')

// Calculer le coût total des événements d'annonce pour un canal donné
def coutTotalGoogle = gestionnaire.calculerCout('google')

// Calculer le coût moyen des événements d'annonce pour un canal donné
def coutMoyenGoogle = gestionnaire.calculerCoutMoyen('google')
```

**Explication**

Ce code définit un gestionnaire d'événements d'annonces multicanaux qui peut être utilisé pour stocker, récupérer et filtrer les événements d'annonces provenant de différents canaux. Il offre également des méthodes pratiques pour regrouper les événements, compter les événements et calculer le coût total et moyen des événements. Le code utilise Groovy pour ses fonctionnalités concises et expressives, telles que les closures, la délégation de méthodes et les expressions GString.