**Gestion avancée des rendez-vous pour un système de clinique**

```java
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.*;

public class GestionRendezVous {

    private Map<LocalDate, Map<LocalTime, RendezVous>> rendezVous = new HashMap<>();

    // Ajouter un rendez-vous
    public void ajouterRendezVous(LocalDate date, LocalTime heure, Patient patient, Medecin medecin) {
        if (!rendezVous.containsKey(date)) {
            rendezVous.put(date, new HashMap<>());
        }
        Map<LocalTime, RendezVous> horaires = rendezVous.get(date);
        if (horaires.containsKey(heure)) {
            throw new IllegalArgumentException("Cet horaire est déjà réservé.");
        }
        RendezVous nouveauRendezVous = new RendezVous(patient, medecin, date, heure);
        horaires.put(heure, nouveauRendezVous);
    }

    // Annuler un rendez-vous
    public boolean annulerRendezVous(LocalDate date, LocalTime heure) {
        if (!rendezVous.containsKey(date)) {
            return false;
        }
        Map<LocalTime, RendezVous> horaires = rendezVous.get(date);
        if (!horaires.containsKey(heure)) {
            return false;
        }
        rendezVous.get(date).remove(heure);
        return true;
    }

    // Rechercher des rendez-vous par date
    public List<RendezVous> rechercherRendezVousParDate(LocalDate date) {
        if (!rendezVous.containsKey(date)) {
            return Collections.emptyList();
        }
        return new ArrayList<>(rendezVous.get(date).values());
    }

    // Rechercher des rendez-vous par patient
    public List<RendezVous> rechercherRendezVousParPatient(Patient patient) {
        List<RendezVous> resultats = new ArrayList<>();
        for (Map<LocalTime, RendezVous> horaires : rendezVous.values()) {
            for (RendezVous rendezVous : horaires.values()) {
                if (rendezVous.getPatient().equals(patient)) {
                    resultats.add(rendezVous);
                }
            }
        }
        return resultats;
    }

    // Rechercher des rendez-vous par médecin
    public List<RendezVous> rechercherRendezVousParMedecin(Medecin medecin) {
        List<RendezVous> resultats = new ArrayList<>();
        for (Map<LocalTime, RendezVous> horaires : rendezVous.values()) {
            for (RendezVous rendezVous : horaires.values()) {
                if (rendezVous.getMedecin().equals(medecin)) {
                    resultats.add(rendezVous);
                }
            }
        }
        return resultats;
    }

    // Classe RendezVous
    public static class RendezVous {

        private Patient patient;
        private Medecin medecin;
        private LocalDate date;
        private LocalTime heure;

        public RendezVous(Patient patient, Medecin medecin, LocalDate date, LocalTime heure) {
            this.patient = patient;
            this.medecin = medecin;
            this.date = date;
            this.heure = heure;
        }

        public Patient getPatient() {
            return patient;
        }

        public Medecin getMedecin() {
            return medecin;
        }

        public LocalDate getDate() {
            return date;
        }

        public LocalTime getHeure() {
            return heure;
        }
    }

    // Classe Patient
    public static class Patient {

        private String nom;
        private String prenom;
        private String numSecu;

        public Patient(String nom, String prenom, String numSecu) {
            this.nom = nom;
            this.prenom = prenom;
            this.numSecu = numSecu;
        }

        public String getNom() {
            return nom;
        }

        public String getPrenom() {
            return prenom;
        }

        public String getNumSecu() {
            return numSecu;
        }
    }

    // Classe Medecin
    public static class Medecin {

        private String nom;
        private String prenom;
        private String specialite;

        public Medecin(String nom, String prenom, String specialite) {
            this.nom = nom;
            this.prenom = prenom;
            this.specialite = specialite;
        }

        public String getNom() {
            return nom;
        }

        public String getPrenom() {
            return prenom;
        }

        public String getSpecialite() {
            return specialite;
        }
    }
}
```

**Explications du code :**

Ce code gère les rendez-vous d'une clinique en Java. Il utilise des structures de données complexes, notamment des cartes et des listes, pour représenter les rendez-vous. Le code offre les fonctionnalités suivantes :

* Ajout, annulation, recherche et liste des rendez-vous par date, patient ou médecin.
* Les classes RendezVous, Patient et Medecin encapsulent les informations respectives sur les rendez-vous, les patients et les médecins.
* Les exceptions IllegalArgumentException sont levées en cas d'erreurs, comme lorsque l'on essaie d'ajouter un rendez-vous à une heure déjà réservée.