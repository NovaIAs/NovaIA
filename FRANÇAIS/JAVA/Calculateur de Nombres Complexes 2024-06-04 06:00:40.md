```java
class CalculateurComplexe {

    private double partieReelle;
    private double partieImaginaire;

    public CalculateurComplexe() {
        this(0.0, 0.0);
    }

    public CalculateurComplexe(double partieReelle, double partieImaginaire) {
        this.partieReelle = partieReelle;
        this.partieImaginaire = partieImaginaire;
    }

    public double getPartieReelle() {
        return partieReelle;
    }

    public void setPartieReelle(double partieReelle) {
        this.partieReelle = partieReelle;
    }

    public double getPartieImaginaire() {
        return partieImaginaire;
    }

    public void setPartieImaginaire(double partieImaginaire) {
        this.partieImaginaire = partieImaginaire;
    }

    @Override
    public String toString() {
        return "(" + partieReelle + " + " + partieImaginaire + "i)";
    }

    public CalculateurComplexe addition(CalculateurComplexe autre) {
        return new CalculateurComplexe(
                this.partieReelle + autre.partieReelle,
                this.partieImaginaire + autre.partieImaginaire);
    }

    public CalculateurComplexe soustraction(CalculateurComplexe autre) {
        return new CalculateurComplexe(
                this.partieReelle - autre.partieReelle,
                this.partieImaginaire - autre.partieImaginaire);
    }

    public CalculateurComplexe multiplication(CalculateurComplexe autre) {
        return new CalculateurComplexe(
                (this.partieReelle * autre.partieReelle) - (this.partieImaginaire * autre.partieImaginaire),
                (this.partieReelle * autre.partieImaginaire) + (this.partieImaginaire * autre.partieReelle));
    }

    public CalculateurComplexe division(CalculateurComplexe autre) {
        double denominateur = (autre.partieReelle * autre.partieReelle) + (autre.partieImaginaire * autre.partieImaginaire);
        return new CalculateurComplexe(
                ((this.partieReelle * autre.partieReelle) + (this.partieImaginaire * autre.partieImaginaire)) / denominateur,
                ((this.partieImaginaire * autre.partieReelle) - (this.partieReelle * autre.partieImaginaire)) / denominateur);
    }

    public static void main(String[] args) {
        CalculateurComplexe complexe1 = new CalculateurComplexe(3.2, 5.4);
        CalculateurComplexe complexe2 = new CalculateurComplexe(5.1, 9.2);

        System.out.println("Premier complexe : " + complexe1);
        System.out.println("Deuxième complexe : " + complexe2);

        System.out.println("Addition : " + complexe1.addition(complexe2));
        System.out.println("Soustraction : " + complexe1.soustraction(complexe2));
        System.out.println("Multiplication : " + complexe1.multiplication(complexe2));
        System.out.println("Division : " + complexe1.division(complexe2));
    }
}
```

**Explication du code :**

La classe `CalculateurComplexe` représente les nombres complexes et fournit des méthodes pour effectuer des opérations arithmétiques.

- Les variables d'instance `partieReelle` et `partieImaginaire` stockent les parties réelle et imaginaire du nombre complexe.
- Le constructeur par défaut initialise un nombre complexe avec des parties réelle et imaginaire nulles.
- Les méthodes `getPartieReelle` et `getPartieImaginaire` récupèrent les parties réelle et imaginaire du nombre, tandis que les méthodes `setPartieReelle` et `setPartieImaginaire` les définissent.
- La méthode `addition` additionne deux nombres complexes.
- La méthode `soustraction` soustrait un nombre complexe à un autre.
- La méthode `multiplication` multiplie deux nombres complexes.
- La méthode `division` divise un nombre complexe par un autre.
- La méthode `toString` renvoie une représentation de chaîne du nombre complexe.
- La méthode `main` crée deux instances de `CalculateurComplexe`, leur applique diverses opérations arithmétiques et affiche les résultats.