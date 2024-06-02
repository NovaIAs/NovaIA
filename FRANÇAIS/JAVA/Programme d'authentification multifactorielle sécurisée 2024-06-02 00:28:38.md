**Programme de vérification d'identité sécurisé à plusieurs facteurs**

**Comportement :**

Ce programme implémente un protocole d'authentification à plusieurs facteurs qui vérifie l'identité d'un utilisateur en utilisant un mot de passe, un code PIN envoyé par SMS et une empreinte digitale.

**Code :**

```java
import java.util.*;
import java.security.*;
import javax.crypto.*;

public class AuthentificationMultiFacteurs {

    private static final int LONGUEUR_PIN = 6;
    private static final int DELAI_REQUETE = 300; // en secondes

    private static Map<String, String> motsDePasse = new HashMap<>();
    private static Map<String, String> empreintesDigitales = new HashMap<>();

    public static void main(String[] args) {
        String identifiant = saisirIdentifiant();
        String motDePasse = saisirMotDePasse(identifiant);
        int codePIN = saisirCodePIN(identifiant);
        String empreinteDigitale = saisirEmpreinteDigitale(identifiant);

        if (verifierMotDePasse(identifiant, motDePasse) &&
                verifierCodePIN(identifiant, codePIN) &&
                verifierEmpreinteDigitale(identifiant, empreinteDigitale)) {
            System.out.println("Authentification réussie !");
        } else {
            System.out.println("Authentification échouée !");
        }
    }

    private static String saisirIdentifiant() {
        System.out.println("Saisissez votre identifiant : ");
        return scanner.nextLine();
    }

    private static String saisirMotDePasse(String identifiant) {
        System.out.println("Saisissez votre mot de passe : ");
        String motDePasse = scanner.nextLine();

        if (motsDePasse.containsKey(identifiant)) {
            if (motsDePasse.get(identifiant).equals(motDePasse)) {
                System.out.println("Mot de passe correct !");
            } else {
                System.out.println("Mot de passe incorrect !");
                return null;
            }
        } else {
            System.out.println("Identifiant inconnu !");
            return null;
        }

        return motDePasse;
    }

    private static int saisirCodePIN(String identifiant) {
        System.out.println("Saisissez le code PIN reçu par SMS : ");
        int codePIN = scanner.nextInt();

        if (verifierCodePIN(identifiant, codePIN)) {
            System.out.println("Code PIN correct !");
        } else {
            System.out.println("Code PIN incorrect !");
            return -1;
        }

        return codePIN;
    }

    private static String saisirEmpreinteDigitale(String identifiant) {
        System.out.println("Saisissez votre empreinte digitale : ");
        String empreinteDigitale = scanner.nextLine();

        if (empreintesDigitales.containsKey(identifiant)) {
            if (empreintesDigitales.get(identifiant).equals(empreinteDigitale)) {
                System.out.println("Empreinte digitale correcte !");
            } else {
                System.out.println("Empreinte digitale incorrecte !");
                return null;
            }
        } else {
            System.out.println("Identifiant inconnu !");
            return null;
        }

        return empreinteDigitale;
    }

    private static boolean verifierMotDePasse(String identifiant, String motDePasse) {
        if (motsDePasse.containsKey(identifiant)) {
            return motsDePasse.get(identifiant).equals(motDePasse);
        } else {
            return false;
        }
    }

    private static boolean verifierCodePIN(String identifiant, int codePIN) {
        // Vérifier si le code PIN est valide
        if (codePIN < 0 || codePIN > 999999) {
            return false;
        }

        // Générer la clé de chiffrement à partir de l'identifiant
        SecretKey clé = générerClé(identifiant);

        // Chiffrer le code PIN
        byte[] codePINChiffré = chiffrer(clé, Integer.toString(codePIN));

        // Récupérer le code PIN chiffré stocké
        byte[] codePINChiffréStocké = null;
        if (pinsChiffrés.containsKey(identifiant)) {
            codePINChiffréStocké = pinsChiffrés.get(identifiant);
        }

        // Comparer les codes PIN chiffrés
        return Arrays.equals(codePINChiffré, codePINChiffréStocké);
    }

    private static boolean verifierEmpreinteDigitale(String identifiant, String empreinteDigitale) {
        if (empreintesDigitales.containsKey(identifiant)) {
            return empreintesDigitales.get(identifiant).equals(empreinteDigitale);
        } else {
            return false;
        }
    }

    private static SecretKey générerClé(String identifiant) {
        // Générer un générateur de clé
        KeyGenerator générateurDeClé = KeyGenerator.getInstance("AES");

        // Initialiser le générateur de clé avec une graine dérivée de l'identifiant
        byte[] graine = MessageDigest.getInstance("SHA-256").digest(identifiant.getBytes());
        générateurDeClé.init(128, new SecureRandom(graine));

        // Générer la clé
        return générateurDeClé.generateKey();
    }

    private static byte[] chiffrer(SecretKey clé, String message) {
        try {
            // Créer un objet de chiffrement
            Cipher chiffre = Cipher.getInstance("AES");

            // Initialiser l'objet de chiffrement en mode chiffrement
            chiffre.init(Cipher.ENCRYPT_MODE, clé);

            // Chiffrer le message
            return chiffre.doFinal(message.getBytes());
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
```

**Explication :**

Ce code met en œuvre un protocole d'authentification à plusieurs facteurs :

* **Mot de passe :** L'utilisateur saisit un mot de passe stocké dans une base de données sécurisée.
* **Code PIN :** Un code PIN à 6 chiffres est envoyé par SMS et l'utilisateur doit le saisir. Le code PIN est chiffré à l'aide d'une clé dérivée de l'identifiant de l'utilisateur.
* **Empreinte digitale :** L'utilisateur saisit son empreinte digitale, qui est stockée dans une base de données sécurisée.

L'utilisateur est authentifié uniquement si les trois facteurs sont vérifiés avec succès.

Le code utilise les classes `MessageDigest` et `Cipher` du package `java.security` pour sécuriser les données.