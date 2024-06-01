```groovy
// Imports nécessaires
import groovy.swing.SwingBuilder
import java.awt.Color
import javax.swing.*

// Création de la fenêtre
def s = new SwingBuilder()
def fenetre = s.frame(
    title: "Fenêtre d'exemple complexe",
    size: [400, 300],
    locationRelativeTo: null
)

// Création des composants
def panel = s.panel(border: BorderFactory.createEmptyBorder(10))
def etiq1 = s.label(text: "Bonjour", foreground: Color.BLUE)
def etiq2 = s.label(text: "Monde", foreground: Color.RED)
def bouton = s.button(text: "Cliquez-moi")

// Ajout des composants au panneau
panel.add(etiq1)
panel.add(etiq2)
panel.add(bouton)

// Ajout du panneau à la fenêtre
fenetre.add(panel)

// Gestionnaire des événements
bouton.addActionListener {
    etiq1.text = "Clic effectué"
    etiq2.background = Color.GREEN
}

// Affichage de la fenêtre
fenetre.pack()
fenetre.visible = true

// Explication du code

// Imports : Importation des bibliothèques nécessaires.
// Création de la fenêtre : Crée une nouvelle fenêtre Java Swing avec le titre, la taille et l'emplacement spécifiés.
// Création des composants : Crée des composants Swing tels que des étiquettes et des boutons.
// Ajout des composants au panneau : Ajoute les composants au panneau.
// Ajout du panneau à la fenêtre : Ajoute le panneau à la fenêtre.
// Gestionnaire des événements : Définit un gestionnaire d'événements pour le bouton, qui modifie le texte et la couleur d'arrière-plan des étiquettes lorsque le bouton est cliqué.
// Affichage de la fenêtre : Affiche la fenêtre en packant le contenu et en la rendant visible.
```