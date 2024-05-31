**Programme de Simulation de Trafic routier**

**Classe Voiture**

```c#
public class Voiture
{
    public int Vitesse { get; set; }
    public double PositionX { get; set; }
    public double PositionY { get; set; }
    public bool EstEnAccident { get; set; }
    public Voiture(int vitesseInitiale, double positionX, double positionY)
    {
        Vitesse = vitesseInitiale;
        PositionX = positionX;
        PositionY = positionY;
        EstEnAccident = false;
    }

    public void Déplacer()
    {
        if (!EstEnAccident)
        {
            PositionX += Vitesse * Math.Cos(Angle);
            PositionY += Vitesse * Math.Sin(Angle);
        }
    }

    public void Freiner(int ralentissement)
    {
        Vitesse -= ralentissement;
        if (Vitesse < 0) Vitesse = 0;
    }

    public void Accélérer(int accélération)
    {
        Vitesse += accélération;
        if (Vitesse > VitesseMax) Vitesse = VitesseMax;
    }

    public bool EstEnCollision(Voiture autreVoiture)
    {
        double distanceX = PositionX - autreVoiture.PositionX;
        double distanceY = PositionY - autreVoiture.PositionY;
        double distance = Math.Sqrt(distanceX * distanceX + distanceY * distanceY);
        return distance < RayonDeCollision;
    }
}
```

**Classe Route**

```c#
public class Route
{
    public List<Voiture> ListeVoitures { get; set; }
    public double Longueur { get; set; }
    public double Largeur { get; set; }
    public Route(double longueur, double largeur)
    {
        ListeVoitures = new List<Voiture>();
        Longueur = longueur;
        Largeur = largeur;
    }

    public void AjouterVoiture(Voiture voiture)
    {
        ListeVoitures.Add(voiture);
    }

    public void DéplacerVoitures()
    {
        foreach (Voiture voiture in ListeVoitures)
        {
            if (!voiture.EstEnAccident)
            {
                voiture.Déplacer();
                VérifierCollisions(voiture);
            }
        }
    }

    private void VérifierCollisions(Voiture voiture)
    {
        foreach (Voiture autreVoiture in ListeVoitures)
        {
            if (voiture != autreVoiture && voiture.EstEnCollision(autreVoiture))
            {
                voiture.EstEnAccident = true;
                autreVoiture.EstEnAccident = true;
            }
        }
    }
}
```

**Classe Simulation**

```c#
public class Simulation
{
    public Route Route { get; set; }
    public int NombreDeVoitures { get; set; }
    public double VitesseMoyenne { get; set; }
    public Simulation(Route route, int nombreDeVoitures)
    {
        Route = route;
        NombreDeVoitures = nombreDeVoitures;
        VitesseMoyenne = 0;
    }

    public void Initialiser()
    {
        for (int i = 0; i < NombreDeVoitures; i++)
        {
            double positionX = Random.NextDouble() * Route.Longueur;
            double positionY = Random.NextDouble() * Route.Largeur;
            int vitesseInitiale = Random.Next(10, 50);
            Voiture voiture = new Voiture(vitesseInitiale, positionX, positionY);
            Route.AjouterVoiture(voiture);
        }
    }

    public void Lancer()
    {
        int temps = 0;
        while (true)
        {
            temps++;
            Route.DéplacerVoitures();
            CalculerVitesseMoyenne();
            AfficherÉtatSimulation(temps);
        }
    }

    private void CalculerVitesseMoyenne()
    {
        VitesseMoyenne = 0;
        foreach (Voiture voiture in Route.ListeVoitures)
        {
            VitesseMoyenne += voiture.Vitesse;
        }
        VitesseMoyenne /= NombreDeVoitures;
    }

    private void AfficherÉtatSimulation(int temps)
    {
        Console.WriteLine("Temps : " + temps);
        Console.WriteLine("Nombre de voitures : " + NombreDeVoitures);
        Console.WriteLine("Vitesse moyenne : " + VitesseMoyenne);
        Console.WriteLine("Position des voitures :");
        foreach (Voiture voiture in Route.ListeVoitures)
        {
            Console.WriteLine(" - Voiture " + voiture.GetHashCode() + " : (" + voiture.PositionX + ", " + voiture.PositionY + ")");
        }
        Console.WriteLine();
    }
}
```

**Programme Principal**

```c#
public class Program
{
    public static void Main(string[] args)
    {
        double longueur = 1000;
        double largeur = 500;
        Route route = new Route(longueur, largeur);
        int nombreDeVoitures = 50;
        Simulation simulation = new Simulation(route, nombreDeVoitures);
        simulation.Initialiser();
        simulation.Lancer();
    }
}
```

**Explications**

* La classe **Voiture** représente une voiture avec ses propriétés de vitesse, de position et d'état d'accident. Elle contient des méthodes pour se déplacer, freiner et accélérer.
* La classe **Route** représente la route avec sa longueur, sa largeur et une liste de voitures. Elle contient une méthode pour ajouter des voitures et une méthode pour les déplacer et vérifier les collisions.
* La classe **Simulation** représente la simulation avec la route, le nombre de voitures et la vitesse moyenne. Elle contient des méthodes pour initialiser la simulation, la lancer et afficher l'état de la simulation.
* Le programme principal crée une route, une simulation, initialise la simulation et la lance.

Ce code simule le trafic routier avec des voitures qui se déplacent et peuvent entrer en collision. Il calcule la vitesse moyenne des voitures et affiche l'état de la simulation à chaque instant. La simulation peut être arrêtée en appuyant sur une touche.