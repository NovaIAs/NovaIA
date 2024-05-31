**Programme d'optimisation topologique en D**

**Introduction**

Ce code implémente un programme d'optimisation topologique en utilisant l'algorithme de gradient basé sur la densité (Density Gradient-Based Optimization, DGB) pour concevoir des structures optimales avec des contraintes de volume. L'optimisation topologique est une technique de conception qui vise à déterminer la disposition optimale de la matière dans un domaine donné afin de minimiser une fonction objective (par exemple, la conformité ou l'énergie de contrainte) sous certaines contraintes.

**Code**

```d
import std.stdio, math, geometry;

immutable input {
    // Paramètres de conception
    real targetVolume = 0.5;     // Volume cible pour la structure optimisée
    real designVolume = 0.5;     // Volume initial de la structure
    real filterRadius = 0.5;      // Rayon du filtre de densité
    immutable pointf[2] designDomain = [[-1, -1], [1, 1]]; // Domaine de conception

    // Paramètres d'optimisation
    real volumeConstraint = 0.5; // Contrainte de volume
    real materialDensity = 1.0;  // Densité du matériau
    real youngsModulus = 1.0;    // Module de Young du matériau
    real poissonsRatio = 0.3;     // Coefficient de Poisson du matériau
    real loadMagnitude = 1.0;    // Ampleur de la charge appliquée

    // Paramètres de visualisation
    immutable pointf[2] visualisationDomain = [[-1, -1], [1, 1]]; // Domaine de visualisation
    immutable pointf[4] colorMap = [[0, 0, 0, 0.5], [0, 0, 1, 1], [0, 1, 0, 1], [1, 0, 0, 1], [0, 1, 1, 1]]; // Carte des couleurs pour visualiser la densité

    // Paramètres de calcul
    immutable real dx = (designDomain[1].x - designDomain[0].x) / (visualizationDomain[1].x - visualizationDomain[0].x);
    immutable real dy = (designDomain[1].y - designDomain[0].y) / (visualizationDomain[1].y - visualizationDomain[0].y);
}

// Représentation de la densité
immutable struct Density {
    []real density;

    Density(uint columns, uint rows) pure nothrow @safe @nogc {
        density = new[]real[columns * rows];
    }
}

// Opérateurs de densité
immutable struct DensityOp {

    // Calcul du gradient de la densité filtrée
    immutable Density filteredDensityGradient(Density density, real radius) @safe {
        immutable real radiusSq = radius * radius;
        immutable uint columns = density.density.length;
        immutable uint rows = columns / (designDomain[1].x - designDomain[0].x);
        immutable real dx = 1.0 / (dx * radius);
        immutable real dy = 1.0 / (dy * radius);

        immutable Density filtered = new Density(density.density.length);
        foreach (auto &cell; filtered.density) cell = 0;
        foreach (auto &cell; density.density) {
            const immutable int i = cell.index / rows;
            const immutable int j = cell.index % rows;
            foreach (int k in -radius..+radius by dx.int) {
                foreach (int l in -radius..+radius by dy.int) {
                    immutable int q = i + k;
                    immutable int r = j + l;
                    if (q >= 0 && q < rows && r >= 0 && r < columns) {
                        filtered[q * columns + r] += cell * math.exp(-(k * k + l * l) / radiusSq);
                    }
                }
            }
        }

        immutable Density gradient = new Density(density.density.length);
        foreach (auto &cell; gradient.density) cell = 0;
        foreach (auto &cell; filtered.density) {
            const immutable int i = cell.index / rows;
            const immutable int j = cell.index % rows;
            foreach (int k in -1..+1) {
                foreach (int l in -1..+1) {
                    immutable int q = i + k;
                    immutable int r = j + l;
                    if (q >= 0 && q < rows && r >= 0 && r < columns) {
                        gradient[q * columns + r] += (filtered[i * columns + j] - filtered[q * columns + r]) * math.sqrt(k * k + l * l) * math.exp(-(k * k + l * l) / radiusSq);
                    }
                }
            }
        }

        return gradient;
    }

    // Calcul de la sensibilité de volume
    immutable real volumeSensitivity(Density density) @safe {
        return 1.0;
    }

    // Calcul de la sensibilité de conformité
    immutable real complianceSensitivity(Density density, real modulus, real poisson) @safe {
        immutable real dx = 1.0 / (dx * radius);
        immutable real dy = 1.0 / (dy * radius);
        immutable uint columns = density.density.length;
        immutable uint rows = columns / (designDomain[1].x - designDomain[0].x);

        immutable real a = (1.0 - poisson) / modulus;
        immutable real b = (poisson) / (2 * modulus);

        immutable real[] stiffness = newreal[columns * rows];
        foreach (auto &cell; stiffness) cell = 0;
        foreach (auto &cell; density.density) {
            const immutable int i = cell.index / rows;
            const immutable int j = cell.index % rows;
            foreach (int k in -1..+1) {
                foreach (int l in -1..+1) {
                    immutable int q = i + k;
                    immutable int r = j + l;
                    if (q >= 0 && q < rows && r >= 0 && r < columns) {
                        stiffness[q * columns + r] += cell * math.sqrt(k * k + l * l) * math.exp(-(k * k + l * l) / radiusSq);
                    }
                }
            }
        }

        real compliance = 0;
        foreach (auto &cell; stiffness) {
            compliance += cell * cell * (a + b * cell * cell);
        }

        return compliance;
    }
}

// Résolveur d'optimisation
immutable struct Optimiser {
    immutable Density density;
    immutable DensityOp op;

    Optimiser(Density density, DensityOp op) pure nothrow @safe @nogc {
        this.density = density;
        this.op = op;
    }

    // Mise à jour de la densité
    void updateDensity(real stepSize) pure nothrow @safe {
        immutable uint columns = density.density.length;
        immutable uint rows = columns / (designDomain[1].x - designDomain[0].x);

        foreach (auto &cell; density.density) {
            const immutable int i = cell.index / rows;
            const immutable int j = cell.index % rows;
            const immutable int neigbours = 8;
            immutable real sum = [d; cell.index + 1] op.volumeSensitivity(density)
                + [d; cell.index - 1] op.volumeSensitivity(density)
                + [d; cell.index + rows] op.volumeSensitivity(density)
                + [d; cell.index - rows] op.volumeSensitivity(density)
                + [d; cell.index + columns] op.volumeSensitivity(density)
                + [d; cell.index - columns] op.volumeSensitivity(density)
                + [d; cell.index + rows + columns] op.volumeSensitivity(density)
                + [d; cell.index - rows + columns] op.volumeSensitivity(density);

            cell -= stepSize * (1 / neigbours * sum);
        }
    }
}

// Fonction de visualisation
immutable auto visualiseDensity(Density density) pure nothrow @safe {
    immutable uint columns = density.density.length;
    immutable uint rows = columns / (designDomain[1].x - designDomain[0].x);

    foreach (auto &cell; density.density) {
        immutable uint i = cell.index / rows;
        immutable uint j = cell.index % rows;
        immutable uint p = (cell.value * colorMap.length).floor;
        immutable auto &color = colorMap[p];
        writefln("%f %f %f %f", i * dx - 1, j * dy - 1, color[0], color[1]);
    }
}

// Fonction principale
void main