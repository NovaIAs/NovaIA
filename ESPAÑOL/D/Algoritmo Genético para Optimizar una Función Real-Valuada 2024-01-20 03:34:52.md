```d
// Este código implementa un algoritmo genético para optimizar una función.

import std.algorithm, std.range, std.stdio, std.thread;

// Definimos la función que queremos optimizar.
double objectiveFunction(double x) {
    return -x * x + 10 * x - 20;
}

// Definimos la clase Individuo, que representa una posible solución al problema.
class Individuo {
    double[] cromosoma; // El cromosoma del individuo, que contiene los valores de las variables a optimizar.
    double fitness; // El valor de la función objetivo para este individuo.

    // Constructor.
    Individuo(double[] cromosoma) {
        this.cromosoma = cromosoma;
        this.fitness = objectiveFunction(cromosoma[0]);
    }

    // Método para imprimir el individuo.
    void print() {
        writefln("Individuo: [%f]", cromosoma[0]);
        writefln("Fitness: %f", fitness);
    }
}

// Definimos la clase Población, que representa un conjunto de individuos.
class Población {
    Individuo[] individuos; // Los individuos de la población.

    // Constructor.
    Población(Individuo[] individuos) {
        this.individuos = individuos;
    }

    // Método para imprimir la población.
    void print() {
        for (Individuo individuo in individuos) {
            individuo.print();
        }
    }
}

// Definimos la clase AlgoritmoGenético, que implementa el algoritmo genético.
class AlgoritmoGenético {
    Población población; // La población actual.
    int generaciones; // El número de generaciones a ejecutar.
    double tasaDeCruce; // La tasa de cruce de los individuos.
    double tasaDeMutación; // La tasa de mutación de los individuos.

    // Constructor.
    AlgoritmoGenético(Población población, int generaciones, double tasaDeCruce, double tasaDeMutación) {
        this.población = población;
        this.generaciones = generaciones;
        this.tasaDeCruce = tasaDeCruce;
        this.tasaDeMutación = tasaDeMutación;
    }

    // Método para ejecutar el algoritmo genético.
    void ejecutar() {
        for (int i = 0; i < generaciones; i++) {
            // Seleccionamos los padres.
            Individuo[] padres = seleccionarPadres();

            // Cruzamos los padres para crear nuevos individuos.
            Individuo[] hijos = cruzarPadres(padres);

            // Mutamos los hijos.
            mutarHijos(hijos);

            // Evaluamos los hijos.
            evaluarHijos(hijos);

            // Reemplazamos los individuos de la población con los hijos.
            reemplazarPoblación(hijos);

            // Imprimimos la mejor solución encontrada hasta ahora.
            Individuo mejorIndividuo = encontrarMejorIndividuo();
            writefln("Mejor individuo en la generación %d:", i);
            mejorIndividuo.print();
        }
    }

    // Método para seleccionar los padres.
    Individuo[] seleccionarPadres() {
        // Ordenamos la población por fitness en orden descendente.
        std.sort(población.individuos, [](Individuo a, Individuo b) { return a.fitness > b.fitness; });

        // Seleccionamos los mejores individuos como padres.
        Individuo[] padres = new Individuo[2];
        padres[0] = población.individuos[0];
        padres[1] = población.individuos[1];

        return padres;
    }

    // Método para cruzar los padres.
    Individuo[] cruzarPadres(Individuo[] padres) {
        // Creamos dos nuevos individuos.
        Individuo hijo1 = new Individuo(new double[1]);
        Individuo hijo2 = new Individuo(new double[1]);

        // Cruzamos los padres para crear los hijos.
        hijo1.cromosoma[0] = padres[0].cromosoma[0] * (1 - tasaDeCruce) + padres[1].cromosoma[0] * tasaDeCruce;
        hijo2.cromosoma[0] = padres[1].cromosoma[0] * (1 - tasaDeCruce) + padres[0].cromosoma[0] * tasaDeCruce;

        // Devolvemos los hijos.
        return new Individuo[]{hijo1, hijo2};
    }

    // Método para mutar los hijos.
    void mutarHijos(Individuo[] hijos) {
        for (Individuo hijo in hijos) {
            // Muta el cromosoma del hijo.
            hijo.cromosoma[0] += (Math.random() - 0.5) * tasaDeMutación;
        }
    }

    // Método para evaluar los hijos.
    void evaluarHijos(Individuo[] hijos) {
        for (Individuo hijo in hijos) {
            // Evaluamos el hijo.
            hijo.fitness = objectiveFunction(hijo.cromosoma[0]);
        }
    }

    // Método para reemplazar la población con los hijos