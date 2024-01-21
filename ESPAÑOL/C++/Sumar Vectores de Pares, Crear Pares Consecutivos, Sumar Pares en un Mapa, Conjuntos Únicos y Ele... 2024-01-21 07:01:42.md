```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <cmath>
#include <cassert>

using namespace std;

// Definición de un tipo de dato personalizado: par de enteros
typedef pair<int, int> par;

// Definición de una función que recibe un vector de vectores de pares de enteros
// y devuelve un vector de enteros con la suma de los elementos de cada vector
// interno.
vector<int> sumarVectoresPares(vector<vector<par>> v) {
	// Se crea un vector de enteros para almacenar las sumas.
	vector<int> sumas;

	// Se recorre el vector de vectores de pares de enteros.
	for (vector<par> v_i : v) {
		// Se crea una variable para almacenar la suma del vector interno.
		int suma = 0;

		// Se recorre el vector interno de pares de enteros.
		for (par p_ij : v_i) {
			// Se suma el primer elemento del par a la variable suma.
			suma += p_ij.first;

			// Se suma el segundo elemento del par a la variable suma.
			suma += p_ij.second;
		}

		// Se añade la suma del vector interno al vector de sumas.
		sumas.push_back(suma);
	}

	// Se devuelve el vector de sumas.
	return sumas;
}

// Definición de una función que recibe un vector de enteros y devuelve un
// vector de pares de enteros con los pares consecutivos del vector original.
vector<par> paresConsecutivos(vector<int> v) {
	// Se crea un vector de pares de enteros para almacenar los pares
	// consecutivos.
	vector<par> paresConsecutivos;

	// Se recorre el vector de enteros.
	for (int i = 0; i < v.size() - 1; i++) {
		// Se crea un par con los dos elementos consecutivos del vector.
		par p_i = par(v[i], v[i + 1]);

		// Se añade el par al vector de pares consecutivos.
		paresConsecutivos.push_back(p_i);
	}

	// Se devuelve el vector de pares consecutivos.
	return paresConsecutivos;
}

// Definición de una función que recibe un vector de pares de enteros y devuelve
// un mapa con los pares de enteros como claves y la suma de sus elementos
// como valores.
map<par, int> sumarPares(vector<par> v) {
	// Se crea un mapa para almacenar los pares de enteros y sus sumas.
	map<par, int> sumaPares;

	// Se recorre el vector de pares de enteros.
	for (par p_i : v) {
		// Se añade el par al mapa con su suma como valor.
		sumaPares[p_i] = p_i.first + p_i.second;
	}

	// Se devuelve el mapa.
	return sumaPares;
}

// Definición de una función que recibe un vector de enteros y devuelve un
// conjunto con los elementos únicos del vector.
set<int> elementosUnicos(vector<int> v) {
	// Se crea un conjunto para almacenar los elementos únicos del vector.
	set<int> elementosUnicos;

	// Se recorre el vector.
	for (int e_i : v) {
		// Si el elemento no está en el conjunto, se añade.
		if (elementosUnicos.find(e_i) == elementosUnicos.end()) {
			elementosUnicos.insert(e_i);
		}
	}

	// Se devuelve el conjunto.
	return elementosUnicos;
}

// Definición de una función que recibe un vector de enteros y devuelve el
// elemento más grande del vector.
int elementoMaximo(vector<int> v) {
	// Se inicializa la variable elementoMaximo con el primer elemento del
	// vector.
	int elementoMaximo = v[0];

	// Se recorre el vector.