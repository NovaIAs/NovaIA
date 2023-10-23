
Claro, posso criar um código complexo em C++ para você. Aqui está um exemplo de um programa que simula um sistema de reservas para restaurantes:
```c
#include <iostream>
#include <string>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

const int MAX_CAPACITY = 100; // capacidade máxima de cada restaurante
const int MAX_TABLES = 20; // número máximo de mesas em cada restaurante

struct Restaurant {
    int capacity; // capacidade máxima do restaurante
    int tables[MAX_TABLES]; // número de mesas ocupadas em cada mesa
    int availableTables[MAX_TABLES]; // número de mesas disponíveis em cada mesa
    int waitingList[MAX_CAPACITY]; // lista de espera para entrar no restaurante
    int waitingListSize; // número de pessoas na lista de espera
};

void reserveTable(Restaurant &restaurant, int table, int partySize) {
    if (restaurant.tables[table] < partySize) {
        restaurant.tables[table] = partySize;
        restaurant.availableTables[table]--;
        restaurant.waitingList[restaurant.waitingListSize++] = partySize;
    }
}

void releaseTable(Restaurant &restaurant, int table, int partySize) {
    restaurant.tables[table] -= partySize;
    restaurant.availableTables[table]++;
    restaurant.waitingList[restaurant.waitingListSize--] = partySize;
}

void serveWaitingList(Restaurant &restaurant) {
    int largestPartySize = 0;
    int tableToAssign = -1;
    for (int i = 0; i < restaurant.waitingListSize; i++) {
        int partySize = restaurant.waitingList[i];
        if (partySize > largestPartySize) {
            largestPartySize = partySize;
            tableToAssign = i;
        }
    }
    if (tableToAssign >= 0) {
        reserveTable(restaurant, tableToAssign, largestPartySize);
    }
}

int main() {
    vector<Restaurant> restaurants;
    // Adicione restaurantes ao vetor

    while (true) {
        // Esperar pela entrada de novos clientes
        int partySize;
        cin >> partySize;
        if (partySize == -1) {
            break;
        }
        // Atribuir clientes a um restaurante vazio
        for (int i = 0; i < restaurants.size(); i++) {
            if (restaurants[i].waitingListSize == 0) {
                reserveTable(restaurants[i], 0, partySize);
                break;
            }
        }
        // Servir a lista de espera em todos os restaurantes
        for (int i = 0; i < restaurants.size(); i++) {
            serveWaitingList(restaurants[i]);
        }
        // Liberar mesas quando as partes terminarem
        for (int i = 0; i < restaurants.size(); i++) {
            for (int j = 0; j < restaurants[i].tables.size(); j++) {
                if (restaurants[i].tables[j] > 0) {
                    releaseTable(restaurants[i], j, restaurants[i].tables[j]);
                }
            }
        }
    }

    // Mostrar o resultado final de cada restaurante
    for (int i = 0; i < restaurants.size(); i++) {
        cout << "Restaurante " << i+1 << ": ";
        cout << "Capacidade total: " << restaurants[i].capacity << endl;
        cout << "Mesa ocupadas: ";
        for (int j = 0; j < restaurants[i].tables.size(); j++) {
            cout << restaurants[i].tables[j] << " ";
        }
        cout << endl;
    }

    return 0;
}
```
Este programa simula um sistema de