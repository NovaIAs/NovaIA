```c++
// Este código es un programa en C++ que implementa un sistema de gestión de pedidos para un restaurante.

#include <iostream>
#include <vector>

using namespace std;

// Clase Pedido
class Pedido {
public:
  // Constructor
  Pedido(string nombre, string plato, int cantidad) {
    this->nombre = nombre;
    this->plato = plato;
    this->cantidad = cantidad;
  }

  // Métodos get y set
  string getNombre() {
    return nombre;
  }

  void setNombre(string nombre) {
    this->nombre = nombre;
  }

  string getPlato() {
    return plato;
  }

  void setPlato(string plato) {
    this->plato = plato;
  }

  int getCantidad() {
    return cantidad;
  }

  void setCantidad(int cantidad) {
    this->cantidad = cantidad;
  }

private:
  string nombre;
  string plato;
  int cantidad;
};

// Clase Restaurante
class Restaurante {
public:
  // Constructor
  Restaurante(string nombre) {
    this->nombre = nombre;
    this->pedidos = vector<Pedido>();
  }

  // Métodos get y set
  string getNombre() {
    return nombre;
  }

  void setNombre(string nombre) {
    this->nombre = nombre;
  }

  vector<Pedido> getPedidos() {
    return pedidos;
  }

  void setPedidos(vector<Pedido> pedidos) {
    this->pedidos = pedidos;
  }

  // Método para añadir un pedido al restaurante
  void addPedido(Pedido pedido) {
    pedidos.push_back(pedido);
  }

  // Método para eliminar un pedido del restaurante
  void removePedido(int index) {
    pedidos.erase(pedidos.begin() + index);
  }

  // Método para mostrar los pedidos del restaurante
  void showPedidos() {
    for (int i = 0; i < pedidos.size(); i++) {
      cout << "Pedido " << i + 1 << ":" << endl;
      cout << "Nombre: " << pedidos[i].getNombre() << endl;
      cout << "Plato: " << pedidos[i].getPlato() << endl;
      cout << "Cantidad: " << pedidos[i].getCantidad() << endl;
      cout << endl;
    }
  }

private:
  string nombre;
  vector<Pedido> pedidos;
};

// Función principal
int main() {
  // Crear un nuevo restaurante
  Restaurante restaurante("El Rincón de los Sabores");

  // Añadir algunos pedidos al restaurante
  restaurante.addPedido(Pedido("Juan", "Pizza", 1));
  restaurante.addPedido(Pedido("María", "Pasta", 2));
  restaurante.addPedido(Pedido("Luis", "Ensalada", 1));

  // Mostrar los pedidos del restaurante
  cout << "Pedidos del restaurante:" << endl;
  restaurante.showPedidos();

  // Eliminar un pedido del restaurante
  restaurante.removePedido(1);

  // Mostrar los pedidos del restaurante después de eliminar uno
  cout << endl;
  cout << "Pedidos del restaurante después de eliminar uno:" << endl;
  restaurante.showPedidos();

  return 0;
}
```

Explicación del código:

* La clase Pedido representa un pedido de un cliente. Tiene tres atributos: el nombre del cliente, el plato que ha pedido y la cantidad que ha pedido.
* La clase Restaurante representa un restaurante. Tiene dos atributos: el nombre del restaurante y una lista de pedidos.
* La función main crea un nuevo restaurante, añade algunos pedidos a ese restaurante y luego muestra los pedidos del restaurante.
* También elimina un pedido del restaurante y muestra los pedidos del restaurante después de eliminar uno.