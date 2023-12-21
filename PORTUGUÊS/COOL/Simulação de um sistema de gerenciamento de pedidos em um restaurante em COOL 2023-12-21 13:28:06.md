Claro! Aqui está um código em COOL que realiza uma simulação de um sistema de gerenciamento de pedidos em um restaurante. O código é dividido em várias classes para melhor organização e manutenção do sistema.

```cool
class Item {
 	attribute name : String;
 	attribute price : Float;
 
 	method init(n : String, p : Float) : SELF_TYPE {
 		{
 			name <- n;
 			price <- p;
 			self;
 		}
 	};
 
 	method getName() : String {
 		name;
 	};
 
 	method getPrice() : Float {
 		price;
 	};
 };
 
 class Order {
 	attribute items : List<Item>;
 
 	method init() : SELF_TYPE {
 		{
 			items <- nil;
 			self;
 		}
 	};
 
 	method addItem(item : Item) : SELF_TYPE {
 		{
 			items <- item :: items;
 			self;
 		}
 	};
 
 	method calculateTotal() : Float {
 		let total : Float <- 0.0 in
 		{
 			for i in items loop
 				total <- total + i.getPrice()
 			pool;
 			total;
 		}
 	};
 };
 
 class Restaurant {
 	attribute orders : List<Order>;
 
 	method init() : SELF_TYPE {
 		{
 			orders <- nil;
 			self;
 		}
 	};
 
 	method placeOrder(order : Order) : SELF_TYPE {
 		{
 			orders <- order :: orders;
 			self;
 		}
 	};
 
 	method printOrders() : Object {
 		let count : Int <- 1 in
 		{
 			for o in orders loop
 				out_string("Pedido ");
 				out_int(count);
 				out_string(": ");
 				out_float(o.calculateTotal());
 				out_string("\n");
 				count <- count + 1
 			pool
 		}
 	};
 };
 
 class Main {
 	main() : Object {
 		let restaurant : Restaurant <- Restaurant.init() in
 		{
 			let order1 : Order <- Order.init() in
 			{
 				let item1 : Item <- Item.init("Hamburguer", 10.0) in
 				{
 					order1.addItem(item1);
 					let item2 : Item <- Item.init("Batata Frita", 5.0) in
 					{
 						order1.addItem(item2);
 						restaurant.placeOrder(order1)
 					}
 				}
 			}
 		};
 
 		let order2 : Order <- Order.init() in
 		{
 			let item3 : Item <- Item.init("Refrigerante", 3.0) in
 			{
 				order2.addItem(item3);
 				restaurant.placeOrder(order2)
 			}
 		};
 
 		restaurant.printOrders();
 	};
 };
```

Neste código, temos três classes principais: `Item`, `Order` e `Restaurant`. A classe `Item` representa um item no cardápio do restaurante, com atributos como nome e preço. A classe `Order` representa um pedido, contendo uma lista de itens. A classe `Restaurant` é responsável por gerenciar os pedidos feitos no restaurante.

A classe `Main` é onde o programa principal é executado. Aqui, criamos um objeto `Restaurant`, adicionamos itens ao pedido `order1` e ao pedido `order2`, e então imprimimos a lista de pedidos e seus totais usando o método `printOrders()`.

Espero que este código atenda às suas expectativas!