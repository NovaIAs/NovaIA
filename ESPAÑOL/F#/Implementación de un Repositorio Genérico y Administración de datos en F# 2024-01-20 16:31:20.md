```f#
open System
open System.Collections.Generic
open System.Linq

// Definición de una interfaz genérica para un repositorio
interface IRepository<'a> =
    member this.GetAll() : IEnumerable<'a>
    member this.GetById(id: int) : 'a option
    member this.Add(item: 'a)
    member this.Update(item: 'a)
    member this.Delete(id: int)

//Definición de una clase genérica para un repositorio in-memory
class InMemoryRepository<'a>(private initialState: IEnumerable<'a>) : IRepository<'a> =
    let mutable items = initialState.ToList() //Mutable list para almacenar los datos

    // Método para obtener todos los elementos del repositorio
    member this.GetAll() = items

    //Método para obtener un elemento del repositorio por su id
    member this.GetById(id) = items.FirstOrDefault(fun x -> x.Id = id)

    //Método para agregar un elemento al repositorio
    member this.Add(item) =
        items.Add(item)

    //Método para actualizar un elemento del repositorio
    member this.Update(item) =
        let index = items.IndexOf(item)
        if index >= 0 then
            items[index] <- item

    //Método para eliminar un elemento del repositorio
    member this.Delete(id) =
        let index = items.IndexOf(fun x -> x.Id = id)
        if index >= 0 then
            items.RemoveAt(index)

//Definición de una clase de modelo para un producto
class Product =
    member x.Id = 0
    member x.Name = ""
    member x.Price = 0.0

//Definición de una clase de modelo para un pedido
class Order =
    member x.Id = 0
    member x.CustomerId = 0
    member x.OrderDate = DateTime.Now
    member x.Products = [] //Lista de productos en el pedido

//Crear un repositorio in-memory para productos
let productRepository = new InMemoryRepository<Product>([
    new Product(Id = 1, Name = "Camiseta", Price = 10.0),
    new Product(Id = 2, Name = "Pantalón", Price = 20.0),
    new Product(Id = 3, Name = "Zapatos", Price = 30.0)
])

//Crear un repositorio in-memory para pedidos
let orderRepository = new InMemoryRepository<Order>([
    new Order(Id = 1, CustomerId = 1, OrderDate = DateTime.Now, Products = [productRepository.GetById(1), productRepository.GetById(2)]),
    new Order(Id = 2, CustomerId = 2, OrderDate = DateTime.Now, Products = [productRepository.GetById(3)])
])

//Obtener todos los productos
let allProducts = productRepository.GetAll()
printfn "Todos los productos:"
allProducts |> Seq.iter (fun p -> printfn "%d: %s (%f)" p.Id p.Name p.Price)

//Obtener un producto por su id
let product = productRepository.GetById(2)
printfn "Producto con id 2:"
printfn "%d: %s (%f)" product.Id product.Name product.Price

//Agregar un nuevo producto
let newProduct = new Product(Name = "Sombrero", Price = 15.0)
productRepository.Add(newProduct)
printfn "Nuevo producto agregado:"
printfn "%d: %s (%f)" newProduct.Id newProduct.Name newProduct.Price

println "Todos los productos después de agregar un nuevo producto:"
allProducts = productRepository.GetAll()
allProducts |> Seq.iter (fun p -> printfn "%d: %s (%f)" p.Id p.Name p.Price)

//Actualizar un producto
let updatedProduct = new Product(Id = 2, Name = "Pantalón (actualizado)", Price = 25.0)
productRepository.Update(updatedProduct)
printfn "Producto con id 2 actualizado:"
printfn "%d: %s (%f)" updatedProduct.Id updatedProduct.Name updatedProduct.Price

//Eliminar un producto
productRepository.Delete(3)
println "Todos los productos después de eliminar el producto con id 3:"
allProducts = productRepository.GetAll()
allProducts |> Seq.iter (fun p -> printfn "%d: %s (%f)" p.Id p.Name p.Price)

//Obtener todos los pedidos
let allOrders = orderRepository.GetAll()
printfn "Todos los pedidos:"
allOrders |> Seq.iter (fun o -> printfn "%d: Cliente %d, Fecha: %s" o.Id o.CustomerId o.OrderDate)

//Obtener un pedido por su id
let order = orderRepository.GetById(1)
printfn "Pedido con id 1:"
printfn "%d: Cliente %d, Fecha: %s" order.Id order.CustomerId order.OrderDate

//Agregar un nuevo pedido
let newOrder = new Order(CustomerId = 3, Products = [
    productRepository.GetById(1),
    productRepository.GetById(3)
])
orderRepository.Add(newOrder)
printfn "Nuevo pedido agregado:"
printfn "%d: Cliente %d, Fecha: %s" newOrder.Id newOrder.CustomerId newOrder.OrderDate

println "Todos los pedidos después de agregar un nuevo pedido:"
allOrders = orderRepository.GetAll()
allOrders |> Seq.iter (fun o -> printfn "%d: Cliente %d, Fecha: %s" o.Id o.CustomerId o.OrderDate)

//Actualizar un pedido
let updatedOrder = new Order(Id = 1, CustomerId = 3, Products = [
    productRepository.GetById(2),
    productRepository.GetById(3)
])
orderRepository.Update(updatedOrder)
printfn "Pedido con id 1 actualizado:"
printfn "%d: Cliente %d, Fecha: %s" updatedOrder.Id updatedOrder.CustomerId updatedOrder.OrderDate

//Eliminar un pedido
orderRepository.Delete(2)
println "Todos los pedidos después de eliminar el pedido con id 2:"
allOrders = orderRepository.GetAll()
allOrders |> Seq.iter (fun o -> printfn "%d: Cliente %d, Fecha: %s" o.Id o.CustomerId o.OrderDate)
```

Explicación del código:

* Se definen dos interfaces genéricas, `IRepository<'a>` y `IEntity`, que representan un repositorio genérico y una entidad genérica, respectivamente.
* Se define una clase genérica `InMemoryRepository<'a>` que implementa la interfaz `IRepository<'a>`. Esta clase representa un repositorio in-memory para almacenar entidades de tipo `'a`.
* Se definen dos clases de modelo, `Product` y `Order`, que representan los productos y los pedidos, respectivamente.
* Se crean dos repositorios in-memory, uno para productos y otro para pedidos.
* Se agregan algunos datos iniciales a los repositorios.
* Se muestran todos los productos y pedidos.
* Se agrega un nuevo producto y pedido.
* Se actualiza un producto y un pedido.
* Se elimina un producto y un pedido.
* Se muestran todos los productos y pedidos después de las operaciones anteriores.

Este código demuestra cómo utilizar un repositorio in-memory genérico para almacenar y administrar datos en una aplicación en F#.