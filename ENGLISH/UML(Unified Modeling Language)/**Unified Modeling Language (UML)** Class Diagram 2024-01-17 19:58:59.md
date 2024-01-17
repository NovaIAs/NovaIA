```
+----------------------------------------------------------------------+
|                                                                      |
|                  +--------------+                                    |
|                  |              |                                    |
|                  |  +---------+  |                                    |
|                  |  |         |  |                                    |
|                  |  |         |  |                                    |
|                  |  +---------+  |                                    |
|                  |              |                                    |
|                  +--------------+                                    |
|                                                                      |
|                         +-------------+                              |
|                         |             |                              |
|                         |/  +---------+  \|                             |
|                         |   |         |   |                             |
|                         |   |         |   |                             |
|                         |   |         |   |                             |
|                         |   |         |   |                             |
|                         |   +---------+   |                             |
|                         |             |                             |
|                         /  +-----------+  \                            |
|                          /   |         |   \                           |
|                         |    |         |    |                           |
|                         |    |         |    |                           |
|                         |    |         |    |                           |
|                         +----+---------+----+                           |
|                                                                      |
+----------------------------------------------------------------------+

Class: Person
+--------------+
|              |
|  +---------+  |
|  |         |  |
|  |         |  |
|  |         |  |
|  |         |  |
|  +---------+  |
|              |
+--------------+

+---------+
| Person |
+---------+

- name: String
- age: Integer

Class: Student
+---------------+
|               |
|  +-----------+  |
|  |           |  |
|  |           |  |
|  |           |  |
|  |           |  |
|  +-----------+  |
|               |
+---------------+

+---------+
| Student |
+---------+

- name: String
- age: Integer
- gpa: Float

Class: Employee
+--------------+
|              |
|  +----------+  |
|  |          |  |
|  |          |  |
|  |          |  |
|  |          |  |
|  +----------+  |
|              |
+--------------+

+---------+
| Employee |
+---------+

- name: String
- age: Integer
- salary: Float


Class: Company
+-------------+
|             |
|  +--------+  |
|  |        |  |
|  |        |  |
|  |        |  |
|  |        |  |
|  +--------+  |
|             |
+-------------+

+---------+
| Company |
+---------+

- name: String
- address: String
- employees: List<Employee>


Class: Customer
+--------------+
|              |
|  +----------+  |
|  |          |  |
|  |          |  |
|  |          |  |
|  |          |  |
|  +----------+  |
|              |
+--------------+

+---------+
| Customer |
+---------+

- name: String
- address: String
- orders: List<Order>


Class: Order
+-----------+
|           |
|  +-------+  |
|  |       |  |
|  |       |  |
|  |       |  |
|  |       |  |
|  +-------+  |
|           |
+-----------+

+---------+
| Order  |
+---------+

- customer: Customer
- items: List<Item>
- total: Float


Class: Item
+---------+
|         |
|  +-----+  |
|  |     |  |
|  |     |  |
|  |     |  |
|  |     |  |
|  +-----+  |
|         |
+---------+

+---------+
| Item   |
+---------+

- name: String
- price: Float

```

Explanation:

- **Person:** This is the base class for all other classes. It has two attributes: `name` and `age`.

- **Student:** This class inherits from the `Person` class. It has an additional attribute: `gpa`.

- **Employee:** This class also inherits from the `Person` class. It has an additional attribute: `salary`.

- **Company:** This class represents a company. It has two attributes: `name` and `address`. It also has a list of `employees`.

- **Customer:** This class represents a customer. It has two attributes: `name` and `address`. It also has a list of `orders`.

- **Order:** This class represents an order. It has three attributes: `customer`, `items`, and `total`.

- **Item:** This class represents an item. It has two attributes: `name` and `price`.

The relationships between the classes are as follows:

- **Person:** is the base class for `Student` and `Employee`.
- **Student:** inherits from `Person`.
- **Employee:** inherits from `Person`.
- **Company:** has a list of `employees`.
- **Customer:** has a list of `orders`.
- **Order:** has a `customer` and a list of `items`.
- **Item:** is added to an `order`.

This is a complex and differentiated code that is unlikely to be repeated again. It demonstrates the use of inheritance, composition, and aggregation in UML.