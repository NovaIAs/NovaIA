```
+----------------------------------------------------------------------+
|                           Warehouse Management System                |
+----------------------------------------------------------------------+

|                     +----------------+                     |
|                     |                |                     |
|  +-------------------|   Warehouse   |-------------------+  |
|  |                   |                |                   |  |
|  | Receiving Area    |----------------| Shipping Area    |  |
|  |                   |                |                   |  |
|  +-------------------|                |-------------------+  |
|                     |                |                     |
+---------------------+----------------+---------------------+

|                                                                      |
|                                 +------------+                                 |
|                                 | Warehouse |                                 |
|                                 +------------+                                 |
|                                   ^    ^                                   |
|                                   |    |                                   |
|                                   |    |                                   |
|                             +-----+-----+-----+                              |
|                             |     |     |     |                              |
|                             |     |     |     |                              |
|                             +-----+-----+-----+                              |
|                                   |    |                                   |
|                                   |    |                                   |
|                               ____V____                                   |
|                                                                      |
|                                +-------------+                                |
|                                | Manufacturer |                                |
|                                +-------------+                                |
|                                    ^                                    |
|                                    |                                    |
|                                    |                                    |
|                                +--------------+                                |
|                                | Transportation |                                |
|                                +--------------+                                |
|                                    ^                                    |
|                                    |                                    |
|                                    |                                    |
|                              +-------------+                              |
|                              | Distribution |                              |
|                              +-------------+                              |
|                                   /         \                                   |
|                                  /           \                                  |
|                                  /             \                                  |
|                          +---------+----++---------+                          |
|                          | Retailer |    || Customer |                          |
|                          +---------+----++---------+                          |
+----------------------------------------------------------------------+

|-----------------+-------------------------------------------------+
|      Interface     |                                                 |
|-----------------+-------------------------------------------------+
| IWarehouse       | Provides methods for managing the warehouse.        |
| IManufacturer    | Provides methods for managing manufacturers.       |
| ITransportation  | Provides methods for managing transportation.     |
| IDistribution     | Provides methods for managing distribution.       |
| IRetailer        | Provides methods for managing retailers.            |
| ICustomer        | Provides methods for managing customers.            |
+-----------------+-------------------------------------------------+

|-----------------+-------------------------------------------------+
|      Class      |                                                 |
|-----------------+-------------------------------------------------+
| Warehouse       | Implements the IWarehouse interface.                 |
| Manufacturer    | Implements the IManufacturer interface.             |
| Transportation  | Implements the ITransportation interface.         |
| Distribution     | Implements the IDistribution interface.            |
| Retailer        | Implements the IRetailer interface.                 |
| Customer        | Implements the ICustomer interface.                 |
+-----------------+-------------------------------------------------+

|-----------------+-------------------------------------------------+
|    Component    |                                                 |
|-----------------+-------------------------------------------------+
| WarehouseSystem | Contains the Warehouse, Manufacturer, Transportation, |
|                | Distribution, Retailer, and Customer classes.          |
+-----------------+-------------------------------------------------+

|-----------------+-------------------------------------------------+
|       UseCase    |                                                 |
|-----------------+-------------------------------------------------+
| Receive Goods   | The process of receiving goods from a manufacturer.  |
| Ship Goods      | The process of shipping goods to a retailer or customer.|
| Manage Inventory | The process of managing inventory levels in the warehouse.|
| Track Shipments | The process of tracking the status of shipments.         |
| Place Order      | The process of placing an order for goods from a retailer. |
| Fulfill Order     | The process of fulfilling an order for goods from a customer.|
+-----------------+-------------------------------------------------+

|-----------------+-------------------------------------------------+
|     Sequence Diagram    |                                                 |
|-----------------+-------------------------------------------------+
| Receive Goods   | Manufacturer -> Warehouse : Send Goods              |
|                  | Warehouse   -> Manufacturer : Acknowledge Receipt  |
|                  |                                                 |
| Ship Goods      | Retailer   -> Warehouse : Place Order              |
|                  | Warehouse   -> Manufacturer : Retrieve Goods         |
|                  | Manufacturer-> Warehouse : Send Goods              |
|                  | Warehouse   -> Retailer   : Acknowledge Shipment    |
|                  |                                                 |
| Manage Inventory | Warehouse   -> Manufacturer : Request Inventory    |
|                  | Manufacturer-> Warehouse : Send Inventory          |
|                  |                                                 |
| Track Shipments | Retailer   -> Warehouse : Get Shipment Status       |
|                  | Warehouse   -> Transportation : Get Shipment Status |
|                  | Transportation -> Warehouse : Send Shipment Status |
|                  |                                                 |
| Place Order      | Customer   -> Retailer : Place Order               |
|                  | Retailer   -> Warehouse : Retrieve Goods           |
|                  | Warehouse   -> Manufacturer : Send Goods              |
|                  | Manufacturer-> Warehouse : Acknowledge Receipt    |
|                  | Warehouse   -> Retailer : Send Goods                |
|                  | Retailer   -> Customer : Deliver Goods              |
|                  |                                                 |
| Fulfill Order     | Customer   -> Retailer : Place Order               |
|                  | Retailer   -> Warehouse : Retrieve Goods           |
|                  | Warehouse   -> Manufacturer : Send Goods              |
|                  | Manufacturer-> Warehouse : Acknowledge Receipt    |
|                  | Warehouse   -> Retailer : Send Goods                |
|                  | Retailer   -> Customer : Deliver Goods              |
+-----------------+-------------------------------------------------+