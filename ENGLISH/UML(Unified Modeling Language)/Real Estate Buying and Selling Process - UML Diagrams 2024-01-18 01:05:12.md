```
+---------------------------------------------------------------------------------------+
|  Class Diagram:                                                                         |
|  ---------------------------------------------------------------------------------------|
|                                                                                       |
|  +----------------------------------------+      +----------------------------------------+  |
|  | Apartment                                |      | House                                  |  |
|  +----------------------------------------+      +----------------------------------------+  |
|  | - address: String                        |      | - address: String                        |  |
|  | - squareFootage: int                     |      | - squareFootage: int                     |  |
|  | - numberOfBedrooms: int                  |      | - numberOfBedrooms: int                  |  |
|  | - numberOfBathrooms: int                 |      | - numberOfBathrooms: int                 |  |
|  | - rent: double                           |      | - price: double                           |  |
|  | - utilities: double                      |      | - mortgage: double                        |  |
|  | - isAvailable: boolean                   |      | - isForSale: boolean                       |  |
|  +----------------------------------------+      +----------------------------------------+  |
|                                                                                       |
|  +-------------------------------------------------+     +---------------------------------------+ |
|  | RealEstateAgent                                 |---->| Property                                  | |
|  +-------------------------------------------------+     +---------------------------------------+ |
|  | - name: String                                  |     | - address: String                        | |
|  | - licenseNumber: String                           |     | - squareFootage: int                     | |
|  | - email: String                                 |     | - numberOfBedrooms: int                  | |
|  | - phoneNumber: String                           |     | - numberOfBathrooms: int                 | |
|  | - yearsOfExperience: int                        |     | - rent: double                           | |
|  | - isLicensed: boolean                           |     | - price: double                           | |
|  | - commissionRate: double                        |     | - isAvailable: boolean                   | |
|  +-------------------------------------------------+     | - isForSale: boolean                       | |
|                                                                                       |
|  +--------------------+     +---------------------------------------+ |
|  | Buyer               |     | Property                                  | |
|  +--------------------+     +---------------------------------------+ |
|  | - name: String         |     | - address: String                        | |
|  | - email: String        |     | - squareFootage: int                     | |
|  | - phoneNumber: String  |     | - numberOfBedrooms: int                  | |
|  | - budget: double       |     | - numberOfBathrooms: int                 | |
|  | - isPreApproved: boolean |     | - rent: double                           | |
|  +--------------------+     | - price: double                           | |
|                               |     | - isAvailable: boolean                   | |
|                               |     | - isForSale: boolean                       | |
|                               +---------------------------------------+ |
|                                                                                       |
+---------------------------------------------------------------------------------------+
|                                                                                       |
|  Sequence Diagram:                                                                      |
|  ---------------------------------------------------------------------------------------|
|                                                                                       |
|                                            +----------------------------------------+   |
|                                            | Buyer                                  |   |
|                                            +----------------------------------------+   |
|                                                                                       |
|  +------------------------+                +-----------------+     +-------------------+   |
|  | RealEstateAgent         |                | Property          |     | Bank              |   |
|  +------------------------+                +-----------------+     +-------------------+   |
|  | - findProperties()      |--------->       | createListing()   |     | approveLoan()     |   |
|  | - scheduleShowings()    |--------->       | updateListing()    |     | denyLoan()       |   |
|  | - negotiateOffer()      |--------->       | deleteListing()    |     |                  |   |
|  | - closeDeal()          |                +-----------------+     +-------------------+   |
|  +------------------------+                                                        |
|                                                                                       |
|  +--------------------------------+     +----------------------------------------+   |
|  | RealEstateAgent                 |     | House                                  |   |
|  +--------------------------------+     +----------------------------------------+   |
|  | - showProperty()                |--------->       | acceptOffer()       |     |                  |   |
|  | - receiveOffer()               |--------->       | rejectOffer()       |     |                  |   |
|  | - counterOffer()               |--------->       | counterOffer()       |     |                  |   |
|  +--------------------------------+     +----------------------------------------+   |
|                                                                                       |
|  +--------------------------------+     +----------------------------------------+   |
|  | RealEstateAgent                 |     | Apartment                              |   |
|  +--------------------------------+     +----------------------------------------+   |
|  | - showProperty()                |--------->       | acceptOffer()       |     |                  |   |
|  | - receiveOffer()               |--------->       | rejectOffer()       |     |                  |   |
|  | - counterOffer()               |--------->       | counterOffer()       |     |                  |   |
|  +--------------------------------+     +----------------------------------------+   |
|                                                                                       |
|  +--------------------------------+     +----------------------------------------+   |
|  | RealEstateAgent                 |     | Property                                  |   |
|  +--------------------------------+     +----------------------------------------+   |
|  | - closeDeal()                  |--------->       | transferOwnership()  |     |                  |   |
|  +--------------------------------+     +----------------------------------------+   |
|                                                                                       |
+---------------------------------------------------------------------------------------+
```

Explanation:

* The class diagram shows the different classes and their relationships in the system.
    * The `Apartment` and `House` classes represent different types of properties.
    * The `RealEstateAgent` class represents the real estate agent who is responsible for selling the properties.
    * The `Buyer` class represents the potential buyer of a property.
    * The `Property` class represents a generic property that can be either an apartment or a house.
* The sequence diagram shows the interactions between the different classes in the system.
    * The real estate agent starts by finding properties that match the buyer's criteria.
    * The real estate agent then schedules showings for the buyer to view the properties.
    * If the buyer is interested in a property, they will make an offer to the seller.
    * The seller can then accept the offer, reject the offer, or counteroffer.
    * If the offer is accepted, the real estate agent will close the deal and transfer ownership of the property to the buyer.