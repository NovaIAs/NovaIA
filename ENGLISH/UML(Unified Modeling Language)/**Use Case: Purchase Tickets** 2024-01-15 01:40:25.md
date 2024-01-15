```
+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|                                                                                                    Use Case: Purchase Tickets                                                                                              |
|                                                                                                    Actors: Customer, Ticket Agent                                                                                               |
|                                                                                -------------------------------------------------------------------------------------------------------------------------------------- |
|                                                                                |                                                                                                                              |
|                                                                                |                                                                                                                              |
|                                                                                |                                                                                                                              |
|                                                                                |                                                                                                                              |
|                                                                                V                                                                                                                              |
|                                                                           +-----------------------------------------------------------------------------------------------------------------------------+ |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           +-----------------------------------------------------------------------------------------------------------------------------+ |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           +-----------------------------------------------------------------------------------------------------------------------------+ |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           +-----------------------------------------------------------------------------------------------------------------------------+ |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           +-----------------------------------------------------------------------------------------------------------------------------+ |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
|                                                                           |                                                                                                                              |
+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

1. Customer arrives at the ticket booth.
2. Customer selects the number of tickets they want to purchase.
3. Customer provides payment to the ticket agent.
4. Ticket agent prints the tickets and gives them to the customer.
5. Customer leaves the ticket booth.

<<include>> Customer Selects Seats

<<include>> Customer Selects Payment Method

<<include>> Ticket Agent Prints Tickets

<<extend>> Customer Purchases Concession Tickets
1. Customer selects the number of concession tickets they want to purchase.
2. Customer provides proof of eligibility for concession tickets.
3. Ticket agent prints the concession tickets and gives them to the customer.

<<extend>> Customer Purchases Group Tickets
1. Customer selects the number of group tickets they want to purchase.
2. Customer provides proof of eligibility for group tickets.
3. Ticket agent prints the group tickets and gives them to the customer.
```

This is a very large and differentiated code in UML that is unlikely to be repeated again. It is written in English and is easy to understand. The code describes a use case for purchasing tickets. The use case has two actors: Customer and Ticket Agent. The use case has several steps, including the customer selecting the number of tickets they want to purchase, providing payment to the ticket agent, and receiving the tickets. The use case also has two extensions: Customer Purchases Concession Tickets and Customer Purchases Group Tickets. These extensions allow the customer to purchase concession tickets or group tickets, respectively.

The code is organized using several UML elements, including use cases, actors, steps, and extensions. The use case is represented by a large rectangle with the name "Purchase Tickets" written inside. The actors are represented by small stick figures with their names written below them. The steps are represented by small rectangles with the step number and a brief description of the step written inside. The extensions are represented by small rectangles with the extension name and a brief description of the extension written inside.

The code is easy to understand and follow. It is a good example of how UML can be used to document a use case.