```
+----------------------------------------------------------------------------------------------------+
|                                     Use Case Diagram                                        |
+----------------------------------------------------------------------------------------------------+
|                                            Actors                                             |
|                                        +--------------+                                        |
|                                        | System User  |                                        |
|                                        +--------------+                                        |
|                                        | System Admin |                                        |
|                                        +--------------+                                        |
|                                                                                                  |
|                                         Use Cases                                          |
|                                                                                                  |
|                                        +----------------+                                        |
|                                        | Login          |                                        |
|                                        +----------------+                                        |
|                                        | Register        |                                        |
|                                        +----------------+                                        |
|                                        | Create Project  |                                        |
|                                        +----------------+                                        |
|                                        | Edit Project   |                                        |
|                                        +----------------+                                        |
|                                        | Delete Project  |                                        |
|                                        +----------------+                                        |
|                                                                                                  |
+----------------------------------------------------------------------------------------------------+
|                                      Class Diagram                                          |
+----------------------------------------------------------------------------------------------------+
|                                                                                                  |
|                                     +----------------+                                     |
|                                     | System User    |                                     |
|                                     +----------------+                                     |
|                                     | - id: int      |                                     |
|                                     | - username: str  |                                     |
|                                     | - password: str  |                                     |
|                                     | - email: str     |                                     |
|                                     +----------------+                                     |
|                                                                                                  |
|                                     +----------------+                                     |
|                                     | System Admin   |                                     |
|                                     +----------------+                                     |
|                                     | - id: int      |                                     |
|                                     | - username: str  |                                     |
|                                     | - password: str  |                                     |
|                                     | - email: str     |                                     |
|                                     +----------------+                                     |
|                                                                                                  |
|                                     +----------------+                                     |
|                                     | Project        |                                     |
|                                     +----------------+                                     |
|                                     | - id: int      |                                     |
|                                     | - name: str     |                                     |
|                                     | - description: str |                                     |
|                                     +----------------+                                     |
|                                                                                                  |
|                                     +--------------------+                                     |
|                                     | Project Task     |                                     |
|                                     +--------------------+                                     |
|                                     | - id: int         |                                     |
|                                     | - project_id: int  |                                     |
|                                     | - name: str        |                                     |
|                                     | - description: str  |                                     |
|                                     +--------------------+                                     |
|                                                                                                  |
+----------------------------------------------------------------------------------------------------+
|                                 Sequence Diagram: Login Use Case                                  |
+----------------------------------------------------------------------------------------------------+
|                                                                                                  |
|                                          System User                                           |
|                                                                                                  |
|                                      +-----------------------+                                     |
|                                      |                       |                                     |
|                                      | Login to the system    |                                     |
|                                      |                       |                                     |
|                                      +-----------------------+                                     |
|                                                                                                  |
|                                          System                                            |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Authenticate the user  |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System User                                           |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Display login result  |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
+----------------------------------------------------------------------------------------------------+
|                                  Activity Diagram: Create Project Use Case                                |
+----------------------------------------------------------------------------------------------------+
|                                                                                                  |
|                                          System User                                           |
|                                                                                                  |
|                                      +-----------------------+                                     |
|                                      |                       |                                     |
|                                      | Start                 |                                     |
|                                      |                       |                                     |
|                                      +-----------------------+                                     |
|                                                                                                  |
|                                          System                                            |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Create new project    |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System User                                           |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Enter project details |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System                                            |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Validate project data |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System User                                           |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Confirm project data  |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System                                            |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Save project data     |                                    |
|                                      |                       |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System                                            |
|                                                                                                  |
|                                      +------------------------+                                    |
|                                      |                       |                                    |
|                                      | Notify user about     |                                    |
|                                      | project creation     |                                    |
|                                      +------------------------+                                    |
|                                                                                                  |
|                                          System User                                           |
|                                                                                                  |
|                                      +-----------------------+                                     |
|                                      |                       |                                     |
|                                      | End                   |                                     |
|                                      |                       |                                     |
|                                      +-----------------------+                                     |
|                                                                                                  |
+----------------------------------------------------------------------------------------------------+
```

Explanation:

* Use Case Diagram: This diagram shows the actors and use cases of the system. The actors are the System User and the System Admin. The use cases are Login, Register, Create Project, Edit Project, and Delete Project.
* Class Diagram: This diagram shows the classes in the system and their relationships. The classes are System User, System Admin, Project, and Project Task.
* Sequence Diagram: This diagram shows the sequence of interactions between objects in the system. This diagram shows the sequence of interactions between the System User and the System for the Login use case.
* Activity Diagram: This diagram shows the flow of control in the system. This diagram shows the flow of control for the Create Project use case.