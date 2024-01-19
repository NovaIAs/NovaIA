```fortran
PROGRAM MAIN
! This program demonstrates the use of derived data types, modules, and procedures.

! Declare the derived data type 'PERSON'.
TYPE PERSON
  CHARACTER(len=20) :: NAME
  INTEGER :: AGE
  REAL :: WEIGHT
END TYPE

! Declare the module 'PERSON_MODULE'.
MODULE PERSON_MODULE
  ! Declare the derived data type 'PERSON'.
  TYPE, PUBLIC :: PERSON
    CHARACTER(len=20) :: NAME
    INTEGER :: AGE
    REAL :: WEIGHT
  END TYPE

! Declare a procedure to print a person's information.
PROCEDURE, PUBLIC :: PRINT_PERSON
  ! Declare the input argument 'PERSON'.
  CLASS(PERSON) :: PERSON

! Print the person's information.
WRITE(*,*) 'Name:', PERSON%NAME
WRITE(*,*) 'Age:', PERSON%AGE
WRITE(*,*) 'Weight:', PERSON%WEIGHT
END PROCEDURE PRINT_PERSON

END MODULE PERSON_MODULE

! Declare the main program.
PROGRAM MAIN

! Declare the variable 'P' of type 'PERSON'.
CLASS(PERSON) :: P

! Initialize the variable 'P'.
P%NAME = 'John Doe'
P%AGE = 30
P%WEIGHT = 180.0

! Call the procedure 'PRINT_PERSON'.
CALL PRINT_PERSON(P)

END PROGRAM MAIN
```

This code demonstrates the use of derived data types, modules, and procedures in Fortran.

1. The derived data type 'PERSON' is declared. This data type represents a person with three attributes: 'NAME', 'AGE', and 'WEIGHT'.

2. The module 'PERSON_MODULE' is declared. This module contains the declaration of the derived data type 'PERSON' and a procedure to print a person's information.

3. The procedure 'PRINT_PERSON' is declared. This procedure takes a person's information as input and prints it to the console.

4. The main program is declared.

5. The variable 'P' of type 'PERSON' is declared.

6. The variable 'P' is initialized with the values 'John Doe', 30, and 180.0 for the attributes 'NAME', 'AGE', and 'WEIGHT', respectively.

7. The procedure 'PRINT_PERSON' is called with the variable 'P' as an argument. This causes the person's information to be printed to the console.