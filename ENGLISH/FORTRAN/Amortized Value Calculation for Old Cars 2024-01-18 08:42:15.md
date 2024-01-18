```fortran
program kalkulasyon_tani_eski_arabalar

implicit none

integer :: yil, yas
real :: fiyat, amortisman_yili, amortisman_toplam

print *, 'Arabanin yilini giriniz:'
read *, yil

yas = 2023 - yil
amortisman_yili = (yas + 1) / 2

fiyat = 100000

do i = 1, amortisman_yili

    amortisman_toplam = amortisman_toplam + fiyat / amortisman_yili

    fiyat = fiyat - amortisman_toplam

end do

print *, 'Arabanin amortisman edilmis fiyati:', fiyat

end program kalkulasyon_tani_eski_arabalar
```

This program calculates the amortized value of an old car based on its age and initial price. Here's a detailed explanation of the code:

1. **Implicit None Statement:**

   - `implicit none` ensures that all variables must be explicitly declared with their data types. This helps prevent errors caused by undeclared or incorrectly declared variables.

2. **Variable Declarations:**

   - `integer :: yil, yas` Declare integer variables `yil` (year) and `yas` (age).
   - `real :: fiyat, amortisman_yili, amortisman_toplam` Declare real (floating-point) variables `fiyat` (price), `amortisman_yili` (amortization years), and `amortisman_toplam` (total amortization).

3. **Input:**

   - `print *, 'Arabanin yilini giriniz:'` Displays a prompt asking the user to enter the car's year.
   - `read *, yil` Reads the user's input and stores it in the `yil` variable.

4. **Calculations:**

   - Calculate the age of the car: `yas = 2023 - yil`
   - Calculate the amortization years: `amortisman_yili = (yas + 1) / 2`
   - Initialize the initial price of the car: `fiyat = 100000`
   - Use a `Do` loop to calculate the amortized value of the car over the amortization years:
     - Calculate the amortization amount for each year: `amortisman_toplam = amortisman_toplam + fiyat / amortisman_yili`
     - Subtract the amortization amount from the current price: `fiyat = fiyat - amortisman_toplam`

5. **Output:**

   - `print *, 'Arabanin amortisman edilmis fiyati:', fiyat` Displays the amortized value of the car.

This program provides a detailed calculation of the amortized value of an old car based on its age and initial price.