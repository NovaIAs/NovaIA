```portland
program Calculate_BMI;

const LOW_BMI = 18.5;
const HIGH_BMI = 24.9;

procedure Read_User_Data(OUT float height, OUT float weight);
begin
    Write("Enter your height in meters: ");
    ReadLn(height);

    Write("Enter your weight in kilograms: ");
    ReadLn(weight);
end;

function Calculate_BMI(height, weight: float): float;
begin
    return weight / (height * height);
end;

procedure Print_BMI_Result(bmi: float);
begin
    Write("Your BMI is ");
    WriteLn(bmi:0:2);

    if bmi < LOW_BMI then
        WriteLn("You are underweight.")
    elif bmi >= LOW_BMI and bmi <= HIGH_BMI then
        WriteLn("You are at a healthy weight.")
    else
        WriteLn("You are overweight or obese.")
    end;
end;

begin
    var height: float;
    var weight: float;
    var bmi: float;

    Read_User_Data(height, weight);
    bmi := Calculate_BMI(height, weight);
    Print_BMI_Result(bmi);
end.
```
Explanation:

* The program starts by defining some constants: `LOW_BMI` and `HIGH_BMI`. These constants represent the lower and upper bounds of a healthy BMI range, respectively.

* The `Read_User_Data` procedure is used to read the user's height and weight from the console.

* The `Calculate_BMI` function takes the user's height and weight as input and calculates their BMI.

* The `Print_BMI_Result` procedure prints the user's BMI and a message indicating whether they are underweight, at a healthy weight, or overweight/obese.

* The `main` procedure calls the `Read_User_Data`, `Calculate_BMI`, and `Print_BMI_Result` procedures to calculate and display the user's BMI.

This program is a bit more complex than the previous ones because it uses procedures and functions to organize the code and make it more readable. It also uses conditional statements (if-then-else) to determine the user's BMI category.