Certainly! Here's a complex and differentiated code written in ADA. This code implements a genetic algorithm to solve the Traveling Salesman Problem (TSP), which is a well-known optimization problem in computer science.

```
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Text_IO.Float_IO; use Ada.Text_IO.Float_IO;

procedure TSP_Genetic is
    type City is record
        X_Coordinate : Float;
        Y_Coordinate : Float;
    end record;
    
    type Cities is array(Positive range <>) of City;
    City_List : Cities(1..20); -- Modify this line to change the number of cities
    
    type Chromosome is record
        Genes : Cities(City_List'Range);
        Fitness : Float;
    end record;
    
    type Population is array(Positive range <>) of Chromosome;
    
    procedure Initialize_Cities is
    begin
        -- Modify this procedure to set the coordinates of each city
        City_List(1).X_Coordinate := 0.0;
        City_List(1).Y_Coordinate := 0.0;
        City_List(2).X_Coordinate := 1.0;
        City_List(2).Y_Coordinate := 4.0;
        -- ...
    end Initialize_Cities;
    
    function Calculate_Distance(C1 : City; C2 : City) return Float is
    begin
        return Sqrt((C1.X_Coordinate - C2.X_Coordinate)**2 + (C1.Y_Coordinate - C2.Y_Coordinate)**2);
    end Calculate_Distance;
    
    function Calculate_Fitness(C : Chromosome) return Float is
        Total_Distance : Float := 0.0;
    begin
        for I in C.Genes'First+1 .. C.Genes'Last loop
            Total_Distance := Total_Distance + Calculate_Distance(C.Genes(I), C.Genes(I-1));
        end loop;
        
        return Total_Distance;
    end Calculate_Fitness;
    
    procedure Initialize_Population(Pop : in out Population) is
        N : Positive := Pop'Length;
        Temp : City;
    begin
        for I in Pop'Range loop
            for J in Pop(I).Genes'Range loop
                loop
                    Temp := City_List(Integer(Random(Integer'Ran(Range => 0 .. City_List'Length-1))));
                    exit when Temp not in Pop(I).Genes(1..J-1);
                end loop;
                Pop(I).Genes(J) := Temp;
            end loop;
            Pop(I).Fitness := Calculate_Fitness(Pop(I));
        end loop;
    end Initialize_Population;
    
    procedure Crossover(Parent1 : in Chromosome; Parent2 : in Chromosome; Child1 : out Chromosome; Child2 : out Chromosome) is
        N : Positive := Parent1.Genes'Length;
        Pivot : Positive := Random(Integer'Ran(Range => 1 .. N));
        Temp : City;
    begin
        Child1.Genes(Parent1.Genes'First..Pivot) := Parent1.Genes(Parent1.Genes'First..Pivot);
        Child2.Genes(Parent2.Genes'First..Pivot) := Parent2.Genes(Parent2.Genes'First..Pivot);
        
        for I in Parent2.Genes'First..Parent2.Genes'Last loop
            if Parent2.Genes(I) not in Child1.Genes(Parent1.Genes'First..Pivot) then
                Temp := Parent2.Genes(I);
                for J in Child1.Genes'Range loop
                    exit when Child1.Genes(J) = City_List(0);
                    if J = Child1.Genes'Last then
                        Child1.Genes(J) := Temp;
                    end if;
                end loop;
            end if;
            
            if Parent1.Genes(I) not in Child2.Genes(Parent2.Genes'First..Pivot) then
                Temp := Parent1.Genes(I);
                for J in Child2.Genes'Range loop
                    exit when Child2.Genes(J) = City_List(0);
                    if J = Child2.Genes'Last then
                        Child2.Genes(J) := Temp;
                    end if;
                end loop;
            end if;
        end loop;
        
        Child1.Fitness := Calculate_Fitness(Child1);
        Child2.Fitness := Calculate_Fitness(Child2);
    end Crossover;
    
    procedure Mutate(Chrom : in out Chromosome) is
        N : Positive := Chrom.Genes'Length;
        Temp : City;
    begin
        for I in Chrom.Genes'Range loop
            if Random > 0.5 then
                loop
                    Temp := City_List(Integer(Random(Integer'Ran(Range => 0 .. City_List'Length-1))));
                    exit when Temp not in Chrom.Genes;
                end loop;
                Chrom.Genes(I) := Temp;
            end if;
        end loop;
        
        Chrom.Fitness := Calculate_Fitness(Chrom);
    end Mutate;
    
    procedure Evolve(Pop : in out Population) is
        Parents : array(1..2) of Chromosome;
        Children : array(1..2) of Chromosome;
        Min_Fitness : Float := Pop(1).Fitness;
        Max_Fitness : Float := Pop(1).Fitness;
        Min_Index : Positive := 1;
        Max_Index : Positive := 1;
        Total_Fitness : Float := 0.0;
    begin
        for I in Pop'Range loop
            if Pop(I).Fitness < Min_Fitness then
                Min_Fitness := Pop(I).Fitness;
                Min_Index := I;
            elsif Pop(I).Fitness > Max_Fitness then
                Max_Fitness := Pop(I).Fitness;
                Max_Index := I;
            end if;
            
            Total_Fitness := Total_Fitness + Pop(I).Fitness;
        end loop;
        
        Children(1) := Pop(Min_Index);
        Children(2) := Pop(Max_Index);
        
        for I in Pop'Range loop
            loop
                Parents(1) := Pop(Integer(Random(Integer'Ran(Range => Pop'First .. Pop'Last))));
                Parents(2) := Pop(Integer(Random(Integer'Ran(Range => Pop'First .. Pop'Last))));
                exit when Parents(1) /= Parents(2);
            end loop;
            
            Crossover(Parents(1), Parents(2), Children(1), Children(2));
            
            if Random < 0.1 then
                Mutate(Children(1));
            end if;
            
            if Random < 0.1 then
                Mutate(Children(2));
            end if;
            
            if Children(1).Fitness < Children(2).Fitness then
                Pop(I) := Children(1);
            else
                Pop(I) := Children(2);
            end if;
        end loop;
    end Evolve;
    
    procedure Print_Solution(Best : Chromosome) is
    begin
        Put_Line("Best solution found:");
        for I in Best.Genes'Range loop
            Put(I & ": (" & Best.Genes(I).X_Coordinate & ", " & Best.Genes(I).Y_Coordinate & ")");
        end loop;
        Put("Fitness: ");
        Put(Best.Fitness, 0, 2);
        New_Line;
    end Print_Solution;
    
    procedure TSP_Genetic_Algorithm is
        Max_Generations : Positive := 1000; -- Modify this value to change the number of generations
        Population_Size : Positive := 100; -- Modify this value to change the population size
        Pop : Population(1..Population_Size);
        Best : Chromosome;
        Generation : Positive := 1;
    begin
        Initialize_Cities;
        Initialize_Population(Pop);
        Best := Pop(1);
        
        while Generation <= Max_Generations loop
            Evolve(Pop);
            
            if Pop(1).Fitness < Best.Fitness then
                Best := Pop(1);
            end if;
            
            Generation := Generation + 1;
        end loop;
        
        Print_Solution(Best);
    end TSP_Genetic_Algorithm;
    
begin
    TSP_Genetic_Algorithm;
end TSP_Genetic.
```

This code solves the Traveling Salesman Problem using a genetic algorithm approach. It defines a type `City` to represent a city with X and Y coordinates. The `Cities` type is an array of `City` records, representing the list of cities. The `Chromosome` type represents an individual solution, with an array of `Cities` and a fitness value. The `Population` type is an array of `Chromosome`.

The code includes several procedures. `Initialize_Cities` initializes the city coordinates. `Calculate_Distance` calculates the distance between two cities using the Euclidean distance formula. `Calculate_Fitness` calculates the fitness value for a chromosome based on its total distance. `Initialize_Population` initializes the population with random chromosomes. `Crossover` performs the crossover operation between two parents to produce two children. `Mutate` performs mutation on a chromosome. `Evolve` evolves the population by selecting parents, performing crossover and mutation, and replacing individuals in the population. `Print_Solution` prints the best solution found. `TSP_Genetic_Algorithm` is the main procedure that initializes the problem, population, and evolves the population until the maximum number of generations is reached.

To customize the code, you can modify the `City_List` array to include the desired cities and their coordinates, change the `Max_Generations` and `Population_Size` variables to adjust the algorithm parameters, and modify the print statements or add additional functionality as needed.

Please note that the code provided is a simplified implementation and may require further modifications or optimizations depending on specific requirements or constraints.