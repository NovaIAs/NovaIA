```c#
//Librerías necesarias para el funcionamento del código.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

//Se definen 3 clases: Empleados, Contratos y Nóminas, así como sus respectivos atributos y métodos.

//Clase Empleados
public class Empleados
{
    //Atributos de la clase Empleados
    private string nombre;
    private string apellido;
    private string dirección;
    private string teléfono;
    private string email;
    private string fecha_nacimiento;
    private string fecha_contratación;
    private string puesto;
    private double salario;

    //Constructor de la clase Empleados
    public Empleados(string nombre, string apellido, string dirección, string teléfono, string email, string fecha_nacimiento, string fecha_contratación, string puesto, double salario)
    {
        this.nombre = nombre;
        this.apellido = apellido;
        this.dirección = dirección;
        this.teléfono = teléfono;
        this.email = email;
        this.fecha_nacimiento = fecha_nacimiento;
        this.fecha_contratación = fecha_contratación;
        this.puesto = puesto;
        this.salario = salario;
    }

    //Getter's y Setter's
    public string Nombre { get => nombre; set => nombre = value; }
    public string Apellido { get => apellido; set => apellido = value; }
    public string Dirección { get => dirección; set => dirección = value; }
    public string Teléfono { get => teléfono; set => teléfono = value; }
    public string Email { get => email; set => email = value; }
    public string Fecha_nacimiento { get => fecha_nacimiento; set => fecha_nacimiento = value; }
    public string Fecha_contratación { get => fecha_contratación; set => fecha_contratación = value; }
    public string Puesto { get => puesto; set => puesto = value; }
    public double Salario { get => salario; set => salario = value; }
}

//Clase Contratos
public class Contratos
{
    //Atributos de la clase Contratos
    private int id_contrato;
    private string tipo_contrato;
    private string fecha_inicio;
    private string fecha_fin;
    private double salario_bruto;
    private double salario_neto;
    private Empleados empleado;

    //Constructor de la clase Contratos
    public Contratos(int id_contrato, string tipo_contrato, string fecha_inicio, string fecha_fin, double salario_bruto, double salario_neto, Empleados empleado)
    {
        this.id_contrato = id_contrato;
        this.tipo_contrato = tipo_contrato;
        this.fecha_inicio = fecha_inicio;
        this.fecha_fin = fecha_fin;
        this.salario_bruto = salario_bruto;
        this.salario_neto = salario_neto;
        this.empleado = empleado;
    }

    //Getter's y Setter's
    public int Id_contrato { get => id_contrato; set => id_contrato = value; }
    public string Tipo_contrato { get => tipo_contrato; set => tipo_contrato = value; }
    public string Fecha_inicio { get => fecha_inicio; set => fecha_inicio = value; }
    public string Fecha_fin { get => fecha_fin; set => fecha_fin = value; }
    public double Salario_bruto { get => salario_bruto; set => salario_bruto = value; }
    public double Salario_neto { get => salario_neto; set => salario_neto = value; }
    public Empleados Empleado { get => empleado; set => empleado = value; }
}

//Clase Nóminas
public class Nóminas
{
    //Atributos de la clase Nóminas
    private int id_nómina;
    private string mes;
    private string año;
    private double salario_bruto;
    private double salario_neto;
    private double retenciones;
    private double cuotas;
    private Contratos contrato;

    //Constructor de la clase Nóminas
    public Nóminas(int id_nómina, string mes, string año, double salario_bruto, double salario_neto, double retenciones, double cuotas, Contratos contrato)
    {
        this.id_nómina = id_nómina;
        this.mes = mes;
        this.año = año;
        this.salario_bruto = salario_bruto;
        this.salario_neto = salario_neto;
        this.retenciones = retenciones;
        this.cuotas = cuotas;
        this.contrato = contrato;
    }

    //Getter's y Setter's
    public int Id_nómina { get => id_nómina; set => id_nómina = value; }
    public string Mes { get => mes; set => mes = value; }
    public string Año { get => año; set => año = value; }
    public double Salario_bruto { get => salario_bruto; set => salario_bruto = value; }
    public double Salario_neto { get => salario_neto; set => salario_neto = value; }
    public double Retenciones { get => retenciones; set => retenciones = value; }
    public double Cuotas { get => cuotas; set => cuotas = value; }
    public Contratos Contrato { get => contrato; set => contrato = value; }
}

//Program.cs
//Se inicia definición del código, el cual dará paso a los procesos y flujos de las clases creadas anteriormente.
namespace Nóminas
{
    class Program
    {
        //Método Main
        static void Main(string[] args)
        {
            //Se crean objetos de las tres clases.
            Empleados empleado1 = new Empleados("Juan", "García", "Calle Mayor, 123", "654321098", "juangarcia@gmail.com", "1980-01-01", "2020-01-01", "Programador", 2000.00);
            Contratos contrato1 = new Contratos(1, "Indefinido", "2020-01-01", "2025-12-31", 2000.00, 1500.00, empleado1);
            Nóminas nómina1 = new Nóminas(1, "Enero", "2023", 2000.00, 1500.00, 200.00, 100.00, contrato1);

            //Se muestra la información de los objetos creados.
            Console.WriteLine("Información del empleado:");
            Console.WriteLine("Nombre: " + empleado1.Nombre);
            Console.WriteLine("Apellido: " + empleado1.Apellido);
            Console.WriteLine("Dirección: " + empleado1.Dirección);
            Console.WriteLine("Teléfono: " + empleado1.Teléfono);
            Console.WriteLine("Email: " + empleado1.Email);
            Console.WriteLine("Fecha de nacimiento: " + empleado1.Fecha_nacimiento);
            Console.WriteLine("Fecha de contratación: " + empleado1.Fecha_contratación);
            Console.WriteLine("Puesto: " + empleado1.Puesto);
            Console.WriteLine("Salario: " + empleado1.Salario);

            Console.WriteLine("\nInformación del contrato:");
            Console.WriteLine("ID Contrato: " + contrato1.Id_contrato);
            Console.WriteLine("Tipo de contrato: " + contrato1.Tipo_contrato);
            Console.WriteLine("Fecha de inicio: " + contrato1.Fecha_inicio);
            Console.WriteLine("Fecha de fin: " + contrato1.Fecha_fin);
            Console.WriteLine("Salario bruto: " + contrato1.Salario_bruto);
            Console.WriteLine("Salario neto: " + contrato1.Salario_neto);
            Console.WriteLine("Empleado: " + contrato1.Empleado.Nombre + " " + contrato1.Empleado.Apellido);

            Console.WriteLine("\nInformación de la nómina:");
            Console.WriteLine("ID Nómina: " + nómina1.Id_nómina);
            Console.WriteLine("Mes: " + nómina1.Mes);
            Console.WriteLine("Año: " + nómina1.Año);
            Console.WriteLine("Salario bruto: " + nómina1.Salario_bruto);
            Console.WriteLine("Salario neto: " + nómina1.Salario_neto);
            Console.WriteLine("Retenciones: " + nómina1.Retenciones);
            Console.WriteLine("Cuotas: " + nómina1.Cuotas);
            Console.WriteLine("Contrato: " + nómina1.Contrato.Id_contrato);

            //Fin del programa.
            Console.ReadKey();
        }
    }
}

**Explicación:**

1. Se definen tres clases: Empleados, Contratos y Nóminas, con sus respectivos atributos y métodos.
2. Se crean objetos de las tres clases.
3. Se muestra la información de los objetos creados.
4. El programa termina cuando el usuario pulsa una tecla.