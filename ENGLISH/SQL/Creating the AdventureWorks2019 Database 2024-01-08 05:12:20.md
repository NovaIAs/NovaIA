```sql
CREATE DATABASE AdventureWorks2019;
GO

USE AdventureWorks2019;
GO

CREATE TABLE Production.Product (
    ProductID int NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    ProductNumber nvarchar(25) NOT NULL,
    MakeFlag bit NOT NULL,
    FinishedGoodsFlag bit NOT NULL,
    Color nvarchar(15),
    SafetyStockLevel smallint NOT NULL,
    ReorderPoint smallint NOT NULL,
    StandardCost money NOT NULL,
    ListPrice money NOT NULL,
    Size nvarchar(5),
    SizeUnitMeasureCode nvarchar(3) NOT NULL,
    Weight decimal(8, 2) NOT NULL,
    WeightUnitMeasureCode nvarchar(3) NOT NULL,
    DaysToManufacture int NOT NULL,
    ProductLine nvarchar(2) NOT NULL,
    Class nvarchar(2) NOT NULL,
    Style nvarchar(2) NOT NULL,
    ProductSubcategoryID int NOT NULL,
    ProductCategoryID int NOT NULL,
    ModificationDate datetime NOT NULL
);
GO

CREATE TABLE Production.ProductCategory (
    ProductCategoryID int NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Production.ProductSubcategory (
    ProductSubcategoryID int NOT NULL PRIMARY KEY,
    ProductCategoryID int NOT NULL,
    Name nvarchar(50) NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Production.ProductModel (
    ProductModelID int NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    Description nvarchar(max),
    CatalogDescription nvarchar(max),
    Instructions nvarchar(max),
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Production.ProductModelProductDescription (
    ProductModelID int NOT NULL,
    ProductDescriptionID int NOT NULL,
    CultureID nvarchar(6) NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL,
    PRIMARY KEY (ProductModelID, ProductDescriptionID, CultureID)
);
GO

CREATE TABLE Production.ProductDescription (
    ProductDescriptionID int NOT NULL PRIMARY KEY,
    Description nvarchar(400) NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Production.Culture (
    CultureID nvarchar(6) NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Production.UnitMeasure (
    UnitMeasureCode nvarchar(3) NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.Customer (
    CustomerID int NOT NULL PRIMARY KEY,
    TerritoryID int,
    AccountNumber nvarchar(10) NOT NULL,
    CustomerType nvarchar(1) NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.SalesOrderHeader (
    SalesOrderID int NOT NULL PRIMARY KEY,
    RevisionNumber smallint NOT NULL,
    OrderDate datetime NOT NULL,
    DueDate datetime NOT NULL,
    ShipDate datetime,
    Status int NOT NULL,
    OnlineOrderFlag bit NOT NULL,
    SalesOrderNumber nvarchar(25) NOT NULL,
    PurchaseOrderNumber nvarchar(25),
    AccountNumber nvarchar(10),
    CustomerID int NOT NULL,
    ShipToAddressID int NOT NULL,
    BillToAddressID int NOT NULL,
    ShipMethodID int NOT NULL,
    CreditCardID int,
    CreditCardApprovalCode nvarchar(15),
    CurrencyRate float NOT NULL,
    SubTotal money NOT NULL,
    TaxAmt money NOT NULL,
    Freight money NOT NULL,
    TotalDue money NOT NULL,
    Comment nvarchar(max),
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.SalesOrderDetail (
    SalesOrderID int NOT NULL,
    SalesOrderDetailID int NOT NULL,
    OrderQty smallint NOT NULL,
    ProductID int NOT NULL,
    UnitPrice money NOT NULL,
    UnitPriceDiscount decimal(4, 2) NOT NULL,
    LineTotal money NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL,
    PRIMARY KEY (SalesOrderID, SalesOrderDetailID)
);
GO

CREATE TABLE Sales.Address (
    AddressID int NOT NULL PRIMARY KEY,
    AddressLine1 nvarchar(60) NOT NULL,
    AddressLine2 nvarchar(60),
    City nvarchar(30) NOT NULL,
    StateProvinceID int NOT NULL,
    PostalCode nvarchar(15) NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.StateProvince (
    StateProvinceID int NOT NULL PRIMARY KEY,
    StateProvinceCode nvarchar(3) NOT NULL,
    CountryRegionCode nvarchar(3) NOT NULL,
    IsOnlyStateProvinceFlag bit NOT NULL,
    Name nvarchar(50) NOT NULL,
    TerritoryID int,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.CountryRegion (
    CountryRegionCode nvarchar(3) NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.ShipMethod (
    ShipMethodID int NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    ShipBase money NOT NULL,
    ShipRate money NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.CreditCard (
    CreditCardID int NOT NULL PRIMARY KEY,
    CardType nvarchar(50) NOT NULL,
    CardNumber nvarchar(25) NOT NULL,
    ExpMonth int NOT NULL,
    ExpYear int NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.SalesPerson (
    SalesPersonID int NOT NULL PRIMARY KEY,
    TerritoryID int,
    SalesQuota money,
    Bonus money,
    CommissionPct decimal(4, 2) NOT NULL,
    SalesYTD money NOT NULL,
    SalesLastYear money NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE Sales.SalesTerritory (
    TerritoryID int NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    CountryRegionCode nvarchar(3) NOT NULL,
    Group nvarchar(50),
    SalesYTD money NOT NULL,
    SalesLastYear money NOT NULL,
    CostYTD money NOT NULL,
    CostLastYear money NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE HumanResources.Employee (
    EmployeeID int NOT NULL PRIMARY KEY,
    NationalIDNumber nvarchar(15),
    ContactID int NOT NULL,
    LoginID nvarchar(256),
    ManagerID int,
    Title nvarchar(50),
    BirthDate datetime NOT NULL,
    MaritalStatus nvarchar(1) NOT NULL,
    Gender nvarchar(1) NOT NULL,
    HireDate datetime NOT NULL,
    SalariedFlag bit NOT NULL,
    VacationHours smallint NOT NULL,
    SickLeaveHours smallint NOT NULL,
    CurrentFlag bit NOT NULL,
    rowguid uniqueidentifier NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE HumanResources.Contact (
    ContactID int NOT NULL PRIMARY KEY,
    Title nvarchar(8),
    FirstName nvarchar(50) NOT NULL,
    LastName nvarchar(50) NOT NULL,
    Suffix nvarchar(10),
    EmailAddress nvarchar(50),
    PhoneNumber nvarchar(25),
    PhoneNumberType nvarchar(50),
    ModifiedDate datetime NOT NULL,
    rowguid uniqueidentifier NOT NULL
);
GO

CREATE TABLE HumanResources.EmployeePayHistory (
    EmployeePayHistoryID int NOT NULL PRIMARY KEY,
    EmployeeID int NOT NULL,
    RateChangeDate datetime NOT NULL,
    Rate float NOT NULL,
    PayFrequency nvarchar(50) NOT NULL,
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE HumanResources.JobCandidate (
    JobCandidateID int NOT NULL PRIMARY KEY,
    EmployeeID int,
    Resume nvarchar(max),
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE HumanResources.Department (
    DepartmentID int NOT NULL PRIMARY KEY,
    Name nvarchar(50) NOT NULL,
    GroupName nvarchar(50),
    ModifiedDate datetime NOT NULL
);
GO

CREATE TABLE HumanResources.Shift (
    Shift