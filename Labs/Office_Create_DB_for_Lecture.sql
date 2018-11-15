CREATE DATABASE LEVEL;

USE LEVEL;

CREATE TABLE Offices 
(OfficeNbr NCHAR(2) NOT NULL PRIMARY KEY,
 City NVARCHAR(20) NOT NULL,
 State NCHAR(2) NOT NULL,
 Region NCHAR(5) NOT NULL,
 Target DECIMAL(10,2) NOT NULL,
 Sales DECIMAL(10,2) NOT NULL,
 Phone NVARCHAR(15) NOT NULL);

select * from offices;

INSERT INTO Offices
VALUES('1', 'Denver', 'CO', 'West', 3000000, 130000, '970.586.3341');
-- 
-- INSERT INTO Offices
-- VALUES('1', 'Denver', 'CO', 'West', 4000000, 130000, '970.586.3341');
-- 

INSERT INTO Offices
VALUES('2', 'New York', 'NY', 'East', 200000, 300000, '212.942.5574');

INSERT INTO Offices
VALUES('57', 'Dallas', 'TX', 'West', 0, 0, '214.781.5342');

# Add a record for Boston office;
insert into offices
values('3', 'Boston', 'MA', 'NE', 5000000,600000, '617.333.2345');


select * from offices;
select City, Region, Target from offices;

drop table if exists SalesReps;
CREATE TABLE SalesReps
(RepNbr NCHAR(3) NOT NULL PRIMARY KEY,
 Name NVARCHAR(20) NOT NULL,
 RepOffice NCHAR(2) NOT NULL,
 Quota DECIMAl(10,2),   #Allow NULLs 
 Sales DECIMAl(10,2) NOT NULL,
 FOREIGN KEY (RepOffice) 
	REFERENCES Offices(OfficeNbr));

INSERT INTO SalesReps
VALUES('53', 'Bill Smith', '1', 100000, 0);

-- INSERT INTO SalesReps
-- VALUES('53', 'Jan Wane', '1', 100000, 0);
-- 
select * from SalesReps;

INSERT INTO SalesReps
VALUES('89', 'Jen Jones', '2', 50000, 130000);

insert into SalesReps
VALUES('54', 'Jane Wane', '1', 200000, 100000);

SELECT 
    Name, RepOffice
FROM
    SalesReps
WHERE
    Sales > 100000;
    
SELECT 
    SUM(Sales) AS TotalSales, AVG(Sales) AS AvgSales
FROM
    SalesReps
where RepOffice = 1;

select Sales from salesreps where RepOffice = 1;


select OfficeNbr, City from Offices;



## Check on rep's performance;

select * from SalesReps;

drop table if exists Customers;
CREATE TABLE Customers
(CustNbr NCHAR(3) NOT NULL PRIMARY KEY,
 Company NVARCHAR(30) NOT NULL,
 CustRep NCHAR(3) NOT NULL,
 CreditLimit DECIMAl(10,2) NOT NULL,
 FOREIGN KEY (CustRep) 
	REFERENCES SalesReps(RepNbr));

SELECT * FROM Customers;

INSERT INTO Customers
VALUES('211', 'Connor Co', '89', 50000);
INSERT INTO Customers
VALUES('522', 'Amaratunga Enterprises', '89', 40000);
INSERT INTO Customers
VALUES('890', 'Feni Fabricators', '53', 1000000);

select * from Customers;

select count(*) from Customers where CustNbr = 211;
select count(*) from Customers where CustRep = 89;

DROP TABLE IF EXISTS Orders;
CREATE TABLE Orders
(OrderNbr INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
 Cust NCHAR(3) NOT NULL,
 Prod NVARCHAR(20) NOT NULL,
 Qty INT NOT NULL,
 Amt DECIMAl(10,2) NOT NULL,
 Disc DECIMAL(3,1) NOT NULL,
 FOREIGN KEY (Cust) 
	REFERENCES Customers(CustNbr));

INSERT INTO Orders (Cust, Prod, Qty, Amt, Disc)
VALUES('211', 'Bulldozer', 7, 31000, 0.2);
INSERT INTO Orders (Cust, Prod, Qty, Amt, Disc)
VALUES('522', 'Riveter', 2, 4000, 0.3);
INSERT INTO Orders (Cust, Prod, Qty, Amt, Disc)
VALUES('522', 'Crane', 1, 500000, 0.4);

SELECT 
    AVG(amt)
FROM
    Orders
WHERE
    cust = 522;
    
SELECT 
    Cust, AVG(amt) AS AvgSpend
FROM
    Orders
GROUP BY cust;
 
select * from Customers;

SELECT 
    CustRep, COUNT(*) as NbrCust
FROM
    Customers
where (Company ='Connor Co' or Company = 'Amaratunga Enterprises');


SELECT 
    CustRep, COUNT(*) as NbrCust
FROM
    Customers
GROUP BY CustRep
HAVING NbrCust > 1;

select * from Orders;

DROP TABLE IF EXISTS Parts;
CREATE TABLE Parts
(PartID NCHAR(4) NOT NULL PRIMARY KEY,
 Vendor NCHAR(4) NOT NULL);

INSERT INTO Parts
VALUES('123', 'A');
INSERT INTO Parts
VALUES('234', 'A');
INSERT INTO Parts
VALUES('345', 'B');
INSERT INTO Parts
VALUES('362', 'A');
INSERT INTO Parts
VALUES('2345', 'C');
INSERT INTO Parts
VALUES('3464', 'A');
INSERT INTO Parts
VALUES('4533', 'C');

select * from Parts;

DROP TABLE IF EXISTS Employees;
CREATE TABLE Employees
(EmpNbr NCHAR(5) NOT NULL PRIMARY KEY,
 Name NVARCHAR(20) NOT NULL,
 Title NVARCHAR(20) NOT NULL,
 Mgr NCHAR(5));

INSERT INTO Employees
VALUES('105', 'Mary Smith', 'Analyst', '104');
INSERT INTO Employees
VALUES('109', 'Jill Jones', 'Sr Analyst', '107');
INSERT INTO Employees
VALUES('104', 'Sally Silver', 'Manager', '111');
INSERT INTO Employees
VALUES('107', 'Pat Brown', 'Manager', '111');
INSERT INTO Employees (EmpNbr, Name, Title)
VALUES('111', 'Eileen Howe', 'President');

## #36 Inner join (where) clause
select * from Customers;
select * from orders;

SELECT 
    Orders.OrderNbr,
    Orders.Amt,
    Customers.Company,
    Customers.CreditLimit
FROM
    Orders,
    Customers
WHERE
    Orders.Cust = Customers.CustNbr;

SELECT OrderNbr, Amt, Company, CreditLimit FROM Customers, Orders WHERE Cust = CustNbr;      

SELECT 
    OrderNbr, Amt, Company, CreditLimit
FROM
    Customers
        INNER JOIN
    Orders ON Customers.CustNbr = Orders.Cust;

## Equivalent as
SELECT OrderNbr, Amt, Company, CreditLimit FROM Customers INNER JOIN Orders ON Customers.CustNbr = Orders.Cust;

SELECT 
    OrderNbr, Amt, Company, CreditLimit, Name
FROM
    Orders,
    Customers,
    salesreps
WHERE
    Orders.Cust = Customers.CustNbr
        AND Customers.CustRep = SalesReps.RepNbr
        AND Orders.Amt > 25000;



## #38 Join multiple (3) tables;
SELECT 
    OrderNbr, Amt, Company, Name
FROM
    Orders,
    Customers,
    SalesReps
WHERE
    Cust = CustNbr AND CustRep = RepNbr
        AND Amt >= 25000;

SELECT 
    OrderNbr, Amt, Company, Name, Region
FROM
    Orders,
    Customers,
    SalesReps,
    offices
WHERE
    Cust = CustNbr AND CustRep = RepNbr
        and RepOffice = OfficeNbr and Region = 'East';
#####################
#17
SELECT Name, Sales, Quota FROM SalesReps;

SELECT Name, Sales, Quota, (Sales-Quota) FROM SalesReps;

SELECT Name, Sales, Quota, (Sales-Quota) FROM SalesReps WHERE Sales < Quota;

#18
SELECT AVG(Amt) FROM Orders;
SELECT AVG(Amt) FROM Orders WHERE Cust = 211;
INSERT INTO Offices (OfficeNbr, City, State, Region, Target, Sales, Phone) VALUES ('55', 'Dallas','TX','West', 200000, 0, '214.333.2222');

SELECT * FROM Customers;
DELETE FROM Customers WHERE Company = 'Connor Co';

select * from Orders;
SELECT COUNT(*) FROM Orders WHERE Cust = '211';

#20
SELECT * FROM Offices;
SELECT Region FROM Offices;
SELECT DISTINCT Region FROM Offices;

SHOW VARIABLES LIKE "%version%";

SELECT 
    customers.Company,
    customers.CreditLimit,
    SalesReps.Name,
    SalesReps.Sales
FROM
    Customers,
    SalesReps
WHERE
    Customers.CustRep = SalesReps.RepNbr
;