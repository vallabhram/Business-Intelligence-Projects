DROP DATABASE IF EXISTS hospitalmanagement;
CREATE DATABASE hospitalManagement;
USE hospitalManagement;
SET SQL_SAFE_UPDATES=0;
/* Create tables. We have 18 tables */
DROP TABLE IF EXISTS Department_T;
CREATE TABLE Department_T(
	departmentID VARCHAR(40),
    deptName VARCHAR(20),
    PRIMARY KEY(departmentID)
);
DROP TABLE IF EXISTS Doctor_T;
CREATE TABLE Doctor_T(
	doctorID VARCHAR(40) NOT NULL,
    doctorName VARCHAR(30),
    doctorQualification VARCHAR(20),
    employeeID VARCHAR(40),
	PRIMARY KEY (doctorID)
);
DROP TABLE IF EXISTS Employee_T;
CREATE TABLE Employee_T(
	employeeID VARCHAR(40),
    accountNumber VARCHAR(40),
    employee_Fname VARCHAR(30),
    employee_Lname VARCHAR(30),
    employeePhone VARCHAR(100),
    empEmail VARCHAR(20),
    employeeType VARCHAR(30),
    empJoiningDate DATE,
    departmentID VARCHAR(40),
	PRIMARY KEY (employeeID)
);

DROP TABLE IF EXISTS Patient_T;
CREATE TABLE Patient_T(
	patientID VARCHAR(40),
    patient_Fname VARCHAR(40),
    patient_Lname VARCHAR(40),
    patient_Address VARCHAR(50),
    patientPhoneNumber INT(12),
    patientGender VARCHAR(10),
    patientDateOfBirth DATE,
    patientBloodGroup VARCHAR(10),
    PRIMARY KEY(patientID)
	
);

DROP TABLE IF EXISTS PatientRegister_T;
CREATE TABLE PatientRegister_T(
	patient_RegisterID VARCHAR(40) NOT NULL,
    patientID VARCHAR(40),
    admissionDate DATE,
    dischargeDate DATE,
	assigned_Doc_ID VARCHAR(40),
    wardID VARCHAR(40),
    roomID VARCHAR(40),
    mealID VARCHAR(40),
    PRIMARY KEY(patient_RegisterID)
);
DROP TABLE IF EXISTS Pharmacy_T;
CREATE TABLE Pharmacy_T(
	medicineID VARCHAR(40) NOT NULL,
	medicineName VARCHAR(20),
    medicinePrice FLOAT(20),
    medicineQuantity INT(30),
    departmentID VARCHAR(40),
    PRIMARY KEY (medicineID)
);
DROP TABLE IF EXISTS Cafeteria_T;
CREATE TABLE Cafeteria_T (
	MealID VARCHAR(40) NOT NULL ,
	mealDescription VARCHAR(45) ,
	PRIMARY KEY(MealID)

);
DROP TABLE IF EXISTS Payroll_T;
CREATE TABLE Payroll_T(
	payrollID VARCHAR(40) NOT NULL,
    netSalary INT,
    employeeID VARCHAR(40),
    accountNumber VARCHAR(40),
	primary key(payrollID)
);

DROP TABLE IF EXISTS Disease_T;
CREATE TABLE Disease_T(
	diseaseID VARCHAR(40),
    diseaseName VARCHAR(40),
    PRIMARY KEY(diseaseID)
);
DROP TABLE IF EXISTS PatientDisease_T;
CREATE TABLE PatientDisease_T(
    diseaseID VARCHAR(40),
    patient_RegisterID VARCHAR(40)
);
Drop table if exists PatientAuditLog_T;
CREATE TABLE PatientAuditLog_T(
	patientID VARCHAR(40),
    Fname varchar(40),
    Lname Varchar(40),
    Address varchar(255),
    Phone Int(30),
	Action_Date Date,
    Action_Type Varchar(40)
    
);

DROP TABLE IF EXISTS DONOR;
CREATE TABLE donor(
	donorName varchar(30),
    donorBloodGroup varchar(2), 
    donorAge INT,
    patientID varchar(40)
    );

DROP TABLE IF EXISTS trackDonorAndPatient;
CREATE TABLE trackDonorAndPatient(
	donorName varchar(30),
	donorBloodGroup varchar(2), 
	donorAge INT,
	patientID varchar(40),
	patient_Fname varchar(40),
	patient_Lname varchar(40)
);
DROP TABLE IF EXISTS PatientVitals_T;
CREATE TABLE PatientVitals_T(
    patient_RegisterID VARCHAR(40) NOT NULL,
    oxygen_Level INT(10) NOT NULL,
    heart_Rate INT(10) NOT NULL);
Drop TABLE IF EXISTS Inventory_T;
CREATE TABLE  Inventory_T(
	itemID VARCHAR(40) NOT NULL, 
	itemName VARCHAR(45),
	lastStockUpDate DATE, 
	quantity VARCHAR(40),
    departmentID VARCHAR(40),
	PRIMARY KEY (itemID)
);
Drop TABLE IF EXISTS room_T;
CREATE TABLE room_T(
	roomID VARCHAR(40) NOT NULL,
    wardID VARCHAR(40),
    roomStatus VARCHAR(20),
    PRIMARY KEY (roomID)

);
Drop TABLE IF EXISTS Ward_T;
CREATE TABLE Ward_T(
	wardID VARCHAR(40) NOT NULL,
    wardType VARCHAR(20),
    wardFloor VARCHAR(20),
    roomID VARCHAR(40),
	PRIMARY KEY (wardID)
);
Drop TABLE IF EXISTS Laboratory_T;
CREATE TABLE Laboratory_T(
	labID VARCHAR(40), 
	labtestName VARCHAR (30),
	testmaxValue VARCHAR(40),
    testminValue VARCHAR(40),
    departmentID VARCHAR(40),
    PRIMARY KEY(LabID)
);

/* Insert Data into tables */
INSERT INTO Department_T(
    departmentID,
    deptName) VALUES ('Dept1', 'Emergency'),
    ('Dept2','Nursing '),
    ('Dept3', 'Pathalogy'),
    ('Dept4', 'Pharmacy'),
    ('Dept5', 'Human Resource'),
    ('Dept6', 'Administration'),
    ('Dept7', 'Ambulance'),
    ('Dept8', 'Payroll'),
    ('Dept9', 'Specialists'),
    ('Dept10', 'Stocks');   
INSERT INTO Doctor_T(
    doctorID,
    doctorName,
    doctorQualification,
    employeeID
    ) VALUES
    ('Doc1','Anagha Shevade','PHD Psychology','Emp1'),
    ('Doc2','Pragati Divekar','Surgeon','Emp2'),
    ('Doc3','Vishvesh Pandey','Neurosurgeon','Emp3'),
    ('Doc4','Aayush Chahuahn','MD Homeopathic','Emp4'),
    ('Doc5','Chikyasa Fakira','MBBS Physician','Emp5');
INSERT INTO Employee_T(
    employeeID,
    accountNumber,
    employee_Fname,
    employee_Lname,
    employeePhone,
    empEmail,
    employeeType,
    empJoiningDate,
    departmentID
    ) VALUES
    ('Emp1','Acc1','Anagha','Shevade','9760981239','ana@gmail.com','Doctor','2000-10-20', 'Dept1'),
    ('Emp2','Acc2','Pragati','Divekar','9760983459','praggs@gmail.com','Doctor','2000-10-20', 'Dept1'),
    ('Emp3','Acc3','Vishvesh','Pandey','9999888875','lmn@hotmail.com','Ward Boy','1999-04-11', 'Dept1'),
    ('Emp4','Acc4','Aayush', 'Chahuahn','757478547','snl@gmail.com','Nurse','2002-06-06', 'Dept1'),
    ('Emp5','Acc5','Chikyasa', 'Fakira','5696858698','fin@orkut.com','Doctor','2006-05-05', 'Dept1'),
    ('Emp6','Acc6','Jeffery','Dahmer','58967569858','watcher@gmail.com','Ward Boy','2004-08-09', 'Dept7'),
    ('Emp7','Acc7','Richard','Ramirez','2541879624','capcitor@gmail.com','Doctor','2003-05-05', 'Dept9'),
    ('Emp8','Acc8','Miranda','Bailey','2051119098','miranda@hotmail.com','Doctor','2020-09-09', 'Dept6'),
    ('Emp9','Acc9','Sadie','Webber','2202911131','sw199@gmail.com','Nurse','2011-12-01', 'Dept5'),
    ('Emp10','Acc10','John','Doe','4236780512','95john@gmail.com','Ward Boy','2022-11-19', 'Dept5'),
    ('Emp11','Acc11','Monica','Geller','1123409876','monica@gmail.com','Doctor','2000-02-08', 'Dept5'),
    ('Emp12','Acc12','Joey','Tribbiani','8754093821','tribb@yahoo.com','Ward Boy','2011-10-10', 'Dept3'),
    ('Emp13','Acc13','Michael','Scott','3459867592','scott@rocketmail.com','Doctor','2005-11-10', 'Dept2'),
    ('Emp14','Acc14','Andy','Bernard','3429945786','andy@gmail.com','Ward Boy','2007-09-06', 'Dept8'),
    ('Emp15','Acc15','Rahul','Roy','2985579834','roy@gmail.com','Doctor','2010-03-11', 'Dept2'),
    ('Emp16','Acc16','Barney','Stinson','8964531889','barney540@yahoo.com','Physician','2009-04-03', 'Dept7'),
    ('Emp17','Acc17','Stephy','Monroe','9238485912','stephy87@yahoo.com','Nurse','2003-05-07', 'Dept6'),
    ('Emp18','Acc18','Pam','Beesly','6572349982','beesly@hotmail.com','Nurse','2017-06-04', 'Dept8'),
    ('Emp19','Acc19','Holly','Tirsten','3569835768','tirsten45@gmail.com','Pediatrician','2010-10-10', 'Dept8'),
    ('Emp20','Acc20','John','Mcclain','3757843398','john@yahoo.com','Doctor','2011-10-10', 'Dept2');
INSERT INTO Patient_T(
    patientID,
    patient_Fname,
    patient_Lname,
    patient_Address,
    patientPhoneNumber,
    patientGender,
    patientDateOfBirth,
    patientBloodGroup) VALUES
    ('1','Jane','Anderson','Dallas,TX','0009984456','Female','1999-12-04','O+'),
    ('2','Riya','Allianza','Austin,TX','0065780256','Female','1999-12-04','B+'),
    ('3','Priya','Lianza','Richardson,TX','0065780257','Female','1991-12-04','O+'),
    ('4','Ritanshu','Kumar','El Paso,TX','0065780255','Male','1955-12-04','A+'),
    ('5','Kumar','Abhishek','Collin County,TX','0065780326','Male','1995-12-04','B+'),
    ('6','Abhishek','Abhishek','Deep Ellum,TX','00657821256','Male','2001-12-04','O+'),
    ('7','Agastya','Pandya','Barbados,TX','1265780256','Male','2005-11-04','B+'),
    ('8','Anushka','Gupta','Dallas,TX','657498743','Female','2020-12-12','A-'),
    ('9','Sowmya','Sharma','Richardson,TX','0093256432','Female','1990-10-04','O+'),
    ('10','Chirag','Rupani','Austin,TX','812511796','Male','1960-12-10','A+'),
    ('11','Shruthi','Pathak','Dallas,TX','984976352','Female','1998-01-02','B+'),
    ('12','Raj','Arora','Barbados,TX','908765049','Male','1992-02-09','O+'),
    ('13','Parth','Roy','Richardson,TX','789634532','Male','1990-06-10','A+'),
    ('14','Nancy','Stellar','Houston,TX','875982342','Female','1980-04-09','B+'),
    ('15','Amy','Santiago','Dallas,TX','98456632','Female','1984-07-09','A-'),
    ('16','Michelle','Parker','Dallas,TX','783445778','Female','1998-04-10','B+'),
    ('17','Chritsitian','Pulisic','Austin,TX','897644513','Male','1996-06-11','O+'), 
    ('18','David','Villa','Barbados,TX','675444698','Male','1991-05-03','O+'),
    ('19','Mahesh','Patil','Richardson,TX','582974685','Male','1998-01-01','B+'),
    ('20','Thomas','Ramsey','Collin County ,TX','486779343','Male','1991-06-09','A-'); 
 
INSERT INTO PatientRegister_T(patient_RegisterID,patientID,
    admissionDate,dischargeDate,
    assigned_Doc_ID, wardID,roomID, mealID) 
    VALUES ('P1','1','2022-11-20','2022-11-24','Doc1', 'W4','W4G5','Meal1'),
    ('P2','2','2022-06-23','2022-06-30','Doc1','W1', 'W1S1','Meal2'),
    ('P3','3','2022-08-15','2022-08-15','Doc5','W1', 'W1S2','Meal5'),
    ('P4','4','2020-07-10','2020-07-11','Doc3','W1','W1S3','Meal5'),
    ('P5','5','2018-02-23','2018-02-28','Doc4','W1','W1S4','Meal9'),
    ('P6','6','2017-05-11','2017-05-11','Doc1','W1','W1S5','Meal8'),
    ('P7','7','2015-03-21','2015-03-22','Doc2','W2','W2G1','Meal1'),
    ('P8','8','2017-11-05','2017-11-15','Doc4','W2', 'W2G2','Meal3'),
    ('P9','9','2020-10-10','2020-10-24','Doc4' , 'W2','W2G3','Meal4'),
    ('P10','10','2022-11-19','2022-11-13','Doc1' , 'W2','W2G4','Meal4'),
    ('P11','11','2021-07-12','2021-08-12','Doc2' ,'W2', 'W2G5','Meal4'),
    ('P12','12','2022-11-23','2022-11-24','Doc3', 'W3','W3S1','Meal8'),
    ('P13','13','2021-09-23','2021-09-29','Doc12', 'W3','W3S2','Meal10'),
    ('P14','14','2000-11-30','2000-12-30','Doc19', 'W3','W3S3','Meal7'),
    ('P15','15','2022-10-08','2022-10-09','Doc18', 'W3','W3S4','Meal6'),
    ('P16','16','2018-09-21','2019-01-21','Doc21', 'W3','W3S5','Meal6'), 
    ('P17','17','2021-11-24','2021-11-28','Doc18', 'W4','W4G1','Meal5'), 
    ('P18','18','2022-09-23','2022-10-23','Doc3', 'W4','W4G2','Meal5'),
    ('P19','19','2021-04-20','2021-04-21','Doc16','W4', 'W4G3','Meal9'),
    ('P20','20','2017-10-14','2017-10-18','Doc15', 'W4','W4G4','Meal10') ; 
INSERT INTO Pharmacy_T(
    medicineID ,
	medicineName ,
    medicinePrice,
    medicineQuantity,
    departmentID) VALUES
    ('M1','Paracetamol','40','4000','Dept4'),
    ('M2','M-solvin Cylinder','90','3000','Dept4'),
    ('M3','Limcee','105','10000','Dept4'),
    ('M4','Benadryl','45','500','Dept4'),
    ('M5','Crocin','20','400','Dept4'),
    ('M6','Cetaphil','150','7000','Dept4'),
    ('M7','Softovac','25','700','Dept4'),
    ('M8','Vikoryl','120','6000','Dept4'),
    ('M9','Ceptilin','110','5000','Dept4'),
    ('M10','Ghilloy','200','200','Dept4'),
    ('M11','NuxVom','500','500','Dept4'),
    ('M12','Allegra','1000','600','Dept4'),
    ('M13','Soframyacin','300','1000','Dept4'),
    ('M14','Meftalspaz','500','1000','Dept4'),
    ('M15','Zanax','750','4000','Dept4'),
    ('M16','Advil','250','4000','Dept4'),
    ('M17','Claritin','560','3000','Dept4'),
    ('M18','Xyzal','350','2000','Dept4'),
    ('M19','Famotidine','750','4000','Dept4'),
    ('M20','Xyzal','400','1000','Dept4'),
    ('M21','Tylenol','700','5000','Dept4'),
    ('M22','Omeprazole','200','1500','Dept4'),
    ('M23','Excedrin','450','2500','Dept4'),
    ('M24','Cystex','600','3000','Dept4'), 
    ('M25','Coricidin','600','7000','Dept4'), 
    ('M26','Boiron','3550','4000','Dept4') ; 
INSERT INTO Cafeteria_T (
    MealID,
    mealDescription)
    VALUES
    ('Meal1','Vegetarian'),
    ('Meal2','NonVegetarian'),
    ('Meal3','Vegan'),
    ('Meal4','GluttenFree'),
    ('Meal5','Diabetese Special'),
    ('Meal6','Cardiac Diet'),
    ('Meal7','Child Special'),
    ('Meal8','Pescetarian'),
    ('Meal9','Low Fiber'),
    ('Meal10','High Fiber');
    
    
INSERT INTO  Payroll_T(
    payrollID,
    netSalary,
    employeeID,
    accountNumber
    ) VALUES
    ( 'PR1', 250000 , 'Emp1' , 'Acc1' ),
    ( 'PR2', 275000 , 'Emp2' , 'Acc2' ),
    ( 'PR3', 75000 , 'Emp3' , 'Acc3' ),
    ( 'PR4', 12000 , 'Emp4' , 'Acc4' ),
    ( 'PR5', 300000 , 'Emp5' , 'Acc5' ),
    ( 'PR6', 850000 , 'Emp6' , 'Acc6' ),
    ( 'PR7', 220000 , 'Emp7' , 'Acc7' ),
    ( 'PR8', 350000 , 'Emp8' , 'Acc8' ),
    ( 'PR9', 120000 , 'Emp9' , 'Acc9' ),
    ( 'PR10', 50000 , 'Emp10' , 'Acc10' ),
    ( 'PR11', 275000 , 'Emp11' , 'Acc11' ),
    ( 'PR12', 50000 , 'Emp12' , 'Acc12' ),
    ( 'PR13', 275000 , 'Emp13' , 'Acc13' ),
    ( 'PR14', 60000 , 'Emp14' , 'Acc14' ),
    ( 'PR15', 325000 , 'Emp15' , 'Acc15' ),
    ( 'PR16', 450000 , 'Emp16' , 'Acc16' ),
    ( 'PR17', 130000 , 'Emp17' , 'Acc17' ),
    ( 'PR18', 90000 , 'Emp18' , 'Acc18' ),
    ( 'PR19', 200000 , 'Emp19' , 'Acc19' ),
    ( 'PR20', 280000 , 'Emp20' , 'Acc20' ) ;
INSERT INTO Disease_T(
    diseaseID ,
	diseaseName ) VALUES
	('D1' , 'Common Cough and cold' ) ,
    ('D2' , ' Diabetes ' ) , 
	('D3' , 'Low/High Blood sugar' ) ,
	('D4' , 'Muscle Sprain ' ) ,
	('D5' , 'Bone fracture' ) ,
	('D6' , 'Covid -19 ') ,
	('D7' , 'Asthma' ) ,
	('D8' , 'Malaria' ) ,
	('D9' , 'Dengue' ) ,
	('D10' , 'Diarhhea' ) ,
	('D11' , 'Typhoid' ) ,
	('D12' ,' Jaundice'   ) ,
	('D13' , 'Kidney Stone' ) ,
	('D14' , 'Heart Attack '   ) ,
	('D15' , 'Food Poisoning' ) ,
	('D16' , 'Common Cough and cold' ) ,
	('D17' , 'Influenza' ) ,
	('D18' , 'Tuberculosis' ) ,
	('D19' , 'Allergies' ) ,
	('D20' , 'Migraine' ) ,
	('D21' , 'Conjuctivitis' ) ,
	('D22' , 'ChickenPox' ) ,
	('D23' , 'HIV/AIDS' ) ,
	('D24' , 'Diphteria' ) ,
	('D25' , 'Chlamydia' ) ,
	('D26' , 'Mumps' ) ,
	('D27' , 'Measles' )  ; 
INSERT INTO patientdisease_t
    (diseaseID,patient_RegisterID)
     VALUES
    ('D5','P1'),
    ('D3','P5'),
    ('D3','P6'),
    ('D8','P8'),
    ('D9','P9'),
    ('D9','P10'),
    ('D3','P11'),
    ('D1','P12'),
    ('D2','P2'),
    ('D8','P3'),
    ('D1','P4'),
    ('D6','P7'),
    ('D7','P13');
Insert into donor
(donorName,donorBloodGroup,donorAge,patientID)
	values
	("Priyam","A+",20,10),
	("ching yang","O+",25,1),
	("Pranay","A+",31,15),
	("Natasha","A-",40,11),
	("Zuben","B+",23,17),
	("Mihun","A-",44,12),
	("Ajinkya","A-",60,9),
	("Roy","B-",45,6),
	("Aaruhi","B-",28,2),
	("Sherlyn","A-",21,8);

INSERT INTO PatientVitals_T
 (patient_RegisterID,oxygen_Level,heart_Rate) 
	VALUES 
    ("P1","93","77"),
    ("P2","90","66"),
    ("P3","98","60"),
    ("P4","93","77"),
    ("P5","92","74"),
    ("P6","91","81"),
    ("P7","96","76"),
    ("P8","93","69"),
    ("P9","90","64"),
    ("P10","95","66"),
    ("P11","97","62"),
    ("P12","99","69"),
    ("P13","99","64"),
    ("P14","99","63"),
    ("P15","99","67");
INSERT INTO Inventory_T(
    itemID , 
	itemName,
	lastStockUpDate , 
	quantity,departmentID ) VALUES
    ('I1','Syringe','2020-12-01','10000','Dept10'),
    ('I2','Oxygen Cylinder','2021-03-04','200','Dept10'),
    ('I3','Viales','2022-12-01','10000','Dept10'),
    ('I4','PPE kits','2022-03-04','2000','Dept10'),
    ('I5','scissors','2020-12-01','5000','Dept10'),
    ('I6','Thermometer','2018-03-04','3000','Dept10'),
    ('I7','Saline','2018-02-01','3500','Dept10'),
    ('I8','Patient Monitor','2015-03-04','250','Dept10'),
    ('I9','ECG Machine','2011-11-04','25','Dept10'),
    ('I10','Steriliziers','2015-03-04','500','Dept10'),
    ('I11','Gloves','2020-10-10','10000','Dept10'),
    ('I12','Scrubs','2022-11-20','200','Dept10'),
    ('I13','Lab Coats','2022-01-01','500','Dept10'),
    ('I14','Sanitization Kits','2022-10-12','1000','Dept10'),
    ('I15','Chest Tubes','2022-01-01','5000','Dept10'),
    ('I16','EKG/ECG Machines','2010-05-07','5','Dept10'),
    ('I17','Hematogy Analyzer','2012-06-08','12','Dept10'),
    ('I18','Biochemistry Analyzer','2014-01-11','5','Dept10'), 
    ('I20','X ray Machines','2010-04-08','2','Dept10'), 
    ('I21','Infusion Pumo','2022-05-09','400','Dept10'), 
    ('I22','Surgical Light','2021-05-07','50','Dept10'), 
    ('I23','Wheelchairs','2022-04-09','5','Dept10') ; 
INSERT INTO Room_T(
    roomID, wardID,
    roomStatus) VALUES
    ('W1S1','W1','Occupied'),
    ('W1S2','W1','Occupied'),
    ('W1S3','W1','Occupied'),
    ('W1S4','W1','Occupied'),
    ('W1S5','W1','Occupied'),
    ('W2G1','W2','Occupied'),
    ('W2G2','W2','Occupied'),
    ('W2G3','W2','Occupied'),
    ('W2G4','W2','Occupied'),
    ('W2G5','W2','Occupied'),
    ('W3S1','W3','Occupied'),
    ('W3S2','W3','Occupied'),
    ('W3S4','W3','Occupied'),
    ('W3S5','W3','Occupied'),
    ('W4S1','W4','Occupied'),
    ('W4S2','W4','Occupied'),
    ('W4S3','W4','Occupied'),
    ('W4S4','W4','Occupied'),
    ('W4S5','W4','Occupied'),
    ('W5S1','W5','Vacant'),
    ('W5S2','W5','Vacant'),
    ('W5S3','W5','Vacant'),
    ('W5S4','W5','Vacant'),
    ('W5S5','W5','Vacant');
INSERT INTO Ward_T(
    wardID,
    wardType,
    wardFloor) VALUES
    ('W1','Special','1st'),
    ('W2','General','1st'),
    ('W3','Special','2nd'),
    ('W4','General','2nd'),
    ('W5','ICU','2nd');
INSERT INTO Laboratory_T(
    labID , 
	labtestName,
	testmaxValue,
    testminvalue,
    departmentID) VALUES 
    ('L101' , 'Red Blood Count test', ' Positive ', 'Negative','Dept3' ),
    ('L102' , 'Thyroid testing' , ' Positive ', 'Negative','Dept3'  ),
    ('L103' , ' Urinalysis' , ' Positive ', 'Negative','Dept3' ),
    ('L104' , 'Ultrasonography' , ' Positive ', 'Negative','Dept3' ),
    ('L105' , 'ECG Test' ,  ' Positive ', 'Negative','Dept3' ),
    ('L106' , 'MRI' , ' Positive ', 'Negative','Dept3' ),
    ('L107' , 'Angiography' , ' Positive ', 'Negative','Dept3'),
    ('L108' , 'X-rays' ,  ' Positive ', 'Negative','Dept3'),
    ('L109' , 'CT scan ' ,  ' Positive ', 'Negative','Dept3' ),
    ('L110' , 'Gastroscopy' ,  ' Positive ', 'Negative','Dept3' ),
    ('L111' , 'Prenatal Testing' ,  ' Positive ', 'Negative','Dept3' ),
    ('L112' , 'White Blood Count Test' ,  ' Positive ', 'Negative','Dept3'),
    ('L113' , 'Plasma Test', ' Positive ', 'Negative' ,'Dept3') ; 
/* Add foreign keys */
ALTER TABLE Employee_T
ADD CONSTRAINT FK_departmentID
FOREIGN KEY (departmentID) REFERENCES Department_T(departmentID);

ALTER TABLE Doctor_T
ADD CONSTRAINT FK_empID
FOREIGN KEY (employeeID) REFERENCES Employee_T(employeeID);

ALTER TABLE Payroll_T
ADD CONSTRAINT FK_empID1
FOREIGN KEY (employeeID) REFERENCES Employee_T(employeeID);

ALTER TABLE PatientRegister_T
ADD CONSTRAINT FK_wardID
FOREIGN KEY (wardID) REFERENCES Ward_T(wardID);

ALTER TABLE PatientRegister_T
ADD CONSTRAINT FK_mealID
FOREIGN KEY (mealID) REFERENCES Cafeteria_T(mealID);

ALTER TABLE Room_T
ADD CONSTRAINT FK_warID
FOREIGN KEY (wardID) REFERENCES Ward_T(wardID);

ALTER TABLE PatientDisease_T
ADD CONSTRAINT FK_DiseaseID
FOREIGN KEY (diseaseID) REFERENCES Disease_T(diseaseID);

ALTER TABLE PatientDisease_T
ADD CONSTRAINT FK_employeeID
FOREIGN KEY (patient_RegisterID) REFERENCES PatientRegister_T(patient_RegisterID);

ALTER TABLE PatientVitals_T
ADD CONSTRAINT FK_patient_RegisterID
FOREIGN KEY (patient_RegisterID) REFERENCES PatientRegister_T(patient_RegisterID);

ALTER TABLE PatientRegister_T
ADD CONSTRAINT FK_patientID
FOREIGN KEY (patientID) REFERENCES Patient_T(patientID);


ALTER TABLE PatientAuditLog_T
ADD CONSTRAINT FK_Patient_ID
FOREIGN KEY (patientID) REFERENCES Patient_T(patientID);


ALTER TABLE donor
ADD CONSTRAINT FK_patientID1
FOREIGN KEY (patientID) REFERENCES Patient_T(patientID);

ALTER TABLE trackDonorAndPatient
ADD CONSTRAINT FK_patientID2
FOREIGN KEY (patientID) REFERENCES Patient_T(patientID);


ALTER TABLE Pharmacy_T
ADD CONSTRAINT FK_deptID
FOREIGN KEY (departmentID) REFERENCES Department_T(departmentID);

ALTER TABLE Laboratory_T
ADD CONSTRAINT FK_deptID1
FOREIGN KEY (departmentID) REFERENCES Department_T(departmentID);


ALTER TABLE Inventory_T
ADD CONSTRAINT FK_deptID2
FOREIGN KEY (departmentID) REFERENCES Department_T(departmentID);



/* FUNCTIONS /*

/* 1. Function to calculatethe age of each Patient and then add a separate column in Patient table to store it */
ALTER TABLE Patient_T ADD COLUMN Patient_Age INT;
UPDATE Patient_T
SET Patient_Age = DATE_FORMAT(FROM_DAYS(DATEDIFF(now(),patientDateOfBirth)), '%Y') + 0;
/* Verify that the age is calculated and function works properly.*/
SELECT * FROM Patient_T;

/* 2. Function(1) to determine age category(Senior citizen,adults,child) of patients in the hospital*/
Drop Function if exists Patient_Age_Category;
DELIMITER $$  
CREATE FUNCTION Patient_Age_Category(  
    Patient_Age INT
)   
RETURNS VARCHAR(20)  
DETERMINISTIC  
BEGIN  
    DECLARE age_Category VARCHAR(20);  
    IF Patient_Age > 60 THEN  
        SET age_Category = 'Senior Citizen';  
    ELSEIF (Patient_Age <= 60 AND   
            Patient_Age >= 14) THEN  
        SET age_Category = 'Adult';  
    ELSEIF Patient_Age < 14 THEN  
        SET age_Category = 'Child';  
    END IF;  
    -- return the Patient's age category  
    RETURN (age_Category);  
END$$  

/*Calling the function Patient_Age_Category*/
SELECT patientID,patient_Fname,Patient_Age,Patient_Age_Category(Patient_Age) FROM Patient_T;


/* 3. Calculate No.of Days of each Patient was admitted in the hospital*/
/* We added a new column that will calculate and store number of days admitted */
ALTER TABLE PatientRegister_T ADD COLUMN Days_Admitted INT;
UPDATE PatientRegister_T
SET Days_Admitted = DATEDIFF(dischargeDate,admissionDate)+1;

/*Checking if the above function works.  */
SELECT * FROM PatientRegister_T

/*4. Function(2) Calculate Patient_room_rate based on no of days they were admitted as calculated in the previous function */
DROP FUNCTION IF EXISTS Patient_Room_Rate ;
DELIMITER $$  
CREATE FUNCTION Patient_Room_Rate(  
    Days_Admitted INT
)   
RETURNS INT (20)  
DETERMINISTIC  
BEGIN  
    DECLARE bill_Rate INT (20);  
    IF Days_Admitted > 20 THEN  
        SET bill_Rate = '2500';  
    ELSEIF (Days_Admitted <= 20 AND   
            Days_Admitted >= 10) THEN  
        SET bill_Rate = '3000';  
	ELSEIF (Days_Admitted <= 9 AND   
		Days_Admitted >= 1) THEN  
		SET bill_Rate = '4000';
    ELSE  
        SET bill_Rate = '5000';  
    END IF;  
    -- return the Patient Bill Rate per day  
    RETURN (bill_Rate);  
END$$  
/* Adding Patient_room_rate column out of function */
Alter table PatientRegister_T add column Patient_room_rate Int;
UPDATE PatientRegister_T SET Patient_room_rate=Patient_Room_Rate(Days_Admitted);
select * from PatientRegister_T;

/* Function(3) Calculate Bill Amount for each Patient from Days_Admitted and Patient_room_rate */
Alter table PatientRegister_T add column room_Bill Int;
UPDATE PatientRegister_T
SET room_Bill = (Days_Admitted) * (Patient_room_rate);

/* 5. Function to calculate bill discount for each patient based on room bill */
DROP FUNCTION IF EXISTS Patient_Discounted_bill ;
DELIMITER $$  
CREATE FUNCTION Patient_Discounted_bill(  
    room_Bill INT
)   
RETURNS INT (30)  
DETERMINISTIC  
BEGIN  
    DECLARE 
    discount INT (20);  
    IF room_Bill > 100000 THEN  
        SET discount = '70';  
    ELSEIF (room_Bill >=75000 AND   
            room_Bill <= 100000) THEN  
        SET discount = '80';  
	ELSEIF (room_Bill >= 25000 AND   
		room_Bill <= 74999) THEN  
		SET discount = '90';
	ELSEIF (room_Bill >= 10000 AND   
		room_Bill <= 24999) THEN  
		SET discount = '95';
    ELSE
		SET discount='1';
    END IF;  
    -- return the Patient Bill Rate per day  
    RETURN (discount);  
END$$  

/* Adding Patient_Discounted_Rate column out of function */
Alter table PatientRegister_T DROP column Patient_discounted_rate;
Alter table PatientRegister_T add column Patient_discounted_rate Int;
UPDATE PatientRegister_T SET Patient_discounted_rate=Patient_Discounted_bill(room_Bill);
/* Checking if the discounted bill rate is sucessfully loaded. */
SELECT * FROM PatientRegister_T;


/*Stored Procedures */
/*SP1. To find the disease whos occurance rate is high */
DROP PROCEDURE IF EXISTS maxDiseaseOcccurence;
/*DELIMITER $$
CREATE PROCEDURE maxDiseaseOcccurence(
OUT result VARCHAR(40)
)
BEGIN
DECLARE cumulativeResult DEC(10,2) DEFAULT 0;
SELECT patient_RegisterID INTO cumulativeResult from patientdisease_t where diseaseID IN (select max(count(diseaseID)) from patientdisease_t);
END$$
DELIMITER ;
CALL maxDiseaseOcccurence(@cumulativeResult);
select @cumulativeResult;
*/
/*SP2.Find the name of the employees whos salary is greater than specified range*/
DROP PROCEDURE IF EXISTS oldEmployeeGreater;
DELIMITER $$
CREATE PROCEDURE oldEmployeeGreater(
IN salary INT)
#OUT employeeName varchar(30))
BEGIN
DECLARE ename varchar(30);
SELECT employee_Fname from employee_t where employeeID IN(select employeeID from payroll_t where netSalary > salary);
END$$
DELIMITER ;
CALL oldEmployeeGreater(700000);

/*SP3.Write procedure to give bonus to employees whos tenure is greater than 10 years */
DROP PROCEDURE IF EXISTS bonusup;
DELIMITER $$
CREATE PROCEDURE bonusup(
IN bonusPercent float(20))
BEGIN
SET bonusPercent=bonusPercent+1;
SELECT employee_Fname,employee_Lname,DATE_FORMAT(FROM_DAYS(DATEDIFF(NOW(), empJoiningDate)), '%Y') + 0 AS tenure,netSalary*bonusPercent as bonus,netSalary as previousSalary
FROM employee_t E JOIN payroll_t P on  E.employeeID=P.employeeID
GROUP BY E.employeeID
HAVING tenure > '10';
END$$
DELIMITER ;
CALL bonusup(0.25);

/*SP4.*/
DROP PROCEDURE IF EXISTS ROOM_STATUS;
DELIMITER $$
CREATE PROCEDURE ROOM_STATUS()
BEGIN
DECLARE id varchar(40);
SELECT roomID into id from patientregister_t where roomID IN(select RoomID from room_t where roomStatus='Vacant');
UPDATE room_t set roomStatus='Occupied' where roomID=id;
END$$
DELIMITER $$
call ROOM_STATUS();
INSERT INTO patientregister_t(patient_RegisterID,patientID, admissionDate, dischargeDate, assigned_Doc_ID, wardID ,roomID,mealID)
VALUES('P89','88','2022-11-20','2022-11-24','Doc1', 'W4', 'W5S1', 'Meal1');#Test
select* from room_t;


/*Views*/
/*view1.Patient ordinary data*/
Create View Patient_Ordinary_Data
as
Select p.patientID,p.patient_Fname,p.patient_Lname,pr.admissionDate,pr.dischargeDate,pr.roomID from Patient_T as p join 
PatientRegister_T as pr on p.patientID=pr.patientID;
select * from Patient_Ordinary_Data;

/*View2.Patient sensitive data*/
Drop view if exists Patient_Sensitive_Data;
Create View Patient_Sensitive_Data
as
Select p.patientID,p.patient_Fname,p.patient_Lname,p.patient_Address,p.patientPhoneNumber,p.Patient_Age,dd.diseaseID,dd.diseaseName from Patient_T as p join 
PatientRegister_T as pr on p.patientID=pr.patientID join PatientDisease_T as d on pr.patient_RegisterID=d.patient_RegisterID inner join Disease_T as dd on d.diseaseID=dd.diseaseID;
select * from Patient_Sensitive_Data;

/*view3.Employee sensitive data*/
Drop view if exists Employee_Sensitive_Data;
Create View Employee_Sensitive_Data
as 
select e.employeeID,e.AccountNumber,concat(e.employee_Fname,e.employee_Lname) as Fullname,e.employeePhone,
e.empEmail,d.deptName,pay.netSalary from Employee_T as e join Department_T as d on e.departmentID=d.departmentID join Payroll_T as pay on e.employeeID=pay.employeeID;
select * from Employee_Sensitive_Data;


/*Join queries */

/* 1.  How many patients in the hospital are assigned diabetic meals? List thier names and meal description */   
SELECT patient_Fname, patient_Lname,Cafeteria_T.mealDescription 
FROM Patient_T 
INNER JOIN
PatientRegister_T ON Patient_T.patientID = PatientRegister_T.patientID INNER JOIN
Cafeteria_T ON Cafeteria_T.mealID = PatientRegister_T.mealID
WHERE PatientRegister_T.mealID IN (SELECT MealID FROM Cafeteria_T WHERE
mealDescription LIKE '%Diabetese%');

/* 2. Find department for Employee using joins */
Select e.employeeID,d.deptName from Employee_T as e 
Inner Join Department_T as d on e.departmentID=d.departmentID;

/*3. Show me the qualifications of the doctor who is assigned to Patient 1 */
Select doctor_t.doctorName, doctor_t.doctorQualification
FROM doctor_t Inner Join patientregister_t ON
patientregister_t.assigned_Doc_ID = doctor_t.doctorID
WHERE patientregister_t.patientID;


/* Triggers */
/* Trigger1 to check if medicine quantity is less than 1000 */
Drop Trigger IF EXISTS Check_quantity;
Delimiter $$
CREATE TRIGGER  Check_quantity  AFTER INSERT ON Pharmacy_T 
FOR EACH ROW
BEGIN
IF NEW.medicineQuantity < 1000 THEN
SIGNAL SQLSTATE '45000'
SET MESSAGE_TEXT = 'ERROR: refill Stock!';
END IF;
END; $$
select * from payroll_t;
INSERT INTO Pharmacy_T(
    medicineID ,
	medicineName ,
    medicinePrice,
    medicineQuantity) VALUES
    ('M45','Paracetamol','40','900');
/*trigger 2.To check after insert on donor data patient record is updated in trackdonorpatient table*/
DROP TRIGGER IF EXISTS archiveDonorPatient;
DELIMITER $$
CREATE TRIGGER archiveDonorPatient
AFTER INSERT ON donor
for each row
BEGIN

INSERT INTO trackDonorAndPatient(donorName,donorBloodGroup,donorAge,patientID,patient_Fname,patient_Lname)
select d.donorName,d.donorBloodGroup, d.donorAge,d.patientID,t.patient_Fname,t.patient_Lname from donor d join patient_t t ON t.patientID=d.patientID;
END$$
DELIMITER ;
Insert into donor(donorName,donorBloodGroup,donorAge,patientID)values
("minini","A-",26,1);

select * from trackDonorAndPatient;
/*Trigger3.Monitor Patient's Oxygen level. If oxygen level goes below 80,it should send trigger and call the doctor*/
DROP TRIGGER IF EXISTS Monitor_Oxygen_Level;
DELIMITER $$
CREATE TRIGGER Monitor_Oxygen_Level
AFTER UPDATE
ON PatientVitals_T
FOR EACH ROW
BEGIN
DECLARE msg VARCHAR (128);
IF NEW.oxygen_Level < 80
THEN
SET msg = concat('Patient Oxygen Level Critical Call Doctor', ' ',
(SELECT assigned_Doc_ID FROM PatientRegister_T INNER JOIN PatientVitals_T
ON PatientRegister_T.patient_RegisterID = PatientVitals_T.patient_RegisterID
WHERE PatientVitals_T.oxygen_Level = NEW.oxygen_Level)) ;
SIGNAL SQLSTATE '45000' set message_text = msg;
END IF;
end
$$
SET
SQL_SAFE_UPDATES=0;
UPDATE PatientVitals_T
SET oxygen_Level = 40 WHERE
patient_RegisterID = "P4";   

/*Trigger4.If paient's heart rate goes above 110 or below 50 then trigger will run and send the msg check on the patient.*/
DROP TRIGGER IF EXISTS HeartRate_Critical;
    DELIMITER $$
    CREATE TRIGGER HeartRate_Critical
    AFTER UPDATE
    ON PatientVitals_T
    FOR EACH ROW
    BEGIN
    DECLARE msg2 VARCHAR (128);
IF NEW.heart_Rate > 110 OR NEW.heart_Rate < 50 THEN
    SET msg2 = CONCAT((SELECT Patient_T.patient_Fname FROM Patient_T
    INNER JOIN (SELECT PatientRegister_T.patient_RegisterID,PatientRegister_T.patientID FROM PatientRegister_T INNER JOIN
    PatientVitals_T
    ON PatientRegister_T.patient_RegisterID = PatientVitals_T.patient_RegisterID
    WHERE PatientVitals_T.heart_Rate = NEW.heart_Rate) AS temp ON temp.patientID = Patient_T.patientID),"'s"," ", "Heartbeat is Abnormal, Check Patient");
    SIGNAL SQLSTATE '45000' set message_text = msg2;
    END IF;
    END
    $$
    
SET SQL_SAFE_UPDATES=0; 
UPDATE PatientVitals_T
SET heart_Rate = 120 WHERE
patient_RegisterID = "P1"; 

