SELECT * FROM EmployeeTable;

SELECT * FROM JobProfileMapping;


SELECT COLUMN_NAME, DATA_TYPE 
FROM INFORMATION_SCHEMA.COLUMNS 
WHERE TABLE_NAME = 'EmployeeTable';



-- segment by age percentile

SELECT TOP (1)
    PERCENTILE_CONT(0) WITHIN GROUP (ORDER BY Age) OVER() AS 'Min',
    PERCENTILE_CONT(.50) WITHIN GROUP (ORDER BY Age) OVER() AS 'Median',
    PERCENTILE_CONT(1) WITHIN GROUP (ORDER BY Age) OVER() AS 'Max'
FROM EmployeeTable
WHERE ActiveStatus = 1



-- segment by age range

SELECT 
    AgeRange, COUNT(*) as Freq
FROM
(
    SELECT
        CASE
        WHEN Age < 25 THEN '25 and under '
        WHEN Age BETWEEN 25 AND 34 THEN '25-34'
        WHEN Age BETWEEN 35 AND 44 THEN '35-44'
        WHEN Age BETWEEN 45 AND 54 THEN '45-54'
        WHEN Age BETWEEN 55 AND 64 THEN '55-64'
        ELSE '64 and older' END AS AgeRange
    FROM EmployeeTable
) AgeRangeFreq
GROUP BY AgeRange
ORDER BY AgeRange;






-- number of new employee of each year by office

WITH cte AS
(
SELECT
    YEAR(Start_Date) AS Year,
    Office,
    COUNT(YEAR(Start_Date)) AS NewHire,
    RANK() OVER(PARTITION BY YEAR(Start_Date) ORDER BY COUNT(YEAR(Start_Date)) DESC) AS RankofMostHireOffice,
    RANK() OVER(PARTITION BY YEAR(Start_Date) ORDER BY COUNT(YEAR(Start_Date)) ASC) AS RankofLeastHireOffice
FROM EmployeeTable
GROUP BY YEAR(Start_Date), Office
)
SELECT Year, Office, NewHire
FROM cte
WHERE RankofMostHireOffice = 1 OR RankofLeastHireOffice = 1
ORDER BY Year, NewHire DESC;



-- number of new employee of each year by department

WITH cte AS
(
SELECT
    YEAR(Start_Date) AS Year,
    Department,
    COUNT(YEAR(Start_Date)) AS NewHire,
    RANK() OVER(PARTITION BY YEAR(Start_Date) ORDER BY COUNT(YEAR(Start_Date)) DESC) AS RankofMostHireDepartment,
    RANK() OVER(PARTITION BY YEAR(Start_Date) ORDER BY COUNT(YEAR(Start_Date)) ASC) AS RankofLeastHireDepartment
FROM EmployeeTable
GROUP BY YEAR(Start_Date), Department
)
SELECT Year, Department, NewHire
FROM cte
WHERE RankofMostHireDepartment = 1 OR RankofLeastHireDepartment = 1
ORDER BY Year, NewHire DESC;






-- retention rate per year

WITH cte AS
(
SELECT
    sq1.Year,
    ISNULL(sq2.num_hire, 0) AS new_hired,
    SUM(sq2.num_hire) OVER (ORDER BY sq1.Year) cum_new_hired,
    ISNULL(sq3.terminated, 0) AS terminated,
    ISNULL(SUM(sq3.terminated) OVER (ORDER BY sq1.Year),0) cum_terminated
FROM
        (
            SELECT DISTINCT(YEAR(Start_Date)) as Year
            FROM EmployeeTable
            UNION
            SELECT DISTINCT(YEAR(Termination_Date))
            FROM EmployeeTable
        ) sq1
    LEFT JOIN 
        (
            SELECT YEAR(Start_Date) as Year, COUNT(YEAR(Start_Date)) as num_hire
            FROM EmployeeTable
            GROUP BY YEAR(Start_Date)
        ) sq2
    ON sq1.Year = sq2.Year
    LEFT JOIN
        (
            SELECT YEAR(Termination_Date) as Year, COUNT(YEAR(Termination_Date)) as terminated
            FROM EmployeeTable
            GROUP BY YEAR(Termination_Date)
        ) sq3
    ON sq1.Year = sq3.Year
    WHERE sq1.Year != 2999
)   
SELECT
    cte.Year,
    ISNULL(LAG((cte.cum_new_hired - cte.cum_terminated), 1) OVER(ORDER BY cte.Year), 0) AS employee_start,
    cte.new_hired, cte.terminated,
    cte.cum_new_hired - cte.cum_terminated AS employee_end,
    ROUND(((CAST(cte.cum_new_hired AS float) - cte.cum_terminated - cte.new_hired) /
    LAG((cte.cum_new_hired - cte.cum_terminated), 1) OVER(ORDER BY cte.Year)), 4)  AS retention_rate
FROM cte;





-- highest and lowest retention rate per year in each department

WITH cte AS
(
SELECT
    sq5.Year,
    sq5.Department,
    sq5.retention_rate,
    DENSE_RANK() OVER(PARTITION BY sq5.Year ORDER BY sq5.retention_rate DESC) AS highest_rr,
    DENSE_RANK() OVER(PARTITION BY sq5.Year ORDER BY sq5.retention_rate) AS lowest_rr
FROM
(
    SELECT
        sq4.Department,
        sq4.Year,
        sq4.new_hired,
        sq4.terminated,
        ISNULL(LAG((sq4.cum_new_hired - sq4.cum_terminated), 1)
            OVER(PARTITION BY sq4.Department ORDER BY sq4.Department, sq4.Year), 0) AS employee_start,
        sq4.cum_new_hired - sq4.cum_terminated AS employee_end,
        CASE
            WHEN 
                ROUND(((CAST(sq4.cum_new_hired AS float) - sq4.cum_terminated - sq4.new_hired) /
                LAG((sq4.cum_new_hired - sq4.cum_terminated), 1) 
                OVER(PARTITION BY sq4.Department ORDER BY sq4.Department, sq4.Year)), 4) < 0 THEN 0
            ELSE
                ROUND(((CAST(sq4.cum_new_hired AS float) - sq4.cum_terminated - sq4.new_hired) /
                LAG((sq4.cum_new_hired - sq4.cum_terminated), 1)
                OVER(PARTITION BY sq4.Department ORDER BY sq4.Department,sq4.Year)), 4)
            END AS retention_rate
    FROM 
        (
        SELECT DISTINCT
            sq1.Department,
            sq1.Year,
            ISNULL(sq2.num_hire, 0) AS new_hired,
            SUM(sq2.num_hire) OVER (PARTITION BY sq1.Department ORDER BY sq1.Department, sq1.Year) AS cum_new_hired,
            ISNULL(sq3.terminated, 0) AS terminated,
            ISNULL(SUM(sq3.terminated) OVER (PARTITION BY sq1.Department ORDER BY sq1.Department, sq1.Year),0) AS cum_terminated
        FROM
            (
                SELECT DISTINCT Department, YEAR(Start_Date) as Year
                FROM EmployeeTable
                UNION
                SELECT DISTINCT Department, YEAR(Termination_Date) as Year
                FROM EmployeeTable
            ) sq1
            LEFT JOIN 
            (
                SELECT Department, YEAR(Start_Date) as Year, COUNT(YEAR(Start_Date)) as num_hire
                FROM EmployeeTable
                GROUP BY Department, YEAR(Start_Date)
            ) sq2
            ON sq1.Year = sq2.Year AND sq1.Department = sq2.Department
            LEFT JOIN
            (
                SELECT Department, YEAR(Termination_Date) as Year, COUNT(YEAR(Termination_Date)) as terminated
                FROM EmployeeTable
                GROUP BY Department, YEAR(Termination_Date)
            ) sq3
            ON sq1.Year = sq3.Year AND sq1.Department = sq3.Department
        ) sq4
    ) sq5
WHERE sq5.Year NOT IN (2009, 2999)
)
SELECT
    cte.Year,
    cte.Department,
    cte.retention_rate
FROM cte
--WHERE cte.highest_rr = 1 OR cte.lowest_rr = 1
ORDER BY cte.Year, cte.retention_rate DESC;







-- yearly average salary of each department (using recursion cte)

WITH cte AS
(
    SELECT
        Department,
        Start_Date, Termination_Date,
        YEAR(Start_Date) AS EmpYr,
        hari = DATEDIFF(DAY, Start_Date, (CASE WHEN YEAR(Start_Date) = YEAR(Termination_Date) THEN Termination_Date ELSE DATEFROMPARTS(YEAR(Start_Date), 12, 31) END)),
        Salary,
        Annual_salary = ROUND(( CAST(Salary AS FLOAT) / ( 365 + (CASE WHEN YEAR(Start_Date)%4=0 THEN 1 ELSE 0 END))) *
                                DATEDIFF(DAY, Start_Date,
                                (CASE   WHEN YEAR(Start_Date) = YEAR(Termination_Date) THEN Termination_Date
                                        ELSE DATEFROMPARTS(YEAR(Start_Date), 12, 31) END)),2)
    FROM EmployeeTable

    UNION ALL

    SELECT
        Department,
        Start_Date, Termination_Date,
        EmpYr = EmpYr + 1,
        hari = CASE
                    WHEN (EmpYr + 1) = YEAR(Termination_Date) THEN DATEDIFF(DAY, DATEFROMPARTS(YEAR(Termination_Date), 1, 1), Termination_Date)
                    ELSE (CASE WHEN (EmpYr + 1) %4 = 0 THEN 366 ELSE 365 END) END,
        Salary,
        Annual_salary = ROUND((CAST(Salary AS FLOAT) / ( 365 + (CASE WHEN (EmpYr + 1) %4=0 THEN 1 ELSE 0 END))) *
                                CASE
                                WHEN (EmpYr + 1) = YEAR(Termination_Date) THEN DATEDIFF(DAY, DATEFROMPARTS(YEAR(Termination_Date), 1, 1), Termination_Date)
                                ELSE (CASE WHEN (EmpYr + 1) %4 = 0 THEN 366 ELSE 365 END) END , 2)

    FROM cte
    WHERE EmpYr < 2022 AND EmpYr < YEAR(Termination_Date)
)
SELECT
    Department, EmpYr, 
    round(avg(Annual_salary), 2) AS avg_annual_salary
FROM cte
GROUP BY Department, EmpYr
ORDER BY 2, 3 DESC;



-- create temporary table from a recursive cte of yearly salary and bonus

SELECT * FROM EmployeeTable;

WITH cte AS
(
    SELECT
        EmployeeID, CONCAT(First_Name, ' ', Surname) AS FullName,
        Office, Office_Type, Department, REPLACE(Job_title, '"', '') AS Job_title, level, Job_Profile,
        Start_Date AS JoinsDate, Termination_Date AS LeavesDate,
        YEAR(Start_Date) AS EmpYr,
        DaysInYear = DATEDIFF(DAY, Start_Date, (CASE WHEN YEAR(Start_Date) = YEAR(Termination_Date) THEN Termination_Date ELSE DATEFROMPARTS(YEAR(Start_Date), 12, 31) END)),
        Salary,
        Annual_salary = ROUND(( CAST(Salary AS FLOAT) / ( 365 + (CASE WHEN YEAR(Start_Date)%4=0 THEN 1 ELSE 0 END))) *
                                DATEDIFF(DAY, Start_Date,
                                (CASE   WHEN YEAR(Start_Date) = YEAR(Termination_Date) THEN Termination_Date
                                        ELSE DATEFROMPARTS(YEAR(Start_Date), 12, 31) END)),2),
        CAST(Bonus_pct AS FLOAT) AS Bonus_pct,
        Currency
    FROM EmployeeTable

    UNION ALL

    SELECT
        EmployeeID, FullName,
        Office, Office_Type, Department, Job_title, level, Job_Profile,
        JoinsDate, LeavesDate,
        EmpYr = EmpYr + 1,
        DaysInYear = CASE
                    WHEN (EmpYr + 1) = YEAR(LeavesDate) THEN DATEDIFF(DAY, DATEFROMPARTS(YEAR(LeavesDate), 1, 1), LeavesDate)
                    ELSE (CASE WHEN (EmpYr + 1)  %4 = 0 THEN 366 ELSE 365 END) END,
        Salary,
        Annual_salary = ROUND((CAST(Salary AS FLOAT) / ( 365 + (CASE WHEN (EmpYr + 1) %4=0 THEN 1 ELSE 0 END))) *
                                CASE
                                WHEN (EmpYr + 1) = YEAR(LeavesDate) THEN DATEDIFF(DAY, DATEFROMPARTS(YEAR(LeavesDate), 1, 1), LeavesDate)
                                ELSE (CASE WHEN (EmpYr + 1) %4 = 0 THEN 366 ELSE 365 END) END , 2),
        Bonus_pct,
        Currency
    FROM cte
    WHERE EmpYr < 2022 AND EmpYr < YEAR(LeavesDate)
),
sq AS
(
SELECT
    EmpYr, EmployeeID, FullName, Office AS OfficeLocation, Office_Type AS OfficeType, Job_title AS JobTitle, level AS ManagementLevel, Job_Profile AS JobProfile, Department,
    DaysInYear, JoinsDate, LeavesDate,
    CASE WHEN EmpYr = YEAR(JoinsDate) THEN 1 ELSE 0 END AS IsJoin,
    CASE WHEN EmpYr = YEAR(LeavesDate) THEN 1 ELSE 0 END AS IsLeave,
    -- convert salary to USD currency
    ROUND(  CASE
                WHEN Currency = 'GBP' THEN Annual_salary * 1.24
                WHEN Currency = 'HKD' THEN Annual_salary * 0.13
                WHEN Currency = 'JPY' THEN Annual_salary * 0.0076
                WHEN Currency = 'NOK' THEN Annual_salary * 0.095
                ELSE Annual_salary END, 2) AS AnnualSalaryUSD,
    -- convert bonus to USD currency
    ROUND(  CASE
                WHEN Currency = 'GBP' THEN Annual_salary * 1.24 * Bonus_pct
                WHEN Currency = 'HKD' THEN Annual_salary * 0.13 * Bonus_pct 
                WHEN Currency = 'JPY' THEN Annual_salary * 0.0076 * Bonus_pct
                WHEN Currency = 'NOK' THEN Annual_salary * 0.095 * Bonus_pct
                ELSE Annual_salary * Bonus_pct END, 2) AS AnnualBonusUSD
FROM cte
)
SELECT *
FROM sq ORDER BY 2, 1;

select * from JobProfileMapping;



SELECT EmployeeID, Age,
                    CASE
                        WHEN Age <= 25 THEN '25 and below'
                        WHEN Age BETWEEN 26 AND 30 THEN '26-30'
                        WHEN Age BETWEEN 31 AND 35 THEN '31-35'
                        WHEN Age BETWEEN 36 AND 40 THEN '36-40'
                        WHEN Age BETWEEN 41 AND 45 THEN '41-45'
                        WHEN Age BETWEEN 46 AND 50 THEN '46-50'
                        WHEN Age BETWEEN 51 AND 55 THEN '51-55'
                        WHEN Age BETWEEN 56 AND 60 THEN '56-60'
                        ELSE '61 and above' END AS AgeGroup
FROM EmployeeTable;

