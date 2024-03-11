# Human Resources Analytics using SQL and Microsoft Power BI
## Description
The objectives of this project are to analyze human resources dataset and build an interactive visualization (dashboard) using SQL and Microsoft Power BI. Below is the generated dashboard of this project, which contains pages that consist of :
* Employees Diversity
* Retention Analysis
* Salary Analysis
* Salary Details (sub-page of Salary Analysis)

Click [here](https://app.powerbi.com/view?r=eyJrIjoiZGU0NmI5MTAtZTFiYy00NGY2LWExYjEtNDkzN2YzMDY4ZTlhIiwidCI6IjA0NmI4YzU4LWFhNTEtNGY2NC04M2RlLWU1NjczZTY0NmY4NiIsImMiOjEwfQ%3D%3D) to go to the dashboard.
<br/>

This project dataset consists of the dataset from Kaggle ([The Company](https://www.kaggle.com/datasets/koluit/human-resource-data-set-the-company)) in `.txt` format (imported as the database tables in SQL Server Management Studio or SSMS), and generated dataset from SQL query. All of the data in this project are connected from SQL Server Management Studio to Power BI. Here are the dataset that was imported from the original source on Kaggle to SSMS.
* CompanyData.txt (`EmployeeTable`)
* Diversity.txt (`EmployeeDiversity`)

## Exploratory Data Analysis in SQL

There are several pre-visualization analyses that were conducted using SQL query to get more understanding of the dataset. Some of it will be the sources of Power BI dataset.

- Employee's age distribution (named `AgeGroup` dataset as a Power BI data source)
    
    ```sql
    SELECT EmployeeID, 
                        CASE
                            WHEN Age <= 20 THEN '20 and below'
                            WHEN Age BETWEEN 21 AND 30 THEN '21-30'
                            WHEN Age BETWEEN 31 AND 40 THEN '31-40'
                            WHEN Age BETWEEN 41 AND 50 THEN '41-50'
                            WHEN Age BETWEEN 51 AND 60 THEN '51-60'
                            ELSE '61 and above' END AS AgeGroup
    FROM EmployeeTable;
    ```
    
- Number of new employee of by year and office
    
    ```sql
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
    ```
    
- Number of new employees by year and department
    
    ```sql
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
    ```
    
- Yearly employees retention rate
    
    ```sql
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
    ```
    
- Yearly employees rate (highest & lowest) by department
    
    ```sql
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
    ```
    
- Employee’s yearly salary, bonus and employee details (named `YearlyEmployeeInformations` dataset as a Power BI data source)
    
    ```sql
    -- Recursive CTE
    
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
    ```
    

## Power BI Data Sources

Dataset from **SSMS database tables**:

- `EmployeeTable`
- `EmployeeDiversity`

Dataset were generated from **SQL query**:

- `AgeGroup`
- `YearlyEmployeeInformations`

## Power BI Measures (DAX)

There are four-grouped Measures that were created throughout analyzing and visualizing process in this project (Power BI part), which includes:

### `_HeadCount` Group

- `Head Count` (number of employee in the specific period)
    
    ```
    Head Count = COUNT(YearlyEmployeeInformations[EmployeeID])
    ```
    
- `Male` (number of male employee in the specific period)
    
    ```
    Male = 
    CALCULATE([Head Count];
    FILTER(EmployeeDiversity; EmployeeDiversity[Gender] = "Male"))
    ```
    
- `Female` (number of female employee in the specific period)
    
    ```
    Female = 
    CALCULATE([Head Count];
    FILTER(EmployeeDiversity; EmployeeDiversity[Gender] = "Female"))
    ```
    
- `Last Year Head Count` (number of employee in the last specific period)
    
    ```
    Last Year Head Count = 
    CALCULATE([Head Count]; OFFSET(-1;ALL(YearlyEmployeeInformations[EmpYr])))
    ```
    
- `YoY Head Count` (year-on-year, number of employee)
    
    ```
    YoY Head Count = ([Head Count] / [Last Year Head Count])-1
    ```
    
- `YoY Male` (year-on-year, number of male employee)
    
    ```
    YoY Male = 
    CALCULATE([YoY Head Count];
    FILTER(EmployeeDiversity; EmployeeDiversity[Gender] = "Male"))
    ```
    
- `YoY Female` (year-on-year, number of female employee)
    
    ```
    YoY Female = 
    CALCULATE([YoY Head Count];
    FILTER(EmployeeDiversity; EmployeeDiversity[Gender] = "Female"))
    ```
    
- `Employees with Disability` (number of male employee with disability)
    
    ```
    Employees with Disability = 
    CALCULATE(
        COUNT(YearlyEmployeeInformations[EmployeeID]);
        FILTER(ALLSELECTED(EmployeeDiversity[Disability]); 
    		EmployeeDiversity[Disability]=1))
    ```
    
- `Veteran Employees` (number of veteran employee)
    
    ```
    Veteran Employees = 
    CALCULATE(
        COUNT(YearlyEmployeeInformations[EmployeeID]);
        FILTER(ALLSELECTED(EmployeeDiversity[Veteran]);
    		EmployeeDiversity[Veteran]=1))
    ```
    

### `_HeadDistinctCount` Group

- `Head Count (Distinct)` (number of unique employee in the specific period)
    
    ```
    Head Count (Distinct) = DISTINCTCOUNT(YearlyEmployeeInformations[EmployeeID])
    ```
    
- `Male (Distinct)` (number of unique male employee in the specific period)
    
    ```
    Male (Distinct) = 
    CALCULATE([Head Count (Distinct)];
    FILTER(EmployeeDiversity; EmployeeDiversity[Gender] = "Male"))
    ```
    
- `Female (Distinct)` (number of unique female employee in the specific period)
    
    ```
    Female (Distinct) = 
    CALCULATE([Head Count (Distinct)];
    FILTER(EmployeeDiversity; EmployeeDiversity[Gender] = "Female"))
    ```
    

### `_RetentionRate` Group

- `JoinedEmp` (number of new employees)
    
    ```
    JoinedEmp = SUM(YearlyEmployeeInformations[IsJoin])
    ```
    
- `CumulativeJoin` (cumulative number of new employees)
    
    ```
    CumulativeJoin = 
    CALCULATE([JoinedEmp];
    FILTER(ALL(YearlyEmployeeInformations[EmpYr]);
    YearlyEmployeeInformations[EmpYr] <= MAX(YearlyEmployeeInformations[EmpYr])))
    ```
    
- `LeftEmp` (number of left employees)
    
    ```
    LeftEmp = SUM(YearlyEmployeeInformations[IsLeave])
    ```
    
- `CumulativeLeave` (cumulative number of left employees)
    
    ```
    CumulativeLeave = 
    CALCULATE([LeftEmp];
    FILTER(ALL(YearlyEmployeeInformations[EmpYr]);
    YearlyEmployeeInformations[EmpYr] <= MAX(YearlyEmployeeInformations[EmpYr])))
    ```
    
- `Leave` (negative number of the left employees)
    
    ```
    Leave = -1 * [LeftEmp]
    ```
    
- `EndEmp` (number of employees at the end of the period)
    
    ```
    EndEmp = [CumulativeJoin] - [CumulativeLeave]
    ```
    
- `StartEmp` (number of employees at the beginning of the period)
    
    ```
    StartEmp = 
    IF(ISBLANK(
    CALCULATE([EndEmp]; OFFSET(-1;ALLSELECTED(YearlyEmployeeInformations[EmpYr]))));
    0;
    CALCULATE([EndEmp]; OFFSET(-1;ALLSELECTED(YearlyEmployeeInformations[EmpYr]))))
    ```
    
- `RetentionRate` (retention rate)
    
    ```
    RetentionRate = 
    IFERROR(
    IF(
    ([CumulativeJoin] - [CumulativeLeave] - SUM(YearlyEmployeeInformations[IsJoin])) / [StartEmp] < 0;0;
    ([CumulativeJoin] - [CumulativeLeave] - SUM(YearlyEmployeeInformations[IsJoin])) / [StartEmp]);
    (SUM(YearlyEmployeeInformations[IsJoin])) / [JoinedEmp])
    ```
    

### `_Salary&Bonus` Group

- `Total Salary` (total annual salary)
    
    ```
    Total Salary = SUM(YearlyEmployeeInformations[AnnualSalaryUSD])
    ```
    
- `Total Bonus` (total annual bonus)
    
    ```
    Total Bonus = SUM(YearlyEmployeeInformations[AnnualBonusUSD])
    ```
    
- `Avg Salary Per Emp` (average annual salary per employee)
    
    ```
    Avg Salary Per Emp = [Total Salary]/[Head Count]
    ```
    
- `Avg Bonus Per Emp` (average annual bonus per employee)
    
    ```
    Avg Bonus Per Emp = [Total Bonus]/[Head Count]
    ```
