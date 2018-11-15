# Remove any databases with the same name as the one we're about to create
DROP DATABASE IF EXISTS lerbay_edx;

# Creating a Database named edx
CREATE DATABASE lerbay_edx;

CREATE TABLE lerbay_edx.RECORDS (
    `course_id` VARCHAR(300) NOT NULL,
    `Course_Short_Title` VARCHAR(300) DEFAULT NULL,
    `Course_Long_Title` VARCHAR(300) DEFAULT NULL,
    `userid_DI` VARCHAR(300) NOT NULL,
    `registered` INT DEFAULT NULL,
    `viewed` INT DEFAULT NULL,
    `explored` INT DEFAULT NULL,
    `certified` INT DEFAULT NULL,
    `Country` VARCHAR(300) DEFAULT NULL,
    `LoE_DI` VARCHAR(300) DEFAULT NULL,
	`YoB` VARCHAR(300) DEFAULT NULL,
    `age` INT DEFAULT NULL,
    `gender` CHAR DEFAULT NULL,
    `grade` DOUBLE DEFAULT NULL,
    `nevents` INT DEFAULT NULL,
    `ndays_act` INT DEFAULT NULL,
    `nplay_video` INT DEFAULT NULL,
    `nchapters` INT DEFAULT NULL,
    `nforum_posts` INT DEFAULT NULL,
    `roles`VARCHAR(300) DEFAULT NULL,
    `Incomplete_flag` INT DEFAULT NULL
);

# Truncate is used to remove all records if already the table exists
 TRUNCATE lerbay_edx.RECORDS;

# Loading data in our newly created table from the external .CSV file
# Make sure to change the path to you file location
-- an example with osx shown below:
LOAD DATA LOCAL INFILE '/Users/LeilaErbay/Desktop/LevelNeu2018/Labs/EDX_Pt1/EdX_Enrollment_Data_10pct.csv'

-- an example with windows shown below:
-- LOAD DATA LOCAL INFILE 'C://Users//smit//Desktop//edx_hybrid//dropbox//EdX_Enrollment_Data_10pct.csv'

INTO TABLE lerbay_edx.RECORDS



# Each column in a CSV file is separated by ','
FIELDS TERMINATED BY ',' 

# Each line in a CSV file is separated by '\n'
LINES TERMINATED BY '\r' 

# Because we want to ignore the header of the .csv file. If your .csv does not have a header remove this line
IGNORE 1 LINES;

SELECT   *
	FROM lerbay_edx.RECORDS;
    

# ADD 3 NEW COLUMNS
ALTER TABLE lerbay_edx.RECORDS ADD institution VARCHAR(50);
ALTER TABLE lerbay_edx.RECORDS ADD course_num VARCHAR(50);
ALTER TABLE lerbay_edx.RECORDS ADD course_term VARCHAR(50);


SELECT 
	course_id, 
    SUBSTRING_INDEX( `course_id` , '/', 1) as institution,
    SUBSTRING_INDEX( `course_id` , '/', -1) as term,
    SUBSTRING_INDEX(SUBSTRING_INDEX( `course_id` , '/', 2),'/',-1) as course_num
FROM
		lerbay_edx.RECORDS;
    

SELECT course_id,
	SUBSTRING_INDEX(SUBSTRING_INDEX( `course_id` , '/', 2),'/',-1) as course_num
FROM
	lerbay_edx.RECORDS;
	
    
SET SQL_SAFE_UPDATES = 0;
    
UPDATE lerbay_edx.RECORDS
	SET institution = SUBSTRING_INDEX( `course_id` , '/', 1 );

    
UPDATE lerbay_edx.RECORDS
	SET course_num =SUBSTRING_INDEX(SUBSTRING_INDEX( `course_id` , '/', 2),'/',-1) ;

UPDATE lerbay_edx.RECORDS 
	SET course_term = SUBSTRING_INDEX( `course_id` , '/', -1) ;
    

SELECT * FROM lerbay_edx.RECORDS LIMIT 100;

SELECT DISTINCT institution, course_num, course_term 
from 
	lerbay_edx.RECORDS;

SELECT DISTINCT Country 
from 
	lerbay_edx.RECORDS;
    
SELECT DISTINCT Country
FROM
	lerbay_edx.RECORDS
WHERE
	LoE_ID = "Doctorate";
    

SELECT COUNT(DISTINCT course_id) as courses
FROM
	lerbay_edx.RECORDS;


SELECT COUNT(DISTINCT user_id) as German_users
FROM
		lerbay_edx.RECORDS
WHERE 
	Country = 'Germany';
    
SELECT COUNT(DISTINCT user_id) as USA_users
FROM
		lerbay_edx.RECORDS
WHERE
	Country = 'United States';

SELECT COUNT(distinct user_id)
FROM
	lerbay_edx.RECORDS;

DROP TABLE IF EXISTS lerbay_edx.Course_Users;

#### COURSES TABLE
CREATE TABLE lerbay_edx.Courses
SELECT  DISTINCT course_id, institution, course_num,  course_term, Course_Short_Title, Course_Long_Title
FROM
	lerbay_edx.RECORDS;
    
SELECT * 
FROM
	lerbay_edx.Courses;



CREATE TABLE lerbay_edx.Users
SELECT  DISTINCT user_id, YoB, Country, LoE_ID, age, gender
FROM 
	lerbay_edx.RECORDS;
    
SELECT COUNT(DISTINCT user_id, YoB, Country, LoE_ID, age, gender )
FROM
	lerbay_edx.Users;


    
CREATE TABLE lerbay_edx.Course_Users
SELECT   course_id, course_num, institution, course_term, user_id, grade,  registered, viewed, explored, certified, nevents, ndays_act, nplay_video, 
									nchapters, nforum_posts, roles, incomplete_flag
FROM 
	lerbay_edx.RECORDS;
 
 
 CREATE TABLE lerbay_edx.Updated_Users
SELECT user_id, Country, LoE_ID, YoB, Age, gender 
FROM 
	lerbay_edx.RECORDS
group by user_id;


ALTER TABLE lerbay_edx.Courses
ADD PRIMARY KEY (course_id);

ALTER TABLE lerbay_edx.Updated_Users
ADD PRIMARY KEY (user_id);

ALTER TABLE lerbay_edx.Course_Users
ADD PRIMARY KEY(course_id, user_id);

SELECT DISTINCT course_id
	FROM
		lerbay_edx.RECORDS;
        

### PART 2 #######################
### Q1
SELECT COUNT(DISTINCT user_id)
FROM
	lerbay_edx.Course_Users
WHERE
	course_id = '
MITx/6.00x/2012_Fall' and registered = 1; 			### 6804

SELECT COUNT(DISTINCT user_id)
FROM
	lerbay_edx.Course_Users
WHERE
	course_id = '
MITx/6.00x/2013_Spring' and registered = 1; 	### 5775

SELECT  AVG(grade), course_id
FROM lerbay_edx.Course_Users
GROUP BY course_id;



SELECT  AVG(grade), course_num
FROM lerbay_edx.Course_Users
WHERE  grade > 0 AND grade IS NOT NULL
GROUP BY course_num;

SELECT  AVG(grade), course_id
FROM lerbay_edx.Course_Users
WHERE  grade > 0 AND grade IS NOT NULL
GROUP BY course_id;

SELECT COUNT(user_id)
FROM 
	lerbay_edx.Course_Users
WHERE  
	course_num= '6.00x' and registered=1 
	and course_term = '2013_Spring' 
    and user_id in (SELECT user_id
							FROM 
								lerbay_edx.Course_Users
							WHERE  
								course_num= '6.00x' and registered=1 
								and course_term = '2012_Fall') ;
                
#### in (QUERY)

SELECT COUNT(user_id) as num_students , course_id
FROM 
	lerbay_edx.Course_Users
GROUP BY course_id
ORDER BY  num_students DESC;

### PART 3
SELECT COUNT(user_id) as num_students, Course_Long_Title
FROM
	lerbay_edx.Course_Users
INNER JOIN 
	lerbay_edx.Courses 
ON 
	lerbay_edx.Course_Users.course_id = lerbay_edx.Courses.course_id
GROUP BY Course_Long_Title
ORDER BY  num_students DESC;

SELECT COUNT(user_id) as num_students, lerbay_edx.Course_Users.course_id
FROM
	lerbay_edx.Course_Users
INNER JOIN 
	lerbay_edx.Courses 
ON lerbay_edx.Course_Users.course_id = lerbay_edx.Courses.course_id
GROUP BY course_id
HAVING COUNT(user_id) > 4000
ORDER BY  num_students DESC;


## PART 4: HARDER	
SELECT  c_u.course_num, 
				totals.Course_Long_Title, COUNT(user_id) as num_students, 
				totals.total as total_students, COUNT(user_id)/totals.total as stat 
FROM lerbay_edx.Course_Users AS c_u
INNER JOIN(
	SELECT c.course_num, Course_Long_Title, COUNT(user_id) as total
	FROM
		lerbay_edx.Course_Users as cu
	INNER JOIN 
		lerbay_edx.Courses  as c
	ON cu.course_id = c.course_id
	GROUP BY Course_Long_Title
) as totals
ON totals.course_num = c_u.course_num
WHERE 
	c_u.registered = 1 AND
    c_u.viewed = 1 AND
	c_u.explored = 1 AND
    c_u.certified = 1
GROUP BY c_u.course_num
ORDER BY stat DESC;


### PART 4: EASIER
SELECT  c_u.course_id, COUNT(user_id) as num_students, 
				totals.total as total_students, COUNT(user_id)/totals.total as stat 
FROM lerbay_edx.Course_Users AS c_u
INNER JOIN(
	SELECT c.course_id, COUNT(user_id) as total
	FROM
		lerbay_edx.Course_Users as cu
	INNER JOIN 
		lerbay_edx.Courses  as c
	ON cu.course_id = c.course_id
	GROUP BY c.course_id
) as totals
ON totals.course_id = c_u.course_id
WHERE 
	c_u.registered = 1 AND
    c_u.viewed = 1 AND
	c_u.explored = 1 AND
    c_u.certified = 1
GROUP BY c_u.course_id
ORDER BY stat DESC;


SELECT  c_u.course_id, COUNT(user_id) as num_students, 
				totals.total as total_students, COUNT(user_id)/totals.total as stat 
FROM lerbay_edx.Course_Users AS c_u
INNER JOIN(
	SELECT c.course_id, COUNT(user_id) as total
	FROM
		lerbay_edx.Course_Users as cu
	INNER JOIN 
		lerbay_edx.Courses  as c
	ON cu.course_id = c.course_id
	GROUP BY c.course_id
) as totals
ON totals.course_id = c_u.course_id
WHERE 
	c_u.registered = 1 AND
    (
    c_u.viewed = 1 OR c_u.viewed = 0 OR
	c_u.explored = 1 OR c_u.explored = 0 OR
    c_u.certified = 1 OR c_u.certified = 0
    )
GROUP BY c_u.course_id
ORDER BY stat DESC;



## PART 4: HARDER	
SELECT  c_u.course_num, 
				totals.Course_Long_Title, COUNT(user_id) as num_students, 
				totals.total as total_students, COUNT(user_id)/totals.total as stat 
FROM lerbay_edx.Course_Users AS c_u
INNER JOIN(
	SELECT c.course_num, Course_Long_Title, COUNT(user_id) as total
	FROM
		lerbay_edx.Course_Users as cu
	INNER JOIN 
		lerbay_edx.Courses  as c
	ON cu.course_id = c.course_id
	GROUP BY Course_Long_Title
) as totals
ON totals.course_num = c_u.course_num
WHERE 
	c_u.registered = 1 AND
     (
    c_u.viewed = 1 OR c_u.viewed = 0 OR
	c_u.explored = 1 OR c_u.explored = 0 OR
    c_u.certified = 1 OR c_u.certified = 0
    )
GROUP BY c_u.course_num
ORDER BY stat DESC;

## PART 5

SELECT 
	c.institution, c.course_num, COUNT(DISTINCT c_u.user_id)
FROM 
	lerbay_edx.Course_Users AS c_u
INNER JOIN 
	lerbay_edx.Courses AS c
ON 
	c.course_id = c_u.course_id
WHERE 
	c.institution = '\nHarvardX'
GROUP BY 
	c.course_num
HAVING 
	COUNT(c_u.user_id) >4000;


SELECT 
	c_u.course_id, COUNT(DISTINCT c_u.user_id) as num
FROM 
	lerbay_edx.Course_Users AS c_u
WHERE 
	c_u.course_id LIKE '%harvard%' 
HAVING 
	num >4000;

####?????

SELECT 
	COUNT(totals.user_id)
FROM 
	(SELECT 
		 user_id, SUM(registered)
	FROM
		lerbay_edx.Course_Users
	WHERE
		registered = 1
	GROUP BY
		user_id
	HAVING
		SUM(registered) >3
) AS totals;




    

SELECT 
	u.Country, COUNT(u.user_id)
FROM 
	lerbay_edx.Users AS u
WHERE u.Country is NOT NULL
GROUP BY u.Country
ORDER BY u.Country ASC;

SELECT 
	u.Country, AVG(cu.grade) as ave
FROM
	lerbay_edx.Users AS u
INNER JOIN
	lerbay_edx.Course_Users as cu
ON 
	cu.user_id = u.user_id
WHERE 
	u.Country is NOT NULL AND
	cu.certified = 1
GROUP BY 
	u.Country
ORDER BY 
	ave DESC;




### GRADES PER INSTITUTION
## AVE G

SELECT 
	u.Country, AVG(harv_avg.grade) as ave
FROM
	lerbay_edx.Users as u
INNER JOIN
    (
		SELECT 
			cu.user_id,  cu.grade   as grade
		FROM
			lerbay_edx.Courses as c
		INNER JOIN
			lerbay_edx.Course_Users as cu
		ON
			c.course_id=cu.course_id
		WHERE
			c.institution = '\nHarvardX' AND
            cu.certified =1
	) as harv_avg
ON
	u.user_id = harv_avg.user_id
GROUP BY 
	u.Country
ORDER BY
ave DESC;
    
    
SELECT 
	u.Country, AVG(harv_avg.grade) as ave
FROM
	lerbay_edx.Users as u
INNER JOIN
    (
		SELECT 
			cu.user_id,  cu.grade   as grade
		FROM
			lerbay_edx.Courses as c
		INNER JOIN
			lerbay_edx.Course_Users as cu
		ON
			c.course_id=cu.course_id
		WHERE
			c.institution = '\nMITx' AND
            cu.certified =1
	) as harv_avg
ON
	u.user_id = harv_avg.user_id
GROUP BY 
	u.Country
ORDER BY ave DESC;

	
    