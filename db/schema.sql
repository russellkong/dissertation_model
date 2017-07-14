use agriplus;

create table app_user (
	id BIGINT NOT NULL AUTO_INCREMENT,
   name VARCHAR(30) NOT NULL,
   email varchar(80) not null,
   age  INTEGER,
   salary REAL,
   PRIMARY KEY (id)
);
   
/* Populate USER Table */
INSERT INTO APP_USER(name,email,age,salary)
VALUES ('Sam','sam@agriplus.hk',30,70000);
   
INSERT INTO APP_USER(name,email,age,salary)
VALUES ('Tom','tom@agriplus.hk',40,50000);
 
commit;

select * from app_user;

create table weather_data (
	id BIGINT NOT NULL AUTO_INCREMENT,
station varchar(30),
RECORD_DT datetime,
RECORD	integer,
WDIR_Avg double,
	WSPD_Max double,
	WSPD_Avg double,
	TEMP_Avg double,
	PRESS_Avg double,
	RH_Avg double,
	DEWP_Avg double,
	T109_C_Avg double,
	Rain_mm_Tot double,
	SOL_RAD_Avg double,
	BattV_Avg double,
   PRIMARY KEY (id)
);
alter table weather_data drop column record_time;

select * from weather_data;
select str_to_date('01/01/2017 00:00', '%d/%m/%Y %H:%i');
  select str_to_date(date_str,"%d%m%Y %h%i"),date_str from weather_data;
  update weather_data set timestamp=str_to_date(date_str,'%d/%m/%Y %H:%i');
  update weather_data set station="farm";
  