SELECT * FROM agriplus.weather_data;

select date(timestamp), avg(temp_avg), min(temp_avg), max(temp_avg), count(if(sol_rad_avg>0,1,null))/4
  from weather_data
  group by date(timestamp);