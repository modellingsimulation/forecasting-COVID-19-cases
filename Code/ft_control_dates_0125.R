ft_control_date = function(city_names,endday.est.1st,daten){
  if (city_names=="Korea"){

    (control_date = c(endday.est.1st+1,as.Date("2020-08-16"),as.Date("2020-08-23"),
                      as.Date("2020-09-14"),as.Date("2020-09-28"),as.Date("2020-10-12"),
                      as.Date("2020-11-05"),
                      as.Date("2020-12-08"),as.Date("2020-12-23"),as.Date("2021-02-01"),as.Date("2021-02-15"),daten+1))
    

  } else if (city_names=="Honam area"){

    (control_date = c(endday.est.1st+1,as.Date("2020-08-23"),
                      as.Date("2020-09-28"),as.Date("2020-11-19"),as.Date("2020-12-23"),
                      as.Date("2021-02-15"),daten+1))
    
    
  } else if (city_names=="Gyeongbuk area"){
    
    (control_date = c(endday.est.1st+1, as.Date("2020-09-28"),as.Date("2020-11-05"),
                      as.Date("2020-12-01"),as.Date("2020-12-23"),
                      as.Date("2021-02-01"),as.Date("2021-02-15"),daten+1))
    

  } else if (city_names=="Gyeongnam area"){
    (control_date = c(endday.est.1st+1, as.Date("2020-08-23"), as.Date("2020-09-28"),as.Date("2020-11-05"),
                      as.Date("2020-11-27"),
                      as.Date("2020-12-15"),as.Date("2021-02-01"),as.Date("2021-02-15"),daten+1))
    

  } else if (city_names=="Capital area"){
    
    (control_date = c(endday.est.1st+1,as.Date("2020-08-16"),as.Date("2020-08-23"),
                      as.Date("2020-09-14"),as.Date("2020-09-28"),as.Date("2020-10-12"),
                      as.Date("2020-11-19"),
                      as.Date("2020-12-08"),as.Date("2020-12-23"),as.Date("2021-02-01"),as.Date("2021-02-15"),daten+1))
    
    
  } else if (city_names=="Non-Capital area"){
    
    (control_date = c(endday.est.1st+1,as.Date("2020-08-23"),
                      as.Date("2020-09-28"),as.Date("2020-10-12"),as.Date("2020-11-05"),
                      as.Date("2020-12-15"),
                      as.Date("2021-02-01"),as.Date("2021-02-15"),daten+1))
    
  } else if (city_names=="Chungcheon area"){
    (control_date = c(endday.est.1st+1,as.Date("2020-08-23"),
                      as.Date("2020-09-28"),as.Date("2020-11-05"),
                      as.Date("2020-12-23"),as.Date("2021-02-01"),as.Date("2021-02-15"),daten+1))

    
  } else if (city_names=="Gangwon province"){

    (control_date = c(endday.est.1st+1,
                      as.Date("2020-09-28"),
                      as.Date("2020-11-10"),
                      as.Date("2020-12-23"),as.Date("2021-02-15"),daten+1))
    
  } 
 
    control_date_final = control_date
  
  return(control_date_final)
}