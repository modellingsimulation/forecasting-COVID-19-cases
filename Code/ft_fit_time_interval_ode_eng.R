(id = select_ids[kk])
(city_names = city_tb$city[id])

if(city_names=="Honam area"){
  startday.est = as.Date("2020-07-16")
  endday.est.1st = as.Date("2020-08-06")
  
} else if(city_names=="Gyeongbuk area"){
  startday.est = as.Date("2020-08-01")
  endday.est.1st = as.Date("2020-08-22")
  
} else if(city_names=="Chungcheon area"){
  startday.est = as.Date("2020-07-16")
  endday.est.1st = as.Date("2020-08-06")
  
} else if(city_names=="Gyeongnam area"){
  startday.est = as.Date("2020-07-18")
  endday.est.1st = as.Date("2020-08-06")
  
} else if(city_names=="Non-Capital area"){
  startday.est = as.Date("2020-07-16")
  endday.est.1st = as.Date("2020-08-06")
  
} else if(city_names=="Gangwon province"){
  startday.est = as.Date("2020-07-31")
  endday.est.1st = as.Date("2020-08-22")
  
} else if(city_names=="Capital area"){
  startday.est = as.Date("2020-07-16")
  endday.est.1st = as.Date("2020-08-06")
  
} else {
  startday.est = as.Date("2020-07-16")
  endday.est.1st = as.Date("2020-08-06")
}
