rm(list=ls())

# Load libraires
libraries = c("ggpubr", "deSolve","dplyr","Bhat","ggExtra","reshape2","gridExtra","ggpubr","openxlsx","matrixStats","tidyquant","sp","RColorBrewer","ggplot2","png","grid","dplyr", 
              "magrittr","scales","tidyr")#)#
for(x in libraries) { library(x,character.only=TRUE,warn.conflicts=FALSE) }



main_folder = "../Result/"
(save_folder= paste0(main_folder,"Fitting_ode/"))

#time interval for data-fitting
startday.est = as.Date("2020-07-16")
finalday = as.Date("2021-03-04")


#serial interval
serial_esti = c(4.8,2.3)
xlabel_epi = "Confirmed date (month/day)"

# dt for ode


# figure options
ymax_vector = 1300
ydiff_vector = 300
xdiff = "14 days"
cols <- c("Local" = "gray60","Imported"="blue")
x_fig = seq(as.Date(startday.est),finalday,by="7 day")

#data load
df_case= read.xlsx("../../Data/dfone_korea.xlsx",sheet="Sheet 1",detectDates=TRUE,colNames=T,startRow=1)



area_name = "Korea"
(pop0=51791764) 
(total.case = df_case$cum_case[1]-df_case$i.local[1])
(i0.est = df_case%>%filter(day==startday.est)%>%dplyr::select(i.local)%>%as.numeric())
p.ode = data.frame(alpha1=1/5,alpha2=1/3,
                   gamma=1/14,N=pop0,w=0,alpha3=1/5)
init0 = c(p.ode$N-i0.est,0,i0.est,0,0,i0.est,0)
(pp = p.ode)
dtts=1




date1=startday.est
daten =finalday
(tmax = as.numeric(finalday-startday.est))


endday.est.1st = as.Date("2020-08-06")
(control_date = c(endday.est.1st+1,as.Date("2020-08-16"),as.Date("2020-08-23"),
                  as.Date("2020-09-14"),as.Date("2020-09-28"),as.Date("2020-10-12"),
                  as.Date("2020-11-05"),
                  as.Date("2020-12-08"),as.Date("2020-12-23"),as.Date("2021-02-01"),as.Date("2021-02-15"),finalday+1))


ft_sc_df = function(t1,tn,control_date){
  sc_df = data.frame(date=seq(t1,tn ,by="day"))%>%
    mutate(time=1:n())
  sc_df%<>%mutate(control_t=1)
  for(kk in 1:length(control_date)){
    sc_df$control_t[which(sc_df$date>=control_date[kk])] =  sc_df$control_t[which(sc_df$date>=control_date[kk])]+1
  }
  return(sc_df)
}

sc_df = ft_sc_df(date1,daten,control_date)
(n_types = sc_df%>%group_by(control_t)%>%summarize(ngp=n())%>%.$ngp)
(ngroup = length(n_types))
#write.xlsx(sc_df,"sc_df.xlsx")
(sc_time_interval = data.frame(interval = 1:ngroup,
                               start.d=c(date1,control_date[1:(ngroup-1)]),
                               end.d=(control_date-1)))


v.w = rep(0,ngroup)
(v.a2=rep(p.ode$alpha2,ngroup))#c(1/tb_diag$diag))
(v.gam =rep(p.ode$gamma,ngroup))# c(1/tb_hosp$hosp))
ft_time_pp =  function(control_date,endday.est,v.w,v.a2,v.gam){
  
  (group = length(control_date))
  
  (p.set = data.frame(matrix(rep(as.numeric(p.ode),each=group),nrow=group)))
  colnames(p.set) =colnames(p.ode)
  #p.set%<>%mutate(tc=tc,n_types)
  (p.sc = p.set)
  p.sc$w = v.w
  p.sc$alpha2 = v.a2
  p.sc$gamma = v.gam
  p.sc
  
  return(p.sc)
  
}
p.sc = ft_time_pp(control_date,endday.est,v.w,v.a2,v.gam)

#여기서부터
(control_date)
source("Paper_functions_ode.R")
source("Paper_function_fitting.R")

dfone = df_case

(filenames_para = paste0(save_folder,"paper_para_table_dfp_",area_name,".xlsx"))
filenames_odefit = paste0(save_folder,"paper_ode_fit_",area_name,".xlsx")
filenames_odefit_ci = paste0(save_folder,"paper_ode_fit_ci_",area_name,".xlsx")


# up to now ---------------------------------------------------------------

nowcalc = 2
results_list =  ft_ode_fit_dfp_result(kk,nowcalc,p.sc,dfone,control_date,sc_df,sc_time_interval,
                                      n_types,ngroup,init0,p.ode,total.case,dtts)

comp_df = results_list[[1]]
para_tb = results_list[[2]]

fit_ode_result=comp_df
(init_last = comp_df%>%dplyr::select(S,E,I,H,R,CI,HI)%>%tail(1)%>%as.numeric())
(fig_tmax = finalday)
legend_posi1=c(0.3,0.9);legend_posi2=c(0.5,0.9)

diff = "7 day"
x_startday = startday.est
(cymax=ymax_vector)##round(max(comp_df$CI)/1000)*1000)
source("paper_ft_fig_ode_comp.R")
titles=area_name
fig = ft_fig_ode_comp(comp_df,x_fig,x_startday,fig_tmax,legend_posi1,xlabel_epi,cymax,titles)
plot_fit <-grid.arrange(fig[[1]],fig[[2]],nrow=2)


fig1_epi = fig[[1]]
fig1_cumul = fig[[2]]


# Forecasting -------------------------------------------------------------


