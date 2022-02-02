rm(list=ls())



source("Step1_Data_processing.R")

(save_folder= paste0(main_folder,"Fitting_ode/"))
# ode fitting-dfp ----------------------------------------------------------

source("ft_df_one.R")
source("ft_control_dates_0305.R")
source("ft_sc_df.R")
source("ft_fig_ode_comp.R")
source("run_ft_ode.R")

source("mle_interval_dfp.R")

source("ft_ode_fit_dfp_result.R")
source("ft_ode_add_calc.R")
# calc for dfp with ci
source("ft_odeci_calc.R")
# Figure for ci
source("ft_fig_odefit_ci.R")
#figure -comparison between cases and estimates

# Input -------------------------------------------------------------------
library("tictoc")

tic()
## nowcalc =1 :지금 fitting, 2: excel load
nowcalc = 2
#ci_calc_decision=1이면 ci 계산함, 1아니면 ci 안함
ci_calc_decision=2
## nowcalc_ci =1 :지금 ci 추가계산함, 2: excel load
nowcalc_ci =2
#samplen number
nsample_b = 1000
fig1_epi = list()
fig1_cumul = list()

# ci 
fig1_epi_ci = list()
fig1_cumul_ci = list()
# sample number for ci
#length(select_ids)
for (kk in 1:length(select_ids)){

  print(kk)
    (id = select_ids[kk])
    (city_names = city_tb$city[id])
    eng_city_name = names_select_ids[kk]
    
    # file name
    (filenames_para = paste0(save_folder,"para_table_dfp_",city_names,".xlsx"))
    filenames_odefit = paste0(save_folder,"ode_fit_",city_names,".xlsx")
    filenames_odefit_ci = paste0(save_folder,"ode_fit_ci_",city_names,".xlsx")
    
    
    source("Step_ODE_run.R")
    
  (control_date)
  # Fitting -----------------------------------------------------------------
  
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
        (cymax=ymax_vector[kk])##round(max(comp_df$CI)/1000)*1000)
  fig = ft_fig_ode_comp(comp_df,x_fig,x_startday,fig_tmax,legend_posi1,case_type,xlabel_epi,cymax,titles=eng_city_name)
  
  plot_fit <-grid.arrange(fig[[1]],fig[[2]],nrow=2)
  
  fig1_epi[[kk]] = fig[[1]]
  fig1_cumul[[kk]] = fig[[2]]
  
  
    if (ci_calc_decision ==1){
      
           if (nowcalc_ci==1){
              (max_T = nrow(sc_df))
            
            ### ci 구하기 위해서는 nowcalc_ci==1
              comp_df_beta = ft_odeci_calc(kk,nowcalc_ci,p.sc,dfone,control_date,comp_df,nsample_b,date1,
                                   n_types,ngroup,init0,total.case,dtts,para_tb,
                                   max_T,filenames_odefit_ci)
      
           } else {
              comp_df_beta = read.xlsx(filenames_odefit_ci,sheet="Sheet 1",detectDates=TRUE,colNames=T,startRow=1)
           }
      # Figure-CI ---------------------------------------------------------------
              (fig_tmax = finalday)
              legend_posi1=c(0.3,0.9);legend_posi2=c(0.5,0.9)
              
              diff = "7 day"
              x_startday = startday.est
              
              ylabels_v =  c("New cases","Cumulative cases")
              title_name = eng_city_name
              fontsize=15
              legend_posi1=c(0.2,0.7)
      fig_ci_dfp = ft_fig_odefit_ci(comp_df_beta,fig_tmax,legend_posi1,
                                  diff,x_startday,ylabels_v,title_name,fontsize,x_fig)
      
      fig1_epi_ci[[kk]] = fig_ci_dfp[[1]]
      fig1_cumul_ci[[kk]] = fig_ci_dfp[[2]]
      
    } else {
      
    }
    
  
}


# Save figure -------------------------------------------------------------
source("save_ft_fig_epi_cumul.R")

if (ci_calc_decision ==1){
source("save_ft_fig_epi_cumul_ci.R")

}
toc()
