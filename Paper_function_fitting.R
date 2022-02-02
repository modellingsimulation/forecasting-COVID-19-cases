



ft_ode_fit_dfp_result = function(kk,nowcalc,p.sc,dfone,control_date,
                                 sc_df,sc_time_interval,n_types,ngroup,init0,p.ode,
                                 total.case,dtts) {
  
  
  # dfp fitting -------------------------------------------------------------  
  if (nowcalc==1){
    
    
    output_dfp=mle_interval_dfp(control_date,n_types,init0,p.sc)
    ode_out_dfp = do.call("rbind",output_dfp)
    
    interval_fit = ode_out_dfp%>%group_by(Group)%>%slice(1)%>%ungroup%>%rename(interval=Group)
    
    final_interval_ft = interval_fit%>%
      mutate(mean_Rt=betas/v.a2)%>%
      mutate(reduction=c(0,(1-betas[2:length(betas)]/betas[1]))*100)%>%
      dplyr::select(-c(S,E,I,H,R,CI,HI,esti_c,esti_o))%>%
      mutate(start.d=c(date1,control_date[1:(ngroup-1)]),end.d=control_date-1)%>%
      mutate(esti_beta=betas,diag=v.a2,hosp=v.gam)%>%
      mutate(mean_Rt_low=beta_low/v.a2,mean_Rt_upp=beta_upp/v.a2)
    
    betas=interval_fit$betas
    
    (para_tb = cbind(p.sc,final_interval_ft))
    
    
    # saving dfp fitting results -------------------------------------------------------------
    # file name
    
    write.xlsx(para_tb,filenames_para)
    
    esti_out = ode_interval(control_date,n_types,init0,p.sc,esti_betas=betas)
    #esti_out 
    all_esti_out = do.call("rbind",esti_out)%>%mutate(date=date1+0:(n()-1))
    #all_esti_out
    
    comp_df_temp <-all_esti_out%>%
      mutate(esti_cic = cumsum(esti_c))%>%
      left_join(dfone%>%rename(date=day)%>%
                  dplyr::select(-c(time)),by=c("date"))%>%
      mutate(i.local=replace_na(i.local,0),
             cum_case=replace_na(cum_case,0))%>%mutate(esti_cic=esti_cic+total.case)
    
    comp_df_temp%>%head
    
    
    comp_df = comp_df_temp%>%left_join(sc_df,by=c("time","date"))%>%
      left_join(para_tb%>%mutate(control_t=1:n())%>%
                  dplyr::select(c(alpha1,alpha2,gamma,N,w,alpha3,mean_Rt,control_t)),
                by="control_t")%>%mutate(rt_ode = beta*S/(alpha2*N))
    write.xlsx(comp_df,filenames_odefit)
    
    
    
  } else {
    comp_df = read.xlsx(filenames_odefit,sheet="Sheet 1",detectDates=TRUE,colNames=T,startRow=1)
    
    para_tb = read.xlsx(filenames_para,sheet="Sheet 1",detectDates=TRUE,colNames=T,startRow=1)
    p.ode = para_tb%>%dplyr::select(alpha1,alpha2,gamma,N,w,alpha3)%>%slice(1,)
    
  }
  
  results_list = list(comp_df,para_tb)
  
  return(results_list)
}



ft_ode_add_calc = function(all_esti_out,dfone){
  
  
  mle_result<-all_esti_out%>%
    mutate(esti_cic = cumsum(esti_c))
  
  
  comp_data = mle_result%>%left_join(dfone%>%rename(date=day)%>%
                                       dplyr::select(-c(time,city)),by=c("date"))%>%
    mutate(i.local=replace_na(i.local,0),
           cum_case=replace_na(cum_case,0))
  return(comp_data)
}

ft_odeci_calc = function(kk,nowcalc_ci,p.sc,dfone,control_date,comp_df,nsample_b,date1,
                         n_types,ngroup,init0,total.case,dtts,para_tb,
                         max_T,filenames_odefit_ci){
  
  betas_low = para_tb$beta_low
  betas_upp = para_tb$beta_upp
  beta_value = para_tb$esti_beta
  
  #beta samples
  
  esti_sample_inci =matrix(rep(0,nsample_b*max_T),nrow=max_T)
  esti_sample_cumul =matrix(rep(0,nsample_b*max_T),nrow=max_T)
  esti_sample_rt =matrix(rep(0,nsample_b*max_T),nrow=max_T)
  
  # for (i in 1:ngroup){
  #   bsample[,i] = runif(nsample_b,betas_low[i],betas_upp[i])
  # }
  #95%ci normal distribution
  std_value = (betas_upp - betas_low)/(1.96*2)
  
  #bsample_temp =matrix(rep(0,nsample_b*ngroup),nrow=nsample_b)
  bsample =matrix(rep(0,nsample_b*ngroup),nrow=nsample_b)
  
  
  for (i in 1:ngroup){
    
    (sample_select1 = rnorm(nsample_b,beta_value[i],std_value[i]))
    sample_select_all1 =  sample_select1[sample_select1>0]
    
    (add_sample = nsample_b-length(sample_select_all1))
    
    
    if (add_sample==0){
      sample_select_posi = sample_select_all1
    } else {
      while(add_sample>0) {
        print(paste0("negative sample:",add_sample))
        
        (sample_select2 = rnorm(nsample_b,beta_value[i],std_value[i]))
        sample_select_all2 =  sample_select2[sample_select2>0]
        
        if (length(sample_select_all2)>=add_sample){
          (sample_select_all3 = sample(sample_select_all2,add_sample))
          sample_select_posi = c(sample_select_all1,sample_select_all3)
        } else {
          sample_select_posi = c(sample_select_all1)
          
        }
        (add_sample = nsample_b - length(sample_select_posi))
        
        
      }
      
    }
    
    
    
    final_sample_posi = sample_select_posi
    
    #bsample_temp[,i] = rnorm(nsample_b,beta_value[i],std_value[i])
    bsample[,i] = final_sample_posi
  }
  
  rts_rate = 1/(comp_df$alpha2*comp_df$N)
  for (ww in 1:nsample_b){
    (beta_value= bsample[ww,])
    s_esti_out = ode_interval(control_date,n_types,init0,p.sc,esti_betas=beta_value)
    #esti_out 
    s_all_esti_out = do.call("rbind",s_esti_out)%>%mutate(date=date1+0:(n()-1))
    
    
    
    #all_esti_out
    s_comp_df_temp = ft_ode_add_calc(s_all_esti_out,dfone)%>%mutate(esti_cic=esti_cic+total.case)%>%
      mutate(rt_ode=beta*S*rts_rate)
    #comp_df
    #Ft_tb_summary(comp_df,esti_para=betas[1],pp=p.sc[1,])
    s_comp_df_temp%>%head
    
    esti_sample_inci[,ww] = s_comp_df_temp$esti_c
    esti_sample_cumul[,ww] = s_comp_df_temp$esti_cic
    esti_sample_rt[,ww] = s_comp_df_temp$rt_ode
    
  }
  
  # Make CI table -----------------------------------------------------------
  
  
  ci_df_case = sc_df%>%mutate(low=0,q1=0,q2=0,q3=0,upp=0,
                              cum_low=0,cum_q1=0,cum_q2=0,cum_q3=0,cum_upp=0)%>%
    mutate(esti_case = comp_df$esti_c,esti_cumcase = comp_df$esti_cic)
  
  
  for (t in 1:max_T){
    ci_df_case$low[t] = as.numeric(quantile(esti_sample_inci[t,],probs=c(0.025)))
    ci_df_case$q1[t] = as.numeric(quantile(esti_sample_inci[t,],probs=c(0.25)))
    ci_df_case$q2[t] = as.numeric(quantile(esti_sample_inci[t,],probs=c(0.5)))
    ci_df_case$q3[t] = as.numeric(quantile(esti_sample_inci[t,],probs=c(0.75)))
    ci_df_case$upp[t]= as.numeric(quantile(esti_sample_inci[t,],probs=c(0.975)))
    
    ci_df_case$cum_low[t] = as.numeric(quantile(esti_sample_cumul[t,],probs=c(0.025)))
    ci_df_case$cum_q1[t] = as.numeric(quantile(esti_sample_cumul[t,],probs=c(0.25)))
    ci_df_case$cum_q2[t] = as.numeric(quantile(esti_sample_cumul[t,],probs=c(0.5)))
    ci_df_case$cum_q3[t] = as.numeric(quantile(esti_sample_cumul[t,],probs=c(0.75)))
    ci_df_case$cum_upp[t] = as.numeric(quantile(esti_sample_cumul[t,],probs=c(0.975)))
    
  }
  
  
  comp_df_beta = ci_df_case%>%left_join(dfone%>%rename(date=day)%>%
                                          dplyr::select(-c(time,city)),by=c("date"))%>%
    mutate(i.local=replace_na(i.local,0),i.import=replace_na(i.import,0),
           cum_case=replace_na(cum_case,0))
  
  write.xlsx(comp_df_beta,filenames_odefit_ci)
  
  
  return(comp_df_beta)
}

