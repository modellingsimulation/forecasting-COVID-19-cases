



model_SEIHR = function(time, y, params,esti_para) {
  S=y[1]; E=y[2]; I=y[3]; H=y[4];R=y[5];CI=y[6];HI=y[7];
  
  beta=esti_para
  alpha1=params[["alpha1"]]
  alpha2=params[["alpha2"]]
  alpha3=params[["alpha3"]]
  w = params[["w"]]
  gamma=params[["gamma"]]
  N = params[["N"]]
  
  dS = -beta * S/N * I
  dE = beta * S/N * I - (1-w)*alpha1*E -w*alpha3*E
  dI = (1-w)*alpha1*E - alpha2 * I
  dH = alpha2 * I - gamma* H+w*alpha3*E
  dR = gamma * H
  dCI = (1-w)*alpha1*E
  dHI = alpha2 * I+w*alpha3*E
  
  solution = c(dS,dE,dI,dH,dR,dCI,dHI)
  return(list(solution))
}
#ode_run
fts_oderun = function(esti_para,times,init,pp){
  init0=as.numeric(init)
  
  
  (init = c(S = init0[1],E = init0[2],I = init0[3],H = init0[4],R = init0[5],
            CI=init0[6],HI=init0[7]))
  
  
  
  t0 = times[1]
  tf = max(times)
  (timev = seq(t0,t0+1,by=dtts))
  output1 = NULL
  init_update = init
  #init_update[3] = init[3]+import[1]
  for (t in 1:(tf-1)){
    out1 = data.frame(ode(y = init_update, times = timev, 
                          func = model_SEIHR, par =esti_para,params=pp))
    local_i = out1[nrow(out1),4]
    # out1[nrow(out1),4] = local_i+import[t+1]
    #initial
    init_update =c(S=out1[nrow(out1),2],E=out1[nrow(out1),3],I=out1[nrow(out1),4],H=out1[nrow(out1),5],
                   R=out1[nrow(out1),6],CI=out1[nrow(out1),7],HI=out1[nrow(out1),8])
    #time
    (timev = seq(t0+t,t0+t+1,by=dtts))
    if (t==1){
      (output1 = rbind(output1,out1[1:(nrow(out1)),]))
    } else {
      (output1 = rbind(output1,out1[2:(nrow(out1)),]))
    }
  }
  
  return(output1)
}

calc_ode_inci=function(out,pp,times,ngroup){
  out%<>%mutate(esti_c=pp$alpha2*I,esti_o=pp$alpha1*(1-pp$w)*E)  
  (tdata = times[length(times)]-times[1]+1)
  
  if (ngroup==1){
    outs =out[1+(0:(tdata-1))/dtts,]    
  } else {outs =out[1+(1:(tdata-1))/dtts,] }
  return(outs)
}


ode_interval = function(control_date,n_types,init0,p.sc,esti_betas){
  
  
  (cum_group = cumsum(n_types))
  
  (group = length(control_date))
  
  time_list = list()
  time_list[[1]] =1:(n_types[1])
  if (group>1){
    for (i in 2:group){
      time_list[[i]] = cum_group[i-1]:cum_group[i]
    }
  }
  
  #group
  out_list = list()
  init_new = init0
  output = NULL
  
  for (k in 1:group){
    
    (ode.t1 = time_list[[k]][1])
    (ode.tn = time_list[[k]][length(time_list[[k]])])
    #(tdata = ode.tn-ode.t1+1)
    (times = seq(ode.t1,ode.tn,by=dtts))
    
    (init = as.numeric(init_new))
    (pp = p.sc[k,])
    
    out = fts_oderun(esti_para=esti_betas[k],times,init,pp)
    outs = calc_ode_inci(out,pp,times,ngroup=k)
    #outs$I[outs$I<0.5]=0
    #outs$E[outs$E<0.5]=0
    
    (init_new = outs%>%dplyr::select(S,E,I,H,R,CI,HI)%>%tail(1)%>%as.numeric())
    
    out_list[[k]] =outs%<>%mutate(beta = esti_betas[k])
    
  }
  return(out_list) 
}



mle_interval_dfp = function(control_date,n_types,init0,p.sc){
  
  (cum_group = cumsum(n_types))
  
  (group = length(control_date))
  
  time_list = list()
  time_list[[1]] =1:(n_types[1])
  if (group>1){
    for (i in 2:group){
      time_list[[i]] = cum_group[i-1]:cum_group[i]
    }
  }
  
  #group
  out_list = list()
  init_new = init0
  output = NULL
  models = list()
  result_dfp = data.frame("beta_dfp"=0,"beta_low"=0,"beta_upp"=0)
  
  for (k in 1:group){
    
    (ode.t1 = time_list[[k]][1])
    (ode.tn = time_list[[k]][length(time_list[[k]])])
    #(tdata = ode.tn-ode.t1+1)
    (times = seq(ode.t1,ode.tn,by=dtts))
    
    (init = as.numeric(init_new))
    (pp = p.sc[k,])
    
    init=init;ngroup=k
    
    Ft_mle_ode_dfp = function(esti_para){
      
      out = fts_oderun(esti_para,times,init,pp)
      outs = calc_ode_inci(out,pp,times,ngroup)
      
      outs%<>%mutate(esti=esti_c)
      
      
      dfone%>%filter(time<=max(outs$time) & time>=outs$time[1])%>%left_join(outs,by="time")%>%
        filter(esti>0)%>%
        mutate(llk1=-esti+i.local*log(esti)-lfactorial(i.local))%>%
        summarize(rss=-sum(llk1))%>%.$rss->output
      return(output)
    }
    
    
    
    par=c(0.001)
    intr0<-list(label="beta",est=par,lower=rep(1e-10,1), upper=(rep(50,1)))
    
    xfinal  = dfp(intr0, f=Ft_mle_ode_dfp)
    intr0$est<-xfinal$est
    models[[k]]<-newton(intr0,Ft_mle_ode_dfp)
    
    result_dfp[k,1:3]=data.frame(beta_dfp= models[[k]]$est,
                                 beta_low=models[[k]]$low[1],beta_upp=models[[k]]$upp[1])
    
    
    
    (esti_para= as.numeric(models[[k]]$est))
    
    out = fts_oderun(esti_para,times,init,pp)
    (outs = calc_ode_inci(out,pp,times,ngroup=k))
    
    (init_new = outs%>%dplyr::select(S,E,I,H,R,CI,HI)%>%tail(1)%>%as.numeric())
    
    out_list[[k]] =outs%>%mutate(betas = esti_para,beta_low=as.numeric(models[[k]]$low[1]),
                                 beta_upp=models[[k]]$upp[1],
                                 convergence=xfinal$status,NLLH=models[[k]]$fmin)%>%
      mutate(AIC=2*NLLH+2*1,AICC=2*NLLH+2*1+4/(length(times)-2),Group=k)
    
  }
  return(out_list) 
}
