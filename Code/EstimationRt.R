
# Rt distribution with 10 days of delay -------------------------------------------------------------


choose_intensity = list("Low","Intermediate","High")
(df_rt_temp = read.xlsx("../../Data/Rt_lag_Korea.xlsx",sheet="Sheet1",detectDates=TRUE,colNames=T,startRow=1))



choose_ngroup = 3


df_rt =  df_rt_temp%>%mutate(Delay="delay 10")%>%filter("lag10exclude"!=1)%>%
  dplyr::select(c("date","lag10","lag10Rt"))%>%
  rename(SD="lag10",Rt="lag10Rt")%>%
  filter(Rt>0)%>%mutate(outbreak=ifelse(Rt>=1,1,0))

df_rt_group = df_rt%>%group_by(SD)%>%group_split()
probs.value = data.frame(Stage=1:choose_ngroup,prob=0)
for ( dd in 1:choose_ngroup) {
  probs.value$prob[dd] = 100*sum(df_rt_group[[dd]]$outbreak/nrow(df_rt_group[[dd]]))
}
print(probs.value)





plot_design =function(fontsize,legend_position,angle1,vjust1,hjust1){
  theme_bw(base_size=fontsize)+
    theme(axis.text.x=element_text(size=fontsize,angle = angle1,vjust=vjust1,hjust = hjust1),axis.text.y=element_text(size=fontsize),axis.title=element_text(size=fontsize),
          plot.title = element_text(size = fontsize+5,face="bold"),
          legend.background = element_rect(fill="transparent"),
          panel.grid= element_blank(),
          legend.text=element_text(size=fontsize-3),
          legend.position = legend_position,#"left",#c(.75, .95),
          strip.background = element_rect(colour="black", fill="white",
                                          size=0.5, linetype="solid"),
          panel.grid.minor.x = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.justification = c(0.5,0.95))
}



gams = function(tt,para) { 
  
  # mu=para[1]
  # sigma=para[2]
  
  #gt=dgamma(tt,shape=mu^2/sigma^2,scale=sigma^2/mu)
  gt=dgamma(tt,shape=para[1],scale=para[2])
  
  #gt=pgamma(tt+1,shape=mu^2/sigma^2,scale=sigma^2/mu)-pgamma(tt,shape=mu^2/sigma^2,scale=sigma^2/mu)
  return(gt)}
ft_result_dfp = function(choose_ngroup,df_rt_group){
  
  result_dfp = list()
  
  for (ii in 1:choose_ngroup){
    df_rt_tb = df_rt_group[[ii]]#%>%filter(Rt>0)
    
    data_point = df_rt_tb%>%nrow()
    
    (RT = df_rt_tb%>%.$Rt)
    
    
    delay_mle_gam<-function(params){
      # (delays=Rt_tb$Rt)
      # (freq =Rt_tb$n)
      
      
      (ft =gams(RT,params))
      #(ft =norms(delays,params))
      ft[ft<=1e-20] = 1e-20
      (neglogk = -sum(log(ft)))
      return(neglogk)
    }
    
    
    if (data_point>1){
      
      params = c(3,10)
      np = length(params)
      par_name = c("par1","par2")
      intr0<-list(label=par_name,est=params,lower=rep(0.00000001,np), upper=rep(100,np))
      xfinal  = dfp(intr0, f=delay_mle_gam)
      intr0$est<-xfinal$est
      models<-newton(intr0,delay_mle_gam)
      # 
      result_dfp[[ii]] = data.frame(par=par_name, mean = models$est, low = models$low, upp=models$upp)%>%
        mutate(Stage=ii)%>%mutate(Rt_mean = c(mean[1]*mean[2],
                                              sqrt(mean[1]*mean[2]^2)))
    } else {
      result_dfp[[ii]] = data.frame(par=par_name, mean =params, low = params, upp=params)%>%
        mutate(Stage=ii)%>%mutate(Rt_mean = params) 
    }
    
  }
  return(result_dfp)
}

result_dfp = ft_result_dfp(choose_ngroup,df_rt_group)
title_names = "Korea"
all_result = do.call("rbind",result_dfp)


fig_rt = function(choose_ngroup,df_rt_group,result_dfp,choose_intensity){
  p1=list()
  
  for (ii in 1:choose_ngroup){
    
    
    Rt_tb_temp = data.frame(Rt = round( df_rt_group[[ii]]$Rt,1))%>%group_by(Rt)%>%summarize(n=n())%>%mutate(Prob=n/sum(n))
    
    Rt_tb = data.frame(Rt=seq(0,round(max(df_rt_group[[1]]$Rt)),by=0.1))%>%left_join(Rt_tb_temp,by="Rt")%>%
      mutate(n=replace_na(n,0))%>%
      mutate(Prob=replace_na(Prob,0))
    
    result_comp = data.frame(Rt=Rt_tb$Rt,obs=Rt_tb$Prob*10,esti=gams(Rt_tb$Rt,result_dfp[[ii]]$mean))
    (data_points = sum(Rt_tb$n))
    
    p1[[ii]] = result_comp%>%ggplot() +
      
      geom_bar(mapping=aes(x=Rt,y=obs),fill="gray",stat="identity",position=position_dodge(),size=1)+
      geom_line(mapping=aes(x=Rt,y=esti),size=1.2,color="red")+
      labs(x="Reproduction number",y="Probability density (%)",title=choose_intensity[ii])+
      annotate("text", label = paste("Mean:",round(result_dfp[[ii]]$Rt_mean[1],2)," SD:",round(result_dfp[[ii]]$Rt_mean[2],2)),
               x =round(max(df_rt_group[[1]]$Rt))*0.7, y = max(result_comp$obs)*0.6, size = 5, colour = "red")+
      
      plot_design(fontsize=14,"right",0,0.5,0.65)
    
    
  }
  
  fig1 = grid.arrange(p1[[1]],p1[[2]],p1[[3]],nrow=1, ncol=3,top = textGrob(title_names,gp=gpar(fontsize=20,fontface="bold")))
  
  return(fig1)
  
}
figs = fig_rt(choose_ngroup,df_rt_group,result_dfp,choose_intensity)

