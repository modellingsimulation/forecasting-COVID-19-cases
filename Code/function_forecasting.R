
ft_forecasting = function(dfone,df_future,finalday,comp_dfa){
  (x_startday=as.Date("2020-12-31"))
  fontsize=20
  
  p1 =ggplot() +
    annotate("rect", xmin =as.Date(x_startday), xmax = as.Date(finalday), ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "gray") +
    geom_vline(aes(xintercept = as.Date(finalday)), linetype="dashed",color="black")+
    
    
    geom_ribbon(comp_dfa[[3]]%>%filter(date>=as.Date(x_startday)&date<=as.Date(fig_tmax)),
                mapping=aes(x=date,ymin=low,ymax=upp), fill="forestgreen",alpha=0.1)+
    geom_ribbon(comp_dfa[[3]]%>%filter(date>=as.Date(finalday)&date<=as.Date(fig_tmax)),
                mapping=aes(x=date,ymin=q1,ymax=q3), fill="forestgreen",alpha=0.2)+
    
    
    geom_ribbon(comp_dfa[[2]]%>%filter(date>=as.Date(x_startday)&date<=as.Date(fig_tmax)),
                mapping=aes(x=date,ymin=low,ymax=upp), fill="gold",alpha=0.1)+
    geom_ribbon(comp_dfa[[2]]%>%filter(date>=as.Date(finalday)&date<=as.Date(fig_tmax)),
                mapping=aes(x=date,ymin=q1,ymax=q3), fill="gold",alpha=0.3)+
    
    geom_ribbon(comp_dfa[[1]]%>%filter(date>=as.Date(x_startday)&date<=as.Date(fig_tmax)),
                mapping=aes(x=date,ymin=low,ymax=upp), fill="coral2",alpha=0.1)+
    geom_ribbon(comp_dfa[[1]]%>%filter(date>=as.Date(finalday)&date<=as.Date(fig_tmax)),
                mapping=aes(x=date,ymin=q1,ymax=q3), fill="coral2",alpha=0.3)+
    
    
    geom_line(comp_dfa[[3]]%>%filter(date>=as.Date(finalday)&date<=as.Date(fig_tmax)),
              mapping=aes(x=date,y=q2),color = "forestgreen",size=2,show.legend=FALSE) +
    
    geom_line(comp_dfa[[2]]%>%filter(date>=as.Date(finalday)&date<=as.Date(fig_tmax)),
              mapping=aes(x=date,y=q2),color = "orange1",size=2,show.legend=FALSE) +
    
    
    geom_line(comp_dfa[[1]]%>%filter(date>=as.Date(finalday)&date<=as.Date(fig_tmax)),
              mapping=aes(x=date,y=q2),color = "red",size=2,show.legend=FALSE) +
    
    geom_line(comp_dfa[[1]]%>%filter(date>=as.Date(x_startday)&date<=as.Date(finalday))%>%filter(i.local>0),
              mapping=aes(x=date,y=q2),color = "gray60",size=2,show.legend=FALSE) +
    
    geom_point(comp_dfa[[1]]%>%filter(date>=as.Date(x_startday)&date<=as.Date(fig_tmax))%>%filter(i.local>0),
               mapping=aes(x=date,y=i.local),size=2,color="black") +
    geom_point(df_future%>%rename(date=day)%>%filter(date>as.Date(finalday)&date<=as.Date(fig_tmax)) ,
               mapping=aes(x=date,y=i.local),size=2,color="black") +
    coord_cartesian(ylim=c(0,1200)) +
    labs(x="Date",y= "Number of cases",title=title_names)+
    
    scale_y_continuous(breaks=seq(0,1200,by=300))+  
    scale_x_date(breaks=new_date.fig,date_labels="%b/%d")+
    plot_design(fontsize,"left",90,0.5,0.5)
  
  return(p1)
}