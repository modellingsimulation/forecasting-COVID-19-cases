plot_design =function(fontsize,legend_position,angle1,vjust1,hjust1){
  theme_bw(base_size=fontsize)+
    theme(axis.text.x=element_text(size=fontsize,angle = angle1,vjust=vjust1,hjust = hjust1),axis.text.y=element_text(size=fontsize),axis.title=element_text(size=fontsize),
          plot.title = element_text(size = fontsize,face="bold"),
          legend.background = element_rect(fill="transparent"), 
          panel.grid= element_blank(),
          legend.text=element_text(size=fontsize),
          legend.position = legend_position,#"left",#c(.75, .95),
          strip.background = element_rect(colour="black", fill="white", 
                                          size=0.5, linetype="solid"),
          panel.grid.minor.x = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.justification = c("right", "top")) #c(0.5,0.95)   
}


ft_fig_ode_comp=function(comp_df,x_fig,x_startday,fig_tmax,legend_posi1,xlabel_epi,cymax,titles){
  options(repr.plot.width=10,repr.plot.height=8)
  
  
  esti_comp_plot = list()
  
  
    comp_df%<>%mutate(inci_esti= esti_c,cumul_esti=esti_cic)
    
 
  original_set = comp_df%>%filter(date<=fig_tmax)
  
  titles_v = c("New cases","Cumulative cases")
  ylabels_v = titles_v
  
  original_set%>%head
  esti_comp_plot[[1]]<-original_set%>%
    dplyr::select(date,i.local,inci_esti)%>%
    rename(Data=i.local,Esti=inci_esti)%>%
    gather(epi,n,-date)
  # datalist[[1]]=original_set%>%dplyr::select(date,ic)%>%rename(n=ic)
  
  esti_comp_plot[[2]]<-original_set%>%
    dplyr::select(date,cum_case,cumul_esti)%>%
    mutate(cumul_esti_modi=cumul_esti)%>%
    dplyr::select(-cumul_esti)%>%
    rename(Data=cum_case,Esti=cumul_esti_modi)%>%
    gather(epi,n,-date)
  
  #datalist[[2]]=original_set%>%dplyr::select(date,cic)%>%rename(n=cic)
  
  fig=list()
  fontsize=15
  for ( i in 1:2){
    pick=i
    fig[[i]] = ggplot() +
      geom_line(esti_comp_plot[[pick]]%>%filter(epi=="Esti")%>%filter(date>=as.Date(x_startday)),
                mapping=aes(x=date,y=n,group=epi,colour=epi),size=2,show.legend=FALSE) +
      geom_point(esti_comp_plot[[pick]]%>%filter(epi=="Data")%>%filter(n>0)%>%filter(date>=as.Date(x_startday)&date<=finalday),
                 mapping=aes(x=date,y=n),size=1.2,color="blue") +
      
      # scale_fill_manual(name="",values=c( "#377eb8","#e41a1c"))+
      #geom_line(esti_comp%>%filter(date>date[tdata]),mapping=aes(x=date,y=cum_estid),size=1,linetype="solid",colour="blue") +
      
      
      geom_vline(xintercept=as.Date(control_date[control_date>=as.Date(x_startday)])-0.4,color="red",size=0.5,linetype="dashed")+
      # coord_x_date(xlim=c(x_startday,as.Date(max(esti_comp_plot[[pick]]$date))))+
      #ylab("Number of cases") + xlab("day") +
      #scale_y_continuous(breaks=seq(0,maxv,by=diffv))+  
      scale_x_date(breaks=x_fig,date_labels="%b/%d")+
      plot_design(fontsize,legend_posi1,90,0.5,0.5)+
      labs(x=xlabel_epi,y=ylabels_v[pick],title=titles)
  }
  
  
  
  # for ( i in 1:2){
  #   pick=i
  #   fig[[i+2]] = ggplot() +
  #     geom_bar(esti_comp_plot[[pick]],mapping=aes(x=date,y=n,group=epi,fill=epi),stat="identity",position=position_dodge(),show.legend = FALSE,colour="black") +
  #     scale_fill_manual(name="",values=c( "#377eb8","#e41a1c"))+
  #     #geom_line(esti_comp%>%filter(date>date[tdata]),mapping=aes(x=date,y=cum_estid),size=1,linetype="solid",colour="blue") +
  #     
  #     
  #     geom_vline(xintercept=as.Date(control_date)-0.4,color="red",size=0.8,linetype="dashed")+
  #     # coord_x_date(xlim=c( x_startday,as.Date(max(esti_comp_plot[[pick]]$date))))+
  #     #ylab("Number of cases") + xlab("day") +
  #     #scale_y_continuous(breaks=seq(0,maxv,by=diffv))+  
  #     # coord_x_date(xlim=c( x_startday,as.Date(max(esti_comp_plot[[pick]]$date))))+
  #     scale_x_date(breaks=x_fig,date_labels="%b/%d")+
  #     plot_design(fontsize,legend_posi1,90,0.5,0.5)+
  #     labs(x=xlabel_epi,y=ylabels_v[pick],title=titles)
  # }
  
  return(fig)
}
