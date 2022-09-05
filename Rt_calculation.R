rm(list=ls())
libraries = c("data.table","deSolve","dplyr","gridExtra","ggpubr","openxlsx","matrixStats","tidyquant","ggplot2", 
              "magrittr","scales","tidyr","EpiEstim")#)#


for(x in libraries) { library(x,character.only=TRUE,warn.conflicts=FALSE) }
st.date = as.Date("2020-06-01")

df_inci = read.xlsx("covid_data.xlsx",detectDates=TRUE,sheet="Sheet1",colNames=T,startRow=1)%>%
          filter(date>=st.date)
df_inci%>%head


para = c(4.8, 2.3)
serial_para  = para

df_one = data.frame(case=df_inci$local)%>%
  mutate(time=1:n())


gams = function(tt,para) { 
  
  mu=para[1]
  sigma=para[2]
  gt=dgamma(tt,shape=mu^2/sigma^2,scale=sigma^2/mu)
  
  return(gt)}

dfs<-df_one%>%mutate(gt=gams(time,serial_para))
conv=numeric()
for (x in 1:(nrow(dfs))){
  conv=c(conv,sum(dfs$gt[1:(x-1)]*(dfs$case[(x-1):1])))
}

dfs%<>%mutate(conv_gt=c(conv))
dfs%<>%mutate(Rt=ifelse(conv_gt>0,(case)/(conv_gt),0))
dfs%<>%mutate(esti=Rt*conv_gt)
dfs%<>%mutate(date=st.date+(1:n())-1)


cori_rt=function(window,mean,sigma,n_final){
  n <- window   
  m <- mean    
  s <- sigma      
  
  (t_start <- seq(2, n_final - (n-1)) )
  (t_end <- t_start + (n-1))
  Rt <- estimate_R(incid = df_inci$local,method= c("parametric_si"), config = make_config(list(mean_si = m, std_si= s, t_start = t_start, t_end = t_end)))
  df_final <- data.table(date = df_inci$date[t_start[n] : tail(t_end, 1)], Rt_med = Rt$R$`Mean(R)`, Rt_min = Rt$R$`Quantile.0.05(R)`, Rt_max = Rt$R$`Quantile.0.95(R)`)
  return( df_final)
}


mean=serial_para [1];
sigma=serial_para [2];
window = 1;

n_final=nrow(df_inci)


output_cori = cori_rt(window,mean,sigma,n_final)

#x_fig = seq(as.Date(st.date),as.Date("2021-12-15"),by="1 month")
start_plot=as.Date("2021-08-01")
x_fig = seq(start_plot,as.Date("2021-12-15"),by="2 week")

p1 <- ggplot(df_inci%>%filter(date>=start_plot+10), aes(x = date, y = local)) + geom_line() + geom_point() +
  labs(x = 'date', y = 'incidence')+
  scale_x_date(breaks=x_fig)+
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

p2 <- ggplot(output_cori%>%filter(date>=start_plot+10), aes(x = date, y = Rt_med)) + geom_line(linetype = "solid",color="red") + 
    geom_ribbon(aes(ymin = Rt_min, ymax = Rt_max, fill = 'grey30'), alpha = 0.5, show.legend = FALSE) +
  geom_line(data = output_cori%>%filter(date>=start_plot+10), aes(x = date, y = 1), linetype = "solid",color="black") +
  #geom_point() +
 geom_line(data = dfs%>%filter(date>=start_plot+10), aes(x = date, y = Rt), linetype = "dotted",color="blue") +#geom_point() +
  labs(x = 'date', y = 'Cori R(t)') +
  scale_x_date(breaks=x_fig)+
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

p3 <- ggplot(dfs%>%filter(date>=start_plot+10), aes(x = date, y = Rt)) + geom_line(color="blue")+
  geom_line(data = dfs%>%filter(date>=start_plot+10), aes(x = date, y = 1), linetype = "solid",color="black") +
    labs(x = 'date', y = 'Calc_Cori R(t)') +
  scale_x_date(breaks=x_fig)+
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

ggarrange(p1,p2,p3, ncol=1, nrow=3, align="v")
