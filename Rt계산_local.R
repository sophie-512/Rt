rm(list=ls())

#필요한 라이브러리들
libraries = c("data.table","deSolve","dplyr","gridExtra","ggpubr","openxlsx","matrixStats","tidyquant","ggplot2","magrittr","scales","tidyr","EpiEstim")#)#
#for(x in libraries) {install.packages(x)}
for(x in libraries) { library(x,character.only=TRUE,warn.conflicts=FALSE) }

#2020-06-01 이후의 데이터 사용
#st.date = as.Date("2020-06-01")

st.date = as.Date("2020-05-01")

df_inci = read.xlsx("D:/covid_data.xlsx",detectDates=TRUE,sheet="Sheet1",colNames=T,startRow=1)
df_inci%>%head(10)

#date, local, import 데이터
nrow(df_inci)
para = c(4.8, 2.3) #gamma분포의 평균, 분산, 발생 간격 Ws~이산화된 gamma분포
serial_para  = para #직렬 매개변수

####local만 사용
#df_one : 감염재생산지수 추정에 사용 할 내부 감염 수 데이터
#mutate : time(일) 1~ case(I_t) 개수까지 열 추가 => length 구해보면 총 533행(533일 동안의 데이터)
df_one = data.frame(case=df_inci$local)%>%mutate(time=1:n())
df_one%>%head #I_t, t(일) 데이터




#####직접 계산

#이산화된 gamma 분포(Ws가 이 분포를 따른다)
gams = function(tt,para) { 
  mu = para[1] #4.8
  sigma = para[2] #2.3
  w = dgamma(tt, shape=mu^2/sigma^2, scale=sigma^2/mu)
  return(w)}


mu = para[1] #4.8
sigma = para[2] #2.3
shape=mu^2/sigma^2
scale=sigma^2/mu
shape;scale


#Ws

dfs <- df_one%>%mutate(w=gams(time,serial_para))
dfs%>%head(10) #I_t, t(일), Ws


#Cori method Rt 계산식의 분모
conv=numeric() 
#t=1~564, Rt의 분모 총 564개 계산
for (x in 1:(nrow(dfs))){ 
  conv=c(conv,sum(dfs$w[1:(x-1)]*(dfs$case[(x-1):1])))
}
conv%>%head
#dfs$gt[1:x-1] : 발생간격 Ws        dfs$case[x-1:1] : t-s일의 감염 발생 수 I_t-s
#s=1부터 t까지, ( I_t-s * Ws )의 sum
# for (s in 1:533) {
#    sum( W[1:(s-1)] * I[(s-1):1] ) }
#     => ex) s=500 => R_500의 분모 = sigma(s=1부터 500)(I_500-s * Ws)
#     => I[499]W[1]+I[498]W[2]+ ... + I[1]*W[499]


dfs%<>%mutate(conv_gt=c(conv)) #conv 값들을 conv_gt라는 이름의 열로 추가
dfs%>%head #case, time, Ws, 분모

dfs%<>%mutate(Rt=ifelse(conv_gt>0,(case)/(conv_gt),0)) 

#분모가 양수이면 Rt 계산(It/분모)
dfs%>%head #It, t, Ws, 분모, Rt

dfs%<>%mutate(esti=Rt*conv_gt) #계산한 Rt * 분모 = It
dfs%>%head #It, t, Ws, 분모, Rt, It

dfs%<>%mutate(date=st.date+(1:n())-1) #그래프 그릴 때 날짜 필요
dfs%>%head(15) #It, t, Ws, 분모, Rt, It, 날짜




#####내장 함수 사용 estimate_R

#parametric_si : 사용자가 시간격의 평균과 표준편차를 정함
#non_parametric_si : 사용자가 시간격의 이산 분포를 정함 // 이거 말고도 여러가지


#window : 시간 간격
#"n일에 한 번 씩 측정"과 같은 경우에는 t_start와 t_end를 간격에 맞게 설정해주어야 한다.
#예를 들어 3일에 한 번 씩 측정 : n_final=10인 경우, t_start=c(2,3,4,5,6,7,8), t_end=c(4,5,6,7,8,9,10) 
#COVID 데이터는 1일 간격으로 측정하였기 때문에 t_start와 t_end에 2부터 순서대로 모든 값이 들어있다.
#예를 들어 수요일에 감염이 특히 많이 되는 감염병의 Rt를 계산할 때, 날짜의 영향을 줄이기 위해 사용.

cori_rt=function(window,mean,sigma,n_final){
  n <- window #시간 간격 
  m <- mean   
  s <- sigma      
  
  (t_start <- seq(2, n_final - (n-1)) )
  (t_end <- t_start + (n-1))
  
  #Rt 추정
  Rt <- estimate_R(incid = df_inci$local, method= c("parametric_si"), 
                   config = make_config(list(mean_si = m, std_si= s, t_start = t_start, t_end = t_end)))
  
  #df_final 테이블 <- data, Rt, 분위수
  df_final <- data.table(date = df_inci$date[t_start[n] : tail(t_end, 1)], Rt_med = Rt$R$`Mean(R)`, 
                         Rt_min = Rt$R$`Quantile.0.025(R)`, Rt_max = Rt$R$`Quantile.0.975(R)`)
  
  return(df_final)
}


mean=serial_para [1];
sigma=serial_para [2];
window = 1;

n_final=nrow(df_inci) #=564

output_cori = cori_rt(window,mean,sigma,n_final)
output_cori%>%head(15)




#####그래프

#x_fig = seq(as.Date(st.date),as.Date("2021-12-15"),by="1 month") #2020-06-01부터 2021-12-15까지 1달 간격으로
start_plot=as.Date("2021-05-01")
x_fig = seq(start_plot,as.Date("2021-12-15"),by="2 week")

#ggplot
#aes(X축, Y축, color=색 등등), lambs(x='x축이름', y='y축이름')
#scale_x_date(breaks=x_fig) : x축의 날짜 척도를 x_fig
#theme : x축의 날짜들을 90도 회전

#원래 데이터
p1 <- ggplot(df_inci%>%filter(date>=start_plot+10), aes(x = date, y = local)) + geom_line() + geom_point() +
  labs(x = 'date', y = 'incidence')+ scale_x_date(breaks=x_fig)+
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

#내장된 함수 estimates_R 사용
p2 <- ggplot(output_cori%>%filter(date>=start_plot+10), aes(x = date, y = Rt_med)) + geom_line(linetype = "solid",color="red") + 
    geom_ribbon(aes(ymin = Rt_min, ymax = Rt_max, fill = 'grey30'), alpha = 0.5, show.legend = FALSE) +
  geom_line(data = output_cori%>%filter(date>=start_plot+10), aes(x = date, y = 1), linetype = "solid",color="black") +
  #geom_point() +
 geom_line(data = dfs%>%filter(date>=start_plot+10), aes(x = date, y = Rt), linetype = "dotted",color="blue") +#geom_point() +
  labs(x = 'date', y = 'Cori R(t)') +
  scale_x_date(breaks=x_fig)+
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

#직접 계산
p3 <- ggplot(dfs%>%filter(date>=start_plot+10), aes(x = date, y = Rt)) + geom_line(color="blue")+
  geom_line(data = dfs%>%filter(date>=start_plot+10), aes(x = date, y = 1), linetype = "solid",color="black") +
    labs(x = 'date', y = 'Calc_Cori R(t)') +
  scale_x_date(breaks=x_fig)+
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

ggarrange(p1,p2,p3, ncol=1, nrow=3, align="v")







#####Bettencourt and Ribeiro

#평균 감염 발생 시간
g1 = nrow(dfs)/sum(dfs$case)
g2 = mean(dfs$w)
g1;g2

Bett_rt=numeric()
Bett_rt_1=numeric() 
Bett_rt_2=numeric()

Bett_method=function(g){
  for (x in 1:(nrow(dfs))-1){
    Bett_rt[x]=(log(dfs$case[x+1])-log(dfs$case[x]))*g + 1
  }
  return(Bett_rt)
}

Bett_rt_1 = Bett_method(g1)
Bett_rt_2 = Bett_method(g2)

Bett_rt<-cbind(Bett_rt_1,Bett_rt_2)
Bett_rt%>%head(20)


#Bett 그래프
x_fig = seq(as.Date("2021-05-07"),as.Date("2021-12-01"),by="1 month")
start_plot=as.Date("2021-05-07")

#g1
Bett_data_1=data.frame(case=df_inci$local[7:563])%>%mutate(time=1:n())
Bett_data_1%<>%mutate(Rt=Bett_rt_1[c(7:563)])
Bett_data_1%<>%mutate(date=st.date+(1:n())-1)
Bett_data_1%>%head

Bett_g1 <- ggplot(Bett_data_1%>%filter(date>=start_plot), aes(x = date, y = Rt)) + geom_line(color="blue")+
  geom_line(data = dfs%>%filter(date>=start_plot+10), aes(x = date, y = 1), linetype = "solid",color="black") +
  labs(x = 'date', y = 'Calc_Bett R(t), g1') + scale_x_date(breaks=x_fig) +
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 

#g2
Bett_data_2=data.frame(case=df_inci$local[7:563])%>%mutate(time=1:n())
Bett_data_2%>%head
Bett_data_2%<>%mutate(Rt=Bett_rt_2[c(7:563)])
Bett_data_2%<>%mutate(date=st.date+(1:n())-1)

Bett_g2 <- ggplot(Bett_data_2%>%filter(date>=start_plot+10), aes(x = date, y = Rt)) + geom_line(color="red")+
  geom_line(data = dfs%>%filter(date>=start_plot+10), aes(x = date, y = 1), linetype = "solid",color="black") +
  labs(x = 'date', y = 'Calc_Bett R(t), g2') + scale_x_date(breaks=x_fig) +
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = .5, vjust=0.5)) 



ggarrange(Bett_g1, Bett_g2, ncol=1, nrow=2, align="v")
