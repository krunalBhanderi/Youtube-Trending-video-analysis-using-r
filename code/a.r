# pie chart of comment disable

a1=read.csv(file = "c:/krunal/CAvideos.csv")

a1
tr=nrow(table(subset(a1$comments_disabled==TRUE)))
fa=nrow(table(subset(a1$comments_disabled==FALSE)))
fa
install.packages("plotrix")
library(plotrix)
p=c(tr,fa)
p1=(p/sum(p))*100
p1
p2=formatC(p1,digits = 2,format = "f")
t=pie3D(p,labels = paste0(p2,"%"),explode = 0.1,radius = .9,labelcex = 1.2, main = "Pie Chart of comment Enable/Disbaled ")
t
legend("topright",c("Disable","Enable"),cex=1, fill=rainbow(length(t)))
#===========================================================================


#hist0 graph of video uploaded each day of week
typeof(a1$publish_time)
#tr=nrow(table(subset(wday(ymd(strsplit(as.character(a1$publish_time),"T")[[1]])[[1]]))=="wed"))

install.packages("lubridate")
sun=0
mon=0
tue=0
wed=0
thu=0
fri=0
sat=0
library(lubridate)
for (i in a1["publish_time"])
{
  for (b in i)
  {
    c=strsplit(b,"T")[[1]]
    date=ymd(c[1])
    a=wday(date)
#      switch(a,mon=mon+1,tue=tue+1,wed=wed+1,thu=thu+1,fri=fri+1,sat=sat+1,sun=sun+1)
    if(a==1)
    {
      mon=mon+1
      print(mon)
    }else if(a==2){
      tue=tue+1
    }else if(a==3){
      wed=wed+1
    }else if(a==4){
      thu=thu+1
    }else if(a==5){
      fri=fri+1
    }else if(a==6){
      sat=sat+1
    }else if(a==7){
      sun=sun+1
    }
    print(count)
    count=count+1
    
      
  }
}

install.packages("lubridate")
date<-ymd("2019-01-12")
print(wday(date))

da=c(mon,tue,wed,thu,fri,sat,sun)
da1=c("mon","tue","wed","thu","fri","sat","sun")
f<-barplot(da,main="video uploaded on each day of week",xlab = "days",ylab = "video uploaded",args.legend=list(bty="n",horiz=TRUE) , border="white",names.arg = da1,col="yellow")


print(vv)
text(f,0,round(da,7),cex=1,pos=3)

#================================================================
#pie chart of comment on videos

comment1k=nrow(table(subset(a1$comment_count,a1$comment_count<1000)))
comment5k=nrow(table(subset(a1$comment_count,a1$comment_count>1000 & a1$comment_count<5000)))
comment20k=nrow(table(subset(a1$comment_count,a1$comment_count>5000 & a1$comment_count<20000)))
comment50k=nrow(table(subset(a1$comment_count,a1$comment_count>20000 & a1$comment_count<100000)))
comment100k=nrow(table(subset(a1$comment_count,a1$comment_count>100000 & a1$comment_count<500000)))
comment500k=nrow(table(subset(a1$comment_count,a1$comment_count>500000 )))

comment1k1=nrow(table(subset(a1$comment_count,a1$comment_count<1000)))
comment1k1
print(comment1k)
print(comment5k)
print(comment20k)
print(comment50k)
print(comment100k)
print(comment500k)

p=c(comment1k,comment5k,comment20k,comment50k,comment100k,comment500k)
p1=(p/sum(p))*100
p1
p2=formatC(p1,digits = 1,format = "f")
t=pie3D(p,labels = paste0(p2,"%"),explode = 0,radius = .9,labelcex = 1, main = "Pie Chart of total comments")
g=c("0-1000","1000-5000","5000-20000","20000-100000"," above 1000000")
legend("topright",g,cex=0.8, fill=rainbow(length(t)))

#===================================================================

#Pie chart of views count
nrow(table(subset(a1$comment_count>1000 & a1$comment_count<5000)))
comment500k=nrow(table(subset(a1$comment_count>500000 )))

typeof(a1$views)
library("plotrix")
views50k=table(subset(a1$views, subset =  a1$views<50000))
views100k=table(subset(a1$views, subset= a1$views>50000 & a1$views<100000))
views250k=table(subset(a1$views,subset = a1$views >100000 & a1$views<250000))
views500k=table(subset(a1$views,subset = a1$views >2500000 & a1$views<5000000 )))
views1000k=table(subset(a1$views>5000000 & a1$views<10000000)))
views2000k=nrow(table(subset(a1$views>10000000)))




print(views50k)
print(views100k)
print(views250k)
print(views500k)
print(views1000k)
print(views2000k)



p=c(views50k,views100k,views250k,views500k,views1000k,views2000k)
p

p1=(p/sum(p))*100
p1
p2=formatC(p1,digits = 1,format = "f")
g=c("0-50k","50k-100k","100k-250k","250k-5000k","5000k-10000k","above 10000k")
t=pie3D(p,labels = paste0(p2,"%"),explode =0 ,radius = .9,labelcex = 1, main = "Pie Chart of total views")
legend("topright",g,cex=0.9, fill=rainbow(length(t)))

#=================================================================
install.packages("gganimate")
install.packages("babynames")
install.packages("grbrthemes")
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)

count=l10=l20=l30=l40=l50=l60=l70=l80=l90=l100=0
for (i in a1$description)
{
  for (c in i)
  {
    b=nchar(c)
    
    print(b)
    if(b<50)
    {
      l10=l10+1
    }else if(50<b & b<100){
      l20=l20+1
    }else if(100<b & b<250){
      l30=l30+1
    }else if(250<b & b<500){
      l40=l40+1
    }else if(500<b & b<1000){
      l50=l50+1
    }else if(1000<b & b<2500){
      l60=l60+1
    }else if(2500<b & b<5000){
      l70=l70+1
    }else if(5000>b){
      l80=l80+1
    }
    
    
    print(count)
    count=count+1
  }
}
l10
l20
l30
l40
l50
l60
l70
l80

y=c(l10,l20,l30,l40,l50,l60,l70,l80)
x=c(50,100,250,500,1000,2500,5000,6000)
x
y
c="hello"
data=data.frame(x,y)
data
x<-ggplot(data,aes(x=x,y=y,group=1))+ geom_line(color="cyan", size=1.5, alpha=.8, linetype=8)+labs(x="Description Length",y="Total Videos", title = "Video count based on Description Length")+expand_limits(x=c(0,6000),y=c(0,15000)) +geom_area(fill="cyan",alpha=0.3)+theme(text = element_text(family = 'Gill Sans',colour = "#444444"),panel.background = element_rect(fill = '#444B5A'),panel.grid.minor = element_line(color='#4d5566'),panel.grid.major = element_line(color = '#586174'),axis.title.y = element_text(vjust = 1,angle = 0),axis.title.x = element_text(hjust = 0))+transition_reveal(x)
x
anim_save("hi.gif")
anim_save("287-smooth-animation-with-tweenr.gif")
transition_reveal()

install.packages("gifski")
library("gifski")
animate(x, duration = 5, fps = 20, width = 1100, height = 600, renderer = gifski_renderer())
anim_save("c://krunal/output.gif")



#===========================================
#==========================================
install.packages("ggplot2")
library(ggplot2)
likes50k=sum(subset(a1$likes, subset=(a1$views<50000)))
likes100k=sum(subset(a1$likes, subset=(a1$views>50000 & a1$views<100000)))
likes250k=sum(subset(a1$likes, subset=(a1$views>100000 & a1$views<250000)))
likes500k=sum(subset(a1$likes, subset=(a1$views>2500000 & a1$views<5000000 )))
likes1000k=sum(subset(a1$likes, subset=(a1$views>5000000 & a1$views<10000000)))
likes2000k=sum(subset(a1$likes, subset=(a1$views>10000000)))

dislikes50k=sum(subset(a1$dislikes, subset=(a1$views<50000)))
dislikes100k=sum(subset(a1$dislikes, subset=(a1$views>50000 & a1$views<100000)))
dislikes250k=sum(subset(a1$dislikes, subset=(a1$views>100000 & a1$views<250000)))
dislikes500k=sum(subset(a1$dislikes, subset=(a1$views>2500000 & a1$views<5000000 )))
dislikes1000k=sum(subset(a1$dislikes, subset=(a1$views>5000000 & a1$views<10000000)))
dislikes2000k=sum(subset(a1$dislikes, subset=(a1$views>10000000)))

sum(dislikes50k)
like=c()
like=c(likes50k,likes100k,likes250k,likes500k,likes1000k,likes2000k)
dislike=c(dislikes50k,dislikes100k,dislikes250k,dislikes500k,dislikes1000k,dislikes2000k)
y=append(like,dislike*-1)
y
options(scipen=999)
x=c(50000,100000,2500000,5000000,10000000,20000000)
length(y)
length(x)
li=data.frame(x,y)

y
f=c("helo")
m<-ggplot(li,aes(x=x,y=y))+ylim(-100000000,440000000)+ geom_area(fill="#a55a5a", aes(x=x,y=y),subset(li,subset=y<0))+geom_area(fill="#609f9f",aes(x,y),subset(li,subset=y>0))+theme_light()+theme(legend.position = c(0.95,0.95),legend.justification = c("vbfh","fdn"))+labs(x="Videos count",y="\n\nlikes\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n Dislikes",title = "Area chart of like and dislike")+theme(text = element_text(family = 'Gill Sans',colour = "#444444"),panel.grid.minor = element_line(color='#4d5566'),panel.grid.major = element_line(color = '#586174'),axis.title.y = element_text(vjust = 1,angle = 0),axis.title.x = element_text(hjust = 0))
m




#==================================================
err=table(subset(a1$video_error_or_removed, subset = a1$video_error_or_removed=="TRUE"))
err[[1]]




install.packages("sqldf")
library("sqldf")

read.csv.sql("c:/krunal/CAvideos.csv", sql = "select likes from file")















###donut chart of rating enable dibale

data<-file("C:/krunal/CAvideos.csv")
da=data.frame(data)
a2<-sqldf("select ratings_disabled from a1")
a3=data.frame(table(a2))
a3[,2][1]
a3[,2][2]


library(ggplot2)
library(plotrix)
p=c(a3[,2][1],a3[,2][2])
p1=(p/sum(p))*100
p1
p2=formatC(p1,digits = 2,format = "f")


data <- data.frame(
  category=c("Enable","Disable"),
  count=p
)

data$fraction <- data$count / sum(data$count)

data$ymax <- cumsum(data$fraction)

data$ymin <- c(0, head(data$ymax, n=-1))

data$labelPosition <- (data$ymax + data$ymin) / 2


data$label <- paste0(data$category, "\n value: ", data$count, "\n percentege: ",p2,"%")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=c("blue","red"))) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=c("blue","red")), size=4) + 
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+labs(x="",y="",title = "                    Dount chart of Rating Enable/Disable")   



