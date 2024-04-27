
#lab4

###01
setwd("C:\\Users\\USER\\Desktop\\rlab")
getwd()

data4<-read.table("DATA 4.txt",header=TRUE,sep="")
data4

names(data4)<-c("team","Attendance","salary","years")
data4

###02

attach(data4)

##boxplot
boxplot(Attendance,main="boxplot for attendance",outline = TRUE,xlab="Attendance",horizontal=TRUE)
boxplot(salary,main="box plot for salary",outline = TRUE,xlab="salary",horizontal = TRUE)
boxplot(years,main="boxplot for years",outline = TRUE,xlab="years",horizontal = TRUE)

##histrogram
hist(Attendance,main="historgram for attendance",ylab="frequency")
abline(h=0)
hist(salary,main="histrogram for salary",ylab="frequency")
hist(years,main="histrogram for years",ylab ="years")

##steam-leaf
stem(Attendance)
stem(salary)
stem(years)

##mean
mean(Attendance)
mean(salary)
mean(years)

##median
median(Attendance)

##standrad deviation
sd(Attendance)
sd(salary)
sd(years)

# min max median q1 q3 
summary(Attendance)

Quantile(Attendance)
Quantile(Attendance)[2]

IQR(Attendance)


####04
#Mode
get.modes<-function(y){
  counts<-table(y)
  names(counts)[counts==max(counts)]
}
get.modes(years)
#q4
get.outliers<-function(y){
  q1<-quantile(y)[2]
  q3<-quantile(y)[4]
  IQR<-q3-q1
  up<-q3+1.5*IQR
  low<-q1-1.5*IQR
  print(paste("upper bound",up))
  print(paste("lower bound",low))
  print(paste("outliers",paste(sort(y[y<low|y>up]),collapse = ",")))
}
get.outliers(years)











