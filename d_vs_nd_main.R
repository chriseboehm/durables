#############################
# Optimal Deleveraging
# Durables vs. Nondurables
# July 20 2014
# Christoph Boehm
#############################

rm(list = ls())
setwd("C:/Daten/University of Michigan/Research Projects/Job Market Paper/Optimal Deleveraging/data/durables_vs_nondurables")

## Load Packages ###########

library(ggplot2)
library(mFilter)
library(grid)
require("reshape")
library("KernSmooth")
library("zoo")

## Load Functions ###########

source("C:/Daten/University of Michigan/Research Projects/Job Market Paper/Optimal Deleveraging/data/R files/multiplot.r")

## Options #################

options(digits=6)

## Load Data ##################### 

data <- read.csv("C:/Daten/University of Michigan/Research Projects/Job Market Paper/Optimal Deleveraging/data/durables_vs_nondurables/input_data_R.csv")

## Set up data for plotting ###

t_select <- subset(data, select = c('date','nber_trough'), drop = T)

p_data <- data.frame(seq(from = -8, to = 16, by = 1))

p_data2 <- p_data

names(p_data) <- "q_around_trough"
names(p_data2) <- "q_around_trough"

t_select$t1949q4 <- c(rep(0,times=12-9) ,seq(from=12-8,to=12+16,by=1)  ,rep(0,times=269-12-16))
t_select$t1954q2 <- c(rep(0,times=30-9) ,seq(from=30-8,to=30+16,by=1)  ,rep(0,times=269-30-16))
t_select$t1958q2 <- c(rep(0,times=46-9) ,seq(from=46-8,to=46+16,by=1)  ,rep(0,times=269-46-16))
t_select$t1961q1 <- c(rep(0,times=57-9) ,seq(from=57-8,to=57+16,by=1)  ,rep(0,times=269-57-16))
t_select$t1970q4 <- c(rep(0,times=96-9) ,seq(from=96-8,to=96+16,by=1)  ,rep(0,times=269-96-16))
t_select$t1975q1 <- c(rep(0,times=113-9),seq(from=113-8,to=113+16,by=1),rep(0,times=269-113-16))
t_select$t1980q3 <- c(rep(0,times=135-9),seq(from=135-8,to=135+16,by=1),rep(0,times=269-135-16))
t_select$t1982q4 <- c(rep(0,times=144-9),seq(from=144-8,to=144+16,by=1),rep(0,times=269-144-16))
t_select$t1991q1 <- c(rep(0,times=177-9),seq(from=177-8,to=177+16,by=1),rep(0,times=269-177-16))
t_select$t2001q4 <- c(rep(0,times=220-9),seq(from=220-8,to=220+16,by=1),rep(0,times=269-220-16))
t_select$t2009q2 <- c(rep(0,times=250-9),seq(from=250-8,to=250+16,by=1),rep(0,times=269-250-16))

t_names <- names(t_select[-c(1,2)])

## By Recession ###

y_min = 82 # lower limit for plotting
y_max = 155 # upper limit for plotting

# 1949q1

p_data[,2:5] <- data[t_select$t1949q4,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,2:5] <- p_data[,2:5]
names(p_data2)[2:5] <- c('goods49','durable_goods49','nondurable_goods49','services49')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1949q1 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") +  #palette="RdBu"
  ggtitle("Trough in 1949q1")
  
ggsave(p_r1949q1, file="p_r1949q1.jpg", dpi = 600,width = 9, height = 6, units = "in")

# 1954q2

p_data[,2:5] <- data[t_select$t1954q2,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,6:(6+3)] <- p_data[,2:5]
names(p_data2)[6:(6+3)] <- c('goods54','durable_goods54','nondurable_goods54','services54')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1954q2 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1954q2")
  
ggsave(p_r1954q2, file="p_r1954q2.jpg", dpi = 600,width = 9, height = 6, units = "in")

# t1958q2

p_data[,2:5] <- data[t_select$t1958q2,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,10:(10+3)] <- p_data[,2:5]
names(p_data2)[10:(10+3)] <- c('goods58','durable_goods58','nondurable_goods58','services58')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1958q2 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1958q2")
  
ggsave(p_r1958q2, file="p_r1958q2.jpg", dpi = 600,width = 9, height = 6, units = "in")

# t1961q1

p_data[,2:5] <- data[t_select$t1961q1,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,14:(14+3)] <- p_data[,2:5]
names(p_data2)[14:(14+3)] <- c('goods61','durable_goods61','nondurable_goods61','services61')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1961q1 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1961q1")
  
ggsave(p_r1961q1, file="p_r1961q1.jpg", dpi = 600,width = 9, height = 6, units = "in")

# t1970q4

p_data[,2:5] <- data[t_select$t1970q4,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,18:(18+3)] <- p_data[,2:5]
names(p_data2)[18:(18+3)] <- c('goods70','durable_goods70','nondurable_goods70','services70')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1970q4 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1970q4")
  
ggsave(p_r1970q4, file="p_r1970q4.jpg", dpi = 600,width = 9, height = 6, units = "in")

# t1975q1

p_data[,2:5] <- data[t_select$t1975q1,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,22:(22+3)] <- p_data[,2:5]
names(p_data2)[22:(22+3)] <- c('goods75','durable_goods75','nondurable_goods75','services75')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1975q1 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1975q1")
  
ggsave(p_r1975q1, file="p_r1975q1.jpg", dpi = 600,width = 9, height = 6, units = "in")
  
  
# t1980q3

p_data[,2:5] <- data[t_select$t1980q3,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,26:(26+3)] <- p_data[,2:5]
names(p_data2)[26:(26+3)] <- c('goods80','durable_goods80','nondurable_goods80','services80')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1980q3 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1980q3")

ggsave(p_r1980q3, file="p_r1980q3.jpg", dpi = 600,width = 9, height = 6, units = "in")  


# t1982q4

p_data[,2:5] <- data[t_select$t1982q4,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,30:(30+3)] <- p_data[,2:5]
names(p_data2)[30:(30+3)] <- c('goods82','durable_goods82','nondurable_goods82','services82')


p_data_l <- melt(p_data, id="q_around_trough")

p_r1982q4 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1982q4")

ggsave(p_r1982q4, file="p_r1982q4.jpg", dpi = 600,width = 9, height = 6, units = "in")  


# t1991q1

p_data[,2:5] <- data[t_select$t1991q1,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,34:(34+3)] <- p_data[,2:5]
names(p_data2)[34:(34+3)] <- c('goods91','durable_goods91','nondurable_goods91','services91')

p_data_l <- melt(p_data, id="q_around_trough")

p_r1991q1 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 1991q1")

ggsave(p_r1991q1, file="p_r1991q1.jpg", dpi = 600,width = 9, height = 6, units = "in")  


# t2001q4

p_data[,2:5] <- data[t_select$t2001q4,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,38:(38+3)] <- p_data[,2:5]
names(p_data2)[38:(38+3)] <- c('goods01','durable_goods01','nondurable_goods01','services01')

p_data_l <- melt(p_data, id="q_around_trough")

p_r2001q4 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 2001q4")
  
ggsave(p_r2001q4, file="p_r2001q4.jpg", dpi = 600,width = 9, height = 6, units = "in") 


# t2009q2

p_data[,2:5] <- data[t_select$t2009q2,c('goods','durable_goods','nondurable_goods','services')]

p_data[,2:5] <- p_data[,-1]/apply(p_data[9,-1],2,rep,dim(p_data[,-1])[1])*100

p_data2[,42:(42+3)] <- p_data[,2:5]
names(p_data2)[42:(42+3)] <- c('goods09','durable_goods09','nondurable_goods09','services09')

p_data_l <- melt(p_data, id="q_around_trough")

p_r2009q2 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(y_min,y_max)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Trough in 2009q2")
  
ggsave(p_r2009q2, file="p_2009q2.jpg", dpi = 600,width = 9, height = 6, units = "in") 

# multiplot(p_r1949q1, p_r1954q2, p_r1958q2, p_r1961q1, p_r1970q4,
#           p_r1975q1, p_r1980q3, p_r1982q4, p_r1991q1, p_r2001q4, p_r2009q2, cols=4)


# Comparison to average recovery

p_data_avg10 <- p_data2[,1:5]

for (i in 2:10){
  p_data_avg10[,2:5] <- p_data_avg10[,2:5]+p_data2[,(2+i*4):(2+i*4+3)]
  
}

p_data_avg10[,2:5] <- p_data_avg10[,2:5]/10
names(p_data_avg10)[2:5] <- c('goods_avg','durable_goods_avg','nondurable_goods_avg','services_avg')

p_data_avg3 <- p_data2[,1:5]

p_data_avg3[,2:5]  <- (p_data2[,30:33]+p_data2[,34:37]+p_data2[,38:41])/3
names(p_data_avg3)[2:5] <- c('goods_avg','durable_goods_avg','nondurable_goods_avg','services_avg')

# plotting

p_data_l <- melt(p_data_avg10, id="q_around_trough")

p_avg10 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(95,140)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Average recovery after other post-war recessions")

ggsave(p_avg10, file="p_avg10.jpg", dpi = 600,width = 9, height = 6, units = "in") 

p_data_l <- melt(p_data_avg3, id="q_around_trough")

p_avg3 <- ggplot(p_data_l,aes(x=q_around_trough,y=value,color=variable)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.20, 0.80)) +
  scale_x_continuous("Quarters relative to trough",breaks=seq(-8, 16, 4)) + #,breaks=seq(2003, 2012, 1)
  scale_y_continuous("Real Consumption Expenditures (0 = 100)", limits = c(95,140)) + scale_color_brewer(palette="RdBu") + #palette="RdBu"
  ggtitle("Average recovery after 82, 91, 01 recessions")

ggsave(p_avg3, file="p_avg3.jpg", dpi = 600,width = 9, height = 6, units = "in") 








  
  

extensions <- c("032","033","034","041","042","043","044","051","052","053","054","061","062","063","064",
                "071","072","073","074","081","082","083","084","091","092","093","094","101","102","103","104",
                "111","112","113","114","121","122","123","124","131")

vars1 <- c('NEWID','STATE','AGE_REF','AGE2','BSINVSTX','BSIN_STX','CKBKACTX','CKBK_CTX',
           'COMPCKGX','COMP_KGX','COMPSAVX','COMP_AVX','CUTENURE','CUTE_URE','EDUC_REF','EDUCA2',
           'FINLWT21','INC_HRS1','INC__RS1','INC_HRS2','INC__RS2',
           'INCNONW1','INCN_NW1','INCNONW2','INCN_NW2','PURSSECX','PURS_ECX',
           'RENTEQVX','RENT_QVX','SAVACCTX','SAVA_CTX','SECESTX','SECESTX_','SELLSECX','SELL_ECX',
           'WTREP01','WTREP02','WTREP03','WTREP04','WTREP05','WTREP06','WTREP07',
           'WTREP08','WTREP09')

vars2 <- paste("WTREP", as.character(10:44), sep = "")

vars <- c(vars1,vars2)

vars3 <- c('TOTEXPPQ','HOUSEQPQ','CARTKNPQ','CARTKUPQ','OTHVEHPQ','TVRDIOPQ','TTOTALP',
           'INCLASS','INCLASS2','INCL_SS2')

vars <- c(vars,vars3)

filenames <- paste("fmli", extensions,".csv", sep = "")

date = seq.Date(as.Date("2003-01-01"), by="1 quarter", to = as.Date("2012-12-31"))

data = data.frame(NULL)

for (count in 1:40) {
  
  temp1 <- read.csv(filenames[count])
  
  temp2 <- subset(temp1,select = vars,drop = T)
  
  temp3 <- cbind(as.Date(date[count]),temp2)
  
  data <- rbind.data.frame(data,temp3)
  
}

names(data)[1] <- 'date'

save(data,file="CEXsample.Rda")

## Savings and Expenditures #################################################################
########################################### Sociodemographic Characteristics
#
# 'STATE'               --- State
# 'AGE_REF'             --- Age of reference person
# 'CUTENURE','CUTE_URE' --- Housing tenure: 1-owned w/ mortgage, 2-owned w/o mortgage, 3-owned mortgage not reported
#                       --- 4-rented, 5-Occupied without payment of cash rent, 6-Student housing                         
# 'EDUC_REF'            --- Education of reference person 
# 'INCLASS'             --- Income class before taxes 01 - less than 5000, 09 - 70.000 and over
# 'INCLASS2','INCL_SS2' --- Income class based on INC_RANK 1 - Less than 0.1667, 6 - 0.8334 – 1.0000
#
########################################### Savings
#
# 'BSINVSTX','BSIN_STX' --- business and farm investment
# 'CKBKACTX','CKBK_CTX' --- market value of checking and brokerage accounts
# 'SECESTX' ,'SECESTX_' --- estimated values of securities (bonds and stocks)
# 'SAVACCTX','SAVA_CTX' --- total balance of savings accoungs
# 'COMPCKGX','COMP_KGX' --- How much more or less in checking accounts
# 'COMPSAVX','COMP_AVX' --- How much more or less in savings accounts
# 'PURSSECX','PURS_ECX' --- Purchase value of securities over the past 12 months
# 'SELLSECX','SELL_ECX' --- Net amount received from sales of securities (bonds and stocks)
#
########################################### Expenditures
#
# 'TOTEXPPQ'            --- Total expenditures last quarter
# 'HOUSEQPQ'            --- House furnishings and equipment last quarter
# 'CARTKNPQ'            --- Cars and trucks, new (net outlay) last quarter
# 'CARTKUPQ'            --- Cars and trucks, used (net outlay) last quarter
# 'OTHVEHPQ'            --- Other vehicles last quarter
# 'TVRDIOPQ'            --- Televisions, radios, and sound equipment last quarter
# 'TTOTALP'             --- Total of all trip expenditures last quarter
#
#############################################################################################

rm(list = ls())
load("CEXsample.Rda")

vars <- c('date','STATE','AGE_REF','CUTENURE','CUTE_URE','EDUC_REF','INCLASS','INCLASS2','INCL_SS2','FINLWT21', 
          'BSINVSTX','BSIN_STX','CKBKACTX','CKBK_CTX','COMPCKGX','COMP_KGX','COMPSAVX','COMP_AVX',
          'PURSSECX','PURS_ECX','SAVACCTX','SAVA_CTX','SECESTX','SECESTX_','SELLSECX','SELL_ECX',
          'TOTEXPPQ','HOUSEQPQ','CARTKNPQ','CARTKUPQ','OTHVEHPQ','TVRDIOPQ','TTOTALP')

CEXsample2 <- subset(data,select = vars,drop=T)

## Strategy #################################################################################
#
# Look at consumption and saving behavior across income groups INCLASS2, AGE_REF, EDUC_REF, STATE,
# CUTENURE levels 1 and 2
#
#############################################################################################

## By income ################################################################################

incvars <- c('date','INCLASS2', 'FINLWT21',
             'BSINVSTX','CKBKACTX','COMPCKGX','COMPSAVX','PURSSECX','SAVACCTX','SECESTX','SELLSECX',
             'TOTEXPPQ','HOUSEQPQ','CARTKNPQ','CARTKUPQ','OTHVEHPQ','TVRDIOPQ','TTOTALP')

data_inc <- subset(CEXsample2,select = incvars,drop=T)

data_inc$timefac <- factor(data_inc$date)

data_inc <- data_inc[which(data_inc$INCLASS2 <= 6),]  

data_inc$int_fac <- interaction(data_inc$INCLASS2,data_inc$timefac)

data_inc$DUREXPPQ <- data_inc$HOUSEQPQ+data_inc$CARTKNPQ+data_inc$CARTKUPQ+data_inc$OTHVEHPQ+data_inc$TVRDIOPQ

# Replace . with NA

for (c in 3:10){
  index <- (as.character(data_inc[,c])=='.')
  data_inc[index,c] <- NA  
  data_inc[,c] <- as.numeric(as.character(data_inc[,c]))
}


## Expenditure data ####

data_inc_exp <- subset(data_inc,select = c('date','INCLASS2','int_fac','TOTEXPPQ','DUREXPPQ','FINLWT21'), drop=T)

data_inc_exp$NONDEXPPQ <- data_inc_exp$TOTEXPPQ - data_inc_exp$DUREXPPQ

data_inc_exp$w.totex <- data_inc_exp$TOTEXPPQ*data_inc_exp$FINLWT21
data_inc_exp$w.nondurex <- data_inc_exp$NONDEXPPQ*data_inc_exp$FINLWT21
data_inc_exp$w.durex <- data_inc_exp$DUREXPPQ*data_inc_exp$FINLWT21

data_inc_exp_agg <- aggregate(data_inc_exp$w.totex,by = list(data_inc_exp$int_fac),sum,simplify=T)

names(data_inc_exp_agg) <- c('int_fac','w.totex')

data_inc_exp_agg$w.nondurex <- aggregate(data_inc_exp$w.nondurex,by = list(data_inc_exp$int_fac),sum,simplify=T)[,2]
data_inc_exp_agg$w.durex <- aggregate(data_inc_exp$w.durex,by = list(data_inc_exp$int_fac),sum,simplify=T)[,2]
data_inc_exp_agg$weight <- aggregate(data_inc_exp$FINLWT21,by = list(data_inc_exp$int_fac),sum,simplify=T)[,2]

data_inc_exp_agg[,2:4] <- data_inc_exp_agg[,2:4]/matrix(data_inc_exp_agg$weight,nrow=length(data_inc_exp_agg$weight),ncol=3,byrow=F)

data_inc_exp_agg$date <- as.Date(substring(as.character(data_inc_exp_agg$int_fac), first=3, last=12))
data_inc_exp_agg$INCLASS2 <- substring(as.character(data_inc_exp_agg$int_fac), first=1, last=1)

data_inc_exp_agg <- data_inc_exp_agg[,c('date','INCLASS2','w.totex','w.nondurex','w.durex')]

data_inc_exp_agg_w <- reshape(data_inc_exp_agg, timevar = "INCLASS2", idvar = c("date"), direction = "wide")

data_inc_exp_agg_w$date <- as.integer(format(data_inc_exp_agg_w$date, "%Y"))

data_inc_exp_agg_w <- aggregate(data_inc_exp_agg_w[,-1],by = list(data_inc_exp_agg_w$date),sum,simplify=T)

names(data_inc_exp_agg_w)[1]  <- 'date'

data_inc_exp_agg_w <- data_inc_exp_agg_w[,-1]/apply(data_inc_exp_agg_w[5,-1],2,rep,dim(data_inc_exp_agg_w[,-1])[1])*100

data_inc_exp_agg_w$date <- 2003:2012

inc_totalexp_plot_w <- data_inc_exp_agg_w[,c('date','w.totex.1','w.totex.2','w.totex.3',
                                             'w.totex.4','w.totex.5','w.totex.6')] 

inc_nondurexp_plot_w <- data_inc_exp_agg_w[,c('date','w.nondurex.1','w.nondurex.2','w.nondurex.3',
                                              'w.nondurex.4','w.nondurex.5','w.nondurex.6')]

inc_durexp_plot_w <- data.frame(data_inc_exp_agg_w[,'date'])
names(inc_durexp_plot_w) <- 'date'
inc_durexp_plot_w$lowest_third <- (data_inc_exp_agg_w$w.durex.1 + data_inc_exp_agg_w$w.durex.2)/2
inc_durexp_plot_w$middle_third <- (data_inc_exp_agg_w$w.durex.3 + data_inc_exp_agg_w$w.durex.4)/2
inc_durexp_plot_w$top_third    <- (data_inc_exp_agg_w$w.durex.5 + data_inc_exp_agg_w$w.durex.6)/2


# Plotting Options

theme_set(theme_gray(base_size = 14))

# Total expenditures

inc_totalexp_plot_l <- melt(inc_totalexp_plot_w, id="date")

#write.csv(inc_totalexp_plot_w, "sql_test.csv", row.names=FALSE)

names(inc_totalexp_plot_l)[2] <- 'Income_Group'

p_inc_totalexp <- ggplot(inc_totalexp_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.88, 0.25)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Consumption Expenditures (2007 = 100)") + scale_color_brewer() #palette="RdBu"

ggsave(p_inc_totalexp, file="p_inc_totalexp.jpg", dpi = 600,width = 9, height = 6, units = "in")

# Nondurable expenditures

inc_nondurexp_plot_l <- melt(inc_nondurexp_plot_w, id="date")

names(inc_nondurexp_plot_l)[2] <- 'Income_Group'

p_inc_nondurexp <- ggplot(inc_nondurexp_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.88, 0.25)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Nondurable Expenditures (2007 = 100)") + scale_color_brewer()

ggsave(p_inc_nondurexp, file="p_inc_nondurexp.jpg", dpi = 600,width = 9, height = 6, units = "in")


# Durable expenditures

inc_durexp_plot_l <- melt(inc_durexp_plot_w, id="date")

names(inc_durexp_plot_l)[2] <- 'Income_Group'

p_inc_durexp <- ggplot(inc_durexp_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.35, 0.16)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Durable Expenditures (2007 = 100)") + scale_color_brewer()

ggsave(p_inc_durexp, file="p_inc_durexp.jpg", dpi = 600,width = 9, height = 6, units = "in")


## Asset Data ###

# 'CKBKACTX','CKBK_CTX' --- market value of checking and brokerage accounts
# 'SECESTX','SECESTX_'  --- estimated values of securities (bonds and stocks)
# 'SAVACCTX','SAVA_CTX' --- total balance of savings accoungs

data_inc_assets <- subset(data_inc,select = c('date','INCLASS2','int_fac','CKBKACTX',
                                              'SECESTX','SAVACCTX','FINLWT21'), drop=T)

data_inc_assets$w.CKBKACTX <- data_inc_assets$CKBKACTX * data_inc_assets$FINLWT21 
data_inc_assets$w.SECESTX <- data_inc_assets$SECESTX * data_inc_assets$FINLWT21
data_inc_assets$w.SAVACCTX<- data_inc_assets$SAVACCTX * data_inc_assets$FINLWT21

data_inc_assets$date <- as.integer(format(data_inc_assets$date, "%Y"))

data_inc_assets$int_fac <- interaction(data_inc_assets$INCLASS2,data_inc_assets$date)

## Checking and Brokerage Accounts

data_checking <- data_inc_assets[complete.cases(data_inc_assets$w.CKBKACTX),]

data_checking <- subset(data_checking, select = c('date','INCLASS2','int_fac','w.CKBKACTX','FINLWT21'),drop = T)

data_checking <- aggregate(data_checking[,4:5],by = list(data_checking$int_fac),sum,simplify=T,na.rm=T)

data_checking$checking <- data_checking$w.CKBKACTX/data_checking$FINLWT21

data_checking$INCLASS2 <- as.factor(substring(as.character(data_checking$Group.1), first=1, last=1))

data_checking$date <- substring(as.character(data_checking$Group.1), first=3, last=12)

data_checking <- subset(data_checking, select = c('date','INCLASS2','checking'), drop=T)

data_checking_w <- reshape(data_checking, timevar = "INCLASS2", idvar = c("date"), direction = "wide")

data_checking_w <- data_checking_w[,-1]/apply(data_checking_w[5,-1],2,rep,dim(data_checking_w[,-1])[1])*100

data_checking_w$date <- 2003:2012

data_checking_l <- melt(data_checking_w, id="date")

data_checking_l$date <- as.integer(data_checking_l$date)

names(data_checking_l)[2] <- 'Income_Group'

p_inc_checking <- ggplot(data_checking_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.80)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Checking and Brokerage Account") +  #,limits = c(0,30000)
  scale_color_brewer() #palette="Reds", palette="RdBu"

ggsave(p_inc_checking, file="p_inc_checking.jpg", dpi = 600,width = 9, height = 6, units = "in")


## Securities

data_securities <- data_inc_assets[complete.cases(data_inc_assets$w.SECESTX),]

data_securities <- subset(data_securities, select = c('date','INCLASS2','int_fac','w.SECESTX','FINLWT21'),drop = T)

data_securities <- aggregate(data_securities[,4:5],by = list(data_securities$int_fac),sum,simplify=T,na.rm=T)

data_securities$securities <- data_securities$w.SECESTX/data_securities$FINLWT21

data_securities$INCLASS2 <- as.factor(substring(as.character(data_securities$Group.1), first=1, last=1))

data_securities$date <- substring(as.character(data_securities$Group.1), first=3, last=12)

data_securities <- subset(data_securities, select = c('date','INCLASS2','securities'), drop=T)

data_securities_w <- reshape(data_securities, timevar = "INCLASS2", idvar = c("date"), direction = "wide")

data_securities_w <- data_securities_w[,-1]/apply(data_securities_w[5,-1],2,rep,dim(data_securities_w[,-1])[1])*100

data_securities_w$date <- 2003:2012

data_securities_l <- melt(data_securities_w, id="date")

data_securities_l$date <- as.integer(data_securities_l$date)

names(data_securities_l)[2] <- 'Income_Group'

p_inc_securities <- ggplot(data_securities_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.80)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Securites (2007 = 100)") +  #,limits = c(0,30000)
  scale_color_brewer() #palette="Reds", palette="RdBu"

ggsave(p_inc_securities, file="p_inc_securities.jpg", dpi = 600,width = 9, height = 6, units = "in")


## Savings

data_savings <- data_inc_assets[complete.cases(data_inc_assets$SAVACCTX),]

data_savings <- subset(data_savings, select = c('date','INCLASS2','int_fac','w.SAVACCTX','FINLWT21'),drop = T)

data_savings <- aggregate(data_savings[,4:5],by = list(data_savings$int_fac),sum,simplify=T,na.rm=T)

data_savings$savings <- data_savings$w.SAVACCTX/data_savings$FINLWT21

data_savings$INCLASS2 <- as.factor(substring(as.character(data_savings$Group.1), first=1, last=1))

data_savings$date <- substring(as.character(data_savings$Group.1), first=3, last=12)

data_savings <- subset(data_savings, select = c('date','INCLASS2','savings'), drop=T)

data_savings_w <- reshape(data_savings, timevar = "INCLASS2", idvar = c("date"), direction = "wide")

data_savings_w <- data_savings_w[,-1]/apply(data_savings_w[5,-1],2,rep,dim(data_savings_w[,-1])[1])*100

data_savings_w$date <- 2003:2012

data_savings_l <- melt(data_savings_w, id="date")

data_savings_l$date <- as.integer(data_savings_l$date)

names(data_savings_l)[2] <- 'Income_Group'

p_inc_savings <- ggplot(data_savings_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.80)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Securites (2007 = 100)") +  #,limits = c(0,30000)
  scale_color_brewer() #palette="Reds", palette="RdBu"

ggsave(p_inc_savings, file="p_inc_savings.jpg", dpi = 600,width = 9, height = 6, units = "in")








# Create an Index
# data_inc_exp_agg_w <- data_inc_exp_agg_w[,-1]/apply(data_inc_exp_agg_w[5,-1],2,rep,dim(data_inc_exp_agg_w[,-1])[1])*100


inc_checking_plot_w <- data_inc_assets_agg_w[,c('date','CKBKACTX.1','CKBKACTX.2','CKBKACTX.3',
                                                'CKBKACTX.4','CKBKACTX.5','CKBKACTX.6')] 

inc_checking_plot_w$lowest_third <- (data_inc_assets_agg_w$CKBKACTX.1 + data_inc_assets_agg_w$CKBKACTX.2)/2
inc_checking_plot_w$middle_third <- (data_inc_assets_agg_w$CKBKACTX.3 + data_inc_assets_agg_w$CKBKACTX.4)/2
inc_checking_plot_w$top_third    <- (data_inc_assets_agg_w$CKBKACTX.5 + data_inc_assets_agg_w$CKBKACTX.6)/2
inc_checking_plot_w <- inc_checking_plot_w[,c('date','lowest_third','middle_third','top_third')]

inc_securities_plot_w <- data_inc_assets_agg_w[,c('date','SECESTX.1','SECESTX.2','SECESTX.3',
                                                  'SECESTX.4','SECESTX.5','SECESTX.6')]

inc_securities_plot_w$lowest_third <- (data_inc_assets_agg_w$SECESTX.1 + data_inc_assets_agg_w$SECESTX.2)/2
inc_securities_plot_w$middle_third <- (data_inc_assets_agg_w$SECESTX.3 + data_inc_assets_agg_w$SECESTX.4)/2
inc_securities_plot_w$top_third    <- (data_inc_assets_agg_w$SECESTX.5 + data_inc_assets_agg_w$SECESTX.6)/2
inc_securities_plot_w <- inc_securities_plot_w[,c('date','lowest_third','middle_third','top_third')]


inc_savings_plot_w <- data_inc_assets_agg_w[,c('date','SAVACCTX.1','SAVACCTX.2','SAVACCTX.3',
                                               'SAVACCTX.4','SAVACCTX.5','SAVACCTX.6')]

inc_savings_plot_w$lowest_third <- (data_inc_assets_agg_w$SAVACCTX.1 + data_inc_assets_agg_w$SAVACCTX.2)/2
inc_savings_plot_w$middle_third <- (data_inc_assets_agg_w$SAVACCTX.3 + data_inc_assets_agg_w$SAVACCTX.4)/2
inc_savings_plot_w$top_third    <- (data_inc_assets_agg_w$SAVACCTX.5 + data_inc_assets_agg_w$SAVACCTX.6)/2
inc_savings_plot_w <- inc_savings_plot_w[,c('date','lowest_third','middle_third','top_third')]

# Checking accounts

inc_checking_plot_l <- melt(inc_checking_plot_w, id="date")

inc_checking_plot_l$date <- as.integer(inc_checking_plot_l$date)

names(inc_checking_plot_l)[2] <- 'Income_Group'

p_inc_checking <- ggplot(inc_checking_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.80)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Checking and Brokerage Account") + scale_color_brewer() #palette="Reds", palette="RdBu"

ggsave(p_inc_checking, file="p_inc_checking.jpg", dpi = 600,width = 9, height = 6, units = "in")

# Securities

inc_securities_plot_l <- melt(inc_securities_plot_w, id="date")

inc_securities_plot_l$date <- as.integer(inc_securities_plot_l$date)

names(inc_securities_plot_l)[2] <- 'Income_Group'

p_inc_securities <- ggplot(inc_securities_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.80)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Value of Securities") + scale_color_brewer() #palette="Reds", palette="RdBu"

ggsave(p_inc_securities, file="p_inc_securities.jpg", dpi = 600,width = 9, height = 6, units = "in")

# Savings Account

inc_savings_plot_l <- melt(inc_savings_plot_w, id="date")

inc_savings_plot_l$date <- as.integer(inc_savings_plot_l$date)

names(inc_savings_plot_l)[2] <- 'Income_Group'

p_inc_savings <- ggplot(inc_savings_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.80)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Balance in Saving Account") + scale_color_brewer() #palette="Reds", palette="RdBu"

ggsave(p_inc_savings, file="p_inc_savings.jpg", dpi = 600,width = 9, height = 6, units = "in")






# Nondurable expenditures

inc_nondurexp_plot_l <- melt(inc_nondurexp_plot_w, id="date")

names(inc_nondurexp_plot_l)[2] <- 'Income_Group'

p_inc_nondurexp <- ggplot(inc_nondurexp_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.88, 0.25)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Nondurable Expenditures (2003 = 100)") + scale_color_brewer()

ggsave(p_inc_nondurexp, file="p_inc_nondurexp.jpg", dpi = 600,width = 9, height = 6, units = "in")


# Durable expenditures

inc_durexp_plot_l <- melt(inc_durexp_plot_w, id="date")

names(inc_durexp_plot_l)[2] <- 'Income_Group'

p_inc_durexp <- ggplot(inc_durexp_plot_l,aes(x=date,y=value,color=Income_Group)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.15, 0.16)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Durable Expenditures (2003 = 100)") + scale_color_brewer()

ggsave(p_inc_durexp, file="p_inc_durexp.jpg", dpi = 600,width = 9, height = 6, units = "in")













quantile_data <- rbind.data.frame(quantile_data,quantile(temp1$TOTEXPPQ, c(.02, .05, .1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98)))

temp3 <- temp1$TOTEXPPQ-temp1$HOUSPQ-temp1$TRANSPQ

nondurexp_data <- rbind.data.frame(nondurexp_data,quantile(temp3, c(.02, .05, .1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98)))               

names(data) <- c('date','totexp')

data$date <- as.Date.numeric(data$date)

save(data,file="expsample.Rda")

load("expsample.Rda")

names(quantile_data) <- c("p02", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p98")
names(nondurexp_data) <- c("p02", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p98")

quantile_data$date <- date
nondurexp_data$date <- date

quantile_data <- quantile_data[,c("date","p02", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p98")]
nondurexp_data <- nondurexp_data[,c("date","p02", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p98")]

save(quantile_data,file="quantile_data.Rdata")
save(nondurexp_data,file="nondurexp_data.Rdata")

##################################################################################################
rm(list = ls())

load("quantile_data.Rdata")
date = seq.Date(as.Date("2003-01-01"), by="1 quarter", to = as.Date("2012-12-31"))

quantile_data_long <- melt(quantile_data, id="date")

names(quantile_data_long)[2] <- "Quantile" 

p1 <- ggplot(quantile_data_long,aes(x=date,y=value,color=Quantile)) + geom_line(size = 1)

p1 + scale_color_brewer()

# As an index number

quantile_data_index <- quantile_data[,-1]/apply(quantile_data[1,-1],2,rep,dim(quantile_data[,-1])[1])*100

quantile_data_index$date <- date

quantile_data_index_long <- melt(quantile_data_index, id="date")

names(quantile_data_index_long)[2] <- "Quantile" 

p2 <- ggplot(quantile_data_index_long,aes(x=date,y=value,color=Quantile)) + geom_line(size = 1)

p2 + scale_color_brewer()

# Annual average as an index number

quantiles_agg <- quantile_data

quantiles_agg$date <- as.factor(format(quantile_data$date, "%Y"))

quantiles_agg <- aggregate(quantiles_agg[,-1],by = list(quantiles_agg$date),mean,simplify=T)

names(quantiles_agg) <- names(quantile_data)

quantiles_agg_index <- quantiles_agg[,-1]/apply(quantiles_agg[5,-1],2,rep,dim(quantiles_agg[,-1])[1])*100

quantiles_agg_index$date <- as.integer(2003:2012)

quantiles_agg_index_long <- melt(quantiles_agg_index, id="date")

names(quantiles_agg_index_long)[2] <- "Quantile" 

theme_set(theme_gray(base_size = 16))

p3 <- ggplot(quantiles_agg_index_long,aes(x=date,y=value,color = Quantile)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.88, 0.25)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Consumption Expenditures (2003 = 100)") + 
  scale_color_brewer()

p3 

ggsave(p3, file="p3.jpg", dpi = 600,width = 9, height = 6, units = "in")

##################################################################################################
# Now go to nondurable expenditure data

rm(list = ls())

load("nondurexp_data.Rdata")
date = seq.Date(as.Date("2003-01-01"), by="1 quarter", to = as.Date("2012-12-31"))

nondurexp_data_long <- melt(nondurexp_data, id="date")

names(nondurexp_data_long)[2] <- "Quantile" 

p4 <- ggplot(nondurexp_data_long,aes(x=date,y=value,color=Quantile)) + geom_line(size = 1) + scale_color_brewer()

# Annual average as an index number

nondurexp_data_agg <- nondurexp_data

nondurexp_data_agg$date <- as.factor(format(nondurexp_data_agg$date, "%Y"))

nondurexp_data_agg <- aggregate(nondurexp_data_agg[,-1],by = list(nondurexp_data_agg$date),mean,simplify=T)

names(nondurexp_data_agg) <- names(nondurexp_data_agg)

nondurexp_data_agg_index <- nondurexp_data_agg[,-1]/apply(nondurexp_data_agg[5,-1],2,rep,dim(nondurexp_data_agg[,-1])[1])*100

nondurexp_data_agg_index$date <- as.integer(2003:2012)

nondurexp_data_agg_index_long <- melt(nondurexp_data_agg_index, id="date")

names(nondurexp_data_agg_index_long)[2] <- "Quantile" 

theme_set(theme_gray(base_size = 16))

p5 <- ggplot(nondurexp_data_agg_index_long,aes(x=date,y=value,color = Quantile)) + geom_line(size = 1.2) +
  theme(legend.position= c(0.88, 0.25)) +
  scale_x_continuous("Year",breaks=seq(2003, 2012, 1)) +
  scale_y_continuous("Nondurable Consumption Expenditures (2003 = 100)") + 
  scale_color_brewer()