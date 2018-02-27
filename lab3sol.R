## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
concordance=TRUE
)

## ------------------------------------------------------------------------
library(vcd)
load("/Users/awilhelm/Data/creditcard.Rdata")
attach(creditcard)

## ------------------------------------------------------------------------
cc.t<-table(CreditCard)
cc.t
# alternatively, one can also use the command xtabs
cc.t2 <-xtabs(~CreditCard)
cc.t2

## ------------------------------------------------------------------------
cc.odds <- cc.t[2]/cc.t[1]
cc.odds

## ------------------------------------------------------------------------
prop.table(cc.t)[1]
cc.p <- 1 - mean(CreditCard,na.rm=TRUE)
cc.p
cc.p <- 1-cc.odds/(1+cc.odds)
cc.p

## ------------------------------------------------------------------------
#
table(CreditCard,HomeOwner)
cc.home.t<-xtabs(~HomeOwner + CreditCard)

## ----ccmosaic, fig.lp= "fig:", fig.cap = 'Mosaic plot visualising the relationship between home  ownership and credit card approval.',out.width='.6\\linewidth'----
mosaicplot(cc.home.t, main="Credit card approval by owning a home")

## ------------------------------------------------------------------------
#
cc.home.rp <- prop.table(cc.home.t,margin=1)
cc.home.rp
cc.home.cp <- prop.table(cc.home.t,margin=2)
cc.home.cp
# alternatively, after loading the library RcmdrMisc you can use the commands
# rowPercents and colPercents
library(RcmdrMisc)
rowPercents(cc.home.t)
colPercents(cc.home.t)

## ------------------------------------------------------------------------
or.cc.home<-oddsratio(cc.home.t, log=FALSE)

## ------------------------------------------------------------------------
rr.cc.home<-cc.home.t[1,2] * (cc.home.t[1,1]+cc.home.t[2,1]) / 
  ((cc.home.t[1,2]+cc.home.t[2,2])*cc.home.t[1,1])

rr.cc.home

## ------------------------------------------------------------------------
chisq.test(cc.home.t)$statistic
chisq.test(cc.home.t)$expected
cc.home.t-chisq.test(cc.home.t)$expected

## ------------------------------------------------------------------------
cc.home.f <- xtabs(~ HomeOwner + CreditCard, data = creditcard, 
                   subset = Gender == 0)
cc.home.f

chisq.test(cc.home.f)$statistic
chisq.test(cc.home.f)$expected
cc.home.m <- xtabs(~ HomeOwner + CreditCard, data = creditcard,  
                   subset = Gender == 1)
cc.home.m
chisq.test(cc.home.m)$statistic
chisq.test(cc.home.m)$expected

## ----chgender, fig.lp= "fig:", fig.cap = 'Mosaic plot visualising the relationship between home  ownership and credit card approval split by gender (males on the left; females on the right.',out.width='.6\\linewidth'----
par(mfrow=c(1,2))
mosaicplot(cc.home.m)
mosaicplot(cc.home.f)

## ------------------------------------------------------------------------
or.cc.home.m<-oddsratio(cc.home.m, log=FALSE)
or.cc.home.f<-oddsratio(cc.home.f, log=FALSE)
or.cc.home.m
or.cc.home.f

## ------------------------------------------------------------------------
cc.emp.t <- xtabs(~ CreditCard + Employment, data = creditcard)
cc.emp.t
chisq.test(cc.emp.t)$statistic

## ------------------------------------------------------------------------
assocstats(cc.emp.t)$phi
assocstats(cc.emp.t)$contingency
assocstats(cc.emp.t)$cramer

## ----ccemp, fig.lp= "fig:", fig.cap = 'Mosaic plot visualising the relationship between employment status and credit card approval.',out.width='.6\\linewidth'----
mosaicplot(cc.emp.t)

## ------------------------------------------------------------------------
cc.mar.t <- xtabs(~ CreditCard + MaritalStatus, data = creditcard)
cc.mar.t
chisq.test(cc.mar.t)$statistic

## ------------------------------------------------------------------------
assocstats(cc.mar.t)$phi
assocstats(cc.mar.t)$contingency
assocstats(cc.mar.t)$cramer

## ----ccmar, fig.lp= "fig:", fig.cap = 'Mosaic plot visualising the relationship between marital status and credit card approval.',out.width='.6\\linewidth'----
mosaicplot(cc.mar.t)

