


#d2d data simulation

# -------------------------------------
# libraries and clear
# -------------------------------------

library(data.table)
library(magrittr)
library(sqldf)
library(ggplot2)

rm(list=ls())
# dev.off()


# -------------------------------------
# build data
# -------------------------------------


# cal table will hold the calendar dates (cal_dt), the calendar effect (cal_m), and portfolio production per cal_dt
cal_table <- data.table(
  cal_dt = 1:100,
  cal_m = 1:100 %>% divide_by(10) %>% sin() %>% multiply_by(0.2) %>% add(1),
  prod = rnorm(100, 1000, 50) %>% round(., 0)
)
cal_table[, dummy:=1]

# vintage table will hold the vintage effect for each month (cal_dt) of origination
vin_table <- data.table(
  cal_dt = 1:100,
  v_m = c(
    rnorm(50, mean = 1, sd = .02), 
    rnorm(4, mean = 1.1, sd = .02), 
    rnorm(8, mean = 1, sd = .02),
    rnorm(4, mean = 1.1, sd = .02), 
    rnorm(8, mean = 1, sd = .02),            
    rnorm(4, mean = 0.9, sd = .02), 
    rnorm(8, mean = 1, sd = .02),            
    rnorm(14, mean = 1, sd = .02)
  )
)
vin_table[, dummy:=1]

# mob table will hold the maturation effect or lifecycle curve
mob_table <- data.table(
  mob = 1:36, 
  mob_v = dnorm(1:36, mean=15, sd=5) %>% multiply_by(2) %>% round(., 2)
)
mob_table[, dummy:=1]


# acct table will hold the account numbers and their vintage (cal_dt or origin)
acct_table <- data.table(
  acct_num = seq(1, sum(cal_table$prod)),
  v = rep(cal_table$cal_dt, cal_table$prod)
)

acct_table[, group := sample(1:3, .N, replace = TRUE)]
acct_table[, dummy:=1]

cal_table[1:5,]
acct_table[, .N, by=v]


# -------------------------------------
# build portfolio
# -------------------------------------

# join acct_table to mob_table to have a lifecycle effect for each loan for each month
dt_mob <- merge.data.table(
  x = acct_table,
  y = mob_table,
  by = 'dummy',
  allow.cartesian = TRUE
)

dt_mob[, cal_dt := (v-1)+mob]

group_table <- data.table(
  group = c(1, 2, 3),
  value = c(-2, 0, 2)
)

dt_mob

# join that table to vin_table to add vintage effect
dt_vin <- merge.data.table(
  x = dt_mob,
  y = vin_table,
  by.x = 'v',
  by.y = 'cal_dt'
)

rm(dt_mob)

# now join that table to cal_table to bring in calendar effect
dt_hit <- merge.data.table(
  x = dt_vin,
  y = cal_table[, .(cal_dt, cal_m)],
  by = 'cal_dt'
)

rm(dt_vin)

dt_hit[, haz := mob_v * v_m * cal_m]

dt_hit[, rand := runif(n = .N)]

dt_hit[, hit := ifelse(rand <= haz, 1, 0)]

dt_hit[order(acct_num, mob), cs1 := cumsum(hit), by=acct_num]

dt_hit[order(acct_num, mob), cs2 := cumsum(cs1), by=acct_num]

dt <- dt_hit[cs2 < 2 & cal_dt >= 25, .(cal_dt, acct_num, v, mob, hit)]

rm(dt_hit)
rm(acct_table)
rm(cal_table)
rm(mob_table)
rm(vin_table)

dt 

str(dt)

# -------------------------------------
# how's it look?
# -------------------------------------

dt[, .(rate = mean(hit)), by=mob]       [order(mob)]    %>% ggplot(aes(x=mob, y=rate))      + geom_line() + ggtitle('by mob')
dt[, .(rate = mean(hit)), by=cal_dt]    [order(cal_dt)] %>% ggplot(aes(x=cal_dt, y=rate))   + geom_line() + ggtitle('by cal_dt')
dt[, .(rate = mean(hit)), by=v]         [order(v)]      %>% ggplot(aes(x=v, y=rate))        + geom_line() + ggtitle('by vintage')
dt[, .(accts = uniqueN(acct_num)), by=cal_dt]  %>% ggplot(aes(x=cal_dt, y=accts)) + geom_line() + ggtitle('accts in portfolio')



