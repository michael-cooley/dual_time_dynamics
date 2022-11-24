
# https://github.com/adu3110/HMM-DTD-RF-GBM-DTW/blob/master/R/dtd.R

library(data.table)
library(ggplot2)
library(gam)
library(plotly)
library(plot3D)

ls()

#######Number of Obligations and Time Points#######
Nobl <- 1000
T_final <- 200 #####time upto which we are generating asset price series######

#######Generating Unemployment Data (Macro economic variable) for all 1000 customers#######################

unemployment_series <- c(0.08) 

while(length(unemployment_series)<T_final){
  curr_unemp <- (unemployment_series[length(unemployment_series)] + rnorm(1, 0, 0.01))
  if(curr_unemp > 0.04 & curr_unemp < 0.12){
    unemployment_series <- c(unemployment_series, curr_unemp) 
  }
}


ggplot(data.table(arp = unemployment_series,
                  pt = 1:T_final)) + geom_line(aes(pt, arp))

#############Generating Obligations Data##########
data_customer <- data.table(oblig_id = rep(paste("ID", seq(1,Nobl,1), sep='_'), T_final),
                            vintage = rep(1:T_final, each=Nobl),
                            unemployment = rep(unemployment_series, each=Nobl),
                            age = -1)

#############Generating Age###########
for (i in 1:20){
  curr_cust_list <- paste("ID", seq((50*(i-1)+1),(50*(i))), sep="_")
  data_customer[oblig_id %in% curr_cust_list, age := vintage-1-9*(i-1)]
}

data_customer[age < 0, age:=NA]

#######20 clusters, 1st 50 customers with all 200 quaters,2nd 50 customers originated at 9th quater, etc.
###########Generating Default For DtD Modeling (randomly aloting defaults)########

data_customer[, is_default := -1]
data_customer[is.na(age), is_default := NA]

for (age_curr in unique(data_customer$age)){
  if(is.na(age_curr)){
    data_customer[age==age_curr, is_default := NA]
  }else if(age_curr <= 25){
    data_customer[age==age_curr, is_default := rbinom(nrow(data_customer[age==age_curr]),
                                                      1, (0.01 + 9*age_curr*0.0004))]
  }else{
    data_customer[age==age_curr, is_default := rbinom(nrow(data_customer[age==age_curr]),
                                                      1, 0.1)]
  }
}

View(data_customer[, mean(is_default),age])
ggplot(data_customer[, list(prob_def=mean(is_default)),age]) + geom_line(aes(age, prob_def))

#######GAM model for DtD########
gam_model <- gam(is_default ~ s(age, df = 3) + s(unemployment, df=3) + s(vintage, df=3), 
                 family = binomial, data = data_customer, na.action=na.omit)

#########Generating Asset Prices##############

sigma_i <-runif(Nobl, 0.03, 0.05)
A_i <- sigma_i *100000     #########asset_price####
B_i <- A_i - 1000          ##########default_threshold_price#####

############generating mu_i for each customer for each quater########
GetReturnRate <- function(dt, group){
  dt$prob_def <- 1/(1+exp(-predict(gam_model, newdata=dt)))
  mu_i <- (log(B_i[group]/A_i[group]) + dt[age>0]$age*(sigma_i[group]^2)/2 - 
             sigma_i[group]*sqrt(dt[age>0]$age)*qnorm(dt[age>0]$prob_def))/dt[age>0]$age
  dt$mu_i <- c(rep(NA, nrow(dt[is.na(age) | age==0])), mu_i)
  return(dt)
}

data_customer <- data_customer[, GetReturnRate(.SD, .GRP), oblig_id]

########generating asset price using merton's formula#####
GetAssetPrice <- function(dt, group){
  dt$asset_price <- exp(log(A_i[group]) + dt$mu_i*dt$age - dt$age*(sigma_i[group]^2)/2 + 
                          sigma_i[group]*sqrt(dt$age)*rnorm(nrow(dt), 0, 1))
  dt[age==0]$asset_price <- A_i[group]
  dt$default_threshold <- B_i[group]
  return(dt)
}

data_customer <- data_customer[, GetAssetPrice(.SD, .GRP), oblig_id]
data_customer <- data_customer[, default_or_not := as.numeric(asset_price < default_threshold)]

CreateRowData <- function(series){
  lst <- as.list(series)
  names(lst) <- paste0('T', (0:(length(series)-1)))
  return(lst)
}

data_cust_asset<- data_customer[,CreateRowData(asset_price), oblig_id]
data_cust_def_or_not<- data_customer[,CreateRowData(default_or_not), oblig_id]
########Plotting#######

ViewPricePlot <- function(Nobl_no){
  curr_obgl <- paste('ID', Nobl_no, sep='_')
  curr_dt <- data_customer[oblig_id == curr_obgl][!is.na(age)]
  p <- ggplot(curr_dt) + geom_point(aes(age, asset_price), size=0.2) + 
    geom_line(aes(age, asset_price)) + 
    geom_hline(aes(yintercept=unique(curr_dt$default_threshold), color='red'))+
    scale_color_identity(guide='legend', labels=c('default threshold'))+ ggtitle(paste("Asset Price for", Nobl_no, "obligation"))
  return(p)
}

ViewPricePlot(600) #######view price plot of 600th customer#####

ggplot(data_customer[!is.na(age), list(prob_def=mean(asset_price<default_threshold)),age]) + 
  geom_line(aes(age, prob_def))


def_rate <- data_customer[!is.na(age), list(prob_def=mean(asset_price<default_threshold)),.(vintage, age)]



Sys.setenv("plotly_username"="aditi_tiwari")
Sys.setenv("plotly_api_key"="Ow0zdx8YcWbyQsdeyJQs")

p <- plot_ly(def_rate, x = ~vintage, y = ~age, z = ~prob_def, 
             type = 'scatter3d', mode = 'surface',
             opacity = 1, line = list(width = 6, 
                                      color = ~vintage, 
                                      reverscale = T))

p

View(data_customer[oblig_id=='ID_100'])