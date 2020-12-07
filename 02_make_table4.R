rm(list=ls())
library(readxl)
library(here)
library(ggplot2)
library(magrittr)

###################
## load data
###################
FellowEye <- read_excel(paste0(here::here(), "/data/UriSoiberman_FellowEye.xlsx"), skip = 2)

# filter out LASIK
FellowEye <- FellowEye[which(FellowEye$`Previous LASIK`==0),]


###################
## compute table elements
###################
# Mean Value (SE) 
get_mean_se <- function(param, period) {
  dat <- FellowEye[,grep(paste0("^", param), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  
  if(period == 1) {
    # b0 overrides 0
    dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]
    mean <- mean(dat[[1]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    se <- sd(dat[[1]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    cat(mean, " (", se, ")", "\n", sep = "")
  }
  
  else if(period == 2) {
    mean <- mean(dat[[3]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    se <- sd(dat[[3]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    cat(mean, " (", se, ")", "\n", sep = "")
  }
  
  else if(period == 3) {
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]
    mean <- mean(dat[[10]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    se <- sd(dat[[10]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    cat(mean, " (", se, ")", "\n", sep = "")
  }
}

# Pre op to post op mean change
get_change_mean <- function(param, period1=1, period2) {
  dat <- FellowEye[,grep(paste0("^", param), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  
  mean <- rep(NA, 3)
  
  # b0 overrides 0
  dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]
  mean[1] <- mean(dat[[1]], na.rm = TRUE)

  mean[2] <- mean(dat[[3]], na.rm = TRUE)

  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]
  mean[3] <- mean(dat[[10]], na.rm = TRUE)
  
  change <- (mean[period2] - mean[period1]) %>% format(digits=2, nsmall=2)
  cat(change, "\n", sep = "")
}

# Pre op to post op p-value
get_change_pval <- function(param, period1=1, period2) {
  dat <- FellowEye[,grep(paste0("^", param), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  
  # b0 overrides 0
  dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]

  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]

  if(period1==3) {period1=10}
  else if(period1==2) {period1=3}

  if(period2==3) {period2=10}
  else if(period2==2) {period2=3}
  
  pval <- t.test(dat[[period1]], dat[[period2]])$p.value %>% round(2) %>% format(digits=2, nsmall=2)
  cat(pval, "\n", sep = "")
}

# Pre op to post op 95% CI
get_change_ci <- function(param, period1=1, period2) {
  dat <- FellowEye[,grep(paste0("^", param), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  
  # b0 overrides 0
  dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]
  
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]
  
  if(period1==3) {period1=10}
  else if(period1==2) {period1=3}
  
  if(period2==3) {period2=10}
  else if(period2==2) {period2=3}
  
  ci <- t.test(dat[[period1]], dat[[period2]])$conf.int %>% round(2) %>% format(digits=2, nsmall=2)
  cat("[", ci[1], ", ", ci[2], "]", "\n", sep = "")
}


###################
## run functions
###################
# variable of interest
param <- c("K1", "K2", "Km", "K_Max_Front", "Astig", "Pachy_apex", "Pachy_thinnest", "Chamber_volume")

suppressWarnings(for (i in param) {
  cat(i, "\n")
  get_mean_se(i, 1)
  get_mean_se(i, 2)
  get_mean_se(i, 3)
})

suppressWarnings(for (i in param) {
  cat(i, "\n", "-", "\n")
  get_change_mean(i, , 2)
  get_change_mean(i, , 3)
})

suppressWarnings(for (i in param) {
  cat(i, "\n", "-", "\n")
  get_change_pval(i, , 2)
  get_change_pval(i, , 3)
})

suppressWarnings(for (i in param) {
  cat(i, "\n", "-", "\n")
  get_change_ci(i, , 2)
  get_change_ci(i, , 3)
})

