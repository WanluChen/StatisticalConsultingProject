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
# CXL
cxl <- which(FellowEye$OD1OS2 == FellowEye$OD1OS2CXL)
# non-CXL
noncxl <- which(FellowEye$OD1OS2 != FellowEye$OD1OS2CXL)

###################
## compute table elements
###################
# Median Value (SE) among CXL/non-CXL
get_median_se_group <- function(param, period, group) {
  dat <- FellowEye[group, grep(paste0("^", param), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  
  if(period == 1) {
    # b0 overrides 0
    dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]
    median <- median(dat[[1]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    se <- sd(dat[[1]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    cat(median, " (", se, ")", "\n", sep = "")
  }
  
  else if(period == 2) {
    median <- median(dat[[3]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    se <- sd(dat[[3]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    cat(median, " (", se, ")", "\n", sep = "")
  }
  
  else if(period == 3) {
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
    dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]
    median <- median(dat[[10]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    se <- sd(dat[[10]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
    cat(median, " (", se, ")", "\n", sep = "")
  }
}

# Group difference at pre op visit or changes at post op visit: p-value
get_change_pval_group <- function(param, period) {
  dat <- FellowEye[, grep(paste0("^", param), names(FellowEye), value=TRUE)]
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
  
  if(period==3) {period=10}
  else if(period==2) {period=3}
  
  if(period==1) {
    pval <- wilcox.test(dat[[period]][cxl], dat[[period]][noncxl])$p.value %>%
      round(3) %>% format(digits=3, nsmall=3)
  }
  else {
    pval <- wilcox.test((dat[[period]][cxl] - dat[[1]][cxl]), (dat[[period]][noncxl] - dat[[1]][noncxl]))$p.value %>%
      round(2) %>% format(digits=2, nsmall=2)
  }
  
  cat(pval, "\n", sep = "")
}

###################
## run functions
###################
# variable of interest
param <- c("K1", "K2", "Km", "K_Max_Front", "Astig", "Pachy_apex", "Pachy_thinnest", "Chamber_volume")

suppressWarnings(for (i in param) {
  cat(i, "\n")
  get_median_se_group(i, 1, cxl)
  get_median_se_group(i, 2, cxl)
  get_median_se_group(i, 3, cxl)
})

suppressWarnings(for (i in param) {
  cat(i, "\n")
  get_median_se_group(i, 1, noncxl)
  get_median_se_group(i, 2, noncxl)
  get_median_se_group(i, 3, noncxl)
})

suppressWarnings(for (i in param) {
  cat(i, "\n")
  get_change_pval_group(i, 1)
  get_change_pval_group(i, 2)
  get_change_pval_group(i, 3)
})
