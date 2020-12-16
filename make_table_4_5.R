rm(list=ls())
library(readxl)
library(here)
library(ggplot2)
library(magrittr)
library(kableExtra)

###################
## help function
###################

################### Table 4 ###################
get_table_4_by_variable <- function(variable) {
  ## process data
  dat <- FellowEye[,grep(paste0("^", variable), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  # baseline
  dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]
  # last visit
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]
  
  ## make table
  variable <- paste(c(variable,"Preoperative, N=","1 month, N=","3-31 months, N="),
                    c("", length(which(!is.na(dat[[1]]))), length(which(!is.na(dat[[3]]))), length(which(!is.na(dat[[10]])))), sep = "")
  table.var <- data.frame('Variable, N' = variable, check.names=FALSE)
  
  ## Median Value (IQR)  
  median.iqr <- rep("",4)
  # baseline
  median <- median(dat[[1]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[[1]], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  median.iqr[2] <- paste0(median, " (", iqr[1], ", ", iqr[2], ")", sep = "")
  # 1st month
  median <- median(dat[[3]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[[3]], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  median.iqr[3] <- paste0(median, " (", iqr[1], ", ", iqr[2], ")", sep = "")
  # last visit
  median <- median(dat[[10]], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[[10]], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  median.iqr[4] <- paste0(median, " (", iqr[1], ", ", iqr[2], ")", sep = "")
  # col 1
  table.var[["Median Value (IQR)"]] <- median.iqr
  
  ## Pre op to post op median change
  median.change <- rep("",4)
  # 1st month
  median.change[3] <- median(dat[[3]]-dat[[1]], na.rm = TRUE) %>% format(digits=2, nsmall=2)
  # last visit
  median.change[4] <- median(dat[[10]]-dat[[1]], na.rm = TRUE) %>% format(digits=2, nsmall=2)
  # col 2
  table.var[["Pre-op to post-op: median change"]] <- median.change

  ## Pre op to post op p-value
  pval <- rep("",4)
  # 1st month
  pval[3] <- wilcox.test(dat[[1]], dat[[3]], paired = TRUE)$p.value %>% round(3) %>% format(digits=3, nsmall=3)
  # last visit
  pval[4] <- wilcox.test(dat[[1]], dat[[10]], paired = TRUE)$p.value %>% round(3) %>% format(digits=3, nsmall=3)
  # col 3
  table.var[["Pre-op to post-op: pvalue"]] <- pval
  
  table.var
}



################### Table 5 ###################
get_table_5_by_variable <- function(variable) {
  ## process data
  dat <- FellowEye[,grep(paste0("^", variable), names(FellowEye), value=TRUE)]
  dat[] <- lapply(dat, function(x) {
    as.numeric(x)
  })
  # baseline
  dat[which(!is.na(dat[,2])), 1] <- dat[which(!is.na(dat[,2])), 2]
  # last visit
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 9]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 8]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 7]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 6]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 5]
  dat[which(is.na(dat[,10])), 10] <- dat[which(is.na(dat[,10])), 4]
  
  dat <- data.frame(dat)
  ## make table
  variable <- c(variable,"Preoperative","1 month","3-31 months")
  table.var <- data.frame(Variable = variable)

  # Median Value (iqr) among CXL
  median.iqr.cnt <- rep("",4)
  # baseline
  median <- median(dat[cxl,1], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[cxl,1], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  cnt <- length(which(!is.na(dat[cxl,1])))
  median.iqr.cnt[2] <- paste0(median, " (", iqr[1], ", ", iqr[2], "), N=", cnt, sep = "")
  # 1st month
  median <- median(dat[cxl,3], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[cxl,3], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  cnt <- length(which(!is.na(dat[cxl,3])))
  median.iqr.cnt[3] <- paste0(median, " (", iqr[1], ", ", iqr[2], "), N=", cnt, sep = "")
  # last visit
  median <- median(dat[cxl,10], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[cxl,10], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  cnt <- length(which(!is.na(dat[cxl,10])))
  median.iqr.cnt[4] <- paste0(median, " (", iqr[1], ", ", iqr[2], "), N=", cnt, sep = "")
  # col 1
  table.var[["Median Value (IQR), Count among CXL"]] <- median.iqr.cnt
  
  # Median Value (iqr) among non-CXL
  median.iqr.cnt <- rep("",4)
  # baseline
  median <- median(dat[noncxl,1], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[noncxl,1], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  cnt <- length(which(!is.na(dat[noncxl,1])))
  median.iqr.cnt[2] <- paste0(median, " (", iqr[1], ", ", iqr[2], "), N=", cnt, sep = "")
  # 1st month
  median <- median(dat[noncxl,3], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[noncxl,3], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  cnt <- length(which(!is.na(dat[noncxl,3])))
  median.iqr.cnt[3] <- paste0(median, " (", iqr[1], ", ", iqr[2], "), N=", cnt, sep = "")
  # last visit
  median <- median(dat[noncxl,10], na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  iqr <- quantile(dat[noncxl,10], probs= c(.25, .75), na.rm = TRUE) %>% round(2) %>% format(digits=2, nsmall=2)
  cnt <- length(which(!is.na(dat[noncxl,10])))
  median.iqr.cnt[4] <- paste0(median, " (", iqr[1], ", ", iqr[2], "), N=", cnt, sep = "")
  # col 2
  table.var[["Median Value (IQR), Count among non-CXL"]] <- median.iqr.cnt
  
  # Changes at post-op visit: Median group difference
  change.diff <- rep("",4)
  # 1st month
  change.cxl <- median(dat[cxl,3] - dat[cxl,1], na.rm = TRUE)
  change.noncxl <- median(dat[noncxl,3] - dat[noncxl,1], na.rm = TRUE)
  median.diff <- (change.cxl - change.noncxl) %>% round(2) %>% format(digits=2, nsmall=2)
  change.cxl %<>% round(2) %>% format(digits=2, nsmall=2) # format
  change.noncxl %<>% round(2) %>% format(digits=2, nsmall=2)
  change.diff[3] <- paste0(median.diff, " (", change.cxl, ", ", change.noncxl, ")", sep = "")
  # last visit
  change.cxl <- median(dat[cxl,10] - dat[cxl,1], na.rm = TRUE)
  change.noncxl <- median(dat[noncxl,10] - dat[noncxl,1], na.rm = TRUE)
  median.diff <- (change.cxl - change.noncxl) %>% round(2) %>% format(digits=2, nsmall=2)
  change.cxl %<>% round(2) %>% format(digits=2, nsmall=2) # format
  change.noncxl %<>% round(2) %>% format(digits=2, nsmall=2)
  change.diff[4] <- paste0(median.diff, " (", change.cxl, ", ", change.noncxl, ")", sep = "")
  # col 3
  table.var[["Changes at post-op visit: CXL"]] <- change.diff
  
  # Group difference of changes at post-op visit: p-value
  pval <- rep("",4)
  # # baseline
  # pval[2] <- wilcox.test((dat[[period]][cxl] - dat[[1]][cxl]), (dat[[period]][noncxl] - dat[[1]][noncxl]))$p.value %>%
  #   round(4) %>% format(digits=4, nsmall=4)
  # 1st month
  pval[3] <- wilcox.test((dat[cxl,3] - dat[cxl,1]), (dat[noncxl,3] - dat[noncxl,1]))$p.value %>%
    round(3) %>% format(digits=3, nsmall=3)
  # last visit
  pval[4] <- wilcox.test((dat[cxl,10] - dat[cxl,1]), (dat[noncxl,10] - dat[noncxl,1]))$p.value %>%
    round(3) %>% format(digits=3, nsmall=3)
  # col 4
  table.var[["Group difference in the change at post-op visit: p-value"]] <- pval

  table.var
}



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
## make table
###################
variable <- c("K1", "K2", "Km", "K_Max_Front", "Astig", "Pachy_apex", "Pachy_thinnest", "Chamber_volume")

# table 4
suppressWarnings(table4 <- get_table_4_by_variable(variable[1]))
suppressWarnings(for (v in variable[2:length(variable)]) {
  table4 <- rbind(table4, get_table_4_by_variable(v))
})
kable(table4) %>%
  kable_styling() %>%
  footnote(
    number = c("Median (IQR) for variable measurements \n",
               "Statistical tests performed: Wilcoxon rank-sum test"),
    footnote_as_chunk = T)

# table 5
suppressWarnings(table5 <- get_table_5_by_variable(variable[1]))
suppressWarnings(for (v in variable[2:length(variable)]) {
  table5 <- rbind(table5, get_table_5_by_variable(v))
})
kable(table5) %>%
  kable_styling() %>%
  footnote(
    number = c("Median (IQR), N for variable measurements \n",
               "Statistical tests performed: Wilcoxon rank-sum test \n",
               "P-value: Whether the difference between the changes in CXL/nonCXL group is significant"),
    footnote_as_chunk = T)
