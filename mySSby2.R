 
################ replace extreme values with Inf or 0

limit <- function(x) {
  for (i in 1:length(x))
    if (x[i]>10000) x[i] = Inf
    else if (x[i]<0.0001) x[i] = 0
  
    x
}


proc_total <- function(x) paste0(x, " (", scales::percent(x/sum(x), 0.1) , ")")
proc_total2 <- function(x) 
  paste0(table(x), 
         " (", 
         prop.table(table(x)) %>% as.numeric() %>% scales::percent(),
         ")") %>% 
  set_names(levels(factor(x))%>% as.character())

#table(baza_de_date$"Etiology", baza_de_date$"DM onset")["biliary","late onset"] # 3
proc_total2x2 <- function(x,y) 
  paste0(table(x, y), 
         " (", 
         prop.table(table(x, y), margin=1) %>% as.numeric() %>% scales::percent(),
         ")") %>% matrix(nrow = length(levels(x))) %>% 
  set_rownames( levels( factor(x) ) %>% as.character() ) %>%
  set_colnames( levels( factor(y) ) %>% as.character() )

proc_total_perc_only <- function(x) 
  prop.table(table(x)) %>% as.numeric() %>% scales::percent() %>% set_names(levels(x))

mean.sd <- function(x) 
  paste0(mean(x, na.rm=T) %>% scales::number(0.1), " ±", sd(x, na.rm=T) %>% scales::number(0.1))

median.iqr <- function(x) 
  paste0(median(x, na.rm=T) %>% scales::number(0.1), " (", IQR(x, na.rm=T) %>% scales::number(0.1), ")")

x.ci <- function(x, lcl, ucl) 
  paste0(scales::number(x, 0.1), " (", scales::number(lcl, 0.1), " to ", scales::number(ucl, 0.1), ")")

x.ci.triplet <- function(x) 
  x.ci(x[1], x[2], x[3])

################## f: works for binary explanatory

f <- function(pair, p="fisher", rr=F, rr_inverse=F, or=T, ci="asymptotic", phi=T) {
  dependent = attr(pair$terms, "variables")[[2]]
  expalanatory = attr(pair$terms, "variables")[[3]]
  
  s <- paste0("OR=", exp(pair$coefficients)[2] %>% scales::number(0.01),  
              " [", exp(confint(pair))[2,] %>% scales::number(0.01) %>% paste0(collapse = ", "), "] (",
              summary(pair)$coefficients[2,4] %>% scales::pvalue(add_p = T),", ", 
              anova(pair, test="Chisq")$`Pr(>Chi)`[2] %>% scales::pvalue(add_p = T), ")")
  
  
  ########## table
  
  temp <- pair$data %>% select(`dependent` = as.character(dependent), `expalanatory` = as.character(expalanatory)) %>% na.omit()
  len <- nrow(temp)
  t1 <- xtabs(~expalanatory+dependent, data=temp)
  t1_inverse <- xtabs(~dependent+expalanatory, data=temp) # for risk ratio: dependent is exposure
  
  m1 <- t1 %>% data.frame() %>% tidyr::spread(dependent, `Freq`)
  m2 <- table(temp$expalanatory) %>% as.vector()
  
  t2 <- cbind(m1, Total=m2) %>% 
    mutate_if(is.numeric, proc_total) %>%
    rename("Levels"=expalanatory)
  
  #print(t2)
  if (levels(t2$Levels) %>% sort() %>% tolower() == c("no", "yes")) 
    t2 <- t2 %>% filter( tolower(Levels) == "yes" )
  if (levels(t2$Levels) %>% sort() %>% tolower() == c("da", "nu")) 
    t2 <- t2 %>% filter( tolower(Levels) == "da" )
  
  
  
  ######### statistics
  
  a2 <- vcd::assocstats(t1) #$ `contingency`, `phi`, `cramer` (V), `chisq_tests`[2,3], `table`
  Fisher <- fisher.test(t1) #$ `p.value`, `estimate` (OR), `conf.int` [1:2] # CI the same as DescTools's method = "mle"
  RR = DescTools::RelRisk(t1, conf.level = 0.95) #$ 1=`rel. risk`, 2=`lwr.ci`, 3=`upr.ci`
  RR_inverse = DescTools::RelRisk(t1_inverse, conf.level = 0.95) #$ 1=`rel. risk`, 2=`lwr.ci`, 3=`upr.ci`
  OR = DescTools::OddsRatio(t1, conf.level = 0.95) #$ `odds ratio`, `lwr.ci`, `upr.ci`
  s2<-"" # result
  
  
  .or = NA 
  .or.ci = NA
  if (or == T) { 
    .or <- OR[1]
    if ( ci == "asymptotic" ) .or.ci = OR[2:3] # default
    else if ( ci == "exact" ) .or.ci = DescTools::OddsRatio(t1, conf.level = 0.95, method="mle")[2:3] # same as Fisher
    
    if( is.na(.or.ci) ) .or.ci=""
    else .or.ci = paste0(" [", scales::number(limit(.or.ci), 0.01) %>% paste0(collapse = ", "), "]")
  }
  if( is.na(.or) ) .or=NA # default
  else .or = paste0("OR=", scales::number(limit(.or), 0.01), .or.ci)
  
  
  .rr = NA
  .rr.ci = NA
  if (rr == T) { # default
    .rr <- RR[1]
    if ( ci == "asymptotic" ) .rr.ci = DescTools::RelRisk(t1, conf.level = 0.95, method="wald")[2:3]  # default
    else if ( ci == "exact" ) .rr.ci = RR[2:3]
    
    if( is.na(.rr.ci) ) .rr.ci=""
    else .rr.ci = paste0(" [", scales::number(limit(.rr.ci), 0.01) %>% paste0(collapse = ", "), "]")
  }
  if( is.na(.rr) ) .rr=NA
  else .rr = paste0("RR=", scales::number(limit(.rr), 0.01), .rr.ci)  # default
  
  .rr_inverse = NA
  .rr.ci_inverse = NA
  if (rr_inverse == T) { # default
    .rr_inverse <- RR_inverse[1]
    if ( ci == "asymptotic" ) .rr.ci_inverse = DescTools::RelRisk(t1_inverse, conf.level = 0.95, method="wald")[2:3]  # default
    else if ( ci == "exact" ) .rr.ci_inverse = RR_inverse[2:3]
    
    if( is.na(.rr.ci_inverse) ) .rr.ci_inverse=""
    else .rr.ci_inverse = paste0(" [", scales::number(limit(.rr.ci_inverse), 0.01) %>% paste0(collapse = ", "), "]")
  }
  if( is.na(.rr_inverse) ) .rr_inverse=NA
  else .rr_inverse = paste0("RR*=", scales::number(limit(.rr_inverse), 0.01), .rr.ci_inverse)  # default
  
  
  if (phi == T) .phi = paste0("phi=", scales::pvalue(a2$phi, add_p = F, accuracy = 0.01) )  # default
  else .phi=NA
  
  
  .p = NA
  if (p == "fisher") .p <- Fisher$p.value # default
  else if (p == "chi2") .p <- chisq.test(t1)$p.value
  else if (p == "yates") .p <- chisq.test(t1, correct = T)$p.value
  else if (p == "pearson") .p <- a2$`chisq_tests`[2,3]
  else if (p == "likelihood") .p <- a2$`chisq_tests`[1,3]
  
  if(or==F & rr==F & rr_inverse==F & phi==F) 
    s2 <- paste0( scales::pvalue(.p, add_p = T), 
                  ifelse(p == "chi2" & grepl("Yates", chisq.test(t1)$method), "*", "") ) 
  else if( is.na(.p) ) .p=NA
  else {
    .p = paste0("(", scales::pvalue(.p, add_p = T), ifelse(p == "chi2" & grepl("Yates", chisq.test(t1)$method), "*", ""), ")")
    s2 <- paste(paste(c(.or, .rr, .rr_inverse, .phi) %>% na.omit(), collapse = ", "), .p, collapse = " ")
  }
  
  #s2
  
  
  t2$Statistics = NA
  t2$Statistics[1] = s2
  t2
}




################## f2: works for categ explanatory

f2 <- function(pair, p.n="pearson", or.n=F, ci.n=F, v=T) {
  dependent = attr(pair$terms, "variables")[[2]]
  expalanatory = attr(pair$terms, "variables")[[3]]
  
  s <- paste0("OR=", exp(pair$coefficients)[2] %>% scales::number(0.01),  
              " [", exp(confint(pair))[2,] %>% scales::number(0.01) %>% paste0(collapse = ", "), "] (",
              summary(pair)$coefficients[2,4] %>% scales::pvalue(add_p = T),", ", 
              anova(pair, test="Chisq")$`Pr(>Chi)`[2] %>% scales::pvalue(add_p = T), ")")
  
  #print(s)
  ########## table
  
  temp <- pair$data %>% select(`dependent` = as.character(dependent), `expalanatory` = as.character(expalanatory)) %>% na.omit()
  len <- nrow(temp)
  t1 <- xtabs(~expalanatory+dependent, data=temp)
  
  m1 <- t1 %>% data.frame() %>% tidyr::spread(dependent, `Freq`)
  m2 <- table(temp$expalanatory) %>% as.vector()
  
  t2 <- cbind(m1, Total=m2) %>% 
    mutate_if(is.numeric, proc_total) %>%
    rename("Levels"=expalanatory)
  
  
  
  ######### statistics
  
  a2 <- vcd::assocstats(t1) #$ `contingency`, `phi`, `cramer` (V), `chisq_tests`[2,3], `table`
  #Fisher <- fisher.test(t1) #$ `p.value`, `estimate` (OR), `conf.int` [1:2] # CI the same as DescTools's method = "mle"
  #RR = DescTools::RelRisk(t1, conf.level = 0.95) #$ 1=`rel. risk`, 2=`lwr.ci`, 3=`upr.ci`
  #OR = DescTools::OddsRatio(t1, conf.level = 0.95) #$ `odds ratio`, `lwr.ci`, `upr.ci`
  s2<-"" # result
  
  s <- paste0("OR=", exp(pair$coefficients)[2] %>% scales::number(0.01),  
              " [", exp(confint(pair))[2,] %>% scales::number(0.01) %>% paste0(collapse = ", "), "] (",
              summary(pair)$coefficients[2,4] %>% scales::pvalue(add_p = T),", ", 
              anova(pair, test="Chisq")$`Pr(>Chi)`[2] %>% scales::pvalue(add_p = T), ")")
  
  .or = NA
  .or.ci = NA
  if (or.n == T) {
    .or <- exp(pair$coefficients)[2]
    if ( ci.n == T ) .or.ci = paste0(" [", exp(confint(pair))[2,] %>% limit() %>% scales::number(0.01) %>% paste0(collapse = ", "), "]")
    else .or.ci=""
  }
  if( is.na(.or) ) .or=NA # default
  else .or = paste0("OR=", scales::number(limit(.or), 0.01), .or.ci)

  if (v == T) .v = paste0("V=", scales::pvalue(a2$cramer, add_p = F, accuracy = 0.01) )  # default
  else .v = NA
  
  
  .p = NA
  if (p.n == "pearson") .p <- a2$`chisq_tests`[2,3]
  else if (p.n == "likelihood") .p <- a2$`chisq_tests`[1,3]
  else if (p.n == "glm") .p <- summary(pair)$coefficients[2,4]
  else if (p.n == "aod.glm") .p <- anova(pair, test="Chisq")$`Pr(>Chi)`[2]
  
  if(or.n==F & v==F) 
    s2 <- scales::pvalue(.p, add_p = T) 
  else if( is.na(.p) ) .p=NA
  else {
    .p = paste0("(", scales::pvalue(.p, add_p = T), ")")
    s2 <- paste(paste(c(.or, .v) %>% na.omit(), collapse = ", "), .p, collapse = " ")
  }
  
  #s2
  
  t2$Statistics = NA
  t2$Statistics[1] = s2
  t2
}




################## g: works for numeric explanatory

g <- function(pair, g.rows=c("med_range")) {
  dependent = attr(pair$terms, "variables")[[2]]
  expalanatory = attr(pair$terms, "variables")[[3]]
  
  s <- paste0("OR=", exp(pair$coefficients)[2] %>% scales::number(0.01),  
              " [", exp(confint(pair))[2,] %>% scales::number(0.01) %>% paste0(collapse = ", "), "] (",
              summary(pair)$coefficients[2,4] %>% scales::pvalue(add_p = T),", ", 
              anova(pair, test="Chisq")$`Pr(>Chi)`[2] %>% scales::pvalue(add_p = T), ")")
  #return (s)
  
  
  ########## table
  gm_correction = 0.001
  
  temp <- pair$data %>% select(`dependent` = as.character(dependent), `expalanatory` = as.character(expalanatory))# %>% na.omit()
  temp$expalanatory <- as.numeric(temp$expalanatory)
  
  #return (temp)
  temp_total <- temp$`expalanatory` %>% na.omit() # allow values without a level
  #temp_total <- na.omit(temp)$`expalanatory` # don't allow values without a level
  temp_p1 <- temp$`expalanatory`[temp$`dependent` == levels(temp$`dependent`)[1]] %>% na.omit()
  temp_p2 <- temp$`expalanatory`[temp$`dependent` == levels(temp$`dependent`)[2]] %>% na.omit()
  t <- length(na.omit(temp$`dependent`))
  p1 <- table(temp$`dependent`)[levels(temp$`dependent`)[1]]
  p2 <- table(temp$`dependent`)[levels(temp$`dependent`)[2]]
  
  levels = c("Total", levels(na.omit(temp$`dependent`)))
  mean_sd <- c(paste0(format(mean(temp_total), nsmall = 2), " ±", format(sd(temp_total), nsmall = 1)),
                paste0(format(mean(temp_p1), nsmall = 2), " ±", format(sd(temp_p1), nsmall = 1)),
                paste0(format(mean(temp_p2), nsmall = 2), " ±", format(sd(temp_p2), nsmall = 1)))
  min_med_max = c(paste(min(temp_total), median(temp_total), max(temp_total), sep=":"),
                  paste(min(temp_p1), median(temp_p1), max(temp_p1), sep=":"),
                  paste(min(temp_p2), median(temp_p2), max(temp_p2), sep=":"))
  med_iqr = c(paste(median(temp_total), " (", quantile(probs=0.25, temp_total), "-", quantile(probs=0.75, temp_total), ")",sep=""),
                  paste(median(temp_p1), " (", quantile(probs=0.25, temp_p1), "-", quantile(probs=0.75, temp_p1), ")",sep=""),
                  paste(median(temp_p2), " (", quantile(probs=0.25, temp_p2), "-", quantile(probs=0.75, temp_p2), ")",sep=""))
  med_range = c(paste(median(temp_total, na.rm=T), " (", min(temp_total, na.rm=T), ":", max(temp_total, na.rm=T), ")",sep=""),
              paste(median(temp_p1), " (", quantile(probs=0, temp_p1), ":", quantile(probs=1, temp_p1), ")",sep=""),
              paste(median(temp_p2), " (", quantile(probs=0, temp_p2), ":", quantile(probs=1, temp_p2), ")",sep=""))
  geo_mean <- c(paste0(format(DescTools::Gmean(temp_total+gm_correction), nsmall = 2), " ±", format(DescTools::Gsd(temp_total+gm_correction), nsmall = 1)),
                paste0(format(DescTools::Gmean(temp_p1+gm_correction), nsmall = 2), " ±", format(DescTools::Gsd(temp_p1+gm_correction), nsmall = 1)),
                paste0(format(DescTools::Gmean(temp_p2+gm_correction), nsmall = 2), " ±", format(DescTools::Gsd(temp_p2+gm_correction), nsmall = 1)))
  
  
  
  
  SW_test    <- shapiro.test(temp$`expalanatory`)$p.value
  F_test     <- var.test(temp_p1, temp_p2)$p.value
  Welch_test <- t.test(temp_p1, temp_p2, var.equal = F)$p.value
  T_test     <- t.test(temp_p1, temp_p2, var.equal = T)$p.value
  MW_test    <- wilcox.test(temp_p1, temp_p2)$p.value
  
  p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test)) %>% scales::pvalue(add_p = T)
  test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch", "T-test"))
  
  t2 = data.frame(t(rep(NA, 5)))
  for (template in g.rows) {
    x <- switch(
      template,
      "med_range" = data.frame(t(c("Med (range)", med_range, NA))),
      "med_iqr" = data.frame(t(c("Med (IQR)", med_iqr, NA))),
      "min_med_max" = data.frame(t(c("Min:Med:Max", min_med_max, NA))),
      "mean_sd" = data.frame(t(c("Mean ±SD", mean_sd, NA))),
      "geo_mean" = data.frame(t(c("GMean ±SD", geo_mean, NA)))
    )
    
    t2 %<>% bind_rows(x)
  }
  
  t2 %<>%
    set_colnames(c("Levels", levels, "Statistics")) %>% 
    mutate_all(as.character) 
  
  t2 <- t2[-1, ]
  t2$Statistics[1] <- paste0(test, ": ", p)
  
  # 
  # 
  # t2 <- data.frame(t(c("Mean ±SD", mean_sd, paste0(test, ": ", p)))) %>% 
  #   #bind_rows(data.frame(t(c("GMean ±SD", geo_mean, NA)))) %>%
  #   bind_rows(data.frame(t(c("Min:Med:Max", min_med_max, NA)))) %>%
  #   set_colnames(c("Levels", levels, "Statistics")) %>% 
  #   mutate_all(as.character)
  # 
  # t2 <- data.frame(t(c("Med (range)", med_range, paste0(test, ": ", p)))) %>% 
  #   set_colnames(c("Levels", levels, "Statistics")) %>% 
  #   mutate_all(as.character)
  # 
  return(t2)
  
  
  
}


################## choose f, f2 or g

h <- function(pair, p="fisher", rr=F, rr_inverse=F, or=T, or.n=F, ci="asymptotic", ci.n=F, phi=T, p.n = "pearson", v=T, g.rows=c("med_range")) {
  
  dependent = attr(pair$terms, "variables")[[2]]
  expalanatory = attr(pair$terms, "variables")[[3]]
  
  s <- paste0("OR=", exp(pair$coefficients)[2] %>% scales::number(0.01),  
              " [", exp(confint(pair))[2,] %>% scales::number(0.01) %>% paste0(collapse = ", "), "] (",
              summary(pair)$coefficients[2,4] %>% scales::pvalue(add_p = T),", ", 
              anova(pair, test="Chisq")$`Pr(>Chi)`[2] %>% scales::pvalue(add_p = T), ")")
  #return (s)
  

  
  temp <- pair$data %>% select(`dependent` = as.character(dependent), `expalanatory` = as.character(expalanatory))# %>% na.omit()
  
  if(is.factor(temp$`expalanatory`))
    if(length(levels(temp$expalanatory))==2)  
      return(f(pair, or=or, phi=phi, rr=rr, rr_inverse=rr_inverse, p=p, ci=ci))
    else return(f2(pair, v=v, p.n=p.n, or.n=or.n, ci.n=ci.n)) #, or=T, v=T
  else return(g(pair, g.rows=g.rows))
  
  
  
}




glmlist <- function (.data, dependent, explanatory) {
  result <- list()
  for (i in 1:length(explanatory)) {
    result[[i]] <- glm(paste(dependent, "~", explanatory[i]), 
                       data = .data, family = "binomial")
  }
  class(result) = "glmlist"
  return(result)
}


################## testing

# db <- baza_de_date %>%
#   mutate(
#     `Etiology` = forcats::fct_collapse(`Etiology`, "toxic/metabolic" = c("ethanolic", "metabolic")) %>% forcats::fct_rev(),
#     `Balthazar score` = forcats::fct_collapse(`Balthazar score`, "A-D" = c("A", "B", "C", "D")),
#     `Necrosis severity` = forcats::fct_collapse(`Necrosis severity`, ">0%" = c("0-30%",  "30-50%", ">50%")),
#     `Atlanta severity (numeric)` = as.numeric(`Atlanta severity`),
#     `Balthazar score (numeric)` = as.numeric(`Balthazar score`),
#     `Necrosis severity (numeric)` = as.numeric(`Necrosis severity`)-1,
#   ) %>%
#   select(
#     `Local complications`,
#     `Age (years)`,
#     `Sex`,
#     `Etiology`,
#     `Atlanta severity`, `Atlanta severity (numeric)`,
#     `Balthazar score`, `Balthazar score (numeric)`,
#     `Necrosis severity`,  `Necrosis severity (numeric)`,
#     `No. AP episodes`
#   )
# 
# #levels(db$`Etiology`) <- c("billiary", "toxic/metabolic", "toxic/metabolic")
# #db$`Etiology` <- forcats::fct_rev(db$`Etiology`)
# 
# dep_labels <- names(db)[1] # "Local complications"
# exp_labels <- names(db)[-1] #%>% str_replace_all("_"," - ")
# 
# dep <- "local.complications"
# exp <- make.names(exp_labels) %>%
#   str_replace_all("\\.+","\\.") %>%
#   str_remove_all("(^\\.)|(\\.$)") %>%
#   tolower()
# exp2 <- exp[-(grep(".numeric", exp)-1)]
# names(db) <- c(dep, exp)
# #str(db)
# 
# for (variable in names(db)) {
#   #if(is.factor(db[[variable]]))
#   #  db[[variable]] <- forcats::fct_rev(db[[variable]])
#   attr(db[[variable]], "label") <- exp_labels[exp==variable]
# }
# #attr(db$atlanta.severity, "label") = "AP severity (Atlanta)"
# #attr(db$atlanta.severity.numeric, "label") = "AP severity (Atlanta)"
# 
# uni2 <- glmlist(db, dep, exp)
# #h(uni2[[2]], or=F, phi=F, rr=T)
# 
# df <- lapply(uni2, FUN=h, or=F, phi=F, rr=T) %>%
#   bind_rows(.id = "Factor") %>%
#   mutate(`Factor` = factor(`Factor`))
# levels(df$Factor) = exp_labels[as.numeric(levels(df$Factor))]
# df$Factor <- as.character(df$Factor)
# df$Factor[duplicated(df$Factor)] <- ""
# 
# t <- table(db[[dep]])
# r1 <- rep("", length(df)) %>% set_names(names(df))
# r1[1] <- dep_labels
# r1[4] <- paste0(t[1], " (", scales::number(100*t[1]/(t[1]+t[2]), 0.1), "%)")
# r1[5] <- paste0(t[2], " (", scales::number(100*t[2]/(t[1]+t[2]), 0.1), "%)")
# r1$Total <- t[1]+t[2]
# r1 <- data.frame(r1) %>% set_colnames(names(df)) %>% set_rownames(NULL)
# df <- rbind(r1, df)
# 
# df %<>% mutate_if(is.factor, as.character)
# df[is.na(df)] <- ""
# 
# df
