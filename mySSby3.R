 
############### Some helper functions


# replace extreme values with Inf or 0
limit <- function(x, .min=0, .max=10000) {
  for (i in 1:length(x))
    if (x[i]>10000) x[i] = .max
    else if (x[i]<0.0001) x[i] = .min
    x
}

# some formatting
proc_total <- function(x, accuracy = 0.1) paste0(x, " (", scales::percent(x/sum(x), accuracy = accuracy) , ")")
proc_total2 <- function(x, accuracy = 0.1) 
  paste0(table(x), 
         " (", 
         prop.table(table(x)) %>% as.numeric() %>% scales::percent(accuracy = accuracy),
         ")") %>% 
  set_names(levels(factor(x))%>% as.character())

#table(baza_de_date$"Etiology", baza_de_date$"DM onset")["biliary","late onset"] # 3
proc_total2x2 <- function(x,y, accuracy = 0.1) 
  paste0(table(x, y), 
         " (", 
         prop.table(table(x, y), margin=1) %>% as.numeric() %>% scales::percent(accuracy = accuracy),
         ")") %>% matrix(nrow = length(levels(x))) %>% 
  set_rownames( levels( factor(x) ) %>% as.character() ) %>%
  set_colnames( levels( factor(y) ) %>% as.character() )

proc_total_perc_only <- function(x, accuracy = 0.1) 
  prop.table(table(x)) %>% as.numeric() %>% scales::percent(accuracy = accuracy) %>% set_names(levels(x))

mean.sd <- function(x, accuracy = 0.1) 
  paste0(mean(x, na.rm=T) %>% scales::number(accuracy = accuracy), " ±", 
         sd(x, na.rm=T) %>% scales::number(accuracy = accuracy))

median.iqr <- function(x, accuracy = 0.1) 
  paste0(median(x, na.rm=T) %>% scales::number(accuracy = accuracy), 
         " (", IQR(x, na.rm=T) %>% scales::number(accuracy = accuracy), ")")

x.ci <- function(x, lcl, ucl, accuracy = 0.01) 
  paste0(scales::number(x, accuracy = accuracy), 
         " (", scales::number(lcl, accuracy = accuracy), " to ", scales::number(ucl, accuracy = accuracy), ")")

x.ci.triplet <- function(x, accuracy = 0.01) 
  x.ci(x[1], x[2], x[3], accuracy)

# append `Total` level to `dependent` column
CreateTotalDependent <- function(df, col){
  df$dependent <- df[[col]]
  temp <- df
  temp$dependent <- "Total"
  merged <-rbind(temp, df)
  
  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])
  
  return(merged)
}

# pick summary statistics from a summary table (as in g(...)) based on col and row names, handling missing levels
pick <- function(s_df, dep, val) {
  vals <- rep(NA, length(dep))
  names(vals) <- dep
  for (i in dep) {
    if (i %in% s_df$dependent) {
      vals[i] <- s_df[[val]][s_df$dependent==i]
    }
  }
  return (vals)
}




#### main function 1: get a list of pair of variables, with relevant columns

pairlist <- function (data, dependent, explanatory) {
  # checks if variables exist
  
  if (dependent %in% explanatory) warning("autocorrelation")
  if (dependent %!in% names(data)) stop("dependent not available")
  if (length(explanatory[explanatory %in% names(data)]) == 0)  warning("no valid explanatory provided")
  
  result <- list()
  for (i in explanatory) {
    if (explanatory %in% names(data)) {
    result[[i]] <- list(dependent = dependent, explanatory = explanatory, 
                        data = data %>% select(`dependent` = as.character(dependent), 
                                               `explanatory` = as.character(i)) )
    }
  }
  return(result)
}


#### main function 2: process each pair with the relevant function

pair.process <- function(pair, p="fisher", rr=F, rr_inverse=F, or=T, or.n=F, ci="asymptotic", ci.n=F, phi=T, p.n = "pearson", v=T, 
                         g.rows=c("med_range"), gm_correction = 0.001, paired=F, prefer="auto") {
  
  dependent = pair$dependent
  explanatory = pair$explanatory
  
  temp <- pair$data #%>% select(`dependent` = as.character(dependent), `explanatory` = as.character(explanatory))# %>% na.omit()
  
  if(is.factor(temp$`explanatory`))
    if(length(levels(temp$explanatory))==2 & length(levels(temp$dependent))==2)  
      return(f(pair, or=or, phi=phi, rr=rr, rr_inverse=rr_inverse, p=p, ci=ci))
  else return(f2(pair, v=v, p.n=p.n, or.n=or.n, ci.n=ci.n)) #, or=T, v=T
  else return(g(pair, g.rows=g.rows, gm_correction = gm_correction, paired=paired, prefer=prefer ))
  
}







################## f: works for binary explanatory

f <- function(pair, p="fisher", rr=F, rr_inverse=F, or=T, ci="asymptotic", phi=T) {
  dependent = pair$dependent
  explanatory = pair$explanatory
  
  ########## table
  
  temp <- pair$data %>% 
    #select(`dependent` = as.character(dependent), `explanatory` = as.character(explanatory)) %>% 
    na.omit()
  len <- nrow(temp)
  t1 <- xtabs(~explanatory+dependent, data=temp)
  t1_inverse <- xtabs(~dependent+explanatory, data=temp) # for risk ratio: dependent is exposure
  
  m1 <- t1 %>% data.frame() %>% tidyr::spread(dependent, `Freq`)
  m2 <- table(temp$explanatory) %>% as.vector()
  
  t2 <- cbind(m1, Total=m2) %>% 
    mutate_if(is.numeric, proc_total) %>%
    rename("Levels"=explanatory)
  
  #print(t2)
  if (levels(t2$Levels) %>% sort() %>% tolower() == c("no", "yes")) 
    t2 <- t2 %>% filter( tolower(Levels) == "yes" )
  if (levels(t2$Levels) %>% sort() %>% tolower() == c("da", "nu")) 
    t2 <- t2 %>% filter( tolower(Levels) == "da" )
  if (levels(t2$Levels) %>% sort() %>% tolower() == c("non", "oui")) 
    t2 <- t2 %>% filter( tolower(Levels) == "oui" )
  
  
  
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
  t2$p.value[1] <- .p
  t2
}





################## f2: works for categ explanatory

f2 <- function(pair, p.n="pearson", or.n=F, ci.n=F, v=T) {
  dependent = pair$dependent
  explanatory = pair$explanatory
  
  ########## table
  
  temp <- pair$data %>% 
    #select(`dependent` = as.character(dependent), `explanatory` = as.character(explanatory)) %>% 
    na.omit()
  
  len <- nrow(temp)
  t1 <- xtabs(~explanatory+dependent, data=temp)
  
  m1 <- t1 %>% data.frame() %>% tidyr::spread(dependent, `Freq`)
  m2 <- table(temp$explanatory) %>% as.vector()
  
  t2 <- cbind(m1, Total=m2) %>% 
    mutate_if(is.numeric, proc_total) %>%
    rename("Levels"=explanatory)
  
  if (length(levels(t2$Levels))==2) {
    if (levels(t2$Levels) %>% sort() %>% tolower() == c("no", "yes")) 
      t2 <- t2 %>% filter( tolower(Levels) == "yes" )
    if (levels(t2$Levels) %>% sort() %>% tolower() == c("da", "nu")) 
      t2 <- t2 %>% filter( tolower(Levels) == "da" )
    if (levels(t2$Levels) %>% sort() %>% tolower() == c("non", "oui")) 
      t2 <- t2 %>% filter( tolower(Levels) == "oui" )
  }
  
  ######### statistics
  t1.drop.levels <- xtabs(~explanatory+dependent, data=droplevels(na.omit(temp)))
  if (dimnames(t1.drop.levels)$dependent != "DUMMY")
    a2 <- vcd::assocstats(t1.drop.levels) #$ `contingency`, `phi`, `cramer` (V), `chisq_tests`[2,3], `table`
  else a2 <- list(`phi`= NaN, contingency=NaN, cramer=NaN, table=xtabs, 
                  chisq_tests=matrix(rep(NaN, 6), nrow = 2, 
                                     dimnames = list(c("Likelihood Ratio", "Pearson"), list("X^2", "df", "P(> X^2)")) ) )
  #Fisher <- fisher.test(t1) #$ `p.value`, `estimate` (OR), `conf.int` [1:2] # CI the same as DescTools's method = "mle"
  #RR = DescTools::RelRisk(t1, conf.level = 0.95) #$ 1=`rel. risk`, 2=`lwr.ci`, 3=`upr.ci`
  #OR = DescTools::OddsRatio(t1, conf.level = 0.95) #$ `odds ratio`, `lwr.ci`, `upr.ci`
  s2<-"" # result
  
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
  #### else if (p.n == "glm") .p <- summary(pair)$coefficients[2,4]
  #else if (p.n == "aod.glm") .p <- anova(pair, test="Chisq")$`Pr(>Chi)`[2]
  
  
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
  t2$p.value[1] <- .p
  t2
}




################## g: works for numeric explanatory

g <- function(pair, g.rows=c("med_range"), gm_correction = 0.001, paired=F, prefer="auto") {
  dependent = pair$dependent
  explanatory = pair$explanatory
  #print(paste("before", dependent, explanatory))
  
  temp <- pair$data
  temp$explanatory <- as.numeric(temp$explanatory)
  #temp %>% group_by(`dependent`) %>% summarise(N=n()) %>% print()
  
  summary <- temp %>% 
    CreateTotalDependent("dependent") %>%
    na.omit() %>%
    group_by(`dependent`) %>% 
    summarise(N=n(),
              MEAN = mean(`explanatory`),
              SD = sd(`explanatory`),
              MEDIAN = median(`explanatory`),
              GMEAN = DescTools::Gmean(abs(`explanatory`)+gm_correction),
              GSD = DescTools::Gsd(abs(`explanatory`)+gm_correction),
              MIN = min(`explanatory`),
              MAX = max(`explanatory`),
              Q25 = quantile(`explanatory`, 0.25),
              Q75 = quantile(`explanatory`, 0.75),
              # composites
              mean_sd = paste0(format(MEAN, nsmall = 2), " ±", format(SD, nsmall = 1)),
              min_med_max = paste(round(MIN, 2), round(MEDIAN, 2), round(MAX,2), sep=":"),
              med_iqr = paste0(round(MEDIAN,2), " (", round(Q25,2), "-", round(Q75,2), ")"),
              med_range = paste0(round(MEDIAN,2), " (", round(MIN,2), ":", round(MAX,2), ")"),
              geo_mean = paste0(format(GMEAN, nsmall = 2), " ±", format(GSD, nsmall = 1))
              ) 
    #print(summary)
  #print(paste("after", dependent, explanatory))
  
  levels = c(levels(na.omit(temp$`dependent`)), "Total")
  possible.levls <- levels(temp$`dependent`) # empty levels included
  #actual.levels <- levels(na.omit(temp$`dependent`)) # at least 1 obs / level
  actual.levels <- summary$`dependent`[summary$`dependent` != "Total" & summary$`N` >=1 ] %>% as.character() # at least 1 obs / level
  usable.levels <- summary$`dependent`[summary$`dependent` != "Total" & summary$`N` >=2 ] %>% as.character() # at least 2 obs / level
  
  #print(dependent);  print(possible.levls);  print(actual.levels);  print(usable.levels);  print("---------")

  p <- NA
  test <- NA
  
  if (prefer == "auto") {
    if (length(na.omit(temp$explanatory)) < 3 | length(na.omit(temp$explanatory)) > 4999)
      SW_test = 0
    else SW_test <- shapiro.test(temp$`explanatory`)$p.value
    
    if (length(actual.levels) == 2) {
      #temp1 <- temp[temp$`dependent` %in% actual.levels]
      temp1 <- temp %>% filter(`dependent` %in% actual.levels); #print(str(temp1))
      MW_test    <- with(temp1, wilcox.test(`explanatory` ~ `dependent`))$p.value
      p    <- MW_test %>% scales::pvalue(add_p = T)
      test <- "Mann-Whitney"
      
      if(length(usable.levels) == 2) { # F, T, MW
        temp2 <- temp %>% filter(`dependent` %in% usable.levels)
        
        if(paired==T) {
          wilcox_test_paired <- with(temp2, wilcox.test(`explanatory`[`dependent`==usable.levels[1]], `explanatory`[`dependent`==usable.levels[2]], paired = T ))$p.value
          T_test_paired <- with(temp2, t.test(`explanatory`[`dependent`==usable.levels[1]], `explanatory`[`dependent`==usable.levels[2]], paired = T ))$p.value
          p    <- ifelse(SW_test<0.1, wilcox_test_paired, T_test_paired) %>% scales::pvalue(add_p = T)
          test <- ifelse(SW_test<0.1, "Wilcoxon paired test", "Paired T-test")
        } else {
          F_test     <- with(temp2, var.test(`explanatory` ~ `dependent`))$p.value
          Welch_test <- with(temp2, t.test(`explanatory` ~ `dependent`, var.equal=F))$p.value
          T_test     <- with(temp2, t.test(`explanatory` ~ `dependent`, var.equal=T))$p.value
          
          p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test)) %>% scales::pvalue(add_p = T)
          test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch", "T-test"))
        }
        
      }
    } else 
      if (length(actual.levels) > 2) {
        temp1 <- temp %>% filter(`dependent` %in% actual.levels)
        KW_test    <- with(temp1, kruskal.test(`explanatory` ~ `dependent`))$p.value
        aov.test <- with(temp1, summary(aov(`explanatory` ~ `dependent`)))[[1]]$`Pr(>F)`[1]
        # .cor <- cor.test(temp1$`explanatory`, as.numeric(test1$`dependent`),method = "spearman")
        # S_cor <- scales::number(.cor$estimate, 0.001)
        # S_cor_p <- scales::pvalue(.cor$p.value, add_p=T)
        
        p    <- ifelse(SW_test<0.1, KW_test, aov.test) %>% scales::pvalue(add_p = T)
        test <- ifelse(SW_test<0.1, "Kruskal-Wallis", "1-way ANOVA")
        
        if(length(usable.levels) == length(actual.levels)) { 
          temp2 <- temp %>% filter(`dependent` %in% usable.levels)
          F_test     <- with(temp2, bartlett.test(`explanatory` ~ `dependent`))$p.value
          p    <- ifelse(SW_test<0.1, KW_test, ifelse(F_test<0.05, KW_test, aov.test)) %>% scales::pvalue(add_p = T)
          test <- ifelse(SW_test<0.1, "Kruskal-Wallis", ifelse(F_test<0.05, "Kruskal-Wallis", "1-way ANOVA"))
        }
      }
  } # prefer == "auto"
  else if (prefer == "parametric") {
    if (length(usable.levels) == 2) {
      temp2 <- temp %>% filter(`dependent` %in% usable.levels)
      if(paired==T) {
        T_test_paired <- with(temp2, t.test(`explanatory`[`dependent`==usable.levels[1]], `explanatory`[`dependent`==usable.levels[2]], paired = T ))$p.value
        p    <- T_test_paired %>% scales::pvalue(add_p = T)
        test <- "Paired T-test"
      } else {
        F_test     <- with(temp2, var.test(`explanatory` ~ `dependent`))$p.value
        Welch_test <- with(temp2, t.test(`explanatory` ~ `dependent`, var.equal=F))$p.value
        T_test     <- with(temp2, t.test(`explanatory` ~ `dependent`, var.equal=T))$p.value
        
        p    <- ifelse(F_test<0.05, Welch_test, T_test) %>% scales::pvalue(add_p = T)
        test <- ifelse(F_test<0.05, "Welch", "T-test")
      }
    } else 
      if (length(usable.levels) > 2) {
        temp1 <- temp %>% filter(`dependent` %in% usable.levels)
        aov.test <- with(temp1, summary(aov(`explanatory` ~ `dependent`)))[[1]]$`Pr(>F)`[1]
        
        p    <- aov.test %>% scales::pvalue(add_p = T)
        test <- "1-way ANOVA"
      }
  } # prefer == "parametric"
  else if (prefer == "nonparametric") {
    if (length(actual.levels) == 2) {
      #temp1 <- temp[temp$`dependent` %in% actual.levels]
      temp1 <- temp %>% filter(`dependent` %in% actual.levels); #print(str(temp1))
      MW_test    <- with(temp1, wilcox.test(`explanatory` ~ `dependent`))$p.value
      p    <- MW_test %>% scales::pvalue(add_p = T)
      test <- "Mann-Whitney"
    } else 
      if (length(actual.levels) > 2) {
        temp1 <- temp %>% filter(`dependent` %in% actual.levels)
        KW_test    <- with(temp1, kruskal.test(`explanatory` ~ `dependent`))$p.value
        
        p    <- KW_test %>% scales::pvalue(add_p = T)
        test <- "Kruskal-Wallis"
        
      }
  } # prefer == "nonparametric"
  
  t2 = data.frame(t(rep(NA, length(possible.levls)+3)))  %>% 
    set_colnames(c("Levels", possible.levls, "Total", "Statistics"))
  
  for (template in g.rows) {
    x <- switch(
      template,
      "med_range" = c(Levels = "Med (range)", pick(s_df=summary, dep=c(possible.levls, "Total"), val="med_range")),
      "med_iqr" = c(Levels = "Med (IQR)", pick(s_df=summary, dep=c(possible.levls, "Total"), val="med_iqr")),
      "min_med_max" = c(Levels = "Min:Med:Max", pick(s_df=summary, dep=c(possible.levls, "Total"), val="min_med_max")),
      "mean_sd" = c(Levels = "Mean ±SD", pick(s_df=summary, dep=c(possible.levls, "Total"), val="mean_sd")),
      "geo_mean" = c(Levels = "GMean ±SD", pick(s_df=summary, dep=c(possible.levls, "Total"), val="geo_mean"))
    )
    x <- tbl_df(t(c(x, Statistics=NA))) # data.frame messes up the colnames
    
    t2 %<>% bind_rows(x)
  }
  
  t2 %<>% mutate_all(as.character) 
  
  t2 <- t2[-1, ]
  t2$Statistics[1] <- ifelse(is.na(p), "", paste0(test, ": ", p))
  t2$p.value[1] <- p
  
  return(t2)
}
















##################### make use of it

make_summary_table <- function(db, dep=NULL, phi=F, rr=F, rr_inverse=F, or=T, g.rows=c("med_range", "mean_sd"), 
                               paired=F, prefer="auto",
                               p.2 = "fisher", p.n="pearson",
                               metadata=NULL, font.face=NULL,
                               footnote = list(lang="ro", values = c("med_range", "mean_sd", "MW", "Welch", "OR/RR", "V", "KW")), # "OR", "RR"
                               header=list(lang="ro", columns = c("Factor", "Levels", "Total", "Statistics"),
                                           labels = list(`Factor` = "Variabila", `Levels` = "Detalii", `Total` = "Total", `Statistics` = "Teste statistice")),
                               # mean_sd_string = "μ ±DS", 
                               mean_sd_string = ifelse(footnote$lang=="en", "μ ±SD", "μ ±DS"), 
                               geo_mean_string = ifelse(footnote$lang=="en", "Gμ ±SD", "Gμ ±DS"),
                               med_range_string = "M (min:max)", med_iqr_string = "M (IQR)") {
  if (is.null(font.face)) {
    if (exists("MAIN.FONT")) { font.face = MAIN.FONT
    } else {font.face = "Arial"} }
  
  
  if(is.null(header$lang)) header$lang = "ro"
  if(is.null(header$columns)) header$columns = c("Factor", "Levels", "Total", "Statistics")
  if(is.null(header$labels)) {
    if (header$lang == "ro")
      header$labels  <- list(`Factor` = "Variabila", `Levels` = "Detalii", `Total` = "Total", `Statistics` = "Teste statistice")
    else if (header$lang == "fr")
      header$labels  <- list(`Factor` = "Variable", `Levels` = "...", `Total` = "Total", `Statistics` = "Statistique")
    else # if (header$lang == "en")
      header$labels  <- list(`Factor` = "Variable", `Levels` = "Details", `Total` = "Total", `Statistics` = "Statistics")
  }
  
  if(is.null(dep)) {
    db %<>% mutate(DUMMY = factor(rep("DUMMY", nrow(db)), levels=c("DUMMY")))
    dep = "DUMMY"
  }
  #db <- baza_de_date  #%>% select(-c("ID"))
  #dep <- "Diagnostic"
  exp <- names(db)[names(db) != dep] #%>% str_replace_all("_"," - ")
  
  # calculate summary table
  
  pair.list <- pairlist(db, dep, exp)
  df <- lapply(pair.list, FUN=pair.process, p.n=p.n, p=p.2,
               phi=phi, rr=rr, rr_inverse=rr_inverse, or=or, g.rows=g.rows, paired=paired, prefer=prefer ) %>% # , "mean_sd")) %>% 
    bind_rows(.id = "Factor") %>% 
    mutate(`Factor` = factor(`Factor`, levels=exp)) 
  df$Factor <- as.character(df$Factor)
  #df[3] %<>% mutate_all(str_remove_all, pattern="\\(NaN%\\)")
  
  # retreive p values
  p.values <- df$p.value %>% str_remove_all("(\\(?p(=|<))|\\)") %>% as.numeric()
  df %<>% select(-`p.value`)
  
  # add total row
  t <- table(db[[dep]]) %>% as.numeric()
  r1 <- rep("", length(df)) %>% set_names(names(df))
  r1[1] <- dep
  r1[levels(db[[dep]])] <- paste0(t, " (", scales::number(100*t/(sum(t)), 0.1), "%)")
  r1$Total <- sum(t)
  r1 <- data.frame(r1) %>% set_colnames(names(df)) %>% set_rownames(NULL)
  df <- rbind(r1, df)
  df %<>% mutate_if(is.factor, as.character)
  
  # prepare for mearginf cells, clean-up long words
  #df %<>% tidyr::fill(`Statistics`, .direction = "down")
  df %<>% mutate(`Levels` = ifelse(is.na(Factor), Factor, Levels))
  df[is.na(df)] <- ""
  df$Statistics %<>% str_replace_all("Mann-Whitney: ", "MW: ")
  df$Statistics %<>% str_replace_all("Kruskal-Wallis: ", "KW: ")
  df$Statistics %<>% str_replace_all("RR\\*", "RR")
  df$Levels %<>% str_replace_all("Med \\(range\\)", med_range_string) #%>%  str_replace_all("da", "")
  df$Levels %<>% str_replace_all("Med \\(IQR\\)", med_iqr_string) #%>%  str_replace_all("da", "")
  df$Levels %<>% str_replace_all("Mean ±SD", mean_sd_string) 
  df$Levels %<>% str_replace_all("GMean ±SD", geo_mean_string) 
  #df %<>% mutate(`Levels` = ifelse(Levels %in% c("da", "yes","oui"), Factor, Levels)) # manually, later
  #df$Factor <- na_duplicates(df$Factor)
  #df$Statistics[is.na(df$Factor)] <- NA
  
  if (!is.null(metadata))
    df$Factor <- metadata.var_to_labels(df$Factor, metadata)
  
  df %<>% mutate_all(str_remove_all, pattern=" \\(NaN%\\)")
  df %<>% mutate_all(str_remove_all, pattern=" \\(0\\.0%\\)")
  df %<>% mutate_all(str_replace_all, pattern= "100\\.0%", "100%")
  
  
  # borders
  tops <- na_duplicates(df$Factor)
  tops <- which(!is.na(tops))[-c(1, 2)]-1
  
  # vertical merge: rows with multiple sub-rows
  R <- rle(df$Factor)
  X <- cbind(c(1, 2, tops+1), cumsum(R$lengths))
  X <- X[which(R$lengths>1),]
  
  # ! has to be a list of lists of arrays, to avoid simplifications
  if(length(X)>2) 
    X <- apply(X, 1, function(x) {list(seq(x[1], x[2]))})
  else if (length(X) == 2)
    X <- list(list(seq(X[1], X[2])))
  else 
    X <- list(NA)
  
  # horizontal merge: first 2 columns if second column is da/yes/oui
  singe_row <- R$values[R$lengths==1] 
  Y <- c()
  for (i in 1:nrow(df)){
    if(df$Levels[i] %in% c("da", "yes","oui") & df$Factor[i] %in% singe_row )
      Y <- c(Y, i)
  }

  if(dep=="DUMMY") {
    header$columns=header$columns[header$columns %!in% c("Statistics")]
    df$Factor[1] = "N="
  }
  
  col_keys = intersect(names(df), c(levels(db[[dep]]), header$columns))
  
  header$labels = header$labels[intersect(names(header$labels), col_keys)]
  
  if(dep=="DUMMY") {
    col_keys = col_keys[col_keys != "DUMMY"]
    header$labels = header$labels[header$labels  != "DUMMY"]
  }
  
  # make a nice table
  my_table <- df %>% flextable::flextable(col_keys = col_keys) %>%
    flextable::theme_booktabs(10)  %>%
    flextable::set_header_labels(values = header$labels) %>%
    flextable::bold(part="header")  %>%
    flextable::align(align="left", part = "all") %>%
    flextable::autofit(add_w = 0, add_h = 0) %>%
    #flextable::height_all(0.05) %>%
    flextable::padding(padding = 0) %>%
    flextable::bold(part="body", i = 1)  %>%
    flextable::bold(part="body", i = which(p.values<0.05)+1)  %>%
    flextable::hline(i = 1, border=officer::fp_border(color = "black", style = "solid", width = 1))  %>%
    flextable::hline(i = tops,
                     border=officer::fp_border(color = "black", style = "dashed", width = 1)) 
  
  # vertical merge
  for (i in X) { 
    if(!is.na(i)) {
      #print(i)
      if ("Factor" %in% header$columns) 
        my_table <- flextable::merge_at(my_table, i=i[[1]], j=1)
      
      if ("Statistics" %in% header$columns) 
        my_table <- flextable::merge_at(my_table, i=i[[1]], j=length(col_keys) )
    }
  }
  
  
  # horizontal merge
  if(!is.null(Y) & "Levels" %in% header$columns)
    my_table <- flextable::merge_h_range(my_table, i=Y, j1=1, j2=2)#=c(1, 2))
  
  # footnote
  if (!is.na(footnote)) {
    if(is.null(footnote$lang)) footnote$lang = "ro"
    if(is.null(footnote$values)) footnote$values = c("med_range", "mean_sd", "MW", "Welch", "OR/RR", "V")
    if(dep=="DUMMY") footnote$values = footnote$values[footnote$values %!in% c("MW", "Welch", "OR/RR", "V")]
    if (footnote$lang=="en") {
      values=paste0(ifelse("mean_sd" %in% footnote$values, paste0(mean_sd_string, " = Mean (standard deviation); "), ""), #é
                    ifelse("geo_mean" %in% footnote$values, paste0(geo_mean_string, " = Geometric mean (geometric standard deviation); "), ""), 
                    ifelse("med_range" %in% footnote$values, paste0(med_range_string, " = Median (min:max); "), ""), 
                    ifelse("med_iqr" %in% footnote$values, paste0(med_range_string, " = Median (interquartile interval); "), ""), 
                    ifelse("MW" %in% footnote$values, "MW = Mann-Whitney Test; ", ""), 
                    ifelse("Welch" %in% footnote$values, "Welch = Welch T-Test (not assuming equal variances); ", ""), 
                    ifelse("OR/RR" %in% footnote$values, "OR/RR = odds-ratio / risk-ratio [95% CI] and p value from Fisher test); ", ""),
                    ifelse("OR" %in% footnote$values, "OR = odds-ratio [95% CI] and p value from Fisher test); ", ""), 
                    ifelse("RR" %in% footnote$values, "RR = risk-ratio [95% CI] and p value from Fisher test); ", ""), 
                    ifelse("V" %in% footnote$values, "V = Cramér V (p value from Chi² test); ", ""), 
                    ifelse("custom" %in% footnote$values, paste(footnote$custom, collapse = "; "), ""))
    } else if (footnote$lang=="fr") {
      values=paste0(ifelse("mean_sd" %in% footnote$values, paste0(mean_sd_string, " = Moyenne (écart type); "), ""), #é
                    ifelse("geo_mean" %in% footnote$values, paste0(geo_mean_string, " = Moyenne geometrique (écart type geometrique); "), ""), 
                    ifelse("med_range" %in% footnote$values, paste0(med_range_string, " = Médiane (min:max); "), ""), 
                    ifelse("med_iqr" %in% footnote$values, paste0(med_range_string, " = Médiane (intervalle interquartile); "), ""), 
                    ifelse("MW" %in% footnote$values, "MW = Test de Mann-Whitney; ", ""), 
                    ifelse("KW" %in% footnote$values, "KW = Test de Kruskal-Wallis; ", ""), 
                    ifelse("Welch" %in% footnote$values, "Welch = Test T de Welch (variances inégales); ", ""), 
                    ifelse("OR/RR" %in% footnote$values, "OR/RR = odds-ratio / risk-ratio [95% CI] et test Fisher pour la valeur p); ", ""), 
                    ifelse("OR" %in% footnote$values, "OR = odds-ratio [95% CI] et test Fisher pour la valeur p); ", ""), 
                    ifelse("RR" %in% footnote$values, "RR = risk-ratio [95% CI] et test Fisher pour la valeur p); ", ""),
                    ifelse("V" %in% footnote$values, "V = Cramér V (est Chi² test pour la valeur p); ", ""), 
                    ifelse("custom" %in% footnote$values, paste(footnote$custom, collapse = "; "), ""))
    } else { # ro
      values=paste0(ifelse("mean_sd" %in% footnote$values, paste0(mean_sd_string, " = Media (deviația standard); "), ""), 
                    ifelse("geo_mean" %in% footnote$values, paste0(geo_mean_string, " = Media geometrcă (deviația standard geometrcă); "), ""), 
                    ifelse("med_range" %in% footnote$values, paste0(med_range_string, " = Mediana (min:max); "), ""), 
                    ifelse("med_iqr" %in% footnote$values, paste0(med_range_string, " = Mediana (interval interquartilic); "), ""), 
                    ifelse("MW" %in% footnote$values, "MW = Test Mann-Whitney; ", ""), 
                    ifelse("KW" %in% footnote$values, "KW = Test Kruskal-Wallis; ", ""), 
                    ifelse("Welch" %in% footnote$values, "Welch = Test T Welch (fără presupunerea de varianțe egale); ", ""), 
                    ifelse("OR/RR" %in% footnote$values, "OR/RR = odds-ratio / risc relativ [cu IC 95%] și p calculat prin testul Fisher); ", ""),
                    ifelse("OR" %in% footnote$values, "OR = odds-ratio [cu IC 95%] și p calculat prin testul Fisher); ", ""),
                    ifelse("RR" %in% footnote$values, "RR = risc relativ [cu IC 95%] și p calculat prin testul Fisher); ", ""), 
                    ifelse("V" %in% footnote$values, "V = Cramér V (p calculat prin testul Chi²); ", ""), 
                    ifelse("custom" %in% footnote$values, paste(footnote$custom, collapse = "; "), ""))
    }
    
    if (values != "")
      my_table %<>%
        flextable::add_footer_lines(values=values) %>%  
        flextable::border(part="footer", border.bottom = officer::fp_border(color = "black", style = "solid", width = 1)) %>%
        flextable::italic(part = "footer")
  }
  
  
  my_table %<>% flextable::fix_border_issues() %>%
    flextable::height_all(0.2) %>% flextable::font(fontname = font.face, part="all")
  
  attr(my_table, "df") <- df
  attr(my_table, "tops") <- tops
  attr(my_table, "X") <- X
  attr(my_table, "Y") <- Y
  my_table
}
