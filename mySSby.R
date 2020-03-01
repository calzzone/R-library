mySSby0 <- function(db, numerica, categ, gm_correction=0.001, na.rm=T, test="auto") {
  
  numerica <- search_for_variable(db, numerica, pattern = "", type = "numeric")
  if (is.na(numerica)) stop("provide a valid numeric variable name or number")
  
  categ <- search_for_variable(db, categ, pattern = "", type = "factor")
  
  if(is.na(categ)) { # no grouping
    db <- db %>% select(numerica = numerica)
    
    grp <- db %>% 
      #select(numerica = numerica, categ=categ) %>%
      #group_by(Grup=`categ`) %>%
      summarise( Grup="(all)",
                 n=length(na.omit(`numerica`)),
                 `%` = 1,
                 mean = mean(`numerica`, na.rm=T),
                 median = median(`numerica`, na.rm=T),
                 min = min(`numerica`, na.rm=T), 
                 max = max(`numerica`, na.rm=T), 
                 sd = sd(`numerica`, na.rm=T), 
                 GM = DescTools::Gmean(abs(`numerica`)+gm_correction, na.rm=T), 
                 GSD = DescTools::Gsd(abs(`numerica`)+gm_correction, na.rm=T) ) 
    
    if(length(db$`numerica`)<1000)
      attr(grp, "test") <- shapiro.test(db$`numerica`)
    else attr(grp, "test") <- list(p.value=0, method="Shapiro-Wilk normality test")
    #return(grp)
  } else { # with grouping
    db <- db %>% select(numerica = numerica, categ=categ)
    
    if (na.rm) db <- db %>% filter(!is.na(`categ`))
    
    grp <- db %>% 
      #select(numerica = numerica, categ=categ) %>%
      group_by(Grup=`categ`) %>%
      summarise(n=length(na.omit(`numerica`)),
                `%` = sum(n),
                mean = mean(`numerica`, na.rm=T),
                median = median(`numerica`, na.rm=T),
                min = min(`numerica`, na.rm=T), 
                max = max(`numerica`, na.rm=T), 
                sd = sd(`numerica`, na.rm=T), 
                GM = DescTools::Gmean(abs(`numerica`)+gm_correction, na.rm=T), 
                GSD = DescTools::Gsd(abs(`numerica`)+gm_correction, na.rm=T) ) %>%
      mutate(`%` = n/sum(n)) #%>%
    #arrange(mean)
    
    if(between(length(na.omit(db$`numerica`)), 3, 1000))
      shapiro <- shapiro.test(db$`numerica`)
    else shapiro <- list(p.value=0, method="Shapiro-Wilk normality test")
    
    if(length(levels(db$categ))==2) {
      if(test=="wilcox") {
        attr(grp, "test") <- wilcox.test(`numerica`~`categ`, data=db)
      } else if (test=="t.auto") { 
        attr(grp, "test") <- t.test(`numerica`~`categ`, data=db)
      } else if (test=="t.var.eq") { 
        attr(grp, "test") <- t.test(`numerica`~`categ`, data=db, var.equal=T)
      } else if (test=="t.welch") { 
        attr(grp, "test") <- t.test(`numerica`~`categ`, data=db, var.equal=F)
      } else if (test=="surv") { 
        test <- survival::survdiff(Surv(time = `numerica`) ~ `categ`, data = db)
        p <- pchisq(test$chisq, length(test$n)-1, lower.tail = FALSE)
        attr(grp, "test") <- list(method="Log-rank test", p.value = p, test = test)
      } else { # auto test
        if (shapiro$p.value < 0.05) 
          attr(grp, "test") <- wilcox.test(`numerica`~`categ`, data=db)
        else {
          t_test <- try(t.test(`numerica`~`categ`, data=db), silent=T)
          if (class(t_test) == "try-error") t_test = list(method="T test ?", p.value = NaN)
          attr(grp, "test") <- t_test
        }
      }
      
    }
    else if(length(levels(db$categ))>2)
    { try(
      if(test=="kruskal") {
        attr(grp, "test") <- kruskal.test(`numerica`~`categ`, data=db)
      } else if (test=="anova") { 
        attr(grp, "test") <- list(method="ANOVA", p.value=summary(aov(`numerica`~`categ`, data=db))[[1]]$'Pr(>F)'[1])
      } else if (test=="surv") { 
        test <- survival::survdiff(Surv(time = `numerica`) ~ `categ`, data = db)
        p <- pchisq(test$chisq, length(test$n)-1, lower.tail = FALSE)
        attr(grp, "test") <- list(method="Log-rank test", p.value = p, test = test)
      } else { # auto test
        if (shapiro$p.value < 0.05) 
          attr(grp, "test") <- kruskal.test(`numerica`~`categ`, data=db)
        else 
          attr(grp, "test") <- list(method="ANOVA", p.value=summary(aov(`numerica`~`categ`, data=db))[[1]]$'Pr(>F)'[1])
      })
      
      if(is.null(attr(grp, "test"))) attr(grp, "test") <- list(test="", p.value=NaN)
    
    }
    else 
      attr(grp, "test") <- shapiro
  }
  
  attr(grp, "test_text") <- 
      paste0("(", attr(grp, "test")$method, ": ", attr(grp, "test")$p.value %>% scales::pvalue(add_p=T), ")")
 
  return(grp)
}


# res0 <- mySSby0(baza_de_date, numerica="Varsta", categ=NA, gm_correction=0.001)
# res0
# attr(res0, "test")
# 
# res2 <- mySSby0(baza_de_date, numerica="Varsta", categ="Sex", gm_correction=0.001)
# res2
# attr(res2, "test")
# 
# resn <- mySSby0(baza_de_date, numerica="Varsta", categ="Ocupatie", gm_correction=0.001)
# resn
# attr(resn, "test")