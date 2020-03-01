#Varsta pacienților (N=30) a avut valori între 27 și 74 ani (mediana: 50) cu o medie ±DS de 51.17 ±10.93 ani. 
#În cazul bărbaților (N=22, 73.3%), Varsta a avut valori între 39 și 54 ani (mediana: 46) cu o medie de 46.25 ±4.86 ani. 
#În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
#Această diferență a fost semnificativă statistic (p=0.039).



descriptives_text <- function(data, numerice="all", sex="auto", living="auto", primary=NA, lang="ro") {
  var_names <- variable.names(data)
  strings <- c()
  
  sex <- search_for_variable(data, sex, pattern = "(sex*)|(gender)", type = "binary")
  living <- search_for_variable(data, living, pattern = "(mediul*)|(mediu)|(mediu de)|(place*living)", type = "binary")
  primary <- search_for_variable(data, primary, pattern = NA, type = "binary")
  
  if(numerice == "all")
    numerice <- names(data %>% select_if(is.numeric))
  if (length(numerice) > 0) for (var_numerica in numerice) {
    #print(var_numerica)
  
  var_numerica <- search_for_variable(data, var_numerica, pattern = "", type = "numeric")
  
  
  #print (var_numerica)
  if (!is.na(var_numerica)) {
    
    # var_numerica:
    temp <- na.omit(data[[var_numerica]])
    strings <-c(strings, (paste( 
      var_names[var_numerica], " (N=", length(temp), ") a avut valori între ", 
      min(temp), " și ", max(temp), " (mediana: ", median(temp),
      ") cu o medie de ", format(mean(temp), nsmall = 2), " ±", format(sd(temp), nsmall = 1), ".",
      sep = ""
    )))
    
    # var_numerica ~ Sex 
    if (!is.na(sex)) {
      temp <- na.omit(data.frame(`var_numerica` = data[[var_numerica]], `sex` = data[[sex]]))
      temp_f <- temp$`var_numerica`[temp$`sex` == "F"]
      temp_m <- temp$`var_numerica`[temp$`sex` == "M"]
      t <- length(temp$`sex`)
      f <- table(temp$`sex`)["F"]
      m <- table(temp$`sex`)["M"]
      #În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
      strings <-c(strings, paste(
        "În cazul femeilor (N=", length(temp_f), ", ", scales::percent(f/t, 0.1), ") ", 
        var_names[var_numerica], " a avut valori între ", 
        min(temp_f), " și ", max(temp_f), " (mediana: ", median(temp_f),
        ") cu o medie de ", format(mean(temp_f), nsmall = 2), " ±", format(sd(temp_f), nsmall = 1), ".",
        sep = ""
      ))
      strings <-c(strings, paste(
        "În cazul bărbaților (N=", length(temp_m), ", ", scales::percent(m/t, 0.1), ") ", 
        var_names[var_numerica], " a avut valori între ", 
        min(temp_m), " și ", max(temp_m), " (mediana: ", median(temp_m),
        ") cu o medie de ", format(mean(temp_m), nsmall = 2), " ±", format(sd(temp_m), nsmall = 1), ".",
        sep = ""
      ))
      #Această diferență a fost semnificativă statistic (p=0.039).
      SW_test    <- shapiro.test(temp$`var_numerica`)$p.value
      F_test     <- var.test(temp_f, temp_m)$p.value
      Welch_test <- t.test(temp_f, temp_m, var.equal = F)$p.value
      T_test     <- t.test(temp_f, temp_m, var.equal = T)$p.value
      MW_test    <- wilcox.test(temp_f, temp_m)$p.value
      
      p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test))
      test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu varaiții diferite)", "T pentru eșantioane cu varaiții egale"))
      
      strings <-c(strings, paste(
        "Această diferență de ", format(abs(mean(temp_f) - mean(temp_m)), nsmall = 1), " ", 
        ifelse (p > 0.1, 'nu a fost semnificativă statistic', 
                ifelse(p>0.05, "a prezentat o tendință spre semnificație statistică",
                       'a fost semnificativă statistic')),
        " (", scales::pvalue(p, add_p = T), ") conform testului ", test, ".",
        sep = ""
      ))
      
      
      
    }
    
    # var_numerica ~ Living
    if (!is.na(living)) {
      temp <- na.omit(data.frame(`var_numerica` = data[[var_numerica]], `living` = data[[living]]))
      temp_r <- temp$`var_numerica`[temp$`living` == "rural"]
      temp_u <- temp$`var_numerica`[temp$`living` == "urban"]
      t <- length(temp$`living`)
      r <- table(temp$`living`)["rural"]
      u <- table(temp$`living`)["urban"]
      
      #În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
      strings <-c(strings, paste(
        var_names[var_numerica], " la pacienții din mediul rural (N=", length(temp_r), ", ", scales::percent(r/t, 0.1), ") a avut valori între ", 
        min(temp_r), " și ", max(temp_r), " (mediana: ", median(temp_r),
        ") cu o medie de ", format(mean(temp_r), nsmall = 2), " ±", format(sd(temp_r), nsmall = 1), ".",
        sep = ""
      ))
      strings <-c(strings, paste(
        var_names[var_numerica], " la pacienții din mediul urban (N=", length(temp_u), ", ", scales::percent(u/t, 0.1), ") a avut valori între ", 
        min(temp_u), " și ", max(temp_u), " (mediana: ", median(temp_u),
        ") cu o medie de ", format(mean(temp_u), nsmall = 2), " ±", format(sd(temp_u), nsmall = 1), ".",
        sep = ""
      ))
      #Această diferență a fost semnificativă statistic (p=0.039).
      SW_test    <- shapiro.test(temp$`var_numerica`)$p.value
      F_test     <- var.test(temp_r, temp_u)$p.value
      Welch_test <- t.test(temp_r, temp_u, var.equal = F)$p.value
      T_test     <- t.test(temp_r, temp_u, var.equal = T)$p.value
      MW_test    <- wilcox.test(temp_r, temp_u)$p.value
      
      p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test))
      test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu varaiții diferite)", "T pentru eșantioane cu varaiții egale"))
      
      strings <-c(strings, paste(
        "Această diferență de ", format(abs(mean(temp_r) - mean(temp_u)), nsmall = 1), " ", 
        ifelse (p > 0.1, 'nu a fost semnificativă statistic', 
                ifelse(p>0.05, "a prezentat o tendință spre semnificație statistică",
                       'a fost semnificativă statistic')),
        " (", scales::pvalue(p, add_p = T), ") conform testului ", test, ".",
        sep = ""
      ))
      
      
    }
    
    # var_numerica ~ primary
    if (!is.na(primary)) {
      temp <- na.omit(data.frame(`var_numerica` = data[[var_numerica]], `primary` = data[[primary]]))
      temp_p1 <- temp$`var_numerica`[temp$`primary` == levels(temp$`primary`)[1]]
      temp_p2 <- temp$`var_numerica`[temp$`primary` == levels(temp$`primary`)[2]]
      t <- length(temp$`primary`)
      p1 <- table(temp$`primary`)[levels(temp$`primary`)[1]]
      p2 <- table(temp$`primary`)[levels(temp$`primary`)[2]]
      
      #În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
      strings <-c(strings, paste(
        var_names[var_numerica], " la pacienții cu ", var_names[primary], ": ", levels(temp$`primary`)[1], 
        " (N=", length(temp_p1), ", ", scales::percent(p1/t, 0.1), ") a avut valori între ", 
        min(temp_p1), " și ", max(temp_p1), " (mediana: ", median(temp_p1),
        ") cu o medie de ", format(mean(temp_p1), nsmall = 2), " ±", format(sd(temp_p1), nsmall = 1), ".",
        sep = ""
      ))
      strings <-c(strings, paste(
        var_names[var_numerica], " la pacienții cu ", var_names[primary], ": ", levels(temp$`primary`)[2], 
        " (N=", length(temp_p2), ", ", scales::percent(p2/t, 0.1), ") a avut valori între ", 
        min(temp_p2), " și ", max(temp_p2), " (mediana: ", median(temp_p2),
        ") cu o medie de ", format(mean(temp_p2), nsmall = 2), " ±", format(sd(temp_p2), nsmall = 1), ".",
        sep = ""
      ))
      #Această diferență a fost semnificativă statistic (p=0.039).
      SW_test    <- shapiro.test(temp$`var_numerica`)$p.value
      F_test     <- var.test(temp_p1, temp_p2)$p.value
      Welch_test <- t.test(temp_p1, temp_p2, var.equal = F)$p.value
      T_test     <- t.test(temp_p1, temp_p2, var.equal = T)$p.value
      MW_test    <- wilcox.test(temp_p1, temp_p2)$p.value
      
      p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test))
      test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu varaiții diferite)", "T pentru eșantioane cu varaiții egale"))
      
      strings <-c(strings, paste(
        "Această diferență de ", format(abs(mean(temp_p1) - mean(temp_p2)), nsmall = 1), " ", 
        ifelse (p > 0.1, 'nu a fost semnificativă statistic', 
                ifelse(p>0.05, "a prezentat o tendință spre semnificație statistică",
                       'a fost semnificativă statistic')),
        " (", scales::pvalue(p, add_p = T), ") conform testului ", test, ".",
        sep = ""
      ))
      
    }
    
    
  }
  }
  
  return (strings)
  
}

demographics_text <- function(data, age="auto", sex="auto", living="auto", primary=NA, lang="ro") {
  var_names <- variable.names(data)
  strings <- c()
  
  age <- search_for_variable(data, age, pattern = "(varsta*)|(age)", type = "numeric")
  sex <- search_for_variable(data, sex, pattern = "(sex*)|(gender)", type = "binary")
  living <- search_for_variable(data, living, pattern = "(mediul*)|(mediu)|(mediu de)|(place*living)", type = "binary")
  primary <- search_for_variable(data, primary, pattern = NA, type = "binary")
  
  #print(length(data[[age]]))
  
  #print (age)
  if (!is.na(age)) {
    #print(length(data[[age]]))
    # Age:
    temp <- na.omit(data[[age]])
    strings <-c(strings, (paste(
      "Vârsta pacienților (N=", length(temp), ") a avut valori între ", 
      min(temp), " și ", max(temp), " ani (mediana: ", median(temp),
      ") cu o medie de ", format(mean(temp), nsmall = 2), " ±", format(sd(temp), nsmall = 1), " ani.",
      sep = ""
    )))
    
    # Age ~ Sex 
    if (!is.na(sex)) {
      temp <- na.omit(data.frame(`age` = data[[age]], `sex` = data[[sex]]))
      temp_f <- temp$`age`[temp$`sex` == "F"]
      temp_m <- temp$`age`[temp$`sex` == "M"]
      t <- length(temp$`sex`)
      f <- table(temp$`sex`)["F"]
      m <- table(temp$`sex`)["M"]
      #În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
      strings <-c(strings, paste(
        "În cazul femeilor (N=", length(temp_f), ", ", scales::percent(f/t, 0.1), ") a avut valori între ", 
        min(temp_f), " și ", max(temp_f), " ani (mediana: ", median(temp_f),
        ") cu o medie de ", format(mean(temp_f), nsmall = 2), " ±", format(sd(temp_f), nsmall = 1), " ani.",
        sep = ""
      ))
      strings <-c(strings, paste(
        "În cazul bărbaților (N=", length(temp_m), ", ", scales::percent(m/t, 0.1), ") a avut valori între ", 
        min(temp_m), " și ", max(temp_m), " ani (mediana: ", median(temp_m),
        ") cu o medie de ", format(mean(temp_m), nsmall = 2), " ±", format(sd(temp_m), nsmall = 1), " ani.",
        sep = ""
      ))
      #Această diferență a fost semnificativă statistic (p=0.039).
      SW_test    <- shapiro.test(temp$`age`)$p.value
      F_test     <- var.test(temp_f, temp_m)$p.value
      Welch_test <- t.test(temp_f, temp_m, var.equal = F)$p.value
      T_test     <- t.test(temp_f, temp_m, var.equal = T)$p.value
      MW_test    <- wilcox.test(temp_f, temp_m)$p.value
      
      p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test))
      test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu varaiții diferite)", "T pentru eșantioane cu varaiții egale"))
      
      strings <-c(strings, paste(
        "Această diferență de ", format(abs(mean(temp_f) - mean(temp_m)), nsmall = 1), " ani ", 
        ifelse (p > 0.1, 'nu a fost semnificativă statistic', 
                ifelse(p>0.05, "a prezentat o tendință spre semnificație statistică",
                       'a fost semnificativă statistic')),
        " (", scales::pvalue(p, add_p = T), ") conform testului ", test, ".",
        sep = ""
      ))
      
      
      
    }
    
    # Age ~ Living
    if (!is.na(living)) {
      temp <- na.omit(data.frame(`age` = data[[age]], `living` = data[[living]]))
      temp_r <- temp$`age`[temp$`living` == "rural"]
      temp_u <- temp$`age`[temp$`living` == "urban"]
      t <- length(temp$`living`)
      r <- table(temp$`living`)["rural"]
      u <- table(temp$`living`)["urban"]
      
      #În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
      strings <-c(strings, paste(
        "Vârsta pacienților din mediul rural (N=", length(temp_r), ", ", scales::percent(r/t, 0.1), ") a avut valori între ", 
        min(temp_r), " și ", max(temp_r), " ani (mediana: ", median(temp_r),
        ") cu o medie de ", format(mean(temp_r), nsmall = 2), " ±", format(sd(temp_r), nsmall = 1), " ani.",
        sep = ""
      ))
      strings <-c(strings, paste(
        "Vârsta pacienților din mediul urban (N=", length(temp_u), ", ", scales::percent(u/t, 0.1), ") a avut valori între ", 
        min(temp_u), " și ", max(temp_u), " ani (mediana: ", median(temp_u),
        ") cu o medie de ", format(mean(temp_u), nsmall = 2), " ±", format(sd(temp_u), nsmall = 1), " ani.",
        sep = ""
      ))
      #Această diferență a fost semnificativă statistic (p=0.039).
      SW_test    <- shapiro.test(temp$`age`)$p.value
      F_test     <- var.test(temp_r, temp_u)$p.value
      Welch_test <- t.test(temp_r, temp_u, var.equal = F)$p.value
      T_test     <- t.test(temp_r, temp_u, var.equal = T)$p.value
      MW_test    <- wilcox.test(temp_r, temp_u)$p.value
      
      p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test))
      test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu varaiții diferite)", "T pentru eșantioane cu varaiții egale"))
      
      strings <-c(strings, paste(
        "Această diferență de ", format(abs(mean(temp_r) - mean(temp_u)), nsmall = 1), " ani ", 
        ifelse (p > 0.1, 'nu a fost semnificativă statistic', 
                ifelse(p>0.05, "a prezentat o tendință spre semnificație statistică",
                       'a fost semnificativă statistic')),
        " (", scales::pvalue(p, add_p = T), ") conform testului ", test, ".",
        sep = ""
      ))
      
      
    }
    
    # Age ~ primary
    if (!is.na(primary)) {
      temp <- na.omit(data.frame(`age` = data[[age]], `primary` = data[[primary]]))
      temp_p1 <- temp$`age`[temp$`primary` == levels(temp$`primary`)[1]]
      temp_p2 <- temp$`age`[temp$`primary` == levels(temp$`primary`)[2]]
      t <- length(temp$`primary`)
      p1 <- table(temp$`primary`)[levels(temp$`primary`)[1]]
      p2 <- table(temp$`primary`)[levels(temp$`primary`)[2]]
      
      #În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
      strings <-c(strings, paste(
        "Vârsta pacienților cu ", var_names[primary], ": ", levels(temp$`primary`)[1], 
        " (N=", length(temp_p1), ", ", scales::percent(p1/t, 0.1), ") a avut valori între ", 
        min(temp_p1), " și ", max(temp_p1), " ani (mediana: ", median(temp_p1),
        ") cu o medie de ", format(mean(temp_p1), nsmall = 2), " ±", format(sd(temp_p1), nsmall = 1), " ani.",
        sep = ""
      ))
      strings <-c(strings, paste(
        "Vârsta pacienților cu ", var_names[primary], ": ", levels(temp$`primary`)[2], 
        " (N=", length(temp_p2), ", ", scales::percent(p2/t, 0.1), ") a avut valori între ", 
        min(temp_p2), " și ", max(temp_p2), " ani (mediana: ", median(temp_p2),
        ") cu o medie de ", format(mean(temp_p2), nsmall = 2), " ±", format(sd(temp_p2), nsmall = 1), " ani.",
        sep = ""
      ))
      #Această diferență a fost semnificativă statistic (p=0.039).
      SW_test    <- shapiro.test(temp$`age`)$p.value
      F_test     <- var.test(temp_p1, temp_p2)$p.value
      Welch_test <- t.test(temp_p1, temp_p2, var.equal = F)$p.value
      T_test     <- t.test(temp_p1, temp_p2, var.equal = T)$p.value
      MW_test    <- wilcox.test(temp_p1, temp_p2)$p.value
      
      p    <- ifelse(SW_test<0.1, MW_test, ifelse(F_test<0.05, Welch_test, T_test))
      test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu varaiții diferite)", "T pentru eșantioane cu varaiții egale"))
      
      strings <-c(strings, paste(
        "Această diferență de ", format(abs(mean(temp_p1) - mean(temp_p2)), nsmall = 1), " ani ", 
        ifelse (p > 0.1, 'nu a fost semnificativă statistic', 
                ifelse(p>0.05, "a prezentat o tendință spre semnificație statistică",
                       'a fost semnificativă statistic')),
        " (", scales::pvalue(p, add_p = T), ") conform testului ", test, ".",
        sep = ""
      ))
      
    }
    
    
  }
  
  return (strings)
  
}




#demographics_text(data = droplevels(baza_de_date), age="auto", sex="auto", living='auto')#, primary="AUDIT")

#var_numerica="Varsta (ani)"
#descriptives_text(data = droplevels(baza_de_date), numerice="all", sex="auto", living='auto', primary="AUDIT")
