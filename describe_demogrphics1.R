#Varsta pacienților (N=30) a avut valori între 27 și 74 ani (mediana: 50) cu o medie ±DS de 51.17 ±10.93 ani. 
#În cazul bărbaților (N=22, 73.3%), Varsta a avut valori între 39 și 54 ani (mediana: 46) cu o medie de 46.25 ±4.86 ani. 
#În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
#Această diferență a fost semnificativă statistic (p=0.039).

search_for_variable <- function(data, parameter="auto", pattern=NA, type="numeric") {
  var_names <- variable.names(data)
  #data <- data.frame(data)
  #colnames(data) <- var_names
  
  if (is.na(parameter) | parameter == "none" | parameter == 0 | parameter == "") {
    print("no parameter")
    parameter <- NA
  }
  else if (parameter == "auto") {
    print("search for parameter")
    locations <- grep(pattern, var_names, ignore.case = T)
    if (length(locations) < 1) parameter <- NA
    else parameter <- locations[1]
    print(paste("found parameter", parameter))
  }
  else {
    print (paste("parameter was provided:", parameter))
    if (is.numeric(parameter) )
      parameter <- parameter
    else if (parameter %in% var_names)
      parameter <- which(parameter==var_names)
    else parameter <- NA
  }
  
  
  
  if (is.na(parameter)) return(NA)
  if (!is.numeric(parameter)) return(NA)
  if (!between(parameter, 1, length(var_names))) return(NA)
  
  if(type == "numeric") if (is.numeric(data[[parameter]])) return (parameter)
  if(type == "binary") if (is.factor(data[[parameter]]) && 
                           length(levels( data[[parameter]] )) == 2) return (parameter)
  if(type == "factor") if (is.factor(data[[parameter]])) return (parameter)
  
  return(NA)
  #return (parameter)
}

demographics_text <- function(data, age="auto", sex="auto", living="auto", primary=NA, lang="ro") {
  var_names <- variable.names(data)
  strings <- c()
  
  age <- search_for_variable(data, age, pattern = "(varsta*)|(age)", type = "numeric")
  sex <- search_for_variable(data, sex, pattern = "(sex*)|(gender)", type = "binary")
  living <- search_for_variable(data, living, pattern = "(mediul*)|(mediu)|(mediu de)|(place*living)", type = "binary")
  primary <- search_for_variable(data, primary, pattern = NA, type = "binary")
 
  #print (age)
  if (!is.na(age)) {
    
    # Age:
      temp <- na.omit(data[[age]])
      strings <-c(strings, (paste(
        "Varsta pacienților (N=", length(temp), ") a avut valori între ", 
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
        test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu variații diferite)", "T pentru eșantioane cu variații egale"))
        
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
        test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu variații diferite)", "T pentru eșantioane cu variații egale"))
        
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
        test <- ifelse(SW_test<0.1, "Mann-Whitney", ifelse(F_test<0.05, "Welch (T pentru eșantioane cu variații diferite)", "T pentru eșantioane cu variații egale"))
        
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

age="Varsta (ani)"
demographics_text(data = droplevels(baza_de_date), age="auto", sex="auto", living='auto', primary="AUDIT")
