#Varsta pacienților (N=30) a avut valori între 27 și 74 ani (mediana: 50) cu o medie ±DS de 51.17 ±10.93 ani. 
#În cazul bărbaților (N=22, 73.3%), Varsta a avut valori între 39 și 54 ani (mediana: 46) cu o medie de 46.25 ±4.86 ani. 
#În cazul femeilor (N=8, 26.7%), Varsta a avut valori între 27 și 74 ani (mediana: 52) cu o medie de 52.95 ±12.02 ani. 
#Această diferență a fost semnificativă statistic (p=0.039).

describe2x2 <- function(data, exposure=NA, outcome=NA, lang="ro") {
  var_names <- variable.names(data)
  strings <- c()
  
  exposure <- search_for_variable(data, exposure, pattern = NA, type = "binary")
  outcome <- search_for_variable(data, outcome, pattern = NA, type = "binary")
  if(is.na(exposure) || is.na(outcome)) return (NA)
  
  temp <- na.omit(data.frame(`exposure` = data[[exposure]], `outcome` = data[[outcome]]))
  t1 <- xtabs(~exposure+outcome, data=temp)
  t2 <- temp %>% 
    group_by(`exposure`, `outcome`) %>%
    #group_by(`Sex`, `BIS-profil (binar)`) %>%
    summarise(`Freq` = n()) %>%
    mutate(
      `Relaive Freq` = Freq/sum(Freq),
      `Freq%` = scales::percent(`Relaive Freq`),
      `sum_exposure` = sum(`Freq`),
      `Label` = paste(`Freq`, " (", `Freq%`, ")", sep="")
      )
  #print (t2)
  
  strings <- c(strings, paste(
    "Dintre cei ", t2$sum_exposure[1], " pacienți cu ", var_names[exposure], ": ", levels(t2$`exposure`)[1], 
    " (", scales::percent(t2$sum_exposure[1]/sum(t2$Freq)), "), ",
    t2$`Label`[1], " au avut ", var_names[outcome], ": ", levels(t2$`outcome`)[1], 
    " față de ", t2$`Label`[3], " dintre cei ", t2$sum_exposure[3], " pacienți cu ", var_names[exposure], ": ", levels(t2$`exposure`)[2], ".",
    sep = ""
  ))
  
  
  #a1 <- DescTools::Assocs(t1)
  a2 <- vcd::assocstats(t1)
  RR = DescTools::RelRisk(t1, conf.level = 0.95, method = "wald")
  phi  = a2$phi
  
  #a3 <- c(
  #  chisq_LR = a2$chisq_tests[1,3],
  #  chisq_pearson = stats::chisq.test(t1, correct = F)$p.value, 
  #  chisq_yates = stats::chisq.test(t1, correct = T)$p.value, 
  #  fisher = stats::fisher.test(t1)$p.value
  #)
  
  strings <- c(strings, paste(
    "Rezultă că pacienții cu ", var_names[exposure], ": ", levels(t2$`exposure`)[1], " au un risc relativ de ", 
    ifelse(RR[1]>1, 
           paste(
             scales::number(RR[1], accuracy = 0.1), " [IC95% = ", 
             scales::number(RR[2], accuracy = 0.1), " la ", 
             scales::number(RR[3], accuracy = 0.1), " ] ori mai mare ", 
             sep = ""), 
           paste(
             scales::number(1/RR[1], accuracy = 0.001), " [IC95% = ", 
             scales::number(1/RR[3], accuracy = 0.1), " la ", 
             scales::number(1/RR[2], accuracy = 0.1), " ] ori mai mic ", 
             sep = "")),
    "de a avea ", var_names[outcome], ": ", levels(t2$`outcome`)[1], " decât cei cu ",
    var_names[exposure], ": ", levels(t2$`exposure`)[2], " (", 
    "phi=", scales::number(phi, accuracy = 0.01), ", ", scales::pvalue(a2$chisq_tests[2,3], add_p = T), 
    ifelse(a2$chisq_tests[2,3]<0.05, " semnificativ ", " nesemnificativ "), "statistic conform testului Chi²).",
    sep = ""
  ))
  
  return (strings)
  
}

#describe2x2(data=baza_de_date, exposure="Sex", outcome="BIS-profil (binar)")

#demographics_text(data = droplevels(baza_de_date), age="auto", sex="auto", living='auto')#, primary="AUDIT")

#var_numerica="Varsta (ani)"
#descriptives_text(data = droplevels(baza_de_date), numerice="all", sex="auto", living='auto', primary="AUDIT")
