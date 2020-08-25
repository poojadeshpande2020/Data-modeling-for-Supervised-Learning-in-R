
#Q3  
options(scipen=999)
p_value <- pt(5.3265,70,lower.tail = FALSE)*2
p_value
critical_t_value <- qt(0.025,70,lower.tail = FALSE)
critical_t_value

#Q7
critical_f_value <- qf(0.05,4,67,lower.tail = FALSE)
critical_f_value
p_value <- pf(56.5,4,67,lower.tail = FALSE)
round(p_value,4)

#Q10
critical_f_value <- qf(0.05,3,65,lower.tail = FALSE)
critical_f_value
p_value <- pf(2.1852,3,65,lower.tail = FALSE)
round(p_value,4)
