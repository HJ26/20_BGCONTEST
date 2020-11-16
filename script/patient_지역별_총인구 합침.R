
data <- patient_ko
data <- merge(data,city,by='city',all.x = TRUE)
lst <- ls()
lst <- lst[-3]

PATIENT <- data

#save.image('./RData/PATIENT.RData')
