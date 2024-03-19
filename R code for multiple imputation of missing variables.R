#
library(mice)

imp_data <- mice(data, 
                 method = "rf"
                 m=5, 
                 printFlag = FALSE 
)
imp_data