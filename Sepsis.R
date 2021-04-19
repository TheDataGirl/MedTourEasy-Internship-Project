install.packages("data.table")
library(data.table)

# Read in the data
antibioticDT <- fread("antibioticDT.csv")
head(antibioticDT, 30)
setorder(antibioticDT, patient_id, antibiotic_type, day_given)
head(antibioticDT, 30)

antibioticDT[ , last_administration_day := shift(day_given, n=1, type = "lag", fill=NA), 
              by = .(patient_id, antibiotic_type)]
head(antibioticDT, 30)

antibioticDT[ , days_since_last_admin := (day_given - last_administration_day), by=.(patient_id)]
head(antibioticDT, 30)

antibioticDT[, antibiotic_new:=1]

antibioticDT[days_since_last_admin <= 2, antibiotic_new := 0]

head(antibioticDT, 30)

#bloodculture data
blood_cultureDT <- fread("blood_cultureDT.csv")

# Print the first 30 rows
head(blood_cultureDT, 30)

# Merging antibioticDT with blood_cultureDT
combinedDT <- merge.data.table(antibioticDT, blood_cultureDT, by = "patient_id", all = FALSE) 
# all=False to keep only rows that match from the data frames

# Sorting by patient_id, blood_culture_day, day_given, and antibiotic_type
setorder(combinedDT, patient_id, blood_culture_day, day_given, antibiotic_type)

# Printing and examining the first 30 rows
head(combinedDT, 30)

combinedDT[, drug_in_bcx_window := as.numeric(abs(blood_culture_day - day_given) <= 2)] # for +- 2 day window
head(combinedDT)

combinedDT[, any_iv_in_bcx_window := as.numeric(any(route == "IV" & drug_in_bcx_window == 1)), 
           by = .(blood_culture_day, patient_id)]
head(combinedDT)

combinedDT <- combinedDT[any_iv_in_bcx_window == 1]
head(combinedDT)

combinedDT[, day_of_first_new_abx_in_window := day_given[antibiotic_new == 1 & drug_in_bcx_window == 1][1], 
           by = .(patient_id, blood_culture_day)]
head(combinedDT, 30)
combinedDT <- combinedDT[day_given >= day_of_first_new_abx_in_window]
head(combinedDT)

simplified_data <- combinedDT[, .(patient_id, blood_culture_day, day_given)]

# Remove duplicate rows
simplified_data <- unique(simplified_data)
head(simplified_data)

simplified_data[, num_antibiotic_days := .N, by = .(patient_id, blood_culture_day)]
head(simplified_data, 10)

simplified_data <- simplified_data[num_antibiotic_days >= 4]

# Select the first four days for each blood culture
first_four_days <- simplified_data[, .SD[1:4], by = .(patient_id, blood_culture_day)]
head(first_four_days, 10)

first_four_days[, four_in_seq := as.numeric(max(diff(day_given)) <= 2), by = .(patient_id, blood_culture_day)]
head(first_four_days,10)

# Selecting the rows which have four_in_seq equal to 1
suspected_infection <- first_four_days[four_in_seq == 1]

# Retaining only the patient_id column
suspected_infection <- suspected_infection[, .(patient_id)]

# Removing duplicates
suspected_infection <- unique(suspected_infection)

# Making an infection indicator
suspected_infection[, infection := 1]
head(suspected_infection)

# Reading in "all_patients.csv"
all_patientsDT <- fread("all_patients.csv")

# Merging this with the infection flag data
all_patientsDT <- merge(all_patientsDT, suspected_infection, all = TRUE) 

# Setting any missing values of the infection flag to 0
all_patientsDT <- all_patientsDT[is.na(infection), infection := 0]

ans  <- round((all_patientsDT[, mean(infection)] * 100), 2)
cat(ans, "% of the patients meet the criteria for presumed infection.")
