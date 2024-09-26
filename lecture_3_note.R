


###############
# Q1. Building a data.frame
###############

# ● Create a data.frame matching the table shown
#       ○ hint: type ?rep() into the console to see help for the replicate function
# ● Save the output to an object named “dat”
# ● Verify that the structure and contents of dat are correct
# ● Save “dat” to disk in your working directory with the name “output.csv”

library(tidyverse)

# creat vectors with rep() function
ID = rep(c(1:3), each = 3)
TIME = rep(c(1,6,24), 3)
DV = rep(c(2,4,6), 3)
SEX = rep(c("M", "F", "M"), each = 3)
WT = rep(c(70, 64, 98), each = 3)
ROW = c(1:9)

# create datafrane "dat" with created vectors
dat <- data.frame (ID, TIME, DV, SEX, WT, ROW)

# modify WT value for row #3 & #6
dat[3,5] = 72
dat[6,5] = 63

# show "dat"
dat

# output dataframe "dat" into a csv file "output.csv"
write.csv(dat,file="output.csv", row.names = FALSE)



###############
# Q2. Modifying an existing data set
###############

# ● Import mad-nonmem.csv
# ● Filter out commented rows (i.e. in column C, rows equal to “C”)
# ● Create new columns in the data set
#       ○ TIMED = time in days, rounded to the nearest hundredth
#       ○ SEX_c = recode SEX column from 0/1 to M/F
#       ○ DOSE_f = factor of doses in ascending order
# ● Drop all columns between TAD and ADDL (inclusive), 
#   and DTTM Move TIMED next to the TIME column

###   Challenge: Create a new data frame using a pivot_* function 
###   to create a three- column data set, where the first column is the ID, 
###   the second column is the name of a covariate, and the third column is 
###   the value of a covariate

# Load readr package and import nonmem data file
library(tidyverse)
library(readr)
nmdata <- read.csv("mad-nonmem.csv", header = TRUE)

# filter out commented rows
nmdata %>% filter(C=="C") %>% 
  # create TIMED as TIME round to nearest hundredth
  mutate(TIMED=round(TIME/24, digits = 2)) %>% 
  # create SEX_c as SEX from 1/2 to Male/Female
  mutate(SEX_c=if_else(SEX=="1", "Male", "Female")) %>%
  # create DOSE_f as ascending level of DOSE as factor
  mutate(DOSE_f=factor(DOSE, levels=c(0.5, 2.5, 10, 25))) %>% 
  # drop TAD to ADDL inclusively, and DTTM
  select(-(TAD:ADDL),-DTTM) %>%
  # relocate TIMED after TIME
  relocate(TIMED,.after = TIME)

# Challenge
Challenge <- read.csv("mad-nonmem.csv", header = TRUE)

Challenge %>% select(-(C:NUM),-(TIME:SEX),-(BLQ:DTTM)) %>%
  pivot_longer(cols=AGE:BILI,
               names_to = "covariates",
               values_to = "value")



###############
# Q3. Plotting our data
###############

# ● Plot 1: boxplot showing the distribution of age for each sex in your data set
#       ○ hint: consider the number of rows per subject
# ● Plot 2: spaghetti plot 
#   (e.g. a line for each subject and a dot for each observation) 
#   of all subjects with each dose group uniquely colored, 
#   faceted by study day (DAY column), with the y-axis presented on a log scale 
#       ○ hint: use Factors to get doses to show up in order

###   Challenge: use summarize() and create a boxplot of individual Cmax values 
###   for each dose level

library(tidyverse)
library(readr)
q3nmdata <- read.csv("mad-nonmem.csv", header = TRUE)

# Plot 1
              # group data based on ID and take the first row
q3nmdata %>% group_by(ID) %>% slice (1) %>% ungroup() %>%
  # add a new variable SEX_c as character form of SEX
  mutate(SEX_c=if_else(SEX=="1", "Male", "Female")) %>%
  # create plot
  ggplot() + 
  # create box plot where y-axis is age (AGE) and group by sex (SEX_c)
  geom_boxplot(aes(x=SEX_c, y=AGE, group=SEX_c)) + 
  # modify label for x- and y-axis
  labs(x="Sex", y="Age")
  
# Plot 2
q3nmdata %>% 
  # create "Day" variable which modified from "DAY" and has the format "Day ##"
  mutate(Day=paste0("Day ", DAY)) %>%
  # create "TIMED" from "TIME" as in hour form and two decimal points
  mutate(TIMED=round(TIME/24, digits = 2)) %>%
  # convert "DOSE" into factor as a new variable "DOSE_f"
  mutate(DOSE_f=factor(DOSE, levels=c(0.5, 2.5, 10, 25))) %>%
  # create spaghetti plot, x-axis = TIMED, y-axis = DV (concentration),
  # group by ID, and color code based on dosage (DOSE_f)
  ggplot(aes(x=TIMED, y=DV, colour=DOSE_f, group=ID)) + 
  # add spaghetti line
  geom_line() +
  # add data point
  geom_point() +
  # change y-axis to log10 scale
  scale_y_log10() +
  # separate graph based on study day, unfixed x-axis so graph is stretch-out, 
  # and make graph separated into 2 rows
  facet_wrap(~Day, scales="free_x", nrow=2) +
  # modify x- and y-axis label, and add legend for dosage group
  labs(x="Time", y="log10(concentration)", colour="Dose") +
  # move legend (Dose) to bottom of the graph
  theme(legend.position="bottom")

# Challenge
q3nmdata %>% 
  # convert "DOSE" into factor as a new variable "DOSE_f"
  mutate(DOSE_f=factor(DOSE, levels=c(0.5, 2.5, 10, 25))) %>%
  # group by ID, and then group by DOSE_f
  group_by(ID, DOSE_f) %>%
  # calculate maximum concentraton (Cmax) from concentration (DV)
  summarize(Cmax=max(DV)) %>%
  # create plot
  ggplot() + 
  # create box plot with DOSE_f as groups on x-axis and Cmax shown on y-axis
  geom_boxplot(aes(x=DOSE_f, y=Cmax))+ 
  # change y-axis to log10 scale 
  scale_y_log10() +
  # modify x- and y-axis label
  labs(x="Dose", y="log10(maximum concentration)")



###############
# Q4. Writing a custom summary function
###############

# ● Import mad_nonmem.csv
# ● Write a function that summarizes continuous covariates and outputs them 
#   in the following format: “mean [5th percentile, 95th percentile]”
# ● Apply the function separately for each dose group across the following covariates
#       ○ WT, AGE, EGFR, BILI
# ● Pivot the data set so you have three columns: Dose, Covariate Name, 
#   and Covariate Summary Value

###   Challenge: Use map() to create a list of individual concentration-time plots

library(tidyverse)
library(readr)
q4nmdata <- read.csv("mad-nonmem.csv", header = TRUE)

# Summarize function that outputs one covariate in the following format: 
# “mean [5th percentile, 95th percentile]”
summarize_func <- function(x) {
  paste0(
    round(mean(x),1), 
    ", [", 
    quantile(x,0.05), 
    ", ", 
    quantile(x,0.95), 
    "]")
}


# Apply the function separately for each dose group across the following covariates
#       ○ WT, AGE, EGFR, BILI
# Pivot the data set so you have three columns: Dose, Covariate Name,
# and Covariate Summary Value
q4nmdata %>% 
  group_by(ID) %>% slice(1) %>% ungroup() %>%
  group_by(DOSE) %>%
  summarize(
    WT = summarize_func(WT),
    AGE = summarize_func(AGE),
    EGFR = summarize_func(EGFR),
    BILI = summarize_func(BILI)
  ) %>%
  pivot_longer(WT:BILI)


