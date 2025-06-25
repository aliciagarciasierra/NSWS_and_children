
##################################################################
######### DATA LOADING AND PREPARATION CHILDREN ######################
###################################################################

#-------- LOAD DATA

# Set working directory
# setwd()


new_data <- read.table('NSWSchildren.dat', sep=' ')
names(new_data) <- c('C0000100',
                     'C0000200',
                     'C0005300',
                     'C0005400',
                     'C0005700',
                     'C0812000',
                     'C0812100',
                     'C0812200',
                     'C0812300',
                     'C0812400',
                     'C0812500',
                     'C0823300',
                     'C0824400',
                     'C0824500',
                     'C0824700',
                     'C0826000',
                     'C0991900',
                     'C0993400',
                     'C0995100',
                     'C1002000',
                     'C1002100',
                     'C1002200',
                     'C1002300',
                     'C1002400',
                     'C1002500',
                     'C1013300',
                     'C1014400',
                     'C1014500',
                     'C1014700',
                     'C1016000',
                     'C1188300',
                     'C1192200',
                     'C1193700',
                     'C1195400',
                     'C1205700',
                     'C1205800',
                     'C1205900',
                     'C1206000',
                     'C1206100',
                     'C1206200',
                     'C1216900',
                     'C1218000',
                     'C1218100',
                     'C1218300',
                     'C1219600',
                     'C1442000',
                     'C1500000',
                     'C1501500',
                     'C1503200',
                     'C1525700',
                     'C1525800',
                     'C1525900',
                     'C1526000',
                     'C1526100',
                     'C1526200',
                     'C1530200',
                     'C1531500',
                     'C1531600',
                     'C1531800',
                     'C1532100',
                     'C1556900',
                     'C1558400',
                     'C1560100',
                     'C1644400',
                     'C1772000',
                     'C1772100',
                     'C1772200',
                     'C1772300',
                     'C1772400',
                     'C1772500',
                     'C1777000',
                     'C1778300',
                     'C1778400',
                     'C1778600',
                     'C1778900',
                     'C1792600',
                     'C1794100',
                     'C1795800',
                     'C1984800',
                     'C2252800',
                     'C2253800',
                     'C2253900',
                     'C2254000',
                     'C2254100',
                     'C2254200',
                     'C2254300',
                     'C2261200',
                     'C2262500',
                     'C2262600',
                     'C2262800',
                     'C2263200',
                     'C2496100',
                     'C2502400',
                     'C2503400',
                     'C2524600',
                     'C2530900',
                     'C2531900',
                     'C2692300',
                     'C2692400',
                     'C2692500',
                     'C2692600',
                     'C2692700',
                     'C2692800',
                     'C2706600',
                     'C2707900',
                     'C2708000',
                     'C2736400',
                     'C2741100',
                     'C2741500',
                     'C2795900',
                     'C2802200',
                     'C2973300',
                     'C2973400',
                     'C2973500',
                     'C2973600',
                     'C2973700',
                     'C2973800',
                     'C2983400',
                     'C2984700',
                     'C2984800',
                     'C3008800',
                     'C3013200',
                     'C3013600',
                     'C3103900',
                     'C3110200',
                     'C3111200',
                     'C3555200',
                     'C3555300',
                     'C3555400',
                     'C3555500',
                     'C3555600',
                     'C3555700',
                     'C3574300',
                     'C3575600',
                     'C3575700',
                     'C3579000',
                     'C3579900',
                     'C3580300',
                     'C3607600',
                     'C3613900',
                     'C3614900',
                     'C3900100',
                     'C3900200',
                     'C3900300',
                     'C3900400',
                     'C3900500',
                     'C3900600',
                     'C3911700',
                     'C3913000',
                     'C3913100',
                     'C3951500',
                     'C3952400',
                     'C3952800',
                     'C3986200',
                     'C3992500',
                     'C3993500',
                     'C5149900',
                     'C5150000',
                     'C5150100',
                     'C5150200',
                     'C5150300',
                     'C5150400',
                     'C5162700',
                     'C5164200',
                     'C5164300',
                     'C5195600',
                     'C5196500',
                     'C5196900',
                     'C5530200',
                     'C5536500',
                     'C5537500',
                     'C5727000',
                     'C5727100',
                     'C5727200',
                     'C5727300',
                     'C5727400',
                     'C5727500',
                     'C5738900',
                     'C5740400',
                     'C5740500',
                     'C5770500',
                     'C5771400',
                     'C5771800',
                     'C5806400',
                     'C5812300',
                     'C5813300',
                     'C5998500',
                     'C5998600',
                     'C5998700',
                     'C5998800',
                     'C5998900',
                     'C5999000',
                     'C6011200',
                     'C6012700',
                     'C6012800',
                     'C6037300',
                     'C6038200',
                     'C6038600',
                     'C6052200',
                     'C6056900',
                     'C6072700',
                     'C6072800',
                     'C6072900',
                     'C6073000',
                     'C6073100',
                     'C6073200',
                     'C6078700',
                     'C6080000',
                     'C6080100',
                     'C6104500',
                     'C6105200',
                     'C6105600',
                     'C6127500',
                     'C6127600',
                     'C6127700',
                     'C6127800',
                     'C6127900',
                     'C6128000',
                     'C6130800',
                     'C6131900',
                     'C6132000',
                     'C6133800',
                     'C6134500',
                     'C6134900',
                     'Y2267000')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -7] = NA  # Missing


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$C0005300 <- factor(data$C0005300,
                          levels=c(1.0,2.0,3.0),
                          labels=c("HISPANIC",
                                   "BLACK",
                                   "NON-BLACK, NON-HISPANIC"))
  data$C0005400 <- factor(data$C0005400,
                          levels=c(1.0,2.0),
                          labels=c("MALE",
                                   "FEMALE"))
  data$C0812000 <- factor(data$C0812000,
                          levels=c(0.0,1.0,2.0),
                          labels=c("NO",
                                   "YES",
                                   "DOES NOT ATTEND SCHOOL"))
  data$C0812100 <- factor(data$C0812100,
                          levels=c(0.0,1.0,2.0),
                          labels=c("NO",
                                   "YES",
                                   "DOES NOT ATTEND SCHOOL"))
  data$C0812200 <- factor(data$C0812200,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0812300 <- factor(data$C0812300,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0812400 <- factor(data$C0812400,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0812500 <- factor(data$C0812500,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0824400 <- factor(data$C0824400,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0824500 <- factor(data$C0824500,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0824700 <- factor(data$C0824700,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C0826000 <- factor(data$C0826000,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1002000 <- factor(data$C1002000,
                          levels=c(0.0,1.0,2.0),
                          labels=c("NO",
                                   "YES",
                                   "DOES NOT ATTEND SCHOOL"))
  data$C1002100 <- factor(data$C1002100,
                          levels=c(0.0,1.0,2.0),
                          labels=c("NO",
                                   "YES",
                                   "DOES NOT ATTEND SCHOOL"))
  data$C1002200 <- factor(data$C1002200,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1002300 <- factor(data$C1002300,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1002400 <- factor(data$C1002400,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1002500 <- factor(data$C1002500,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1014400 <- factor(data$C1014400,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1014500 <- factor(data$C1014500,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1014700 <- factor(data$C1014700,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1016000 <- factor(data$C1016000,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1188300 <- factor(data$C1188300,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C1205700 <- factor(data$C1205700,
                          levels=c(0.0,1.0,2.0),
                          labels=c("NO",
                                   "YES",
                                   "DOES NOT ATTEND SCHOOL"))
  data$C1205800 <- factor(data$C1205800,
                          levels=c(0.0,1.0,2.0),
                          labels=c("NO",
                                   "YES",
                                   "DOES NOT ATTEND SCHOOL"))
  data$C1205900 <- factor(data$C1205900,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1206000 <- factor(data$C1206000,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1206100 <- factor(data$C1206100,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1206200 <- factor(data$C1206200,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1218000 <- factor(data$C1218000,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1218100 <- factor(data$C1218100,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1218300 <- factor(data$C1218300,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1219600 <- factor(data$C1219600,
                          levels=c(0.0,1.0),
                          labels=c("NO",
                                   "YES"))
  data$C1442000 <- factor(data$C1442000,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C1525700 <- factor(data$C1525700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1525800 <- factor(data$C1525800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1525900 <- factor(data$C1525900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1526000 <- factor(data$C1526000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1526100 <- factor(data$C1526100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1526200 <- factor(data$C1526200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1531500 <- factor(data$C1531500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1531600 <- factor(data$C1531600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1531800 <- factor(data$C1531800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1532100 <- factor(data$C1532100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1644400 <- factor(data$C1644400,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C1772000 <- factor(data$C1772000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1772100 <- factor(data$C1772100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1772200 <- factor(data$C1772200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1772300 <- factor(data$C1772300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1772400 <- factor(data$C1772400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1772500 <- factor(data$C1772500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1778300 <- factor(data$C1778300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1778400 <- factor(data$C1778400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1778600 <- factor(data$C1778600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1778900 <- factor(data$C1778900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C1984800 <- factor(data$C1984800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C2252800 <- factor(data$C2252800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C2253800 <- factor(data$C2253800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2253900 <- factor(data$C2253900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2254000 <- factor(data$C2254000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2254100 <- factor(data$C2254100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2254200 <- factor(data$C2254200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2254300 <- factor(data$C2254300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2262500 <- factor(data$C2262500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2262600 <- factor(data$C2262600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2262800 <- factor(data$C2262800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2263200 <- factor(data$C2263200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2692300 <- factor(data$C2692300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2692400 <- factor(data$C2692400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2692500 <- factor(data$C2692500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2692600 <- factor(data$C2692600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2692700 <- factor(data$C2692700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2692800 <- factor(data$C2692800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2707900 <- factor(data$C2707900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2708000 <- factor(data$C2708000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2736400 <- factor(data$C2736400,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C2741100 <- factor(data$C2741100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2741500 <- factor(data$C2741500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2973300 <- factor(data$C2973300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2973400 <- factor(data$C2973400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2973500 <- factor(data$C2973500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2973600 <- factor(data$C2973600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2973700 <- factor(data$C2973700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2973800 <- factor(data$C2973800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2984700 <- factor(data$C2984700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C2984800 <- factor(data$C2984800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3008800 <- factor(data$C3008800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C3013200 <- factor(data$C3013200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3013600 <- factor(data$C3013600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3555200 <- factor(data$C3555200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3555300 <- factor(data$C3555300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3555400 <- factor(data$C3555400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3555500 <- factor(data$C3555500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3555600 <- factor(data$C3555600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3555700 <- factor(data$C3555700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3575600 <- factor(data$C3575600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3575700 <- factor(data$C3575700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3579000 <- factor(data$C3579000,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C3579900 <- factor(data$C3579900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3580300 <- factor(data$C3580300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3900100 <- factor(data$C3900100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3900200 <- factor(data$C3900200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3900300 <- factor(data$C3900300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3900400 <- factor(data$C3900400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3900500 <- factor(data$C3900500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3900600 <- factor(data$C3900600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3913000 <- factor(data$C3913000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3913100 <- factor(data$C3913100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3951500 <- factor(data$C3951500,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C3952400 <- factor(data$C3952400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C3952800 <- factor(data$C3952800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5149900 <- factor(data$C5149900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5150000 <- factor(data$C5150000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5150100 <- factor(data$C5150100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5150200 <- factor(data$C5150200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5150300 <- factor(data$C5150300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5150400 <- factor(data$C5150400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5164200 <- factor(data$C5164200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5164300 <- factor(data$C5164300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5195600 <- factor(data$C5195600,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C5196500 <- factor(data$C5196500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5196900 <- factor(data$C5196900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5727000 <- factor(data$C5727000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5727100 <- factor(data$C5727100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5727200 <- factor(data$C5727200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5727300 <- factor(data$C5727300,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5727400 <- factor(data$C5727400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5727500 <- factor(data$C5727500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5740400 <- factor(data$C5740400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5740500 <- factor(data$C5740500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5770500 <- factor(data$C5770500,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C5771400 <- factor(data$C5771400,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5771800 <- factor(data$C5771800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5998500 <- factor(data$C5998500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5998600 <- factor(data$C5998600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5998700 <- factor(data$C5998700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5998800 <- factor(data$C5998800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5998900 <- factor(data$C5998900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C5999000 <- factor(data$C5999000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6012700 <- factor(data$C6012700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6012800 <- factor(data$C6012800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6037300 <- factor(data$C6037300,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C6038200 <- factor(data$C6038200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6038600 <- factor(data$C6038600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6072700 <- factor(data$C6072700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6072800 <- factor(data$C6072800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6072900 <- factor(data$C6072900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6073000 <- factor(data$C6073000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6073100 <- factor(data$C6073100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6073200 <- factor(data$C6073200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6080000 <- factor(data$C6080000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6080100 <- factor(data$C6080100,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6104500 <- factor(data$C6104500,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C6105200 <- factor(data$C6105200,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6105600 <- factor(data$C6105600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6127500 <- factor(data$C6127500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6127600 <- factor(data$C6127600,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6127700 <- factor(data$C6127700,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6127800 <- factor(data$C6127800,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6127900 <- factor(data$C6127900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6128000 <- factor(data$C6128000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6131900 <- factor(data$C6131900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6132000 <- factor(data$C6132000,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6133800 <- factor(data$C6133800,
                          levels=c(1.0,2.0,3.0,4.0),
                          labels=c("POOR",
                                   "FAIR",
                                   "GOOD",
                                   "EXCELLENT"))
  data$C6134500 <- factor(data$C6134500,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$C6134900 <- factor(data$C6134900,
                          levels=c(0.0,1.0),
                          labels=c("No",
                                   "Yes"))
  data$Y2267000 <- factor(data$Y2267000,
                          levels=c(532.0),
                          labels=c("532"))
  return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
  data$C0000100[1.0 <= data$C0000100 & data$C0000100 <= 9999999.0] <- 1.0
  data$C0000100 <- factor(data$C0000100,
                          levels=c(1.0),
                          labels=c("1 TO 9999999: See Min & Max values below for range as of this release"))
  data$C0000200[1.0 <= data$C0000200 & data$C0000200 <= 12686.0] <- 1.0
  data$C0000200 <- factor(data$C0000200,
                          levels=c(1.0),
                          labels=c("1 TO 12686: NLSY79 Public ID"))
  data$C0005700[1970.0 <= data$C0005700 & data$C0005700 <= 1978.0] <- 1970.0
  data$C0005700 <- factor(data$C0005700,
                          levels=c(1970.0,1979.0,1980.0,1981.0,1982.0,1983.0,1984.0,1985.0,1986.0,1987.0,1988.0,1989.0,1990.0,1991.0,1992.0,1993.0,1994.0,1995.0,1996.0,1997.0,1998.0,1999.0,2000.0,2001.0,2002.0,2003.0,2004.0,2005.0,2006.0,2007.0,2008.0,2009.0,2010.0,2011.0,2012.0,2013.0,2014.0,2015.0,2016.0,2017.0,2018.0,2019.0,2020.0,2021.0),
                          labels=c("1970 TO 1978: < before 1979",
                                   "1979",
                                   "1980",
                                   "1981",
                                   "1982",
                                   "1983",
                                   "1984",
                                   "1985",
                                   "1986",
                                   "1987",
                                   "1988",
                                   "1989",
                                   "1990",
                                   "1991",
                                   "1992",
                                   "1993",
                                   "1994",
                                   "1995",
                                   "1996",
                                   "1997",
                                   "1998",
                                   "1999",
                                   "2000",
                                   "2001",
                                   "2002",
                                   "2003",
                                   "2004",
                                   "2005",
                                   "2006",
                                   "2007",
                                   "2008",
                                   "2009",
                                   "2010",
                                   "2011",
                                   "2012",
                                   "2013",
                                   "2014",
                                   "2015",
                                   "2016",
                                   "2017",
                                   "2018",
                                   "2019",
                                   "2020",
                                   "2021"))
  data$C0823300[17.0 <= data$C0823300 & data$C0823300 <= 99999.0] <- 17.0
  data$C0823300 <- factor(data$C0823300,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$C0991900[1.0 <= data$C0991900 & data$C0991900 <= 99.0] <- 1.0
  data$C0991900[100.0 <= data$C0991900 & data$C0991900 <= 199.0] <- 100.0
  data$C0991900[200.0 <= data$C0991900 & data$C0991900 <= 299.0] <- 200.0
  data$C0991900[300.0 <= data$C0991900 & data$C0991900 <= 399.0] <- 300.0
  data$C0991900[400.0 <= data$C0991900 & data$C0991900 <= 499.0] <- 400.0
  data$C0991900[500.0 <= data$C0991900 & data$C0991900 <= 599.0] <- 500.0
  data$C0991900[600.0 <= data$C0991900 & data$C0991900 <= 699.0] <- 600.0
  data$C0991900[700.0 <= data$C0991900 & data$C0991900 <= 799.0] <- 700.0
  data$C0991900[800.0 <= data$C0991900 & data$C0991900 <= 899.0] <- 800.0
  data$C0991900[900.0 <= data$C0991900 & data$C0991900 <= 999.0] <- 900.0
  data$C0991900[1000.0 <= data$C0991900 & data$C0991900 <= 1099.0] <- 1000.0
  data$C0991900[1100.0 <= data$C0991900 & data$C0991900 <= 1199.0] <- 1100.0
  data$C0991900[1200.0 <= data$C0991900 & data$C0991900 <= 1299.0] <- 1200.0
  data$C0991900[1300.0 <= data$C0991900 & data$C0991900 <= 1399.0] <- 1300.0
  data$C0991900[1400.0 <= data$C0991900 & data$C0991900 <= 1499.0] <- 1400.0
  data$C0991900[1500.0 <= data$C0991900 & data$C0991900 <= 9999999.0] <- 1500.0
  data$C0991900 <- factor(data$C0991900,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C0993400[0.0 <= data$C0993400 & data$C0993400 <= 64.0] <- 0.0
  data$C0993400[65.0 <= data$C0993400 & data$C0993400 <= 69.0] <- 65.0
  data$C0993400[70.0 <= data$C0993400 & data$C0993400 <= 84.0] <- 70.0
  data$C0993400[85.0 <= data$C0993400 & data$C0993400 <= 99.0] <- 85.0
  data$C0993400[100.0 <= data$C0993400 & data$C0993400 <= 114.0] <- 100.0
  data$C0993400[115.0 <= data$C0993400 & data$C0993400 <= 129.0] <- 115.0
  data$C0993400[130.0 <= data$C0993400 & data$C0993400 <= 144.0] <- 130.0
  data$C0993400[145.0 <= data$C0993400 & data$C0993400 <= 9999.0] <- 145.0
  data$C0993400 <- factor(data$C0993400,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C0995100[0.0 <= data$C0995100 & data$C0995100 <= 64.0] <- 0.0
  data$C0995100[65.0 <= data$C0995100 & data$C0995100 <= 69.0] <- 65.0
  data$C0995100[70.0 <= data$C0995100 & data$C0995100 <= 84.0] <- 70.0
  data$C0995100[85.0 <= data$C0995100 & data$C0995100 <= 99.0] <- 85.0
  data$C0995100[100.0 <= data$C0995100 & data$C0995100 <= 114.0] <- 100.0
  data$C0995100[115.0 <= data$C0995100 & data$C0995100 <= 129.0] <- 115.0
  data$C0995100[130.0 <= data$C0995100 & data$C0995100 <= 144.0] <- 130.0
  data$C0995100[145.0 <= data$C0995100 & data$C0995100 <= 9999.0] <- 145.0
  data$C0995100 <- factor(data$C0995100,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1013300[17.0 <= data$C1013300 & data$C1013300 <= 99999.0] <- 17.0
  data$C1013300 <- factor(data$C1013300,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
                          labels=c("0: < 1",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16",
                                   "17 TO 99999: 17+"))
  data$C1192200[1.0 <= data$C1192200 & data$C1192200 <= 99.0] <- 1.0
  data$C1192200[100.0 <= data$C1192200 & data$C1192200 <= 199.0] <- 100.0
  data$C1192200[200.0 <= data$C1192200 & data$C1192200 <= 299.0] <- 200.0
  data$C1192200[300.0 <= data$C1192200 & data$C1192200 <= 399.0] <- 300.0
  data$C1192200[400.0 <= data$C1192200 & data$C1192200 <= 499.0] <- 400.0
  data$C1192200[500.0 <= data$C1192200 & data$C1192200 <= 599.0] <- 500.0
  data$C1192200[600.0 <= data$C1192200 & data$C1192200 <= 699.0] <- 600.0
  data$C1192200[700.0 <= data$C1192200 & data$C1192200 <= 799.0] <- 700.0
  data$C1192200[800.0 <= data$C1192200 & data$C1192200 <= 899.0] <- 800.0
  data$C1192200[900.0 <= data$C1192200 & data$C1192200 <= 999.0] <- 900.0
  data$C1192200[1000.0 <= data$C1192200 & data$C1192200 <= 1099.0] <- 1000.0
  data$C1192200[1100.0 <= data$C1192200 & data$C1192200 <= 1199.0] <- 1100.0
  data$C1192200[1200.0 <= data$C1192200 & data$C1192200 <= 1299.0] <- 1200.0
  data$C1192200[1300.0 <= data$C1192200 & data$C1192200 <= 1399.0] <- 1300.0
  data$C1192200[1400.0 <= data$C1192200 & data$C1192200 <= 1499.0] <- 1400.0
  data$C1192200[1500.0 <= data$C1192200 & data$C1192200 <= 9999999.0] <- 1500.0
  data$C1192200 <- factor(data$C1192200,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C1193700[0.0 <= data$C1193700 & data$C1193700 <= 64.0] <- 0.0
  data$C1193700[65.0 <= data$C1193700 & data$C1193700 <= 69.0] <- 65.0
  data$C1193700[70.0 <= data$C1193700 & data$C1193700 <= 84.0] <- 70.0
  data$C1193700[85.0 <= data$C1193700 & data$C1193700 <= 99.0] <- 85.0
  data$C1193700[100.0 <= data$C1193700 & data$C1193700 <= 114.0] <- 100.0
  data$C1193700[115.0 <= data$C1193700 & data$C1193700 <= 129.0] <- 115.0
  data$C1193700[130.0 <= data$C1193700 & data$C1193700 <= 144.0] <- 130.0
  data$C1193700[145.0 <= data$C1193700 & data$C1193700 <= 9999.0] <- 145.0
  data$C1193700 <- factor(data$C1193700,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1195400[0.0 <= data$C1195400 & data$C1195400 <= 64.0] <- 0.0
  data$C1195400[65.0 <= data$C1195400 & data$C1195400 <= 69.0] <- 65.0
  data$C1195400[70.0 <= data$C1195400 & data$C1195400 <= 84.0] <- 70.0
  data$C1195400[85.0 <= data$C1195400 & data$C1195400 <= 99.0] <- 85.0
  data$C1195400[100.0 <= data$C1195400 & data$C1195400 <= 114.0] <- 100.0
  data$C1195400[115.0 <= data$C1195400 & data$C1195400 <= 129.0] <- 115.0
  data$C1195400[130.0 <= data$C1195400 & data$C1195400 <= 144.0] <- 130.0
  data$C1195400[145.0 <= data$C1195400 & data$C1195400 <= 9999.0] <- 145.0
  data$C1195400 <- factor(data$C1195400,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1216900[16.0 <= data$C1216900 & data$C1216900 <= 99999.0] <- 16.0
  data$C1216900 <- factor(data$C1216900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11",
                                   "12",
                                   "13",
                                   "14",
                                   "15",
                                   "16 TO 99999: 16+"))
  data$C1500000[1.0 <= data$C1500000 & data$C1500000 <= 99.0] <- 1.0
  data$C1500000[100.0 <= data$C1500000 & data$C1500000 <= 199.0] <- 100.0
  data$C1500000[200.0 <= data$C1500000 & data$C1500000 <= 299.0] <- 200.0
  data$C1500000[300.0 <= data$C1500000 & data$C1500000 <= 399.0] <- 300.0
  data$C1500000[400.0 <= data$C1500000 & data$C1500000 <= 499.0] <- 400.0
  data$C1500000[500.0 <= data$C1500000 & data$C1500000 <= 599.0] <- 500.0
  data$C1500000[600.0 <= data$C1500000 & data$C1500000 <= 699.0] <- 600.0
  data$C1500000[700.0 <= data$C1500000 & data$C1500000 <= 799.0] <- 700.0
  data$C1500000[800.0 <= data$C1500000 & data$C1500000 <= 899.0] <- 800.0
  data$C1500000[900.0 <= data$C1500000 & data$C1500000 <= 999.0] <- 900.0
  data$C1500000[1000.0 <= data$C1500000 & data$C1500000 <= 1099.0] <- 1000.0
  data$C1500000[1100.0 <= data$C1500000 & data$C1500000 <= 1199.0] <- 1100.0
  data$C1500000[1200.0 <= data$C1500000 & data$C1500000 <= 1299.0] <- 1200.0
  data$C1500000[1300.0 <= data$C1500000 & data$C1500000 <= 1399.0] <- 1300.0
  data$C1500000[1400.0 <= data$C1500000 & data$C1500000 <= 1499.0] <- 1400.0
  data$C1500000[1500.0 <= data$C1500000 & data$C1500000 <= 9999999.0] <- 1500.0
  data$C1500000 <- factor(data$C1500000,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 1099",
                                   "1100 TO 1199",
                                   "1200 TO 1299",
                                   "1300 TO 1399",
                                   "1400 TO 1499",
                                   "1500 TO 9999999: 1500+"))
  data$C1501500[0.0 <= data$C1501500 & data$C1501500 <= 64.0] <- 0.0
  data$C1501500[65.0 <= data$C1501500 & data$C1501500 <= 69.0] <- 65.0
  data$C1501500[70.0 <= data$C1501500 & data$C1501500 <= 84.0] <- 70.0
  data$C1501500[85.0 <= data$C1501500 & data$C1501500 <= 99.0] <- 85.0
  data$C1501500[100.0 <= data$C1501500 & data$C1501500 <= 114.0] <- 100.0
  data$C1501500[115.0 <= data$C1501500 & data$C1501500 <= 129.0] <- 115.0
  data$C1501500[130.0 <= data$C1501500 & data$C1501500 <= 144.0] <- 130.0
  data$C1501500[145.0 <= data$C1501500 & data$C1501500 <= 9999.0] <- 145.0
  data$C1501500 <- factor(data$C1501500,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1503200[0.0 <= data$C1503200 & data$C1503200 <= 64.0] <- 0.0
  data$C1503200[65.0 <= data$C1503200 & data$C1503200 <= 69.0] <- 65.0
  data$C1503200[70.0 <= data$C1503200 & data$C1503200 <= 84.0] <- 70.0
  data$C1503200[85.0 <= data$C1503200 & data$C1503200 <= 99.0] <- 85.0
  data$C1503200[100.0 <= data$C1503200 & data$C1503200 <= 114.0] <- 100.0
  data$C1503200[115.0 <= data$C1503200 & data$C1503200 <= 129.0] <- 115.0
  data$C1503200[130.0 <= data$C1503200 & data$C1503200 <= 144.0] <- 130.0
  data$C1503200[145.0 <= data$C1503200 & data$C1503200 <= 9999.0] <- 145.0
  data$C1503200 <- factor(data$C1503200,
                          levels=c(0.0,65.0,70.0,85.0,100.0,115.0,130.0,145.0),
                          labels=c("0 TO 64",
                                   "65 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 144",
                                   "145 TO 9999: 145 +"))
  data$C1530200[11.0 <= data$C1530200 & data$C1530200 <= 999999.0] <- 11.0
  data$C1530200 <- factor(data$C1530200,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 999999: > 10"))
  data$C1556900[1.0 <= data$C1556900 & data$C1556900 <= 99.0] <- 1.0
  data$C1556900[100.0 <= data$C1556900 & data$C1556900 <= 199.0] <- 100.0
  data$C1556900[200.0 <= data$C1556900 & data$C1556900 <= 299.0] <- 200.0
  data$C1556900[300.0 <= data$C1556900 & data$C1556900 <= 399.0] <- 300.0
  data$C1556900[400.0 <= data$C1556900 & data$C1556900 <= 499.0] <- 400.0
  data$C1556900[500.0 <= data$C1556900 & data$C1556900 <= 599.0] <- 500.0
  data$C1556900[600.0 <= data$C1556900 & data$C1556900 <= 699.0] <- 600.0
  data$C1556900[700.0 <= data$C1556900 & data$C1556900 <= 799.0] <- 700.0
  data$C1556900[800.0 <= data$C1556900 & data$C1556900 <= 899.0] <- 800.0
  data$C1556900[900.0 <= data$C1556900 & data$C1556900 <= 999.0] <- 900.0
  data$C1556900[1000.0 <= data$C1556900 & data$C1556900 <= 9.9999999E7] <- 1000.0
  data$C1556900 <- factor(data$C1556900,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C1558400[0.0 <= data$C1558400 & data$C1558400 <= 69.0] <- 0.0
  data$C1558400[70.0 <= data$C1558400 & data$C1558400 <= 84.0] <- 70.0
  data$C1558400[85.0 <= data$C1558400 & data$C1558400 <= 99.0] <- 85.0
  data$C1558400[100.0 <= data$C1558400 & data$C1558400 <= 114.0] <- 100.0
  data$C1558400[115.0 <= data$C1558400 & data$C1558400 <= 129.0] <- 115.0
  data$C1558400[130.0 <= data$C1558400 & data$C1558400 <= 999999.0] <- 130.0
  data$C1558400 <- factor(data$C1558400,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C1560100[0.0 <= data$C1560100 & data$C1560100 <= 69.0] <- 0.0
  data$C1560100[70.0 <= data$C1560100 & data$C1560100 <= 84.0] <- 70.0
  data$C1560100[85.0 <= data$C1560100 & data$C1560100 <= 99.0] <- 85.0
  data$C1560100[100.0 <= data$C1560100 & data$C1560100 <= 114.0] <- 100.0
  data$C1560100[115.0 <= data$C1560100 & data$C1560100 <= 129.0] <- 115.0
  data$C1560100[130.0 <= data$C1560100 & data$C1560100 <= 999999.0] <- 130.0
  data$C1560100 <- factor(data$C1560100,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C1777000[11.0 <= data$C1777000 & data$C1777000 <= 9999.0] <- 11.0
  data$C1777000 <- factor(data$C1777000,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C1792600[1.0 <= data$C1792600 & data$C1792600 <= 99.0] <- 1.0
  data$C1792600[100.0 <= data$C1792600 & data$C1792600 <= 199.0] <- 100.0
  data$C1792600[200.0 <= data$C1792600 & data$C1792600 <= 299.0] <- 200.0
  data$C1792600[300.0 <= data$C1792600 & data$C1792600 <= 399.0] <- 300.0
  data$C1792600[400.0 <= data$C1792600 & data$C1792600 <= 499.0] <- 400.0
  data$C1792600[500.0 <= data$C1792600 & data$C1792600 <= 599.0] <- 500.0
  data$C1792600[600.0 <= data$C1792600 & data$C1792600 <= 699.0] <- 600.0
  data$C1792600[700.0 <= data$C1792600 & data$C1792600 <= 799.0] <- 700.0
  data$C1792600[800.0 <= data$C1792600 & data$C1792600 <= 899.0] <- 800.0
  data$C1792600[900.0 <= data$C1792600 & data$C1792600 <= 999.0] <- 900.0
  data$C1792600[1000.0 <= data$C1792600 & data$C1792600 <= 9.9999999E7] <- 1000.0
  data$C1792600 <- factor(data$C1792600,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999"))
  data$C1794100[0.0 <= data$C1794100 & data$C1794100 <= 69.0] <- 0.0
  data$C1794100[70.0 <= data$C1794100 & data$C1794100 <= 84.0] <- 70.0
  data$C1794100[85.0 <= data$C1794100 & data$C1794100 <= 99.0] <- 85.0
  data$C1794100[100.0 <= data$C1794100 & data$C1794100 <= 114.0] <- 100.0
  data$C1794100[115.0 <= data$C1794100 & data$C1794100 <= 129.0] <- 115.0
  data$C1794100[130.0 <= data$C1794100 & data$C1794100 <= 999999.0] <- 130.0
  data$C1794100 <- factor(data$C1794100,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999"))
  data$C1795800[0.0 <= data$C1795800 & data$C1795800 <= 69.0] <- 0.0
  data$C1795800[70.0 <= data$C1795800 & data$C1795800 <= 84.0] <- 70.0
  data$C1795800[85.0 <= data$C1795800 & data$C1795800 <= 99.0] <- 85.0
  data$C1795800[100.0 <= data$C1795800 & data$C1795800 <= 114.0] <- 100.0
  data$C1795800[115.0 <= data$C1795800 & data$C1795800 <= 129.0] <- 115.0
  data$C1795800[130.0 <= data$C1795800 & data$C1795800 <= 999999.0] <- 130.0
  data$C1795800 <- factor(data$C1795800,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999"))
  data$C2261200[11.0 <= data$C2261200 & data$C2261200 <= 9999.0] <- 11.0
  data$C2261200 <- factor(data$C2261200,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C2496100[0.0 <= data$C2496100 & data$C2496100 <= 69.0] <- 0.0
  data$C2496100[70.0 <= data$C2496100 & data$C2496100 <= 84.0] <- 70.0
  data$C2496100[85.0 <= data$C2496100 & data$C2496100 <= 99.0] <- 85.0
  data$C2496100[100.0 <= data$C2496100 & data$C2496100 <= 114.0] <- 100.0
  data$C2496100[115.0 <= data$C2496100 & data$C2496100 <= 129.0] <- 115.0
  data$C2496100[130.0 <= data$C2496100 & data$C2496100 <= 999999.0] <- 130.0
  data$C2496100 <- factor(data$C2496100,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2502400[1.0 <= data$C2502400 & data$C2502400 <= 99.0] <- 1.0
  data$C2502400[100.0 <= data$C2502400 & data$C2502400 <= 199.0] <- 100.0
  data$C2502400[200.0 <= data$C2502400 & data$C2502400 <= 299.0] <- 200.0
  data$C2502400[300.0 <= data$C2502400 & data$C2502400 <= 399.0] <- 300.0
  data$C2502400[400.0 <= data$C2502400 & data$C2502400 <= 499.0] <- 400.0
  data$C2502400[500.0 <= data$C2502400 & data$C2502400 <= 599.0] <- 500.0
  data$C2502400[600.0 <= data$C2502400 & data$C2502400 <= 699.0] <- 600.0
  data$C2502400[700.0 <= data$C2502400 & data$C2502400 <= 799.0] <- 700.0
  data$C2502400[800.0 <= data$C2502400 & data$C2502400 <= 899.0] <- 800.0
  data$C2502400[900.0 <= data$C2502400 & data$C2502400 <= 999.0] <- 900.0
  data$C2502400[1000.0 <= data$C2502400 & data$C2502400 <= 9.9999999E7] <- 1000.0
  data$C2502400 <- factor(data$C2502400,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C2503400[0.0 <= data$C2503400 & data$C2503400 <= 69.0] <- 0.0
  data$C2503400[70.0 <= data$C2503400 & data$C2503400 <= 84.0] <- 70.0
  data$C2503400[85.0 <= data$C2503400 & data$C2503400 <= 99.0] <- 85.0
  data$C2503400[100.0 <= data$C2503400 & data$C2503400 <= 114.0] <- 100.0
  data$C2503400[115.0 <= data$C2503400 & data$C2503400 <= 129.0] <- 115.0
  data$C2503400[130.0 <= data$C2503400 & data$C2503400 <= 999999.0] <- 130.0
  data$C2503400 <- factor(data$C2503400,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2524600[0.0 <= data$C2524600 & data$C2524600 <= 69.0] <- 0.0
  data$C2524600[70.0 <= data$C2524600 & data$C2524600 <= 84.0] <- 70.0
  data$C2524600[85.0 <= data$C2524600 & data$C2524600 <= 99.0] <- 85.0
  data$C2524600[100.0 <= data$C2524600 & data$C2524600 <= 114.0] <- 100.0
  data$C2524600[115.0 <= data$C2524600 & data$C2524600 <= 129.0] <- 115.0
  data$C2524600[130.0 <= data$C2524600 & data$C2524600 <= 999999.0] <- 130.0
  data$C2524600 <- factor(data$C2524600,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2530900[1.0 <= data$C2530900 & data$C2530900 <= 99.0] <- 1.0
  data$C2530900[100.0 <= data$C2530900 & data$C2530900 <= 199.0] <- 100.0
  data$C2530900[200.0 <= data$C2530900 & data$C2530900 <= 299.0] <- 200.0
  data$C2530900[300.0 <= data$C2530900 & data$C2530900 <= 399.0] <- 300.0
  data$C2530900[400.0 <= data$C2530900 & data$C2530900 <= 499.0] <- 400.0
  data$C2530900[500.0 <= data$C2530900 & data$C2530900 <= 599.0] <- 500.0
  data$C2530900[600.0 <= data$C2530900 & data$C2530900 <= 699.0] <- 600.0
  data$C2530900[700.0 <= data$C2530900 & data$C2530900 <= 799.0] <- 700.0
  data$C2530900[800.0 <= data$C2530900 & data$C2530900 <= 899.0] <- 800.0
  data$C2530900[900.0 <= data$C2530900 & data$C2530900 <= 999.0] <- 900.0
  data$C2530900[1000.0 <= data$C2530900 & data$C2530900 <= 9.9999999E7] <- 1000.0
  data$C2530900 <- factor(data$C2530900,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C2531900[0.0 <= data$C2531900 & data$C2531900 <= 69.0] <- 0.0
  data$C2531900[70.0 <= data$C2531900 & data$C2531900 <= 84.0] <- 70.0
  data$C2531900[85.0 <= data$C2531900 & data$C2531900 <= 99.0] <- 85.0
  data$C2531900[100.0 <= data$C2531900 & data$C2531900 <= 114.0] <- 100.0
  data$C2531900[115.0 <= data$C2531900 & data$C2531900 <= 129.0] <- 115.0
  data$C2531900[130.0 <= data$C2531900 & data$C2531900 <= 999999.0] <- 130.0
  data$C2531900 <- factor(data$C2531900,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2706600[11.0 <= data$C2706600 & data$C2706600 <= 9999.0] <- 11.0
  data$C2706600 <- factor(data$C2706600,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C2795900[0.0 <= data$C2795900 & data$C2795900 <= 69.0] <- 0.0
  data$C2795900[70.0 <= data$C2795900 & data$C2795900 <= 84.0] <- 70.0
  data$C2795900[85.0 <= data$C2795900 & data$C2795900 <= 99.0] <- 85.0
  data$C2795900[100.0 <= data$C2795900 & data$C2795900 <= 114.0] <- 100.0
  data$C2795900[115.0 <= data$C2795900 & data$C2795900 <= 129.0] <- 115.0
  data$C2795900[130.0 <= data$C2795900 & data$C2795900 <= 999999.0] <- 130.0
  data$C2795900 <- factor(data$C2795900,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C2802200[1.0 <= data$C2802200 & data$C2802200 <= 99.0] <- 1.0
  data$C2802200[100.0 <= data$C2802200 & data$C2802200 <= 199.0] <- 100.0
  data$C2802200[200.0 <= data$C2802200 & data$C2802200 <= 299.0] <- 200.0
  data$C2802200[300.0 <= data$C2802200 & data$C2802200 <= 399.0] <- 300.0
  data$C2802200[400.0 <= data$C2802200 & data$C2802200 <= 499.0] <- 400.0
  data$C2802200[500.0 <= data$C2802200 & data$C2802200 <= 599.0] <- 500.0
  data$C2802200[600.0 <= data$C2802200 & data$C2802200 <= 699.0] <- 600.0
  data$C2802200[700.0 <= data$C2802200 & data$C2802200 <= 799.0] <- 700.0
  data$C2802200[800.0 <= data$C2802200 & data$C2802200 <= 899.0] <- 800.0
  data$C2802200[900.0 <= data$C2802200 & data$C2802200 <= 999.0] <- 900.0
  data$C2802200[1000.0 <= data$C2802200 & data$C2802200 <= 9.9999999E7] <- 1000.0
  data$C2802200 <- factor(data$C2802200,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C2983400[11.0 <= data$C2983400 & data$C2983400 <= 9999.0] <- 11.0
  data$C2983400 <- factor(data$C2983400,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C3103900[0.0 <= data$C3103900 & data$C3103900 <= 69.0] <- 0.0
  data$C3103900[70.0 <= data$C3103900 & data$C3103900 <= 84.0] <- 70.0
  data$C3103900[85.0 <= data$C3103900 & data$C3103900 <= 99.0] <- 85.0
  data$C3103900[100.0 <= data$C3103900 & data$C3103900 <= 114.0] <- 100.0
  data$C3103900[115.0 <= data$C3103900 & data$C3103900 <= 129.0] <- 115.0
  data$C3103900[130.0 <= data$C3103900 & data$C3103900 <= 999999.0] <- 130.0
  data$C3103900 <- factor(data$C3103900,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3110200[1.0 <= data$C3110200 & data$C3110200 <= 99.0] <- 1.0
  data$C3110200[100.0 <= data$C3110200 & data$C3110200 <= 199.0] <- 100.0
  data$C3110200[200.0 <= data$C3110200 & data$C3110200 <= 299.0] <- 200.0
  data$C3110200[300.0 <= data$C3110200 & data$C3110200 <= 399.0] <- 300.0
  data$C3110200[400.0 <= data$C3110200 & data$C3110200 <= 499.0] <- 400.0
  data$C3110200[500.0 <= data$C3110200 & data$C3110200 <= 599.0] <- 500.0
  data$C3110200[600.0 <= data$C3110200 & data$C3110200 <= 699.0] <- 600.0
  data$C3110200[700.0 <= data$C3110200 & data$C3110200 <= 799.0] <- 700.0
  data$C3110200[800.0 <= data$C3110200 & data$C3110200 <= 899.0] <- 800.0
  data$C3110200[900.0 <= data$C3110200 & data$C3110200 <= 999.0] <- 900.0
  data$C3110200[1000.0 <= data$C3110200 & data$C3110200 <= 9.9999999E7] <- 1000.0
  data$C3110200 <- factor(data$C3110200,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C3111200[0.0 <= data$C3111200 & data$C3111200 <= 69.0] <- 0.0
  data$C3111200[70.0 <= data$C3111200 & data$C3111200 <= 84.0] <- 70.0
  data$C3111200[85.0 <= data$C3111200 & data$C3111200 <= 99.0] <- 85.0
  data$C3111200[100.0 <= data$C3111200 & data$C3111200 <= 114.0] <- 100.0
  data$C3111200[115.0 <= data$C3111200 & data$C3111200 <= 129.0] <- 115.0
  data$C3111200[130.0 <= data$C3111200 & data$C3111200 <= 999999.0] <- 130.0
  data$C3111200 <- factor(data$C3111200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3574300[11.0 <= data$C3574300 & data$C3574300 <= 9999.0] <- 11.0
  data$C3574300 <- factor(data$C3574300,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C3607600[0.0 <= data$C3607600 & data$C3607600 <= 69.0] <- 0.0
  data$C3607600[70.0 <= data$C3607600 & data$C3607600 <= 84.0] <- 70.0
  data$C3607600[85.0 <= data$C3607600 & data$C3607600 <= 99.0] <- 85.0
  data$C3607600[100.0 <= data$C3607600 & data$C3607600 <= 114.0] <- 100.0
  data$C3607600[115.0 <= data$C3607600 & data$C3607600 <= 129.0] <- 115.0
  data$C3607600[130.0 <= data$C3607600 & data$C3607600 <= 999999.0] <- 130.0
  data$C3607600 <- factor(data$C3607600,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3613900[1.0 <= data$C3613900 & data$C3613900 <= 99.0] <- 1.0
  data$C3613900[100.0 <= data$C3613900 & data$C3613900 <= 199.0] <- 100.0
  data$C3613900[200.0 <= data$C3613900 & data$C3613900 <= 299.0] <- 200.0
  data$C3613900[300.0 <= data$C3613900 & data$C3613900 <= 399.0] <- 300.0
  data$C3613900[400.0 <= data$C3613900 & data$C3613900 <= 499.0] <- 400.0
  data$C3613900[500.0 <= data$C3613900 & data$C3613900 <= 599.0] <- 500.0
  data$C3613900[600.0 <= data$C3613900 & data$C3613900 <= 699.0] <- 600.0
  data$C3613900[700.0 <= data$C3613900 & data$C3613900 <= 799.0] <- 700.0
  data$C3613900[800.0 <= data$C3613900 & data$C3613900 <= 899.0] <- 800.0
  data$C3613900[900.0 <= data$C3613900 & data$C3613900 <= 999.0] <- 900.0
  data$C3613900[1000.0 <= data$C3613900 & data$C3613900 <= 9.9999999E7] <- 1000.0
  data$C3613900 <- factor(data$C3613900,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C3614900[0.0 <= data$C3614900 & data$C3614900 <= 69.0] <- 0.0
  data$C3614900[70.0 <= data$C3614900 & data$C3614900 <= 84.0] <- 70.0
  data$C3614900[85.0 <= data$C3614900 & data$C3614900 <= 99.0] <- 85.0
  data$C3614900[100.0 <= data$C3614900 & data$C3614900 <= 114.0] <- 100.0
  data$C3614900[115.0 <= data$C3614900 & data$C3614900 <= 129.0] <- 115.0
  data$C3614900[130.0 <= data$C3614900 & data$C3614900 <= 999999.0] <- 130.0
  data$C3614900 <- factor(data$C3614900,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3911700[11.0 <= data$C3911700 & data$C3911700 <= 9999.0] <- 11.0
  data$C3911700 <- factor(data$C3911700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C3986200[0.0 <= data$C3986200 & data$C3986200 <= 69.0] <- 0.0
  data$C3986200[70.0 <= data$C3986200 & data$C3986200 <= 84.0] <- 70.0
  data$C3986200[85.0 <= data$C3986200 & data$C3986200 <= 99.0] <- 85.0
  data$C3986200[100.0 <= data$C3986200 & data$C3986200 <= 114.0] <- 100.0
  data$C3986200[115.0 <= data$C3986200 & data$C3986200 <= 129.0] <- 115.0
  data$C3986200[130.0 <= data$C3986200 & data$C3986200 <= 999999.0] <- 130.0
  data$C3986200 <- factor(data$C3986200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C3992500[1.0 <= data$C3992500 & data$C3992500 <= 99.0] <- 1.0
  data$C3992500[100.0 <= data$C3992500 & data$C3992500 <= 199.0] <- 100.0
  data$C3992500[200.0 <= data$C3992500 & data$C3992500 <= 299.0] <- 200.0
  data$C3992500[300.0 <= data$C3992500 & data$C3992500 <= 399.0] <- 300.0
  data$C3992500[400.0 <= data$C3992500 & data$C3992500 <= 499.0] <- 400.0
  data$C3992500[500.0 <= data$C3992500 & data$C3992500 <= 599.0] <- 500.0
  data$C3992500[600.0 <= data$C3992500 & data$C3992500 <= 699.0] <- 600.0
  data$C3992500[700.0 <= data$C3992500 & data$C3992500 <= 799.0] <- 700.0
  data$C3992500[800.0 <= data$C3992500 & data$C3992500 <= 899.0] <- 800.0
  data$C3992500[900.0 <= data$C3992500 & data$C3992500 <= 999.0] <- 900.0
  data$C3992500[1000.0 <= data$C3992500 & data$C3992500 <= 9.9999999E7] <- 1000.0
  data$C3992500 <- factor(data$C3992500,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C3993500[0.0 <= data$C3993500 & data$C3993500 <= 69.0] <- 0.0
  data$C3993500[70.0 <= data$C3993500 & data$C3993500 <= 84.0] <- 70.0
  data$C3993500[85.0 <= data$C3993500 & data$C3993500 <= 99.0] <- 85.0
  data$C3993500[100.0 <= data$C3993500 & data$C3993500 <= 114.0] <- 100.0
  data$C3993500[115.0 <= data$C3993500 & data$C3993500 <= 129.0] <- 115.0
  data$C3993500[130.0 <= data$C3993500 & data$C3993500 <= 999999.0] <- 130.0
  data$C3993500 <- factor(data$C3993500,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5162700[11.0 <= data$C5162700 & data$C5162700 <= 9999.0] <- 11.0
  data$C5162700 <- factor(data$C5162700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C5530200[0.0 <= data$C5530200 & data$C5530200 <= 69.0] <- 0.0
  data$C5530200[70.0 <= data$C5530200 & data$C5530200 <= 84.0] <- 70.0
  data$C5530200[85.0 <= data$C5530200 & data$C5530200 <= 99.0] <- 85.0
  data$C5530200[100.0 <= data$C5530200 & data$C5530200 <= 114.0] <- 100.0
  data$C5530200[115.0 <= data$C5530200 & data$C5530200 <= 129.0] <- 115.0
  data$C5530200[130.0 <= data$C5530200 & data$C5530200 <= 999999.0] <- 130.0
  data$C5530200 <- factor(data$C5530200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5536500[1.0 <= data$C5536500 & data$C5536500 <= 99.0] <- 1.0
  data$C5536500[100.0 <= data$C5536500 & data$C5536500 <= 199.0] <- 100.0
  data$C5536500[200.0 <= data$C5536500 & data$C5536500 <= 299.0] <- 200.0
  data$C5536500[300.0 <= data$C5536500 & data$C5536500 <= 399.0] <- 300.0
  data$C5536500[400.0 <= data$C5536500 & data$C5536500 <= 499.0] <- 400.0
  data$C5536500[500.0 <= data$C5536500 & data$C5536500 <= 599.0] <- 500.0
  data$C5536500[600.0 <= data$C5536500 & data$C5536500 <= 699.0] <- 600.0
  data$C5536500[700.0 <= data$C5536500 & data$C5536500 <= 799.0] <- 700.0
  data$C5536500[800.0 <= data$C5536500 & data$C5536500 <= 899.0] <- 800.0
  data$C5536500[900.0 <= data$C5536500 & data$C5536500 <= 999.0] <- 900.0
  data$C5536500[1000.0 <= data$C5536500 & data$C5536500 <= 9.9999999E7] <- 1000.0
  data$C5536500 <- factor(data$C5536500,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C5537500[0.0 <= data$C5537500 & data$C5537500 <= 69.0] <- 0.0
  data$C5537500[70.0 <= data$C5537500 & data$C5537500 <= 84.0] <- 70.0
  data$C5537500[85.0 <= data$C5537500 & data$C5537500 <= 99.0] <- 85.0
  data$C5537500[100.0 <= data$C5537500 & data$C5537500 <= 114.0] <- 100.0
  data$C5537500[115.0 <= data$C5537500 & data$C5537500 <= 129.0] <- 115.0
  data$C5537500[130.0 <= data$C5537500 & data$C5537500 <= 999999.0] <- 130.0
  data$C5537500 <- factor(data$C5537500,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5738900[11.0 <= data$C5738900 & data$C5738900 <= 9999.0] <- 11.0
  data$C5738900 <- factor(data$C5738900,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C5806400[0.0 <= data$C5806400 & data$C5806400 <= 69.0] <- 0.0
  data$C5806400[70.0 <= data$C5806400 & data$C5806400 <= 84.0] <- 70.0
  data$C5806400[85.0 <= data$C5806400 & data$C5806400 <= 99.0] <- 85.0
  data$C5806400[100.0 <= data$C5806400 & data$C5806400 <= 114.0] <- 100.0
  data$C5806400[115.0 <= data$C5806400 & data$C5806400 <= 129.0] <- 115.0
  data$C5806400[130.0 <= data$C5806400 & data$C5806400 <= 999999.0] <- 130.0
  data$C5806400 <- factor(data$C5806400,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C5812300[1.0 <= data$C5812300 & data$C5812300 <= 99.0] <- 1.0
  data$C5812300[100.0 <= data$C5812300 & data$C5812300 <= 199.0] <- 100.0
  data$C5812300[200.0 <= data$C5812300 & data$C5812300 <= 299.0] <- 200.0
  data$C5812300[300.0 <= data$C5812300 & data$C5812300 <= 399.0] <- 300.0
  data$C5812300[400.0 <= data$C5812300 & data$C5812300 <= 499.0] <- 400.0
  data$C5812300[500.0 <= data$C5812300 & data$C5812300 <= 599.0] <- 500.0
  data$C5812300[600.0 <= data$C5812300 & data$C5812300 <= 699.0] <- 600.0
  data$C5812300[700.0 <= data$C5812300 & data$C5812300 <= 799.0] <- 700.0
  data$C5812300[800.0 <= data$C5812300 & data$C5812300 <= 899.0] <- 800.0
  data$C5812300[900.0 <= data$C5812300 & data$C5812300 <= 999.0] <- 900.0
  data$C5812300[1000.0 <= data$C5812300 & data$C5812300 <= 9.9999999E7] <- 1000.0
  data$C5812300 <- factor(data$C5812300,
                          levels=c(0.0,1.0,100.0,200.0,300.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0),
                          labels=c("0",
                                   "1 TO 99",
                                   "100 TO 199",
                                   "200 TO 299",
                                   "300 TO 399",
                                   "400 TO 499",
                                   "500 TO 599",
                                   "600 TO 699",
                                   "700 TO 799",
                                   "800 TO 899",
                                   "900 TO 999",
                                   "1000 TO 99999999: 1000+"))
  data$C5813300[0.0 <= data$C5813300 & data$C5813300 <= 69.0] <- 0.0
  data$C5813300[70.0 <= data$C5813300 & data$C5813300 <= 84.0] <- 70.0
  data$C5813300[85.0 <= data$C5813300 & data$C5813300 <= 99.0] <- 85.0
  data$C5813300[100.0 <= data$C5813300 & data$C5813300 <= 114.0] <- 100.0
  data$C5813300[115.0 <= data$C5813300 & data$C5813300 <= 129.0] <- 115.0
  data$C5813300[130.0 <= data$C5813300 & data$C5813300 <= 999999.0] <- 130.0
  data$C5813300 <- factor(data$C5813300,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C6011200[11.0 <= data$C6011200 & data$C6011200 <= 9999.0] <- 11.0
  data$C6011200 <- factor(data$C6011200,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C6052200[0.0 <= data$C6052200 & data$C6052200 <= 69.0] <- 0.0
  data$C6052200[70.0 <= data$C6052200 & data$C6052200 <= 84.0] <- 70.0
  data$C6052200[85.0 <= data$C6052200 & data$C6052200 <= 99.0] <- 85.0
  data$C6052200[100.0 <= data$C6052200 & data$C6052200 <= 114.0] <- 100.0
  data$C6052200[115.0 <= data$C6052200 & data$C6052200 <= 129.0] <- 115.0
  data$C6052200[130.0 <= data$C6052200 & data$C6052200 <= 999999.0] <- 130.0
  data$C6052200 <- factor(data$C6052200,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C6056900[0.0 <= data$C6056900 & data$C6056900 <= 69.0] <- 0.0
  data$C6056900[70.0 <= data$C6056900 & data$C6056900 <= 84.0] <- 70.0
  data$C6056900[85.0 <= data$C6056900 & data$C6056900 <= 99.0] <- 85.0
  data$C6056900[100.0 <= data$C6056900 & data$C6056900 <= 114.0] <- 100.0
  data$C6056900[115.0 <= data$C6056900 & data$C6056900 <= 129.0] <- 115.0
  data$C6056900[130.0 <= data$C6056900 & data$C6056900 <= 999999.0] <- 130.0
  data$C6056900 <- factor(data$C6056900,
                          levels=c(0.0,70.0,85.0,100.0,115.0,130.0),
                          labels=c("0 TO 69",
                                   "70 TO 84",
                                   "85 TO 99",
                                   "100 TO 114",
                                   "115 TO 129",
                                   "130 TO 999999: 130 +"))
  data$C6078700[11.0 <= data$C6078700 & data$C6078700 <= 9999.0] <- 11.0
  data$C6078700 <- factor(data$C6078700,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  data$C6130800[11.0 <= data$C6130800 & data$C6130800 <= 9999.0] <- 11.0
  data$C6130800 <- factor(data$C6130800,
                          levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
                          labels=c("0",
                                   "1",
                                   "2",
                                   "3",
                                   "4",
                                   "5",
                                   "6",
                                   "7",
                                   "8",
                                   "9",
                                   "10",
                                   "11 TO 9999"))
  return(data)
}

varlabels <- c("ID CODE OF CHILD",
               "ID CODE OF MOTHER OF CHILD",
               "RACE OF CHILD (FROM MOTHERS SCREENER 79)",
               "SEX OF CHILD",
               "DATE OF BIRTH OF CHILD - YEAR",
               "CONDITION LIMITS CHILD TO ATTEND SCHOOL",
               "CONDITION LIMITS ABILITY TO DO SCHL WORK",
               "CONDITION LIMITS ABILITY TO PLAY SPORTS",
               "CONDITION REQUIRES ATTENTION FROM DOCTOR",
               "CONDITION REQUIRES USE OF MEDICINE,DRUGS",
               "CONDITION REQUIRES USE OF SPECIAL EQUIP.",
               "NUMBER OF ILLNESSESS IN PAST 12 MONTHS",
               "CHILD'S HEALTH COVERD BY HEALTH INSURNCE",
               "CHILD'S HEALTH NOW COVERED BY MEDICAID",
               "SEEN PSYCHIATRIST FOR AN EMOTIONAL PROB.",
               "MOTHER FEEL CHILD NEED HELP FOR BEHAVIOR",
               "HOME: TOTAL STANDARD SCORE",
               "MOTOR & SOCIAL: STD SCORE 90",
               "BPI: STD SCORE-ALL 90",
               "CH HAS CONDIT THAT LIMITS SCHL ATT-92",
               "CH HAS CONDIT THAT LIMITS SCHL WORK-92",
               "CH HAS CONDIT THAT LIMITS PHYS ACTV-92",
               "CH HAS CONDIT THAT REQIRES ATTN FR DR-92",
               "CH HAS CONDIT THAT REQS MEDICN/DRUGS-92",
               "CH HAS CONDIT REQS USE OF SPEC EQUIP-92",
               "NO. OF ILLNESSESS IN PAST 12 MONTHS-92",
               "CHLD'S HLTH COVERD BY HLTH INSURNCE?-92",
               "CHILD'S HLTH NOW COVERED BY MEDICAID-92",
               "SEEN PSYCHIATRIST FOR AN EMOT PROB?-92",
               "MTHR FEEL CHILD NEED HELP FOR BEHAV-92",
               "MOM'S RATING OF CHILD'S HEALTH-92",
               "HOME: TOTAL STANDARD SCORE",
               "MOTOR & SOC DEV: STANDARD SCORE - 92",
               "BPI: TOTAL STANDARD SCORE - ALL - 92",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 94",
               "CH HAS CONDIT THAT LIMITS SCHL WORK 94",
               "CH HAS CONDIT THAT LIMITS PHYS ACTV 94",
               "CH HAS CONDIT THAT REQIRES ATTN FR DR 94",
               "CH HAS CONDIT THAT REQS MEDICN/DRUGS 94",
               "CH HAS CONDIT REQS USE OF SPEC EQUIP 94",
               "NO. OF ILLNESSESS IN PAST 12 MONTHS 94",
               "CHLD*S HLTH COVERD BY HLTH INSURNCE 94",
               "CHILD*S HLTH NOW COVERED BY MEDICAID 94",
               "SEEN PSYCHIATRIST FOR AN EMOT PROB 94",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 94",
               "MOM RATING OF CHILD HEALTH 94",
               "HOME: TOTAL STANDARD SCORE",
               "MOTOR & SOC DEV: STANDARD SCORE - 94",
               "BPI: TOTAL STANDARD SCORE - ALL - 94",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 96",
               "CH HAS CONDIT THAT LIMITS SCHL WORK 96",
               "CH HAS CONDIT THAT LIMITS PHYS ACTV 96",
               "CH HAS CONDIT THAT REQIRES ATTN FR DR 96",
               "CH HAS CONDIT THAT REQS MEDICN/DRUGS 96",
               "CH HAS CONDIT REQS USE OF SPEC EQUIP 96",
               "NO. OF ILLNESSESS IN PAST 12 MONTHS 96",
               "CHILD HEALTH COVERD BY INSURANCE 96",
               "CHILD HEALTH COVERED BY MEDICAID 96",
               "SEEN PSYCHIATRIST FOR AN EMOT PROBLEM 96",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 96",
               "HOME: TOTAL STANDARD SCORE",
               "MOTOR & SOC DEV: STANDARD SCORE 96",
               "BPI: TOTAL STANDARD SCORE - ALL 96",
               "MOM RATING OF CHILD HEALTH 96",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 1998",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 1998",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 1998",
               "CH HAS CONDIT THAT REQS MED  ATTN 1998",
               "CH HAS CONDIT THAT REQS MEDICATION 1998",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 1998",
               "# OF ILLNESSESS IN PAST 12 MONTHS 1998",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 1998",
               "CHILD HEALTH COVERED BY MEDICAID 1998",
               "SEEN PSYCHIATRIST FOR AN EMOT PROB 1998",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 1998",
               "HOME: TOTAL STANDARD SCORE 1998",
               "MOTOR & SOC DEV: STANDARD SCORE 1998",
               "BPI: TOTAL STANDARD SCORE - ALL 1998",
               "MOM RATING OF CHILD HEALTH 1998",
               "MOM RATING OF CHILD HEALTH 2000",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2000",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2000",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2000",
               "CH HAS CONDIT THAT REQS MED  ATTN 2000",
               "CH HAS CONDIT THAT REQS MEDICATION 2000",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2000",
               "# OF ILLNESSESS IN PAST 12 MONTHS 2000",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2000",
               "CHILD HEALTH COVERED BY MEDICAID 2000",
               "SEEN PSYCHIATRIST FOR AN EMOT PROB 2000",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2000",
               "BPI: TOTAL STANDARD SCORE - ALL 2000",
               "HOME: TOTAL STANDARD SCORE 2000",
               "MOTOR & SOC DEV: STANDARD SCORE 2000",
               "BPI: TOTAL STANDARD SCORE - ALL 2002",
               "HOME: TOTAL STANDARD SCORE 2002",
               "MOTOR & SOC DEV: STANDARD SCORE 2002",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2002",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2002",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2002",
               "CH HAS CONDIT THAT REQS MED  ATTN 2002",
               "CH HAS CONDIT THAT REQS MEDICATION 2002",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2002",
               "# OF ILLNESSES IN PAST 12 MONTHS 2002",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2002",
               "CHILD HEALTH COVERED BY MEDICAID 2002",
               "MOM RATING OF CHILD HEALTH 2002",
               "SEEN PSYCHIATRIST FOR AN EMOT PROB 2002",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2002",
               "BPI: TOTAL STANDARD SCORE - ALL 2004",
               "HOME: TOTAL STANDARD SCORE 2004",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2004",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2004",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2004",
               "CH HAS CONDIT THAT REQS MED ATTN 2004",
               "CH HAS CONDIT THAT REQS MEDICATION 2004",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2004",
               "# OF ILLNESSES IN PAST 12 MONTHS 2004",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2004",
               "CHILD HEALTH COVERED BY MEDICAID 2004",
               "MOM RATING OF CHILD HEALTH 2004",
               "SEEN PSYCHIATRIST FOR AN EMOT PROB 2004",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2004",
               "BPI: TOTAL STANDARD SCORE - ALL 2006",
               "HOME: TOTAL STANDARD SCORE 2006",
               "MOTOR & SOC DEV: STANDARD SCORE 2006",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2006",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2006",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2006",
               "CH HAS CONDIT THAT REQS MED  ATTN 2006",
               "CH HAS CONDIT THAT REQS MEDICATION 2006",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2006",
               "# OF ILLNESSES IN PAST 12 MONTHS 2006",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2006",
               "CHILD HEALTH COVERED BY MEDICAID 2006",
               "MOM RATING OF CHILD HEALTH 2006",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2006",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2006",
               "BPI: TOTAL STANDARD SCORE - ALL 2008",
               "HOME: TOTAL STANDARD SCORE 2008",
               "MOTOR & SOC DEV: STANDARD SCORE 2008",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2008",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2008",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2008",
               "CH HAS CONDIT THAT REQS MED  ATTN 2008",
               "CH HAS CONDIT THAT REQS MEDICATION 2008",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2008",
               "# OF ILLNESSES IN PAST 12 MONTHS 2008",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2008",
               "CHILD HEALTH COVERED BY MEDICAID 2008",
               "MOM RATING OF CHILD HEALTH 2008",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2008",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2008",
               "BPI: TOTAL STANDARD SCORE - ALL 2010",
               "HOME: TOTAL STANDARD SCORE 2010",
               "MOTOR & SOC DEV: STANDARD SCORE-ALL 2010",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2010",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2010",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2010",
               "CH HAS CONDIT THAT REQS MED  ATTN 2010",
               "CH HAS CONDIT THAT REQS MEDICATION 2010",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2010",
               "# OF ILLNESSES IN PAST 12 MONTHS 2010",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2010",
               "CHILD HEALTH COVERED BY MEDICAID 2010",
               "MOM RATING OF CHILD HEALTH 2010",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2010",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2010",
               "BPI: TOTAL STANDARD SCORE - ALL 2012",
               "HOME: TOTAL STANDARD SCORE 2012",
               "MOTOR & SOC DEV: STANDARD SCORE-ALL 2012",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2012",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2012",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2012",
               "CH HAS CONDIT THAT REQS MED  ATTN 2012",
               "CH HAS CONDIT THAT REQS MEDICATION 2012",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2012",
               "# OF ILLNESSES IN PAST 12 MONTHS 2012",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2012",
               "CHILD HEALTH COVERED BY MEDICAID 2012",
               "MOM RATING OF CHILD HEALTH 2012",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2012",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2012",
               "BPI: TOTAL STANDARD SCORE - ALL 2014",
               "HOME: TOTAL STANDARD SCORE 2014",
               "MOTOR & SOC DEV: STANDARD SCORE-ALL 2014",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2014",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2014",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2014",
               "CH HAS CONDIT THAT REQS MED  ATTN 2014",
               "CH HAS CONDIT THAT REQS MEDICATION 2014",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2014",
               "# OF ILLNESSES IN PAST 12 MONTHS 2014",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2014",
               "CHILD HEALTH COVERED BY MEDICAID 2014",
               "MOM RATING OF CHILD HEALTH 2014",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2014",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2014",
               "BPI: TOTAL STANDARD SCORE - ALL 2016",
               "MOTOR & SOC DEV: STANDARD SCORE-ALL 2016",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2016",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2016",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2016",
               "CH HAS CONDIT THAT REQS MED  ATTN 2016",
               "CH HAS CONDIT THAT REQS MEDICATION 2016",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2016",
               "# OF ILLNESSES IN PAST 12 MONTHS 2016",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2016",
               "CHILD HEALTH COVERED BY MEDICAID 2016",
               "MOM RATING OF CHILD HEALTH 2016",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2016",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2016",
               "CH HAS CONDIT THAT LIMITS SCHL ATT 2018",
               "CH HAS CONDIT THAT LIMITS SCHL WRK 2018",
               "CH HAS CONDIT THAT LIMITS PHYS ACT 2018",
               "CH HAS CONDIT THAT REQS MED  ATTN 2018",
               "CH HAS CONDIT THAT REQS MEDICATION 2018",
               "CH HAS CONDIT REQS USE OF EQUIPMNT 2018",
               "# OF ILLNESSES IN PAST 12 MONTHS 2018",
               "CHILD HEALTH COVRD BY HLTH INSURNCE 2018",
               "CHILD HEALTH COVERED BY MEDICAID 2018",
               "MOM RATING OF CHILD HEALTH 2018",
               "SEEN PSYCHIATRIST FOR  EMOTIONAL PROBLEM 2018",
               "MOM FEEL CHILD NEED HELP FOR BEHAV 2018",
               "VERSION_R29 CHILD/YOUNG ADULT XRND"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("CPUBID_XRND",
                   "MPUBID_XRND",
                   "CRACE_XRND",
                   "CSEX_XRND",
                   "CYRB_XRND",
                   "CS901631_1990",
                   "CS901633_1990",
                   "CS901635_1990",
                   "CS901637_1990",
                   "CS901639_1990",
                   "CS901641_1990",
                   "CS902033_1990",
                   "CS902055_1990",
                   "CS902057_1990",
                   "CS902111_1990",
                   "CS902137_1990",
                   "HOMEZ1990_1990",
                   "MOTOZ1990_1990",
                   "BPIZ1990_1990",
                   "CS921758_1992",
                   "CS921760_1992",
                   "CS921762_1992",
                   "CS921764_1992",
                   "CS921766_1992",
                   "CS921768_1992",
                   "CS922133_1992",
                   "CS922155_1992",
                   "CS922157_1992",
                   "CS922211_1992",
                   "CS922237_1992",
                   "MS921515_1992",
                   "HOMEZ1992_1992",
                   "MOTOZ1992_1992",
                   "BPIZ1992_1992",
                   "CS94H-2A_1994",
                   "CS94H-2B_1994",
                   "CS94H-2C_1994",
                   "CS94H-3A_1994",
                   "CS94H-3B_1994",
                   "CS94H-3C_1994",
                   "CS94H-8A_1994",
                   "CS94H-13_1994",
                   "CS94H-14_1994",
                   "CS94H-16_1994",
                   "CS94H-17_1994",
                   "MS941711_1994",
                   "HOMEZ1994_1994",
                   "MOTOZ1994_1994",
                   "BPIZ1994_1994",
                   "CS96H-2A_1996",
                   "CS96H-2B_1996",
                   "CS96H-2C_1996",
                   "CS96H-3A_1996",
                   "CS96H-3B_1996",
                   "CS96H-3C_1996",
                   "CS96H-8A_1996",
                   "CS96H-13_1996",
                   "CS96H-14_1996",
                   "CS96H-16_1996",
                   "CS96H-17_1996",
                   "HOMEZ1996_1996",
                   "MOTOZ1996_1996",
                   "BPIZ1996_1996",
                   "MS961711_1996",
                   "CS98H-2A_1998",
                   "CS98H-2B_1998",
                   "CS98H-2C_1998",
                   "CS98H-3A_1998",
                   "CS98H-3B_1998",
                   "CS98H-3C_1998",
                   "CS98H-8A_1998",
                   "CS98H-13_1998",
                   "CS98H-14_1998",
                   "CS98H-16_1998",
                   "CS98H-17_1998",
                   "HOMEZ1998_1998",
                   "MOTOZ1998_1998",
                   "BPIZ1998_1998",
                   "MS985019A_1998",
                   "BKGN-46A_2000",
                   "HLTH-2A_2000",
                   "HLTH-2B_2000",
                   "HLTH-2C_2000",
                   "HLTH-3A_2000",
                   "HLTH-3B_2000",
                   "HLTH-3C_2000",
                   "HLTH-8A_2000",
                   "HLTH-14_2000",
                   "HLTH-15_2000",
                   "HLTH-17_2000",
                   "HLTH-19_2000",
                   "BPIZ2000_2000",
                   "HOMEZ2000_2000",
                   "MOTOZ2000_2000",
                   "BPIZ2002_2002",
                   "HOMEZ2002_2002",
                   "MOTOZ2002_2002",
                   "HLTH-2A_2002",
                   "HLTH-2B_2002",
                   "HLTH-2C_2002",
                   "HLTH-3A_2002",
                   "HLTH-3B_2002",
                   "HLTH-3C_2002",
                   "HLTH-8A_2002",
                   "HLTH-14_2002",
                   "HLTH-15_2002",
                   "BKGN-46A_2002",
                   "HLTH-17_2002",
                   "HLTH-19_2002",
                   "BPIZ2004_2004",
                   "HOMEZ2004_2004",
                   "HLTH-2A_2004",
                   "HLTH-2B_2004",
                   "HLTH-2C_2004",
                   "HLTH-3A_2004",
                   "HLTH-3B_2004",
                   "HLTH-3C_2004",
                   "HLTH-8A_2004",
                   "HLTH-14_2004",
                   "HLTH-15_2004",
                   "BKGN-46A_2004",
                   "HLTH-17_2004",
                   "HLTH-19_2004",
                   "BPIZ2006_2006",
                   "HOMEZ2006_2006",
                   "MOTOZ2006_2006",
                   "MS-HLTH-2A_2006",
                   "MS-HLTH-2B_2006",
                   "MS-HLTH-2C_2006",
                   "MS-HLTH-3A_2006",
                   "MS-HLTH-3B_2006",
                   "MS-HLTH-3C_2006",
                   "MS-HLTH-8A_2006",
                   "MS-HLTH-14_2006",
                   "MS-HLTH-15_2006",
                   "MS-BKGN-46A_2006",
                   "MS-HLTH-17_2006",
                   "MS-HLTH-19_2006",
                   "BPIZ2008_2008",
                   "HOMEZ2008_2008",
                   "MOTOZ2008_2008",
                   "MS-HLTH-2A_2008",
                   "MS-HLTH-2B_2008",
                   "MS-HLTH-2C_2008",
                   "MS-HLTH-3A_2008",
                   "MS-HLTH-3B_2008",
                   "MS-HLTH-3C_2008",
                   "MS-HLTH-8A_2008",
                   "MS-HLTH-14_2008",
                   "MS-HLTH-15_2008",
                   "MS-BKGN-46A_2008",
                   "MS-HLTH-17_2008",
                   "MS-HLTH-19_2008",
                   "BPIZ2010_2010",
                   "HOMEZ2010_2010",
                   "MOTOZ2010_2010",
                   "MS-HLTH-2A_2010",
                   "MS-HLTH-2B_2010",
                   "MS-HLTH-2C_2010",
                   "MS-HLTH-3A_2010",
                   "MS-HLTH-3B_2010",
                   "MS-HLTH-3C_2010",
                   "MS-HLTH-8A_2010",
                   "MS-HLTH-14_2010",
                   "MS-HLTH-15_2010",
                   "MS-BKGN-46A_2010",
                   "MS-HLTH-17_2010",
                   "MS-HLTH-19_2010",
                   "BPIZ2012_2012",
                   "HOMEZ2012_2012",
                   "MOTOZ2012_2012",
                   "MS-HLTH-2A_2012",
                   "MS-HLTH-2B_2012",
                   "MS-HLTH-2C_2012",
                   "MS-HLTH-3A_2012",
                   "MS-HLTH-3B_2012",
                   "MS-HLTH-3C_2012",
                   "MS-HLTH-8A_2012",
                   "MS-HLTH-14_2012",
                   "MS-HLTH-15_2012",
                   "MS-BKGN-46A_2012",
                   "MS-HLTH-17_2012",
                   "MS-HLTH-19_2012",
                   "BPIZ2014_2014",
                   "HOMEZ2014_2014",
                   "MOTOZ2014_2014",
                   "MS-HLTH-2A_2014",
                   "MS-HLTH-2B_2014",
                   "MS-HLTH-2C_2014",
                   "MS-HLTH-3A_2014",
                   "MS-HLTH-3B_2014",
                   "MS-HLTH-3C_2014",
                   "MS-HLTH-8A_2014",
                   "MS-HLTH-14_2014",
                   "MS-HLTH-15_2014",
                   "MS-BKGN-46A_2014",
                   "MS-HLTH-17_2014",
                   "MS-HLTH-19_2014",
                   "BPIZ2016_2016",
                   "MOTOZ2016_2016",
                   "MS-HLTH-2A_2016",
                   "MS-HLTH-2B_2016",
                   "MS-HLTH-2C_2016",
                   "MS-HLTH-3A_2016",
                   "MS-HLTH-3B_2016",
                   "MS-HLTH-3C_2016",
                   "MS-HLTH-8A_2016",
                   "MS-HLTH-14_2016",
                   "MS-HLTH-15_2016",
                   "MS-BKGN-46A_2016",
                   "MS-HLTH-17_2016",
                   "MS-HLTH-19_2016",
                   "MS-HLTH-2A_2018",
                   "MS-HLTH-2B_2018",
                   "MS-HLTH-2C_2018",
                   "MS-HLTH-3A_2018",
                   "MS-HLTH-3B_2018",
                   "MS-HLTH-3C_2018",
                   "MS-HLTH-8A_2018",
                   "MS-HLTH-14_2018",
                   "MS-HLTH-15_2018",
                   "MS-BKGN-46A_2018",
                   "MS-HLTH-17_2018",
                   "MS-HLTH-19_2018",
                   "VERSION_R29_XRND")
  return(data)
}





# Assign to data
categories <- vallabels(new_data)
data <- qnames(new_data)


