library(data.table)

utah_dat <- fread("data-raw/measles_school_data_final.csv")

utah_dat <- utah_dat[, .(
  state = "UT",
  county = county,
  school_name = name,
  vaccination_rate = as.numeric(vac_rate)/100,
  num_students = 500,
  school_id = sprintf("UT-%05d", .I)
)]

utah_dat <- utah_dat[!is.na(vaccination_rate)]

fwrite(utah_dat, "data-raw/01_utah_school_data.csv", row.names = FALSE)
