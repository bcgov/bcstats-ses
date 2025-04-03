library(dplyr)
library(lubridate)

# Define parameters
fyr1 <- 2022
fyr2 <- 2023

# Assuming you have the dataframes ahip.cdr_merged_long_cur, ahip.sn_dtl_ft_clntrstfyr_vwp_hist,
# ahip.cb_dtl_dm_clnt_age_vw, and ahip.geog_chsa loaded into R as cdr_merged, sn_dtl, age_vw, and geog_chsa

# --- Numerator ---

# Step 1: Create 'c' dataframe
c <- cdr_merged %>%
  inner_join(sn_dtl, by = "clnt_label") %>%
  filter(
    sn_id == '202301',
    bc_res_flg == 'Y',
    case_date >= msp_start_dt,
    case_date < as.Date(paste0(fyr2, '-04-01'))
  )

# Step 2: Create 'cdr' dataframe
cdr <- c %>%
  mutate(
    alz = ifelse(
      disease == 'ALZHEIMER_DEMENTIA' &
        case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ckd = ifelse(
      disease == 'CKD_GBD' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    copd = ifelse(
      disease == 'COPD' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    diab = ifelse(
      disease == 'DIABETES' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    epi = ifelse(
      disease == 'EPILEPSY' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    hf = ifelse(
      disease == 'HEART_FAILURE' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    hyper = ifelse(
      disease == 'HYPERTENSION' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ihd = ifelse(
      disease == 'IHD' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ja = ifelse(
      disease == 'JUVENILE_ARTHRITIS' &
        case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ms = ifelse(
      disease == 'MS' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ostar = ifelse(
      disease == 'OSTEOARTHRITIS' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ostpo = ifelse(
      disease == 'OSTEOPOROSIS' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    park = ifelse(
      disease == 'PARKINSONISM' & case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    ra = ifelse(
      disease == 'RHEUMATOID_ARTHRITIS' &
        case_date < as.Date(paste0(fyr2, '-04-01')),
      1,
      0
    ),
    gout = ifelse(
      disease == 'GOUT' &
        ((record_type == 'INCIDENCE' &
          case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
          case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d")) |
          (record_type == 'ACTIVE_HEALTHCARE_CONTACT' &
            case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
            case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d"))),
      1,
      0
    ),
    sdd = ifelse(
      disease == 'SCHIZOPHRENIA' &
        ((record_type == 'INCIDENCE' &
          case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
          case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d")) |
          (record_type == 'ACTIVE_HEALTHCARE_CONTACT' &
            case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
            case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d"))),
      1,
      0
    ),
    stroke = ifelse(
      disease == 'HOSP_STROKE' &
        ((record_type == 'INCIDENCE' &
          case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
          case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d")) |
          (record_type == 'ACTIVE_HEALTHCARE_CONTACT' &
            case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
            case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d"))),
      1,
      0
    ),
    sud = ifelse(
      disease == 'SUD' &
        ((record_type == 'INCIDENCE' &
          case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
          case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d")) |
          (record_type == 'ACTIVE_HEALTHCARE_CONTACT' &
            case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
            case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d"))),
      1,
      0
    ),
    amd = ifelse(
      disease == 'MOOD_ANX' &
        ((record_type == 'INCIDENCE' &
          case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
          case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d")) |
          (record_type == 'ACTIVE_HEALTHCARE_CONTACT' &
            case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
            case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d"))),
      1,
      0
    ),
    ast = ifelse(
      disease == 'ASTHMA' &
        ((record_type == 'INCIDENCE' &
          case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
          case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d")) |
          (record_type == 'ACTIVE_HEALTHCARE_CONTACT' &
            case_date >= as.Date(paste0(fyr1, '0401'), format = "%Y%m%d") &
            case_date <= as.Date(paste0(fyr2, '0331'), format = "%Y%m%d"))),
      1,
      0
    )
  ) %>%
  group_by(clnt_label) %>%
  summarise(
    alz = max(alz),
    ckd = max(ckd),
    copd = max(copd),
    diab = max(diab),
    epi = max(epi),
    hf = max(hf),
    hyper = max(hyper),
    ihd = max(ihd),
    ja = max(ja),
    ms = max(ms),
    ostar = max(ostar),
    ostpo = max(ostpo),
    park = max(park),
    ra = max(ra),
    gout = max(gout),
    sdd = max(sdd),
    stroke = max(stroke),
    sud = max(sud),
    amd = max(amd),
    ast = max(ast)
  ) %>%
  mutate(
    cdr_1 = ifelse(
      alz +
        amd +
        ast +
        ckd +
        copd +
        diab +
        epi +
        gout +
        hf +
        hyper +
        ihd +
        ja +
        ms +
        ostar +
        ostpo +
        park +
        ra +
        sdd +
        stroke +
        sud >=
        1,
      1,
      0
    ),
    cdr_2 = ifelse(
      alz +
        amd +
        ast +
        ckd +
        copd +
        diab +
        epi +
        gout +
        hf +
        hyper +
        ihd +
        ja +
        ms +
        ostar +
        ostpo +
        park +
        ra +
        sdd +
        stroke +
        sud >=
        2,
      1,
      0
    ),
    cdr_3 = ifelse(
      alz +
        amd +
        ast +
        ckd +
        copd +
        diab +
        epi +
        gout +
        hf +
        hyper +
        ihd +
        ja +
        ms +
        ostar +
        ostpo +
        park +
        ra +
        sdd +
        stroke +
        sud >=
        3,
      1,
      0
    ),
    cdr_5 = ifelse(
      alz +
        amd +
        ast +
        ckd +
        copd +
        diab +
        epi +
        gout +
        hf +
        hyper +
        ihd +
        ja +
        ms +
        ostar +
        ostpo +
        park +
        ra +
        sdd +
        stroke +
        sud >=
        5,
      1,
      0
    )
  )

# Step 3: Create 'a1' dataframe
a1 <- cdr %>%
  left_join(
    sn_dtl %>%
      filter(
        fisc_yr_label == paste0(fyr1, '.', fyr2),
        bc_res_flg == 'Y',
        sn_id == '202301'
      ),
    by = "clnt_label"
  ) %>%
  left_join(
    age_vw,
    by = c(
      "clnt_age_end_tydf_code" = "tydf_code",
      "clnt_age_end_label" = "label"
    )
  ) %>%
  left_join(geog_chsa, by = c("chsa" = "chsa_title")) %>%
  filter(clnt_age_in_yrs_num >= 1) %>%
  select(clnt_label, cdr_1, cdr_2, cdr_3, cdr_5, fiscal_year = fisc_yr_label)

# Step 4: Create 'a2' dataframe
a2 <- cdr %>%
  left_join(
    sn_dtl %>%
      filter(
        fisc_yr_label == paste0(fyr1, '.', fyr2),
        bc_res_flg == 'Y',
        sn_id == '202301'
      ),
    by = "clnt_label"
  ) %>%
  left_join(
    age_vw,
    by = c(
      "clnt_age_end_tydf_code" = "tydf_code",
      "clnt_age_end_label" = "label"
    )
  ) %>%
  left_join(geog_chsa, by = c("chsa" = "chsa_title")) %>%
  filter(clnt_age_in_yrs_num >= 20) %>%
  select(clnt_label, cdr_2, cdr_3, cdr_5, fiscal_year = fisc_yr_label)

# Step 5: Create 'b' dataframe
b <- a1 %>%
  left_join(
    a2,
    by = c("clnt_label", "fiscal_year"),
    suffix = c(".a1", ".a2")
  ) %>%
  mutate(
    cdr_2 = coalesce(cdr_2.a2, 0),
    cdr_3 = coalesce(cdr_3.a2, 0),
    cdr_5 = coalesce(cdr_5.a2, 0)
  ) %>%
  select(fiscal_year, clnt_label, cdr_1, cdr_2, cdr_3, cdr_5)

# Step 6: Create 'bc' dataframe
bc <- b %>%
  group_by(fiscal_year) %>%
  summarise(
    geo_level = 'BC',
    health_boundaries = 'BC',
    cdr_1 = sum(cdr_1),
    cdr_2 = sum(cdr_2),
    cdr_3 = sum(cdr_3),
    cdr_5 = sum(cdr_5)
  ) %>%
  arrange(fiscal_year)

# --- Denominator ---

# Denominator for age 1+
denominator_1plus <- sn_dtl %>%
  left_join(
    age_vw,
    by = c(
      "clnt_age_mid_tydf_code" = "tydf_code",
      "clnt_age_mid_label" = "label"
    )
  ) %>%
  filter(
    fisc_yr_label == paste0(fyr1, '.', fyr2),
    sn_id == '202301',
    bc_res_flg == 'Y',
    msp_start_dt <=
      as.Date(paste0(substr(fisc_yr_label, 1, 4), '1001'), format = "%Y%m%d"),
    msp_end_dt >=
      as.Date(paste0(substr(fisc_yr_label, 1, 4), '1001'), format = "%Y%m%d"),
    clnt_age_in_yrs_num_mid_prd >= 1 | is.na(clnt_age_in_yrs_num_mid_prd)
  ) %>%
  group_by(fisc_yr_label) %>%
  summarise(denominator = n())

# Denominator for age 20+
denominator_20plus <- sn_dtl %>%
  left_join(
    age_vw,
    by = c(
      "clnt_age_mid_tydf_code" = "tydf_code",
      "clnt_age_mid_label" = "label"
    )
  ) %>%
  filter(
    fisc_yr_label == paste0(fyr1, '.', fyr2),
    sn_id == '202301',
    bc_res_flg == 'Y',
    msp_start_dt <=
      as.Date(paste0(substr(fisc_yr_label, 1, 4), '1001'), format = "%Y%m%d"),
    msp_end_dt >=
      as.Date(paste0(substr(fisc_yr_label, 1, 4), '1001'), format = "%Y%m%d"),
    clnt_age_in_yrs_num_mid_prd >= 20 | is.na(clnt_age_in_yrs_num_mid_prd)
  ) %>%
  group_by(fisc_yr_label) %>%
  summarise(denominator = n())

# Output results
print(bc)
print(denominator_1plus)
print(denominator_20plus)


###################################################################################################
library(dplyr)
library(lubridate)

# Assuming 'pharmanet' is your data frame

result <- pharmanet %>%
  filter(
    TOT_PCARE_PD_AMT > 0,
    srv_date >= ymd("2018-04-01"),
    srv_date <= ymd("2023-03-31")
  ) %>%
  mutate(
    FY = case_when(
      srv_date >= ymd("2018-04-01") & srv_date <= ymd("2019-03-31") ~
        "2018/2019",
      srv_date >= ymd("2019-04-01") & srv_date <= ymd("2020-03-31") ~
        "2019/2020",
      srv_date >= ymd("2020-04-01") & srv_date <= ymd("2021-03-31") ~
        "2020/2021",
      srv_date >= ymd("2021-04-01") & srv_date <= ymd("2022-03-31") ~
        "2021/2022",
      srv_date >= ymd("2022-04-01") & srv_date <= ymd("2023-03-31") ~
        "2022/2022",
      TRUE ~ NA_character_ # Handle cases outside the specified ranges
    )
  ) %>%
  group_by(FY) %>%
  summarise(clnts = n_distinct(clnt_key)) %>%
  arrange(FY)

print(result)


library(dplyr)
library(stringr)

# Assuming you have your data loaded into data frames named 'pharmanet' and 'census_geodata_a_dcy'

result <- pharmanet %>%
  mutate(year = as.integer(format(SRV_DATE, "%Y"))) %>%
  inner_join(
    census_geodata_a_dcy,
    by = c(
      "year" = str_extract(
        census_geodata_a_dcy$filename,
        "(?<=\\D)(\\d{4})(?=\\D)"
      ),
      "clnt_key" = "phn"
    )
  ) %>%
  mutate(is_pc = ifelse(tot_pcare_pd_amt == 0, 0, 1)) %>%
  group_by(year, DAUID, is_pc) %>%
  summarise(clnts = n_distinct(clnt_key)) %>%
  ungroup() %>%
  arrange(year, is_pc)

print(result)


library(dplyr)
library(stringr)

# Example data (replace with your actual data)
value_string <- "-4,05000000000000e+00"
df <- data.frame(VALUE = value_string)

# dplyr workflow
df <- df %>%
  mutate(
    VALUE = str_replace(VALUE, ",", "."), # Replace comma with period
    VALUE = as.numeric(VALUE), # Convert to numeric
    VALUE = round(VALUE, 3)
  ) # Round to 3 decimal places

# Check for NA's
if (any(is.na(df$VALUE))) {
  print(
    "NA values were generated during as.numeric conversion. Review original data"
  )
}

# Check the results
print(df)
print(class(df$VALUE))

#Example with a dataframe.
value_vector = c(
  "-4,05000000000000e+00",
  "123.456",
  "5,67e+01",
  "test",
  "9,93334000000000e+03",
  "NA"
)
df2 = data.frame(VALUE = value_vector)

df2 = df2 %>%
  mutate(
    VALUE = str_replace(VALUE, ",", "."),
    VALUE = as.numeric(VALUE),
    VALUE = round(VALUE, 3)
  ) %>%
  filter(!is.na(VALUE))

print(df2)
print(class(df2$VALUE))

#Example of checking the total number of digits.
df = df %>%
  mutate(total_digits = nchar(str_remove_all(as.character(VALUE), "[.-]")))

if (any(df$total_digits > 18)) {
  print("Warning: Number of total digits exceeds 18.")
}

print(df)
