credential_supvars_gender <- credential_supvars_enrolment |>
  distinct(
    ENCRYPTED_TRUE_PEN,
    PSI_GENDER
  )

# ------------------------------------------------------------------------------------
# Replicates qry03f Cleaning 1 through 5 in a single optimized pipeline
tmp_CredentialGenderCleaning_Step3 <- credential_supvars_enrolment |>
  group_by(ENCRYPTED_TRUE_PEN) |>
  filter(n_distinct(PSI_GENDER) > 1) |>
  slice_max(
    order_by = tibble(PSI_SCHOOL_YEAR, PSI_ENROLMENT_SEQUENCE),
    n = 1,
    with_ties = FALSE
  ) |>
  filter(
    !is.na(ENCRYPTED_TRUE_PEN),
    !(ENCRYPTED_TRUE_PEN %in% c("", " ", "(Unspecified)"))
  ) |>
  select(
    ENCRYPTED_TRUE_PEN,
    PSI_GENDER_To_Use = PSI_GENDER
  ) |>
  ungroup() |>
  distinct()

# Replicates qry03f Cleaning 6 through 9 in a single optimized pipeline
target_pens <- tmp_CredentialGenderCleaning_Step3 |>
  select(ENCRYPTED_TRUE_PEN) |>
  semi_join(credential_supvars, by = "ENCRYPTED_TRUE_PEN")

CredentialSupVarsFromEnrolment_MultiGender <- stp_enrolment |>
  semi_join(stp_enrolment_valid, by = "ID") |>
  inner_join(target_pens, by = "ENCRYPTED_TRUE_PEN") |>
  select(
    EnrolmentID = ID,
    ENCRYPTED_TRUE_PEN,
    PSI_BIRTHDATE,
    PSI_MIN_START_DATE,
    psi_birthdate_cleaned,
    PSI_VISA_STATUS,
    PSI_STUDENT_POSTAL_CODE_CURRENT,
    PSI_SCHOOL_YEAR,
    PSI_PROGRAM_CODE,
    PSI_CREDENTIAL_PROGRAM_DESCRIPTION,
    PSI_ENROLMENT_SEQUENCE,
    PSI_CIP_CODE,
    PSI_CONTINUING_EDUCATION_COURSE_ONLY,
    PSI_GENDER
  )

# 1. Normalization of the Multi-Gender Enrollment Dataset (qry03f_10 & 11)
CredentialSupVarsFromEnrolment_MultiGender <- CredentialSupVarsFromEnrolment_MultiGender |>
  left_join(
    tmp_CredentialGenderCleaning_Step3 |>
      select(ENCRYPTED_TRUE_PEN, PSI_GENDER_To_Use),
    by = "ENCRYPTED_TRUE_PEN"
  ) |>
  rename(psi_gender_cleaned = PSI_GENDER_To_Use)

# 2. Comprehensive Gender Normalization and Flagging (qry03f_12, 13, & 14)
credential_supvars_gender <- credential_supvars_gender |>
  left_join(
    tmp_CredentialGenderCleaning_Step3 |>
      select(ENCRYPTED_TRUE_PEN, PSI_GENDER_To_Use),
    by = "ENCRYPTED_TRUE_PEN"
  ) |>
  mutate(
    psi_gender_cleaned_flag = if_else(
      !is.na(PSI_GENDER_To_Use),
      "Yes",
      NA_character_
    ),
    psi_gender_cleaned = coalesce(PSI_GENDER_To_Use, PSI_GENDER)
  ) |>
  select(-PSI_GENDER_To_Use)
# ------------------------------------------------------------------------------------

# Unknowns
# ---------------------
# 1. Consolidated Resolution Lookup (Replicates Steps 15, 17, 18, and 19)
gender_recovery_lookup <- credential_supvars_gender |>
  filter(psi_gender_cleaned %in% c("U", "Unknown")) |>
  inner_join(
    credential_supvars_enrolment |>
      filter(!PSI_GENDER %in% c("U", "Unknown")) |>
      select(ENCRYPTED_TRUE_PEN, ResolvedGender = PSI_GENDER),
    by = "ENCRYPTED_TRUE_PEN"
  ) |>
  filter(
    !is.na(ENCRYPTED_TRUE_PEN),
    !(ENCRYPTED_TRUE_PEN %in% c("", " ", "(Unspecified)"))
  ) |>
  group_by(ENCRYPTED_TRUE_PEN) |>
  slice_max(order_by = ResolvedGender, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ENCRYPTED_TRUE_PEN, psi_gender_cleaned_NEW = ResolvedGender)

# 2. Synchronized Data Updates (Replicates Steps 20 and 21)
# Update: Multi-Gender Enrolment Subset
CredentialSupVarsFromEnrolment_MultiGender <- CredentialSupVarsFromEnrolment_MultiGender |>
  left_join(gender_recovery_lookup, by = "ENCRYPTED_TRUE_PEN") |>
  mutate(
    psi_gender_cleaned = coalesce(psi_gender_cleaned_NEW, psi_gender_cleaned)
  ) |>
  select(-psi_gender_cleaned_NEW)

# Update: Master Gender Reference Table
credential_supvars_gender <- credential_supvars_gender |>
  left_join(gender_recovery_lookup, by = "ENCRYPTED_TRUE_PEN") |>
  mutate(
    psi_gender_cleaned = if_else(
      psi_gender_cleaned == 'Unknown',
      psi_gender_cleaned_NEW,
      psi_gender_cleaned
    )
  ) |>
  select(-psi_gender_cleaned_NEW)

credential_supvars <- credential_supvars |>
  left_join(
    credential_supvars_gender |>
      filter(
        !is.na(ENCRYPTED_TRUE_PEN),
        !(ENCRYPTED_TRUE_PEN %in% c("", " ", "(Unspecified)"))
      ) |>
      select(ENCRYPTED_TRUE_PEN, psi_gender_cleaned),
    by = "ENCRYPTED_TRUE_PEN"
  )
credential_supvars <- credential_supvars |> distinct()

# ---------------------------------------------------------------------------------------------------------
# NULLS
# ---------------------------------------------------------------------------------------------------------
tmp_credentialgendercleaning_step5 <- credential_supvars |>
  filter(is.na(psi_gender_cleaned)) |>
  select(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE, psi_gender_cleaned)

tmp_credentialgendercleaning_step6 <- tmp_credentialgendercleaning_step5 |>
  inner_join(
    stp_enrolment |> select(PSI_STUDENT_NUMBER, PSI_CODE, PSI_GENDER),
    by = c("PSI_STUDENT_NUMBER", "PSI_CODE"),
    relationship = "many-to-many"
  ) |>
  distinct()

credentialsupvars_multigenderfornulls <- tmp_credentialgendercleaning_step6 |>
  group_by(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE) |>
  # Count distinct genders per student/institution pairing
  summarise(GenderCount = n_distinct(PSI_GENDER), .groups = "drop") |>
  filter(GenderCount > 1)

tmp_CredentialGenderCleaning_Step7 <- credentialsupvars_multigenderfornulls |>
  select(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE) |>
  inner_join(
    credential_supvars_enrolment |>
      select(
        ENCRYPTED_TRUE_PEN,
        PSI_GENDER,
        PSI_SCHOOL_YEAR,
        PSI_ENROLMENT_SEQUENCE
      ),
    by = "ENCRYPTED_TRUE_PEN"
  ) |>
  group_by(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE, PSI_GENDER) |>
  summarise(
    MAX_PSI_SCHOOL_YEAR = max(PSI_SCHOOL_YEAR, na.rm = TRUE),
    MAX_PSI_ENROLMENT_SEQUENCE = max(PSI_ENROLMENT_SEQUENCE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE) |>
  slice_max(
    order_by = tibble(MAX_PSI_SCHOOL_YEAR, MAX_PSI_ENROLMENT_SEQUENCE),
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  rename(psi_gender_cleaned = PSI_GENDER)

tmp_credentialgendercleaning_step6 <- tmp_credentialgendercleaning_step6 |>
  left_join(
    tmp_CredentialGenderCleaning_Step7 |>
      select(PSI_STUDENT_NUMBER, PSI_CODE, psi_gender_cleaned),
    by = c("PSI_STUDENT_NUMBER", "PSI_CODE")
  ) |>
  mutate(
    psi_gender_cleaned = coalesce(psi_gender_cleaned.x, psi_gender_cleaned.y),
    psi_gender_cleaned_flag = if_else(
      !is.na(psi_gender_cleaned.y),
      "Yes",
      NA_character_
    )
  )

tmp_credentialgendercleaning_step6 <- tmp_credentialgendercleaning_step6 |>
  select(-psi_gender_cleaned.x, -psi_gender_cleaned.y)


# -------------------------------------------------------------------------
# qry03f_24 & 25: Isolation of Null Cohort and Institutional Mapping
# -------------------------------------------------------------------------
tmp_credentialgendercleaning_step5 <- credential_supvars |>
  filter(is.na(psi_gender_cleaned)) |>
  select(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE, psi_gender_cleaned)

tmp_credentialgendercleaning_step6 <- tmp_credentialgendercleaning_step5 |>
  inner_join(
    stp_enrolment |> select(PSI_STUDENT_NUMBER, PSI_CODE, PSI_GENDER),
    by = c("PSI_STUDENT_NUMBER", "PSI_CODE"),
    relationship = "many-to-many"
  ) |>
  distinct()

# -------------------------------------------------------------------------
# qry03f_26 & 27: Conflict Detection and Evidence-Based Resolution
# -------------------------------------------------------------------------
credentialsupvars_multigenderfornulls <- tmp_credentialgendercleaning_step6 |>
  group_by(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE) |>
  summarise(GenderCount = n_distinct(PSI_GENDER), .groups = "drop") |>
  filter(GenderCount > 1)

tmp_CredentialGenderCleaning_Step7 <- credentialsupvars_multigenderfornulls |>
  select(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE) |>
  inner_join(
    credential_supvars_enrolment |>
      select(
        ENCRYPTED_TRUE_PEN,
        PSI_GENDER,
        PSI_SCHOOL_YEAR,
        PSI_ENROLMENT_SEQUENCE
      ),
    by = "ENCRYPTED_TRUE_PEN"
  ) |>
  group_by(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE, PSI_GENDER) |>
  summarise(
    MAX_PSI_SCHOOL_YEAR = max(PSI_SCHOOL_YEAR, na.rm = TRUE),
    MAX_PSI_ENROLMENT_SEQUENCE = max(PSI_ENROLMENT_SEQUENCE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(ENCRYPTED_TRUE_PEN, PSI_STUDENT_NUMBER, PSI_CODE) |>
  slice_max(
    order_by = tibble(MAX_PSI_SCHOOL_YEAR, MAX_PSI_ENROLMENT_SEQUENCE),
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  rename(ResolvedGender = PSI_GENDER)

# -------------------------------------------------------------------------
# qry03f_28, 32, 33, & 34: Final Reconciliation, Flagging, and Normalization
# -------------------------------------------------------------------------
tmp_credentialgendercleaning_step6 <- tmp_credentialgendercleaning_step6 |>
  left_join(
    tmp_CredentialGenderCleaning_Step7 |>
      select(PSI_STUDENT_NUMBER, PSI_CODE, ResolvedGender),
    by = c("PSI_STUDENT_NUMBER", "PSI_CODE")
  ) |>
  mutate(
    PSI_GENDER_CLEANED_FLAG = if_else(
      PSI_GENDER %in% c("U", "Unknown", "(Unspecified)"),
      "Yes",
      NA_character_
    ),
    PSI_GENDER_CLEANED_FLAG = if_else(
      !is.na(ResolvedGender),
      "Yes",
      PSI_GENDER_CLEANED_FLAG
    ),
    psi_gender_cleaned = coalesce(ResolvedGender, psi_gender_cleaned),
    psi_gender_cleaned = if_else(
      is.na(PSI_GENDER_CLEANED_FLAG),
      PSI_GENDER,
      psi_gender_cleaned
    ),
    PSI_GENDER_CLEANED_FLAG = if_else(
      is.na(PSI_GENDER_CLEANED_FLAG),
      "Yes",
      PSI_GENDER_CLEANED_FLAG
    )
  ) |>
  select(-ResolvedGender)

# -------------------------------------------------------------------------
# qry03f_35: Final Synchronization of Institutionally Resolved Genders
# -------------------------------------------------------------------------
credential_supvars <- credential_supvars |>
  left_join(
    tmp_credentialgendercleaning_step6 |>
      filter(PSI_GENDER_CLEANED_FLAG == "Yes") |>
      select(PSI_STUDENT_NUMBER, PSI_CODE, psi_gender_cleaned),
    by = c("PSI_STUDENT_NUMBER", "PSI_CODE")
  ) |>
  mutate(
    psi_gender_cleaned = if_else(
      is.na(psi_gender_cleaned.x),
      psi_gender_cleaned.y,
      psi_gender_cleaned.x
    )
  ) |>
  select(-psi_gender_cleaned.x, -psi_gender_cleaned.y)
credential_supvars <- credential_supvars |> distinct()
