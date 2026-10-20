 
# Clinical Trial:

# 1. Bloodpressure Data :

trial <- data.frame(
		
		id = 1:8,
		group = c("New", "New", "New", "New",
				"Placebo","Placebo", "Placebo", "Placebo"),
		baseline = c(150, 146, 154, 148, 149, 151, 147, 153),
		week8 = c(138, 137, 141, 136, 144, 146, 143, 148)	
)
trial

# Add SUB information :
trial$SUBJID <- sprintf("TRIAL-%03d", trial$id)
trial$TRT01P <- ifelse(trial$group == "New", "DRUGA", "PBO")
trial$AGE <- c(45, 62, 38, 51, 47, 59, 42, 55)
trial$SAFFL = "Y"
trial$SEX = c("Female", "Male","Female", "Male","Female", "Male", "Female", "Male")

trial

# 2. ADSL data:ADSL is the Subject-Level Analysis Dataset.
#It is unique because it contains one record per subject

adsl <- transform(
			trial,
			USUBJID = trial$SUBJID,
			TRT01P = trial$TRT01P,
			AGE = trial$AGE,
			SAFFL = trial$SAFFL,
			BASEBP = trial$baseline

			)
adsl <- adsl[c("USUBJID", "TRT01P", "AGE", "SAFFL","BASEBP" )]
adsl


# 3. STDM Demograpgic(DM)Domain:
dm <- transform(
			trial,
			STUDYID = "TRIAL01",
			USUBJID = SUBJID,
			AGE = AGE, 
			AGEU = "YEARS",
			SEX = SEX,
			ARMCD = TRT01P			)

dm <- dm[c("STUDYID", "USUBJID", "AGE", "AGEU", "SEX", "ARMCD")]
dm


# 4. STDM Adverse Events(AE) Domain:

# raw AE data:
raw_ae <- data.frame(
			SUBJID = c("TRIAL-001", "TRIAL-002", "TRIAL-002", "TRIAL-003", "TRIAL-004",
					"TRIAL-004", "TRIAL-005", "TRIAL-006", "TRIAL-007", "TRIAL-008"),
			event = c("Headache", "Nausea", "Fatigue", "Dizziness","Nausea", 
					"Fatigue","Nausea", "Fatigue","Nausea", "Fatigue" ),
			startdate = c("2006-01-01", "2006-01-02", "2006-01-03", "2006-01-04", "2006-01-05",
					"2006-01-06", "2006-01-07", "2006-01-08", "2006-01-09", "2006-01-10")

			)


ae <- transform(
			raw_ae,
			STUDYID = "TRIAL01",
			USUBJID = SUBJID,
			AETERM = event,
			AESTDTC = startdate
			)

ae$AESEQ <- ave(
			seq_len(nrow(ae)),
			ae$USUBJID,
			FUN = seq_along
			)

ae <- ae[c("STUDYID", "USUBJID", "AESEQ", "AETERM", "AESTDTC")]


# 5. STDM- style Demographics(dm) amd exposure(ex)data:

# Suppose everyone except TRIAL-002 recive treatment:

ex <- data.frame(USUBJID = dm$USUBJID[ dm$USUBJID != "TRIAL-002"])
ex


#6. SAFFL: Safety Population Flag
adsl$SAFFL <- ifelse(adsl$SUBJID %in% ex$USUBJID, "Y", "N")

adsl[, c("SUBJID", "SAFFL")]


# Practice dose dates, in the same order as the seven rows of ex:

ex$EXSTDTC <- c("2006-01-02","2006-01-02","2006-01-03","2006-01-04",
			"2006-01-05","2006-01-06","2006-01-07")

ex$EXSTDTC <- as.Date(ex$EXSTDTC)


# Match each participat's dose date to ADSL

adsl$TRTSDT <- EXSTDTC[match(adsl$SUBJID, ex$USUBJID)]

adsl[, c("SUBJID", "SAFFL", "TRTSDT")]


# 7.TRTSDT: Treatment Start Date
adsl$TRTSDT <- as.Date(ex$EXSTDTC[match(adsl$USUBJID, ex$USUBJID)])



# 8. Calculate Change from Baseline: CHG = Week8 - Baseline

trial$CHG <- trial$week8 - trial$baseline

trial[, c("id", "group", "baseline", "week8", "CHG" )]

# Average chnage for each treatment group:
aggregate(CHG~ group, 
		data = trial,
		FUN = mean) 


# save existing dataset:
saveRDS(trial, "trial_data.rds")


