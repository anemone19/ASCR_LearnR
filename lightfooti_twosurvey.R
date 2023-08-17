# ACRE: LIGHTFOOTI TWO SESSION SURVEY ANALYSIS

# METHOD 2 - NO FOR LOOPS 

# Create a matrix of row and column indices where capt == 1
indices <- which(capt == 1, arr.ind = TRUE)

# Extract the row and column indices
ID <- indices[, 1]
traps_ID <- indices[, 2]

# Create a dataframe with the results
captures <- data.frame(ID, traps = traps_ID) %>%
  arrange(ID)

# add session and occasion columns
# replace rownames in ID with ids 

captures <- captures %>%
  mutate(session = 1, occasion = 1) %>%
  select(session,ID,occasion,traps)

# Read data 
lightf_data <- read.acre(captures = captures,
                         traps = traps,
                         control_create_mask = list(buffer = 15, spacing = 0.5))

### Model fitting ---------------------------------------------------------------------------

model1 <- fit.acre(
  dat = lightf_data,
  detfn = "hhn"
)

summary(model1)
