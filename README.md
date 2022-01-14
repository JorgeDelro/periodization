
# periodization

This package is currently under development. Functions to calculate the
ACWR of several individuals (or just one individual) have been
implemented.

## ACWR calculation example

First, we have to load the data which consists in a database with the
following columns:

<ul>

<li>

ID: subject identifier (integer).

</li>

<li>

TL: training load (sRPE - integer)

</li>

<li>

weeks: week of training (integer).

</li>

<li>

training\_dates: date of the training (date format in excel -
e.g. 7/1/22).

</li>

</ul>

``` r
# Read dataframe (excel)
training_data <- readxl::read_xlsx("training_data.xlsx")

# check column names
# names(training_data)
# "ID"  "TL"  "Week" "Training_Dates"

# Calculate ACWR for every ID using the three available methods
res_ACWR <- (df = training_data,
             ID = "ID",
             TL = "TL",
             weeks = "Week",
             training_dates = "Training_Dates",
             ACWR_method = c("EWMA", "RAC", "RAU"))
```

The ACWR function returns the training data dataframe plus the acute,
chronic and ACWR for each method. In the previous example nine columns
have been added to the dataframe (EWMA\_acute, EWMA\_chronic,
EWMA\_ACWR, RAC\_acute, RAC\_chronic, RAC\_ACWR, RAU\_acute,
RAU\_chronic and RAU\_ACWR).

## EWMA, RAC and RAU functions

EWMA, RAC and RAU functions could be used for each subjects
individually.

``` r
# Get the data of the first participant
participant_1 <- training_data[ training_data[["ID"]] == 1,  ]

res_EWMA <- EWMA(TL = participant_1$TL)

res_RAC <- RAC(TL = participant_1$TL ,
              weeks = participant_1$Week,
              training_dates = participant_1$Training_Dates)
              
res_RAU <- RAU(TL = participant_1$TL ,
              weeks = participant_1$Week,
              training_dates = participant_1$Training_Dates)
```

Each function returns a list with the acute, chronic and ACWR.
