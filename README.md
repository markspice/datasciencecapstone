# datasciencecapstone
Capstone project for the JHU Coursera Data Science Specialization

## Shiny application:
https://markspice.shinyapps.io/DataScienceCapstone/

## Product pitch:
http://rpubs.com/markspice/392333

## Milestone report:
http://rpubs.com/markspice/387810

## Repository file index:

- README.md: This read me file
- PA10_2MilestoneReport.Rmd: R markdown file generating the week 2 milestone report
- CapstonePitch.Rpres: RStudio Presenter code for week 6 slide deck
- server.R / ui.R: Final code for Shiny word prediction application
- 01GenerateNgramData.R: R code for loading data and generating ngram-word pariring realtive frequency tables
- 02GenerateCleanNgramData.R: R code for transforming ngrams before generating relative frequency tables
- 03GenerateTestData.R: R code for generating model success rate test data sets
- 04GenerateModelDefinitions.R: R code for defining the model specifications
- 05GenerateTestPredictions.R: R code for generating test predictions from each model against the test data sets
- 06GenerateFunctions.R: R code defining word prediction functions for speed tests for selected models
- 07GenerateSizeTestFunction.R: R code defining function to test model using different size training data
- 08PredictionTests.R: R code for building test result output tables
- ngramfrequency.RData: Source data used in generating word frequency plots in milestone report
- testResults.RData: Test results generated from 08PredictionTests used in slide deck
- cleanNGram33b.csv: Chosen initial ngram-word frequency pairing data used in Shiny app. Built from model c3b with max 3-gram input

## Model definitions:

| No. | Code | No. predictions | Min. frequency | Other details                                           |
| :-: | :--- | :-------------: | :------------: | :------------------------------------------------------ |
| 1   | 1    | 1               | 1              | -                                                       |
| 2   | 2    | 1               | 2              | -                                                       |
| 3   | 3    | 3               | 1              | -                                                       |
| 4   | 4    | 3               | 1              | Matches include missing intermediate words              |
| 5   | 3b   | 3               | 2              | -                                                       |
| 6   | 3b2  | 3               | 3              | -                                                       |
| 7   | 3b3  | 3               | 4              | -                                                       |
| 8   | 3b4  | 3               | 5              | -                                                       |
| 9   | c3   | 3               | 1              | Name, time, No. substitution and elongation replacement |
| 10  | c3b  | 3               | 2              | Name, time, No. substitution and elongation replacement |
| 11  | c3b2 | 3               | 3              | Name, time, No. substitution and elongation replacement |
| 12  | c3b3 | 3               | 4              | Name, time, No. substitution and elongation replacement |
| 13  | c3b4 | 3               | 5              | Name, time, No. substitution and elongation replacement |

Null models:

| No. | Code | No. predictions | Min. frequency | Other details                                           |
| :-: | :--- | :-------------: | :------------: | :------------------------------------------------------ |
| N1  | N1   | 1               | N/A            | N/A                                                     |
| N3  | N3   | 3               | N/A            | N/A                                                     |

All models except model 4 are tested with maximum ngram sizes of one, two and three words.
