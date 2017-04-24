# AVdatafest-Xtreme-ML-Hack

### Introduction
Code to create half of the winning voH2O submission for Analytics Vidhya's hackathon AVdatafest Xtreme ML Hack:  
https://datahack.analyticsvidhya.com/contest/machine-learning-hackathon/lb  

The code in thie repository was used as a 30% blend with vopani's code, which can be found here:  
https://github.com/rohanrao91/AnalyticsVidhya_XtremeMLHack

### Running Code
My solution is in R and I used the data.table and h2o package to create my solution. Versions of either should not be important, but I used  1.10.0 and 3.11.0.3784, respectively. Run on a Macbook pro, but again, it should be hardware agnostic.

I created the spanish_holidays.csv file that is stored in the input folder using the data found at: https://www.timeanddate.com/holidays/spain/

### Description
I looked at this as a forecasting problem from that start, and thought I might approach it as Rohan did by lining up the yearly calendars and using a smoothed version of direct history at the same point in the year. But with 7 years of history and so many splits (types, category+subject), by the time I started working on a solution, I was aiming for a machine learning solution. Fortunately, this worked well, and even better, it complemented Rohan's strategy well.

I kept the feature space fairly small.
* Calendar Features
  * day of week
  * week of year
  * day of year
  * year
* Holiday Features (all binary)
  * national
  * local
  * observance
  * common local
  * source: https://www.timeanddate.com/holidays/spain/
* Average Features
  * average Contacts per day of week and Contact type
  * average Resolution per day of week, category, and subject
* Other modeling
  * used the entire training timeframe
  * all H2O gbm
  * separate models for Contacts and Resolution
  * validation: started with Leave-One-Year-Out, finished measuring January - March 2016 (prior year from test predictions)

I started modeling with simple averages, as shown above, where the entire data set was used, and those results were applied to the entire data set. But soon I moved to a more sound version of the calculation where the impact of each record is removed from the calculation so that it is not leaked. And toward the end, I used an entire validation set to where no records of that set were used in the average calculations (similar to the actual prediction environment).
