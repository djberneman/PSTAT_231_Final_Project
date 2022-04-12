---
title: "Final Project Data Memo"
author: "Dylan Berneman"
date: '2022-04-10'
output: 
  html_document:
      toc: true
      toc_float: true
      toc_depth: '3'
      keep_md: true
---


## Final Project Data Memo
To make sure that each student is progressing in the final project for this course, and to verify that your data set(s) of choice will work for a machine learning project, everyone is asked to submit a check-in or "data memo."

Your final project is intended to showcase your analytical abilities and model-building skills on a dataset of your choosing. Your data memo should be written in the form of a short paper; if you've already read your data file into R, you can include code or figures, but not required to. Your memo should answer all of the following:

### An overview of your dataset

#### What does it include?

My dataset will include comprehensive data about the amount of cases and deaths in each county of the United States. It will also include the amount of people who are vaccination with one dose and those who have both doses.

#### Where and how will you be obtaining it? Include the link and source.


I will be obtaining this from the New York Times Covid-19 github page.
(https://github.com/nytimes/covid-19-data)

Sources: State and local health agencies (cases, deaths); U.S. Department of Health and Human Services (tests, hospitalizations, I.C.U. patients); Centers for Disease Control and Prevention (Vaccinations); U.S. Census Bureau(Census statistics) 


#### About how many observations? How many predictors?
For cases and deaths, there are 2.37 million observations and 5 variables. For vaccinations, there should be a similar amount of observations with 29 variables. This is a bit misleading because many of the variable are subsets of other variables. For example here are the variables that involve having at least one dose:

Administered_Dose1_Recip	          Administered_Dose1_Pop_Pct	
Administered_Dose1_Recip_5Plus	    Administered_Dose1_Recip_5PlusPop_Pct	Administered_Dose1_Recip_12Plus	    Administered_Dose1_Recip_12PlusPop_Pct	Administered_Dose1_Recip_18Plus	    Administered_Dose1_Recip_18PlusPop_Pct	Administered_Dose1_Recip_65Plus	    Administered_Dose1_Recip_65PlusPop_Pct

The first row is the value for the observation and the percentage of the population while the next 4 rows are subsets of the first row based on age range. The reason why I have them is because different age group were allowed access to vaccination at different points in time. 

I may also use a few more data sets that each only have around 56 observations each and anywhere between between 3 and 11 variables. I will have to find my source information for those data sets. 

While I know that this may seem like a lot of observations and variables, I was partly inspired by these three web pages:

(https://www.nytimes.com/interactive/2020/us/covid-19-vaccine-doses.html)

(https://www.nytimes.com/interactive/2020/us/covid-19-vaccine-doses.html#new-doses)

(https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=Alabama&data-type=CommunityLevels&list_select_county=1001)

I love all of the graphs that they have and the only thing that is missing from these websites are future forecasts for everything.


#### What types of variables will you be working with?

All variables will be numerical except for the date, county, and state, which will be categorical.

#### Is there any missing data? About how much? Do you have an idea for how to handle it?

I do not know how much missing data there is. I have been going through the counties of each state and deleting specific counties that are missing almost all of their data. This is mostly limited to two counties in each state called "Out of state_name", and "Unknown". While I have been deleting those counties from most states, there are a few states that actually have comprehensive data for these nameless counties, in which case I keep them. There is also missing data for some variable due to them not applying to a specific age range before a certain date. Also, I will have to observe each case of 'unknown' counties that I keep and determine if there are more the one 'unknown' county in a single state. The same thing would not apply to the 'out of state_name' counties.  

### An overview of your research question(s)

#### What variable(s) are you interested in predicting? What question(s) are you interested in answering?

I am interested in predicting cases, and deaths based on all of the information that I have. I want to make a small forecast for these two variables for a limited time frame in the future (after the final project will be due). I also want to answer the question of whether or not poverty, ethnicity, and minorities are significant predictors for cases and deaths. 

#### Name your response/outcome variable(s) and briefly describe it/them.

My response/outcome variables are 'cases' and 'deaths'.

Cases -     the amount of people who have been infected by covid-19
Deaths -    the amount of people who have died as a result of being infected by covid-19

I don't think any more explanations are necessary for them as they are pretty self-explanatory.

#### Will these questions be best answered with a classification or regression approach?

The forecasting part of my project will be answered with a regression approach while the significance part will be answered with a classification approach.

#### Which predictors do you think will be especially useful?

I believe that vaccination predictors and past cases will be useful in predicting future cases and deaths.

#### Is the goal of your model descriptive, predictive, inferential, or a combination? Explain.

The goal of my model is a combination of predictive and inferential. I want to predict the amount of future cases and deaths while determining how significant ethnicity, poverty, and being part of a minority are to the amount of cases and deaths of these same same predictors.

### Your proposed project timeline

#### When do you plan on having your data set loaded, beginning your exploratory data analysis, etc?

I already have most of my data set loaded, sorted, and filtered. My data set for cumulative cases and deaths by county per day are all complete with each state having its own excel worksheet with anywhere  (365 days) * (2+ years) * (counties per state)
observations each. 

(Hooray for Guam only having 1 county! Boo for Texas having over 250 counties!)

Within the next week or so, I will finalize all of my sorting and filtering for the data sets that I will decide to use.

#### Provide a general timeline for the rest of the quarter.

Once I have my data sets ready to go, I will take some time to research how I can use the models with time series data sets and then I will start making some graphs for all of my data sets. Once I figure out how to graph one variable in time, it will be easy to apply it to every other state. This part may take a few weeks. The last few weeks will be reserved for modeling and anything else that needs to be done.

### Any questions or concerns

#### Are there any problems or difficult aspects of the project you anticipate?

I anticipate the having difficulties adapting the models to time series data sets.
I have spent days cleaning, sorting and filtering data, but all of it has been done within excel. Due to how long each operation takes to be run, I won't be able to replicate all of it in r studio. The only thing that I can do is include the code for the advanced editor within excel's power query editor and describe the process of search for which counties to remove from the data sets and the add-in I used to split three 1,000,000 observation worksheets into multiple worksheets based on the state value of the observation. Can I still include this in my project? I saw that there was a section for cleaning the data set in the example student's project.

#### Any specific questions you have for me/the instructional team?

I am a little concerned about whether or not any of this will work and if I will even be able to include some of my data sets that are not all time series. Can I make an appointment, either in person or over zoom, to show you what I have and explain my concerns more thoroughly? 
