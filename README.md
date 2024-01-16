# Data Science Capstone - Overview
This is a repo to host the files for the Coursera "Data Science Capstone" Course from Johns Hopkins. The purpose of this course is to create a text prediction app on Shiny using an ngram based algorithm that is trained on a provided corpus of text from Swiftkey, an industry partner to Johns Hopkins in this course. 

## Overview  
This project was broken down on Coursera into pieces including: 
1. Data import (download data from the corpus)
2. Data cleaning (remove punctuation, capitalization, swear words, numbers)
3. Model building (analyze data to create ngrams for model and create model)
4. Shiny app creation
5. Pitch presentation

For a more complete examination of what the task details, see the Notes and Tasks files. In Notes, I recorded information from the Coursera page so I had a single place to go and review the progress of the project goals and in Tasks, I implemented many of them. 

### Descripton of Files in Repo
* Milestone Report: halfway through the project, a milestone report was created that detailed the results of exploratory data analysis and the state of the project. 
* Notes.qmd: record of tasks from Coursera
* README.md: This file
* Tasks.qmd: record of different efforts at implementing the tasks
* process corpus.R: record of the steps taken to process the raw data files into the frequency tables of observed ngrams. 
* ngram_Prediction: folder containing files supporting published Shiny app
  * katz_backoff.r: implementation of backoff algorithm functions used in Shiny app
  * server.R: server function of shiny app
  * ui.R: ui function of shiny app