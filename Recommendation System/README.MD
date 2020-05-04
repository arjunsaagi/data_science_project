
This is a academic project by IIITB- upgrad.

In this problem statement we had to build recommendation system for "BeerMart", an online beer store. Based on recommendation system, customer of the store will be recommendated the beer that they are most likely to buy. 

Data file: beer_data.csv

Following activities has been performed: 

1. Data preparation
   Selected the bear that had at least N number of reviews and selected the reviewer who have given atleast M number of review.
   Convert the data frame to a “realratingMatrix” before building collaborative filtering models

2. Data Exploration

    Determine how similar the first ten users are with each other and visualise it
    Compute and visualise the similarity between the first 10 beers
    What are the unique values of ratings?
    Visualise the rating values and notice:
        The average beer ratings
        The average user ratings
        The average number of ratings given to the beers
        The average number of ratings given by the users

3. Recommendation Models
    Divided data into training and testing datasets
    Experiment with 'split' and 'cross-validation' evaluation schemes
    Build IBCF and UBCF models
    Compare the performance of the two models and suggest the one that should be deployed
    Plot the ROC curves for UBCF and IBCF and compare them
    Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
