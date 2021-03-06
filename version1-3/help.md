# **Sample video**

<iframe width="100%" height="400" src="https://www.youtube.com/embed/SoELWc8N3zI" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

# Select database or create

The first action you need to do is to select a database to explore. You have three options available. For create databases selection, you have the option to select a "hierachy"; that is, a word will take prosodic TOBI tagging and other information from vowels, an intonational phrase will take information from words and also from vowels; a collapse1 or collapse 2 will take information from intonational phrases, words and vowels.

## Select sample databases

In this version you have access to two sample databases: fonocortesiadb and picodeorodb. These are two separate projects:

1.  Fonocortesia (phonopoliteness).

2.  Pico de oro / Padre Zorro.

## Create

An alternative way to explore with Oralstats is to create your own data. You will be able not just to create, but also to explore the data you just created with the common variables generated by Oralstats. There's a relatively steep learning curve here.

## Upload custom databases

If you have already a database, not necessarily with prosody associated but maybe with speech data, you can upload it here and explore it.

# Filter

With the database selected you will have the option to filter by at least three categorical variables (the ones that you desire from your data) and two numeric variables. Here you will get also a datatable with the variables you can be interested in. As long as you filter, the data will be reduced accordingly; if you do not filter, all the data will be shown.

# N-Grams

With n-grams you can select the number of consecutive words you want to explore; you can select the variable to get the n-grams from and the size of n-grams.

# Basic methods

## Descriptive by group

You can combine here several variables and you will get absolute frequencies. If you select some numeric variable, you will get values as mean, median, max or min, but for every one of the categories, or combinations, selected from nominal variables above.

## Descriptive by variable

Similar to "descriptive by group", but this time you get common descriptive statistics for variables not grouped by category; the options here come from Psych package.

## Crosstabs and chi-square

Two variables can be put together. Chi-square and residuals will test the null hypothesis of independence. A basic bar chart will be also generated.

# Advanced methods

## Heatmap

The heatmap allows you to explore the similarity of groups. As the variables can have different scales (hertz, semitone, millisecond...), these are previously scaled.

## Decision tree

Decision trees allow you to select one dependent variable and a group of predictors. In this case, you will get two charts, the first is generated by Rpart packages; the seconds is done by Party package.

## Correlation plot

First, you can select which numeric variables want to correlate. Second, a correlation plot will be generated.

## ANOVA and boxplot

A common analysis of variance among groups will be generated along with boxplots.

## PCA

Principal Component Analysis done with FactoMineR package (just numeric variables here). You have the option to create groups using cluster analysis. As this is a really time consuming method, a sample proportion of data is selected.

## MFA

Mixed Factor Analysis done with FactoMineR package (it combines numeric and nominal variables). You have the option to create groups using cluster analysis. As this is a really time consuming method, a sample proportion of data is selected.

## Logistic regression

A binary logistic regression to make predictions on a nominal variable.

# Prosody

## Raw prosody

Here you can select the time of beginning and ending and you will be able to see the pitch or intensity raw data. It is possible to see a data table with information of interest.

## Linear tagged prosody

In this case, you can explore the linearity of data accordingly to a factor variable. For example, you can see the intonational phrases that are beyond or above the mean of the corpus; this intonational phrases can be grouped by speaker or by any other taxonomy. For example, at Fonocortesia database you can explore the individuals belonging to polite or impolite categories.

## Pitch contour

This is an evolution of raw prosody. Here you can get the tagged contours of pitch but you can make a bunch of configurations to explore relationships on data: you can change lines, colors, and project even TOBI tags. It is thought as a way to explore pitch pattern configuration but also to project another values on dot sizes: intensity, duration...
