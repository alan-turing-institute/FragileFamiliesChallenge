# Fragile Families Challenge - Understanding the Data

## Variable Descriptions

There are several PDF files that explain what types of information were collected. Of particular use are the overview files for the publicly available data, which are in the Codebook folder. 

You will notice that all variable names are short (6-10 characters), and have their own grammar. 

## Variable Encodings

The Codebook contains information about the way in which the variables measured are encoded in the data. All values are numeric, but they don't necessarily represent quantitative or even interval scaled data. This means we will need to recode quite a few of these variables to allow for meaningful analysis. 

All variables carry additional information about reasons for missing values:

| Value | Description | Meaning |
| -------| ----------| ---------------- |
| -1 | Refused | participant refused to answer, meaningful missingness |
| -2 | Don't Know | information could not be elicited | 

– valid skips are coded as -6, “Don’t Know” as -2, “Refused” as -1, general missing as -3, and -9 as “Not in Wave.” 

For our purposes, we need to distinguish between the following types of variable encodings:

Qualitative: 
	These are variables that encode concepts numerically. A good example is *5i12a_code from the Year 9 data. 
	The three digit codes stand for very different occupations. 
	
	When dealing with these variables, it would be useful to apply one or more of the contrast-coding approaches used in regression modelling
	
Binary: 