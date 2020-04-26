# PDandCS-code

This repository contains R code for my paper Proof Discontinuities and Civil Settlements, which is forthcoming in the journal Theoretical Inquiries in Law.  I am happy to answer any questions or provide assistance to interested readers who wish to play with the code.  I can be reached via email at spottswood@law.fsu.edu.  

Here is a guide to the files in this repository:

1.  DataSim of Settlements Under Different Burden Rules - Final 2020.R

This file contains the necessary parameters and code to run a data simulation, generating both unbiased and biased confidence forecasts, amounts-in-controversy, and cost parameters for a given number of cases, and then computing model predictions for whether the cases settle, how much, and with how much error, under each of the three burden of proof rules I study in the article. 

2. Code to Create Dataset of 1M simulated cases.R

This file can be used to replicate the specific dataset I explored in the article.  It generates one million simulated cases, but the generation is broken into smaller chunks due to the tendency of R to run more and more slowly as the lenth of loops grow in size.  You should source the first file before attempting to run this one. 

3. Summarizing Simulated Data - Part IB FINAL.R

This code was used to generate the descriptive statistics and graphics that were used in Part IB of the paper.  For this and all subsequent code, you should have run the preceding file and created the large dataframe "CompareDF," which these files will draw on to produce graphics and results.

4. Analyzing Data - Paper Part II - April 19 2020 FINAL.R

This code was used to generate the descriptive statistics, statistical test results, and graphics that are used in Part II of the paper.

5. Analyzing Data - Paper Part III - April 24 2020 - Final.R

This code was used to generate the descriptive statistics, statistical test results, and graphics that are used in Part III of the paper.

6. Analyzing Data - Paper Part IV - April 23 2020 FINAL.R

This code was used to generate the descriptive statistics, statistical test results, and graphics that are used in Part IV of the paper.

7. Unbiased Mean Prediction Parameter Generation - Code for App IA.R
8. Simulating Damages Values - Code for App IB.R
9. Cost Model Details - Code for App IC.R

These last three files contain the code needed to produce the information and graphics regarding the process of generating parameter values for the dataset, which was set out in the appendix to the article.



---

Mark Spottswood
Associate Professor
FSU College of Law
