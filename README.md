% simulateDCE

# Introduction
This project develops a tool to simulate data for choice experiments. The main aim of the project is to allow you to test your experimental designs via Monte-Carlo simulations. You can use as many designs as you want, the code will read in one design after the other and do the simulation. There are three main criteria you can use to evaluate your design.

- Statistical Power: In how many cases do you detect all effects, given that they exist on Alpha = 0.05
- Bias: Are your estimates biased
- Efficiency: Which design has the lowest standard errors




The project contains three files and the design folder. 

- functions.R provides the relevant functions needed for the simulation. You should not alter this file
- simulationcore.R is for you to create all your specifics. You define the beta parameters, the utility function, the number of respondents to simulate and so on.
- simulation_output.qmd is a quarto document that creates nicely formatted output. As it is now, there is no need to modify it. However, if you want to change the presentation of the output, feel free to modify it as you like.
- Put all your designs you want to test in this folder. Do not change the name of the folder. It is important that you make sure that the folder only contains the files you want to use. 

# Getting started

