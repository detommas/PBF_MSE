# Pacific Bluefin Tuna Management Strategy Evaluation

This repository contains the code to run a management strategy
evaluation (MSE) for Pacific Bluefin tuna. Note that the MSE and code is
still in development. This is a very early beta version with a
simplified harvest control rule that maintains a catch cap based on the current catch limits. 
Additional harvest control rules will be added as the code progresses. 
The repository is intended to be a code sharing and collaboration platform for all the
members of the ISC Pacific Bluefin Working Group (PBFWG) that are
working on developing and testing the MSE.

Note that both the operating and estimation models are based on the
Stock Synthesis software. The operating model is based on the ISC PBFWG
2022 short (1983 start) assessment model. Note that the code was written
for a Windows operating system and tested with SS version V3.30.18.00, R
version 4.1.3, and r4ss package 1.44.0.

## Getting started with running the PBF MSE

-   Clone the repository your computer. It contains all the directories
    and files needed to run the MSE for scenario 1, harvest strategy 1,
    and harvest control rule 15. Note that you need to keep the directory
    structure as is. The numbered directory has the following format
    *harvest strategy/hcr/scenario/iteration/time step*
-   Change paths at the start of *PBF_MSE_itr1.R* or *PBF_MSE_prll.R* and *PBF_MSE_hs1.R* to
    reflect the path where the PBF_MSE folder is on your computer.
-   Run *PBF_MSE_itr1.R* to make sure the code works for 1 iteration (EM
    is now run without computing the hessian, so one iteration for the
    30 year simulation should take about 8 hrs). This code
    calls the *PBF_MSE_hs1.R* wrapper function that runs the MSE. You can also 
    run the simulation with no EM by selecting sa=0 instead of 1 in the *PBF_MSE_hs1.R* function.
-   Go over the *PBF_MSE_hs1.R code* to make sure you understand what it
    does. Open it and run bits of it (for example, only run tstep=1 in
    the loop, making sure you have all the inputs specified in
    *PBF_MSE_hs1.R*). All the functions it calls are in the
    PBF_MSE/Rcode directory so you can look them up to see if they need
    any modifications or check for bugs.
-   When you finish running, you should see in RStudio the output of the
    outmat table that collects all the information to generate
    performance metrics. This information is also saved as a text file,
    *outlist.txt*.
-   Once the code for 1 iteration works, you can try running the *PBF_MSE_prll.R* to run multiple iterations in parallel
