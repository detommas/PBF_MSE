# Pacific Bluefin Tuna Management Strategy Evaluation

This repository contains the code to run a management strategy
evaluation (MSE) for Pacific Bluefin tuna. Note that the MSE and code is
still in development. This is a very early beta version with the candidate harvest control rules 
proposed by the IATTC and WCPFC NC Joint Working Group in summer 2023.
The repository is intended to be a code sharing and collaboration platform for all the
members of the ISC Pacific Bluefin Working Group (PBFWG) that are
working on developing and testing the MSE.

Note that both the operating and estimation models are based on the
Stock Synthesis software. The operating model is based on the ISC PBFWG
2024 assessment model. Note that the code was written
for a Windows operating system and tested with SS version V3.30.22.1, R
version 4.3.1, and r4ss package 1.49.2.

## Getting started with running the PBF MSE

-   Clone the repository your computer. It contains all the directories
    and files needed to run the MSE for scenario 1, harvest strategy 1,
    and harvest control rule 15. Note that you need to keep the directory
    structure as is. The numbered directory has the following format
    *harvest strategy/hcr/scenario/iteration/time step*
-   Change paths at the start of *PBF_MSE_prll_hs1_hcr1_sam24_ncmm.R* and *PBF_MSE_hs1_for_sam24_ncmm.R* to
    reflect the path where the PBF_MSE folder is on your computer.
-   Run *PBF_MSE_itr1.R* to make sure the code works for 1 iteration (EM
    is now run without computing the hessian, so one iteration for the
    30 year simulation should take about 8 hrs). This code
    calls the *PBF_MSE_hs1_for_sam24_ncmm.R* wrapper function that runs the MSE. You can also 
    run the simulation with no EM by selecting sa=0 instead of 1 in the call to the *PBF_MSE_hs1_for_sam24_ncmm.R* function.
-   Go over the *PBF_MSE_hs1_for_sam24_ncmm.R code* to make sure you understand what it
    does. Open it and run bits of it (for example, only run tstep=1 in
    the loop, making sure you have all the inputs specified in
    *PBF_MSE_hs1_for_sam24_ncmm.R*). All the functions it calls are in the
    PBF_MSE/Rcode directory so you can look them up to see if they need
    any modifications or check for bugs.
-   When you finish running, you should see in RStudio the output of the
    outmat table that collects all the information to generate
    performance metrics. This information is also saved as a text file,
    *outlist.txt*.
-   Once the code for 1 iteration works, you can try running the *PBF_MSE_prll_hs1_hcr1_sam24_ncmm.R* to run multiple iterations in parallel
