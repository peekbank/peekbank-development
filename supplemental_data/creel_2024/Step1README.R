# Step1README.R
# README file for eyemagic
# AUTHOR
# 8/15/23
# Last updated 5/2/24

############################################################
############################################################
############## Throughout, really important ################
##############    text appears in # boxes.  ################
############################################################
############################################################

### Not all important text may appear in boxes. ###

### If something is commented out and far-indented, it's   ###
### something not very useful that I was afraid to delete. ###


### You need to run Step2a and Step2b scripts to run most ###
### any of the remaining scripts. Step2a needs to be run  ###
### with different values of whichCorrsToGenerate, which  ###
### will give you different values of all resulting       ###
### variables. Also writes some to file.                  ###


### STUFF:

### Data file: allnovels.csv
  # NOVEL WORD, MELODY LEARNING, TALKER LEARNING trials but not familiar stuff
  #
  # Contains averaged look locations every 50 ms of every trial of every child.
  # Experiment ID, experimental condition, age, and sex data are noted.
  # Age outliers, dropped Ss from old studies, etc. are already eliminated.
  # Children whose data are eliminated for too few onscreen looks still included,
  #   since this can vary depending on your analysis presets.


### Data file: allfamiliar.csv
  # FAMILIAR WORD trials, all 2AFC, that were interspersed in KL studies and MN3.
  #
  # Contains averaged look locations every 50 ms of every trial of every child.
  # Experiment ID, experimental condition, age, and sex data are noted.
  # Age outliers, dropped Ss from old studies, etc. are already eliminated.
  # Children whose data are eliminated for too few onscreen looks still included,
  #   since this can vary depending on your analysis presets.



### SCRIPTS:

### Step2aCorrsNDataPrep.R *****
  # Basically NO scripts will work if you haven't run this
  # CALCULATES several different sets of looking-pointing correlations
  # Preprocessing of gaze data to be used in following analyses
  # 
  # Select data to be processed from all learning studies; familiar words; 
  #   novel words; all words
  #
  # CHANGE whichCorrsToGenerate to 4 different values depending on what you
  #   want to get out of this script. Set to 1 for most analyses.
  #
  # VARIABLES created:
  #   windf (data frame of time window[s] analyzed, with correlation values
  #          within whatever Reference is)
  #   cleanlookacc2 (one line of data per cell, 
  #                  with summary values of looks and pointing; 
  #                  probably cell = one subject in a particular condition, but
  #                  it can be whatever you set Reference to)
  #   datcleaned (each line is a single trial, trials with 
  #               bad looks have been removed)
   

###  Step2bDatAnuPrep.R *****
  # VARIABLES created:
  #   datAnu, modified from dat, for analyzing looks *over time*
  #     - Filters out trials with bad looks from the moment-by-moment looking data
  #     - Scripts 4,5,6 won't work without datAnu variable


### Step3aDescriptiveXYplots.R
  # PLOTS
  #   *FIGURE 2*:
  # - scatterplot of ageXlooks (Figure 2 upper)
  # - scatterplot of ageXacc (Figure 2 middle)
  # - scatterplot of accXlooks (Figure 2 lower)
  # - scatterplot of accXlooks for FAMILIAR words (Supplementary Figure 1)



### Step3bDescriptiveCorrPlots.R
  # PLOTS
  #  - sideways barplot of individual correlations per condition (Figure 3)
  #  - supplementary figure 3x3 plot of age group X expt subtype correlations (Suppl Fig 2)



### Step3cMultiTimeWinPlots.R
  # You MUST run Script2 with whichCorrsToGenerate 
  #    set to 2 & 3 to generate proper files to plot
  # PLOTS:
  #  - Look-point correlations over time for growing and moving windows by expt subtype (Figure 4)
  #  - Look-point correlations over time for growing and moving windows by age (Suppl Fig 3)



### Step4aMainAnalyses.R
  # Does some analyses, using datcleaned and cleanlookacc2 as output from Step2 script.
  # ANALYSES:
  # - simple correlations (uses datcleaned [single trials] and cleanlookacc2 .[overall])
  # - LMER() models of Looks ~ Acc
  # - LMER() models of Acc ~ Looks
  # - Analyses of looks when pointing acc is at chance
  # - Analyses of pointing acc when looks are at chance
  #
  # PLOTS:
  #  - looks at chance pointing (Figure 5 left)
  #  - points at chance looking (Figure 5 right)
  #  - looks over time of chance lookers with high pointing accuracy (Suppl Fig 6)

    
    
### Step4bModelAgeExtrap.R
  # Uses (hard-coded) model values from Step4aMainAnalyses
  #  to generate fits to a simple model of Looks ~ PointingAccuracy
  # Extrapolates to earlier, later ages
  # DATA FOR (old Supplementary Table 2)
  # Could potentially be a 'yardstick' for studies of younger ages
  # Note: syAge is age in years, centered (but not /SD) so easier to calc
  #
  # Models
  #     - SIMPLE model with just pointing accuracy, Age (years), acc*Age as predictors

    
    
### Step5LooksXTimeXPointAcc.R
  # uses datcleaned from Step2a and datAnu from Step2b
  #   to look at individual subject look patterns
  # PLOTS:
  #   - looks over time, grouped by pointing accuracy bins (Suppl Fig 5)



### Step6PresignalLooks.R
    # Uses datAnu from Step2bscript
    # How much of looks in 1-200 bin (before can react to audio signal) are
    # related to final pointing response?
    # CALCULATES binomial test of presignal looks ax conditions
    # PLOTS
    #   - looks over time as fcn of eventual pointing choice (Suppl Fig 7)
    #   - looks over time to selected picture as fcn of overall pointing accuracy (Suppl Fig 9)
    #   - sideways bar plot of tendency for presignal looks to be toward pointing choice, ax conditions (Suppl Fig 8)

    
    
###  Step7LookRTs.R
  # Uses dat variable to scan through to find distractor-to-target looking reaction times (RTs)
  # Scan through those data to find move-off-distractor looking reaction times (RTs)
  # REQUIRES
  #  dat variable (or read in file like allnovels.csv) containing each trial over time
  # VARIABLES created:
  #   - RTdat (contains looking reaction times--how quickly viewers move from
  #      an initial competitor look location to looking at the target.
  # ***This takes a long time to generate, so you should save the resulting 
  #       RTdat data frame and reimport later***
  # Provides option to set RT to logRT
  # CALCULATES correlations on lookRT values with looking proportion values and pointing accuracy means
  # PREPARES 
  #   - RTdat for later raster plot
  #   - RTdat for reliability calculations
  # PLOTS
  #  - Simple line plot of look RT x point accuracy (not used in paper but fun)



###  Step7aLookRTsPkbk.R
  # VERY SIMILAR to Step7LookRTs.R
  # Reads in dataset(s) from Peekbank, can select Adams et al 2018 or 6 studies with 3-4-year-olds
  # Scan through those data to find move-off-distractor looking reaction times (RTs)
  # VARIABLES created:
  #   - RTdat (contains looking reaction times--how quickly viewers move from
  #      an initial competitor look location to looking at the target.
  # ***This takes a long time to generate, so you should save the resulting 
  #       RTdat data frame and reimport later***
  # Provides option to set RT to logRT
  # CALCULATES correlations on lookRT values with looking proportion values (no pointing)
  # PREPARES 
  #   - RTdat for later raster plot
  #   - RTdat for reliability calculations
  # PLOTS
  #  - Simple line plot of look RT x point accuracy (not used in paper but fun)



### Step7bRaster.R
  # REQUIRES:
  # - RTdat from Step7, generated from corresponding dat file (like dat or pbkdata)
  # - dat (current data) or pbkdata from Step9 Peekbank analyses
  #
  # choose:
  #  - doingpbk=0 or 1, depending on whether you're using current data or Peekbank data
  #
  # PLOT generated:
  #    - *SUPPLEMENTARY FIGURE 4* cool raster plot of individual comp-initial trials
  #    - option to do Peekbank rasters
  #
  # Raster plots show a single trial per thin horizontal slice,
  # which lets you see patterns across large numbers of trials.
  # Requires ordering trials by lookRT, which was tricky.
  


### Step8permutedreliability.R
  # See Byers-Heinlein paper (Six solutions, 2021)
  # and Parsons et al. (2019)
  # uses Parsons (2021) splithalf toolkit
  #
  # REQUIRES:
  #  - datcleaned, and briefly dat3 and cleanlookacc2, from Step2a script
  #  - RTdatgoodpluslooks from Step7/7a scripts for RT reliability analyses 
  #  - pbkdata from ?Step7a for basic Peekbank reliability (could modify to read in)
  #
  # FWIW, the permuted split-halves values on my data are quite similar 
  # (within about .01) to odds-evens split halves. First half-second half 
  # is a bit lower.



### Step9Peekbankanalyses.R
  # Aim: look at non-pointing studies with kids in age range of current study,
  #  to see if pointing task results in very different look profiles. SAME!
  # Step 1: figure out which studies have kids ages 36 mos +
  #   Step 1b: keep all the ones with 20+ kids
  # Step 2: read in and filter
  # Step 3: read in MY 2afc familiar-word data (just KL3-6,MN3) and novel-word data
  #   Step 3b: created new Step 01 to KEEP familiar trials instead of discard
  # Step 4: filter by same criteria as Peekbank
  # Step 5: graph them all together
  #
  # plots generated:
  #    - *FIGURE 6 LOWER RIGHT* - familiar words plot Peekbank + current data!!!!
  #    - familiar words Peekbank and current familiar word data as individual studies (not in paper)
  #    - *FIGURE 6 LOWER LEFT* - novel words plot Peekbank + current data!!!!
  #    - novel words Peekbank and current novel word data as individual studies (not in paper)