# nur core möglich. for lr plus unendlich viele regressionen und abwägungen nötig

#' LR Scores by Jahn
#' 
#' Computes scores based on a multidemsional scaling.. 
#' X categories are derived deductively. Scores 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert scores
#' 
#' mds to get stimulus scores and then gl_scaling function
# 



jahn.lrcore.cats <- c(413, 412, 404, 403, 601, 603, 606, 401, 414, 505)

#
# SPSS syntax



*****omitt cases from mds if average vote share < 2% (but include if party gained seats) & progtype = "estimated"

SORT CASES BY
party (A) edate (A) .
aggregate
/break = party 
/avg_vote = mean(pervote) .
exe . 


****************MDS LR CORE (revised version 2011-11)

**mds lr core (1944 till october 73, 21 oecd; ignore esp, prt, grc, turk, isr, mex, sri lanka, northern ireland and all cee)

USE ALL.
COMPUTE filter_$=(edate >= DATE.DMY(1,1,44) AND edate <= DATE.DMY(1,10,73))  AND (country~=52) AND (country~=72) AND (country~=73) AND (country~=74) AND (country~=171) AND (avg_vote>=2 OR absseat>0) AND (progtype~=3).
VALUE LABELS filter_$  0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE .

PROXIMITIES per413 per412 per404 per403 per601 per603 per606 per401 per414 per505
/PRINT NONE 
/MATRIX OUT('lrcore.tmp')
/MEASURE=PH2 /STANDARDIZE=NONE /VIEW=VARIABLE .
ALSCAL
/MATRIX= IN('lrcore.tmp')
/LEVEL=ORDINAL
/CONDITION=MATRIX
/MODEL=EUCLID
/CRITERIA=CONVERGE(.001) STRESSMIN(.005) ITER(30) CUTOFF(0) DIMENS(2,2) 
/PLOT=DEFAULT 
/OUTFILE='stimuli_lrcore.tmp'.
ERASE FILE='lrcore.tmp'.

GET FILE='stimuli_lrcore.tmp' /KEEP=DIM1.

FLIP VARIABLES=DIM1.
RENAME VARIABLES (var001 to var010 = sc_lr_per413 sc_lr_per412 sc_lr_per404 sc_lr_per403 sc_lr_per601 sc_lr_per603 sc_lr_per606 sc_lr_per401 sc_lr_per414 sc_lr_per505).
compute hilfsid = 1 .
exe . 

SAVE OUTFILE='stimuli_lrcore.sav'
/keep hilfsid sc_lr_per413 sc_lr_per412 sc_lr_per404 sc_lr_per403 sc_lr_per601 sc_lr_per603 sc_lr_per606 sc_lr_per401 sc_lr_per414 sc_lr_per505 .
ERASE FILE='stimuli_lrcore.tmp' .
ERASE FILE='cmp_mds_lr.sav'.

GET FILE 'stimuli_lrcore.sav'.