# ### 
# ### stata script
#    
# ********************************************
#    *** MEDIAN VOTER CALCULATION (KIM & FORDING)
# ********************************************
#    
#    * drops parties with vote share == 0 as they do not matter for median voter calculcation
# drop if pervote == 0
# 
# * create rilerank variable
# bys country date: egen rilerank = rank(rile)
# 
# * generate pervote 2: merges parties from one coalition (same rile-score) to the same datapoint and adds up the pervote of the different rows
# gen pervote2 = pervote 
# sort country date rilerank pervote
# by country date: replace pervote2 = pervote2[_n-1] + pervote2[_n] if rile[_n]==rile[_n-1]
# by country date: replace pervote2 = 0 if rile[_n]== rile[_n+1]
# drop if pervote2==0 
# 
# *** calculate midpoints
# * kim & fording assumption
# by country date (rilerank), sort: gen midleft = (rile + rile[_n-1])/2
# mvencode midleft, mv(-100)
# by country date (rilerank), sort: gen midright = (rile + rile[_n+1])/2
# mvencode midright, mv(100)
# 
# * adjusted assumption
# by country date (rilerank), sort: gen midleftA = (rile + rile[_n-1])/2
# by country date (rilerank), sort: gen midrightA = (rile + rile[_n+1])/2
# by country date (rilerank), sort: replace midrightA = rile + (rile - midleftA) if rilerank == rilerank[_N]
# by country date (rilerank), sort: replace midleftA = rile - (midrightA - rile) if rilerank == 1
# 
# 
# *** Interval (W) (with Kim-Fording assumption) 
# gen voter_interval_width = midright-midleft
# 
# *** and ADJUSTED Kim-Fording assunption)
# gen voter_interval_widthA = midrightA-midleftA
# 
# *** Real Total percent:
#    by country date, sort: gen pertotal = sum(pervote2)
#    by country date, sort: replace pertotal = pertotal[_N]
#    
#    
#    *** Detect Median Party
#    sort country date rilerank 
#    by country date: generate medpart = _n if sum(pervote2[_n-1]) < pertotal/2
#    mvencode medpart, mv(0)
#    generate medparty = .
#    label variable medparty "Median Party"
#    label define medianparty 0 "not median party" 1 "median party"
#    label values medparty medianparty
#    
#    sort country date medpart
#    by country date: replace medparty = 1 if medpart==medpart[_N]
#    by country date: replace medparty = 0 if medpart<medpart[_N]
#    by country date: replace medpart = medpart[_N] 
#    
#    * Median with Kim-Fording assumption
#    bys country date (rilerank): gen median_voter = midleft + ((pertotal/2 - sum(pervote2[_n-1]))/ pervote2 ) * voter_interval_width if (sum(pervote2[_n-1]) < pertotal/2) 
#       
#       * Median with ADJUSTED Kim-Fording assumption
#    bys country date (rilerank): gen median_voter_adj = midleftA + ((pertotal/2 - sum(pervote2[_n-1]))/ pervote2 ) * voter_interval_widthA if (sum(pervote2[_n-1]) < pertotal/2) 
#       
#       by country date: replace median_voter = median_voter[medpart]
#       by country date: replace median_voter_adj = median_voter_adj[medpart]