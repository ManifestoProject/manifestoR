
**************************************Nicheness**************************************************
*************************************************************************************************
*************************************************************************************************

*normal values 
gen n_ecology = per416 + per410 + per501 + per106
gen n_nationalist = per601 + per602 + per605 + per607 + per608
gen n_agrarian = per703
gen n_regional = per301 + per302 + per706
gen n_europe = per406 + per407 + per108 + per110


*logged values
gen ecology_1 = log(per416 + per410 + per501 + per106+1)
gen nationalist_1 = log(per601 + per602 + per605 + per607 + per608+1)
gen agrarian_1 = log(per703+1)
gen regional_1 = log(per301 + per302 + per706+1)
gen europe_1 = log(per406 + per407 + per108 + per110+1)

*Lags
sort country party edate
bysort party: gen ecology = ((ecology_1 + ecology_1[_n-1])/2)
sort country party edate
bysort party: replace ecology = ecology_1 if _n == 1
sort country party edate
bysort party: gen nationalist = ((nationalist_1 + nationalist_1[_n-1])/2)
sort country party edate
bysort party: replace nationalist = nationalist_1 if _n == 1
sort country party edate
bysort party: gen agrarian = ((agrarian_1 + agrarian_1[_n-1])/2)
sort country party edate
bysort party: replace agrarian = agrarian_1 if _n == 1
sort country party edate
bysort party: gen regional = ((regional_1 + regional_1[_n-1])/2)
sort country party edate
bysort party: replace regional = regional_1 if _n == 1
sort country party edate
bysort party: gen europe = ((europe_1 + europe_1[_n-1])/2)
sort country party edate
bysort party: replace europe = europe_1 if _n == 1

*** change issues into percentages:
bysort party edate: gen sum_seg = ecology + nationalist + agrarian +  regional +  europe
gen seg_eco = ecology/sum_seg
gen seg_nat = nationalist/sum_seg
gen seg_agr = agrarian/sum_seg
gen seg_reg = regional/sum_seg
gen seg_eur = europe/sum_seg

***Shannon's Entropy:
gen diversification = ln(1/( ///
seg_eco^seg_eco * ///
seg_nat^seg_nat * ///
seg_agr^seg_agr * ///
seg_reg^seg_reg * ///
seg_eur^seg_eur))
replace diversification=0 if seg_eco==1 | seg_nat==1 | seg_agr==1 | seg_reg==1 | seg_eur==1
*gen spec_ex = exp(diversification)
egen max_divers = max(diversification)
egen min_divers = min(diversification)
gen specialization = (min_divers + max_divers) - diversification


*weighted dimensions:
sort country party edate
gen w_ecology = ecology
gen w_nationalist = nationalist
gen w_agrarian = agrarian
gen w_regional = regional 
gen w_europe = europe

*mean without party of interest
bysort country edate: gen N = _N
gen npwq = N-1
bysort country edate: egen t_ecology = total(w_ecology)
gen sumop_ecology = t_ecology-w_ecology
bysort country edate: gen mean_ecology = (sumop_ecology/npwq) 
bysort country edate: egen t_nationalist = total(w_nationalist)
gen sumop_nationalist = t_nationalist-w_nationalist
bysort country edate: gen mean_nationalist = (sumop_nationalist/npwq) 
bysort country edate: egen t_agrarian = total(w_agrarian)
gen sumop_agrarian = t_agrarian-w_agrarian
bysort country edate: gen mean_agrarian = (sumop_agrarian/npwq) 
bysort country edate: egen t_regional = total(w_regional)
gen sumop_regional = t_regional-w_regional
bysort country edate: gen mean_regional = (sumop_regional/npwq) 
bysort country edate: egen t_europe = total(w_europe)
gen sumop_europe = t_europe-w_europe
bysort country edate: gen mean_europe = (sumop_europe/npwq)

*Distance to weighted mean:
gen dis_ecology = ((ecology - mean_ecology)^2)
gen dis_nationalist = ((nationalist - mean_nationalist)^2)
gen dis_agrarian = ((agrarian - mean_agrarian)^2)
gen dis_regional = ((regional - mean_regional)^2)
gen dis_europe = ((europe - mean_europe)^2)

*Across countries
gen sum_distance =  dis_ecology + dis_nationalist + dis_agrarian + dis_regional + dis_europe
gen divide_distance = sum_distance/5
gen distance = sqrt(divide_distance)

*Per country:
bysort country edate: egen t_distance = total(distance)
gen top_distance = t_distance-distance
bysort country edate: gen mean_distance = (top_distance/npwq)
gen nicheness = distance - mean_distance

*Standardization
egen max_nic=max(nicheness)
egen min_nic=min(nicheness)
gen nicheness_stand = (nicheness-min_nic)/(max_nic-min_nic)
egen max_spec=max(specialization)
egen min_spec=min(specialization)
gen specialization_stand = (specialization-min_spec)/(max_spec-min_spec)
gen specialization_stand_two = specialization_stand
replace specialization_stand_two = 0 if specialization_stand_two==.

*two dimensions niche:
gen nicheness_two = nicheness_stand + specialization_stand_two



