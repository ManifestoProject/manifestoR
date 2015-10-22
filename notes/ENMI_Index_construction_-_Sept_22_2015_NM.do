#delimit;
set more off;

/*	****************************************************************	*/
/*     	File Name:					*/
/*     	Date:   	09 22 2015							*/
/*      Author: 	Zachary D. Greene					            */
/*      Purpose:	Create ENMI for MARPOR						*/
/*      Output File:	 								*/
/*												*/
/*	Directory:										*/
/*	****************************************************************	*/


*cd "C:\Users\kwb15146\Dropbox\data";

*import delimited "MPDataset_MPDS2015a.csv" , clear;
use "MPDataset_MPDS2015a.dta" , clear;


/* First, I transform the measure of the issue focus to reflect 
only codable data. In practice, I drop all uncodable portions
of the manifesto except for in Sweden and Norway b\c the data 
is missing in the CMP	*/

/*************************************/


 
replace peruncod=. if peruncod>99;

gen totalcod= total - (total*(peruncod/100)) if peruncod<99 | total<9999;


replace totalcod= total if peruncod>=99.99 & total <9999;
replace totalcod= 100-(100*peruncod/100) if total==9999;
replace totalcod= total if peruncod>=99.99 ;
replace total=100 if total==.;


/* Rescales the categories as a percentage of the codeable documents	*/

/* MAKES SURE THE PERCENTAGES ADD UP TO 100	*/
egen pertot= rowtotal(per101 per102 per103 per104 per105 per106 per107 per108 per109 per110 per201 per202 per203 per204
	per301 per302 per303 per304 per305
	per401 per402 per403 per404 per405 per406 per407 per408 per409 per410 per411 per412 per413 per414 per415 per416
	per501 per502 per503 per504 per505 per506 per507
	per601 per602 per603 per604 per605 per606 per607 per608
	per701 per702 per703 per704 per705 per706);

	

 forv i=101/110{;

 replace per`i'= ((per`i')/pertot);

	}
;

 forv i=201/204{;

replace per`i'= ((per`i')/pertot);

	}
;
 forv i=301/305{;

replace per`i'= ((per`i')/pertot);

	}
;
forv i=401/416{;
replace per`i'= ((per`i')/pertot);

	}
;
forv i=501/507{;
replace per`i'= ((per`i')/pertot);

	}
;
forv i=601/608{;

replace per`i'= ((per`i')/pertot);

}
;
forv i=701/706{;	
replace per`i'= ((per`i')/pertot);

}
;

/* A) - 7 issue dimensions from the CMP*/ 

	egen dim1 = rowtotal(per101 per102 per103 per104 per105 per106 per107 per108 per109 per110);
	egen dim2 = rowtotal(per201 per202 per203 per204);
	egen dim3 = rowtotal(per301 per302 per303 per304 per305);
	egen dim4 = rowtotal(per401 per402 per403 per404 per405 per406 per407 per408 per409 per410 per411 per412 per413 per414 per415 per416);
	egen dim5 = rowtotal(per501 per502 per503 per504 per505 per506 per507);
	egen dim6 = rowtotal(per601 per602 per603 per604 per605 per606 per607 per608);
	egen dim7 = rowtotal(per701 per702 per703 per704 per705 per706);

	/* Generates the measures for each issue dimension	*/
forv i=1/7{;


		gen dsqr`i'= (dim`i' * dim`i');
	gen sh_dim`i'	= (dim`i' )*(ln(dim`i')); 
}
;

egen d_fraction= rowtotal(dsqr*);

	gen en_dim = 1/ d_fraction;
		label var en_dim "Effective number of CMP dimensions - Herfindahl";

egen shannon_dim_mi= rowtotal(sh_dim1 sh_dim2 sh_dim3 sh_dim4 sh_dim5 sh_dim6 sh_dim7 );
		
	replace shannon_dim_mi= shannon_dim_mi*-1;
		gen ensh_dim_mi=exp(shannon_dim_mi);
	label var ensh_dim_mi "Effective number of CMP dimensions - Shannon's H";
		

/****generate the total effective number of issues combining the natural pairs	****/

gen i1= ((per102*total + per101*total)/total);
gen i2= ((per104*total + per105*total) / total);
gen i3= ((per109*total + per107*total)/total);
gen i4= ((per110*total + per108*total)/total);
gen i5= ((per204*total + per203*total)/total);
gen i6= ((per302*total + per301*total)/total);
gen i7= ((per406*total + per407*total)/total);
gen i8= ((per414*total + per409*total)/ total);
gen i9= ((per601*total + per602*total) /total);
gen i10= ((per603*total + per604*total) /total);
gen i11= ((per608*total + per607*total) /total);
gen i12= ((per702*total + per701*total) /total);
gen i13= ((per504*total + per505*total ) /total);
gen i14= ((per507*total + per506*total) /total); 


/* Prepare the issue codes to create the issue level Effective Number of Manifesto Issues variable	*/

foreach var of varlist i1 per103 i2 per106  i3 i4 per201 per202 i5 i6 per303 per304 per305 per401 per402 per403 per404 per405 i7
		 per408 i8 per410 per411 per412 per413  per415 per416 per501 per502 per503 i13 i14 
	i9 i10 per605 per606 i11 i12 per703 per704 per705 per706 {;

		gen sh_`var'= `var'*ln(`var'); 	
			replace sh_`var'=0 if `var'==0;
		gen enmi_`var'= `var'*`var';
}
;


		
/* Generates the ENMI measures for the main analysis	*/

	egen enmi=rowtotal(enmi_* );
replace enmi=(1/enmi);
	
	egen shannon_mi= rowtotal(sh_i1 sh_per103 sh_i2 sh_per106  sh_i3 sh_i4 sh_per201 sh_per202 sh_i5 sh_i6 sh_per303 sh_per304 sh_per305 sh_per401 
	sh_per402 sh_per403 sh_per404 sh_per405 sh_i7 sh_per408 sh_i8 sh_per410 sh_per411 sh_per412 sh_per413  sh_per415 sh_per416 sh_per501 sh_per502 
	sh_per503 sh_i13 sh_i14 sh_i9 sh_i10 sh_per605 sh_per606 sh_i11 sh_i12 sh_per703 sh_per704 sh_per705 sh_per706 );
	
	replace shannon_mi= shannon_mi*-1;
	
	gen ensh_mi=exp(shannon_mi);
		
label var enmi "Effective Number of Manifesto Issues";
	label var ensh_mi "Effective Number of Manifesto Issues from Shannon's H";

	
