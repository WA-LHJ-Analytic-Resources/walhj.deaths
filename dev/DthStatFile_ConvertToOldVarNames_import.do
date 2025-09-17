/*WASHINGTON STATE DEPARTMENT OF HEALTH - AUGUST, 2018

USE THIS DO FILE WITH WA DEATH STATISTICAL FILES TO
CONVERT NEW VARIABLE NAMES (2016 ONWARDS) TO OLD VARIABLE NAMES (PRE-2016)

WORKS WITH .DTA FILES CREATED BY IMPORTING .CSV INTO STATA.

IF YOU IMPORT THE CSV FILE DIRECTLY INTO STATA (INSTEAD OF USING STAT TRANSFER TO 
CONVERT THE FILE) THERE ARE NO UNDERSCORES OR SPACES E.G. "Age Unit" OR "Age_Unit".
IMPORTED DATA WILL SHOW THIS VARIABLE AS "ageunit".  

This do file is for use with death statistical files for 2016 and later.  Run
this do file to:
(1) convert new (WHALES) variable names to the old (2015 and prior)
variable names, and
(2) change the format of values for specific variables in 2016
onwards to match format in 2015 and older death files.


If you have any questions please contact chsdatafiles@doh.wa.gov*/ 

tostring statefilenumber, replace format (%10.0f)
rename statefilenumber certno

rename age_type ageunit
rename age ageunum
rename ageyears age

rename dateofbirth dob
tostring dateofbirthmonth, replace format (%02.0f)
rename dateofbirthmonth dob_mo
tostring dateofbirthday, replace format (%02.0f)
rename dateofbirthday dob_da
tostring dateofbirthyear, replace format (%04.0f)
rename dateofbirthyear dob_yr

rename dateofdeath dth_date
tostring dateofdeathmonth, replace format (%02.0f)
rename dateofdeathmonth dth_mo
tostring dateofdeathday, replace format (%02.0f)
rename dateofdeathday dth_da
tostring dateofdeathyear, replace format (%04.0f)
rename dateofdeathyear dth_yr
rename dateofdeathmodifier dod_modi

tostring timeofdeathhour, replace format (%02.0f)
rename timeofdeathhour dth_hour
tostring timeofdeathminute, replace format (%02.0f)
rename timeofdeathminute dth_min

rename State_of_birth_code statebir 
rename Birthplace_Country bcountry

tostring deathcountycitywacode, replace format (%04.0f)
rename deathcountycitywacode city_occ
rename deathcountywacode cnty_occ

gen deathstate2 = deathstate
replace deathstate2="WA" if deathstate2=="WASHINGTON"
rename deathstate2 st_occ 

rename deathzipcode zip_occ

rename Place_of_Death_type fac_type

rename deathfacility facility

rename armedforces armforce
rename maritalstatus married
rename education educ
rename education8orless edu_le8

rename occupation occ_lit
rename occupationmilham occ_sam

rename industry ind_lit 

rename informantrelationship informrl 

rename racewhite race_wht
rename raceblack race_blk
rename raceamerindianalaskan race_ami
rename raceasianindian race_asi
rename racechinese race_chi
rename racefilipino race_fil
rename racejapanese race_jap
rename racekorean race_kor
rename racevietnamese race_vie
rename raceotherasian race_oas
rename racehawaiian race_haw
rename raceguamanianorchamorro race_gua
rename racesamoan race_sam
rename raceotherpacificislander race_opi
rename raceother race_oth

tostring racesummary, replace format (%02.0f)
rename racesummary sum_race
rename bridgerace brg_race

rename hispanicno hisp_no 
rename hispanicmexican hisp_mex
rename hispanicpuertorican hisp_pr
rename hispaniccuban hisp_cub
rename hispanicother hisp_oth
tostring hispanicnchsbridge, replace format (%02.0f)
rename hispanicnchsbridge hisp
 
tostring residencecountycitywacode, replace format (%04.0f)
rename residencecountycitywacode city_res 

tostring residencecountywacode, replace format (%02.0f)
rename residencecountywacode cnty_res

gen residencestatefipscode2 = residencestatefipscode
replace residencestatefipscode2="48" if residencestatefipscode2=="WA"
rename residencestatefipscode2 st_res 
drop residencestatefipscode2

rename residencezip zipcode

rename residencetribalreservationcod res_trbc 

rename Disposition disptype

gen dispositiondate2 = dispositiondate
replace dispositiondate2 = "0"+ dispositiondate2 if substr(dispositiondate2, 2, 1)=="/"
replace dispositiondate2 = substr(dispositiondate2, 1, 3) + "0" + substr(dispositiondate2, 4, 6) if length(dispositiondate2)==9
replace dispositiondate2 = subinstr(dispositiondate2, "/", "",.)
gen disposdate = date(dispositiondate2, "MDY")
format disposdate %tdD_m_Y
rename disposdate dispdate
drop dispositiondate2 disposdate

rename dispositiondateyear disp_yr
rename dispositiondatemonth disp_mo
rename dispositiondateday disp_da

tostring certifierdesignation, replace
rename certifierdesignation attclass

replace mecoronerreferred = "1" if mecoronerreferred=="Y"
replace mecoronerreferred = "2" if mecoronerreferred=="N"
replace mecoronerreferred = "9" if mecoronerreferred=="U"
rename mecoronerreferred referred

rename acmenatureofinjuryflag1 injflg1
rename acmenatureofinjuryflag2 injflg2
rename acmenatureofinjuryflag3 injflg3
rename acmenatureofinjuryflag4 injflg4
rename acmenatureofinjuryflag5 injflg5
rename acmenatureofinjuryflag6 injflg6
rename acmenatureofinjuryflag7 injflg7
rename acmenatureofinjuryflag8 injflg8
rename acmenatureofinjuryflag9 injflg9
rename acmenatureofinjuryflag10 injflg10
rename acmenatureofinjuryflag11 injflg11
rename acmenatureofinjuryflag12 injflg12
rename acmenatureofinjuryflag13 injflg13
rename acmenatureofinjuryflag14 injflg14
rename acmenatureofinjuryflag15 injflg15
rename acmenatureofinjuryflag16 injflg16
rename acmenatureofinjuryflag17 injflg17
rename acmenatureofinjuryflag18 injflg18
rename acmenatureofinjuryflag19 injflg19
rename acmenatureofinjuryflag20 injflg20

rename underlyingcodcode underly

rename recordaxiscode1 mltcse1
rename recordaxiscode2 mltcse2
rename recordaxiscode3 mltcse3
rename recordaxiscode4 mltcse4
rename recordaxiscode5 mltcse5
rename recordaxiscode6 mltcse6
rename recordaxiscode7 mltcse7
rename recordaxiscode8 mltcse8
rename recordaxiscode9 mltcse9
rename recordaxiscode10 mltcse10
rename recordaxiscode11 mltcse11
rename recordaxiscode12 mltcse12
rename recordaxiscode13 mltcse13
rename recordaxiscode14 mltcse14
rename recordaxiscode15 mltcse15
rename recordaxiscode16 mltcse16
rename recordaxiscode17 mltcse17
rename recordaxiscode18 mltcse18
rename recordaxiscode19 mltcse19
rename recordaxiscode20 mltcse20


rename autopsyavailable autopava
rename pregnancy pregstat
rename tobacco tbcontri
rename manner rinj_caus
rename injurydate inj_date
rename localfilenumber cntyfile 
rename injuryplace injplace
rename injurycitywacode city_inj 
rename injurycountywacode cnty_inj 
rename injurystate st_inj 
rename injuryzipcode zipinjoc 
rename injuryatwork injatwrk
rename datereceived rcvdt

rename residencelength res_auni
rename residencelengthunits res_lena 
 
rename injurytransportation transinj 
rename injuryacmeplace injpnchs 
rename ageyears age 


rename injurydatemonth inj_mo
rename injurydateday inj_da
rename injurydateyear inj_yr
rename injurytimehour inj_hour
rename injurytimeminute inj_min

/* Convert all mltcse fields to string to allow annual files to be appended
to - sometimes, the last few mltcse fields are imported as numeric when the rest
are string leading to a problem with data manipulation later on*/

forvalues i = 1/20 {

  tostring mltcse`i', replace
  
}


/* Although the "smoking" variable is blank (replaced by tbcontri since 2004)
it is generated to avoid any problems with appending multiple years of data in 
Stata. Use "tbcontri" to analyze tobacco-related deaths.*/

gen smoking = . 
replace smoking = 0 


/*The following section of the do file recodes or reformats values for specific
variables to match values in 2015 and earlier death files*/

gen tbcontri1 = . 
replace tbcontri1 = 1 if tbcontri=="Y"
replace tbcontri1 = 2 if tbcontri=="N"
replace tbcontri1 = 7 if tbcontri=="P"
replace tbcontri1 = 9 if tbcontri=="U" 
drop tbcontri
rename tbcontri1 tbcontri

gen armforce1 = . 
replace armforce1 = 1 if armforce=="Y"
replace armforce1 = 2 if armforce=="N"
replace armforce1 = 9 if armforce=="U" 
drop armforce
rename armforce1 armforce

replace hisp_mex = "Y" if hisp_mex=="H" 

replace hisp_pr = "Y" if hisp_pr=="H" 

replace hisp_cub = "Y" if hisp_cub=="H" 

replace hisp_oth = "Y" if hisp_oth=="H" 


/*Ageunit and ageunum--converting to pre-2016 coding schema*/
recode ageunit 1=0
replace ageunit = 1 if age==100|age==101|age==102|age==103|age==104|age==105| ///
age==106|age==107|age==108|age==109|age==110

recode ageunum 100=0
recode ageunum 101=0
recode ageunum 102=0
recode ageunum 103=0
recode ageunum 104=0
recode ageunum 105=0
recode ageunum 106=0
recode ageunum 107=0 
recode ageunum 108=0 
recode ageunum 109=0
recode ageunum 110=0 

replace facility="000" if facility=="999"

/*Married--converting to pre-2016 coding schema*/
gen married1 = . 
replace married1 = 1 if married=="S"
replace married1 = 2 if married=="M"
replace married1 = 3 if married=="D"
replace married1 = 4 if married=="W"
replace married1 = 5 if married=="A"
replace married1 = 6 if married=="P"
replace married1 = 9 if married=="U"
drop married
rename married1 married
ta married

/*Autopsy--converting to pre-2016 coding schema*/
gen autopsy1 = . 
replace autopsy1 = 1 if Autopsy=="Y"
replace autopsy1 = 2 if Autopsy=="N"
replace autopsy1 = 9 if Autopsy=="U"
drop Autopsy
rename autopsy1 autopsy
ta autopsy

/*Cnty_Occ--converting values to 2 character string*/
tostring cnty_occ, replace
replace cnty_occ = "00" if cnty_occ=="0"
replace cnty_occ = "01" if cnty_occ=="1"
replace cnty_occ = "02" if cnty_occ=="2"
replace cnty_occ = "03" if cnty_occ=="3"
replace cnty_occ = "04" if cnty_occ=="4"
replace cnty_occ = "05" if cnty_occ=="5"
replace cnty_occ = "06" if cnty_occ=="6"
replace cnty_occ = "07" if cnty_occ=="7"
replace cnty_occ = "08" if cnty_occ=="8"
replace cnty_occ = "09" if cnty_occ=="9"


/*Statebir, st_occ, st_res, st_inj--converting all to 2 character 
(numerals as string) string variables*/

foreach x in statebir st_res st_occ st_inj {
	replace `x' = "01" if `x'=="AL" | `x'=="ALABAMA" 
	replace `x' = "02" if `x'=="AK" | `x'=="ALASKA" 
	replace `x' = "03" if `x'=="AZ" | `x'=="ARIZONA" 
	replace `x' = "04" if `x'=="AR" | `x'=="ARKANSAS" 
	replace `x' = "05" if `x'=="CA" | `x'=="CALIFORNIA" 
	replace `x' = "06" if `x'=="CO" | `x'=="COLORADO" 
	replace `x' = "07" if `x'=="CT" | `x'=="CONNECTICUT" 
	replace `x' = "08" if `x'=="DE" | `x'=="DELAWARE" 
	replace `x' = "09" if `x'=="DC" | `x'=="DISTRICT OF COLUMBIA" 
	replace `x' = "10" if `x'=="FL" | `x'=="FLORIDA" 
	replace `x' = "11" if `x'=="GA" | `x'=="GEORGIA" 
	replace `x' = "12" if `x'=="HI" | `x'=="HAWAII" 
	replace `x' = "13" if `x'=="ID" | `x'=="IDAHO" 
	replace `x' = "14" if `x'=="IL" | `x'=="ILLINOIS" 
	replace `x' = "15" if `x'=="IN" | `x'=="INDIANA" 
	replace `x' = "16" if `x'=="IA" | `x'=="IOWA" 
	replace `x' = "17" if `x'=="KS" | `x'=="KANSAS" 
	replace `x' = "18" if `x'=="KY" | `x'=="KENTUCKY" 
	replace `x' = "19" if `x'=="LA" | `x'=="LOUISIANA" 
	replace `x' = "20" if `x'=="ME" | `x'=="MAINE" 
	replace `x' = "21" if `x'=="MD" | `x'=="MARYLAND" 
	replace `x' = "22" if `x'=="MA" | `x'=="MASSACHUSETTS" 
	replace `x' = "23" if `x'=="MI" | `x'=="MICHIGAN" 
	replace `x' = "24" if `x'=="MN" | `x'=="MINNESOTA"
	replace `x' = "25" if `x'=="MS" | `x'=="MISSISSIPPI" 
	replace `x' = "26" if `x'=="MO" | `x'=="MISSOURI" 
	replace `x' = "27" if `x'=="MT" | `x'=="MONTANA" 
	replace `x' = "28" if `x'=="NE" | `x'=="NEBRASKA" 
	replace `x' = "29" if `x'=="NV" | `x'=="NEVADA"
	replace `x' = "30" if `x'=="NH" | `x'=="NEW HAMPSHIRE" 
	replace `x' = "31" if `x'=="NJ" | `x'=="NEW JERSEY" 
	replace `x' = "32" if `x'=="NM" | `x'=="NEW MEXICO" 
	replace `x' = "33" if `x'=="NY" | `x'=="NEW YORK" 
	replace `x' = "34" if `x'=="NC" | `x'=="NORTH CAROLINA"
	replace `x' = "35" if `x'=="ND" | `x'=="NORTH DAKOTA" 
	replace `x' = "36" if `x'=="OH" | `x'=="OHIO" 
	replace `x' = "37" if `x'=="OK" | `x'=="OKLAHOMA" 
	replace `x' = "38" if `x'=="OR" | `x'=="OREGON" 
	replace `x' = "39" if `x'=="PA" | `x'=="PENNSYLVANIA" 
	replace `x' = "40" if `x'=="RI" | `x'=="RHODE ISLAND" 
	replace `x' = "41" if `x'=="SC" | `x'=="SOUTH CAROLINA" 
	replace `x' = "42" if `x'=="SD" | `x'=="SOUTH DAKOTA" 
	replace `x' = "43" if `x'=="TN" | `x'=="TENNESSEE"
	replace `x' = "44" if `x'=="TX" | `x'=="TEXAS" 
	replace `x' = "45" if `x'=="UT" | `x'=="UTAH" 
	replace `x' = "46" if `x'=="VT" | `x'=="VERMONT" 
	replace `x' = "47" if `x'=="VA" | `x'=="VIRGINIA" 
	replace `x' = "48" if `x'=="WA" | `x'=="WASHINGTON" 
	replace `x' = "49" if `x'=="WV" | `x'=="WEST VIRGINIA" 
	replace `x' = "50" if `x'=="WI" | `x'=="WISCONSIN" 
	replace `x' = "51" if `x'=="WY" | `x'=="WYOMING" 
	replace `x' = "52" if `x'=="PR" | `x'=="PUERTO RICO"
	replace `x' = "53" if `x'=="VI" | `x'=="VIRGIN ISLANDS" 
	replace `x' = "54" if `x'=="GU" | `x'=="GUAM" 
	replace `x' = "55" if `x'=="CANADA" | `x'=="BRITISH COLUMBIA" | `x'=="BC" | ///
						  `x'=="AB" | `x'=="MB" | `x'=="NB" | `x'=="NL" | ///
						  `x'=="NS" | `x'=="NT" | `x'=="NU" | `x'=="ON" | ///
						  `x'=="PE" | `x'=="QC" | `x'=="SK" | `x'=="YT" | `x'=="NF" 
	replace `x' = "56" if `x'=="CUBA" 
	replace `x' = "57" if `x'=="MEXICO" 
	replace `x' = "59" if `x'=="ZZ" 
	replace `x' = "60" if `x'=="AS" | `x'=="AMERICAN SAMOA" 
	replace `x' = "69" if `x'=="MP" | `x'=="MARIANA ISLANDS" 
	replace `x' = "99" if `x'=="XX" | `x'=="NOT CLASSIFIABLE"
}

/*Cnty_Res--converting to 2 character string*/
tostring cnty_res, replace
replace cnty_res = "00" if cnty_res=="0"
replace cnty_res = "01" if cnty_res=="1"
replace cnty_res = "02" if cnty_res=="2"
replace cnty_res = "03" if cnty_res=="3"
replace cnty_res = "04" if cnty_res=="4"
replace cnty_res = "05" if cnty_res=="5"
replace cnty_res = "06" if cnty_res=="6"
replace cnty_res = "07" if cnty_res=="7"
replace cnty_res = "08" if cnty_res=="8"
replace cnty_res = "09" if cnty_res=="9"
ta cnty_res

gen inj_date1 = date(inj_date, "MDY")
format inj_date1 %td
drop inj_date
rename inj_date1 inj_date
format inj_date %tdCCYYNNDD

/*Injury Month--converting to 2 character string*/
replace inj_mo = "01" if inj_mo=="1"
replace inj_mo = "02" if inj_mo=="2"
replace inj_mo = "03" if inj_mo=="3"
replace inj_mo = "04" if inj_mo=="4"
replace inj_mo = "05" if inj_mo=="5"
replace inj_mo = "06" if inj_mo=="6"
replace inj_mo = "07" if inj_mo=="7"
replace inj_mo = "08" if inj_mo=="8"
replace inj_mo = "09" if inj_mo=="9"
tab inj_mo

/*Injury Day--converting to 2 character string*/
replace inj_da = "01" if inj_da=="1"
replace inj_da = "02" if inj_da=="2"
replace inj_da = "03" if inj_da=="3"
replace inj_da = "04" if inj_da=="4"
replace inj_da = "05" if inj_da=="5"
replace inj_da = "06" if inj_da=="6"
replace inj_da = "07" if inj_da=="7"
replace inj_da = "08" if inj_da=="8"
replace inj_da = "09" if inj_da=="9"
tab inj_da 


gen injatwrk1 = ""
replace injatwrk1 = "1" if injatwrk=="Y"
replace injatwrk1 = "2" if injatwrk=="N"
replace injatwrk1 = "9" if injatwrk=="U"|injatwrk=="X"
tab injatwrk1
drop injatwrk
rename injatwrk1 injatwrk

/*Cnty_inj--converting to 2 character string*/
tostring cnty_inj, replace
replace cnty_inj = "01" if cnty_inj=="1"
replace cnty_inj = "02" if cnty_inj=="2"
replace cnty_inj = "03" if cnty_inj=="3"
replace cnty_inj = "04" if cnty_inj=="4"
replace cnty_inj = "05" if cnty_inj=="5"
replace cnty_inj = "06" if cnty_inj=="6"
replace cnty_inj = "07" if cnty_inj=="7"
replace cnty_inj = "08" if cnty_inj=="8"
replace cnty_inj = "09" if cnty_inj=="9"
tab cnty_inj 


/*St_occ--collapsing all out of state variables into 90 (US not specified)*/
gen st_occ1 = st_occ
replace st_occ1 = "90" if st_occ1 ~= "48"
drop st_occ
rename st_occ1 st_occ
ta st_occ

/*Disptype-- changing to match pre-2016 coding schema*/
replace disptype = "1" if disptype=="B"
replace disptype = "2" if disptype=="C"
replace disptype = "3" if disptype=="R"
replace disptype = "4" if disptype=="D"
replace disptype = "5" if disptype=="N"
replace disptype = "6" if disptype=="E"
replace disptype = "7" if disptype=="O"
replace disptype = "9" if disptype=="U"
tab disptype

/*Referred--changing to match pre-2016 coding schema*/
replace referred = "1" if referred=="Y"
replace referred = "2" if referred=="N"
replace referred = "9" if referred=="U" 
tab referred

/*Res_lena--changing to match pre-2016 coding schema*/
replace res_lena = "0" if res_lena=="Y"
replace res_lena = "2" if res_lena=="M"
replace res_lena = "4" if res_lena=="D"
replace res_lena = "5" if res_lena=="H"
replace res_lena = "6" if res_lena=="N"
replace res_lena = "9" if res_lena=="U"
tab res_lena

/*Autopava--changing to match pre-2016 coding schema*/
replace autopava = "1" if autopava=="Y"
replace autopava = "2" if autopava=="N"
replace autopava = "8" if autopava=="X"
replace autopava = "9" if autopava=="U"
tab autopava

/*Transinj--changing to match pre-2016 coding schema*/

replace transinj = "1" if transinj=="DR"|transinj=="DRIVER/OPERATOR"
replace transinj = "2" if transinj=="PA"|transinj=="PASSENGER"
replace transinj = "3" if transinj=="PE"|transinj=="PEDESTRIAN"
replace transinj = "4" if transinj=="OTHER"
tab transinj, m

/*rinj_caus--changing to match pre-2016 coding schema*/
replace rinj_caus = "1" if rinj_caus=="N"
replace rinj_caus = "2" if rinj_caus=="A"
replace rinj_caus = "3" if rinj_caus=="S"
replace rinj_caus = "4" if rinj_caus=="H"
replace rinj_caus = "5" if rinj_caus=="C"
replace rinj_caus = "6" if rinj_caus=="P"
tab rinj_caus

/*brg_race--changing to match pre-2016 coding schema*/ 
tostring brg_race, replace
replace brg_race = "00" if brg_race=="0"
replace brg_race = "01" if brg_race=="1"
replace brg_race = "02" if brg_race=="2"
replace brg_race = "03" if brg_race=="3"
replace brg_race = "04" if brg_race=="4"
replace brg_race = "05" if brg_race=="5"
replace brg_race = "06" if brg_race=="6"
replace brg_race = "07" if brg_race=="7"
replace brg_race = "08" if brg_race=="8"
replace brg_race = "09" if brg_race=="9"
tab brg_race

