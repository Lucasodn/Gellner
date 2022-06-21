
clear matrix
clear mata
clear
set rmsg on
set maxvar 10000
set matsize 5000

ssc install use13
ssc install ivreg2
ssc install psmatch2
ssc install xtivreg2


*******************************************************************************************************************************************************************************************

*** Globals defining control variables for the cross sectional analysis ***

global controls street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military ///
fac1849_total2_pc county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1

global controls2 street1848_dummy ship1849_dummy delta16_31 ln_pop1849civil ln_pop1849military ///
fac1849_total2_pc county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1

global controls_nolag street1848_dummy ship1849_dummy ln_pop1849civil ln_pop1849military ///
fac1849_total2_pc county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1

global controls2_nolag street1848_dummy ship1849_dummy ln_pop1849civil ln_pop1849military ///
fac1849_total2_pc county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1

global precontrols ln_pop1837civil delta21_37 occ1819_merch_pc loom1819_city_pc pop1816_prot_pc ///
buil1821_home_city_pc buil1821_fac_city_pc ln_assu1821_value_city_ph

global deltas delta16_21 delta21_31 delta31_37 delta37_40 delta40_43 delta43_46 delta46_49 ///
delta49_52 delta52_55 delta55_58 delta58_61 delta61_64 delta64_67 delta67_71

global faccontrols street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military ///
county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1

global cwrcontrols street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military ///
fac1849_total2_pc county_mining county_landownership edu1849_pri_enrol dist1

*******************************************************************************************************************************************************************************************

use13 hornung-rail-cross-section.dta, clear

***	Table 2: Annual Population Growth Rates by Railroad Access Status in 1848 ***

foreach delta in $deltas{
sum `delta' if abs(`delta')<0.1 & rail1848==1 & node1848==1
sum `delta' if abs(`delta')<0.1 & rail1848==1 & node1848==0
sum `delta' if abs(`delta')<0.1 & rail1848==0 & node1848==0
ttest `delta' if abs(`delta')<0.1 & node1848==0, by(rail1848) level(99)
}

*** Table 3: Control Variables (post Railroad) by Access Status in 1848 ***

foreach controlvar in slc1848 $controls{
sum `controlvar' if rail1848==1 & node1848==1
sum `controlvar' if rail1848==1 & node1848==0
sum `controlvar' if rail1848==0 & node1848==0
ttest `controlvar' if node1848==0, by(rail1848) level(99)
}

*** Table 4: Matching Variables (pre Railroad) by Access Status in 1848 ***

foreach matchvar in $precontrols{
sum `matchvar' if rail1848==1 & node1848==1
sum `matchvar' if rail1848==1 & node1848==0
sum `matchvar' if rail1848==0 & node1848==0
ttest `matchvar' if node1848==0, by(rail1848) level(99)
}

*******************************************************************************************************************************************************************************************

***	Table 5: The Impact of Railroad Access - Cross-sectional Estimates ***
use13 hornung-rail-cross-section.dta, clear

***	Panel A: OLS - Population growth rate and actual railroad access. ***

reg delta31_37 rail1848 $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1871, cluster(kreiskey1849)
reg delta49_52 rail1848 addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
reg delta52_55 rail1848 addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
reg delta55_58 rail1848 addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
reg delta58_61 rail1848 addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
reg delta61_64 rail1848 addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
reg delta64_67 rail1848 addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
reg delta67_71 rail1848 addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

***	Panel B: IV First stage - Actual railroad access and location within straight-line corridor. ***

reg rail1848 slc1848 $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
reg rail1848 slc1848 addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
reg rail1848 slc1848 addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
reg rail1848 slc1848 addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
reg rail1848 slc1848 addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
reg rail1848 slc1848 addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
reg rail1848 slc1848 addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
reg rail1848 slc1848 addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
reg rail1848 slc1848 addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

***	Panel C: IV Second stage - Population growth rate and actual railroad access. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

***	Panel D: Reduced form - Population growth rate and location within straight-line corridor. ***

reg delta31_37 slc1848 $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
reg delta49_71 slc1848 addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
reg delta49_52 slc1848 addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
reg delta52_55 slc1848 addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
reg delta55_58 slc1848 addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
reg delta58_61 slc1848 addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
reg delta61_64 slc1848 addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
reg delta64_67 slc1848 addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
reg delta67_71 slc1848 addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*******************************************************************************************************************************************************************************************

*** Table 6: Testing for Plausibility of the Exclusion Restriction ***
use13 hornung-rail-cross-section.dta, clear

*** Columns 1 and 2: Full sample ***

foreach var in $controls $precontrols {
reg slc1848 `var' if node1848==0, cluster(kreiskey1849)
}

*** Columns 3 and 4: Matched sample ***

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_71)<0.1 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(delta49_71)
foreach var in $controls $precontrols {
reg slc1848 `var' if node1848==0 [pweight=_weight], cluster(kreiskey1849)
}

*******************************************************************************************************************************************************************************************

*** Table 7: Descriptive Statistics after Propensity Score Matching  ***
use13 hornung-rail-cross-section.dta, clear

*** Panel A: Radius matched sample  ***

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_71)<0.1 & addrail71==0 & areachange2!=1871, radius caliper(0.001) common logit out(delta49_71)

foreach matchvar in delta49_71 $precontrols{
mean `matchvar' [pweight=_weight], over (rail1848)
reg `matchvar' rail1848 [pweight=_weight]
}


*** Panel B: Kernel matched sample  ***

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_71)<0.1 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(delta49_71)

foreach matchvar in delta49_71 $precontrols{
mean `matchvar' [pweight=_weight], over (rail1848)
reg `matchvar' rail1848 [pweight=_weight]
}


*******************************************************************************************************************************************************************************************


*** Table 8: The Impact of Railroad Access - Cross-sectional IV-Estimations in Matched Sample  ***
use13 hornung-rail-cross-section.dta, clear

*** Panel A: IV estimates - Sample and weights obtained from PSM on pre-railroad variables (radius: caliper=0.001)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta31_37)<0.1, radius caliper(0.001) common logit out(delta31_37)
ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==. [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_71)<0.1 & addrail71==0 & areachange2!=1871, radius caliper(0.001) common logit out(delta49_71)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2!=1871 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_52)<0.1 & addrail52==0 & areachange2!=1852, radius caliper(0.001) common logit out(delta49_52)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta52_55)<0.1 & addrail55==0 & areachange2!=1855, radius caliper(0.001) common logit out(delta52_55)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta55_58)<0.1 & addrail58==0 & areachange2!=1858, radius caliper(0.001) common logit out(delta55_58)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta58_61)<0.1 & addrail61==0 & areachange2!=1861, radius caliper(0.001) common logit out(delta58_61)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta61_64)<0.1 & addrail64==0 & areachange2!=1864, radius caliper(0.001) common logit out(delta61_64)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta64_67)<0.1 & addrail67==0 & areachange2!=1867, radius caliper(0.001) common logit out(delta64_67)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta67_71)<0.1 & addrail71==0 & areachange2!=1871, radius caliper(0.001) common logit out(delta67_71)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871 [pweight=_weight], cluster(kreiskey1849)


*** Panel B: IV estimates - Sample and weights obtained from PSM on pre-railroad variables (kernel).

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta31_37)<0.1, kernel kerneltype(normal) common logit out(delta31_37)
ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==. [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_71)<0.1 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(delta49_71)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2!=1871 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta49_52)<0.1 & addrail52==0 & areachange2!=1852, kernel kerneltype(normal) common logit out(delta49_52)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta52_55)<0.1 & addrail55==0 & areachange2!=1855, kernel kerneltype(normal) common logit out(delta52_55)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta55_58)<0.1 & addrail58==0 & areachange2!=1858, kernel kerneltype(normal) common logit out(delta55_58)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta58_61)<0.1 & addrail61==0 & areachange2!=1861, kernel kerneltype(normal) common logit out(delta58_61)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta61_64)<0.1 & addrail64==0 & areachange2!=1864, kernel kerneltype(normal) common logit out(delta61_64)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta64_67)<0.1 & addrail67==0 & areachange2!=1867, kernel kerneltype(normal) common logit out(delta64_67)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 $precontrols if node1848==0 & abs(delta67_71)<0.1 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(delta67_71)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871 [pweight=_weight], cluster(kreiskey1849)


*** Panel C: IV estimates - Sample and weights obtained from PSM on pre-railroad variables and geography (kernel).

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta31_37)<0.1, kernel kerneltype(normal) common logit out(delta31_37)
ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==. [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta49_71)<0.1 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(delta49_71)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2!=1871 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta49_52)<0.1 & addrail52==0 & areachange2!=1852, kernel kerneltype(normal) common logit out(delta49_52)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta52_55)<0.1 & addrail55==0 & areachange2!=1855, kernel kerneltype(normal) common logit out(delta52_55)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta55_58)<0.1 & addrail58==0 & areachange2!=1858, kernel kerneltype(normal) common logit out(delta55_58)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta58_61)<0.1 & addrail61==0 & areachange2!=1861, kernel kerneltype(normal) common logit out(delta58_61)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta61_64)<0.1 & addrail64==0 & areachange2!=1864, kernel kerneltype(normal) common logit out(delta61_64)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta64_67)<0.1 & addrail67==0 & areachange2!=1867, kernel kerneltype(normal) common logit out(delta64_67)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867 [pweight=_weight], cluster(kreiskey1849)

psmatch2 rail1848 longitude latitude $precontrols if node1848==0 & abs(delta67_71)<0.1 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(delta67_71)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871 [pweight=_weight], cluster(kreiskey1849)


*******************************************************************************************************************************************************************************************

*** Table 9: The Impact of Railroad Access - Panel Estimates ***
use13 hornung-rail-panel-city.dta, clear
xtset townkey1849 year

gen ln_popcivil=ln(popcivil)
replace popmilitary=popmilitary+0.5
gen ln_popmilitary=ln(popmilitary)

bysort townkey1849: gen delta_civil=((popcivil/(popcivil[_n-1]))-1)/3 if year<1871

label var delta_civil 	"Change in city size"

gen small_town3000=0
bysort townkey1849: replace small_town3000=1 if popcivil<3001 & year==1837
bysort townkey1849: replace small_town3000=1 if small_town3000[1]==1

gen big_town5000=0
bysort townkey1849: replace big_town5000=1 if popcivil>5000 & year==1837
bysort townkey1849: replace big_town5000=1 if big_town5000[1]==1

label var ln_popcivil 		"(ln) Civilian population"
label var ln_popmilitary 	"(ln) Military population"
label var delta_civil 		"Change in city size"
label var small_town3000 	"Small city in 1837"
label var big_town5000 		"Large city in 1837"

xtreg ln_popcivil railaccess ln_popmilitary distance areachange i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849)
xtreg ln_popcivil railaccess ln_popmilitary distance areachange i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xi: xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year*i.kreiskey1849 if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year if townkey1849<24100 & abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year if townkey1849>24100 & abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year if within15km_1861==1 & abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year if small_town3000==1 & abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess ln_popmilitary distance areachange node i.year if big_town5000==1 & abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil railaccess stateown stateadmin ln_popmilitary distance areachange node i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xi: xtivreg2 ln_popcivil (railaccess=slc) ln_popmilitary distance areachange node i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe first

*******************************************************************************************************************************************************************************************

*** Table 10: Year of Establishment and the Profitabilty of Railroad Lines ***
use13 hornung-rail-roi.dta, clear

forvalues num = 1/9 {
reg roi_year`num' railyear, r
}

*******************************************************************************************************************************************************************************************

*** Table 11: The Impact of Railroad Access - Non-linear Panel Estimates ***
use13 hornung-rail-panel-city.dta, clear
xtset townkey1849 year

gen ln_popcivil=ln(popcivil)
replace popmilitary=popmilitary+0.5
gen ln_popmilitary=ln(popmilitary)

bysort townkey1849: gen delta_civil=((popcivil/(popcivil[_n-1]))-1)/3 if year<1871

label var delta_civil 	"Change in city size"

gen small_town3000=0
bysort townkey1849: replace small_town3000=1 if popcivil<3001 & year==1837
bysort townkey1849: replace small_town3000=1 if small_town3000[1]==1

label var ln_popcivil 		"(ln) Civilian population"
label var ln_popmilitary 	"(ln) Military population"
label var delta_civil 		"Change in city size"
label var small_town3000 	"Small city in 1837"

xtset townkey1849 year

gen access1=0
bysort  townkey1849: replace access1=1 if railaccess!= railaccess[_n-1]
replace access1=0 if railaccess==0
bysort  townkey1849: gen access2=access1[_n-1]
bysort  townkey1849: gen access3=access1[_n-2]
bysort  townkey1849: gen access4=access1[_n-3]
bysort  townkey1849: gen access5=access1[_n-4]
bysort  townkey1849: gen access6=access1[_n-5]
bysort  townkey1849: gen access7=access1[_n-6]
bysort  townkey1849: gen access8=access1[_n-7]
bysort  townkey1849: gen access_m6 = access6+access7+access8
bysort  townkey1849: gen preaccess1=access1[_n+1]
bysort  townkey1849: gen preaccess2=access1[_n+2]
bysort  townkey1849: gen preaccess3=access1[_n+3]
bysort  townkey1849: gen preaccess4=access1[_n+4]
bysort  townkey1849: gen preaccess5=access1[_n+5]
recode access* (.=0)
recode pre* (.=0)

bysort townkey1849: replace slc=slc[_n-1] if slc==.
gen slc1=0
bysort townkey1849: replace slc1=1 if slc!= slc[_n-1]
replace slc1=0 if slc==0
bysort  townkey1849: gen slc2=slc1[_n-1]
bysort  townkey1849: gen slc3=slc1[_n-2]
bysort  townkey1849: gen slc4=slc1[_n-3]
bysort  townkey1849: gen slc5=slc1[_n-4]
bysort  townkey1849: gen slc6=slc1[_n-5]
bysort  townkey1849: gen slc7=slc1[_n-6]
bysort  townkey1849: gen slc8=slc1[_n-7]
bysort  townkey1849: gen slc_m6 = slc6+slc7+slc8
bysort  townkey1849: gen preslc1=slc1[_n+1]
bysort  townkey1849: gen preslc2=slc1[_n+2]
bysort  townkey1849: gen preslc3=slc1[_n+3]
bysort  townkey1849: gen preslc4=slc1[_n+4]
bysort  townkey1849: gen preslc5=slc1[_n+5]
recode slc* (.=0)
recode preslc* (.=0)

label var access1	"Access for 1 period"
label var access2	"Access for 2 periods"
label var access3	"Access for 3 periods"
label var access4	"Access for 4 periods"
label var access5	"Access for 5 periods"
label var access6	"Access for 6 periods"
label var access7	"Access for 7 periods"
label var access8	"Access for 8 periods"
label var access_m6	"Access for more than 5 periods"

label var preaccess1	"1 period prior to access"
label var preaccess2	"2 periods prior to access"
label var preaccess3	"3 periods prior to access"
label var preaccess4	"4 periods prior to access"
label var preaccess5	"5 periods prior to access"

xtreg ln_popcivil preaccess5 preaccess4 preaccess3 preaccess2 access1-access5 access_m6 ln_popmilitary distance areachange i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil preaccess5 preaccess4 preaccess3 preaccess2 access1-access5 access_m6 ln_popmilitary distance areachange node i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xtreg ln_popcivil preaccess5 preaccess4 preaccess3 preaccess2 access1-access5 access_m6 ln_popmilitary distance areachange node i.year if abs(delta_civil)<0.1 & year<1864 & small_town3000==1 , cluster(kreiskey1849) fe
xtreg ln_popcivil preaccess5 preaccess4 preaccess3 preaccess2 access1-access5 access_m6 ln_popmilitary distance areachange node i.year if within15km_1861==1 & abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe
xi: xtivreg2 ln_popcivil (preaccess5 preaccess4 preaccess3 preaccess2 access1-access5 access_m6 = preslc5 preslc4 preslc3 preslc2 slc1 slc2 slc3 slc4 slc5 slc_m6) ln_popmilitary distance areachange node i.year if abs(delta_civil)<0.1 & year<1864, cluster(kreiskey1849) fe


*******************************************************************************************************************************************************************************************

*** Table 12: Railroads, Industrial Development and Sources of Population Growth ***
use13 hornung-rail-cross-section.dta, clear

*** Firm Size ***

reg ln_fac_size rail1848 addrail71 $faccontrols if node1848==0, cluster(kreiskey1849)
ivreg2 ln_fac_size (rail1848=slc1848) addrail71 $faccontrols if node1848==0, cluster(kreiskey1849)
psmatch2 rail1848 $precontrols if node1848==0 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(ln_fac_size)
ivreg2 ln_fac_size (rail1848=slc1848) addrail71 $faccontrols if node1848==0 [pweight=_weight], cluster(kreiskey1849)

*** Number of firms ***
 
reg ln_no_fac rail1848 addrail71 $faccontrols if node1848==0, cluster(kreiskey1849)
ivreg2 ln_no_fac (rail1848=slc1848) addrail71 $faccontrols if node1848==0, cluster(kreiskey1849)
psmatch2 rail1848 $precontrols if node1848==0 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(ln_no_fac)
ivreg2 ln_no_fac (rail1848=slc1848) addrail71 $faccontrols if node1848==0 [pweight=_weight], cluster(kreiskey1849)

*** Child woman ratio ***

reg cwr rail1848 addrail71 $cwrcontrols if node1848==0, cluster(kreiskey1849)
ivreg2 cwr (rail1848=slc1848) addrail71 $cwrcontrols if node1848==0, cluster(kreiskey1849)
psmatch2 rail1848 $precontrols if node1848==0 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(cwr)
ivreg2 cwr (rail1848=slc1848) addrail71 $cwrcontrols if node1848==0 [pweight=_weight], cluster(kreiskey1849)

*** Born in city ***

reg migration1871_pc rail1848 addrail71 $controls if node1848==0, cluster(kreiskey1849)
ivreg2 migration1871_pc (rail1848=slc1848) addrail71 $controls if node1848==0, cluster(kreiskey1849)
psmatch2 rail1848 $precontrols if node1848==0 & addrail71==0 & areachange2!=1871, kernel kerneltype(normal) common logit out(migration1871_pc)
ivreg2 migration1871_pc (rail1848=slc1848) addrail71 $controls if node1848==0 [pweight=_weight], cluster(kreiskey1849)

*******************************************************************************************************************************************************************************************

*** Table 13: Railroads and Urbanization in a Panel of Counties ***
use13 hornung-rail-panel-county.dta, clear
xtset kreiskey1849 year

*** Railroad-stations ***

xtreg pop_urban_pc rail_cities indu_pc agri_pc pop_young_old land_pc land_bigestates edu_pri_enrol rel_prot_pc dist_node i.year, cluster(regbez1849) fe
xi: xtivreg2 pop_urban_pc (rail_cities=slc_cities) indu_pc agri_pc pop_young_old land_pc land_bigestates edu_pri_enrol rel_prot_pc dist_node i.year, cluster(regbez1849) fe

*** Railroad-population ***

xtreg pop_urban_pc rail_pop indu_pc agri_pc pop_young_old land_pc land_bigestates edu_pri_enrol rel_prot_pc dist_node i.year, cluster(regbez1849) fe
xi: xtivreg2 pop_urban_pc (rail_pop=slc_pop) indu_pc agri_pc pop_young_old land_pc land_bigestates edu_pri_enrol rel_prot_pc dist_node i.year, cluster(regbez1849) fe


*******************************************************************************************************************************************************************************************

*** Table A-VI: Railroad Access and Growth in a Cross Section of Cities ***
use13 hornung-rail-cross-section.dta, clear

reg delta49_71 rail1848, cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71, cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 street1848_dummy ship1849_dummy, cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military, cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military fac1849_total2_pc county_mining county_landownership, cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military fac1849_total2_pc county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1, cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 street1848_dummy ship1849_dummy delta31_37 ln_pop1849civil ln_pop1849military fac1849_total2_pc county_mining county_landownership pop1849_young_pc edu1849_pri_enrol dist1 if abs(delta49_71)<0.1 & areachange1==., cluster(kreiskey1849)

*******************************************************************************************************************************************************************************************

*** Table A-VII: Railroad Access and Growth in Different Periods ***
use13 hornung-rail-cross-section.dta, clear

reg delta31_37 rail1848 $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
reg delta49_71 rail1848 addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1871, cluster(kreiskey1849)
reg delta49_52 rail1848 addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
reg delta52_55 rail1848 addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
reg delta55_58 rail1848 addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
reg delta58_61 rail1848 addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
reg delta61_64 rail1848 addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
reg delta64_67 rail1848 addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
reg delta67_71 rail1848 addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*******************************************************************************************************************************************************************************************

*** Table A-VIII: Placebo Lines ***
use13 hornung-rail-cross-section.dta, clear

gen placebo=0
replace placebo=1 if railyear>1871 & railyear<1885 & railyear!=.

gen delta71_80=(ln(pop1880civil)-ln(pop1871civil))/(1880-1871)

reg delta31_37 placebo rail1848 $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
reg delta49_52 placebo rail1851 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
reg delta52_55 placebo rail1854 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
reg delta55_58 placebo rail1857 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
reg delta58_61 placebo rail1860 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
reg delta61_64 placebo rail1863 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
reg delta64_67 placebo rail1866 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
reg delta67_71 placebo rail1870 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)
reg delta71_80 placebo rail1870 $controls if abs(delta71_80)<0.1 & node1848==0 & areachange2!=1880, cluster(kreiskey1849)


*******************************************************************************************************************************************************************************************

*** Table A-IX: Further Robustness Checks in the Cross Section ***
use13 hornung-rail-cross-section.dta, clear

*** Panel A: IV estimates - Full sample including nodes and outliers. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if areachange2!=1871, cluster(kreiskey1849)

*** Panel B: IV estimates - Sample including nodes but excluding implausible outliers (growth rates is >+/- 10%). ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & areachange2!=1871, cluster(kreiskey1849)

*** Panel C: IV estimates - Sample excluding nodes and outliers (standardized residual is >+/-2.58). ***

reg delta31_37 delta49_52 delta52_55 delta55_58 delta58_61 delta61_64 delta64_67 delta67_71
predict rstand37, rstandard
reg delta49_52 delta52_55 delta55_58 delta58_61 delta61_64 delta64_67 delta67_71
predict rstand52, rstandard
reg delta52_55 delta49_52 delta55_58 delta58_61 delta61_64 delta64_67 delta67_71
predict rstand55, rstandard
reg delta55_58 delta49_52 delta52_55 delta58_61 delta61_64 delta64_67 delta67_71
predict rstand58, rstandard
reg delta58_61 delta49_52 delta52_55 delta55_58 delta61_64 delta64_67 delta67_71
predict rstand61, rstandard
reg delta61_64 delta49_52 delta52_55 delta55_58 delta58_61 delta64_67 delta67_71
predict rstand64, rstandard
reg delta64_67 delta49_52 delta52_55 delta55_58 delta58_61 delta61_64 delta67_71
predict rstand67, rstandard
reg delta67_71 delta49_52 delta52_55 delta55_58 delta58_61 delta61_64 delta64_67 
predict rstand71, rstandard

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(rstand37)<2.58 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(rstand52)<2.58 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(rstand55)<2.58 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(rstand58)<2.58 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(rstand61)<2.58 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(rstand64)<2.58 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(rstand67)<2.58 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(rstand71)<2.58 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*** Panel D: IV estimates - Baseline estimates without lagged dependent variable. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2_nolag if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls_nolag if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls_nolag if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls_nolag if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls_nolag if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls_nolag if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls_nolag if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls_nolag if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls_nolag if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*** Panel E: IV estimates - Controlling for county-level soil texture as proxy for geographical endowments. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls soil_clay_pc soil_loam_pc soil_sand_pc if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*** Panel F: IV estimates - Controlling for Average County-level Crop Prices 1838-1860. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 price183760_wheat price183760_rye price183760_potatoe if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls price183760_wheat price183760_rye price183760_potatoe if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*** Panel G: IV estimates using a sample of cities smaller than 3000 inhabitants in 1837. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==. & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867 & pop1837civil<3000, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871 & pop1837civil<3000, cluster(kreiskey1849)

*** Panel H: IV estimates using a sample and weights obtained from geographical matching. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==. & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871 & geo_match_sample==1 [fweight=geo_match_weight], cluster(kreiskey1849)


*** Panel I: IV estimates using a sample of cities within 15 km distance to the straight line corridor. ***

ivreg2 delta31_37 (rail1848=slc1848) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==. & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867 & slc1848_15km==1, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871 & slc1848_15km==1, cluster(kreiskey1849)


*******************************************************************************************************************************************************************************************

*** Figure A-V: Results from IV Estimates Using Different Corridor Sizes ***
use13 hornung-rail-cross-section.dta, clear

ivreg2 delta49_71 (rail1848=slc1848_1km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848_2km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848_3km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848_10km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848_15km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848_20km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)

*******************************************************************************************************************************************************************************************

*** Table A-X: Alternative Instruments ***
use13 hornung-rail-cross-section.dta, clear

*** Panel A: Second stage estimates - Actual railroad access instrumented by SLC location (according to List's railroad plan). ***

ivreg2 delta31_37 (rail1848=slc_list_1_5km) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc_list_1_5km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc_list_1_5km) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc_list_1_5km) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc_list_1_5km) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc_list_1_5km) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc_list_1_5km) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc_list_1_5km) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc_list_1_5km) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*** Panel B: Second stage estimates - Actual railroad access instrumented by Least Cost Path location. ***

ivreg2 delta31_37 (rail1848=slc1848_lcp_1_5km) $controls2 if abs(delta31_37)<0.1 & node1848==0 & areachange1==., cluster(kreiskey1849)
ivreg2 delta49_71 (rail1848=slc1848_lcp_1_5km) addrail71 $controls if abs(delta49_71)<0.1 & node1848==0 & areachange2>1872, cluster(kreiskey1849)
ivreg2 delta49_52 (rail1848=slc1848_lcp_1_5km) addrail52 $controls if abs(delta49_52)<0.1 & node1848==0 & areachange2!=1852, cluster(kreiskey1849)
ivreg2 delta52_55 (rail1848=slc1848_lcp_1_5km) addrail55 $controls if abs(delta52_55)<0.1 & node1848==0 & areachange2!=1855, cluster(kreiskey1849)
ivreg2 delta55_58 (rail1848=slc1848_lcp_1_5km) addrail58 $controls if abs(delta55_58)<0.1 & node1848==0 & areachange2!=1858, cluster(kreiskey1849)
ivreg2 delta58_61 (rail1848=slc1848_lcp_1_5km) addrail61 $controls if abs(delta58_61)<0.1 & node1848==0 & areachange2!=1861, cluster(kreiskey1849)
ivreg2 delta61_64 (rail1848=slc1848_lcp_1_5km) addrail64 $controls if abs(delta61_64)<0.1 & node1848==0 & areachange2!=1864, cluster(kreiskey1849)
ivreg2 delta64_67 (rail1848=slc1848_lcp_1_5km) addrail67 $controls if abs(delta64_67)<0.1 & node1848==0 & areachange2!=1867, cluster(kreiskey1849)
ivreg2 delta67_71 (rail1848=slc1848_lcp_1_5km) addrail71 $controls if abs(delta67_71)<0.1 & node1848==0 & areachange2!=1871, cluster(kreiskey1849)

*******************************************************************************************************************************************************************************************

