use "C:\Users\Jasmin\OneDrive - Universität Hamburg\Desktop\Dissertation\Leviathan\constitutional-regression-advanced\data\statadata.dta"
ivregress 2sls jud_replace_con ruth_populism (ruth_populism#c.lag_trust_share lag_trust_share = jud_ind_mean5 ruth_populism#c.jud_ind_mean5 v2jucorrdc ruth_populism#c.v2jucorrdc), noheader
ivregress 2sls jud_replace_con ruth_populism (ruth_populism#c.lag_trust_share lag_trust_share = jud_ind_mean5 v2jucorrdc), noheader
ivregress 2sls jud_replace_con evnt_sum_lag3 ruth_populism (ruth_populism#c.lag_trust_share lag_trust_share = jud_ind_mean5 ruth_populism#c.jud_ind_mean5 v2jucorrdc ruth_populism#c.v2jucorrdc), noheader
estat endogenous
estat firststage
margins ruth_populism#c.lag_trust_share
margins ruth_populism#lag_trust_share
margins ruth_populism#c.lag_trust_share2
margins 2.ruth_populism#c.lag_trust_share
margins
xtivreg jud_replace ruth_populism (lag_trust_share = jud_ind_mean5 jud_corr_lag), fe
xtset country year
xtivreg jud_replace ruth_populism (lag_trust_share = jud_ind_mean5 jud_corr_lag), fe
xtivreg jud_replace_con evnt_sum_lag3 ruth_populism (ruth_populism#c.lag_trust_share lag_trust_share = jud_ind_mean5 ruth_populism#c.jud_ind_mean5 v2jucorrdc ruth_populism#c.v2jucorrdc)
gen poptrsut=ruth_populism#lag_trust_share
 gen poptrsut=ruth_populism*lag_trust_share
gen poptrsut=ruth_populism*jud_ind_mean5
gen popind=ruth_populism*jud_ind_mean5
gen popcorr=ruth_populism*v2jucorrdc
 xtivreg jud_replace_con evnt_sum_lag3 ruth_populism (poptrsut lag_trust_share = jud_ind_mean5 popind v2jucorrdc popcorr)
xtivreg jud_replace_con evnt_sum_lag3 ruth_populism (poptrsut lag_trust_share = jud_ind_mean5 popind v2jucorrdc popcorr), fe
estat endogenous
estat firststage
estat overid
xtivreg jud_replace_con evnt_sum_lag3 ruth_populism (poptrsut lag_trust_share = jud_ind_mean5 popind v2jucorrdc popcorr), fe
estat overid
ivregress jud_replace_con evnt_sum_lag3 country ruth_populism (poptrsut lag_trust_share = jud_ind_mean5 popind v2jucorrdc popcorr)
ivregress 2sls jud_replace_con evnt_sum_lag3 country ruth_populism (poptrsut lag_trust_share = jud_ind_mean5 popind v2jucorrdc popcorr)
estat overid
estat firststage
estat endogenous
use "C:\Users\Jasmin\OneDrive - Universität Hamburg\Desktop\Dissertation\Leviathan\constitutional-regression-advanced\data\statadata_final.dta", clear
use "C:\Users\Jasmin\OneDrive - Universität Hamburg\Desktop\Dissertation\Leviathan\constitutional-regression-advanced\data\statadata_final.dta", clear
gen poptrust=ruth_populism*lagged_trust_share_1
gen popcorr=ruth_populism*v2jucorrdc_mean_3
gen popacc=ruth_populism*v2juaccnt_mean_3
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = jud_ind_mean5 popacc v2jucorrdc popcorr)
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = v2juaccnt_mean_3 popacc v2jucorrdc_mean_3 popcorr)
estat endogenous
estat endogenous
estat firststage
estat overid
ivreg2 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = v2juaccnt_mean_3 popacc v2jucorrdc_mean_3 popcorr)
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = v2juaccnt_mean_3 popacc v2jucorrdc_mean_3 popcorr), vce(robust)
estat overid
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = v2juaccnt_mean_3 popacc)
estat overid
estat endogenous
estat firststage
 ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = v2jucorrdc_mean_3 popcorr)
estat endogenous
estat firststage
margins ruth_populism lagged_trust_share_1, post
margins ruth_populism lagged_trust_share_1
 margins lagged_trust_share_1 ruth_populism
margins poptrust
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_1 = v2jucorrdc_mean_3 popcorr)
margins ruth_populism lagged_trust_share_1
margins, dydx(ruth_populism lagged_trust_share_1)
estat firststage
margins, at(ruth_populism=(0 1))
margins, dydx(ruth_populism) at(lagged_trust_share_1=(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8))
use "C:\Users\Jasmin\OneDrive - Universität Hamburg\Desktop\Dissertation\Leviathan\constitutional-regression-advanced\data\statadata_final.dta", clear
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_linear_imp_1 = v2jucorrdc_mean_3 popcorr)
gen poptrust=ruth_populism*lagged_trust_share_linear_imp_1
gen popcorr=ruth_populism*v2jucorrdc_mean_3
gen popacc=ruth_populism*v2juaccnt_mean_3
ivregress 2sls jud_replace_cont evnt_sum_lag3 executive senior_length country ruth_populism (poptrust lagged_trust_share_linear_imp_1 = v2jucorrdc_mean_3 popcorr)
