program drop _all

program define sim1
	use "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	logit napadopt vdem_gender if lawadoptany == 1, cluster(ccode)
end
simulate _b _se, reps(1000): sim1
save "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\simulation results (Reference results).dta", replace


program drop _all

program define sim2
	use "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	by ccode: gen lag_lawadoptany = lawadoptany[_n-1]
	by ccode: gen lag_napadoptany = napadoptany[_n-1]
	replace lag_lawadoptany = 0 if lag_lawadoptany == .
	replace lag_napadoptany = 0 if lag_napadoptany == .
	
	mkdyads vdem_gender lawadopt lawadoptany napadopt napadoptany lag_lawadoptany lag_napadoptany, unit(ccode) time(year)

	logit napadopt_01 vdem_gender_01 if ccode_01 != ccode_02, cluster(ccode_01)

end
simulate _b _se, reps(1000): sim2
save "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\simulation results (Unconditioned results).dta", replace

program drop _all

program define sim3
	use "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	by ccode: gen lag_lawadoptany = lawadoptany[_n-1]
	by ccode: gen lag_napadoptany = napadoptany[_n-1]
	replace lag_lawadoptany = 0 if lag_lawadoptany == .
	replace lag_napadoptany = 0 if lag_napadoptany == .
	
	mkdyads vdem_gender lawadopt lawadoptany napadopt napadoptany lag_lawadoptany lag_napadoptany, unit(ccode) time(year)

	logit napadopt_01 vdem_gender_01 if ccode_01 != ccode_02 & lag_napadoptany_02, cluster(ccode_01)

end
simulate _b _se, reps(1000): sim3
save "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\simulation results (Conditioned results).dta", replace

program drop _all

program define sim4
	use "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	by ccode: gen lag_lawadoptany = lawadoptany[_n-1]
	by ccode: gen lag_napadoptany = napadoptany[_n-1]
	replace lag_lawadoptany = 0 if lag_lawadoptany == .
	replace lag_napadoptany = 0 if lag_napadoptany == .
	
	mkdyads vdem_gender lawadopt lawadoptany napadopt napadoptany lag_lawadoptany lag_napadoptany, unit(ccode) time(year)

	logit napadopt_01 vdem_gender_01 if ccode_01 != ccode_02 & lag_napadoptany_02 & lawadoptany_01, cluster(ccode_01)

end
simulate _b _se, reps(1000): sim4
save "C:\Users\woobd1\Dropbox\Iowa\Qualification\IOWA Method Paper\Qualification-Paper\replication test\simulation results (Diffusion with Steps results).dta", replace
