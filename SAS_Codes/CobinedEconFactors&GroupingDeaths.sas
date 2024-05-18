libname capstone '/home/u59891211/Capstone';
libname old '/home/u59891211/Capstone/old';

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/owid-covid-data.xlsx'
	DBMS=XLSX
	OUT=capstone.covid;
	GETNAMES=YES;
RUN;

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_9.csv'
	DBMS=CSV
	OUT=capstone.ppppercapita;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_184.csv'
	DBMS=CSV
	OUT=capstone.gdppercapita;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

/* PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/API_NY.GDP.MKTP.PP.CD_DS2_en_csv_v2_1687.csv' */
/* 	DBMS=CSV */
/* 	OUT=capstone.ppp; */
/* 	GETNAMES=YES; */
/* RUN; */
/*  */
/* PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_2.csv' */
/* 	DBMS=CSV */
/* 	OUT=capstone.gdp; */
/* 	GETNAMES=YES; */
/* RUN; */

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/inflation.csv'
	DBMS=CSV
	OUT=capstone.inflation;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/co2empc.csv'
	DBMS=CSV
	OUT=capstone.co2empc;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

/* PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/co2emkt.csv' */
/* 	DBMS=CSV */
/* 	OUT=capstone.co2emkt; */
/* 	GETNAMES=YES; */
/* RUN; */

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/mean-years-of-schooling-long-run.csv'
	DBMS=CSV
	OUT=capstone.meanYearsOfSchooling;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/Unemployment.csv'
	DBMS=CSV
	OUT=capstone.Unemployment;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/healthcare-access-and-quality-index.csv'
	DBMS=CSV
	OUT=capstone.HealthcareIndex;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;

PROC IMPORT DATAFILE='/home/u59891211/Capstone/csv/Coverage_of_essential_health_services.csv'
	DBMS=CSV
	OUT=capstone.CEHSIndex;
	GETNAMES=YES;
	GUESSINGROWS = MAX;
RUN;


data capstone.covidPerYear;
	set capstone.covid;
	if substr(date,6,5) eq "12-31";
run;

data combined;
	length country $1200 indicator_name 'Indicator Name'n $200 indicator_code 'Indicator Code'n $40;
	set capstone.ppppercapita(drop=YR1960-YR1989 rename=('Country Name'n=country)) 
	capstone.gdppercapita(drop=YR1960-YR1989 rename=('Country Name'n=country))
/* 	old.ppp(drop=YR1960-YR1989 rename=('Country Name'n=country)) old.gdp( rename=('Country Name'n=country)) old.co2emkt(drop=YR1960-YR1989 YR2021 YR2022  rename=('Country Name'n=country)) */
	capstone.inflation(rename=('Country Name'n=country)) 
	capstone.co2empc(drop=YR1960-YR1989 YR2021 YR2022  rename=('Country Name'n=country)) 
 	capstone.Unemployment(drop=YR1960-YR1990 rename=('Country Name'n=country))
 	capstone.CEHSIndex(rename=('Country/area'n=country) in=CEHS);
 	
	if CEHS then indicator_name = "CEHS_Index";
	if ('Indicator Name'n eq "GDP per capita, PPP (current international $)") then indicator_name = "PPP_Per_capita";
/* 	if ('Indicator Name'n eq "GDP, PPP (current international $)") then indicator_name = "PPP"; */
	if ('Indicator Name'n eq "GDP per capita (current US$)") then indicator_name = "GDP_Per_capita";
/* 	if ('Indicator Name'n eq "GDP (current US$)") then indicator_name = "GDP"; */
	if ('Indicator Name'n eq "Inflation, consumer prices (annual %)") then indicator_name = "Annual_Inflation_percent";
	if ('Indicator Name'n eq "Unemployment, total (% of total labor force) (modeled ILO estimate)") then indicator_name = "Unemployment_percent";
	if ('Indicator Name'n eq "CO2 emissions (metric tons per capita)") then indicator_name = "CO2_mt_Per_Capita";
/* 	if ('Indicator Name'n eq "CO2 emissions (kt)") then indicator_name = "CO2_kt"; */
	indicator_code = 'Indicator Code'n;
	
	if country eq "Bahamas, The" then country = "Bahamas";
	if country eq "Democratic Republic of Congo" then country = "Congo, Dem. Rep.";
	if country eq "Congo" then country = "Congo, Rep.";
	if country eq "Gambia, The" then country = "Gambia";
	if country eq "Iran, Islamic Rep." then country = "Iran";
	if country eq "Lao PDR" then country = "Laos";
	if country eq "Korea, Dem. People's Rep." then country = "North Korea";
	if country eq "Korea, Rep." then country = "South Korea";
	if country eq "Micronesia (country)" then country = "Micronesia";
	if country eq "Micronesia, Fed. Sts." then country = "Micronesia";
	if country eq "Russian Federation" then country = "Russia";
	if country eq "Slovak Republic" then country = "Slovakia";
	if country eq "Syrian Arab Republic" then country = "Syria";
	if country eq "Turkey" then country = "Turkiye";
	if country eq "Venezuela, RB" then country = "Venezuela";
	if country eq "Viet Nam" then country = "Vietnam";
	if country eq "Yemen, Rep." then country = "Yemen";	
	if country eq "Egypt" then country = "Egypt, Arab Rep.";
	if country eq "Kyrgyzstan" then country = "Kyrgyz Republic";	
	if country eq "Saint Kitts and Nevis" then country = "St. Kitts and Nevis";
	if country eq "Saint Lucia" then country = "St. Lucia";	
	if country eq "Saint Vincent and the Grenadines" then country = "St. Vincent and the Grenadines";
	if country eq "United States Virgin Islands" then country = "Virgin Islands (U.S.)";
	if country eq "East Timor" then country = "Timor-Leste";
	if country eq "Brunei" then country = "Brunei Darussalam";
	if country eq "Other" then country = "Other small states";
	if country eq "Cape Verde" then country = "Cabo Verde";
	if country eq "Taiwan" then country = "China, Taiwan Province of China";
	
	drop 'Indicator Name'n 'Indicator Code'n;
run;

proc sort data=combined;
	by country indicator_name;
run;

/* data combined1; */
/* 	set combined; */
/* 	retain Country_Code; */
/* 	if not missing('Country Code'n) then Country_Code = 'Country Code'n; */
/* 	drop 'Country Code'n; */
/* run; */

data setCountryCode;
	set combined;
	if indicator_name eq "PPP_Per_capita";
	Country_Code = 'Country Code'n;
	keep country Country_Code;
run;

proc sort data=setCountryCode;
	by country Country_Code;
run;

data combined1;
	merge combined setCountryCode;
	by country;
run;

proc sort data=combined1;
	by country Country_Code;
run;

proc transpose data = combined1 out= combinedTransposed;
	by country Country_Code;
	id indicator_name;
	var YR1960-YR2022;
run;

data fixedCombinedTransposed;
	length country $1200 year 8.; 
	set combinedTransposed;
	year = compress(_NAME_, ' ', 'A');
	drop _NAME_;
run;

data fixedMeanOfSchooling;
	length country $1200 Year 8.;
	set capstone.meanYearsOfSchooling;
	country = entity;
	Aberage_Years_Of_Education = "Average years of education"n;
	format Year;
	informat Year;
	keep country Year Aberage_Years_Of_Education;
run;

data fixedHealthcareIndex;
	length country $1200 Year 8.;
	set capstone.HealthcareIndex;
	country = entity;
	country_code = code;
	Healthcare_Access_Quality_Index = "HAQIndex"n;
	format Year;
	informat Year;
	keep country Year Healthcare_Access_Quality_Index;
run;

proc sql;
	create table capstone.economicFactorsCombined as
	select a.*,b.Aberage_Years_Of_Education,c.Healthcare_Access_Quality_Index
	from fixedCombinedTransposed as a
	left join fixedMeanOfSchooling as b 
	on a.country = b.country and a.Year = b.Year
	left join fixedHealthcareIndex as c
	on a.country = c.country and a.Year = c.Year;
quit;

proc export data=capstone.economicFactorsCombined
  outfile='/home/u59891211/Capstone/csv/economicFactorsCombined.csv'
  dbms=csv
  replace;
run;

/* proc sql; */
/* 	create table dist as */
/* 	select distinct country,Country_Code */
/* 	from capstone.economicFactorsCombined; */
/* quit; */

/* data economicFactorsCombined; */
/* 	set combinedTransposed; */
/*  */
/* run; */

/* %for_length(dataset = economicFactorsCombined); */

%macro grouping (category=,sex =);
	data &category._&sex._groups;
		set capstone.&category._&sex.;
		if Type = "Country/Area" or Type = "World";
		country = 'Region, subregion, country or ar'n;
		Country_Code = "ISO3 Alpha-code"n;
		
		children = input(age0, best.);
		array childrenarr {7} age1-age7;
		do i=1 to 7;
			children = sum(children , input(childrenarr[i], best.));
		end;
		
		youngadults = input(age8, best.);
		array youngadultsarr {9} age9-age17;
		do i=1 to 9;
			youngadults = sum(youngadults , input(youngadultsarr[i], best.));
		end;
		
		adults = input(age18, best.);
		array adultsarr {41} age19-age59;
		do i=1 to 41;
			adults = sum(adults , input(adultsarr[i], best.));
		end;
		
		geriatric = input('age100+'n, best.);
		array geriatricarr {40} age60-age99;
		do i=1 to 40;
			geriatric = sum(geriatric , input(geriatricarr[i], best.));
		end;
		
		keep year country Country_Code children youngadults adults geriatric;
	run;
%mend;

%grouping(category=deaths,sex=male);
%grouping(category=deaths,sex=female);
%grouping(category=deaths,sex=both_sexes);
%grouping(category=population,sex=male);
%grouping(category=population,sex=female);
%grouping(category=population,sex=both_sexes);

proc sql;
	create table dth_per1000_bth as
	select a.year, a.country, a.Country_Code, 'Total' as sex, a.children/b.children*1000 as children, a.youngadults/b.youngadults*1000 as youngadults,
	a.adults/b.adults*1000 as adults, a.geriatric/b.geriatric*1000 as geriatric
	from deaths_both_sexes_groups as a inner join population_both_sexes_groups as b
	on a.year = b.year and a.country = b.country;
quit;

proc sort data=dth_per1000_bth;
	by year country Country_Code sex;
run;

proc transpose data=dth_per1000_bth out=deaths_per1000_both_sexes(rename=(col1=deaths)) name=group;
	by year country Country_Code sex;
	var children youngadults adults geriatric;
run;

proc sql;
	create table dth_per1000_male as
	select a.year, a.country, a.Country_Code, 'Male' as sex, a.children/b.children*1000 as children, a.youngadults/b.youngadults*1000 as youngadults,
	a.adults/b.adults*1000 as adults, a.geriatric/b.geriatric*1000 as geriatric
	from deaths_male_groups as a inner join population_male_groups as b
	on a.year = b.year and a.country = b.country;
quit;

proc sort data=dth_per1000_male;
	by year country Country_Code sex;
run;

proc transpose data=dth_per1000_male out=deaths_per1000_male(rename=(col1=deaths)) name=group;
	by year country Country_Code sex;
	var children youngadults adults geriatric;
run;

proc sql;
	create table dth_per1000_female as
	select a.year, a.country, a.Country_Code, 'Female' as sex, a.children/b.children*1000 as children, a.youngadults/b.youngadults*1000 as youngadults,
	a.adults/b.adults*1000 as adults, a.geriatric/b.geriatric*1000 as geriatric
	from deaths_female_groups as a inner join population_female_groups as b
	on a.year = b.year and a.country = b.country;
quit;

proc sort data=dth_per1000_female;
	by year country Country_Code sex;
run;

proc transpose data=dth_per1000_female out=deaths_per1000_female(rename=(col1=deaths)) name=group;
	by year country Country_Code sex;
	var children youngadults adults geriatric;
run;

data capstone.deaths_per_1000_groups;
	length Year 8. country $60. Country_Code $3. sex $8. group$12. deaths 8.;
	set deaths_per1000_both_sexes deaths_per1000_male deaths_per1000_female;
run;


proc sort data=capstone.deaths_per_1000_groups;
	by year country;
run;

proc export data=capstone.deaths_per_1000_groups
  outfile='/home/u59891211/Capstone/csv/deaths_per_1000_groups.csv'
  dbms=csv
  replace;
run;

proc sql;
	create table test as
	select a.*,b.country as test,b.Country_Code as test_code
	from capstone.deaths_per_1000_groups as a left join dist as b
	on a.Country_Code = b.Country_Code or a.country = b.country
	;
quit;

proc sort data = test out = test1 (where = (test = "")) nodupkey; by country; run;












