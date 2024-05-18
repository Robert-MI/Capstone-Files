proc datasets library=work kill nolist; run;

data deaths;
	set capstone.outdata;
	keep year country sex total age:;
run;

proc transpose data = deaths out= deaths1;
	 by country year sex;
	 var age: total;
run;

data population;
/* 	length age14 age15 age16 age17 $20; */
	set capstone.population_FEMALE (in = b) capstone.population_MALE (in = a) ;
	where type = "Country/Area";
	if b then sex = "Female";
	if a then sex = "Male";
	
	total = input('age100+'n, best.);
	array test {100} age0-age99;
	 do i=1 to 100;
	 	total = sum(total , input(test[i], best.));
	 end;
	rename 'Region, subregion, country or ar'n = country;
	
	keep 'Region, subregion, country or ar'n age: total sex year;
run;
proc sort data = population; by country year sex; run;

proc transpose data = population out= population1;
	 by country year sex;
	 var age: total;
run;

proc sql;
	create table test as
	select country, year, sum(total) as totalp
	from population
	group by country, year
	order by country, year
	;
	
	create table all1 as 
	select a.*, totalp
	from deaths1 as a left join test as b
	on a.country = b.country and a.year = b.year
	;
quit;

data all2;
	set all1;
	deaths = input(strip(col1), best.);
	if _NAME_ = "total" then cat = "total";
	else do;
		cat = "Age";
		age = input(substr(_NAME_, 4,3), best.);
	end;
		
	if deaths = 0 or totalp = 0 then deaths_per_1000 = 0;
	else deaths_per_1000 = deaths*1000/totalp;

	keep country year sex age cat deaths deaths_per_1000;
run;
	
proc sort data = all2; by country year sex age ; run;

data capstone.countries_group1 capstone.countries_group2 capstone.countries_group3 capstone.countries_group4;
	set all2;
	by country year sex age ;
	page = ceil(_n_/14688);
	 
	group = ceil(page/59);
	if group = 1 then output capstone.countries_group1;
	if group = 2 then output capstone.countries_group2;
	if group = 3 then output capstone.countries_group3;
	if group = 4 then output capstone.countries_group4;
	
	drop page group;
run;
	