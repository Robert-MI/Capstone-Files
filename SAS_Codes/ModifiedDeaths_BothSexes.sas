proc datasets library=work kill nolist; run;

data deaths;
	set capstone.deaths_both_sexes;
	where type = "Country/Area";
	
	total = input('age100+'n, best.);
	array test {100} age0-age99;
	 do i=1 to 100;
	 	total = sum(total , input(test[i], best.));
	 end;
	rename 'Region, subregion, country or ar'n = country;
	
	keep 'Region, subregion, country or ar'n age: total year;
run;

proc sort data = deaths; by country year; run;

proc transpose data = deaths out= deaths1;
	 by country year;
	 var age: total;
run;

data population;
	set capstone.population_both_sexes;
		where type = "Country/Area";
	
	total = input('age100+'n, best.);
	array test {100} age0-age99;
	 do i=1 to 100;
	 	total = sum(total , input(test[i], best.));
	 end;
	rename 'Region, subregion, country or ar'n = country;
	
	keep 'Region, subregion, country or ar'n age: total year;
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
	if _NAME_ = "total" then cat = "Total";
	else do;
		cat = "Age";
		age = input(substr(_NAME_, 4,3), best.);
	end;
		
	if deaths = 0 or totalp = 0 then deaths_per_1000 = 0;
	else deaths_per_1000 = deaths*1000/totalp;

	keep country year age cat deaths deaths_per_1000;
run;
	
proc sort data = all2; by country year age; run;

data capstone.countries1 capstone.countries2;
	set all2;
	by country year age ;
	page = ceil(_n_/14688);
	 
	group = ceil(page/59);
	if group = 1 then output capstone.countries1;
	if group = 2 then output capstone.countries2;
	
	drop page group;
run;

	
	