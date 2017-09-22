%include 'T:\\STOR455\Book\Macros\scatter.sas';
%scatter(data=houses,var = price sqfeet bedrooms bathrooms ac garage pool year quality style lot highway);
run;

proc print data=houses;
run;

proc univariate data=houses all;
run;
/*style seems to be either 1 2 3or 7 or somethng else we will recode that way;
quality needs to be recoded;
price and possibly sqfeet abd lot might need a transformation*/

data houses1;
set houses;
qualityH=0;qualityM=0;qualityL=0;
if quality=1 then qualityH=1;
if quality=2 then qualityM=1;
if quality=3 then qualityL=1;
/*quality recoded as qualityH qualityH - will use qualityL as baseline*/
style1=0;style2=0;style3=0;style7=0;
if style=1 then style1=1;
if style=2 then style2=1;
if style=3 then style3=1;
if style=7 then style7=1;
/*style recoded into style 1 2 3 7 and all the other styles kept as baseline for lack of datapoints*/
run;

proc reg data=houses1;
model price=sqfeet bedrooms bathrooms ac garage pool year qualityH qualityM style1 style2 style3 style7 lot highway/VIF;
plot rstudent.*(p. sqfeet bedrooms bathrooms ac garage pool year lot highway)
     r.*nqq.;
run;
/*problem with nonconstant variance*/

proc transreg data=houses1;
model boxcox(price / lambda = -1 to 1 by .05)=identity(sqfeet bedrooms bathrooms ac garage pool year qualityH qualityM style1 style2 style3 style7 lot highway);
run;
/*use log at this moment - might change later*/

data houses2;
set houses1;
logprice=log(price);
run;


proc reg data=houses2;
model logprice=sqfeet bedrooms bathrooms ac garage pool year quality style1 style2 style3 style7 lot highway/VIF;
plot rstudent.*(p. sqfeet bedrooms bathrooms ac garage pool year lot highway quality)
     r.*nqq.;
run;
proc reg data=houses2;
model logprice=sqfeet bedrooms bathrooms ac garage pool year 
            quality style1 style2 style3 style7 lot highway/partial;
run;
/*need to recode garage and year2, no need to recode quality*/
data houses3;
set houses2;
year2=year*year;
garage0=0;garage1=0;garage2=0; /*three or more is baseline*/
if garage=0 then garage0=1;
if garage=1 then garage1=1;
if garage=2 then garage2=1;
run;



proc reg data=houses3;
model logprice=sqfeet bedrooms bathrooms ac {garage0 garage1 garage2} 
        pool year year2 {qualityH qualityM} {style1 style2 style3 style7} lot highway/
selection=stepwise groupnames='sqfeet' 'bedrooms' 'bathrooms' 'ac' 'garage' 
      'pool' 'year' 'year2' 'quality' 'style' 'lot' 'highway';
run;
/*stepwise procedure suggests droping bedrooms!*/

/*look for interactions*/
data houses4;
set houses3;
sqfeetbedroom=sqfeet*bedrooms;
sqfeetbathrooms=sqfeet*bathrooms;
sqfeetac=sqfeet*ac;
sqfeetgarage=sqfeet*garage;
sqfeetpool=sqfeet*pool;
sqfeetyqar=sqfeet*year;
sqfeetqualityH=sqfeet*qualityH; 
sqfeetqualityM=sqfeet*qualityM;
sqfeetstyle1=sqfeet*style1;
sqfeetstyle2=sqfeet*style2;
sqfeetstyle3=sqfeet*style3;
sqfeetstyle7=sqfeet*style7;
sqfeetlot=sqfeet*lot;
sqfeethighway=sqfeet*highway;
/*many more needed*/
run;

proc reg data=houses4;
model logprice=sqfeet bedrooms bathrooms ac {garage0 garage1 garage2} 
        pool year year2 {qualityH qualityM} {style1 style2 style3 style7} lot highway
       sqfeetbedroom sqfeetbathrooms sqfeetac sqfeetgarage sqfeetpool
       sqfeetyqar {sqfeetqualityH sqfeetqualityM}
       {sqfeetstyle1 sqfeetstyle2 sqfeetstyle3 sqfeetstyle7}
        sqfeetlot sqfeethighway /
       selection=stepwise groupnames='sqfeet' 'bedrooms' 'bathrooms' 'ac' 'garage' 
      'pool' 'year' 'year2' 'quality' 'style' 'lot' 'highway'
       'sqfeetbedroom' 'sqfeetbathrooms' 'sqfeetac' 'sqfeetgarage' 'sqfeetpool'
       'sqfeetyqar' 'sqfeetquality'
       'sqfeetstyle'
        'sqfeetlot' 'sqfeethighway';
run;
proc reg data=houses4;
model logprice=sqfeet bedrooms bathrooms ac {garage0 garage1 garage2} 
        pool year {qualityH qualityM} {style1 style2 style3 style7} lot highway
        sqfeetbathrooms  sqfeetpool
       sqfeetyqar {sqfeetqualityH sqfeetqualityM}
       {sqfeetstyle1 sqfeetstyle2 sqfeetstyle3 sqfeetstyle7}
         sqfeethighway;
g2mg1: test garage2-garage1=0;
sqfeetstyle: test sqfeetstyle1, sqfeetstyle2, sqfeetstyle3, sqfeetstyle7;
run;
