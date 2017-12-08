PROC IMPORT OUT= WORK.boro 
            DATAFILE= "D:\hpp\veroeff\17\BRRI\Corrected Final GG Aman Boro    09.08.2017.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Boro$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

data boro;
set boro;
L=Location;
if year='2001-2002' then year2=2001;
if year='2002-2003' then year2=2002;
if year='2003-2004' then year2=2003;
if year='2004-2005' then year2=2004;
if year='2005-2006' then year2=2005;
if year='2006-2007' then year2=2006;
if year='2007-2008' then year2=2007;
if year='2008-2009' then year2=2008;
if year='2009-2010' then year2=2009;
if year='2010-2011' then year2=2010;
if year='2011-2012' then year2=2011;
if year='2012-2013' then year2=2012;
if year='2013-2014' then year2=2013;
if year='2014-2015' then year2=2014;
if year='2015-2016' then year2=2015;
Y=year;
ri=Year_of_release;
tj=year2;
G=Variety;
R=rep;
if yield=. then delete;
run;

proc sort data=boro;
by G;
run;

proc means data=boro noprint;
by G;
var ri;
output out=G_boro mean=mean_ri max=max_ri min=min_ri;
run;


proc sort data=boro;
by Y L Group G;
run;

proc means data=boro noprint;
by Y L Group G;
var tj ri yield;
output out=boro2 mean=;
run;

proc hpmixed data=boro;
ods output lsmeans=means_boro;
class rep Group G;
model yield=rep Group*G;
lsmeans Group*G;
by Y L;
run;

data boro3;
merge boro2 means_boro;
by Y L Group G;
run;

data boro3;
set boro3;
w=1/StdErr**2;
ErrorVar=StdErr**2;
run;

/*Table 3*/
proc mixed data=boro3 lognote covtest;
ods output covparms=cp0;
class Y L G;
model estimate=ri tj/solution ddf=1e4,1e4,1e4;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms (1)(1)(1)(1)(1)(1)(1)(1)/hold=8;
run;

proc means data=boro3 mean;
var ErrorVar;
run;

/*test for heterogeneity between groups*/
proc hpmixed data=boro3;
ods output covparms=cp1;
class Y L G group;
model estimate=group ri group*ri tj;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms /hold=8 pdata=cp0;
run;

proc mixed data=boro3 lognote covtest;
ods output covparms=cp;
class Y L G group;
model estimate=group ri group*ri tj/solution ddf=1e4,1e4,1e4,1e4;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms /hold=8 pdata=cp1;
run;

/*Table 4 - just the G means based on the last four years of data*/
data cp000;
set cp0;
if _N_=1 then delete;
run;

%include 'd:\hpp\veroeff\12\mult\mult.sas';
proc mixed data=boro3 lognote convg=1e-3 maxfunc=10000;
where y in ('2011-2012', '2012-2013', '2014-2015', '2015-2016');
ods output lsmeans=lsmeans diffs=diffs;
class Y L G;
model estimate=G;
random L Y Y*L L*G Y*G Y*G*L;
lsmeans G/pdiff adjust=sim(nsamp=1e6);
weight w;
parms /hold=7 pdata=cp000;
%mult(trt=G, p=adjp);
run;

/*Table 5*/
/*estimate group-specific slopes*/
proc mixed data=boro3 lognote covtest;
class Y L G group;
model estimate=group group*ri tj/solution noint ddf=1e4,1e4,1e4,1e4;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms /hold=8 pdata=cp;
run;

proc sort data=boro3;
by group g;
run;

proc means data=boro3 noprint;
by group g;
var yield;
output out=b mean=;
run;

proc means data=b n;
by group;
var yield;
run;

/*Stability analysis*/

/*Shukla*/
proc hpmixed data=boro3 lognote;
ods output covparms=cp5;
class Y L G;
model estimate=ri tj/solution;
random G L Y Y*L L*G Y*G;
random Y*L/ Group=G;
weight w;
parms (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)
/hold=38;
run;

proc mixed data=boro3 lognote covtest;
class Y L G;
model estimate=ri tj/solution;
random G L Y Y*L L*G Y*G;
random Y*L/ Group=G;
weight w;
parms/hold=38 pdata=cp5;
run;

/*Finlay Wilkinson*/
proc mixed data=boro3 lognote covtest;
class Y L G;
model estimate=ri tj/solution ddf=1e4,1e4,1e4;
random G/sub=Y*L type=FA(1);
random G L Y L*G Y*G;
weight w;
parms (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)
/hold=64;
run;

