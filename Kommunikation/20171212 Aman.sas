PROC IMPORT OUT= WORK.aman 
            DATAFILE= "D:\hpp\veroeff\17\BRRI\Corrected Final GG Aman Boro    09.08.2017.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Aman$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

data aman;
set aman;
L=Location;
Y=year;
ri=Year_of_release;
tj=year;
G=Variety;
R=rep;
if yield=. then delete;
run;

proc sort data=aman;
by G;
run;

proc means data=aman noprint;
by G;
var ri;
output out=G_aman mean=mean_ri max=max_ri min=min_ri;
run;


proc sort data=aman;
by Y L Group G;
run;

proc means data=aman noprint;
by Y L Group G;
var tj ri yield;
output out=aman2 mean=;
run;

proc hpmixed data=aman;
ods output lsmeans=means_aman;
class rep Group G;
model yield=rep Group*G;
lsmeans Group*G;
by Y L;
run;

data aman3;
merge aman2 means_aman;
by Y L Group G;
run;

data aman3;
set aman3;
w=1/StdErr**2;
ErrorVar=StdErr**2;
run;

/*Table 3*/
proc hpmixed data=aman3;
ods output covparms=cp00;
class Y L G;
model estimate=ri tj;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms (1)(1)(1)(1)(1)(1)(1)(1)/hold=8;
run;

proc mixed data=aman3 lognote covtest;
ods output covparms=cp0;
class Y L G;
model estimate=ri tj/solution ddf=1e4,1e4,1e4;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms /hold=8 pdata=cp00;
run;

proc means data=aman3 mean;
var ErrorVar;
run;

/*test for heterogeneity between groups*/
proc mixed data=aman3 lognote;
ods output covparms=cp;
class Y L G group;
model estimate=group ri group*ri tj/ddf=1e4,1e4,1e4,1e4;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms pdata=cp0/hold=8 pdata=cp0;
run;

/*Table 4 - just the G means based on the last four years of data*/

data cp000;
set cp00;
if _N_=1 then delete;
run;

%include 'd:\hpp\veroeff\12\mult\mult.sas';
proc mixed data=aman3 lognote convg=1e-3 maxfunc=10000;
where Y>2011;
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
proc mixed data=aman3 lognote covtest;
class Y L G group;
model estimate=group group*ri tj/solution noint ddf=1e4,1e4,1e4,1e4;
random G L Y Y*L L*G Y*G Y*G*L;
weight w;
parms /hold=8 pdata=cp;
run;

proc sort data=aman3;
by group g;
run;

proc means data=aman3 noprint;
by group g;
var yield;
output out=a mean=;
run;

proc means data=a n;
by group;
var yield;
run;

/*Stability analysis*/

/*Shukla*/
proc hpmixed data=aman3 lognote;
ods output covparms=cp5;
class Y L G;
model estimate=ri tj/solution;
random G L Y Y*L L*G Y*G;
random Y*L/ Group=G;
weight w;
parms (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)(1)(1)(1)(1)
      (1)(1)(1)(1)(1)(1)
/hold=36;
run;

proc mixed data=aman3 lognote covtest;
class Y L G;
model estimate=ri tj/solution;
random G L Y Y*L L*G Y*G;
random Y*L/ Group=G;
weight w;
parms/hold=36 pdata=cp5;
run;

/*Finlay Wilkinson*/
proc mixed data=aman3 lognote covtest;
class Y L G;
model estimate=ri tj;
random G/sub=Y*L type=FA(1);
random G L Y L*G Y*G;
weight w;
parms (.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)
      (.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)
      (.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)
      (.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)
      (.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)
      (.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)(.1)
      (.1)(.1)(.1)(1)
/hold=64;
run;
