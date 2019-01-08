proc datasets lib=work kill; run; quit; 
PROC IMPORT OUT= WORK.DATA 
            DATAFILE= "D:\user\shadasch\Documents\GitHub\projectX\Dec2018\second data inspection means aman.txt" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data data; set data; drop y; y=tj; run;
data data; set data; y=tj; run;

ods output solutionf=f tests3=tests;
proc mixed data=data lognote cl;
weight w;
class Y L G; 
model adjmean= tj ri/s;
random Y L G Y*L Y*G G*L; 
parms (1)(1)(1)(1)(1)(1)(1)/hold=7;
run;

ods output estimates=estimates_group solutionf=f_group tests3=tests_group;
proc mixed data=data lognote;
weight w;
class Y L G group; 
model adjmean= group tj ri*group /s noint;
random Y L G Y*L Y*G G*L; 
parms (1)(1)(1)(1)(1)(1)(1)/hold=7;

estimate 'ri aromatic vs long'   ri*group 1 -1  0  0  0;
estimate 'ri aromatic vs medium' ri*group 1  0 -1  0  0;
estimate 'ri aromatic vs short'  ri*group 1  0  0 -1  0;
estimate 'ri aromatic vs stress' ri*group 1  0  0  0 -1;
estimate 'ri long vs medium'     ri*group 0  1 -1  0  0;
estimate 'ri long vs short'      ri*group 0  1  0 -1  0;
estimate 'ri long vs stress'     ri*group 0  1  0  0 -1;
estimate 'ri medium vs short'    ri*group 0  0  1 -1  0;
estimate 'ri medium vs stress'   ri*group 0  0  1  0 -1;
estimate 'ri short vs stress'    ri*group 0  0  0  1 -1;
run;
