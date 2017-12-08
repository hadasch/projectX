PROC IMPORT OUT= WORK.a 
            DATAFILE= "C:\Users\Hadasch\Desktop\besag_aplha_balanced.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data a ; set a; 
if env>3 then delete; 
if gen>3 then delete; 
if replication>3 then delete; 
run;
