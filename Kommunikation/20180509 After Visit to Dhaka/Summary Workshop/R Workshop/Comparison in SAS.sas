PROC IMPORT OUT= WORK.GXEMEANS 
            DATAFILE= "D:\User\pschmidt\Desktop\GitHub\projectX\Kommunikation\20180509 After Visit to Dhaka\Summary Workshop\R Workshop\GxEmeans2.txt" 
            DBMS=TAB REPLACE; GETNAMES=YES; DATAROW=2; 
RUN;

DATA gxemeans;
SET gxemeans;
IF t_k=2222 THEN DELETE;
RUN;

PROC MIXED DATA=gxemeans;
class G Y L;
model mean_yield=G t_k;
RANDOM L Y Y*L L*G Y*G;
lsmeans G / at t_k=2008.38; 
RUN;
