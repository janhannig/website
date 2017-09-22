%macro allsubsreg(version, data=_last_, depvar=, indepvar=, method=rsquare,
                  sortvar=, printvar=, out=_final, print=yes);

   %if &version ne %then %put ALLSUBSREG macro Version 1.1;
   %if &data=_last_ %then %let data=&syslast;
   ods noresults;
   %local notesopt; 
   %let notesopt = %sysfunc(getoption(notes)) %sysfunc(getoption(source));

   options nosource nonotes;

   %let qlist =;
   %let n = 1;
   %do %while(%length(%scan(&indepvar,&n)));
      %let qlist = &qlist "%scan(&indepvar,&n)";
      %let n = %eval(&n+1);
   %end;
   %let n = %eval(&n-1);

   data _tempAll; if (0); run;
   %do i = 1 %to &n;
      proc plan;
         factors comb  = nrow     ordered
                 index = &i of &n comb    / noprint;
         output out=_temp index cvals=(&qlist);
         run;
      data _temp; set _temp; i = &i; run;
      proc transpose data=_temp out=_temp;
         by comb;
         var index;
         run;
      data _temp; set _temp;
         length Subset $ %length(&indepvar);
         array col{&i};
         Subset = trim(left(col{1}));
         do i = 2 to dim(col);
            Subset = trim(left(Subset)) || ' ' || trim(left(col{i}));
            end;
         keep i comb Subset;
         run;
      data _tempAll; set _tempAll _temp; run;
   %end;

   %global nSubs;
   %let nSubs=%eval(2**&n-1);
   %do i = 1 %to &nSubs;
      %global modelRhs&i;
      data _null_;
        set _tempAll(obs=&i firstobs=&i);
        call symput("modelRhs&i",subset);
      run;
   %end;

   proc reg data=&data outest=_modelparms2 noprint;
     %do i = 1 %to &nSubs;
       model &depvar=&&modelRhs&i/press mse rsquare adjrsq;
     %end;
   proc sort data=_modelparms2 ;
      by _IN_ descending _RSQ_;
      run;
   ods exclude all;
   proc reg data=&data outest=_regparms2(keep=_CP_ _AIC_ _BIC_ _SBC_);
      model &depvar=&indepvar /selection=&method best=&nsubs cp aic bic sbc;
      ods output subsetselsummary=_table(keep=varsinmodel);
      run;
   ods select all;
   data &out; merge _modelparms2 _regparms2 _table;
      run;
   proc sort data=&out;
      by _IN_ descending &sortvar;
      run;
   ods results;
   options &notesopt;
   %if %upcase(%substr(&print,1,1))=Y %then %do;
      proc print data=&out;
         by _IN_;
         var  Varsinmodel _IN_ &printvar Intercept &indepvar;
         Title "All Possible Subset Models Sorted by &sortvar";
         run;
   %end;
   
   options &notesopt;
   title;
%mend allsubsreg;

