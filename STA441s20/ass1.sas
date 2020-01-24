/********************* ass1.sas ***************************/
title 'Ru Lan Luo 1004475249';
title2 'Little Statclass Data';
title3 'Basic Descriptive Statistics';

data auto;
     infile '/home/u45014532/441s20/1ass/A1LittleStatClassData2b.txt';
     input ID sex $ race $ quiz assignment midterm final; /*what is $ for?*/
     label quiz      = 'Quiz average'
           assignment= 'Computer assignment average'
           midterm   = 'Midterm score'
           final     = 'Final Exam score';
proc freq;
     tables sex race; /*make frequency distributions of the Sex and Race*/
     
proc means;
     var quiz assignment midterm final; /*compute n, mean and standard deviation, min and max for the other variables*/
