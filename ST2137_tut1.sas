/* Q1 */
data htwt;
	infile '~/ST2137_tut_1_data_set/tut1htwt.csv' 
		delimiter=","
		firstobs=2;
	input id gender $ height weight siblings;
	
data test;
	infile '~/ST2137_tut_1_data_set/tut1test.csv' 
		delimiter=","
		firstobs=2;
	input id test;
	
/* Q2 */
data htwtf;
	set htwt;
	where (gender = "F");
/* There are 161 observations in htwtf*/

/* Q3 */
proc sort data=test;
	by id;
proc sort data=htwt;
	by id;
data htwttest;
	merge htwt test;
	by id;
data taller_than_183;
	set htwttest;
	where (height > 183);
	
/* The test scores of subjects who are taller than 183 cm:
id gender height weight siblings test
160	M	185	75	1	62
285	M	184	67	3	54
367	M	186	65	3	93
*/

/* Q4 */
data htwtfixed;
	infile '~/ST2137_tut_1_data_set/tut1htwtfixed.txt'
		firstobs=2;
	input id 1-3 gender $ 4 height 5-7 weight 8-9 siblings 10;
proc sort data=htwtfixed; 
	by id;

/* Q5 */
data htwtfixedremo;
	set htwtfixed;
	if id = 356 then delete;

/* Q6 */
proc sort data=htwttest; 
	by gender descending height;
/* There is a tie in the third highest female at 172cm, their details are as below:
id gender height weight siblings test
173	F	172	58	2	86
234	F	172	64	1	76
333	F	172	51	3	84
*/

/* Q7 */
data htwttestgrade;
	set htwttest;
	if test >= 80 then grade = "A";
		else if test >= 70 then grade = "B";
			else if test >= 60 then grade = "C";
				else if test >= 50 then grade = "D";
					else grade = "F";
					
proc sort data=htwttestgrade;
	by descending gender descending grade;
/* 15 male students get grade="F" */

/* Q8a */
data expResultsDo;
	do i = 1 to 3;
		input batch 1 treatment_1 3-5 treatment_2 7-9 treatment_3 11-13 treatment_4 15-17;
		output;
	end;
datalines;
1 303 311 289 270
2 242 290 259 263
3 289 282 277 257
;
data expResultsDo;
	set expResultsDo;
	drop i;
	
/* Q8b */
data expResultsNoDo;
	input batch 1 treatment_1 3-5 treatment_2 7-9 treatment_3 11-13 treatment_4 15-17;
datalines;
1 303 311 289 270
2 242 290 259 263
3 289 282 277 257
;

/* Q8c*/
data expResultsDo8c;
	do i = 1 to 3;
		input batch 1 treatment_1 3-5 treatment_2 7-9 treatment_3 11-13 treatment_4 15-17;
		output;
	end;
datalines;
1 303 311 289 270
2 242     259 263
3 289 282 277 257
;
