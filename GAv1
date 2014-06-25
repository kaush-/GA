(* A simple implementation of Genetic Algorithm*********************************************************************************************
	Author: Piyush Kaushik *****************************************************************************************************************
	Date: 20th June 2014********************************************************************************************************************
*)

(* The Following are values used through out the program. Here 'n' represents the number of candidates in one generation, 'g' is the number 
	of bits in each candidate solution, and finally 'r' is the termination condition, which is the total number of generations required in 
	the algorithm. All three can be changed according to your need and input data.
*)

let n = 5;;
let g = 4;;
let r = 6;;

(* Utility Functions ***********************************************************************************************************************
	these are the utility functions used in the program, some are just for simplifications and others are for calculation purposes. ********
*)

let soi x = string_of_int x;;
let foi x = float_of_int x;;
let iof x = int_of_float x;;

let rec pow n x =
	if n = 0 then 1
	else x * (pow (n-1) x);
;;

let rec maxVal lst num =
	match lst with
	[] -> num
	|h :: t -> if h>=num then (maxVal t h) else (maxVal t num);
;;

let rec binVal lst num =
	match lst with
	[] -> 0
	|h::t -> (h * (pow num 2)) + (binVal t (num-1));
;;

let norm x = 
	let temp = x -. foi(iof x) in
	if temp >= 0.5 then (ceil x)
	else
		(floor x);
;;

let rec sumList lst =
	match lst with
	[] -> 0
	|h :: t -> h + sumList t;
;;
	
let avgVal lst = 
	let sum = sumList lst in
	sum / (List.length lst);
;;

let objFun x = x*x;;

(* Mutation Code ****************************************************************************************************************************
	These two functions takes the input population and mutate each bit with a probability of 1/n ********************************************
*)

let rec mutate lst = 
	let x = Random.int(n) in
	match lst with
	[] -> []
	|h :: t -> if x = (n-1) then (1 :: mutate t) else (h :: mutate t);
;;

let rec mutation sample =
	match sample with
	[] -> []
	|h :: t -> (mutate h) :: mutation t;
;;

(* End of Mutation *)

(* CrossOver Code ****************************************************************************************************************************
	Similar to the mutation, here two parents are selected to mate and create a new child. I am using a random point to perform the crossover 
	instead of going with the traditional fixed point mating. ********************************************************************************
*)

let rec innerMate lst prt cpoint sample num =
	match lst with
	[] -> []
	|h :: t -> if num >= cpoint then ((List.nth (List.nth sample prt) num) :: (innerMate t prt cpoint sample (num + 1))) else (h :: (innerMate t prt cpoint sample (num + 1)));
;;

let mate lst prt cpoint sample =
	print_string (""^soi (List.nth lst 0)^soi (List.nth lst 1)^soi (List.nth lst 2)^soi (List.nth lst 3)^soi (List.nth lst 4)^" is mating with "^soi prt^" at "^soi cpoint^"\n");
	innerMate lst prt cpoint sample 0;
;;

let rec crossover sample csample =
	match sample with
	[] -> []
	|h :: t -> (mate h (Random.int(g)) (Random.int(n-1)) csample) :: (crossover t csample);
;;

(* End of CrossOver *)

(* Selection Code ***************************************************************************************************************************
	This is the most complex part of this code, It implements the selection procedure of our genetic algorithm. This is done by, first calculating
	the fitness of each candidate, then normalizing it and after that replacing the candidate solution with '0' normalization value with the max
	value candidate. ***************************************************************************************************************************
*)

let rec binToDec sample = 
	match sample with
	[] -> []
	|h::t -> (binVal h (n-1)) :: (binToDec t);
;;
	
let rec objVal lst =
	match lst with 
	[] -> []
	|h::t -> (objFun h) :: (objVal t);
;;
	
let rec innerFitVal lst favg =
	match lst with 
	[] -> []
	|h::t ->  try (((foi h) /. favg)  :: (innerFitVal t favg)) with Division_by_zero -> [];
;;
	
let fitVal lst =
	let avg = (avgVal lst) in 
	let favg = foi avg in
	innerFitVal lst favg;
;;

let rec normVal lst = 
	match lst with
	[] -> []
	|h :: t -> norm h :: normVal t;
;;	

let rec minInx lst num = 
	match lst with 
	[] -> -1
	|h :: t -> if h = 0. then num else (minInx t (num + 1));
;;

let rec maxInx lst clst num temp =
	match lst with 
	[] -> temp
	|h :: t -> if h > (List.nth clst temp) then (maxInx t clst (num + 1) num) else (maxInx t clst (num + 1) temp);
;;

let rec innerReplace sample csample min max =
	match sample with 
	[] -> []
	|h :: t -> if h = (List.nth csample min) then ((List.nth csample max) :: (innerReplace t csample min max)) else (h :: (innerReplace t csample min max));
;;	

let replace sample min max =
	innerReplace sample sample min max;
;;

let select sample lst =
	let min = minInx lst 0  in
	let max = maxInx lst lst 0 0  in
	if min <> -1 then
		replace sample min max
	else
		sample;
;;

let rec selection sample =
      let declist = (binToDec sample) in
      let objlist = (objVal declist) in
      let fitlist = (fitVal objlist) in
	  let normlist = (normVal fitlist) in
	  select sample normlist;
;;

(* End of Selection *)

let rec gProc sample gen =
	let newSample = selection sample in
	let new2Sample = crossover newSample newSample in
	let new3Sample = mutation new2Sample in
	print_string (""^soi (List.nth (List.nth sample 0) 0)^soi (List.nth (List.nth sample 0) 1)^soi (List.nth (List.nth sample 0) 2)^soi (List.nth (List.nth sample 0) 3)^soi (List.nth (List.nth sample 0) 4)^"\n"^soi (List.nth (List.nth sample 1) 0)^soi (List.nth (List.nth sample 1) 1)^soi (List.nth (List.nth sample 1) 2)^soi (List.nth (List.nth sample 1) 3)^soi (List.nth (List.nth sample 1) 4)^"\n"^soi (List.nth (List.nth sample 2) 0)^soi (List.nth (List.nth sample 2) 1)^soi (List.nth (List.nth sample 2) 2)^soi (List.nth (List.nth sample 2) 3)^soi (List.nth (List.nth sample 2) 4)^"\n"^soi (List.nth (List.nth sample 3) 0)^soi (List.nth (List.nth sample 3) 1)^soi (List.nth (List.nth sample 3) 2)^soi (List.nth (List.nth sample 3) 3)^soi (List.nth (List.nth sample 3) 4)^"\n"    );
	print_string ("The maximum value after "^soi (gen + 1)^" is "^soi (maxVal (objVal (binToDec sample)) 0)^"\n");
	gAlgo new3Sample (gen+1);

and gAlgo sample gen = 
	if gen = r then
		true
	else
		gProc sample gen;
;;

let main =
	let sample = [[0;1;1;0;1];[1;1;0;0;0];[0;1;0;0;0];[1;0;0;1;1]] in
	print_string (""^soi (List.nth (List.nth sample 0) 0)^soi (List.nth (List.nth sample 0) 1)^soi (List.nth (List.nth sample 0) 2)^soi (List.nth (List.nth sample 0) 3)^soi (List.nth (List.nth sample 0) 4)^"\n"^soi (List.nth (List.nth sample 1) 0)^soi (List.nth (List.nth sample 1) 1)^soi (List.nth (List.nth sample 1) 2)^soi (List.nth (List.nth sample 1) 3)^soi (List.nth (List.nth sample 1) 4)^"\n"^soi (List.nth (List.nth sample 2) 0)^soi (List.nth (List.nth sample 2) 1)^soi (List.nth (List.nth sample 2) 2)^soi (List.nth (List.nth sample 2) 3)^soi (List.nth (List.nth sample 2) 4)^"\n"^soi (List.nth (List.nth sample 3) 0)^soi (List.nth (List.nth sample 3) 1)^soi (List.nth (List.nth sample 3) 2)^soi (List.nth (List.nth sample 3) 3)^soi (List.nth (List.nth sample 3) 4)^"\n"    );
	print_string ("The maximum value after "^soi 0^" is "^soi (maxVal (objVal (binToDec sample)) 0)^"\n");
	
	let ans = gAlgo sample 0 in
	
	
	Printf.printf "%d\n",ans;

;;
