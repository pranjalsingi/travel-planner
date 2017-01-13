# travel-planner

The travel planner is an AI which is developed to plan how one can travel optimally from the initial to final destination.. 
It uses A* Search Algorithm which uses both Uniform Cost Search and Best First Search.

The traveler is coded in LISP. It has several lisp files.

<b>initial.lisp</b>
It has the main logic which implements travel planner. 

<b>setup.lisp</b>
It sets up the database. It contains the mode of transportation between different cities and the distance between them.

<b>testcases.lisp</b>
It has the testcases which checks for the optimal cost from going from initial to final destination. The preferencce is flying, taking a train and at last bus. 

# execution

load setup - (load setup.lisp)

load initial - (load initial.lisp)

execute testcases - (load testcases.lisp)
 
