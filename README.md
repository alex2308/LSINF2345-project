# LSINF2345-project

### Authors	: Alexandre Carlier & Bastien Gillon
### Date	: 09/05/2018
### Course	: LSINF2345



### FOLDERS and FILES
 - src/ 			: erlang source files
 - tests/			: text files containing transactions
 - bin/ 			: compiled object codes of erlang source files
 - Makefile			: makefile to clean and build the erlang code
 - README			: this file
 - datastore.pdf	: the instructions file
 - launch.sh		: script to launch distributed storage service
 - parser.sh        : script to launch the parser function on an input file

### MAKEFILE
 - clean	: remove all compiled object files from bin/
 - all		: compile all *.erl files found in src/ to bin/
 - service	: compile only the erlang files from src/ to bin/ that are needed by the distributed storage service

### Execute/Run
 - Use <./parser.sh INPUTFILE OUTPUTFILE> to launch a system, send the commands (INPUTFILE) to it, store the result in a file (OUTPUTFILE) and stop the system
 
 
### ERLANG MODULES
 - app			: module that can do update, snapshot_read and gc on a database when a transaction manager is provided
 - parser		: module that can read and execute transactions from an input file and output the result in a file
 - tmanager		: module containing the code for the transaction manager 
 - datastore	: module containing the code for the datastore
 - performance  : module containing the code for testing and finding the performance of our system
