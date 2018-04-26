# LSINF2345-project

### Authors	: Alexandre Carlier & Bastien Gillon
### Date	: 09/05/2018
### Course	: LSINF2345



### FOLDERS and FILES
 - src/ 			: erlang source files
 - tests/			: text files containing transactions
 - bin/ 			: compiled object codes of erlang source files
 - Makefile			: makefile to clean and build the compiled code
 - README			: this file
 - datastore.pdf	: the instructions file
 - launch.sh		: script to launch distributed storage service

### MAKEFILE
 - clean	: remove all compiled object files from bin/
 - all		: compile all .erl files found in src/ to bin/
 - service	: compile only the erlang files from src/ to bin/ that are needed by the distributed storage service

### Execute/Run
 - Use the launch.sh script to launch the distributed DB service
 
 
### ERLANG MODULES
 - app			: module that is used to start the datastore and the transaction manager and do the requests (update,snapshotread,gc)
 - parser		: module that can read and execute transactions from a file and output the result in a file
 - tmanager		: module containing the transaction manager code
 - datastore	: module containing the datastore implementation
