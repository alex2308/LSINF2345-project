# LSINF2345-project

### Authors	: Alexandre Carlier & Bastien Gillon
### Date		: 09/05/2018
### Course	: LSINF2345


===============================
### FOLDERS and FILES
 - src/ 			: erlang source files
 - tests/			: text files containing transactions
 - bin/ 			: compiled object codes of erlang source files
 - Makefile			: makefile to clean and build the compiled code
 - README			: this file
 - datastore.pdf	: the instructions file
===============================

===============================
### MAKEFILE
 - clean	: remove all compiled object files from bin/
 - all		: compile all .erl files found in src/ to bin/
 - service	: compile only the erlang files from src/ to bin/ that are needed by the distributed storage service
===============================
