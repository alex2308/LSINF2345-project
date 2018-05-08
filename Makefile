all:
	erlc -o bin/ src/*
	
clean:
	rm -rf bin/*

service: src/app.erl src/datastore.erl src/tmanager.erl
	erlc -o bin/ src/app.erl src/datastore.erl src/tmanager.erl


