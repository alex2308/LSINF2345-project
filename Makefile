
clean:
	rm -rf bin/*

all:
	erlc -o bin/ src/*

service: src/app.erl src/datastore.erl src/tmanager.erl
	erlc -o bin/ src/app.erl src/datastore.erl src/tmanager.erl


