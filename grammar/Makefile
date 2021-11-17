GRAMMAR_FILE=Scene.g4
ANTLR_JAR=../antlr-4.9.2-complete.jar
ANTLR_FLAGS=-Dlanguage=Cpp -no-listener -visitor -o gen 


Scene.cpp: Scene.g4
	java -jar $(ANTLR_JAR) $(ANTLR_FLAGS) $(GRAMMAR_FILE)
