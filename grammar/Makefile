GRAMMAR_FILE=Python3.g4
ANTLR_JAR=../antlr-4.9.2-complete.jar
ANTLR_FLAGS=-Dlanguage=Cpp -visitor -o gen


Scene.cpp: Python3.g4
	java -jar $(ANTLR_JAR) $(ANTLR_FLAGS) $(GRAMMAR_FILE)
