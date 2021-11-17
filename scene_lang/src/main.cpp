#include <iostream>

#include "antlr4-runtime.h"
#include "SceneLexer.h"
#include "SceneParser.h"
#include "Scene.h"

using namespace antlr4;

int main(int argc, const char* argv[]) {

  std::ifstream stream("input.scene");
  if (!stream) {
    puts("file was not found\n");
    return -1;
  }

  ANTLRInputStream input(stream);
  SceneLexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  SceneParser parser(&tokens);

  SceneParser::FileContext *tree = parser.file();

  return 0;  
}
