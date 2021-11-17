#pragma once

// stdlib includes
#include <vector>
#include <cstdint>

#include "SceneBaseVisitor.h"
#include "Scene.h"
#include "antlr4-runtime.h"

enum Action { Dessiner, Ecrite, PasAction }; // Draw, Write, NoAction
enum Shape { Cercle, Carre, PasShape }; // Circle, Square, NoShape
enum Scene_Size { Grand, Moyen, Petit }; // Big, Medium, Small

using namespace antlr4;

class Element;

class ImageVisitor : public SceneBaseVisitor {

public:

    virtual antlrcpp::Any visitFile(SceneParser::FileContext *ctx) override;

    virtual antlrcpp::Any visitAction(SceneParser::ActionContext *ctx) override;

};

struct Scene {
    std::string name;
    std::vector<Element> elements;
};

void dessiner(Scene* scene);

void getSize(int32_t size_img, Scene_Size size);

