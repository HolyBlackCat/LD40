#include <iostream>
#include "mat.h"
#include "program.h"
#include "ui.h"
#include "window.h"
#include "wrappers.h"
#include "reflection.h"

ReflectStruct(S, ((int)(a,b,c),))

int main(int, char **)
{
    Window win("Woah", {800,600});

    SDL_Event e;
    while (1)
    while (SDL_PollEvent(&e))
    {
        if (e.type == SDL_QUIT)
            return 0;
    }

    return 0;
}