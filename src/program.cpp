#include "program.h"

#include <cstdlib>

#include "platform.h"
#include "ui.h"

namespace Program
{
    static std::string error_messagebox_caption = "Error!",
                       error_messagebox_prefix  = "Error: ";

    void Exit(int code)
    {
        std::exit(code);
    }
    void Exit_NoCleanup(int code)
    {
        std::_Exit(code);
    }

    void Error(std::string text, int code)
    {
        UI::MessageBox(error_messagebox_caption, error_messagebox_prefix + text, UI::error);
        Exit(code);
    }
    void Error_NoCleanup(std::string text, int code)
    {
        UI::MessageBox(error_messagebox_caption, error_messagebox_prefix + text, UI::error);
        Exit_NoCleanup(code);
    }

    void SetErrorMessageBoxCaption(std::string caption)
    {
        error_messagebox_caption = caption;
    }
    void SetErrorMessageBoxPrefix(std::string prefix)
    {
        error_messagebox_prefix = prefix;
    }
}

OnPC
(
    extern "C"
    {
        // This makes video drivers use best available videocard for the application.
        __declspec(dllexport) uint32_t NvOptimusEnablement                  = 1; // For Nvidia. Docs suggest that this should have type dword, but I don't want windows headers here.
        __declspec(dllexport) int      AmdPowerXpressRequestHighPerformance = 1; // For Amd.
    }
)
