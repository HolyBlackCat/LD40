#include "everything.h"

#include <iostream>
#include <sstream>

static constexpr ivec2 screen_sz(800,600);

Window win;
Renderers::Poly2D_PBR r;
Graphics::Texture tex_lookup, tex_albedo, tex_normal, tex_m_r_h_ao, tex_emission;

Input::Mouse mouse(screen_sz/2);
Audio::Context audio_con;
Timing::TickStabilizer ts;

void Init()
{
    Events::SetErrorHandlers();
    win.Create("Woah", screen_sz);

    Graphics::Image img_lookup   ("assets/lookup.png"),
                    img_albedo   ("assets/texture-albedo.png"),
                    img_ao       ("assets/texture-ao.png"),
                    img_emission ("assets/texture-emission.png"),
                    img_metallic ("assets/texture-metallic.png"),
                    img_roughness("assets/texture-roughness.png"),
                    img_height   ("assets/texture-height.png"),
                    img_normal   ("assets/texture-normal.png");

    ivec2 tex_sz = img_albedo.Size();
    Graphics::Image img_m_r_h_ao(tex_sz);
    for (int y = 0; y < tex_sz.y; y++)
    for (int x = 0; x < tex_sz.x; x++)
    img_m_r_h_ao.FastSet({x,y}, u8vec4(img_metallic .FastGet({x,y}).r,
                                       img_roughness.FastGet({x,y}).r,
                                       img_height   .FastGet({x,y}).r,
                                       255 - img_ao .FastGet({x,y}).a));

    tex_lookup.Create();
    tex_lookup.Interpolation(Graphics::Texture::linear);
    tex_lookup.SetData(img_lookup);
    tex_albedo.Create();
    tex_albedo.Interpolation(Graphics::Texture::linear);
    tex_albedo.SetData(img_albedo);
    tex_normal.Create();
    tex_normal.Interpolation(Graphics::Texture::linear);
    tex_normal.SetData(img_normal);
    tex_m_r_h_ao.Create();
    tex_m_r_h_ao.Interpolation(Graphics::Texture::linear);
    tex_m_r_h_ao.SetData(img_m_r_h_ao);
    tex_emission.Create();
    tex_emission.Interpolation(Graphics::Texture::linear);
    tex_emission.SetData(img_emission);

    r.Create(screen_sz, 0x10000, tex_lookup);
    r.SetTextures(tex_albedo, tex_normal, tex_m_r_h_ao, tex_emission);

    Graphics::Blending::FuncNormalPre();
}

int main(int, char **)
{
    auto Tick = [&]
    {

    };
    auto Render = [&]
    {
        r.Quad({0,0}, 30, {64,64}).tex({0,0});
        r.Quad({0,0}, 0, {64,64}).tex({0,0}).center();
    };

    Init();
    Graphics::SetClearColor(fvec3(0));
    r.SetBackground(fvec3(1,1,2) / 80);
    r.SetMatrix(fmat4::ortho({-400,300},{400,-300},-1000000,1000000));

    uint64_t t0 = Timing::Clock();
    while (1)
    {
        if (Input::Keys::f11.pressed())
            win.ToggleFullscreen();

        uint64_t t1 = Timing::Clock(), delta = t1 - t0;
        t0 = t1;

        while (ts.Tick(delta))
        {
            Events::Process();
            Graphics::CheckErrors();
            audio_con.CheckErrors();
            Audio::Source::Tick();
            Tick();
        }

        Render();
        r.DrawAmbient();
        r.PointLight(fvec3(1,0.8,.6)*300, mouse.pos().to_vec3(-30));
        r.DrawFinal();
        win.Swap();
    }

    return 0;
}
