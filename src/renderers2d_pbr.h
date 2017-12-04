#ifndef RENDERERS2D_PBR_H_INCLUDED
#define RENDERERS2D_PBR_H_INCLUDED

#include <iostream>
#include <utility>

#include "graphics.h"
#include "input.h"
#include "mat.h"
#include "reflection.h"
#include "strings.h"
#include "template_utils.h"

namespace Renderers
{
    namespace Poly2D_PBR_impl
    {
        ReflectStruct(AttrMain, (
            (fvec3)(pos),
            (fvec2)(tex_pos),
            (fvec2)(alpha_beta),
            (fvec4)(normal_mat),
        ))

        ReflectStruct(UniMain, (
            (Graphics::Shader::VertexUniform<fmat4>)(matrix),
            (Graphics::Shader::VertexUniform<fvec2>)(tex_size),
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(lookup),
            (Graphics::Shader::FragmentUniform<fvec3>)(background),
            (Graphics::Shader::FragmentUniform<float>)(emission_factor),
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(tex_albedo,tex_normal,tex_m_r_h_ao,tex_emission),
        ))

        ReflectStruct(UniLight, (
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(tex_albedo,tex_normal,tex_m_r_h_ao,tex_depth),
            (Graphics::Shader::VertexUniform<fvec2>)(tex_size),
            (Graphics::Shader::VertexUniform<fmat4>)(matrix),
            (Graphics::Shader::FragmentUniform<fvec3>)(light_pos,light_color),
            (Graphics::Shader::FragmentUniform<float>)(light_radius),
            (Graphics::Shader::FragmentUniform<bool>)(light_tube),
            (Graphics::Shader::FragmentUniform<fvec3>)(light_end),
        ))


        ReflectStruct(AttrTex, (
            (fvec2)(pos),
        ))

        ReflectStruct(UniExBr, (
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
            (Graphics::Shader::FragmentUniform<float>)(threshold),
        ))

        ReflectStruct(UniBlur, (
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
            (Graphics::Shader::FragmentUniform<fvec2>)(step),
            (Graphics::Shader::FragmentUniform<bool>)(vertical),
        ))

        ReflectStruct(UniCopy, (
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
            (Graphics::Shader::FragmentUniform<float>)(factor),
        ))

        ReflectStruct(UniFinal, (
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
            (Graphics::Shader::FragmentUniform<float>)(exposure),
        ))

        ReflectStruct(UniIden, (
            (Graphics::Shader::FragmentUniform<Graphics::Texture>)(texture),
        ))


        namespace ShaderSource
        {
            namespace Main
            {
                extern const char *const v, *const f;
            }
            namespace Light
            {
                extern const char *const v, *const f;
            }
            namespace ExBr // Extract bright
            {
                extern const char *const v, *const f;
            }
            namespace Blur
            {
                extern const char *const v, *const f;
            }
            namespace Copy
            {
                extern const char *const v, *const f;
            }
            namespace Final
            {
                extern const char *const v, *const f;
            }
            namespace Iden
            {
                extern const char *const v, *const f;
            }
        }
    }

    class Poly2D_PBR
    {
        static constexpr int blur_passes = 4;

        Graphics::Shader sh_main;
        Graphics::Shader sh_light;
        Graphics::Shader sh_exbr;
        Graphics::Shader sh_blur;
        Graphics::Shader sh_copy;
        Graphics::Shader sh_final;
        Graphics::Shader sh_iden;
        Poly2D_PBR_impl::UniMain  uni_main;
        Poly2D_PBR_impl::UniLight uni_light;
        Poly2D_PBR_impl::UniExBr  uni_exbr;
        Poly2D_PBR_impl::UniBlur  uni_blur;
        Poly2D_PBR_impl::UniCopy  uni_copy;
        Poly2D_PBR_impl::UniFinal uni_final;
        Poly2D_PBR_impl::UniIden  uni_iden;

        Graphics::Texture tex_framebuffer_hdr;
        Graphics::Texture tex_framebuffer_hdr_depth;
        Graphics::FrameBuffer framebuffer_hdr;
        Graphics::FrameBuffer framebuffer_hdr_color;
        Graphics::Texture tex_framebuffer_bloom1;
        Graphics::Texture tex_framebuffer_bloom2;
        Graphics::FrameBuffer framebuffer_bloom1;
        Graphics::FrameBuffer framebuffer_bloom2;
        Graphics::Texture tex_framebuffer_final;
        Graphics::FrameBuffer framebuffer_final;

        Graphics::RenderQueue<Poly2D_PBR_impl::AttrMain, Graphics::triangles, Graphics::expand> queue;

        const Graphics::CharMap *ch_map = 0;

        ivec2 scr_sz, real_sz;
        float scale;

        static void FullscreenQuad()
        {
            static Graphics::VertexBuffer<Poly2D_PBR_impl::AttrTex> vbuf;
            static bool first = 1;
            if (first)
            {
                first = 0;
                vbuf.Create();
                Poly2D_PBR_impl::AttrTex arr[]{{{-1,-1}},{{-1,10}},{{10,-1}}};
                vbuf.SetData(3, arr);
            }
            vbuf.Draw(Graphics::triangles);
        }

      public:
        Input::Mouse mouse;

        class Quad_t : TemplateUtils::MoveFunc<Quad_t>
        {
            using ref = Quad_t &&;

            // The constructor sets those:
            decltype(Poly2D_PBR::queue) *queue;
            fvec2 m_pos, m_size;
            float m_depth;

            bool has_texture = 0;
            fvec2 m_tex_pos = fvec2(0), m_tex_size = fvec2(0);

            bool has_center = 0;
            fvec2 m_center = fvec2(0);
            bool m_center_pos_tex = 0;

            bool has_matrix = 0;
            fmat3 m_matrix = fmat3::identity();

            float m_alpha[4] = {1,1,1,1};
            float m_beta[4] = {1,1,1,1};

            bool m_abs_pos = 0;
            bool m_abs_tex_pos = 0;

            bool m_flip_x = 0, m_flip_y = 0;

            static void OnMove(Quad_t &&from, Quad_t &/*to*/)
            {
                from.queue = 0;
            }
          public:
            Quad_t(decltype(Poly2D_PBR::queue) *queue, fvec2 pos, float d, fvec2 size) : queue(queue), m_pos(pos), m_size(size), m_depth(d) {}

            Quad_t(const Quad_t &) = delete;
            Quad_t &operator=(const Quad_t &) = delete;

            Quad_t(Quad_t &&) = default;
            Quad_t &operator=(Quad_t &&) = default;

            ~Quad_t()
            {
                if (!queue)
                    return;

                DebugAssert("2D poly PBR: Quad with no texture nor color specified.", has_texture);
                DebugAssert("2D poly PBR: Quad with absolute corner coodinates with a center specified.", m_abs_pos + has_center < 2);
                DebugAssert("2D poly PBR: Quad with absolute texture coordinates mode but no texture coordinates specified.", m_abs_tex_pos <= has_texture);
                DebugAssert("2D poly PBR: Quad with a matrix but without a center specified.", has_matrix <= has_center);

                if (m_abs_pos)
                    m_size -= m_pos;
                if (m_abs_tex_pos)
                    m_tex_size -= m_tex_pos;

                Poly2D_PBR_impl::AttrMain out[4];

                for (int i = 0; i < 4; i++)
                {
                    out[i].alpha_beta.x = m_alpha[i];
                    out[i].alpha_beta.y = m_beta[i];
                }

                if (m_center_pos_tex)
                {
                    if (m_tex_size.x)
                        m_center.x *= m_size.x / m_tex_size.x;
                    if (m_tex_size.y)
                        m_center.y *= m_size.y / m_tex_size.y;
                }

                if (m_flip_x)
                {
                    m_tex_pos.x += m_tex_size.x;
                    m_tex_size.x = -m_tex_size.x;
                    if (has_center)
                        m_center.x = m_size.x - m_center.x;
                }
                if (m_flip_y)
                {
                    m_tex_pos.y += m_tex_size.y;
                    m_tex_size.y = -m_tex_size.y;
                    if (has_center)
                        m_center.y = m_size.y - m_center.y;
                }

                out[0].pos = (-m_center).to_vec3(m_depth);
                out[2].pos = (m_size - m_center).to_vec3(m_depth);
                out[1].pos = fvec2(out[0].pos.x, out[2].pos.y).to_vec3(m_depth);
                out[3].pos = fvec2(out[2].pos.x, out[0].pos.y).to_vec3(m_depth);

                for (auto &it : out)
                    it.pos = m_pos.to_vec3(it.pos.z) + (m_matrix /mul/ it.pos.set_z(1)).set_z(0);
                fmat2 nm = m_matrix.to_mat2().inverse().transpose();

                out[0].tex_pos = m_tex_pos;
                out[2].tex_pos = m_tex_pos + m_tex_size;
                out[1].tex_pos = {out[0].tex_pos.x, out[2].tex_pos.y};
                out[3].tex_pos = {out[2].tex_pos.x, out[0].tex_pos.y};

                fvec4 nm_v;
                std::memcpy(&nm_v, &nm, sizeof nm);
                for (auto &it : out)
                    it.normal_mat = nm_v;

                queue->Quad(out[0], out[1], out[2], out[3]);
            }

            ref depth(float d)
            {
                m_depth = d;
                return (ref)*this;
            }
            ref tex(ivec2 pos, ivec2 size)
            {
                tex_f(pos, size);
                return (ref)*this;
            }
            ref tex(ivec2 pos)
            {
                tex_f(pos);
                return (ref)*this;
            }
            ref tex_f(fvec2 pos, fvec2 size)
            {
                DebugAssert("2D poly renderer: Quad_t texture specified twice.", !has_texture);
                has_texture = 1;

                m_tex_pos = pos;
                m_tex_size = size;
                return (ref)*this;
            }
            ref tex_f(fvec2 pos)
            {
                tex_f(pos, m_size);
                return (ref)*this;
            }
            ref center(fvec2 c)
            {
                DebugAssert("2D poly renderer: Quad_t center specified twice.", !has_center);
                has_center = 1;

                m_center = c;
                m_center_pos_tex = 1;
                return (ref)*this;
            }
            ref pixel_center(fvec2 c) // Same as `center()`, but the coordinates are always measured in pixels instead of texels even if a texture is specified.
            {
                DebugAssert("2D poly renderer: Quad_t center specified twice.", !has_center);
                has_center = 1;

                m_center = c;
                m_center_pos_tex = 0;
                return (ref)*this;
            }
            ref center()
            {
                pixel_center(m_size / 2);
                return (ref)*this;
            }
            ref matrix(fmat3 m) // This can be called multiple times, resulting in multiplying matrices in the order they were passed.
            {
                if (has_matrix)
                    m_matrix = m_matrix /mul/ m;
                else
                {
                    has_matrix = 1;
                    m_matrix = m;
                }
                return (ref)*this;
            }
            ref matrix(fmat2 m)
            {
                matrix(m.to_mat3());
                return (ref)*this;
            }
            ref rotate(float a) // Uses `matrix()`.
            {
                matrix(fmat3::rotate2D(a));
                return (ref)*this;
            }
            ref translate(ivec2 v) // Uses a matrix.
            {
                translate_f(v);
                return (ref)*this;
            }
            ref translate_f(fvec2 v) // Uses a matrix.
            {
                matrix(fmat3::translate2D(v));
                return (ref)*this;
            }
            ref scale(ivec2 s) // Uses a matrix.
            {
                scale_f(s);
                return (ref)*this;
            }
            ref scale(int s) // Uses a matrix.
            {
                scale_f(s);
                return (ref)*this;
            }
            ref scale_f(fvec2 s) // Uses a matrix.
            {
                matrix(fmat3::scale(s));
                return (ref)*this;
            }
            ref scale_f(float s) // Uses a matrix.
            {
                scale_f(fvec2(s));
                return (ref)*this;
            }
            ref alpha(float a)
            {
                for (auto &it : m_alpha)
                    it = a;
                return (ref)*this;
            }
            ref alpha(float a, float b, float c, float d)
            {
                m_alpha[0] = a;
                m_alpha[1] = b;
                m_alpha[2] = c;
                m_alpha[3] = d;
                return (ref)*this;
            }
            ref beta(float a) // 1 - normal blending, 0 - additive blending
            {
                for (auto &it : m_beta)
                    it = a;
                return (ref)*this;
            }
            ref beta(float a, float b, float c, float d)
            {
                m_beta[0] = a;
                m_beta[1] = b;
                m_beta[2] = c;
                m_beta[3] = d;
                return (ref)*this;
            }
            ref absolute(bool x = 1) // Interpret size as a position of the second corner
            {
                m_abs_pos = x;
                return (ref)*this;
            }
            ref absolute_tex(bool x = 1) // Interpret texture size as a position of the second corner
            {
                m_abs_tex_pos = x;
                return (ref)*this;
            }
            ref flip_x(bool f = 1) // Flips texture horizontally if it was specified. Updates the center accordingly if it was specified.
            {
                m_flip_x = f;
                return (ref)*this;
            }
            ref flip_y(bool f = 1) // Flips texture vertically if it was specified. Updates the center accordingly if it was specified.
            {
                m_flip_y = f;
                return (ref)*this;
            }
        };
        #if 0
        class Triangle_t : TemplateUtils::MoveFunc<Triangle_t>
        {
            using ref = Triangle_t &&;

            // The constructor sets those:
            decltype(Poly2D_PBR::queue) *queue;
            fvec2 m_pos, m_vectices[3];

            bool has_texture = 0;
            fvec2 m_tex_pos[3] = {};

            bool has_matrix = 0;
            fmat3 m_matrix = fmat3::identity();

            bool has_color = 0;
            fvec3 m_colors[3] {};

            bool has_tex_color_fac = 0;
            float m_tex_color_factors[3] = {1,1,1};

            float m_alpha[3] = {1,1,1};
            float m_beta[3] = {1,1,1};

            static void OnMove(Triangle_t &&from, Triangle_t &/*to*/)
            {
                from.queue = 0;
            }
          public:
            Triangle_t(decltype(Poly2D_PBR::queue) *queue, fvec2 pos, fvec2 a, fvec2 b, fvec2 c) : queue(queue), m_pos(pos), m_vectices{a, b, c} {}

            Triangle_t(const Triangle_t &) = delete;
            Triangle_t &operator=(const Triangle_t &) = delete;

            Triangle_t(Triangle_t &&) = default;
            Triangle_t &operator=(Triangle_t &&) = default;

            ~Triangle_t()
            {
                if (!queue)
                    return;

                DebugAssert("2D poly renderer: Triangle with no texture nor color specified.", has_texture || has_color);
                DebugAssert("2D poly renderer: Triangle with texture and color, but without a mixing factor.", (has_texture && has_color) == has_tex_color_fac);

                Poly2D_PBR_impl::Attributes out[3];

                if (has_texture)
                {
                    for (int i = 0; i < 3; i++)
                    {
                        out[i].color = m_colors[i].to_vec4(0);
                        out[i].factors.x = m_tex_color_factors[i];
                        out[i].factors.y = m_alpha[i];
                    }
                }
                else
                {
                    for (int i = 0; i < 3; i++)
                    {
                        out[i].color = m_colors[i].to_vec4(m_alpha[i]);
                        out[i].factors.x = out[i].factors.y = 0;
                    }
                }

                for (int i = 0; i < 3; i++)
                {
                    out[i].factors.z = m_beta[i];
                    out[i].tex_pos = m_tex_pos[i];
                }

                if (has_matrix)
                {
                    for (int i = 0; i < 3; i++)
                        out[i].pos = m_pos + (m_matrix /mul/ m_vectices[i].to_vec3(1)).to_vec2();
                }
                else
                {
                    for (int i = 0; i < 3; i++)
                        out[i].pos = m_pos + m_vectices[i];
                }

                queue->Triangle(out[0], out[1], out[2]);
            }

            ref tex(ivec2 pos)
            {
                tex_f(pos);
                return (ref)*this;
            }
            ref tex(ivec2 a, ivec2 b, ivec2 c)
            {
                tex_f(a, b, c);
                return (ref)*this;
            }
            ref tex_f(fvec2 pos)
            {
                tex_f(pos, pos, pos);
                return (ref)*this;
            }
            ref tex_f(fvec2 a, fvec2 b, fvec2 c)
            {
                DebugAssert("2D poly renderer: Triangle texture specified twice.", !has_texture);
                has_texture = 1;

                m_tex_pos[0] = a;
                m_tex_pos[1] = b;
                m_tex_pos[2] = c;
                return (ref)*this;
            }
            ref matrix(fmat3 m) // This can be called multiple times, resulting in multiplying matrices in the order they were passed.
            {
                if (has_matrix)
                    m_matrix = m_matrix /mul/ m;
                else
                {
                    has_matrix = 1;
                    m_matrix = m;
                }
                return (ref)*this;
            }
            ref matrix(fmat2 m)
            {
                matrix(m.to_mat3());
                return (ref)*this;
            }
            ref rotate(float a) // Uses `matrix()`.
            {
                matrix(fmat3::rotate2D(a));
                return (ref)*this;
            }
            ref translate(ivec2 v) // Uses a matrix.
            {
                translate_f(v);
                return (ref)*this;
            }
            ref translate_f(fvec2 v) // Uses a matrix.
            {
                matrix(fmat3::translate2D(v));
                return (ref)*this;
            }
            ref scale(ivec2 s) // Uses a matrix.
            {
                scale_f(s);
                return (ref)*this;
            }
            ref scale(int s) // Uses a matrix.
            {
                scale_f(s);
                return (ref)*this;
            }
            ref scale_f(fvec2 s) // Uses a matrix.
            {
                matrix(fmat3::scale(s));
                return (ref)*this;
            }
            ref scale_f(float s) // Uses a matrix.
            {
                scale_f(fvec2(s));
                return (ref)*this;
            }
            ref color(fvec3 c)
            {
                DebugAssert("2D poly renderer: Triangle color specified twice.", !has_color);
                has_color = 1;

                for (auto &it : m_colors)
                    it = c;
                return (ref)*this;
            }
            ref color(fvec3 a, fvec3 b, fvec3 c)
            {
                DebugAssert("2D poly renderer: Triangle color specified twice.", !has_color);
                has_color = 1;

                m_colors[0] = a;
                m_colors[1] = b;
                m_colors[2] = c;
                return (ref)*this;
            }
            ref mix(float x) // 0 - fill with color, 1 - use texture
            {
                DebugAssert("2D poly renderer: Triangle texture/color factor specified twice.", !has_tex_color_fac);
                has_tex_color_fac = 1;

                for (auto &it : m_tex_color_factors)
                    it = x;
                return (ref)*this;
            }
            ref mix(float a, float b, float c)
            {
                DebugAssert("2D poly renderer: Triangle texture/color factor specified twice.", !has_tex_color_fac);
                has_tex_color_fac = 1;

                m_tex_color_factors[0] = a;
                m_tex_color_factors[1] = b;
                m_tex_color_factors[2] = c;
                return (ref)*this;
            }
            ref alpha(float a)
            {
                for (auto &it : m_alpha)
                    it = a;
                return (ref)*this;
            }
            ref alpha(float a, float b, float c)
            {
                m_alpha[0] = a;
                m_alpha[1] = b;
                m_alpha[2] = c;
                return (ref)*this;
            }
            ref beta(float a) // 1 - normal blending, 0 - additive blending
            {
                for (auto &it : m_beta)
                    it = a;
                return (ref)*this;
            }
            ref beta(float a, float b, float c)
            {
                m_beta[0] = a;
                m_beta[1] = b;
                m_beta[2] = c;
                return (ref)*this;
            }
        };
        class Text_t : TemplateUtils::MoveFunc<Text_t>
        {
            using ref = Text_t &&;

            // The constructor sets those:
            decltype(Poly2D_PBR::queue) *queue;
            const Graphics::CharMap *m_ch_map;
            fvec2 m_pos;
            std::string_view m_str;

            fmat3 m_matrix = fmat3::identity();
            fvec3 m_color = {1,1,1};
            float m_alpha = 1, m_beta = 1;
            int m_spacing = 0;
            bool m_kerning = 1;
            ivec2 m_offset = {0,0};
            bool m_continue = 0;

            static void OnMove(Text_t &&from, Text_t &/*to*/)
            {
                from.queue = 0;
            }

            void Render()
            {
                DebugAssert("2D poly renderer: Text with no font specified.", m_ch_map != 0);

                if (!queue)
                    return;

                auto it = m_str.begin();

                uint16_t prev_ch = 0xffff;

                while (it != m_str.end())
                {
                    if (!u8isfirstbyte(it))
                    {
                        it++;
                        continue;
                    }

                    uint16_t ch = u8decode(it);

                    const auto &info = m_ch_map->Get(ch);

                    if (m_kerning)
                        m_offset.x += m_ch_map->Kerning(prev_ch, ch);

                    Quad_t(queue, m_pos, info.size)
                        .tex(info.tex_pos)
                        .alpha(m_alpha).beta(m_beta).color(m_color).mix(0)
                        .center(info.size.div_x(2)).matrix(m_matrix /mul/ fmat3::translate2D(m_offset + info.offset + ivec2(info.size.x / 2, m_ch_map->Ascent() + info.size.y)));

                    m_offset.x += info.advance + m_spacing;
                    prev_ch = ch;

                    it++;
                }
            }

          public:
            Text_t(decltype(Poly2D_PBR::queue) *queue, const Graphics::CharMap *ch_map, fvec2 pos, std::string_view str) : queue(queue), m_ch_map(ch_map), m_pos(pos), m_str(str) {}

            Text_t(const Text_t &) = delete;
            Text_t &operator=(const Text_t &) = delete;

            Text_t(Text_t &&) = default;
            Text_t &operator=(Text_t &&) = default;

            ~Text_t()
            {
                Render();
            }

            ref font(Graphics::CharMap &&) = delete;
            ref font(const Graphics::CharMap &map)
            {
                m_ch_map = &map;
                return (ref)*this;
            }
            ref move(ivec2 o) // Usual translation.
            {
                m_pos += o;
                return (ref)*this;
            }
            ref offset(ivec2 o) // This is not for usual translation, but for moving the first line only (useful for combining) styles.
            {
                m_offset += o;
                return (ref)*this;
            }
            ref append(std::string_view str)
            {
                Render();
                m_continue = 1;
                m_str = str;
                return (ref)*this;
            }
            ref render_this()
            {
                ivec2 pos_copy = m_pos;
                ivec2 offset_copy = m_offset;
                Render();
                m_pos = pos_copy;
                m_offset = offset_copy;
                return (ref)*this;
            }

            ref matrix(fmat3 m) // Multiplies the matrix by `m`.
            {
                m_matrix = m_matrix /mul/ m;
                return (ref)*this;
            }
            ref back_matrix(fmat3 m) // Multiplies `m` by the matrix.
            {
                m_matrix = m /mul/ m_matrix;
                return (ref)*this;
            }
            ref matrix(fmat2 m) // Multiplies the matrix by `m`.
            {
                matrix(m.to_mat3());
                return (ref)*this;
            }
            ref back_matrix(fmat2 m) // Multiplies `m` by the matrix.
            {
                back_matrix(m.to_mat3());
                return (ref)*this;
            }
            ref rotate(float a) // Uses `matrix()`.
            {
                matrix(fmat3::rotate2D(a));
                return (ref)*this;
            }
            ref scale(ivec2 s) // Uses `matrix()`.
            {
                scale_f(s);
                return (ref)*this;
            }
            ref scale_f(fvec2 s) // Uses `matrix()`.
            {
                matrix(fmat3::scale(s));
                return (ref)*this;
            }
            ref scale(int s) // Uses `matrix()`.
            {
                scale(ivec2(s));
                return (ref)*this;
            }
            ref scale_f(float s) // Uses `matrix()`.
            {
                scale(fvec2(s));
                return (ref)*this;
            }
            ref color(fvec3 c)
            {
                m_color = c;
                return (ref)*this;
            }
            ref alpha(float a)
            {
                m_alpha = a;
                return (ref)*this;
            }
            ref beta(float b)
            {
                m_beta = b;
                return (ref)*this;
            }
            ref spacing(int s)
            {
                spacing_f(s);
                return (ref)*this;
            }
            ref spacing_f(float s)
            {
                m_spacing += s;
                return (ref)*this;
            }
            ref kerning(bool k) // Enabled by default
            {
                m_kerning = k;
                return (ref)*this;
            }
        };
        #endif

        Poly2D_PBR() {}
        Poly2D_PBR(ivec2 scr, int size, const Graphics::Texture &lut)
        {
            Create(scr, size, lut);
        }
        void Create(ivec2 scr, int size, const Graphics::Texture &lut)
        {
            scr_sz = scr;
            queue.Create(size);

            sh_main .Create<Poly2D_PBR_impl::AttrMain>("PBR main" , Poly2D_PBR_impl::ShaderSource::Main ::v, Poly2D_PBR_impl::ShaderSource::Main ::f, &uni_main );
            sh_light.Create<Poly2D_PBR_impl::AttrMain>("PBR light", Poly2D_PBR_impl::ShaderSource::Light::v, Poly2D_PBR_impl::ShaderSource::Light::f, &uni_light);
            sh_exbr .Create<Poly2D_PBR_impl::AttrTex >("PBR exbr" , Poly2D_PBR_impl::ShaderSource::ExBr ::v, Poly2D_PBR_impl::ShaderSource::ExBr ::f, &uni_exbr );
            sh_blur .Create<Poly2D_PBR_impl::AttrTex >("PBR blur" , Poly2D_PBR_impl::ShaderSource::Blur ::v, Poly2D_PBR_impl::ShaderSource::Blur ::f, &uni_blur );
            sh_copy .Create<Poly2D_PBR_impl::AttrTex >("PBR copy" , Poly2D_PBR_impl::ShaderSource::Copy ::v, Poly2D_PBR_impl::ShaderSource::Copy ::f, &uni_copy );
            sh_final.Create<Poly2D_PBR_impl::AttrTex >("PBR final", Poly2D_PBR_impl::ShaderSource::Final::v, Poly2D_PBR_impl::ShaderSource::Final::f, &uni_final);
            sh_iden .Create<Poly2D_PBR_impl::AttrTex >("PBR iden" , Poly2D_PBR_impl::ShaderSource::Iden ::v, Poly2D_PBR_impl::ShaderSource::Iden ::f, &uni_iden );
            uni_main.lookup = lut;
            SetMatrix(fmat4::identity());

            tex_framebuffer_hdr.Create();
            tex_framebuffer_hdr.SetData(GL_RGB16F, GL_RGB, GL_UNSIGNED_BYTE, scr);
            tex_framebuffer_hdr.Interpolation(Graphics::Texture::nearest);
            tex_framebuffer_hdr_depth.Create();
            tex_framebuffer_hdr_depth.SetData(GL_R32F, GL_RGB, GL_UNSIGNED_BYTE, scr);
            tex_framebuffer_hdr_depth.Interpolation(Graphics::Texture::nearest);
            framebuffer_hdr.Create();
            framebuffer_hdr.Attach({tex_framebuffer_hdr, tex_framebuffer_hdr_depth});
            framebuffer_hdr.Unbind();
            framebuffer_hdr_color.Create();
            framebuffer_hdr_color.Attach(tex_framebuffer_hdr);
            framebuffer_hdr_color.Unbind();

            tex_framebuffer_final.Create();
            tex_framebuffer_final.Attach();
            tex_framebuffer_final.Interpolation(Graphics::Texture::linear);
            framebuffer_final.Create();
            framebuffer_final.Attach(tex_framebuffer_final);
            framebuffer_final.Unbind();

            for (auto it : {&tex_framebuffer_bloom1, &tex_framebuffer_bloom2})
            {
                it->Create();
                it->SetData(GL_RGB8, GL_RGB, GL_UNSIGNED_BYTE, scr);
                it->Interpolation(Graphics::Texture::linear);
                it->Wrap(Graphics::Texture::clamp);
            }
            framebuffer_bloom1.Create();
            framebuffer_bloom2.Create();
            framebuffer_bloom1.Attach(tex_framebuffer_bloom1);
            framebuffer_bloom2.Attach(tex_framebuffer_bloom2);

            uni_main.emission_factor = 4;

            uni_light.tex_depth = tex_framebuffer_hdr_depth;

            uni_exbr.texture = tex_framebuffer_hdr;
            uni_exbr.threshold = 1.1;

            uni_blur.step = 1. / scr;

            uni_copy.texture = tex_framebuffer_bloom1;
            uni_copy.factor = 0.05;

            uni_final.texture = tex_framebuffer_hdr;
            uni_final.exposure = 0;

            uni_iden.texture = tex_framebuffer_final;
        }
        /*
        void Destroy()
        {
            shader.Destroy();
            queue.Destroy();
        }*/

        void Resize(ivec2 sz)
        {
            real_sz = sz;
            scale = (fvec2(sz) / scr_sz).min();
            float scale_fl = floor(scale);
            tex_framebuffer_final.SetData(GL_RGB8, GL_RGB, GL_UNSIGNED_BYTE, scr_sz * scale_fl);

            mouse.offset = sz / 2;
            mouse.scale = 1 / scale;
        }

        void DrawAmbient()
        {
            Graphics::Blending::Enable();
            sh_main.Bind();
            framebuffer_hdr.Bind();
            Graphics::Clear(Graphics::color);
            queue.DrawNoReset();
            framebuffer_hdr_color.Bind();
            sh_light.Bind();
        }

        void PointLight(fvec3 color, fvec3 pos, float rad = 0)
        {
            uni_light.light_pos = pos;
            uni_light.light_color = color;
            uni_light.light_radius = rad;
            uni_light.light_tube = 0;
            queue.DrawNoReset();
        }
        void LineLight(fvec3 color, fvec3 a, fvec3 b, float rad = 0)
        {
            uni_light.light_pos = a;
            uni_light.light_end = b;
            uni_light.light_color = color;
            uni_light.light_radius = rad;
            uni_light.light_tube = 1;
            queue.DrawNoReset();
        }

        void DrawStart()
        {
            Graphics::Viewport(scr_sz);
        }

        void DrawFinal()
        {
            queue.Reset();

            Graphics::Blending::Disable();

            framebuffer_bloom1.Bind();
            sh_exbr.Bind();
            FullscreenQuad();

            sh_blur.Bind();

            #if 0 // Render bloom only
            framebuffer_hdr.Bind();
            Graphics::Clear(Graphics::color);
            #endif

            for (int i = 0; i < blur_passes; i++)
            {
                framebuffer_bloom2.Bind();
                uni_blur.vertical = 0;
                uni_blur.texture = tex_framebuffer_bloom1;
                FullscreenQuad();

                framebuffer_bloom1.Bind();
                uni_blur.vertical = 1;
                uni_blur.texture = tex_framebuffer_bloom2;
                FullscreenQuad();

                Graphics::Blending::Enable();
                framebuffer_hdr.Bind();
                sh_copy.Bind();
                FullscreenQuad();
                Graphics::Blending::Disable();
            }

            framebuffer_final.Bind();
            sh_final.Bind();
            Graphics::Viewport(tex_framebuffer_final.Size());
            FullscreenQuad();

            Graphics::FrameBuffer::Unbind();
            sh_iden.Bind();
            Graphics::Viewport(real_sz/2 - scr_sz * scale / 2, scr_sz * scale);
            FullscreenQuad();
        }

        void Exposure(float e)
        {
            uni_final.exposure = e;
        }

        void SetMatrix(fmat4 m) // Binds a shader. Only the last call has effect for each frame.
        {
            uni_main.matrix = m;
            uni_light.matrix = m;
        }
        void SetBackground(fvec3 c) // Binds a shader. Only the last call has effect for each frame.
        {
            uni_main.background = c;
        }

        void SetTextures(const Graphics::Texture &tex_albedo,
                         const Graphics::Texture &tex_normal,
                         const Graphics::Texture &tex_m_r_h_ao,
                         const Graphics::Texture &tex_emission) // Binds a shader. Only the last call has effect for each frame.
        {
            uni_main.tex_size = tex_albedo.Size();
            uni_main.tex_albedo   = tex_albedo;
            uni_main.tex_normal   = tex_normal;
            uni_main.tex_m_r_h_ao = tex_m_r_h_ao;
            uni_main.tex_emission = tex_emission;
            uni_light.tex_size = tex_albedo.Size();
            uni_light.tex_albedo   = tex_albedo;
            uni_light.tex_normal   = tex_normal;
            uni_light.tex_m_r_h_ao = tex_m_r_h_ao;
        }

        void SetDefaultFont(const Graphics::CharMap &map)
        {
            ch_map = &map;
        }

        Quad_t Quad(ivec2 pos, float d, ivec2 size)
        {
            return {&queue, pos, d, size};
        }
        Quad_t Quad_f(fvec2 pos, float d, fvec2 size)
        {
            return {&queue, pos, d, size};
        }
        #if 0
        Triangle_t Triangle(ivec2 pos, ivec2 a, ivec2 b, ivec2 c)
        {
            return {&queue, pos, a, b, c};
        }
        Triangle_t Triangle_f(fvec2 pos, fvec2 a, fvec2 b, fvec2 c)
        {
            return {&queue, pos, a, b, c};
        }
        Text_t Text(ivec2 pos, std::string_view str)
        {
            return {&queue, ch_map, pos, str};
        }
        Text_t Text_f(fvec2 pos, std::string_view str)
        {
            return {&queue, ch_map, pos, str};
        }
        #endif
    };
}

#endif
