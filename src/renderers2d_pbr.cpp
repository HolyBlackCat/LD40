#include "renderers2d_pbr.h"

namespace Renderers::Poly2D_PBR_impl::ShaderSource
{
    namespace Main
    {
        const char *const v = R"(
VARYING(float, depth)
VARYING(vec2, tex_pos)
VARYING(vec2, alpha_beta)
VARYING(mat2, normal_mat)
void main()
{
    gl_Position = u_matrix * vec4(a_pos.xy, 0, 1);
    v_depth = a_pos.z;
    v_tex_pos = a_tex_pos / u_tex_size;
    v_alpha_beta = a_alpha_beta;
    v_normal_mat = mat2(a_normal_mat.x, a_normal_mat.y, a_normal_mat.z, a_normal_mat.w);
})";
        const char *const f = R"(
VARYING(float, depth)
VARYING(vec2, tex_pos)
VARYING(vec2, alpha_beta)
VARYING(mat2, normal_mat)

const float PI = 3.14159265359;

vec3 fresnelSchlickRoughness(float cosTheta, vec3 F0, float roughness)
{
    return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

void main()
{
    vec4 albedo_a = texture2D(u_tex_albedo, v_tex_pos);
    vec3 albedo   = albedo_a.xyz;
    vec3 N        = texture2D(u_tex_normal, v_tex_pos).xyz * vec3(2,2,-2) - 1.;
    vec4 stats    = texture2D(u_tex_m_r_h_ao, v_tex_pos);
    vec3 emission = texture2D(u_tex_emission, v_tex_pos).xyz * u_emission_factor;
    float metallic  = stats.x;
    float roughness = stats.y;
    float depth     = (0.5 - stats.z) * 256. / 5. + v_depth;
    float ao        = stats.w;

    N = normalize(mat3(v_normal_mat) * N);
    const vec3 V = vec3(0,0,-1);

    vec3 F0 = vec3(0.04);
    F0 = mix(F0, albedo, metallic);

    // ambient lighting (we now use IBL as the ambient term)
    vec3 F = fresnelSchlickRoughness(max(dot(N, V), 0.0), F0, roughness);

    vec3 kS = F;
    vec3 kD = 1.0 - kS;
    kD *= 1.0 - metallic;

    vec3 irradiance = u_background;
    vec3 diffuse      = irradiance * albedo;

    // sample both the pre-filter map and the BRDF lut and combine them together as per the Split-Sum approximation to get the IBL specular part.
    vec3 prefilteredColor = u_background; // for reflections
    vec2 brdf = texture2D(u_lookup, vec2(max(dot(N, V), 0.0), roughness)).rg;
    vec3 specular = prefilteredColor * (F * brdf.x + brdf.y);

    vec3 ambient = (kD * diffuse + specular) * ao;

    vec4 final = vec4(ambient + emission, v_alpha_beta.x * albedo_a.a);
    final.rgb *= final.a;
    final.a *= v_alpha_beta.y;
    gl_FragData[0] = final;
    if (final.a < 255./256. + 1./512.)
        gl_FragData[1] = vec4(0);
    else
        gl_FragData[1] = vec4(depth, 0, 0, 1);
})";
    }
    namespace Light
    {
        const char *const v = R"(
VARYING(vec2, sspos)
VARYING(vec2, pos)
VARYING(float, depth)
VARYING(vec2, tex_pos)
VARYING(mat2, normal_mat)
void main()
{
    v_pos = a_pos.xy;
    gl_Position = u_matrix * vec4(a_pos.xy, 0, 1);
    v_sspos = gl_Position.xy / 2. + .5;
    v_depth = a_pos.z;
    v_tex_pos = a_tex_pos / u_tex_size;
    v_normal_mat = mat2(a_normal_mat.x, a_normal_mat.y, a_normal_mat.z, a_normal_mat.w);
})";
        const char *const f = R"(
VARYING(vec2, sspos)
VARYING(vec2, pos)
VARYING(float, depth)
VARYING(vec2, tex_pos)
VARYING(mat2, normal_mat)

const float PI = 3.14159265359;

float DistributionGGX(float NdotH, float a)
{
    float a2     = a * a;
    float NdotH2 = NdotH*NdotH;

    float nom   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return nom / denom;
}
float GeometrySmith(float NdotV, float NdotL, float roughness)
{
    float r = roughness + 1.0;
    float k = (r*r) / 8.0;
    float f = 1. - k;
    float ggx2  = NdotV / (NdotV * f + k);
    float ggx1  = NdotL / (NdotL * f + k);

    return ggx1 * ggx2;
}
vec3 fresnelSchlick(float cosTheta, vec3 F0)
{
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

void main()
{
    const float min_roughness = 1./256.;

    vec4 stats    = texture2D(u_tex_m_r_h_ao, v_tex_pos);
    float depth     = (0.5 - stats.z) * 256. / 5. + v_depth;
    if (depth > texture2D(u_tex_depth, v_sspos).r+0.5) discard;

    vec4 albedo_a = texture2D(u_tex_albedo, v_tex_pos);
    vec3 albedo   = albedo_a.xyz;
    vec3 N        = texture2D(u_tex_normal, v_tex_pos).xyz * vec3(2,2,-2) - 1.;
    float metallic  = stats.x;
    float roughness = stats.y * (1.-min_roughness) + min_roughness;
    float ao        = stats.w;

    N = normalize(mat3(v_normal_mat) * N);
    const vec3 V = vec3(0,0,-1);

    float a = roughness * roughness;

    vec3 pos = vec3(v_pos, depth);

    float NdotV = max(dot(N, V), 0.0);

    vec3 r = reflect(-V, N);

    vec3 F0 = vec3(0.04);
    F0 = mix(F0, albedo, metallic);

        float light_radius = u_light_radius;
        vec3 radiance = u_light_color;

        vec3 L = u_light_pos - pos;

        if (u_light_tube) // is tube
        {
            vec3 L2 = u_light_end - pos;
            vec3 Ld = L2 - L;
            float len = length(L);
            float len2 = length(L2);
            float lend = length(Ld);
            // This is broken:
            radiance *= 2 * clamp(dot(N, L) / (2 * len) + dot(N, L2) / (2 * len2), 0.0, 1.0) / (len * len2 + dot(L, L2) + 2);
            // Ugh, whatever.
            float r_dot_ld = dot(r, Ld);
            float t = (dot(r, L) * dot(r, Ld) - dot(L, Ld)) / (lend * lend - r_dot_ld * r_dot_ld); // This is an approximation. Epic games paper has another better & slower one.
            L += Ld * clamp(t, 0.0, 1.0);
        }

        if (light_radius != 0)
        {
            vec3 v = dot(L, r) * r - L;
            L += v * clamp(light_radius / length(v), 0.0, 1.0);
        }
        float distance = length(L);
        L /= distance;

        vec3 H = normalize(V + L);

        radiance /= (distance * distance);

        float ap = clamp(light_radius / (distance * 2.0) + a, 0.0, 1.0);

        float norm_factor;
        if (u_light_tube)
            norm_factor = a / ap;
        else
            norm_factor = a * a / ap / ap;

        float NdotL = max(dot(N, L), 0.0);
        float NdotH = max(dot(N, H), 0.0);
        float HdotV = max(dot(H, V), 0.0);

        // cook-torrance brdf
        float NDF = DistributionGGX(NdotH, a) * norm_factor;
        float G   = GeometrySmith(NdotV, NdotL, roughness);
        vec3 F    = fresnelSchlick(HdotV, F0);

        vec3 kS = F;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;

        vec3 nominator    = NDF * G * F;
        float denominator = 4 * NdotV * NdotL + 0.001;
        vec3 specular     = nominator / denominator;

        vec3 Lo = (kD * albedo / PI + specular) * radiance * NdotL;

    gl_FragColor = vec4(Lo * ao * albedo_a.a, 0.0);
})";
    }
    namespace ExBr // Extract bright
    {
        const char *const v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        const char *const f = R"(
VARYING(vec2, pos)
void main()
{
    vec3 color = texture2D(u_texture, v_pos).rgb;
    float b = dot(color, vec3(0.2126, 0.7152, 0.0722));
    b -= u_threshold;
    gl_FragColor = vec4(color * b, 1);
})";
    }
    namespace Blur
    {
        const char *const v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        const char *const f = R"(
VARYING(vec2, pos)
#define R 7
void main()
{
    const float c[R] = float[](0.149446, 0.139483, 0.11333, 0.0799976, 0.0488874, 0.0257302, 0.0115786);
    vec3 color = texture2D(u_texture, v_pos).rgb * c[0];
    if (u_vertical)
    {
        for(int i = 1; i < R; ++i)
        {
            float s = u_step.y * i;
            color += texture(u_texture, v_pos + vec2(0.0, s)).rgb * c[i];
            color += texture(u_texture, v_pos - vec2(0.0, s)).rgb * c[i];
        }
    }
    else
    {
        for(int i = 1; i < R; ++i)
        {
            float s = u_step.x * i;
            color += texture(u_texture, v_pos + vec2(s, 0.0)).rgb * c[i];
            color += texture(u_texture, v_pos - vec2(s, 0.0)).rgb * c[i];
        }
    }
    gl_FragColor = vec4(color,1);
})";
    }
    namespace Copy
    {
        const char *const v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        const char *const f = R"(
VARYING(vec2, pos)
void main()
{
    vec4 c = texture2D(u_texture, v_pos);
    gl_FragColor = vec4(c.rgb * u_factor, 0);
})";
    }
    namespace Final
    {
        const char *const v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        const char *const f = R"(
VARYING(vec2, pos)
void main()
{
    vec4 tex_color = texture2D(u_texture, v_pos);
    vec3 color = tex_color.rgb;
    color = vec3(1.0) - exp(-color * exp(u_exposure));
    color = pow(color, vec3(1.0/2.2));
    gl_FragColor = vec4(color, 1);
})";
    }
    namespace Iden
    {
        const char *const v = R"(
VARYING(vec2, pos)
void main()
{
    v_pos = a_pos;
    gl_Position = vec4(a_pos * 2. - 1., 0, 1);
})";
        const char *const f = R"(
VARYING(vec2, pos)
void main()
{
    gl_FragColor = vec4(texture2D(u_texture, v_pos).rgb, 1);
    //gl_FragColor = vec4(v_pos, 0, 1);
})";
    }
}
