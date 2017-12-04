#include "everything.h"

#include <iostream>
#include <deque>
#include <random>
#include <vector>

namespace Keys = Input::Keys;

static constexpr ivec2 screen_sz(480,270);

Window win;
Renderers::Poly2D_PBR r;
Graphics::Texture tex_lookup, tex_albedo, tex_normal, tex_m_r_h_ao, tex_emission;

Audio::Context audio_con;
Timing::TickStabilizer ts;

Input::Mouse &mouse = r.mouse;

namespace Rand
{
    std::mt19937 rng;
    int Int(int from, int to)
    {
        std::uniform_int_distribution<int> d(from, to);
        return d(rng);
    }
    float Float(float from, float to)
    {
        std::uniform_real_distribution<float> d(from, to);
        return d(rng);
    }
}
static constexpr ivec2 circle[] {{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}};

namespace Sounds
{
    #define SOUND_LIST \
        SOUND( death               , 0.2  ) \
        SOUND( restart             , 0.2  ) \
        SOUND( laser               , 0.35 ) \
        SOUND( laser_hitting       , 0.3  ) \
        SOUND( block_breaks        , 0.2  ) \
        SOUND( trash_block_breaks  , 0.2  ) \
        SOUND( block_attaches      , 0.3  ) \
        SOUND( mine_explodes       , 0.2  ) \
        SOUND( mine_spawns         , 0.2  ) \
        SOUND( checkpoint          , 0.2  ) \
        SOUND( boss_trash          , 0.2  ) \
        SOUND( boss_hit            , 0.2  ) \
        SOUND( boss_charge         , 0.2  ) \
        SOUND( boss_dash           , 0.2  ) \
        SOUND( boss_mines          , 0.2  ) \
        SOUND( boss_death          , 0    ) \
        SOUND( boss_start          , 0.2  ) \
        SOUND( boss_wings_move     , 0.2  ) \
        SOUND( goal                , 0    ) \

    #define SOUND(NAME, RAND) \
        auto NAME(fvec2 pos, float vol = 1, float pitch = 0) \
        { \
            static Audio::Buffer buf = Audio::Sound::WAV("assets/" #NAME ".wav"); \
            return buf(vol, std::pow(2, pitch + Rand::Float(-1,1) * RAND)).pos(pos.to_vec3()); \
        }

    SOUND_LIST
    #undef SOUND

    Audio::Buffer theme_buf;
    Audio::Source theme;
    constexpr float theme_vol = 1/3.;
    bool theme_plays = 1;
}

void Init()
{
    Events::SetErrorHandlers();
    win.Create("Ni by HolyBlackCat", screen_sz * 2, Window::Settings{}.Resizable(1).MinSize(screen_sz));
    win.Fullscreen(1);

    Graphics::Image img_lookup   ("assets/lookup.gnp"),
                    img_albedo   ("assets/texture-albedo.gnp"),
                    img_ao       ("assets/texture-ao.gnp"),
                    img_emission ("assets/texture-emission.gnp"),
                    img_metallic ("assets/texture-metallic.gnp"),
                    img_roughness("assets/texture-roughness.gnp"),
                    img_height   ("assets/texture-height.gnp"),
                    img_normal   ("assets/texture-normal.gnp");

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
    tex_albedo.Interpolation(Graphics::Texture::nearest);
    tex_albedo.SetData(img_albedo);
    tex_normal.Create();
    tex_normal.Interpolation(Graphics::Texture::nearest);
    tex_normal.SetData(img_normal);
    tex_m_r_h_ao.Create();
    tex_m_r_h_ao.Interpolation(Graphics::Texture::nearest);
    tex_m_r_h_ao.SetData(img_m_r_h_ao);
    tex_emission.Create();
    tex_emission.Interpolation(Graphics::Texture::nearest);
    tex_emission.SetData(img_emission);

    r.Create(screen_sz, 0x10000, tex_lookup);
    r.SetTextures(tex_albedo, tex_normal, tex_m_r_h_ao, tex_emission);

    Graphics::Blending::FuncNormalPre();

    Audio::Volume(2);
    Audio::Source::DefaultRefDistance(200);
    Audio::Source::DefaultRolloffFactor(1);

    Sounds::theme_buf.Create();
    Sounds::theme_buf.SetData(Audio::Sound::OGG("assets/theme.ogg"));
    Sounds::theme.Create(Sounds::theme_buf);
    Sounds::theme.loop(1).volume(Sounds::theme_vol).play();
}

namespace Tiles
{
    enum class Re
    {
        spawn,
        trash,
        homing_mine,
        mine_sp,
        las_turret,
        checkpoint,
        key,
        one_way,
        one_way_r,
        one_way_d,
        boss,
        _rec_count,
    };

    #define TILE_LIST \
        /*    name              color            render mode              group  light            solid breaks*/\
        TILE( air               , 255,255,255,255, invis()                , 0 , 0                  , 0 , 0 , ) \
        TILE( wall              ,  0 , 0 , 0 ,255, rotate(0)              , 1 , 0                  , 1 , 0 , ) \
        TILE( wall_back         ,  63, 63, 63,255, rotate(4)              , 1 , 0                  , 0 , 0 , ) \
        TILE( light             , 255,255, 0 ,255, norm(1)                , 0 , fvec3(1,.8,.6)*550 , 1 , 0 , ) \
        TILE( spawn             ,  0 , 0 ,255,255, invis(Re::spawn)       , 0 , 0                  , 0 , 0 , ) \
        TILE( breakable         , 127,127,127,255, norm(2)                , 2 , 0                  , 1 , 1 , ) \
        TILE( help_sign         ,  0 ,127, 0 ,255, decal(208,215,64,64)   , 0 , 0                  , 0 , 0 , ) \
        TILE( help_sign_2       ,  0 , 63, 0 ,255, decal(286,215,120,24)  , 0 , 0                  , 0 , 0 , ) \
        TILE( trash_block       , 255, 0 , 0 ,255, invis(Re::trash)       , 0 , 0                  , 0 , 0 , ) \
        TILE( homing_mine       , 255,127, 0 ,255, invis(Re::homing_mine) , 0 , 0                  , 0 , 0 , ) \
        TILE( laser_turret      , 127, 0 , 0 ,255, invis(Re::las_turret)  , 0 , 0                  , 0 , 0 , ) \
        TILE( mine_spawn        , 255,191,127,255, invis(Re::mine_sp)     , 0 , 0                  , 0 , 0 , ) \
        TILE( checkpoint        ,  0 ,255, 0 ,255, invis(Re::checkpoint)  , 0 , 0                  , 0 , 0 , ) \
        TILE( door              , 255, 0 ,255,255, norm(3)                , 0 , 0                  , 1 , 0 , ) \
        TILE( key               , 255,127,255,255, invis(Re::key)         , 0 , 0                  , 0 , 0 , ) \
        TILE( door_boss         , 127, 0 ,127,255, norm(5)                , 0 , 0                  , 1 , 0 , ) \
        TILE( one_way_passage   ,  0 , 0 ,127,255, invis(Re::one_way)     , 0 , 0                  , 0 , 0 , ) \
        TILE( one_way_passage_r ,  0 , 0 , 63,255, invis(Re::one_way_r)   , 0 , 0                  , 0 , 0 , ) \
        TILE( one_way_passage_d ,  0 , 0 ,191,255, invis(Re::one_way_d)   , 0 , 0                  , 0 , 0 , ) \
        TILE( boss              , 255,127,127,255, invis(Re::boss)        , 0 , 0                  , 0 , 0 , ) \

    enum Enum
    {
        #define TILE(name, ...) name,
        TILE_LIST
        #undef TILE
        _tile_count
    };

    std::vector<u8vec4> colors
    {
        #define TILE(name, r,g,b,a, ...) u8vec4(r,g,b,a),
        TILE_LIST
        #undef TILE
    };

    struct Info
    {
        enum Mode
        {
            inv, nor, rot, dec
        };

        Mode mode = inv;
        int index = 0;
        ivec2 decal_pos = ivec2(0,0), decal_sz = ivec2(0,0);
        static Info invis(Re re = Re::_rec_count)
        {
            Info ret;
            ret.mode = inv;
            ret.index = (re != Re::_rec_count ? int(re) : -1);
            return ret;
        }
        static Info norm(int index)
        {
            Info ret;
            ret.mode = nor;
            ret.index = index;
            return ret;
        }
        static Info rotate(int index)
        {
            Info ret;
            ret.mode = rot;
            ret.index = index;
            return ret;
        }
        static Info decal(int x, int y, int sx, int sy)
        {
            Info ret;
            ret.mode = dec;
            ret.index = -1;
            ret.decal_pos = ivec2(x, y);
            ret.decal_sz  = ivec2(sx, sy);
            return ret;
        }
    };

    std::vector<Info> info
    {
        #define TILE(name, r,g,b,a, info, ...) Info::info,
        TILE_LIST
        #undef TILE
    };
    std::vector<int> groups
    {
        #define TILE(name, r,g,b,a, info, group, ...) group,
        TILE_LIST
        #undef TILE
    };

    std::vector<fvec3> lights
    {
        #define TILE(name, r,g,b,a, info, group, light, ...) fvec3(light),
        TILE_LIST
        #undef TILE
    };

    std::vector<bool> solid
    {
        #define TILE(name, r,g,b,a, info, group, light, solid, ...) solid,
        TILE_LIST
        #undef TILE
    };

    std::vector<bool> breaks
    {
        #define TILE(name, r,g,b,a, info, group, light, solid, breaks, ...) breaks,
        TILE_LIST
        #undef TILE
    };

    Enum find_enum(u8vec4 color)
    {
        auto it = std::find(colors.begin(), colors.end(), color);
        if (it == colors.end())
            return _tile_count;
        return Enum(it - colors.begin());
    }
}


class Map
{
    std::vector<Tiles::Enum> layers[3];
    ivec2 size = ivec2(0);
    std::vector<std::vector<ivec2>> rec{int(Tiles::Re::_rec_count)};
  public:
    inline static constexpr int tsz = 24, z_offset = 92, light_z_offset = -30;
    inline static constexpr float light_rad = 2.5;

    std::vector<ivec2> Rec(Tiles::Re index)
    {
        if (rec[int(index)].empty())
            Program::Error(Str("Map record vector ", int(index), " is empty."));
        return rec[int(index)];
    }

    void Set(int z, ivec2 pos, Tiles::Enum e)
    {
        DebugAssert("Invalid map layer", z >= 0 && z < 3);
        if ((pos < 0).any() || (pos >= size).any())
            return;
        layers[z][pos.y * size.x + pos.x] = e;
    }
    Tiles::Enum Get(int z, ivec2 pos) const
    {
        DebugAssert("Invalid map layer", z >= 0 && z < 3);
        pos = clamp(pos, ivec2(0,0), size-1);
        return layers[z][pos.y * size.x + pos.x];
    }
    const Tiles::Info &Info(int z, ivec2 pos) const
    {
        auto e = Get(z, pos);
        return Tiles::info[e];
    }
    int Group(int z, ivec2 pos) const
    {
        auto e = Get(z, pos);
        return Tiles::groups[e];
    }
    fvec3 Lights(int z, ivec2 pos) const
    {
        auto e = Get(z, pos);
        return Tiles::lights[e];
    }
    bool Solid(int z, ivec2 pos) const
    {
        auto e = Get(z, pos);
        return Tiles::solid[e];
    }
    bool PosSolid(fvec2 pos) const
    {
        ivec2 p = TileFromPos(pos);
        return Solid(0, p) || Solid(1, p) || Solid(2, p);
    }

    static ivec2 TileFromPos(fvec2 pos)
    {
        ivec2 p = iround(pos);
        p = div_ex(p, tsz);
        return p;
    }

    ivec2 Size() const
    {
        return size;
    }

    Map() {}

    Map(std::string fname)
    {
        Graphics::Image img0(fname + "_3.gnp");
        Graphics::Image img1(fname + "_2.gnp");
        Graphics::Image img2(fname + "_1.gnp");

        if (img0.Size() != img1.Size() || img1.Size() != img2.Size())
            Program::Error(Str("Map size mismatch for \"", fname, "\"."));

        size = img0.Size();

        struct Pair
        {
            Graphics::Image &img;
            int z;
        };

        for (auto &it : layers)
            it.resize(size.product());

        Pair pairs[] {{img0, 0}, {img1, 1}, {img2, 2}};

        for (auto it : pairs)
        {
            for (int y = 0; y < size.y; y++)
            for (int x = 0; x < size.x; x++)
            {
                u8vec4 color = it.img.FastGet({x,y});
                auto e = Tiles::find_enum(color);
                if (e == Tiles::_tile_count)
                    Program::Error(Str("Invalid tile at \"", fname, "\":", it.z, ivec2(x,y), ": ", ivec4(color), "."));
                Set(it.z, {x, y}, e);

                const auto &info = Tiles::info[e];
                if (info.mode == Tiles::Info::inv && info.index != -1)
                    rec[info.index].push_back(ivec2(x,y) * tsz + tsz / 2);
            }
        }
    }

    void Render(int z) const;
};

struct Light
{
    fvec3 pos, color;
    float rad;
    bool tube;
    fvec3 end;
};
struct Particle
{
    fvec2 pos = fvec2(0), vel = fvec2(0);
    float vel_fac = 1;
    int index = 0;
    int t = 0, maxt = 1;
};
struct Laser
{
    float max_len, len = 0;
    int life = 60*10;
    fvec2 pos;
    float angle, speed;
    int index;
    fvec3 color;
    bool dead = 0;
    bool enemy = 0;

    fvec2 dir() const
    {
        return fvec2(std::cos(angle), std::sin(angle));
    }
    bool hits(fvec2 p, float rad) const
    {
        // The collision is tested agains laser-aligned square bounding box.
        if (len <= 0 || dead == 1)
            return 0;
        fvec2 a = pos;
        fvec2 b = pos - dir() * len;
        fvec2 d = (b - a).norm();
        a -= d * rad;
        b += d * rad;
        if ((b - a) /dot/ (p - a) < 0 ||
            (a - b) /dot/ (p - b) < 0)
            return 0;
        return abs((p - a) /cross/ d) < rad;
    }
};
struct TrashBlock
{
    fvec2 pos;
    float angle = 0, speed = 0, vis_angle = 0, vis_av = 0;
    bool is_key = 0;
};
struct HomingMine
{
    fvec2 pos, vel = fvec2(0);
    float angle = f_pi/2;
};
struct HomingMineSpawn
{
    fvec2 pos;
    int spawn_cd = 0;
};
struct LaserTurret
{
    fvec2 pos, vel = fvec2(0);
    int laser_cd = 0;
};

struct AttachedTrashBlock
{
    fvec2 offset;
    float vis_angle;
    bool is_key = 0;
};

struct OneWay
{
    fvec2 pos;
    int dir; // 0 - up, 1 - right, 2 - down
};

int death_counter = 0;
uint64_t starting_time = 0, ending_time = 0;
float starting_screen_alpha = 2;

struct World
{
    struct Player
    {
        fvec2 pos = fvec2(0);
        float rot = 0;
        fvec2 vel = fvec2(0);
        float av = 0;
        int vc = 0, hc = 0;
        float ac = 0;
        int dead = 0;
        int laser_cd = 0;
        bool las_right = 0;
        std::vector<AttachedTrashBlock> trash;
    };
    Player p;
    fvec2 cam_pos = fvec2(0), cam_pos_p = fvec2(0);
    float cam_rot = 0;
    int active_checkpoint = -1;
    bool just_hit_checkpoint = 0;
    Map map;

    inline static constexpr float extra_exp_max = 5;
    float extra_exp = extra_exp_max;

    bool goal_exists = 0;
    int fin = 0;

    std::deque<Light> light_queue;
    std::deque<Particle> par_queue;
    std::deque<Laser> laser_list;
    std::deque<TrashBlock> trash_block_list;
    std::deque<HomingMine> homing_mine_list;
    std::deque<HomingMineSpawn> homing_mine_sp_list;
    std::deque<LaserTurret> laser_turret_list;
    std::vector<fvec2> checkpoint_list;
    std::vector<OneWay> one_way_list;

    struct Boss
    {
        enum State
        {
            idle,
            waking_up,
            waiting,
            aiming_t,
            shooting_trash,
            aiming_l,
            shooting_lasers,
            aiming_d,
            dash,
            aiming_d2,
            dash2,
            aiming_d3,
            dash3,
            shooting_mines,
            charging,
        };
        State state = idle;

        int dead = 0;
        int timer = 0;
        fvec2 pos, home;
        float rot = f_pi/2;
        float wing_an = 0, wing_an_s = 0, wing_offset = 0;
        fvec2 dash_src, dash_dst;
        float core_offset = 0;
        bool was_hit = 0;
        int hp = 3;

        bool open_core = 0;
        bool open_wings = 0;
        int aim = 0;
    };
    Boss boss;

    void kill_player()
    {
        if (!p.dead && !fin)
        {
            p.dead = 1;
            death_counter++;
        }
    }

    bool at_screen_circle(fvec2 pos, float rad)
    {
        return (pos - cam_pos).len_sqr() <= ipow(screen_sz.x/2+rad,2) + ipow(screen_sz.y/2+rad,2);
    }

    void finalize_map()
    {
        laser_list.clear();

        trash_block_list.clear();
        for (auto it : map.Rec(Tiles::Re::trash))
        {
            TrashBlock tb;
            tb.pos = it;
            tb.vis_av = Rand::Float(-0.01, 0.01);
            trash_block_list.push_back(tb);
        }
        for (auto it : map.Rec(Tiles::Re::key))
        {
            TrashBlock tb;
            tb.pos = it;
            tb.vis_av = Rand::Float(-0.01, 0.01);
            tb.is_key = 1;
            trash_block_list.push_back(tb);
        }

        homing_mine_list.clear();
        for (auto it : map.Rec(Tiles::Re::homing_mine))
        {
            HomingMine hm;
            hm.pos = it;
            homing_mine_list.push_back(hm);
        }

        homing_mine_sp_list.clear();
        for (auto it : map.Rec(Tiles::Re::mine_sp))
        {
            HomingMineSpawn hms;
            hms.pos = it;
            homing_mine_sp_list.push_back(hms);
        }


        laser_turret_list.clear();
        for (auto it : map.Rec(Tiles::Re::las_turret))
        {
            LaserTurret lt;
            lt.pos = it;
            laser_turret_list.push_back(lt);
        }

        checkpoint_list.clear();
        for (auto it : map.Rec(Tiles::Re::checkpoint))
            checkpoint_list.push_back(it);

        one_way_list.clear();
        for (auto it : map.Rec(Tiles::Re::one_way))
            one_way_list.push_back({it, 0});
        for (auto it : map.Rec(Tiles::Re::one_way_r))
            one_way_list.push_back({it, 1});
        for (auto it : map.Rec(Tiles::Re::one_way_d))
            one_way_list.push_back({it, 2});

        boss.pos = boss.home = map.Rec(Tiles::Re::boss)[0];
    }
    void push_light(fvec3 color, fvec3 pos, float rad = 0)
    {
        Light l;
        l.pos = pos;
        l.color = color;
        l.rad = rad;
        l.tube = 0;
        l.end = pos;
        light_queue.push_back(l);
    }
    void push_light_line(fvec3 color, fvec3 a, fvec3 b, float rad = 0)
    {
        Light l;
        l.pos = a;
        l.color = color;
        l.rad = rad;
        l.tube = 1;
        l.end = b;
        light_queue.push_back(l);
    }
    void push_particle(int index, float fac, int t, int maxt, fvec2 pos, fvec2 vel)
    {
        Particle p;
        p.index = index;
        p.vel_fac = fac;
        p.t = t;
        p.maxt = maxt;
        p.pos = pos;
        p.vel = vel;
        par_queue.push_back(p);
    }
    void add_laser(fvec2 pos, float angle, float speed, float max_len, fvec3 color, int index, bool enemy = 0)
    {
        Laser l;
        l.max_len = max_len;
        l.pos = pos;
        l.angle = angle;
        l.speed = speed;
        l.color = color;
        l.index = index;
        l.enemy = enemy;
        laser_list.push_back(l);
    }
};
World w, w_saved;

void Map::Render(int z) const
{
    ivec2 min = w.cam_pos_p / tsz - (screen_sz.x / 2 / tsz + 2);
    ivec2 max = w.cam_pos_p / tsz + (screen_sz.x / 2 / tsz + 2);

    for (int y = min.y; y <= max.y; y++)
    for (int x = min.x; x <= max.x; x++)
    {
        auto info = Info(z, {x,y});
        fvec3 l_color = Lights(z, {x,y});
        if ((l_color > 0.1).any())
            w.push_light(l_color, ((fvec2(x,y) + 0.5) * tsz).to_vec3(z_offset * z + light_z_offset), light_rad);

        float dep = z * z_offset - 32;
        switch (info.mode)
        {
          case Tiles::Info::inv:
            break;
          case Tiles::Info::nor:
            r.Quad(ivec2(x,y) * tsz, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, 0));
            break;
          case Tiles::Info::dec:
            r.Quad((ivec2(x,y)+0.5) * tsz, dep, info.decal_sz).tex(info.decal_pos).center();
            break;
          case Tiles::Info::rot:
            {
                int this_g = Group(z, {x,y});
                int g = 0;
                for (auto it : circle)
                    g = g << 1 | (this_g == Group(z, ivec2(x,y) + it));
                auto beq = [=](int b) -> bool {return (g & b) == b;};
                if (g == 0b1111'1111)
                    r.Quad(ivec2(x,y) * tsz, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, tsz));
                else if ((beq(0b1110'0011) && !(g & 0b0000'1000)) ||
                         (beq(0b1111'1000) && !(g & 0b0000'0010)) ||
                         (beq(0b0011'1110) && !(g & 0b1000'0000)) ||
                         (beq(0b1000'1111) && !(g & 0b0010'0000)))
                {
                    float angle;
                         if (beq(0b1110'0011)) angle = f_pi * 3 / 2;
                    else if (beq(0b1111'1000)) angle = 0;
                    else if (beq(0b0011'1110)) angle = f_pi / 2;
                    else                       angle = f_pi;
                    r.Quad(ivec2(x,y) * tsz + tsz/2, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, tsz*2)).center().rotate(angle);
                }
                else if (beq(0b1010'1010) ||
                         beq(0b0010'1010) ||
                         beq(0b1000'1010) ||
                         beq(0b1010'0010) ||
                         beq(0b1010'1000))
                {
                    r.Quad(ivec2(x,y) * tsz, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, 0));
                }
                else if (beq(0b1000'1000) ||
                         beq(0b0010'0010))
                {
                    float angle;
                    if (beq(0b1000'1000)) angle = 0;
                    else                   angle = f_pi/2;
                    r.Quad(ivec2(x,y) * tsz + tsz/2, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, tsz*4)).center().rotate(angle);
                }
                else if (beq(0b1110'0000) ||
                         beq(0b0011'1000) ||
                         beq(0b0000'1110) ||
                         beq(0b1000'0011))
                {
                    float angle;
                         if (beq(0b1110'0000)) angle = f_pi * 3 / 2;
                    else if (beq(0b0011'1000)) angle = 0;
                    else if (beq(0b0000'1110)) angle = f_pi / 2;
                    else                        angle = f_pi;
                    r.Quad(ivec2(x,y) * tsz + tsz/2, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, tsz*3)).center().rotate(angle);
                }
                else
                    r.Quad(ivec2(x,y) * tsz, dep, ivec2(tsz)).tex(ivec2(info.index * tsz, 0));

            }
            break;
        }
    }
}

namespace Draw
{
    void Background()
    {
        constexpr int offset = 256, cap = 3, count = 4;
        constexpr float z_step = 1;
        fvec2 center = iround(w.cam_pos_p / float(offset)) * offset;

        for (int z = 1; z <= count; z++)
        for (int y = -cap; y <= cap; y++)
        for (int x = -cap; x <= cap; x++)
            r.Quad((center + ivec2(x,y)*offset - w.cam_pos_p) / (1 + z * z_step) + w.cam_pos_p, 100, ivec2(9)).tex(ivec2(48,168)+1).center().beta(0).alpha(0.1 / (1+z)).scale_f(1. / z);
    }
}

int main(int, char **)
{
    constexpr float player_z = 92, cam_dist = 270/2-50, cam_snap_fac = 0.2, cam_snap_fac_a = 0.4, light_z = -20, light_dist = 25,
                    engine_light_z_off = -5, engine_light_y_off = 22, engine_light_y_off_front = 20, engine_light_x_off = 17,
                    player_hitbox_off = 12,
                    min_speed = 0.07, allow_respawn_after = 30,
                    exposure_step = -0.05, laser_pa_gap = 12, laser_pa_rand = 1, laser_pa_rspeed = 0.5,
                    laser_li_z = 50, death_light_2_fac = 1000,
                    laser_x_off = 11, laser_y_off = -7,
                    player_rad = 13, trash_rad = 6, trash_rad_las = 4, homing_mine_rad = 12,
                    homing_mine_ang_vel = 0.05, homing_mine_acc = 0.04, homing_mine_spd_cap = 6, homing_mine_vel_fac = 0.98,
                    mine_spawn_cd = 180,
                    laser_turret_acc = 0.2, laser_turret_rad = 18, laser_turret_speed_cap = 1.5, laser_turret_hitbox_x = 27, laser_turret_hitbox_y = 12,
                    laser_turret_shoot_at = 20, laser_turret_cd = 60, laser_turret_las_y = 14,
                    trash_mass = 0.12, trash_inertia_moment_fac = 1./1000,
                    boss_hitbox_rad = 35, boss_z = 120, boss_wing_a_sp = 0.015, boss_av = 0.04, boss_trash_av = 0.05, boss_trash_a_range = f_pi/8,
                    boss_trash_offset = 8, boss_trash_min = 40, boss_trash_max = 150, boss_trash_sp_min = 1, boss_trash_sp_max = 3,
                    boss_las_offset = 50, boss_las_len = 120, boss_mine_count = 3, boss_mine_offset = 45, boss_mine_speed = 0.2,
                    boss_core_shell_sp = 0.5, boss_core_shell_max = 30,
                    boss_hp_offset = 10, boss_hp_gap = 8;
    constexpr int laser_cd = 20, death_light_len = 100, death_light_2_start = 120, death_light_2_t_cap = 120,
                  boss_trash_per_wing = 9;

    Events::SetErrorHandlers();
    Init();
    Graphics::SetClearColor(fvec3(0));
    r.SetBackground(fvec3(0));
    //r.SetBackground(fvec3(1,1,2) / 80);
    r.Resize(win.Size());

    mouse.Show(0);
    mouse.Relative(1);

    w.map = Map("assets/map");
    w.finalize_map();
    w.p.pos = w.cam_pos = w.map.Rec(Tiles::Re::spawn)[0];

    w_saved = w;

    fmat4 view, proj_view;

    auto Tick = [&]
    {
        auto HitsPlayer = [&](fvec2 pos, float rad) -> bool
        {
            if (w.p.dead)
                return 0;
            if ((pos - w.p.pos).len_sqr() <= ipow(player_rad + rad, 2))
                return 1;
            fmat3 m = fmat3::translate2D(w.p.pos) /mul/ fmat3::rotate2D(w.p.rot);
            for (const auto &o : w.p.trash)
            {
                if ((pos - (m /mul/ o.offset.to_vec3(1)).to_vec2()).len_sqr() <= ipow(trash_rad + rad, 2))
                    return 1;
            }
            return 0;
        };
        auto FreeLineToPlayer = [&](fvec2 pos) -> bool
        {
            constexpr float pixel_step = 4;
            int count = (w.p.pos - pos).len() / pixel_step;
            fvec2 step = (w.p.pos - pos).norm() * pixel_step;
            while (count-- > 0)
            {
                if (w.map.PosSolid(pos))
                    return 0;
                pos += step;
            }
            return 1;
        };

        { // Music
            if (Keys::m.pressed())
                Sounds::theme.volume(Sounds::theme_plays = !Sounds::theme_plays);
        }

        { // Begin
            w.cam_pos_p = w.cam_pos;
            if (w.p.dead)
                w.p.dead++;

            if (starting_time == 0 && (Keys::any.pressed() || mouse.any_button.pressed()))
                starting_time = ts.ticks;
        }

        { // Particles
            auto it = w.par_queue.begin();
            while (it != w.par_queue.end())
            {
                it->pos += it->vel;
                it->vel *= it->vel_fac;
                it->t++;
                if (it->t >= it->maxt)
                    it = w.par_queue.erase(it);
                else
                    it++;
            }
        }

        { // Lasers
            auto it = w.laser_list.begin();
            while (it != w.laser_list.end())
            {
                fvec2 d = it->dir();

                if (!it->dead)
                {
                    it->len = clamp(it->len + it->speed, 0, it->max_len);
                    it->pos += d * it->speed;
                }
                else
                    it->len -= it->speed;

                fvec2 dy(-d.y, d.x);

                fvec2 p = it->pos;
                for (float x = 0; x < it->len; x += laser_pa_gap) // Laser particles
                {
                    w.push_particle(it->index, 0.99, Rand::Int(15, 25), 30, p + dy * Rand::Float(-1,1) * laser_pa_rand + d * Rand::Float(-.5,.5) * laser_pa_gap, dy * Rand::Float(-1,1)*laser_pa_rspeed);
                    p -= d * laser_pa_gap;
                }

                if (w.map.PosSolid(it->pos) && !it->dead) // Hitting block
                {
                    it->dead = 1;
                    ivec2 pos = w.map.TileFromPos(it->pos);
                    auto e = w.map.Get(1, pos);
                    if (!it->enemy && Tiles::breaks[e])
                    {
                        Sounds::laser_hitting(it->pos);
                    }
                }

                if (!it->dead && !w.boss.dead && it->hits(w.boss.pos, boss_hitbox_rad)) // Hitting boss
                {
                    it->dead = 1;
                    w.boss.was_hit = 1;
                }

                if (it->enemy && it->hits(w.p.pos, player_rad)) // Hitting player
                {
                    it->dead = 1;
                    w.kill_player();
                }

                it->life--;

                if (it->life <= 0)
                    it = w.laser_list.erase(it);
                else if (it->dead && it->len < 0)
                {
                    ivec2 pos = w.map.TileFromPos(it->pos);
                    auto e = w.map.Get(1, pos);
                    if (!it->enemy && Tiles::breaks[e])
                    {
                        w.map.Set(1, pos, Tiles::air);
                        Sounds::block_breaks(it->pos);
                        for (int i = 0; i < 50; i++) // Breaking particles
                        {
                            float a = Rand::Float(-f_pi, f_pi);
                            w.push_particle(3, 0.97, Rand::Int(-5, 5), 30, pos * Map::tsz + fvec2(Rand::Float(0,Map::tsz), Rand::Float(0,Map::tsz)), fvec2(std::cos(a), std::sin(a)) * Rand::Float(0,0.5));
                        }
                    }
                    it = w.laser_list.erase(it);
                }
                else
                    it++;
            }
        }

        { // Trash blocks
            fmat3 m = fmat3::translate2D(w.p.pos) /mul/ fmat3::rotate2D(w.p.rot);
            fmat3 mi = m.inverse();

            auto it = w.trash_block_list.begin();
            while (it != w.trash_block_list.end())
            {
                if (!w.at_screen_circle(it->pos, 180))
                {
                    it++;
                    continue;
                }
                it->pos += fvec2(std::cos(it->angle), std::sin(it->angle)) * it->speed;
                it->vis_angle += it->vis_av;
                bool destroy = w.map.PosSolid(it->pos), des_quiet = 0;
                if (!destroy && !it->is_key) // Hitting a laser
                {
                    for (const auto &l : w.laser_list)
                    {
                        if (l.enemy)
                            continue;
                        if (l.hits(it->pos, trash_rad_las))
                        {
                            destroy = 1;
                            break;
                        }
                    }
                }
                if (!destroy && !w.fin) // Hitting a player
                {
                    if ((it->pos - w.p.pos).len_sqr() <= ipow(player_rad + trash_rad, 2))
                    {
                        destroy = 1;
                        des_quiet = 1;
                        w.p.trash.push_back({{(mi /mul/ it->pos.to_vec3(1)).to_vec2()}, it->vis_angle - w.p.rot, it->is_key});
                    }
                }
                if (!destroy && !w.fin) // Hitting a box attached to player
                {
                    for (const auto &o : w.p.trash)
                    {
                        if ((it->pos - (m /mul/ o.offset.to_vec3(1)).to_vec2()).len_sqr() <= ipow(trash_rad + trash_rad, 2))
                        {
                            destroy = 1;
                            des_quiet = 1;
                            w.p.trash.push_back({{(mi /mul/ it->pos.to_vec3(1)).to_vec2()}, it->vis_angle - w.p.rot, it->is_key});
                            break;
                        }
                    }
                }

                if (destroy)
                {
                    if (!des_quiet)
                    {
                        Sounds::trash_block_breaks(it->pos);
                        for (int i = 0; i < 50; i++) // Breaking particles
                        {
                            float a = Rand::Float(-f_pi, f_pi);
                            w.push_particle(3, 0.97, Rand::Int(-5, 5), 30, it->pos + fvec2(Rand::Float(-6,6),Rand::Float(-6,6)), fvec2(std::cos(a), std::sin(a)) * Rand::Float(0,0.5));
                        }
                    }
                    else
                        Sounds::block_attaches(it->pos);
                    it = w.trash_block_list.erase(it);
                }
                else
                    it++;
            }
        }

        { // Homing mines
            auto it = w.homing_mine_list.begin();
            while (it != w.homing_mine_list.end())
            {
                it->pos += it->vel;

                fvec2 dst_dir = (w.p.pos - it->pos).norm();
                fvec2 dir = fvec2(std::cos(it->angle), std::sin(it->angle));

                float a_delta = std::asin(dst_dir /cross/ dir);
                if (abs(a_delta) > homing_mine_ang_vel)
                    it->angle -= sign(a_delta) * homing_mine_ang_vel;

                if (it->vel.any() || (w.at_screen_circle(it->pos, 10) && FreeLineToPlayer(it->pos)))
                {
                    it->vel += fvec2(std::cos(it->angle), std::sin(it->angle)) * homing_mine_acc;
                    fvec2 vel_n = fvec2(-dst_dir.y, dst_dir.x);
                    vel_n = vel_n * (vel_n /dot/ it->vel) * (1 - homing_mine_vel_fac);
                    it->vel -= vel_n;
                    if (it->vel.len() > homing_mine_spd_cap)
                        it->vel = it->vel.norm() * homing_mine_spd_cap;

                    for (int i = 0; i < 2; i++) // Particles
                        w.push_particle(4, 0.99, Rand::Int(0, 5), 30, it->pos - dir * 21 + fvec2(-dir.y,dir.x) * Rand::Float(-12,12), -dir * 0.7);
                }
                bool destroy = w.map.PosSolid(it->pos + fvec2(std::cos(it->angle), std::sin(it->angle)) * 10);
                if (!destroy) // Hitting a laser
                {
                    for (auto &l : w.laser_list)
                    {
                        if (l.enemy)
                            continue;
                        if (l.hits(it->pos, homing_mine_rad))
                        {
                            destroy = 1;
                            l.dead = 1;
                            break;
                        }
                    }
                }
                if (!destroy) // Hitting a player
                {
                    if (HitsPlayer(it->pos, homing_mine_rad))
                    {
                        destroy = 1;
                        w.kill_player();
                    }
                }

                if (destroy)
                {
                    Sounds::mine_explodes(it->pos);
                    for (int i = 0; i < 85; i++) // Breaking particles
                    {
                        float a0 = Rand::Float(-f_pi, f_pi);
                        float a = Rand::Float(-f_pi, f_pi);
                        w.push_particle(3, 0.97, Rand::Int(-5, 5), 30, it->pos + fvec2(std::cos(a0), std::sin(a0))*Rand::Float(0,18), fvec2(std::cos(a), std::sin(a)) * Rand::Float(0,0.6));
                    }
                    it = w.homing_mine_list.erase(it);
                }
                else
                    it++;
            }
        }

        { // Homing mine spawn points
            for (auto &it : w.homing_mine_sp_list)
            {
                if (!w.at_screen_circle(it.pos, 30))
                    continue;
                if (it.spawn_cd > 0)
                    it.spawn_cd--;
                else if (FreeLineToPlayer(it.pos))
                {
                    it.spawn_cd = mine_spawn_cd;
                    Sounds::mine_spawns(it.pos);
                    HomingMine new_mine;
                    new_mine.pos = it.pos;
                    fvec2 delta = w.p.pos - it.pos;
                    if (delta.any())
                        new_mine.angle = std::atan2(delta.y, delta.x);
                    w.homing_mine_list.push_back(new_mine);
                    for (int i = 0; i < 50; i++)
                    {
                        float a = Rand::Float(-f_pi, f_pi);
                        float d = Rand::Float(0,18);
                        fvec2 rv = fvec2(std::cos(a), std::sin(a));
                        w.push_particle(4, 0.98, Rand::Int(5, 15), 50, it.pos + rv * d, rv * 1.8 + fvec2(-rv.y,rv.x)*Rand::Float(-0.3,0.3));
                    }
                }
                float a = Rand::Float(-f_pi, f_pi);
                float d = Rand::Float(0,18);
                fvec2 rv = fvec2(std::cos(a), std::sin(a));
                for (int i = 0; i < 6; i++)
                    w.push_particle(4, 0.98, Rand::Int(15, 25), 50, it.pos + rv * d, (fvec2(rv.y, -rv.x) + rv/3).norm());
            }
        }

        { // Laser turrets
            auto it = w.laser_turret_list.begin();
            while (it != w.laser_turret_list.end())
            {
                if (!w.at_screen_circle(it->pos, 10))
                {
                    it++;
                    continue;
                }

                bool can_move = 1;
                if (w.map.PosSolid(it->pos + it->vel*2 + ivec2(laser_turret_hitbox_x,0)) ||
                    w.map.PosSolid(it->pos + it->vel*2 - ivec2(laser_turret_hitbox_x,0)))
                    can_move = 0;
                if (can_move)
                {
                    for (const auto &o : w.laser_turret_list)
                    {
                        if (&o != &*it && (abs(it->pos + it->vel*2 - o.pos) < 2*fvec2(laser_turret_hitbox_x, laser_turret_hitbox_y)).all())
                        {
                            can_move = 0;
                            break;
                        }
                    }
                }

                if (!can_move)
                    it->vel.x *= -0.3;

                if (!w.map.PosSolid(it->pos + it->vel))
                    it->pos += it->vel;

                if (it->laser_cd > 0)
                    it->laser_cd--;

                if (it->laser_cd == 0 && !w.p.dead && FreeLineToPlayer(it->pos) && abs(it->pos.x - w.p.pos.x) < laser_turret_shoot_at) // Shooting lasers
                {
                    bool up = it->pos.y < w.p.pos.y;
                    w.add_laser(it->pos + (up ? 1 : -1) * fvec2(0,laser_turret_las_y), (up ? 1 : -1) * f_pi/2, 3, 160, fvec3(1,0,0.5)*20000, 4, 1);
                    it->laser_cd = laser_turret_cd;
                    Sounds::laser(it->pos,1,-0.3);
                }

                int dir = sign(iround((w.p.pos - it->pos).x));
                it->vel += fvec2(dir * laser_turret_acc, 0);
                it->vel = clamp(it->vel, -laser_turret_speed_cap, laser_turret_speed_cap);

                bool destroy = 0;
                if (!destroy) // Hitting a laser
                {
                    for (auto &l : w.laser_list)
                    {
                        if (l.enemy)
                            continue;
                        if (l.hits(it->pos, laser_turret_rad))
                        {
                            destroy = 1;
                            l.dead = 1;
                            break;
                        }
                    }
                }
                if (!destroy) // Hitting a player
                {
                    if (HitsPlayer(it->pos, laser_turret_rad))
                    {
                        destroy = 1;
                        w.kill_player();
                    }
                }

                if (destroy)
                {
                    Sounds::mine_explodes(it->pos);
                    for (int i = 0; i < 85; i++) // Breaking particles
                    {
                        float a0 = Rand::Float(-f_pi, f_pi);
                        float a = Rand::Float(-f_pi, f_pi);
                        w.push_particle(3, 0.97, Rand::Int(-5, 5), 30, it->pos + fvec2(std::cos(a0), std::sin(a0))*Rand::Float(0,22), fvec2(std::cos(a), std::sin(a)) * Rand::Float(0,0.6));
                    }
                    it = w.laser_turret_list.erase(it);
                }
                else
                    it++;
            }
        }

        { // Checkpoints
            bool any_hit = 0;
            int hit_pos = 0;

            int pos = -1;
            for (const auto &it : w.checkpoint_list)
            {
                pos++;
                if (!w.at_screen_circle(it, 30))
                    continue;
                float a = Rand::Float(-f_pi, f_pi);
                float d = Rand::Float(22,26);
                fvec2 rv = fvec2(std::cos(a), std::sin(a));
                for (int i = 0; i < 4; i++)
                    w.push_particle(5, 0.98, Rand::Int(15, 25), 50, it + rv * d, pos == w.active_checkpoint ? -rv : fvec2(-rv.y, rv.x));

                if (!any_hit && HitsPlayer(it, 5))
                {
                    any_hit = 1;
                    hit_pos = pos;
                }
            }

            if (any_hit)
            {
                if (!w.just_hit_checkpoint)
                {
                    w.just_hit_checkpoint = 1;
                    w.active_checkpoint = hit_pos;
                    Sounds::checkpoint({0,0}).relative();
                    w_saved = w;
                }
            }
            else
                w.just_hit_checkpoint = 0;
        }

        { // One way passages
            for (const auto &it : w.one_way_list)
            {
                if (!w.at_screen_circle(it.pos, 30))
                    continue;
                fvec2 rv = fvec2(Rand::Float(-Map::tsz/2,Map::tsz/2),
                                 Rand::Float(-Map::tsz/2,Map::tsz/2));
                static const std::vector<fvec2> dirs{{0,-1},{1,0},{0,1}};
                for (int i = 0; i < 4; i++)
                    w.push_particle(6, 0.98, Rand::Int(15, 25), 30, it.pos + rv, dirs[it.dir] * !w.fin);
            }
        }

        { // Boss
            if (!w.boss.dead)
            {
                fmat2 bm = fmat2::rotate2D(w.boss.rot);

                // Wings
                if (!w.p.dead)
                {
                    w.boss.wing_an += (w.boss.open_wings ? boss_wing_a_sp : -boss_wing_a_sp);
                    w.boss.wing_an = clamp(w.boss.wing_an, 0, f_pi / 2);
                    w.boss.wing_an_s = smoothstep(w.boss.wing_an / f_pi * 2) * f_pi / 2;
                    w.boss.wing_offset = std::sin(w.boss.wing_an_s * 2) * 20;
                }

                // Aim
                if (w.boss.aim && !w.p.dead)
                {
                    fvec2 dst_dir = (w.p.pos - w.boss.pos).norm();
                    fvec2 dir = fvec2(std::cos(w.boss.rot), std::sin(w.boss.rot));

                    if (w.boss.aim < 0)
                        dst_dir = -dst_dir;

                    float a_delta = std::asin(dst_dir /cross/ dir);
                    if (abs(a_delta) > boss_av)
                        w.boss.rot -= sign(a_delta) * boss_av;
                }

                // Core shell
                if (!w.p.dead)
                {
                    w.boss.core_offset += (w.boss.open_core ? boss_core_shell_sp : -boss_core_shell_sp);
                    w.boss.core_offset = clamp(w.boss.core_offset, 0, boss_core_shell_max);
                }

                // Kill player
                if ((w.boss.pos - w.p.pos).len() < boss_hitbox_rad + player_rad)
                    w.kill_player();

                auto Next = [&](World::Boss::State st, int time)
                {
                    if (w.boss.timer < time)
                        return;
                    w.boss.state = st;
                    w.boss.timer = 0;
                };

                switch (w.boss.state)
                {
                  case w.boss.idle:
                    w.boss.aim = 0;
                    w.boss.open_wings = 0;
                    if (w.boss.was_hit)
                    {
                        Sounds::boss_start(w.boss.pos);
                        Next(w.boss.waking_up, 0);
                    }
                    else
                        Next(w.boss.idle, 0);
                    break;
                  case w.boss.waking_up:
                    w.boss.aim = 0;
                    w.boss.open_wings = 0;
                    Next(w.boss.waiting, 90);
                    break;
                  case w.boss.waiting:
                    w.boss.aim = 0;
                    w.boss.open_wings = 0;
                    Next(w.boss.aiming_t, 60);
                    break;
                  case w.boss.aiming_t:
                    w.boss.aim = -1;
                    if (w.boss.timer == 1)
                        Sounds::boss_wings_move(w.boss.pos);
                    w.boss.open_wings = 1;
                    Next(w.boss.shooting_trash, 90);
                    break;
                  case w.boss.shooting_trash:
                    w.boss.aim = 0;
                    w.boss.open_wings = 1;
                    if (w.boss.timer == 1 || w.boss.timer == 30)
                    {
                        Sounds::boss_trash(w.boss.pos);
                        for (int x = 0; x < boss_trash_per_wing; x++)
                        {
                            for (int s = -1; s <= 1; s += 2)
                            {
                                TrashBlock tb;
                                tb.pos = w.boss.pos + bm /mul/ fvec2(boss_trash_offset, s * Rand::Float(boss_trash_min, boss_trash_max));
                                tb.angle = w.boss.rot + f_pi + Rand::Float(-boss_trash_a_range, boss_trash_a_range);
                                tb.speed = Rand::Float(boss_trash_sp_min, boss_trash_sp_max);
                                tb.vis_angle = Rand::Float(-f_pi, f_pi);
                                tb.vis_av = Rand::Float(-boss_trash_av, boss_trash_av);
                                w.trash_block_list.push_back(tb);
                            }
                        }
                    }
                    Next(w.boss.aiming_l, 45);
                    break;
                  case w.boss.aiming_l:
                    w.boss.aim = -1;
                    w.boss.open_wings = 1;
                    Next(w.boss.shooting_lasers, 60);
                    break;
                  case w.boss.shooting_lasers:
                    w.boss.aim = 0;
                    w.boss.open_wings = 0;
                    if (w.boss.timer == 1)
                        Sounds::boss_wings_move(w.boss.pos);
                    if (w.boss.timer % 13 == 6)
                    {
                        for (int s = -1; s <= 1; s += 2)
                        {
                            fvec2 pos = w.boss.pos + bm /mul/ fmat2::rotate2D(s * w.boss.wing_an_s) /mul/ fvec2(-boss_las_offset - w.boss.wing_offset,0);
                            w.add_laser(pos, w.boss.rot + f_pi + s * w.boss.wing_an_s, 3, boss_las_len, fvec3(1,0,0.5)*20000, 4, 1);
                            Sounds::laser(pos,0.5,-0.3);
                        }
                    }
                    if (w.boss.timer == 90)
                    {
                        fvec2 pos = w.boss.pos + bm /mul/ fvec2(-boss_las_offset - w.boss.wing_offset,0);
                        w.add_laser(pos, w.boss.rot + f_pi, 3, boss_las_len, fvec3(1,0,0.5)*20000, 4, 1);
                    }
                    Next(w.boss.aiming_d, 90);
                    break;
                  case w.boss.aiming_d:
                  case w.boss.aiming_d2:
                  case w.boss.aiming_d3:
                    w.boss.aim = (w.boss.state == w.boss.aiming_d);
                    w.boss.open_wings = 0;
                    {
                        int end_t = (w.boss.state == w.boss.aiming_d ? 90 : 45);
                        if (w.boss.timer == end_t)
                        {
                            w.boss.dash_src = w.boss.pos;
                            if (w.boss.state == w.boss.aiming_d3)
                                w.boss.dash_dst = w.boss.home;
                            else
                                w.boss.dash_dst = w.p.pos;
                        }
                             if (w.boss.state == w.boss.aiming_d ) Next(w.boss.dash , end_t);
                        else if (w.boss.state == w.boss.aiming_d2) Next(w.boss.dash2, end_t);
                        else if (w.boss.state == w.boss.aiming_d3) Next(w.boss.dash3, end_t);
                    }
                    break;
                  case w.boss.dash:
                  case w.boss.dash2:
                  case w.boss.dash3:
                    w.boss.aim = 0;
                    w.boss.open_wings = 0;
                    if (w.boss.timer == 1)
                        Sounds::boss_dash(w.boss.pos);
                    {
                        float t = 0;
                        if (w.boss.timer == 180)
                            t = 0;
                        if (w.boss.timer < 60)
                            t = w.boss.timer / 60.;
                        else if (w.boss.timer < 120)
                            t = 1;
                        else
                            t = 1 - (w.boss.timer - 120) / 60.;
                        w.boss.pos = w.boss.dash_src + (w.boss.dash_dst - w.boss.dash_src) * smoothstep(t);
                    }
                         if (w.boss.state == w.boss.dash ) Next(w.boss.aiming_d2, 90);
                    else if (w.boss.state == w.boss.dash2) Next(w.boss.aiming_d3, 90);
                    else if (w.boss.state == w.boss.dash3) Next(w.boss.shooting_mines, 90);
                    break;
                  case w.boss.shooting_mines:
                    w.boss.aim = 0;
                    w.boss.open_wings = 0;
                    if (w.boss.timer == 45)
                    {
                        Sounds::boss_mines(w.boss.pos);
                        for (int i = 0; i < boss_mine_count; i++)
                        {
                            float a = f_pi / 2 + 2 * f_pi / boss_mine_count * i;
                            fvec2 d = fvec2(std::cos(a), std::sin(a));
                            HomingMine hm;
                            hm.pos = w.boss.pos + d * boss_mine_offset;
                            hm.vel = d * boss_mine_speed;
                            hm.angle = a;
                            w.homing_mine_list.push_back(hm);
                        }
                    }
                    else if (w.boss.timer < 45)
                    {
                        for (int i = 0; i < 3; i++)
                        {
                            float a = Rand::Float(-f_pi, f_pi);
                            float d = Rand::Float(0,12);
                            fvec2 rv = fvec2(std::cos(a), std::sin(a));
                            w.push_particle(4, 0.98, Rand::Int(5, 15), 50, w.boss.pos + rv * d, rv * 1.8 + fvec2(-rv.y,rv.x)*Rand::Float(-0.3,0.3));
                        }
                    }
                    Next(w.boss.charging, 240);
                    break;
                  case w.boss.charging:
                    if (w.boss.timer == 1)
                    {
                        Sounds::boss_charge(w.boss.pos);
                        w.boss.open_core = 1;
                    }
                    if (w.boss.was_hit && w.boss.open_core && w.boss.core_offset > boss_core_shell_max/2)
                    {
                        w.boss.hp--;
                        w.boss.timer = 230;
                        if (w.boss.hp > 0)
                        {
                            Sounds::boss_hit(w.boss.pos);
                            for (int i = 0; i < 80; i++)
                            {
                                float a = Rand::Float(-f_pi, f_pi);
                                float d = Rand::Float(20,25);
                                fvec2 rv = fvec2(std::cos(a), std::sin(a));
                                w.push_particle(7, 0.98, Rand::Int(5, 15), 50, w.boss.pos + rv * d, rv * Rand::Float(0.1,0.8) + fvec2(-rv.y,rv.x)*Rand::Float(-0.1,0.1));
                            }
                        }
                        else
                        {
                            w.boss.dead = 1;
                            w.goal_exists = 1;
                            Sounds::boss_death(w.boss.pos);
                            for (int i = 0; i < 100; i++)
                            {
                                float a = Rand::Float(-f_pi, f_pi);
                                float d = Rand::Float(0,34);
                                fvec2 rv = fvec2(std::cos(a), std::sin(a));
                                w.push_particle(4, 0.98, Rand::Int(-30, -20), 50, w.boss.pos + rv * d, rv * Rand::Float(0.1,4.4) + fvec2(-rv.y,rv.x)*Rand::Float(-0.1,0.1));
                                w.push_particle(0, 0.98, Rand::Int(-20, -10), 50, w.boss.pos + rv * d, rv * Rand::Float(0.1,5.4) + fvec2(-rv.y,rv.x)*Rand::Float(-0.3,0.3));
                            }
                        }
                    }
                    if (w.boss.timer == 230)
                        w.boss.open_core = 0;
                    for (int i = 0; i < iround(2 * w.boss.core_offset / float(boss_core_shell_max)); i++)
                    {
                        float a = Rand::Float(-f_pi, f_pi);
                        float d = Rand::Float(32,42);
                        fvec2 rv = fvec2(std::cos(a), std::sin(a));
                        w.push_particle(4, 0.98, Rand::Int(0, 5), 50, w.boss.pos + rv * d, -rv);
                    }
                    Next(w.boss.waiting, 240);
                    break;
                }

                w.boss.was_hit = 0;

                if (!w.p.dead)
                    w.boss.timer++;
            }
            else
                w.boss.dead++;
        }

        { // Camera
            fvec2 cam_dst = w.p.pos + fvec2(std::sin(w.p.rot), -std::cos(w.p.rot)) * cam_dist;
            if ((cam_dst - w.cam_pos).len_sqr() < 1)
                w.cam_pos = cam_dst;
            w.cam_pos += (cam_dst - w.cam_pos) * cam_snap_fac;
            w.cam_rot += (w.p.rot - w.cam_rot) * cam_snap_fac_a;
        }

        { // Player
            w.p.pos += w.p.vel;
            w.p.rot += w.p.av;

            fmat2 m = fmat2::rotate2D(w.p.rot);

            Audio::ListenerPos(w.p.pos.to_vec3(-250));
            Audio::ListenerRot(fvec3(0,0,1), (m /mul/ fvec2(0,-1)).to_vec3());

            float mass = 1 + w.p.trash.size() * trash_mass;
            float inertia_moment = 1;
            fvec2 mass_offset = fvec2(0);
            for (const auto &it : w.p.trash)
            {
                mass_offset = it.offset * trash_mass;
                inertia_moment += it.offset.len_sqr() * trash_mass * trash_inertia_moment_fac;
            }
            mass_offset /= mass;


            float damp_fac = 1;
            if (w.p.dead)
                damp_fac = 20;
            w.p.vel *= 1 - clamp((1 - 0.977) * damp_fac, 0, 1);
            w.p.av *= 1 - clamp((1 - 0.90) * damp_fac, 0, 1);
            w.p.ac = 0;
            if (!w.p.dead)
            {
                w.p.hc = Keys::d.down() - Keys::a.down();
                w.p.vc = Keys::s.down() - Keys::w.down();
                w.p.ac += mouse.rel_pos().x * 0.25;
            }
            else
                w.p.hc = w.p.vc = 0;

            w.p.vel += m /mul/ ivec2(w.p.hc,w.p.vc) * 0.085 / mass;
            w.p.ac += fvec2(w.p.hc,0) /cross/ mass_offset + fvec2(0,w.p.vc) /cross/ mass_offset; // Sic!
            w.p.av += w.p.ac / inertia_moment * 0.0004;

            if (!w.p.hc && !w.p.vc && (w.p.vel.len() < min_speed)) // Set speed to 0 if too small
                w.p.vel = fvec2(0,0);

            { // Reaching the goal
                if (!w.p.dead && w.goal_exists && (w.p.pos - w.boss.home).len() < player_rad)
                {
                    w.goal_exists = 0;
                    Sounds::goal({0,0}).relative();
                    Tiles::solid[Tiles::door_boss] = 0;
                    Tiles::info[Tiles::door_boss] = Tiles::Info::invis();
                    ending_time = ts.ticks;
                    w.fin = 1;
                    w.p.trash.clear();
                }
            }

            { // Shooting
                if (!w.p.dead && mouse.left.down() && w.p.laser_cd == 0)
                {
                    w.add_laser(w.p.pos + m /mul/ fvec2(laser_x_off * ((w.p.las_right = !w.p.las_right) ? 1 : -1), laser_y_off), w.p.rot - f_pi/2, 3, 160, fvec3(1,0.5,0)*20000, 2);
                    w.p.laser_cd = laser_cd;
                    Sounds::laser(w.p.pos);
                }
            }

            { // Wall collision check
                if (!w.p.dead)
                {
                    bool hit_one_way = 0,
                         hit_one_way_r = 0,
                         hit_one_way_d = 0;
                    for (const auto &it : circle)
                    {
                        fvec2 pos = w.p.pos + m /mul/ (it * player_hitbox_off);
                        if (w.map.PosSolid(pos))
                            w.kill_player();

                        auto e = w.map.Get(1, Map::TileFromPos(pos));
                        if (e == Tiles::one_way_passage) hit_one_way = 1;
                        if (e == Tiles::one_way_passage_r) hit_one_way_r = 1;
                        if (e == Tiles::one_way_passage_d) hit_one_way_d = 1;
                    }
                    if (hit_one_way && w.p.vel.y > -1 && !w.fin) // One way passage interaction
                    {
                        fvec2 dst_vel = fvec2(0,-1);
                        w.p.vel += (dst_vel - w.p.vel) * 0.15;
                    }
                    if (hit_one_way_d && w.p.vel.y < 1 && !w.fin)
                    {
                        fvec2 dst_vel = fvec2(0,1);
                        w.p.vel += (dst_vel - w.p.vel) * 0.15;
                    }
                    if (hit_one_way_r && w.p.vel.x < 1 && !w.fin)
                    {
                        fvec2 dst_vel = fvec2(1,0);
                        w.p.vel += (dst_vel - w.p.vel) * 0.15;
                    }
                }
                if (!w.p.dead) // Trash wall collisions
                {
                    auto it = w.p.trash.begin();
                    while (it != w.p.trash.end())
                    {
                        fvec2 p = w.p.pos + m /mul/ it->offset;
                        if (w.map.PosSolid(p))
                        {
                            ivec2 tp = Map::TileFromPos(p);
                            if (it->is_key && w.map.Get(1, tp) == Tiles::door) // Using a key
                            {
                                w.map.Set(1, tp, Tiles::air);
                                Sounds::block_breaks(p);
                                for (int i = 0; i < 50; i++) // Breaking particles
                                {
                                    float a = Rand::Float(-f_pi, f_pi);
                                    w.push_particle(3, 0.97, Rand::Int(-5, 5), 30, tp * Map::tsz + fvec2(Rand::Float(0,Map::tsz), Rand::Float(0,Map::tsz)), fvec2(std::cos(a), std::sin(a)) * Rand::Float(0,0.5));
                                }
                                it = w.p.trash.erase(it);
                                continue;
                            }
                            w.kill_player();
                        }
                        it++;
                    }
                }
            }

            { // Engine particles
                if (w.p.vc != 0)
                {
                    int range = (w.p.vc < 0 ? 4 : 2);
                    w.push_particle(0, 0.99, Rand::Int(w.p.vc < 0 ? 0 : 15,
                                                       w.p.vc < 0 ? 15 : 25), 30, w.p.pos + m /mul/ ivec2(Rand::Int(-range,range),w.p.vc < 0 ? engine_light_y_off : -engine_light_y_off_front), w.p.vel + m /mul/ ivec2(0,w.p.vc * 0.1));
                }
                if (w.p.hc != 0)
                {
                    w.push_particle(0, 0.99, Rand::Int(15, 25), 30, w.p.pos + m /mul/ ivec2(-w.p.hc * engine_light_x_off, Rand::Int(-2,2)), w.p.vel + m /mul/ ivec2(w.p.hc * 0.1, 0));
                }
            }

            { // Death effects
                if (w.p.dead)
                {
                    if (w.p.dead == 1)
                    {
                        Sounds::death(w.p.pos);

                        for (int i = 0; i < 100; i++)
                        {
                            float a = Rand::Float(-f_pi, f_pi);
                            float r = Rand::Float(0, 1);
                            fvec2 v = fvec2(std::cos(a)*r, std::sin(a)*r);

                            float a2 = Rand::Float(-f_pi, f_pi);
                            float r2 = Rand::Float(0, 1);
                            fvec2 v2 = fvec2(std::cos(a2)*r2, std::sin(a2)*r2);

                            w.push_particle(1, 0.99, Rand::Int(-60, -30), 60, w.p.pos + v*22, v2*3);
                        }
                    }

                    if (w.p.dead > allow_respawn_after && (Keys::any.pressed() || mouse.any_button.pressed()))
                    {
                        w = w_saved;
                        Sounds::restart({0,0}).relative();
                        w.p.trash.clear();
                        w.finalize_map();
                        if (w.active_checkpoint != -1)
                        {
                            w.p.pos = w.checkpoint_list[w.active_checkpoint];
                            w.p.vel = fvec2(0);
                        }
                        w.extra_exp = w.extra_exp_max;
                    }

                }
            }
        }

        { // Timing
            w.extra_exp = max(0, w.extra_exp + exposure_step);
            if (w.p.laser_cd > 0)
                w.p.laser_cd--;
            if (w.fin > 0)
            {
                w.fin++;
                if (w.fin > 180 && Keys::escape.pressed())
                    Program::Exit();
            }
            if (starting_time != 0)
                starting_screen_alpha = max(starting_screen_alpha - 0.015, 0);
        }
    };
    auto Render = [&]
    {
        // Background
        Draw::Background();

        fmat2 m = fmat2::rotate2D(w.p.rot);

        // Map background
        w.map.Render(2);

        { // Boss
            // Core
            if (!w.boss.dead)
                r.Quad_f(w.boss.pos, boss_z+30, ivec2(50)).tex(ivec2(336,376)).center().rotate(w.boss.rot);

            // Shell
            r.Quad_f(w.boss.pos, boss_z, ivec2(96/2,96)).tex(ivec2(336,279)).center(fvec2(96/2,96/2)).rotate(w.boss.rot).translate_f(fvec2(-w.boss.core_offset,0));
            r.Quad_f(w.boss.pos, boss_z, ivec2(96/2,96)).tex(ivec2(336+96/2,279)).center(fvec2(0,96/2)).rotate(w.boss.rot).translate_f(fvec2(w.boss.core_offset,0));

            // HP indicator
            for (int i = 0; i < w.boss.hp; i++)
                r.Quad(w.boss.pos, 100, ivec2(9)).tex(ivec2(60,168)+1).center().beta(0).rotate(w.boss.rot).translate_f(fvec2(-boss_hp_offset - i * boss_hp_gap - w.boss.core_offset,0));

            // Wings
            r.Quad_f(w.boss.pos, boss_z, ivec2(44,157)).tex(ivec2(433,279)).center(fvec2(-4-w.boss.core_offset,-10-w.boss.wing_offset)).rotate(w.boss.rot - w.boss.wing_an_s + f_pi/2);
            r.Quad_f(w.boss.pos, boss_z, ivec2(44,157)).tex(ivec2(433,279)).center(fvec2(-4-w.boss.core_offset,-10-w.boss.wing_offset)).rotate(w.boss.rot + w.boss.wing_an_s + f_pi/2).scale(ivec2(-1,1));
            if (w.boss.dead)
            {
                float t = 1 - abs(w.boss.dead - 70) / 70.;
                if (t > 0)
                {
                    t = smoothstep(t);
                    w.push_light(fvec3(1,0,0.5) * t * 200000, w.boss.pos.to_vec3(light_z), t * 24);
                }
            }
        }

        { // Goal
            if (w.goal_exists)
            {
                float t = clamp(w.boss.dead / 90. - 1, 0,1);
                r.Quad(w.boss.home + fvec2(Rand::Float(-0.7,0.7),Rand::Float(-0.7,0.7)), 100, ivec2(11)).tex(ivec2(72,168)).center().beta(0).alpha(t);
                w.push_light(fvec3(1,0.8,0.6) * 1000 * t, w.boss.home.to_vec3(light_z));
            }
        }

        { // Homing mines
            for (const auto &it : w.homing_mine_list)
            {
                if (!w.at_screen_circle(it.pos, 20)) continue;
                r.Quad_f(it.pos, player_z + 12, ivec2(64-2)).tex(ivec2(208,279)+1).center().rotate(it.angle + f_pi/2);
                if (it.vel.any())
                    w.push_light(fvec3(1,0.1,0.25) * 300, it.pos.to_vec3(light_z), 4);
            }
        }

        { // Laser turrets
            for (const auto &it : w.laser_turret_list)
            {
                if (!w.at_screen_circle(it.pos, 20)) continue;
                r.Quad_f(it.pos, player_z + 18, ivec2(64-2)).tex(ivec2(208+64,279)+1).center();
                w.push_light(fvec3(1,0.1,0.25) * 300, it.pos.to_vec3(light_z), 4);
            }
        }

        { // Player
            // Sprite
            if (!w.p.dead) r.Quad_f(w.p.pos, player_z, ivec2(48-2)).tex(ivec2(0,168)+1).center().rotate(w.p.rot);

            // Player trash blocks
            {
                if (!w.p.dead)
                {
                    for (const auto &it : w.p.trash)
                        r.Quad_f(w.p.pos, player_z - 10, ivec2(14)).tex(ivec2(272,215 + 14 * it.is_key)).center().rotate(w.p.rot).translate_f(it.offset).rotate(it.vis_angle);
                }
            }

            { // Main light
                w.push_light(fvec3(1,0.8,.6)*1000, (w.p.pos + m /mul/ ivec2(0, -light_dist)).to_vec3(light_z), 5);
            }

            { // Engine lights
                if (w.p.vc != 0) w.push_light(fvec3(1,0.7,0) * 20, (w.p.pos + m /mul/ ivec2(0,-w.p.vc * engine_light_y_off)).to_vec3(player_z + engine_light_z_off));
                if (w.p.hc != 0) w.push_light(fvec3(1,0.7,0) * 20, (w.p.pos + m /mul/ ivec2(-w.p.hc * engine_light_y_off,0)).to_vec3(player_z + engine_light_z_off));
            }
        }

        { // Trash blocks
            for (const auto &it : w.trash_block_list)
            {
                r.Quad_f(it.pos, 60, ivec2(14)).tex(ivec2(272,215 + 14 * it.is_key)).center().rotate(it.vis_angle);
            }
        }

        // Map
        w.map.Render(1);
        w.map.Render(0);

        { // Lasers
            for (const auto &it : w.laser_list)
                w.push_light_line(it.color * it.len, it.pos.to_vec3(laser_li_z), (it.pos - it.dir() * it.len).to_vec3(laser_li_z));
        }

        { // Checkpoints
            for (const auto &it : w.checkpoint_list)
            {
                if (!w.at_screen_circle(it, 150))
                    continue;
                w.push_light(fvec3(0.05,0.7,1) * 200, it.to_vec3(light_z), 8);
            }
        }

        { // Particles
            for (const auto &it : w.par_queue)
            {
                r.Quad_f(it.pos, 0, ivec2(9)).tex(ivec2(48 + 12 * (it.index / 3),180 + 12 * (it.index % 3))+1).center().beta(0).scale_f(1 - it.t / float(it.maxt));
            }
        }

        // Death effects
        if (w.p.dead)
        {
            constexpr float start = 45, len = 90;

            if (w.p.dead > 30)
            {
                bool alt = Rand::Float(0,1) < 0.1;
                r.Quad_f(w.cam_pos, 0, ivec2(200,50)).tex({0,216 + 50 * alt}).beta(0).alpha(clamp((w.p.dead - start) / len, 0, 1)).center().rotate(w.cam_rot);
                if (!alt)
                    r.Quad_f(w.cam_pos + m /mul/ fvec2(0, 40), 0, ivec2(200,20)).tex({0,316}).beta(0).alpha(clamp((w.p.dead - start) / len, 0, 1)).center().rotate(w.cam_rot);
            }

            if (w.p.dead < death_light_len)
            {
                float s = std::sin(w.p.dead / float(death_light_len) * f_pi);
                s = ipow(s, 2);
                w.push_light(fvec3(0,0.7,1) * 70000 * s, w.p.pos.to_vec3(light_z), s * 30);
            }
            else if (w.p.dead > death_light_2_start)
            {
                int t = min(w.p.dead - death_light_2_start, death_light_2_t_cap);
                w.push_light(fvec3(2,0.1,1) * t / float(death_light_2_t_cap) * death_light_2_fac, w.cam_pos.to_vec3(light_z), t / float(death_light_2_t_cap) * 30);
            }
        }

        // Ending
        if (w.fin > 60)
        {
            constexpr int stats_gap = 12;
            bool alt = Rand::Float(0,1) < 0.1;
            float t = clamp((w.fin - 60) / 120., 0, 1);
            r.Quad_f(w.cam_pos, 0, ivec2(200,86)).tex({0,336+86*alt}).beta(0).alpha(t).center().rotate(w.cam_rot).translate(ivec2(0,-20));
            float t2 = clamp((w.fin - 120) / 120., 0, 1);
            r.Quad_f(w.cam_pos, 0, ivec2(200,50)).tex({0,508}).beta(0).alpha(t2 * (alt ? 0.2 : 1)).center().rotate(w.cam_rot).translate(ivec2(-stats_gap,100));

            auto Text = [&](std::string str, fvec2 pos)
            {
                static const std::string font = "0123456789:.";
                for (char ch : str)
                {
                    auto index = font.find_first_of(ch);
                    if (index != str.npos)
                    {
                        r.Quad_f(w.cam_pos, 0, ivec2(8,16)).tex({int(8*index),559}).beta(0).alpha(t2 * (alt ? 0.2 : 1)).center(fvec2(0,8)).rotate(w.cam_rot).translate(pos);
                    }
                    pos.x += 11;
                }
            };

            Text(Str(death_counter), fvec2(stats_gap, 86));
            int ticks = ending_time - starting_time;
            int secs = ticks / 60;
            int mins = secs / 60;
            int hours = mins / 60;
            ticks %= 60;
            secs %= 60;
            mins %= 60;
            std::string ms_str = Str(iround(ticks / 60. * 1000));
            while (ms_str.size() < 3)
                ms_str = '0' + ms_str;
            Text(Str(hours, ':', mins, ':', secs, '.', ms_str), fvec2(stats_gap, 86+24));
        }

        // Start screen
        if (starting_screen_alpha > 0)
            r.Quad_f(w.cam_pos, 0, screen_sz).tex({1024-screen_sz.x,0}).beta(0).alpha(smoothstep(clamp(starting_screen_alpha,0,1)) * 0.04).center().rotate(w.cam_rot);

        { // Transitions
            r.Exposure(-w.extra_exp);
        }
    };
    auto Light = [&]
    {
        for (const auto &it : w.light_queue)
        {
            if (it.tube)
                r.LineLight(it.color, it.pos, it.end, it.rad);
            else
                r.PointLight(it.color, it.pos, it.rad);
        }

        w.light_queue.clear();
    };

    const fmat4 proj = fmat4::ortho(screen_sz * fvec2(-0.5,0.5),screen_sz * fvec2(0.5,-0.5),-1000000,1000000);

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
            if (win.size_changed)
            {
                win.size_changed = 0;
                r.Resize(win.Size());
            }
            Graphics::CheckErrors();
            audio_con.CheckErrors();
            Audio::Source::Tick();
            Tick();
        }

        view = fmat4::rotate2D(-w.cam_rot) /mul/ fmat4::translate(-w.cam_pos.to_vec3());
        r.SetMatrix(proj_view = proj /mul/ view);
        r.DrawStart();
        Render();
        r.DrawAmbient();
        Light();
        r.DrawFinal();
        win.Swap();
    }

    return 0;
}
