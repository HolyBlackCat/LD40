#ifndef AUDIO_H_INCLUDED
#define AUDIO_H_INCLUDED

#include <cstring>
#include <fstream>
#include <memory>
#include <unordered_set>
#include <utility>

#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alc.h>
#include <vorbis/vorbisfile.h>

#include "mat.h"
#include "program.h"
#include "utils.h"

namespace Audio
{
    //{ Exceptions
    DefineExceptionBase(exception)

    DefineExceptionInline(wrong_format, :exception, "Wrong audio format.",
        (std::string,type,"Expected")
    )
    DefineExceptionInline(cant_parse_sound, :exception, "Unable to parse a sound.",
        (std::string,name,"Name")
        (std::string,reason,"Reason")
    )
    DefineExceptionInline(cant_save_sound, :exception, "Unable to save a sound.",
        (std::string,name,"Name")
        (std::string,reason,"Reason")
    )
    DefineExceptionInline(cant_create_al_resource, :exception, "Can't create an OpenAL resource.",
        (std::string,type,"Type")
    )
    //}

    // Those settings are global for all audio objects.
    inline void Volume(float vol)
    {
        alListenerf(AL_GAIN, vol);
    }
    inline void Pitch(float pitch)
    {
        alListenerf(AL_PITCH, pitch);
    }

    inline void ListenerPos(const fvec3 &v)
	{
		alListenerfv(AL_POSITION, v.as_array());
	}
	inline void ListenerVelocity(const fvec3 &v)
	{
		alListenerfv(AL_VELOCITY, v.as_array());
	}
	inline void ListenerRot(const fvec3 &forw, const fvec3 &up)
	{
		fvec3 arr[2] = {forw, up};
		alListenerfv(AL_ORIENTATION, (float *)arr);
	}

    inline void DopplerFactor(float n)
    {
        alDopplerFactor(n);
    }
	inline void SpeedOfSound(float n)
	{
	    alSpeedOfSound(n);
    }


    class Sound
    {
      public:
        enum Format_t
        {
            mono8    = AL_FORMAT_MONO8,
            mono16   = AL_FORMAT_MONO16,
            stereo8  = AL_FORMAT_STEREO8,
            stereo16 = AL_FORMAT_STEREO16,
        };
      private:
        std::vector<uint8_t> data;
        int freq = 44100;
        Format_t format = mono8;
      public:
        void FromWAV(Utils::MemoryFile file)
        {
            if (file.Size() < 44) // 44 is the size of WAV header.
                throw cant_parse_sound(file.Name(), "The file is too small for a header.");

            const uint8_t *ptr = file.Data();

            if (memcmp(ptr, "RIFF", 4)) throw cant_parse_sound(file.Name(), "No \"RIFF\" label.");
            ptr += 4;

            uint32_t new_byte_size = Utils::Little((uint32_t &)*ptr) - 36;
            if (file.Size() < new_byte_size + 44) throw cant_parse_sound(file.Name(), "Unexpected end of file.");
            ptr += 4;

            if (memcmp(ptr, "WAVE", 4)) throw cant_parse_sound(file.Name(), "No \"WAVE\" label.");
            ptr += 4;

            if (memcmp(ptr, "fmt ", 4)) throw cant_parse_sound(file.Name(), "No \"fmt \" label.");
            ptr += 4;

            if (memcmp(ptr, "\x10\0\0", 4)) throw cant_parse_sound(file.Name(), "File structure is too complicated.");
            ptr += 4;

            if (memcmp(ptr, "\x1", 2)) throw cant_parse_sound(file.Name(), "File is compresssed or uses floating-point samples.");
            ptr += 2;

            uint16_t channels = Utils::Little((uint16_t &)*ptr);
            if (channels == 0 || channels > 2) throw cant_parse_sound(file.Name(), "The file must be mono or stereo.");
            ptr += 2;

            uint32_t new_freq = Utils::Little((uint32_t &)*ptr);
            ptr += 4;

            ptr += 6; // Skip useless data.

            uint16_t bits_per_sample = Utils::Little((uint16_t &)*ptr);
            if (bits_per_sample != 8 && bits_per_sample != 16) throw cant_parse_sound(file.Name(), "The file must use 8 or 16 bits per sample.");
            if (bits_per_sample == 16 && new_byte_size % 2 != 0) throw cant_parse_sound(file.Name(), "The file uses 16 bits per sample, but data size is not a multiple of two.");
            ptr += 2;

            if (memcmp(ptr, "data", 4)) throw cant_parse_sound(file.Name(), "No \"data\" label.");
            ptr += 4;

            if (Utils::Little((uint32_t &)*ptr) != new_byte_size) throw cant_parse_sound(file.Name(), "The file contains additional sectons or is corrupted.");
            ptr += 4;

            Format_t new_format;
            switch (channels << 16 | bits_per_sample)
            {
                case 1 << 16 |  8: new_format = mono8;    break;
                case 1 << 16 | 16: new_format = mono16;   break;
                case 2 << 16 |  8: new_format = stereo8;  break;
                case 2 << 16 | 16: new_format = stereo16; break;
            }

            std::vector<uint8_t> new_data;
            new_data.reserve(new_byte_size);
            new_data.insert(new_data.end(), ptr, ptr + new_byte_size);
            if (Utils::big_endian && bits_per_sample == 16)
                for (std::size_t i = 0; i < new_data.size() / 2; i++)
                    Utils::MakeLittle(((uint16_t *)new_data.data())[i]);

            data = std::move(new_data);
            freq = new_freq;
            format = new_format;
        }
        void FromOGG(Utils::MemoryFile file, bool load_as_8bit = 0)
        {
            (void)OV_CALLBACKS_DEFAULT;
            (void)OV_CALLBACKS_NOCLOSE;
            (void)OV_CALLBACKS_STREAMONLY;
            (void)OV_CALLBACKS_STREAMONLY_NOCLOSE;

            struct Desc
            {
                const uint8_t *start, *cur, *end;
            };
            Desc desc{file.Data(), file.Data(), file.Data() + file.Size()};

            ov_callbacks callbacks;
            callbacks.tell_func = [](void *ptr) -> long
            {
                Desc &ref = *(Desc *)ptr;
                return ref.cur - ref.start;
            };
            callbacks.seek_func = [](void *ptr, int64_t offset, int mode) -> int
            {
                Desc &ref = *(Desc *)ptr;
                switch (mode)
                {
                  case SEEK_SET:
                    ref.cur = ref.start + offset;
                    break;
                  case SEEK_CUR:
                    ref.cur += offset;
                    break;
                  case SEEK_END:
                    ref.cur = ref.end + offset;
                    break;
                  default:
                    return 1;
                }
                if (ref.cur < ref.start || ref.cur > ref.end)
                    return 1;
                return 0;
            };
            callbacks.read_func = [](void *dst, std::size_t sz, std::size_t count, void *ptr) -> std::size_t
            {
                Desc &ref = *(Desc *)ptr;
                if (ref.cur + count * sz > ref.end)
                    count = (ref.end - ref.cur) / sz;
                std::copy(ref.cur, ref.cur + count, (uint8_t *)dst);
                ref.cur += count * sz;
                return count;
            };
            callbacks.close_func = 0;

            OggVorbis_File ogg_file;
            switch (ov_open_callbacks(&desc, &ogg_file, 0, 0, callbacks))
            {
              case 0:
                break;
              case OV_EREAD:
                throw cant_parse_sound(file.Name(), "Unable to read data from the stream.");
                break;
              case OV_ENOTVORBIS:
                throw cant_parse_sound(file.Name(), "This is not vorbis audio.");
                break;
              case OV_EVERSION:
                throw cant_parse_sound(file.Name(), "Vorbis version mismatch.");
                break;
              case OV_EBADHEADER:
                throw cant_parse_sound(file.Name(), "Invalid header.");
                break;
              case OV_EFAULT:
                throw cant_parse_sound(file.Name(), "Internal vorbis error.");
                break;
              default:
                throw cant_parse_sound(file.Name(), "Unknown vorbis error.");
                break;
            }
            // IMPORTANT: Below this line you must do `ov_clear(&ogg_file);` before returning or throwing anything.

            vorbis_info *info = ov_info(&ogg_file, -1);
            uint64_t samples = ov_pcm_total(&ogg_file, -1);
            if (samples > 0xffffffffu)
            {
                ov_clear(&ogg_file);
                throw cant_parse_sound(file.Name(), "The file is too big.");
            }

            Sound new_obj;

            try
            {
                switch (info->channels << 16 | load_as_8bit)
                {
                  case 1 << 16 | 1:
                    new_obj.FromMemory(mono8, info->rate, samples);
                    break;
                  case 1 << 16 | 0:
                    new_obj.FromMemory(mono16, info->rate, samples);
                    break;
                  case 2 << 16 | 1:
                    new_obj.FromMemory(stereo8, info->rate, samples);
                    break;
                  case 2 << 16 | 0:
                    new_obj.FromMemory(stereo16, info->rate, samples);
                    break;
                  default:
                    throw cant_parse_sound(file.Name(), Str("The file must be mono or stereo, but this one has ", info->channels, " channels."));
                    break;
                }
            }
            catch (...)
            {
                ov_clear(&ogg_file);
                throw;
            }

            uint32_t buf_len = new_obj.Bytes();
            char *buf = (char *)new_obj.Data();

            int current_bitstream = -1;
            while (1)
            {
                if (buf_len == 0)
                    break;
                int bitstream;
                long val = ov_read(&ogg_file, buf, buf_len, Utils::big_endian, load_as_8bit ? 1 : 2, !load_as_8bit, &bitstream);
                if (val == 0)
                {
                    ov_clear(&ogg_file);
                    throw cant_parse_sound(file.Name(), "Unexpected end of stream.");
                }
                if (bitstream != current_bitstream)
                {
                    current_bitstream = bitstream;
                    vorbis_info *local_info = ov_info(&ogg_file, bitstream);
                    if (local_info->channels != info->channels)
                    {
                        ov_clear(&ogg_file);
                        throw cant_parse_sound(file.Name(), Str("The amount of channels has changed from ", info->channels, " to ", local_info->channels, ". Dynamic amount of channels is not supported."));
                    }
                    if (local_info->rate != info->rate)
                    {
                        ov_clear(&ogg_file);
                        throw cant_parse_sound(file.Name(), Str("The sampling rate has changed from ", info->rate, " to ", local_info->rate, ". Dynamic sampling rate is not supported."));
                    }
                }
                switch (val)
                {
                  case OV_HOLE:
                    ov_clear(&ogg_file);
                    throw cant_parse_sound(file.Name(), "The file is corrupted.");
                    break;
                  case OV_EBADLINK:
                    ov_clear(&ogg_file);
                    throw cant_parse_sound(file.Name(), "Bad link.");
                    break;
                  case OV_EINVAL:
                    ov_clear(&ogg_file);
                    throw cant_parse_sound(file.Name(), "Invalid header.");
                    break;
                  default:
                    buf += val;
                    buf_len -= val;
                    break;
                }
            }

            ov_clear(&ogg_file);

            *this = std::move(new_obj);
        }
        void FromWAV_Mono(Utils::MemoryFile file)
        {
            FromWAV(file);
            if (Stereo())
                throw wrong_format("Mono");
        }
        void FromOGG_Mono(Utils::MemoryFile file, bool load_as_8bit = 0)
        {
            FromOGG(file, load_as_8bit);
            if (Stereo())
                throw wrong_format("Mono");
        }
        void FromWAV_Stereo(Utils::MemoryFile file)
        {
            FromWAV(file);
            if (Mono())
                throw wrong_format("Stereo");
        }
        void FromOGG_Stereo(Utils::MemoryFile file, bool load_as_8bit = 0)
        {
            FromOGG(file, load_as_8bit);
            if (Mono())
                throw wrong_format("Stereo");
        }

        void SaveWAV(std::string fname)
        {
            std::ofstream out(fname, out.out | out.binary);
            if (!out)
                throw cant_save_sound(fname, "Unable to open the file.");
            uint32_t tmp32;
            uint16_t tmp16;
            out.write("RIFF", 4);
            tmp32 = Utils::Little<uint32_t>(Bytes() + 36);
            out.write((char *)&tmp32, sizeof tmp32);
            out.write("WAVEfmt " "\x10\0\0\0" "\1" /*"\0"*/, 14);
            tmp16 = Utils::Little<uint16_t>(Stereo()+1);
            out.write((char *)&tmp16, sizeof tmp16);
            tmp32 = Utils::Little<uint32_t>(freq);
            out.write((char *)&tmp32, sizeof tmp32);
            tmp32 = Utils::Little<uint32_t>(BytesPerSample() * freq);
            out.write((char *)&tmp32, sizeof tmp32);
            tmp16 = Utils::Little<uint16_t>(BytesPerSample());
            out.write((char *)&tmp16, sizeof tmp16);
            tmp16 = Utils::Little<uint16_t>(Bits16() ? 16 : 8);
            out.write((char *)&tmp16, sizeof tmp16);
            out.write("data", 4);
            tmp32 = Utils::Little<uint32_t>(Bytes());
            out.write((char *)&tmp32, sizeof tmp32);

            if (!out)
                throw cant_save_sound(fname, "Unable to write to the file.");

            if (Bits8() || Utils::little_endian)
                out.write((char *)data.data(), data.size());
            else
            {
                for (std::size_t i = 0; i < data.size() / 2; i++)
                {
                    tmp16 = Utils::Little(((uint16_t *)data.data())[i]);
                    out.write((char *)&tmp16, sizeof tmp16);
                }
            }

            if (data.size() % 2 == 1)
                out.put(0); // Padding.

            if (!out)
                throw cant_save_sound(fname, "Unable to write to the file.");
        }

        [[nodiscard]] static Sound WAV       (Utils::MemoryFile file) {Sound ret; ret.FromWAV       (file); return ret;}
        [[nodiscard]] static Sound WAV_Mono  (Utils::MemoryFile file) {Sound ret; ret.FromWAV_Mono  (file); return ret;}
        [[nodiscard]] static Sound WAV_Stereo(Utils::MemoryFile file) {Sound ret; ret.FromWAV_Stereo(file); return ret;}
        [[nodiscard]] static Sound OGG       (Utils::MemoryFile file, bool load_as_8bit = 0) {Sound ret; ret.FromOGG       (file, load_as_8bit); return ret;}
        [[nodiscard]] static Sound OGG_Mono  (Utils::MemoryFile file, bool load_as_8bit = 0) {Sound ret; ret.FromOGG_Mono  (file, load_as_8bit); return ret;}
        [[nodiscard]] static Sound OGG_Stereo(Utils::MemoryFile file, bool load_as_8bit = 0) {Sound ret; ret.FromOGG_Stereo(file, load_as_8bit); return ret;}

        void FromMemory(Format_t new_format, int new_freq, uint32_t new_sample_count, const uint8_t *new_data = 0)
        {
            format = new_format;
            freq = new_freq;
            data.resize(new_sample_count * BytesPerSample());
            if (new_data)
                std::copy(new_data, new_data + data.size(), (uint8_t *)data.data());
        }

        int BytesPerSample() const
        {
            switch (format)
            {
                default:
                case mono8:    return 1;
                case mono16:   return 2;
                case stereo8:  return 2;
                case stereo16: return 4;
            }
        }

        void Clear()
        {
            data = {};
        }
        uint8_t *Data()
        {
            return data.data();
        }
        const uint8_t *Data() const
        {
            return data.data();
        }
        int Samples() const
        {
            return data.size() / BytesPerSample();
        }
        uint32_t Bytes() const
        {
            return data.size();
        }
        uint32_t Frequency() const
        {
            return freq;
        }
        Format_t Format() const
        {
            return format;
        }
        bool Mono() const
        {
            return format == Format_t::mono8 || format == Format_t::mono16;
        }
        bool Stereo() const
        {
            return format == Format_t::stereo8 || format == Format_t::stereo16;
        }
        bool Bits8() const
        {
            return format == Format_t::mono8 || format == Format_t::stereo8;
        }
        bool Bits16() const
        {
            return format == Format_t::mono16 || format == Format_t::stereo16;
        }
    };

    class Buffer
    {
        class HandleFuncs
        {
            template <typename> friend class ::Utils::Handle;
            static ALuint Create() {ALuint value; alGenBuffers(1, &value); return value;}
            static void Destroy(ALuint value) {alDeleteBuffers(1, &value);}
            static void Error() {throw cant_create_al_resource("Buffer");}
        };
        using Handle_t = Utils::Handle<HandleFuncs>;
        Handle_t handle;

      public:
        Buffer(decltype(nullptr)) : handle(Handle_t::params_t{}) {}
        Buffer() {}
        Buffer(const Sound &sound) : Buffer(nullptr) {SetData(sound);}

        void Create()
        {
            handle.create({});
        }
        void Destroy()
        {
            handle.destroy();
        }
        bool Exists() const
        {
            return bool(handle);
        }

        void SetData(Sound::Format_t format, int freq, int bytes, const uint8_t *ptr = 0)
        {
            DebugAssert("Attempt to use a null audio buffer.", *handle);
            alBufferData(*handle, (ALenum)format, ptr, bytes, freq);
        }
        void SetData(const Sound &data)
        {
            SetData(data.Format(), data.Frequency(), data.Bytes(), data.Data());
        }
    };

    class SourceList;

    class Source
    {
        friend class SourceList;

        class Object
        {
            ALuint handle;
          public:
            Object()
            {
                alGenSources(1, &handle);
                if (!handle)
                    throw cant_create_al_resource("Buffer");
            }
            Object(const Object &) = delete;
            Object &operator=(const Object &) = delete;
            ~Object()
            {
                alDeleteSources(1, &handle);
            }

            operator ALuint() const
            {
                return handle;
            }

            std::unordered_set<std::shared_ptr<Source>> *set;
        };

        std::shared_ptr<Object> object;

      public:
        Source() {}

        // This deletes copy constructor and assignment.
        Source(Source &&o) = default;
        Source &operator=(Source &&o) = default;

        void Create(SourceList &list);
        void Destroy();
        bool Exists() const
        {
            return bool(object);
        }

        #error make functions here
    };

    class SourceList
    {
        std::unordered_set<std::shared_ptr<Source>> set;
      public:
        SourceList()
        {
            #error make me
        }
        void Tick()
        {
            #error make me
            #error remove unused sources here
        }
    };

    inline void Source::Create(SourceList &list)
    {
        if (Exists())
            Destroy();
        Object new_object = std::make_shared<Object>();
        new_object->set = &list;
        list.set.insert(new_object);
        object = new_object;
    }
    inline void Source::Destroy()
    {
        if (!Exists())
            return;
        object->set.erase(object);
        object.reset();
    }
}

#endif
