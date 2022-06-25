#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

using std::conditional_t;
using std::domain_error;
using std::exception;
using std::ifstream;
using std::int16_t;
using std::int32_t;
using std::int8_t;
using std::ofstream;
using std::optional;
using std::string;
using std::string_view;
using std::uint16_t;
using std::uint32_t;
using std::uint8_t;
using std::variant;
using std::filesystem::path;

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

enum class Endian {
    BIG,
    LITTLE,
};

enum class BitSize {
    N16 = 16,
    N32 = 32,
    N64 = 64,
};

enum class Sign {
    SIGNED,
    UNSIGNED,
};

void my_assert(bool value) {
    if (value) {
        return;
    }
    throw std::domain_error("assert failed");
}

void my_print(string_view buffer) {
    std::cerr << "buffer: ";
    for (auto ch : buffer) {
        std::cerr << std::setfill('0') << std::setw(2) << std::hex << (unsigned(ch) & 0xff);
    }
    std::cerr << "\n";
}

template <Sign sign, BitSize bit_size, Endian endian>
struct Int {
    using Int16T = conditional_t<sign == Sign::UNSIGNED, uint16_t, int16_t>;
    using Int32T = conditional_t<sign == Sign::UNSIGNED, uint32_t, int32_t>;
    using Int64T = conditional_t<sign == Sign::UNSIGNED, uint64_t, int64_t>;

    using Int_1_T = conditional_t<bit_size == BitSize::N32, Int32T, Int16T>;
    using IntT = conditional_t<bit_size == BitSize::N64, Int64T, Int_1_T>;

    [[nodiscard]] auto operator()() const { return value_; }

    auto from_str(string_view buffer) {
        my_assert(buffer.size() >= byte_size());
        auto value = UIntT{};
        if constexpr (endian == Endian::BIG) {
            for (auto i = 0; i < byte_size(); i++) {
                value |= (static_cast<UIntT>(buffer[byte_size() - i - 1]) & 0xff) << (i * 8);
            }
        } else {
            for (auto i = 0; i < byte_size(); i++) {
                value |= (static_cast<UIntT>(buffer[i]) && 0xff) << (i * 8);
            }
        }
        value_ = value;
        return byte_size();
    }

private:
    using UInt_1_T = conditional_t<bit_size == BitSize::N32, uint32_t, uint16_t>;
    using UIntT = conditional_t<bit_size == BitSize::N64, uint64_t, UInt_1_T>;

    constexpr auto byte_size() { return static_cast<int>(bit_size) / 8; }

    IntT value_{};
};

using UInt16Little = Int<Sign::UNSIGNED, BitSize::N16, Endian::LITTLE>;
using UInt32Little = Int<Sign::UNSIGNED, BitSize::N32, Endian::LITTLE>;
using UInt64Little = Int<Sign::UNSIGNED, BitSize::N64, Endian::LITTLE>;
using SInt16Little = Int<Sign::SIGNED, BitSize::N16, Endian::LITTLE>;
using SInt32Little = Int<Sign::SIGNED, BitSize::N32, Endian::LITTLE>;
using SInt64Little = Int<Sign::SIGNED, BitSize::N64, Endian::LITTLE>;

using UInt16Big = Int<Sign::UNSIGNED, BitSize::N16, Endian::BIG>;
using UInt32Big = Int<Sign::UNSIGNED, BitSize::N32, Endian::BIG>;
using UInt64Big = Int<Sign::UNSIGNED, BitSize::N64, Endian::BIG>;
using SInt16Big = Int<Sign::SIGNED, BitSize::N16, Endian::BIG>;
using SInt32Big = Int<Sign::SIGNED, BitSize::N32, Endian::BIG>;
using SInt64Big = Int<Sign::SIGNED, BitSize::N64, Endian::BIG>;

struct FramePacket {
    [[nodiscard]] auto buffer() const { return string_view(buffer_); }

    auto consume(std::string_view buffer) {
        constexpr auto HEADER_SIZE = 4;

        if (buffer.size() < HEADER_SIZE) {
            throw(domain_error(string("to less frame packet size: ") + std::to_string(buffer.size())));
        }

        auto size = UInt16Big{};
        size.from_str(buffer);
        buffer_ = buffer.substr(HEADER_SIZE, size() - HEADER_SIZE);
        return size();
    }

private:
    string_view buffer_{};
};

struct FrameUpdate {
    [[nodiscard]] auto buffer() const { return string_view(buffer_); }

    auto consume(std::string_view buffer) {
        constexpr auto HEADER_SIZE = 2;

        if (buffer.size() < HEADER_SIZE) {
            throw(domain_error(string("to less frame update size: ") + std::to_string(buffer.size())));
        }

        auto size = UInt16Big{};
        size.from_str(buffer);
        buffer_ = buffer.substr(HEADER_SIZE, size() - HEADER_SIZE);
        return size();
    }

private:
    string_view buffer_{};
};

struct Quote {
    auto from_str(string_view buffer) {}
};

struct Trade {
    [[nodiscard]] auto symbol() const { return symbol_; }
    [[nodiscard]] auto size() const { return size_(); }
    [[nodiscard]] auto price() const { return price_(); }

    auto from_str(string_view buffer) {
        constexpr auto MIN_SIZE = 16;

        if (buffer.size() < MIN_SIZE) {
            throw(domain_error(string("to less trade size: ") + std::to_string(buffer.size())));
        }

        symbol_ = buffer.substr(1, 5);
        auto first_space = symbol_.find_first_of(' ');
        if (first_space != string_view::npos) {
            symbol_.remove_suffix(symbol_.size() - first_space);
        }
        size_.from_str(buffer.substr(6, 2));
        price_.from_str(buffer.substr(8, 8));
    }

private:
    string_view symbol_{};
    UInt16Big size_{};
    UInt64Big price_{};
};

using Update = variant<Quote, Trade>;

auto to_update(FrameUpdate const& frame) {
    auto buffer = frame.buffer();
    constexpr auto MIN_SIZE = 1;

    if (buffer.size() < MIN_SIZE) {
        throw(domain_error(string("to less update size: ") + std::to_string(buffer.size())));
    }

    switch (buffer[0]) {
    case 'Q': {
        auto quote = Quote{};
        quote.from_str(buffer);
        return Update{quote};
    }
    case 'T': {
        auto trade = Trade{};
        trade.from_str(buffer);
        return Update{trade};
    }
    default: throw(domain_error(string("wrong frame update: ") + buffer[0]));
    }
}

template <typename F>
struct Stream {
    struct Iterator {
        using iterator_category = std::input_iterator_tag;
        using value_type = F;
        using pointer = value_type const*;
        using reference = value_type const&;

        Iterator(Stream& stream, optional<F> frame) : stream_(stream), frame_(std::move(frame)) {}

        auto operator*() const -> reference { return frame_.value(); }

        auto operator++() -> Iterator& {
            frame_ = stream_.next();
            return *this;
        }

        friend auto operator!=(Iterator const& l, Iterator const& r) {
            return l.frame_.has_value() != r.frame_.has_value();
        }

    private:
        Stream& stream_;
        optional<F> frame_{};
    };

    Stream(string_view buffer) : buffer_(buffer) {}

    auto begin() { return Iterator{*this, next()}; }
    auto end() { return Iterator{*this, {}}; }

private:
    friend Iterator;

    auto next() -> optional<F> {
        if (buffer_.empty()) {
            return {};
        }
        auto frame = F{};
        buffer_.remove_prefix(frame.consume(buffer_));
        return frame;
    }

    string_view buffer_{};
};

int main() {
    auto const path_in = path{"input.dat"};
    auto const path_out = path{"output.log"};
    try {
        auto const file_size = std::filesystem::file_size(path_in);
        auto buffer = string(file_size, 0);
        ifstream{path_in, std::ios::in | std::ios::binary}.read(buffer.data(), static_cast<std::streamsize>(file_size));

        auto out_stream = ofstream(path_out, std::ios::out);

        for (auto const& frame_packet : Stream<FramePacket>(buffer)) {
            for (auto const& frame_update : Stream<FrameUpdate>(frame_packet.buffer())) {
                auto update = to_update(frame_update);
                std::visit(
                    overloaded{
                        [](Quote const& quote) {},
                        [&](Trade const& trade) {
                            out_stream << std::dec << trade.size() * 1000 << " " << trade.symbol() << " @ "
                                       << std::setprecision(3) << double(trade.price()) / 10000 << "\n";
                        }},
                    update);
            }
        }
    } catch (exception const& err) {
        std::cerr << err.what() << "\n";
        return 1;
    }
    return 0;
}
