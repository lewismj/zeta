#include "spot_summary_helpers.h"

#include <sstream>
#include <string>
#include <vector>

namespace zeta::holdem::ui {

    namespace {

        [[nodiscard]] QString actor_label(const spot& source)
        {
            if (source.root_actor < source.players.size()) {
                return QString::fromStdString(source.players[source.root_actor]);
            }
            return QStringLiteral("Actor %1").arg(static_cast<unsigned>(source.root_actor));
        }

        [[nodiscard]] std::vector<std::string> range_tokens(const std::string& range)
        {
            std::vector<std::string> tokens;
            std::string token;
            std::istringstream input{range};
            while (std::getline(input, token, ',')) {
                const auto first = token.find_first_not_of(" \t\r\n");
                const auto last = token.find_last_not_of(" \t\r\n");
                if (first != std::string::npos && last != std::string::npos) {
                    tokens.push_back(token.substr(first, last - first + 1));
                }
            }
            return tokens;
        }

    }

    std::size_t editable_range_index(const spot& source)
    {
        if (source.root_actor < source.ranges.size()) {
            return source.root_actor;
        }
        if (source.hero_seat < source.ranges.size()) {
            return source.hero_seat;
        }
        return 0;
    }

    QString range_summary_title(const spot& source)
    {
        return QStringLiteral("%1 range").arg(actor_label(source));
    }

    QString range_summary_value(const spot& source)
    {
        const auto range_index = editable_range_index(source);
        const auto range = range_index < source.ranges.size() ? source.ranges[range_index] : std::string{};
        return QStringLiteral("%1 hands").arg(range_tokens(range).size());
    }

    QString bet_summary_value(const spot& source)
    {
        return QStringLiteral("%1% pot").arg(source.bet_fraction * 100.0, 0, 'f', 1);
    }

}
