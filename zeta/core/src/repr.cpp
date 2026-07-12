#include "repr.h"

namespace zeta {

    std::ostream& operator<<(std::ostream& os, const suit s) {
        switch (s) {
            case suit::spades: os << "♠"; break;
            case suit::hearts: os << "♥"; break;
            case suit::diamonds: os << "♦"; break;
            case suit::clubs: os << "♣"; break;
        }
        return os;
    }

}
