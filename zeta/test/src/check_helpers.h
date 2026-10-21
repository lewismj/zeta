#pragma once

#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <iomanip>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>

namespace zeta::test {

    /**
     * Collects element-level checks from a large loop and reports them as a single
     * Boost assertion (see ZETA_CHECK_REPORT). Per-element BOOST_CHECKs log one line
     * each when passing assertions are logged (--log_level=all, as IDE test runners
     * often do), which turns exhaustive tests into hundreds of thousands of lines.
     * Failures keep the first few examples with their index and values.
     */
    class mismatch_report {
    public:
        explicit mismatch_report(std::string subject, const std::size_t max_examples = 5)
            : subject_(std::move(subject)), max_examples_(max_examples)
        {
        }

        /** Records `ok`; on failure, calls `describe()` for the message while under the example cap. */
        template <typename Describe>
        bool expect(const bool ok, Describe&& describe)
        {
            ++checks_;
            if (ok) {
                return true;
            }
            if (failures_++ < max_examples_) {
                details_ << "\n  " << describe();
            }
            return false;
        }

        template <typename Actual, typename Expected>
        bool expect_equal(const Actual& actual, const Expected& expected, const std::string_view label, const std::size_t index)
        {
            return expect(actual == expected, [&] {
                std::ostringstream out;
                out << std::setprecision(17) << label << '[' << index << "]: " << actual << " != " << expected;
                return out.str();
            });
        }

        /** Absolute tolerance. */
        bool expect_small(const double difference, const double tolerance, const std::string_view label, const std::size_t index)
        {
            return expect(std::abs(difference) <= tolerance, [&] {
                std::ostringstream out;
                out << std::setprecision(17) << label << '[' << index << "]: difference " << difference
                    << " exceeds " << tolerance;
                return out.str();
            });
        }

        /** Relative tolerance in percent, with the same strong rule as BOOST_CHECK_CLOSE. */
        bool expect_close(
            const double actual,
            const double expected,
            const double percent,
            const std::string_view label,
            const std::size_t index)
        {
            const auto difference = std::abs(actual - expected);
            const auto fraction = percent / 100.0;
            const bool ok = difference == 0.0
                || (difference <= fraction * std::abs(actual) && difference <= fraction * std::abs(expected));
            return expect(ok, [&] {
                std::ostringstream out;
                out << std::setprecision(17) << label << '[' << index << "]: " << actual << " vs " << expected
                    << " differs by more than " << percent << '%';
                return out.str();
            });
        }

        [[nodiscard]] bool passed() const noexcept
        {
            return failures_ == 0;
        }

        [[nodiscard]] std::size_t checks() const noexcept
        {
            return checks_;
        }

        [[nodiscard]] std::string summary() const
        {
            std::ostringstream out;
            out << subject_ << ": ";
            if (failures_ == 0) {
                out << checks_ << " checks passed";
            } else {
                out << failures_ << " of " << checks_ << " checks failed" << details_.str();
                if (failures_ > max_examples_) {
                    out << "\n  ... " << (failures_ - max_examples_) << " more";
                }
            }
            return out.str();
        }

    private:
        std::string subject_;
        std::size_t max_examples_;
        std::size_t checks_ = 0;
        std::size_t failures_ = 0;
        std::ostringstream details_;
    };

}

/** One Boost assertion for a whole mismatch_report, reported at the call site. */
#define ZETA_CHECK_REPORT(report) BOOST_CHECK_MESSAGE((report).passed(), (report).summary())
