#include <chrono>
#include <charconv>
#include <cstdlib>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <string>
#include <string_view>

#include "cli/solve_cli.h"

namespace {

    void print_usage()
    {
        std::cerr
            << "Usage:\n"
            << "  zeta-solve solve --spot <spot.json> --iterations <n> --output <solution.json>\n"
            << "  zeta-solve validate <solution.json>\n"
            << "  zeta-solve dump <solution.json>\n";
    }

    [[nodiscard]] int fail(const zeta::holdem::cli::cli_error& error)
    {
        std::cerr << error.message << "\n";
        return 2;
    }

}

int main(int argc, char** argv)
{
    using namespace zeta::holdem::cli;
    if (argc < 2) {
        print_usage();
        return 1;
    }

    const std::string_view command{argv[1]};
    if (command == "solve") {
        std::filesystem::path spot_path;
        std::filesystem::path output_path;
        uint64_t iterations = 1000;

        for (int i = 2; i < argc; ++i) {
            const std::string_view arg{argv[i]};
            if (arg == "--spot" && i + 1 < argc) {
                spot_path = argv[++i];
            } else if (arg == "--iterations" && i + 1 < argc) {
                const std::string_view value{argv[++i]};
                const auto begin = value.data();
                const auto end = begin + value.size();
                auto [ptr, ec] = std::from_chars(begin, end, iterations);
                if (ec != std::errc{} || ptr != end) {
                    std::cerr << "Invalid --iterations value.\n";
                    return 1;
                }
            } else if (arg == "--output" && i + 1 < argc) {
                output_path = argv[++i];
            } else {
                std::cerr << "Unknown solve argument: " << arg << "\n";
                return 1;
            }
        }

        if (spot_path.empty() || output_path.empty()) {
            print_usage();
            return 1;
        }

        auto spot_text = read_file_text(spot_path);
        if (!spot_text) {
            return fail(spot_text.error());
        }
        auto spot = parse_spot_json(*spot_text);
        if (!spot) {
            return fail(spot.error());
        }

        solve_runtime_options runtime{};
        if (const char* revision = std::getenv("ZETA_GIT_REVISION")) {
            runtime.git_revision = revision;
        }

        const auto total_begin = std::chrono::steady_clock::now();
        auto solved = solve_spot(*spot, iterations, runtime);
        if (!solved) {
            return fail(solved.error());
        }

        const auto serialization_begin = std::chrono::steady_clock::now();
        const auto json = serialize_artifact_json(solved->artifact);
        auto write = write_file_text(output_path, json);
        if (!write) {
            return fail(write.error());
        }
        const auto serialization_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - serialization_begin).count();
        const auto total_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - total_begin).count();

        std::cout
            << "solve:\n"
            << "  graph_build        " << solved->timing.graph_build_ms << "ms\n"
            << "  cfr_iterations     " << solved->timing.cfr_iterations_ms << "ms\n"
            << "  extraction         " << solved->timing.extraction_ms << "ms\n"
            << "  serialization      " << serialization_ms << "ms\n"
            << "\n"
            << "total                " << total_ms << "ms\n";
        return 0;
    }

    if (command == "validate") {
        if (argc != 3) {
            print_usage();
            return 1;
        }
        const auto begin = std::chrono::steady_clock::now();
        auto json = read_file_text(argv[2]);
        if (!json) {
            return fail(json.error());
        }
        auto artifact = parse_artifact_json(*json);
        if (!artifact) {
            return fail(artifact.error());
        }
        auto validation = validate_artifact(*artifact);
        if (!validation) {
            return fail(validation.error());
        }
        const auto elapsed_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - begin).count();
        std::cout
            << "validate:\n"
            << "  structural         " << elapsed_ms << "ms\n"
            << "\n"
            << "total                " << elapsed_ms << "ms\n";
        return 0;
    }

    if (command == "dump") {
        if (argc != 3) {
            print_usage();
            return 1;
        }
        const auto begin = std::chrono::steady_clock::now();
        auto json = read_file_text(argv[2]);
        if (!json) {
            return fail(json.error());
        }
        auto artifact = parse_artifact_json(*json);
        if (!artifact) {
            return fail(artifact.error());
        }
        auto validation = validate_artifact(*artifact);
        if (!validation) {
            return fail(validation.error());
        }
        const auto table = format_dump(*artifact);
        std::cout << table;
        const auto elapsed_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - begin).count();
        std::cout
            << "dump:\n"
            << "  formatting         " << elapsed_ms << "ms\n"
            << "\n"
            << "total                " << elapsed_ms << "ms\n";
        return 0;
    }

    std::cerr << "Unknown subcommand: " << command << "\n";
    print_usage();
    return 1;
}
