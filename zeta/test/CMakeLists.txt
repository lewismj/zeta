# Specify dependencies.
if(POLICY CMP0167)
    # Prefer Boost's package config (vcpkg-provided on Windows).
    cmake_policy(SET CMP0167 NEW)
endif()

if(WIN32)
    set(_zeta_boost_triplet "${VCPKG_TARGET_TRIPLET}")
    if(NOT _zeta_boost_triplet)
        set(_zeta_boost_triplet x64-windows)
    endif()

    set(_zeta_boost_prefix_candidates "")
    if(DEFINED VCPKG_INSTALLED_DIR AND VCPKG_INSTALLED_DIR)
        list(APPEND _zeta_boost_prefix_candidates "${VCPKG_INSTALLED_DIR}/${_zeta_boost_triplet}")
    endif()
    if(DEFINED ENV{VCPKG_ROOT})
        list(APPEND _zeta_boost_prefix_candidates "$ENV{VCPKG_ROOT}/installed/${_zeta_boost_triplet}")
    endif()
    if(EXISTS "${CMAKE_SOURCE_DIR}/../vcpkg/installed/${_zeta_boost_triplet}")
        list(APPEND _zeta_boost_prefix_candidates "${CMAKE_SOURCE_DIR}/../vcpkg/installed/${_zeta_boost_triplet}")
    endif()

    foreach(_zeta_boost_prefix IN LISTS _zeta_boost_prefix_candidates)
        if(EXISTS "${_zeta_boost_prefix}/share/boost/BoostConfig.cmake")
            list(PREPEND CMAKE_PREFIX_PATH "${_zeta_boost_prefix}")
            set(Boost_DIR "${_zeta_boost_prefix}/share/boost" CACHE PATH "Boost config path")
            break()
        endif()
    endforeach()

    if((NOT DEFINED BOOST_ROOT OR BOOST_ROOT STREQUAL "") AND DEFINED Boost_DIR AND Boost_DIR)
        get_filename_component(_zeta_boost_share "${Boost_DIR}" DIRECTORY)
        if(_zeta_boost_share)
            get_filename_component(BOOST_ROOT "${_zeta_boost_share}" DIRECTORY)
        endif()
    endif()
endif()

find_package(Boost 1.88 REQUIRED COMPONENTS unit_test_framework)
if(NOT Boost_FOUND)
    message(FATAL_ERROR "Boost not found.")
endif()

add_executable(zeta_tests
    src/test_main.cpp
    src/test_repr.cpp
    src/test_holdem.cpp
)
target_link_libraries(zeta_tests PRIVATE zeta_core zeta-holdem Boost::unit_test_framework)
target_compile_definitions(zeta_tests PRIVATE BOOST_TEST_DYN_LINK)
target_compile_features(zeta_tests PRIVATE cxx_std_23)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta_tests PRIVATE stdc++ m)
endif()

function(zeta_register_boost_test_cases label)
    foreach(test_case IN LISTS ARGN)
        add_test(
            NAME "${label}.${test_case}"
            COMMAND $<TARGET_FILE:zeta_tests> --run_test=${test_case}
        )
        set_tests_properties("${label}.${test_case}" PROPERTIES
            LABELS "${label}"
            WORKING_DIRECTORY $<TARGET_FILE_DIR:zeta_tests>
        )
    endforeach()
endfunction()

set(_zeta_core_test_cases    default_deck_suit_masks_have_13_cards_each
    enum_suit_mask_api_matches_index_api
    num_cards_default_deck_is_52
    rank_mask_default_deck_is_13_low_bits
    suit_masks_cover_correct_bit_ranges
    ops_suit_masks_constexpr_matches_make_suit_masks
    lsb_returns_lowest_set_bit
    pop_lsb_removes_lowest_set_bit_and_returns_it
    lsb_index_returns_position_of_lowest_set_bit
    msb_index_returns_position_of_highest_set_bit
    is_empty_true_only_for_zero_mask
    nth_set_bit_selects_correct_bit
    cards_in_suit_isolates_one_suit
    has_suit_detects_presence_of_suit
    suit_ranks_and_ranks_to_cards_roundtrip
    highest_in_suit_returns_top_card_or_zero
    lowest_in_suit_returns_bottom_card_or_zero
    make_suit_masks_size_matches_num_suits
    jass_deck_num_cards_is_36
    jass_rank_mask_is_9_low_bits
    jass_suit_masks_have_9_cards_each_no_overlap
    jass_suit_masks_cover_correct_bit_ranges
    jass_suit_ranks_and_ranks_to_cards_roundtrip
    shuffled_contains_each_card_exactly_once
    swiss_aliases_map_to_correct_suits
)
zeta_register_boost_test_cases(core ${_zeta_core_test_cases})

set(_zeta_holdem_test_cases    holdem_detects_straight_flush
    holdem_detects_quads
    holdem_precomputed_masks_match_card_mask_path
    holdem_ranking_ordering_is_monotonic
    holdem_board_tracks_streets_and_mutations
    holdem_combination_masks_do_not_overlap_sample_boards
    non_flush_quinary_table_integrity
    holdem_combination_masks_are_complete_and_ordered
    non_flush_rank_classes_exhaustively_match_dense_table_and_evaluator
    flush_rank_patterns_match_flush_table_and_evaluator
    holdem_evaluator_release_perf_smoke
)
zeta_register_boost_test_cases(holdem ${_zeta_holdem_test_cases})

add_custom_target(zeta_test
    COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIG> --output-on-failure --test-dir ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS zeta_tests
    USES_TERMINAL
    COMMENT "Running all tests in zeta/test"
)

if(TARGET zeta-check-holdem-tables)
    add_dependencies(zeta_test zeta-check-holdem-tables)
endif()

if(WIN32)
    if(NOT DEFINED BOOST_ROOT)
        if(DEFINED ENV{BOOST_ROOT})
            set(BOOST_ROOT "$ENV{BOOST_ROOT}")
        elseif(Boost_INCLUDE_DIRS)
            list(GET Boost_INCLUDE_DIRS 0 _boost_inc)
            get_filename_component(BOOST_ROOT "${_boost_inc}" DIRECTORY)
        elseif(DEFINED Boost_DIR AND Boost_DIR)
            get_filename_component(_zeta_boost_share "${Boost_DIR}" DIRECTORY)
            get_filename_component(BOOST_ROOT "${_zeta_boost_share}" DIRECTORY)
        else()
            message(FATAL_ERROR "BOOST_ROOT not set.")
        endif()
    endif()

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(BOOST_DLL_PATH "${BOOST_ROOT}/debug/bin")
    else()
        set(BOOST_DLL_PATH "${BOOST_ROOT}/bin")
    endif()
    message(STATUS "Looking for Boost DLLs in: ${BOOST_DLL_PATH}")
    file(GLOB BOOST_TEST_DLL "${BOOST_DLL_PATH}/boost_unit_test_framework*.dll")

    foreach(dll IN LISTS BOOST_TEST_DLL)
        add_custom_command(TARGET zeta_tests POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${dll}"
            $<TARGET_FILE_DIR:zeta_tests>
            COMMENT "Copying Boost.Test DLL to output directory"
        )
    endforeach()
endif()