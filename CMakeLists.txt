add_library(lib_tl OBJECT)
init_target(lib_tl)

get_filename_component(src_loc . REALPATH)

set(lib_tl_sources
PRIVATE
    tl/tl_basic_types.cpp
    tl/tl_basic_types.h
    tl/tl_boxed.h
    tl/tl_type_owner.h

    tl/generate_tl.py
)
nice_target_sources(lib_tl ${src_loc} "${lib_tl_sources}")

target_include_directories(lib_tl
PUBLIC
    ${src_loc}
)

target_link_libraries(lib_tl
PUBLIC
    lib_base
)
