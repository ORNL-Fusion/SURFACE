cmake_minimum_required (VERSION 3.14)

project (surface C Fortran)

add_executable (xsurface)
add_dependencies (xsurface bmw)

target_link_libraries (xsurface bmw)
target_include_directories (xsurface PUBLIC $<TARGET_PROPERTY:xbmw,BINARY_DIR>)

add_subdirectory (Sources)
