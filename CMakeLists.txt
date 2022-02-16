cmake_minimum_required (VERSION 3.14)

project (surface C Fortran)

add_executable (xsurface)

target_link_libraries (xsurface PUBLIC bmw)

add_subdirectory (Sources)
