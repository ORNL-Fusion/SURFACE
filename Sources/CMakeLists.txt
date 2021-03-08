target_sources(xsurface
               PRIVATE
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/surface_commandline_parser.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/surface_context.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/surface.f>
)
