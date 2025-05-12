cmake_minimum_required(VERSION 3.10)
project(GPA C)

set (PROJECT_VERSION 1.6.0)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

option(BUILD_SHARED "Build shared library (DLL/SO)" ON)


set(GPA_SOURCES src/gpa.c)
set(GPA_HEADERS src/gpa.h)


if(BUILD_SHARED)
    add_library(gpa SHARED ${GPA_SOURCES} ${GPA_HEADERS})
    target_compile_definitions(gpa PRIVATE GPA_BUILD_DLL)
else()
    add_library(gpa STATIC ${GPA_SOURCES} ${GPA_HEADERS})
endif()


target_include_directories(gpa 
    PUBLIC 
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include/gpa>
)


find_package(ZLIB REQUIRED)
target_link_libraries(gpa PRIVATE ZLIB::ZLIB)


option(ENABLE_LZMA "Enable LZMA compression support" ON)
if(ENABLE_LZMA)
    find_package(LibLZMA)
    if(LibLZMA_FOUND)
        message(STATUS "LZMA found: enabling LZMA support")
        target_compile_definitions(gpa PRIVATE -DGPA_WITH_LZMA)
        target_link_libraries(gpa PRIVATE LibLZMA::LibLZMA)
    else()
        message(FATAL_ERROR "LZMA requested but not found")
    endif()
endif()

option(ENABLE_ZSTD "Enable ZSTD compression support" ON)
if(ENABLE_ZSTD)
    find_package(zstd QUIET)
    if(zstd_FOUND)
        message(STATUS "zstd found: enabling zstd support")
        message(STATUS "zstd version: ${zstd_VERSION}")
        
        get_target_property(ZSTD_SHARED_LOCATION zstd::libzstd_shared IMPORTED_LOCATION_RELEASE)
        get_target_property(ZSTD_STATIC_LOCATION zstd::libzstd_static IMPORTED_LOCATION_RELEASE)
        
        message(STATUS "zstd shared library: ${ZSTD_SHARED_LOCATION}")
        message(STATUS "zstd static library: ${ZSTD_STATIC_LOCATION}")
        
        target_compile_definitions(gpa PRIVATE -DGPA_WITH_ZSTD)
        if(BUILD_SHARED)
            target_link_libraries(gpa PRIVATE zstd::libzstd_shared)
        else()
            target_link_libraries(gpa PRIVATE zstd::libzstd_static)
        endif()
    else()
        message(FATAL_ERROR "ZSTD requested but not found")
    endif()
endif()

option(ENABLE_LZ4 "Enable LZ4 compression support" ON)
if(ENABLE_LZ4)
    find_package(LZ4 QUIET)
    if(LZ4_FOUND)
        message(STATUS "LZ4 found: enabling LZ4 support")
        message(STATUS "LZ4 version: ${LZ4_VERSION}")
        
        get_target_property(LZ4_SHARED_LOCATION LZ4::lz4_shared IMPORTED_LOCATION_RELEASE)
        get_target_property(LZ4_STATIC_LOCATION LZ4::lz4_static IMPORTED_LOCATION_RELEASE)
        
        message(STATUS "LZ4 shared library: ${LZ4_SHARED_LOCATION}")
        message(STATUS "LZ4 static library: ${LZ4_STATIC_LOCATION}")
        
        target_compile_definitions(gpa PRIVATE -DGPA_WITH_LZ4)
        target_link_libraries(gpa PRIVATE LZ4::lz4)
    else()
        message(FATAL_ERROR "LZ4 requested but not found")
    endif()
endif()

option(ENABLE_BZ2 "Enable BZip2 compression support" ON)
if(ENABLE_BZ2)
    find_package(BZip2 QUIET)
    if(BZIP2_FOUND)
        message(STATUS "BZip2 found: enabling BZip2 support")
        message(STATUS "BZip2 version: ${BZIP2_VERSION_STRING}")
        message(STATUS "BZip2 include directory: ${BZIP2_INCLUDE_DIR}")
        message(STATUS "BZip2 library: ${BZIP2_LIBRARIES}")
        
        target_compile_definitions(gpa PRIVATE -DGPA_WITH_BZ2)
        target_include_directories(gpa PRIVATE ${BZIP2_INCLUDE_DIR})
        target_link_libraries(gpa PRIVATE bz2_static)
    else()
        message(FATAL_ERROR "BZ2 requested but not found")
    endif()
endif()


add_executable(gpa_tool gpa_tool/gpa_tool.c)
target_link_libraries(gpa_tool PRIVATE gpa)


option(BUILD_EXAMPLES "Build example applications" ON)
if(BUILD_EXAMPLES)
    add_executable(gpa_example examples/example.c)
    target_link_libraries(gpa_example PRIVATE gpa)
endif()


set_target_properties(gpa gpa_tool PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)


if(BUILD_EXAMPLES)
    set_target_properties(gpa_example PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    )
endif()


if(WIN32)
    set_target_properties(gpa PROPERTIES 
        OUTPUT_NAME "gpa"
        PREFIX "")
else()
    set_target_properties(gpa PROPERTIES 
        OUTPUT_NAME "gpa")
endif()


include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

# Настройка версии для config-файла
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/cmake/gpa-config-version.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

# Конфигурирование config-файла из шаблона
configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/gpa-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake/gpa-config.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/gpa
    PATH_VARS CMAKE_INSTALL_INCLUDEDIR
)

install(TARGETS gpa
    EXPORT gpa-targets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}     
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}      
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}      
)

install(FILES ${GPA_HEADERS} 
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gpa
)

install(TARGETS gpa_tool 
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(EXPORT gpa-targets
    FILE gpa-targets.cmake
    NAMESPACE GPA::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/gpa
)

include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/gpa-config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/gpa-config.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/gpa
)

write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/gpa-config-version.cmake
    VERSION 1.6.0  
    COMPATIBILITY SameMajorVersion
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/gpa-config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/gpa-config-version.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/gpa
)