cmake_minimum_required(VERSION 3.10)
project(GPA C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

option(BUILD_SHARED "Build shared library (DLL/SO)" ON)

# Поиск обязательной зависимости ZLIB
find_package(ZLIB REQUIRED)

# Поиск опциональных зависимостей LZMA и Zstd
find_package(LibLZMA)
find_package(zstd REQUIRED)
message(STATUS "zstd found: ${zstd_FOUND}")
message(STATUS "zstd include dirs: ${zstd_INCLUDE_DIRS}")
message(STATUS "zstd libraries: ${zstd_LIBRARIES}")

# Определение макросов для поддержки LZMA и Zstd
if(LibLZMA_FOUND)
    add_definitions(-DGPA_WITH_LZMA)
endif()
if(Zstd_FOUND)
    add_definitions(-DGPA_WITH_ZSTD)
endif()

set(SOURCES gpa.c)
set(HEADERS gpa.h)

if(BUILD_SHARED)
    add_library(gpa SHARED ${SOURCES} ${HEADERS})
    target_compile_definitions(gpa PRIVATE GPA_BUILD_DLL)
else()
    add_library(gpa STATIC ${SOURCES} ${HEADERS})
endif()

# Линковка библиотек
target_link_libraries(gpa PRIVATE ZLIB::ZLIB)
if(LibLZMA_FOUND)
    target_link_libraries(gpa PRIVATE LibLZMA::LibLZMA)
endif()
if(Zstd_FOUND)
    target_link_libraries(gpa PRIVATE Zstd::Zstd)
endif()

target_include_directories(gpa PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(gpa_tool gpa_tool.c)
target_link_libraries(gpa_tool PRIVATE gpa)

set_target_properties(gpa gpa_tool PROPERTIES
                      ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
                      LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
                      RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
                     )

if(WIN32)
    set_target_properties(gpa PROPERTIES OUTPUT_NAME "gpa")
else()
    set_target_properties(gpa PROPERTIES OUTPUT_NAME "gpa")
endif()