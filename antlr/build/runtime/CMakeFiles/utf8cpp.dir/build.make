# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/glasser/toys/prss/antlr

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/glasser/toys/prss/antlr/build

# Utility rule file for utf8cpp.

# Include the progress variables for this target.
include runtime/CMakeFiles/utf8cpp.dir/progress.make

runtime/CMakeFiles/utf8cpp: runtime/CMakeFiles/utf8cpp-complete


runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-install
runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-mkdir
runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download
runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-patch
runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure
runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-build
runtime/CMakeFiles/utf8cpp-complete: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-install
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Completed 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/CMakeFiles
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/CMakeFiles/utf8cpp-complete
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-done

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-install: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-build
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Performing install step for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build && $(MAKE) install
	cd /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-install

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-mkdir:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Creating directories for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/thirdparty/utfcpp
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/tmp
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E make_directory /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-mkdir

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-gitinfo.txt
runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-mkdir
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Performing download step (git clone) for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime/thirdparty && /usr/bin/cmake -P /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/tmp/utf8cpp-gitclone.cmake
	cd /home/glasser/toys/prss/antlr/build/runtime/thirdparty && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-patch: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "No patch step for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E echo_append
	cd /home/glasser/toys/prss/antlr/build/runtime && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-patch

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure: runtime/utf8cpp-prefix/tmp/utf8cpp-cfgcmd.txt
runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-skip-update
runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-patch
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Performing configure step for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build && /usr/bin/cmake -DCMAKE_INSTALL_PREFIX=/home/glasser/toys/prss/antlr/build/runtime/thirdparty/utfcpp/install -DUTF8_TESTS=off -DUTF8_SAMPLES=off "-GUnix Makefiles" /home/glasser/toys/prss/antlr/build/runtime/thirdparty/utfcpp
	cd /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-build: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Performing build step for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build && $(MAKE)
	cd /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-build && /usr/bin/cmake -E touch /home/glasser/toys/prss/antlr/build/runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-build

runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-skip-update: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/home/glasser/toys/prss/antlr/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Skipping update step for 'utf8cpp'"
	cd /home/glasser/toys/prss/antlr/build/runtime/thirdparty/utfcpp && /usr/bin/cmake -E echo_append

utf8cpp: runtime/CMakeFiles/utf8cpp
utf8cpp: runtime/CMakeFiles/utf8cpp-complete
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-install
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-mkdir
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-download
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-patch
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-configure
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-build
utf8cpp: runtime/utf8cpp-prefix/src/utf8cpp-stamp/utf8cpp-skip-update
utf8cpp: runtime/CMakeFiles/utf8cpp.dir/build.make

.PHONY : utf8cpp

# Rule to build all files generated by this target.
runtime/CMakeFiles/utf8cpp.dir/build: utf8cpp

.PHONY : runtime/CMakeFiles/utf8cpp.dir/build

runtime/CMakeFiles/utf8cpp.dir/clean:
	cd /home/glasser/toys/prss/antlr/build/runtime && $(CMAKE_COMMAND) -P CMakeFiles/utf8cpp.dir/cmake_clean.cmake
.PHONY : runtime/CMakeFiles/utf8cpp.dir/clean

runtime/CMakeFiles/utf8cpp.dir/depend:
	cd /home/glasser/toys/prss/antlr/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/glasser/toys/prss/antlr /home/glasser/toys/prss/antlr/runtime /home/glasser/toys/prss/antlr/build /home/glasser/toys/prss/antlr/build/runtime /home/glasser/toys/prss/antlr/build/runtime/CMakeFiles/utf8cpp.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : runtime/CMakeFiles/utf8cpp.dir/depend

