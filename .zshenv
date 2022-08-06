# usr local binaries installed by e.g. pip
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# make rust-tools like cargo, rustup, rustc available
if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# root path of boost sources
# used by e.g. b2 and cmake
export BOOST_ROOT="$HOME/src/boost"
# path to the boost-build.jam for finding the jam-code
# that e.g. support integration of boost libs
export BOOST_BUILD_PATH="$HOME/src/b2"
