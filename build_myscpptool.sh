#!/bin/bash

currentdir1=$PWD

# obtaining the directory of this script file
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd $SCRIPT_DIR

#This script checks whether it's running on an x86_64 architecture with Ubuntu as its OS
arch=$(uname -m) # Get system architecture using 'uname -m' command

os=$(cat /etc/*release | grep DISTRIB_ID=Ubuntu) # Check if the current distribution is Ubuntu

if [ "$arch" == "x86_64" ] && [ ! -z $os ]; then # If architecture is x86_64 and OS is Ubuntu, print a message

	echo "This script is running on an x86_64 Ubuntu platform."

	llvmsubdir="clang+llvm-15.0.6-x86_64-linux-gnu-ubuntu-18.04"
	llvmdir=$PWD/$llvmsubdir
	if [ -d $llvmdir ]; then
	    echo "The directory $llvmdir already exists."
	else
	    echo "The directory $llvmdir does not seem to exist."
	    echo "Downloading llvm prebuilt binares..."
	    wget "https://github.com/llvm/llvm-project/releases/download/llvmorg-15.0.6/clang+llvm-15.0.6-x86_64-linux-gnu-ubuntu-18.04.tar.xz"
	    tar -xvf clang+llvm-15.0.6-x86_64-linux-gnu-ubuntu-18.04.tar.xz
	fi
else

	echo "This script is not running on an x86_64 Ubuntu platform. Architecture: $arch"  # Print the detected architecture if it's not x86_64 or OS isn't Ubuntu
	echo ""
	echo "Please download the clang+llvm-15.0.6 pre-built binaries tar file for your platform (located here: https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.6 or in a sibling directory) and extract the contents. "

	read -p "Then enter the full path of the extracted directory: " dir_path

	dir_path=${dir_path//[\"\']} # remove any enclosing quotes

	if [[ ! -e $dir_path ]] || [[ ! -r $dir_path ]]; then
	    echo "The specified path doesn't exist or isn't accessible." >&2
	    exit 1
	fi

	llvmdir=$dir_path
fi

srcdir=$PWD/src

cd $srcdir

echo "Compiling scpptool... "

make LLVM_CONF=$llvmdir/bin/llvm-config BUILD_MODE=DEBUG

scpptoolfilename=scpptool    # The filename we're checking for existence.
if [ -f "$scpptoolfilename" ]; then  # Test if $FILE exists and is a regular file.
    echo "File '$scpptoolfilename' found."
else
    echo "'$scpptoolfilename' not found. The build seems to have failed."
    exit -1
fi

clangincludedir=$llvmdir/lib/clang/15.0.6/include

# Here we're making a shell script to run scpptool
echo '#!/bin/bash

# obtaining the directory of this script file, which we will assume also contains the scpptool executable
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# executing scpptool with the given arguments and an additional include argument for the standard headers needed by scpptool
$SCRIPT_DIR/scpptool $@ -I' $clangincludedir '
' > myscpptool.sh

chmod +x myscpptool.sh

echo "
Build complete. 


The usage syntax is as follows:

{scpptool src directory}/myscpptool.sh {source filename(s)} -- {compiler options}

where the {scpptool src directory} is $srcdir

So for example:

$srcdir/myscpptool.sh hello_world.cpp -- -I./msetl -std=c++17


(Note that the path to the clang include directory used is hard-coded into myscpptool.sh, so that directory cannot be moved without updating myscpptool.sh accordingly.)

"

cd $currentdir1

exit 0

