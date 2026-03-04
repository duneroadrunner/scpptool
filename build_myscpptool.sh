#!/bin/bash

originaldir=$PWD

# obtaining the directory of this script file
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd $SCRIPT_DIR

#This script checks whether it's running on an x86_64 architecture with Linux as its OS
arch=$(uname -m) # Get system architecture using 'uname -m' command

if [ "$arch" == "x86_64" ] || [ -f /etc/os-release ]; then # If architecture is x86_64 and OS is Linux, print a message

	echo "This script is running on an x86_64 Linux platform."

	llvmsubdir="LLVM-21.1.8-Linux-X64"
	llvmdir=$PWD/$llvmsubdir
	if [ -d $llvmdir ]; then
	    echo "The directory $llvmdir already exists."
	else
	    echo "The directory $llvmdir does not seem to exist."
	    echo "Downloading llvm prebuilt binares..."
	    wget "https://github.com/llvm/llvm-project/releases/download/llvmorg-21.1.8/LLVM-21.1.8-Linux-X64.tar.xz"
	    tar -xvf LLVM-21.1.8-Linux-X64.tar.xz
	fi
else

	echo "This script is not running on an x86_64 Linux platform. Architecture: $arch"  # Print the detected architecture if it's not x86_64 or OS isn't Linux
	echo ""
	echo "Please download the LLVM-21.1.8 pre-built binaries tar file for your platform (located here: https://github.com/llvm/llvm-project/releases/tag/llvmorg-21.1.8 or in a sibling path) and extract the contents. "

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

srcclangincludedir=$llvmdir/lib/clang/21/include

mkdir ../lib
mkdir ../lib/clang
mkdir ../lib/clang/21
cp -n -r $srcclangincludedir ../lib/clang/21

echo "
Build complete. 


Once you've verified that scpptool is working properly, you may then delete the clang+llvm-21.1.8 tar file that was downloaded manually or by this build script, and the directory extracted from it.

Note that scpptool uses a clang include directory located in the relative path '../lib/clang/21' created by this script. So copying or moving the scpptool executable may also require moving that include directory so that the relative path remains the same.


The usage syntax is as follows:

{scpptool src directory}/scpptool {source filename(s)} -- {compiler options}

where the {scpptool src directory} is $srcdir

So for example:

$srcdir/scpptool hello_world.cpp -- -I./msetl -std=c++17

"

cd $originaldir

exit 0

