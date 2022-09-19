#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling branch
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/branch.o  -x assembler /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/branch.s
if [ $? != 0 ]; then DoExitAsm branch; fi
rm /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/branch.s
echo Assembling repo
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/repo.o  -x assembler /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/repo.s
if [ $? != 0 ]; then DoExitAsm repo; fi
rm /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/repo.s
echo Assembling config
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/config.o  -x assembler /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/config.s
if [ $? != 0 ]; then DoExitAsm config; fi
rm /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/config.s
echo Assembling gitmanager
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/gitmanager.o  -x assembler /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/gitmanager.s
if [ $? != 0 ]; then DoExitAsm gitmanager; fi
rm /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/gitmanager.s
echo Assembling main
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/main.o  -x assembler /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/main.s
if [ $? != 0 ]; then DoExitAsm main; fi
rm /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/main.s
echo Assembling gitwhat
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/gitwhat.o  -x assembler /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/gitwhat.s
if [ $? != 0 ]; then DoExitAsm gitwhat; fi
rm /Users/cloudsoft/Code/git_what/lib/x86_64-darwin/gitwhat.s
echo Linking /Users/cloudsoft/Code/git_what/gitwhat
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa      -order_file /Users/cloudsoft/Code/git_what/symbol_order.fpc -multiply_defined suppress -L. -o /Users/cloudsoft/Code/git_what/gitwhat `cat /Users/cloudsoft/Code/git_what/link66222.res` -filelist /Users/cloudsoft/Code/git_what/linkfiles66222.res
if [ $? != 0 ]; then DoExitLink /Users/cloudsoft/Code/git_what/gitwhat; fi
IFS=$OFS
