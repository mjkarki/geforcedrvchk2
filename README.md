# geforcedrvchk2
GeForceDrvChk is a small no-nonsense application for automatically checking Geforce driver updates under Windows.

## Introduction
This little piece of code checks the GeForce website for new driver versions. It only checks for the International GeForce GTX series driver for 64-bit Windows desktop (so, no laptop version of the driver).

The main point of the application is to prove myself that I'm able to implement everything required using only Gambit Scheme:

- calling a console application and catching the output
- creating my own (very simple) pattern matching library
- fetching a page from a WWW server over SSL using native WINHTTP API
- embedding all necessary Win32 API C code to the scheme source code
- compiling single statically linked binary without any dependencies (except of course Windows APIs)

## Usage

Load the project from GitHub, use make.bat with MinGW64 and Gambit Scheme to create an executable and put a link to *shell:startup* to execute the application automatically every time Windows starts. Alternatively, if you do not want to compile the application yourself, you can download a pre-compiled binary from [here](https://github.com/mattijk/geforcedrvchk2/releases).

Note that because this project uses embedded C code, the source code has to be compiled to native Windows binary. Running this code under Gambit gsi interpreter will not work.

## Why another version?

This is the second published version in GitHub. A graphics driver checker tool is my go-to project to practice with different development tools, libraries and compilers. It's a versatile project to test and learn different aspects about the language, tools and environment. The previous version written with Racket is still available, but it's not guaranteed that I'll update it anymore now that I've managed to set up stable Gambit environment. 
