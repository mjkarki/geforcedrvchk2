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
- a simple unit test macro and unit tests for pattern matching (macros both in define-syntax and define-macro format)

## Usage

Load the project from GitHub, use make.bat with fully functional [Gambit Scheme](https://github.com/gambit/gambit) + C compiler installation to create an executable and put a link to *shell:startup* to execute the application automatically every time Windows starts. Alternatively, if you do not want to compile the application yourself, you can download a pre-compiled binary from [here](https://github.com/mattijk/geforcedrvchk2/releases).

Note that because this project uses embedded C code, the source code has to be compiled to native Windows binary. Running this code under Gambit gsi interpreter will not work.

## Why another version?

This is the second published version in GitHub. A graphics driver checker tool is my go-to project to practice with different development tools, libraries and compilers. It's a versatile project to test and learn different aspects about the language, tools and environment. The previous version written with Racket is still available, but it's not guaranteed that I'll update it anymore now that I've managed to set up stable Gambit environment. 

## About the files in this project

This project contains more than is necessary for such a simple tool as GeForceDrvChk. By accident this project contains all my helper functions and tools that I have written for Gambit Scheme. I might consider moving these tools to a separate GitHub project at some point. But for now, everything is hosted under this project.

Libraries:

- *misctools.scm* small helper function, which I need time to time. log10, list sorting...
- *match.scm* simple regular expression inspired library for text matching
- *unittest.scm* small unit test library written with define-syntax, requires the syntax-case library (command line option -:s)
- *unittest2.scm* small unit test library written with define-macro, which works without including the syntax-case library
- *nativewin.scm* native Win32 funtions (MessageBoxA and simple SSL client support)
