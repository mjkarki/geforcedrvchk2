@echo off

gsc -exe -cc-options "-mwindows -s -static" -ld-options "-lwinhttp" GeForceDrvChk.scm
