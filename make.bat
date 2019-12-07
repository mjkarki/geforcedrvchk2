@echo off

if %1x==debugx goto DEBUG
echo Building release version...
gsc -exe -cc-options "-mwindows -s -static" -ld-options "-lwinhttp" GeForceDrvChk.scm
goto END
:DEBUG
echo Building debug version...
gsc -exe -debug -ld-options "-lwinhttp" GeForceDrvChk.scm
:END

