@echo off
rem Finds Rscript.exe and puts its path into the RSCRIPT variable (used by the two .bat files).
rem Order: Windows registry (R-core), then C:\Program Files\R\R-*, then PATH.
set "RSCRIPT="
set "RHOME="
for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul') do set "RHOME=%%b"
if not defined RHOME for /f "tokens=2*" %%a in ('reg query "HKCU\SOFTWARE\R-core\R" /v InstallPath 2^>nul') do set "RHOME=%%b"
if not defined RHOME for /f "tokens=2*" %%a in ('reg query "HKLM\SOFTWARE\WOW6432Node\R-core\R" /v InstallPath 2^>nul') do set "RHOME=%%b"
if defined RHOME if exist "%RHOME%\bin\Rscript.exe" set "RSCRIPT=%RHOME%\bin\Rscript.exe"
if not defined RSCRIPT for /d %%d in ("%ProgramFiles%\R\R-*") do if exist "%%d\bin\Rscript.exe" set "RSCRIPT=%%d\bin\Rscript.exe"
if not defined RSCRIPT for /d %%d in ("%LocalAppData%\Programs\R\R-*") do if exist "%%d\bin\Rscript.exe" set "RSCRIPT=%%d\bin\Rscript.exe"
if not defined RSCRIPT (
  where Rscript.exe >nul 2>nul && set "RSCRIPT=Rscript.exe"
)
if not defined RSCRIPT (
  echo.
  echo R is not installed or was not found.
  echo Install R from https://cran.r-project.org/bin/windows/base/ and run this file again.
  echo.
  pause
  exit /b 1
)
echo Using R: %RSCRIPT%
exit /b 0
