@echo off
rem Step 2: installs the R packages that memopt needs (internet connection required, about 5 minutes).
chcp 65001 >nul
cd /d "%~dp0"
call "%~dp0_find_r.cmd" || exit /b 1
"%RSCRIPT%" "%~dp0install_packages.R"
echo.
pause
