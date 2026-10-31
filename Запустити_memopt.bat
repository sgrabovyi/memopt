@echo off
rem Opens the memopt app in the browser. Keep this window open while you work; close it to stop the app.
chcp 65001 >nul
cd /d "%~dp0"
call "%~dp0_find_r.cmd" || exit /b 1
echo Starting memopt. The app opens in your browser in a few seconds.
"%RSCRIPT%" -e "shiny::runApp('app', launch.browser = TRUE)"
pause
