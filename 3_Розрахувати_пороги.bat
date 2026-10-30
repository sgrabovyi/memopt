@echo off
rem Step 3: runs the threshold calculation (Rscript run_optimizer.R). Results go to the output folder.
chcp 65001 >nul
cd /d "%~dp0"
call "%~dp0_find_r.cmd" || exit /b 1
echo Calculation started %date% %time%. It takes 3 to 10 minutes, keep this window open.
"%RSCRIPT%" "%~dp0run_optimizer.R" "%~dp0config.json"
if errorlevel 1 (
  echo.
  echo The calculation stopped with an error. Read the message above. If it says "there is no package called",
  echo run 2_Vstanovyty_pakety.bat first.
) else (
  echo.
  echo Done %date% %time%. Results: output\memopt_results.xlsx and output\memopt_thresholds.csv
)
echo.
pause
