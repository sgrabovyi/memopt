@echo off
rem Step 4 (optional): rebuilds the Word documents from the results workbook. Needs Python 3.10+ with the packages
rem from requirements.txt (pip install -r requirements.txt). Run after step 3.
chcp 65001 >nul
cd /d "%~dp0"
set "PY="
where python >nul 2>nul && set "PY=python"
if not defined PY where py >nul 2>nul && set "PY=py -3"
if not defined PY (
  echo Python was not found. Install it from https://www.python.org/downloads/windows/ with the option
  echo "Add python.exe to PATH", then run: pip install -r requirements.txt
  pause
  exit /b 1
)
%PY% -c "import pandas, openpyxl, docx, matplotlib" 2>nul || (
  echo Installing Python packages from requirements.txt
  %PY% -m pip install -r "%~dp0requirements.txt"
)
echo Examples for the manual (R)
call "%~dp0_find_r.cmd" || exit /b 1
"%RSCRIPT%" "%~dp0R\memopt_examples.R"
echo Parameters document
%PY% build_params_doc.py
echo Manual
%PY% build_manual.py
echo Workbook report
%PY% build_report.py
echo District confirmation rule document
%PY% build_gate_report.py
echo Series for memapp (R)
"%RSCRIPT%" "%~dp0build_memapp_series.R"
echo.
echo Done. Documents are in the docs folder.
pause
