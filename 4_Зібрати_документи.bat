@echo off
rem Step 4 (optional): rebuilds all Word documents and workbooks from the results and checks the texts. Needs Python 3.10+ with the packages
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
call "%~dp0_find_r.cmd" || exit /b 1
echo Building all documents and workbooks, then checking the texts
%PY% build_all.py
if errorlevel 1 (
  echo.
  echo Some step failed or the text check found problems. Read the messages above.
) else (
  echo.
  echo Done. Documents are in the docs folder, workbooks in the output folder.
)
pause
