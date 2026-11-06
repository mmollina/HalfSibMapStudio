@echo off
REM HalfSibMap Studio launcher for Windows. Double-click or run from a terminal.
setlocal
cd /d "%~dp0"
where Rscript >nul 2>nul
if errorlevel 1 (
  if exist "%ProgramFiles%\R" (
    for /f "delims=" %%d in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
      set "RSCRIPT=%ProgramFiles%\R\%%d\bin\Rscript.exe"
      goto :found
    )
  )
  echo Rscript was not found. Install R 4.2 or newer from https://cran.r-project.org/bin/windows/base/
  echo and make sure its bin folder is on PATH, then run this file again.
  pause
  exit /b 1
) else (
  set "RSCRIPT=Rscript"
)
:found
"%RSCRIPT%" scripts\run_app.R %*
if errorlevel 1 (
  echo.
  echo The app stopped with an error. If packages are missing, run:  Rscript scripts\bootstrap.R
  pause
)
endlocal
