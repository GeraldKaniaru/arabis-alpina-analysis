@echo off
setlocal
set "ARABIS_PYTHON=%USERPROFILE%\.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe"
if exist "%ARABIS_PYTHON%" (
  "%ARABIS_PYTHON%" "%~dp0publish_updates.py" --publish %*
  exit /b
)
where py >nul 2>nul
if not errorlevel 1 (
  py -3 "%~dp0publish_updates.py" --publish %*
  exit /b
)
python "%~dp0publish_updates.py" --publish %*
