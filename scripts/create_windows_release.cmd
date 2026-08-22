@echo off
setlocal enabledelayedexpansion

:: create_windows_release.cmd <tag>
::
:: Builds zeta-ui-holdem and zeta-solve locally (MSVC Release) and stages a
:: redistributable zip archive under artifacts\:
::
::   artifacts\
::     zeta-holdem-solver-windows-x64-<tag>\
::       install.cmd
::       zeta\
::         <tag>\
::           holdem\
::             bin\   <- exe, dlls, Qt plugins, styles
::           README.md
::           doc\
::     zeta-holdem-solver-windows-x64-<tag>.zip
::
:: Prerequisites: Visual Studio (MSVC), CMake, Ninja, vcpkg at
::   C:\Users\lewis\develop\vcpkg

if "%~1"=="" (
    echo Usage: scripts\create_windows_release.cmd ^<tag^>
    echo Example: scripts\create_windows_release.cmd v0.2.0
    exit /b 1
)

set "TAG=%~1"
for %%i in ("%~dp0..") do set "REPO_ROOT=%%~fi"
set "ARTIFACTS=%REPO_ROOT%\artifacts"
set "BUNDLE_NAME=zeta-holdem-solver-windows-x64-%TAG%"
set "BUNDLE_ROOT=%ARTIFACTS%\%BUNDLE_NAME%"
set "VERSION_DIR=%BUNDLE_ROOT%\zeta\%TAG%"
set "BIN_DIR=%VERSION_DIR%\holdem\bin"
set "BUILD_DIR=%REPO_ROOT%\cmake-build-visual-studio-release"

:: ── Locate vswhere ────────────────────────────────────────────────────────────
set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
if not exist "%VSWHERE%" (
    echo ERROR: vswhere.exe not found. Install Visual Studio installer first.
    exit /b 1
)

:: ── Locate vcvars64.bat ───────────────────────────────────────────────────────
for /f "usebackq delims=" %%i in (`"%VSWHERE%" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
    set "VS_PATH=%%i"
)
if "!VS_PATH!"=="" (
    echo ERROR: No Visual Studio installation with MSVC tools found.
    exit /b 1
)

set "VCVARS=!VS_PATH!\VC\Auxiliary\Build\vcvars64.bat"
if not exist "!VCVARS!" (
    echo ERROR: vcvars64.bat not found at !VCVARS!
    exit /b 1
)

echo Setting up MSVC x64 environment...
call "!VCVARS!" >nul
if errorlevel 1 (
    echo ERROR: Failed to initialise MSVC environment.
    exit /b 1
)

:: ── Configure ────────────────────────────────────────────────────────────────
cd /d "%REPO_ROOT%"
echo Configuring (preset: windows-msvc-release)...
cmake --preset windows-msvc-release ^
    -DZETA_BUILD_TESTS=OFF ^
    -DZETA_BUILD_BENCHMARKS=OFF
if errorlevel 1 (
    echo ERROR: CMake configure step failed.
    exit /b 1
)

:: ── Build ────────────────────────────────────────────────────────────────────
echo Building zeta-ui-holdem and zeta-solve...
cmake --build --preset windows-msvc-release ^
    --target zeta-ui-holdem zeta-solve ^
    --parallel
if errorlevel 1 (
    echo ERROR: CMake build step failed.
    exit /b 1
)

:: ── Validate build outputs ───────────────────────────────────────────────────
if not exist "%BUILD_DIR%\zeta\ui\holdem\zeta-ui-holdem.exe" (
    echo ERROR: zeta-ui-holdem.exe not found in build output.
    exit /b 1
)
if not exist "%BUILD_DIR%\zeta\tools\holdem\zeta-solve.exe" (
    echo ERROR: zeta-solve.exe not found in build output.
    exit /b 1
)

:: ── Stage bundle ─────────────────────────────────────────────────────────────
echo Staging release bundle...
if exist "%BUNDLE_ROOT%" rmdir /s /q "%BUNDLE_ROOT%"
mkdir "%BIN_DIR%"

xcopy /e /i /y "%BUILD_DIR%\zeta\ui\holdem" "%BIN_DIR%\" >nul
if errorlevel 1 (
    echo ERROR: Failed to copy UI holdem build output.
    exit /b 1
)

copy /y "%BUILD_DIR%\zeta\tools\holdem\zeta-solve.exe" "%BIN_DIR%\" >nul
if errorlevel 1 (
    echo ERROR: Failed to copy zeta-solve.exe.
    exit /b 1
)

copy /y "%REPO_ROOT%\README.md" "%VERSION_DIR%\" >nul
xcopy /e /i /y "%REPO_ROOT%\doc" "%VERSION_DIR%\doc\" >nul

:: ── Generate install.cmd (tag baked in) ───────────────────────────────────────
powershell -NoProfile -Command ^
    "$tag = '%TAG%'; $lines = @(" ^
    "'@echo off'," ^
    "'setlocal'," ^
    "\"set \"\"BIN_PATH=%%~dp0zeta\\$tag\\holdem\\bin\"\"\"," ^
    "'echo Adding to user PATH: %%BIN_PATH%%'," ^
    "'powershell -NoProfile -Command \"[Environment]::SetEnvironmentVariable(''PATH'', ''%%BIN_PATH%%'' + '';'' + [Environment]::GetEnvironmentVariable(''PATH'', ''User''), ''User'')\"'," ^
    "'if errorlevel 1 (echo ERROR: Failed to update PATH. ^& exit /b 1)'," ^
    "'echo Done. Please restart your terminal or open a new Command Prompt.'," ^
    "'endlocal'" ^
    "); Set-Content -Path '%BUNDLE_ROOT%\install.cmd' -Value $lines -Encoding ASCII"
if errorlevel 1 (
    echo ERROR: Failed to generate install.cmd.
    exit /b 1
)

:: ── Validate bundle ───────────────────────────────────────────────────────────
for %%f in (
    "%BIN_DIR%\zeta-ui-holdem.exe"
    "%BIN_DIR%\zeta-solve.exe"
    "%VERSION_DIR%\README.md"
    "%BUNDLE_ROOT%\install.cmd"
) do (
    if not exist %%f (
        echo ERROR: Bundle is missing: %%f
        exit /b 1
    )
)

:: ── Zip ───────────────────────────────────────────────────────────────────────
set "ZIP_FILE=%ARTIFACTS%\%BUNDLE_NAME%.zip"
if exist "%ZIP_FILE%" del "%ZIP_FILE%"
echo Creating archive: %ZIP_FILE%
powershell -NoProfile -Command ^
    "Compress-Archive -Path '%BUNDLE_ROOT%\*' -DestinationPath '%ZIP_FILE%' -Force"
if errorlevel 1 (
    echo ERROR: Failed to create zip archive.
    exit /b 1
)

echo.
echo --- Release bundle ready ---
echo   Archive : %ZIP_FILE%
echo   Staging : %BUNDLE_ROOT%
echo.
echo To upload: scripts\upload_windows_release.cmd %TAG%
echo.

endlocal
