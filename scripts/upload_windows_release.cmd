@echo off
setlocal enabledelayedexpansion

:: upload_windows_release.cmd <tag> [--prerelease]
::
:: Uploads artifacts\zeta-holdem-solver-windows-x64-<tag>.zip to the
:: GitHub release for <tag>.  If a release does not yet exist it is created
:: (notes generated automatically from commits).
::
:: Requires: gh CLI authenticated with write access to lewismj/zeta

if "%~1"=="" (
    echo Usage: scripts\upload_windows_release.cmd ^<tag^> [--prerelease]
    echo Example: scripts\upload_windows_release.cmd v0.2.0
    echo Example: scripts\upload_windows_release.cmd v0.2.0-beta --prerelease
    exit /b 1
)

set "TAG=%~1"
set "PRERELEASE_FLAG="
if /i "%~2"=="--prerelease" set "PRERELEASE_FLAG=--prerelease"

for %%i in ("%~dp0..") do set "REPO_ROOT=%%~fi"
set "BUNDLE_NAME=zeta-holdem-solver-windows-x64-%TAG%"
set "ZIP_FILE=%REPO_ROOT%\artifacts\%BUNDLE_NAME%.zip"

:: ── Verify zip exists ─────────────────────────────────────────────────────────
if not exist "%ZIP_FILE%" (
    echo ERROR: Release archive not found: %ZIP_FILE%
    echo Run scripts\create_windows_release.cmd %TAG% first.
    exit /b 1
)

:: ── Verify gh is available ───────────────────────────────────────────────────
where gh >nul 2>&1
if errorlevel 1 (
    echo ERROR: gh CLI not found. Install from https://cli.github.com
    exit /b 1
)

:: ── Upload or create release ─────────────────────────────────────────────────
echo Checking for existing GitHub release: %TAG%
gh release view "%TAG%" --repo lewismj/zeta >nul 2>&1
if not errorlevel 1 (
    echo Release exists - uploading asset...
    gh release upload "%TAG%" "%ZIP_FILE%" --repo lewismj/zeta --clobber
    if errorlevel 1 (
        echo ERROR: Failed to upload release asset.
        exit /b 1
    )
) else (
    echo Release does not exist - creating...
    gh release create "%TAG%" "%ZIP_FILE%" ^
        --repo lewismj/zeta ^
        --title "%TAG%" ^
        --generate-notes ^
        %PRERELEASE_FLAG%
    if errorlevel 1 (
        echo ERROR: Failed to create GitHub release.
        exit /b 1
    )
)

echo.
echo --- Upload complete ---
echo   Tag  : %TAG%
echo   Asset: %BUNDLE_NAME%.zip
echo   URL  : https://github.com/lewismj/zeta/releases/tag/%TAG%
echo.

endlocal
