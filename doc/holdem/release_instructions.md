# Hold'em Solver Release Instructions

This guide explains how to build a Windows release bundle for `zeta-ui-holdem`
and `zeta-solve`, and how to publish it with the manual GitHub Actions release
workflow.

## Release bundle

The Windows release asset is:

```text
zeta-holdem-solver-windows-x64.zip
```

It contains a self-contained `zeta` directory:

```text
zeta/
  README.md
  doc/
  bin/
    holdem/
      zeta-ui-holdem.exe
      zeta-solve.exe
      *.dll
      platforms/
      iconengines/
      imageformats/
      styles/
```

The GitHub Actions **Windows Release** workflow creates and uploads this zip as
both a workflow artifact and a GitHub Release asset.

## Publish with GitHub Actions

The release workflow is manual only. It does not run on every push, pull request,
or tag creation.

1. Create and push the release tag.
2. Open **Actions > Windows Release > Run workflow**.
3. Enter the tag, for example `v0.1.0`.
4. Choose whether the release is a prerelease.

The same workflow can be started from the CLI:

```text
gh workflow run "Windows Release" -f tag=v0.1.0 -f prerelease=false
```

The workflow avoids long always-on release builds by:

- running only through `workflow_dispatch`
- building only `zeta-ui-holdem` and `zeta-solve`
- configuring with tests and benchmarks disabled
- using the vcpkg binary cache for third-party dependencies

## Build locally

Configure the build tree with vcpkg and benchmarks disabled:

```powershell
cmake -S . -B build `
  -DCMAKE_TOOLCHAIN_FILE=C:\vcpkg\scripts\buildsystems\vcpkg.cmake `
  -DZETA_BUILD_TESTS=OFF `
  -DZETA_BUILD_BENCHMARKS=OFF
```

Build the release targets:

```powershell
cmake --build build --config Release --target zeta-ui-holdem zeta-solve --parallel
```

Create the release zip:

```powershell
$bundleName = 'zeta-holdem-solver-windows-x64'
$staging = "artifacts\$bundleName"
$root = "$staging\zeta"
$bin = "$root\bin\holdem"

New-Item -ItemType Directory -Force -Path $bin | Out-Null
Copy-Item README.md $root -Force
Copy-Item doc "$root\doc" -Recurse -Force
Copy-Item "build\zeta\ui\holdem\Release\*" $bin -Recurse -Force
Copy-Item "build\zeta\tools\holdem\Release\zeta-solve.exe" $bin -Force
Compress-Archive -Path $root -DestinationPath "artifacts\$bundleName.zip" -Force
```

## Publish a local bundle

Create a new release and upload the zip:

```powershell
gh release create v0.1.0 artifacts\zeta-holdem-solver-windows-x64.zip --title v0.1.0 --generate-notes
```

Upload or replace the asset on an existing release:

```powershell
gh release upload v0.1.0 artifacts\zeta-holdem-solver-windows-x64.zip --clobber
```
