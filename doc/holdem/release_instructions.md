# Hold'em Solver Release Instructions

This guide explains how to build a Windows release bundle for `zeta-ui-holdem` and publish it to a GitHub Release without triggering a release-time source build in GitHub Actions.

## Release bundle

The Windows release asset is:

```text
zeta-holdem-solver-windows-x64.zip
```

It contains the Release build output from:

```text
build\zeta\ui\holdem\Release
```

The GitHub Actions **Windows Build** workflow creates and uploads this zip as a workflow artifact named:

```text
zeta-holdem-solver-windows-x64
```

## Build locally

Configure the build tree with vcpkg and benchmarks disabled:

```powershell
cmake -S . -B build `
  -DCMAKE_TOOLCHAIN_FILE=C:\vcpkg\scripts\buildsystems\vcpkg.cmake `
  -DZETA_BUILD_BENCHMARKS=OFF
```

Build the UI and tests:

```powershell
cmake --build build --config Release --target zeta-ui-holdem zeta_tests
```

Run the tests:

```powershell
$env:QT_QPA_PLATFORM = 'offscreen'
ctest --test-dir build\zeta\test -C Release --output-on-failure
```

Create the release zip:

```powershell
$bundleName = 'zeta-holdem-solver-windows-x64'
$source = 'build\zeta\ui\holdem\Release'
$staging = "artifacts\$bundleName"

New-Item -ItemType Directory -Force -Path $staging | Out-Null
Copy-Item "$source\*" $staging -Recurse -Force
Compress-Archive -Path "$staging\*" -DestinationPath "artifacts\$bundleName.zip" -Force
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

## Publish a GitHub Actions artifact

Use this path when the **Windows Build** workflow has already produced the bundle artifact and you only need to publish it to a release.

Find the workflow run ID:

```powershell
gh run list --workflow "Windows Build" --limit 10
```

Then run the **Publish Release Bundle** workflow from GitHub Actions with:

| Input | Value |
| --- | --- |
| `tag` | Release tag, for example `v0.1.0` |
| `run_id` | The completed **Windows Build** run ID |
| `artifact_name` | `zeta-holdem-solver-windows-x64` |

The workflow downloads the artifact from the selected run, checks that it contains exactly one zip file, then creates or updates the GitHub Release asset.

The same workflow can be started from the CLI:

```powershell
gh workflow run "Publish Release Bundle" `
  -f tag=v0.1.0 `
  -f run_id=1234567890 `
  -f artifact_name=zeta-holdem-solver-windows-x64
```
