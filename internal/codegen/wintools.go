package codegen

import (
	"archive/zip"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

// ---------------------------------------------------------------------------
// Windows toolchain auto-download
//
// When compiling on Windows and NASM/GoLink are not in PATH, we
// automatically download them into ~/.novus/tools/ and use them.
// ---------------------------------------------------------------------------

const (
	nasmVersion = "2.16.03"
	nasmURL     = "https://www.nasm.us/pub/nasm/releasebuilds/2.16.03/win64/nasm-2.16.03-win64.zip"
	golinkURL   = "https://www.godevtool.com/Golink.zip"
)

// novusToolsDir returns the path to the Novus tools directory (~/.novus/tools/).
func novusToolsDir() string {
	home, err := os.UserHomeDir()
	if err != nil {
		home = "."
	}
	return filepath.Join(home, ".novus", "tools")
}

// EnsureWindowsToolchain checks if NASM and GoLink are available. If not,
// downloads them into ~/.novus/tools/ and returns their paths.
// Returns (nasmPath, golinkPath, error).
func EnsureWindowsToolchain(verbose bool) (string, string, error) {
	if runtime.GOOS != "windows" {
		return "nasm", "golink", nil
	}

	toolsDir := novusToolsDir()

	nasmPath, nasmOK := findNASM(toolsDir)
	golinkPath, golinkOK := findGoLink(toolsDir)

	if nasmOK && golinkOK {
		return nasmPath, golinkPath, nil
	}

	// Ensure tools directory exists.
	if err := os.MkdirAll(toolsDir, 0o755); err != nil {
		return "", "", fmt.Errorf("cannot create tools directory %s: %w", toolsDir, err)
	}

	if !nasmOK {
		if verbose {
			fmt.Println("[toolchain] NASM not found — downloading...")
		}
		var err error
		nasmPath, err = downloadNASM(toolsDir, verbose)
		if err != nil {
			return "", "", fmt.Errorf("failed to download NASM: %w", err)
		}
		if verbose {
			fmt.Printf("[toolchain] NASM installed to %s\n", nasmPath)
		}
	}

	if !golinkOK {
		if verbose {
			fmt.Println("[toolchain] GoLink not found — downloading...")
		}
		var err error
		golinkPath, err = downloadGoLink(toolsDir, verbose)
		if err != nil {
			return "", "", fmt.Errorf("failed to download GoLink: %w", err)
		}
		if verbose {
			fmt.Printf("[toolchain] GoLink installed to %s\n", golinkPath)
		}
	}

	return nasmPath, golinkPath, nil
}

// findNASM checks PATH first, then the tools directory.
func findNASM(toolsDir string) (string, bool) {
	if p, err := exec.LookPath("nasm"); err == nil {
		return p, true
	}
	// Check tools directory.
	localNasm := filepath.Join(toolsDir, "nasm", "nasm.exe")
	if _, err := os.Stat(localNasm); err == nil {
		return localNasm, true
	}
	// Also check nasm subdirectories (nasm-X.XX.XX).
	entries, err := os.ReadDir(toolsDir)
	if err == nil {
		for _, e := range entries {
			if e.IsDir() && strings.HasPrefix(e.Name(), "nasm") {
				candidate := filepath.Join(toolsDir, e.Name(), "nasm.exe")
				if _, err := os.Stat(candidate); err == nil {
					return candidate, true
				}
			}
		}
	}
	return "", false
}

// findGoLink checks PATH first, then the tools directory.
func findGoLink(toolsDir string) (string, bool) {
	if p, err := exec.LookPath("golink"); err == nil {
		return p, true
	}
	// GoLink may be named GoLink.exe.
	for _, name := range []string{"golink.exe", "GoLink.exe"} {
		localGoLink := filepath.Join(toolsDir, "golink", name)
		if _, err := os.Stat(localGoLink); err == nil {
			return localGoLink, true
		}
	}
	return "", false
}

// downloadNASM downloads and extracts NASM to toolsDir/nasm/.
func downloadNASM(toolsDir string, verbose bool) (string, error) {
	zipPath := filepath.Join(toolsDir, "nasm.zip")
	if err := downloadFile(nasmURL, zipPath, verbose); err != nil {
		return "", err
	}
	defer os.Remove(zipPath)

	nasmDir := filepath.Join(toolsDir, "nasm")
	if err := os.MkdirAll(nasmDir, 0o755); err != nil {
		return "", err
	}

	if err := unzip(zipPath, nasmDir); err != nil {
		return "", fmt.Errorf("failed to extract NASM: %w", err)
	}

	// NASM zip usually contains a subdirectory like nasm-2.16.03/.
	// Find nasm.exe recursively.
	var nasmExe string
	filepath.Walk(nasmDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if !info.IsDir() && strings.EqualFold(info.Name(), "nasm.exe") {
			nasmExe = path
			return filepath.SkipAll
		}
		return nil
	})

	if nasmExe == "" {
		return "", fmt.Errorf("nasm.exe not found after extraction")
	}
	return nasmExe, nil
}

// downloadGoLink downloads and extracts GoLink to toolsDir/golink/.
func downloadGoLink(toolsDir string, verbose bool) (string, error) {
	zipPath := filepath.Join(toolsDir, "golink.zip")
	if err := downloadFile(golinkURL, zipPath, verbose); err != nil {
		return "", err
	}
	defer os.Remove(zipPath)

	golinkDir := filepath.Join(toolsDir, "golink")
	if err := os.MkdirAll(golinkDir, 0o755); err != nil {
		return "", err
	}

	if err := unzip(zipPath, golinkDir); err != nil {
		return "", fmt.Errorf("failed to extract GoLink: %w", err)
	}

	// Find GoLink.exe.
	var golinkExe string
	filepath.Walk(golinkDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if !info.IsDir() && strings.EqualFold(info.Name(), "golink.exe") {
			golinkExe = path
			return filepath.SkipAll
		}
		return nil
	})

	if golinkExe == "" {
		return "", fmt.Errorf("GoLink.exe not found after extraction")
	}
	return golinkExe, nil
}

// downloadFile downloads a URL to a local file path.
func downloadFile(url, dest string, verbose bool) error {
	if verbose {
		fmt.Printf("[toolchain] Downloading %s ...\n", url)
	}

	resp, err := http.Get(url)
	if err != nil {
		return fmt.Errorf("download failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download returned status %d", resp.StatusCode)
	}

	out, err := os.Create(dest)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, resp.Body)
	return err
}

// unzip extracts a zip archive to the destination directory.
func unzip(src, dest string) error {
	r, err := zip.OpenReader(src)
	if err != nil {
		return err
	}
	defer r.Close()

	for _, f := range r.File {
		fpath := filepath.Join(dest, f.Name)

		// Prevent zip slip.
		if !strings.HasPrefix(filepath.Clean(fpath), filepath.Clean(dest)+string(os.PathSeparator)) {
			// Allow exact match (for files extracted to dest root).
			if filepath.Clean(fpath) != filepath.Clean(dest) {
				continue
			}
		}

		if f.FileInfo().IsDir() {
			os.MkdirAll(fpath, 0o755)
			continue
		}

		if err := os.MkdirAll(filepath.Dir(fpath), 0o755); err != nil {
			return err
		}

		outFile, err := os.OpenFile(fpath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, f.Mode())
		if err != nil {
			return err
		}

		rc, err := f.Open()
		if err != nil {
			outFile.Close()
			return err
		}

		_, err = io.Copy(outFile, rc)
		rc.Close()
		outFile.Close()
		if err != nil {
			return err
		}
	}

	return nil
}
