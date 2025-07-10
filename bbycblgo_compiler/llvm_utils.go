package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// CompileIRToExecutable compiles a .ll file to an executable.
func CompileIRToExecutable(irFile string) (string, error) {
	objFile := strings.TrimSuffix(irFile, ".ll") + ".o"
	exeFile := strings.TrimSuffix(irFile, ".ll")

	// llc -filetype=obj test.ll -o test.o
	llcCmd := exec.Command("llc", "-filetype=obj", irFile, "-o", objFile)
	llcCmd.Stderr = os.Stderr
	if err := llcCmd.Run(); err != nil {
		return "", fmt.Errorf("llc failed: %w", err)
	}

	// clang test.o -o test
	clangCmd := exec.Command("clang", objFile, "-o", exeFile)
	clangCmd.Stderr = os.Stderr
	if err := clangCmd.Run(); err != nil {
		return "", fmt.Errorf("clang failed: %w", err)
	}

	return exeFile, nil
}
