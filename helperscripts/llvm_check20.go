// Save this as llvm_check20.go and run: go run -tags=llvm20 llvm_check20.go
package main

import (
	"fmt"
	"os"

	"tinygo.org/x/go-llvm"
)

func main() {
	fmt.Println("LLVM 20 Initialization Test")
	fmt.Println("===========================")

	// Test 1: Basic initialization
	fmt.Println("\n1. Initializing LLVM components...")
	llvm.InitializeAllTargets()
	llvm.InitializeAllTargetMCs()
	llvm.InitializeAllAsmParsers()
	llvm.InitializeAllAsmPrinters()
	llvm.InitializeNativeTarget()
	llvm.InitializeNativeAsmPrinter()
	fmt.Println("   ✓ Initialization calls completed")

	// Test 2: Check default target triple
	fmt.Printf("\n2. Default target triple: %s\n", llvm.DefaultTargetTriple())

	// Test 3: List available targets - simplified approach
	fmt.Println("\n3. Testing target enumeration...")
	targetCount := 0

	// Try to get the first target to see if any exist
	firstTarget := llvm.FirstTarget()
	if fmt.Sprintf("%v", firstTarget) == "{<nil>}" || fmt.Sprintf("%v", firstTarget) == "" {
		fmt.Println("   ❌ ERROR: No targets found!")
		fmt.Println("\nThis indicates a problem with your LLVM installation or TinyGo bindings.")
		fmt.Println("Possible solutions:")
		fmt.Println("1. Reinstall LLVM development packages:")
		fmt.Println("   Ubuntu/Debian: sudo apt-get install llvm-20-dev libllvm20")
		fmt.Println("   Fedora: sudo dnf install llvm-devel")
		fmt.Println("   macOS: brew install llvm")
		fmt.Println("2. Reinstall TinyGo")
		fmt.Println("3. Check LLVM version compatibility with TinyGo")
		os.Exit(1)
	}

	// Count targets by iterating
	target := firstTarget
	for {
		if fmt.Sprintf("%v", target) == "{<nil>}" || fmt.Sprintf("%v", target) == "" {
			break
		}
		fmt.Printf("   - %s: %s\n", target.Name(), target.Description())
		targetCount++
		nextTarget := target.NextTarget()
		if fmt.Sprintf("%v", nextTarget) == fmt.Sprintf("%v", target) {
			// Avoid infinite loop if NextTarget returns same target
			break
		}
		target = nextTarget

		// Safety limit to avoid infinite loops
		if targetCount > 100 {
			break
		}
	}

	if targetCount == 0 {
		fmt.Println("   ❌ ERROR: No targets enumerated!")
		os.Exit(1)
	}

	fmt.Printf("   ✓ Found %d targets\n", targetCount)

	// Test 4: Try to get target from default triple
	fmt.Println("\n4. Testing target lookup...")
	defaultTriple := llvm.DefaultTargetTriple()
	targetFromTriple, err := llvm.GetTargetFromTriple(defaultTriple)
	if err != nil {
		fmt.Printf("   ❌ ERROR getting default target: %v\n", err)

		// Try fallbacks
		fmt.Println("   Trying fallback targets...")
		fallbacks := []string{
			"x86_64-unknown-linux-gnu",
			"x86_64-pc-linux-gnu",
			"x86_64-linux-gnu",
			"i386-unknown-linux-gnu",
		}

		found := false
		for _, fallback := range fallbacks {
			targetFromTriple, err = llvm.GetTargetFromTriple(fallback)
			if err == nil {
				fmt.Printf("   ✓ Fallback target works: %s\n", fallback)
				defaultTriple = fallback
				found = true
				break
			}
			fmt.Printf("   ❌ Fallback failed: %s (%v)\n", fallback, err)
		}

		if !found {
			fmt.Println("   ❌ All targets failed!")
			os.Exit(1)
		}
	} else {
		fmt.Printf("   ✓ Default target works: %s (%s)\n", targetFromTriple.Name(), targetFromTriple.Description())
	}

	// Test 5: Create a simple context and module
	fmt.Println("\n5. Testing LLVM context and module creation...")
	context := llvm.NewContext()
	defer context.Dispose()

	module := context.NewModule("test")
	if module.String() == "" {
		fmt.Println("   ❌ ERROR: Failed to create module")
		os.Exit(1)
	}
	fmt.Println("   ✓ Successfully created LLVM context and module")

	// Test 6: Create a target machine
	fmt.Println("\n6. Testing target machine creation...")
	targetMachine := targetFromTriple.CreateTargetMachine(defaultTriple, "", "", llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelDefault)

	// Check target machine validity by trying to use it
	dataLayout := targetMachine.CreateTargetData()
	if dataLayout.String() == "" {
		fmt.Println("   ❌ ERROR: Failed to create target machine or data layout")
		os.Exit(1)
	}
	fmt.Println("   ✓ Successfully created target machine")
	fmt.Printf("   ✓ Data layout: %s\n", dataLayout.String())

	fmt.Println("\n✅ All tests passed! LLVM should work with your compiler.")
}
