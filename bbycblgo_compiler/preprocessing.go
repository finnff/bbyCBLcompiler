// Package main provides the core logic for the COBOL parser performance test.
package main

import (
	"strings"
)

// preprocessCobolAdvanced preprocesses COBOL source code lines.
// It handles fixed-format COBOL rules, including line length, indicator area,
// and continuation lines, to produce a clean string suitable for parsing.
func preprocessCobolAdvanced(lines []string) string {
	var processed strings.Builder

	for _, line := range lines {
		// Handle COBOL fixed format (columns 1-80)
		if len(line) == 0 {
			continue
		}

		// Extend line to 80 characters if shorter
		if len(line) < 80 {
			line = line + strings.Repeat(" ", 80-len(line))
		}

		// Extract different areas of COBOL line:
		// Columns 1-6: Sequence numbers (ignored)
		// Column 7: Indicator area
		// Columns 8-11: Area A
		// Columns 12-72: Area B
		// Columns 73-80: Identification area (ignored)

		if len(line) > 6 {
			indicator := line[6]

			// Skip comment lines (indicator area contains '*' or '/')
			if indicator == '*' || indicator == '/' {
				continue
			}

			// Skip debug lines (indicator area contains 'D' or 'd')
			if indicator == 'D' || indicator == 'd' {
				continue
			}

			// Handle continuation lines (indicator area contains '-')
			if indicator == '-' {
				// This is a continuation of the previous line
				// Remove trailing whitespace from previous line and continue
				if processed.Len() > 0 {
					content := processed.String()
					content = strings.TrimRight(content, " \t\n")
					processed.Reset()
					processed.WriteString(content)
				}
			}

			// Extract code area (columns 8-72)
			if len(line) >= 8 {
				codeArea := line[7:min(len(line), 72)]

				// Clean up the code area by trimming trailing whitespace
				codeArea = strings.TrimRight(codeArea, " \t")

				if len(codeArea) > 0 {
					processed.WriteString(codeArea)
					// Add newline unless it's a continuation line
					if indicator != '-' {
						processed.WriteString("\n")
					}
				}
			}
		}
	}

	return processed.String()
}

// min is a helper function to find the minimum of two integers.
// This is used for compatibility with older Go versions that might not have built-in min.
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}