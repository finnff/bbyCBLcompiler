// Package main provides the core logic for the COBOL parser performance test.
package main

import (
	"strings"
)

// preprocessCobolAdvanced preprocesses COBOL source code lines.
// It handles fixed-format COBOL rules, including line length,
// indicator area, and continuation lines, to produce a clean string
// suitable for parsing.
// Enhanced preprocessing to fix token concatenation issues
func preprocessCobolAdvanced(lines []string) string {
	var processed strings.Builder
	lastWasNonSpace := false // Track if last character was non-space

	for _, line := range lines {
		var indicator byte
		var codeArea string

		if len(line) == 0 {
			continue
		}

		// Pad to 80 if shorter
		if len(line) < 80 {
			line = line + strings.Repeat(" ", 80-len(line))
		}

		if len(line) < 7 {
			indicator = ' '
			codeArea = line[0:min(len(line), 72)]
		} else {
			indicator = line[6]
			codeArea = line[7:min(len(line), 72)]
		}

		// Skip comments and debug
		if indicator == '*' || indicator == '/' || indicator == 'D' || indicator == 'd' {
			continue
		}

		// Handle continuation
		if indicator == '-' {
			// Get current content and trim trailing whitespace
			if processed.Len() > 0 {
				content := processed.String()
				content = strings.TrimRight(content, " \t\n")
				processed.Reset()
				processed.WriteString(content)

				// Check if last character was non-whitespace
				lastWasNonSpace = len(content) > 0 &&
					content[len(content)-1] != ' ' && content[len(content)-1] != '\t'
			}

			// Trim trailing from current codeArea
			codeArea = strings.TrimRight(codeArea, " \t")

			// Trim leading whitespace
			trimmedLeft := strings.TrimLeft(codeArea, " \t")

			// Handle different continuation scenarios
			if strings.HasPrefix(trimmedLeft, "\"") {
				// String literal continuation: use original spacing
				processed.WriteString(codeArea)
			} else if len(trimmedLeft) > 0 {
				// Non-literal continuation: ensure proper token separation
				firstChar := trimmedLeft[0]

				// If previous line ended with non-space and current starts with alphanumeric,
				// we need a space to separate tokens
				needSpace := lastWasNonSpace && (isAlphaNumeric(firstChar) || firstChar == '-')
				if needSpace {
					penult := byte(' ')
					if processed.Len() > 1 {
						penult = processed.String()[processed.Len()-2]
					}
					// insert the space only when we are sure we are *between* two tokens,
					// i.e. previous char *and* the one before it look like part of a word.
					if isAlphaNumeric(penult) || penult == '-' {
						processed.WriteString(" ")
					}
				}

				processed.WriteString(trimmedLeft)
			}

			// Update tracking for next iteration
			if len(trimmedLeft) > 0 {
				lastChar := trimmedLeft[len(trimmedLeft)-1]
				lastWasNonSpace = lastChar != ' ' && lastChar != '\t'
			}

		} else if indicator != ' ' {
			return "Error: Invalid indicator '" + string(indicator) + "' in line: " + line
		} else {
			// Normal line processing
			codeArea = strings.TrimRight(codeArea, " \t")

			if len(codeArea) > 0 {
				processed.WriteString(codeArea)
				processed.WriteString("\n")
				lastWasNonSpace = false // Reset after newline
			}
		}
	}

	return processed.String()
}

// Helper function to check if a character is alphanumeric or underscore
func isAlphaNumeric(c byte) bool {
	return (c >= 'A' && c <= 'Z') ||
		(c >= 'a' && c <= 'z') ||
		(c >= '0' && c <= '9') ||
		c == '_' || c == '$'
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// Alternative simpler fix - just ensure space between continuation lines
func preprocessCobolAdvancedSimple(lines []string) string {
	var processed strings.Builder

	for _, line := range lines {
		var indicator byte
		var codeArea string

		if len(line) == 0 {
			continue
		}

		// Pad to 80 if shorter
		if len(line) < 80 {
			line = line + strings.Repeat(" ", 80-len(line))
		}

		if len(line) < 7 {
			indicator = ' '
			codeArea = line[0:min(len(line), 72)]
		} else {
			indicator = line[6]
			codeArea = line[7:min(len(line), 72)]
		}

		// Skip comments and debug
		if indicator == '*' || indicator == '/' || indicator == 'D' || indicator == 'd' {
			continue
		}

		// Handle continuation
		if indicator == '-' {
			// Trim trailing from previous
			if processed.Len() > 0 {
				content := processed.String()
				content = strings.TrimRight(content, " \t\n")
				processed.Reset()
				processed.WriteString(content)
			}

			// Trim trailing from current codeArea
			codeArea = strings.TrimRight(codeArea, " \t")
			trimmedLeft := strings.TrimLeft(codeArea, " \t")

			// If after left-trim it starts with '"', it's a literal continuation
			if strings.HasPrefix(trimmedLeft, "\"") {
				processed.WriteString(codeArea)
			} else {
				// Non-literal: always add space for safety
				processed.WriteString(" " + trimmedLeft)
			}
		} else if indicator != ' ' {
			return "Error: Invalid indicator '" + string(indicator) + "' in line: " + line
		} else {
			codeArea = strings.TrimRight(codeArea, " \t")
		}

		if len(codeArea) > 0 {
			if indicator != '-' {
				processed.WriteString(codeArea)
				processed.WriteString("\n")
			}
		}
	}

	return processed.String()
}