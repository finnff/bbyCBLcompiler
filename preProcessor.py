# **Logical requirements for a COBOL parser:**
# ---
# 1. **Ignore columns 1–6 (sequence number).**
#    → Do not include or validate these.
# 2. **Process column 7 (line status indicator):**
#    * If space → normal line, parse content.
#    * If * → comment line, ignore entire line.
#    * If - → continuation line, append trimmed content (cols 8–72) to previous line.
#    * Else → raise parse error.
# 3. **Content columns:**
#    * Columns 8–11 → Area A:
#      * Divisions, section names, paragraph names, 01 level numbers must start here.
#    * Columns 12–72 → Area B:
#      * Statements (procedure division) and clauses (identification division) must start here.
# 4. **Ignore columns 73–80 (identification area).**
# 5. **Combined lines (with continuations) must be parsed as a single logical line.**

# **Logical requirements for case insensitivity:**
# ---
# 1. **Case-insensitive keywords and identifiers:**
#    * COBOL keywords (IF, MOVE, DISPLAY, etc.) are matched case-insensitively
#    * Variables and identifiers are matched case-insensitively
# 2. **Case-sensitive regions:**
#    * String literals (content between quotes)
#    * PICTURE clauses (patterns after PICTURE or PIC)
#    * Comment lines (ignored anyway, but preserve content)
#    * Identification division values (e.g., program name after PROGRAM-ID)
# 3. **Ambiguity resolution:**
#    * When a token could be both keyword and identifier (e.g., "END")
#    * Prefer keyword interpretation if written in uppercase
#    * Otherwise, treat as identifier

import re


def preprocess_cobol(lines):
    result = []
    in_proc_div = False
    current_line = ""
    current_start_col = None

    for line in lines:
        indicator = line[6] if len(line) >= 7 else " "
        area_a = line[7:11] if len(line) >= 11 else ""
        area_b = (
            line[11:72] if len(line) >= 72 else (line[11:] if len(line) > 11 else "")
        )

        if indicator == "*":
            continue

        if indicator == " ":
            content = line[7:72]
            if not re.search(r"\S", content):
                continue

            match = re.search(r"\S", content)
            if match:
                start_col = 7 + match.start()
            else:
                continue

            is_area_b = start_col == 11
            should_continue = (
                current_line
                and in_proc_div
                and is_area_b
                and not current_line.rstrip().endswith(".")
            )

            if should_continue:
                current_line += " " + (area_a + area_b).strip()
                continue

            if current_line:
                validate_area(current_line, current_start_col, in_proc_div)
                result.append(current_line)

            current_line = (area_a + area_b).rstrip()
            current_start_col = start_col

            if current_line.strip().upper().startswith("PROCEDURE DIVISION"):
                in_proc_div = True
            continue

        if indicator == "-":
            if not current_line:
                raise ValueError("Continuation without a preceding line")

            continuation_raw = (area_a + area_b).rstrip()
            if continuation_raw.startswith("."):
                continuation = "." + continuation_raw[1:].lstrip()
            else:
                continuation = continuation_raw.lstrip()

            last_match = re.search(r"[A-Za-z]+$", current_line.rstrip())
            first_match = re.match(r"[A-Za-z]+", continuation)

            join_without_space = (
                last_match
                and first_match
                and last_match.group(0).lower() not in ALL_KEYWORDS
                and first_match.group(0).lower() not in ALL_KEYWORDS
            )

            glue = "" if join_without_space else " "
            current_line = current_line.rstrip() + glue + continuation
            continue

        raise ValueError(f"Invalid line indicator '{indicator}' at column 7")

    if current_line:
        validate_area(current_line, current_start_col, in_proc_div)
        result.append(current_line)

    normalized_lines = [normalize_case(line) for line in result]

    return "\n".join(normalized_lines)


def validate_area(line_text, start_col, in_proc_div):
    text = line_text.lstrip().upper()
    original_text = line_text.lstrip()

    if (
        text.startswith("IDENTIFICATION DIVISION")
        or text.startswith("DATA DIVISION")
        or text.startswith("PROCEDURE DIVISION")
    ):
        if start_col != 7:
            raise ValueError(
                f"Division header '{text.split()[0]} DIVISION' must start in column 8, found col {start_col+1}"
            )
    elif in_proc_div:
        if (
            text.strip().endswith(".")
            and " " not in text.strip()
            and not text.strip().startswith("END-")
            and text.strip() != "STOP."
        ):
            if start_col != 7:
                raise ValueError(
                    f"Paragraph name '{text.strip()}' must start in column 8, found col {start_col+1}"
                )
            return
        else:
            if start_col != 11:
                raise ValueError(
                    f"In PROCEDURE DIVISION, statement '{text.split()[0]}' must start in column 12, found col {start_col+1}"
                )


ID_KEYWORDS = {
    "program-id",
    "author",
    "date-written",
    "date-compiled",
    "installation",
    "security",
}

ALL_KEYWORDS = {
    "identification",
    "data",
    "procedure",
    "division",
    "accept",
    "add",
    "subtract",
    "multiply",
    "divide",
    "move",
    "display",
    "evaluate",
    "if",
    "then",
    "else",
    "end",
    "end-if",
    "end-evaluate",
    "perform",
    "varying",
    "loop",
    "while",
    "until",
    "go",
    "call",
    "copy",
    "signal",
    "stop",
    "run",
    "next",
    "sentence",
    "delimited",
    "by",
    "with",
    "no",
    "advancing",
    "occurs",
    "times",
    "using",
    "reference",
    "value",
    "content",
    "giving",
    "remainder",
    "of",
    "size",
    "space",
    "spaces",
    "picture",
    "pic",
    "is",
    "like",
    "when",
    "other",
    "also",
    "high-values",
    "replacing",
    "proceed",
    "alter",
    "and",
    "or",
    "xor",
    "base",
    "description",
    "error",
}

_word_re = re.compile(r"[A-Za-z_$][A-Za-z0-9_$-]*")


def normalize_case(line: str) -> str:
    out = []
    i = 0
    in_string = False
    in_picture = False
    in_id_value = False
    after_copy = False

    while i < len(line):
        ch = line[i]

        if ch == '"':
            in_string = not in_string
            out.append(ch)
            i += 1
            continue

        if in_string:
            out.append(ch)
            i += 1
            continue

        if in_picture:
            out.append(ch)
            i += 1
            if ch == ".":
                in_picture = False
            continue

        if in_id_value:
            out.append(ch)
            i += 1
            if ch == ".":
                in_id_value = False
            continue

        if ch.isalpha() or ch in "_$":
            start = i
            while i < len(line) and (line[i].isalnum() or line[i] in "_$-"):
                i += 1
            word = line[start:i]
            lower = word.lower()

            if lower in {"picture", "pic"}:
                out.append(word.upper())
                j = i
                while j < len(line) and line[j].isspace():
                    j += 1
                if line[j : j + 2].lower() == "is":
                    out.append(line[i:j] + "IS")
                    j += 2
                else:
                    out.append(line[i:j])
                i = j
                in_picture = True
                continue

            if lower in ID_KEYWORDS:
                out.append(word.upper())
                dot = line.find(".", i)
                if dot != -1:
                    out.append(line[i : dot + 1])
                    i = dot + 1
                    in_id_value = True
                continue

            if lower == "copy" and not after_copy:
                out.append("COPY")
                after_copy = True
                continue

            if after_copy:
                out.append(word)
                after_copy = False
                continue

            if lower in ALL_KEYWORDS:
                out.append(word.upper())
            else:
                out.append(word)
        else:
            out.append(ch)
            i += 1

    return "".join(out)
