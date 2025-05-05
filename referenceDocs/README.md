Convert html site to markdown for grepping :)

```
#!/bin/bash
for file in *.html; do
    filename="${file%.html}"
    cat "$file" | sed '1,/<img src="https:\/\/slebok.github.io\/www\/babycobol.png" style="width:200px;height:200px;" class="flr" \/>/d' | sed '/<div class="last">/,/<\/html>/d' | pandoc --wrap=none --from html --to markdown > "$filename.md"
done
cat *.md >> combined.md
