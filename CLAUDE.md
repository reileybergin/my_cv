# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an academic CV repository that generates a PDF curriculum vitae using Quarto. The CV content is stored in an Excel file (`data/cv.xlsx`) and rendered through a Quarto document (`reiley_bergin_cv.qmd`) into a professionally formatted PDF (`reiley_bergin_cv.pdf`).

## Build and Rendering

### Generate the CV PDF
```bash
quarto render reiley_bergin_cv.qmd
```

This command processes the Quarto document and outputs `reiley_bergin_cv.pdf`. The rendering uses XeLaTeX as the PDF engine.

## Project Structure

### Core Files
- `reiley_bergin_cv.qmd` - Main Quarto document containing CV structure and R code chunks
- `data/cv.xlsx` - Excel workbook with sheets for different CV sections (classes, pubs, etc.)
- `functions.R` - Helper functions for data processing and formatting
- `preamble.tex` - LaTeX configuration for fonts, styling, headers/footers

### Data Architecture
The CV uses a data-driven approach:
1. Content is maintained in `data/cv.xlsx` with separate sheets for different sections:
   - `classes` - Teaching activity data
   - `pubs` - Publications, abstracts, and dissertations
2. R code chunks in the `.qmd` file call `get_cv_sheet()` to load data from specific sheets
3. Helper functions process and format the data:
   - `make_ordered_list_filtered()` - Filters by category and creates ordered lists
   - `enquote()` - Adds quotation marks around text
   - `na_to_space()` - Converts NA values to empty strings

### Publication Categories
Publications in the Excel sheet use a `category` column to differentiate types:
- `peer_rev_pub_article` - Published peer-reviewed journal articles
- `dissertation` - Theses and dissertations
- `peer_rev_abstract` - Conference abstracts and peer-reviewed original research
- `peer_review_article_in_review` - Submissions under review (currently unused)

### Styling and Fonts
- Uses custom Raleway font for main text (loaded from `fonts/` directory)
- Includes KaiTi font for Chinese text support via xeCJK package
- Custom LaTeX preamble includes footer with update date, page numbers, and GitHub URL

## Environment Setup

### System Dependencies (macOS)

The project requires several system libraries for R package compilation:

```bash
# Install required Homebrew packages
brew install freetype pkg-config
```

### R Package Compilation Configuration

Create `~/.R/Makevars` to ensure R can find Homebrew-installed libraries:

```makefile
CPPFLAGS += -I/opt/homebrew/include
LDFLAGS += -L/opt/homebrew/lib
PKG_CONFIG_PATH = /opt/homebrew/lib/pkgconfig
```

This is required for packages like `systemfonts` and `svglite` to compile successfully.

### R Package Management

This project uses `renv` for R package management:
- `.Rprofile` activates renv on project load
- `renv.lock` contains the package dependency specifications (R 4.5.0)
- `renv/` directory stores project-local package library

To restore dependencies:
```bash
Rscript -e "renv::restore()"
```

**Note:** The project requires R 4.5.0 or compatible version. The `stringi` package version is pinned to 1.8.7+ to avoid compilation issues with R 4.5.x and clang 17.

### LaTeX/PDF Dependencies

Install TinyTeX for PDF generation:
```bash
quarto install tinytex
```

TinyTeX will automatically install required LaTeX packages (including `xecjk` for Chinese font support) on first render.

## Key Dependencies
- `kableExtra` - Table formatting in LaTeX/PDF output
- `readxl` - Reading Excel data files
- `dplyr`, `stringr`, `lubridate` - Data manipulation
- `pander` - Markdown list generation
- `systemfonts`, `svglite` - Graphics and font rendering (require system libraries)

## Troubleshooting

### Package Compilation Errors
If you encounter compilation errors with `stringi`, `systemfonts`, or `svglite`:
1. Ensure Homebrew packages are installed: `brew install freetype pkg-config`
2. Verify `~/.R/Makevars` is configured with Homebrew paths
3. For `stringi` errors on R 4.5.x: ensure using version 1.8.7 or later

### PDF Rendering Issues
If PDF rendering fails with "No TeX installation detected":
```bash
quarto install tinytex
```
