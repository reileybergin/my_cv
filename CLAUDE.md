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
   - `pubs` - Publications, abstracts, and dissertations
   - `presentations` - Conference presentations and talks
   - `media` - Media coverage and appearances
   - `grants` - Grant funding information
   - `grant_totals` - Summary of grant funding
   - `awards` - Honors and awards
   - `classes` - Teaching activity data
   - `teaching` - Workshops, seminars, guest lectures
   - `advising` - Student advising (thesis committees, dissertations)
   - `service` - Administrative and university service activities
   - `reviews` - Journal and conference review activities
2. R code chunks in the `.qmd` file call `get_cv_sheet()` to load data from specific sheets
3. Helper functions process and format the data:
   - `make_ordered_list_filtered()` - Filters by category and creates ordered lists
   - `make_ordered_list()` - Creates ordered lists from data
   - `make_bullet_list()` - Creates unordered bullet lists
   - `enquote()` - Adds quotation marks around text
   - `na_to_space()` - Converts NA values to empty strings

### Data Sheet Categories

#### Publication Categories (`pubs` sheet)
Publications use a `category` column to differentiate types:
- `peer_rev_pub_article` - Published peer-reviewed journal articles
- `dissertation` - Theses and dissertations
- `peer_rev_abstract` - Conference abstracts and peer-reviewed original research
- `peer_review_article_in_review` - Submissions under review (currently unused)

#### Advising Categories (`advising` sheet)
The advising sheet tracks student mentorship with these categories:
- `phd` - PhD students (current and completed)
- `ms` - Master's students (current and completed)
- `ug` - Undergraduate students
- `hs` - High school students
- `committee` - PhD committee member (not primary advisor)

Key fields:
- `complete` - 0 for current students, 1 for graduated
- `name` - Student name
- `institution` - Institution/program
- `defense_date` - Defense or expected graduation date (Excel date format)
- `chair` - TRUE if chair/primary advisor, FALSE if committee member
- `title` - Thesis/dissertation title
- `notes` - Program or additional details (e.g., "Biomechanics")

#### Service Categories (`service` sheet)
The service sheet tracks committee and service work:
- `dept` - Department-level service
- `college` - College-level service
- `professional` - Professional service outside the university

Key fields:
- `dates` - Date range (e.g., "2025 - present")
- `activity` - Committee name and role (e.g., "CHS AI Steering Committee, Faculty Member")

### How to Add New CV Sections

Follow this data-driven pattern when adding new sections:

1. **Update the Excel file** (`data/cv.xlsx`):
   - Add data to the appropriate sheet (or create a new sheet if needed)
   - Use the `category` field to differentiate entry types within a sheet
   - For dates, use Excel date format or text strings like "2025 - present"

2. **Add the section to the Quarto document** (`reiley_bergin_cv.qmd`):
   ```r
   # Section Header

   ## Subsection (if needed)

   ```{r}
   #| results: asis

   # Read and process data
   section_df <- get_cv_sheet('sheet_name') %>%
     filter(category == 'desired_category', !is.na(key_field)) %>%
     mutate(
       # Format your citation/entry text
       citation = paste0(field1, ', ', field2, ' (', dates, ').')
     )

   # Output as a numbered list
   section_df %>%
     pull(citation) %>%
     make_ordered_list()
   ```
   ```

3. **Date Formatting for Excel Dates**:
   - Excel stores dates as numbers (days since 1970-01-01)
   - Convert using: `as.Date(date_field, origin = "1970-01-01")`
   - Format output: `format(date, "%B %Y")` for "Month Year" or `"%Y"` for year only
   - Example: `format(as.Date(defense_date, origin = "1970-01-01"), "%B %Y")`

4. **Page Break Controls**:
   - To prevent list items from splitting across pages, add before the R chunk:
   ```latex
   <!-- Prevent page breaks within list items -->
   \interlinepenalty=10000
   ```

### Styling and Fonts
- Uses custom Raleway font for main text (loaded from `fonts/` directory)
- Includes KaiTi font for Chinese text support via xeCJK package
- Custom LaTeX preamble includes footer with update date, page numbers, and GitHub URL
- List item spacing controlled by `\apptocmd{\tightlist}{\setlength{\itemsep}{4pt}}{}{}`

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
- `readxl` - Reading Excel data files (`data/cv.xlsx` is the single source of truth)
- `dplyr`, `stringr`, `lubridate` - Data manipulation
- `pander` - Markdown list generation
- `systemfonts`, `svglite` - Graphics and font rendering (require system libraries)
- `writexl` - Writing Excel files (for updating `data/cv.xlsx`)

**Note**: The Excel file `data/cv.xlsx` is committed to the repository and serves as the single source of truth for all CV content. Update this file to modify CV entries.

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
