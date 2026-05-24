# Task: Generate matxingraphout CSV inputs — Zimbabwe NCD care pathway (Figure 2)

## Purpose

This file instructs Claude Code to generate the three CSV input files for the
matxingraphout Shiny app representing the healthcare system care pathway
described in Dixon et al. (2025).

---

## Source document

**Citation:** Dixon J, Kranzer K, Goode K, et al. (2025). "Multimorbidity,
non-communicable diseases and the health system in Zimbabwe: a KnowMM study."
*PLOS Global Public Health.*

**What to read:** The full article text — specifically the sections describing
the structure of the Zimbabwean health system: facility types, funding streams,
patient pathways between levels of care, and community-level actors.

**Critical constraint:** Use only information readable from the article text.
Do not use any embedded figures, diagrams, or Mermaid code blocks in the
document.

---

## Output files

Produce three CSV files in the same directory as this TASK.md:

| File | Format |
|---|---|
| `Figure_2_nodes_prop.csv` | Node properties: columns `id, label, shape, colour` |
| `Figure_2_adj.csv` | Adjacency matrix: first column = from-node IDs; remaining column headers = to-node IDs; numeric 0/1 values |
| `Figure_2_edges.csv` | Edge label matrix: same layout as `Figure_2_adj.csv` but string cell values (empty where no edge) |

All three must share the same set of node IDs, in the same order. Node IDs
must match exactly between `Figure_2_nodes_prop.csv` (the `id` column) and
the row/column labels of `Figure_2_adj.csv` and `Figure_2_edges.csv`.

---

## Node schema

Assign `shape` and `colour` according to the following conventions:

| Node category | `shape` | `colour` |
|---|---|---|
| Funding source | `diamond` | See colour table |
| Major facility cluster (header) | `rect` | See colour table |
| Sub-facility / service type | `rounded` | See colour table |
| Access/cost node | `ellipse` | See colour table |
| Community actor | `circle` | See colour table |

| Colour | Applies to |
|---|---|
| `#264653` | Government / secondary & central hospitals |
| `#00A896` | Partner / NGO / other partner HIV clinics |
| `#C77DFF` | Private sector (funding and facilities) |
| `#2A9D8F` | Primary clinics & facilities |
| `#E0C3FC` | Private sub-facilities (hospitals, labs, GPs, pharmacies) |
| `#A8DADC` | Secondary sub-services (Casualty, NCD, Inpatient, HIV, TB) |
| `#E76F51` | User fees / medicine costs |
| `#6A994E` | Free care / medicines; also community health workers |
| `#B7E4C7` | General OPD |
| `#52B788` | Community-based organisations |
| `#CCC5B9` | Informal/plural healthcare |

---

## Reference output

The derivation below was produced from the article text. Use it to verify your
output or as a starting point if regenerating from scratch.

### Nodes (24)

| id | label | shape | colour |
|---|---|---|---|
| GOVERNMENT FUNDING | GOVERNMENT FUNDING | diamond | #264653 |
| PARTNER FUNDING | PARTNER FUNDING | diamond | #00A896 |
| PRIVATE FUNDING | PRIVATE FUNDING | diamond | #C77DFF |
| PRIVATE SECTOR | PRIVATE SECTOR | rect | #C77DFF |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | rect | #264653 |
| PRIMARY CLINICS, FACILITIES | PRIMARY CLINICS, FACILITIES | rect | #2A9D8F |
| Hospitals | Hospitals | rounded | #E0C3FC |
| Laboratories | Laboratories | rounded | #E0C3FC |
| Clinics & GPs | Clinics & GPs | rounded | #E0C3FC |
| Retail pharmacies | Retail pharmacies | rounded | #E0C3FC |
| Casualty | Casualty | rounded | #A8DADC |
| NCD clinic(s) | NCD clinic(s) | rounded | #A8DADC |
| Inpatient facilities | Inpatient facilities | rounded | #A8DADC |
| HIV | HIV | rounded | #A8DADC |
| TB | TB | rounded | #A8DADC |
| User fees & medicine costs | User fees & medicine costs | ellipse | #E76F51 |
| Free care and medicines | Free care and medicines | ellipse | #6A994E |
| General OPD | General OPD | rounded | #B7E4C7 |
| User fees & medicine costs (Primary) | User fees & medicine costs (Primary) | ellipse | #E76F51 |
| Free care and medicines (Primary) | Free care and medicines (Primary) | ellipse | #6A994E |
| OTHER PARTNER-SUPPORTED HIV CLINICS | OTHER PARTNER-SUPPORTED HIV CLINICS | rect | #00A896 |
| Community health workers | Community health workers | circle | #6A994E |
| Community-based organisations | Community-based organisations | circle | #52B788 |
| Informal/plural healthcare | Informal/plural healthcare | circle | #CCC5B9 |

### Edges (30)

| From | To | Label |
|---|---|---|
| PRIVATE SECTOR | Hospitals | Private hospital |
| PRIVATE SECTOR | Laboratories | Private lab |
| PRIVATE SECTOR | Clinics & GPs | Private clinic/GP |
| PRIVATE SECTOR | Retail pharmacies | Pharmacy |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | PRIVATE SECTOR | Referral (private) |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Laboratories | Investigations |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | User fees & medicine costs | NCD user fees |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Casualty | Emergency |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | NCD clinic(s) | NCD management |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Inpatient facilities | Admission |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Free care and medicines | HIV/TB free care |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | HIV | HIV clinic |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | TB | TB clinic |
| SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | PRIMARY CLINICS, FACILITIES | Discharge (stable) |
| PRIMARY CLINICS, FACILITIES | Retail pharmacies | Medicine purchase |
| PRIMARY CLINICS, FACILITIES | SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Referral (NCD/complex) |
| PRIMARY CLINICS, FACILITIES | General OPD | NCD consultation |
| PRIMARY CLINICS, FACILITIES | User fees & medicine costs (Primary) | Urban NCD fees |
| PRIMARY CLINICS, FACILITIES | Free care and medicines (Primary) | HIV/rural free |
| PRIMARY CLINICS, FACILITIES | OTHER PARTNER-SUPPORTED HIV CLINICS | HIV (NGO) |
| OTHER PARTNER-SUPPORTED HIV CLINICS | SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Complex referral |
| Informal/plural healthcare | PRIMARY CLINICS, FACILITIES | Care-seeking |
| Community health workers | PRIMARY CLINICS, FACILITIES | HIV support |
| Community-based organisations | Community health workers | CHW deployment |
| GOVERNMENT FUNDING | SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | State budget |
| GOVERNMENT FUNDING | PRIMARY CLINICS, FACILITIES | State budget |
| PARTNER FUNDING | SECONDARY, TERTIARY, AND CENTRAL HOSPITALS | Donor (HIV/TB) |
| PARTNER FUNDING | PRIMARY CLINICS, FACILITIES | Donor (HIV) |
| PARTNER FUNDING | OTHER PARTNER-SUPPORTED HIV CLINICS | NGO funding |
| PRIVATE FUNDING | PRIVATE SECTOR | Private payment |

---

## Verification steps

After generating the three CSV files, verify with R:

```r
library(matxingraphout)

# Load node properties
nodes <- read.csv("Figure_2_nodes_prop.csv", stringsAsFactors = FALSE)
stopifnot(nrow(nodes) == 24)
stopifnot(all(c("id","label","shape","colour") %in% names(nodes)))

# Load adjacency matrix
adj_raw <- read.csv("Figure_2_adj.csv", check.names = FALSE,
                    stringsAsFactors = FALSE)
row_ids <- as.character(adj_raw[[1]])
adj     <- as.matrix(adj_raw[, -1])
rownames(adj) <- row_ids
storage.mode(adj) <- "numeric"
stopifnot(identical(dim(adj), c(24L, 24L)))
stopifnot(sum(adj) == 30)

# Load edge labels
elbl_raw <- read.csv("Figure_2_edges.csv", check.names = FALSE,
                     stringsAsFactors = FALSE)
elbl_ids <- as.character(elbl_raw[[1]])
elbl     <- as.matrix(elbl_raw[, -1])
rownames(elbl) <- elbl_ids
stopifnot(identical(dim(elbl), c(24L, 24L)))
stopifnot(sum(nzchar(trimws(elbl))) == 30)

# Render (manual layout requires x, y columns — add coordinates first, or
# switch to layout = "auto" for a quick topology check)
result <- graph_to_outputs(
  adj_matrix   = adj,
  node_props   = nodes,
  edge_labels  = elbl,
  directed     = TRUE,
  layout       = "auto",
  svg_file     = "Figure_2.svg",
  dot_file     = "Figure_2.dot",
  mermaid_file = "Figure_2.mmd"
)

cat("Type:", result$topology$type, "\n")
cat("Nodes:", result$topology$n_nodes,
    " Edges:", result$topology$n_edges, "\n")
```

Expected output:
```
Nodes: 24   Edges: 30
```
