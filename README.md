# ShinyDisVar

An R package integrated with Shiny for user-friendly web applications in disease variant identification from large-scale personal genetic information.

## Overview

ShinyDisVar is a web-based application that enables researchers and clinicians to identify disease-associated genetic variants from large-scale genomic data. Built on the R Shiny framework, it provides an accessible platform for analyzing genetic variants without requiring high-end hardware or extensive bioinformatics expertise.

## Key Features

- **Multi-database Integration**: Searches across six major genetic disease databases simultaneously
- **Large-scale Analysis**: Handles up to 9.5 million variants from a single VCF file
- **Latest Reference Genome**: Supports GRCh38/hg38 reference genome
- **User-friendly Interface**: Web-based application with interactive visualizations
- **Fast Processing**: Processes whole-genome VCF files in less than a minute
- **Cross-platform**: Server-based architecture accessible from any device with a web browser

## Supported Databases

ShinyDisVar integrates the following genetic disease databases:

1. **GWAS Catalog** - Gold standard for genome-wide association studies
2. **GWASdb** - Enhanced functional annotations and regulatory information
3. **GRASP** - Over 8.87 million SNP-phenotype associations
4. **GADCDC** - Clinically focused associations
5. **Johnson and O'Donnell's Database** - Freely accessible results with detailed statistics
6. **ClinVar** - Clinical-grade pathogenic and benign classifications

## Access the Application

ShinyDisVar is available as a web application at:
**https://bioinformatics-shinydisvar.shinyapps.io/ShinyDisVar/**

No installation required - simply visit the link and start analyzing your genetic data!

### Input Data

ShinyDisVar accepts Variant Call Format (VCF) files:
- Supported formats: `.vcf` and `.vcf.gz`
- Compatible with GRCh38/hg38 reference genome
- Handles whole-genome sequencing data

### Quick Start Guide

1. **Upload VCF File**: Click "Browse" to upload your `.vcf` or `.vcf.gz` file
   - *Note: Compression (.vcf.gz) is recommended for large VCF files*
2. **Select Databases**: Choose at least one database for variant analysis
3. **Set P-value**: Enter your desired significance threshold (e.g., 1e-8)
4. **Run Analysis**: Click the "Run Analysis" button to start processing
5. **View Results**: Interactive tables and visualizations
6. **Download Results**: Export results in TSV format

### File Format Requirements

- Standard VCF format (v4.0 or later)
- Must contain standard headers
- Can be compressed (.gz) or uncompressed
- Maximum file size: 9GB

## Performance

ShinyDisVar demonstrates excellent performance with large-scale genomic data:

- **Reading time**: 15-24 seconds for VCF files with 3.87-4.74 million variants
- **Processing time**: 27-35 seconds for variant analysis
- **Total processing time**: 42-59 seconds per whole-genome sample
- **Scalability**: Handles up to 9.5 million variants per VCF file

## Comparison with DisVar

| Tool | Max Variants | Batch Processing | GRCh38 Support | Web Interface |
|------|:-------------:|:----------------:|:--------------:|:-------------:|
| ShinyDisVar | 9.5M | Single file | ✅ | ✅ |
| DisVar (R pkg) | 11M | Single file | ✅ | ❌ |
| DisVar (R pkg) | 138M (batch) | Multiple files | ✅ | ❌ |

## Output

Results include:
- **Interactive Tables**: Variant details with hyperlinked IDs to NCBI's SNP database
- **Visualizations**: Bar plots showing variant distribution across databases
- **Disease Associations**: Top disease associations with statistical significance
- **Downloadable Reports**: TSV format with comprehensive variant information

## Citation

If you use ShinyDisVar in your research, please cite:

## Related Work

This tool builds upon the [DisVar R package](https://github.com/Khunanon-Chanasongkhram/DisVar) for command-line variant analysis.

---

**Keywords**: genetic variants, disease association, GWAS, genomics, bioinformatics, R, Shiny, web application
