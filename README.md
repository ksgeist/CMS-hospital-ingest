# Developing a Reproducible Computational Pipeline for Hospital-Level Readmissions Analysis Using Routinely-Collected CMS Data
Katie Geist

This ingest pipeline uses the annually reported hospital-level readmissions data collected under the the federal [Hospital Readmissions Reduction Program (HRRP)](https://www.cms.gov/medicare/quality/value-based-programs/hospital-readmissions), which is intended to incentivize hospital-provided patient care and healthcare quality through reimbursement penalties. 

The goal of this project was to develop a flexible computational workflow. CMS hospital-level aggregates of readmissions are reported annually, by each hospital, and across 6 difference readmission metrics:

- Pneumonia (PN)
- Chronic Obstructive Pulmonary Disease (COPD)
- Heart Failure (HF)
- Acute Myocardial Infarction (AMI)
- Coronary Artery Bypass Graft (CABG)
- Total Hip/Knee Arthroplasty (HIP-KNEE)
- Sample pipeline for CMS hospital files from API

## Brief Methods
This ingest pipeline retrieves all hospital-level records programmatically from the [CMS Provider Data Catalog API](https://data.cms.gov/provider-data/about/faq). I made this choice to avoid temporal structure (longitudinal records) for an analytical pipeline under development.

After ingestion and parsing of the JSON files, the pipeline begins handling the various data structures and formatting, including:

- long vs. wide format ingestion
- small numbers reporting
- missing values
- type conversion
- variable extraction

**Currently the pipeline is not testable end-to-end.** I have it through ingestion and cleanup, but the merges into a final, single dataset are incomplete. 

Future work is to include completing readiness for spatial structure analysis (since data are collected from across every state in the U.S.), as well as machine learning to predict readmissions ratios based on user input and the recommendations from the spatial structure analyses.

Final deployment will be through a Shiny application. 
