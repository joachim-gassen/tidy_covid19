TARGETS := data

.phony: data gtrends_data all clean

all: $(TARGETS)

clean:
	rm -f -r data/*

data: data/jh_covid19_ctry_level.csv data/acaps_npi.csv \
	data/wbank.csv data/merged.csv gtrends_data

data/jh_covid19_ctry_level.csv: code/import_jhu_csse_covid19_data.R
	Rscript code/import_jhu_csse_covid19_data.R
	
data/acaps_npi.csv: code/import_acaps_npi_data.R
	Rscript code/import_acaps_npi_data.R

data/wbank.csv: code/import_wbank_data.R
	Rscript code/import_wbank_data.R

gtrends_data: code/import_google_trends_data.R
	Rscript code/import_google_trends_data.R


data/merged.csv: code/merge_data.R \
	data/jh_covid19_ctry_level.csv \
	data/acaps_npi.csv data/wbank.csv gtrends_data
	Rscript code/merge_data.R \ 
	@echo Pulling data completed. \
		You can now source \'code/descriptive_analyses.R\' if you want.
	