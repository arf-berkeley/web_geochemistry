install :
	R -e "roxygen2::roxygenize()"
	R CMD Rd2pdf --force --no-preview --output="./man/web_geochemistry.pdf" --title="web\_geochemistry" man/*.Rd

no-build:
	R -e "roxygen2::roxygenize(); warnings()"