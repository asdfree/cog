# courthouse steps wedding
# schools police fire water
# no fed mail invite
library(readxl)

tf_gus <- tempfile()

gus_url <- "https://www2.census.gov/programs-surveys/gus/datasets/2022/govt_units_2022.ZIP"

download.file( gus_url , tf_gus , mode = 'wb' )

unzipped_files_gus <- unzip( tf_gus , exdir = tempdir() )

xlsx_gus_fn <- grep( "\\.xlsx$" , unzipped_files_gus , value = TRUE )

xlsx_sheets <- excel_sheets( xlsx_gus_fn )

# read all sheets into a list of tibbles
gus_tbl_list <- lapply( xlsx_sheets , function( w ) read_excel( xlsx_gus_fn , sheet = w ) )

# convert all tibbles to data.frame objects
gus_df_list <- lapply( gus_tbl_list , data.frame )

# lowercase all column names
gus_df_list <-
	lapply( 
		gus_df_list , 
		function( w ){ names( w ) <- tolower( names( w ) ) ; w } 
	)

# add the excel tab source to each data.frame
for( i in seq( xlsx_sheets ) ) gus_df_list[[ i ]][ , 'source_tab' ] <- xlsx_sheets[ i ]

# determine which columns are in all tables
column_intersect <- Reduce( intersect , lapply( gus_df_list , names ) )

# determine which columns are in some but not all tables
column_union <- unique( unlist( lapply( gus_df_list , names ) ) )

# these columns will be discarded by stacking:
unique(
	unlist(
		lapply(
			lapply( gus_df_list , names ) , 
			function( w ) column_union[ !column_union %in% w ]
		)
	)
)

# stack all excel sheets, keeping only the columns that all tables have in common
gus_df <- Reduce( rbind , lapply( gus_df_list , function( w ) w[ column_intersect ] ) )
tf_apes <- tempfile()

apes_url <-
	paste0(
		"https://www2.census.gov/programs-surveys/apes/datasets/" ,
		"2022/2022%20COG-E%20Individual%20Unit%20Files.zip"
	)

download.file( apes_url , tf_apes , mode = 'wb' )

unzipped_files_apes <- unzip( tf_apes , exdir = tempdir() )

xlsx_apes_fn <- grep( "\\.xlsx$" , unzipped_files_apes , value = TRUE )

apes_tbl <- read_excel( xlsx_apes_fn )

apes_df <- data.frame( apes_tbl )

names( apes_df ) <- tolower( names( apes_df ) )
# all DEP School Districts and a third of Special Districts are not in the `apes_df`
table(
	gus_df[ , 'census_id_gidid' ] %in% apes_df[ , 'individual.unit.id' ] ,
	gus_df[ , 'source_tab' ] ,
	useNA = 'always'
)

# state governments are not in the `gus_df`
table(
	apes_df[ , 'individual.unit.id' ] %in% gus_df[ , 'census_id_gidid' ] ,
	apes_df[ , 'type.of.government' ] ,
	useNA = 'always'
)

# check for overlapping field names:
( overlapping_names <- intersect( names( apes_df ) , names( gus_df ) ) )

# rename the state column in `gus_df` to state abbreviation
names( gus_df )[ names( gus_df ) == 'state' ] <- 'stateab'

double_df <-
	merge(
		apes_df ,
		gus_df ,
		by.x = 'individual.unit.id' ,
		by.y = 'census_id_gidid' ,
		all.x = TRUE
	)

stopifnot( nrow( double_df ) == nrow( apes_df ) )

# replace dots with underscores
names( double_df ) <- gsub( "\\." , "_" , names( double_df ) )
# `Total - All Government Employment Functions` records sum to the same as all other records:
with( double_df , tapply( full_time_employees , grepl( "Total" , government_function ) , sum ) )

with( double_df , tapply( part_time_payroll , grepl( "Total" , government_function ) , sum ) )

# keep one record per government function (multiple records per agency):
cog_df <- subset( double_df , !grepl( "Total" , government_function ) )

# keep one record per government agency:
# cog_df <- subset( double_df , grepl( "Total" , government_function ) )
# cog_fn <- file.path( path.expand( "~" ) , "COG" , "this_file.rds" )
# saveRDS( cog_df , file = cog_fn , compress = FALSE )
# cog_df <- readRDS( cog_fn )
cog_df <- 
	transform( 
		cog_df , 
		
		one = 1 ,
		
		total_payroll = full_time_payroll + part_time_payroll ,
		
		total_employees = full_time_employees + part_time_employees ,

		any_full_time_employees = full_time_employees > 0
	)
nrow( cog_df )

table( cog_df[ , "type_of_government" ] , useNA = "always" )
mean( cog_df[ , "full_time_employees" ] )

tapply(
	cog_df[ , "full_time_employees" ] ,
	cog_df[ , "type_of_government" ] ,
	mean 
)
prop.table( table( cog_df[ , "census_region" ] ) )

prop.table(
	table( cog_df[ , c( "census_region" , "type_of_government" ) ] ) ,
	margin = 2
)
sum( cog_df[ , "full_time_employees" ] )

tapply(
	cog_df[ , "full_time_employees" ] ,
	cog_df[ , "type_of_government" ] ,
	sum 
)
quantile( cog_df[ , "full_time_employees" ] , 0.5 )

tapply(
	cog_df[ , "full_time_employees" ] ,
	cog_df[ , "type_of_government" ] ,
	quantile ,
	0.5 
)
sub_cog_df <- subset( cog_df , grepl( 'Education' , government_function ) )
mean( sub_cog_df[ , "full_time_employees" ] )
var( cog_df[ , "full_time_employees" ] )

tapply(
	cog_df[ , "full_time_employees" ] ,
	cog_df[ , "type_of_government" ] ,
	var 
)
t.test( full_time_employees ~ any_full_time_employees , cog_df )
this_table <- table( cog_df[ , c( "any_full_time_employees" , "census_region" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		full_time_employees ~ any_full_time_employees + census_region , 
		data = cog_df
	)

summary( glm_result )
financial_admin_df <- subset( cog_df , government_function == 'Financial Administration' )

stopifnot( sum( financial_admin_df[ , 'full_time_employees' ] ) == 404228 )
library(dplyr)
cog_tbl <- as_tibble( cog_df )
cog_tbl %>%
	summarize( mean = mean( full_time_employees ) )

cog_tbl %>%
	group_by( type_of_government ) %>%
	summarize( mean = mean( full_time_employees ) )
library(data.table)
cog_dt <- data.table( cog_df )
cog_dt[ , mean( full_time_employees ) ]

cog_dt[ , mean( full_time_employees ) , by = type_of_government ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'cog' , cog_df )
dbGetQuery( con , 'SELECT AVG( full_time_employees ) FROM cog' )

dbGetQuery(
	con ,
	'SELECT
		type_of_government ,
		AVG( full_time_employees )
	FROM
		cog
	GROUP BY
		type_of_government'
)
