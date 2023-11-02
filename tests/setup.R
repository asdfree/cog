# city hall marriage
# schools cops with fire water
# fed mail invite lost
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

# columns in all tables
column_intersect <- Reduce( intersect , lapply( gus_df_list , names ) )

# columns in some but not all tables
column_union <- unique( unlist( lapply( gus_df_list , names ) ) )

# these columns will be discarded by stacking
unique( unlist( lapply( lapply( gus_df_list , names ) , function( w ) column_union[ !column_union %in% w ] ) ) )

# stack all excel sheets, keeping only the columns that all tables have in common
gus_df <-
	Reduce( rbind , lapply( gus_df_list , function( w ) w[ column_intersect ] ) )
tf_apes <- tempfile()

apes_url <- "https://www2.census.gov/programs-surveys/apes/datasets/2022/2022%20COG-E%20Individual%20Unit%20Files.zip"

download.file( apes_url , tf_apes , mode = 'wb' )

unzipped_files_apes <- unzip( tf_apes , exdir = tempdir() )

xlsx_apes_fn <- grep( "\\.xlsx$" , unzipped_files_apes , value = TRUE )

apes_tbl <- read_excel( xlsx_apes_fn )

apes_df <- data.frame( apes_tbl )

names( apes_df ) <- tolower( names( apes_df ) )
# all DEP School Districts and a third of Special Districts are not in the apes_df
table(
	gus_df[ , 'census_id_gidid' ] %in% apes_df[ , 'individual.unit.id' ] ,
	gus_df[ , 'source_tab' ] ,
	useNA = 'always'
)

# state governments are not in the gus_df
table(
	apes_df[ , 'individual.unit.id' ] %in% gus_df[ , 'census_id_gidid' ] ,
	apes_df[ , 'type.of.government' ] ,
	useNA = 'always'
)

# check for overlapping field names
( overlapping_names <- intersect( names( apes_df ) , names( gus_df ) ) )

# rename the state column in `gus_df` to state abbreviation
names( gus_df )[ names( gus_df ) == 'state' ] <- 'stateab'

cog_df <-
	merge(
		apes_df ,
		gus_df ,
		by.x = 'individual.unit.id' ,
		by.y = 'census_id_gidid' ,
		all.x = TRUE
	)

stopifnot( nrow( cog_df ) == nrow( apes_df ) )
tapply( cog_df$full.time.employees , grepl('Total',cog_df$government.function),sum)
# FALSE TRUE 
# 14944806 14944806 
# cog_fn <- file.path( path.expand( "~" ) , "COG" , "this_file.rds" )
# saveRDS( cog_df , file = cog_fn , compress = FALSE )
# cog_df <- readRDS( cog_fn )
cog_df <- 
	transform( 
		cog_df , 
		
		cbsa_indicator_code = 
			factor( 
				as.numeric( f1406720 ) , 
				levels = 0:2 ,
				labels = c( "not metro" , "metro" , "micro" ) 
			) ,
			
		mhi_2020 = f1322620 ,
		
		whole_county_hpsa_2022 = as.numeric( f0978722 ) == 1 ,
		
		census_region = 
			factor( 
				as.numeric( f04439 ) , 
				levels = 1:4 ,
				labels = c( "northeast" , "midwest" , "south" , "west" ) 
			)

	)
	
nrow( cog_df )

table( cog_df[ , "cbsa_indicator_code" ] , useNA = "always" )
mean( cog_df[ , "mhi_2020" ] , na.rm = TRUE )

tapply(
	cog_df[ , "mhi_2020" ] ,
	cog_df[ , "cbsa_indicator_code" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( cog_df[ , "census_region" ] ) )

prop.table(
	table( cog_df[ , c( "census_region" , "cbsa_indicator_code" ) ] ) ,
	margin = 2
)
sum( cog_df[ , "mhi_2020" ] , na.rm = TRUE )

tapply(
	cog_df[ , "mhi_2020" ] ,
	cog_df[ , "cbsa_indicator_code" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( cog_df[ , "mhi_2020" ] , 0.5 , na.rm = TRUE )

tapply(
	cog_df[ , "mhi_2020" ] ,
	cog_df[ , "cbsa_indicator_code" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_cog_df <- subset( cog_df , f12424 == "CA" )
mean( sub_cog_df[ , "mhi_2020" ] , na.rm = TRUE )
var( cog_df[ , "mhi_2020" ] , na.rm = TRUE )

tapply(
	cog_df[ , "mhi_2020" ] ,
	cog_df[ , "cbsa_indicator_code" ] ,
	var ,
	na.rm = TRUE 
)
t.test( mhi_2020 ~ whole_county_hpsa_2022 , cog_df )
this_table <- table( cog_df[ , c( "whole_county_hpsa_2022" , "census_region" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		mhi_2020 ~ whole_county_hpsa_2022 + census_region , 
		data = cog_df
	)

summary( glm_result )
stopifnot( nrow( cog_df ) == 3232 )
library(dplyr)
cog_tbl <- as_tibble( cog_df )
cog_tbl %>%
	summarize( mean = mean( mhi_2020 , na.rm = TRUE ) )

cog_tbl %>%
	group_by( cbsa_indicator_code ) %>%
	summarize( mean = mean( mhi_2020 , na.rm = TRUE ) )
library(data.table)
cog_dt <- data.table( cog_df )
cog_dt[ , mean( mhi_2020 , na.rm = TRUE ) ]

cog_dt[ , mean( mhi_2020 , na.rm = TRUE ) , by = cbsa_indicator_code ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'cog' , cog_df )
dbGetQuery( con , 'SELECT AVG( mhi_2020 ) FROM cog' )

dbGetQuery(
	con ,
	'SELECT
		cbsa_indicator_code ,
		AVG( mhi_2020 )
	FROM
		cog
	GROUP BY
		cbsa_indicator_code'
)
