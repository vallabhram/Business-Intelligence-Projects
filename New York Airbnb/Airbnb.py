import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime, timedelta

## Load the dataset. 
file_path = 'C://Vallabh/SELF/Misc/New York Airbnb/new_york_listings_2024.csv'
df = pd.read_csv(file_path)
df2 = df[df['availability_365'] != 0] # Exlcuding rows with no availability 

## Get the number of records and column names.
num_records = len(df)
column_names = df.columns.tolist()
num_records, column_names

## Data types of each column.
data_types = df.dtypes
data_types

## Check for missing values in each column.
missing_values = df.isnull().sum()
columns_with_missing_values = missing_values[missing_values > 0]
columns_with_missing_values

## Summary.
df.describe(include = 'number')
df2.describe()

## Minimum and Maximum Prices (Price Range).
price_range = df['price'].agg(['min', 'max'])
price_range

## Average price of listing per borough, and overall average price.
avg_price_per_borough = df.groupby('borough')['price'].mean()
avg_price_per_borough
avg_price = df['price'].mean()
avg_price

## Number of listings per borough and neighborhood.
borough_count = df['borough'].nunique()
borough = df.groupby('borough').size()
neighborhood_count = df['neighbourhood'].nunique()
neighborhood = df.groupby('neighbourhood').size()
borough_count, borough
neighborhood_count, neighborhood
df['borough'].value_counts()

## Count of each room type.
room_type_counts = df['room_type'].value_counts()
room_type_counts

## Number of hosts per borough, and average listings ber host.
hosts_per_borough = df.groupby('borough')['host_id'].nunique()
hosts_per_borough
avg_listings_per_host = df.groupby('host_id')['listing_id'].count().mean()
avg_listings_per_host

## Count of listings with a license.
license_count = df['license'].notnull().sum()
license_count

## Display the distribution of ratings.
rating_distribution = df['rating'].value_counts()
rating_distribution

## Correlation Matrix Plot
numerical_columns = df.select_dtypes(include='number')
correlation_matrix = numerical_columns.corr()
# Heatmap
plt.figure(figsize=(10, 6))
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', fmt=".2f", linewidths=0.5)
plt.title('Correlation Matrix of Numerical Features')
plt.xticks(rotation = 25)
plt.show()
# The two comparisons that seem important are beds vs bedrooms and the number of reviews vs reviews per month, and we can't really draw any conclusions from these.

## Bar plot for number of listings, and the average price per borough.
plt.figure(figsize=(10, 6))
sns.countplot(x='borough', data=df, palette='viridis', order=borough.index)
for i, count in enumerate(borough):
    plt.text(i, count+100, str(count), ha='center', va='center', fontsize=10)
    plt.text(i, count-250, f'Avg: ${avg_price_per_borough.iloc[i]:.2f}', ha='center', va='bottom', fontsize=8, weight='bold', color='white')
plt.title('Count of listings in each borough')
plt.show()

## Box plot for price ber borough.
sns.boxplot(x='borough', y='price', data=df, palette='viridis')
plt.show()
# Removing the outlier and plotting again.
plt.figure(figsize=(10, 6))
df_nooutlier = df[(df['price'] != 100000)]
sns.boxplot(x='borough', y='price', data=df_nooutlier, palette='viridis')
plt.title('Price Distribution per Borough (Excluding Outliers)')
plt.tight_layout()
plt.show()

## % of listings having more than 4, and 4.5 rating.
df_for_rating0 = df[df['rating'] != 0]
df_for_rating1 = df[(df['rating'] > 0) & (df['rating'] < 4)]
df_for_rating2 = df[(df['rating'] > 0) & (df['rating'] < 4.5)]
new_listings_count = df.shape[0] - df_for_rating2.shape[0]
new_listings_count # 3752 listings have no ratings (could be new listings too).
lessthan4ratingpercentage = (df_for_rating1.groupby('borough')['rating'].count().sum() / df_for_rating0.groupby('borough')['rating'].count().sum()) * 100
100 - lessthan4ratingpercentage # 97.75% listings (excluding 3752 listings with no ratings) have 4+ rating.
lessthan4dot5ratingpercentage = (df_for_rating2.groupby('borough')['rating'].count().sum() / df_for_rating0.groupby('borough')['rating'].count().sum()) * 100
100 - lessthan4dot5ratingpercentage # 86% listings (excluding 3752 listings with no ratings) have 4.5+ rating.

####
####
## License vs Non-licensed/exempted listings avg rating
df_rating = df[df['rating'] != 0] # (Exlcuding non-rated/new listings)
df_rating[df_rating['license'] == 'Licensed']['rating'].mean()
df_rating[df_rating['license'] != 'Licensed']['rating'].mean()
## When licensed, the average rating is 4.86, but when not licensed or exempted, the average rating is 4.73 (cx are unaware of whether a rating is licensed or not).

## % of listings having its last review given before a 100 days, 60 days while availability > 0, (this value changes every day due to change in date).
days100 = datetime.now().date() - timedelta(days = 100)
days100 = days100.strftime('%m/%d/%Y')
days100
(len(df2[df2['last_review'] < days100]) / len(df2))*100

# ~11% listings were last reviewed more than 100 days ago. 
days60 = datetime.now().date() - timedelta(days = 60)
days60 = days60.strftime('%m/%d/%Y')
days60
len(df2[df2['last_review'] < days60])
 / len(df2))*100
# ~31% listings were last reviewed more than 60 days ago.
days30 = datetime.now().date() - timedelta(days = 30)
days30 = days30.strftime('%m/%d/%Y')
days30
(len(df2[df2['last_review'] >= days3]) / len(df2))*100
# In the last 30 days, ~48% listings were reviewed.
####
####

## Avg rating of hosts with most listings (top 10) excluding listings which are non-rated or are new.
hosts_with_most_listings = df_rating.groupby(['host_id','host_name'])['listing_id'].agg(['count']).nlargest(10, 'count')
hosts_with_most_listings
top10_listings_avg_rating = []
for host_id, host_name in hosts_with_most_listings.index:
    host_listings_count = hosts_with_most_listings.loc[(host_id, host_name), 'count']
    avg_rating = df_rating[df_rating['host_id'] == host_id]['rating'].mean()
    top10_listings_avg_rating.append((host_id,host_name,host_listings_count,avg_rating))
hosts_with_most_listings_avg_rating = pd.DataFrame(top10_listings_avg_rating, columns = ['host_id','host_name','host_listing_count','avg_rating'])
hosts_with_most_listings_avg_rating
hosts_with_most_listings_avg_rating['avg_rating'].mean()
# Top 10 hosts with the most listings have an average rating of ~ 4.6.

## Bedroom analysis
(df.groupby('bedrooms')['price'].count() / df['price'].count())* 100
(df.groupby(['borough','bedrooms'])['price'].count() / df['price'].count()) * 100
# 64.74% of listings have only 1 bedroom.
# Only 2.45% listings have > 3 bedrooms.

####
####
df.groupby('bedrooms')['beds'].value_counts()
len(df[(df['beds'] / df['bedrooms'] > 2) & (df['bedrooms'] > 0)]); len(df[(df['beds'] / df['bedrooms'] > 3) & (df['bedrooms'] > 0)])
len(df[(df['beds'] / df['bedrooms'] > 4) & (df['bedrooms'] > 0)]); len(df[(df['beds'] / df['bedrooms'] > 6) & (df['bedrooms'] > 0)])
# 633 listings have more than 2 beds per bedroom, and 126 listings have more than 3 beds per bedroom.
# 25 listings have more than 4 beds per bedroom, and 10 listings have more than 5 beds per bedroom.
# One listing has the largest amount of beds (9 beds) per bedroom, while another one has 42 beds in total.
####
####

### Regression was conducted on R, which makes it simple to retrieve p-values with coefficients. 
### To improve understanding and obtain new insights, two separate regressions were performed with Price and Rating as the independent variables.

## Creating a new column df_rating_availability
df_rating_availability = df[(df['availability_365'] != 0) & (df['rating'] != 0)] # Exlcuding records with availability > 0 and are not new or non-rated.

####
####
## Adding a new column no_of_days to the df_rating_availability
todays_date = datetime.now().date().strftime('%Y-%m-%d')
df_rating_availability['todays_date'] = todays_date
df_rating_availability['todays_date'] = pd.to_datetime(df_rating_availability['todays_date'])
df_rating_availability['last_review']
df_rating_availability['no_of_days'] = df_rating_availability['todays_date'] - df_rating_availability['last_review']
df_rating_availability['no_of_days']
df_rating_availability.groupby('borough')['no_of_days'].max()
df_rating_availability.groupby(['borough','price'])['no_of_days'].mean() #NaN
# There is at least one property from each borough that was last reviewed more than 5.5 years ago
####
####

## Rating Analysis
df_rating_availability.groupby('borough')['rating'].count()
df_rating_availability.groupby('borough')['rating'].mean()
df[(df['availability_365'] != 0) & (df['rating'] == 0)].groupby('borough')['rating'].count()
# When properties are more available, and though Brooklyn has lesser newer listings(1046 to 1625), it has higher overall review cout(5783) and higher overall
# rating(4.77) compared to Manhattan(5359 review count and 4.77 rating)
df_rating_availability.groupby('borough')['availability_365']
# Brooklyn availability avg = 223 days / year, Manhattan availability avg = 227 days / year
df.groupby('room_type')['rating'].mean()
# For all the listings in the dataset, rating is the highest when the entire space (private space) is rented out compared to hotel/shared space
df_rating_availability.groupby('borough')['rating'].max()
df_rating_availability.groupby('borough')['rating'].min()
df_rating_availability[df_rating_availability['rating'] == 5].groupby('borough')['rating'].count()
df_rating_availability[df_rating_availability['rating'] <= 2].groupby('borough')['rating'].count()
# Manhattan has the most least rated listings while Brooklyn has the most highest rated listings.

## Price Analysis
df_rating_availability.groupby('borough')['price'].mean()
df_rating_availability.groupby('room_type')['price'].mean()
# The average price of hotel room listings in New York City exceeds the average of all other room types combined. 
# Based on the rating, even if the average cost of hotel rooms is higher, privately operated Airbnbs offer better services.









