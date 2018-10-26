Fact Documentation
================
Naphlin Peter Akena
10/23/2018

Background
----------

The DDH fact schema is composed mainly of oecd datasets. To create the fact schema, we will be working with crs, dac1,dac2a,dac2b and dac5 datasets from oecd, the field descriptions can be found on official oecd website can be found on the

oda dataset
-----------

#### Assumptions

-   Mapping table between oecd donors codes and DI donor codes is up to date (*dimension.oecd\_donor\_to\_di\_id\_map*)
-   Mapping table between oecd recipient codes and DI recipient codes is up to date (*dimension.oecd\_recipient\_to\_di\_id\_map*)
-   Mapping table betwween oecd crs sector codes and DI sector codes is up to date (*oecd\_crs\_sector\_code\_3\_digit\_to\_itep\_sector\_web\_id\_map*)
-   Mapping between CRS bundle codes and DI ITEP channel codes should be upto date (*oecd\_crs\_channel\_code\_5\_digit\_to\_itep\_channel\_web\_id\_map and crs\_channel\_code\_5\_digit\_to\_di\_itep\_channel\_map*)
-   The DI bundle codes must be upto date

To generate oda dataset, we will extract the fields belowfrom CRS data, most of the fields are self explanatory except a few

(year, donor\_code, recipient\_code, flow\_code, category, finance\_type, aid\_type, usd\_disbursement, short\_description, purpose\_code, sector\_code, channel\_code, long\_description, ftc, pba)

### Important Notes

Of importance in filtering crs data are

1.  category

Category indicates type of flow eg. ODA, Private flows etc. In our case, we are only looking for offical flows (ODA). The code for this is 10

1.  flow\_code

Only 3 flows will be considered the filtering

| flow\_code | flow\_name        |
|------------|-------------------|
| 19         | Equity Investment |
| 13         | ODA Loans         |
| 11         | ODA Grants        |

Including Plots
---------------

You can also embed plots, for example:
