substantive question
===

* how can we understand what global trade patterns look like, in the presence of a china that is more active in africa

	- that speaks to the importance of the network context, just understanding bilateral trade stuff does not get you that far

	- counterfactual with africa: what happens if africa became more active, even if each african country grew only 10\% over the next couple of years, what predictions would we make

	- does this speak to what countries are most likely to engate in disputes at the WTO

* complicated interdependencies in the world...milner

* kantian triad...endogenous

dyadic trade model:
===

* dependent variable:
	- exports: IMF
		+ need to go back and regather data for:
> unique(unlist(lapply(strsplit(tmp, '_'), function(x) x[1])))
 [1] "KOREA, REPUBLIC OF" "SENEGAL"            "SEYCHELLES"
 [4] "SIERRA LEONE"       "SINGAPORE"          "SLOVAKIA"
 [7] "SLOVENIA"           "SOLOMON ISLANDS"    "SOMALIA"
[10] "SOUTH AFRICA"       "SPAIN"              "SRI LANKA"
[13] "SUDAN"

* monadic covariates:

	- polity of both countries, gathered monthly level scores from CRISP

	- gdp of both countries, gathered from World Bank in constant 2005 US dollars

	- population of both countries

	- spatial distance trade variable, created by weighting trade with distance measures from cshapes
		+ Will add this later. To create this I first need to summarize the level of total exports each indiv country receives in a given month and then for each country I would weight by the total exports their "neighbors" received. Likely overkill in this analysis so for now lets just forget about it, and we should already be able to capture this kind of geographic interdependency through our overall modeling approach.

	- wto membership
		+ no point in collecting, most all countries gained entry into the WTO on the same date
		+ https://www.wto.org/english/thewto_e/whatis_e/tif_e/org6_e.htm

* dyadic disputes:

	- alliance, gathered from COW

	- geographic proximity, gathered from cshapes

	- verbal conflict/material conflict

model formulation:
===

* mltr

* additive approach

other todos:
===

* set up the four quad variables at the monthly level for all the countries and then include the quad variables for the domestic within each country

* the cross-sections should be populated by domestic events
