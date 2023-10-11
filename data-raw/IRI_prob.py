
# IRI only works on python versions less than 3.10 -- using 3.9.16 for this code


# Download proability forecast
from ochanticipy import create_country_config, CodAB, GeoBoundingBox, \
                      IriForecastDominant, IriForecastProb

country_config = create_country_config(iso3="som")

codab = CodAB(country_config=country_config)
codab.download()
admin0 = codab.load(admin_level=0)

geo_bounding_box = GeoBoundingBox.from_shape(admin0)


iri_prob = IriForecastProb(country_config=country_config,
                           geo_bounding_box=geo_bounding_box)
iri_prob.download( clobber=True)
iri_prob.process(clobber = True)
# iri_prob_data = iri_prob.load()


# Download dominant tercile data

IriForecastDominant
iri_dom = IriForecastDominant(country_config=country_config,
                           geo_bounding_box=geo_bounding_box)
iri_dom.download(clobber=True)
iri_dom.process(clobber=True)
# iri_prob_data = iri_prob.load()
