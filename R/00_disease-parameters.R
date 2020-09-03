# ==============================================================================
# * Parameter references for pandemic 2009 H1N1:
# ==============================================================================
#
# Pourbohloul B, Ahued A, Davoudi B, et al. Initial human transmission dynamics
# of the pandemic (H1N1) 2009 virus in North America. Influenza Other Respi
# Viruses 2009; 3: 215–22.
# 
# Tuite, Ashleigh R., Amy L. Greer, Michael Whelan, Anne-Luise Winter,
# Brenda Lee, Ping Yan, Jianhong Wu, et al. 2010. “Estimated Epidemiologic
# Parameters and Morbidity Associated with Pandemic H1N1 Influenza.”
# CMAJ: Canadian Medical Association Journal =
# Journal de l’Association Medicale Canadienne 182 (2): 131–36.
# 
# Tuite, Ashleigh R., Amy L. Greer, Michael Whelan, Anne-Luise Winter, Brenda
# Lee, Ping Yan, Jianhong Wu, et al. 2010. “Estimated Epidemiologic Parameters
# and Morbidity Associated with Pandemic H1N1 Influenza.” CMAJ: Canadian
# Medical Association Journal = Journal de l’Association Medicale Canadienne
# 182 (2): 131–36.
#
# Longini, Ira M., Jr, Azhar Nizam, Shufu Xu, Kumnuan Ungchusak, Wanna
# Hanshaoworakul, Derek A. T. Cummings, and M. Elizabeth Halloran. 2005.
# “Containing Pandemic Influenza at the Source.” Science 309 (5737): 1083–87.
# 
# 
# currently unused: https://www.ncbi.nlm.nih.gov/pubmed/19545404
# -----------------------------------------------------------------------------

# ==============================================================================
# * Disease parameters
r0 = 1.44 # Pourbohloul
latent_period = 2.62 # Tuite
inf_period = 3.38 # Tuite
mu = 1/inf_period
tau = 3 # Return rate
r_beta = 0.50 # Longini 2005
p_a = 1/3 # Lonigni 2005

beta = (r0 * mu)/((r_beta * p_a) + (1 - p_a)) # from balcan pg 143
