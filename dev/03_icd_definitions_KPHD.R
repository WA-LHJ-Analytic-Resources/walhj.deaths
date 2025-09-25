# ==============================================================================
# scripts/03_icd_definitions.R - ICD code pattern definitions
# ==============================================================================

# INJURY DEFINITIONS
# To be used with any contributing cause (recordaxiscode1 - recordaxiscode20)
k_injury <- '([VWX]..|Y[0|1|2].|Y3[0-6]|Y8[5-7|9]|U0[1-3])'
k_injury_unint <- '([VW]..|X[0-5].|Y8[5-6])'
k_injury_suicide <- '(X[6-7].|X8[0-4]|Y870|U03)'
k_injury_homicide <- '(X8[5-9]|X9.|Y0.|Y871|U0[1-2])'
k_injury_legalwar <- '(Y3[5-6]|Y89[0-1])'
k_injury_undetermined <- '(Y[1-2].|Y3[0-4]|Y872|Y899)'
k_fall <- '(W[0-1].)'
k_firearm <- '(W3[2-4]|X7[2-4]|X9[3-5]|Y2[2-4]|Y350|U014)'
k_transportation <- '(V..|X82|Y03|Y32|Y361|U011)'
k_drowning <- '(W6[5-9]|W7[0-4]|X71|Y92|Y21)'

# SUBSTANCE USE DEFINITIONS
# To be used with only recordaxiscode1
k_drug <- '(X4[0-4]|X6[0-4]|X85|Y1[0-4])'

# To be used with any contributing cause (recordaxiscode1 - recordaxiscode20)
k_opioid <- '(T40[0-4|6])'
k_heroin <- "T401"
k_synth_non_methadone <- "T404"
k_cocaine <- "T405"
k_stimulant <- '(T405|T436)'

# Polysubstance use trends
k_prescribedOp <- '(T402|T403)'
k_otherOpioid <- '(T400|T406)'
k_stim_non_cocaine <- "T436"
k_sedative <- "T42"
k_psychotropic <- '(T43[0-1|3-5])'
k_alcohol <- "T51"
k_otherDrugs <- '(T40[0-6]|T436|T42|T430|T431|T43[3-5]|T51)'
k_drugCatch <- '(T3[6-9]|T[4-5])'

# WHO 2019 Chapters
a00_b99 <- '([AB]..)'
c00_d48 <- '(C..|D[0-3].|D4[0-8])'
d50_d89 <- '(D[5-8].)'
e00_e90 <- '(E[0-8].|E90)'
f00_f99 <- '(F..)'
g00_g99 <- '(G..)'
h00_h59 <- '(H[0-5].)'
h60_h95 <- '(H[6-8].|H9[0-5])'
i00_i99 <- '(I..)'
j00_j99 <- '(J..)'
k00_k93 <- '(K[0-8].|K9[0-3])'
l00_l99 <- '(L..)'
m00_m99 <- '(M..)'
n00_n99 <- '(N..)'
o00_o99 <- '(O..)'
p00_p96 <- '(P[0-8].|P9[0-6])'
q00_q99 <- '(Q..)'
r00_r99 <- '(R..)'
s00_t98 <- '(S..|T[0-8].|T9[0-8])'
v01_y98 <- '(V0[1-9]|V[1-9].|Y[0-8].|Y9[1-8])'
z00_z99 <- '(Z..)'