r2ogs6_rate(
    kinetic_reactant = "Productc",
    expression = list(
        statement = "Km = 10",
        statement = "U = 1e-3",
        statement =
            "rate = U * TOT(\"Synthetica\") / (Km + TOT(\"Syntheticb\"))",
        statement = "moles = - rate * TIME",
        statement = "save moles"
    )
)
