prj_chemical_system(
    chemical_solver = "Phreeqc",
    database = "PSINA_12_07_110615_DAV_s.dat",
    solution = prj_solution(
        temperature = 25,
        pressure = 1,
        pe = 4,
        components = list(
            component = "C(4)",
            component = "Ca"
        ),
        charge_balance = "pH"
    ),
    mesh = "calcite_ReactiveDomain",
    knobs = list(
        max_iter = "100",
        relative_convergence_tolerance = "1e-12",
        tolerance = "1e-15",
        step_size = "100",
        scaling = "0"
    ),
    equilibrium_reactants = list(
        phase_component = prj_phase_component(
            name = "Calcite",
            saturation_index = 0,
            initial_amount = 0.000207
        )
    )
)
