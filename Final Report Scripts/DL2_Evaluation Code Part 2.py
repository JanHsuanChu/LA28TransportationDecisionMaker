def get_travel_time_reduction(d3, d5, d6):
    # Define the min/max of the utility scale
    w_min = 0
    w_max = 30  # max plausible % travel time reduction

    # D3: more sources = more available data so time can be reduced
    # 1 source, 2 sources, or 3 sources
    A_map = {1: 0.3, 2: 0.6, 3: 1.0}
    A = A_map.get(d3, 0)

    # D5 better engine implies better data use 
    # 0 = Plug-and-Play Engine; 1 = Custom Engine
    Q_map = {0: 0.5, 1: 1.0}
    Q = Q_map.get(d5, 0)

    # D6: AI improves routing intelligence 
    # 0 = No AI; 1 = AI Used
    M_map = {0: 0.4, 1: 1.0}
    M = M_map.get(d6, 0)

    # Coefficients (theta): weighted contribution of each intermediate variable
    #Theta_1 has highest weight since data availability is the foundation
    theta_1 = 0.40  # weight on data availability
    theta_2 = 0.30  # weight on data quality
    theta_3 = 0.30  # weight on model capability

    # Compute raw score in [0,1] then scale to utility range
    raw_score = theta_1 * A + theta_2 * Q + theta_3 * M
    metric = raw_score * (w_max - w_min) + w_min

    return metric

def get_prediction_accuracy(d2, d3, d6):
    # Define the min/max of the utility scale
    # A system with zero data still has ~50% floor (random baseline)
    w_min = 0.50
    w_max = 0.99

    # D3 -> data quality (Q): more sources improve accuracy
    # 1 source, 2 sources, or 3 sources
    Q_map = {1: 0.3, 2: 0.6, 3: 1.0}
    Q = Q_map.get(d3, 0)

    # D2 -> data sharing bonus: open sharing improves Q by removing gaps
    Q_bonus_map = {1: 0.00, 2: 0.10, 3: 0.20}
    Q_bonus = Q_bonus_map.get(d2, 0)
    Q_adj = min(Q + Q_bonus, 1.0)  # cap at 1.0

    # D6 -> model capability (M): AI improves prediction accuracy
    # 0 = No AI; 1 = AI Used
    M_map = {0: 0.4, 1: 1.0}
    M = M_map.get(d6, 0)

    # Coefficients (eta): data quality weighted slightly higher than model
    # because even good model fails on bad data
    eta_1 = 0.55  # weight on adjusted data quality
    eta_2 = 0.45  # weight on model capability

    raw_score = eta_1 * Q_adj + eta_2 * M
    metric = raw_score * (w_max - w_min) + w_min

    return metric

def get_response_time(d2, d5, d6):
    # Define the min/max of the utility scale (seconds)
    
    w_min = 1   # best = 1 second
    w_max = 60  # worst = 60 seconds

    # d2 = integration complexity (I):
    # More sharing partners means more overhead to ingest data
    I_map = {1: 0.2, 2: 0.5, 3: 1.0}
    I = I_map.get(d2, 0)

    # d5 = base computational complexity (C)
    # 0 = Plug-and-Play Engine; 1 = Custom Engine
    C_engine_map = {0: 0.4, 1: 0.8}
    C_engine = C_engine_map.get(d5, 0)

    # d6 = additional computational complexity from AI (C)
    # 0 = No AI; 1 = AI Used
    C_ai_map = {0: 0.0, 1: 0.8}
    C_ai = C_ai_map.get(d6, 0)

    # Combined computational complexity
    C = min(C_engine + C_ai, 1.0)

    # Coefficients (lambda)
    lambda_1 = 0.35  # weight on integration complexity
    lambda_2 = 0.65  # weight on computational complexity

    raw_score = lambda_1 * I + lambda_2 * C
    metric = raw_score * (w_max - w_min) + w_min  # seconds

    return metric

def get_data_coverage(d2, d3):
    # Define the min/max of the utility scale
    w_min = 0
    w_max = 100

    # d3 = base source coverage fraction
    # 1 source, 2 sources, or 3 sources
    source_cov_map = {1: 0.30, 2: 0.60, 3: 1.00}
    source_cov = source_cov_map.get(d3, 0)

    # d2 = open sharing fills in gaps within sources. The more the better
    sharing_mult_map = {1: 0.70, 2: 0.85, 3: 1.00}
    sharing_mult = sharing_mult_map.get(d2, 0)

    # Coverage = sources selected * how accessible those sources are
    raw_score = source_cov * sharing_mult
    metric = raw_score * (w_max - w_min) + w_min

    return metric

#Evaluate all metrics for one architecture
def evaluate_architecture(arch):
    d2, d3, d5, d6 = arch["d2"], arch["d3"], arch["d5"], arch["d6"]

    return {
        "travel_time_reduction": get_travel_time_reduction(d3, d5, d6),
        "prediction_accuracy":       get_prediction_accuracy(d2, d3, d6),
        "response_time":         get_response_time(d2, d5, d6),
        "data_coverage":         get_data_coverage(d2, d3),
    }

#Reference Architectures
if __name__ == "__main__":
    import pandas as pd

    
    arch_A = {"d2": 1, "d3": 1, "d5": 0, "d6": 0}

    arch_B = {"d2": 2, "d3": 1, "d5": 1, "d6": 1}

    arch_C = {"d2": 2, "d3": 2, "d5": 1, "d6": 1}

    archs = {
        "A: DC Metro":          arch_A,
        "B: CityMapper":         arch_B,
        "C: Paris 2024":       arch_C,
    }

    rows = []
    for name, arch in archs.items():
        result = evaluate_architecture(arch)
        rows.append({
            "Architecture":           name,
            "TTR (%)":                round(result["travel_time_reduction"], 2),
            "Accuracy":               round(result["prediction_accuracy"], 3),
            "Response Time (s)":      round(result["response_time"], 1),
            "Data Coverage (%)":      round(result["data_coverage"], 1),
        })

    df = pd.DataFrame(rows).set_index("Architecture")
    pd.set_option("display.float_format", "{:.2f}".format)
    print(df.to_string())

