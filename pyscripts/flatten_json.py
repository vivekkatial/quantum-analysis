import json
from itertools import chain, starmap
import pandas as pd
from copy import deepcopy


def flatten_json_iterative_solution(dictionary):
    """Flatten a nested json file"""

    def unpack(parent_key, parent_value):
        """Unpack one level of nesting in json file"""
        # Unpack one level only!!!

        if isinstance(parent_value, dict):
            for key, value in parent_value.items():
                temp1 = parent_key + '_' + key
                yield temp1, value
        elif isinstance(parent_value, list):
            i = 0
            for value in parent_value:
                temp2 = parent_key + '_' + str(i)
                i += 1
                yield temp2, value
        else:
            yield parent_key, parent_value

    # Keep iterating until the termination condition is satisfied
    while True:
        # Keep unpacking the json file until all values are atomic elements (not dictionary or list)
        dictionary = dict(chain.from_iterable(starmap(unpack, dictionary.items())))
        # Terminate condition: not any value in the json file is dictionary or list
        if not any(
            isinstance(value, dict) for value in dictionary.values()
        ) and not any(isinstance(value, list) for value in dictionary.values()):
            break

    return dictionary


if __name__ == "__main__":

    instance_id = "data/7-node-example.json"

    with open(
        instance_id
    ) as file:
        data = json.load(file)

    evolution_data_row_item = {
        "layer": None,
        "state": None,
        "probability": None,
        "p_success": None,
        "energy": None,
        "qubo_ind": None,
        "instance": None,
        "restart": None
    }

    evolution_data = []

    for i, qubo in enumerate(data["solution_data"]):
        evolution_data_row_item = {}
        evolution_data_row_item["instance"] = instance_id
        evolution_data_row_item["qubo_ind"] = i

        restart = 0
        for k,layer in enumerate(qubo["evolution"]):
            evolution_data_row_item["layer"] = layer["p"]
            evolution_data_row_item["p_success"] = layer["probability_success"]
 
            if k % 10 == 0:
                restart += 1
            for state in layer["state"].items():
                val = deepcopy(state)
                evolution_data_row_item["state"] = val[0]
                evolution_data_row_item["probability"] = val[1]["probability"]
                evolution_data_row_item["energy"] = val[1]["energy"]
                evolution_data_row_item["restart"] = restart


                new_dict = evolution_data_row_item.copy()
                evolution_data.append(new_dict)

    df = pd.DataFrame(evolution_data)

    df.to_csv("data/mlflow_instance.csv")
