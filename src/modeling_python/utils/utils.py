import yaml


def import_config(relative_fp):
    """
    Pulls in yaml file and creates a python dictionary
    :param file_path: file path for yaml file
    :output: A dictionary with format {(channel, campaign) : {"alpha":0.3, "lwr"...}
    """
    with open(relative_fp, 'r') as stream:
        try:
            config = yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            print(exc)
    return config
