import argparse
from .defaults import *


def parse_arguments():
    """Reads program arguments

    ----------
    :return: Parsed program arguments
    """
    parser = argparse.ArgumentParser(description='Program that clusters characters')

    parser.add_argument("images_list_file_path",
                        help="path to file with listed character images")
    parser.add_argument("--output_file", default=default_output_file,
                        help="name of file with the final clustering "
                             "(default: {})".format(default_output_file))
    parser.add_argument("--output_html_file", default=default_output_html_file,
                        help="name of HTML file with the final clustering "
                             "(default: {})".format(default_output_html_file))
    parser.add_argument("--metric", default=default_metric,
                        help="DBSCAN metric value "
                             "(default: {})".format(default_metric))
    parser.add_argument("--eps", default=default_eps, type=int,
                        help="DBSCAN eps value "
                             "(default: {})".format(default_eps))
    parser.add_argument("--n_jobs", default=default_n_jobs, type=int,
                        help="DBSCAN n_jobs value "
                             "(default: {})".format(default_n_jobs))
    parser.add_argument("--image_additional_padding", default=default_image_additional_padding, type=int,
                        help="Minimal padding added to images"
                             "(default: {})".format(default_image_additional_padding))

    return parser.parse_args()
