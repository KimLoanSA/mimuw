from pyhtml import *


class HTMLCreator:
    """Generates HTML file with result clusters.

    Generates HTML file with given name with result clusters, where clusters are separated with <hr>

    ----------
    :param clusters: dict,
        Dictionary with clusters to display
    """
    def __init__(self, clusters):
        self.clusters = clusters

    def generate_html(self, output_file_name):
        """Creates HTML with name ``output_file_name`` and saves clustering there.

        ----------
        :param output_file_name: Name of the file
        :return: self
        """
        template = html(
            head(
                title("Clustering results"),
            ),
            body(
                div(
                    HTMLCreator._generate_all_clusters(self.clusters)
                )
            )
        )

        with open(output_file_name, "w") as file:
            file.write(template.render())

    @staticmethod
    def _generate_all_clusters(clusters):
        for _, cluster in clusters.items():
            yield HTMLCreator._generate_single_cluster(cluster), hr

    @staticmethod
    def _generate_single_cluster(cluster):
        for image in cluster:
            yield img(src=image.image_path)
