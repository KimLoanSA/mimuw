from .cluster import CharacterCluster
from .html_creator import HTMLCreator
from .argument_parser import parse_arguments
import ntpath
import timeit


class Main:
    """Entrypoint to the clustering program
    """
    def __init__(self):
        self.parsed_arguments = parse_arguments()

    def execute(self):
        """Executes clustering - read program arguments, fits model and generates files with results

        ----------
        :return: self
        """
        start_time = timeit.default_timer()

        images = self._parse_images_list()
        character_cluster = CharacterCluster(
            self.parsed_arguments.metric,
            self.parsed_arguments.eps,
            self.parsed_arguments.n_jobs,
            self.parsed_arguments.image_additional_padding
        )
        clusters = character_cluster.cluster_images(images)

        print("Generating result file with name: {} ...".format(self.parsed_arguments.output_file))
        self._save_clusters_to_file(clusters)
        print("Result file generated!")

        print("Generating HTML file with name: {} ...".format(self.parsed_arguments.output_html_file))
        html_creator = HTMLCreator(clusters)
        html_creator.generate_html(self.parsed_arguments.output_html_file)
        print("HTML file generated!")

        stop_time = timeit.default_timer()
        time_span = stop_time - start_time
        print("Execution finished in {} seconds ({} minutes).".format(time_span, time_span / 60))

    def _parse_images_list(self):
        with open(self.parsed_arguments.images_list_file_path) as file:
            return file.read().splitlines()

    def _save_clusters_to_file(self, clusters):
        with open(self.parsed_arguments.output_file, "w") as file:
            for _, cluster in clusters.items():
                for image in cluster:
                    file.write("{} ".format(Main._extract_file_name(image.image_path)))
                file.write("\n")

    @staticmethod
    def _extract_file_name(file_path):
        head, tail = ntpath.split(file_path)
        return tail or ntpath.basename(head)
